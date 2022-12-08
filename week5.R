################################
# 第五週: さまざまなデータの扱い
################################
library(tidyverse) # データ分析全般
library(here) # フォルダ、ファイルの指定を簡易に
library(ssdse)
library(patchwork)
library(tsibble) # 時系列データへの変換
library(feasts) # 時系列データ操作
library(sf) # 地理空間データへの変換、演算
library(mapview)
library(jmastats)
library(rnaturalearth)
library(rnaturalearthhires)
library(covidregionaldata) # 新型コロナウイルス感染症感染者数
course_colors <- c("#364968", "#fddf97", "#e09664", "#6c4343", "#ffffff")
theme_set(theme_bw()) # ggplot2のテーマを指定

# データの用意 ------------------------------------------------------------------
source(here("_pins.R")) # pins_resources_online, pins_resources_local
df_ice_weather <-
  pins_resources_local |> 
  pins::pin_read("tksm_sales_weather") |> 
  select(ym, ice) |> 
  transmute(ymd = lubridate::ym(ym),
            ice)

df_covid19_japan <- 
  # ジョンズ・ホプキンズ大学による日本国内の新型コロナウイルス感染症に関するデータ
  get_national_data(countries = "japan", source = "JHU") |> 
  select(date, cases_new)

# 時系列データ ------------------------------------------------------------------
# 時系列プロット
df_ice_weather |>
  ggplot() +
  aes(ymd, ice) +
  geom_point() +
  geom_line() + 
  labs(title = "アイスクリーム・シャーベットの購入頻度についての時系列プロット")

# 同じデータでも観測頻度の違いによって見た目が変わる
p1 <- 
  df_covid19_japan |> 
  ggplot(aes(date, cases_new)) +
  geom_line() +
  labs(title = "日次")
p2 <- 
  df_covid19_japan |> 
  mutate(ym = lubridate::ym(paste0(lubridate::year(date),
                                   stringr::str_pad(lubridate::month(date),
                                                    width = 2,
                                                    pad = "0")))) |> 
  group_by(ym) |> 
  summarise(cases_new = sum(cases_new, na.rm = TRUE)) |> 
  ggplot(aes(ym, cases_new)) +
  geom_line() +
  scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
  labs(title = "月次")
p3 <- 
  df_covid19_japan |> 
  mutate(year = lubridate::ym(paste0(lubridate::year(date),
                                     "01"))) |> 
  group_by(year) |> 
  summarise(cases_new = sum(cases_new, na.rm = TRUE)) |> 
  ggplot(aes(year, cases_new)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "年次")

p1 + p2 + p3 +
  plot_layout(ncol = 3)
# ggsave(here("images/covid19_cases.png"),
#        width = 15,
#        height = 4)


# STL分解
as_tsibble(df_covid19_japan,
           index = date) |>
  as.ts() |>
  stl("periodic") |>
  plot()
dcmp <-
  as_tsibble(df_covid19_japan,
             index = date) |> 
  model(STL(cases_new ~ season(window = Inf)))
components(dcmp) |>
  autoplot()
# ggsave(here("images/covid19_cases_tsl.png"),
#        width = 5,
#        height = 7)

# 地理空間データ -----------------------------------------------------------------
# 徳島駅の座標
tksm_st_p <- 
  st_point(c(134.55129, 34.07470))
# plot(tksm_st_p)
# 佐古駅の座標
sako_st_p <- 
  st_point(c(134.52978, 34.0806))
# 徳島駅と佐古駅の間の線（直線）
tksm_st_line <- 
  st_linestring(
  matrix(c(134.55129, 34.07470,
           134.52978, 34.0806),
         nrow = 2,
         byrow = TRUE))
# plot(tksm_st_line)
# 北方領土を含めた国土
jp_area <- 
  matrix(
  c(122.932502747, 20.42527771,
    153.986663818, 20.42527771,
    153.986663818, 45.557220459,
    122.932502747, 45.557220459,
    122.932502747, 20.42527771),
  byrow = TRUE,
  ncol = 2) |>  
  list() |> 
  st_polygon()
# plot(jp_area)

# 徳島駅と佐古駅のポイントに地理座標系（世界測地系）を指定する
sfc_tksm_st_p <- 
  st_sfc(tksm_st_p, crs = 4326)
sfc_sako_st_p <- 
  st_sfc(sako_st_p, crs = 4326)

# 地図上にポイントをマッピング
mapview(sfc_tksm_st_p) +
  mapview(sfc_sako_st_p)

# 座標参照系
# 徳島駅と佐古駅の距離
st_distance(
  sfc_tksm_st_p,
  sfc_sako_st_p)
st_distance(
  st_transform(sfc_tksm_st_p, crs = 6672),
  st_transform(sfc_sako_st_p, crs = 6672))
# バッファの生成
# 地理座標系ではゆがみが生じる
tksm_st_p |> 
  st_sfc(crs = 4326) |> 
  st_buffer(dist = units::set_units(1, km)) |> 
  st_sf() |> 
  mapview()
tksm_st_p |> 
  st_sfc(crs = 4326) |> 
  st_transform(crs = 6672) |> 
  st_buffer(dist = units::set_units(1, km)) |> 
  st_transform(crs = 4326) |> 
  st_sf() |> 
  mapview()
