################################
# 第五週: さまざまなデータの扱い
################################
library(tidyverse) # データ分析全般
library(here) # フォルダ、ファイルの指定を簡易に
library(ssdse)
library(patchwork)
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

as_tsibble(df_covid19_japan) |> 
  as.ts() |> 
  stl("per") |> 
  plot()
