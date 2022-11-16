################################
# 第二週: データを扱うためのリテラシー
################################
library(tidyverse)
library(here)
library(patchwork)
library(ssdse)
library(units)
library(ggrepel)
course_colors <- c("#364968", "#fddf97", "#e09664", "#6c4343", "#ffffff")


# データの準備 ------------------------------------------------------------------
df_animal <-
  read_csv("https://raw.githubusercontent.com/uribo/tokupon_ds/main/data-raw/tokushima_zoo_animals22.csv",
                  col_types = "ccdd")
df_shikoku_kome_sisyutu2019to2021 <-
  read_csv(here("data-raw/家計調査_1世帯当たり1か月間の支出金額_購入数量及び平均価格_四国4県.csv"),
           col_types = "cccccd")
df_shikoku_weather2019to2021 <-
  read_csv(here("data-raw/shikoku_weather2019to2021.csv"),
           col_types = "iiccdd")
df_ssdse_b_raw <- 
  read_ssdse_b(here("data-raw/SSDSE-B-2022.csv"), lang = "ja")
df_ssdse_b <- 
  df_ssdse_b_raw |> 
  # マルチバイト文字列（日本語など）の変数名を指定する際は アクセント ` で囲むようにします 
  select(`年度`, `都道府県`, `人口・世帯`, `家計`) |> 
  unnest(cols = c(`人口・世帯`, `家計`))
df_ssdse_b2019 <- 
  df_ssdse_b |> 
  filter(`年度` == 2019)
glimpse(df_ssdse_b2019)


# データの種類 ------------------------------------------------------------------
# 問題: 次のデータの各変数はどの尺度水準に分類されるか
# 四国4件の婚姻件数と婚姻率（2019年度）
df_ssdse_b2019 |> 
  filter(`都道府県` %in% c("徳島県", "香川県", "愛媛県", "高知県")) |> 
  mutate(`人口順位` = as.numeric(fct_rev(fct_reorder(`都道府県`, `総人口`))),
         `婚姻率_人口千人あたりの婚姻件数` =  (`婚姻件数` / `総人口`) * 1000) |> 
  select(`年度`, `都道府県`, 
         `総人口`,
         `人口順位`, `婚姻件数`, `婚姻率_人口千人あたりの婚姻件数`)


# 欠損値 ---------------------------------------------------------------------
df_ssdse_b2019 |> 
  purrr::map_dbl(~ sum(is.na(.x))) |> 
  unname()


# 外れ値 ---------------------------------------------------------------------
# 動物のデータではホッキョクグマ、ライオン、シロオリックスが他の動物と比べて大きい
df_animal |> 
  ggplot() +
  aes(body_length_cm, weight_kg) +
  geom_point() +
  geom_text_repel(data = df_animal |> 
                    filter(weight_kg > 200),
                  aes(label = name))
# 動物データ全体での体重の平均
mean(df_animal$body_length_cm, na.rm = TRUE)
# 大きな3種を含めないで体重の平均値を求めると結果が大きく異なる
mean(df_animal |> 
       filter(weight_kg <= 200) |> 
       pull(body_length_cm), na.rm = TRUE)

# 東京都の人口は外れ値？
df_ssdse_b2019 |> 
  ggplot() +
  aes(forcats::fct_reorder(`都道府県`, `総人口`), `総人口`) +
  geom_bar(stat = "identity") +
  coord_flip()
# 東京都の人口は外れ値？
# 箱ひげ図での確認
df_ssdse_b2019 |> 
  ggplot() +
  aes(y = `総人口`) +
  geom_boxplot()
# 箱ひげ図の発展系... 
# library(ggbeeswarm)
# library(ggdist)


# データの要約 ------------------------------------------------------------------
summary(df_animal)
skimr::skim(df_animal)
psych::describe(df_animal)


# 共分散 ---------------------------------------------------------------------

# 相関係数 --------------------------------------------------------------------
# library(corrr)
df_icecream_temperature <- 
  df_shikoku_kome_sisyutu2019to2021 |> 
  filter(`市` == "徳島市",
         `品目分類` == "アイスクリーム・シャーベット",
         `項目` == "購入頻度_100世帯当たり") |> 
  select(ym, `項目`, value) |> 
  left_join(
    df_shikoku_weather2019to2021 |> 
      filter(station_name == "徳島") |> 
      transmute(ym = str_c(year,
                           str_pad(month, width = 2, pad = "0")),
                temperature_average_c),
    by = "ym")

df_icecream_temperature |> 
  ggplot() +
  aes(temperature_average_c, value) +
  geom_point() +
  xlab("月平均気温(\u2103)") +
  ylab("100世帯当たり購入頻度") +
  labs(title = "徳島市における気温と「アイスクリーム・シャーベット」の購入頻度の関係",
       caption = "データ: 気象庁 過去の気象データおよび
       「家計調査」表番号4-1
       1世帯当たり1か月間の支出金額，購入数量及び平均価格 都市階級・地方・都道府県庁所在市別")
cor(df_icecream_temperature$temperature_average_c,
    df_icecream_temperature$value)


# 見せかけの相関（疑似相関） -----------------------------------------------------------
df_pesticide_ice <- 
  df_shikoku_kome_sisyutu2019to2021 |> 
  filter(`市` == "徳島市",
         `項目` == "購入頻度_100世帯当たり",
         `品目分類` == "殺虫・防虫剤") |> 
  select(ym, pesticide = value) |> 
  left_join(
    df_shikoku_kome_sisyutu2019to2021 |> 
      filter(`市` == "徳島市",
             `項目` == "購入頻度_100世帯当たり",
             `品目分類` == "アイスクリーム・シャーベット") |> 
      select(ym, ice = value),
    by = "ym")

# 「殺虫・防虫剤」と「アイスクリーム・シャーベット」の購入頻度の間には正の相関
cor(df_pesticide_ice$pesticide,
    df_pesticide_ice$ice)

df_pesticide_ice |>
  ggplot() +
  aes(ice, pesticide) +
  geom_point() +
  xlab("「アイスクリーム・シャーベット」") +
  ylab("「殺虫・防虫剤」") +
  labs(title = "徳島市における「殺虫・防虫剤」と「アイスクリーム・シャーベット」の\n100世帯当たりの購入頻度",
       caption = "データ: 「家計調査」表番号4-1
       1世帯当たり1か月間の支出金額，購入数量及び平均価格 都市階級・地方・都道府県庁所在市別") +
  theme(title = element_text(size = 6))
# ggsave(here("images/relationships_for_ice_and_pesticide.png"),
#        width = 5,
#        height = 4)

df_pesticide_ice |> 
  pivot_longer(cols = c(pesticide, ice)) |> 
  mutate(ym = lubridate::ym(ym),
         name = case_when(
           name == "pesticide" ~ "殺虫・防虫剤",
           name == "ice" ~ "アイスクリーム・シャーベット",
         )) |> 
  ggplot() +
  aes(ym, value, group = name, fill = name) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab(NULL) +
  ylab("100世帯当たりの購入頻度") +
  labs(title = "徳島市における「殺虫・防虫剤」と「アイスクリーム・シャーベット」の100世帯当たりの購入頻度",
       caption = "データ: 「家計調査」表番号4-1
       1世帯当たり1か月間の支出金額，購入数量及び平均価格 都市階級・地方・都道府県庁所在市別") +
  scale_fill_manual(values = course_colors[1:2]) +
  scale_x_date(labels = scales::label_date("%Y年%b", locale = "ja"),
               date_breaks = "6 month") +
  guides(fill = guide_legend(title = "")) +
  theme(title = element_text(size = 6),
        legend.position = "bottom")
# ggsave(here("images/ts_ice_and_pesticide_purchase.png"),
#        width = 7,
#        height = 4)

# クロス集計表 ------------------------------------------------------------------




# アンスコムの例 -----------------------------------------------------------------
anscombe

anscombe_long <- 
  anscombe |> 
  tidyr::pivot_longer(
    tidyselect::everything(),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)")
# 記述統計量（平均と分散）の算出
# setがデータセットの種類を示します
# set間で値に大きな差はありません
# 相関係数も小数点第二位まではset間で同じ値となります
anscombe_long |> 
  group_by(set) |> 
  summarise(across(.cols = c(x, y), .fns = list(mean = mean, sd = sd)),
            .groups = "keep") |> 
  summarise(across(.cols = contains("_"), .fns = ~ round(.x, digits = 2))) |> 
  left_join(
    anscombe_long |> 
      group_by(set) |> 
      group_modify(~ tibble::tibble(cor = cor.test(.x$x, .x$y)$estimate)) |> 
      ungroup() |> 
      mutate(cor = round(cor, digits = 2)),
    by = "set")

anscombe_long |> 
  group_by(set) |> 
  group_map(
    ~ ggplot(.x, aes(x, y)) +
      geom_point(color = course_colors[1]) +
      geom_smooth(method = lm, 
                  se = FALSE, 
                  color = course_colors[2])) |> 
  wrap_plots(ncol = 4)
# ggsave(here("images/anscombes_quartet.png"),
#        width = 7,
#        height = 2.2)

# アンスコムサウルス -------------------------------------------------------------------
library(datasauRus)
datasaurus_dozen |> 
  filter(dataset == "dino") |> 
  ggplot(aes(x = x, y = y)) +
  geom_point()
# ggsave(here("images/datasaurus.png"),
#        width = 5,
#        height = 4)

datasaurus_dozen |> 
  filter(dataset != "dino") |> 
  ggplot(aes(x = x, y = y, colour = dataset)) +
  geom_point() +
  theme(legend.position = "none") +
  facet_wrap(~dataset, ncol = 3)
# ggsave(here("images/several_datasaurus.png"),
#        width = 5,
#        height = 6)
datasaurus_dozen |> 
  group_by(dataset) |> 
  summarise(across(.cols = c(x, y), .fns = list(mean = mean, sd = sd)),
            .groups = "keep") |> 
  summarise(across(.cols = contains("_"), .fns = ~ round(.x, digits = 2))) |> 
  left_join(
    datasaurus_dozen |> 
      group_by(dataset) |> 
      group_modify(~ tibble::tibble(cor = cor.test(.x$x, .x$y)$estimate)) |> 
      ungroup() |> 
      mutate(cor = round(cor, digits = 2)),
    by = "dataset")


# ヒストグラム ------------------------------------------------------------------

# 箱ひげ図 --------------------------------------------------------------------
quantile(df_ssdse_b2019$`総人口`)

# 棒グラフ --------------------------------------------------------------------
# 縦にする、軸にだまされない

# 散布図 ---------------------------------------------------------------------


