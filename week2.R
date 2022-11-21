################################
# 第二週: データを扱うためのリテラシー
################################
library(tidyverse)
library(here)
library(patchwork)
library(ssdse)
library(units)
library(ggrepel)
library(visdat)
course_colors <- c("#364968", "#fddf97", "#e09664", "#6c4343", "#ffffff")


# データの準備 ------------------------------------------------------------------
source(here("_pins.R"))
# 動物データ
df_animal <-
  pins_resources_online |> 
  pins::pin_download("tokushima_zoo_animals22") |> 
  read_csv(col_types = "ccdd") |> 
  # 体重が200kg以上の場合にTRUE
  mutate(heavy_mt_200kg = weight_kg > 200)

df_shikoku_kome_sisyutu2019to2021 <-
  pins_resources_local |> 
  pins::pin_read("shikoku_kome_sisyutu2019to2021") |> 
  as_tibble()

df_shikoku_weather2019to2021 <-
  pins_resources_local |> 
  pins::pin_read("shikoku_weather2019to2021") |> 
  as_tibble()

df_ssdse_b_raw <- 
  pins_resources_online |> 
  pins::pin_download("ssdse_b") |> 
  read_ssdse_b(lang = "ja")
df_ssdse_b <- 
  df_ssdse_b_raw |> 
  # マルチバイト文字列（日本語など）の変数名を指定する際は アクセント ` で囲むようにします 
  select(`年度`, `都道府県`, `人口・世帯`, `家計`) |> 
  unnest(cols = c(`人口・世帯`, `家計`))
df_ssdse_b2019 <- 
  df_ssdse_b |> 
  filter(`年度` == 2019)
# glimpse(df_ssdse_b2019)

df_icecream_temperature <- 
  df_shikoku_kome_sisyutu2019to2021 |> 
  filter(`市` == "徳島市",
         `品目分類` == "アイスクリーム・シャーベット",
         `項目` == "購入頻度_100世帯当たり") |> 
  select(ym, `項目`, value) |> 
  mutate(ym = as.character(ym)) |> 
  left_join(
    df_shikoku_weather2019to2021 |> 
      filter(station_name == "徳島") |> 
      transmute(ym = str_c(year,
                           str_pad(month, width = 2, pad = "0")),
                temperature_average_c),
    by = "ym")

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
    by = "ym") |> 
  mutate(ym = as.character(ym)) |> 
  left_join(
    df_shikoku_weather2019to2021 |> 
      filter(station_name == "徳島") |> 
      transmute(ym = str_c(year,
                           str_pad(month, width = 2, pad = "0")),
                temperature_average_c),
    by = "ym")

anscombe_long <- 
  anscombe |> 
  tidyr::pivot_longer(
    tidyselect::everything(),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)")

# データの種類 ------------------------------------------------------------------
# 問題: 次のデータの各変数はどの尺度水準に分類されるか
# 四国4件の婚姻件数と婚姻率（2019年度）
df_ssdse_b2019 |> 
  filter(`都道府県` %in% c("徳島県", "香川県", "愛媛県", "高知県")) |> 
  mutate(`人口順位` = as.numeric(fct_rev(fct_reorder(`都道府県`, `総人口`))),
         `婚姻率_人口千人あたりの婚姻件数` =  (`婚姻件数` / `総人口`) * 1000) |> 
  select(`年度`, `都道府県`, 
         # `総人口`,
         `人口順位`, `婚姻件数`, `婚姻率_人口千人あたりの婚姻件数`)
# 年度... 間隔尺度
# 都道府県... 名義尺度
# 人口順位... 順序尺度
# 婚姻件数... 比例尺度
# 婚姻率... 比例尺度


# 欠損値 ---------------------------------------------------------------------
# 動物データの各列にどれだけ欠損を含むか
df_animal |> 
  purrr::map_dbl(~ sum(is.na(.x)))
# Rでは欠損はNAとして記録される
df_animal$body_length_cm
c(NA, 1)
is.na(c(NA, 1))
# 欠損値を処理しない場合には、統計的計算処理が不可能になることがある
sum(df_animal$body_length_cm)
mean(df_animal$body_length_cm)
# Rではそのような統計的計算処理を行う関数には、欠損値を除いて処理を行うためのオプションが用意される
# na.rm = TRUE を指定すると欠損値を除外して処理が行われる
sum(df_animal$body_length_cm, na.rm = TRUE)
mean(df_animal$body_length_cm, na.rm = TRUE)

# visdatパッケージを使った欠損値の視覚化
vis_miss(df_animal)

# 外れ値 ---------------------------------------------------------------------
# 動物のデータではホッキョクグマ、ライオン、シロオリックスが他の動物と比べて大きい
df_animal |> 
  ggplot() +
  aes(body_length_cm, weight_kg) +
  geom_point() +
  geom_text_repel(data = df_animal |> 
                    filter(heavy_mt_200kg == TRUE),
                  aes(label = name))
# 動物データ全体での体重の平均
mean(df_animal$body_length_cm, na.rm = TRUE)
# 大きな3種を含めないで体重の平均値を求めると結果が大きく異なる
mean(df_animal |> 
       filter(heavy_mt_200kg == FALSE) |> 
       pull(body_length_cm), na.rm = TRUE)

# 東京都の人口は外れ値？
df_ssdse_b2019 |> 
  ggplot() +
  aes(forcats::fct_reorder(`都道府県`, `総人口`), `総人口`) +
  geom_bar(stat = "identity") +
  coord_flip()

# 平均値
x <- c(1, 10, 5, 3, 7)
sum(x) / length(x)
# mean()関数を用いて平均値を計算します
mean(x)
# 中央値
sort(x)[ceiling(length(x)/2)]
# median()関数で数値ベクトルの中央値を計算できます
median(x)
# 要素の数が偶数のときの中央値
x <- sort(c(1, 2, 4, 6))
(x[length(x)/2] + rev(x)[length(x)/2]) / 2
median(x)
# 最頻値
x <- c(1, 3, 3, 5, 5, 5, 7, 10)
as.numeric(names(which.max(table(x))))


# 四分位 --------------------------------------------------------------------
quantile(df_animal$weight_kg, na.rm = TRUE)


# 分散 ----------------------------------------------------------------------
# Rは分散を求める関数（不偏分散）がありますが、これまで使った関数や演算子を使って分散を求めてみましょう
# 答え合わせは講座の中で
# 1. 変数の平均値を求める
mean(df_animal$body_length_cm, na.rm = TRUE)
# 2. 変数の各値と平均値の差（偏差）を求める
df_animal$body_length_cm[1] - mean(df_animal$body_length_cm, na.rm = TRUE)
# 3. 偏差を二乗する
(df_animal$body_length_cm[1] - mean(df_animal$body_length_cm, na.rm = TRUE))^2
# 4. すべての値に対して1から3を繰り返し、偏差の二乗を合計する
sum(c(df_animal$body_length_cm - mean(df_animal$body_length_cm, na.rm = TRUE))^2, na.rm = TRUE)
# 5. 合計した値をデータの数で割る
sum(c(df_animal$body_length_cm - mean(df_animal$body_length_cm, na.rm = TRUE))^2, na.rm = TRUE) / (length(na.omit(df_animal$body_length_cm)) - 1)

var(df_animal$body_length_cm, na.rm = TRUE)
var(df_animal$weight_kg, na.rm = TRUE)

var(c(0, 0, 0, 0))
var(c(1, 2, 3, 2, 1))
var(c(1, 100, 5, 8, 1))
var(c(1, 6, 40, 56, 1))


# 標準偏差 --------------------------------------------------------------------
units::set_units(df_animal$body_length_cm, cm)^2
sqrt(var(df_animal$body_length_cm, na.rm = TRUE))
# Rの標準偏差を求める関数sd()は不偏標準偏差として扱います
sd(df_animal$body_length_cm, na.rm = TRUE)


# 度数分布表 -------------------------------------------------------------------
# 動物データの分類群ごとの頻度を数える
table(df_animal$taxon)
# 度数分布表の形にする
df_animal |> 
  count(taxon, name = "frequency")

body_length_freq <- 
  # 動物データの体長を40cm間隔の階級に分けて頻度を数える
  table(cut(df_animal$body_length_cm, 
            breaks = seq(20, 
                         200, 
                         # 各度数に含まれる区間の幅を階級幅という
                         # 階級幅や階級数はデータの範囲を見て決める
                         by = 40)))
tibble(
  class = names(body_length_freq),
  frequency = body_length_freq)


# ヒストグラム ------------------------------------------------------------------
df_animal |> 
  ggplot(aes(body_length_cm)) +
  # ヒストグラムでは柱の階級をビン bin と呼びます
  geom_histogram(bins = 4) +
  ylab("Frequency") +
  xlab("Body length (cm)") +
  labs(title = "動物の体長のヒストグラム")
# ggsave(here("images/animal_body_length_histogram.png"),
#        width = 5,
#        height = 4)

# ヒストグラムの階級数を変えると形も変わります
p1 <- 
  df_animal |> 
  ggplot(aes(body_length_cm)) +
  geom_histogram(bins = 2, fill = course_colors[1]) +
  ylab("Frequency") +
  xlab("Body length (cm)") +
  labs(title = "動物の体長のヒストグラム",
       subtitle = "階級数2")
p2 <- 
  df_animal |> 
  ggplot(aes(body_length_cm)) +
  geom_histogram(bins = 10, fill = course_colors[2]) +
  ylab("Frequency") +
  xlab("Body length (cm)") +
  labs(title = "動物の体長のヒストグラム",
       subtitle = "階級数10")
p1 + p2 +
  plot_layout(ncol = 2)
# ggsave(here("images/animal_body_length_histogram2.png"),
#        width = 7,
#        height = 4)

# 箱ひげ図 --------------------------------------------------------------------
quantile(df_ssdse_b2019$`総人口`)

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
d <- 
  df_animal |> 
  filter(!is.na(body_length_cm), !is.na(weight_kg)) |> 
  select(body_length_cm, weight_kg)

d |> 
  mutate(across(everything(),.fns = mean, .names = "{.col}_mean")) |> 
  rowwise() |> 
  mutate(body_length_cm_deviation = body_length_cm - body_length_cm_mean,
         weight_kg_deviation = weight_kg - weight_kg_mean) |> 
  mutate(deviation_cross = body_length_cm_deviation * weight_kg_deviation) |> 
  ungroup() |> 
  pull(deviation_cross) |> 
  sum() / (nrow(d) - 1)
# Rの標準関数で共分散を求めるとデータの数 - 1で割る不偏共分散になります
cov(df_animal$body_length_cm,
    df_animal$weight_kg,
    use = "complete.obs")

# 共分散の性質... 変数の単位に依存して値が変わる
cov(
  # cm を m に
  set_units(set_units(df_animal$body_length_cm, cm), m),
  df_animal$weight_kg,
  use = "complete.obs")
cov(
  # cm を m に
  set_units(set_units(df_animal$body_length_cm, cm), m),
  # kg を g に
  set_units(set_units(df_animal$weight_kg, kg), g),
    use = "complete.obs")

# 相関係数 --------------------------------------------------------------------
# 2つの変数の関係を散布図で表す
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

# 気温とアイスの相関係数を求める
cor(df_icecream_temperature$temperature_average_c,
    df_icecream_temperature$value)
# 相関係数は外れ値の影響を受けやすい
cor(df_animal$body_length_cm,
    df_animal$weight_kg,
    use = "complete.obs")
cor(df_animal |> 
      filter(!heavy_mt_200kg) |> 
      pull(body_length_cm),
    df_animal |> 
      filter(!heavy_mt_200kg) |> 
      pull(weight_kg),
    use = "complete.obs")


# 見せかけの相関（疑似相関） -----------------------------------------------------------
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


# 相関係数行列 ------------------------------------------------------------------
# 変数のペアごとに計算した相関係数を行列形式で表現します
# 対角成分は、自分自身との相関に対応するので値は1になる
# corrr::correlate()関数では対角成分はNAで出力されますが、引数diagonalで任意の値（1）に変更できます
corrr::correlate(df_pesticide_ice, diagonal = 1.0)
# visdatパッケージを使った相関係数行列の視覚化
vis_cor(df_pesticide_ice[, 2:4])


# クロス集計表 ------------------------------------------------------------------
xtabs(~ taxon + heavy_mt_200kg, 
      data = df_animal)
df_animal |> 
  janitor::tabyl(taxon, heavy_mt_200kg)



# アンスコムの例 -----------------------------------------------------------------
anscombe # Rの組み込みパッケージ datasets によりアンスコムの例のデータが利用できます
anscombe_long # アンスコムデータを縦長の形にしたもの
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


# 棒グラフ --------------------------------------------------------------------
# 縦にする、軸にだまされない

# 散布図 ---------------------------------------------------------------------


