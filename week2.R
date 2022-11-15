################################
# 第二週: データを扱うためのリテラシー
################################
library(tidyverse)
library(here)
library(patchwork)
library(ssdse)
course_colors <- c("#364968", "#fddf97", "#e09664", "#6c4343", "#ffffff")

df_animal <-
  read_csv(here("data-raw/tokushima_zoo_animals5.csv"),
           col_types = "ccdi")
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



# 外れ値 ---------------------------------------------------------------------
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

set.seed(123)
df_corr <- 
  tibble::tibble(
    x = rnorm(100),
    y = rnorm(100),
    y_positive = 5 * x + rnorm(100, sd = 3),
    y_negative = -5 * x + rnorm(100, sd = 4))
p1 <- 
  df_corr |> 
  ggplot(aes(x, y_positive)) +
  geom_point() +
  ylab("y") +
  labs(title = "正の相関関係")
p2 <- 
  df_corr |>  
  ggplot(aes(x, y)) +
  geom_point() +
  labs(title = "無相関")
p3 <- 
  df_corr |> 
  ggplot(aes(x, y_negative)) +
  geom_point() +
  ylab("y") +
  labs(title = "負の相関関係")
p1 + p2 + p3 + 
  plot_layout(ncol = 3)



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


# アンスコムサウルス -------------------------------------------------------------------
library(datasauRus)
datasaurus_dozen |> 
  filter(dataset == "dino") |> 
  ggplot(aes(x = x, y = y)) +
  geom_point()
datasaurus_dozen |> 
  filter(dataset != "dino") |> 
  ggplot(aes(x = x, y = y, colour = dataset)) +
  geom_point() +
  theme(legend.position = "none") +
  facet_wrap(~dataset, ncol = 3)
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


