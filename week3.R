################################
# 第三週: データ分析入門（1）
################################
library(tidyverse) # データ分析全般
library(patchwork) # ggplot2で作成した図のレイアウトを調整
library(here) # フォルダ、ファイルの指定を簡易に
library(ggpmisc)
library(tidymodels) # モデリング全般
library(broom)
course_colors <- c("#364968", "#fddf97", "#e09664", "#6c4343", "#ffffff")
theme_set(theme_bw()) # ggplot2のテーマを指定

# データの用意 ------------------------------------------------------------------
source(here("_pins.R")) # pins_resources_online, pins_resources_local
df_ice_weather <-
  pins_resources_local |> 
  pins::pin_read("tksm_sales_weather") |> 
  select(!c(pesticide, rice))

# 都道府県別の教育費と各教育機関（幼稚園から大学）での人口1万人あたり教員数
df_ssdse_b2019 <- 
  pins_resources_online |> 
  pins::pin_download("ssdse_b") |> 
  ssdse::read_ssdse_b(lang = "ja") |> 
  filter(`年度` == 2019) |> 
  select(`都道府県`, `人口・世帯`, `教育`, `家計`) |> 
  tidyr::unnest(cols = c(`人口・世帯`, `教育`, `家計`)) |>
  # ends_with("園数"), matches("(小|中|高等)学校数"), `大学数`
  select(`都道府県`, `総人口`, ends_with("員数"), `教育費（二人以上の世帯）`) |> 
  select(!`短期大学教員数`) |> 
  mutate(across(.cols = c(ends_with("数")), 
                .fns = ~  (.x / `総人口`) * 10000, 
                .names = "{.col}_人口1万人あたり")) |> 
  select(!c(`都道府県`, `総人口`, ends_with("数"))) |> 
  purrr::set_names(c("kyouikuhi", "you", "syou", "tyu", "kou", "dai"))

# 単回帰モデル ------------------------------------------------------------------
# y = a + bx + e
# a ... 切片
# b ... 傾き
# e ... 誤差（残差、実測値 - 予測値）
# 線形回帰モデルの当てはめは lm()関数で行います
lm_res <- 
  # 「アイスの売り上げ = 切片 + 気温と気温の傾き」のモデルを考える
  lm(ice ~ temperature_average_c, data = df_ice_weather)
summary(lm_res)
# 回帰式、残差の情報、切片・傾きの推定値と検定結果、決定係数、調整済み決定係数
tidy(lm_res) # 気温が高くなることでアイスクリーム・シャーベットの売り上げが増加する
# y^ = 5.46 + 16.0 temperature_average_c

# 回帰係数を直接得る
coef(lm_res)

# 残差 ... 予測値からのズレ
# summary(lm_res$residuals)
# y = ax+bを考えるとき、残差は y - (a + b * x) で求められます
# 回帰直線の係数から目的変数の値（yハット)を推定
df_ice_weather$ice[1] - ((coef(lm_res)[[1]] + coef(lm_res)[[2]] * df_ice_weather$temperature_average_c[1]))
# 同じ値はlm()関数の実行結果に residuals として記録されています
lm_res$residuals[1]
lm_res$residuals[10]
df_ice_weather$ice[10] - ((coef(lm_res)[[1]] + coef(lm_res)[[2]] * df_ice_weather$temperature_average_c[10]))
all.equal(
  # 残差 (実際に得られた目的変数の値とyハットとの差)
  df_ice_weather$ice - predict(lm_res),
  lm_res$residuals)
# すべての y に対する残差... lm()関数の結果にresiduals(残差)として記録
lm_res$residuals

# 残差平方和
sum(lm_res$residuals^2)
deviance(lm_res)

# 回帰式には必ず誤差が含まれる
# 観測値 y も x のの値、モデルの係数（切片と傾き）と誤差項により得られる
all.equal(
  df_ice_weather$ice[1],
  unname(coef(lm_res)[[1]] + coef(lm_res)[[2]] * df_ice_weather$temperature_average_c[1] + lm_res$residuals[1]))



# 回帰診断図
# plot(lm_res)


# 回帰直線
df_ice_weather |> 
  ggplot() +
  aes(temperature_average_c, ice) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = course_colors[1]) +
  stat_poly_eq(use_label(c("eq"))) +
  xlab("月平均気温(\u2103)") +
  ylab("アイスクリーム・シャーベットの100世帯当たり1か月間の購入頻度") +
  labs(title = "気温とアイスクリーム・シャーベットの売上の関係",
       caption = "データ: 気象庁 過去の気象データおよび
       「家計調査」表番号4-1
       1世帯当たり1か月間の支出金額，購入数量及び平均価格 都市階級・地方・都道府県庁所在市別") +
  theme(title = element_text(size = 6))
# ggsave(here("images/気温_アイスクリーム1世帯当たり1か月間の購入数量.png"),
#        width = 5,
#        height = 4)



# 最小二乗法 -------------------------------------------------------------------
# 実際の目的変数の値は、説明変数の値に対してさまざまな値を取り得る
# ... 残差平方和の最も少ない回帰式を求める
# 最小二乗法により、残差平方和が最小となる定数項（傾きと切片）を求める
# 最も当てはまりのよい直線
# いくつかの回帰直線を考える
library(infer)
set.seed(20221124)
df_resample_fit <-
  x <-
  df_ice_weather |>
  specify(ice ~ temperature_average_c) |>
  hypothesise(null = "independence") |>
  generate(reps = 5, type = "permute") |>
  fit()
# 残差
all.equal(
  unname(lm_res$residuals),
  df_ice_weather$ice - (coefficients(lm_res)[[1]] + coefficients(lm_res)[[2]] * df_ice_weather$temperature_average_c)
)
# 残差平方和
all.equal(
  deviance(lm_res),
  sum((df_ice_weather$ice - (coefficients(lm_res)[[1]] + coefficients(lm_res)[[2]] * df_ice_weather$temperature_average_c))^2)  
)
# 残差平方和が最小となる係数を回帰直線の係数とする
which.min(
  c(deviance(lm_res),
    sum((df_ice_weather$ice - (df_resample_fit$estimate[3] + df_resample_fit$estimate[4] * df_ice_weather$temperature_average_c))^2),
    sum((df_ice_weather$ice - (df_resample_fit$estimate[5] + df_resample_fit$estimate[6] * df_ice_weather$temperature_average_c))^2),
    sum((df_ice_weather$ice - (df_resample_fit$estimate[7] + df_resample_fit$estimate[8] * df_ice_weather$temperature_average_c))^2)
  )
)

# 正規方程式を解くことにより求まる
value_b <- 
  df_ice_weather %>% 
  transmute(x = temperature_average_c - mean(temperature_average_c, na.rm = TRUE), 
            y = ice - mean(ice)) %>% 
  mutate(xy = x * y) %>% 
  summarise(SP_xy = sum(xy), 
            SS_x = sum(x^2)) %>% 
  summarise(b = SP_xy / SS_x) |> 
  pull(b)
value_a <- 
  mean(df_ice_weather$ice) - value_b * mean(df_ice_weather$temperature_average_c, na.rm = TRUE)
glue::glue("傾き = {value_b}")
glue::glue("切片 = {value_a}")

mean(lm_res$residuals^2) # 平均二乗残差


# 残差標準誤差 ------------------------------------------------------------------
# yの値が回帰直線から平均してどの程度離れているか（モデルの当てはまりの悪さ）の指標として利用する
sqrt(deviance(lm_res) / (length(lm_res$residuals) - (1 + length(lm_res$coefficients) - 1)))
# 同じ値はsummary(lm())$sigmaで参照できる
summary(lm_res)$sigma


# 決定係数 R^2 --------------------------------------------------------------------
# 1に近いほど、回帰式が実際のデータに当てはまっていることを表す
# 1 - 残差平方和 / 全平方和
1 - (deviance(lm_res) / sum((df_ice_weather$ice - mean(df_ice_weather$ice))^2))
# lm()関数の結果に対してsummary()を適用し、r.squared で参照する
summary(lm_res)$r.squared
# 2変数間の相関係数の二乗と一致する
cor(df_ice_weather$ice, df_ice_weather$temperature_average_c)^2

# 1 - (sum((df_ice_weather$ice - (df_resample_fit$estimate[3] + df_resample_fit$estimate[4] * df_ice_weather$temperature_average_c))^2) / sum((df_ice_weather$ice - mean(df_ice_weather$ice))^2))
# 1 - (sum((df_ice_weather$ice - (df_resample_fit$estimate[5] + df_resample_fit$estimate[6] * df_ice_weather$temperature_average_c))^2) / sum((df_ice_weather$ice - mean(df_ice_weather$ice))^2))

# 重回帰モデル ---------------------------------------------------------------------
# 「アイスの売り上げ = 気温 + 降水量」のモデルを考える
lm_res <- 
  lm(ice ~ temperature_average_c + precipitation_sum_mm, data = df_ice_weather)
tidy(lm_res) # 降水量よりも気温の効果が大きい、降水量の効果は有意ではない

# 多重共線性 -------------------------------------------------------------------
# Rのモデル式の指定では、 . を使うと目的変数に与えた変数以外のすべての変数を指定することになります
lm_full_res <- 
  lm(kyouikuhi ~ ., data = df_ssdse_b2019)
tidy(lm_full_res)
# 中学校教員数は小学校教員数、高校教員数と強い相関関係をもつ
corrr::autoplot(
  corrr::correlate(df_ssdse_b2019))
# ggsave(here("images/都道府県別での教育費と人口1万人あたり各教育機関の教員数の相関.png"),
#        width = 5,
#        height = 4)
# 分散拡大要因 VIF を求める
# 中学校教員数_人口1万人あたりが VIF 10を超える
car::vif(lm_full_res)
# 特定の変数のVIF ... 対象の説明変数を目的変数として、ほかのすべての説明変数で予測する回帰モデルを作成し、その決定係数を1から引いて分母とする
1 / (1- summary(lm(tyu ~ you + syou + kou + dai, data = df_ssdse_b2019))$r.squared)

# 中学校教員数、高校教員数を除いたモデルを検討
lm_res <- 
  lm(kyouikuhi ~ you + syou + dai, data = df_ssdse_b2019)
# 小学校教員数の係数が有意 p < 0.05 となった (フルモデルでは p = 0.495)
# 小学校の教員数が減ると教育費は上がる？
tidy(lm_res)

# 標準化 ---------------------------------------------------------------------
# 平均0、標準偏差1に変換
# 変換前の平均値と標準偏差を確認
mean(df_ice_weather$precipitation_sum_mm, na.rm = TRUE)
sd(df_ice_weather$precipitation_sum_mm, na.rm = TRUE)
mean(df_ice_weather$ice, na.rm = TRUE)
sd(df_ice_weather$ice, na.rm = TRUE)

x <- 
  scale(df_ice_weather$precipitation_sum_mm)
round(mean(x, na.rm = TRUE)) # 平均 0
sd(x, na.rm = TRUE) # 標準偏差 1

df_ice_weather_scaled <- 
  df_ice_weather |> 
  mutate(across(.cols = c(ice, precipitation_sum_mm, temperature_average_c), .fns = ~ c(scale(.x))))

df_ice_weather_scaled |> 
  summarise(across(.cols = c(ice, precipitation_sum_mm, temperature_average_c), 
                   .fns = list(mean = ~ round(mean(.x, na.rm = TRUE), digits = 0),
                               sd = ~ sd(.x, na.rm = TRUE)))) |> 
  pivot_longer(cols = everything(),
               names_to = c("variable", "stats"),
               names_pattern = "(.*)_(.*)",
               values_to = "value") |> 
  pivot_wider(names_from = stats,
              values_from = value)

# 標準化したデータを使って重回帰モデルを行う
lm_res_scaled <- 
  lm(ice ~ temperature_average_c + precipitation_sum_mm, data = df_ice_weather_scaled)
# 標準化によりすべての係数が同じ物差しとして評価できる（標準偏回帰係数）
tidy(lm_res_scaled)


# データ分割 -------------------------------------------------------------------
# モデルの学習・評価のためにデータを分割する
data("two_class_dat", package = "modeldata")
two_class_split <- 
  initial_split(two_class_dat, strata = "Class")
two_class_split
# 学習データ
train_two_class <-
  training(two_class_split)
# 評価データ
test_two_class <-
  testing(two_class_split)


# ロジスティック回帰 ---------------------------------------------------------------
glm(Class ~ A + B, data = train_two_class, family = binomial) |> 
  broom::tidy()

# lr ... logistic regression
lr_spec <- 
  logistic_reg(mode = "classification",
               engine = "glm")
lr_fitted <- 
  lr_spec |> 
  fit(Class ~ A + B, data = train_two_class)

lr_fitted |> 
  tidy()

predict(lr_fitted, new_data = test_two_class)
two_class_dat |> 
  ggplot() +
  aes(A, B) + 
  geom_point(aes(color = Class)) +
  scale_color_manual(values = course_colors[1:2]) +
  theme(legend.position = "top")
# ggsave(here("images/two_class_classification.png"),
#        width = 5,
#        height = 4)

two_class_dat |> 
  ggplot() + 
  aes(B, as.numeric(Class) - 1) + 
  geom_point() +
  geom_smooth(method = "glm", 
              se = FALSE, 
              method.args = list(family = binomial),
              color = course_colors[1]) +
  ylab("Class")
# ggsave(here("images/two_class_logistic_curve.png"),
#        width = 5,
#        height = 4)


# 分類モデルの評価 ----------------------------------------------------------------
# 混同行列
augment(lr_fitted, new_data = test_two_class) |> 
  conf_mat(truth = Class, estimate = .pred_class) |> 
  autoplot(type = "heatmap")
# ggsave(here("images/logistic_regression_confusion_matrix.png"),
#        width = 5,
#        height = 4)

multi_metric <- 
  metric_set(accuracy, precision, recall)
augment(lr_fitted, new_data = train_two_class) |> 
  multi_metric(truth = Class, estimate = .pred_class)
augment(lr_fitted, new_data = test_two_class) |> 
  multi_metric(truth = Class, estimate = .pred_class)

augment(lr_fitted, new_data = test_two_class) |> 
  roc_auc(Class, .pred_Class1)
# 正確度、kappa
augment(lr_fitted, new_data = test_two_class) |> 
  metrics(Class, .pred_class)
# 適合度
augment(lr_fitted, new_data = test_two_class) |> 
  precision(Class, .pred_class)
augment(lr_fitted, new_data = test_two_class) |> 
  recall(Class, .pred_class)

# サポートベクトルマシン -------------------------------------------------------------
library(LiblineaR)
svm_spec <- 
  svm_linear(mode = "classification",
             engine = "LiblineaR")

svm_fitted <- 
  svm_spec |> 
  fit(Class ~ A + B, data = train_two_class)

tidy(svm_fitted)

augment(svm_fitted, new_data = test_two_class) |> 
  multi_metric(truth = Class, estimate = .pred_class)

augment(svm_fitted, new_data = test_two_class) |> 
  metrics(Class, .pred_class)
