################################
# 第三週: データ分析入門（1）
################################
library(tidyverse)
library(patchwork)
library(here)
library(broom)
library(ggpmisc)
library(tidymodels)
course_colors <- c("#364968", "#fddf97", "#e09664", "#6c4343", "#ffffff")
theme_set(theme_bw())

# データの用意 ------------------------------------------------------------------
source(here("_pins.R")) # pins_resources_online, pins_resources_local
df_animal <- 
  pins_resources_online |>
  pins::pin_download("tokushima_zoo_animals22") |> 
  readr::read_csv(col_types = "ccdd")

df_ice_weather <-
  pins_resources_local |> 
  pins::pin_read("tksm_sales_weather") |> 
  select(!c(pesticide, rice))

df_ssdse_b <- 
  pins_resources_online |> 
  pins::pin_download("ssdse_b") |> 
  read_ssdse_b(lang = "ja") |> 
  filter(`年度` == 2019) |> 
  select(`都道府県`, `教育`, `家計`) |> 
  tidyr::unnest(cols = c(`教育`, `家計`)) |>
  select(ends_with("教員数"), `教育費（二人以上の世帯）`)

# 単回帰モデル ------------------------------------------------------------------
# y = a + bx + e
# a ... 切片
# b ... 傾き
# e ... 誤差（残差、実測値 - 予測値）
lm_res <- 
  lm(ice ~ temperature_average_c, data = df_ice_weather)
summary(lm_res)
# 回帰式、残差の情報、切片・傾きの推定値と検定結果、決定係数、調整済み決定係数
tidy(lm_res) # 気温が高くなることでアイスクリーム・シャーベットの売り上げが増加する
# y^ = 5.46 + 16.0 temperature_average_c

# 回帰係数
coef(lm_res)

# 残差 ... 予測値からのズレ
# summary(lm_res$residuals)
lm_res$residuals[1]
df_ice_weather$ice[1] - ((5.46 + 16.0 * df_ice_weather$temperature_average_c[1]))
lm_res$residuals[10]
df_ice_weather$ice[10] - ((5.46 + 16.0 * df_ice_weather$temperature_average_c[10]))
all.equal(
  df_ice_weather$ice - predict(lm_res),
  lm_res$residuals)



# 残差平方和
sum(lm_res$residuals^2)
deviance(lm_res)

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

# 回帰直線の係数から目的変数の値（yハット)を推定
coefficients(lm_res)[[1]] + coefficients(lm_res)[[2]] * df_ice_weather$temperature_average_c[1]
y_hat <- coefficients(lm_res)[[1]] + coefficients(lm_res)[[2]] * df_ice_weather$temperature_average_c
df_ice_weather$ice[1]　- y_hat[1]
# 残差 (実際に得られた目的変数の値とyハットとの差)
df_ice_weather$ice - y_hat
# lm()関数の結果にresiduals(残差)として記録
unname(lm_res$residuals)

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


# 決定係数 R^2 --------------------------------------------------------------------
# 1に近いほど、回帰式が実際のデータに当てはまっていることを表す
# 1 - 残差平方和 / 全平方和
1 - (deviance(lm_res) / sum((df_ice_weather$ice - mean(df_ice_weather$ice))^2))
summary(lm_res)$r.squared
cor(df_ice_weather$ice, df_ice_weather$temperature_average_c)^2

1 - (sum((df_ice_weather$ice - (df_resample_fit$estimate[3] + df_resample_fit$estimate[4] * df_ice_weather$temperature_average_c))^2) / sum((df_ice_weather$ice - mean(df_ice_weather$ice))^2))
1 - (sum((df_ice_weather$ice - (df_resample_fit$estimate[5] + df_resample_fit$estimate[6] * df_ice_weather$temperature_average_c))^2) / sum((df_ice_weather$ice - mean(df_ice_weather$ice))^2))




# 標準化 ---------------------------------------------------------------------
mean(df_animal$body_length_cm, na.rm = TRUE)
sd(df_animal$body_length_cm, na.rm = TRUE)
mean(df_animal$weight_kg, na.rm = TRUE)
sd(df_animal$weight_kg, na.rm = TRUE)

x <- 
  scale(df_animal$body_length_cm)
round(mean(x, na.rm = TRUE)) # 平均 0
sd(x, na.rm = TRUE) # 標準偏差 1

df_animal_scaled <- 
  df_animal |> 
  mutate(across(.cols = c(body_length_cm, weight_kg), .fns = ~ c(scale(.x))))

df_animal_scaled |> 
  summarise(across(.cols = c(body_length_cm, weight_kg), .fns = list(mean = ~ round(mean(.x, na.rm = TRUE), digits = 0),
                                                                     sd = ~ sd(.x, na.rm = TRUE))))

# 重回帰モデル ---------------------------------------------------------------------
lm_res <- 
  lm(ice ~ precipitation_sum_mm + temperature_average_c, data = df_ice_weather)
tidy(lm_res) # 降水量よりも気温の効果が大きい

# 標準化したデータを使う
df_ice_weather_scaled <- 
  df_ice_weather |> 
  mutate(across(.cols = c(precipitation_sum_mm, temperature_average_c), .fns = ~ c(scale(.x))))
lm_res_scaled <- 
  lm(ice ~ precipitation_sum_mm + temperature_average_c, data = df_ice_weather_scaled)
tidy(lm_res_scaled)

p1 <- 
  ggplot(df_ice_weather) +
  aes(precipitation_sum_mm, ice) +
  geom_point()
p2 <- 
  ggplot(df_ice_weather) +
  aes(temperature_average_c, ice) +
  geom_point()

p1 + p2 +
  plot_layout(ncol = 2)

df_ice_weather_scaled |> 
  pivot_longer(cols = c(precipitation_sum_mm, temperature_average_c)) |> 
  ggplot() +
  aes(value, ice, color = name) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = course_colors[1:2]) +
  theme(legend.position = "top")


# 多重共線性 -------------------------------------------------------------------
# car::vif(lm_res)
df_ssdse_b <- 
  df_ssdse_b |> 
  purrr::set_names(c("a", "b", "c", "d", "e", "f", "y"))

lm_res <- 
  lm(y ~ ., data = df_ssdse_b)
tidy(lm_res)
car::vif(lm_res)
# stats::step(lm_res)

corrr::correlate(df_ssdse_b, diagonal = 1.0)

lm_res <- 
  lm(y ~ a + e + f, data = df_ssdse_b)
tidy(lm_res)

ms1 <- 
  MuMIn::dredge(lm_res)
MuMIn::sw(ms1)

# ロジスティック回帰（線形分類） ---------------------------------------------------------------
# lr ... logistic regression
lr_spec <- 
  logistic_reg(mode = "classification",
                      engine = "glm")
data("credit_data", package = "modeldata")
lr_spec |> 
  fit(Status ~ Seniority + Age, data = credit_data) |> 
  tidy()

data("two_class_dat", package = "modeldata")
lr_fitted <- 
  lr_spec |> 
  fit(Class ~ A + B, data = two_class_dat)

lr_fitted |> 
  tidy()

predict(lr_fitted, new_data = two_class_dat)
two_class_dat |> 
  ggplot() +
  aes(A, B) + 
  geom_point(aes(color = Class)) +
  scale_color_manual(values = course_colors[1:2]) +
  theme(legend.position = "top")

augment(lr_fitted, new_data = two_class_dat) |> 
  roc_auc(Class, .pred_Class1)
# 正確度、kappa
augment(lr_fitted, new_data = two_class_dat) |> 
  metrics(Class, .pred_class)
# 適合度
augment(lr_fitted, new_data = two_class_dat) |> 
  precision(Class, .pred_class)
augment(lr_fitted, new_data = two_class_dat) |> 
  recall(Class, .pred_class)

multi_metric <- 
  metric_set(accuracy, precision, recall)
augment(lr_fitted, new_data = two_class_dat) |> 
  multi_metric(truth = Class, estimate = .pred_class)

# 混同行列
augment(lr_fitted, new_data = two_class_dat) |> 
  conf_mat(truth = Class, estimate = .pred_class) |> 
  autoplot(type = "heatmap")

# サポートベクトルマシン（非線形分類） -------------------------------------------------------------
library(LiblineaR)
svm_spec <- 
  svm_linear(mode = "classification",
                    engine = "LiblineaR")

svm_fitted <- 
  svm_spec |> 
  fit(Class ~ A + B, data = two_class_dat)

tidy(svm_fitted)

augment(svm_fitted, new_data = two_class_dat) |> 
  metrics(Class, .pred_class)
