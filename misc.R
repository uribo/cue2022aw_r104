library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
course_colors <- c("#364968", "#fddf97", "#e09664", "#6c4343", "#ffffff")
theme_set(theme_bw())
# 【総務省】我が国のインターネットにおけるトラヒックの集計・試算 -----------------------------------------
# https://www.soumu.go.jp/joho_tsusin/eidsystem/market01_05_03.html
df_traffic <- 
  tibble::tibble(
  year = rep(seq.int(2017, 2022), each = 2),
  month = rep(c(5, 11), times = 6),
  upload = c(1406, 1160, 1309, 1401, 1563, 1571, 2321, 2373, 2781, 2816, 3088, NA_real_),
  download = c(8027, 8903, 10289, 10976, 12086, 12650, 19025, 19821, 23899, 23650, 25993, NA_real_)) |> 
  tidyr::pivot_longer(cols = c(upload, download),
                      names_to = "type",
                      values_to = "traffic") |> 
  transmute(ymd = lubridate::make_date(year, month),
            type,
            traffic)

df_traffic |> 
  ggplot() +
  aes(ymd, traffic, color = type, group = type) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  geom_text_repel(data = subset(df_traffic, ymd == "2022-05-01"),
                  aes(label = type),
                  show.legend = FALSE) +
  scale_color_manual(values = c("#364968", "#e09664")) +
  scale_x_date(date_labels = "%b-%Y") +
  scale_y_continuous(labels = scales::comma) +
  xlab(NULL) +
  ylab("トラヒック(Gbps)") +
  labs(title = "我が国のブロードバンドサービス契約者の総トラヒック（推定値）",
       caption = "総務省 報道資料より作成\nhttps://www.soumu.go.jp/joho_tsusin/eidsystem/market01_05_03.html")

ggsave(filename = here::here("images/我が国のブロードバンドサービス契約者の総トラヒック_推定量.png"), 
       last_plot(),
       width = 7,
       height = 4)


# RとRStudioのインストール --------------------------------------------------------------------
# webshot2::webshot("https://cran.r-project.org/",
#                   file = here::here("images/cran.png"))
# webshot2::webshot("https://posit.co/download/rstudio-desktop/",
#                   file = here::here("images/rstudio_install.png"),
#                   selector = "section.download-content")
# webshot2::webshot("https://github.com/topics/r",
#                   file = here::here("images/github_topics_r.png"),
#                   cliprect = "viewport")
# webshot2::webshot("https://www.nstac.go.jp/use/literacy/ssdse/",
#                   file = here::here("images/ssdse.png"),
#                   cliprect = "viewport")


# 相関 ----------------------------------------------------------------------
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

ggsave(filename = here::here("images/2つの変数の関係を示す3つの状態.png"), 
       last_plot(),
       width = 7,
       height = 2.6)


# week3 -------------------------------------------------------------------
source(here::here("_pins.R"))
df_ice_weather <-
  pins_resources_local |> 
  pins::pin_read("tksm_sales_weather") |> 
  select(!c(pesticide, rice))
library(infer)
set.seed(20221124)
df_resample_fit <-
  df_ice_weather |>
  specify(ice ~ temperature_average_c) |>
  hypothesise(null = "independence") |>
  generate(reps = 5, type = "permute") |>
  fit()
lm_res <-
  lm(ice ~ temperature_average_c, data = df_ice_weather)
p <-
  df_ice_weather |>
  ggplot() +
  aes(temperature_average_c, ice) +
  geom_point()
p +
  geom_abline(intercept = df_resample_fit$estimate[3],
              slope = df_resample_fit$estimate[4],
              color = course_colors[2]) +
  geom_abline(intercept = df_resample_fit$estimate[5],
              slope = df_resample_fit$estimate[6],
              color = course_colors[3]) +
  geom_abline(intercept = df_resample_fit$estimate[7],
              slope = df_resample_fit$estimate[8],
              color = course_colors[4]) +
  geom_line(stat = "smooth",
            method = lm,
            color = course_colors[1],
            linewidth = 1,
            alpha = 0.5,
            linetype = 2) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = NA)
ggsave(here::here("images/さまざまな回帰直線.png"),
       width = 5,
       height = 4)


library(gifski)
# library(av)
library(gganimate)
renv::settings$ignored.packages(unique(c(renv::settings$ignored.packages(), c("gifski", "gganimate"))))

# 予測値
d <- 
  df_ice_weather |> 
  bind_cols(tibble(
    .pred = predict(lm_res),
    residual = residuals(lm_res))) |> 
  arrange(desc(ice)) |> 
  mutate(ymin = ice - residual,
         time = 1:nrow(df_ice_weather))
# plot(d$temperature_average_c, d$ice)
# abline(lm_res)
# d |> 
#   ggplot() +
#   aes(temperature_average_c, ice) +
#   geom_point() +
#   geom_point(aes(temperature_average_c, .pred - residual), color = course_colors[2])

p <- 
  d |>
  ggplot() +
  aes(temperature_average_c, ice) +
  geom_point(color = course_colors[2]) +
  geom_abline(intercept = coefficients(lm_res)[[1]],
              slope = coefficients(lm_res)[[2]],
              color = course_colors[1]) +
#  geom_smooth(method = "lm", se = FALSE, color = course_colors[1]) +
  # geom_point(data = NULL,
  #            aes(x = temperature_average_c[6],
  #                y = ice[6]),
  #            color = course_colors[2]) +
  geom_linerange(aes(x = temperature_average_c,
                ymin = ymin,
                ymax = ice))

ggsave(here::here("images/回帰直線と残差のプロット.png"),
       p,
       width = 5,
       height = 4)

anim <- 
  p + 
  geom_text(aes(x = temperature_average_c,
                y = if_else(residual > 0,
                            ice + 20,
                            ice - 20),
                label = round(residual, digits = 2)),
            fontface = "plain") +
  transition_states(time, state_length = 3)
# anim
anim_save(here::here("images/残差アニメーション.gif"))

# library(ggpmisc)
# p +
#   stat_fit_deviations(colour = course_colors[2]) +
#   stat_poly_line(method = "lm", se = FALSE, color = course_colors[1])



