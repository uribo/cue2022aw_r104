library(dplyr)
library(ggplot2)
library(ggrepel)
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
# webshot2::webshot("https://www.nstac.go.jp/use/literacy/ssdse/",
#                   file = here::here("images/ssdse.png"),
#                   cliprect = "viewport")

