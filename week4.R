################################
# 第四週: データサイエンス入門(2)
################################
library(tidyverse) # データ分析全般
library(here) # フォルダ、ファイルの指定を簡易に
library(ssdse)
course_colors <- c("#364968", "#fddf97", "#e09664", "#6c4343", "#ffffff")
theme_set(theme_bw()) # ggplot2のテーマを指定


# データの用意 ------------------------------------------------------------------
source(here("_pins.R"))
df_ssdse_c <- 
  pins_resources_online |> 
  pins::pin_download("ssdse_c") |> 
  read_ssdse_c(lang = "ja")

# クラスタリング -----------------------------------------------------------------



# 因子分析 --------------------------------------------------------------------
d <- 
  df_ssdse_c |> 
  select(10, 11, 12, 13, 14, 15, 16)

fa_res <-
  factanal(d, factors = 2, rotation = "promax")

fa_res$correlation


# 主成分分析 -------------------------------------------------------------------
# 主成分軸
# 寄与率（80%以上が目安）
colnames(df_ssdse_c)[c(10, 11, 12, 13, 14, 15, 16)]
colnames(df_ssdse_c)
colnames(df_ssdse_c)[c(21, 24, 28, 33, 34, 40, 43)]

df_ssdse_c <- 
  df_ssdse_c |> 
  select(2, 10, 11, 12, 13, 14, 15, 16)

# df_ssdse_c <- 
#   df_ssdse_c |>
#   select(`都道府県`, matches("^06_果物.+")) |> 
#   select(!`06_果物果物加工品`)

df_ssdse_c <-
  df_ssdse_c |>
  purrr::set_names(names(df_ssdse_c) |>
                     stringr::str_remove(".+_(魚介類|穀類)"))

df_ssdse_c <- 
  tibble::column_to_rownames(df_ssdse_c, "都道府県")

pca_res <-
  prcomp(df_ssdse_c)
# 第二種成分軸までで元データの分散のおよそ70%を説明
summary(pca_res)
plot(pca_res)
plot(pca_res, type = "l")
# 第1主成分得点が最も高いのは山形市（山形県）
# 高松市（香川県）は第1主成分得点、第2主成分得点ともに高い
# 高松市（香川県）は「生うどん・そば」に対する支出が特徴的
# 山形市（山形県）は麺類全般に対して支出している
biplot(pca_res)

# pca_res$sdev
# pca_res$rotation |> 
#   as.data.frame() |> 
#   tibble::rownames_to_column(var = "都道府県") |> 
#   tibble::as_tibble()

library(ggplot2)
library(ggrepel)
pca_score <- 
  pca_res$x[, 1:2] # 第一、第二主成分得点

pca_score <- 
  pca_score |> 
  tibble::as_tibble() |> 
  mutate(prefecture = rownames(df_ssdse_c)) |> 
  relocate(prefecture, .before = 1)

pca_score |> 
  ggplot() +
  aes(PC1, PC2) +
  geom_point() +
  geom_text_repel(aes(label = prefecture))


pca_res$rotation
# 生うどん・そば、乾うどん・そばの値が大きくなると第一、第二主成分得点が小さくなる
round(pca_res$rotation, digits = 2)

pca_res$x[1, 1]
# sum(df_ssdse_c[1, ] * pca_res$rotation[, 1])

# pca_res <- 
#   prcomp(df_ssdse_c, center = TRUE, scale. = TRUE)
# summary(pca_res)
# plot(pca_res)
# biplot(pca_res)

