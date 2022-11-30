################################
# 第四週: データサイエンス入門(2)
################################
library(tidyverse) # データ分析全般
library(here) # フォルダ、ファイルの指定を簡易に
library(ssdse)
library(ggrepel)
course_colors <- c("#364968", "#fddf97", "#e09664", "#6c4343", "#ffffff")
theme_set(theme_bw()) # ggplot2のテーマを指定


# データの用意 ------------------------------------------------------------------
source(here("_pins.R"))
df_animal <-
  pins_resources_online |> 
  pins::pin_download("tokushima_zoo_animals22") |> 
  read_csv(col_types = "ccdd")

df_ssdse_c <- 
  pins_resources_online |> 
  pins::pin_download("ssdse_c") |> 
  read_ssdse_c(lang = "ja")

# クラスタリング（k-平均法） -----------------------------------------------------------------
# データの特徴からグループ分けを行いたい
df_animal_tiny |> 
  ggplot() +
  aes(body_length_cm, weight_kg) +
  geom_point()

# クラスタ（集団）の数 k を事前に決定する
df_animal_tiny <- 
  df_animal[, c(3, 4)] |> 
  # 欠損値を含む行を削除
  tidyr::drop_na()
set.seed(123)
cluster_res <-
  df_animal_tiny |> 
  # 変数の標準化
  mutate(across(.cols = c(body_length_cm, weight_kg),
                .fns = ~ c(scale(.x)))) |> 
  stats::kmeans(centers = 4, iter.max = 10)
names(cluster_res)

cluster_res$betweenss/cluster_res$totss
sum(cluster_res$withinss)

df_animal_x <- 
  df_animal |> 
  filter(!is.na(body_length_cm), !is.na(weight_kg)) |> 
  mutate(cluster =  cluster_res$cluster)

df_center <- 
  df_animal_x %>% 
  group_by(cluster) %>%
  summarize(across(.cols = c(body_length_cm, weight_kg),
                   .fns = mean)); df_center

p <- 
  ggplot(df_animal_x, 
         aes(x = body_length_cm, y = weight_kg, colour = factor(cluster))) + 
  geom_point()
# 各クラスタの中心をプロット
p +
  geom_point(data = df_center, 
             aes(x = body_length_cm, y = weight_kg, colour = factor(cluster)), 
             shape = 3,
             size = 3)

# 単位が違う変数、標準化なし
set.seed(123)
cluster_res <- 
  df_animal_tiny |> 
  mutate(body_length_cm = body_length_cm * 10) |> 
  stats::kmeans(centers = 4, iter.max = 10)

df_animal_x <- 
  df_animal |> 
  filter(!is.na(body_length_cm), !is.na(weight_kg)) |> 
  mutate(cluster =  cluster_res$cluster)

df_animal_x %>% 
  group_by(cluster) %>%
  summarize(across(.cols = c(body_length_cm, weight_kg),
                   .fns = mean))

# tidymodelsの枠組み tidyclustパッケージで行う
library(tidymodels)
library(tidyclust)
kmeans_spec <- 
  k_means(num_clusters = 4) |> 
  set_engine("stats")
kmeans_spec_fit <- 
  kmeans_spec |> 
  fit(~ ., data = df_animal_tiny)
kmeans_spec_fit

predict(kmeans_spec_fit,
        df_animal_tiny)
augment(kmeans_spec_fit,
        df_animal_tiny) |> 
  ggplot() + 
  aes(x = body_length_cm, y = weight_kg, colour = .pred_cluster) +
  geom_point()

# augment(kmeans_spec_fit,
#         df_animal_tiny) %>% 
#   group_by(.pred_cluster) %>%
#   summarize(across(.cols = c(body_length_cm, weight_kg),
#                    .fns = mean))

extract_centroids(kmeans_spec_fit)


animal_cv <- 
  vfold_cv(df_animal_tiny, v = 5)
kmeans_spec <- 
  k_means(num_clusters = tune())
animal_rec <- 
  recipe(~ body_length_cm + weight_kg, data = df_animal_tiny)

kmeans_wflow <- 
  workflow(animal_rec, kmeans_spec)
clust_num_grid <- grid_regular(num_clusters(),
                               levels = 10)
res <- tune_cluster(
  kmeans_wflow,
  # 交差検証用データを指定
  resamples = animal_cv,
  grid = clust_num_grid,
  control = control_grid(save_pred = TRUE, extract = identity),
  metrics = cluster_metric_set(sse_within_total, sse_total, sse_ratio))
res
res_metrics <- 
  res |> 
  collect_metrics()
res_metrics

res_metrics |> 
  filter(.metric == "sse_ratio") |> 
  ggplot(aes(x = num_clusters, y = mean)) +
  geom_point() +
  geom_line() +
  ylab("mean WSS/TSS ratio, over 5 folds") +
  xlab("Number of clusters") +
  scale_x_continuous(breaks = 1:10)

# 主成分分析 -------------------------------------------------------------------
# 主成分軸
# # 寄与率（80%以上が目安）
# colnames(df_ssdse_c)[c(10, 11, 12, 13, 14, 15, 16)]
# colnames(df_ssdse_c)
# colnames(df_ssdse_c)[c(21, 24, 28, 33, 34, 40, 43)]

df_ssdse_c_tiny <-
  df_ssdse_c |>
  # 麺類の変数名だけを選択
  select(2, 10, 11, 12, 13, 14, 15, 16) |> 
  filter(都道府県 != "全国")

df_ssdse_c_tiny <- 
  df_ssdse_c_tiny |> 
  tibble::column_to_rownames("都道府県") |> 
  purrr::set_names(names(df_ssdse_c_tiny)[-1] |> 
                     stringr::str_remove("01_穀類"))

pca_res <-
  prcomp(df_ssdse_c_tiny, scale. = TRUE)
# Cumulative Proportion 累積寄与率を確認 ... 第三種成分軸までで元データの分散のおよそ70%を説明
summary(pca_res)
plot(pca_res)
plot(pca_res, type = "l")
# 第1主成分得点が最も高いのは山形県
# 山形県は麺類全般に対して支出している
# 香川県は第2主成分得点が最も低い
# 香川県は「生うどん・そば」に対する支出が特徴的
biplot(pca_res)

# pca_res$sdev
# pca_res$rotation |> 
#   as.data.frame() |> 
#   tibble::rownames_to_column(var = "都道府県") |> 
#   tibble::as_tibble()

pca_score <- 
  pca_res$x[, 1:2] |>  # 第一、第二主成分得点
  tibble::as_tibble() |> 
  mutate(prefecture = rownames(df_ssdse_c_tiny)) |> 
  relocate(prefecture, .before = 1)
pca_score |> 
  slice_max(order_by = PC1,
            n = 5)
pca_score |>
  arrange(desc(PC2)) |> 
  mutate(rank = row_number()) |> 
  filter(prefecture == "香川県")

pca_score |> 
  ggplot() +
  aes(PC1, PC2) +
  geom_point() +
  geom_text_repel(aes(label = prefecture))

pca_res$rotation
# 「生うどん・そば」、「乾うどん・そば」の値が大きくなると第一主成分得点が大きく、第二主成分得点が小さくなる
# 「パスタ」の値が大きくなると第二主成分得点も大きくなる
round(pca_res$rotation, digits = 2)

df_ssdse_c_tiny |> 
  ggplot() +
  aes(`生うどん・そば`, `乾うどん・そば`) +
  geom_point()
df_ssdse_c_tiny |> 
  ggplot() +
  aes(`生うどん・そば`, `即席麺`) +
  geom_point()

pca_res$x[1, 1]
# sum(df_ssdse_c[1, ] * pca_res$rotation[, 1])

# pca_res <- 
#   prcomp(df_ssdse_c, center = TRUE, scale. = TRUE)
# summary(pca_res)
# plot(pca_res)
# biplot(pca_res)
