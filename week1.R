################################
# 第一週: R言語入門
################################
getwd() # 作業スペースを確認
# setwd(dir = ) # 作業スペースを変更

# Hello World -------------------------------------------------------------
# 1. Rスクリプトを新たに開いて、つぎのコードを記述してみましょう。
# 2. 実行するコードが書かれた行にカーソルを移動し、
#     - `Ctrl` + `Enter` からコンソールで実行しましょう
#     - ソースパネルの右上にある `Run` ボタンを押して実行しましょう
# (実行したい範囲を選択した状態で `Ctrl` + `Enter` を押しても良いです)
# 行頭に # をつけた行や行の途中での # 以降はコメントとして扱われます
# 引用符（" または '）で囲んだ文字は文字列として処理されます
print("Hello World")
print("日本語の入出力を確認します")

# 四則演算 --------------------------------------------------------------------
# 演算の優先順位は数学と同じ
# 括弧内の演算 > かけ算・割り算 > 足し算・引き算
1 + 1
5 - 5
2 * 3
(2 + 4) / 3


# 論理演算 --------------------------------------------------------------------
# Rでは論理値ベクトルとして TRUE（真）、FALSE（偽）、NAがある
9 > 8
1.1 <= 1.0
"apple" == "apple"
"apple" != "banana"

class(c(TRUE, FALSE, NA))


# 変数へのオブジェクトの代入 --------------------------------------------------------------
# Rでは <- （または ->、=）によってオブジェクトの内容を変数に割り当てます
x <- 6
y <- 3
# 変数は環境中に保存され、変数名により参照できます
z <- x + y
z
# 変数に保存された内容は上書きされるまで残ります
x
x <- 9
x
# xの値が変わっても変更前に計算された値には影響ありません
z


# 関数 ----------------------------------------------------------------------
# Rには一般的な数学関数が定義されている
# 関数は 関数名(引数名 = 値) の形で実行する
sqrt(x = 2) # 平方根
sqrt(3)
log(10) # 自然対数
log10(100) # 常用対数（底が10の対数）
exp(1) # 指数関数


# ドキュメント ---------------------------------------------------------------
help("sqrt")
?log


# さまざまな関数 -------------------------------------------------------------------
paste("今日", "の", "天気", "は", "晴れ")
paste("今日", "の", "天気", "は", "晴れ", sep = ",")
paste0("今日", "の", "天気", "は", 
       c("晴れ", "曇り", "雨"))

# 統計センターが提供している教育用標準データセットをダウンロードする処理
# download.file()関数を使う
# この関数は引数にurl、destfileなどをもつ（いずれも値の入力が必須）
# url引数にはダウンロード対象のファイルのURLを文字列として指定する
# destfile引数には保存する際のファイル名（パスを含むことができる）を指定する
download.file(url = "https://www.nstac.go.jp/sys/files/SSDSE-B-2022.csv", 
              # WindowsとUNIXとでパスの記述方法が異なるので注意
              destfile = here::here("data-raw/SSDSE-B-2022.csv"))
download.file(url = "https://www.nstac.go.jp/sys/files/SSDSE-B-2022.xlsx", 
              destfile = here::here("data-raw/SSDSE-B-2022.xlsx"))


# ベクトル --------------------------------------------------------------------
# 順序のある要素の集まりを作成するには c()関数(combineから)を使います
c(1, 3, 5)
# ベクトルの中の要素は同じデータ型となります
c(1, "3", 5) # 数値が文字列に変換される
c(TRUE, 1, FALSE) # 論理値が数値に変換される
c(FALSE, 1, "aa") # 論理値、数値が文字列に変換される


# リスト ---------------------------------------------------------------------
# list()関数によりリストオブジェクトを生成します
# リストは異なるクラスからなるオブジェクトをひとまとめに扱うことができます
x <- 
  list(
  c("a", "b", NA, "d"),
  num = c(3, 1.1, 5),
  data = head(mtcars, 1))
x


# 参照 ----------------------------------------------------------------------
# オブジェクトは [演算子 や [[演算子を使って参照できる
c(1, 3, 5)[2] # ベクトル中の2番目の要素を参照
# 名前付きの数値ベクトルを作成
fruits <- 
  c(apple = 120, banana = 100, grape = 800)
length(fruits) # 要素の数を表示
names(fruits) # 要素に与えられた名前を出力
fruits[1] # 要素の位置を指定して参照する
fruits["grape"] # 要素の名前を指定して参照する
fruits[[1]]  # 値だけを参照する
fruits[["grape"]]

# リストオブジェクトに対する[演算子を使った参照の返り値はリスト
x[1]
x["num"]
# [[演算子での参照により、リストオブジェクトの要素を元のクラスとして出力する
x[[2]]
x[["data"]]


# パッケージの利用 ----------------------------------------------------------------
# 組み込みパッケージの確認
rownames(installed.packages(priority = "base"))

# 1. CRANに登録されているパッケージのインストール
# RStudioでは`tab` の入力によってinstall.packages()関数の中でのパッケージ名の補完を行う
install.packages("here")
install.packages("remotes")
# 2. GitHub上のパッケージの追加 <user名>/<repository名>
remotes::install_github("uribo/ssdse")

# パッケージの読み込み
here() # パッケージを読み込んでいないと関数を利用できない
library(here) # hereパッケージを利用可能にする
here()
# library()関数でパッケージを読み込まずにパッケージの関数を利用することもできる
# 名前空間（パッケージ名と関数名）を<パッケージ名>::<関数名>の形で指定し、関数を直接実行する方法
here::here()


# tidyverse ---------------------------------------------------------------
install.packages("tidyverse")
# パッケージを読み込むといくつかのパッケージが同時に利用可能になる
library(tidyverse)


# 表形式のデータ（データフレーム） --------------------------------------------------------
# データフレームの作成方法はいくつかあります
# ここではtibble::tibble()関数を使う方法を紹介します
# tibbleパッケージはtidyverseに含まれるので、個別にパッケージを読み込む必要はありません
# library(tibble)
# dfはdata frameを表します。dataやd、好きな名前にしてもよいですが、中身が推測しやすいものにしましょう
df_zoo <-
  tibble(
    # 変数 = 値の形式でデータを記述します。
    # 複数の値を扱うときは c()関数でベクトルを作ります
    taxon = c("食肉類", "霊長類", "霊長類", "食肉類", "鳥類"),
    name = c("レッサーパンダ", "チンパンジー", "マントヒヒ", "ライオン", "フンボルトペンギン"),
    body_length_cm = c(63.5, 85, 80, 250, 69),
    weight_kg = c(6, 60, 20, 225, 6))
# 出力を確認しましょう
# 一行目はデータフレームのサイズ（行と列の数）を示しています
# 二行目は変数の名前、三行目は各変数のデータ型、三行目以降にデータを表示します
df_zoo

# write_csv(df_zoo,
#           here::here("data-raw/tokushima_zoo_animals5.csv"))


# データフレーム中の要素の参照 ----------------------------------------------------------
# $演算子、[[演算子を使った参照の返り値はベクトル
df_zoo$name
df_zoo[[1]]
df_zoo[["name"]]
# [演算子を使った参照では [行, 列]の形式となる
# 返り値はデータフレーム
df_zoo[2, ] # 2行目を表示
df_zoo[, 3] # 3列目を表示
df_zoo[, "name"] # name列を表示



# 多様な表形式ファイルの読み込み ---------------------------------------------------------
# readrパッケージはtidyverseに含まれるので、個別にパッケージを読み込む必要はありません
# library(readr)
read_csv(file = here("data-raw/tokushima_zoo_animals5.csv"))
# ファイルによっては文字化けを起こすことがあります
read_csv(here("data-raw/SSDSE-B-2022.csv"),
         show_col_types = FALSE)
read_csv(here("data-raw/SSDSE-B-2022.csv"), 
         show_col_types = FALSE,
         locale = locale(encoding = "cp932"))


# 表計算ソフトからの読み込み -----------------------------------------------------------
library(readxl)
read_xlsx(here("data-raw/SSDSE-B-2022.xlsx"),
          sheet = 1)


# 個別のデータ読み込みに特化したパッケージの利用 -------------------------------------------------
library(ssdse)
read_ssdse_b(here("data-raw/SSDSE-B-2022.csv"),
             lang = "ja")

# SSDSE-B-2022.xlsxを表計算ソフトで開き、適当な範囲を選択してコピーする
# Rのコンソールで次のコマンドを実行
# read.table("clipboard") # Windows
# read.table(pipe("pbpaste"), header = TRUE) # macOS, Linux
# 出力を再現するための生成コマンドを出力
# dput(.Last.value)
# structure(
#   list(
#     V1 = 2019:2016,
#     V2 = c("R01000", "R01000", "R01000",
#            "R01000"),
#     V3 = c("北海道", "北海道", "北海道", "北海道"),
#     V4 = c(5250000L, 5286000L, 5320000L, 5352000L),
#     V5 = c(2472000L,
#            2489000L, 2506000L, 2521000L),
#     V6 = c(2778000L, 2797000L, 2814000L,
#            2830000L),
#     V7 = c(5211000L, 5253000L, 5292000L, 5327000L),
#     V8 = c(2455000L,
#            2475000L, 2494000L, 2511000L),
#     V9 = c(2756000L, 2778000L, 2797000L,
#            2816000L),
#     V10 = c(565000L, 577000L, 588000L, 600000L),
#     V11 = c(289000L,
#             295000L, 301000L, 306000L)
#   ),
#   class = "data.frame",
#   row.names = c(NA,-4L)
# )

# データ可視化 ------------------------------------------------------------------
library(ragg)
agg_png(here("images/barplot.png"), width = 800, pointsize = 16)
barplot(df_zoo$body_length_cm,
        names.arg = df_zoo$name)
dev.off()

agg_png(here("images/scatterplot.png"), width = 800, pointsize = 16)
plot(df_zoo$body_length_cm,
     df_zoo$weight_kg)
dev.off()

agg_png(here("images/histogram.png"), width = 800, pointsize = 16)
hist(df_zoo$weight_kg)
dev.off()


# Rでの可視化を簡単に --------------------------------------------------------------
theme_set(theme_bw(base_size = 16))
ggplot(df_zoo,
       aes(name, body_length_cm)) +
  geom_bar(stat = "identity")
ggsave(here("images/barplot_ggplot2.png"),
       width = 8,
       height = 6)

ggplot(df_zoo,
       aes(body_length_cm, weight_kg)) +
  geom_point()
ggsave(here("images/scatterplot_ggplot2.png"),
       width = 8,
       height = 6)

ggplot(df_zoo,
       aes(weight_kg)) +
  geom_histogram(bins = 5)
ggsave(here("images/histogram_ggplot2.png"),
       width = 8,
       height = 6)



# Rにおける連続した処理の記述 ----------------------------------------------------------
x |> f()
# 1. 処理ごとにオブジェクトへ保存する
r <- rnorm(100)
d <- matrix(r, ncol = 2)
plot(d)
# 2. 処理内容を入れ子式に記述する
plot(
  matrix(
    rnorm(100), 
    ncol = 2))
# 3. パイプ演算子を使う
rnorm(100) |> 
  matrix(ncol = 2) |> 
  plot()


