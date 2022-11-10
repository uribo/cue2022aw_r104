ビジネスに役立つデータ分析（入門編）
=====================

徳島大学 人と地域共創センター2022年秋冬期公開講座
[R104]ビジネスに役立つデータ分析（入門編）の資料置き場です。

> 身近なデータを題材に、データの見方を学びます。データを要約し、グラフで表現することで、意外な特徴を発見し、意思決定につなぐことができるようになります。また必修化された高校情報Iの内容を学ぶこともできます。

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/uribo/cue2022aw_r104/main?urlpath=rstudio)


## 講座の内容

### 第一週: 情報化社会におけるデータの利活用

<p align="center">
<a href="https://github.com/uribo/cue2022aw_r104/blob/main/slide/week1.pdf">スライド</a> |
<a href="https://speakerdeck.com/s_uryu/introduction-to-r">R入門スライド</a> |
<a href="https://github.com/uribo/cue2022aw_r104/blob/main/week1.R">Rスクリプト</a>
</p>

- ビジネスにおけるデータ分析の重要性
    - 情報化社会とこれから
    - ビッグデータとビッグテック
    - ビジネスとデータ分析を紐付ける
- データ分析に取りかかる前に…
    - 変数
    - 表形式のデータ（データフレーム）
- R速習

### 第二週: データを扱うためのリテラシー

- 欠損値と外れ値
- データの要約、代表値
- クロス集計
- 時系列データ
- グラフの作り方、読み方
    - ヒストグラム
    - 箱ヒゲ図
    - 散布図
    - 棒グラフ

### 第三週: データサイエンス入門(1)

- 回帰
    - 売り上げを予測する
- 分類
- 評価指標

### 第四週: データサイエンス入門(2)

- 主成分分析
- クラスタリング
- 時系列データ

### 第五週（最終週）

- （前の週までの積み残し）
- ツールの使い方
- 振り返り

## 実行環境

用意したRスクリプトの実行には2つの方法があります。
一つはウェブブラウザ上で行う方法（binderを利用）、もう一つは手元のコンピューター上で行う方法です。
それぞれの方法を以下で説明します。

### 1. binderを利用

こちらはウェブブラウザを介して実行する、簡単な方法です。

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/uribo/cue2022aw_r104/main?urlpath=rstudio)

こちらのボタンをクリックし、しばらく（数秒〜数十分）待ちます。
画面が切り替わり、RStudio Serverのページが表示されたら準備完了です。
必要なパッケージ、Rスクリプトが用意された状態のため、すぐに講座の内容を試すことができます。

### 2. 手元のコンピューターの利用

RとRStudioがインストールされていることが前提となります。
必要なファイル一式をダウンロードし、手元のRStudioで動かす方法です。

このページ https://github.com/uribo/cue2022aw_r104 の上部にある緑色の"Code"ボタンをクリック、
さらに`Download ZIP`をクリックしてzipファイルをダウンロードします。
zipファイルを展開、`cue2022aw_r104.Rproj`ファイルをダブルクリックするとRStudioが起動します。

この状態ではRスクリプトを動かすためのパッケージがインストールされていないため、
次のコマンドをコンソールで実行します。

```r
install.packages("renv")
renv::restore()
```

パッケージのインストールを促すメッセージが出力されるので、<kbd>y</kbd>をコンソール上で入力します。
パッケージのインストールが完了したら準備完了です。

## データセット

- 【独立行政法人統計センター】SSDSE（教育用標準データセット） https://www.nstac.go.jp/use/literacy/ssdse/

## テーマ

### カラーパレット

```r
scales::show_col(c("#364968", "#fddf97", "#e09664", "#6c4343", "#ffffff"), ncol = 5, borders = TRUE)
```
