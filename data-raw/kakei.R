################################
# 家計調査
# 提供分類: 家計収支編 > 二人以上の世帯 > 詳細結果表
# 提供周期: 月次 (2019年1月〜2021年12月)
# 表番号: 4-1
# 1世帯当たり1か月間の支出金額，購入数量及び平均価格 
## 都市階級・地方・都道府県庁所在市別
### 二人以上の世帯・勤労者世帯・無職世帯
################################
library(dplyr)
library(httr2)
library(estatapi)
library(here)
renv::settings$ignored.packages(unique(c(renv::settings$ignored.packages(), c("estatapi", "httr2"))))
fs::dir_create(here("data-raw/家計調査"))

pins_resources_local <- 
  pins::board_folder(here("data-raw"))

if (pins_resources_local |> 
    pins::pin_list() |> 
    stringr::str_detect("shikoku_kome_sisyutu2019to2021") |> 
    sum() != 1) {
  if (length(fs::dir_ls(here("data-raw/家計調査"), regexp = ".(xls|xlsx)$")) != 36L) {
    
    if (Sys.getenv("ESTAT_TOKEN") == "") {
      Sys.setenv("ESTAT_TOKEN") <- rstudioapi::askForPassword("e-statのtokenを入力してください")
    }
    
    # x <- 
    #   request("https://www.e-stat.go.jp") |> 
    #   req_url_path_append("stat-search") |> 
    #   req_url_path_append("file-download") |> 
    #   req_url_query(statInfId = "000032062550",
    #                 fileKind = "0")
    # res <- 
    #   x |> 
    #   req_perform() |> 
    #   resp_body_raw()
    # readr::write_file(
    #   res,
    #   file = here("data-raw/a401.xlsx"))
    
    # df_kakei_estat2022 <- 
    #   estatapi::estat_getDataCatalog(appId = Sys.getenv("ESTAT_TOKEN"),
    #                                  lang = "J",
    #                                  statsCode = "00200561",
    #                                  surveyYears = "2022",
    #                                  dataType = "XLS", 
    #                                  searchWord = "品目分類 AND 1世帯当たり1か月間の支出金額 AND 都道府県庁所在市別")
    df_kakei_estat2021 <- 
      estatapi::estat_getDataCatalog(appId = Sys.getenv("ESTAT_TOKEN"),
                                     lang = "J",
                                     statsCode = "00200561",
                                     surveyYears = "2021",
                                     dataType = "XLS", 
                                     searchWord = "品目分類 AND 1世帯当たり1か月間の支出金額 AND 都道府県庁所在市別")
    df_kakei_estat2020 <- 
      estatapi::estat_getDataCatalog(appId = Sys.getenv("ESTAT_TOKEN"),
                                     lang = "J",
                                     statsCode = "00200561",
                                     surveyYears = "2020",
                                     dataType = "XLS", 
                                     searchWord = "品目分類 AND 1世帯当たり1か月間の支出金額 AND 都道府県庁所在市別")
    df_kakei_estat2019 <- 
      estatapi::estat_getDataCatalog(appId = Sys.getenv("ESTAT_TOKEN"),
                                     lang = "J",
                                     statsCode = "00200561",
                                     surveyYears = "2019",
                                     dataType = "XLS", 
                                     searchWord = "品目分類 AND 1世帯当たり1か月間の支出金額 AND 都道府県庁所在市別")
    
    download_estat_file <- function(url, file) {
      res <-
        httr2::request(url) |> 
        httr2::req_perform()
      if (res$status_code == 200L) {
        readr::write_file(
          httr2::resp_body_raw(res),
          file = file)
      }
    }
    
    # 2021
    seq.int(nrow(df_kakei_estat2021)) |>
      purrr::walk(
        function(.x) {
          download_estat_file(df_kakei_estat2021$URL[.x],
                              file = here(glue::glue("data-raw/家計調査/4-1_都市階級・地方・都道府県庁所在市別_二人以上の世帯・勤労者世帯・無職世帯_{df_kakei_estat2021$SURVEY_DATE[.x]}.xlsx")))
        }
      )
    # 2020 ... 11, 12月だけxlsx
    seq.int(10) |>
      purrr::walk(
        function(.x) {
          download_estat_file(df_kakei_estat2020$URL[.x],
                              file = here(glue::glue("data-raw/家計調査/4-1_都市階級・地方・都道府県庁所在市別_二人以上の世帯・勤労者世帯・無職世帯_{df_kakei_estat2020$SURVEY_DATE[.x]}.xls")))
        }
      )
    seq.int(11, 12) |>
      purrr::walk(
        function(.x) {
          download_estat_file(df_kakei_estat2020$URL[.x],
                              file = here(glue::glue("data-raw/家計調査/4-1_都市階級・地方・都道府県庁所在市別_二人以上の世帯・勤労者世帯・無職世帯_{df_kakei_estat2020$SURVEY_DATE[.x]}.xlsx")))
        }
      )
    seq.int(2, nrow(df_kakei_estat2019)) |>
      purrr::walk(
        function(.x) {
          download_estat_file(df_kakei_estat2019$URL[.x],
                              file = here(glue::glue("data-raw/家計調査/4-1_都市階級・地方・都道府県庁所在市別_二人以上の世帯・勤労者世帯・無職世帯_{df_kakei_estat2019$SURVEY_DATE[.x]}.xls")))
        }
      )
  }
  
  
  
  # cities <- 
  #   readxl::read_xlsx(here("data-raw/家計調査/4-1_都市階級・地方・都道府県庁所在市別_二人以上の世帯・勤労者世帯・無職世帯_202102.xlsx"),
  #                   sheet = 2,
  #                   range = "N9:DM9") |> names()
  
  read_kakei_4_1xlsx_sheet2 <- function(file) {
    d <-
      suppressMessages(
        readxl::read_excel(file,
                           sheet = 2,
                           range = "H10:DM700"))
    
    cities <- 
      paste(
        rep(
          paste(
            c("01100", "02201", "03201", "04100", "05201", "06201", "07201", 
              "08201", "09201", "10201", "11100", "12100", "13100", "14100", 
              "15100", "16201", "17201", "18201", "19201", "20201", "21201", 
              "22100", "23100", "24201", "25201", "26100", "27100", "28100", 
              "29201", "30201", "31201", "32201", "33100", "34100", "35203", 
              "36201", "37201", "38201", "39201", "40130", "41201", "42201", 
              "43100", "44201", "45201", "46201", "47201", "14130", "14150", 
              "22130", "27140", "40100"),
            c("札幌市", "青森市", "盛岡市", "仙台市", "秋田市", 
              "山形市", "福島市", "水戸市", "宇都宮市", "前橋市", 
              "さいたま市", "千葉市", "東京都区部", "横浜市", 
              "新潟市", "富山市", "金沢市", "福井市", "甲府市", 
              "長野市", "岐阜市", "静岡市", "名古屋市", "津市", 
              "大津市", "京都市", "大阪市", "神戸市", "奈良市", 
              "和歌山市", "鳥取市", "松江市", "岡山市", "広島市", 
              "山口市", "徳島市", "高松市", "松山市", "高知市", 
              "福岡市", "佐賀市", "長崎市", "熊本市", "大分市", 
              "宮崎市", "鹿児島市", "那覇市", "川崎市", "相模原市", 
              "浜松市", "堺市", "北九州市"),
            sep = "_"
          ),
          each = 2,
        ),
        rep(c("購入頻度", "支出金額"), times = 52),
        sep = "_"
      )
    
    d |> 
      purrr::set_names(
        c("一連番号", "時間軸コード", "階層コード", "分類コード", 
          "品目分類", "支出金額の単位",
          paste(cities,
                rep(c("100世帯当たり", "複数単位"), times = length(cities)/2),
                sep = "_") |> 
            stringr::str_replace_all("[[:space:]]", "_")))
  }
  
  read_kakei_4_1xlsx_sheet2_old <- function(file) {
    d <- 
      readxl::read_excel(file,
                         sheet = 2,
                         range = "H13:DJ703")
    
    d <- 
      d |> 
      select(!3)
    
    cities <- 
      paste(
        rep(
          paste(
            c("01100", "02201", "03201", "04100", "05201", "06201", "07201", 
              "08201", "09201", "10201", "11100", "12100", "13100", "14100", 
              "15100", "16201", "17201", "18201", "19201", "20201", "21201", 
              "22100", "23100", "24201", "25201", "26100", "27100", "28100", 
              "29201", "30201", "31201", "32201", "33100", "34100", "35203", 
              "36201", "37201", "38201", "39201", "40130", "41201", "42201", 
              "43100", "44201", "45201", "46201", "47201", "14130", "14150", 
              "22130", "27140", "40100"),
            c("札幌市", "青森市", "盛岡市", "仙台市", "秋田市", 
              "山形市", "福島市", "水戸市", "宇都宮市", "前橋市", 
              "さいたま市", "千葉市", "東京都区部", "横浜市", 
              "新潟市", "富山市", "金沢市", "福井市", "甲府市", 
              "長野市", "岐阜市", "静岡市", "名古屋市", "津市", 
              "大津市", "京都市", "大阪市", "神戸市", "奈良市", 
              "和歌山市", "鳥取市", "松江市", "岡山市", "広島市", 
              "山口市", "徳島市", "高松市", "松山市", "高知市", 
              "福岡市", "佐賀市", "長崎市", "熊本市", "大分市", 
              "宮崎市", "鹿児島市", "那覇市", "川崎市", "相模原市", 
              "浜松市", "堺市", "北九州市"),
            sep = "_"
          ),
          each = 2,
        ),
        rep(c("購入頻度", "支出金額"), times = 52),
        sep = "_"
      )
    
    d |> 
      purrr::set_names(c("分類コード", 
                         "品目分類",
                         paste(cities,
                               rep(c("100世帯当たり", "複数単位"), times = length(cities)/2),
                               sep = "_")))
  }
    
    # d <- 
    #   readxl::read_xlsx(here("data-raw/家計調査/4-1_都市階級・地方・都道府県庁所在市別_二人以上の世帯・勤労者世帯・無職世帯_202102.xlsx"),
    #                   sheet = 2,
    #                   range = "H10:DM700") |> 
    #   purrr::set_names(
    #     c("一連番号", "時間軸コード", "階層コード", "分類コード", 
    #       "品目分類", "支出金額の単位",
    #       paste(cities,
    #             rep(c("100世帯当たり", "複数単位"), times = length(cities)/2),
    #             sep = "_") |> 
    #         stringr::str_replace_all("[[:space:]]", "_"))
    #   )
    
    # d |> 
    #   filter(`品目分類` == "米") |> 
    #   select(`支出金額の単位`, contains("徳島市"))
    
    
    df_shikoku_kome_sisyutu <- 
      fs::dir_ls(here("data-raw/家計調査/"), 
                 regexp = ".xlsx$") |>
      ensurer::ensure(length(.) == 14L) |> 
      purrr::map_dfr(
        function(.x) {
          read_kakei_4_1xlsx_sheet2(.x) |> 
            filter(`品目分類` %in% c("米", "アイスクリーム・シャーベット", "殺虫・防虫剤")) |> 
            select(`品目分類`, matches("(徳島|高松|松山|高知)市"))
        },
        .id = "file") |> 
      tidyr::pivot_longer(cols = 3:10,
                          names_to = c("都道府県庁所在市", "項目"),
                          names_pattern = "(.*)_(.*_.*)",
                          values_to = "value") |> 
      # year and month ... ym 年月
      purrr::set_names(c("ym", "品目分類", "都道府県庁所在市", "項目", "value")) |> 
      mutate(ym = stringr::str_remove_all(basename(ym), "(.+_|.xlsx)")) |> 
      tidyr::separate(都道府県庁所在市,
                      into = c("市区町村コード", "市"),
                      sep = "_") |> 
      ensurer::ensure(nrow(.) == 336)
    
    df_shikoku_kome_sisyutu2 <- 
      fs::dir_ls(here("data-raw/家計調査/"), 
                 regexp = ".xls$") |>
      ensurer::ensure(length(.) == 22L) |> 
      purrr::map_dfr(
        function(.x) {
          read_kakei_4_1xlsx_sheet2_old(.x) |> 
            filter(`品目分類` %in% c("米", "アイスクリーム・シャーベット", "殺虫・防虫剤")) |> 
            select(`品目分類`, matches("(徳島|高松|松山|高知)市"))
        },
        .id = "file") |> 
      tidyr::pivot_longer(cols = 3:10,
                          names_to = c("都道府県庁所在市", "項目"),
                          names_pattern = "(.*)_(.*_.*)",
                          values_to = "value") |> 
      # year and month ... ym 年月
      purrr::set_names(c("ym", "品目分類", "都道府県庁所在市", "項目", "value")) |> 
      mutate(ym = stringr::str_remove_all(basename(ym), "(.+_|.xls)")) |> 
      tidyr::separate(都道府県庁所在市,
                      into = c("市区町村コード", "市"),
                      sep = "_") |> 
      ensurer::ensure(nrow(.) == 528)
    
    df_shikoku_kome_sisyutu2019to2021 <- 
      df_shikoku_kome_sisyutu |> 
      bind_rows(df_shikoku_kome_sisyutu2) |> 
      arrange(ym, `市区町村コード`, `品目分類`, `項目`)
    
    pins_resources_local |> 
      pins::pin_write(
        df_shikoku_kome_sisyutu2019to2021,
        name = "shikoku_kome_sisyutu2019to2021",
        description = "家計調査_1世帯当たり1か月間の支出金額_購入数量及び平均価格_四国4県",
        type = "csv")
}


