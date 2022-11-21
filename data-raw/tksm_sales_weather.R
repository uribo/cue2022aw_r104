library(tidyverse)
pins_resources_local <- 
  pins::board_folder(here::here("data-raw"))

if (pins_resources_local |> 
    pins::pin_list() |> 
    stringr::str_detect("tksm_sales_weather") |> 
    sum() != 1) {
  df_shikoku_kome_sisyutu2019to2021 <-
    pins_resources_local |> 
    pins::pin_read("shikoku_kome_sisyutu2019to2021") |> 
    as_tibble()
  
  df_shikoku_weather2019to2021 <-
    pins_resources_local |> 
    pins::pin_read("shikoku_weather2019to2021") |> 
    as_tibble()
  
  df_pesticide_ice <- 
    df_shikoku_kome_sisyutu2019to2021 |> 
    filter(`市` == "徳島市",
           `項目` == "購入頻度_100世帯当たり") |> 
    select(!c(`市`, `項目`)) |> 
    pivot_wider(id_cols = `ym`,
                names_from = `品目分類`) |> 
    rename(`ice` = `アイスクリーム・シャーベット`,
           `pesticide` = `殺虫・防虫剤`,
           `rice` = `米`) |> 
    mutate(ym = as.character(ym)) |> 
    left_join(
      df_shikoku_weather2019to2021 |> 
        filter(station_name == "徳島") |> 
        transmute(ym = str_c(year,
                             str_pad(month, width = 2, pad = "0")),
                  precipitation_sum_mm,
                  temperature_average_c),
      by = "ym")
  
  pins_resources_local |> 
    pins::pin_write(
      df_pesticide_ice,
      name = "tksm_sales_weather",
      type = "rds")
}
