################################
################################
# renv::install("uribo/jmastats")
# renv::settings$ignored.packages(unique(c(renv::settings$ignored.packages(), "jmastats")))

if (file.exists(here("data-raw/shikoku_weather2019to2021.csv")) == FALSE) {
  library(jmastats)
  library(dplyr)
  library(here)
  
  df_shikoku_weather2019to2021 <- 
    tidyr::expand_grid(
      item = "monthly",
      block_no = c("47895", "47891", "47887", "47893"),
      year = seq.int(2019, 2021),
      month = seq.int(12)) |> 
    purrr::pmap_dfr(
      function(item, block_no, year, month) {
        jma_collect(item = item, block_no = block_no, year = year, month = month) |> 
          dplyr::mutate(year = year,
                        block_no = block_no) |> 
          tidyr::unnest(cols = c("precipitation", "temperature"),
                        names_sep = "_") |> 
          dplyr::select(year, month, block_no, `precipitation_sum(mm)`, `temperature_average(℃)`) 
      }
    ) |> 
    jmastats::parse_unit() |> 
    mutate(block_no = as.character(block_no)) |> 
    # なぜか重複するので
    distinct(block_no, year, month, .keep_all = TRUE) |> 
    ensurer::ensure(nrow(.) == 12*4*3) |> 
    arrange(year, month, desc(block_no))
  
  df_shikoku_weather2019to2021 <- 
    df_shikoku_weather2019to2021 |> 
    left_join(
      jmastats::stations |> 
        filter(stringr::str_detect(address, "徳島市|高松市|松山市|高知市")) |> 
        select(station_name, block_no) |> 
        filter(block_no %in% c("47895", "47891", "47887", "47893")) |> 
        sf::st_drop_geometry() |> 
        distinct(station_name, block_no) |> 
        ensurer::ensure(nrow(.) == 4L),
      by = "block_no"
    ) |> 
    relocate(station_name, .after = 3)
  
  df_shikoku_weather2019to2021 |> 
    readr::write_csv(here("data-raw/shikoku_weather2019to2021.csv"))
}
