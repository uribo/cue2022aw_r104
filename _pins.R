################################
# データの用意
################################
# local -------------------------------------------------------------------
pins_resources_local <-
  pins::board_folder(here::here("data-raw"))

# online ------------------------------------------------------------------
pins_resources_online <-
  pins::board_url(c(
    "ssdse_b" = "https://www.nstac.go.jp/sys/files/SSDSE-B-2022.csv",
    "tokushima_zoo_animals22" = "https://raw.githubusercontent.com/uribo/tokupon_ds/main/data-raw/tokushima_zoo_animals22.csv"))
