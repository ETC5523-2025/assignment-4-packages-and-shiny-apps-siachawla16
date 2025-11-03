library(readxl)
library(dplyr)
library(lubridate)
library(janitor)
library(readr)


yarra_wq <- read_excel("data-raw/yarra_wq.xls",
                       col_types = c("numeric",
                                     "text",
                                     "date",
                                     "text",
                                     "numeric",
                                     "text",
                                     "numeric",
                                     "text",
                                     "numeric",
                                     "text"))



yarra_wq <- yarra_wq |> mutate( `Site ID` = as.factor(`Site ID`),
                                date    = as_date(Datetime),
                                year    = year(Datetime),
                                month   = month(Datetime, label = TRUE),
                                hour    = hour(Datetime) ,
                                `Parameter ID`= as.factor(`Parameter ID`)) |>
  clean_names()



yarra_wq <- yarra_wq |> filter(data_type == "Quality")

parameter_coverage <- yarra_wq |>
  group_by(parameter) |>
  summarise(n_obs = n()) |> arrange(n_obs)

yarra_wq <- yarra_wq |>
  group_by(parameter) |>
  filter(n() >= 30) |>
  ungroup()

coverage <- yarra_wq |>
  group_by(parameter) |>
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    n_years    = n_distinct(year),
    total_obs  = n(),
    year_coverage = last_year - first_year,
    .groups = "drop"
  )

keep_parameter <- coverage |>
  filter(year_coverage > 20) |>
  pull(parameter)

yarra_wq <- yarra_wq |>
  filter(parameter %in% keep_parameter)



usethis::use_data(yarra_wq, overwrite = TRUE)
