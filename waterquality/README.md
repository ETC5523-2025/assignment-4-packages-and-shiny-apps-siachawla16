
<!-- README.md is generated from README.Rmd. Please edit that file -->

# waterquality

This R package contains a cleaned Yarra River water-quality dataset and
a Shiny app to explore the data interactively.

## Install

``` r
# install.packages("remotes")
remotes::install_github(
  "ETC5523-2025/assignment-4-packages-and-shiny-apps-siachawla16",
  subdir = "waterquality"
)
```

## Launch the app

``` r
library(waterquality)
run_app()
```

## What the app shows

- overlay multiple water-quality parameters on one time-series plot

- choose which parameters to include

- date slider to change the time window

- option to standardise values (z score) to compare different units

- summaries tab: mean, median, sd, min, max for BEFORE or AFTER a chosen
  cutoff date

- clear note and shaded band indicating the WMIS data gap between
  1995–2019

## Data source

Victoria Government: Department of Energy, Environment and Climate
Action (2024) WMIS – Water Measurement Information System
<https://data.water.vic.gov.au/WMIS/>

## pkgdown site

Visit:
<https://etc5523-2025.github.io/assignment-4-packages-and-shiny-apps-siachawla16/>
