#' Yarra River water-quality observations (cleaned)
#'
#' Cleaned Yarra River water-quality records built from a raw Excel file in
#' \code{data-raw/}. Includes derived time fields and parameters with sufficient
#' coverage for interactive exploration.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{site_id}{Monitoring site identifier (factor/character).}
#'   \item{name}{Site name (character).}
#'   \item{datetime}{Original timestamp (POSIXct).}
#'   \item{data_type}{Data category; retained as "Quality".}
#'   \item{parameter_id}{Parameter code (factor/character).}
#'   \item{parameter}{Parameter name (e.g., Nitrate, Turbidity).}
#'   \item{value}{Measurement value (numeric).}
#'   \item{unit_of_measurement}{Units (character).}
#'   \item{quality}{Quality flag/label (character).}
#'   \item{resolution}{Resolution/precision (character).}
#'   \item{date}{Calendar date (Date).}
#'   \item{year}{Calendar year (integer).}
#'   \item{month}{Month (ordered factor).}
#'   \item{hour}{Hour of day (integer).}
#' }
#'
#' @details See the build script \code{data-raw/yarra_wq.R}.
#' @source Department of Energy, Environment and Climate Action (2024).
#'  Victoria’s Water Measurement Information System (WMIS).
#'  https://data.water.vic.gov.au/WMIS/
#'
 "yarra_wq"

