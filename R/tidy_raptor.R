#' Calculate Travel Times with RAPTOR Algorithm
#'
#' @description
#' The `tidy_raptor` function calculates travel times from a set of origin stops to all reachable stops within a GTFS dataset.
#' It uses the RAPTOR (Round-Based Public Transit Routing) algorithm from the `tidytransit` package and integrates it with the GTFSwizard framework.
#'
#' @param gtfs A GTFS object, preferably of class `wizardgtfs`. If not, the function will attempt to convert it using `GTFSwizard::as_wizardgtfs()`.
#' @param min_departure A string representing the earliest departure time, in "HH:MM:SS" format. Defaults to `"0:0:0"`.
#' @param max_arrival A string representing the latest arrival time, in "HH:MM:SS" format. Defaults to `"23:59:59"`.
#' @param dates A date (in `"YYYY-MM-DD"` format) to filter the GTFS dataset to specific calendar days. Defaults to `NULL`, meaning the furthest date.
#' @param stop_ids A character vector of stop IDs from where journeys should start (or end, if `arrival = TRUE`).
#' @param arrival Logical. If `FALSE` (default), journeys start from `stop_ids`. If `TRUE`, journeys end at `stop_ids`.
#' @param time_range Either a range in seconds (numeric) or a vector with the minimal and maximal departure time (e.g., `c(0, 3600)` or `"HH:MM:SS"`) describing the journey window.
#' @param max_transfers Maximum number of transfers allowed. Defaults to `NULL` (no limit).
#' @param keep One of `"all"`, `"shortest"`, `"earliest"`, or `"latest"`. Determines which journeys to retain:
#'   - `"all"`: All journeys are returned (default).
#'   - `"shortest"`: Only journeys with the shortest travel time.
#'   - `"earliest"`: Journeys arriving at stops the earliest.
#'   - `"latest"`: Journeys arriving at stops the latest.
#' @param filter A logical to filter for min_departure, max_arrivel, and dates. Defaults to `TRUE`.
#'
#' @return A tibble containing the RAPTOR algorithm results, including:
#' \describe{
#'   \item{from_stop_id}{The ID of the stop where the journey starts.}
#'   \item{to_stop_id}{The ID of the stop where the journey ends.}
#'   \item{departure_time}{Departure time from the origin stop.}
#'   \item{arrival_time}{Arrival time at the destination stop.}
#'   \item{travel_time}{Total travel time in seconds.}
#' }
#'
#' @note
#' Ensure that the `stop_times` is present and correctly structured in the GTFS dataset.
#' Time values in `min_departure`, `max_arrival`, and `time_range` should be correctly formatted to avoid errors.
#' `max_arrival` must be 23:59:59 or earlier.
#'
#' @examples
#' tidy_raptor(for_rail_gtfs,
#'    min_departure = '06:20:00',
#'    max_arrival = '09:40:00',
#'    dates = "2021-12-13",
#'    max_transfers = 2,
#'    keep = "all",
#'    stop_ids = '66')
#'
#' @seealso [tidytransit::raptor()], [GTFSwizard::as_wizardgtfs()], [GTFSwizard::filter_time()]
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_split
#' @importFrom data.table data.table
#' @importFrom tidytransit raptor
#' @importFrom hms as_hms
#' @export

tidy_raptor <- function(gtfs,
                        min_departure = "0:0:0",
                        max_arrival = "23:59:59",
                        dates = NULL,
                        stop_ids,
                        arrival = FALSE,
                        time_range = 3600,
                        max_transfers = NULL,
                        keep = "all",
                        filter = TRUE) {

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('The gtfs object is not of the wizardgtfs class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  checkmate::assert_logical(filter)

  if(stringr::str_split(max_arrival, ":") %>% lapply(FUN = as.numeric) %>% lapply(FUN = function(x){x[1]*60*60+x[2]*60+x[3]}) %>% unlist > 86399){
    stop(crayon::cyan('max.arrival'), ' must be', crayon::cyan(' 23:59:59'), ' or earlier')
  }

  if(filter) {
  gtfs2 <-
    gtfs %>%
    filter_time(min_departure, max_arrival) %>%
    filter_date(dates)
  } else {
    gtfs2 <- gtfs
  }

  stop_times <-
    gtfs2$stop_times %>%
    dplyr::mutate(arrival_time_num = arrival_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit(),
                  departure_time_num = departure_time %>%
                    stringr::str_split(":") %>%
                    lapply(FUN = as.numeric) %>%
                    lapply(FUN = function(x){
                      x[1]*60*60+x[2]*60+x[3]
                    }) %>%
                    unlist() %>%
                    na.omit()) %>%
    dplyr::mutate(arrival_time = hms::as_hms(arrival_time),
                  departure_time = hms::as_hms(departure_time)) %>%
    data.table::data.table()

  raptor_results <-
    tidytransit::raptor(
      stop_times = stop_times,
      transfers = NULL,
      stop_ids,
      arrival,
      time_range,
      max_transfers,
      keep
    )

  raptor_results <-
    raptor_results %>%
    tibble::tibble()

  return(raptor_results)

}
