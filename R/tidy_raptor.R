#' Calculate Travel Times with RAPTOR Algorithm
#'
#' @description
#' The `tidy_raptor` function calculates travel times from a set of origin stops to all reachable stops within a GTFS dataset.
#' It uses the RAPTOR (Round-Based Public Transit Routing) algorithm from the `tidytransit` package and integrates it with the GTFSwizard framework.
#'
#' @param gtfs A GTFS object, preferably of class `wizardgtfs`. If not, the function will attempt to convert it using `GTFSwizard::as_wizardgtfs()`.
#' @param min_departure A string representing the earliest departure time, in "HH:MM:SS" format. Defaults to `"0:0:0"`.
#' @param max_arrival A string representing the latest arrival time, in "HH:MM:SS" format. Defaults to `"23:59:59"`.
#' @param dates One service date. When `NULL`, the latest active service date
#'   is used.
#' @param stop_ids A character vector of stop IDs from where journeys should start (or end, if `arrival = TRUE`).
#' @param arrival Logical. If `FALSE` (default), journeys start from `stop_ids`. If `TRUE`, journeys end at `stop_ids`.
#' @param time_range Either a range in seconds (numeric) or a vector with the minimal and maximal departure time (e.g., `c(0, 3600)` or `"HH:MM:SS"`) describing the journey window.
#' @param max_transfers Maximum number of transfers allowed. Defaults to `NULL` (no limit).
#' @param keep One of `"all"`, `"shortest"`, `"earliest"`, or `"latest"`. Determines which journeys to retain:
#'   - `"all"`: All journeys are returned (default).
#'   - `"shortest"`: Only journeys with the shortest travel time.
#'   - `"earliest"`: Journeys arriving at stops the earliest.
#'   - `"latest"`: Journeys arriving at stops the latest.
#' @param filter Logical. Apply `min_departure`, `max_arrival`, and `dates`
#'   before routing.
#' @param separate_starts Logical. Keep results for each origin start time
#'   separately; passed to [tidytransit::raptor()].
#'
#' @return A tibble containing the RAPTOR algorithm results, including:
#' \describe{
#'   \item{from_stop_id}{The ID of the stop where the journey starts.}
#'   \item{to_stop_id}{The ID of the stop where the journey ends.}
#'   \item{departure_time}{Departure time from the origin stop.}
#'   \item{arrival_time}{Arrival time at the destination stop.}
#'   \item{travel_time}{Total travel time in seconds.}
#'   \item{transfers}{Number of transfers in the journey.}
#' }
#'
#' @note
#' Ensure that the `stop_times` is present and correctly structured in the GTFS dataset.
#' Time values in `min_departure`, `max_arrival`, and `time_range` should be correctly formatted to avoid errors.
#' GTFS times beyond 24:00:00 are supported.
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
                        filter = TRUE,
                        separate_starts = FALSE) {

  require_pkg("tidytransit", "`tidy_raptor()`")
  require_pkg("data.table", "`tidy_raptor()`")
  require_pkg("hms", "`tidy_raptor()`")
  gtfs <- ensure_wizardgtfs(gtfs)

  gw_assert_flag(filter, "filter")
  gw_assert_flag(arrival, "arrival")
  gw_assert_flag(separate_starts, "separate_starts")
  assert_known_ids(stop_ids, gtfs$stops$stop_id, "stop", "`gtfs$stops`")

  limits <- gtfs_time_to_seconds(c(min_departure, max_arrival))
  if(anyNA(limits)){
    gw_stop("`min_departure` and `max_arrival` must be valid GTFS times.")
  }
  if(limits[1] > limits[2]){
    gw_stop("`min_departure` must not be later than `max_arrival`.")
  }
  keep_choices <- c("all", "shortest", "earliest", "latest")
  if(!is.character(keep) || length(keep) != 1L ||
     is.na(keep) || !keep %in% keep_choices){
    gw_stop(
      "`keep` must be one of ",
      paste(sprintf("`%s`", keep_choices), collapse = ", "), "."
    )
  }
  if(!is.null(max_transfers)){
    gw_assert_int(max_transfers, "max_transfers", lower = 0L)
  }
  if(!length(time_range) %in% c(1L, 2L)){
    gw_stop("`time_range` must contain one duration or two time bounds.")
  }
  if(length(time_range) == 1L &&
     (!is.numeric(time_range) || is.na(time_range) || time_range < 1)){
    gw_stop("a single `time_range` value must be a positive number of seconds.")
  }
  if(length(time_range) == 2L){
    parsed_range <- if(is.character(time_range)){
      gtfs_time_to_seconds(time_range)
    } else {
      suppressWarnings(as.numeric(time_range))
    }
    if(anyNA(parsed_range) || any(!is.finite(parsed_range))){
      gw_stop("two-value `time_range` bounds must be valid times or seconds.")
    }
    time_range <- parsed_range
  }

  if(filter) {
    if(is.null(dates)){
      dates <- max(as.Date(gtfs$dates_services$date))
    }
    dates <- parse_gtfs_date(dates)
    if(length(dates) != 1L || is.na(dates)){
      gw_stop("`dates` must contain exactly one valid service date.")
    }
    gtfs2 <- gtfs %>%
      filter_time(min_departure, max_arrival) %>%
      filter_date(dates)
  } else {
    gtfs2 <- gtfs
  }

  stop_times <- gtfs2$stop_times |>
    dplyr::mutate(
      arrival_time_num = gtfs_time_to_seconds(arrival_time),
      departure_time_num = gtfs_time_to_seconds(departure_time),
      arrival_time = hms::as_hms(arrival_time_num),
      departure_time = hms::as_hms(departure_time_num)
    ) |>
    data.table::data.table()

  transfers <- gtfs2$transfers
  if(is.null(transfers)){
    transfers <- data.frame(
      from_stop_id = character(),
      to_stop_id = character(),
      transfer_type = integer(),
      min_transfer_time = integer()
    )
  }

  raptor_results <- tidytransit::raptor(
      stop_times = stop_times,
      transfers = transfers,
      stop_ids = stop_ids,
      arrival = arrival,
      time_range = time_range,
      max_transfers = max_transfers,
      keep = keep,
      separate_starts = separate_starts
    )

  raptor_results <- tibble::as_tibble(raptor_results)
  names(raptor_results)[
    match(
      c("journey_departure_time", "journey_arrival_time"),
      names(raptor_results)
    )
  ] <- c("departure_time", "arrival_time")
  raptor_results <- raptor_results |>
    dplyr::mutate(
      departure_time = seconds_to_gtfs_time(.data$departure_time),
      arrival_time = seconds_to_gtfs_time(.data$arrival_time)
    )
  raptor_results

}
