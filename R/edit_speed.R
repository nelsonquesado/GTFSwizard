#' Adjust Travel Speed in a GTFS Dataset
#'
#' @description
#' The `edit_speed` function adjusts the travel speeds between stops in a GTFS dataset by modifying trip durations based on a specified speed multiplier. It allows selective adjustments for specific trips and stops or applies changes globally across the dataset.
#'
#' @param gtfs A GTFS object, preferably of class `wizardgtfs`. If not, the function attempts to convert it using `GTFSwizard::as_wizardgtfs()`.
#' @param trips A character vector specifying the `trip_id`s to modify. Defaults to `"all"` to include all trips.
#' @param stops A character vector specifying the `stop_id`s to include in the adjustment. Defaults to `"all"` to include all stops.
#' @param factor A numeric value representing the multiplier for the speed. For example, a value of `2` doubles the speed, halving the travel time.
#'
#' @return A GTFS object with updated `stop_times` reflecting the adjusted travel durations.
#'
#' @details
#' The function performs the following steps:
#' \describe{
#'   \item{1. Retrieve Durations}{The `get_durations()` function calculates trip durations, filtered by the specified trips and stops.}
#'   \item{2. Adjust Durations}{Durations are divided by the speed factor to compute new durations. Time differences are calculated.}
#'   \item{3. Update Stop Times}{Cumulative time differences are added to the `arrival_time` and `departure_time` columns in the `stop_times` table.}
#' }
#' If no specific trips or stops are provided, the function adjusts all trips and stops in the GTFS object.
#'
#' @note Ensure that the `factor` is greater than 0. Using a value less than or equal to 0 will result in invalid or nonsensical time adjustments.
#'
#' @examples
#' edit_speed(for_rail_gtfs,
#'           trips = for_rail_gtfs$trips$trip_id[1:2],
#'           stops = for_rail_gtfs$stops$stop_id[1:2],
#'           factor = 1.5)
#'
#' @seealso [GTFSwizard::get_speeds()]
#'
#' @importFrom dplyr mutate select arrange filter group_by ungroup right_join
#' @importFrom hms as_hms
#' @importFrom lubridate seconds hms
#' @export
edit_speed <- function(gtfs, trips = 'all', stops = 'all', factor) {

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(any(stops == 'all')) {
    durations <- get_durations(gtfs, 'detailed', trips = trips)
  } else {
    durations <- get_durations(gtfs, 'detailed', trips = trips) %>%
      dplyr::filter(from_stop_id %in% stops | to_stop_id %in% stops)
  }

  gtfs$stop_times <-
    durations %>%
    dplyr::mutate(new.duration = duration / factor,
                  diff.time = new.duration - duration) %>%
    dplyr::select(trip_id, arrival_time, stop_id = to_stop_id, diff.time) %>%
    dplyr::right_join(.,
                      gtfs$stop_times,
                      by = c('trip_id', 'stop_id', 'arrival_time')) %>%
    dplyr::arrange(trip_id, stop_sequence) %>%
    dplyr::mutate(diff.time = if_else(is.na(diff.time), 0, diff.time)) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::mutate(cum.diff.time = cumsum(diff.time)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at(c('arrival_time', 'departure_time'), .funs = function(x){as.character(hms::as_hms(round(as.numeric(lubridate::seconds(lubridate::hms(x)) + .$cum.diff.time))))}) %>%
    select(-diff.time, -cum.diff.time)

  return(gtfs)

}
