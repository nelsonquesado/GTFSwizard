#' Modify Dwell Times in GTFS Data
#'
#' @description
#' The `edit_dwelltime` function adjusts dwell times for specified trips and stops in a GTFS dataset. The dwell times are scaled by a given factor, and arrival and departure times are updated accordingly in the `stop_times` table.
#'
#' @param gtfs A GTFS object, preferably of class `wizardgtfs`. If not, the function will attempt to convert it using `GTFSwizard::as_wizardgtfs()`.
#' @param trips A character vector of trip IDs for which dwell times should be modified. Use `'all'` to include all trips (default).
#' @param stops A character vector of stop IDs for which dwell times should be modified. Use `'all'` to include all stops (default).
#' @param factor A numeric value representing the scaling factor for dwell times. For example, a factor of 1.5 increases dwell times by 50\%, while a factor of 0.5 reduces them by 50\%.
#'
#' @return A modified GTFS object with updated arrival and departure times in the `stop_times` table.
#'
#' @details
#' The function calculates the original dwell time (the difference between `departure_time` and `arrival_time`) for the specified trips and stops. The dwell time is then scaled by the `factor`, and the arrival and departure times are updated accordingly.
#'
#' If `trips` or `stops` is set to `'all'`, all trips or stops, respectively, will be considered. Input validation ensures that provided `trips` and `stops` exist in the GTFS dataset.
#'
#' @examples
#' gtfs <- set_dwelltime(for_rail_gtfs,
#'                     trips = for_rail_gtfs$trips$trip_id[1:100],
#'                     stops = for_rail_gtfs$stops$stop_id[1:20],
#'                     duration = 10)
#'
#' gtfs <- edit_dwelltime(gtfs,
#'                     trips = for_rail_gtfs$trips$trip_id[1:100],
#'                     stops = for_rail_gtfs$stops$stop_id[1:20],
#'                     factor = 1.5)
#'
#' get_dwelltimes(gtfs, method = 'detailed')
#'
#' @note
#' Ensure the `stop_times` table contains valid `arrival_time` and `departure_time` values. Empty or missing times may cause computation issues.
#'
#' @seealso [GTFSwizard::set_dwelltime()], [GTFSwizard::as_wizardgtfs()]
#'
#' @importFrom dplyr filter mutate arrange group_by lag mutate_at select
#' @importFrom stringr str_split
#' @importFrom tidyr replace_na
#' @importFrom checkmate assert_subset
#' @importFrom hms as_hms
#' @export
edit_dwelltime <- function(gtfs, trips = 'all', stops = 'all', factor) {

  checkmate::assert_subset(trips, choices = c('all', gtfs$trips$trip_id))
  checkmate::assert_subset(stops, choices = c('all', gtfs$stops$stop_id))

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('The gtfs object is not of the wizardgtfs class.\nComputation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(any(stops == 'all')) {stops <- gtfs$stops$stop_id}
  if(any(trips == 'all')) {trips <- gtfs$trips$trip_id}

  gtfs$stop_times <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '' | !departure_time == "") %>%
    dplyr::arrange(trip_id, stop_sequence) %>%
    dplyr::mutate(edit = trip_id %in% trips & stop_id %in% stops) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::mutate(
      arrival_time_sec = arrival_time %>%
        stringr::str_split(":") %>%
        lapply(FUN = as.numeric) %>%
        lapply(FUN = function(x){
          x[1]*60*60+x[2]*60+x[3]
        }) %>%
        unlist() %>%
        na.omit(),
      departure_time_sec = departure_time %>%
        stringr::str_split(":") %>%
        lapply(FUN = as.numeric) %>%
        lapply(FUN = function(x){
          x[1]*60*60+x[2]*60+x[3]
        }) %>%
        unlist() %>%
        na.omit(),
      dwell_time = departure_time_sec - arrival_time_sec,
      new_dwell_time = ifelse(edit, round(factor * dwell_time), dwell_time),
      diff_dwell_time = new_dwell_time - dwell_time,
      departure_time_diff = cumsum(diff_dwell_time),
      arrival_time_diff = tidyr::replace_na(dplyr::lag(departure_time_diff), 0)
    ) %>%
    dplyr::mutate(
      arrival_time = arrival_time_sec + arrival_time_diff,
      departure_time = departure_time_sec + departure_time_diff,
    ) %>%
    dplyr::mutate_at(c('arrival_time', 'departure_time'), function(x){as.character(hms::as_hms(x))}) %>%
    select(-dwell_time, -edit, -arrival_time_sec, -departure_time_sec, -new_dwell_time, -diff_dwell_time, -arrival_time_diff, -departure_time_diff)

  return(gtfs)

}
