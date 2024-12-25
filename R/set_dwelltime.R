#' Set Dwell Time for GTFS Stops
#'
#' @description
#' The `set_dwelltime` function updates the arrival and departure times in the `stop_times` table of a GTFS object based on a specified dwell time duration. The function modifies the dwell time for selected trips and stops, or for all trips and stops by default.
#'
#' @param gtfs A GTFS object, preferably of class `wizardgtfs`. If not, the function will attempt to convert it using `GTFSwizard::as_wizardgtfs()`.
#' @param duration A numeric value specifying the desired dwell time in seconds. Defaults to 30 seconds.
#' @param trips A character vector of trip IDs for which the dwell time will be updated. Use `'all'` to update all trips (default).
#' @param stops A character vector of stop IDs for which the dwell time will be updated. Use `'all'` to update all stops (default).
#'
#' @return A modified GTFS object with updated arrival and departure times in the `stop_times` table.
#'
#' @details
#' This function calculates the midpoint between the original `arrival_time` and `departure_time` for the specified trips and stops. It then adjusts these times based on the desired dwell time (`duration`), ensuring that the dwell time is evenly distributed around the midpoint.
#'
#' @examples
#' # Set dwell time to 30 seconds for specific trips and stops
#' set_dwelltime(for_rail_gtfs, duration = 30,
#'                trips = for_rail_gtfs$trips$trip_id[1:2],
#'                stops = for_rail_gtfs$stops$stop_id[1:2])
#'
#' @note
#' Ensure the `stop_times` table contains valid `arrival_time` and `departure_time` values. Empty or missing times may cause computation issues.
#'
#' @seealso [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_dwelltimes()]
#'
#' @importFrom dplyr filter mutate mutate_at select
#' @importFrom stringr str_split
#' @importFrom checkmate assert_subset
#' @importFrom hms as_hms
#' @export
set_dwelltime <- function(gtfs, duration = 30, trips = 'all', stops = 'all') {

  checkmate::assert_subset(trips, choices = c('all', gtfs$trips$trip_id))
  checkmate::assert_subset(stops, choices = c('all', gtfs$stops$stop_id))

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('The gtfs object is not of the wizardgtfs class.\nComputation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(!is.numeric(duration)) {stop(crayon::cyan("duration"), " must be of the class numeric (seconds)")}

  if(any(stops == 'all')) {stops <- gtfs$stops$stop_id}

  if(any(trips == 'all')) {trips <- gtfs$trips$trip_id}

  gtfs$stop_times <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '' | !departure_time == "") %>%
    dplyr::mutate(edit = trip_id %in% trips & stop_id %in% stops) %>%
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
      mid_dwelltime = ifelse(edit, (arrival_time_sec + departure_time_sec)/2, NA),
    ) %>%
  dplyr::mutate(
    arrival_time = ifelse(edit, mid_dwelltime - round((duration/2)), arrival_time_sec),
    departure_time = ifelse(edit, mid_dwelltime + (duration - round((duration/2))), departure_time_sec),
  ) %>%
    dplyr::mutate_at(c('arrival_time', 'departure_time'), function(x){as.character(hms::as_hms(x))}) %>%
    select(-mid_dwelltime, -edit, -arrival_time_sec, -departure_time_sec)

  return(gtfs)

}

