#' Delay Specified Trips in a `wizardgtfs` Object
#'
#' This function adds a delay to the arrival and departure times of specified trips within a `wizardgtfs` object.
#' If the input GTFS object is not of class `wizardgtfs`, it will be converted.
#'
#' @param gtfs An object representing GTFS data, preferably of class `wizardgtfs`.
#' @param trip A character vector of `trip_id`s in the `wizardgtfs` object that will be delayed. Each `trip_id` must exist in `gtfs$trips$trip_id`.
#' @param duration A delay duration, either as a `duration` object or a numeric value representing seconds.
#'
#' @details
#' This function adjusts the arrival and departure times of the specified `trip_id`s in `gtfs$stop_times` by the specified `duration`.
#' If `gtfs` is not a `wizardgtfs` object, the function will attempt to convert it using `GTFSwizard::as_wizardgtfs()`, and a warning will be issued.
#' The function checks that `trip` contains valid `trip_id`s and that `duration` is either a `duration` or numeric (seconds).
#'
#' @return A modified `wizardgtfs` object with updated arrival and departure times for the specified trips.
#'
#' @examples
#' \dontrun{
#' # Delay trips by 5 minutes
#' gtfs <- delay_trip(gtfs = for_gtfs, for_gtfs$trips$trip_id[1:2], duration = 300)
#'
#' # Delay trips by duration
#' gtfs <- delay_trip(gtfs = for_gtfs,
#'                     trip = for_gtfs$trips$trip_id[1],
#'                     duration = lubridate::duration(10, "minutes"))
#'  }
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()] for converting GTFS objects to `wizardgtfs` class.
#'
#' @importFrom lubridate hms as.duration duration
#' @importFrom checkmate assert_subset
#' @export
delay_trip <- function(gtfs, trip, duration){

  # checa os argumentos -------------------------------------------------------------------------
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('The gtfs object is not of the wizardgtfs class.\nComputation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(!lubridate::is.duration(duration)) {

    if(!is.numeric(duration)) {
      warning("'duration' muste be of the class duration or numeric (seconds)")
      stop()
    }

    dur <- lubridate::duration(duration, units = 'seconds')

  } else {dur <- duration}

  checkmate::assert_subset(trip, choices = gtfs$trips$trip_id)

  # atrasa as trips -----------------------------------------------------------------------------
  temp.arrival <-
    gtfs$stop_times$arrival_time[gtfs$stop_times$trip_id %in% trip & !gtfs$stop_times$arrival_time == ""] %>%
    lubridate::hms() %>%
    lubridate::as.duration() + dur

  gtfs$stop_times$arrival_time[gtfs$stop_times$trip_id %in% trip & !gtfs$stop_times$arrival_time == ""] <-
    temp.arrival %>%
    as.numeric() %>%
    { sprintf("%02d:%02d:%02d", . %/% 3600, (. %% 3600) %/% 60, round(. %% 60)) }

  temp.departure <-
    gtfs$stop_times$departure_time[gtfs$stop_times$trip_id %in% trip & !gtfs$stop_times$departure_time == ""] %>%
    lubridate::hms() %>%
    lubridate::as.duration() + dur

  gtfs$stop_times$departure_time[gtfs$stop_times$trip_id %in% trip & !gtfs$stop_times$departure_time == ""] <-
    temp.departure %>%
    as.numeric() %>%
    { sprintf("%02d:%02d:%02d", . %/% 3600, (. %% 3600) %/% 60, round(. %% 60)) }

  # retornando gtfs -----------------------------------------------------------------------------
  return(gtfs)

}
