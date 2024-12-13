#' Get First Departure Times for GTFS Trips
#'
#' Extracts the first departure time for each trip in a `wizardgtfs` object, along with the associated `route_id`, and `stop_id` where the first departure occurs.
#'
#' @param gtfs A GTFS object. If not of class `wizardgtfs`, it will be converted internally using `as_wizardgtfs()`. This may increase computation time.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{route_id}{ID of the route associated with the trip.}
#'   \item{trip_id}{ID of the trip.}
#'   \item{departure_time}{Time of the first departure for the trip, as a character string in "HH:MM:SS" format.}
#'   \item{stop_id}{ID of the stop where the first departure occurs.}
#' }
#'
#' @details
#' This function identifies the first departure time for each trip in the GTFS dataset. It uses the `stop_times` table to find the earliest `departure_time` for each `trip_id` and joins this information with the `trips` table to include the corresponding `route_id`. The result is a tidy tibble suitable for further analysis or visualization.
#'
#' If the input GTFS object is not of the `wizardgtfs` class, the function converts it using `as_wizardgtfs()`. A message is displayed to inform the user about the conversion.
#'
#' @examples
#' # Load GTFS data
#' gtfs <- for_rail_gtfs
#'
#' # Get the first departures
#' first_departures <- get_1stdeparture(gtfs)
#' head(first_departures)
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()]
#'
#' @importFrom dplyr select left_join group_by reframe
#' @export

get_1stdeparture <- function(gtfs) {

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  departures <-
    gtfs$trips %>%
    dplyr::select(trip_id, route_id) %>%
    dplyr::left_join(gtfs$stop_times %>%
                       dplyr::group_by(trip_id) %>%
                dplyr::reframe(departure_time = departure_time[1],
                        stop_id = stop_id[1]),
              by = 'trip_id') %>%
    dplyr::select(route_id, trip_id, departure_time, stop_id)

  return(departures)

}
