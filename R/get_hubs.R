#' Identify Transit Hubs
#'
#' @description
#' The `get_hubs` function identifies transit hubs from a GTFS dataset by calculating the number of trips and unique routes served by each stop. It returns a spatial object with metadata for each hub, allowing for further analysis or visualization.
#'
#' @param gtfs A GTFS object, preferably of class `wizardgtfs`. If not, the function will attempt to convert it using `GTFSwizard::as_wizardgtfs()`.
#'
#' @return An `sf` object containing the following columns:
#' \describe{
#'   \item{stop_id}{The unique identifier for each stop.}
#'   \item{trip_id}{A list of trip IDs associated with the stop.}
#'   \item{route_id}{A list of unique route IDs associated with the stop.}
#'   \item{n_trip}{The total number of trips that pass through the stop.}
#'   \item{n_routes}{The total number of unique routes that pass through the stop.}
#'   \item{geometry}{The spatial location of the stop as an `sf` point object.}
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Extracts `stop_id` and `trip_id` pairs from the `stop_times` table.
#'   \item Joins this data with the `trips` table to associate `route_id` with each trip.
#'   \item Groups by `stop_id` to compute the number of trips (`n_trip`) and unique routes (`n_route`) per stop.
#'   \item Joins the resulting data with the spatial geometry of stops, transforming it into an `sf` object.
#'   \item Sorts the hubs by the number of unique routes (`n_routes`) in descending order.
#' }
#'
#' @note The function uses `sf` for spatial data manipulation. Ensure that the GTFS dataset includes the `stop_times`, `trips`, and `stops` tables.
#'
#' @examples
#' # Identify hubs in a GTFS dataset
#' get_hubs(for_rail_gtfs)
#'
#' @seealso [GTFSwizard::get_stops_sf()], [GTFSwizard::as_wizardgtfs()]
#'
#' @importFrom dplyr select distinct left_join group_by reframe mutate arrange ungroup rowwise
#' @importFrom sf st_as_sf
#' @importFrom GTFSwizard get_stops_sf as_wizardgtfs
#' @export
get_hubs <- function(gtfs) {

  # INCLUIR A DIVISAO POR SERVICE PATTERN

  if(!"wizardgtfs" %in% class(gtfs)){
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
  }

  hubs <-
    gtfs$stop_times %>%
    dplyr::select(stop_id, trip_id) %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      gtfs$trips %>% select(route_id, trip_id),
      by = 'trip_id'
    ) %>%
    dplyr::group_by(stop_id) %>%
    dplyr::reframe(trip_id = list(trip_id),
                   route_id = list(unique(route_id))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(n_trip = length(trip_id),
                  n_routes = length(route_id)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-n_routes) %>%
    dplyr::left_join(GTFSwizard::get_stops_sf(gtfs$stops) %>%
                       dplyr::select(stop_id),
                     by = 'stop_id') %>%
    sf::st_as_sf()

  return(hubs)

}
