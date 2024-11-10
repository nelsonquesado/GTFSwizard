#' Generate Shapes Table for GTFS Data
#'
#' The `get_shapes` function reconstructs the `shapes` table for a GTFS dataset using an approximation based on stop coordinates and sequence information. It creates geometric representations of trips by connecting stops in sequence for each trip.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If not, it will be converted automatically.
#'
#' @return A modified GTFS object that includes a `shapes` table derived from the stops and trips information.
#'
#' @details
#' This function constructs the `shapes` table by sequentially connecting stops along each trip using a Euclidean approximation. If the GTFS object already contains a `shapes` table, it will be overwritten, and a warning will be displayed. The process involves:
#'
#' - Selecting and arranging stops by trip and sequence
#'
#' - Connecting stops with line segments to form a path for each trip
#'
#' - Grouping unique paths into distinct shape IDs
#'
#' @note
#' This approximation may not perfectly represent real-world shapes, especially for complex or curved routes.
#' `get_shapes()` uses stop sequences to recriate the shapes table; accordingly, it should not be used after `filter_time()`, as this function removes invalid `stop_times`.
#'
#' @examples
#' # Generate a shapes table for a GTFS object
#' gtfs_with_shapes <- get_shapes(gtfs = for_rail_gtfs)
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_shapes_df()]
#'
#' @importFrom dplyr select arrange group_by left_join mutate reframe summarise
#' @importFrom sf st_as_sf st_cast st_combine
#' @importFrom tidyr unnest
#' @importFrom crayon cyan red
#' @export

get_shapes <- function(gtfs){

  message(crayon::cyan('get_shapes()'), ' reconstructs the shapes table using euclidean approximation, based on the coordinates and sequence of stops for each trip, and', crayon::red(' may not be accurate'), '.')

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(!purrr::is_null(gtfs$shapes)){
    warning('This gtfs object already contains a shapes table. ', crayon::cyan('get_shapes()'), ' will', crayon::red(" overwrite"), ' it.')
  }

  shapes.dic <-
    gtfs$stop_times %>%
    dplyr::select(trip_id, stop_id, stop_sequence) %>%
    dplyr::arrange(trip_id, stop_sequence) %>%
    dplyr::left_join(gtfs$stops %>%
                       GTFSwizard::get_stops_sf() %>%
                       dplyr::select(stop_id),
                     by = dplyr::join_by(stop_id)
    ) %>%
    sf::st_as_sf(crs = 4326) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::arrange(stop_sequence) %>%
    dplyr::reframe(geometry = sf::st_cast(geometry, to = 'LINESTRING', ids = trip_id)) %>%
    dplyr::left_join(gtfs$trips %>%
                       dplyr::select(trip_id, route_id),
                     by = dplyr::join_by(trip_id)
    ) %>%
    dplyr::group_by(geometry) %>%
    dplyr::reframe(trip_id = list(trip_id)) %>%
    dplyr::mutate(shape_id = paste0('shape-', 1:n()))

  gtfs$shapes <-
    shapes.dic[, c(3, 1)] %>%
    sf::st_as_sf()

  gtfs$trips <-
    gtfs$trips %>%
    dplyr::select(-shape_id) %>%
    dplyr::left_join(shapes.dic %>%
                       dplyr::select(-geometry) %>%
                       tidyr::unnest(cols = 'trip_id'),
                     dplyr::join_by(trip_id))

  gtfs$shapes <-
    GTFSwizard::get_shapes_df(gtfs$shapes)

  return(gtfs)

}
