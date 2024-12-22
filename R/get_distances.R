#' Calculate Distances in GTFS Data
#'
#' The `get_distances` function calculates distances within a `wizardgtfs` object based on various methods.
#' Depending on the `method` chosen, it can calculate average route distances, trip-specific distances, or detailed distances between stops.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If it is not of this class, it will be converted.
#' @param method A character string indicating the calculation method. Choices are:
#'   \describe{
#'     \item{"by.route"}{Calculates average distances for each route.}
#'     \item{"by.trip"}{Calculates distances for each trip, associating each trip ID with its total distance.}
#'     \item{"detailed"}{Calculates detailed distances between each consecutive stop for all trips. This is the most computationally intensive option and may take several minutes to complete.}
#'   }
#' @param trips A character vector of trip IDs to consider. When set to `all`, includes all trips.
#'
#' @return A data frame with calculated distances based on the specified method:
#'   \describe{
#'     \item{If `method = "by.route"`}{Returns a summary with columns: `route_id`, `trips`, `average.distance`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "by.trip"`}{Returns a data frame with columns: `route_id`, `trip_id`, `distance`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "detailed"`}{Returns a data frame with columns: `shape_id`, `from_stop_id`, `to_stop_id`, and `distance`.}
#'   }
#'
#' @details
#' The function calls specific sub-functions based on the selected method:
#'
#' - "by.route": Calculates average distances per route.
#'
#' - "by.trip": Calculate distances per trip.
#'
#' - "detailed": Calculates detailed stop-to-stop distances within each route. Note that this method may be slow for large datasets.
#'
#' If an invalid `method` is provided, the function defaults to `"by.route"` and issues a warning.
#'
#' @examples
#' # Calculate average route distances
#' distances_by_route <- get_distances(gtfs = for_rail_gtfs, method = "by.route", trips = 'all')
#'
#' # Calculate distances by trip
#' distances_by_trip <- get_distances(gtfs = for_rail_gtfs, method = "by.trip", trips = 'all')
#'
#' \donttest{
#' # Calculate detailed distances between stops
#' detailed_distances <- get_distances(gtfs = for_rail_gtfs, method = "detailed", trips = 'all')
#' }
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
#'
#' @importFrom dplyr mutate group_by reframe select left_join filter
#' @importFrom sf st_length
#' @export
get_distances <- function(gtfs, method = 'by.route', trips = 'all'){

  sf::sf_use_s2(FALSE)

  if(!any(trips == 'all')) {gtfs <- GTFSwizard::filter_trip(gtfs, trip = trips)}

  if (method == 'by.route') {
    distances <- get_distances_byroute(gtfs)
  }

  if (method == 'by.trip') {
    distances <- get_distances_bytrip(gtfs)
  }

  if (method == 'detailed') {
    message('This operation may take several minutes to complete.')
    distances <- get_distances_detailed(gtfs)
  }

  if (!method %in% c('by.route',
                     'by.trip',
                     'detailed')) {
    distances <- get_distances_byroute(gtfs)
    warning('\n"method" should be one of "by.route", "by.trip" or "detailed".\nReturning "method = by.route"".')
  }

  return(distances)

}

get_distances_byroute <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(purrr::is_null(gtfs$shapes)){

    gtfs <- GTFSwizard::get_shapes(gtfs)

    message('This gtfs object does not contain a shapes table. Using ', crayon::cyan('get_shapes()'), '.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  distances <-
    GTFSwizard::get_shapes_sf(gtfs$shapes) %>%
    dplyr::mutate(distance = sf::st_length(geometry))

  distances <-
    gtfs$trips %>%
    dplyr::left_join(distances, by = 'shape_id') %>%
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    dplyr::group_by(route_id, service_pattern, pattern_frequency) %>%
    dplyr::reframe(average.distance = mean(distance, na.rm = TRUE),
                   trips = n()) %>%
    dplyr::select(route_id, trips, average.distance, service_pattern, pattern_frequency)

  return(distances)

}

get_distances_bytrip <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(purrr::is_null(gtfs$shapes)){

    gtfs <- GTFSwizard::get_shapes(gtfs)

    message('This gtfs object does not contain a shapes table. Using get_shapes().')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  distances <-
    GTFSwizard::get_shapes_sf(gtfs$shapes) %>%
    dplyr::mutate(distance = sf::st_length(geometry))

  distances <-
    gtfs$trips %>%
    dplyr::left_join(distances, by = 'shape_id') %>%
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    dplyr::select(route_id, trip_id, distance, service_pattern, pattern_frequency)

  return(distances)

}

get_distances_detailed <- function(gtfs){

  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  if(purrr::is_null(gtfs$shapes)){

    gtfs <- GTFSwizard::get_shapes(gtfs)

    message('This gtfs object', crayon::red(' does not'), ' contain a shapes table. Using', crayon::cyan(' get_shapes()'), '.')
  }

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  pairs <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '') %>%
    dplyr::arrange(trip_id, stop_sequence) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::reframe(from_stop_id = stop_id,
                   to_stop_id = dplyr::lead(stop_id)) %>%
    na.omit%>%
    dplyr::left_join(gtfs$trips %>% dplyr::select(trip_id, shape_id), by = 'trip_id') %>%
    dplyr::group_by(from_stop_id, to_stop_id, shape_id) %>%
    dplyr::reframe(trips = list(trip_id))

  shapes_stops <-
    pairs %>%
    dplyr::group_by(shape_id, from_stop_id) %>%
    dplyr::reframe(to_stop_id = list(to_stop_id)) %>%
    dplyr::group_by(shape_id) %>%
    dplyr::reframe(from_stop_id = list(from_stop_id),
                   to_stop_id = list(to_stop_id))

  dist_matrix <- NULL

  pb <- txtProgressBar(max = nrow(shapes_stops),
                       style = 3)

  for (i in 1:nrow(shapes_stops)) {

    max.length <- 25

    suppressWarnings({shapes.sf <-
      gtfs$shapes %>%
      dplyr::filter(shape_id == shapes_stops$shape_id[i]) %>%
      GTFSwizard::get_shapes_sf() %>%
      stplanr::line_segment(
        segment_length = max.length,
        n_segments = NA,
        use_rsgeo = NULL,
        debug_mode = FALSE
      ) %>%
      sf::st_cast(., 'POINT')  %>%
      tidyr::unnest(cols = 'geometry')
    })

    suppressWarnings({network <-
      sfnetworks::as_sfnetwork(shapes.sf, directed = FALSE)})

    origins <-
      shapes_stops %>%
      dplyr::filter(shape_id == shapes_stops$shape_id[i]) %>%
      tidyr::unnest(cols = c('from_stop_id', 'to_stop_id'))

    routes <- NULL

    for (j in 1:nrow(origins)) {

      origin <-
        gtfs$stops %>%
        dplyr::filter(stop_id == origins$from_stop_id[j]) %>%
        GTFSwizard::get_stops_sf() %>%
        dplyr::select(stop_id)

      destinations_ids <-
        origins[j, ] %>%
        tidyr::unnest(cols = 'to_stop_id') %>%
        .$to_stop_id

      destinations <-
        gtfs$stops %>%
        dplyr::filter(stop_id %in% destinations_ids) %>%
        tidytransit::stops_as_sf() %>%
        dplyr::select(stop_id)

      suppressMessages({shortest_edges <-
        destinations %>%
        dplyr::bind_cols(
          sfnetworks::st_network_paths(network, origin, destinations) %>%
            dplyr::select(edge_paths)
        )})

      distances <- NULL

      for (k in 1:nrow(shortest_edges)) {

        suppressMessages({distance <-
          network %>%
          sfnetworks::activate(edges) %>%
          dplyr::slice(shortest_edges %>%
                         tibble::tibble() %>%
                         .[k, 3] %>%
                         unlist) %>%
          sf::st_as_sf() %>%
          sf::st_union() %>%
          sf::st_length()})

        distances <-
          distances %>%
          c(., distance)

      }

      routes <-
        routes %>%
        dplyr::bind_rows(
          tibble::tibble(
            shape = shapes_stops$shape_id[i],
            origins = origin %>% tibble::tibble() %>% .[1,1],
            destinations = destinations_ids,
            distance = distances

          )
        )

    }

    dist_matrix <-
      dist_matrix %>%
      dplyr::bind_rows(., routes)

    setTxtProgressBar(pb, i)

  }

  dist_matrix <-
    dist_matrix %>%
    tidyr::unnest(cols = origins) %>%
    stats::setNames(c('shape_id', 'from_stop_id', 'to_stop_id', 'distance'))

  return(dist_matrix)

}
