#' Identify and Extract Transit Corridors
#'
#' The `get_corridor` function identifies and extracts high-density transit corridors based on trip frequency between stops. It groups segments into connected corridors, and filters them based on a minimum length criterion.
#'
#' @param gtfs A GTFS object, preferably of class `wizardgtfs`. If not, the function will attempt to convert it using `GTFSwizard::as_wizardgtfs()`.
#' @param i A numeric value representing the percentile threshold for selecting high-density segments. Defaults to `0.01` (top 1\% of segments by trip frequency).
#' @param min.lenght A numeric value specifying the minimum corridor length (in meters) to retain. Defaults to `1500`.
#'
#' @return An `sf` object containing the following columns:
#' \describe{
#'   \item{corridor}{A unique identifier for each corridor, prefixed with "corridor-".}
#'   \item{stops}{A list of stop IDs included in each corridor.}
#'   \item{length}{The total length of the corridor, in meters.}
#'   \item{geometry}{The spatial representation of the corridor as an `sf` linestring object.}
#' }
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Filters and orders `stop_times` data to identify consecutive stops (`stop_from` and `stop_to`) for each trip.
#'   \item Counts the number of trips between each stop pair and selects the top `i` percentile of segments by trip frequency.
#'   \item Groups spatially connected segments into corridors using graph theory and adjacency matrices.
#'   \item Filters corridors by the minimum length (`min.lenght`).
#'   \item Returns the resulting corridors with their metadata and geometry.
#'   }
#'
#' @note The function uses `sf` and `igraph` for spatial and graph-based computations. Ensure the `gtfs` object includes `stop_times` table.
#'
#' @examples
#' corridors <- get_corridor(for_bus_gtfs, i = 0.02, min.lenght = 2000)
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()]
#'
#' @importFrom dplyr filter arrange select group_by mutate reframe
#' @importFrom sf st_as_sf st_distance st_touches st_length st_union
#' @importFrom igraph graph_from_adj_list components
#' @importFrom stats na.omit
#' @export
get_corridor <- function(gtfs, i = .01, min.lenght = 1500) {

  # Validate input parameters
  if (!is.numeric(i) || i <= 0 || i >= 1) {
    stop('Parameter ', crayon::cyan('i'), ' must be a numeric value between 0 and 1 (', crayon::red('exclusive'), ').')
  }
  if (!is.numeric(min.lenght) || min.lenght <= 0) {
    stop('Parameter ', crayon::cyan('min.lenght'), ' must be a ', crayon::green('positive'), ' numeric value (in meters).')
  }

  if(!"wizardgtfs" %in% class(gtfs)){
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
  }

  stops_sf <- get_stops_sf(gtfs$stops)

  transit_data <-
    gtfs$stop_times %>%
    dplyr::filter(arrival_time != '') %>%
    dplyr::arrange(trip_id, arrival_time) %>%
    dplyr::select(trip_id, stop_from = stop_id) %>%
    dplyr::mutate(stop_to = dplyr::lead(stop_from)) %>%
    stats::na.omit() %>%
    dplyr::group_by(stop_from, stop_to) %>%
    dplyr::reframe(trips = n()) %>%
    dplyr::filter(dplyr::percent_rank(trips) >= (1 - i)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(origin = stops_sf$geometry[stops_sf$stop_id == stop_from],
           destination = stops_sf$geometry[stops_sf$stop_id == stop_to],
           ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(geometry = purrr::map2(origin, destination, ~sf::st_sfc(sf::st_linestring(rbind(sf::st_coordinates(.x), sf::st_coordinates(.y))), crs = 4326))) %>%
    dplyr::select(-origin, -destination) %>%
    tidyr::unnest(cols = 'geometry') %>%
    sf::st_as_sf()

  adjacency_matrix <- sf::st_touches(transit_data$geometry) # Step 1: Create a spatial adjacency matrix using 'st_touches'

  graph <- igraph::graph_from_adj_list(adjacency_matrix, mode = "all") # Step2: Build an undirected graph from the adjacency matrix

  components <- igraph::components(graph) # Step 3: Identify connected components in the graph

  transit_data$group_id <- components$membership # Step 4: Add the component ID as a new column to your data

  transit_data <-
    transit_data %>%
    dplyr::group_by(group_id) %>%
    dplyr::reframe(geometry = sf::st_union(geometry),
                   stops = list(c(unique(stop_from), unique(stop_to)))
    ) %>%
    dplyr::mutate(length = sf::st_length(geometry)) %>%
    dplyr::filter(as.numeric(length) >= min.lenght) %>%
    dplyr::arrange(-length) %>%
    dplyr::mutate(corridor = paste0('corridor-', 1:n())) %>%
    dplyr::select(corridor, stops, length, geometry) %>%
    sf::st_as_sf()

  return(transit_data)

}
