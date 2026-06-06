#' Identify Stops Serving Multiple Routes
#'
#' @param gtfs A GTFS object.
#'
#' @return A WGS84 `sf` object with one row per served stop, list-columns
#'   `trip_id` and `route_id`, counts `n_trip` and `n_routes`, and point
#'   geometry.
#'
#' @examples
#' get_hubs(for_rail_gtfs)
#'
#' @seealso [GTFSwizard::plot_hubs()], [GTFSwizard::get_stops_sf()]
#' @export
get_hubs <- function(gtfs){
  gtfs <- ensure_wizardgtfs(gtfs)
  usage <- gtfs$stop_times |>
    dplyr::distinct(stop_id, trip_id) |>
    dplyr::left_join(
      gtfs$trips |> dplyr::select(trip_id, route_id),
      by = "trip_id"
    ) |>
    dplyr::group_by(stop_id) |>
    dplyr::summarise(
      n_trip = dplyr::n_distinct(trip_id),
      n_routes = dplyr::n_distinct(route_id),
      trip_id = list(unique(trip_id)),
      route_id = list(unique(route_id)),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(n_routes), dplyr::desc(n_trip))
  dplyr::left_join(
    get_stops_sf(gtfs$stops),
    usage,
    by = "stop_id"
  ) |>
    dplyr::filter(!is.na(n_trip)) |>
    dplyr::select(stop_id, trip_id, route_id, n_trip, n_routes, geometry) |>
    dplyr::arrange(dplyr::desc(n_routes), dplyr::desc(n_trip))
}
