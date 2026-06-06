#' Infer Straight-Line Shapes from Stop Sequences
#'
#' Builds one shape for each unique ordered stop pattern and assigns it to the
#' corresponding trips.
#'
#' @param gtfs A GTFS object.
#'
#' @return A `wizardgtfs` object with a new `shapes` table and updated
#'   `trips$shape_id`.
#'
#' @details
#' Consecutive stop coordinates are connected directly. The result is useful
#' for visualization and approximate distance analysis, but it is not a
#' map-matched representation of the vehicle path.
#'
#' @examples
#' gtfs_with_shapes <- get_shapes(for_rail_gtfs)
#'
#' @seealso [GTFSwizard::get_shapes_sf()], [GTFSwizard::get_shapes_df()]
#' @export
get_shapes <- function(gtfs){
  gtfs <- ensure_wizardgtfs(gtfs)
  if(!is.null(gtfs$shapes)){
    gw_warn("overwriting the existing `shapes` table with inferred shapes.")
  } else {
    gw_msg("building straight-line shapes from ordered stop coordinates.")
  }

  calls <- gtfs$stop_times |>
    dplyr::arrange(trip_id, stop_sequence) |>
    dplyr::select(trip_id, stop_id, stop_sequence) |>
    dplyr::left_join(
      gtfs$stops |> dplyr::select(stop_id, stop_lon, stop_lat),
      by = "stop_id"
    )
  if(any(!stats::complete.cases(calls$stop_lon, calls$stop_lat))){
    gw_stop("all stops used by trips need valid coordinates to infer shapes.")
  }
  if(any(table(calls$trip_id) < 2L)){
    gw_stop("each trip needs at least two stops to infer a shape.")
  }
  patterns <- calls |>
    dplyr::group_by(trip_id) |>
    dplyr::summarise(
      .signature = paste(stop_id, collapse = "\r"),
      .groups = "drop"
    )
  signatures <- unique(patterns$.signature)
  patterns$shape_id <- paste0("shape-", match(patterns$.signature, signatures))

  representative <- patterns |>
    dplyr::group_by(shape_id) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::select(shape_id, trip_id)
  shape_points <- dplyr::left_join(representative, calls, by = "trip_id") |>
    dplyr::arrange(shape_id, stop_sequence) |>
    dplyr::group_by(shape_id) |>
    dplyr::mutate(
      shape_pt_sequence = dplyr::row_number(),
      shape_dist_traveled = c(
        0,
        cumsum(haversine_m(
          stop_lon[-dplyr::n()], stop_lat[-dplyr::n()],
          stop_lon[-1L], stop_lat[-1L]
        ))
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      shape_id, shape_pt_lat = stop_lat, shape_pt_lon = stop_lon,
      shape_pt_sequence, shape_dist_traveled
    )

  gtfs$shapes <- shape_points
  gtfs$trips <- gtfs$trips |>
    dplyr::select(-dplyr::any_of("shape_id")) |>
    dplyr::left_join(
      patterns |> dplyr::select(trip_id, shape_id),
      by = "trip_id"
    )
  gtfs
}
