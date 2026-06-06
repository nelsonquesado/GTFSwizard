#' Convert Shape Geometries to a GTFS Shapes Table
#'
#' @param shape An `sf` object with a `shape_id` field and line geometries.
#'
#' @return A tibble containing `shape_id`, WGS84 point coordinates,
#'   `shape_pt_sequence`, and cumulative `shape_dist_traveled` in meters.
#'
#' @details
#' Distances are geodesic approximations between consecutive coordinates.
#' GTFS permits any consistent distance unit; this function uses meters.
#'
#' @examples
#' shapes_sf <- get_shapes_sf(for_rail_gtfs)$shapes
#' shapes_df <- get_shapes_df(shapes_sf)
#'
#' @references
#' [GTFS Schedule Reference](https://gtfs.org/documentation/schedule/reference/#shapestxt)
#' @seealso [GTFSwizard::get_shapes_sf()], [GTFSwizard::get_shapes()]
#' @export
get_shapes_df <- function(shape){
  if(!inherits(shape, "sf")){
    gw_stop("`shape` must be an `sf` object.")
  }
  if(!"shape_id" %in% names(shape)){
    gw_stop("`shape` must contain `shape_id`.")
  }
  if(is.na(sf::st_crs(shape))){
    gw_stop("`shape` must have a defined coordinate reference system.")
  }
  shape <- sf::st_transform(shape, 4326)

  rows <- vector("list", nrow(shape))
  for(i in seq_len(nrow(shape))){
    coordinates <- sf::st_coordinates(sf::st_geometry(shape)[[i]])[, 1:2, drop = FALSE]
    rows[[i]] <- tibble::tibble(
      shape_id = as.character(shape$shape_id[i]),
      shape_pt_lon = coordinates[, 1],
      shape_pt_lat = coordinates[, 2]
    )
  }
  points <- dplyr::bind_rows(rows)
  points |>
    dplyr::group_by(shape_id) |>
    dplyr::mutate(
      shape_pt_sequence = dplyr::row_number(),
      shape_dist_traveled = c(
        0,
        cumsum(haversine_m(
          shape_pt_lon[-dplyr::n()], shape_pt_lat[-dplyr::n()],
          shape_pt_lon[-1L], shape_pt_lat[-1L]
        ))
      )
    ) |>
    dplyr::ungroup()
}
