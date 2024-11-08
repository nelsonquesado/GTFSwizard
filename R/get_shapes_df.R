#' Convert Shape Geometries to GTFS Shape Points Data Frame
#'
#' The `get_shapes_df` function converts a spatial object of shapes (with geometry) into a GTFS-compliant `shapes` data frame format, detailing latitude, longitude, point sequence, and cumulative distance traveled along each shape.
#'
#' @param shape A spatial (`sf`) object containing shapes, with `shape_id` and geometry information.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{`shape_id`}{Unique identifier for each shape.}
#'     \item{`shape_pt_lon`}{Longitude coordinates of each shape point.}
#'     \item{`shape_pt_lat`}{Latitude coordinates of each shape point.}
#'     \item{`shape_pt_sequence`}{Sequence of points along each shape.}
#'     \item{`shape_dist_traveled`}{Cumulative distance traveled along the shape in meters.}
#'   }
#'
#' @details
#' The function performs the following steps:
#'
#' - Validates that the `shape` object is of class `sf` and contains a `shape_id` column.
#'
#' - Extracts point coordinates from each shape’s geometry, creating a sequence of latitude and longitude points.
#'
#' - Computes cumulative distances along the shape, using Euclidean distance between consecutive points.
#'
#' The resulting data frame conforms to the GTFS `shapes.txt` format. Distances are expressed in meters.
#'
#' @examples
#' # Convert a shape geometry to a GTFS-compliant shapes data frame
#' shape <- get_shapes_sf(for_rail_gtfs$shapes)
#' shapes_df <- get_shapes_df(shape = shape)
#'
#' @seealso
#' [GTFSwizard::get_shapes()], [GTFSwizard::get_shapes_sf()]
#'
#' @importFrom dplyr select mutate group_by ungroup
#' @importFrom tidyr unnest tibble
#' @importFrom sf st_as_sf st_coordinates st_distance
#' @importFrom data.table data.table
#' @export


get_shapes_df <- function(shape){

  if(!'sf' %in% class(shape)){
    stop(crayon::cyan('shape'), ' is not a ', crayon::cyan('simple feature'), ' object.')
  }

  if(purrr::is_null(shape$shape_id)){
    stop(crayon::cyan('shape'), ' does not contains the ', crayon::cyan('shape_id'), ' column.')
  }

  x <- 0
  units(x) <- 'm'

  shapes_df <-
    shape %>%
    select(shape_id) %>%
    dplyr::as_tibble() %>%
    stats::setNames(c('shape_id', 'geometry')) %>%
    dplyr::group_by(shape_id) %>%
    dplyr::mutate(geometry = list(st_coordinates(geometry) %>% .[, -3])) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(cols = geometry) %>%
    data.table::data.table() %>%
    stats::setNames(c('shape_id', 'shape_pt_lon', 'shape_pt_lat')) %>%
    dplyr::group_by(shape_id) %>%
    st_as_sf(coords = c('shape_pt_lon', 'shape_pt_lat'), remove = FALSE, crs = 4326) %>%
    dplyr::mutate(shape_pt_sequence = 1:n(),
                  shape_dist_traveled = abs(sf::st_distance(geometry, lag(geometry), by_element = TRUE)) %>%
                    tidyr::replace_na(x) %>%
                    cumsum %>%
                    as.numeric()) %>%
    tidyr::tibble() %>%
    dplyr::select(-geometry)

  return(shapes_df)

  message(crayon::cyan('shape_dist_traveled'), ' unit is meters [m].')

}
