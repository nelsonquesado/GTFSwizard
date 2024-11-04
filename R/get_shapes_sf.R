#' Convert GTFS Shapes Table to Simple Features (sf) Format
#'
#' `get_shapes_sf` converts the shapes table in a `wizardgtfs` object into a simple features (`sf`) object, making it suitable for spatial analysis. This function checks and processes the `shapes` data in the provided GTFS object and structures it as `LINESTRING` features.
#'
#' @param gtfs A GTFS object containing the `shapes` table or the shape table itself. If the `shapes` table is missing, it will be created using `get_shapes()`.
#'
#' @return An `sf` object with shapes as `LINESTRING` geometries:
#'
#' @details
#' - When the input `wizardgtfs` object lacks a `shapes` table, the function automatically generates one using `get_shapes()`.
#'
#' - The `shapes` table in the GTFS object are transformed into `LINESTRING` geometries. If `shape_pt_sequence` is absent, the points are treated as ordered by their position in the data.
#'
#' - If `shape_dist_traveled` is available, cumulative distance calculations are included for each shape point.
#'
#' @note
#' If `shape_pt_sequence` is missing, the function will assume that points are ordered, constructing the shape accordingly.
#'
#' @examples
#' \dontrun{
#' # Convert shapes data in a GTFS object to sf format
#' gtfs_sf <- get_shapes_sf(for_gtfs)
#' }
#'
#' @seealso
#' [GTFSwizard::get_shapes()], [GTFSwizard::get_shapes_df()]
#'
#' @importFrom dplyr select mutate group_by ungroup arrange reframe
#' @importFrom sf st_as_sf st_crs
#' @importFrom tibble as_tibble
#' @importFrom crayon blue red
#' @export
get_shapes_sf <- function(gtfs){
  UseMethod('get_shapes_sf')
}

#' @exportS3Method GTFSwizard::get_shapes_sf
get_shapes_sf.wizardgtfs <- function(gtfs){
  if('shapes' %in% names(gtfs) == FALSE){
    warning("GTFS doesn't have a shapes table. Using ", crayon::blue("get_shapes"), " to build it")
    gtfs <- get_shapes(gtfs)
  }
  gtfs$shapes <- get_shapes_sf.data.frame(gtfs$shapes)
  return(gtfs)
}

#' @exportS3Method GTFSwizard::get_shapes_sf
get_shapes_sf.list <- function(gtfs){
  if('shapes' %in% names(gtfs) == FALSE){
    warning("GTFS doesn't have a shapes table, using ", crayon::blue("get_shapes"), " to build it")
    gtfs <- get_shapes(gtfs)
  }
  gtfs$shapes <- get_shapes_sf.data.frame(gtfs$shapes)
  return(gtfs)
}

#' @exportS3Method GTFSwizard::get_shapes_sf
get_shapes_sf.gtfs <- function(gtfs){
  if('shapes' %in% names(gtfs) == FALSE){
    warning("GTFS doesn't have a shapes table, using ", crayon::blue("get_shapes"), " to build it")
    gtfs <- get_shapes(gtfs)
  }
  gtfs$shapes <- get_shapes_sf.data.frame(gtfs$shapes)
  return(gtfs)
}

#' @exportS3Method GTFSwizard::get_shapes_sf
get_shapes_sf.data.frame <- function(gtfs){
  if('sf'%in%class(gtfs)){
    st_crs(gtfs) <- 4326
    return(gtfs)
  }else{

    if('shape_pt_sequence' %in% names(gtfs)){
      gtfs <- gtfs %>%
        dplyr::mutate(shape_pt_sequence = as.numeric(shape_pt_sequence)) %>%
        dplyr::arrange(shape_id,shape_pt_sequence)
    }else{
      warning("When the ", crayon::cyan('"shape_pt_sequence"')," column is not defined, the line will be built considering the points in order.")
    }

    if('shape_dist_traveled' %in% names(gtfs)){
      gtfs <- gtfs %>%
        dplyr::group_by(shape_id) %>%
        dplyr::mutate(geometry = paste0(shape_pt_lon,' ',shape_pt_lat)) %>%
        dplyr::group_by(shape_id) %>%
        dplyr::reframe(
          shape_dist_traveled = sum(as.numeric(shape_dist_traveled),na.rm = T),
          geometry = paste0('LINESTRING(',paste0(geometry,collapse = ', '), ')')
        ) %>%
        sf::st_as_sf(wkt = 'geometry',crs=4326) %>%
        tibble::as_tibble() %>%
        sf::st_as_sf()

    }else{
      gtfs <- gtfs %>%
        dplyr::group_by(shape_id) %>%
        dplyr::mutate(geometry = paste0(shape_pt_lon,' ',shape_pt_lat)) %>%
        dplyr::group_by(shape_id)
        dplyr::reframe(
          geometry = paste0('LINESTRING(',paste0(geometry,collapse = ', '), ')')
        ) %>%
        sf::st_as_sf(wkt = 'geometry',crs=4326) %>%
          tibble::as_tibble() %>%
          sf::st_as_sf()
    }

    return(gtfs)
  }
}
