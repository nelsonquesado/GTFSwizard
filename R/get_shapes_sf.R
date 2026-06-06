#' Convert GTFS Shapes to Simple Features
#'
#' @param gtfs A GTFS object or a `shapes` data frame.
#'
#' @return For a data frame, one WGS84 `LINESTRING` feature per `shape_id`.
#'   For a GTFS object, the same object with its `shapes` table replaced by that
#'   `sf` object. If present, `shape_dist_traveled` is summarized as its maximum
#'   value for each shape.
#'
#' @examples
#' gtfs_sf <- get_shapes_sf(for_rail_gtfs)
#' shapes_sf <- get_shapes_sf(for_rail_gtfs$shapes)
#'
#' @seealso [GTFSwizard::get_shapes_df()], [GTFSwizard::get_shapes()]
#' @export
get_shapes_sf <- function(gtfs){
  UseMethod("get_shapes_sf")
}

#' @exportS3Method GTFSwizard::get_shapes_sf wizardgtfs
get_shapes_sf.wizardgtfs <- function(gtfs){
  if(is.null(gtfs$shapes)){
    gw_warn("input has no `shapes` table; building straight-line shapes.")
    gtfs <- get_shapes(gtfs)
  }
  gtfs$shapes <- get_shapes_sf.data.frame(gtfs$shapes)
  gtfs
}

#' @exportS3Method GTFSwizard::get_shapes_sf list
get_shapes_sf.list <- function(gtfs){
  if(is.null(gtfs$shapes)){
    gtfs <- get_shapes(ensure_wizardgtfs(gtfs))
  }
  gtfs$shapes <- get_shapes_sf.data.frame(gtfs$shapes)
  gtfs
}

#' @exportS3Method GTFSwizard::get_shapes_sf gtfs
get_shapes_sf.gtfs <- get_shapes_sf.list

#' @exportS3Method GTFSwizard::get_shapes_sf data.frame
get_shapes_sf.data.frame <- function(gtfs){
  if(inherits(gtfs, "sf")){
    if(is.na(sf::st_crs(gtfs))){
      gw_stop("the shapes `sf` object must have a defined CRS.")
    }
    return(sf::st_transform(gtfs, 4326))
  }
  required <- c("shape_id", "shape_pt_lon", "shape_pt_lat")
  missing <- setdiff(required, names(gtfs))
  if(length(missing)){
    gw_stop("the shapes table is missing: ", paste(missing, collapse = ", "), ".")
  }
  if("shape_pt_sequence" %in% names(gtfs)){
    gtfs <- gtfs[order(gtfs$shape_id, as.numeric(gtfs$shape_pt_sequence)), ]
  } else {
    gw_warn("`shape_pt_sequence` is absent; using the current row order.")
  }

  groups <- split(gtfs, gtfs$shape_id)
  if(any(vapply(groups, nrow, integer(1)) < 2L)){
    gw_stop("each shape requires at least two points.")
  }
  geometry <- lapply(groups, function(x){
    sf::st_linestring(as.matrix(x[, c("shape_pt_lon", "shape_pt_lat")]))
  })
  attributes <- tibble::tibble(shape_id = names(groups))
  if("shape_dist_traveled" %in% names(gtfs)){
    attributes$shape_dist_traveled <- vapply(
      groups,
      function(x){
        distance <- suppressWarnings(as.numeric(x$shape_dist_traveled))
        if(all(is.na(distance))){
          return(NA_real_)
        }
        max(distance, na.rm = TRUE)
      },
      numeric(1)
    )
  }
  sf::st_sf(attributes, geometry = sf::st_sfc(geometry, crs = 4326))
}
