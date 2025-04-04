



# Auxiliar functions ------------------------------------------------------

`%intersects%` <- function(x,y){
  x_ <- st_sfc(unique(x),crs = 4326)
  res_match <- st_intersects(x_,y,sparse = FALSE) %>%
    apply( MARGIN = 1, FUN = any)
  return(res_match[match(x,x_)])
}

`%touches%` <- function(x,y){
  x_ <- st_sfc(unique(x),crs = 4326)
  res_match <- st_touches(x_,y,sparse = FALSE) %>%
    apply( MARGIN = 1, FUN = any)
  return(res_match[match(x,x_)])
}

`%crosses%` <- function(x,y){
  st_crosses(x,y,sparse = FALSE) %>%
    apply( MARGIN = 1, FUN = any)
}

`%within%` <- function(x,y){
  st_within(x,y,sparse = FALSE) %>%
    apply( MARGIN = 1, FUN = any)
}

`%contains%` <- function(x,y){
  st_contains(x,y,sparse = FALSE) %>%
    apply( MARGIN = 1, FUN = any)
}

`%overlaps%` <- function(x,y){
  st_overlaps(x,y,sparse = FALSE) %>%
    apply( MARGIN = 1, FUN = any)
}

`%equals%` <- function(x,y){
  st_equals(x,y,sparse = FALSE) %>%
    apply( MARGIN = 1, FUN = any)
}

`%nin%` <- function(x,y){
  x %in% y == FALSE
}


# Main Function ----------------------------------------------------------------
#' Select Subsets of GTFS Data
#'
#' The “selection” function makes a selection in the GTFS file without altering or filtering the GTFS file.
#'
#' @param gtfs An object representing GTFS data. It can be a list or a `wizardgtfs` class gtfsect.
#' @param ... Expressions used to filter the data within `gtfs`. The expressions can operate on four GTFS variables:
#'   \describe{
#'     \item{\code{"stop_id"}}{Select the GTFS by stops using a vector of stop_id, must be character.}
#'     \item{\code{"route_id"}}{Select the GTFS by routes using a vector of route_id, must be character.}
#'     \item{\code{"trip_id"}}{Select the GTFS by trip using a vector of trip_id, must be character.}
#'     \item{\code{"geometry"}}{Select the GTFS by stops using an `sf`, `sfc`, or `sfg` object. The geometry predicate function is evaluated with the geometry of the GTFS stops. Available predicates are:
#'
#'              \code{\%intersects\%}
#'
#'              \code{\%touches\%}
#'
#'              \code{\%within\%}
#'
#'              \code{\%equals\%}
#'
#'              \code{\%overlaps\%}
#'
#'              \code{\%contains\%}.}
#'   }
#' @param add A logical argument. If `TRUE`, appends the new selection to existing ones in the gtfsect; otherwise, creates a new selection.
#'
#' @return A `wizardgtfs_selected` wizardgtfs, which is a modified version of the original attributes with the selections applied. If the expression yields no matches, returns the original gtfs unchanged.
#'
#' @details The function evaluates the provided expressions in an environment restricted to recognized variables (`stop_id`, `route_id`, `trip_id`, `geometry`). An error is thrown if an unrecognized variable is used, indicating that only specific variables are allowed.
#'
#' @examples
#' # Apply the selection function
#' result <- selection(for_rail_gtfs,
#'  stop_id == for_rail_gtfs$stops$stop_id[1] & trip_id %in% for_rail_gtfs$trips$trip_id[1:5])
#'
#' # Check the selection
#' class(result)
#' attr(result, 'selection')
#'
#' # Use geometry selection
#' bbox <- sf::st_bbox(c(
#'   xmin = -38.57219059002416,
#'   ymin = -3.7999496173114118,
#'   xmax = -38.50455165901261,
#'   ymax = -3.756631724636505
#' ),
#' crs = sf::st_crs(4326))  # Set CRS to WGS 84
#'
#' # Convert the bounding box to a polygon
#' polygon <- sf::st_as_sfc(bbox)
#'
#' result <- selection(for_rail_gtfs, geometry %intersects% polygon)
#'
#' @importFrom magrittr %>%
#'
#' @rdname selection
#' @aliases selection
#' @export

selection <- function(gtfs, ..., add = FALSE){

  expr <- substitute(...)

  tryCatch(
    eval(expr,
         list('stop_id'=character(0),'route_id'=character(0),
           'trip_id'=character(0),'geometry'=st_sfc(st_point(c(0,0)),crs = 4326) )),
    error = function(e){
      e <- as.character(e)
      gtfs <- regmatches(e, regexpr("(?<=gtfseto ')[^']+", e, perl = TRUE))
      stop(e,"Variables must be a subset of {'stop_id','route_id','trip_id','geometry'}, but has additional elements {'",gtfs,"'}")
    }
  )

  UseMethod('selection')
}

#' @exportS3Method GTFSwizard::selection list
selection.list <- function(gtfs,...,add = FALSE){
  gtfs <- as_wizardgtfs(gtfs)
  selection.wizardgtfs(gtfs,...,add)
}

#' @exportS3Method GTFSwizard::selection wizardgtfs
selection.wizardgtfs <- function(gtfs,...,add = FALSE){

  if(add){
    message('add=TRUE, but there is no selection in the gtfsect')
  }

  expr <- substitute(...)

  variables <- dplyr::enquos(..., .ignore_empty = 'all') %>%
    sapply(rlang::quo_get_expr) %>% as.character() %>%
    strsplit('&|\\|') %>% unlist() %>%
    stringr::str_trim() %>% sapply(function(x) unlist(strsplit(x,' '))[1])

  attr(gtfs,'selection_expr') <- expr

  if('geometry' %nin% variables){

    stop_times <- dplyr::left_join(
      gtfs$stop_times,
      gtfs$trips[,c('trip_id','route_id')],
      by = 'trip_id'
    )

    selection <- eval(expr,stop_times)
    if(sum(selection)==0){
      warning('The expression returned a null selection.')
      return(gtfs)
    }

    selection <- list(
      expr = expr,
      variables = variables,
      routes = unique(stop_times$route_id[selection]),
      trips = unique(stop_times$trip_id[selection]),
      stops = unique(stop_times$stop_id[selection])
    )

    attr(gtfs,'selection') <- selection
  }

  if('geometry' %in% variables){

    stop_times <- dplyr::left_join(
      gtfs$stop_times,
      gtfs$trips[,c('trip_id','route_id')],
      by = 'trip_id'
    ) %>%
      dplyr::left_join(
        get_stops_sf(gtfs$stops)[,'stop_id'],
        by = 'stop_id'
      ) %>% sf::st_as_sf()

    selection <- eval(expr,stop_times)
    if(sum(selection)==0){
      warning('The expression returned a null selection.')
      return(gtfs)
    }

    selection <- list(
      expr = expr,
      variables = variables,
      routes = unique(stop_times$route_id[selection]),
      trips = unique(stop_times$trip_id[selection]),
      stops = unique(stop_times$stop_id[selection])
    )

    attr(gtfs,'selection') <- selection

  }

  class(gtfs) <- c('wizardgtfs_selected','wizardgtfs','gtfs','list')

  return(gtfs)
}

#' @exportS3Method GTFSwizard::selection wizardgtfs_selected
selection.wizardgtfs_selected <- function(gtfs,...,add = FALSE){

  if(add){

    expr <- substitute(...)

    variables <- dplyr::enquos(..., .ignore_empty = 'all') %>%
      sapply(rlang::quo_get_expr) %>% as.character() %>%
      strsplit('&|\\|') %>% unlist() %>%
      stringr::str_trim() %>% sapply(function(x) unlist(strsplit(x,' '))[1])

    attr(gtfs,'selection_expr') <- c(attr(gtfs,'selection_expr'),expr)

    if('geometry' %nin% variables){

      stop_times <- dplyr::left_join(
        gtfs$stop_times,
        gtfs$trips[,c('trip_id','route_id')],
        by = 'trip_id'
      )

      selection <- eval(expr,stop_times)
      if(sum(selection)==0){
        warning('The expression returned a null selection.')
        return(gtfs)
      }

      selection <- list(
        expr = attr(gtfs,'selection_expr'),
        variables = c(attr(gtfs,'selection')$variables,variables),
        routes = unique(c(attr(gtfs,'selection')$routes,stop_times$route_id[selection])),
        trips = unique(c(attr(gtfs,'selection')$trips,stop_times$trip_id[selection])),
        stops = unique(c(attr(gtfs,'selection')$stops,stop_times$stop_id[selection]))
      )

      attr(gtfs,'selection') <- selection
    }

    if('geometry' %in% variables){

      stop_times <- dplyr::left_join(
        gtfs$stop_times,
        gtfs$trips[,c('trip_id','route_id')],
        by = 'trip_id'
      ) %>%
        dplyr::left_join(
          get_stops_sf(gtfs$stops)[,'stop_id'],
          by = 'stop_id'
        ) %>% sf::st_as_sf()

      selection <- eval(expr,stop_times)
      if(sum(selection)==0){
        warning('The expression returned a null selection.')
        return(gtfs)
      }

      selection <- list(
        expr = attr(gtfs,'selection_expr'),
        variables = c(attr(gtfs,'selection')$variables,variables),
        routes = unique(c(attr(gtfs,'selection')$routes,stop_times$route_id[selection])),
        trips = unique(c(attr(gtfs,'selection')$trips,stop_times$trip_id[selection])),
        stops = unique(c(attr(gtfs,'selection')$stops,stop_times$stop_id[selection]))
      )

      attr(gtfs,'selection') <- selection

    }

    return(gtfs)

  }else{
    selection(gtfs = unselection(gtfs) , ... ,add = FALSE) %>%
      return()
  }


}






