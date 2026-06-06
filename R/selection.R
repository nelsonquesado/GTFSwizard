`%intersects%` <- function(x, y){
  unique_x <- sf::st_sfc(unique(x), crs = 4326)
  matched <- apply(
    sf::st_intersects(unique_x, y, sparse = FALSE),
    MARGIN = 1,
    FUN = any
  )
  matched[match(x, unique_x)]
}

`%touches%` <- function(x, y){
  unique_x <- sf::st_sfc(unique(x), crs = 4326)
  matched <- apply(
    sf::st_touches(unique_x, y, sparse = FALSE),
    MARGIN = 1,
    FUN = any
  )
  matched[match(x, unique_x)]
}

`%crosses%` <- function(x, y){
  apply(sf::st_crosses(x, y, sparse = FALSE), MARGIN = 1, FUN = any)
}

`%within%` <- function(x, y){
  apply(sf::st_within(x, y, sparse = FALSE), MARGIN = 1, FUN = any)
}

`%contains%` <- function(x, y){
  apply(sf::st_contains(x, y, sparse = FALSE), MARGIN = 1, FUN = any)
}

`%overlaps%` <- function(x, y){
  apply(sf::st_overlaps(x, y, sparse = FALSE), MARGIN = 1, FUN = any)
}

`%equals%` <- function(x, y){
  apply(sf::st_equals(x, y, sparse = FALSE), MARGIN = 1, FUN = any)
}

`%nin%` <- function(x, y){
  !x %in% y
}

#' Group or Select GTFS Records Without Filtering the Feed
#'
#' Creates persistent selection metadata while leaving every GTFS table
#' unchanged. Bare columns and computed values define groups, similarly to
#' [dplyr::group_by()]. Logical expressions restrict the records represented
#' by those groups.
#'
#' @param gtfs A GTFS object.
#' @param ... Unquoted grouping columns, computed grouping expressions, or
#'   logical selection expressions. Columns from `stop_times`, `trips`,
#'   `routes`, and `stops` are available. A `geometry` column is available for
#'   stop-based spatial predicates.
#' @param add Logical. When `TRUE`, add the new grouping or selection
#'   expressions to an existing selection. When `FALSE`, replace it.
#'
#' @return The unchanged feed with class `wizardgtfs_selected` and a
#'   `selection` attribute. The attribute contains `groups`, `group_vars`,
#'   selected route, trip, and stop IDs, and the original expressions.
#'
#' @details
#' A bare expression such as `route_id` creates one group per route. Multiple
#' grouping expressions create combinations, for example
#' `selection(gtfs, route_id, direction_id)`. Logical expressions such as
#' `route_id %in% c("1", "2")` restrict the records represented in the
#' selection but do not remove rows from the GTFS feed.
#'
#' Spatial expressions may use `geometry` with `%intersects%`, `%touches%`,
#' `%crosses%`, `%within%`, `%contains%`, `%overlaps%`, or `%equals%`.
#'
#' @examples
#' grouped <- selection(for_rail_gtfs, route_id, direction_id)
#' attr(grouped, "selection")$groups
#'
#' selected <- selection(
#'   for_rail_gtfs,
#'   route_id,
#'   stop_id %in% for_rail_gtfs$stops$stop_id[1:3]
#' )
#'
#' bbox <- sf::st_bbox(
#'   c(xmin = -38.58, ymin = -3.81, xmax = -38.50, ymax = -3.75),
#'   crs = sf::st_crs(4326)
#' )
#' spatial <- selection(
#'   for_rail_gtfs,
#'   geometry %intersects% sf::st_as_sfc(bbox)
#' )
#'
#' @seealso [GTFSwizard::unselection()], [dplyr::group_by()]
#' @export
selection <- function(gtfs, ..., add = FALSE){
  UseMethod("selection")
}

#' @exportS3Method GTFSwizard::selection list
selection.list <- function(gtfs, ..., add = FALSE){
  selection.wizardgtfs(
    as_wizardgtfs(gtfs),
    ...,
    add = add
  )
}

#' @exportS3Method GTFSwizard::selection wizardgtfs
selection.wizardgtfs <- function(gtfs, ..., add = FALSE){
  gw_assert_flag(add, "add")
  quosures <- rlang::enquos(..., .ignore_empty = "all")
  if(!length(quosures)){
    gw_stop("supply at least one grouping or logical selection expression.")
  }
  build_selection(gtfs, quosures)
}

#' @exportS3Method GTFSwizard::selection wizardgtfs_selected
selection.wizardgtfs_selected <- function(gtfs, ..., add = FALSE){
  gw_assert_flag(add, "add")
  new_quosures <- rlang::enquos(..., .ignore_empty = "all")
  if(!length(new_quosures)){
    gw_stop("supply at least one grouping or logical selection expression.")
  }
  if(add){
    old_quosures <- attr(gtfs, "selection")$quosures
    return(build_selection(gtfs, c(old_quosures, new_quosures)))
  }
  build_selection(unselection(gtfs), new_quosures)
}

build_selection <- function(gtfs, quosures){
  data <- selection_data(gtfs, quosures)
  group_vars <- character()
  filters <- character()
  used_names <- character()

  for(index in seq_along(quosures)){
    quosure <- quosures[[index]]
    expression <- rlang::quo_get_expr(quosure)
    label <- names(quosures)[index]
    if(is.null(label) || !nzchar(label)){
      label <- rlang::as_label(quosure)
    }
    evaluation_env <- rlang::env(
      rlang::quo_get_env(quosure),
      `%intersects%` = `%intersects%`,
      `%touches%` = `%touches%`,
      `%crosses%` = `%crosses%`,
      `%within%` = `%within%`,
      `%contains%` = `%contains%`,
      `%overlaps%` = `%overlaps%`,
      `%equals%` = `%equals%`,
      `%nin%` = `%nin%`
    )
    evaluation_quosure <- rlang::new_quosure(expression, evaluation_env)

    value <- tryCatch(
      rlang::eval_tidy(evaluation_quosure, data = data),
      error = function(error){
        gw_stop(
          "could not evaluate selection expression `", label,
          "`: ", conditionMessage(error)
        )
      }
    )
    if(length(value) == 1L){
      value <- rep(value, nrow(data))
    }
    if(length(value) != nrow(data)){
      gw_stop(
        "selection expression `", label, "` returned ", length(value),
        " values; expected 1 or ", nrow(data), "."
      )
    }

    is_bare_column <- rlang::is_symbol(expression) &&
      rlang::as_string(expression) %in% names(data)
    if(is.logical(value) && !is_bare_column){
      value[is.na(value)] <- FALSE
      data <- data[value, , drop = FALSE]
      filters <- c(filters, label)
    } else {
      group_name <- if(is_bare_column){
        rlang::as_string(expression)
      } else {
        make.unique(c(used_names, label))[length(used_names) + 1L]
      }
      data[[group_name]] <- value
      group_vars <- c(group_vars, group_name)
      used_names <- c(used_names, group_name)
    }
  }

  groups <- summarize_selection_groups(data, group_vars)
  if(!nrow(data)){
    gw_warn("the logical selection expressions returned no records.")
  }
  selection_info <- list(
    expr = lapply(quosures, rlang::quo_get_expr),
    quosures = quosures,
    variables = unique(c(group_vars, filters)),
    group_vars = group_vars,
    filters = filters,
    groups = groups,
    routes = unique(as.character(data$route_id)),
    trips = unique(as.character(data$trip_id)),
    stops = unique(as.character(data$stop_id))
  )
  attr(gtfs, "selection") <- selection_info
  attr(gtfs, "selection_expr") <- selection_info$expr
  class(gtfs) <- unique(c(
    "wizardgtfs_selected", "wizardgtfs", "gtfs", "list", class(gtfs)
  ))
  gtfs
}

selection_data <- function(gtfs, quosures){
  expressions <- lapply(quosures, rlang::quo_get_expr)
  requested <- unique(unlist(lapply(expressions, all.vars)))
  identifiers <- c("trip_id", "stop_id", "route_id")

  stop_fields <- unique(c(
    intersect(requested, names(gtfs$stop_times)),
    intersect(identifiers, names(gtfs$stop_times))
  ))
  data <- gtfs$stop_times[, stop_fields, drop = FALSE]

  data <- join_selection_fields(
    data, gtfs$trips, "trip_id",
    unique(c("route_id", intersect(requested, names(gtfs$trips))))
  )
  data <- join_selection_fields(
    data, gtfs$routes, "route_id",
    intersect(requested, names(gtfs$routes))
  )
  data <- join_selection_fields(
    data, gtfs$stops, "stop_id",
    intersect(requested, names(gtfs$stops))
  )

  if("geometry" %in% requested){
    geometry <- get_stops_sf(gtfs$stops)[, c("stop_id", "geometry")]
    data <- dplyr::left_join(data, geometry, by = "stop_id")
  }
  data
}

join_selection_fields <- function(x, table, key, fields){
  fields <- setdiff(unique(fields), names(x))
  if(is.null(table) || !key %in% names(x) || !key %in% names(table) ||
     !length(fields)){
    return(x)
  }
  dplyr::left_join(
    x,
    table[, unique(c(key, fields)), drop = FALSE],
    by = key
  )
}

summarize_selection_groups <- function(data, group_vars){
  if(!length(group_vars)){
    return(tibble::tibble(
      n_stop_calls = nrow(data),
      routes = list(unique(as.character(data$route_id))),
      trips = list(unique(as.character(data$trip_id))),
      stops = list(unique(as.character(data$stop_id)))
    ))
  }
  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      n_stop_calls = dplyr::n(),
      routes = list(unique(as.character(route_id))),
      trips = list(unique(as.character(trip_id))),
      stops = list(unique(as.character(stop_id))),
      .groups = "drop"
    )
}
