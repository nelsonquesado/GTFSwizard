#' Print, Summarize, and Plot `wizardgtfs` Objects
#'
#' @param x,object A `wizardgtfs` object or its summary.
#' @param ... Additional arguments passed to print methods.
#' @param n Number of rows shown per GTFS table.
#'
#' @return `print()` returns `x` invisibly; `summary()` returns a
#'   `summary.wizardgtfs` object; `plot()` returns a `ggplot` object.
#'
#' @name wizardgtfs-methods
NULL

#' @rdname wizardgtfs-methods
#' @exportS3Method base::print wizardgtfs
print.wizardgtfs <- function(x, ..., n = 5L){
  gw_assert_int(n, "n", lower = 1L)
  agency_names <- if(!is.null(x$agency$agency_name)){
    paste(x$agency$agency_name, collapse = ", ")
  } else {
    "not supplied"
  }
  cat("<wizardgtfs>\n")
  cat("  Agency: ", agency_names, "\n", sep = "")
  cat(
    "  ", nrow(x$routes), " routes; ", nrow(x$trips), " trips; ",
    nrow(x$stops), " stops\n", sep = ""
  )
  for(table_name in setdiff(names(x), "dates_services")){
    cat("\n$", table_name, " [", nrow(x[[table_name]]), " rows]\n", sep = "")
    print(utils::head(x[[table_name]], n), ...)
  }
  invisible(x)
}

#' @rdname wizardgtfs-methods
#' @exportS3Method base::summary wizardgtfs
summary.wizardgtfs <- function(object, ...){
  object <- ensure_wizardgtfs(object)
  dates <- object$dates_services$date
  result <- list(
    tables = vapply(
      object[setdiff(names(object), "dates_services")], nrow, integer(1)
    ),
    agency = paste(object$agency$agency_name, collapse = ", "),
    service_days = if(length(dates)) range(as.Date(dates)) else as.Date(c(NA, NA)),
    routes = nrow(object$routes),
    stops = nrow(object$stops),
    trips = nrow(object$trips),
    shapes = if(is.null(object$shapes)) 0L else length(unique(object$shapes$shape_id)),
    total_days = length(unique(dates)),
    median_stop_spacing = suppressWarnings(get_stop_dists(object))
  )
  class(result) <- "summary.wizardgtfs"
  result
}

#' @rdname wizardgtfs-methods
#' @exportS3Method base::print summary.wizardgtfs
print.summary.wizardgtfs <- function(x, ...){
  service_days <- format(as.Date(x$service_days), "%Y-%m-%d")
  cat("<summary.wizardgtfs>\n")
  cat("  Agency: ", x$agency, "\n", sep = "")
  cat(
    "  Service: ", service_days[1], " to ", service_days[2],
    " (", x$total_days, " active dates)\n", sep = ""
  )
  cat(
    "  ", x$routes, " routes; ", x$trips, " trips; ", x$stops,
    " stops; ", x$shapes, " shapes\n", sep = ""
  )
  if(is.finite(x$median_stop_spacing)){
    cat("  Median consecutive-stop spacing: ", x$median_stop_spacing, " m\n", sep = "")
  }
  cat("\nTables:\n")
  print(x$tables)
  invisible(x)
}

#' @rdname wizardgtfs-methods
#' @exportS3Method base::plot wizardgtfs
plot.wizardgtfs <- function(x, ...){
  if(is.null(x$stops) && is.null(x$shapes)){
    gw_stop("the feed has neither `stops` nor `shapes` to plot.")
  }
  colors <- gtfswizard_colors()
  plot <- ggplot2::ggplot()
  if(!is.null(x$stops)){
    stops <- get_stops_sf(x$stops)
    plot <- plot + ggplot2::geom_sf(
      data = stops, color = colors[["gray"]], fill = "white",
      shape = 21, size = 0.9, stroke = 0.3
    )
  }
  if(!is.null(x$shapes)){
    shapes <- get_shapes_sf(x$shapes)
    if("shape_id" %in% names(x$trips)){
      route_shapes <- unique(x$trips[c("shape_id", "route_id")])
      shapes <- dplyr::left_join(shapes, route_shapes, by = "shape_id")
      plot <- plot + ggplot2::geom_sf(
        data = shapes, ggplot2::aes(color = route_id),
        linewidth = 0.75, show.legend = FALSE
      )
    } else {
      plot <- plot + ggplot2::geom_sf(
        data = shapes, color = colors[["blue"]], linewidth = 0.7
      )
    }
  }
  title <- paste(x$agency$agency_name, collapse = ", ")
  plot +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(title = title, subtitle = "GTFS routes and stops") +
    theme_gtfswizard_map()
}
