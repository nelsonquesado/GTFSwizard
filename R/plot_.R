#' Plot System Frequency by Hour
#'
#' Shows the distribution and weighted mean of scheduled trip departures by
#' hour. Service-pattern date counts provide the weights.
#'
#' @param gtfs A GTFS object.
#' @return A `ggplot` object.
#'
#' @examples
#' plot_frequency(for_rail_gtfs)
#'
#' @seealso [GTFSwizard::get_frequency()]
#' @export
plot_frequency <- function(gtfs){
  colors <- gtfswizard_colors()
  data <- GTFSwizard::get_frequency(gtfs, method = "detailed") |>
    dplyr::mutate(hour = as.numeric(hour)) |>
    dplyr::filter(is.finite(hour), is.finite(frequency))
  if(!nrow(data)){
    gw_stop("no frequency data are available to plot.")
  }
  hourly <- data |>
    dplyr::group_by(hour) |>
    dplyr::summarise(
      frequency = stats::weighted.mean(
        frequency, pattern_frequency, na.rm = TRUE
      ),
      .groups = "drop"
    )
  overall <- stats::weighted.mean(
    data$frequency, data$pattern_frequency, na.rm = TRUE
  )

  ggplot2::ggplot(hourly, ggplot2::aes(hour, frequency)) +
    ggplot2::geom_col(
      width = 0.82, fill = colors[["light"]], color = "white",
      linewidth = 0.35
    ) +
    ggplot2::geom_line(
      color = colors[["teal"]], linewidth = 1
    ) +
    ggplot2::geom_point(
      color = colors[["teal"]], size = 2
    ) +
    ggplot2::geom_hline(
      yintercept = overall, color = colors[["coral"]],
      linetype = "dashed", linewidth = 0.8
    ) +
    ggplot2::labs(
      title = "Scheduled Trips by Hour",
      subtitle = paste(
        "Bars are service-pattern weighted means; overall mean:",
        round(overall, 1), "trips per hour"
      ),
      x = "Hour", y = "Scheduled trips"
    ) +
    hour_scale(data$hour) +
    ggplot2::expand_limits(y = 0) +
    theme_gtfswizard()
}

#' Plot Route Frequency by Hour
#'
#' Creates a heatmap of scheduled departures by route, hour, and service
#' pattern. The tile fill is the number of scheduled trips.
#'
#' @param gtfs A GTFS object.
#' @param route Optional character vector of route IDs. `NULL` includes all
#'   routes after applying `top_n`.
#' @param top_n Maximum number of routes to display when `route` is `NULL`.
#' @return A `ggplot` object.
#'
#' @examples
#' plot_routefrequency(
#'   for_rail_gtfs,
#'   route = for_rail_gtfs$routes$route_id[1:2]
#' )
#'
#' @seealso [GTFSwizard::get_frequency()]
#' @export
plot_routefrequency <- function(gtfs, route = NULL, top_n = 25L){
  gw_assert_int(top_n, "top_n", lower = 1L)
  requested_routes <- route
  if(!is.null(route)){
    gtfs <- GTFSwizard::filter_route(gtfs, route)
  } else {
    gtfs <- ensure_wizardgtfs(gtfs)
  }
  data <- GTFSwizard::get_frequency(gtfs, method = "detailed") |>
    dplyr::mutate(hour = as.numeric(hour)) |>
    dplyr::filter(is.finite(hour), is.finite(frequency))
  if(!nrow(data)){
    gw_stop("no route-frequency data are available to plot.")
  }
  data <- data |>
    dplyr::filter(is.finite(.data$hour), is.finite(.data$frequency)) |>
    dplyr::group_by(.data$route_id, .data$service_pattern, .data$hour) |>
    dplyr::summarise(frequency = sum(.data$frequency), .groups = "drop")
  route_order <- data |>
    dplyr::group_by(.data$route_id) |>
    dplyr::summarise(total_trips = sum(.data$frequency), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$total_trips))
  if(is.null(requested_routes)){
    route_order <- utils::head(route_order, top_n)
    data <- data |>
      dplyr::filter(.data$route_id %in% route_order$route_id)
  }
  if(!nrow(data)){
    gw_stop("no route-frequency data are available to plot.")
  }
  route_order <- route_order |>
    dplyr::arrange(.data$total_trips)
  data$route_id <- factor(data$route_id, levels = route_order$route_id)
  subtitle <- if(is.null(requested_routes)){
    paste(
      "Each tile is scheduled departures; showing up to",
      top_n, "routes with the most scheduled trips"
    )
  } else {
    "Each tile is scheduled departures for one selected route, hour, and service pattern"
  }

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = hour, y = route_id, fill = frequency
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.25) +
    ggplot2::facet_wrap(~service_pattern, ncol = 1) +
    ggplot2::scale_fill_gradient(
      low = "#E7F1EF", high = gtfswizard_colors()[["blue"]],
      labels = function(x) format(round(x), big.mark = ",", trim = TRUE)
    ) +
    ggplot2::labs(
      title = "Route Frequency",
      subtitle = subtitle,
      x = "Hour", y = "Route", fill = "Trips"
    ) +
    hour_scale(data$hour) +
    theme_gtfswizard()
  plot
}

#' Plot System Headway by Hour
#'
#' @param gtfs A GTFS object.
#' @return A `ggplot` object with headway in minutes.
#'
#' @examples
#' plot_headways(for_rail_gtfs)
#'
#' @seealso [GTFSwizard::get_headways()]
#' @export
plot_headways <- function(gtfs){
  colors <- gtfswizard_colors()
  data <- GTFSwizard::get_headways(gtfs, method = "by_hour") |>
    dplyr::mutate(
      weight = .data$pattern_frequency * .data$valid_trips,
      hour = as.numeric(.data$hour)
    ) |>
    dplyr::filter(
      is.finite(.data$hour),
      is.finite(.data$headway_minutes),
      is.finite(.data$weight)
    )
  if(!nrow(data)){
    gw_stop("no headway data are available to plot.")
  }
  overall <- stats::weighted.mean(
    data$headway_minutes, data$weight, na.rm = TRUE
  )
  pattern_values <- stats::setNames(
    gtfswizard_palette(length(unique(data$service_pattern))),
    unique(data$service_pattern)
  )
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      hour, headway_minutes, color = service_pattern,
      group = service_pattern
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::geom_hline(
      yintercept = overall, color = colors[["coral"]],
      linetype = "dashed", linewidth = 0.8
    ) +
    ggplot2::scale_color_manual(values = pattern_values) +
    ggplot2::labs(
      title = "System Headway",
      subtitle = paste("Overall weighted mean:", round(overall, 1), "minutes"),
      x = "Hour", y = "Average headway (minutes)", color = "Service pattern"
    ) +
    hour_scale(data$hour) +
    ggplot2::expand_limits(y = 0) +
    theme_gtfswizard()
}

#' Plot High-Frequency Corridors
#'
#' @param gtfs A GTFS object.
#' @param i Proportion of highest-frequency segments to retain.
#' @param min_length Minimum corridor length in meters.
#' @param ... Supports the legacy argument `min.length`.
#' @return A `ggplot` map.
#'
#' @seealso [GTFSwizard::get_corridor()]
#' @export
plot_corridor <- function(gtfs, i = 0.01, min_length = 1500, ...){
  resolved <- resolve_legacy_argument(
    min_length, missing(min_length), list(...), "min.length", "min_length"
  )
  min_length <- resolved$value
  gw_check_unused_dots(resolved$dots)
  colors <- gtfswizard_colors()
  gtfs <- ensure_shapes(ensure_wizardgtfs(gtfs))
  shapes <- get_shapes_sf(gtfs$shapes)
  corridors <- get_corridor(gtfs, i = i, min_length = min_length)
  base_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = shapes, linewidth = 0.45, color = colors[["light"]]) +
    ggplot2::coord_sf(datum = NA) +
    theme_gtfswizard_map()
  if(!nrow(corridors)){
    return(
      base_plot +
        ggplot2::labs(
          title = "High-Frequency Corridors",
          subtitle = "No corridor meets the current filters and minimum length"
        )
    )
  }
  corridor_values <- stats::setNames(
    gtfswizard_palette(nrow(corridors)), corridors$corridor
  )
  base_plot +
    ggplot2::geom_sf(
      data = corridors,
      ggplot2::aes(color = corridor),
      linewidth = 1.4, lineend = "round"
    ) +
    ggplot2::scale_color_manual(values = corridor_values) +
    ggplot2::guides(color = "none") +
    ggplot2::labs(
      title = "High-Frequency Corridors",
      subtitle = "Connected high-service stop pairs",
      color = NULL
    )
}

#' Plot Transit Hubs
#'
#' @param gtfs A GTFS object.
#' @param i Proportion of stops with the most routes to retain.
#' @return A `ggplot` map.
#'
#' @details To keep dense networks readable, at most 40 of the highest-ranked
#'   stops are drawn. Ranking uses distinct route count and then trip count.
#'
#' @seealso [GTFSwizard::get_hubs()]
#' @export
plot_hubs <- function(gtfs, i = 0.05){
  if(!is.numeric(i) || length(i) != 1L || is.na(i) || i <= 0 || i > 1){
    gw_stop("`i` must be one number greater than 0 and no greater than 1.")
  }
  colors <- gtfswizard_colors()
  gtfs <- ensure_shapes(ensure_wizardgtfs(gtfs))
  hubs <- get_hubs(gtfs)
  requested <- max(1L, ceiling(nrow(hubs) * i))
  shown <- min(40L, requested)
  hubs <- utils::head(hubs, shown)
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = get_shapes_sf(gtfs$shapes),
      linewidth = 0.45, color = colors[["light"]]
    ) +
    ggplot2::geom_sf(
      data = hubs,
      ggplot2::aes(size = n_routes, fill = n_routes),
      shape = 21, color = "white", stroke = 0.5, alpha = 0.92
    ) +
    ggplot2::scale_fill_gradient(
      low = colors[["gold"]], high = colors[["coral"]]
    ) +
    ggplot2::scale_size(range = c(2.2, 7.5)) +
    ggplot2::guides(size = "none") +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title = "Transit Hubs",
      subtitle = paste(
        "Top", shown, "stops ranked by distinct routes",
        if(requested > shown) "(display capped for readability)" else ""
      ),
      fill = "Routes", size = "Routes"
    ) +
    theme_gtfswizard_map()
}
