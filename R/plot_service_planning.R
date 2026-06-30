#' Plot Scheduled Service Span by Route
#'
#' Shows the first departure and final arrival for route and service-pattern
#' combinations. The longest and most frequently operated route-pattern
#' combinations are retained when the feed is large.
#'
#' @param gtfs A GTFS object.
#' @param top_n Maximum number of route-pattern combinations to display.
#'
#' @return A `ggplot` object. Each horizontal line is one route and service
#'   pattern; its endpoints are scheduled clock hours.
#'
#' @examples
#' plot_servicespan(for_rail_gtfs)
#'
#' @references
#' [GTFS Routes, Stops, and Trips](https://gtfs.org/documentation/schedule/examples/routes-stops-trips/)
#' @export
plot_servicespan <- function(gtfs, top_n = 30L){
  gw_assert_int(top_n, "top_n", lower = 1L)
  gtfs <- ensure_wizardgtfs(gtfs)
  durations <- trip_duration_table(gtfs)
  starts <- get_1stdeparture(gtfs) |>
    dplyr::mutate(start_hour = gtfs_time_to_seconds(.data$departure_time) / 3600)
  data <- gtfs$trips |>
    dplyr::select(route_id, trip_id, service_id) |>
    dplyr::left_join(starts[, c("trip_id", "start_hour")], by = "trip_id") |>
    dplyr::left_join(durations, by = "trip_id") |>
    dplyr::left_join(get_servicepattern(gtfs), by = "service_id") |>
    dplyr::mutate(end_hour = .data$start_hour + .data$duration / 3600) |>
    dplyr::filter(is.finite(.data$start_hour), is.finite(.data$end_hour)) |>
    dplyr::group_by(.data$route_id, .data$service_pattern) |>
    dplyr::summarise(
      first_departure = min(.data$start_hour),
      final_arrival = max(.data$end_hour),
      scheduled_trips = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$scheduled_trips),
      dplyr::desc(.data$final_arrival - .data$first_departure)
    ) |>
    utils::head(top_n)
  if(!nrow(data)){
    gw_stop("no service-span data are available to plot.")
  }
  data$route_pattern <- paste(data$route_id, data$service_pattern, sep = " | ")
  data$route_pattern <- stats::reorder(
    data$route_pattern, data$first_departure
  )
  pattern_values <- stats::setNames(
    gtfswizard_palette(length(unique(data$service_pattern))),
    unique(data$service_pattern)
  )
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      y = route_pattern, x = first_departure, color = service_pattern
    )
  ) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = final_arrival, yend = route_pattern),
      linewidth = 2.2, lineend = "round"
    ) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::geom_point(
      ggplot2::aes(x = final_arrival), size = 2.4
    ) +
    ggplot2::scale_color_manual(values = pattern_values) +
    hour_scale(c(data$first_departure, data$final_arrival)) +
    ggplot2::labs(
      title = "Scheduled Service Span",
      subtitle = paste(
        "Each line is one route-service pattern; showing up to",
        top_n, "combinations"
      ),
      x = "Scheduled clock hour", y = "Route | service pattern",
      color = "Service pattern"
    ) +
    theme_gtfswizard()
}

#' Plot Average Scheduled Departures by Weekday and Hour
#'
#' Creates a heatmap of average scheduled departures on active service dates.
#'
#' @param gtfs A GTFS object.
#'
#' @return A `ggplot` object. Each tile is a weekday-hour combination and its
#'   fill is the mean number of scheduled trip departures per active date.
#'
#' @examples
#' plot_serviceheatmap(for_rail_gtfs)
#'
#' @references
#' [GTFS Schedule Reference](https://gtfs.org/documentation/schedule/reference/)
#' @export
plot_serviceheatmap <- function(gtfs){
  gtfs <- ensure_wizardgtfs(gtfs)
  frequency <- get_frequency(gtfs, "detailed") |>
    dplyr::group_by(.data$service_pattern, .data$hour) |>
    dplyr::summarise(departures = sum(.data$frequency), .groups = "drop")
  pattern_dates <- service_pattern_date_table(gtfs) |>
    dplyr::distinct(.data$date, .data$service_pattern) |>
    dplyr::mutate(
      weekday = factor(
        weekdays(.data$date),
        levels = weekdays(as.Date("2026-01-05") + 0:6)
      )
    )
  data <- dplyr::left_join(
    pattern_dates, frequency,
    by = "service_pattern", relationship = "many-to-many"
  ) |>
    dplyr::group_by(.data$date, .data$weekday, .data$hour) |>
    dplyr::summarise(
      departures = sum(.data$departures, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$weekday, .data$hour) |>
    dplyr::summarise(
      average_departures = mean(.data$departures, na.rm = TRUE),
      service_dates = dplyr::n_distinct(.data$date),
      .groups = "drop"
    ) |>
    dplyr::filter(
      is.finite(.data$hour),
      is.finite(.data$average_departures)
    )
  if(!nrow(data)){
    gw_stop("no weekday-hour service data are available to plot.")
  }
  colors <- gtfswizard_colors()
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = hour, y = weekday, fill = average_departures)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::scale_fill_gradient(
      low = "#E7F1EF", high = colors[["teal"]],
      labels = function(x) format(round(x), big.mark = ",", trim = TRUE),
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        barwidth = grid::unit(12, "lines")
      )
    ) +
    hour_scale(data$hour) +
    ggplot2::labs(
      title = "Scheduled Departures by Weekday and Hour",
      subtitle = "Each tile is the mean number of departures per active service date",
      x = "Scheduled departure hour", y = "Weekday",
      fill = "Average departures"
    ) +
    theme_gtfswizard()
}

#' Plot Scheduled Trip Duration by Route
#'
#' Shows the distribution of scheduled end-to-end trip duration for the
#' busiest routes.
#'
#' @param gtfs A GTFS object.
#' @param top_n Maximum number of routes to display.
#'
#' @return A `ggplot` object. Each observation is one scheduled trip; boxes
#'   summarize duration in minutes by route.
#'
#' @examples
#' plot_routeduration(for_rail_gtfs)
#'
#' @references
#' [FTA Evaluation Introduction](https://www.transit.dot.gov/research-innovation/evaluation-introduction)
#' @export
plot_routeduration <- function(gtfs, top_n = 20L){
  gw_assert_int(top_n, "top_n", lower = 1L)
  gtfs <- ensure_wizardgtfs(gtfs)
  data <- trip_duration_table(gtfs) |>
    dplyr::left_join(
      gtfs$trips[, c("trip_id", "route_id", "service_id")],
      by = "trip_id"
    ) |>
    dplyr::left_join(get_servicepattern(gtfs), by = "service_id") |>
    dplyr::mutate(duration_minutes = .data$duration / 60) |>
    dplyr::filter(
      is.finite(.data$duration_minutes), .data$duration_minutes >= 0
    )
  route_order <- data |>
    dplyr::count(.data$route_id, sort = TRUE) |>
    utils::head(top_n)
  data <- data |>
    dplyr::filter(.data$route_id %in% route_order$route_id)
  data$route_id <- factor(data$route_id, levels = rev(route_order$route_id))
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = duration_minutes, y = route_id, fill = service_pattern
    )
  ) +
    ggplot2::geom_boxplot(
      outlier.alpha = 0.18, linewidth = 0.45
    ) +
    ggplot2::scale_fill_manual(
      values = gtfswizard_palette(length(unique(data$service_pattern)))
    ) +
    ggplot2::labs(
      title = "Scheduled Trip Duration by Route",
      subtitle = paste(
        "Each observation is one trip; showing the", top_n,
        "routes with the most scheduled trips"
      ),
      x = "Scheduled end-to-end duration (minutes)",
      y = "Route", fill = "Service pattern"
    ) +
    theme_gtfswizard()
}

#' Plot Scheduled Vehicle-Hours by Route
#'
#' Sums scheduled end-to-end trip durations for each route and service
#' pattern.
#'
#' @param gtfs A GTFS object.
#' @param top_n Maximum number of routes to display.
#'
#' @return A `ggplot` object. Each bar is scheduled vehicle-hours for one
#'   route on one representative service-pattern day.
#'
#' @examples
#' plot_servicesupply(for_rail_gtfs)
#'
#' @references
#' [FTA National Transit Database](https://www.transit.dot.gov/ntd)
#' @export
plot_servicesupply <- function(gtfs, top_n = 20L){
  gw_assert_int(top_n, "top_n", lower = 1L)
  gtfs <- ensure_wizardgtfs(gtfs)
  data <- trip_duration_table(gtfs) |>
    dplyr::left_join(
      gtfs$trips[, c("trip_id", "route_id", "service_id")],
      by = "trip_id"
    ) |>
    dplyr::left_join(get_servicepattern(gtfs), by = "service_id") |>
    dplyr::filter(is.finite(.data$duration), .data$duration >= 0) |>
    dplyr::group_by(.data$route_id, .data$service_pattern) |>
    dplyr::summarise(
      vehicle_hours = sum(.data$duration) / 3600,
      scheduled_trips = dplyr::n(),
      .groups = "drop"
    )
  route_order <- data |>
    dplyr::group_by(.data$route_id) |>
    dplyr::summarise(total_hours = sum(.data$vehicle_hours), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$total_hours)) |>
    utils::head(top_n)
  data <- data |>
    dplyr::filter(.data$route_id %in% route_order$route_id)
  data$route_id <- factor(data$route_id, levels = rev(route_order$route_id))
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = vehicle_hours, y = route_id, fill = service_pattern
    )
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_fill_manual(
      values = gtfswizard_palette(length(unique(data$service_pattern)))
    ) +
    ggplot2::labs(
      title = "Scheduled Vehicle-Hours by Route",
      subtitle = paste(
        "Each bar is one route-service pattern; showing the top",
        top_n, "routes"
      ),
      x = "Scheduled vehicle-hours per service-pattern day",
      y = "Route", fill = "Service pattern"
    ) +
    theme_gtfswizard()
}
