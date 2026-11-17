#' Plot the GTFS Service Calendar
#'
#' Creates a calendar heatmap of scheduled trips by service date.
#'
#' @param gtfs A GTFS object.
#' @param ncol Number of facet columns when `facet_by_year = FALSE`.
#' @param facet_by_year Logical. Arrange years in rows and months in columns.
#' @param fill Calendar fill. `"trips"` shows trip counts and draws dates with
#'   no active service as zero trips; `"service_pattern"` uses a discrete color
#'   for each service pattern and labels dates with no active service as
#'   `"No service"`.
#' @param max_patterns Maximum number of service patterns given individual
#'   colors when `fill = "service_pattern"`. Remaining patterns share an
#'   `"Other service patterns"` category. Dates and original pattern IDs remain
#'   available in the plot data.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' plot_calendar(for_rail_gtfs, ncol = 4)
#' plot_calendar(
#'   for_rail_gtfs,
#'   facet_by_year = TRUE,
#'   fill = "service_pattern"
#' )
#'
#' @seealso [GTFSwizard::get_servicepattern()]
#' @export
plot_calendar <- function(gtfs, ncol = 4, facet_by_year = FALSE,
                          fill = c("trips", "service_pattern"), max_patterns = 20L){
  gtfs <- ensure_wizardgtfs(gtfs)
  gw_assert_int(ncol, "ncol", lower = 1L)
  gw_assert_flag(facet_by_year, "facet_by_year")
  gw_assert_int(max_patterns, "max_patterns", lower = 1L)
  fill <- match.arg(fill)
  colors <- gtfswizard_colors()

  trips_per_service <- trip_instance_starts(gtfs) |>
    dplyr::left_join(gtfs$trips[, c("trip_id", "service_id")], by = "trip_id") |>
    dplyr::count(.data$service_id, name = "trips")
  trip_counts <- stats::setNames(trips_per_service$trips, trips_per_service$service_id)
  data <- service_pattern_date_table(gtfs)
  if(!nrow(data)) gw_stop("no active calendar dates are available to plot.")
  data$count <- vapply(data$service_ids, function(service_ids){
    sum(trip_counts[service_ids], na.rm = TRUE)
  }, numeric(1))
  weekday_number <- as.integer(format(data$date, "%w"))
  first <- as.Date(format(data$date, "%Y-%m-01"))
  data$weekday <- factor(
    weekday_number,
    levels = 0:6,
    labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  )
  data$day_of_month <- as.integer(format(data$date, "%d"))
  data$month <- factor(
    format(data$date, "%B"), levels = month.name
  )
  data$year <- format(data$date, "%Y")
  data$week_of_month <- (
    data$day_of_month - 1L + as.integer(format(first, "%w"))
  ) %/% 7L + 1L

  plot <- ggplot2::ggplot(data, ggplot2::aes(weekday, -week_of_month))
  if(fill == "service_pattern"){
    active_patterns <- sort(table(data$service_pattern[data$service_pattern != "No service"]),
                            decreasing = TRUE)
    individual <- utils::head(names(active_patterns), max_patterns)
    data$.calendar_pattern <- ifelse(
      data$service_pattern %in% c(individual, "No service"),
      data$service_pattern, "Other service patterns"
    )
    plot$data <- data
    pattern_levels <- unique(data$.calendar_pattern)
    pattern_values <- stats::setNames(
      gtfswizard_palette(length(pattern_levels)),
      pattern_levels
    )
    pattern_values["No service"] <- "#F0F2F3"
    pattern_values["Other service patterns"] <- "#87939D"
    plot <- plot +
      ggplot2::geom_tile(
        ggplot2::aes(fill = .data$.calendar_pattern),
        color = "white", linewidth = 0.4
      ) +
      ggplot2::scale_fill_manual(
        values = pattern_values, na.value = "#F0F2F3", drop = FALSE,
        guide = ggplot2::guide_legend(ncol = min(4L, length(pattern_levels)), byrow = TRUE)
      )
  } else {
    plot <- plot +
      ggplot2::geom_tile(
        ggplot2::aes(fill = count),
        color = "white", linewidth = 0.4
      ) +
      ggplot2::scale_fill_gradient(
        low = "#DCEBE8", high = colors[["teal"]], na.value = "#F0F2F3",
        labels = function(x) format(round(x), big.mark = ",", trim = TRUE),
        guide = ggplot2::guide_colorbar(
          title.position = "top",
          barwidth = grid::unit(12, "lines")
        )
      )
  }
  plot <- plot +
    ggplot2::scale_x_discrete(
      labels = if(facet_by_year){
        c("S", "M", "T", "W", "T", "F", "S")
      } else {
        c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
      },
      drop = FALSE
    )
  if(!facet_by_year){
    plot <- plot + ggplot2::geom_text(
      ggplot2::aes(label = day_of_month),
      size = 3.1,
      color = colors[["ink"]]
    )
  }
  plot <- plot +
    ggplot2::labs(
      title = "Scheduled Service Calendar",
      subtitle = if(fill == "trips"){
        "Trip count by service date"
      } else {
        "Service pattern by date"
      },
      x = NULL, y = NULL,
      fill = if(fill == "trips") "Trips" else "Service pattern"
    ) +
    theme_gtfswizard() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        size = if(facet_by_year) 7 else 9
      ),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.spacing = grid::unit(if(facet_by_year) 4 else 10, "pt"),
      strip.text = ggplot2::element_text(
        size = if(facet_by_year) 8 else 9,
        face = "bold"
      )
    )
  if(facet_by_year){
    plot + ggplot2::facet_grid(year ~ month)
  } else {
    plot + ggplot2::facet_wrap(
      ggplot2::vars(year, month), ncol = ncol
    )
  }
}
