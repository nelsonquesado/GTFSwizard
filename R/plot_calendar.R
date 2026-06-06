#' Plot the GTFS Service Calendar
#'
#' Creates a calendar heatmap of scheduled trips by service date.
#'
#' @param gtfs A GTFS object.
#' @param ncol Number of facet columns when `facet_by_year = FALSE`.
#' @param facet_by_year Logical. Arrange years in rows and months in columns.
#' @param fill Calendar fill. `"trips"` shows trip counts; `"service_pattern"`
#'   uses a discrete color for each service pattern.
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
                          fill = c("trips", "service_pattern")){
  gtfs <- ensure_wizardgtfs(gtfs)
  gw_assert_int(ncol, "ncol", lower = 1L)
  gw_assert_flag(facet_by_year, "facet_by_year")
  fill <- match.arg(fill)
  colors <- gtfswizard_colors()

  trips_per_service <- gtfs$trips |>
    dplyr::count(service_id, name = "trips")
  counts <- tidyr::unnest(gtfs$dates_services, cols = "service_id") |>
    dplyr::left_join(trips_per_service, by = "service_id") |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      count = sum(trips, na.rm = TRUE),
      service_ids = list(sort(unique(service_id))),
      .groups = "drop"
    )
  signatures <- counts |>
    dplyr::mutate(.signature = vapply(
      .data$service_ids, paste, collapse = "|", FUN.VALUE = character(1)
    )) |>
    dplyr::count(.signature, sort = TRUE, name = "pattern_frequency") |>
    dplyr::mutate(
      service_pattern = paste0("servicepattern-", dplyr::row_number())
    )
  counts <- counts |>
    dplyr::mutate(.signature = vapply(
      .data$service_ids, paste, collapse = "|", FUN.VALUE = character(1)
    )) |>
    dplyr::left_join(signatures, by = ".signature")
  all_dates <- tibble::tibble(
    date = seq(min(gtfs$dates_services$date), max(gtfs$dates_services$date), by = "day")
  )
  data <- dplyr::left_join(all_dates, counts, by = "date")
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
    pattern_values <- stats::setNames(
      gtfswizard_palette(length(unique(stats::na.omit(data$service_pattern)))),
      unique(stats::na.omit(data$service_pattern))
    )
    plot <- plot +
      ggplot2::geom_tile(
        ggplot2::aes(fill = service_pattern),
        color = "white", linewidth = 0.4
      ) +
      ggplot2::scale_fill_manual(
        values = pattern_values, na.value = "#F0F2F3", drop = FALSE
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
    ggplot2::geom_text(
      ggplot2::aes(label = day_of_month),
      size = 3.1, color = colors[["ink"]]
    ) +
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
      axis.text.x = ggplot2::element_text(size = 9),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.spacing = grid::unit(10, "pt"),
      strip.text = ggplot2::element_text(size = 9, face = "bold")
    )
  if(facet_by_year){
    plot + ggplot2::facet_grid(year ~ month)
  } else {
    plot + ggplot2::facet_wrap(
      ggplot2::vars(year, month), ncol = ncol
    )
  }
}
