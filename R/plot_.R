#' Plot System Frequency by Hour
#'
#' `plot_frequency` generates an interactive plot of the frequency of trips by hour across the GTFS dataset. The plot shows hourly trip distributions, hourly average frequency, and an overall average frequency for the system, providing insights into peak times and overall transit service frequency.
#'
#' @param gtfs A GTFS object. This should ideally be of the `wizardgtfs` class, or it will be converted.
#'
#' @return A `plotly` interactive plot displaying hourly frequency distributions, including:
#'
#'   - Hourly Distribution: Boxplots showing frequency distribution across hours.
#'
#'   - Hourly Average Frequency: A line indicating the weighted average frequency for each hour.
#'
#'   - Overall Average Frequency: A dashed line marking the system's overall average frequency.
#'
#' @details
#' The function first calculates hourly and overall average frequencies using a weighted mean based on `pattern_frequency`. Frequencies are plotted by hour of the day to visualize the system's trip distribution patterns.
#'
#' @examples
#' if (interactive()) {
#' # Plot the frequency of trips by hour for a GTFS object
#' plot_frequency(for_rail_gtfs)
#'}
#'
#' @seealso
#' [GTFSwizard::get_frequency()]
#'
#' @importFrom dplyr mutate group_by reframe
#' @importFrom ggplot2 ggplot geom_boxplot geom_hline geom_line labs scale_x_continuous scale_y_continuous scale_color_manual
#' @importFrom hrbrthemes theme_ipsum
#' @importFrom plotly ggplotly
#' @export
# plot_frequency
plot_frequency <- function(gtfs){

  data <-
    GTFSwizard::get_frequency(gtfs, method = 'detailed') %>%
    dplyr::mutate(hour = as.numeric(hour))

  overall.average <-
    weighted.mean(data$frequency, data$pattern_frequency, na.rm = TRUE)

  plot <-
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = data, ggplot2::aes(x = hour, y = frequency, color = 'Hourly\nDistribution\n', group = hour, weight = pattern_frequency), fill = 'gray', alpha = .65) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = overall.average, color = paste0('Overall\nAverage\nFrequency\n', round(overall.average, 1), ' trips')), linetype = 'dashed', linewidth = .75) +
    ggplot2::geom_line(data = dplyr::group_by(data, hour) %>% dplyr::reframe(frequency = round(weighted.mean(frequency, pattern_frequency, na.rm = TRUE), 1)), ggplot2::aes(hour, frequency, color = 'Hourly\nAverage\nFrequency\n', group = NA), linewidth = 1) +
    ggplot2::labs(x = 'Hour of the Day', y = 'Hourly Frequency', colour = '', title = 'System Frequency') +
    hrbrthemes::theme_ipsum() +
    ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24), limits = c(0, 24)) +
    ggplot2::scale_y_continuous(limits = c(0, max(data$frequency))) +
    ggplot2::scale_color_manual(values = c('#00BFC4', 'black', '#F8766D'))

  plotly <-
    suppressWarnings(
      plotly::ggplotly(plot,
                       tooltip = c('x', 'y')
      )
    )

  return(plotly)
}


#' Plot Route Frequency by Hour
#'
#' `plot_routefrequency` generates an interactive plot of the frequency of trips by hour for specified routes in a GTFS dataset. The plot shows the hourly frequency distribution for each route and visualizes different service patterns.
#'
#' @param gtfs A GTFS object. Ideally, this should be of the `wizardgtfs` class, or it will be converted.
#' @param route A character vector specifying one or more `route_id` values to plot. If `NULL`, all routes are included.
#'
#' @return A `plotly` interactive plot displaying the frequency distribution by hour for each selected route, with:
#'
#'   - Hourly Frequency: A line for each route, indicating its frequency distribution across the day.
#'
#'   - Service Patterns: Transparency levels indicate different service patterns, with the primary pattern highlighted.
#'
#' @details
#' The function filters the GTFS dataset by route and computes hourly frequencies for each service pattern. The plot shows variations in service frequency across hours and highlights the primary service pattern.
#'
#' @examples
#' if (interactive()) {
#' # Plot frequency by hour for specific routes
#' plot_routefrequency(for_rail_gtfs, route = for_rail_gtfs$routes$route_id[1:2])
#' }
#'
#' @seealso
#' [GTFSwizard::filter_route()], [GTFSwizard::get_frequency()]
#'
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_line geom_point labs scale_alpha_manual scale_x_continuous theme
#' @importFrom hrbrthemes theme_ipsum
#' @importFrom plotly ggplotly
#' @export
plot_routefrequency <- function(gtfs, route = NULL){

  data <-
    GTFSwizard::filter_route(gtfs, route) %>%
    GTFSwizard::get_frequency(method = 'detailed') %>%
    dplyr::mutate(hour = as.numeric(hour))

  plot <-
    ggplot2::ggplot() +
    ggplot2::geom_line(data = data, ggplot2::aes(x = hour, y = frequency, color = route_id, alpha = service_pattern), linewidth = 1) +
    ggplot2::geom_point(data = data, ggplot2::aes(x = hour, y = frequency, color = route_id, alpha = service_pattern)) +
    ggplot2::labs(x = 'Hour of the day', y = 'Hourly Frequency', colour = 'Route(s)', linewidth = "", title = 'Route(s) Frequency') +
    ggplot2::scale_alpha_manual(values = c(.85, rep(.15, length(unique(data$service_pattern)) - 1)), labels = unique(data$service_pattern)) +
    hrbrthemes::theme_ipsum() +
    ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24), limits = c(0, 24)) +
    ggplot2::theme(legend.position = 'none')

  plotly <-
    suppressWarnings(
      plotly::ggplotly(plot,
                       tooltip = c('x', 'y', 'colour')
      )
    )

  return(plotly)
}

#' Plot System Average Headway by Hour
#'
#' `plot_headways` generates an interactive plot of the average headways (time between trips) by hour across the GTFS dataset. The plot displays hourly headway distributions for each service pattern and includes an overall average headway line.
#'
#' @param gtfs A GTFS object. This should ideally be of the `wizardgtfs` class, or it will be converted.
#'
#' @return A `plotly` interactive plot showing the hourly average headway (in minutes) across service patterns, including:
#'
#'   - Service Pattern Distribution: Lines for each service pattern, showing hourly headway values.
#'
#'   - Overall Average Headway: A dashed line marking the weighted overall average headway.
#'
#' @details
#' The function calculates hourly and overall average headways by weighting `pattern_frequency` and `trips` for each service pattern. The plot provides a visual representation of how average headways vary by hour and across service patterns.
#'
#' @examples
#' if (interactive()) {
#' # Plot average headway by hour for a GTFS object
#' plot_headways(for_rail_gtfs)
#' }
#'
#' @seealso
#' [GTFSwizard::get_headways()]
#'
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_line geom_point geom_hline labs scale_linetype_manual scale_alpha_manual scale_x_continuous theme
#' @importFrom hrbrthemes theme_ipsum
#' @importFrom plotly ggplotly
#' @export
# plot_headways
plot_headways <- function(gtfs){

  data <-
    GTFSwizard::get_headways(gtfs, method = 'by.hour') %>%
    dplyr::mutate(average.headway = round(headway_minutes / 60, 0),
                  weight = pattern_frequency * valid_trips,
                  hour = as.numeric(hour))

  overall.average <-
    weighted.mean(data$average.headway, data$weight, na.rm = TRUE) %>%
    round(., 1)


  plot <-
    ggplot(data) +
    geom_line(aes(x = hour, y = average.headway, color = service_pattern, group = service_pattern, alpha = service_pattern), linewidth = 1.25) +
    geom_point(aes(x = hour, y = average.headway, color = service_pattern, group = service_pattern, alpha = service_pattern), size = 1.25) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = overall.average, linetype = paste0('Overall\nAverage\nHeadway of\n', round(overall.average, 1), ' minutes')), linewidth = 1, color = '#113322') +
    ggplot2::labs(x = 'Hour of the Day ', title = 'System Average Headway', linetype = '', y = 'Average Headway (min)') +
    ggplot2::scale_linetype_manual(values = 'dashed') +
    ggplot2::scale_alpha_manual(values = c(.85, rep(.2, length(unique(data$service_pattern)) - 1))) +
    hrbrthemes::theme_ipsum() +
    ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24), limits = c(0, 24)) +
    ggplot2::theme(legend.position = 'none')

  plotly <-
    suppressWarnings(
      plotly::ggplotly(plot,
                       tooltip = c('x', 'y', 'yintercept')
      )
    )

  return(plotly)
}

#' Plot Transit Corridors
#'
#' @description
#' The `plot_corridor` function visualizes high-density transit corridors on a map. It overlays the identified corridors
#' on the route shapes from the GTFS data, providing a representation of the transit network and its key corridors.
#'
#' @param gtfs A GTFS object, preferably of class `wizardgtfs`. If not, the function will attempt to convert it using `GTFSwizard::as_wizardgtfs()`.
#' @param i A numeric value representing the percentile threshold for selecting high-density segments. Defaults to `0.01` (top 1\% of segments by trip frequency).
#' @param min.length A numeric value specifying the minimum corridor length (in meters) to retain. Defaults to `1500`.
#'
#' @return A `ggplot` object representing the transit network with corridors overlaid. The plot includes:
#' \describe{
#'   \item{Base map}{Route shapes from the GTFS data, displayed in gray.}
#'   \item{Corridors}{High-density transit corridors, colored uniquely for each corridor.}
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Extracts route shapes from the GTFS data using `get_shapes_sf`.
#'   \item Identifies transit corridors using the `get_corridor` function with the specified `i` and `min.length` parameters.
#'   \item Creates a map using `ggplot2` with:
#'     \enumerate{
#'       \item Route shapes as the base layer (gray lines).
#'       \item High-density transit corridors as colored lines, with transparency for visualization.
#'     }
#' }
#'
#' @note
#' Ensure the `gtfs` object includes valid `shapes` and `stop_times` tables for accurate visualization. The corridors are identified
#' using the `get_corridor` function, which relies on stop and trip data.
#'
#' @examples
#' plot_corridor(for_bus_gtfs, i = 0.02, min.length = 2000)
#'
#'
#' @seealso
#' [GTFSwizard::get_corridor()], [GTFSwizard::get_shapes_sf()]
#'
#' @importFrom ggplot2 ggplot geom_sf aes theme_minimal theme element_blank
#' @export

plot_corridor <- function(gtfs, i = 0.01, min.length = 1500) { # adicionar argumento 'base = TRUE' e 'corridors = "all"'

  plot <-
    ggplot() +
    geom_sf(data = get_shapes_sf(gtfs)$shapes, size = .5, color = 'gray40') +
    geom_sf(data = get_corridor(gtfs, i, min.length), aes(color = as.factor(corridor)), linewidth = 1.5, alpha = .75, fill = NA) +
    theme_minimal() +
    theme(legend.title = element_blank())

  return(plot)

  }

