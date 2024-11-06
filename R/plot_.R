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
#' \dontrun{
#' # Plot the frequency of trips by hour for a GTFS object
#' plot_frequency(for_gtfs)
#' }
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
    weighted.mean(data$frequency, data$pattern_frequency, na.rm = T)

  plot <-
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = data, ggplot2::aes(x = hour, y = frequency, color = 'Hourly\nDistribution\n', group = hour, weight = pattern_frequency), fill = 'gray', alpha = .65) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = overall.average, color = paste0('Overall\nAverage\nFrequency\n', round(overall.average, 1), ' trips')), linetype = 'dashed', linewidth = .75) +
    ggplot2::geom_line(data = dplyr::group_by(data, hour) %>% dplyr::reframe(frequency = round(weighted.mean(frequency, pattern_frequency, na.rm = T), 1)), ggplot2::aes(hour, frequency, color = 'Hourly\nAverage\nFrequency\n', group = NA), linewidth = 1) +
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
#' \dontrun{
#' # Plot frequency by hour for specific routes
#' plot_routefrequency(for_gtfs, route = for_gtfs$routes$route_id[1:2])
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

#route = c('004', '011')
plot_routefrequency <- function(gtfs, route = NULL){

  data <-
    GTFSwizard::filter_route(gtfs, route) %>%
    GTFSwizard::get_frequency(method = 'detailed') %>%
    dplyr::mutate(hour = as.numeric(hour))

  # data <-
  #   tibble(route_id = rep(route, times = rep(24, times = length(route))),
  #          hour = rep(1:24, times = length(route)),
  #          service_pattern = list(unique(freq$service_pattern))) %>%
  #   tidyr::unnest(cols = 'service_pattern') %>%
  #   dplyr::full_join(freq, .) %>%
  #   dplyr::mutate(frequency = dplyr::if_else(is.na(frequency), 0, frequency))
  #
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
#' \dontrun{
#' # Plot average headway by hour for a GTFS object
#' plot_headways(for_gtfs)
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
    dplyr::mutate(average.headway = round(average.headway / 60, 0),
                  weight = pattern_frequency * trips,
                  hour = as.numeric(hour))

  overall.average <-
    weighted.mean(data$average.headway, data$weight, na.rm = T) %>%
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


#' Plot Route-Specific Headway by Hour
#'
#' `plot_routeheadways` generates an interactive plot of the headways (time between trips) by hour for specified routes within the GTFS dataset. The plot includes the hourly headway distribution, hourly average headway, and an overall average headway line.
#'
#' @param gtfs A GTFS object, ideally of the `wizardgtfs` class, or it will be converted.
#' @param route A character vector specifying the route ID(s) to plot. If `NULL`, all routes will be included.
#'
#' @return A `plotly` interactive plot showing hourly headway distributions for the selected routes, including:
#'
#'   - Hourly Distribution: Boxplots of the headway distribution across hours.
#'
#'   - Hourly Average Headway: A line showing the weighted average headway for each hour.
#'
#'   - Overall Average Headway: A dashed line marking the system's overall average headway.
#'
#' @details
#' The function calculates hourly and overall average headways, weighting by `pattern_frequency`. It provides a visual analysis of headway patterns by hour, allowing insights into route-specific service frequency variations throughout the day.
#'
#' @examples
#' \dontrun{
#' # Plot the headway distribution by hour for specific routes
#' plot_routeheadways(for_gtfs, route = for_gtfs$trips$trip_id[1:2])
#' }
#'
#' @seealso
#' [GTFSwizard::get_headways()]
#'
#' @importFrom dplyr mutate group_by reframe
#' @importFrom ggplot2 ggplot geom_boxplot geom_hline geom_line labs scale_x_continuous scale_y_continuous scale_color_manual
#' @importFrom hrbrthemes theme_ipsum
#' @importFrom plotly ggplotly

plot_routeheadways <- function(gtfs, route = NULL){

  data <-
    GTFSwizard::filter_route(gtfs, route) %>%
    GTFSwizard::get_headways(method = 'detailed') %>%
    dplyr::mutate(hour = as.numeric(hour),
                  headway = headway/60)

  overall.average <-
    weighted.mean(data$headway, data$pattern_frequency, na.rm = T)

  plot <-
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = data, ggplot2::aes(x = hour, y = headway, color = 'Hourly\nDistribution\n', group = hour, weight = pattern_frequency), fill = 'gray', alpha = .65) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = overall.average, color = paste0('Overall\nAverage\nHeadway\n', round(overall.average, 1), ' trips')), linetype = 'dashed', linewidth = .75) +
    ggplot2::geom_line(data = dplyr::group_by(data, hour) %>% dplyr::reframe(headway = round(weighted.mean(headway, pattern_frequency, na.rm = T), 1)), ggplot2::aes(hour, headway, color = 'Hourly\nAverage\nFrequency\n', group = NA), linewidth = 1) +
    ggplot2::labs(x = 'Hour of the Day', y = 'Hourly headway', colour = '', title = 'Route(s) headway') +
    hrbrthemes::theme_ipsum() +
    ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18, 24), limits = c(0, 24)) +
    ggplot2::scale_y_continuous(limits = c(0, max(data$headway))) +
    ggplot2::scale_color_manual(values = c('#00BFC4', 'black', '#F8766D'))

  plotly <-
    suppressWarnings(
      plotly::ggplotly(plot,
                       tooltip = c('x', 'y')
      )
    )

  return(plotly)

}
