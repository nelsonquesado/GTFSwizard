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