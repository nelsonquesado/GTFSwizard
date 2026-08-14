devtools::load_all(quiet = TRUE)

figure_width <- 10
figure_height <- 6.4
figure_dpi <- 160

save_figure <- function(filename, plot){
  ggplot2::ggsave(
    filename = file.path("figs", filename),
    plot = plot,
    width = figure_width,
    height = figure_height,
    units = "in",
    dpi = figure_dpi,
    bg = "white"
  )
}

route_id <- for_bus_gtfs$routes$route_id[3]

figures <- list(
  "plot.for_bus_gtfs.png" = plot(for_bus_gtfs),
  "get_routefrequency.png" = plot_routefrequency(
    for_bus_gtfs, route = route_id
  ),
  "plot_calendar.png" = plot_calendar(
    for_bus_gtfs, facet_by_year = TRUE
  ),
  "plot_corridor.png" = plot_corridor(for_bus_gtfs),
  "plot_hubs.png" = plot_hubs(for_bus_gtfs),
  "plot_frequency.png" = plot_frequency(for_bus_gtfs),
  "plot_routefrequency.png" = plot_routefrequency(for_bus_gtfs),
  "plot_headway.png" = plot_headways(for_bus_gtfs),
  "plot_routeduration.png" = plot_routeduration(for_bus_gtfs),
  "plot_serviceheatmap.png" = plot_serviceheatmap(for_bus_gtfs),
  "plot_servicespan.png" = plot_servicespan(for_bus_gtfs),
  "plot_servicesupply.png" = plot_servicesupply(for_bus_gtfs)
)

invisible(Map(save_figure, names(figures), figures))

