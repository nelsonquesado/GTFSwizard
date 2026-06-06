test_that("Fortaleza bus feed remains warning-free in spatial workflows", {
  skip_on_cran()

  expect_no_warning(shapes <- get_shapes_sf(for_bus_gtfs))
  expect_true(all(is.na(shapes$shapes$shape_dist_traveled)))
  expect_false(any(is.infinite(shapes$shapes$shape_dist_traveled)))

  expect_no_warning(system_plot <- plot(for_bus_gtfs))
  expect_s3_class(system_plot, "ggplot")
  expect_no_warning(route_plot <- plot_routefrequency(for_bus_gtfs))
  expect_lte(length(unique(route_plot$layers[[1]]$data$route_id)), 25)
  expect_no_warning(hubs <- plot_hubs(for_bus_gtfs))
  expect_lte(nrow(hubs$layers[[2]]$data), 40)
  expect_gt(max(hubs$layers[[2]]$data$n_routes), 1)
  expect_no_warning(corridors <- plot_corridor(for_bus_gtfs))
  expect_s3_class(corridors, "ggplot")
  expect_no_warning(span <- plot_servicespan(for_bus_gtfs, top_n = 10L))
  expect_s3_class(span, "ggplot")
  expect_no_warning(heatmap <- plot_serviceheatmap(for_bus_gtfs))
  expect_s3_class(heatmap, "ggplot")
  expect_no_warning(duration <- plot_routeduration(for_bus_gtfs, top_n = 10L))
  expect_s3_class(duration, "ggplot")
  expect_no_warning(supply <- plot_servicesupply(for_bus_gtfs, top_n = 10L))
  expect_s3_class(supply, "ggplot")
  expect_no_warning(app <- explore_gtfs(for_bus_gtfs))
  expect_s3_class(app, "shiny.appobj")
})

test_that("Fortaleza bus feed supports partial filters and grouped selection", {
  skip_on_cran()

  stop <- for_bus_gtfs$stops$stop_id[1]
  partial <- filter_stop(for_bus_gtfs, stop)
  expect_true(nrow(partial$stop_times) > 0)
  expect_equal(unique(partial$stop_times$stop_id), stop)
  expect_true(all(partial$stop_times$trip_id %in% partial$trips$trip_id))

  grouped <- selection(
    for_bus_gtfs,
    route_id,
    direction_id,
    route_id %in% for_bus_gtfs$routes$route_id[1:3]
  )
  info <- attr(grouped, "selection")
  expect_equal(info$group_vars, c("route_id", "direction_id"))
  expect_true(all(info$routes %in% for_bus_gtfs$routes$route_id[1:3]))
  expect_true(nrow(info$groups) >= 3)
})

test_that("service pattern notation remains stable for the bus feed", {
  skip_on_cran()

  patterns <- get_servicepattern(for_bus_gtfs)
  expect_true(all(grepl("^servicepattern-[0-9]+$", patterns$service_pattern)))
  expect_identical(
    as.character(patterns$service_pattern),
    paste0("servicepattern-", seq_len(nrow(patterns)))
  )
})
