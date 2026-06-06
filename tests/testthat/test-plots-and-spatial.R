test_that("spatial conversions round-trip standard columns", {
  feed <- minimal_feed()
  shapes <- get_shapes_sf(feed$shapes)
  expect_s3_class(shapes, "sf")
  expect_equal(sf::st_crs(shapes)$epsg, 4326)
  shape_table <- get_shapes_df(shapes)
  expect_true(all(diff(shape_table$shape_dist_traveled) >= 0))

  stops <- get_stops_sf(feed$stops)
  expect_s3_class(stops, "sf")
  expect_equal(sf::st_crs(latlon2epsg(stops))$IsGeographic, FALSE)
})

test_that("all-missing shape distances remain missing without warnings", {
  feed <- minimal_feed()
  feed$shapes$shape_dist_traveled <- NA_real_
  expect_no_warning(shapes <- get_shapes_sf(feed$shapes))
  expect_true(all(is.na(shapes$shape_dist_traveled)))
  expect_false(any(is.infinite(shapes$shape_dist_traveled)))
})

test_that("all public static plots return ggplot objects", {
  feed <- minimal_feed()
  expect_s3_class(plot(feed), "ggplot")
  expect_s3_class(plot_frequency(feed), "ggplot")
  expect_s3_class(plot_routefrequency(feed), "ggplot")
  expect_s3_class(plot_headways(feed), "ggplot")
  expect_s3_class(plot_calendar(feed), "ggplot")
  expect_s3_class(plot_servicespan(feed), "ggplot")
  expect_s3_class(plot_serviceheatmap(feed), "ggplot")
  expect_s3_class(plot_routeduration(feed), "ggplot")
  expect_s3_class(plot_servicesupply(feed), "ggplot")
  expect_s3_class(plot_hubs(feed, i = 1), "ggplot")
  expect_s3_class(plot_corridor(feed, i = 1, min_length = 0), "ggplot")
})

test_that("hub counts are computed before identifier list-columns", {
  hubs <- get_hubs(minimal_feed())
  expect_equal(hubs$n_trip, rep(2L, 3))
  expect_equal(hubs$n_routes, rep(1L, 3))
  expect_true(all(lengths(hubs$trip_id) == 2L))
  expect_true(all(lengths(hubs$route_id) == 1L))
})

test_that("map plots use readable layer order, labels, and legends", {
  feed <- minimal_feed()
  system_plot <- plot(feed)
  expect_true(all(sf::st_geometry_type(system_plot$layers[[1]]$data) == "POINT"))
  expect_true(all(sf::st_geometry_type(system_plot$layers[[2]]$data) == "LINESTRING"))
  expect_equal(system_plot$layers[[1]]$aes_params$size, 0.9)

  route_plot <- plot_routefrequency(feed)
  expect_s3_class(route_plot$layers[[1]]$geom, "GeomTile")
  selected_route_plot <- plot_routefrequency(feed, route = "R")
  expect_false(identical(selected_route_plot$theme$legend.position, "none"))

  corridors <- get_corridor(feed, i = 1, min_length = 0)
  expect_equal(corridors$corridor, "Corridor 1")
  expect_null(plot_corridor(feed, i = 1, min_length = 0)$labels$colour)

  hubs <- plot_hubs(feed, i = 1)
  expect_lte(nrow(hubs$layers[[2]]$data), 40)
})

test_that("route-frequency plot limits unselected feeds to top routes", {
  feed <- minimal_feed()
  extra_trips <- feed$trips
  extra_trips$route_id <- "R2"
  extra_trips$trip_id <- paste0(extra_trips$trip_id, "-R2")
  extra_stop_times <- feed$stop_times
  extra_stop_times$trip_id <- paste0(extra_stop_times$trip_id, "-R2")
  feed$routes <- rbind(
    feed$routes,
    transform(feed$routes, route_id = "R2", route_short_name = "2")
  )
  feed$trips <- rbind(feed$trips, extra_trips)
  feed$stop_times <- rbind(feed$stop_times, extra_stop_times)
  feed <- as_wizardgtfs(feed)

  limited <- plot_routefrequency(feed, top_n = 1)
  expect_equal(length(unique(limited$data$route_id)), 1)
})

test_that("planning plots identify their observational units", {
  feed <- minimal_feed()
  expect_match(plot_servicespan(feed)$labels$subtitle, "Each line")
  expect_match(plot_serviceheatmap(feed)$labels$subtitle, "Each tile")
  expect_match(plot_routeduration(feed)$labels$subtitle, "Each observation")
  expect_match(plot_servicesupply(feed)$labels$subtitle, "Each bar")
})

test_that("calendar service-pattern legends are discrete in both layouts", {
  feed <- minimal_feed()
  wrapped <- plot_calendar(feed, fill = "service_pattern")
  faceted <- plot_calendar(
    feed, facet_by_year = TRUE, fill = "service_pattern"
  )
  expect_s3_class(wrapped$scales$get_scales("fill"), "ScaleDiscrete")
  expect_s3_class(faceted$scales$get_scales("fill"), "ScaleDiscrete")
})

test_that("planning indicators expose schedule-based system and route metrics", {
  feed <- minimal_feed()
  system <- planning_system_indicators(feed)
  routes <- planning_route_indicators(feed, top_n = 2L)
  expect_true(all(c("Indicator", "Value", "Unit") %in% names(system)))
  expect_true(all(c(
    "daily_trips", "average_headway_minutes", "average_speed_kmh",
    "average_duration_minutes"
  ) %in% names(routes)))
  expect_lte(nrow(routes), 2L)
})

test_that("corridor plotting tolerates numeric stop IDs in stops table", {
  feed <- minimal_feed()
  ids <- stats::setNames(seq_len(nrow(feed$stops)), feed$stops$stop_id)
  feed$stop_times$stop_id <- as.character(ids[feed$stop_times$stop_id])
  feed$stops$stop_id <- seq_len(nrow(feed$stops))

  expect_s3_class(get_corridor(feed, i = 1, min_length = 0), "sf")
  expect_s3_class(plot_corridor(feed, i = 1, min_length = 0), "ggplot")
})

test_that("explorer requires an explicit feed outside interactive sessions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  if(interactive()){
    expect_s3_class(explore_gtfs(minimal_feed()), "shiny.appobj")
  } else {
    expect_error(explore_gtfs(), "required in non-interactive sessions")
  }
})

test_that("explorer dispatches on a feed selected from the file browser", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  path <- tempfile(fileext = ".zip")
  write_gtfs(minimal_feed(), path)

  browse_explorer <- explore_gtfs
  mock_environment <- new.env(parent = environment(browse_explorer))
  mock_environment$interactive <- function() TRUE
  mock_environment$file.choose <- function() path
  environment(browse_explorer) <- mock_environment

  expect_s3_class(browse_explorer(), "shiny.appobj")
})

test_that("explorer tolerates feeds without shape identifiers", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  feed <- minimal_feed()
  feed$trips$shape_id <- NULL
  feed$shapes <- feed$shapes[0, ]

  app <- explore_gtfs(feed)
  expect_s3_class(app, "shiny.appobj")
  suppressMessages(suppressWarnings(
    shiny::testServer(app, {
      session$flushReact()
      expect_s3_class(output$network_map, "json")
    })
  ))
})

test_that("summary returns a summary object", {
  result <- summary(minimal_feed())
  expect_s3_class(result, "summary.wizardgtfs")
  expect_equal(result$routes, 1)
  expect_snapshot(print(result))
})
