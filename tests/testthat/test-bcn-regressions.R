test_that("Barcelona filters retain referenced parent stations through export and merge", {
  g <- filter_route(bcn_bus_gtfs, bcn_bus_gtfs$routes$route_id[1])
  parents <- g$stops$parent_station
  parents <- parents[!is.na(parents) & nzchar(parents)]
  expect_true(length(parents) > 0)
  expect_true(all(parents %in% g$stops$stop_id))
  path <- tempfile(fileext = ".zip")
  on.exit(unlink(path), add = TRUE)
  expect_no_warning(write_gtfs(g, path))
  expect_no_warning(back <- read_gtfs(path))
  expect_equal(nrow(back$trips), nrow(g$trips))
  expect_no_warning(merged <- merge_gtfs(g, g))
  expect_equal(nrow(merged$trips), 2L * nrow(g$trips))
})

test_that("Barcelona frequency trips contribute all departures and vehicle-hours", {
  g <- filter_trip(bcn_bus_gtfs, bcn_bus_gtfs$frequencies$trip_id)
  expect_no_warning(supply <- plot_servicesupply(g))
  expect_equal(sort(supply$data$scheduled_trips), c(156, 174))
  expect_equal(sort(supply$data$vehicle_hours), c(5.2, 5.8))
  expect_no_warning(span <- plot_servicespan(g))
  expect_true(all(span$data$final_arrival > 21))
  expect_equal(sort(unique(plot_calendar(g)$data$count)), c(156, 174))
  expect_no_warning(routes <- planning_route_indicators(g, top_n = Inf))
  expect_no_warning(system <- planning_system_indicators(g, routes))
  expect_equal(system$Value[system$Indicator == "Median daily vehicle-hours"], 5.5)
  expect_gt(system$Value[system$Indicator == "Median service span"], 12)
  attr(routes, "trip_schedule") <- NULL
  attr(routes, "trip_instances") <- NULL
  expect_no_warning(rebuilt <- planning_system_indicators(g, routes))
  expect_equal(rebuilt, system)
})

test_that("system frequency sums directions and includes zero-service pattern hours", {
  g <- filter_trip(bcn_bus_gtfs, bcn_bus_gtfs$frequencies$trip_id)
  expect_no_warning(p <- plot_frequency(g))
  expect_equal(p$data$frequency[p$data$hour == 10], 12)
  expect_equal(p$data$frequency[p$data$hour == 7], 6 * 172 / 241)
})

test_that("time filtering retains actual frequency departures rather than template clock times", {
  g <- filter_trip(bcn_bus_gtfs, bcn_bus_gtfs$frequencies$trip_id)
  expect_no_warning(filtered <- filter_time(g, "10:00:00", "10:12:00"))
  expect_equal(nrow(filtered$trips), 8L)
  expect_null(filtered$frequencies)
  expect_true(all(is_valid_gtfs_time(filtered$stop_times$arrival_time)))
  expect_equal(sort(unique(filtered$stop_times$arrival_time)),
               c("10:00:00", "10:02:00", "10:10:00", "10:12:00"))
  expect_equal(length(rle(filtered$stop_times$trip_id)$lengths), nrow(filtered$trips))
  expect_no_warning(frequency <- get_frequency(filtered, "by_route"))
  expect_true(all(frequency$daily.frequency == 2))
  expect_no_warning(as_wizardgtfs(unclass(filtered)))
})

test_that("frequency expansion keeps trip-specific transfers and avoids ID collisions", {
  g <- minimal_feed()
  collision <- "T1.frequency1.1"
  g$trips$trip_id[g$trips$trip_id == "T2"] <- collision
  g$stop_times$trip_id[g$stop_times$trip_id == "T2"] <- collision
  g$frequencies <- tibble::tibble(
    trip_id = "T1", start_time = "23:50:00", end_time = "24:50:00", headway_secs = 600L
  )
  g$transfers <- tibble::tibble(
    from_stop_id = "S3", to_stop_id = "S1", transfer_type = 0L,
    from_trip_id = "T1", to_trip_id = collision
  )
  expect_no_warning(expanded <- filter_time(g))
  expect_equal(nrow(expanded$trips), 7L)
  expect_false(anyDuplicated(expanded$trips$trip_id) > 0L)
  expect_true(collision %in% expanded$trips$trip_id)
  expect_equal(nrow(expanded$transfers), 6L)
  expect_true(all(expanded$transfers$from_trip_id %in% expanded$trips$trip_id))
  expect_true(all(expanded$transfers$to_trip_id == collision))
  expect_no_warning(as_wizardgtfs(unclass(expanded)))
})

test_that("Barcelona dashboard shows an empty-corridor message without repeated warnings", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  g <- filter_route(bcn_bus_gtfs, bcn_bus_gtfs$routes$route_id[1])
  expect_no_warning(shiny::testServer(explore_gtfs(g), {
    session$setInputs(corridor_i = 0.01, corridor_min_length = 1e8, hubs_i = 0.05)
    expect_true(all(c("src", "width", "height") %in% names(output$corridor_plot)))
    expect_s3_class(output$network_map, "json")
    session$setInputs(selected_patterns = get_servicepattern(g)$service_pattern[1],
                      selected_services = character())
    expect_gt(nrow(filtered_gtfs()$trips), 0)
    expect_true(all(c("src", "width", "height") %in% names(output$service_supply_plot)))
  }))
})
