test_that("AMB Barcelona filters can be exported, reimported, and merged", {
  g <- filter_route(bcn_bus_gtfs, bcn_bus_gtfs$routes$route_id[1])
  expect_equal(nrow(bcn_bus_gtfs$agency), 6L)
  expect_equal(nrow(bcn_bus_gtfs$routes), 128L)
  expect_equal(nrow(bcn_bus_gtfs$trips), 25122L)
  expect_equal(nrow(bcn_bus_gtfs$stop_times), 776187L)
  expect_equal(range(bcn_bus_gtfs$dates_services$date),
               as.Date(c("2017-01-27", "2026-12-31")))
  expect_true(all(g$routes$agency_id %in% g$agency$agency_id))
  path <- tempfile(fileext = ".zip")
  on.exit(unlink(path), add = TRUE)
  expect_no_warning(write_gtfs(g, path))
  expect_no_warning(back <- read_gtfs(path))
  expect_equal(nrow(back$trips), nrow(g$trips))
  expect_no_warning(merged <- merge_gtfs(g, g))
  expect_equal(nrow(merged$trips), 2L * nrow(g$trips))
})

test_that("AMB calendar keeps every date with a bounded pattern legend", {
  p <- plot_calendar(bcn_bus_gtfs, fill = "service_pattern", facet_by_year = TRUE,
                     max_patterns = 12L)
  expect_equal(range(p$data$date), as.Date(c("2017-01-27", "2026-12-31")))
  expect_equal(length(unique(p$data$service_pattern)), 239L)
  expect_lte(length(unique(p$data$.calendar_pattern)), 14L)
  expect_true("Other service patterns" %in% p$data$.calendar_pattern)
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("dense AMB service plots bound displayed patterns without filtering the feed", {
  original_dates <- bcn_bus_gtfs$dates_services
  for(fun in c("plot_routefrequency", "plot_headways", "plot_routeduration", "plot_servicesupply")){
    expect_no_warning(p <- do.call(fun, list(gtfs = bcn_bus_gtfs, max_patterns = 3L)))
    expect_lte(length(unique(p$data$service_pattern)), 3L)
    expect_no_warning(ggplot2::ggplot_build(p))
  }
  expect_identical(bcn_bus_gtfs$dates_services, original_dates)
})

test_that("services without active dates do not create missing-weight plots", {
  g <- minimal_feed()
  g$trips$service_id[2] <- "INACTIVE"
  inactive <- g$calendar
  inactive$service_id <- "INACTIVE"
  inactive[, c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")] <- 0L
  g$calendar <- dplyr::bind_rows(g$calendar, inactive)
  g <- as_wizardgtfs(unclass(g), build_shapes = FALSE)
  expect_equal(nrow(g$trips), 2L)
  expect_no_warning(frequency <- get_frequency(g))
  expect_identical(frequency$trip_id, "T1")
  expect_true(all(is.finite(frequency$pattern_frequency)))
  expect_no_warning(p <- plot_frequency(g))
  expect_true(all(is.finite(p$data$frequency)))
  expect_no_warning(ggplot2::ggplot_build(p))
  inactive_only <- filter_service(g, "INACTIVE")
  expect_error(plot_calendar(inactive_only), "no active calendar dates")
})

test_that("headways and dwell summaries agree with nominal frequency schedules", {
  g <- frequency_planning_feed()
  expect_no_warning(headways <- get_headways(g, "by_stop"))
  expect_equal(nrow(headways), 12L)
  expect_true(all(headways$headway_minutes == 10))
  expect_equal(sort(unique(headways$valid_trips)), c(77L, 86L))
  expect_no_warning(detailed <- get_headways(g, "detailed"))
  expect_equal(nrow(detailed), 978L)
  expect_true(all(detailed$headway_minutes == 10))
  at <- g$stop_times$trip_id == "T1" & g$stop_times$stop_sequence == 2L
  g$stop_times$departure_time[at] <- "00:02:00"
  at <- g$stop_times$trip_id == "T3" & g$stop_times$stop_sequence == 2L
  g$stop_times$departure_time[at] <- "00:01:30"
  expect_no_warning(dwell <- get_dwelltimes(g, method = "by_route"))
  expect_equal(sort(dwell$average.dwelltime), c(0, 0, 10, 20))
  expect_no_warning(hourly <- get_dwelltimes(g, method = "by_hour"))
  expect_equal(sort(hourly$average.dwelltime), c(5, 10))
  expect_true(all(hourly$trips == 6L))
})

test_that("AMB stop summaries match explicit service-pattern expansion", {
  g <- filter_route(bcn_bus_gtfs, bcn_bus_gtfs$routes$route_id[1])
  patterns <- get_servicepattern(g)
  expanded <- stop_call_instances(g) |>
    dplyr::inner_join(g$trips, by = "trip_id") |>
    dplyr::inner_join(patterns, by = "service_id", relationship = "many-to-many")
  expected_frequency <- expanded |>
    dplyr::distinct(trip_id, stop_id, direction_id, service_pattern, pattern_frequency) |>
    dplyr::count(stop_id, direction_id, service_pattern, pattern_frequency, name = "daily.frequency")
  expect_equal(get_frequency(g, "by_stop"), expected_frequency)
  expected_headways <- expanded |>
    dplyr::arrange(route_id, direction_id, stop_id, service_pattern, time_seconds) |>
    dplyr::group_by(route_id, direction_id, stop_id, service_pattern, pattern_frequency) |>
    dplyr::mutate(headway_minutes = (.data$time_seconds - dplyr::lag(.data$time_seconds)) / 60) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$headway_minutes), .data$headway_minutes >= 0) |>
    dplyr::group_by(stop_id, direction_id, service_pattern, pattern_frequency) |>
    dplyr::summarise(headway_minutes = mean(.data$headway_minutes),
                     valid_trips = dplyr::n(), .groups = "drop")
  expect_equal(get_headways(g, "by_stop"), expected_headways)
})

test_that("frequency trips contribute all departures and vehicle-hours", {
  g <- frequency_planning_feed()
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
  g <- frequency_planning_feed()
  expect_no_warning(p <- plot_frequency(g))
  expect_equal(p$data$frequency[p$data$hour == 10], 12)
  expect_equal(p$data$frequency[p$data$hour == 7], 6 * 5 / 7)
})

test_that("time filtering retains actual frequency departures rather than template clock times", {
  g <- frequency_planning_feed()
  expect_no_warning(filtered <- filter_time(g, "10:00:00", "10:12:00"))
  expect_equal(nrow(filtered$trips), 8L)
  expect_null(filtered$frequencies)
  expect_true(all(is_valid_gtfs_time(filtered$stop_times$arrival_time)))
  expect_equal(sort(unique(filtered$stop_times$arrival_time)),
               c("10:00:00", "10:01:00", "10:02:00", "10:10:00", "10:11:00", "10:12:00"))
  expect_equal(length(rle(filtered$stop_times$trip_id)$lengths), nrow(filtered$trips))
  expect_no_warning(frequency <- get_frequency(filtered, "by_route"))
  expect_true(all(frequency$daily.frequency == 2))
  expect_no_warning(as_wizardgtfs(unclass(filtered)))
})

test_that("trip filters retain parent stations absent from stop times", {
  g <- minimal_feed()
  g$stops$location_type <- 0L
  g$stops$parent_station <- c("ST", "", "")
  station <- g$stops[1, ]
  station$stop_id <- "ST"
  station$location_type <- 1L
  station$parent_station <- ""
  g$stops <- dplyr::bind_rows(g$stops, station)
  retained <- filter_trip(g, "T1")
  expect_true("ST" %in% retained$stops$stop_id)
  expect_false("ST" %in% retained$stop_times$stop_id)
  expect_no_warning(as_wizardgtfs(unclass(retained)))
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

test_that("the full AMB date-range selection preserves every source trip and calendar year", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")
  dates <- as.Date(as.integer(range(bcn_bus_gtfs$dates_services$date)), origin = "1970-01-01")
  shiny::testServer(explore_gtfs(bcn_bus_gtfs), {
    session$setInputs(selected_dates = dates, selected_services = character())
    expect_equal(nrow(filtered_gtfs()$trips), nrow(bcn_bus_gtfs$trips))
    expect_equal(range(filtered_gtfs()$dates_services$date), dates)
  })
})
