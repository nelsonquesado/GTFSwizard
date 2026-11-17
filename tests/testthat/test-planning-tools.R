test_that("prepared routing preserves dated frequency journeys and extended times", {
  skip_if_not_installed("tidytransit")
  g <- frequency_planning_feed()
  p <- prepare_raptor(g, "08:00:00", "09:00:00", "2026-01-03")
  ref <- filter_date(filter_time(g, "08:00:00", "09:00:00"), "2026-01-03")
  expect_setequal(p$stop_times$trip_id, ref$stop_times$trip_id)
  expect_equal(sort(p$stop_times$arrival_time_num), sort(gtfs_time_to_seconds(ref$stop_times$arrival_time)))
  g <- minimal_feed()
  before <- data.table::copy(g)
  for(reverse in c(FALSE, TRUE)){
    origin <- if(reverse) "S3" else "S1"
    result <- tidy_raptor(g, "23:00:00", "25:00:00", "2026-01-01", origin,
                          arrival = reverse, keep = "shortest")
    expect_gt(nrow(result), 1L)
  }
  expect_equal(g, before)
})

test_that("distance sums, area crops, and scenario changes reflect the served stops", {
  g <- minimal_feed()
  cropped <- filter_area(g, c(-38.5225, -3.7325, -38.5205, -3.7305), full_trips = FALSE)
  expect_equal(nrow(cropped$stop_times), 4L)
  expect_equal(nrow(filter_area(g, c(-38.5225, -3.7325, -38.5205, -3.7305))$stop_times), 6L)
  for(method in c("shapes", "straight")){
    d <- get_distances(cropped, "detailed", distance_method = method)
    expect_equal(get_distances(cropped, distance_method = method)$distance, d$distance)
    expect_true(all(d$distance < get_distances(g, distance_method = method)$distance))
    expect_equal(nrow(get_speeds(cropped, "detailed", distance_method = method)), 2L)
  }
  comparison <- compare_scenarios(g, cropped, "2026-01-01")
  expect_lt(comparison$change[comparison$metric == "vehicle_km"], 0)
  expect_equal(comparison$change[comparison$metric == "departures"], 0)
})

test_that("time and reference repairs leave an audit trail", {
  g <- minimal_feed()
  g$stop_times$arrival_time[2] <- g$stop_times$departure_time[2] <- ""
  fixed <- repair_gtfs(g, "stop_times")
  expect_equal(fixed$stop_times$arrival_time[2], "24:05:00")
  expect_setequal(repair_report(fixed)$field, c("arrival_time", "departure_time"))
  g$stop_times$stop_id[2] <- "absent"
  expect_error(as_wizardgtfs(g, repair = "none"), "stop")
  expect_warning(clean <- as_wizardgtfs(g), "removed")
  expect_equal(repair_report(clean)$row, 2L)
})
