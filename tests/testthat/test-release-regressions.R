test_that("shape inference aligns distance units and accepts long stop sequences", {
  g <- minimal_feed()
  g$shapes <- NULL
  g$stop_times$shape_dist_traveled <- rep(c(0, .157, .314), 2)
  g <- as_wizardgtfs(g)
  expect_equal(get_distances(g)$distance, get_distances(g, distance_method = "straight")$distance)
  expect_gt(g$stop_times$shape_dist_traveled[3], 300)
  g$stop_times <- g$stop_times[rep(1:3, 600), ]
  g$stop_times$stop_sequence <- seq_len(1800)
  expect_equal(nrow(get_distances(g, "detailed", distance_method = "straight")), 1799L)
})

test_that("repairs preserve explicit service days and original transfer row identities", {
  g <- minimal_feed()
  g$stop_times$arrival_time[2] <- "00:05:00"
  g$stop_times$departure_time[2] <- "00:06:00"
  fixed <- repair_gtfs(g, "overnight")
  expect_equal(fixed$stop_times$arrival_time[1:3], c("23:50:00", "24:05:00", "24:20:00"))
  g$stop_times <- g$stop_times[-c(2,3), ]
  g$transfers <- tibble::tibble(from_stop_id=c("S1","S2"), to_stop_id=c("S2","S3"),
    transfer_type=0L, from_trip_id=c("T1","T2"), to_trip_id=c("T1","T2"))
  expect_warning(fixed <- repair_gtfs(g), "removed")
  audit <- repair_report(fixed)
  expect_equal(audit$row[audit$table == "transfers"], 1L)
  expect_equal(audit$action[audit$table == "transfers"], "remove_row")
  g$stop_times <- g$stop_times[g$stop_times$trip_id == "T2", ]
  expect_error(as_wizardgtfs(g, repair = "none"), "two stop-time records")
})

test_that("spatial cropping rejects untimed endpoints and removes dangling pathways", {
  g <- minimal_feed()
  box <- c(-38.5225,-3.7325,-38.5205,-3.7305)
  g$pathways <- tibble::tibble(pathway_id=c("X","Y"), from_stop_id=c("S1","S2"), to_stop_id="S3")
  expect_equal(filter_area(g, box, full_trips = FALSE)$pathways$pathway_id, "Y")
  g$stop_times$arrival_time[2] <- ""
  expect_error(filter_area(g, box, full_trips = FALSE), "both endpoints: T1")
})
