test_that("creation validates and preserves GTFS service-day times", {
  feed <- minimal_feed()
  expect_s3_class(feed, "wizardgtfs")
  expect_true(any(feed$stop_times$arrival_time == "24:50:00"))
  expect_equal(nrow(feed$dates_services), 7)
  expect_equal(
    gtfs_time_to_seconds(c("0:0:0", "24:50:00", "120:05:09", "bad")),
    c(0, 89400, 432309, NA_real_)
  )
})

test_that("conversion filters trips with fewer than two stop-time records", {
  feed <- minimal_feed()
  feed$trips <- rbind(feed$trips, feed$trips[1, ])
  feed$trips$trip_id[3] <- "T_bad"
  feed$stop_times <- rbind(feed$stop_times, feed$stop_times[1, ])
  feed$stop_times$trip_id[7] <- "T_bad"

  expect_warning(
    cleaned <- as_wizardgtfs(feed, build_shapes = FALSE),
    "removed 1 trip\\(s\\) with fewer than two stop-time records"
  )
  expect_false("T_bad" %in% cleaned$trips$trip_id)
  expect_false("T_bad" %in% cleaned$stop_times$trip_id)
})

test_that("conversion filters stop-times referencing absent stops", {
  feed <- minimal_feed()
  feed$stop_times$stop_id[feed$stop_times$trip_id == "T1" &
                            feed$stop_times$stop_sequence == 2] <- "missing-stop"

  expect_warning(
    cleaned <- as_wizardgtfs(feed, build_shapes = FALSE),
    "removed 1 stop-time record\\(s\\) referencing stop_id values absent from `stops`"
  )
  expect_s3_class(cleaned, "wizardgtfs")
  expect_false("missing-stop" %in% cleaned$stop_times$stop_id)
  expect_true("T1" %in% cleaned$trips$trip_id)
})

test_that("conversion creates placeholder stops when all stop-times would be removed", {
  feed <- minimal_feed()
  feed$stop_times$stop_id <- paste0("missing-", feed$stop_times$stop_id)

  expect_warning(
    cleaned <- as_wizardgtfs(feed, build_shapes = FALSE),
    "created 3 placeholder stop\\(s\\)"
  )
  expect_s3_class(cleaned, "wizardgtfs")
  expect_true(all(feed$stop_times$stop_id %in% cleaned$stops$stop_id))
  expect_equal(nrow(cleaned$trips), nrow(feed$trips))
})

test_that("placeholder stops warn when coordinates default to zero", {
  feed <- minimal_feed()
  feed$stop_times$stop_id <- paste0("missing-", feed$stop_times$stop_id)
  feed$stops <- feed$stops[0, ]

  expect_warning(
    expect_warning(
      cleaned <- as_wizardgtfs(feed, build_shapes = FALSE),
      "created 3 placeholder stop\\(s\\)"
    ),
    "placeholder stop coordinates defaulted to \\(0, 0\\)"
  )
  placeholders <- cleaned$stops[grepl("^missing-", cleaned$stops$stop_id), ]
  expect_true(all(placeholders$stop_lat == 0))
  expect_true(all(placeholders$stop_lon == 0))
})

test_that("reading filters trips with fewer than two stop-time records", {
  feed <- minimal_feed()
  feed$trips <- rbind(feed$trips, feed$trips[1, ])
  feed$trips$trip_id[3] <- "T_bad"
  feed$stop_times <- rbind(feed$stop_times, feed$stop_times[1, ])
  feed$stop_times$trip_id[7] <- "T_bad"
  path <- tempfile(fileext = ".zip")
  feed$dates_services <- NULL
  class(feed) <- c("gtfs", "list")
  gtfsio::export_gtfs(feed, path = path)

  expect_warning(
    cleaned <- read_gtfs(path),
    "removed 1 trip\\(s\\) with fewer than two stop-time records"
  )
  expect_s3_class(cleaned, "wizardgtfs")
  expect_false("T_bad" %in% cleaned$trips$trip_id)
})

test_that("reading supports UTF-16 encoded GTFS text files", {
  feed <- minimal_feed()
  stop_ids <- c(S1 = "001", S2 = "002", S3 = "003")
  feed$stop_times$stop_id <- unname(stop_ids[feed$stop_times$stop_id])
  feed$stops$stop_id <- unname(stop_ids[feed$stops$stop_id])
  feed$dates_services <- NULL
  class(feed) <- c("gtfs", "list")
  tmpdir <- tempfile("utf16-gtfs-")
  dir.create(tmpdir)
  zipfile <- tempfile(fileext = ".zip")
  on.exit(unlink(c(tmpdir, zipfile), recursive = TRUE), add = TRUE)

  write_utf16_table <- function(data, path){
    line <- function(x) paste(x, collapse = ",")
    values <- apply(data, 1L, line)
    con <- file(path, open = "w", encoding = "UTF-16LE")
    tryCatch(
      writeLines(c(line(names(data)), values), con, useBytes = FALSE),
      finally = close(con)
    )
  }
  for(table_name in names(feed)){
    write_utf16_table(feed[[table_name]], file.path(tmpdir, paste0(table_name, ".txt")))
  }
  old_wd <- getwd()
  tryCatch({
    setwd(tmpdir)
    utils::zip(zipfile, files = paste0(names(feed), ".txt"), flags = "-q")
  }, finally = {
    setwd(old_wd)
  })

  expect_silent(feed <- read_gtfs(zipfile))
  expect_s3_class(feed, "wizardgtfs")
  expect_true(all(c("001", "002", "003") %in% feed$stops$stop_id))
  expect_warning(
    read_gtfs(zipfile, quiet = FALSE),
    "GTFS text files are encoded in UTF-16"
  )
})

test_that("stop and time filters retain partial trips and valid references", {
  feed <- minimal_feed()
  feed$transfers <- data.frame(
    from_stop_id = "S1", to_stop_id = "S3", transfer_type = 0
  )
  filtered <- filter_stop(feed, "S2")
  expect_equal(nrow(filtered$stop_times), 2)
  expect_equal(unique(filtered$stop_times$stop_id), "S2")
  expect_equal(nrow(filtered$trips), 2)
  expect_equal(nrow(filtered$transfers), 0)

  timed <- filter_time(feed, "24:00:00", "25:00:00")
  expect_equal(nrow(timed$trips), 2)
  expect_equal(nrow(timed$stop_times), 5)
  expect_true(all(
    gtfs_time_to_seconds(timed$stop_times$arrival_time) >= 24 * 3600
  ))
})

test_that("time edits propagate without wrapping at midnight", {
  feed <- minimal_feed()
  delayed <- delay_trip(feed, "T1", 900)
  expect_equal(
    delayed$stop_times$arrival_time[
      delayed$stop_times$trip_id == "T1" &
        delayed$stop_times$stop_sequence == 3
    ],
    "24:35:00"
  )

  dwell <- set_dwelltime(feed, 120, trips = "T1", stops = "S2")
  t1 <- dwell$stop_times[dwell$stop_times$trip_id == "T1", ]
  expect_equal(t1$arrival_time[2], "24:05:00")
  expect_equal(t1$departure_time[2], "24:07:00")
  expect_equal(t1$arrival_time[3], "24:21:00")
})

test_that("merge and split update identifiers and references", {
  feed <- minimal_feed()
  merged <- merge_gtfs(feed, feed)
  expect_equal(anyDuplicated(merged$trips$trip_id), 0L)
  expect_true(all(merged$stop_times$trip_id %in% merged$trips$trip_id))
  expect_true(all(merged$stop_times$stop_id %in% merged$stops$stop_id))

  split <- split_trip(feed, "T1", split = 1L)
  expect_true(all(c("T1.part1", "T1.part2") %in% split$trips$trip_id))
  expect_false("T1" %in% split$trips$trip_id)
  expect_true(all(split$stop_times$trip_id %in% split$trips$trip_id))
  expect_error(
    split_trip(feed, "T1", split = 2L),
    "`split` is too large.*maximum 1"
  )

  partial <- filter_stop(feed, "S2")
  expect_error(
    split_trip(partial, "T1", split = 1L),
    "`split` is too large.*maximum 0"
  )

  split_at_stop <- split_trip(feed, "T1", stops = "S2")
  expect_true(all(c("T1.part1", "T1.part2") %in% split_at_stop$trips$trip_id))
  expect_equal(
    split_at_stop$stop_times$stop_id[
      split_at_stop$stop_times$trip_id == "T1.part1"
    ],
    c("S1", "S2")
  )
  expect_equal(
    split_at_stop$stop_times$stop_id[
      split_at_stop$stop_times$trip_id == "T1.part2"
    ],
    c("S2", "S3")
  )
  expect_error(
    split_trip(feed, "T1", split = 1L, stops = "S2"),
    "either `split` or `stops`"
  )
  expect_error(
    split_trip(feed, "T1", stops = "S1"),
    "no matching internal stop"
  )
})

test_that("frequency rows are expanded with an exclusive end time", {
  feed <- minimal_feed()
  feed$frequencies <- data.frame(
    trip_id = "T1", start_time = "24:00:00",
    end_time = "25:00:00", headway_secs = 1200, exact_times = 0
  )
  frequency <- get_frequency(feed, "by_route")
  expect_equal(sum(frequency$daily.frequency), 4)
})

test_that("get defaults, aliases, and direction groupings are consistent", {
  feed <- minimal_feed()
  getters <- list(
    get_frequency(feed),
    get_headways(feed),
    get_distances(feed),
    get_durations(feed),
    get_speeds(feed),
    get_dwelltimes(feed)
  )
  expect_true(all(vapply(getters, function(x){
    all(c("trip_id", "direction_id") %in% names(x))
  }, logical(1))))
  expect_equal(get_frequency(feed, "by_trip"), get_frequency(feed, "by.trip"))
  expect_equal(get_distances(feed, "by_route"), get_distances(feed, "by.route"))
  expect_true("direction_id" %in% names(get_fleet(feed, "by_route")))
})

test_that("underscore argument names preserve legacy dotted aliases", {
  feed <- minimal_feed()
  expect_equal(
    get_dwelltimes(feed, max_dwelltime = 60),
    get_dwelltimes(feed, max.dwelltime = 60)
  )
  expect_equal(
    get_corridor(feed, i = 1, min_length = 0),
    get_corridor(feed, i = 1, min.length = 0)
  )
  expect_equal(
    merge_gtfs(feed, feed),
    merge_gtfs(gtfs.x = feed, gtfs.y = feed)
  )
  path <- tempfile(fileext = ".zip")
  write_gtfs(feed, path)
  expect_equal(read_gtfs(file_path = path), read_gtfs(file.path = path))
})

test_that("tidy_raptor validates inputs and returns documented columns", {
  skip_if_not_installed("tidytransit")
  skip_if_not_installed("data.table")
  skip_if_not_installed("hms")
  feed <- minimal_feed()
  result <- tidy_raptor(
    feed,
    min_departure = "23:00:00",
    max_arrival = "25:00:00",
    dates = max(feed$dates_services$date),
    stop_ids = "S1",
    time_range = 3600,
    max_transfers = 1L,
    keep = "shortest"
  )
  expect_true(all(c(
    "from_stop_id", "to_stop_id", "travel_time",
    "departure_time", "arrival_time", "transfers"
  ) %in% names(result)))
  expect_error(
    tidy_raptor(feed, stop_ids = "S1", keep = "invalid"),
    "^GTFSwizard:"
  )
})

test_that("date filters retain exact dates and usable service metadata", {
  feed <- minimal_feed()
  selected_dates <- feed$dates_services$date[c(1, 3)]
  filtered <- filter_date(feed, selected_dates)

  expect_null(filtered[["calendar"]])
  expect_equal(sort(filtered$calendar_dates$date), sort(selected_dates))
  expect_equal(sort(filtered$dates_services$date), sort(selected_dates))
  expect_gt(nrow(get_frequency(filtered)), 0)
  expect_gt(nrow(get_headways(filtered)), 0)

  feed$calendar_dates <- data.frame(
    service_id = "unused", date = as.Date("2027-01-01"), exception_type = 2L
  )
  pruned <- filter_route(feed, "R")
  expect_null(pruned[["calendar_dates"]])
})

test_that("package assertions use the standard condition prefix", {
  expect_error(plot_calendar(minimal_feed(), ncol = 0), "^GTFSwizard:")
  expect_error(filter_route(minimal_feed(), "R", keep = NA), "^GTFSwizard:")
})

test_that("selection groups records without altering GTFS tables", {
  feed <- minimal_feed()
  grouped <- selection(feed, route_id, direction_id)
  groups <- attr(grouped, "selection")$groups

  expect_s3_class(grouped, "wizardgtfs_selected")
  expect_equal(attr(grouped, "selection")$group_vars, c("route_id", "direction_id"))
  expect_equal(groups$n_stop_calls, 6)
  expect_identical(grouped$stop_times, feed$stop_times)

  selected <- selection(grouped, stop_id %in% "S2", add = TRUE)
  expect_equal(attr(selected, "selection")$groups$n_stop_calls, 2)
  expect_equal(attr(selected, "selection")$stops, "S2")
  expect_identical(unselection(selected)$stop_times, feed$stop_times)
})

test_that("selection supports computed groups and rejects invalid results", {
  feed <- minimal_feed()
  grouped <- selection(
    feed,
    route_direction = paste(route_id, direction_id, sep = "-")
  )
  expect_equal(
    attr(grouped, "selection")$groups$route_direction,
    "R-0"
  )
  expect_error(
    selection(feed, rep(route_id, 2)),
    "returned 12 values"
  )
})

test_that("selection evaluates package spatial predicates in caller contexts", {
  feed <- minimal_feed()
  area <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = -38.523, ymin = -3.733, xmax = -38.520, ymax = -3.729),
    crs = sf::st_crs(4326)
  ))
  selected <- selection(feed, geometry %intersects% area)
  expect_s3_class(selected, "wizardgtfs_selected")
  expect_true(length(attr(selected, "selection")$stops) >= 1)
})

test_that("write and read preserve a standards-compliant feed", {
  feed <- minimal_feed()
  path <- tempfile(fileext = ".zip")
  write_gtfs(feed, path)
  restored <- read_gtfs(path)

  expect_equal(nrow(restored$stop_times), nrow(feed$stop_times))
  expect_equal(restored$stop_times$stop_id, feed$stop_times$stop_id)
  expect_true(all(restored$stop_times$trip_id %in% restored$trips$trip_id))
})
