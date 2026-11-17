minimal_feed <- function(){
  create_gtfs(
    agency = data.frame(
      agency_id = "A", agency_name = "Test Transit",
      agency_url = "https://example.com",
      agency_timezone = "America/Fortaleza"
    ),
    routes = data.frame(
      route_id = "R", agency_id = "A",
      route_short_name = "1", route_type = 3
    ),
    trips = data.frame(
      route_id = "R", service_id = "S",
      trip_id = c("T1", "T2"), direction_id = 0
    ),
    stop_times = data.frame(
      trip_id = rep(c("T1", "T2"), each = 3),
      arrival_time = c(
        "23:50:00", "24:05:00", "24:20:00",
        "24:20:00", "24:35:00", "24:50:00"
      ),
      departure_time = c(
        "23:50:00", "24:06:00", "24:20:00",
        "24:20:00", "24:36:00", "24:50:00"
      ),
      stop_id = rep(c("S1", "S2", "S3"), 2),
      stop_sequence = rep(1:3, 2)
    ),
    stops = data.frame(
      stop_id = c("S1", "S2", "S3"),
      stop_name = c("One", "Two", "Three"),
      stop_lat = c(-3.73, -3.731, -3.732),
      stop_lon = c(-38.52, -38.521, -38.522)
    ),
    calendar = data.frame(
      service_id = "S",
      monday = 1, tuesday = 1, wednesday = 1, thursday = 1,
      friday = 1, saturday = 1, sunday = 1,
      start_date = "20260101", end_date = "20260107"
    ),
    build_shapes = TRUE
  )
}

frequency_planning_feed <- function(){
  g <- minimal_feed()
  second <- g$trips
  second$trip_id <- c("T3", "T4")
  second$service_id <- "W"
  g$trips$direction_id <- c(0L, 1L)
  second$direction_id <- c(0L, 1L)
  g$trips <- dplyr::bind_rows(g$trips, second)
  calls <- g$stop_times
  calls$trip_id <- ifelse(calls$trip_id == "T1", "T3", "T4")
  g$stop_times <- dplyr::bind_rows(g$stop_times, calls)
  g$stop_times$arrival_time <- rep(c("00:00:00", "00:01:00", "00:02:00"), 4)
  g$stop_times$departure_time <- g$stop_times$arrival_time
  g$calendar[, c("saturday", "sunday")] <- 0L
  weekend <- g$calendar
  weekend$service_id <- "W"
  weekend[, c("monday", "tuesday", "wednesday", "thursday", "friday")] <- 0L
  weekend[, c("saturday", "sunday")] <- 1L
  g$calendar <- dplyr::bind_rows(g$calendar, weekend)
  g$frequencies <- tibble::tibble(
    trip_id = c("T1", "T2", "T3", "T4"),
    start_time = c("07:30:00", "07:30:00", "09:00:00", "09:00:00"),
    end_time = "22:00:00", headway_secs = 600L
  )
  as_wizardgtfs(unclass(g), build_shapes = FALSE)
}
