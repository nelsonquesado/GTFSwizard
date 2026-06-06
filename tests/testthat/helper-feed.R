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
