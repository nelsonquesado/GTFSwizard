# Create a GTFS Feed from Tables

Creates and validates a GTFS Schedule feed from data frames. The
required files and fields follow the current GTFS Schedule reference,
including its conditional rules for route names, stop locations, and
service calendars.

## Usage

``` r
create_gtfs(
  agency,
  routes,
  trips,
  stop_times,
  stops,
  calendar = NULL,
  calendar_dates = NULL,
  shapes = NULL,
  frequencies = NULL,
  transfers = NULL,
  fare_attributes = NULL,
  fare_rules = NULL,
  build_shapes = TRUE,
  zipfile = NULL,
  ...
)
```

## Arguments

- agency, routes, trips, stop_times, stops:

  Required GTFS tables supplied as data frames. \`agency_id\` may be
  omitted when the feed contains one agency.

- calendar, calendar_dates:

  Service calendar tables. At least one must define every \`service_id\`
  used by \`trips\`.

- shapes, frequencies, transfers, fare_attributes, fare_rules:

  Optional GTFS tables.

- build_shapes:

  Logical. If \`TRUE\`, infer straight-line shapes when \`shapes\` is
  not supplied.

- zipfile:

  Optional output path ending in \`.zip\`.

- ...:

  Additional named GTFS tables, such as \`pathways\`, \`levels\`,
  \`feed_info\`, or GTFS Fares v2 tables.

## Value

A validated \`wizardgtfs\` object, invisibly written as well when
\`zipfile\` is supplied.

## References

\[GTFS Schedule
Reference\](https://gtfs.org/documentation/schedule/reference/)

## Examples

``` r
feed <- create_gtfs(
  agency = data.frame(
    agency_id = "A", agency_name = "Demo Transit",
    agency_url = "https://example.com",
    agency_timezone = "America/Fortaleza"
  ),
  routes = data.frame(
    route_id = "R1", agency_id = "A", route_short_name = "1",
    route_long_name = "Central", route_type = 3
  ),
  trips = data.frame(route_id = "R1", service_id = "WK", trip_id = "T1"),
  stop_times = data.frame(
    trip_id = "T1", arrival_time = c("08:00:00", "08:10:00"),
    departure_time = c("08:00:00", "08:10:00"),
    stop_id = c("S1", "S2"), stop_sequence = c(1, 2)
  ),
  stops = data.frame(
    stop_id = c("S1", "S2"), stop_name = c("First", "Second"),
    stop_lat = c(-3.73, -3.74), stop_lon = c(-38.52, -38.53)
  ),
  calendar = data.frame(
    service_id = "WK", monday = 1, tuesday = 1, wednesday = 1,
    thursday = 1, friday = 1, saturday = 0, sunday = 0,
    start_date = "20260101", end_date = "20261231"
  )
)
#> GTFSwizard: building straight-line shapes from ordered stop coordinates.
```
