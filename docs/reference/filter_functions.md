# Filter a GTFS Feed

Filters a GTFS feed while preserving referential integrity across
routes, trips, stops, shapes, calendars, frequencies, fares, and
transfers.

## Usage

``` r
filter_servicepattern(gtfs, servicepattern = NULL)

filter_date(gtfs, dates = NULL)

filter_service(gtfs, service)

filter_route(gtfs, route, keep = TRUE)

filter_trip(gtfs, trip, keep = TRUE)

filter_stop(gtfs, stop)

filter_time(gtfs, from = "00:00:00", to = "48:00:00")
```

## Arguments

- gtfs:

  A GTFS object.

- servicepattern:

  Character vector of service-pattern IDs returned by
  \[get_servicepattern()\]. A pattern represents a distinct set of
  services active on a date. When \`NULL\`, the most frequent active
  pattern is used. The synthetic \`"No service"\` pattern cannot be used
  to filter trips.

- dates:

  Dates to retain. Values accepted by \[as.Date()\] are supported. When
  \`NULL\`, the latest available service date is used.

- service, route, trip, stop:

  Character vectors of IDs to retain.

- keep:

  Logical. If \`FALSE\`, the specified route or trip IDs are excluded.

- from, to:

  Inclusive GTFS time bounds in \`"HH:MM:SS"\` form. Hours may exceed
  24.

## Value

A \`wizardgtfs\` object.

## Details

\`filter_stop()\` retains the requested stop calls and therefore may
return partial trips. \`filter_time()\` retains individual stop calls
whose arrival or departure falls inside the inclusive time interval and
may also return partial trips. This behavior is useful for network
experiments. Route, trip, service, service-pattern, and date filters
retain complete trips.

\`filter_date()\` rewrites service availability as \`calendar_dates\`
additions for exactly the selected dates. Other filters preserve the
selected services' original date ranges and exceptions.

## References

\[GTFS Schedule
Reference\](https://gtfs.org/documentation/schedule/reference/)

## See also

\[GTFSwizard::as_wizardgtfs()\], \[GTFSwizard::get_servicepattern()\]

## Examples

``` r
typical <- filter_servicepattern(for_rail_gtfs)
#> Warning: GTFSwizard: no `servicepattern` supplied; using `servicepattern-1`, the most frequent pattern.
one_day <- filter_date(for_rail_gtfs, "2021-02-10")
one_route <- filter_route(for_rail_gtfs, for_rail_gtfs$routes$route_id[1])
two_trips <- filter_trip(for_rail_gtfs, for_rail_gtfs$trips$trip_id[1:2])
serving_stop <- filter_stop(for_rail_gtfs, for_rail_gtfs$stops$stop_id[1])
morning <- filter_time(for_rail_gtfs, from = "06:30:00", to = "10:00:00")
```
