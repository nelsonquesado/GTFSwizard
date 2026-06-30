# GTFSwizard Cheat Sheet

Compact workflow reference for GTFSwizard 1.2.1.

## 1. Load Or Create A Feed

```r
library(GTFSwizard)

gtfs <- GTFSwizard::for_rail_gtfs
bus_gtfs <- GTFSwizard::for_bus_gtfs

# gtfs <- read_gtfs("path/to/feed.zip")
# gtfs <- as_wizardgtfs(gtfs_list)

created_gtfs <- create_gtfs(
  agency = gtfs$agency,
  routes = gtfs$routes,
  trips = gtfs$trips,
  stop_times = gtfs$stop_times,
  stops = gtfs$stops,
  calendar = gtfs$calendar,
  calendar_dates = gtfs$calendar_dates,
  shapes = gtfs$shapes
)
```

The examples below use the embedded `for_rail_gtfs` and `for_bus_gtfs`
objects, so they can be run without downloading an external feed. `read_gtfs()`,
`as_wizardgtfs()`, and `create_gtfs()` return a `wizardgtfs` object.

## 2. Inspect The Feed

```r
summary(gtfs)
plot(gtfs)

get_servicepattern(gtfs)
get_shapes_sf(gtfs)
get_stops_sf(gtfs)
```

Use `summary()` for table counts, service range, and spacing diagnostics. Use
`plot()` for a quick network map. `get_servicepattern()` reports active service
patterns and uses `"No service"` with `service_id = NA` for calendar dates that
have no trips.

## 3. Select Parts Of The Feed

```r
route_choice <- gtfs$routes$route_id[1]
trip_choice <- gtfs$trips$trip_id[1]
service_choice <- gtfs$trips$service_id[1]
stop_choice <- gtfs$stops$stop_id[1]
pattern_choice <- get_servicepattern(gtfs)$service_pattern[1]

gtfs |> selection(route_id)
gtfs |> selection(route_id %in% route_choice)
gtfs |> selection(stop_id %in% stop_choice)

filter_route(gtfs, route_choice)
filter_service(gtfs, service_choice)
filter_servicepattern(gtfs, pattern_choice)
filter_trip(gtfs, trip_choice)
filter_stop(gtfs, stop_choice)
filter_date(gtfs, as.Date("2021-12-31"))
filter_time(gtfs, from = "06:00:00", to = "09:00:00")
```

`selection()` is useful when combining filters. The `filter_*()` functions are
explicit one-step tools. Use `filter_servicepattern()` with active service
patterns; `"No service"` describes trip-free calendar days and is not a trip
filter.

## 4. Analyze Operations

```r
get_frequency(gtfs)
get_headways(gtfs, method = "by_hour")
get_dwelltimes(gtfs, max_dwelltime = 90, method = "by_route")
get_speeds(gtfs, method = "by_route")
get_durations(gtfs, method = "by_route")
get_distances(gtfs)
get_fleet(gtfs)
get_1stdeparture(gtfs)
```

Check each help page for its observational unit. Common methods include
`by_trip`, `by_route`, `by_hour`, and `detailed`.

## 5. Plot Planning Indicators

```r
plot_frequency(gtfs)
plot_routefrequency(gtfs)
plot_headways(gtfs)
plot_servicespan(gtfs, top_n = 20)
plot_serviceheatmap(gtfs)
plot_servicesupply(gtfs, top_n = 20)
plot_routeduration(gtfs, top_n = 20)
plot_calendar(gtfs, facet_by_year = TRUE)
```

These functions return `ggplot2` objects and can be customized with regular
`ggplot2` layers. In `plot_calendar()`, dates without active service are shown
as `0` trips or `"No service"`, depending on the fill mode.

## 6. Find Corridors And Hubs

```r
get_corridor(gtfs, i = 0.01, min_length = 1500)
plot_corridor(gtfs, i = 0.01, min_length = 1500)

get_hubs(gtfs)
plot_hubs(gtfs, i = 0.05)
```

Increase `i` to show fewer, stronger corridors or hubs. Decrease it to show
more candidates.

## 7. Edit A Feed

```r
edit_speed(
  gtfs,
  trips = gtfs$trips$trip_id[1:2],
  stops = "all",
  factor = 1.25
)

edit_dwelltime(
  gtfs,
  trips = gtfs$trips$trip_id[1:2],
  stops = gtfs$stops$stop_id[1:2],
  factor = 1.5
)

set_dwelltime(
  gtfs,
  duration = 30,
  trips = gtfs$trips$trip_id[1:2],
  stops = gtfs$stops$stop_id[1:2]
)

delay_trip(gtfs, trip = gtfs$trips$trip_id[1], duration = 300)
split_trip(gtfs, trip = gtfs$trips$trip_id[1], split = 1)
merge_gtfs(gtfs, gtfs, suffix = TRUE)
```

Editing functions keep the GTFS structure consistent while preserving partial
trips when they are useful for experimentation.

## 8. Explore Interactively

```r
if (interactive()) {
  explore_gtfs(gtfs)
  explore_gtfs()
}
```

Call `explore_gtfs()` without an argument to choose a GTFS `.zip` file from a
browse window.

## 9. Save Results

```r
zipfile <- tempfile(fileext = ".zip")
write_gtfs(gtfs, zipfile)
```

Use `write_gtfs()` after validating edits and plots.
