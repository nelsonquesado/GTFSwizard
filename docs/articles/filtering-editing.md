# Filtering, selecting, and editing feeds

GTFSwizard separates three related operations:

- filtering removes records and prunes related GTFS tables;
- selection stores groups or subsets as metadata without removing rows;
- editing changes times or trip structure in the returned object.

Assign each result to a new object when you need to compare scenarios.

## Filter a feed

Filter functions keep related identifiers consistent across the feed.

``` r

route_ids <- gtfs$routes$route_id[1:2]
route_feed <- filter_route(gtfs, route_ids)

service_id <- gtfs$trips$service_id[1]
service_feed <- filter_service(gtfs, service_id)

time_feed <- filter_time(gtfs, from = "06:00:00", to = "09:00:00")
```

[`filter_stop()`](https://opatp.github.io/GTFSwizard/reference/filter_functions.md)
and
[`filter_time()`](https://opatp.github.io/GTFSwizard/reference/filter_functions.md)
deliberately retain partial trips. This is useful for corridor,
express-service, and time-window experiments. The retained trip can
therefore begin or end at a different stop than it did in the source
feed.

``` r

stop_ids <- gtfs$stops$stop_id[1:5]
partial_feed <- filter_stop(gtfs, stop_ids)
```

Service patterns describe exact sets of services active on calendar
dates. Filtering a pattern retains precisely its represented dates and
services. With no explicit pattern,
[`filter_servicepattern()`](https://opatp.github.io/GTFSwizard/reference/filter_functions.md)
uses the most frequent active pattern.

``` r

typical_feed <- filter_servicepattern(gtfs)
#> Warning: GTFSwizard: no `servicepattern` supplied; using `servicepattern-1`,
#> the most frequent pattern.
```

Use
[`filter_date()`](https://opatp.github.io/GTFSwizard/reference/filter_functions.md)
only with dates inside the feed calendar. Calendar and calendar-date
tables are rebuilt to represent the retained dates.

``` r

active_date <- gtfs$dates_services$date[
  lengths(gtfs$dates_services$service_id) > 0
][1]
date_feed <- filter_date(gtfs, active_date)
```

## Select or group without filtering

[`selection()`](https://opatp.github.io/GTFSwizard/reference/selection.md)
resembles
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
but stores GTFS-specific selection metadata while leaving all feed
tables unchanged. Bare names create groups; logical expressions restrict
the records represented by those groups.

``` r

grouped <- selection(gtfs, route_id, direction_id)
attr(grouped, "selection")$groups
#> # A tibble: 6 × 6
#>   route_id direction_id n_stop_calls routes    trips      stops     
#>   <chr>           <int>        <int> <list>    <list>     <list>    
#> 1 6                   0         1260 <chr [1]> <chr [63]> <chr [20]>
#> 2 6                   1         1280 <chr [1]> <chr [64]> <chr [20]>
#> 3 7                   0          150 <chr [1]> <chr [15]> <chr [10]>
#> 4 7                   1          150 <chr [1]> <chr [15]> <chr [10]>
#> 5 8                   0          290 <chr [1]> <chr [29]> <chr [10]>
#> 6 8                   1          290 <chr [1]> <chr [29]> <chr [10]>

selected <- selection(
  gtfs,
  route_id,
  route_id %in% route_ids
)

selected <- unselection(selected)
```

## Edit scheduled times

The editing functions return modified copies. They do not write back to
the zip archive or object from which the feed was loaded.

``` r

trip_ids <- gtfs$trips$trip_id[1:2]
stop_ids <- gtfs$stops$stop_id[1:3]

delayed <- delay_trip(gtfs, trip = trip_ids, duration = 300)
faster <- edit_speed(gtfs, trips = trip_ids, stops = stop_ids, factor = 1.2)
fixed_dwell <- set_dwelltime(
  gtfs, duration = 30, trips = trip_ids, stops = stop_ids
)
scaled_dwell <- edit_dwelltime(
  gtfs, trips = trip_ids, stops = stop_ids, factor = 1.5
)
```

`duration` is expressed in seconds. A speed or dwell-time `factor` is a
multiplier: values above one increase the stated quantity and values
between zero and one decrease it.

## Split trips

[`split_trip()`](https://opatp.github.io/GTFSwizard/reference/split_trip.md)
can create `split + 1` approximately equal consecutive parts, or split
at selected internal stop IDs. It keeps the original stop order and
updates references handled by the package.

``` r

trip_id <- gtfs$trips$trip_id[1]
split_equal <- split_trip(gtfs, trip = trip_id, split = 1)
#> GTFSwizard: building straight-line shapes from ordered stop coordinates.

trip_stops <- gtfs$stop_times$stop_id[gtfs$stop_times$trip_id == trip_id]
if (length(trip_stops) > 2) {
  split_at_stop <- split_trip(gtfs, trip = trip_id, stops = trip_stops[2])
}
#> GTFSwizard: building straight-line shapes from ordered stop coordinates.
```

The first and final stops are not valid split boundaries. When several
trips are edited together, stop IDs are interpreted within each selected
trip.

## Merge and export scenarios

By default,
[`merge_gtfs()`](https://opatp.github.io/GTFSwizard/reference/merge_gtfs.md)
suffixes identifiers and their foreign-key references to avoid
collisions between feeds.

``` r

merged <- merge_gtfs(
  filter_route(gtfs, gtfs$routes$route_id[1]),
  filter_route(gtfs, gtfs$routes$route_id[2])
)
```

Export only when the scenario is ready.
[`write_gtfs()`](https://opatp.github.io/GTFSwizard/reference/write_gtfs.md)
writes a new archive at the requested path and does not overwrite the
original unless the same path is explicitly supplied.

``` r

output <- tempfile(fileext = ".zip")
write_gtfs(delayed, output)
unlink(output)
```
