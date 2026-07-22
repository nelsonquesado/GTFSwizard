# Using the interactive GTFS dashboard

[`explore_gtfs()`](https://opatp.github.io/GTFSwizard/reference/explore_gtfs.md)
provides an interactive Shiny dashboard for inspecting, filtering,
plotting, editing, and exporting a feed. `shiny` and `leaflet` are
optional dependencies; `plotly` is optional when interactive charts are
requested.

## Start the dashboard

Pass an existing object when working from R.

``` r

library(GTFSwizard)
explore_gtfs(for_rail_gtfs)
```

In an interactive session, omitting the feed opens a file browser. This
is the simplest route for users who do not need to write R code.

``` r

explore_gtfs()
```

Static `ggplot2` charts are the default. Enable Plotly only when hover
and zoom behavior is useful; the calendar and trip-duration boxplot
remain static where the conversion would reduce readability.

``` r

explore_gtfs(for_rail_gtfs, plotly = TRUE)
```

## Filter the dashboard

The sidebar filters the working dashboard feed by routes, service
patterns, services, dates, time, and stops. Secondary choices are
recomputed from the current valid combination. An empty service
selection means all services, rather than no service.

Filters affect the maps, planning indicators, and service or performance
plots. Large route and service-pattern sets are summarized or limited in
plots where showing every class would make labels and legends
unreadable. Plot-specific `top_n` controls can expand those views when
required.

## Interpret the planning indicators

The system and route tables summarize scheduled supply, not observed
demand or performance. Common indicators include route and stop counts,
scheduled trips, service span, vehicle-hours, peak fleet, total network
length, and stop spacing.

Corridor share and hub share are proportions used to define which links
or stops are emphasized. Minimum corridor length is measured in meters.
Adjust these controls in the corridor and hub view; unsuitable or
incomplete feeds may not support every spatial analysis, in which case
the dashboard leaves the view unavailable and reports the reason.

## Edit without overwriting the source

The Edit tab applies
[`delay_trip()`](https://opatp.github.io/GTFSwizard/reference/delay_trip.md),
[`split_trip()`](https://opatp.github.io/GTFSwizard/reference/split_trip.md),
[`edit_speed()`](https://opatp.github.io/GTFSwizard/reference/edit_speed.md),
[`set_dwelltime()`](https://opatp.github.io/GTFSwizard/reference/set_dwelltime.md),
or
[`edit_dwelltime()`](https://opatp.github.io/GTFSwizard/reference/edit_dwelltime.md)
to the dashboard’s in-memory working copy. The object or zip archive
originally loaded is unchanged.

Review the affected trips and stops before applying an edit. Several
edits can be combined, and the Export GTFS control writes the currently
filtered and edited working feed to a destination selected by the user.
A source file is overwritten only when that exact path is deliberately
chosen as the export destination.

## Reproduce an analysis in R

The dashboard is useful for discovery. For a reproducible project,
translate the final choices into explicit function calls:

``` r

route_ids <- for_rail_gtfs$routes$route_id[1:2]
scenario <- for_rail_gtfs |>
  filter_route(route_ids) |>
  filter_time("06:00:00", "10:00:00") |>
  edit_speed(factor = 1.1)

write_gtfs(scenario, "morning-scenario.zip")
```

See [Filtering, selecting, and editing
feeds](https://opatp.github.io/GTFSwizard/articles/filtering-editing.md)
for the semantics of each operation.
