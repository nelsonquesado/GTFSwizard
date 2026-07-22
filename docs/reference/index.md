# Package index

## Package overview

- [`GTFSwizard`](https://opatp.github.io/GTFSwizard/reference/GTFSwizard-package.md)
  [`GTFSwizard-package`](https://opatp.github.io/GTFSwizard/reference/GTFSwizard-package.md)
  : GTFSwizard: Create, Explore, and Edit GTFS Feeds

## Create, read, and export feeds

Build the GTFSwizard object used throughout the package.

- [`create_gtfs()`](https://opatp.github.io/GTFSwizard/reference/create_gtfs.md)
  : Create a GTFS Feed from Tables
- [`read_gtfs()`](https://opatp.github.io/GTFSwizard/reference/read_gtfs.md)
  : Read a GTFS Feed
- [`as_wizardgtfs()`](https://opatp.github.io/GTFSwizard/reference/as_wizardgtfs.md)
  : Convert a GTFS Feed to \`wizardgtfs\`
- [`write_gtfs()`](https://opatp.github.io/GTFSwizard/reference/write_gtfs.md)
  : Write a GTFS Feed
- [`merge_gtfs()`](https://opatp.github.io/GTFSwizard/reference/merge_gtfs.md)
  : Merge Two GTFS Feeds

## Filter, select, and group

Focus an analysis while retaining a valid, internally consistent feed.

- [`filter_servicepattern()`](https://opatp.github.io/GTFSwizard/reference/filter_functions.md)
  [`filter_date()`](https://opatp.github.io/GTFSwizard/reference/filter_functions.md)
  [`filter_service()`](https://opatp.github.io/GTFSwizard/reference/filter_functions.md)
  [`filter_route()`](https://opatp.github.io/GTFSwizard/reference/filter_functions.md)
  [`filter_trip()`](https://opatp.github.io/GTFSwizard/reference/filter_functions.md)
  [`filter_stop()`](https://opatp.github.io/GTFSwizard/reference/filter_functions.md)
  [`filter_time()`](https://opatp.github.io/GTFSwizard/reference/filter_functions.md)
  : Filter a GTFS Feed
- [`selection()`](https://opatp.github.io/GTFSwizard/reference/selection.md)
  [`unselection()`](https://opatp.github.io/GTFSwizard/reference/selection.md)
  : Group or Select GTFS Records Without Filtering the Feed

## Analyze scheduled service

Calculate service patterns and planning or operational indicators.

- [`get_servicepattern()`](https://opatp.github.io/GTFSwizard/reference/get_servicepattern.md)
  : Identify Recurring Daily Service Patterns
- [`get_frequency()`](https://opatp.github.io/GTFSwizard/reference/get_frequency.md)
  : Calculate Scheduled Service Frequency
- [`get_headways()`](https://opatp.github.io/GTFSwizard/reference/get_headways.md)
  : Calculate Scheduled Headways
- [`get_dwelltimes()`](https://opatp.github.io/GTFSwizard/reference/get_dwelltimes.md)
  : Calculate Dwell Times in GTFS Data
- [`get_durations()`](https://opatp.github.io/GTFSwizard/reference/get_durations.md)
  : Calculate Trip Durations in GTFS Data
- [`get_distances()`](https://opatp.github.io/GTFSwizard/reference/get_distances.md)
  : Calculate Distances in GTFS Data
- [`get_speeds()`](https://opatp.github.io/GTFSwizard/reference/get_speeds.md)
  : Calculate Scheduled Speeds
- [`get_fleet()`](https://opatp.github.io/GTFSwizard/reference/get_fleet.md)
  : Estimate Simultaneous Vehicles
- [`get_1stdeparture()`](https://opatp.github.io/GTFSwizard/reference/get_1stdeparture.md)
  : Get the First Departure of Each Trip

## Analyze geography and networks

Work with stops, shapes, corridors, hubs, and metric coordinate systems.

- [`get_stops_sf()`](https://opatp.github.io/GTFSwizard/reference/get_stops_sf.md)
  : Convert GTFS Stops to Simple Features
- [`get_shapes()`](https://opatp.github.io/GTFSwizard/reference/get_shapes.md)
  : Infer Straight-Line Shapes from Stop Sequences
- [`get_shapes_sf()`](https://opatp.github.io/GTFSwizard/reference/get_shapes_sf.md)
  : Convert GTFS Shapes to Simple Features
- [`get_shapes_df()`](https://opatp.github.io/GTFSwizard/reference/get_shapes_df.md)
  : Convert Shape Geometries to a GTFS Shapes Table
- [`latlon2epsg()`](https://opatp.github.io/GTFSwizard/reference/latlon2epsg.md)
  : Transform Spatial Data to a Local Metric CRS
- [`get_corridor()`](https://opatp.github.io/GTFSwizard/reference/get_corridor.md)
  : Identify High-Frequency Transit Corridors
- [`get_hubs()`](https://opatp.github.io/GTFSwizard/reference/get_hubs.md)
  : Identify Stops Serving Multiple Routes

## Edit scheduled service

Apply timetable and trip-structure scenarios to a copy of a feed.

- [`delay_trip()`](https://opatp.github.io/GTFSwizard/reference/delay_trip.md)
  : Shift Trips in Time
- [`split_trip()`](https://opatp.github.io/GTFSwizard/reference/split_trip.md)
  : Split Trips into Consecutive Parts
- [`edit_speed()`](https://opatp.github.io/GTFSwizard/reference/edit_speed.md)
  : Scale In-Vehicle Travel Speed
- [`set_dwelltime()`](https://opatp.github.io/GTFSwizard/reference/set_dwelltime.md)
  : Set Dwell Times
- [`edit_dwelltime()`](https://opatp.github.io/GTFSwizard/reference/edit_dwelltime.md)
  : Scale Dwell Times

## Visualize feeds and service

Produce the package’s consistent static network and service plots.

- [`print(`*`<wizardgtfs>`*`)`](https://opatp.github.io/GTFSwizard/reference/wizardgtfs-methods.md)
  [`summary(`*`<wizardgtfs>`*`)`](https://opatp.github.io/GTFSwizard/reference/wizardgtfs-methods.md)
  [`print(`*`<summary.wizardgtfs>`*`)`](https://opatp.github.io/GTFSwizard/reference/wizardgtfs-methods.md)
  [`plot(`*`<wizardgtfs>`*`)`](https://opatp.github.io/GTFSwizard/reference/wizardgtfs-methods.md)
  : Print, Summarize, and Plot \`wizardgtfs\` Objects
- [`plot_calendar()`](https://opatp.github.io/GTFSwizard/reference/plot_calendar.md)
  : Plot the GTFS Service Calendar
- [`plot_frequency()`](https://opatp.github.io/GTFSwizard/reference/plot_frequency.md)
  : Plot System Frequency by Hour
- [`plot_routefrequency()`](https://opatp.github.io/GTFSwizard/reference/plot_routefrequency.md)
  : Plot Route Frequency by Hour
- [`plot_headways()`](https://opatp.github.io/GTFSwizard/reference/plot_headways.md)
  : Plot System Headway by Hour
- [`plot_servicespan()`](https://opatp.github.io/GTFSwizard/reference/plot_servicespan.md)
  : Plot Scheduled Service Span by Route
- [`plot_serviceheatmap()`](https://opatp.github.io/GTFSwizard/reference/plot_serviceheatmap.md)
  : Plot Average Scheduled Departures by Weekday and Hour
- [`plot_routeduration()`](https://opatp.github.io/GTFSwizard/reference/plot_routeduration.md)
  : Plot Scheduled Trip Duration by Route
- [`plot_servicesupply()`](https://opatp.github.io/GTFSwizard/reference/plot_servicesupply.md)
  : Plot Scheduled Vehicle-Hours by Route
- [`plot_corridor()`](https://opatp.github.io/GTFSwizard/reference/plot_corridor.md)
  : Plot High-Frequency Corridors
- [`plot_hubs()`](https://opatp.github.io/GTFSwizard/reference/plot_hubs.md)
  : Plot Transit Hubs

## Explore and route interactively

- [`explore_gtfs()`](https://opatp.github.io/GTFSwizard/reference/explore_gtfs.md)
  : Explore GTFS Data in an Interactive Shiny Dashboard
- [`tidy_raptor()`](https://opatp.github.io/GTFSwizard/reference/tidy_raptor.md)
  : Calculate Travel Times with RAPTOR Algorithm

## Example feeds

- [`for_bus_gtfs`](https://opatp.github.io/GTFSwizard/reference/for_bus_gtfs.md)
  : GTFS Data for Fortaleza (Bus System), Brazil.
- [`for_rail_gtfs`](https://opatp.github.io/GTFSwizard/reference/for_rail_gtfs.md)
  : GTFS Data for Fortaleza (Rail System), Brazil
