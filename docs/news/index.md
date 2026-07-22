# Changelog

## GTFSwizard 1.2.1

- Organized the package website by workflow, added task-based function
  reference sections, and added vignettes for getting started, service
  analysis, filtering and editing, and the interactive dashboard.

- Standardized public method values with underscores: `by_trip`,
  `by_route`, `by_hour`, `by_stop`, and `by_shape`. Legacy dotted values
  remain accepted.

- Standardized public argument names including `file_path`, `gtfs_x`,
  `gtfs_y`, `max_dwelltime`, and `min_length`, with compatibility
  aliases for previous names.

- Improved
  [`tidy_raptor()`](https://opatp.github.io/GTFSwizard/reference/tidy_raptor.md)
  validation, transfer handling, date selection, and output column
  names.

- Added planning indicators to
  [`explore_gtfs()`](https://opatp.github.io/GTFSwizard/reference/explore_gtfs.md),
  including scheduled headway, commercial speed, service span,
  vehicle-hours, peak fleet, stop spacing, and route connectivity.

- Calendar and service-pattern summaries now represent dates with no
  active service explicitly: trip counts are `0`, and the service
  pattern is labeled `"No service"`.

## GTFSwizard 1.2.0

CRAN release: 2026-06-22

- [`plot_routefrequency()`](https://opatp.github.io/GTFSwizard/reference/plot_routefrequency.md)
  now uses a route-hour-service pattern tile plot and limits the default
  display to the busiest routes through `top_n`, improving readability
  for large feeds.

- [`explore_gtfs()`](https://opatp.github.io/GTFSwizard/reference/explore_gtfs.md)
  adds a route-count control for route-frequency plots and keeps
  filtered service and service-pattern selections aligned.

## GTFSwizard 1.1.1

- [`create_gtfs()`](https://opatp.github.io/GTFSwizard/reference/create_gtfs.md)
  introduced.

- CRAN resubmission release following archival caused by the archived
  `hrbrthemes` dependency. The dependency has been removed, and the
  package dependency set has been substantially reduced.

- GTFS editing and selection

  - [`filter_stop()`](https://opatp.github.io/GTFSwizard/reference/filter_functions.md)
    and
    [`filter_time()`](https://opatp.github.io/GTFSwizard/reference/filter_functions.md)
    now preserve partial trips for experimentation instead of restoring
    complete stop sequences.
  - [`selection()`](https://opatp.github.io/GTFSwizard/reference/selection.md)
    now supports grouping columns and computed groups in a style similar
    to
    [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
    while continuing to support logical and spatial selections without
    altering GTFS tables.

- Plotting and dashboard

  - Fixed repeated warnings and `-Inf` values when `shape_dist_traveled`
    contains only missing values.
  - [`explore_gtfs()`](https://opatp.github.io/GTFSwizard/reference/explore_gtfs.md)
    removes duplicated system information, assigns distinct route
    colors, and filters non-finite plotting values.
  - All static plots use a shared visual theme and palette.

- Package quality

  - Added Francisco Moraes de Oliveira Neto as an author.
  - Expanded integrity, round-trip, selection, spatial, plotting, and
    regression tests.
  - Reviewed exported function documentation and GTFS logic against the
    current GTFS Schedule reference.

## GTFSwizard 1.1.0

CRAN release: 2025-03-15

- Now featuring travel time matrices, trip speed and dwell-time editing,
  corridors, hubs, and more!

- New functions

  - tidy_raptor() estimates travel time matrices from gtfs feeds;
  - edit_speed() adjusts travel speed in a GTFS dataset;
  - edit_dwelltime() modifies the dwell time of trips;
  - set_dwelltime() overwrites the dwell time of trips;
  - get_hubs() estimates high integration potential stops;
  - get_corridor() suggests “right-of-way” transit corridors;
  - plot_corridor() plots the suggested corridors from get_corridor()
    function;
  - get_1stdeparture() retrieves the start timepoint of each trip;
  - latlon2epsg() determines the appropriate EPSG code for a given sf
    object.

- New methods

  - get_headways() now accepts ‘method = “by_stop”’ and ‘method =
    “by_shape”’.
  - get_frequency() now accepts ‘method = “by_shape”’ and ‘method =
    “by_stop”’.

- Bug fix & improvements

  - Better column naming;
  - Minor bug fixed on filter_date() and get_shapes_sf();
  - get_headways() output fixed;
  - split_trip() can now be used after filter_time();
  - get_speeds(), get_durations() and get_distances() now accept the
    `trips` argument.

## GTFSwizard 1.0.0

CRAN release: 2024-11-11

- Initial CRAN submission.
