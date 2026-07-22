# Set Dwell Times

Sets dwell time at selected trip-stop calls and propagates each change
to all later times in the same trip. Arrival at the edited stop is
retained.

## Usage

``` r
set_dwelltime(gtfs, duration = 30, trips = "all", stops = "all")
```

## Arguments

- gtfs:

  A GTFS object.

- duration:

  One non-negative dwell time in seconds.

- trips, stops:

  Character ID vectors or \`"all"\`.

## Value

A modified \`wizardgtfs\` object.

## See also

\[GTFSwizard::edit_dwelltime()\], \[GTFSwizard::get_dwelltimes()\]

## Examples

``` r
edited <- set_dwelltime(
  for_rail_gtfs,
  duration = 30,
  trips = for_rail_gtfs$trips$trip_id[1:2],
  stops = for_rail_gtfs$stops$stop_id[1:2]
)
```
