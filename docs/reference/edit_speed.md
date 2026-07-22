# Scale In-Vehicle Travel Speed

Changes travel time between consecutive stops by dividing it by a speed
multiplier. Dwell times are preserved and all downstream times are
shifted.

## Usage

``` r
edit_speed(gtfs, trips = "all", stops = "all", factor)
```

## Arguments

- gtfs:

  A GTFS object.

- trips:

  Character trip IDs or \`"all"\`.

- stops:

  Character stop IDs or \`"all"\`. A segment is edited when either
  endpoint is selected.

- factor:

  One positive speed multiplier. For example, \`2\` halves segment
  travel times.

## Value

A modified \`wizardgtfs\` object.

## See also

\[GTFSwizard::get_speeds()\], \[GTFSwizard::get_durations()\]

## Examples

``` r
edited <- edit_speed(
  for_rail_gtfs,
  trips = for_rail_gtfs$trips$trip_id[1:2],
  stops = "all",
  factor = 1.25
)
```
