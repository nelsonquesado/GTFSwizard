# Scale Dwell Times

Multiplies dwell time at selected trip-stop calls and propagates each
change to all later times in the same trip. Arrival at the edited stop
is retained; departure and subsequent calls move by the dwell-time
difference.

## Usage

``` r
edit_dwelltime(gtfs, trips = "all", stops = "all", factor)
```

## Arguments

- gtfs:

  A GTFS object.

- trips, stops:

  Character ID vectors or \`"all"\`.

- factor:

  One non-negative numeric multiplier.

## Value

A modified \`wizardgtfs\` object.

## See also

\[GTFSwizard::set_dwelltime()\], \[GTFSwizard::get_dwelltimes()\]

## Examples

``` r
edited <- edit_dwelltime(
  for_rail_gtfs,
  trips = for_rail_gtfs$trips$trip_id[1:2],
  stops = for_rail_gtfs$stops$stop_id[1:2],
  factor = 1.5
)
```
