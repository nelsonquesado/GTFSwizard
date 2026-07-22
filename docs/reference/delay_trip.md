# Shift Trips in Time

Adds a number of seconds to every non-empty arrival and departure time
for selected trips. GTFS times above 24 hours are preserved.

## Usage

``` r
delay_trip(gtfs, trip, duration)
```

## Arguments

- gtfs:

  A GTFS object.

- trip:

  Character vector of \`trip_id\` values.

- duration:

  Numeric seconds or an object coercible to seconds, such as a
  \`difftime\`.

## Value

A modified \`wizardgtfs\` object.

## See also

\[GTFSwizard::edit_speed()\], \[GTFSwizard::set_dwelltime()\]

## Examples

``` r
delayed <- delay_trip(
  for_rail_gtfs,
  trip = for_rail_gtfs$trips$trip_id[1:2],
  duration = 300
)
```
