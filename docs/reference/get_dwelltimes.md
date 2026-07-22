# Calculate Dwell Times in GTFS Data

The \`get_dwelltimes\` function calculates dwell times within a
\`wizardgtfs\` object using different methods. Depending on the selected
\`method\`, it can provide average dwell times per route, per trip, by
hour, or detailed dwell times at each stop.

## Usage

``` r
get_dwelltimes(gtfs, max_dwelltime = 90, method = "by_trip", ...)
```

## Arguments

- gtfs:

  A GTFS object, ideally of class \`wizardgtfs\`. If not, it will be
  converted.

- max_dwelltime:

  Numeric. Maximum allowable dwell time in seconds.

- method:

  A character string specifying the calculation method. Options include:

  "by_hour"

  : Calculates the average dwell time per hour of the day across all
    trips.

  "by_route"

  : Calculates the average dwell time for each route.

  "by_trip"

  : Calculates the average dwell time for each trip.

  "detailed"

  : Calculates detailed dwell times at each stop within every trip.

- ...:

  Supports the legacy argument \`max.dwelltime\`.

## Value

A data frame containing dwell times based on the specified method:

- If \`method = "by_hour"\`:

  Returns a data frame with columns: \`hour\`, \`trips\`,
  \`average.dwelltime\`, \`service_pattern\`, and \`pattern_frequency\`.

- If \`method = "by_route"\`:

  Returns \`route_id\`, optional \`direction_id\`, \`trips\`,
  \`average.dwelltime\`, \`service_pattern\`, and \`pattern_frequency\`.

- If \`method = "by_trip"\`:

  Returns \`route_id\`, \`trip_id\`, optional \`direction_id\`,
  \`average.dwelltime\`, \`service_pattern\`, and \`pattern_frequency\`.

- If \`method = "detailed"\`:

  Returns \`route_id\`, \`trip_id\`, optional \`direction_id\`,
  \`stop_id\`, \`hour\`, \`dwell_time\`, \`service_pattern\`, and
  \`pattern_frequency\`.

## Details

This function calls specific sub-functions based on the selected method:

\- "by_hour": Calculates the average dwell time for each hour of the
day.

\- "by_route": Calculates average dwell times across each route.

\- "by_trip": Calculates the mean dwell time for each trip.

\- "detailed": Calculates departure minus arrival at each stop call.

Legacy dotted values remain accepted. If an invalid \`method\` is
specified, the function defaults to \`"by_trip"\` and provides a
warning.

## See also

\[GTFSwizard::as_wizardgtfs()\], \[GTFSwizard::get_servicepattern()\]

## Examples

``` r
# Calculate dwell times by hour
dwelltimes_by_hour <- get_dwelltimes(gtfs = for_rail_gtfs, max_dwelltime = 120, method = "by_hour")

# Calculate dwell times by route
dwelltimes_by_route <- get_dwelltimes(gtfs = for_rail_gtfs, max_dwelltime = 90, method = "by_route")

# Calculate dwell times by trip
dwelltimes_by_trip <- get_dwelltimes(gtfs = for_rail_gtfs, max_dwelltime = 45, method = "by_trip")

# Calculate detailed dwell times between stops
detailed_dwelltimes <- get_dwelltimes(gtfs = for_rail_gtfs, max_dwelltime = 60, method = "detailed")
```
