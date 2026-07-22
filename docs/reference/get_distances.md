# Calculate Distances in GTFS Data

The \`get_distances\` function calculates distances within a
\`wizardgtfs\` object based on various methods. Depending on the
\`method\` chosen, it can calculate average route distances,
trip-specific distances, or detailed distances between stops.

## Usage

``` r
get_distances(gtfs, method = "by_trip", trips = "all")
```

## Arguments

- gtfs:

  A GTFS object, ideally of class \`wizardgtfs\`. If it is not of this
  class, it will be converted.

- method:

  A character string indicating the calculation method. Choices are:

  "by_route"

  : Calculates average distances for each route.

  "by_trip"

  : Calculates distances for each trip, associating each trip ID with
    its total distance.

  "detailed"

  : Calculates detailed distances between each consecutive stop for all
    trips. This is the most computationally intensive option and may
    take several minutes to complete.

- trips:

  A character vector of trip IDs to consider. When set to \`all\`,
  includes all trips.

## Value

A data frame with calculated distances based on the specified method:

- If \`method = "by_route"\`:

  Returns a summary by route and direction with trip counts, average
  distance, service pattern, and pattern frequency.

- If \`method = "by_trip"\`:

  Returns trip distance with route, direction, service pattern, and
  pattern frequency.

- If \`method = "detailed"\`:

  Returns a data frame with columns: \`shape_id\`, \`from_stop_id\`,
  \`to_stop_id\`, and \`distance\`.

## Details

The function calls specific sub-functions based on the selected method:

\- "by_route": Calculates average distances per route.

\- "by_trip": Calculate distances per trip.

\- "detailed": Calculates detailed stop-to-stop distances within each
route. Note that this method may be slow for large datasets.

Legacy dotted values remain accepted. If an invalid \`method\` is
provided, the function defaults to \`"by_trip"\` and issues a warning.

## See also

\[GTFSwizard::as_wizardgtfs()\], \[GTFSwizard::get_servicepattern()\]

## Examples

``` r
# Calculate average route distances
distances_by_route <- get_distances(gtfs = for_rail_gtfs, method = "by_route", trips = 'all')

# Calculate distances by trip
distances_by_trip <- get_distances(gtfs = for_rail_gtfs, method = "by_trip", trips = 'all')

# \donttest{
# Calculate detailed distances between stops
detailed_distances <- get_distances(gtfs = for_rail_gtfs, method = "detailed", trips = 'all')
#> GTFSwizard: calculating detailed stop-to-stop distances.
# }
```
