# Calculate Travel Times with RAPTOR Algorithm

The \`tidy_raptor\` function calculates travel times from a set of
origin stops to all reachable stops within a GTFS dataset. It uses the
RAPTOR (Round-Based Public Transit Routing) algorithm from the
\`tidytransit\` package and integrates it with the GTFSwizard framework.

## Usage

``` r
tidy_raptor(
  gtfs,
  min_departure = "0:0:0",
  max_arrival = "23:59:59",
  dates = NULL,
  stop_ids,
  arrival = FALSE,
  time_range = 3600,
  max_transfers = NULL,
  keep = "all",
  filter = TRUE,
  separate_starts = FALSE
)
```

## Arguments

- gtfs:

  A GTFS object, preferably of class \`wizardgtfs\`. If not, the
  function will attempt to convert it using
  \`GTFSwizard::as_wizardgtfs()\`.

- min_departure:

  A string representing the earliest departure time, in "HH:MM:SS"
  format. Defaults to \`"0:0:0"\`.

- max_arrival:

  A string representing the latest arrival time, in "HH:MM:SS" format.
  Defaults to \`"23:59:59"\`.

- dates:

  One service date. When \`NULL\`, the latest active service date is
  used.

- stop_ids:

  A character vector of stop IDs from where journeys should start (or
  end, if \`arrival = TRUE\`).

- arrival:

  Logical. If \`FALSE\` (default), journeys start from \`stop_ids\`. If
  \`TRUE\`, journeys end at \`stop_ids\`.

- time_range:

  Either a range in seconds (numeric) or a vector with the minimal and
  maximal departure time (e.g., \`c(0, 3600)\` or \`"HH:MM:SS"\`)
  describing the journey window.

- max_transfers:

  Maximum number of transfers allowed. Defaults to \`NULL\` (no limit).

- keep:

  One of \`"all"\`, \`"shortest"\`, \`"earliest"\`, or \`"latest"\`.
  Determines which journeys to retain: - \`"all"\`: All journeys are
  returned (default). - \`"shortest"\`: Only journeys with the shortest
  travel time. - \`"earliest"\`: Journeys arriving at stops the
  earliest. - \`"latest"\`: Journeys arriving at stops the latest.

- filter:

  Logical. Apply \`min_departure\`, \`max_arrival\`, and \`dates\`
  before routing.

- separate_starts:

  Logical. Keep results for each origin start time separately; passed to
  \[tidytransit::raptor()\].

## Value

A tibble containing the RAPTOR algorithm results, including:

- from_stop_id:

  The ID of the stop where the journey starts.

- to_stop_id:

  The ID of the stop where the journey ends.

- departure_time:

  Departure time from the origin stop.

- arrival_time:

  Arrival time at the destination stop.

- travel_time:

  Total travel time in seconds.

- transfers:

  Number of transfers in the journey.

## Note

Ensure that the \`stop_times\` is present and correctly structured in
the GTFS dataset. Time values in \`min_departure\`, \`max_arrival\`, and
\`time_range\` should be correctly formatted to avoid errors. GTFS times
beyond 24:00:00 are supported.

## See also

\[tidytransit::raptor()\], \[GTFSwizard::as_wizardgtfs()\],
\[GTFSwizard::filter_time()\]

## Examples

``` r
tidy_raptor(for_rail_gtfs,
   min_departure = '06:20:00',
   max_arrival = '09:40:00',
   dates = "2021-12-13",
   max_transfers = 2,
   keep = "all",
   stop_ids = '66')
#> # A tibble: 85 × 6
#>    from_stop_id to_stop_id travel_time departure_time arrival_time transfers
#>    <chr>        <chr>            <dbl> <chr>          <chr>            <int>
#>  1 66           66                   0 06:20:00       06:20:00             0
#>  2 66           67                 240 06:34:00       06:38:00             0
#>  3 66           65                 240 06:51:00       06:55:00             0
#>  4 66           67                 240 07:11:00       07:15:00             0
#>  5 66           68                 420 06:34:00       06:41:00             0
#>  6 66           64                 420 06:51:00       06:58:00             0
#>  7 66           68                 420 07:11:00       07:18:00             0
#>  8 66           63                 660 06:51:00       07:02:00             0
#>  9 66           41                 960 06:51:00       07:07:00             0
#> 10 66           40                1200 06:51:00       07:11:00             0
#> # ℹ 75 more rows
```
