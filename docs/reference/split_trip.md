# Split Trips into Consecutive Parts

Splits each selected trip into consecutive parts. Trips can be split
into an approximately even number of parts with \`split\`, or at
specific internal stop IDs with \`stops\`. The boundary stop is included
at the end of one part and the start of the next, producing valid
complete stop sequences.

## Usage

``` r
split_trip(gtfs, trip, split = 1L, stops = NULL)
```

## Arguments

- gtfs:

  A GTFS object.

- trip:

  Character vector of \`trip_id\` values.

- split:

  Positive integer number of split points. \`split = 1\` creates two
  parts. For each trip, the maximum is the number of stop-time records
  minus two, so every resulting part contains at least two stops. Used
  when \`stops = NULL\`.

- stops:

  Optional character vector of stop IDs where the trip should be split.
  When supplied, \`split\` must be omitted. Each internal occurrence of
  these stops becomes a split boundary. First and last stops cannot be
  used as split boundaries because they would create one-stop parts.

## Value

A modified \`wizardgtfs\` object.

## Details

New IDs use \`.part1\`, \`.part2\`, and so on. New straight-line shapes
are inferred from stop coordinates for the split parts. Frequency
periods are shifted by each part's offset from the original first
departure. Trip-level transfers are reassigned to the part containing
their transfer stop. Trips with fewer than three retained stop-time
records cannot be split. When using \`stops\`, each selected trip must
contain at least one matching internal stop.

## See also

\[GTFSwizard::get_shapes()\], \[GTFSwizard::merge_gtfs()\]

## Examples

``` r
gtfs_split <- split_trip(
  for_rail_gtfs,
  trip = for_rail_gtfs$trips$trip_id[1],
  split = 2
)
#> GTFSwizard: building straight-line shapes from ordered stop coordinates.

first_trip_stops <- for_rail_gtfs$stop_times[
  for_rail_gtfs$stop_times$trip_id == for_rail_gtfs$trips$trip_id[1],
]
gtfs_split_at_stop <- split_trip(
  for_rail_gtfs,
  trip = for_rail_gtfs$trips$trip_id[1],
  stops = first_trip_stops$stop_id[2]
)
#> GTFSwizard: building straight-line shapes from ordered stop coordinates.
```
