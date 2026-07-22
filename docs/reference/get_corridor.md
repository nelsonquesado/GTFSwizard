# Identify High-Frequency Transit Corridors

Finds frequently served adjacent stop pairs, joins connected pairs into
corridors, and measures them in a local metric CRS.

## Usage

``` r
get_corridor(gtfs, i = 0.01, min_length = 1500, ...)
```

## Arguments

- gtfs:

  A GTFS object.

- i:

  Proportion of the highest-frequency stop pairs to retain. Must be
  greater than 0 and no greater than 1.

- min_length:

  Minimum corridor length in meters.

- ...:

  Supports the legacy argument \`min.length\`.

## Value

An \`sf\` object with \`corridor\`, list-columns \`stop_id\` and
\`trip_id\`, numeric \`length\` in meters, and WGS84 geometry. Segments
are straight lines between stop coordinates, not shape geometry.

## See also

\[GTFSwizard::plot_corridor()\]

## Examples

``` r
corridors <- get_corridor(for_rail_gtfs, i = 0.2, min_length = 100)
```
