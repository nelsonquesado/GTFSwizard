# Convert GTFS Stops to Simple Features

Convert GTFS Stops to Simple Features

## Usage

``` r
get_stops_sf(gtfs)
```

## Arguments

- gtfs:

  A GTFS object or a \`stops\` data frame.

## Value

For a data frame, a WGS84 point \`sf\` object. For a GTFS object, the
same object with its \`stops\` table converted to \`sf\`.

## Examples

``` r
gtfs_sf <- get_stops_sf(for_rail_gtfs)
stops_sf <- get_stops_sf(for_rail_gtfs$stops)
```
