# Get the First Departure of Each Trip

Returns the departure at the lowest \`stop_sequence\` for every trip.

## Usage

``` r
get_1stdeparture(gtfs)
```

## Arguments

- gtfs:

  A GTFS object.

## Value

A tibble with \`route_id\`, \`trip_id\`, \`departure_time\`, and
\`stop_id\`.

## References

\[GTFS Schedule
Reference\](https://gtfs.org/documentation/schedule/reference/#stop_timestxt)

## Examples

``` r
head(get_1stdeparture(for_rail_gtfs))
#> # A tibble: 6 × 4
#>   route_id trip_id departure_time stop_id
#>   <chr>    <chr>   <chr>          <chr>  
#> 1 7        4       05:30:00       29     
#> 2 7        5       06:15:00       29     
#> 3 7        6       07:00:00       29     
#> 4 7        7       07:45:00       29     
#> 5 7        8       08:30:00       29     
#> 6 7        9       10:15:00       29     
```
