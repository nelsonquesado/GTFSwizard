# Calculate Scheduled Service Frequency

Counts trip departures by route, shape, stop, or hour. Trips referenced
by \`frequencies.txt\` are expanded using the period's inclusive
\`start_time\`, exclusive \`end_time\`, and \`headway_secs\`, as
required by GTFS.

## Usage

``` r
get_frequency(gtfs, method = "by_trip")
```

## Arguments

- gtfs:

  A GTFS object.

- method:

  One of \`"by_trip"\`, \`"by_route"\`, \`"by_shape"\`, \`"by_stop"\`,
  or \`"detailed"\`. Legacy dotted values remain accepted.

## Value

A tibble whose observational unit depends on \`method\`:

- \`"by_trip"\`:

  \`route_id\`, \`trip_id\`, optional \`direction_id\`,
  \`service_pattern\`, \`pattern_frequency\`, and \`daily.frequency\`.

- \`"by_route"\`:

  \`route_id\`, optional \`direction_id\`, \`service_pattern\`,
  \`pattern_frequency\`, and \`daily.frequency\`.

- \`"by_shape"\`:

  \`shape_id\`, optional \`direction_id\`, \`service_pattern\`,
  \`pattern_frequency\`, and \`daily.frequency\`.

- \`"by_stop"\`:

  \`stop_id\`, optional \`direction_id\`, \`service_pattern\`,
  \`pattern_frequency\`, and \`daily.frequency\`.

- \`"detailed"\`:

  \`route_id\`, optional \`direction_id\`, departure \`hour\`,
  \`service_pattern\`, \`pattern_frequency\`, and \`frequency\`.

## References

\[GTFS Schedule
Reference\](https://gtfs.org/documentation/schedule/reference/#frequenciestxt)

## See also

\[GTFSwizard::get_headways()\]

## Examples

``` r
get_frequency(for_rail_gtfs, "by_route")
#> # A tibble: 6 × 5
#>   route_id direction_id service_pattern  pattern_frequency daily.frequency
#>   <chr>           <int> <chr>                        <int>           <int>
#> 1 6                   0 servicepattern-1               614              63
#> 2 6                   1 servicepattern-1               614              64
#> 3 7                   0 servicepattern-1               614              15
#> 4 7                   1 servicepattern-1               614              15
#> 5 8                   0 servicepattern-1               614              29
#> 6 8                   1 servicepattern-1               614              29
get_frequency(for_rail_gtfs, "detailed")
#> # A tibble: 100 × 6
#>    route_id direction_id  hour service_pattern  pattern_frequency frequency
#>    <chr>           <int> <dbl> <chr>                        <int>     <int>
#>  1 6                   0     5 servicepattern-1               614         2
#>  2 6                   0     6 servicepattern-1               614         4
#>  3 6                   0     7 servicepattern-1               614         3
#>  4 6                   0     8 servicepattern-1               614         4
#>  5 6                   0     9 servicepattern-1               614         4
#>  6 6                   0    10 servicepattern-1               614         4
#>  7 6                   0    11 servicepattern-1               614         3
#>  8 6                   0    12 servicepattern-1               614         4
#>  9 6                   0    13 servicepattern-1               614         4
#> 10 6                   0    14 servicepattern-1               614         4
#> # ℹ 90 more rows
```
