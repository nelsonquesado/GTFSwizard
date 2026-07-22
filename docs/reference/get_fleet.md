# Estimate Simultaneous Vehicles

Estimates the number of active trip instances from their first departure
to final arrival. Frequency-based services are expanded from
\`frequencies.txt\`.

## Usage

``` r
get_fleet(gtfs, method = "by_route")
```

## Arguments

- gtfs:

  A GTFS object.

- method:

  One of \`"by_route"\`, \`"by_hour"\`, \`"peak"\`, or \`"detailed"\`.
  Legacy dotted values remain accepted.

## Value

A tibble whose observational unit depends on \`method\`:

- \`"by_route"\`:

  \`route_id\`, optional \`direction_id\`, \`service_pattern\`,
  \`pattern_frequency\`, and maximum simultaneous \`fleet\`.

- \`"by_hour"\`:

  \`service_pattern\`, \`pattern_frequency\`, \`hour\`, and maximum
  simultaneous \`fleet\`.

- \`"peak"\`:

  The three highest hourly fleet estimates per service pattern, with the
  same columns as \`"by_hour"\`.

- \`"detailed"\`:

  \`route_id\`, optional \`direction_id\`, vehicle-change event
  \`net.fleet\`, cumulative \`fleet\`, event \`time\` in seconds,
  \`service_pattern\`, and \`pattern_frequency\`.

GTFS times beyond 24 hours remain in the following service day rather
than being wrapped.

## Examples

``` r
get_fleet(for_rail_gtfs, "by_route")
#> # A tibble: 6 × 5
#>   route_id direction_id service_pattern  pattern_frequency fleet
#>   <chr>           <int> <chr>                        <int> <int>
#> 1 6                   0 servicepattern-1               614     3
#> 2 6                   1 servicepattern-1               614     4
#> 3 7                   0 servicepattern-1               614     1
#> 4 7                   1 servicepattern-1               614     1
#> 5 8                   0 servicepattern-1               614     1
#> 6 8                   1 servicepattern-1               614     1
get_fleet(for_rail_gtfs, "by_hour")
#> # A tibble: 19 × 4
#>    service_pattern  pattern_frequency  hour fleet
#>    <chr>                        <int> <dbl> <int>
#>  1 servicepattern-1               614     5     8
#>  2 servicepattern-1               614     6     9
#>  3 servicepattern-1               614     7     9
#>  4 servicepattern-1               614     8     9
#>  5 servicepattern-1               614     9     9
#>  6 servicepattern-1               614    10     8
#>  7 servicepattern-1               614    11     8
#>  8 servicepattern-1               614    12     8
#>  9 servicepattern-1               614    13     8
#> 10 servicepattern-1               614    14     8
#> 11 servicepattern-1               614    15     8
#> 12 servicepattern-1               614    16     9
#> 13 servicepattern-1               614    17     9
#> 14 servicepattern-1               614    18    10
#> 15 servicepattern-1               614    19     9
#> 16 servicepattern-1               614    20     9
#> 17 servicepattern-1               614    21     7
#> 18 servicepattern-1               614    22     6
#> 19 servicepattern-1               614    23     4
```
