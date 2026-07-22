# Identify Recurring Daily Service Patterns

Identifies each distinct set of \`service_id\` values active on a
calendar date. The same service may belong to more than one pattern when
it operates alongside different services on different dates.

## Usage

``` r
get_servicepattern(gtfs)
```

## Arguments

- gtfs:

  A GTFS object.

## Value

A tibble with one row per service-to-pattern association and columns
\`service_id\`, \`service_pattern\`, and \`pattern_frequency\`. The
frequency is the number of dates having that exact set of active
services. Pattern numbers are ordered from most to least frequent.
Calendar dates without active services are represented by
\`service_pattern = "No service"\` and \`service_id = NA\`.

## See also

\[GTFSwizard::filter_servicepattern()\]

## Examples

``` r
get_servicepattern(for_rail_gtfs)
#> # A tibble: 2 × 3
#>   service_id service_pattern  pattern_frequency
#>   <chr>      <chr>                        <int>
#> 1 4          servicepattern-1               614
#> 2 NA         No service                     116
```
