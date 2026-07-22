# Plot Scheduled Service Span by Route

Shows the first departure and final arrival for route and
service-pattern combinations. The longest and most frequently operated
route-pattern combinations are retained when the feed is large.

## Usage

``` r
plot_servicespan(gtfs, top_n = 30L)
```

## Arguments

- gtfs:

  A GTFS object.

- top_n:

  Maximum number of route-pattern combinations to display.

## Value

A \`ggplot\` object. Each horizontal line is one route and service
pattern; its endpoints are scheduled clock hours.

## References

\[GTFS Routes, Stops, and
Trips\](https://gtfs.org/documentation/schedule/examples/routes-stops-trips/)

## Examples

``` r
plot_servicespan(for_rail_gtfs)

```
