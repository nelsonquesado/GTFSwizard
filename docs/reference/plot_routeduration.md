# Plot Scheduled Trip Duration by Route

Shows the distribution of scheduled end-to-end trip duration for the
busiest routes.

## Usage

``` r
plot_routeduration(gtfs, top_n = 20L)
```

## Arguments

- gtfs:

  A GTFS object.

- top_n:

  Maximum number of routes to display.

## Value

A \`ggplot\` object. Each observation is one scheduled trip; boxes
summarize duration in minutes by route.

## References

\[FTA Evaluation
Introduction\](https://www.transit.dot.gov/research-innovation/evaluation-introduction)

## Examples

``` r
plot_routeduration(for_rail_gtfs)

```
