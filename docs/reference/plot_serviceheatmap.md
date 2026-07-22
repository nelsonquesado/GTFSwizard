# Plot Average Scheduled Departures by Weekday and Hour

Creates a heatmap of average scheduled departures on active service
dates.

## Usage

``` r
plot_serviceheatmap(gtfs)
```

## Arguments

- gtfs:

  A GTFS object.

## Value

A \`ggplot\` object. Each tile is a weekday-hour combination and its
fill is the mean number of scheduled trip departures per active date.

## References

\[GTFS Schedule
Reference\](https://gtfs.org/documentation/schedule/reference/)

## Examples

``` r
plot_serviceheatmap(for_rail_gtfs)

```
