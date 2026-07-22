# Plot System Frequency by Hour

Shows the distribution and weighted mean of scheduled trip departures by
hour. Service-pattern date counts provide the weights.

## Usage

``` r
plot_frequency(gtfs)
```

## Arguments

- gtfs:

  A GTFS object.

## Value

A \`ggplot\` object.

## See also

\[GTFSwizard::get_frequency()\]

## Examples

``` r
plot_frequency(for_rail_gtfs)

```
