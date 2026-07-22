# Plot Transit Hubs

Plot Transit Hubs

## Usage

``` r
plot_hubs(gtfs, i = 0.05)
```

## Arguments

- gtfs:

  A GTFS object.

- i:

  Proportion of stops with the most routes to retain.

## Value

A \`ggplot\` map.

## Details

To keep dense networks readable, at most 40 of the highest-ranked stops
are drawn. Ranking uses distinct route count and then trip count.

## See also

\[GTFSwizard::get_hubs()\]

## Examples

``` r
plot_hubs(for_rail_gtfs, i = 0.1)
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

```
