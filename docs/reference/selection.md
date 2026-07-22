# Group or Select GTFS Records Without Filtering the Feed

Creates persistent selection metadata while leaving every GTFS table
unchanged. Bare columns and computed values define groups, similarly to
\[dplyr::group_by()\]. Logical expressions restrict the records
represented by those groups.

## Usage

``` r
selection(gtfs, ..., add = FALSE)

unselection(gtfs)
```

## Arguments

- gtfs:

  A GTFS object.

- ...:

  Unquoted grouping columns, computed grouping expressions, or logical
  selection expressions. Columns from \`stop_times\`, \`trips\`,
  \`routes\`, and \`stops\` are available. A \`geometry\` column is
  available for stop-based spatial predicates.

- add:

  Logical. When \`TRUE\`, add the new grouping or selection expressions
  to an existing selection. When \`FALSE\`, replace it.

## Value

The unchanged feed with class \`wizardgtfs_selected\` and a
\`selection\` attribute. The attribute contains \`groups\`,
\`group_vars\`, selected route, trip, and stop IDs, and the original
expressions.

## Details

A bare expression such as \`route_id\` creates one group per route.
Multiple grouping expressions create combinations, for example
\`selection(gtfs, route_id, direction_id)\`. Logical expressions such as
\`route_id %in% c("1", "2")\` restrict the records represented in the
selection but do not remove rows from the GTFS feed.

Spatial expressions may use \`geometry\` with \`%intersects%\`,
\`%touches%\`, \`%crosses%\`, \`%within%\`, \`%contains%\`,
\`%overlaps%\`, or \`%equals%\`.

## See also

\[GTFSwizard::unselection()\], \[dplyr::group_by()\]

## Examples

``` r
grouped <- selection(for_rail_gtfs, route_id, direction_id)
attr(grouped, "selection")$groups
#> # A tibble: 6 × 6
#>   route_id direction_id n_stop_calls routes    trips      stops     
#>   <chr>           <int>        <int> <list>    <list>     <list>    
#> 1 6                   0         1260 <chr [1]> <chr [63]> <chr [20]>
#> 2 6                   1         1280 <chr [1]> <chr [64]> <chr [20]>
#> 3 7                   0          150 <chr [1]> <chr [15]> <chr [10]>
#> 4 7                   1          150 <chr [1]> <chr [15]> <chr [10]>
#> 5 8                   0          290 <chr [1]> <chr [29]> <chr [10]>
#> 6 8                   1          290 <chr [1]> <chr [29]> <chr [10]>

selected <- selection(
  for_rail_gtfs,
  route_id,
  stop_id %in% for_rail_gtfs$stops$stop_id[1:3]
)

bbox <- sf::st_bbox(
  c(xmin = -38.58, ymin = -3.81, xmax = -38.50, ymax = -3.75),
  crs = sf::st_crs(4326)
)
spatial <- selection(
  for_rail_gtfs,
  geometry %intersects% sf::st_as_sfc(bbox)
)
```
