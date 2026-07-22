# Convert a GTFS Feed to \`wizardgtfs\`

Converts a list-like GTFS feed to the class used by GTFSwizard.
Character GTFS dates are converted to \[Date\] values, a date-to-service
lookup is created, and missing shapes can optionally be inferred from
stop locations.

## Usage

``` r
as_wizardgtfs(gtfs_list, build_shapes = TRUE)
```

## Arguments

- gtfs_list:

  A named list of GTFS tables or a \`tidygtfs\` object.

- build_shapes:

  Logical. If \`TRUE\`, infer \`shapes\` when the table is absent.
  Inferred shapes connect stops with straight line segments and are
  intended for analysis and visualization, not map-matched routing.

## Value

A \`wizardgtfs\` object.

## Details

The input is checked for required tables, required fields, primary-key
duplication, foreign-key consistency, increasing stop and shape
sequences, valid service dates, and valid GTFS time strings. Times
remain character values because GTFS permits hours greater than 24.

## See also

\[GTFSwizard::create_gtfs()\], \[GTFSwizard::get_shapes()\]

## Examples

``` r
gtfs_wizard <- as_wizardgtfs(for_rail_gtfs, build_shapes = TRUE)
```
