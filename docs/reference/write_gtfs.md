# Write a GTFS Feed

Exports a GTFS object to a zip archive. Internal \`dates_services\` data
are omitted, GTFS dates use \`YYYYMMDD\`, and spatial stops/shapes are
converted back to standard text-table columns.

## Usage

``` r
write_gtfs(gtfs, zipfile, ...)
```

## Arguments

- gtfs:

  A GTFS object.

- zipfile:

  Output \`.zip\` path.

- ...:

  Additional arguments passed to the format-specific writer.

## Value

The normalized output path, invisibly.

## See also

\[GTFSwizard::read_gtfs()\]

## Examples

``` r
path <- tempfile(fileext = ".zip")
write_gtfs(for_rail_gtfs, path)
```
