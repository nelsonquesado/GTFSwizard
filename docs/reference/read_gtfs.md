# Read a GTFS Feed

Imports a GTFS zip archive and converts it to \`wizardgtfs\`.

## Usage

``` r
read_gtfs(file_path = NULL, files = NULL, quiet = TRUE, ...)
```

## Arguments

- file_path:

  Path to a GTFS \`.zip\` archive.

- files:

  Optional character vector of table names without \`.txt\`.

- quiet:

  Logical. Suppress importer messages.

- ...:

  Additional arguments passed to \[gtfsio::import_gtfs()\], including
  the legacy argument \`file.path\`.

## Value

A validated \`wizardgtfs\` object.

## See also

\[GTFSwizard::write_gtfs()\], \[GTFSwizard::as_wizardgtfs()\]

## Examples

``` r
path <- tempfile(fileext = ".zip")
write_gtfs(for_rail_gtfs, path)
gtfs <- read_gtfs(path)
```
