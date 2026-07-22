# Merge Two GTFS Feeds

Combines two feeds table by table. By default, all identifiers and their
foreign-key references receive feed-specific suffixes, preventing
accidental collisions.

## Usage

``` r
merge_gtfs(gtfs_x = NULL, gtfs_y = NULL, suffix = TRUE, ...)
```

## Arguments

- gtfs_x, gtfs_y:

  GTFS objects.

- suffix:

  Logical. Append \`.x\` and \`.y\` to identifiers when \`TRUE\`.

- ...:

  Supports the legacy arguments \`gtfs.x\` and \`gtfs.y\`.

## Value

A validated \`wizardgtfs\` object.

## Details

Standard references in routes, trips, stop times, shapes, calendars,
frequencies, transfers, pathways, fare tables, networks, and booking
rules are updated together. Non-standard tables are row-bound without
identifier rewriting.

## References

\[GTFS Schedule
Reference\](https://gtfs.org/documentation/schedule/reference/)

## Examples

``` r
merged <- merge_gtfs(for_rail_gtfs, for_rail_gtfs, suffix = TRUE)
```
