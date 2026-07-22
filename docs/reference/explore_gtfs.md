# Explore GTFS Data in an Interactive Shiny Dashboard

Opens a lightweight Shiny dashboard for exploring a GTFS feed. The
dashboard shows summary cards, route and stop maps, planning indicators,
service calendars, operational charts, and corridor and hub views.
Route, service, service-pattern, stop, date, and time filters update the
dashboard without changing the original object. Edits and exports
operate on an in-memory copy.

## Usage

``` r
explore_gtfs(gtfs = NULL, plotly = FALSE)
```

## Arguments

- gtfs:

  A GTFS object, preferably of class \`wizardgtfs\`. When omitted or
  \`NULL\` in an interactive session, a file-selection window opens so
  the user can choose a GTFS \`.zip\` archive.

- plotly:

  Logical. If \`TRUE\`, eligible dashboard charts are rendered as
  interactive plotly widgets. The calendar and trip-duration boxplot
  remain static because conversion reduces their readability. \`plotly\`
  is optional and only required when this argument is \`TRUE\`.

## Value

A Shiny app object.

## References

Ceder, A. (2007). \*Public Transit Planning and Operation\*. Vuchic, V.
R. (2005). \*Urban Transit: Operations, Planning, and Economics\*.
White, P. (2008). \*Public Transport: Its Planning, Management and
Operation\*. Cascetta, E. (2009). \*Transportation Systems Analysis\*.

## See also

\[GTFSwizard::as_wizardgtfs()\], \[GTFSwizard::get_shapes()\],
\[GTFSwizard::plot_calendar()\]

## Examples

``` r
if (interactive()) {
  explore_gtfs()
  explore_gtfs(GTFSwizard::for_rail_gtfs)
}
```
