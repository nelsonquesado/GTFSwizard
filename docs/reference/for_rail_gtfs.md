# GTFS Data for Fortaleza (Rail System), Brazil

This dataset contains GTFS (General Transit Feed Specification) data for
Fortaleza's rail transit system, managed by METROFOR. The data includes
information on routes, trips, stops, stop times, shapes, and other
necessary elements for transit analysis and planning.

## Format

An object of class `wizardgtfs`, consisting of multiple data frames:

- agency:

  Data frame with 1 row and 7 columns, providing information about the
  transit agency, including agency name, URL, timezone, language, and
  contact details.

- calendar:

  Data frame with 1 row and 10 columns, detailing the service
  availability by day of the week, along with start and end dates for
  each service.

- calendar_dates:

  Data frame with 26 rows and 3 columns, listing specific dates and
  exceptions (e.g., holidays) that modify the usual service pattern.

- routes:

  Data frame with 3 rows and 9 columns, listing route details such as
  route ID, short and long names, route type, and colors associated with
  each route.

- stops:

  Data frame with 39 rows and 10 columns, containing information about
  each stop, including stop ID, name, location (latitude and longitude),
  and additional details.

- stop_times:

  Data frame with 3,420 rows and 10 columns, detailing arrival and
  departure times for each trip, along with stop sequences and stop IDs.

- trips:

  Data frame with 215 rows and 7 columns, providing trip-specific
  information such as trip ID, headsign, direction, associated service
  ID, route ID, and shape ID.

- shapes:

  Data frame with 80 rows and 5 columns, representing spatial paths of
  routes using latitude, longitude, point sequence, and cumulative
  distance traveled.

## Source

Cia Cearense de Transportes Metropolitanos (METROFOR).

## Details

The GTFS data format is widely adopted for representing public
transportation schedules and spatial information. This dataset follows
GTFS standards and is tailored for advanced analysis, particularly in
transit planning and operations. Key tables included are \`agency\`,
\`routes\`, \`stops\`, \`stop_times\`, \`trips\`, and \`shapes\`, each
providing essential attributes for a comprehensive transit analysis.

## Examples

``` r
# Load the dataset
data(for_rail_gtfs)

# Access trips data
head(for_rail_gtfs$trips)
#> # A tibble: 6 × 7
#>   trip_id trip_headsign direction_id block_id service_id route_id shape_id
#>   <chr>   <chr>                <int> <chr>    <chr>      <chr>    <chr>   
#> 1 4       Caucaia                  0 ""       4          7        shape-1 
#> 2 5       Caucaia                  0 ""       4          7        shape-1 
#> 3 6       Caucaia                  0 ""       4          7        shape-1 
#> 4 7       Caucaia                  0 ""       4          7        shape-1 
#> 5 8       Caucaia                  0 ""       4          7        shape-1 
#> 6 9       Caucaia                  0 ""       4          7        shape-1 

# Access stops data
head(for_rail_gtfs$stops)
#> # A tibble: 6 × 10
#>   stop_id stop_code stop_name       stop_desc stop_lat stop_lon zone_id stop_url
#>   <chr>   <chr>     <chr>           <chr>        <dbl>    <dbl> <chr>   <chr>   
#> 1 66      ""        Papicu          ""           -3.74    -38.5 ""      ""      
#> 2 65      ""        Antônio Sales   ""           -3.75    -38.5 ""      ""      
#> 3 64      ""        Pontes Vieira   ""           -3.75    -38.5 ""      ""      
#> 4 63      ""        São João do Ta… ""           -3.76    -38.5 ""      ""      
#> 5 41      ""        Borges de Melo  ""           -3.76    -38.5 ""      ""      
#> 6 40      ""        Vila União      ""           -3.77    -38.5 ""      ""      
#> # ℹ 2 more variables: location_type <int>, stop_timezone <chr>
```
