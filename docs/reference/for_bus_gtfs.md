# GTFS Data for Fortaleza (Bus System), Brazil.

A dataset containing GTFS (General Transit Feed Specification) data for
Fortaleza's transit system by bus. The data includes information on
routes, trips, stops, stop times, and other elements necessary for
transit planning and analysis.

## Format

An object of class `wizardgtfs`, containing multiple data frames:

- agency:

  Data frame with 1 row and 7 columns, providing information about the
  transit agency, including agency name, URL, timezone, and contact
  details.

- calendar:

  Data frame with 3 rows and 10 columns, detailing service availability
  by day of the week, start and end dates for each service.

- fare_attributes:

  Data frame with 2 rows and 6 columns, showing fare information,
  including price, currency, payment method, and transfer rules.

- fare_rules:

  Data frame with 345 rows and 5 columns, linking fare IDs to routes,
  along with optional restrictions on origins, destinations, and zones.

- routes:

  Data frame with 345 rows and 9 columns, listing route details such as
  route ID, agency ID, route short and long names, route type, and
  colors.

- shapes:

  Data frame with 125,776 rows and 5 columns, representing the spatial
  paths of routes with latitude, longitude, point sequence, and
  cumulative distance traveled.

- stop_times:

  Data frame with 2,659,737 rows and 9 columns, including stop times for
  each trip, with arrival and departure times, stop sequence, and stop
  ID information.

- stops:

  Data frame with 4,676 rows and 12 columns, containing information
  about each stop, including stop ID, name, location (latitude and
  longitude), and accessibility.

- trips:

  Data frame with 85,410 rows and 9 columns, detailing trips associated
  with routes, including trip IDs, route IDs, direction, block, and
  shape IDs.

## Source

Fortaleza transit agency (ETUFOR).

## Details

The GTFS data format is widely used for representing public
transportation schedules and associated geographic information. This
dataset follows the GTFS standard and includes elements for advanced
analysis in transit planning.

## Examples

``` r
# Load the dataset
data(for_bus_gtfs)

# Access trips data
head(for_bus_gtfs$trips)
#> # A tibble: 6 × 9
#>   route_id service_id trip_id         trip_headsign trip_short_name direction_id
#>   <chr>    <chr>      <chr>           <chr>         <chr>                  <dbl>
#> 1 011      D          D011-T01V01B01… ""            ""                         1
#> 2 011      D          D011-T01V02B01… ""            ""                         1
#> 3 011      D          D011-T01V03B01… ""            ""                         1
#> 4 011      D          D011-T01V04B01… ""            ""                         1
#> 5 011      D          D011-T01V05B01… ""            ""                         1
#> 6 011      D          D011-T01V06B01… ""            ""                         1
#> # ℹ 3 more variables: block_id <chr>, shape_id <chr>,
#> #   wheelchair_accessible <int>

# Access stops data
head(for_bus_gtfs$stops)
#> # A tibble: 6 × 12
#>   stop_id stop_code stop_name       stop_desc stop_lat stop_lon zone_id stop_url
#>   <chr>   <chr>     <chr>           <chr>        <dbl>    <dbl> <chr>   <chr>   
#> 1 10      ""        AVENIDA CEL CA… ""           -3.70    -38.6 ""      ""      
#> 2 1000    ""        RUA DOR JOAO M… ""           -3.72    -38.5 ""      ""      
#> 3 1001    ""        RUA DOR JOAO M… ""           -3.72    -38.5 ""      ""      
#> 4 1009    ""        AVENIDA TRISTÃ… ""           -3.73    -38.5 ""      ""      
#> 5 1010    ""        AVENIDA IMPERA… ""           -3.73    -38.5 ""      ""      
#> 6 1013    ""        AVENIDA IMPERA… ""           -3.72    -38.5 ""      ""      
#> # ℹ 4 more variables: location_type <int>, parent_station <chr>,
#> #   stop_timezone <chr>, wheelchair_boarding <int>
```
