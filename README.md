# GTFSwizard <img align="right" src="GTFSwizard_logo.png?raw=true" alt="logo" width="180">
[![Lifecycle:
experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html)

GTFSwizard is a set of tools for exploring and manipulating [General Transit Feed Specification (GTFS)](https://gtfs.org/) files in R.

Its main purpose is to provide researchers and practitioners with a seamless and easy way to visually explore and simulate changes in  frequency, headway, dwell time, speed, and routes within a GTFS file.

## Installation
``` r
install.packages('remotes') # if not already installed
# wait for the installation to complete

remotes::install_github('OPATP/GTFSwizard')
```
## Cheat Sheet

## Usage
GTFS feeds are read using the `read_gtfs()` function.\
`read_gtfs()` returns a `wizardgtfs` object, which is a slightly improved `gtfs` object.\
You can also convert a regular `gtfs` object to a `wizardgtfs` object using the `as.wizardgtfs()` function
``` r
library(GTFSwizard)

gtfs <- read_gtfs('path-to-gtfs.zip') # or
gtfs <- as.wizardgtfs(gtfs_obj)

names(gtfs)
# [1] "agency"          "calendar"
# [3] "calendar_dates"  "fare_attributes"
# [5] "fare_rules"      "routes"
# [7] "shapes"          "stop_times"
# [9] "stops"           "trips"
# [11] "dates_services"

class(gtfs)
# [1] "wizardgtfs" "gtfs" "list"

summary(gtfs)
#A wizardgtfs object with:  
#
#10  GTFS tables 
#With the following names and respective numbers of entries in each: 
#         agency        calendar  calendar_dates fare_attributes 
#              1               3               6               2 
#     fare_rules          routes          shapes      stop_times 
#            345             345          125776         2659737 
#          stops           trips 
#           4676           85410 
#345  routes 
#4676  stops 
#85410  trips 
#823  valid days of service 
#271.4  meters is the average distance between sequencial stops in a given route 
```

GTFS feeds are explored using the `explore_gtfs()` function:
``` r
explore_gtfs(gtfs)
```

Routes frequency, headways, dell times, and speeds are calculated using the `get_frequency()`, the `get_headways()`, the `get_dwelltimes()`, and the `get_speed()` functions:
``` r
get_frequency(gtfs, simplify = FALSE)
## A tibble: 5,487 × 5
#   route_id  hour frequency service_pattern  pattern_frequency
#   <chr>    <dbl>     <int> <chr>                        <int>
# 1 1012-10     12         1 servicepattern-1             13779
# 2 1012-10     12         1 servicepattern-2              9951
# 3 1012-10     12         1 servicepattern-3              5358
# 4 1012-10     18         1 servicepattern-1             13779
# 5 1012-10     18         1 servicepattern-2              9951
# 6 1012-10     18         1 servicepattern-3              5358
# 7 1015-10     17         1 servicepattern-1             13779
# 8 1015-10     17         1 servicepattern-2              9951
# 9 1015-10     17         1 servicepattern-3              5358
#10 1016-10     12         1 servicepattern-1             13779
## ℹ 5,477 more rows
## ℹ Use `print(n = ...)` to see more rows

get_headways(gtfs, simplify = TRUE)
# A tibble: 5,635 × 5
#   route_id  hour average.headway service_pattern  pattern_frequency
#   <chr>    <dbl>           <dbl> <chr>                        <int>
# 1 1012-10     12             360 servicepattern-1             13779
# 2 1012-10     12             360 servicepattern-2              9951
# 3 1012-10     12             360 servicepattern-3              5358
# 4 1016-10     12             300 servicepattern-1             13779
# 5 1016-10     17              63 servicepattern-1             13779
# 6 1016-10     12             300 servicepattern-2              9951
# 7 1016-10     17              63 servicepattern-2              9951
# 8 1016-10     12             300 servicepattern-3              5358
# 9 1016-10     17              63 servicepattern-3              5358
#10 1017-10     17              60 servicepattern-1             13779
## ℹ 5,625 more rows
## ℹ Use `print(n = ...)` to see more rows

get_dwelltimes(gtfs, max.dwelltime = 60, simplify = FALSE)
## A tibble: 259,940 × 6
#   route_id stop_id   hour dwell_time service_pattern  pattern_frequency
#   <chr>    <fct>    <dbl>      <dbl> <chr>                        <int>
# 1 1012-10  301729      18          0 servicepattern-1             13779
# 2 1012-10  301729      18          0 servicepattern-2              9951
# 3 1012-10  301729      18          0 servicepattern-3              5358
# 4 1012-10  301764      18          0 servicepattern-1             13779
# 5 1012-10  301764      18          0 servicepattern-2              9951
# 6 1012-10  301764      18          0 servicepattern-3              5358
# 7 1012-10  301724      18          0 servicepattern-1             13779
# 8 1012-10  301724      18          0 servicepattern-2              9951
# 9 1012-10  301724      18          0 servicepattern-3              5358
#10 1012-10  30003042    18          0 servicepattern-1             13779
## ℹ 259,930 more rows
## ℹ Use `print(n = ...)` to see more rows

get_speed(gtfs)
## A tibble: 2,114,523 × 9
#   route_id from_stop_id to_stop_id  hour duration distance speed service_pattern  pattern_frequency
#   <fct>    <chr>        <chr>      <dbl>    <dbl>    <dbl> <dbl> <chr>                        <int>
# 1 011      3500         1013           5      120     376.  11.3 servicepattern-3               121
# 2 011      1013         1015           5       60     240.  14.4 servicepattern-3               121
# 3 011      1015         4251           5       60     265.  15.9 servicepattern-3               121
# 4 011      4251         990            5       60     244.  14.7 servicepattern-3               121
# 5 011      990          991            5       60     266.  16.0 servicepattern-3               121
# 6 011      991          989            5       60     282.  16.9 servicepattern-3               121
# 7 011      989          1600           5      120     627.  18.8 servicepattern-3               121
# 8 011      1600         1608           5      120     338.  10.1 servicepattern-3               121
# 9 011      1608         4767           5       60     337.  20.2 servicepattern-3               121
#10 011      4767         6450           5       60     260.  15.6 servicepattern-3               121
## ℹ 2,114,513 more rows
## ℹ Use `print(n = ...)` to see more rows
```

GTFSwizard also reconstructs missing shape tables using the `get_shapes()` function:
``` r
gtfs$shapes
#NULL

gtfs <- get_shapes(gtfs)
gtfs$shapes
## A tibble: 6,830 × 5
#   shape_id shape_pt_lon shape_pt_lat shape_pt_sequence shape_dist_traveled
#   <chr>           <dbl>        <dbl>             <int>               <dbl>
# 1 shape-1         -38.7        -3.96                 1                  0 
# 2 shape-1         -38.7        -3.95                 2                499.
# 3 shape-1         -38.7        -3.96                 3               1428 
# 4 shape-1         -38.7        -3.98                 4               3404.
# 5 shape-1         -38.7        -3.97                 5               5490.
# 6 shape-1         -38.7        -3.97                 6               6248.
# 7 shape-1         -38.7        -3.98                 7               7871.
# 8 shape-1         -38.8        -3.97                 8              11398.
# 9 shape-1         -38.7        -3.97                 9              13093.
#10 shape-1         -38.8        -3.97                10              13395 
## ℹ 6,820 more rows
## ℹ Use `print(n = ...)` to see more rows

```

## Related Packages
GTFSwizard mainly rellies on [dplyr](https://dplyr.tidyverse.org/), [tidytransit](https://cran.r-project.org/web/packages/tidytransit/vignettes/introduction.html) and [gtfsio](https://r-transit.github.io/gtfsio/articles/gtfsio.html) for data wrangling, [leaflet](https://leafletjs.com/) for map rendering, [ggplot2](https://ggplot2.tidyverse.org/) and [plotly](https://plotly.com/r/) for data visualization, and [shiny](https://shiny.posit.co/) for the `explore_gtfs()` application assembling.

## Acknowledgement <a href="https://www.det.ufc.br/petran"><img align="right" src="opatp.png" alt="OPA-TP" width="150" /></a>
**GTFSwizard** is developed by Nelson Quesado and Caio Guimarães at OPA-TP research group, Universidade Federal do Ceará.
