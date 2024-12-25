# GTFSwizard 1.0.1 (dev)

* Now featuring travel time matrices, trip speed and dweel time edition, corridors, hubs, and more!

* New functions
  - tidy_raptor() estimates travel time matrices from gtfs feeds;
  - edit_speeds() Adjust Travel Speed in a GTFS Dataset
  - get_1stdeparture() retrieves the start timepoint of each trip;
  - get_corridor() suggests "right-of-way" transit corridors;
  - plot_corridor() plots the suggested corridors from get_corridor() function;
  - edit_dweeltime() modifies the dwell time of trips;
  - get_hubs_cluster() Identifies and Cluster High-Transfer Stops ;
  - get_stops_hubs() Identify and Extract Stop Hubs Based on Transfers and Routes;
  - plot_hubs_cluster() Visualize Clusters of Transit Hubs;
  - latlon2epsg() determines the appropriate EPSG code for a given sf object.
 
* New methods
  - get_headways() now accepts 'method = "by.stop"' and 'method = "by.shape"'.
  - get_frequency() now accepts 'method = "by.shape"' and 'method = "by.stop"'.
  
* Bug fix & improvements
  - Better column naming;
  - Minor bug fixed on filter_date();
  - get_headways() output fixed;
  - split_trip() can now be used after filter_time();
  - Now one can choose the trips to perform get_speed(), get_durations() and get_distances().

# GTFSwizard 1.0.0

* Initial CRAN submission.
