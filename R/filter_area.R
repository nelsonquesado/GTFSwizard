#' Filter a Feed by Stop Locations in an Area
#'
#' Select stops intersecting a polygon or bounding box, retaining either whole
#' trips that serve those stops or only the calls inside the area.
#' @param gtfs A GTFS object.
#' @param area An `sf`/`sfc` polygon with a known CRS, an `st_bbox`, or a numeric
#'   WGS84 bounding box ordered xmin, ymin, xmax, ymax.
#' @param full_trips Logical. Retain complete trips (default). If `FALSE`, retain
#'   only inside calls; trips with fewer than two remaining calls are removed.
#'   Cropped endpoints must have known arrival and departure times.
#' @return A `wizardgtfs` object with related tables pruned. Parent stations are
#'   retained. No matching service returns an empty feed with a warning.
#' @details Selection uses stop points, including polygon boundaries, rather
#'   than shape intersection. Cropped trips keep original stop sequences and
#'   shapes; distance calculations cover only retained stops. Frequency windows
#'   are shifted when cropping changes the first stop. Original times and
#'   measures are retained. A crop may connect retained stops across an excluded
#'   area; it does not split trips into separate contiguous sections.
#' @examples
#' filter_area(for_rail_gtfs, c(-39, -4, -38, -3))
#' @export
filter_area <- function(gtfs, area, full_trips = TRUE){
  gw_assert_flag(full_trips, "full_trips")
  gtfs <- ensure_wizardgtfs(gtfs)
  if(inherits(area, "bbox")) area <- sf::st_as_sfc(area)
  if(is.numeric(area) && !inherits(area, "sfc")){
    if(length(area) != 4L || any(!is.finite(area)) || area[1] > area[3] || area[2] > area[4] ||
       any(area[c(1, 3)] < -180 | area[c(1, 3)] > 180) ||
       any(area[c(2, 4)] < -90 | area[c(2, 4)] > 90)){
      gw_stop("`area` must be an ordered WGS84 bounding box: xmin, ymin, xmax, ymax.")
    }
    area <- sf::st_as_sfc(sf::st_bbox(stats::setNames(as.numeric(area),
      c("xmin", "ymin", "xmax", "ymax")), crs = sf::st_crs(4326)))
  }
  if(!inherits(area, c("sf", "sfc")) || is.na(sf::st_crs(area)) ||
     !all(sf::st_geometry_type(area) %in% c("POLYGON", "MULTIPOLYGON")) ||
     any(!sf::st_is_valid(area))){
    gw_stop("`area` must contain valid polygons with a known CRS.")
  }
  stops <- gtfs$stops
  valid <- is.finite(stops$stop_lon) & is.finite(stops$stop_lat)
  points <- sf::st_as_sf(stops[valid, ], coords = c("stop_lon", "stop_lat"), crs = 4326)
  # Closed boundaries have predictable selection semantics, also under s2.
  inside <- lengths(sf::st_intersects(points, sf::st_transform(area, 4326), model = "closed")) > 0L
  ids <- stops$stop_id[valid][inside]
  calls <- gtfs$stop_times[gtfs$stop_times$stop_id %in% ids, , drop = FALSE]
  trips <- unique(calls$trip_id)
  if(!full_trips){
    counts <- table(calls$trip_id)
    trips <- names(counts)[counts >= 2L]
    calls <- calls[calls$trip_id %in% trips, , drop = FALSE]
    ordered <- calls[order(calls$trip_id, calls$stop_sequence), , drop = FALSE]
    endpoint <- !duplicated(ordered$trip_id) | !duplicated(ordered$trip_id, fromLast = TRUE)
    invalid <- endpoint & (is.na(gtfs_time_to_seconds(ordered$arrival_time)) |
                           is.na(gtfs_time_to_seconds(ordered$departure_time)))
    if(any(invalid)){
      gw_stop("cropped trips require known arrival and departure times at both endpoints: ",
              paste(unique(ordered$trip_id[invalid]), collapse = ", "), ".")
    }
    if(!is.null(gtfs$frequencies) && nrow(calls)){
      first_time <- function(x){
        x <- x[order(x$trip_id, x$stop_sequence), ]
        x <- x[!duplicated(x$trip_id), ]
        value <- gtfs_time_to_seconds(x$departure_time)
        missing <- is.na(value)
        value[missing] <- gtfs_time_to_seconds(x$arrival_time[missing])
        stats::setNames(value, x$trip_id)
      }
      old <- first_time(gtfs$stop_times)
      new <- first_time(calls)
      f <- gtfs$frequencies
      shift <- new[as.character(f$trip_id)] - old[as.character(f$trip_id)]
      retained <- f$trip_id %in% trips
      if(any(retained & !is.finite(shift))){
        gw_stop("cropped frequency trips require a known time at their new first stop.")
      }
      for(field in c("start_time", "end_time")){
        f[[field]][retained] <- seconds_to_gtfs_time(gtfs_time_to_seconds(f[[field]][retained]) + shift[retained])
      }
      gtfs$frequencies <- f
    }
  }
  if(!length(trips)) gw_warn("the area returned no service.")
  prune_gtfs(gtfs, trips, stop_times = if(full_trips) NULL else calls)
}
