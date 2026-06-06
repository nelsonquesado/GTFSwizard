#' Merge Two GTFS Feeds
#'
#' Combines two feeds table by table. By default, all identifiers and their
#' foreign-key references receive feed-specific suffixes, preventing accidental
#' collisions.
#'
#' @param gtfs_x,gtfs_y GTFS objects.
#' @param suffix Logical. Append `.x` and `.y` to identifiers when `TRUE`.
#' @param ... Supports the legacy arguments `gtfs.x` and `gtfs.y`.
#'
#' @return A validated `wizardgtfs` object.
#'
#' @details
#' Standard references in routes, trips, stop times, shapes, calendars,
#' frequencies, transfers, pathways, fare tables, networks, and booking rules
#' are updated together. Non-standard tables are row-bound without identifier
#' rewriting.
#'
#' @examples
#' merged <- merge_gtfs(for_rail_gtfs, for_rail_gtfs, suffix = TRUE)
#'
#' @references
#' [GTFS Schedule Reference](https://gtfs.org/documentation/schedule/reference/)
#' @export
merge_gtfs <- function(gtfs_x = NULL, gtfs_y = NULL, suffix = TRUE, ...){
  dots <- list(...)
  resolved_x <- resolve_legacy_argument(
    gtfs_x, missing(gtfs_x), dots, "gtfs.x", "gtfs_x"
  )
  gtfs_x <- resolved_x$value
  resolved_y <- resolve_legacy_argument(
    gtfs_y, missing(gtfs_y), resolved_x$dots, "gtfs.y", "gtfs_y"
  )
  gtfs_y <- resolved_y$value
  gw_check_unused_dots(resolved_y$dots)
  if(is.null(gtfs_x) || is.null(gtfs_y)){
    gw_stop("supply both `gtfs_x` and `gtfs_y`.")
  }
  gw_assert_flag(suffix, "suffix")
  gtfs_x <- ensure_wizardgtfs(gtfs_x)
  gtfs_y <- ensure_wizardgtfs(gtfs_y)
  gtfs_x$dates_services <- NULL
  gtfs_y$dates_services <- NULL

  if(suffix){
    gtfs_x <- suffix_gtfs_ids(gtfs_x, ".x")
    gtfs_y <- suffix_gtfs_ids(gtfs_y, ".y")
  }

  table_names <- union(names(gtfs_x), names(gtfs_y))
  merged <- setNames(vector("list", length(table_names)), table_names)
  for(table_name in table_names){
    merged[[table_name]] <- dplyr::distinct(dplyr::bind_rows(
      gtfs_x[[table_name]], gtfs_y[[table_name]]
    ))
  }
  GTFSwizard::as_wizardgtfs(merged, build_shapes = FALSE)
}

suffix_gtfs_ids <- function(gtfs, suffix){
  if(!"agency_id" %in% names(gtfs$agency)){
    gtfs$agency$agency_id <- "agency"
  }
  if(!"agency_id" %in% names(gtfs$routes) && nrow(gtfs$agency) == 1L){
    gtfs$routes$agency_id <- gtfs$agency$agency_id[1L]
  }

  id_fields <- list(
    agency = "agency_id",
    routes = c("route_id", "agency_id", "network_id"),
    trips = c("trip_id", "route_id", "service_id", "shape_id", "block_id"),
    stop_times = c(
      "trip_id", "stop_id", "pickup_booking_rule_id",
      "drop_off_booking_rule_id", "location_group_id", "location_id"
    ),
    stops = c("stop_id", "parent_station", "level_id"),
    calendar = "service_id",
    calendar_dates = "service_id",
    shapes = "shape_id",
    frequencies = "trip_id",
    transfers = c(
      "from_stop_id", "to_stop_id", "from_route_id", "to_route_id",
      "from_trip_id", "to_trip_id"
    ),
    fare_attributes = "fare_id",
    fare_rules = c("fare_id", "route_id"),
    pathways = c("pathway_id", "from_stop_id", "to_stop_id"),
    levels = "level_id",
    attributions = c("attribution_id", "agency_id", "route_id", "trip_id"),
    fare_products = "fare_product_id",
    fare_leg_rules = c(
      "fare_product_id", "network_id", "from_area_id", "to_area_id"
    ),
    fare_transfer_rules = c(
      "from_leg_group_id", "to_leg_group_id", "fare_product_id"
    ),
    areas = "area_id",
    stop_areas = c("area_id", "stop_id"),
    networks = "network_id",
    route_networks = c("route_id", "network_id"),
    location_groups = "location_group_id",
    location_group_stops = c("location_group_id", "stop_id"),
    booking_rules = "booking_rule_id"
  )
  for(table_name in intersect(names(id_fields), names(gtfs))){
    for(field in intersect(id_fields[[table_name]], names(gtfs[[table_name]]))){
      value <- as.character(gtfs[[table_name]][[field]])
      present <- !is.na(value) & nzchar(value)
      value[present] <- paste0(value[present], suffix)
      gtfs[[table_name]][[field]] <- value
    }
  }
  gtfs
}
