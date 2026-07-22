#' GTFSwizard: Create, Explore, and Edit GTFS Feeds
#'
#' @description
#' GTFSwizard creates, reads, validates, analyzes, visualizes, edits, and
#' exports General Transit Feed Specification (GTFS) Schedule feeds.
#'
#' @section Main workflows:
#' Use [create_gtfs()] or [read_gtfs()] to obtain a `wizardgtfs` object,
#' [get_servicepattern()] and the `get_*()` functions to analyze scheduled
#' service, the `filter_*()` and edit functions to construct scenarios, and
#' [explore_gtfs()] for an interactive dashboard. Use [write_gtfs()] to export
#' a standard GTFS zip archive.
#'
#' @section Example feeds:
#' [for_rail_gtfs] is a compact feed suitable for examples. [for_bus_gtfs]
#' supports larger-network checks and demonstrations.
#'
#' @seealso [wizardgtfs-methods]
#' @md
#' @keywords internal
"_PACKAGE"
