#' Compare Scheduled Supply in Two Scenarios
#'
#' Compare one common service date by route or across the network. No changes
#' are made to either feed.
#' @param baseline,scenario GTFS objects.
#' @param dates One service date available in both feeds. Required explicitly
#'   so calendar patterns are never accidentally compared as calendar days.
#' @param by Either `"route"` (default) or `"network"`.
#' @param distance_method Distance method passed to [get_distances()].
#' @return A long tibble with `route_id` (route comparisons), `metric`, `unit`,
#'   `baseline`, `scenario`, `change`, and `percent_change`. Percentage changes
#'   from a zero baseline are `NA`. Missing routes have zero additive supply;
#'   their span and mean headway are undefined (`NA`).
#' @details Metrics are departure instances, scheduled vehicle-hours,
#'   vehicle-kilometres, service span, mean departure headway, and maximum
#'   simultaneous active trips. Vehicle-hours include intermediate dwell but
#'   exclude layover/deadheading. Fleet is concurrent active trips, not an
#'   operational vehicle requirement. Headways compare first departures within
#'   route and direction and then average intervals; network headway pools these
#'   intervals rather than interleaving different routes. Frequency departures
#'   are nominal for `exact_times = 0`. Both feeds must use comparable route IDs.
#' @examples
#' compare_scenarios(for_rail_gtfs, for_rail_gtfs, dates = "2021-12-13")
#' @export
compare_scenarios <- function(baseline, scenario, dates, by = c("route", "network"),
                              distance_method = c("shapes", "straight")){
  by <- match.arg(by)
  distance_method <- match.arg(distance_method)
  dates <- parse_gtfs_date(dates)
  if(length(dates) != 1L || is.na(dates)) gw_stop("supply exactly one comparison date.")
  summarize <- function(gtfs){
    gtfs <- filter_date(ensure_wizardgtfs(gtfs), dates)
    durations <- trip_duration_table(gtfs)
    distances <- get_distances(gtfs, distance_method = distance_method)
    instances <- trip_instance_starts(gtfs) |>
      dplyr::left_join(gtfs$trips[, c("trip_id", "route_id", direction_field(gtfs$trips))], by = "trip_id") |>
      dplyr::left_join(durations, by = "trip_id") |>
      dplyr::left_join(distances[, c("trip_id", "distance")], by = "trip_id")
    if(any(!is.finite(instances$duration)) || any(instances$duration < 0) ||
       any(!is.finite(instances$distance))){
      gw_stop("scenario metrics require finite distances and nonnegative durations for every trip.")
    }
    groups <- if(by == "route") "route_id" else character()
    parts <- if(length(groups)) split(instances, instances$route_id) else list(network = instances)
    dplyr::bind_rows(lapply(parts, function(x){
      headway_groups <- c("route_id", direction_field(x))
      h <- calculate_headway_rows(x, headway_groups, "start_seconds")$headway_minutes
      starts <- x$start_seconds; ends <- starts + x$duration
      # Combine events at equal instants before computing fleet, as get_fleet does.
      events <- tibble::tibble(time = c(starts, ends), delta = rep(c(1L, -1L), each = nrow(x))) |>
        dplyr::group_by(time) |>
        dplyr::summarise(delta = sum(.data$delta), .groups = "drop") |>
        dplyr::arrange(time)
      values <- c(nrow(x), sum(x$duration) / 3600, sum(x$distance) / 1000,
                  if(nrow(x)) (max(ends) - min(starts)) / 3600 else NA_real_,
                  if(length(h)) mean(h) else NA_real_,
                  if(nrow(events)) max(cumsum(events$delta)) else 0)
      result <- tibble::tibble(metric = c("departures", "vehicle_hours", "vehicle_km", "service_span", "mean_headway", "active_fleet"),
        unit = c("trips", "hours", "km", "hours", "minutes", "vehicles"), value = values)
      if(by == "route") result$route_id <- rep(as.character(x$route_id[1]), nrow(result))
      result
    }))
  }
  a <- summarize(baseline); b <- summarize(scenario)
  names(a)[names(a) == "value"] <- "baseline"
  names(b)[names(b) == "value"] <- "scenario"
  result <- dplyr::full_join(a, b, by = c(if(by == "route") "route_id", "metric", "unit"))
  additive <- result$metric %in% c("departures", "vehicle_hours", "vehicle_km", "active_fleet")
  for(field in c("baseline", "scenario")) result[[field]][additive & is.na(result[[field]])] <- 0
  result$change <- result$scenario - result$baseline
  result$percent_change <- ifelse(!is.na(result$baseline) & result$baseline != 0,
                                   100 * result$change / result$baseline, NA_real_)
  result
}
