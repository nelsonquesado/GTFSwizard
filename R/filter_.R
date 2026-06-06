#' Filter a GTFS Feed
#'
#' Filters a GTFS feed while preserving referential integrity across routes,
#' trips, stops, shapes, calendars, frequencies, fares, and transfers.
#'
#' @param gtfs A GTFS object.
#' @param servicepattern Character vector of service-pattern IDs returned by
#'   [get_servicepattern()]. When `NULL`, the most frequent pattern is used.
#' @param dates Dates to retain. Values accepted by [as.Date()] are supported.
#'   When `NULL`, the latest available service date is used.
#' @param service,route,trip,stop Character vectors of IDs to retain.
#' @param from,to Inclusive GTFS time bounds in `"HH:MM:SS"` form. Hours may
#'   exceed 24.
#' @param keep Logical. If `FALSE`, the specified route or trip IDs are
#'   excluded.
#'
#' @details
#' `filter_stop()` retains the requested stop calls and therefore may return
#' partial trips. `filter_time()` retains individual stop calls whose arrival
#' or departure falls inside the inclusive time interval and may also return
#' partial trips. This behavior is useful for network experiments. Route,
#' trip, service, service-pattern, and date filters retain complete trips.
#'
#' `filter_date()` rewrites service availability as `calendar_dates` additions
#' for exactly the selected dates. Other filters preserve the selected
#' services' original date ranges and exceptions.
#'
#' @return A `wizardgtfs` object.
#'
#' @examples
#' typical <- filter_servicepattern(for_rail_gtfs)
#' one_day <- filter_date(for_rail_gtfs, "2021-02-10")
#' one_route <- filter_route(for_rail_gtfs, for_rail_gtfs$routes$route_id[1])
#' two_trips <- filter_trip(for_rail_gtfs, for_rail_gtfs$trips$trip_id[1:2])
#' serving_stop <- filter_stop(for_rail_gtfs, for_rail_gtfs$stops$stop_id[1])
#' morning <- filter_time(for_rail_gtfs, from = "06:30:00", to = "10:00:00")
#'
#' @references
#' [GTFS Schedule Reference](https://gtfs.org/documentation/schedule/reference/)
#' @seealso [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
#' @rdname filter_functions
#' @export
filter_servicepattern <- function(gtfs, servicepattern = NULL){
  gtfs <- ensure_wizardgtfs(gtfs)
  patterns <- GTFSwizard::get_servicepattern(gtfs)
  if(!nrow(patterns)){
    gw_stop("no service patterns are available.")
  }
  if(is.null(servicepattern)){
    servicepattern <- patterns$service_pattern[
      which.max(patterns$pattern_frequency)
    ]
    gw_warn(
      "no `servicepattern` supplied; using `", servicepattern,
      "`, the most frequent pattern."
    )
  }
  assert_known_ids(
    servicepattern, patterns$service_pattern, "service pattern",
    "`get_servicepattern()`"
  )
  services <- unique(patterns$service_id[
    patterns$service_pattern %in% servicepattern
  ])
  prune_gtfs(gtfs, gtfs$trips$trip_id[gtfs$trips$service_id %in% services])
}

#' @rdname filter_functions
#' @export
filter_date <- function(gtfs, dates = NULL){
  gtfs <- ensure_wizardgtfs(gtfs)
  available <- as.Date(gtfs$dates_services$date)
  if(!length(available)){
    gw_stop("the feed has no active service dates.")
  }
  if(is.null(dates)){
    dates <- max(available)
    gw_warn("no `dates` supplied; using the latest service date, ", dates, ".")
  } else {
    dates <- parse_gtfs_date(dates)
  }
  if(anyNA(dates)){
    gw_stop("`dates` must contain valid calendar dates.")
  }
  unavailable <- setdiff(dates, available)
  if(length(unavailable)){
    gw_stop(
      "date(s) are outside the feed calendar: ",
      paste(unavailable, collapse = ", "), "."
    )
  }

  selected <- gtfs$dates_services[
    as.Date(gtfs$dates_services$date) %in% dates, , drop = FALSE
  ]
  services_by_date <- tidyr::unnest(selected, cols = "service_id")
  gtfs <- prune_gtfs(gtfs, gtfs$trips$trip_id[
    gtfs$trips$service_id %in% services_by_date$service_id
  ])
  gtfs[["calendar"]] <- NULL
  gtfs[["calendar_dates"]] <- unique(tibble::tibble(
    service_id = as.character(services_by_date$service_id),
    date = as.Date(services_by_date$date),
    exception_type = 1L
  ))
  gtfs <- create_dates_services_table(gtfs)
  class(gtfs) <- c("wizardgtfs", "gtfs", "list")
  gtfs
}

#' @rdname filter_functions
#' @export
filter_service <- function(gtfs, service){
  gtfs <- ensure_wizardgtfs(gtfs)
  assert_known_ids(service, gtfs$trips$service_id, "service", "`gtfs$trips`")
  prune_gtfs(gtfs, gtfs$trips$trip_id[gtfs$trips$service_id %in% service])
}

#' @rdname filter_functions
#' @export
filter_route <- function(gtfs, route, keep = TRUE){
  gtfs <- ensure_wizardgtfs(gtfs)
  gw_assert_flag(keep, "keep")
  assert_known_ids(route, gtfs$routes$route_id, "route", "`gtfs$routes`")
  selected <- if(keep){
    gtfs$trips$route_id %in% route
  } else {
    !gtfs$trips$route_id %in% route
  }
  prune_gtfs(gtfs, gtfs$trips$trip_id[selected])
}

#' @rdname filter_functions
#' @export
filter_trip <- function(gtfs, trip, keep = TRUE){
  gtfs <- ensure_wizardgtfs(gtfs)
  gw_assert_flag(keep, "keep")
  assert_known_ids(trip, gtfs$trips$trip_id, "trip", "`gtfs$trips`")
  selected <- if(keep){
    gtfs$trips$trip_id %in% trip
  } else {
    !gtfs$trips$trip_id %in% trip
  }
  prune_gtfs(gtfs, gtfs$trips$trip_id[selected])
}

#' @rdname filter_functions
#' @export
filter_stop <- function(gtfs, stop){
  gtfs <- ensure_wizardgtfs(gtfs)
  assert_known_ids(stop, gtfs$stops$stop_id, "stop", "`gtfs$stops`")
  stop_times <- gtfs$stop_times[
    gtfs$stop_times$stop_id %in% stop, , drop = FALSE
  ]
  prune_gtfs(gtfs, unique(stop_times$trip_id), stop_times = stop_times)
}

#' @rdname filter_functions
#' @export
filter_time <- function(gtfs, from = "00:00:00", to = "48:00:00"){
  gtfs <- ensure_wizardgtfs(gtfs)
  bounds <- gtfs_time_to_seconds(c(from, to))
  if(anyNA(bounds)){
    gw_stop("`from` and `to` must be valid GTFS times in `HH:MM:SS` form.")
  }
  if(bounds[1] > bounds[2]){
    gw_stop("`from` must be earlier than or equal to `to`.")
  }

  arrival <- gtfs_time_to_seconds(gtfs$stop_times$arrival_time)
  departure <- gtfs_time_to_seconds(gtfs$stop_times$departure_time)
  selected <- (
    !is.na(arrival) & arrival >= bounds[1] & arrival <= bounds[2]
  ) | (
    !is.na(departure) & departure >= bounds[1] & departure <= bounds[2]
  )
  stop_times <- gtfs$stop_times[selected, , drop = FALSE]
  if(!nrow(stop_times)){
    gw_warn("the time interval returned no stop calls.")
  }
  prune_gtfs(
    gtfs,
    unique(stop_times$trip_id),
    stop_times = stop_times
  )
}

assert_known_ids <- function(ids, available, label, source){
  if(is.null(ids) || !length(ids)){
    gw_stop("supply at least one ", label, " ID.")
  }
  unknown <- setdiff(as.character(ids), as.character(available))
  if(length(unknown)){
    shown <- paste(utils::head(unknown, 5L), collapse = ", ")
    suffix <- if(length(unknown) > 5L) ", ..." else ""
    gw_stop(
      "unknown ", label, " ID(s): ", shown, suffix,
      ". See ", source, " for available values."
    )
  }
  invisible(TRUE)
}
