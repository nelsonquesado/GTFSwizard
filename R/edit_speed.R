#' Scale In-Vehicle Travel Speed
#'
#' Changes travel time between consecutive stops by dividing it by a speed
#' multiplier. Dwell times are preserved and all downstream times are shifted.
#' Edited times that would move two or more days after midnight are wrapped back
#' to the equivalent time on the next service day while keeping stop times
#' non-decreasing within each trip.
#'
#' @param gtfs A GTFS object.
#' @param trips Character trip IDs or `"all"`.
#' @param stops Character stop IDs or `"all"`. A segment is edited when either
#'   endpoint is selected.
#' @param factor One positive speed multiplier. For example, `2` halves segment
#'   travel times.
#'
#' @return A modified `wizardgtfs` object.
#'
#' @examples
#' edited <- edit_speed(
#'   for_rail_gtfs,
#'   trips = for_rail_gtfs$trips$trip_id[1:2],
#'   stops = "all",
#'   factor = 1.25
#' )
#'
#' @seealso [GTFSwizard::get_speeds()], [GTFSwizard::get_durations()]
#' @export
edit_speed <- function(gtfs, trips = "all", stops = "all", factor){
  if(!is.numeric(factor) || length(factor) != 1L || is.na(factor) || factor <= 0){
    gw_stop("`factor` must be one positive numeric value.")
  }
  gtfs <- ensure_wizardgtfs(gtfs)
  trips <- resolve_selection_ids(trips, gtfs$trips$trip_id, "trip")
  stops <- resolve_selection_ids(stops, gtfs$stops$stop_id, "stop")

  stop_times <- gtfs$stop_times
  stop_times$.row <- seq_len(nrow(stop_times))
  stop_times <- stop_times[order(stop_times$trip_id, stop_times$stop_sequence), ]
  arrival <- gtfs_time_to_seconds(stop_times$arrival_time)
  departure <- gtfs_time_to_seconds(stop_times$departure_time)

  groups <- split(seq_len(nrow(stop_times)), stop_times$trip_id)
  edited_groups <- list()
  for(index in groups){
    if(!stop_times$trip_id[index[1L]] %in% trips || length(index) < 2L){
      next
    }
    segment <- index[-1L]
    previous <- index[-length(index)]
    travel <- arrival[segment] - departure[previous]
    edit <- (stop_times$stop_id[segment] %in% stops |
      stop_times$stop_id[previous] %in% stops) &
      !is.na(travel)
    if(any(edit & travel < 0)){
      gw_stop("selected segments contain arrival before the previous departure.")
    }
    if(any(edit)){
      edited_groups[[length(edited_groups) + 1L]] <- index
    }
    delta <- rep(0, length(index))
    segment_position <- seq_along(index)[-1L]
    delta[segment_position[edit]] <- round(travel[edit] / factor) - travel[edit]
    cumulative <- cumsum(delta)
    arrival[index] <- arrival[index] + cumulative
    departure[index] <- departure[index] + cumulative
  }

  for(index in edited_groups){
    wrapped <- wrap_trip_to_next_service_day(arrival[index], departure[index])
    arrival[index] <- wrapped$arrival
    departure[index] <- wrapped$departure
  }

  valid <- !is.na(arrival)
  stop_times$arrival_time[valid] <- seconds_to_gtfs_time(arrival[valid])
  valid <- !is.na(departure)
  stop_times$departure_time[valid] <- seconds_to_gtfs_time(departure[valid])
  stop_times <- stop_times[order(stop_times$.row), ]
  stop_times$.row <- NULL
  gtfs$stop_times <- tibble::as_tibble(stop_times)
  gtfs
}

wrap_time_to_next_service_day <- function(seconds){
  day <- 24 * 60 * 60
  overflow <- !is.na(seconds) & seconds >= 2 * day
  seconds[overflow] <- day + (seconds[overflow] %% day)
  seconds
}

wrap_trip_to_next_service_day <- function(arrival, departure){
  arrival <- wrap_time_to_next_service_day(arrival)
  departure <- wrap_time_to_next_service_day(departure)

  previous_departure <- NA_real_
  for(i in seq_along(arrival)){
    if(!is.na(previous_departure) && !is.na(arrival[i]) &&
       arrival[i] < previous_departure){
      arrival[i] <- previous_departure
    }
    if(!is.na(arrival[i]) && !is.na(departure[i]) &&
       departure[i] < arrival[i]){
      departure[i] <- arrival[i]
    }
    if(!is.na(departure[i])){
      previous_departure <- departure[i]
    }
  }

  list(arrival = arrival, departure = departure)
}
