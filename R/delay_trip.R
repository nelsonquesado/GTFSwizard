#' Shift Trips in Time
#'
#' Adds a number of seconds to every non-empty arrival and departure time for
#' selected trips. Edited times that would move two or more days after midnight
#' are wrapped back to the equivalent time on the next service day while keeping
#' stop times non-decreasing within each trip.
#'
#' @param gtfs A GTFS object.
#' @param trip Character vector of `trip_id` values.
#' @param duration Numeric seconds or an object coercible to seconds, such as
#'   a `difftime`.
#'
#' @return A modified `wizardgtfs` object.
#'
#' @examples
#' delayed <- delay_trip(
#'   for_rail_gtfs,
#'   trip = for_rail_gtfs$trips$trip_id[1:2],
#'   duration = 300
#' )
#'
#' @seealso [GTFSwizard::edit_speed()], [GTFSwizard::set_dwelltime()]
#' @export
delay_trip <- function(gtfs, trip, duration){
  gtfs <- ensure_wizardgtfs(gtfs)
  assert_known_ids(trip, gtfs$trips$trip_id, "trip", "`gtfs$trips`")
  if(!(is.numeric(duration) || inherits(duration, c("difftime", "Duration"))) ||
     length(duration) != 1L || is.na(duration)){
    gw_stop("`duration` must be one numeric duration in seconds.")
  }
  seconds <- as.numeric(duration)

  selected <- gtfs$stop_times$trip_id %in% trip
  shifted_times <- gtfs$stop_times
  for(field in c("arrival_time", "departure_time")){
    original <- shifted_times[[field]]
    parsed <- gtfs_time_to_seconds(original)
    edit <- selected & !is.na(parsed)
    shifted <- parsed
    shifted[edit] <- shifted[edit] + seconds
    if(any(shifted[edit] < 0)){
      gw_stop("the requested shift would create a negative GTFS time.")
    }
    original[edit] <- seconds_to_gtfs_time(shifted[edit])
    shifted_times[[field]] <- original
  }
  original_order <- seq_len(nrow(shifted_times))
  shifted_times$.row <- original_order
  shifted_times <- shifted_times[order(shifted_times$trip_id, shifted_times$stop_sequence), ]
  arrival <- gtfs_time_to_seconds(shifted_times$arrival_time)
  departure <- gtfs_time_to_seconds(shifted_times$departure_time)
  groups <- split(seq_len(nrow(shifted_times)), shifted_times$trip_id)
  for(index in groups){
    if(!shifted_times$trip_id[index[1L]] %in% trip){
      next
    }
    wrapped <- wrap_trip_to_next_service_day(arrival[index], departure[index])
    arrival[index] <- wrapped$arrival
    departure[index] <- wrapped$departure
  }
  valid_arrival <- !is.na(arrival)
  valid_departure <- !is.na(departure)
  shifted_times$arrival_time[valid_arrival] <- seconds_to_gtfs_time(arrival[valid_arrival])
  shifted_times$departure_time[valid_departure] <- seconds_to_gtfs_time(departure[valid_departure])
  shifted_times <- shifted_times[order(shifted_times$.row), ]
  shifted_times$.row <- NULL
  gtfs$stop_times <- tibble::as_tibble(shifted_times)
  gtfs
}
