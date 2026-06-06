#' Shift Trips in Time
#'
#' Adds a number of seconds to every non-empty arrival and departure time for
#' selected trips. GTFS times above 24 hours are preserved.
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
  for(field in c("arrival_time", "departure_time")){
    original <- gtfs$stop_times[[field]]
    parsed <- gtfs_time_to_seconds(original)
    edit <- selected & !is.na(parsed)
    shifted <- parsed
    shifted[edit] <- shifted[edit] + seconds
    if(any(shifted[edit] < 0)){
      gw_stop("the requested shift would create a negative GTFS time.")
    }
    original[edit] <- seconds_to_gtfs_time(shifted[edit])
    gtfs$stop_times[[field]] <- original
  }
  gtfs
}
