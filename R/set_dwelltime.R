#' Set Dwell Times
#'
#' Sets dwell time at selected trip-stop calls and propagates each change to
#' all later times in the same trip. Arrival at the edited stop is retained.
#'
#' @param gtfs A GTFS object.
#' @param duration One non-negative dwell time in seconds.
#' @param trips,stops Character ID vectors or `"all"`.
#'
#' @return A modified `wizardgtfs` object.
#'
#' @examples
#' edited <- set_dwelltime(
#'   for_rail_gtfs,
#'   duration = 30,
#'   trips = for_rail_gtfs$trips$trip_id[1:2],
#'   stops = for_rail_gtfs$stops$stop_id[1:2]
#' )
#'
#' @seealso [GTFSwizard::edit_dwelltime()], [GTFSwizard::get_dwelltimes()]
#' @export
set_dwelltime <- function(gtfs, duration = 30, trips = "all", stops = "all"){
  if(!is.numeric(duration) || length(duration) != 1L ||
     is.na(duration) || duration < 0){
    gw_stop("`duration` must be one non-negative number of seconds.")
  }
  update_dwell_times(gtfs, trips, stops, function(x) rep(round(duration), length(x)))
}

update_dwell_times <- function(gtfs, trips, stops, transform){
  gtfs <- ensure_wizardgtfs(gtfs)
  trips <- resolve_selection_ids(trips, gtfs$trips$trip_id, "trip")
  stops <- resolve_selection_ids(stops, gtfs$stops$stop_id, "stop")

  original_order <- seq_len(nrow(gtfs$stop_times))
  stop_times <- gtfs$stop_times
  stop_times$.row <- original_order
  stop_times <- stop_times[order(stop_times$trip_id, stop_times$stop_sequence), ]
  arrival <- gtfs_time_to_seconds(stop_times$arrival_time)
  departure <- gtfs_time_to_seconds(stop_times$departure_time)
  editable <- stop_times$trip_id %in% trips & stop_times$stop_id %in% stops &
    !is.na(arrival) & !is.na(departure)
  dwell <- departure - arrival
  if(any(editable & dwell < 0)){
    gw_stop("selected calls contain a departure before arrival.")
  }

  delta <- rep(0, nrow(stop_times))
  delta[editable] <- transform(dwell[editable]) - dwell[editable]
  groups <- split(seq_len(nrow(stop_times)), stop_times$trip_id)
  for(index in groups){
    cumulative <- cumsum(delta[index])
    before <- c(0, utils::head(cumulative, -1L))
    arrival[index] <- arrival[index] + before
    departure[index] <- departure[index] + cumulative
  }
  valid_arrival <- !is.na(arrival)
  valid_departure <- !is.na(departure)
  stop_times$arrival_time[valid_arrival] <- seconds_to_gtfs_time(arrival[valid_arrival])
  stop_times$departure_time[valid_departure] <- seconds_to_gtfs_time(departure[valid_departure])
  stop_times <- stop_times[order(stop_times$.row), ]
  stop_times$.row <- NULL
  gtfs$stop_times <- tibble::as_tibble(stop_times)
  gtfs
}

resolve_selection_ids <- function(value, available, label){
  if(length(value) == 1L && identical(value, "all")){
    return(as.character(available))
  }
  assert_known_ids(value, available, label, paste0("the `", label, "s` table"))
  as.character(value)
}
