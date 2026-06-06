#' Scale Dwell Times
#'
#' Multiplies dwell time at selected trip-stop calls and propagates each change
#' to all later times in the same trip. Arrival at the edited stop is retained;
#' departure and subsequent calls move by the dwell-time difference.
#'
#' @param gtfs A GTFS object.
#' @param trips,stops Character ID vectors or `"all"`.
#' @param factor One non-negative numeric multiplier.
#'
#' @return A modified `wizardgtfs` object.
#'
#' @examples
#' edited <- edit_dwelltime(
#'   for_rail_gtfs,
#'   trips = for_rail_gtfs$trips$trip_id[1:2],
#'   stops = for_rail_gtfs$stops$stop_id[1:2],
#'   factor = 1.5
#' )
#'
#' @seealso [GTFSwizard::set_dwelltime()], [GTFSwizard::get_dwelltimes()]
#' @export
edit_dwelltime <- function(gtfs, trips = "all", stops = "all", factor){
  if(!is.numeric(factor) || length(factor) != 1L || is.na(factor) || factor < 0){
    gw_stop("`factor` must be one non-negative numeric value.")
  }
  update_dwell_times(gtfs, trips, stops, function(x) round(x * factor))
}
