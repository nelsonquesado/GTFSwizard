#' Convert a GTFS Feed to `wizardgtfs`
#'
#' Converts a list-like GTFS feed to the class used by GTFSwizard. Character
#' GTFS dates are converted to [Date] values, a date-to-service lookup is
#' created, and missing shapes can optionally be inferred from stop locations.
#'
#' @param gtfs_list A named list of GTFS tables or a `tidygtfs` object.
#' @param build_shapes Logical. If `TRUE`, infer `shapes` when the table is
#'   absent. Inferred shapes connect stops with straight line segments and are
#'   intended for analysis and visualization, not map-matched routing.
#'
#' @return A `wizardgtfs` object.
#'
#' @details
#' The input is checked for required tables, required fields, primary-key
#' duplication, foreign-key consistency, increasing stop and shape sequences,
#' valid service dates, and valid GTFS time strings. Times remain character
#' values because GTFS permits hours greater than 24.
#'
#' @examples
#' gtfs_wizard <- as_wizardgtfs(for_rail_gtfs, build_shapes = TRUE)
#'
#' @seealso [GTFSwizard::create_gtfs()], [GTFSwizard::get_shapes()]
#' @export
as_wizardgtfs <- function(gtfs_list, build_shapes = TRUE){
  UseMethod("as_wizardgtfs")
}

#' @exportS3Method GTFSwizard::as_wizardgtfs tidygtfs
as_wizardgtfs.tidygtfs <- function(gtfs_list, build_shapes = TRUE){
  gw_assert_flag(build_shapes, "build_shapes")

  if(!is.null(gtfs_list$.) && !is.null(gtfs_list$.$dates_services)){
    dates_services <- gtfs_list$.$dates_services
    gtfs_list <- gtfs_list[names(gtfs_list) != "."]
    gtfs_list$dates_services <- dates_services |>
      dplyr::mutate(date = as.Date(date)) |>
      dplyr::group_by(date) |>
      dplyr::summarise(service_id = list(unique(service_id)), .groups = "drop")
  } else {
    gtfs_list <- gtfs_list[names(gtfs_list) != "."]
  }

  as_wizardgtfs.list(gtfs_list, build_shapes = build_shapes)
}

#' @exportS3Method GTFSwizard::as_wizardgtfs list
as_wizardgtfs.list <- function(gtfs_list, build_shapes = TRUE){
  gw_assert_flag(build_shapes, "build_shapes")
  if(is.null(names(gtfs_list)) || any(!nzchar(names(gtfs_list)))){
    gw_stop("`gtfs_list` must be a named list of GTFS tables.")
  }

  gtfs_obj <- convert_to_tibble(gtfs_list)
  gtfs_obj$dates_services <- NULL
  gtfs_obj <- drop_stop_times_missing_stops(gtfs_obj)
  gtfs_obj <- drop_short_stop_time_trips(gtfs_obj)
  validate_gtfs_tables(gtfs_obj)
  gtfs_obj <- convert_times_and_dates(gtfs_obj)
  gtfs_obj <- create_dates_services_table(gtfs_obj)
  class(gtfs_obj) <- c("wizardgtfs", "gtfs", "list")

  if(build_shapes && is.null(gtfs_obj$shapes)){
    gtfs_obj <- get_shapes(gtfs_obj)
  }
  gtfs_obj
}

convert_to_tibble <- function(x){
  lapply(x, tibble::as_tibble)
}

convert_times_and_dates <- function(gtfs_list){
  if("calendar" %in% names(gtfs_list)){
    gtfs_list[["calendar"]]$start_date <- parse_gtfs_date(gtfs_list[["calendar"]]$start_date)
    gtfs_list[["calendar"]]$end_date <- parse_gtfs_date(gtfs_list[["calendar"]]$end_date)
  }
  if("calendar_dates" %in% names(gtfs_list)){
    gtfs_list[["calendar_dates"]]$date <- parse_gtfs_date(gtfs_list[["calendar_dates"]]$date)
  }
  gtfs_list
}
