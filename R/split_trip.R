#' Split a Trip into Sub-Trips within a GTFS Object
#'
#' `split_trip` divides a specified trip in a `wizardgtfs` object into multiple sub-trips by updating the stop sequences, trip identifiers, and related data, allowing for analysis or adjustments to different segments of the original trip.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If not, it will be converted.
#' @param trip A character vector specifying the `trip_id` to be split.
#' @param split An integer indicating the number of splits to apply. One split means two trip segments.
#'
#' @return A GTFS object with the specified trip split into new sub-trips.
#'
#' @details
#' - The function creates sub-trips by dividing the specified trip(s) into equal parts based on the stop sequence.
#'
#' - New trip IDs are generated for each sub-trip, and `stop_times`, `trips`, `frequencies`, and `transfers` tables are updated accordingly.
#'
#' - If `shape_dist_traveled` is present, it is adjusted to reflect distances within each new sub-trip.
#'
#' - After the split, the function re-generates the shapes table for the new trips using `get_shapes`, and merges it back into the `wizardgtfs` object.
#'
#' - Be aware: `get_shapes` reconstructs shapes using euclidean approximation and may not be accurate.
#'
#' - The maximum number of sections in a given trip is restricted by its amount of stops
#'
#' @note
#' `split_trip()` uses stop sequences to recriate the shapes table of split trips; accordingly, it should not be used after `filter_time()`, as this function removes invalid `stop_times`.
#'
#' @examples
#' # Split a trip into 3 segments
#' gtfs_split <- split_trip(for_rail_gtfs, trip = for_rail_gtfs$trips$trip_id[1:3], split = 2)
#'
#' @seealso
#' [GTFSwizard::get_shapes()], [GTFSwizard::merge_gtfs()]
#'
#' @importFrom dplyr mutate select group_by ungroup left_join bind_rows
#' @importFrom checkmate assert_int assert_subset
#' @importFrom crayon cyan
#' @export
split_trip <- function(gtfs, trip, split = 1){

  # checa os argumentos --------------------------------------------  -----------------------------
  if(!"wizardgtfs" %in% class(gtfs)){
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
    message('This gtfs object is not of the ', crayon::cyan('wizardgtfs'), ' class. Computation may take longer. Using ', crayon::cyan('as_gtfswizard()'), ' is advised.')
  }

  checkmate::assert_int(split)

  checkmate::assert_subset(trip, choices = gtfs$trips$trip_id)

  # identifica trips -------------------------------------------------------------------------
  groups <- split + 1

  split_data <-
    gtfs$stop_times %>%
    dplyr::mutate(split = trip_id %in% trip) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::mutate(subtrip = if_else(split == TRUE, ceiling(1:n()/n() * groups), NA) |> forcats::as_factor() |> as.numeric(),
           dupe = split == TRUE & !subtrip == lead(subtrip)) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(dplyr::slice(., .$dupe %>% which()) %>% mutate(subtrip = subtrip + 1)) %>%
    group_by(trip_id, subtrip) %>%
    mutate(n = n()) %>%
    filter(!n == 1) %>%
    ungroup() %>%
    select(-n)

  trip.dic <-
    split_data %>%
    dplyr::select(trip_id, subtrip) %>%
    na.omit() %>%
    dplyr::distinct() %>%
    group_by(trip_id) %>%
    dplyr::mutate(new.trip_id = 1:n())  %>%
    dplyr::mutate(new.trip_id = paste0(trip_id, '.', LETTERS[new.trip_id]))

  # stop times ----------------------------------------------------------------------------------
  gtfs$stop_times <-
    split_data %>%
    left_join(trip.dic, by = c('trip_id', 'subtrip')) %>%
    mutate(trip_id = if_else(is.na(new.trip_id), trip_id, new.trip_id)) %>%
    select(-subtrip, -dupe, -new.trip_id)

  if (!purrr::is_null(gtfs$stop_times$shape_dist_traveled)) {

    gtfs$stop_times <-
      gtfs$stop_times %>%
      dplyr::mutate(shape_dist_traveled = if_else(split, shape_dist_traveled - shape_dist_traveled[1], shape_dist_traveled))

      }

  gtfs$stop_times <- dplyr::select(gtfs$stop_times, -split)

  # trips --------------------------------------------------------------------------------------
  gtfs$trips <-
    dplyr::left_join(gtfs$trips, trip.dic, by = 'trip_id') %>%
    dplyr::mutate(trip_id = if_else(is.na(new.trip_id), trip_id, new.trip_id),
                  shape_id = if_else(is.na(new.trip_id), shape_id, paste0('shape-', new.trip_id))) %>%
    dplyr::select(-new.trip_id, -subtrip)

  # frequencies ---------------------------------------------------------------------------------
  if (!purrr::is_null(gtfs$frequencies$trip_id)) {

    gtfs$frequencies <-
      dplyr::left_join(gtfs$frequencies, trip.dic, by = 'trip_id') %>%
      dplyr::mutate(trip_id = if_else(is.na(new.trip_id), trip_id, new.trip_id)) %>%
      dplyr::select(-new.trip_id, -subtrip)

  }

  # transfers -----------------------------------------------------------------------------------
  if (!purrr::is_null(gtfs$transfers$trip_id)) {

    gtfs$transfers <-
      dplyr::left_join(gtfs$transfers, trip.dic, by = 'trip_id') %>%
      dplyr::mutate(trip_id = if_else(is.na(new.trip_id), trip_id, new.trip_id)) %>%
      dplyr::select(-new.trip_id)

  }

  # corrigindo shapes ---------------------------------------------------------------------------
  if(all(gtfs$trips$trip_id %in% trip.dic$new.trip_id)) {

    gtfs <- GTFSwizard::get_shapes(gtfs)

  } else {
    gtfs.x <-
      GTFSwizard::filter_trip(gtfs, trip.dic$new.trip_id, keep = FALSE)

    gtfs.y <-
      GTFSwizard::filter_trip(gtfs, trip.dic$new.trip_id, keep = TRUE) %>%
      GTFSwizard::get_shapes()

    gtfs <- GTFSwizard::merge_gtfs(gtfs.x, gtfs.y, suffix = TRUE)
  }

  # retornando gtfs -----------------------------------------------------------------------------
  return(gtfs)

}
