#' Split Trips into Consecutive Parts
#'
#' Splits each selected trip into approximately equal consecutive parts. The
#' boundary stop is included at the end of one part and the start of the next,
#' producing valid complete stop sequences.
#'
#' @param gtfs A GTFS object.
#' @param trip Character vector of `trip_id` values.
#' @param split Positive integer number of split points. `split = 1` creates
#'   two parts. For each trip, the maximum is the number of stop-time records
#'   minus two, so every resulting part contains at least two stops.
#'
#' @return A modified `wizardgtfs` object.
#'
#' @details
#' New IDs use `.part1`, `.part2`, and so on. New straight-line shapes are
#' inferred from stop coordinates for the split parts. Frequency periods are
#' shifted by each part's offset from the original first departure. Trip-level
#' transfers are reassigned to the part containing their transfer stop.
#' Trips with fewer than three retained stop-time records cannot be split.
#'
#' @examples
#' gtfs_split <- split_trip(
#'   for_rail_gtfs,
#'   trip = for_rail_gtfs$trips$trip_id[1],
#'   split = 2
#' )
#'
#' @seealso [GTFSwizard::get_shapes()], [GTFSwizard::merge_gtfs()]
#' @export
split_trip <- function(gtfs, trip, split = 1L){
  gtfs <- ensure_wizardgtfs(gtfs)
  gw_assert_int(split, "split", lower = 1L)
  assert_known_ids(trip, gtfs$trips$trip_id, "trip", "`gtfs$trips`")
  parts_count <- split + 1L

  selected_times <- gtfs$stop_times[
    gtfs$stop_times$trip_id %in% trip, , drop = FALSE
  ]
  selected_times <- selected_times[
    order(selected_times$trip_id, selected_times$stop_sequence), , drop = FALSE
  ]
  counts <- table(selected_times$trip_id)
  if(any(counts < parts_count + 1L)){
    too_short <- names(counts)[counts < parts_count + 1L]
    limits <- pmax(0L, as.integer(counts[too_short]) - 2L)
    gw_stop(
      "`split` is too large for trip(s): ",
      paste0(too_short, " (maximum ", limits, ")", collapse = ", "), "."
    )
  }

  part_times <- list()
  dictionary <- list()
  offsets <- list()
  for(trip_id in trip){
    rows <- selected_times[selected_times$trip_id == trip_id, , drop = FALSE]
    boundaries <- round(seq(1, nrow(rows), length.out = parts_count + 1L))
    first_departure <- gtfs_time_to_seconds(rows$departure_time[1L])
    for(part in seq_len(parts_count)){
      index <- boundaries[part]:boundaries[part + 1L]
      section <- rows[index, , drop = FALSE]
      new_id <- paste0(trip_id, ".part", part)
      section$trip_id <- new_id
      section$stop_sequence <- seq_len(nrow(section))
      if("shape_dist_traveled" %in% names(section)){
        section$shape_dist_traveled <- as.numeric(section$shape_dist_traveled) -
          as.numeric(section$shape_dist_traveled[1L])
      }
      part_times[[length(part_times) + 1L]] <- section
      dictionary[[length(dictionary) + 1L]] <- data.frame(
        trip_id = trip_id, new_trip_id = new_id, part = part,
        first_stop_id = section$stop_id[1L],
        last_stop_id = section$stop_id[nrow(section)],
        stringsAsFactors = FALSE
      )
      offsets[[length(offsets) + 1L]] <- data.frame(
        trip_id = trip_id, new_trip_id = new_id,
        offset = gtfs_time_to_seconds(section$departure_time[1L]) -
          first_departure,
        stringsAsFactors = FALSE
      )
    }
  }
  dictionary <- dplyr::bind_rows(dictionary)
  offsets <- dplyr::bind_rows(offsets)

  gtfs$stop_times <- dplyr::bind_rows(
    gtfs$stop_times[!gtfs$stop_times$trip_id %in% trip, , drop = FALSE],
    dplyr::bind_rows(part_times)
  )

  original_trips <- gtfs$trips[gtfs$trips$trip_id %in% trip, , drop = FALSE]
  new_trips <- dplyr::left_join(dictionary, original_trips, by = "trip_id")
  new_trips$trip_id <- new_trips$new_trip_id
  new_trips$new_trip_id <- NULL
  new_trips$part <- NULL
  new_trips$first_stop_id <- NULL
  new_trips$last_stop_id <- NULL
  new_trips$shape_id <- paste0("shape-", new_trips$trip_id)
  gtfs$trips <- dplyr::bind_rows(
    gtfs$trips[!gtfs$trips$trip_id %in% trip, , drop = FALSE],
    new_trips
  )

  if(!is.null(gtfs$frequencies)){
    original_frequency <- gtfs$frequencies[
      gtfs$frequencies$trip_id %in% trip, , drop = FALSE
    ]
    if(nrow(original_frequency)){
      new_frequency <- dplyr::left_join(offsets, original_frequency, by = "trip_id")
      new_frequency$trip_id <- new_frequency$new_trip_id
      new_frequency$start_time <- seconds_to_gtfs_time(
        gtfs_time_to_seconds(new_frequency$start_time) + new_frequency$offset
      )
      new_frequency$end_time <- seconds_to_gtfs_time(
        gtfs_time_to_seconds(new_frequency$end_time) + new_frequency$offset
      )
      new_frequency$new_trip_id <- NULL
      new_frequency$offset <- NULL
      gtfs$frequencies <- dplyr::bind_rows(
        gtfs$frequencies[!gtfs$frequencies$trip_id %in% trip, , drop = FALSE],
        new_frequency
      )
    }
  }

  if(!is.null(gtfs$transfers)){
    gtfs$transfers <- reassign_split_transfers(
      gtfs$transfers, dictionary, "from_trip_id", "from_stop_id"
    )
    gtfs$transfers <- reassign_split_transfers(
      gtfs$transfers, dictionary, "to_trip_id", "to_stop_id"
    )
  }

  old_shapes <- gtfs$shapes
  gtfs$shapes <- NULL
  split_only <- prune_gtfs(gtfs, dictionary$new_trip_id)
  split_only <- get_shapes(split_only)
  referenced_old <- unique(gtfs$trips$shape_id[
    !gtfs$trips$trip_id %in% dictionary$new_trip_id
  ])
  if(!is.null(old_shapes)){
    old_shapes <- old_shapes[old_shapes$shape_id %in% referenced_old, , drop = FALSE]
  }
  gtfs$shapes <- dplyr::bind_rows(old_shapes, split_only$shapes)
  gtfs$dates_services <- NULL
  gtfs <- create_dates_services_table(gtfs)
  class(gtfs) <- c("wizardgtfs", "gtfs", "list")
  gtfs
}

reassign_split_transfers <- function(transfers, dictionary, trip_field, stop_field){
  if(!trip_field %in% names(transfers) || !stop_field %in% names(transfers)){
    return(transfers)
  }
  selected <- !is.na(transfers[[trip_field]]) &
    transfers[[trip_field]] %in% dictionary$trip_id
  if(!any(selected)){
    return(transfers)
  }
  for(i in which(selected)){
    candidates <- dictionary[
      dictionary$trip_id == transfers[[trip_field]][i], , drop = FALSE
    ]
    match <- candidates$new_trip_id[
      candidates$first_stop_id == transfers[[stop_field]][i] |
        candidates$last_stop_id == transfers[[stop_field]][i]
    ]
    if(length(match)){
      transfers[[trip_field]][i] <- match[1L]
    } else {
      gw_warn(
        "could not assign a trip-specific transfer at stop `",
        transfers[[stop_field]][i], "`; removing its `", trip_field, "` value."
      )
      transfers[[trip_field]][i] <- ""
    }
  }
  transfers
}
