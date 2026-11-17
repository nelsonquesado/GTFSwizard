# Prepare private routing tables for a single tidy_raptor() query.
prepare_raptor <- function(gtfs, min_departure = "0:0:0",
                           max_arrival = "23:59:59", dates = NULL, filter = TRUE){
  require_pkg("data.table", "preparing RAPTOR data")
  require_pkg("hms", "preparing RAPTOR data")
  gw_assert_flag(filter, "filter")
  bounds <- gtfs_time_to_seconds(c(min_departure, max_arrival))
  if(length(bounds) != 2L || anyNA(bounds) || bounds[1] > bounds[2]){
    gw_stop("supply valid, ordered GTFS time bounds.")
  }
  if(!inherits(gtfs, "wizardgtfs")) gtfs <- as_wizardgtfs(gtfs, build_shapes = FALSE)
  gtfs <- ensure_wizardgtfs(gtfs)
  trips <- gtfs$trips
  if(filter){
    available <- as.Date(gtfs$dates_services$date)
    if(!length(available)) gw_stop("the feed has no active service dates.")
    if(is.null(dates)) dates <- max(available)
    dates <- parse_gtfs_date(dates)
    if(length(dates) != 1L || is.na(dates) || !dates %in% available){
      gw_stop("`dates` must contain exactly one active service date.")
    }
    services <- unlist(gtfs$dates_services$service_id[available == dates], use.names = FALSE)
    trips <- trips[trips$service_id %in% services, , drop = FALSE]
  }
  fields <- intersect(c("trip_id", "stop_id", "stop_sequence", "arrival_time",
                         "departure_time", "pickup_type", "drop_off_type",
                         "start_pickup_drop_off_window", "end_pickup_drop_off_window"),
                       names(gtfs$stop_times))
  calls <- gtfs$stop_times[gtfs$stop_times$trip_id %in% trips$trip_id, fields, drop = FALSE]
  calls$arrival_time_num <- gtfs_time_to_seconds(calls$arrival_time)
  calls$departure_time_num <- gtfs_time_to_seconds(calls$departure_time)
  freq <- gtfs$frequencies
  if(!is.null(freq)){
    freq$.index <- seq_len(nrow(freq))
    freq <- freq[freq$trip_id %in% trips$trip_id, , drop = FALSE]
  }
  transfers <- gtfs$transfers
  if(!is.null(freq) && nrow(freq)){
    ordered <- calls[calls$trip_id %in% freq$trip_id, , drop = FALSE]
    ordered <- ordered[order(ordered$trip_id, ordered$stop_sequence), , drop = FALSE]
    first <- ordered[!duplicated(ordered$trip_id), c("trip_id", "departure_time_num")]
    # Reject instances entirely outside the requested bounds before the large
    # stop-call join. Extents include both arrival and departure times.
    extents <- lapply(split(ordered, ordered$trip_id), function(x){
      values <- c(x$arrival_time_num, x$departure_time_num)
      values <- values[is.finite(values)]
      if(length(values)) range(values) else c(NA_real_, NA_real_)
    })
    starts <- gtfs_time_to_seconds(freq$start_time)
    ends <- gtfs_time_to_seconds(freq$end_time)
    parts <- lapply(seq_len(nrow(freq)), function(i){
      id <- as.character(freq$trip_id[i])
      base <- first$departure_time_num[match(id, first$trip_id)]
      times <- seq(starts[i], ends[i] - 1, by = as.numeric(freq$headway_secs[i]))
      index <- seq_along(times)
      if(filter && is.finite(base) && all(is.finite(extents[[id]]))){
        range <- extents[[id]] - base
        keep <- times + range[2] >= bounds[1] & times + range[1] <= bounds[2]
        times <- times[keep]
        index <- index[keep]
      }
      tibble::tibble(trip_id = rep(id, length(times)),
                     instance_id = paste0(id, ".frequency", freq$.index[i], ".", index, recycle0 = TRUE),
                     offset = times - base)
    })
    mapping <- dplyr::bind_rows(parts)
    mapping$new_trip_id <- utils::tail(make.unique(c(as.character(gtfs$trips$trip_id),
                                                     mapping$instance_id)), nrow(mapping))
    expanded <- dplyr::inner_join(calls, mapping, by = "trip_id",
                                  relationship = "many-to-many")
    expanded$arrival_time_num <- expanded$arrival_time_num + expanded$offset
    expanded$departure_time_num <- expanded$departure_time_num + expanded$offset
    expanded$trip_id <- expanded$new_trip_id
    expanded <- expanded[, names(calls), drop = FALSE]
    calls <- dplyr::bind_rows(calls[!calls$trip_id %in% freq$trip_id, ], expanded)
    if(!is.null(transfers)){
      for(field in intersect(c("from_trip_id", "to_trip_id"), names(transfers))){
        dictionary <- mapping[, c("trip_id", "new_trip_id")]
        names(dictionary)[1] <- field
        transfers <- dplyr::left_join(transfers, dictionary, by = field,
                                      relationship = "many-to-many")
        matched <- !is.na(transfers$new_trip_id)
        transfers[[field]][matched] <- transfers$new_trip_id[matched]
        transfers$new_trip_id <- NULL
      }
    }
  }
  if(filter){
    keep <- (!is.na(calls$arrival_time_num) & calls$arrival_time_num >= bounds[1] &
               calls$arrival_time_num <= bounds[2]) |
      (!is.na(calls$departure_time_num) & calls$departure_time_num >= bounds[1] &
         calls$departure_time_num <= bounds[2])
    calls <- calls[keep, , drop = FALSE]
  }
  if(!nrow(calls)) gw_stop("no stop calls remain in the routing date/time selection.")
  if(is.null(transfers)){
    transfers <- data.frame(from_stop_id = character(), to_stop_id = character(),
                            transfer_type = integer(), min_transfer_time = integer())
  } else if(filter){
    stop_ids <- filter_stops_with_parents(gtfs$stops, unique(calls$stop_id))$stop_id
    keep <- transfers$from_stop_id %in% stop_ids & transfers$to_stop_id %in% stop_ids
    for(field in intersect(c("from_trip_id", "to_trip_id"), names(transfers))){
      keep <- keep & (is.na(transfers[[field]]) | transfers[[field]] == "" |
                        transfers[[field]] %in% calls$trip_id)
    }
    active_ids <- calls$trip_id
    if(exists("mapping", inherits = FALSE)){
      active_ids <- c(active_ids, mapping$trip_id[mapping$new_trip_id %in% active_ids])
    }
    routes <- trips$route_id[trips$trip_id %in% active_ids]
    for(field in intersect(c("from_route_id", "to_route_id"), names(transfers))){
      keep <- keep & (is.na(transfers[[field]]) | transfers[[field]] == "" |
                        transfers[[field]] %in% routes)
    }
    transfers <- transfers[keep, , drop = FALSE]
  }
  calls$arrival_time <- hms::as_hms(calls$arrival_time_num)
  calls$departure_time <- hms::as_hms(calls$departure_time_num)
  list(stop_times = data.table::as.data.table(calls),
       transfers = data.table::as.data.table(transfers),
       stop_ids = as.character(gtfs$stops$stop_id))
}
