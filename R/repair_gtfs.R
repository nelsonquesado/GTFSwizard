#' Explicitly Repair a GTFS Feed
#'
#' Apply selected repairs and record every changed cell or removed row.
#' @param gtfs A named list of GTFS tables or a `wizardgtfs` object.
#' @param repairs Character vector containing `"references"`, `"overnight"`,
#'   and/or `"stop_times"`. Reference repair uses the legacy import cleanup.
#'   Overnight repair adds days only to backward jumps greater than 12 hours.
#'   Stop-time repair interpolates between known times using straight distances;
#'   no extrapolation is performed. Defaults to reference repair only.
#' @param build_shapes Logical. Infer missing shapes after repair.
#' @return A validated `wizardgtfs`. Its `repair_report` attribute is a tibble
#'   with table, original row, field, old value, new value, and action. Read it
#'   with [repair_report()]. The input remains unchanged.
#' @details Repairs are estimates. Review their report before exporting.
#'   Interpolation preserves known times and dwell, and sets `timepoint = 0`
#'   for estimated calls when that field exists. Missing endpoint times and
#'   intervals without usable coordinates remain missing. Overnight adjustments
#'   precede interpolation when both are requested.
#' @examples
#' fixed <- repair_gtfs(for_rail_gtfs, repairs = "references")
#' repair_report(fixed)
#' @export
repair_gtfs <- function(gtfs, repairs = "references", build_shapes = TRUE){
  if(!is.character(repairs) || anyNA(repairs) ||
     any(!repairs %in% c("references", "overnight", "stop_times"))){
    gw_stop("`repairs` must contain references, overnight, and/or stop_times.")
  }
  tables <- lapply(gtfs, tibble::as_tibble)
  tables$dates_services <- NULL
  original <- tables
  marker <- repair_row_field(tables)
  tables <- mark_repair_rows(tables, marker)
  if("references" %in% repairs){
    tables <- drop_stop_times_missing_stops(tables)
    tables <- drop_short_stop_time_trips(tables)
  }
  if(any(c("overnight", "stop_times") %in% repairs)){
    calls <- tables$stop_times
    needed <- c("trip_id", "stop_id", "stop_sequence", "arrival_time", "departure_time")
    if(!all(needed %in% names(calls))) gw_stop("stop-time repair requires trip, stop, sequence, arrival and departure fields.")
    for(rows in split(seq_len(nrow(calls)), calls$trip_id)){
      rows <- rows[order(calls$stop_sequence[rows])]
      arrival <- gtfs_time_to_seconds(calls$arrival_time[rows])
      departure <- gtfs_time_to_seconds(calls$departure_time[rows])
      if("overnight" %in% repairs){
        values <- as.vector(rbind(arrival, departure))
        previous <- NA_real_
        for(i in seq_along(values)){
          if(!is.finite(values[i])) next
          value <- values[i]
          if(is.finite(previous) && previous - value > 43200){
            days <- ceiling((previous - value) / 86400)
            value <- value + days * 86400
          }
          values[i] <- value
          previous <- value
        }
        arrival <- values[seq(1, length(values), 2)]
        departure <- values[seq(2, length(values), 2)]
      }
      estimated <- rep(FALSE, length(rows))
      if("stop_times" %in% repairs){
        blank_a <- is.na(calls$arrival_time[rows]) | trimws(calls$arrival_time[rows]) == ""
        blank_d <- is.na(calls$departure_time[rows]) | trimws(calls$departure_time[rows]) == ""
        one_a <- blank_a & is.finite(departure)
        one_d <- blank_d & is.finite(arrival)
        arrival[one_a] <- departure[one_a]
        departure[one_d] <- arrival[one_d]
        coords <- tables$stops[match(calls$stop_id[rows], tables$stops$stop_id), ]
        known <- which(is.finite(arrival) & is.finite(departure))
        if(length(known) >= 2L){
          for(j in seq_len(length(known) - 1L)){
            left <- known[j]; right <- known[j + 1L]
            if(right <= left + 1L || arrival[right] < departure[left]) next
            inner <- seq.int(left + 1L, right - 1L)
            if(!all(blank_a[inner] & blank_d[inner])) next
            segment <- left:right
            distances <- haversine_m(utils::head(coords$stop_lon[segment], -1), utils::head(coords$stop_lat[segment], -1),
                                       utils::tail(coords$stop_lon[segment], -1), utils::tail(coords$stop_lat[segment], -1))
            if(any(!is.finite(distances))) next
            position <- c(0, cumsum(distances))
            weight <- if(utils::tail(position, 1) > 0) position / utils::tail(position, 1) else seq(0, 1, length.out = length(segment))
            value <- round(departure[left] + weight[-c(1, length(weight))] * (arrival[right] - departure[left]))
            arrival[inner] <- departure[inner] <- value
            estimated[inner] <- TRUE
          }
        }
      }
      for(field in c("arrival_time", "departure_time")){
        value <- if(field == "arrival_time") arrival else departure
        # Preserve malformed and blank strings unless actually repaired.
        changed <- is.finite(value) & (is.na(gtfs_time_to_seconds(calls[[field]][rows])) |
                                       value != gtfs_time_to_seconds(calls[[field]][rows]))
        calls[[field]][rows[changed]] <- seconds_to_gtfs_time(value[changed])
      }
      if("timepoint" %in% names(calls)) calls$timepoint[rows[estimated]] <- 0L
    }
    tables$stop_times <- calls
  }
  report <- gtfs_repair_diff(original, tables, marker)
  tables <- clear_repair_rows(tables, marker)
  result <- as_wizardgtfs(tables, build_shapes = build_shapes, repair = "none")
  attr(result, "repair_report") <- report
  result
}

#' Inspect Changes Made by Feed Repair
#' @param gtfs A feed returned by [repair_gtfs()], [read_gtfs()], or [as_wizardgtfs()].
#' @return A tibble of recorded changes; empty when none were recorded.
#' @export
repair_report <- function(gtfs){
  report <- attr(gtfs, "repair_report", exact = TRUE)
  if(is.null(report)) empty_repair_report() else report
}

empty_repair_report <- function(){
  tibble::tibble(table = character(), row = integer(), field = character(),
                 old_value = character(), new_value = character(), action = character())
}

repair_row_field <- function(tables){
  fields <- unique(unlist(lapply(tables, names), use.names = FALSE))
  utils::tail(make.unique(c(fields, ".wizard_original_row")), 1L)
}

mark_repair_rows <- function(tables, marker){
  lapply(tables, function(x){ x[[marker]] <- seq_len(nrow(x)); x })
}

clear_repair_rows <- function(tables, marker){
  lapply(tables, function(x){ x[[marker]] <- NULL; x })
}

gtfs_repair_diff <- function(before, after, marker){
  changes <- list()
  add <- function(table, row, field, old, new, action){
    changes[[length(changes) + 1L]] <<- tibble::tibble(table = table, row = as.integer(row),
      field = field, old_value = as.character(old), new_value = as.character(new), action = action)
  }
  for(table in union(names(before), names(after))){
    old <- before[[table]]; new <- after[[table]]
    original_row <- new[[marker]]
    new[[marker]] <- NULL
    if(identical(old, new)) next
    matched <- match(seq_len(nrow(old)), original_row)
    row_text <- function(x, i) paste(utils::capture.output(dput(as.list(x[i, , drop = FALSE]))), collapse = " ")
    for(i in which(is.na(matched))) add(table, i, NA_character_, row_text(old, i), NA_character_, "remove_row")
    retained <- which(!is.na(matched))
    for(field in union(names(old), names(new))){
      a <- if(field %in% names(old)) as.character(old[[field]][retained]) else rep(NA_character_, length(retained))
      b <- if(field %in% names(new)) as.character(new[[field]][matched[retained]]) else rep(NA_character_, length(retained))
      changed <- which((is.na(a) != is.na(b)) | (!is.na(a) & !is.na(b) & a != b))
      if(length(changed)) add(table, retained[changed], field, a[changed], b[changed], "update_cell")
    }
    for(i in which(is.na(original_row))) add(table, NA_integer_, NA_character_, NA_character_,
                                           row_text(new, i), "add_row")
  }
  if(!length(changes)) empty_repair_report() else dplyr::bind_rows(changes)
}
