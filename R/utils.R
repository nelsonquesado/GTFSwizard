
gw_msg <- function(...){
  message("GTFSwizard: ", paste0(..., collapse = ""))
}

gw_warn <- function(...){
  warning("GTFSwizard: ", paste0(..., collapse = ""), call. = FALSE)
}

gw_stop <- function(...){
  stop("GTFSwizard: ", paste0(..., collapse = ""), call. = FALSE)
}

gw_assert_flag <- function(x, name){
  if(!is.logical(x) || length(x) != 1L || is.na(x)){
    gw_stop("`", name, "` must be `TRUE` or `FALSE`.")
  }
  invisible(TRUE)
}

gw_assert_int <- function(x, name, lower = -Inf){
  if(!is.numeric(x) || length(x) != 1L || is.na(x) ||
     !is.finite(x) || x != as.integer(x) || x < lower){
    qualifier <- if(is.finite(lower)){
      paste0(" greater than or equal to ", as.integer(lower))
    } else {
      ""
    }
    gw_stop("`", name, "` must be one integer", qualifier, ".")
  }
  invisible(TRUE)
}

gw_warn_invalid_method <- function(method, choices, default){
  gw_warn(
    "`method` must be one of ",
    paste(sprintf("`%s`", choices), collapse = ", "),
    ". Using `method = \"", default, "\"`."
  )
}

normalize_method <- function(method, choices, default){
  if(!is.character(method) || length(method) != 1L || is.na(method)){
    gw_warn_invalid_method(method, choices, default)
    return(default)
  }
  normalized <- gsub(".", "_", method, fixed = TRUE)
  if(!normalized %in% choices){
    gw_warn_invalid_method(method, choices, default)
    return(default)
  }
  normalized
}

resolve_legacy_argument <- function(value, was_missing, dots, legacy, modern){
  dot_names <- names(dots)
  if(is.null(dot_names)){
    dot_names <- rep("", length(dots))
  }
  matched <- which(dot_names == legacy)
  if(length(matched) > 1L){
    gw_stop("`", legacy, "` was supplied more than once.")
  }
  if(length(matched) == 1L){
    if(!was_missing){
      gw_stop("supply only `", modern, "`, not both `", modern,
              "` and legacy `", legacy, "`.")
    }
    value <- dots[[matched]]
    dots <- dots[-matched]
  }
  list(value = value, dots = dots)
}

gw_check_unused_dots <- function(dots){
  if(!length(dots)){
    return(invisible(TRUE))
  }
  dot_names <- names(dots)
  if(is.null(dot_names) || any(!nzchar(dot_names))){
    gw_stop("all arguments in `...` must be named.")
  }
  gw_stop("unused argument(s): ",
          paste(sprintf("`%s`", dot_names), collapse = ", "), ".")
}

direction_field <- function(data){
  if("direction_id" %in% names(data)) "direction_id" else character()
}

ensure_wizardgtfs <- function(gtfs){
  if(!inherits(gtfs, "wizardgtfs")){
    gw_msg("converting input to `wizardgtfs` with `as_wizardgtfs()`.")
    gtfs <- GTFSwizard::as_wizardgtfs(gtfs)
  }
  if(!inherits(gtfs, "wizardgtfs")){
    gw_stop("could not convert the input to a `wizardgtfs` object.")
  }
  if(is.null(gtfs$dates_services) ||
     !inherits(gtfs$dates_services$date, "Date")){
    gtfs <- convert_times_and_dates(gtfs)
    gtfs <- create_dates_services_table(gtfs)
    class(gtfs) <- c("wizardgtfs", "gtfs", "list")
  }
  gtfs
}

ensure_shapes <- function(gtfs){
  if(is.null(gtfs$shapes)){
    gw_warn("input has no `shapes` table. Building one with `get_shapes()`.")
    gtfs <- GTFSwizard::get_shapes(gtfs)
  }
  gtfs
}

gtfs_time_to_seconds <- function(x){
  x <- as.character(x)
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x) & nzchar(x)
  if(!any(ok)){
    return(out)
  }
  values <- x[ok]
  target <- which(ok)
  width <- nchar(values)
  standard <- width >= 7L &
    substr(values, width - 5L, width - 5L) == ":" &
    substr(values, width - 2L, width - 2L) == ":"

  if(any(standard)){
    value <- values[standard]
    value_width <- width[standard]
    hour <- suppressWarnings(as.numeric(
      substr(value, 1L, value_width - 6L)
    ))
    minute <- suppressWarnings(as.numeric(
      substr(value, value_width - 4L, value_width - 3L)
    ))
    second <- suppressWarnings(as.numeric(
      substr(value, value_width - 1L, value_width)
    ))
    valid <- is.finite(hour) & is.finite(minute) & is.finite(second) &
      hour >= 0 & minute >= 0 & minute < 60 & second >= 0 & second < 60
    rows <- target[standard]
    out[rows[valid]] <- hour[valid] * 3600 +
      minute[valid] * 60 + second[valid]
  }

  if(any(!standard)){
    parts <- strsplit(values[!standard], ":", fixed = TRUE)
    valid <- lengths(parts) == 3L
    parsed <- rep(NA_real_, length(parts))
    if(any(valid)){
      vals <- suppressWarnings(do.call(
        rbind, lapply(parts[valid], as.numeric)
      ))
      valid_values <- is.finite(vals[, 1]) &
        is.finite(vals[, 2]) & is.finite(vals[, 3]) &
        vals[, 1] >= 0 & vals[, 2] >= 0 & vals[, 2] < 60 &
        vals[, 3] >= 0 & vals[, 3] < 60
      valid_rows <- which(valid)
      parsed[valid_rows[valid_values]] <- vals[valid_values, 1] * 3600 +
        vals[valid_values, 2] * 60 + vals[valid_values, 3]
    }
    out[target[!standard]] <- parsed
  }
  out
}

haversine_m <- function(lon1, lat1, lon2, lat2){
  radius <- 6371008.8
  to_rad <- pi / 180
  lon1 <- as.numeric(lon1) * to_rad
  lat1 <- as.numeric(lat1) * to_rad
  lon2 <- as.numeric(lon2) * to_rad
  lat2 <- as.numeric(lat2) * to_rad
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  radius * 2 * atan2(sqrt(a), sqrt(1 - a))
}

seconds_to_gtfs_time <- function(x){
  x <- round(as.numeric(x))
  valid <- !is.na(x) & is.finite(x) & x >= 0
  out <- rep("", length(x))
  out[valid] <- sprintf(
    "%02d:%02d:%02d",
    floor(x[valid] / 3600),
    floor((x[valid] %% 3600) / 60),
    x[valid] %% 60
  )
  out
}

parse_gtfs_date <- function(x){
  if(inherits(x, "Date")){
    return(x)
  }
  if(inherits(x, c("POSIXct", "POSIXlt"))){
    return(as.Date(x))
  }
  value <- as.character(x)
  compact <- grepl("^[0-9]{8}$", value)
  out <- as.Date(rep(NA_character_, length(value)))
  out[compact] <- as.Date(value[compact], format = "%Y%m%d")
  out[!compact] <- as.Date(value[!compact])
  out
}

format_gtfs_date <- function(x){
  format(parse_gtfs_date(x), "%Y%m%d")
}

is_valid_gtfs_time <- function(x, allow_blank = TRUE){
  value <- as.character(x)
  blank <- is.na(value) | !nzchar(value)
  parsed <- gtfs_time_to_seconds(value)
  (allow_blank & blank) | (!blank & !is.na(parsed))
}

create_dates_services_table <- function(gtfs_list){
  rows <- list()
  calendar <- gtfs_list[["calendar"]]
  calendar_dates <- gtfs_list[["calendar_dates"]]
  if(!is.null(calendar) && nrow(calendar)){
    weekdays <- c(
      "monday", "tuesday", "wednesday", "thursday",
      "friday", "saturday", "sunday"
    )
    for(i in seq_len(nrow(calendar))){
      dates <- seq(calendar$start_date[i], calendar$end_date[i], by = "day")
      active <- as.integer(format(dates, "%u"))
      flags <- as.integer(unlist(calendar[i, weekdays], use.names = FALSE))
      dates <- dates[flags[active] == 1L]
      if(length(dates)){
        rows[[length(rows) + 1L]] <- data.frame(
          date = dates,
          service_id = as.character(calendar$service_id[i]),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  services <- if(length(rows)){
    dplyr::bind_rows(rows)
  } else {
    tibble::tibble(date = as.Date(character()), service_id = character())
  }

  if(!is.null(calendar_dates) && nrow(calendar_dates)){
    exceptions <- calendar_dates
    additions <- exceptions[exceptions$exception_type == 1, c("date", "service_id")]
    removals <- exceptions[exceptions$exception_type == 2, c("date", "service_id")]
    services <- dplyr::bind_rows(services, additions)
    if(nrow(removals)){
      key <- paste(services$date, services$service_id, sep = "\r")
      remove_key <- paste(removals$date, removals$service_id, sep = "\r")
      services <- services[!key %in% remove_key, , drop = FALSE]
    }
  }

  services <- unique(services)
  gtfs_list$dates_services <- services |>
    dplyr::arrange(date, service_id) |>
    dplyr::group_by(date) |>
    dplyr::summarise(service_id = list(service_id), .groups = "drop")
  gtfs_list
}

service_pattern_date_table <- function(gtfs){
  dates_services <- gtfs$dates_services
  if(is.null(dates_services) || !nrow(dates_services)){
    return(tibble::tibble(
      date = as.Date(character()),
      service_ids = list(),
      service_pattern = character(),
      pattern_frequency = integer()
    ))
  }

  active_dates <- dates_services |>
    dplyr::mutate(
      service_ids = lapply(
        .data$service_id,
        function(x) sort(unique(as.character(x)))
      ),
      .signature = vapply(
        .data$service_ids, paste, collapse = "|", FUN.VALUE = character(1)
      )
    ) |>
    dplyr::select("date", "service_ids", ".signature")
  active_patterns <- active_dates |>
    dplyr::count(.data$.signature, sort = TRUE, name = "pattern_frequency") |>
    dplyr::mutate(
      service_pattern = paste0("servicepattern-", dplyr::row_number())
    )
  all_dates <- tibble::tibble(
    date = seq(min(dates_services$date), max(dates_services$date), by = "day")
  )
  data <- dplyr::left_join(all_dates, active_dates, by = "date")
  no_service <- is.na(data$.signature)
  data$service_ids[no_service] <- rep(list(character()), sum(no_service))
  data$.signature[no_service] <- ""
  data <- dplyr::left_join(data, active_patterns, by = ".signature")
  data$service_pattern[no_service] <- "No service"
  data$pattern_frequency[no_service] <- sum(no_service)
  data |>
    dplyr::select("date", "service_ids", "service_pattern", "pattern_frequency")
}

service_patterns_for_services <- function(gtfs, include_no_service = FALSE){
  service_dates <- tidyr::unnest(gtfs$dates_services, cols = "service_id") |>
    dplyr::arrange(.data$service_id, .data$date) |>
    dplyr::group_by(.data$service_id) |>
    dplyr::summarise(
      .signature = paste(as.character(.data$date), collapse = "|"),
      pattern_frequency = dplyr::n(),
      .groups = "drop"
    )
  patterns <- service_dates |>
    dplyr::distinct(.data$.signature, .data$pattern_frequency) |>
    dplyr::arrange(dplyr::desc(.data$pattern_frequency), .data$.signature) |>
    dplyr::mutate(
      service_pattern = paste0("servicepattern-", dplyr::row_number())
    )
  service_patterns <- service_dates |>
    dplyr::left_join(patterns, by = c(".signature", "pattern_frequency")) |>
    dplyr::arrange(.data$service_pattern, .data$service_id) |>
    dplyr::select("service_id", "service_pattern", "pattern_frequency")
  if(!include_no_service){
    return(service_patterns)
  }

  no_service_frequency <- sum(
    service_pattern_date_table(gtfs)$service_pattern == "No service"
  )
  if(no_service_frequency > 0L){
    service_patterns <- dplyr::bind_rows(
      service_patterns,
      tibble::tibble(
        service_id = NA_character_,
        service_pattern = "No service",
        pattern_frequency = no_service_frequency
      )
    )
  }
  service_patterns
}

drop_short_stop_time_trips <- function(tables){
  if(!all(c("trips", "stop_times") %in% names(tables)) ||
     !"trip_id" %in% names(tables$trips) ||
     !"trip_id" %in% names(tables$stop_times)){
    return(tables)
  }
  stop_counts <- table(as.character(tables$stop_times$trip_id))
  trip_ids <- as.character(tables$trips$trip_id)
  trip_counts <- setNames(integer(length(trip_ids)), trip_ids)
  matched <- intersect(names(stop_counts), trip_ids)
  trip_counts[matched] <- as.integer(stop_counts[matched])
  invalid_trips <- names(trip_counts)[trip_counts < 2L]
  if(!length(invalid_trips)){
    return(tables)
  }
  if(length(invalid_trips) == length(trip_ids)){
    gw_stop("all trips have fewer than two stop-time records.")
  }
  gw_warn(
    "removed ", length(invalid_trips),
    " trip(s) with fewer than two stop-time records."
  )
  filter_gtfs_tables_by_trips(tables, setdiff(trip_ids, invalid_trips))
}

drop_stop_times_missing_stops <- function(tables){
  if(!all(c("stop_times", "stops") %in% names(tables)) ||
     !"stop_id" %in% names(tables$stop_times) ||
     !"stop_id" %in% names(tables$stops)){
    return(tables)
  }
  valid <- as.character(tables$stop_times$stop_id) %in%
    as.character(tables$stops$stop_id)
  if(all(valid)){
    return(tables)
  }
  removed <- sum(!valid)
  remaining_counts <- table(as.character(tables$stop_times$trip_id[valid]))
  trip_ids <- as.character(tables$trips$trip_id)
  surviving_trips <- intersect(names(remaining_counts)[remaining_counts >= 2L], trip_ids)
  if(!length(surviving_trips)){
    missing_ids <- setdiff(
      unique(as.character(tables$stop_times$stop_id)),
      as.character(tables$stops$stop_id)
    )
    tables$stops <- add_placeholder_stops(tables$stops, missing_ids)
    gw_warn(
      "created ", length(missing_ids),
      " placeholder stop(s) for stop_id values referenced by `stop_times` ",
      "but absent from `stops`."
    )
    return(tables)
  }
  tables$stop_times <- tables$stop_times[valid, , drop = FALSE]
  gw_warn(
    "removed ", removed,
    " stop-time record(s) referencing stop_id values absent from `stops`."
  )
  tables
}

add_placeholder_stops <- function(stops, stop_ids){
  stop_ids <- sort(unique(as.character(stop_ids)))
  if(!length(stop_ids)){
    return(stops)
  }
  if(!"stop_name" %in% names(stops)){
    stops$stop_name <- NA_character_
  }
  if(!"stop_lat" %in% names(stops)){
    stops$stop_lat <- NA_real_
  }
  if(!"stop_lon" %in% names(stops)){
    stops$stop_lon <- NA_real_
  }
  if(!"location_type" %in% names(stops)){
    stops$location_type <- NA_integer_
  }
  lon <- suppressWarnings(as.numeric(stops$stop_lon))
  lat <- suppressWarnings(as.numeric(stops$stop_lat))
  has_lon <- any(is.finite(lon))
  has_lat <- any(is.finite(lat))
  center_lon <- if(has_lon) mean(lon[is.finite(lon)]) else 0
  center_lat <- if(has_lat) mean(lat[is.finite(lat)]) else 0
  if(!has_lon || !has_lat){
    gw_warn(
      "placeholder stop coordinates defaulted to (0, 0) because existing ",
      "`stops` coordinates are unavailable."
    )
  }
  placeholders <- stops[rep(NA_integer_, length(stop_ids)), , drop = FALSE]
  placeholders$stop_id <- stop_ids
  placeholders$stop_name <- paste("Placeholder stop", stop_ids)
  placeholders$stop_lat <- center_lat
  placeholders$stop_lon <- center_lon
  placeholders$location_type <- 0L
  if("parent_station" %in% names(placeholders)){
    placeholders$parent_station <- ""
  }
  rbind(stops, placeholders)
}

filter_gtfs_tables_by_trips <- function(tables, trip_ids){
  trip_ids <- unique(as.character(trip_ids))
  tables$trips <- filter_table_key(tables$trips, "trip_id", trip_ids)
  tables$stop_times <- filter_table_key(tables$stop_times, "trip_id", trip_ids)
  route_ids <- unique(tables$trips$route_id)
  service_ids <- unique(tables$trips$service_id)
  stop_ids <- unique(tables$stop_times$stop_id)
  shape_ids <- if("shape_id" %in% names(tables$trips)){
    unique(tables$trips$shape_id)
  } else {
    character()
  }

  tables$routes <- filter_table_key(tables$routes, "route_id", route_ids)
  tables$stops <- filter_table_key(tables$stops, "stop_id", stop_ids)
  tables[["calendar"]] <- filter_table_key(
    tables[["calendar"]], "service_id", service_ids
  )
  tables[["calendar_dates"]] <- filter_table_key(
    tables[["calendar_dates"]], "service_id", service_ids
  )
  tables$shapes <- filter_table_key(tables$shapes, "shape_id", shape_ids)
  tables$frequencies <- filter_table_key(tables$frequencies, "trip_id", trip_ids)
  tables$fare_rules <- filter_table_key(tables$fare_rules, "route_id", route_ids)
  if(!is.null(tables$fare_attributes) && !is.null(tables$fare_rules) &&
     "fare_id" %in% names(tables$fare_rules)){
    tables$fare_attributes <- filter_table_key(
      tables$fare_attributes, "fare_id", unique(tables$fare_rules$fare_id)
    )
  }
  if(!is.null(tables$agency) && "agency_id" %in% names(tables$agency) &&
     "agency_id" %in% names(tables$routes)){
    tables$agency <- filter_table_key(
      tables$agency, "agency_id", unique(tables$routes$agency_id)
    )
  }
  if(!is.null(tables$transfers)){
    keep <- tables$transfers$from_stop_id %in% stop_ids &
      tables$transfers$to_stop_id %in% stop_ids
    if("from_route_id" %in% names(tables$transfers)){
      keep <- keep & (
        is.na(tables$transfers$from_route_id) |
          !nzchar(as.character(tables$transfers$from_route_id)) |
          tables$transfers$from_route_id %in% route_ids
      )
    }
    if("to_route_id" %in% names(tables$transfers)){
      keep <- keep & (
        is.na(tables$transfers$to_route_id) |
          !nzchar(as.character(tables$transfers$to_route_id)) |
          tables$transfers$to_route_id %in% route_ids
      )
    }
    if("from_trip_id" %in% names(tables$transfers)){
      keep <- keep & (
        is.na(tables$transfers$from_trip_id) |
          !nzchar(as.character(tables$transfers$from_trip_id)) |
          tables$transfers$from_trip_id %in% trip_ids
      )
    }
    if("to_trip_id" %in% names(tables$transfers)){
      keep <- keep & (
        is.na(tables$transfers$to_trip_id) |
          !nzchar(as.character(tables$transfers$to_trip_id)) |
          tables$transfers$to_trip_id %in% trip_ids
      )
    }
    tables$transfers <- tables$transfers[keep, , drop = FALSE]
  }
  tables
}

validate_gtfs_tables <- function(tables){
  required_tables <- c("agency", "routes", "trips", "stop_times", "stops")
  missing_tables <- setdiff(required_tables, names(tables))
  if(length(missing_tables)){
    gw_stop("missing required table(s): ", paste(missing_tables, collapse = ", "), ".")
  }
  calendar <- tables[["calendar"]]
  calendar_dates <- tables[["calendar_dates"]]
  if(is.null(calendar) && is.null(calendar_dates)){
    gw_stop("supply at least one of `calendar` or `calendar_dates`.")
  }

  required_fields <- list(
    agency = c("agency_name", "agency_url", "agency_timezone"),
    routes = c("route_id", "route_type"),
    trips = c("route_id", "service_id", "trip_id"),
    stop_times = c("trip_id", "stop_id", "stop_sequence"),
    stops = "stop_id",
    calendar = c(
      "service_id", "monday", "tuesday", "wednesday", "thursday",
      "friday", "saturday", "sunday", "start_date", "end_date"
    ),
    calendar_dates = c("service_id", "date", "exception_type"),
    shapes = c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"),
    frequencies = c("trip_id", "start_time", "end_time", "headway_secs"),
    transfers = c("from_stop_id", "to_stop_id", "transfer_type"),
    fare_attributes = c("fare_id", "price", "currency_type", "payment_method", "transfers"),
    fare_rules = "fare_id"
  )
  for(table_name in intersect(names(required_fields), names(tables))){
    missing <- setdiff(required_fields[[table_name]], names(tables[[table_name]]))
    if(length(missing)){
      gw_stop(
        "table `", table_name, "` is missing required field(s): ",
        paste(missing, collapse = ", "), "."
      )
    }
  }

  routes <- tables$routes
  short_ok <- rep(FALSE, nrow(routes))
  long_ok <- rep(FALSE, nrow(routes))
  if("route_short_name" %in% names(routes)){
    short_ok <- !is.na(routes$route_short_name) &
      nzchar(as.character(routes$route_short_name))
  }
  if("route_long_name" %in% names(routes)){
    long_ok <- !is.na(routes$route_long_name) &
      nzchar(as.character(routes$route_long_name))
  }
  route_name_ok <- short_ok | long_ok
  if(any(!route_name_ok)){
    gw_stop("each route must have a non-empty `route_short_name` or `route_long_name`.")
  }

  agency_count <- nrow(tables$agency)
  if(agency_count > 1L && !"agency_id" %in% names(tables$agency)){
    gw_stop("`agency$agency_id` is required when the feed has multiple agencies.")
  }
  if(agency_count > 1L && !"agency_id" %in% names(routes)){
    gw_stop("`routes$agency_id` is required when the feed has multiple agencies.")
  }

  primary_keys <- list(
    agency = if("agency_id" %in% names(tables$agency)) "agency_id" else NULL,
    routes = "route_id",
    trips = "trip_id",
    stops = "stop_id",
    calendar = "service_id",
    calendar_dates = c("service_id", "date"),
    stop_times = c("trip_id", "stop_sequence"),
    shapes = c("shape_id", "shape_pt_sequence")
  )
  for(table_name in intersect(names(primary_keys), names(tables))){
    key <- primary_keys[[table_name]]
    if(length(key) && anyDuplicated(tables[[table_name]][key])){
      gw_stop("table `", table_name, "` contains duplicated key values.")
    }
  }

  assert_reference <- function(child, parent, label){
    values <- child[!is.na(child) & nzchar(as.character(child))]
    if(any(!values %in% parent)){
      gw_stop("invalid foreign key in ", label, ".")
    }
  }
  assert_reference(tables$trips$route_id, routes$route_id, "`trips$route_id`")
  assert_reference(tables$stop_times$trip_id, tables$trips$trip_id, "`stop_times$trip_id`")
  assert_reference(tables$stop_times$stop_id, tables$stops$stop_id, "`stop_times$stop_id`")

  service_ids <- unique(c(
    if(!is.null(calendar)) as.character(calendar$service_id),
    if(!is.null(calendar_dates)) as.character(calendar_dates$service_id)
  ))
  assert_reference(tables$trips$service_id, service_ids, "`trips$service_id`")

  if("agency_id" %in% names(routes) && "agency_id" %in% names(tables$agency)){
    assert_reference(routes$agency_id, tables$agency$agency_id, "`routes$agency_id`")
  }
  if(!is.null(tables$shapes) && "shape_id" %in% names(tables$trips)){
    assert_reference(tables$trips$shape_id, tables$shapes$shape_id, "`trips$shape_id`")
  }

  stop_times <- tables$stop_times
  if(!all(c("arrival_time", "departure_time") %in% names(stop_times))){
    gw_stop("`stop_times` must contain `arrival_time` and `departure_time`.")
  }
  for(field in c("arrival_time", "departure_time")){
    if(any(!is_valid_gtfs_time(stop_times[[field]]))){
      gw_stop("`stop_times$", field, "` contains invalid GTFS time values.")
    }
  }
  ordered <- stop_times[order(stop_times$trip_id, stop_times$stop_sequence), ]
  sequence_ok <- vapply(
    split(ordered$stop_sequence, ordered$trip_id),
    function(x) all(is.finite(x)) && all(x >= 0) && all(diff(x) > 0),
    logical(1)
  )
  if(any(!sequence_ok)){
    gw_stop("`stop_sequence` must be non-negative and strictly increase within each trip.")
  }
  trip_sizes <- lengths(split(ordered$stop_sequence, ordered$trip_id))
  if(any(trip_sizes < 2L)){
    gw_stop("every trip must contain at least two stop-time records.")
  }
  endpoints_ok <- vapply(
    split(ordered, ordered$trip_id),
    function(x){
      idx <- c(1L, nrow(x))
      all(is_valid_gtfs_time(x$arrival_time[idx], allow_blank = FALSE)) &&
        all(is_valid_gtfs_time(x$departure_time[idx], allow_blank = FALSE))
    },
    logical(1)
  )
  if(any(!endpoints_ok)){
    gw_stop("arrival and departure times are required at the first and last stop of every trip.")
  }
  if("direction_id" %in% names(tables$trips) &&
     any(!is.na(tables$trips$direction_id) &
         !tables$trips$direction_id %in% c(0, 1))){
    gw_stop("`trips$direction_id` must be empty, 0, or 1.")
  }

  stops <- tables$stops
  location_type <- if("location_type" %in% names(stops)){
    suppressWarnings(as.integer(stops$location_type))
  } else {
    rep(0L, nrow(stops))
  }
  location_type[is.na(location_type)] <- 0L
  if(any(!location_type %in% 0:4)){
    gw_stop("`stops$location_type` must be empty or one of 0, 1, 2, 3, or 4.")
  }
  point_types <- location_type %in% c(0L, 1L, 2L)
  if(any(point_types) &&
     !all(c("stop_lat", "stop_lon") %in% names(stops))){
    gw_stop("`stop_lat` and `stop_lon` are required for stops, stations, and entrances.")
  }
  if(all(c("stop_lat", "stop_lon") %in% names(stops))){
    coords_ok <- is.finite(stops$stop_lat) & is.finite(stops$stop_lon) &
      stops$stop_lat >= -90 & stops$stop_lat <= 90 &
      stops$stop_lon >= -180 & stops$stop_lon <= 180
    if(any(point_types & !coords_ok)){
      gw_stop("stop coordinates must be finite longitude/latitude values in WGS84.")
    }
  }
  if(any(point_types)){
    if(!"stop_name" %in% names(stops)){
      gw_stop("`stops$stop_name` is required for stops, stations, and entrances.")
    }
    name_ok <- !is.na(stops$stop_name) & nzchar(as.character(stops$stop_name))
    if(any(point_types & !name_ok)){
      gw_stop("stops, stations, and entrances require a non-empty `stop_name`.")
    }
  }
  if("parent_station" %in% names(stops)){
    assert_reference(stops$parent_station, stops$stop_id, "`stops$parent_station`")
    needs_parent <- location_type %in% c(2L, 3L, 4L)
    parent_present <- !is.na(stops$parent_station) &
      nzchar(as.character(stops$parent_station))
    if(any(needs_parent & !parent_present)){
      gw_stop("entrances, generic nodes, and boarding areas require `parent_station`.")
    }
    if(any(location_type == 1L & parent_present)){
      gw_stop("stations (`location_type = 1`) must not define `parent_station`.")
    }
  } else if(any(location_type %in% c(2L, 3L, 4L))){
    gw_stop("entrances, generic nodes, and boarding areas require `parent_station`.")
  }

  if(!is.null(calendar)){
    dates_ok <- !is.na(parse_gtfs_date(calendar$start_date)) &
      !is.na(parse_gtfs_date(calendar$end_date))
    if(any(!dates_ok)){
      gw_stop("`calendar` contains invalid `start_date` or `end_date` values.")
    }
    if(any(parse_gtfs_date(calendar$start_date) >
           parse_gtfs_date(calendar$end_date))){
      gw_stop("`calendar$start_date` must not be later than `end_date`.")
    }
    weekday_fields <- c(
      "monday", "tuesday", "wednesday", "thursday",
      "friday", "saturday", "sunday"
    )
    if(any(!unlist(calendar[weekday_fields]) %in% c(0, 1))){
      gw_stop("calendar weekday fields must contain only 0 or 1.")
    }
  }
  if(!is.null(calendar_dates)){
    if(any(is.na(parse_gtfs_date(calendar_dates$date)))){
      gw_stop("`calendar_dates$date` contains invalid dates.")
    }
    if(any(!calendar_dates$exception_type %in% c(1, 2))){
      gw_stop("`calendar_dates$exception_type` must contain only 1 or 2.")
    }
  }

  if(!is.null(tables$shapes)){
    shapes <- tables$shapes[order(
      tables$shapes$shape_id, tables$shapes$shape_pt_sequence
    ), ]
    shape_ok <- vapply(
      split(shapes$shape_pt_sequence, shapes$shape_id),
      function(x) all(is.finite(x)) && all(x >= 0) && all(diff(x) > 0),
      logical(1)
    )
    if(any(!shape_ok)){
      gw_stop("`shape_pt_sequence` must be non-negative and strictly increase within each shape.")
    }
    if("shape_dist_traveled" %in% names(shapes)){
      distance_ok <- vapply(
        split(as.numeric(shapes$shape_dist_traveled), shapes$shape_id),
        function(x) all(is.na(x) | is.finite(x)) &&
          all(diff(x[!is.na(x)]) >= 0),
        logical(1)
      )
      if(any(!distance_ok)){
        gw_stop("`shape_dist_traveled` must not decrease within a shape.")
      }
    }
  }

  if(!is.null(tables$frequencies)){
    frequencies <- tables$frequencies
    assert_reference(frequencies$trip_id, tables$trips$trip_id, "`frequencies$trip_id`")
    start <- gtfs_time_to_seconds(frequencies$start_time)
    end <- gtfs_time_to_seconds(frequencies$end_time)
    if(anyNA(start) || anyNA(end) || any(start >= end) ||
       any(!is.finite(frequencies$headway_secs)) || any(frequencies$headway_secs <= 0)){
      gw_stop("frequency periods require valid times, `start_time < end_time`, and positive headways.")
    }
    if("exact_times" %in% names(frequencies) &&
       any(!is.na(frequencies$exact_times) & !frequencies$exact_times %in% c(0, 1))){
      gw_stop("`frequencies$exact_times` must contain only 0 or 1.")
    }
    periods <- frequencies[order(frequencies$trip_id, start), ]
    periods$.start <- gtfs_time_to_seconds(periods$start_time)
    periods$.end <- gtfs_time_to_seconds(periods$end_time)
    overlap <- vapply(
      split(periods, periods$trip_id),
      function(x) nrow(x) > 1L && any(x$.start[-1L] < x$.end[-nrow(x)]),
      logical(1)
    )
    if(any(overlap)){
      gw_stop("frequency periods for the same trip must not overlap.")
    }
  }

  if(!is.null(tables$transfers)){
    assert_reference(tables$transfers$from_stop_id, stops$stop_id, "`transfers$from_stop_id`")
    assert_reference(tables$transfers$to_stop_id, stops$stop_id, "`transfers$to_stop_id`")
    for(field in intersect(c("from_route_id", "to_route_id"), names(tables$transfers))){
      assert_reference(tables$transfers[[field]], routes$route_id, paste0("`transfers$", field, "`"))
    }
    for(field in intersect(c("from_trip_id", "to_trip_id"), names(tables$transfers))){
      assert_reference(tables$transfers[[field]], tables$trips$trip_id, paste0("`transfers$", field, "`"))
    }
    if(any(!tables$transfers$transfer_type %in% 0:5)){
      gw_stop("`transfers$transfer_type` must be one of 0, 1, 2, 3, 4, or 5.")
    }
  }

  invisible(TRUE)
}

filter_table_key <- function(table, field, values){
  if(is.null(table) || !field %in% names(table)){
    return(table)
  }
  table[table[[field]] %in% values, , drop = FALSE]
}

prune_gtfs <- function(gtfs, trip_ids, stop_times = NULL){
  trip_ids <- unique(as.character(trip_ids))
  gtfs$trips <- filter_table_key(gtfs$trips, "trip_id", trip_ids)
  if(is.null(stop_times)){
    gtfs$stop_times <- filter_table_key(gtfs$stop_times, "trip_id", trip_ids)
  } else {
    gtfs$stop_times <- filter_table_key(stop_times, "trip_id", trip_ids)
  }

  route_ids <- unique(gtfs$trips$route_id)
  service_ids <- unique(gtfs$trips$service_id)
  stop_ids <- unique(gtfs$stop_times$stop_id)
  shape_ids <- if("shape_id" %in% names(gtfs$trips)){
    unique(gtfs$trips$shape_id)
  } else {
    character()
  }

  gtfs$routes <- filter_table_key(gtfs$routes, "route_id", route_ids)
  gtfs$stops <- filter_table_key(gtfs$stops, "stop_id", stop_ids)
  gtfs[["calendar"]] <- filter_table_key(
    gtfs[["calendar"]], "service_id", service_ids
  )
  gtfs[["calendar_dates"]] <- filter_table_key(
    gtfs[["calendar_dates"]], "service_id", service_ids
  )
  if(!is.null(gtfs[["calendar_dates"]]) &&
     !nrow(gtfs[["calendar_dates"]])){
    gtfs[["calendar_dates"]] <- NULL
  }
  gtfs$shapes <- filter_table_key(gtfs$shapes, "shape_id", shape_ids)
  gtfs$frequencies <- filter_table_key(gtfs$frequencies, "trip_id", trip_ids)
  gtfs$fare_rules <- filter_table_key(gtfs$fare_rules, "route_id", route_ids)
  if(!is.null(gtfs$fare_attributes) && !is.null(gtfs$fare_rules) &&
     "fare_id" %in% names(gtfs$fare_rules)){
    gtfs$fare_attributes <- filter_table_key(
      gtfs$fare_attributes, "fare_id", unique(gtfs$fare_rules$fare_id)
    )
  }

  if(!is.null(gtfs$agency) && "agency_id" %in% names(gtfs$agency) &&
     "agency_id" %in% names(gtfs$routes)){
    gtfs$agency <- filter_table_key(
      gtfs$agency, "agency_id", unique(gtfs$routes$agency_id)
    )
  }

  if(!is.null(gtfs$transfers)){
    keep <- gtfs$transfers$from_stop_id %in% stop_ids &
      gtfs$transfers$to_stop_id %in% stop_ids
    if("from_route_id" %in% names(gtfs$transfers)){
      keep <- keep & (
        is.na(gtfs$transfers$from_route_id) |
          !nzchar(as.character(gtfs$transfers$from_route_id)) |
          gtfs$transfers$from_route_id %in% route_ids
      )
    }
    if("to_route_id" %in% names(gtfs$transfers)){
      keep <- keep & (
        is.na(gtfs$transfers$to_route_id) |
          !nzchar(as.character(gtfs$transfers$to_route_id)) |
          gtfs$transfers$to_route_id %in% route_ids
      )
    }
    if("from_trip_id" %in% names(gtfs$transfers)){
      keep <- keep & (
        is.na(gtfs$transfers$from_trip_id) |
          !nzchar(as.character(gtfs$transfers$from_trip_id)) |
          gtfs$transfers$from_trip_id %in% trip_ids
      )
    }
    if("to_trip_id" %in% names(gtfs$transfers)){
      keep <- keep & (
        is.na(gtfs$transfers$to_trip_id) |
          !nzchar(as.character(gtfs$transfers$to_trip_id)) |
          gtfs$transfers$to_trip_id %in% trip_ids
      )
    }
    gtfs$transfers <- gtfs$transfers[keep, , drop = FALSE]
  }

  gtfs$dates_services <- NULL
  gtfs <- create_dates_services_table(gtfs)
  class(gtfs) <- c("wizardgtfs", "gtfs", "list")
  gtfs
}

verify_tables <- function(x,tables){
  ls <- rep(FALSE,length(tables))
  ls <- tables%in%names(x)==FALSE
  names(ls) <- tables
  return(ls)
}

verify_field <- function(tbl,x){
  x %in% names(tbl)
}

field_if_exist <- function(tbl,x){
  if(x %in% names(tbl)){
    return(x)
  }else{
    return(NULL)
  }
}

get_stop_dists <- function(gtfs){
  calls <- gtfs$stop_times |>
    dplyr::arrange(trip_id, stop_sequence) |>
    dplyr::left_join(
      gtfs$stops |> dplyr::select(stop_id, stop_lon, stop_lat),
      by = "stop_id"
    ) |>
    dplyr::group_by(trip_id) |>
    dplyr::mutate(
      .distance = haversine_m(
        stop_lon, stop_lat, dplyr::lead(stop_lon), dplyr::lead(stop_lat)
      )
    ) |>
    dplyr::ungroup()
  round(stats::median(calls$.distance, na.rm = TRUE), 1)
}

get_trip_stops_dist <- function(lon,lat){
  if(length(lon) < 2 || length(lat) < 2){
    return(numeric())
  }
  haversine_m(
    lon[-length(lon)],
    lat[-length(lat)],
    lon[-1],
    lat[-1]
  )
}

trip_instance_starts <- function(gtfs){
  gtfs <- ensure_wizardgtfs(gtfs)
  first <- get_1stdeparture(gtfs)
  first$.base_start <- gtfs_time_to_seconds(first$departure_time)
  frequency_trip <- if(is.null(gtfs$frequencies)){
    character()
  } else {
    unique(as.character(gtfs$frequencies$trip_id))
  }
  scheduled <- first[!first$trip_id %in% frequency_trip, c("trip_id", ".base_start")]
  scheduled$instance_id <- paste0(scheduled$trip_id, ".scheduled")
  scheduled$start_seconds <- scheduled$.base_start

  generated <- list()
  if(!is.null(gtfs$frequencies) && nrow(gtfs$frequencies)){
    for(i in seq_len(nrow(gtfs$frequencies))){
      row <- gtfs$frequencies[i, ]
      start <- gtfs_time_to_seconds(row$start_time)
      end <- gtfs_time_to_seconds(row$end_time)
      times <- seq(start, end - 1, by = as.numeric(row$headway_secs))
      generated[[i]] <- tibble::tibble(
        trip_id = as.character(row$trip_id),
        .base_start = first$.base_start[match(row$trip_id, first$trip_id)],
        instance_id = paste0(row$trip_id, ".frequency", i, ".", seq_along(times)),
        start_seconds = times
      )
    }
  }
  dplyr::bind_rows(scheduled, generated)
}

stop_call_instances <- function(gtfs){
  starts <- trip_instance_starts(gtfs)
  calls <- gtfs$stop_times |>
    dplyr::arrange(trip_id, stop_sequence) |>
    dplyr::mutate(.call_seconds = gtfs_time_to_seconds(arrival_time)) |>
    dplyr::select(trip_id, stop_id, stop_sequence, .call_seconds)
  dplyr::left_join(calls, starts, by = "trip_id") |>
    dplyr::mutate(time_seconds = start_seconds + .call_seconds - .base_start) |>
    dplyr::filter(!is.na(time_seconds))
}

require_pkg <- function(pkg, context = "this function"){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    gw_stop("package `", pkg, "` is required for ", context, ". Please install it.")
  }
  invisible(TRUE)
}

utils::globalVariables(c(
  "agency_id", "agency_name", "arrival_filter", "arrival_time", "edit", "arrival_time_sec",
  "average.distance", "average.duration", "average.dwelltime", "departure_time_sec",
  "average.headway", "average.speed", "daily.frequency", "get_hubs_clusters.wzd_transfers",
  "day_of_month", "departure", "departure_filter", "departure_time", "mid_dwelltime",
  "distance", "dupe", "edge_paths", "edges", "end_date", "ends",
  "exception_type", "first_day_of_month", "frequency", "from_stop_id",
  "geometry", "headway", "headway.minutes", "median", "na.omit",
  "name", "name.ends", "name.starts", "net.fleet", "new.trip_id",
  "pattern_frequency", "route_id", "service_id", "service_pattern",
  "setNames", "setTxtProgressBar", "shape_dist_traveled", "shape_id",
  "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence", "start_date",
  "starts", "stop_id", "stop_lat", "stop_lon", "stop_sequence", "get_transfer_clusters.wzd_transfers",
  "subtrip", "time", "to_stop_id", "trip_id", "trips", "get_high_transfer_stops",
  "txtProgressBar", "type", "value", "week_of_month", "weekday",
  "weighted.mean", ".", "lead_arrival_time", "cum.diff.time", "diff.time", "new.duration",
  "average_headway_minutes", "headway_minutes", "valid_trips", "corridor", "destination",
  "group_id", "origin", "percent_rank", "stop_from", "stop_to", "stops", "stops_sf",
  "actual_dweeltime", "actual_dwelltime", "change_dweeltime", "change_value", "cluster",
  "groups", "index", "max_dweeltime", "max_dweeltime_ant", "max_dweeltime_post",
  "mean_n_routes", "n_routes", "next_stop", "non_negative", "routes_similary", "selection_stops",
  "arrival_time_diff", "departure_time_diff", "diff_dwell_time", "dwell_time", "new_dwell_time",
  "route_short_name", "route_long_name", "arrival_seconds", "departure_seconds",
  "time_seconds", "fleet", ".arrival", ".base_start", ".call_seconds",
  ".component", ".data", ".departure", ".duration", ".emphasis",
  ".next_stop", ".signature", "arrival_time_num", "departure_time_num",
  "duration", "month", "n_trip", "speed", "start_seconds",
  "trips.duration", "year", "average_departures", "duration_minutes",
  "final_arrival", "first_departure", "route_pattern", "vehicle_hours"
))
