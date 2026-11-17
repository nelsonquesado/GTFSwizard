#' Calculate Consistent Trip and Segment Distances
#'
#' Calculate distances in metres using the same segments for every summary.
#' @param gtfs A GTFS object.
#' @param method One of `"by_trip"`, `"by_route"`, or `"detailed"`.
#' @param trips Character trip IDs or `"all"`.
#' @param distance_method Either `"shapes"` (default) or `"straight"`.
#' @return A tibble. Detailed rows identify trips and their originating
#'   `stop_sequence`; trip and route summaries retain service patterns.
#' @details Shape distances use stop and shape `shape_dist_traveled` values
#'   together when available, converting their common units to metres using
#'   the shape geometry. Otherwise stops are projected onto shape segments in
#'   sequence order. This is an approximation, especially on loops; supplied
#'   distance measures are preferred. Missing shapes fall back to straight
#'   geodesic segments with a warning. Trip lengths cover only served segments,
#'   rather than the complete shared shape. Straight distances are geodesic.
#' @examples
#' get_distances(for_rail_gtfs, "by_trip")
#' @export
get_distances <- function(gtfs, method = "by_trip", trips = "all",
                          distance_method = c("shapes", "straight")){
  method <- normalize_method(method, c("by_route", "by_trip", "detailed"), "by_trip")
  distance_method <- match.arg(distance_method)
  gtfs <- ensure_wizardgtfs(gtfs)
  if(!identical(trips, "all")) gtfs <- filter_trip(gtfs, trips)
  segments <- distance_segments(gtfs, distance_method)
  if(method == "detailed") return(segments)
  totals <- segments |>
    dplyr::group_by(trip_id) |>
    dplyr::summarise(distance = sum(distance), .groups = "drop")
  result <- gtfs$trips |>
    dplyr::left_join(totals, by = "trip_id") |>
    dplyr::inner_join(get_servicepattern(gtfs), by = "service_id",
                       relationship = "many-to-many")
  groups <- c("route_id", direction_field(result), "service_pattern", "pattern_frequency")
  if(method == "by_route"){
    return(result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(average.distance = mean(distance), trips = dplyr::n(), .groups = "drop"))
  }
  result[, c("route_id", "trip_id", direction_field(result), "distance",
              "service_pattern", "pattern_frequency"), drop = FALSE]
}

distance_segments <- function(gtfs, distance_method){
  calls <- gtfs$stop_times[order(gtfs$stop_times$trip_id, gtfs$stop_times$stop_sequence), ]
  coords <- gtfs$stops[match(calls$stop_id, gtfs$stops$stop_id), ]
  calls$.lon <- coords$stop_lon
  calls$.lat <- coords$stop_lat
  shape_ids <- if("shape_id" %in% names(gtfs$trips)) {
    as.character(gtfs$trips$shape_id[match(calls$trip_id, gtfs$trips$trip_id)])
  } else rep(NA_character_, nrow(calls))
  calls$shape_id <- shape_ids
  n <- nrow(calls)
  from <- if(n > 1L) which(calls$trip_id[-n] == calls$trip_id[-1L]) else integer()
  to <- from + 1L
  distance <- haversine_m(calls$.lon[from], calls$.lat[from], calls$.lon[to], calls$.lat[to])
  if(distance_method == "shapes" && length(from)){
    shapes <- if(!is.null(gtfs$shapes)) split(gtfs$shapes, gtfs$shapes$shape_id) else list()
    matched <- shape_ids[from] %in% names(shapes)
    if(any(!matched)) gw_warn("missing trip shapes; using straight distances for affected trips.")
    if(any(matched)){
      segments <- split(which(matched), calls$trip_id[from[matched]], drop = TRUE)
      starts <- vapply(segments, `[[`, integer(1), 1L)
      call_rows <- Map(function(i, start) c(from[start], to[i]), segments, starts)
      tokens <- match(calls$stop_id, unique(calls$stop_id))
      if("shape_dist_traveled" %in% names(calls)){
        measures <- calls$shape_dist_traveled
        tokens <- paste(tokens, match(measures, unique(measures)), sep = "/")
      }
      signatures <- paste(match(shape_ids[from[starts]], names(shapes)),
                          vapply(call_rows, function(i) paste(tokens[i], collapse = ","), character(1)))
      pattern <- match(signatures, unique(signatures))
      by_pattern <- lapply(call_rows[!duplicated(pattern)], function(i){
        diff(stop_shape_positions(calls[i, ], shapes[[shape_ids[i[1L]]]]))
      })
      distance[unlist(segments, use.names = FALSE)] <- unlist(by_pattern[pattern], use.names = FALSE)
    }
  }
  tibble::tibble(trip_id = calls$trip_id[from], shape_id = shape_ids[from],
                 stop_sequence = calls$stop_sequence[from], from_stop_id = calls$stop_id[from],
                 to_stop_id = calls$stop_id[to], distance = distance)
}

stop_shape_positions <- function(calls, shape){
  shape <- shape[order(shape$shape_pt_sequence), ]
  lengths <- haversine_m(utils::head(shape$shape_pt_lon, -1), utils::head(shape$shape_pt_lat, -1),
                         utils::tail(shape$shape_pt_lon, -1), utils::tail(shape$shape_pt_lat, -1))
  cumulative <- c(0, cumsum(lengths))
  if(identical(as.numeric(calls$.lon), as.numeric(shape$shape_pt_lon)) &&
     identical(as.numeric(calls$.lat), as.numeric(shape$shape_pt_lat))) return(cumulative)
  if("shape_dist_traveled" %in% names(calls) && "shape_dist_traveled" %in% names(shape)){
    measures <- shape$shape_dist_traveled
    stops <- calls$shape_dist_traveled
    if(all(is.finite(measures)) && all(diff(measures) > 0) &&
       all(is.finite(stops)) && all(diff(stops) >= 0) &&
       all(stops >= min(measures) & stops <= max(measures))){
      return(stats::approx(measures, cumulative, xout = stops)$y)
    }
  }
  if(any(!is.finite(c(calls$.lon, calls$.lat)))) return(rep(NA_real_, nrow(calls)))
  # Local azimuthal projection provides metric nearest-point calculations.
  crs <- paste0("+proj=aeqd +lat_0=", mean(shape$shape_pt_lat),
                 " +lon_0=", mean(shape$shape_pt_lon), " +datum=WGS84 +units=m")
  xy <- function(lon, lat){
    sf::st_coordinates(sf::st_transform(sf::st_as_sf(data.frame(lon, lat),
      coords = c("lon", "lat"), crs = 4326), crs))
  }
  points <- xy(shape$shape_pt_lon, shape$shape_pt_lat)
  stops <- xy(calls$.lon, calls$.lat)
  a <- points[-nrow(points), , drop = FALSE]
  delta <- points[-1, , drop = FALSE] - a
  squared <- rowSums(delta^2)
  previous <- 0
  positions <- numeric(nrow(stops))
  for(i in seq_len(nrow(stops))){
    fraction <- pmax(0, pmin(1, rowSums(sweep(a, 2, stops[i, ], "-") * -delta) /
                              ifelse(squared == 0, 1, squared)))
    along <- utils::head(cumulative, -1) + fraction * lengths
    eligible <- along >= previous - 1e-6
    errors <- rowSums((a + delta * fraction - matrix(stops[i, ], nrow(a), 2, byrow = TRUE))^2)
    errors[!eligible] <- Inf
    best <- which.min(errors)
    if(!length(best) || !is.finite(errors[best])){
      gw_stop("stops cannot be projected in shape order; supply `shape_dist_traveled` or use `straight`.")
    }
    positions[i] <- max(previous, along[best])
    previous <- positions[i]
  }
  positions
}
