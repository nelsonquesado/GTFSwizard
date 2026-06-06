#' Identify High-Frequency Transit Corridors
#'
#' Finds frequently served adjacent stop pairs, joins connected pairs into
#' corridors, and measures them in a local metric CRS.
#'
#' @param gtfs A GTFS object.
#' @param i Proportion of the highest-frequency stop pairs to retain. Must be
#'   greater than 0 and no greater than 1.
#' @param min_length Minimum corridor length in meters.
#' @param ... Supports the legacy argument `min.length`.
#'
#' @return An `sf` object with `corridor`, list-columns `stop_id` and
#'   `trip_id`, numeric `length` in meters, and WGS84 geometry. Segments are
#'   straight lines between stop coordinates, not shape geometry.
#'
#' @examples
#' corridors <- get_corridor(for_rail_gtfs, i = 0.2, min_length = 100)
#'
#' @seealso [GTFSwizard::plot_corridor()]
#' @export
get_corridor <- function(gtfs, i = 0.01, min_length = 1500, ...){
  resolved <- resolve_legacy_argument(
    min_length, missing(min_length), list(...), "min.length", "min_length"
  )
  min_length <- resolved$value
  gw_check_unused_dots(resolved$dots)
  if(!is.numeric(i) || length(i) != 1L || is.na(i) || i <= 0 || i > 1){
    gw_stop("`i` must be one number greater than 0 and no greater than 1.")
  }
  if(!is.numeric(min_length) || length(min_length) != 1L ||
     is.na(min_length) || min_length < 0){
    gw_stop("`min_length` must be one non-negative number of meters.")
  }
  gtfs <- ensure_wizardgtfs(gtfs)

  segments <- gtfs$stop_times |>
    dplyr::arrange(trip_id, stop_sequence) |>
    dplyr::group_by(trip_id) |>
    dplyr::mutate(.next_stop = dplyr::lead(stop_id)) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.next_stop)) |>
    dplyr::transmute(
      trip_id,
      stop_from = pmin(as.character(stop_id), as.character(.next_stop)),
      stop_to = pmax(as.character(stop_id), as.character(.next_stop))
    ) |>
    dplyr::distinct() |>
    dplyr::group_by(stop_from, stop_to) |>
    dplyr::summarise(
      trips = dplyr::n(),
      trip_id = list(trip_id),
      .groups = "drop"
    )
  if(!nrow(segments)){
    gw_stop("the feed has no consecutive stop pairs.")
  }
  cutoff <- stats::quantile(
    segments$trips, probs = 1 - i, names = FALSE, type = 1
  )
  segments <- segments[segments$trips >= cutoff, , drop = FALSE]

  stops <- gtfs$stops |>
    dplyr::select(stop_id, stop_lon, stop_lat) |>
    dplyr::mutate(stop_id = as.character(stop_id))
  from <- dplyr::left_join(
    segments, stops, by = c("stop_from" = "stop_id")
  )
  to <- dplyr::left_join(
    segments, stops, by = c("stop_to" = "stop_id")
  )
  valid <- stats::complete.cases(
    from$stop_lon, from$stop_lat, to$stop_lon, to$stop_lat
  )
  segments <- segments[valid, , drop = FALSE]
  from <- from[valid, , drop = FALSE]
  to <- to[valid, , drop = FALSE]
  geometry <- lapply(seq_len(nrow(segments)), function(row){
    sf::st_linestring(matrix(
      c(
        from$stop_lon[row], from$stop_lat[row],
        to$stop_lon[row], to$stop_lat[row]
      ),
      ncol = 2, byrow = TRUE
    ))
  })
  segments <- sf::st_sf(
    segments,
    geometry = sf::st_sfc(geometry, crs = 4326)
  )
  segments$.component <- segment_components(
    segments$stop_from, segments$stop_to
  )
  metric <- latlon2epsg(segments)
  metric <- metric |>
    dplyr::group_by(.component) |>
    dplyr::summarise(
      stop_id = list(unique(c(stop_from, stop_to))),
      trip_id = list(unique(unlist(trip_id))),
      length = as.numeric(sum(sf::st_length(geometry))),
      .groups = "drop"
    ) |>
    dplyr::filter(length >= min_length) |>
    dplyr::arrange(dplyr::desc(length))
  if(!nrow(metric)){
    gw_warn("no corridors meet `min_length`; returning an empty result.")
    return(sf::st_transform(metric, 4326))
  }
  metric$corridor <- paste("Corridor", seq_len(nrow(metric)))
  metric <- metric[, c("corridor", "stop_id", "trip_id", "length", "geometry")]
  sf::st_transform(metric, 4326)
}

segment_components <- function(from, to){
  vertices <- unique(c(as.character(from), as.character(to)))
  parent <- seq_along(vertices)
  find_root <- function(x){
    while(parent[x] != x){
      parent[x] <<- parent[parent[x]]
      x <- parent[x]
    }
    x
  }
  for(i in seq_along(from)){
    a <- find_root(match(as.character(from[i]), vertices))
    b <- find_root(match(as.character(to[i]), vertices))
    if(a != b){
      parent[b] <- a
    }
  }
  roots <- vapply(match(as.character(from), vertices), find_root, integer(1))
  match(roots, unique(roots))
}
