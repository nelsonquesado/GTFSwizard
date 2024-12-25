#' Visualize Clusters of Transit Hubs
#'
#' `plot_hubs_cluster` generates an interactive map to visualize clusters of transit hubs for a specified hour. Clusters are color-coded, and stops within each cluster are displayed with varying opacity based on their cluster assignment.
#'
#' @param obj An object of class `wzd_hubscluster`, containing clustered hub data.
#' @param hour A numeric value indicating the hour (0-24) for which to visualize the clusters. This parameter is required.
#'
#' @return A `leaflet` map object visualizing the transit hubs and their clusters. Each cluster is color-coded, and stops are displayed as circle markers.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Filters the `obj` for the specified `hour`.
#'   \item Joins stop positions from the object's attributes (`stop_lon`, `stop_lat`).
#'   \item Converts the stop data into an `sf` object for spatial visualization.
#'   \item Applies a color palette to distinguish clusters and adjusts marker opacity for better visualization.
#'   \item Generates an interactive map using `leaflet`, including cluster markers and a legend.
#' }
#'
#' @note
#' - Ensure the input object `obj` contains valid hub cluster data and stop positions.
#'
#' - The hour must be specified as a numeric value between 0 and 24.
#'
#' @examples
#' # Visualize hub clusters for hour 7 (7 AM)
#' plot_hubs_cluster(hub_clusters, hour = 7)
#'
#' # Visualize hub clusters for hour 15 (3 PM)
#' plot_hubs_cluster(hub_clusters, hour = 15)
#'
#' @seealso
#' [leaflet::leaflet()], [sf::st_as_sf()]
#'
#' @importFrom dplyr filter mutate left_join
#' @importFrom sf st_as_sf
#' @importFrom leaflet leaflet addCircleMarkers addTiles addLegend colorFactor
#' @importFrom viridis viridis_pal
#' @importFrom checkmate assert_numeric
#' @export
plot_hubs_cluster <- function(obj,hour){

  if(missing('hour')){
    stop("'hour' is missing with no default")
  }
  checkmate::assert_numeric(hour,len = 1)

  UseMethod('plot_hubs_cluster')
}

#' @exportS3Method GTFSwizard::plot_hubs_cluster wzd_hubscluster
plot_hubs_cluster.wzd_hubscluster <- function(obj,hour){

  h = hour
  checkmate::assert_subset(hour,obj$hour)
  pal <- leaflet::colorFactor(palette = viridis::viridis_pal(option = 'H')(4),domain = obj$cluster)
  obj %>%
    dplyr::filter(hour == h) %>%
    dplyr::left_join(attr(obj,'stop_position'),by = 'stop_id') %>%
    sf::st_as_sf(coords = c('stop_lon','stop_lat'),crs = 4326) %>%
    dplyr::mutate(opacity = ifelse(cluster==1,0,cluster/max(cluster))) %>%
    leaflet::leaflet() %>%
    leaflet::addCircleMarkers(color = ~pal(cluster),fillOpacity = ~opacity,opacity = ~opacity) %>%
    leaflet::addTiles() %>%
    leaflet::addLegend(pal = pal,values = ~cluster,opacity = 1)
}


#' @exportS3Method base::plot wzd_hubscluster
plot.wzd_hubscluster <- function(x,...){

  if(missing('hour')){
    message("Filtering the time with the most trips")
    hour <- x$hour[which.max(x$n_trips)] %>% as.numeric()
  }
  checkmate::assert_numeric(hour)
  checkmate::assert_subset(hour,x$hour)
  h <- hour
  x %>%
    dplyr::filter(hour == h) %>%
    dplyr::left_join(attr(x,'stop_position'),by = 'stop_id') %>%
    dplyr::mutate(cluster = as.character(cluster)) %>%
    sf::st_as_sf(coords = c('stop_lon','stop_lat'),crs = 4326) %>%
    ggplot2::ggplot(ggplot2::aes(color = cluster,alpha = cluster))+
    ggplot2::geom_sf()+
    ggplot2::theme_void()+
    ggplot2::facet_wrap(~service_pattern)+
    ggplot2::scale_alpha_manual(
      breaks = as.character(1:4),
      values = c(0,0.1,.8,1)
    ) +
    ggplot2::guides(alpha = 'none')+
    ggplot2::labs(title = 'Clusters oh high transfers',subtitle = paste0('Hour: ',hour))
}
