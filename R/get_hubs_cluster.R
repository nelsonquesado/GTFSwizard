#' Identify and Cluster High-Transfer Stops
#'
#' `get_hubs_clusters` identifies high-transfer stops and groups them into clusters based on the number of routes served. This helps in identifying transit hubs and patterns in transit networks.
#'
#' @param obj An object of class `wizardgtfs` or `wzd_hubs`. If a `wizardgtfs` object is provided, high-transfer stops are first extracted using `get_stop_hubs`.
#' @param ... Additional arguments passed to the underlying methods, such as parameters for `get_stop_hubs`.
#'
#' @return A `wzd_hubscluster` object (a specialized tibble) with the following columns:
#' \describe{
#'   \item{cluster}{The cluster ID assigned to each stop, based on its transfer characteristics.}
#'   \item{stop_id}{The ID of the stop included in the cluster.}
#'   \item{n_routes}{The number of unique routes serving the stop.}
#'   \item{mean_n_routes}{The average number of routes for all stops in the same cluster.}
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Groups stops by `service_pattern` and `hour`.
#'   \item For stops with a high number of routes (above the 75th percentile), applies k-means clustering (with 4 centers) to assign cluster IDs.
#'   \item For stops with fewer routes (below or equal to the 75th percentile), assigns all stops to a single cluster.
#'   \item Combines clustered data into a single table and calculates the mean number of routes for each cluster.
#' }
#'
#' @note
#' - Ensure the input `obj` contains high-transfer stop data if passing a `wzd_transfers` object.
#'
#' - The clustering process uses k-means, so results may vary depending on the data distribution.
#'
#' @examples
#' # Generate transfer clusters from a wizardgtfs object
#' clusters <- get_transfer_clusters(for_bus_gtfs, threshold = 10)
#'
#' # Generate clusters directly from a high-transfer stops object
#' high_transfer_stops <- get_high_transfer_stops(for_bus_gtfs)
#' clusters <- get_transfer_clusters(high_transfer_stops)
#'
#' @seealso
#' [get_stop_hubs()], [stats::kmeans()]
#'
#' @importFrom dplyr group_by group_split mutate arrange bind_rows ungroup reframe cur_group_id filter
#' @importFrom data.table rbindlist
#' @importFrom stats kmeans quantile
#' @export
get_hubs_clusters <- function(obj,...){
  UseMethod('get_hubs_clusters')
}

#' @exportS3Method GTFSwizard::get_hubs_clusters wizardgtfs
get_hubs_clusters.wizardgtfs <- function(obj,...){
  transfers <- get_high_transfer_stops(obj,...)
  get_transfer_clusters.wzd_transfers(transfers)
}

#' @exportS3Method GTFSwizard::get_hubs_clusters list
get_hubs_clusters.list <- function(obj,...){
  transfers <- get_stop_hubs(obj,...)
  get_hubs_clusters.wzd_transfers(transfers)
}


#' @exportS3Method GTFSwizard::get_hubs_clusters wzd_hubs
get_hubs_clusters.wzd_hubs <- function(obj,...){
  transfers_lst <- obj %>%
    dplyr::group_by(service_pattern,hour) %>%
    dplyr::group_split()

  transfer_clusters <- lapply(transfers_lst, function(tran){

    if(nrow(tran)>4){

      tran_low <- tran %>%
        dplyr::filter(n_routes<=quantile(n_routes,.75)) %>%
        dplyr::mutate(cluster=1)

      tran <- tran %>%
        dplyr::filter(n_routes>quantile(n_routes,.75))

      tran$cluster <- stats::kmeans(tran$n_routes,centers = 4)$cluster

      tran %>%
        dplyr::group_by(cluster) %>%
        dplyr::mutate(mean_n_routes = mean(n_routes,na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(mean_n_routes) %>%
        dplyr::group_by(mean_n_routes) %>%
        dplyr::mutate(cluster = dplyr::cur_group_id()) %>%
        dplyr::bind_rows(tran_low) %>%
        dplyr::group_by(cluster) %>%
        dplyr::mutate(mean_n_routes = mean(n_routes,na.rm = T)) %>%
        return()
    }else{
      tran %>%
        dplyr::arrange(n_routes) %>%
        dplyr::mutate(cluster = 1:dplyr::n()) %>%
        dplyr::group_by(cluster) %>%
        dplyr::mutate(mean_n_routes = mean(n_routes,na.rm = T)) %>%
        return()
    }




  }) %>% data.table::rbindlist()

  class(transfer_clusters) <- c('wzd_hubscluster',class(transfer_clusters))

  attr(transfer_clusters,'stop_position') <- attr(obj,'stop_position')

  return(transfer_clusters)
}
