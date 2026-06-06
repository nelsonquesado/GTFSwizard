#' @rdname selection
#' @aliases unselection
#' @export

unselection <- function(gtfs){
  UseMethod('unselection')
}

#' @exportS3Method GTFSwizard::unselection wizardgtfs_selected
unselection.wizardgtfs_selected <- function(gtfs){
  attr(gtfs, "selection") <- NULL
  attr(gtfs, "selection_expr") <- NULL
  class(gtfs) <- c("wizardgtfs", "gtfs", "list")
  gtfs
}

#' @exportS3Method GTFSwizard::unselection wizardgtfs
unselection.wizardgtfs <- function(gtfs){
  gw_msg("the object has no active selection.")
  gtfs
}
