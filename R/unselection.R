#' @rdname selection
#' @aliases unselection
#' @export

unselection <- function(gtfs){
  UseMethod('unselection')
}

#' @exportS3Method GTFSwizard::unselection wizardgtfs_selected
unselection.wizardgtfs_selected <- function(gtfs){
  attributes(gtfs) <- attributes(gtfs)[names(attributes(gtfs))%nin%c('selection','selection_expr')]
  class(gtfs) <- c('wizardgtfs','gtfs','list')
  return(gtfs)
}

#' @exportS3Method GTFSwizard::unselection wizardgtfs
unselection.wizardgtfs <- function(gtfs){
  cat('There is no selection on the gtfsect')
  return(gtfs)
}
