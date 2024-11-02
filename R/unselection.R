unselection <- function(obj){
  UseMethod('unselection')
}

unselection.wizardgtfs_selected <- function(obj){
  attributes(obj) <- attributes(obj)[names(attributes(obj))%nin%c('selection','selection_expr')]
  class(obj) <- c('wizardgtfs','gtfs','list')
  return(obj)
}

unselection.wizardgtfs <- function(obj){
  cat('There is no selection on the object')
  return(obj)
}