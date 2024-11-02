

read_gtfs <- function(file.path, files = NULL, quiet = TRUE, ...){
  
  obj <- gtfsio::import_gtfs(path = file.path,files = files, quiet = quiet, ...)
  obj <- as_wizardgtfs(obj)
  return(obj)
  
}
