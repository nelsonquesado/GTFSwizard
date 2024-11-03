#' Read and Convert GTFS Data to `wizardgtfs` Format
#'
#' `read_gtfs` reads a GTFS feed from a zip file and converts it into a `wizardgtfs` object, suitable for transit data analysis.
#'
#' @param file.path A string specifying the path to the GTFS zip file.
#' @param files A character vector indicating specific GTFS files to read. If `NULL`, all files in the GTFS feed will be read.
#' @param quiet A logical value indicating whether to suppress messages and warnings during the import. Defaults to `TRUE`.
#' @param ... Additional arguments.
#'
#' @return An object of class `wizardgtfs`, containing GTFS data as a list of data frames.
#'
#' @details
#' If no specific files are indicated, all GTFS files within the zip archive are read. After importing, the function converts the GTFS data into a `wizardgtfs` object, which is tailored for efficient handling and analysis of transit data.
#'
#' @examples
#' \dontrun{
#' # Read a GTFS file and convert it to a wizardgtfs object
#' gtfs_data <- read_gtfs("path/to/gtfs.zip")
#'
#' # Read only specific files from the GTFS feed
#' gtfs_data <- read_gtfs("path/to/gtfs.zip", files = c("stops.txt", "routes.txt"))
#' }
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::write_gtfs()]
#'
#' @export
read_gtfs <- function(file.path, files = NULL, quiet = TRUE, ...){

  obj <- gtfsio::import_gtfs(path = file.path,files = files, quiet = quiet, ...)
  obj <- as_wizardgtfs(obj)
  return(obj)

}
