#' Read GTFS file
#'
#' Reads GTFS files from a .zip file.
#'
#' @param file.path A path to a .zip GTFS file.
#' @param files A character vector containing the text files to be read from the GTFS zip (without the .txt extension). Defaults to NULL, which reads all files.
#' @param quiet Logical. If TRUE, suppresses messages from gtfsio::import_gtfs(). Defaults to TRUE.
#' @param ... Additional arguments to pass to gtfsio::import_gtfs().
#'
#' @details
#' If no specific files are indicated, all GTFS files within the zip archive are read. After importing, the function converts the GTFS data into a `wizardgtfs` object, which is tailored for efficient handling and analysis of transit data.
#'
#' @return A `wizardgtfs` object: a list of tibbles representing each text file in the .zip and a tibble for services by date.
#'
#' @note
#' Additional notes can be added here if needed.
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()]
#'
#' @examples
#' \dontrun{
#' gtfs_data <- read_gtfs("path/to/gtfs.zip")
#' }
#'
#' @export



read_gtfs <- function(file.path, files = NULL, quiet = TRUE, ...){

  obj <- gtfsio::import_gtfs(path = file.path,files = files, quiet = quiet, ...)
  obj <- as_wizardgtfs(obj)
  return(obj)

}
