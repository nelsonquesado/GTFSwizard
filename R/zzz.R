.onAttach <- function(libname, pkgname) {
  check_pkg_all()
  packageStartupMessage(GTFSwizard.StartupMessage())
}

# Define check_pkg_all to ensure all required packages are available
check_pkg_all <- function() {
  required_packages <- c("gtfstools", "tidytransit", "lubridate", "sf",
                         "tidyr", "data.table", "tibble", "shiny",
                         "plotly", "leaflet", "leaflet.extras",
                         "crayon", "geosphere", "stplanr", "hrbrthemes",
                         "checkmate", "dplyr", "ggplot2", "glue",
                         "gtfsio", "hms", "purrr", "rlang",
                         "sfnetworks", "stringr")

  # Apply check to each package
  lapply(required_packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is needed for this package to work. Please install it.")
    }
  })

  invisible(suppressMessages(suppressWarnings(
    try(detach("package:tidylog", unload = TRUE),silent = T)
  )))
  if(interactive())
    packageStartupMessage(GTFSwizard.StartupMessage())

}
