devtools::load_all(quiet = TRUE)

arguments <- commandArgs(trailingOnly = TRUE)
source_zip <- if(length(arguments)) arguments[1] else {
  file.path("data", "es-barcelona-autos-castellbisbal-gtfs-892.zip")
}
bcn_bus_gtfs <- read_gtfs(source_zip, quiet = FALSE)
save(bcn_bus_gtfs, file = file.path("data", "bcn_bus_gtfs.rda"),
     compress = "xz", compression_level = 9)
print(vapply(bcn_bus_gtfs, nrow, integer(1)))
print(range(bcn_bus_gtfs$dates_services$date))
