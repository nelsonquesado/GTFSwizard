\docType{data}
\name{for_rail_gtfs}
\alias{for_rail_gtfs}
\title{GTFS Data for Fortaleza (Rail System), Brazil}
\format{
An object of class \code{wizardgtfs}, consisting of multiple data frames:
\describe{
  \item{agency}{Data frame with 1 row and 7 columns, providing information about the transit agency, including agency name, URL, timezone, language, and contact details.}
  \item{calendar}{Data frame with 1 row and 10 columns, detailing the service availability by day of the week, along with start and end dates for each service.}
  \item{calendar_dates}{Data frame with 26 rows and 3 columns, listing specific dates and exceptions (e.g., holidays) that modify the usual service pattern.}
  \item{routes}{Data frame with 3 rows and 9 columns, listing route details such as route ID, short and long names, route type, and colors associated with each route.}
  \item{stops}{Data frame with 39 rows and 10 columns, containing information about each stop, including stop ID, name, location (latitude and longitude), and additional details.}
  \item{stop_times}{Data frame with 3,420 rows and 10 columns, detailing arrival and departure times for each trip, along with stop sequences and stop IDs.}
  \item{trips}{Data frame with 215 rows and 7 columns, providing trip-specific information such as trip ID, headsign, direction, associated service ID, route ID, and shape ID.}
  \item{shapes}{Data frame with 80 rows and 5 columns, representing spatial paths of routes using latitude, longitude, point sequence, and cumulative distance traveled.}
}
}
\source{
Cia Cearense de Transportes Metropolitanos (METROFOR).
}
\description{
This dataset contains GTFS (General Transit Feed Specification) data for Fortaleza's rail transit system, managed by METROFOR. The data includes information on routes, trips, stops, stop times, shapes, and other necessary elements for transit analysis and planning.
}
\details{
The GTFS data format is widely adopted for representing public transportation schedules and spatial information. This dataset follows GTFS standards and is tailored for advanced analysis, particularly in transit planning and operations. Key tables included are `agency`, `routes`, `stops`, `stop_times`, `trips`, and `shapes`, each providing essential attributes for a comprehensive transit analysis.
}
\examples{
# Load the dataset
data(for_rail_gtfs)

# Access trips data
head(for_rail_gtfs$trips)

# Access stops data
head(for_rail_gtfs$stops)

}

