\name{filter_functions}
\alias{filter_servicepattern}
\alias{filter_date}
\alias{filter_service}
\alias{filter_route}
\alias{filter_trip}
\alias{filter_stop}
\alias{filter_time}
\title{Filter GTFS Data by Service, Route, Date, Stop, Trip, and Time}
\usage{
filter_servicepattern(gtfs, servicepattern = NULL)

filter_date(gtfs, dates = NULL)

filter_service(gtfs, service)

filter_route(gtfs, route, keep = TRUE)

filter_trip(gtfs, trip, keep = TRUE)

filter_stop(gtfs, stop)

filter_time(gtfs, from = "0:0:0", to = "48:00:00")
}
\arguments{
\item{gtfs}{A GTFS object, preferably of class `wizardgtfs`. If not, the function will attempt to convert it using `GTFSwizard::as_wizardgtfs()`.}

\item{servicepattern}{(Optional) A character vector of service patterns to retain. Defaults to the most frequent pattern (typical day) if `NULL`.}

\item{dates}{(Optional) A date or vector of dates (as "YYYY-MM-DD" character or POSIXct) to filter services active on those dates. Return the furtherst available date if `NULL`.}

\item{service}{(Optional) A character vector of service IDs to retain in the `wizardgtfs` object.}

\item{route}{(Optional) A character vector of route IDs to retain in the `wizardgtfs` object. When `keep = FALSE`, excludes the specified routes.}

\item{keep}{Logical. When `TRUE` (default), retains specified `route` or `trip` IDs; when `FALSE`, excludes them.}

\item{trip}{(Optional) A character vector of trip IDs to retain in the `wizardgtfs` object. When `keep = FALSE`, excludes the specified trips.}

\item{stop}{(Optional) A character vector of stop IDs to retain.}

\item{from}{(Optional) Start time in "HH:MM:SS" format to include only trips that start after this time. Defaults to `0:0:0`.}

\item{to}{(Optional) End time in "HH:MM:SS" format to include only trips that end before this time. Defaults to `48:00:00`.}
}
\value{
A filtered `wizardgtfs` object containing only the records that match the specified criteria.
}
\description{
The `filter_` functions selectively filter data within a `wizardgtfs` object based on criteria such as service patterns, specific dates, service IDs, route IDs, trip IDs, stop IDs, or time ranges.
}
\details{
Each `filter_` function targets a specific aspect of the GTFS data, applying filters to the relevant tables:

- filter_servicepattern: Filters by specified service patterns in the GTFS data. If no pattern is provided, defaults to the most frequent one.

- filter_date: Filters data by a date or dates, returning only services active on those dates.

- filter_service: Filters by service ID, retaining data related to specified services.

- filter_route: Filters by route ID. When `keep = TRUE`, only specified routes are retained; when `FALSE`, the specified routes are excluded.

- filter_trip: Filters by trip ID, using `keep` to either retain or exclude specified trips.

- filter_stop: Filters by stop ID, retaining only stops and related data (trips, routes, etc.) associated with the specified stops.

- filter_time: Filters stop times within a specified time range (between `from` and `to`).

These functions selectively subset the GTFS tables (`trips`, `stop_times`, `routes`, `agency`, `shapes`, etc.), maintaining only the records that meet the defined criteria. If a table or required column is missing from the GTFS data, the function will either attempt to infer it using available data or exclude the table as necessary.
}
\examples{
# Filter by service pattern
filtered_gtfs <- filter_servicepattern(gtfs = for_rail_gtfs, servicepattern = "servicepattern-1")

# Filter by a specific date
filtered_gtfs <- filter_date(gtfs = for_rail_gtfs, dates = "2021-02-10")

# Filter by route ID, keeping only specified routes
filtered_gtfs <- filter_route(gtfs = for_rail_gtfs, route = for_rail_gtfs$routes$route_id[1:2])

# Filter by trip ID, excluding specified trips
filtered_gtfs <- filter_trip(gtfs = for_rail_gtfs,
                              trip = for_rail_gtfs$trips$trip_id[1:2],
                              keep = FALSE)

# Filter by a time range
filtered_gtfs <- filter_time(gtfs = for_rail_gtfs, from = "06:30:00", to = "10:00:00")

}
\seealso{
[GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
}
