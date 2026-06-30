#' Explore GTFS Data in an Interactive Shiny Dashboard
#'
#' Opens a lightweight Shiny dashboard for exploring a GTFS feed. The dashboard
#' shows summary cards, route and stop maps, service calendar, and key
#' operational charts. Route, service, service-pattern, stop, date, and time
#' filters update the dashboard without changing the original object.
#'
#' @param gtfs A GTFS object, preferably of class `wizardgtfs`. When omitted
#'   or `NULL` in an interactive session, a file-selection window opens so the
#'   user can choose a GTFS `.zip` archive.
#' @param plotly Logical. If `TRUE`, dashboard ggplot panels are rendered with
#'   interactive plotly widgets. `plotly` is optional and only required when
#'   this argument is `TRUE`.
#'
#' @return A Shiny app object.
#'
#' @examples
#' if (interactive()) {
#'   explore_gtfs()
#'   explore_gtfs(GTFSwizard::for_rail_gtfs)
#' }
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_shapes()], [GTFSwizard::plot_calendar()]
#'
#' @references
#' Ceder, A. (2007). *Public Transit Planning and Operation*.
#' Vuchic, V. R. (2005). *Urban Transit: Operations, Planning, and Economics*.
#' White, P. (2008). *Public Transport: Its Planning, Management and Operation*.
#' Cascetta, E. (2009). *Transportation Systems Analysis*.
#'
#' @export
explore_gtfs <- function(gtfs = NULL, plotly = FALSE){
  require_pkg("shiny", "`explore_gtfs()`")
  require_pkg("leaflet", "`explore_gtfs()`")
  gw_assert_flag(plotly, "plotly")
  if(isTRUE(plotly)){
    require_pkg("plotly", "`explore_gtfs(plotly = TRUE)`")
  }
  feed_name <- deparse(substitute(gtfs))
  if(is.null(gtfs)){
    if(!interactive()){
      gw_stop("`gtfs` is required in non-interactive sessions.")
    }
    selected_file <- tryCatch(
      file.choose(),
      error = function(error){
        gw_stop("no GTFS file was selected.")
      }
    )
    gtfs <- GTFSwizard::read_gtfs(selected_file)
    attr(gtfs, "gw_source_name") <- basename(selected_file)
    return(explore_gtfs.wizardgtfs(gtfs, plotly = plotly))
  }
  if(!identical(feed_name, "gtfs")){
    attr(gtfs, "gw_source_name") <- feed_name
  }
  UseMethod('explore_gtfs')
}

#' @exportS3Method GTFSwizard::explore_gtfs list
explore_gtfs.list <- function(gtfs, plotly = FALSE){
  gtfs <- ensure_wizardgtfs(gtfs)
  explore_gtfs.wizardgtfs(gtfs, plotly = plotly)
}

#' @exportS3Method GTFSwizard::explore_gtfs wizardgtfs
explore_gtfs.wizardgtfs <- function(gtfs, plotly = FALSE){
  gw_assert_flag(plotly, "plotly")
  if(isTRUE(plotly)){
    require_pkg("plotly", "`explore_gtfs(plotly = TRUE)`")
  }
  gtfs <- ensure_shapes(ensure_wizardgtfs(gtfs))
  feed_name <- attr(gtfs, "gw_source_name", exact = TRUE)
  if(is.null(feed_name) || !nzchar(feed_name)){
    feed_name <- "GTFS object"
  }

  all_routes <- sort(unique(as.character(gtfs$routes$route_id)))
  patterns_table <- GTFSwizard::get_servicepattern(gtfs)
  all_patterns <- as.character(patterns_table$service_pattern)
  all_services <- sort(unique(as.character(gtfs$trips$service_id)))
  stop_labels <- if("stop_name" %in% names(gtfs$stops)){
    paste(gtfs$stops$stop_id, "-", gtfs$stops$stop_name)
  } else {
    as.character(gtfs$stops$stop_id)
  }
  stop_choices <- stats::setNames(
    as.character(gtfs$stops$stop_id), stop_labels
  )
  date_range <- range(as.Date(gtfs$dates_services$date))
  stop_time_seconds <- gtfs_time_to_seconds(c(
    gtfs$stop_times$arrival_time, gtfs$stop_times$departure_time
  ))
  hour_limits <- range(floor(stop_time_seconds[is.finite(stop_time_seconds)] / 3600))
  if(!all(is.finite(hour_limits))){
    hour_limits <- c(0, 24)
  }
  hour_limits[1] <- max(0, hour_limits[1])
  hour_limits[2] <- max(hour_limits[1] + 1, min(48, hour_limits[2]))

  css <- "
    body { background: #f6f8fb; }
    .gw-title { margin: 10px 18px 0; color: #1f2d3a; font-weight: 700; }
    .gw-page { padding: 18px; }
    .gw-card {
      background: #fff; border: 1px solid #d9e1ea; border-radius: 6px;
      padding: 14px; margin-bottom: 14px;
    }
    .gw-stat { font-size: 28px; font-weight: 700; line-height: 1; color: #17456b; }
    .gw-label { color: #5f6f7d; font-size: 12px; text-transform: uppercase; }
    .gw-feed-info { color: #314457; font-size: 14px; }
    .gw-help { color: #667785; font-size: 12px; margin-top: -4px; }
    .gw-map { border: 1px solid #d9e1ea; border-radius: 6px; overflow: hidden; }
    .gw-table-scroll { overflow-x: auto; max-height: 360px; overflow-y: auto; }
    .selectize-control.multi .selectize-input {
      max-height: 92px; overflow-x: hidden; overflow-y: auto;
    }
    .form-group { margin-bottom: 10px; }
    @media (max-width: 767px) {
      .gw-page { padding: 12px; }
      .gw-stats .col-sm-2 { float: left; width: 50%; }
      .gw-stat { font-size: 24px; }
    }
  "

  stat_card <- function(label, value){
    shiny::div(class = "gw-card", shiny::div(class = "gw-stat", value), shiny::div(class = "gw-label", label))
  }
  value_or <- function(x, y){
    if(is.null(x) || !length(x) || anyNA(x)) y else x
  }
  route_plot_height <- function(n, base = 520L){
    paste0(max(base, 220L + as.integer(n) * 18L), "px")
  }
  pattern_plot_height <- function(n, base = 520L){
    paste0(max(base, 300L + as.integer(n) * 18L), "px")
  }
  route_pattern_plot_height <- function(routes, patterns, base = 520L){
    rows <- max(1L, as.integer(routes)) * max(1L, as.integer(patterns))
    paste0(max(base, 240L + rows * 16L), "px")
  }
  calendar_plot_height <- function(g){
    years <- unique(format(as.Date(g$dates_services$date), "%Y"))
    paste0(max(520L, length(years) * 260L), "px")
  }
  plot_ui <- function(output_id, height = "520px", interactive = TRUE){
    if(isTRUE(plotly) && isTRUE(interactive)){
      plotly::plotlyOutput(output_id, height = height)
    } else {
      shiny::plotOutput(output_id, height = height)
    }
  }
  render_plot <- function(expr, interactive = TRUE){
    expr <- substitute(expr)
    env <- parent.frame()
    if(isTRUE(plotly) && isTRUE(interactive)){
      plotly::renderPlotly({
        suppressMessages(suppressWarnings(plotly::ggplotly(eval(expr, env))))
      })
    } else {
      shiny::renderPlot(eval(expr, env))
    }
  }
  export_filename <- function(name){
    name <- gsub("\\.zip$", "", basename(name), ignore.case = TRUE)
    name <- gsub("[^A-Za-z0-9_-]+", "-", name)
    name <- gsub("(^-+|-+$)", "", name)
    if(!nzchar(name)) name <- "gtfswizard-feed"
    paste0(name, "-filtered.zip")
  }
  choose_export_directory <- function(){
    if(requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()){
      path <- tryCatch(
        rstudioapi::selectDirectory(
          caption = "Choose export folder",
          label = "Choose",
          path = getwd()
        ),
        error = function(error) character()
      )
      if(!is.null(path) && length(path) && nzchar(path)){
        return(path)
      }
    }
    if(.Platform$OS.type == "windows"){
      path <- utils::choose.dir(default = getwd(), caption = "Choose export folder")
      if(is.na(path)) return(character())
      return(path)
    }
    if(Sys.info()[["sysname"]] != "Darwin" &&
       requireNamespace("tcltk", quietly = TRUE)){
      path <- tcltk::tk_choose.dir(default = getwd(), caption = "Choose export folder")
      if(is.na(path) || !nzchar(path)) return(character())
      return(path)
    }
    getwd()
  }

  ui <- shiny::fluidPage(
    class = "gw-page",
    shiny::tags$head(shiny::tags$style(css)),
    shiny::h2(class = "gw-title", "GTFSwizard Explorer"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::selectizeInput(
          "selected_routes",
          "Routes",
          choices = NULL,
          selected = character(),
          multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "All routes")
        ),
        shiny::selectizeInput(
          "selected_patterns",
          "Service patterns",
          choices = NULL,
          selected = character(),
          multiple = TRUE,
          options = list(
            plugins = list("remove_button"),
            placeholder = paste("All", length(all_patterns), "service patterns")
          )
        ),
        shiny::selectizeInput(
          "selected_services",
          "Services",
          choices = NULL,
          selected = character(),
          multiple = TRUE,
          options = list(
            plugins = list("remove_button"),
            placeholder = paste("All", length(all_services), "services")
          )
        ),
        shiny::selectizeInput(
          "selected_stops",
          "Stops",
          choices = NULL,
          selected = character(),
          multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "All stops")
        ),
        shiny::dateRangeInput(
          "selected_dates",
          "Service dates",
          start = date_range[1],
          end = date_range[2],
          min = date_range[1],
          max = date_range[2]
        ),
        shiny::sliderInput(
          "selected_hours",
          "Scheduled hours",
          min = hour_limits[1],
          max = hour_limits[2],
          value = hour_limits,
          step = 1
        ),
        shiny::numericInput(
          "plot_pattern_top_n",
          "Service patterns shown in plots",
          value = max(1L, min(12L, length(all_patterns))),
          min = 1L,
          max = max(1L, length(all_patterns)),
          step = 1L
        ),
        shiny::div(
          class = "gw-card",
          shiny::h4("Export"),
          shiny::textInput(
            "export_directory",
            "Folder",
            value = getwd()
          ),
          shiny::actionButton("choose_export_directory", "Choose folder"),
          shiny::textInput(
            "export_filename",
            "File name",
            value = export_filename(feed_name)
          ),
          shiny::actionButton("export_gtfs", "Export GTFS"),
          shiny::div(class = "gw-help", shiny::textOutput("export_status"))
        ),
        shiny::div(
          class = "gw-card",
          shiny::h4("Routes"),
          shiny::div(
            class = "gw-table-scroll",
            shiny::tableOutput("route_table")
          )
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::uiOutput("summary_cards"),
        shiny::tabsetPanel(
          id = "dashboard_view",
          shiny::tabPanel(
            "Network",
            shiny::div(
              class = "gw-map",
              leaflet::leafletOutput("network_map", height = "66vh")
            )
          ),
          shiny::tabPanel(
            "Service",
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Frequency",
                shiny::div(
                  class = "gw-card",
                  plot_ui(
                    "frequency_plot",
                    height = "520px",
                    interactive = FALSE
                  )
                )
              ),
              shiny::tabPanel(
                "Headways",
                shiny::div(
                  class = "gw-card",
                  shiny::uiOutput("headway_plot_ui")
                )
              ),
              shiny::tabPanel(
                "Route frequency",
                shiny::div(
                  class = "gw-card",
                  shiny::numericInput(
                    "route_frequency_top_n",
                    "Routes to show",
                    value = 25L,
                    min = 1L,
                    max = length(all_routes),
                    step = 1L
                  ),
                  shiny::uiOutput("route_frequency_plot_ui")
                )
              ),
              shiny::tabPanel(
                "Service span",
                shiny::div(
                  class = "gw-card",
                  shiny::numericInput(
                    "service_span_top_n",
                    "Routes to show",
                    value = 25L,
                    min = 1L,
                    max = length(all_routes),
                    step = 1L
                  ),
                  shiny::uiOutput("service_span_plot_ui")
                )
              ),
              shiny::tabPanel(
                "Weekday and hour",
                shiny::div(
                  class = "gw-card",
                  plot_ui("service_heatmap_plot", height = "520px")
                )
              ),
              shiny::tabPanel(
                "Fleet",
                shiny::div(
                  class = "gw-card",
                  shiny::uiOutput("fleet_plot_ui")
                )
              )
            )
          ),
          shiny::tabPanel(
            "Performance",
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Speed",
                shiny::div(
                  class = "gw-card",
                  plot_ui(
                    "speed_plot",
                    height = "520px",
                    interactive = FALSE
                  )
                )
              ),
              shiny::tabPanel(
                "Dwell time",
                shiny::div(
                  class = "gw-card",
                  plot_ui(
                    "dwell_plot",
                    height = "520px",
                    interactive = FALSE
                  )
                )
              ),
              shiny::tabPanel(
                "Trip duration",
                shiny::div(
                  class = "gw-card",
                  shiny::numericInput(
                    "route_duration_top_n",
                    "Routes to show",
                    value = 15L,
                    min = 1L,
                    max = length(all_routes),
                    step = 1L
                  ),
                  shiny::uiOutput("route_duration_plot_ui")
                )
              ),
              shiny::tabPanel(
                "Service supply",
                shiny::div(
                  class = "gw-card",
                  shiny::numericInput(
                    "service_supply_top_n",
                    "Routes to show",
                    value = 15L,
                    min = 1L,
                    max = length(all_routes),
                    step = 1L
                  ),
                  shiny::uiOutput("service_supply_plot_ui")
                )
              )
            )
          ),
          shiny::tabPanel(
            "Calendar",
            shiny::div(
              class = "gw-card",
              shiny::checkboxInput(
                "calendar_fill_pattern",
                "Fill by service pattern",
                value = FALSE
              ),
              shiny::uiOutput("calendar_plot_ui")
            )
          ),
          shiny::tabPanel(
            "Planning indicators",
            shiny::div(
              class = "gw-card",
              shiny::h4("System indicators"),
              shiny::tableOutput("planning_system_table")
            ),
            shiny::div(
              class = "gw-card",
              shiny::numericInput(
                "planning_top_n",
                "Route-direction-pattern rows to show",
                value = 30L,
                min = 1L,
                max = 200L,
                step = 1L
              ),
              shiny::h4("Route indicators"),
              shiny::div(
                class = "gw-table-scroll",
                shiny::tableOutput("planning_route_table")
              )
            )
          ),
          shiny::tabPanel(
            "Corridors and hubs",
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Corridors",
                shiny::div(
                  class = "gw-card",
                  shiny::fluidRow(
                    shiny::column(
                      4,
                      shiny::numericInput(
                        "corridor_i",
                        "Corridor share",
                        value = 0.01,
                        min = 0.001,
                        max = 1,
                        step = 0.005
                      ),
                      shiny::div(
                        class = "gw-help",
                        "Share of the highest-frequency stop-to-stop links retained as corridor candidates."
                      )
                    ),
                    shiny::column(
                      4,
                      shiny::numericInput(
                        "corridor_min_length",
                        "Minimum length (m)",
                        value = 1500,
                        min = 0,
                        step = 100
                      ),
                      shiny::div(
                        class = "gw-help",
                        "Shortest corridor candidate to display after connected high-frequency links are joined."
                      )
                    )
                  ),
                  plot_ui(
                    "corridor_plot",
                    height = "620px",
                    interactive = FALSE
                  )
                )
              ),
              shiny::tabPanel(
                "Hubs",
                shiny::div(
                  class = "gw-card",
                  shiny::fluidRow(
                    shiny::column(
                      4,
                      shiny::numericInput(
                        "hubs_i",
                        "Hub share",
                        value = 0.05,
                        min = 0.001,
                        max = 1,
                        step = 0.005
                      ),
                      shiny::div(
                        class = "gw-help",
                        "Share of the busiest stops retained as hub candidates."
                      )
                    )
                  ),
                  plot_ui(
                    "hubs_plot",
                    height = "620px",
                    interactive = FALSE
                  )
                )
              )
            )
          ),
          shiny::tabPanel(
            "Agency",
            shiny::div(
              class = "gw-card",
              shiny::h4("Agency"),
              shiny::div(
                class = "gw-table-scroll",
                shiny::tableOutput("agency_table")
              )
            )
          ),
          shiny::tabPanel(
            "Edit",
            shiny::div(
              class = "gw-card",
              shiny::h4("Trips"),
              shiny::selectizeInput(
                "edit_trips",
                "Trips for edits",
                choices = NULL,
                selected = character(),
                multiple = TRUE,
                options = list(
                  plugins = list("remove_button"),
                  placeholder = "All retained trips"
                )
              ),
              shiny::div(
                class = "gw-help",
                "Blank applies speed, dwell-time, and delay edits to all retained trips. Split requires explicit trip selection."
              )
            ),
            shiny::div(
              class = "gw-card",
              shiny::h4("Stops"),
              shiny::selectizeInput(
                "edit_stops",
                "Stops for speed/dwell edits",
                choices = NULL,
                selected = character(),
                multiple = TRUE,
                options = list(
                  plugins = list("remove_button"),
                  placeholder = "All retained stops"
                )
              ),
              shiny::div(
                class = "gw-help",
                "Blank applies speed and dwell-time edits to all retained stops."
              )
            ),
            shiny::div(
              class = "gw-card",
              shiny::h4("Speed"),
              shiny::checkboxInput("edit_speed_enabled", "Apply speed multiplier", FALSE),
              shiny::numericInput(
                "edit_speed_factor",
                "Speed multiplier",
                value = 1.25,
                min = 0.01,
                step = 0.05
              ),
              shiny::div(
                class = "gw-help",
                "Applies edit_speed() to all trips and stops retained by the current filters."
              )
            ),
            shiny::div(
              class = "gw-card",
              shiny::h4("Dwell time"),
              shiny::checkboxInput("edit_dwell_enabled", "Apply dwell-time multiplier", FALSE),
              shiny::numericInput(
                "edit_dwell_factor",
                "Dwell-time multiplier",
                value = 1.5,
                min = 0,
                step = 0.1
              ),
              shiny::checkboxInput("set_dwell_enabled", "Set dwell time", FALSE),
              shiny::numericInput(
                "set_dwell_duration",
                "Dwell time (seconds)",
                value = 30,
                min = 0,
                step = 5
              ),
              shiny::div(
                class = "gw-help",
                "Applies edit_dwelltime() and set_dwelltime() to all retained trip-stop calls."
              )
            ),
            shiny::div(
              class = "gw-card",
              shiny::h4("Delay"),
              shiny::checkboxInput("delay_enabled", "Shift selected trips in time", FALSE),
              shiny::numericInput(
                "delay_duration",
                "Shift duration (seconds)",
                value = 300,
                step = 60
              ),
              shiny::div(
                class = "gw-help",
                "Applies delay_trip(). Positive values delay trips; negative values advance trips when valid."
              )
            ),
            shiny::div(
              class = "gw-card",
              shiny::h4("Split"),
              shiny::checkboxInput("split_enabled", "Split selected trips", FALSE),
              shiny::numericInput(
                "split_count",
                "Split points",
                value = 1L,
                min = 1L,
                step = 1L
              ),
              shiny::div(
                class = "gw-help",
                "Applies split_trip(). Each selected trip is split into split + 1 valid consecutive parts."
              )
            ),
            shiny::div(
              class = "gw-card",
              shiny::h4("Current edit state"),
              shiny::verbatimTextOutput("edit_status")
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session){
    shiny::updateSelectizeInput(
      session, "selected_routes", choices = all_routes, server = TRUE
    )
    shiny::updateSelectizeInput(
      session, "selected_patterns", choices = all_patterns, server = TRUE
    )
    shiny::updateSelectizeInput(
      session, "selected_services", choices = all_services, server = TRUE
    )
    shiny::updateSelectizeInput(
      session, "selected_stops", choices = stop_choices, server = TRUE
    )

    filtered_gtfs <- shiny::reactive({
      g <- gtfs
      routes <- input$selected_routes
      if(!is.null(routes) && length(routes)){
        g <- GTFSwizard::filter_route(g, routes)
      }
      services <- input$selected_services
      if(is.null(services) || !length(services)){
        services <- all_services
      }
      patterns <- input$selected_patterns
      if(is.null(patterns) || !length(patterns)){
        patterns <- all_patterns
      }
      pattern_services <- patterns_table$service_id[
        patterns_table$service_pattern %in% patterns
      ]
      selected_services <- intersect(services, pattern_services)
      available_services <- intersect(selected_services, unique(g$trips$service_id))
      if(length(available_services)){
        g <- GTFSwizard::filter_service(g, available_services)
      } else {
        g <- prune_gtfs(g, character())
      }
      dates <- input$selected_dates
      if(!is.null(dates) && length(dates) == 2L && !anyNA(dates)){
        selected_dates <- seq(as.Date(dates[1]), as.Date(dates[2]), by = "day")
        if(!identical(range(selected_dates), date_range)){
          selected_dates <- selected_dates[
            selected_dates %in% as.Date(g$dates_services$date)
          ]
          g <- if(length(selected_dates)){
            GTFSwizard::filter_date(g, selected_dates)
          } else {
            prune_gtfs(g, character())
          }
        }
      }
      stops <- input$selected_stops
      if(!is.null(stops) && length(stops)){
        available_stops <- intersect(stops, unique(g$stops$stop_id))
        g <- if(length(available_stops)){
          GTFSwizard::filter_stop(g, available_stops)
        } else {
          prune_gtfs(g, character())
        }
      }
      hours <- input$selected_hours
      if(!is.null(hours) && length(hours) == 2L &&
         !identical(as.numeric(hours), as.numeric(hour_limits))){
        g <- GTFSwizard::filter_time(
          g,
          from = seconds_to_gtfs_time(hours[1] * 3600),
          to = seconds_to_gtfs_time(hours[2] * 3600)
        )
      }
      g
    })

    edit_trip_choices <- shiny::reactive({
      g <- filtered_gtfs()
      trip_ids <- sort(unique(as.character(g$trips$trip_id)))
      stats::setNames(trip_ids, trip_ids)
    })

    edit_stop_choices <- shiny::reactive({
      g <- filtered_gtfs()
      stops <- g$stops
      if(!nrow(stops)){
        return(stats::setNames(character(), character()))
      }
      stop_ids <- as.character(stops$stop_id)
      labels <- stop_ids
      if("stop_name" %in% names(stops)){
        stop_names <- as.character(stops$stop_name)
        labels <- ifelse(
          nzchar(stop_names),
          paste0(stop_ids, " - ", stop_names),
          stop_ids
        )
      }
      stats::setNames(stop_ids, labels)
    })

    shiny::observeEvent(edit_trip_choices(), {
      choices <- edit_trip_choices()
      selected <- intersect(shiny::isolate(input$edit_trips), unname(choices))
      shiny::updateSelectizeInput(
        session, "edit_trips", choices = choices, selected = selected,
        server = TRUE
      )
    }, ignoreNULL = FALSE)

    shiny::observeEvent(edit_stop_choices(), {
      choices <- edit_stop_choices()
      selected <- intersect(shiny::isolate(input$edit_stops), unname(choices))
      shiny::updateSelectizeInput(
        session, "edit_stops", choices = choices, selected = selected,
        server = TRUE
      )
    }, ignoreNULL = FALSE)

    selected_edit_trips <- function(g, require_selected = FALSE){
      selected <- input$edit_trips
      available <- unique(as.character(g$trips$trip_id))
      if(is.null(selected) || !length(selected)){
        if(require_selected){
          shiny::validate(shiny::need(
            FALSE,
            "Select at least one trip in the Edit tab before splitting trips."
          ))
        }
        return(available)
      }
      selected <- intersect(as.character(selected), available)
      shiny::validate(shiny::need(
        length(selected) > 0,
        "Selected edit trips are not available after the current filters."
      ))
      selected
    }

    selected_edit_stops <- function(g){
      selected <- input$edit_stops
      available <- unique(as.character(g$stops$stop_id))
      if(is.null(selected) || !length(selected)){
        return(available)
      }
      selected <- intersect(as.character(selected), available)
      shiny::validate(shiny::need(
        length(selected) > 0,
        "Selected edit stops are not available after the current filters."
      ))
      selected
    }

    validate_split_request <- function(g, trip_ids, split){
      stop_counts <- table(g$stop_times$trip_id[g$stop_times$trip_id %in% trip_ids])
      missing <- setdiff(trip_ids, names(stop_counts))
      shiny::validate(shiny::need(
        !length(missing),
        "Selected split trips have no retained stop-time records."
      ))
      limits <- stats::setNames(
        pmax(0L, as.integer(stop_counts) - 2L),
        names(stop_counts)
      )
      too_short <- names(limits)[limits < split]
      shiny::validate(shiny::need(
        !length(too_short),
        paste0(
          "`split` is too large for selected trip(s): ",
          paste0(too_short, " (maximum ", limits[too_short], ")", collapse = ", "),
          "."
        )
      ))
      invisible(TRUE)
    }

    edited_gtfs <- shiny::reactive({
      g <- filtered_gtfs()
      if(isTRUE(input$edit_speed_enabled)){
        factor <- as.numeric(value_or(input$edit_speed_factor, 1))
        shiny::validate(shiny::need(
          is.finite(factor) && factor > 0,
          "Speed multiplier must be greater than zero."
        ))
        trip_ids <- selected_edit_trips(g, require_selected = FALSE)
        stop_ids <- selected_edit_stops(g)
        g <- suppressMessages(GTFSwizard::edit_speed(
          g, trips = trip_ids, stops = stop_ids, factor = factor
        ))
      }
      if(isTRUE(input$edit_dwell_enabled)){
        factor <- as.numeric(value_or(input$edit_dwell_factor, 1))
        shiny::validate(shiny::need(
          is.finite(factor) && factor >= 0,
          "Dwell-time multiplier must be non-negative."
        ))
        trip_ids <- selected_edit_trips(g, require_selected = FALSE)
        stop_ids <- selected_edit_stops(g)
        g <- suppressMessages(GTFSwizard::edit_dwelltime(
          g, trips = trip_ids, stops = stop_ids, factor = factor
        ))
      }
      if(isTRUE(input$set_dwell_enabled)){
        duration <- as.numeric(value_or(input$set_dwell_duration, 30))
        shiny::validate(shiny::need(
          is.finite(duration) && duration >= 0,
          "Dwell time must be non-negative."
        ))
        trip_ids <- selected_edit_trips(g, require_selected = FALSE)
        stop_ids <- selected_edit_stops(g)
        g <- suppressMessages(GTFSwizard::set_dwelltime(
          g, duration = duration, trips = trip_ids, stops = stop_ids
        ))
      }
      if(isTRUE(input$delay_enabled)){
        duration <- as.numeric(value_or(input$delay_duration, 300))
        shiny::validate(shiny::need(
          is.finite(duration),
          "Delay duration must be one finite number of seconds."
        ))
        trip_ids <- selected_edit_trips(g, require_selected = FALSE)
        shiny::validate(shiny::need(
          length(trip_ids) > 0,
          "No trips are available to delay after the current filters."
        ))
        g <- suppressMessages(GTFSwizard::delay_trip(
          g, trip = trip_ids, duration = duration
        ))
      }
      if(isTRUE(input$split_enabled)){
        split_value <- as.numeric(value_or(input$split_count, 1L))
        split <- as.integer(split_value)
        shiny::validate(shiny::need(
          is.finite(split_value) && split_value == split && split >= 1L,
          "Split points must be a positive integer."
        ))
        trip_ids <- selected_edit_trips(g, require_selected = TRUE)
        validate_split_request(g, trip_ids, split)
        g <- suppressMessages(GTFSwizard::split_trip(
          g, trip = trip_ids, split = split
        ))
      }
      g
    })

    dashboard_gtfs <- function(){
      g <- edited_gtfs()
      shiny::validate(
        shiny::need(nrow(g$trips) > 0, "No trips match the selected filters."),
        shiny::need(nrow(g$stop_times) > 0, "No stop calls match the selected filters.")
      )
      g
    }
    plot_gtfs <- function(){
      g <- dashboard_gtfs()
      top_n <- as.integer(value_or(input$plot_pattern_top_n, 12L))
      patterns <- GTFSwizard::get_servicepattern(g)
      pattern_order <- patterns |>
        dplyr::arrange(dplyr::desc(.data$pattern_frequency), .data$service_pattern) |>
        dplyr::pull(.data$service_pattern) |>
        unique()
      selected_patterns <- utils::head(pattern_order, top_n)
      selected_services <- patterns$service_id[
        patterns$service_pattern %in% selected_patterns
      ]
      if(length(selected_services) && length(selected_patterns) < length(unique(patterns$service_pattern))){
        g <- GTFSwizard::filter_service(g, selected_services)
      }
      g
    }
    skip_output <- function(label, error){
      shiny::validate(shiny::need(
        FALSE,
        paste0(label, " skipped: ", conditionMessage(error))
      ))
    }
    safe_output <- function(label, expr){
      tryCatch(
        suppressMessages(force(expr)),
        error = function(error) skip_output(label, error)
      )
    }
    export_message <- shiny::reactiveVal("")

    output$export_status <- shiny::renderText(export_message())

    shiny::observeEvent(input$choose_export_directory, {
      directory <- choose_export_directory()
      if(length(directory) && nzchar(directory)){
        shiny::updateTextInput(session, "export_directory", value = directory)
        export_message(paste("Export folder:", normalizePath(
          directory, winslash = "/", mustWork = FALSE
        )))
      } else {
        export_message("Folder selection cancelled.")
      }
    })

    output$edit_status <- shiny::renderPrint({
      trip_scope <- if(length(input$edit_trips)){
        paste0(length(input$edit_trips), " selected trip(s)")
      } else {
        "all retained trips"
      }
      stop_scope <- if(length(input$edit_stops)){
        paste0(length(input$edit_stops), " selected stop(s)")
      } else {
        "all retained stops"
      }
      edits <- c(
        if(isTRUE(input$edit_speed_enabled)){
          paste0(
            "Speed multiplier: ", value_or(input$edit_speed_factor, 1),
            " for ", trip_scope, " and ", stop_scope
          )
        },
        if(isTRUE(input$edit_dwell_enabled)){
          paste0(
            "Dwell-time multiplier: ", value_or(input$edit_dwell_factor, 1),
            " for ", trip_scope, " and ", stop_scope
          )
        },
        if(isTRUE(input$set_dwell_enabled)){
          paste0(
            "Set dwell time: ", value_or(input$set_dwell_duration, 30),
            " seconds for ", trip_scope, " and ", stop_scope
          )
        },
        if(isTRUE(input$delay_enabled)){
          paste0("Delay/advance: ", value_or(input$delay_duration, 300), " seconds for ", trip_scope)
        },
        if(isTRUE(input$split_enabled)){
          trip_text <- if(length(input$edit_trips)){
            paste0(length(input$edit_trips), " selected trip(s)")
          } else {
            "no trips selected"
          }
          paste0("Split points: ", value_or(input$split_count, 1L), " for ", trip_text)
        }
      )
      if(!length(edits)){
        cat("No edits enabled.\n")
      } else {
        cat(paste(edits, collapse = "\n"), "\n")
      }
      g <- edited_gtfs()
      cat("Current filtered/edited feed:", nrow(g$routes), "routes,",
          nrow(g$trips), "trips,", nrow(g$stops), "stops.\n")
    })

    shiny::observeEvent(input$export_gtfs, {
      tryCatch({
        directory <- value_or(input$export_directory, getwd())
        if(!length(directory) || !nzchar(directory) || !dir.exists(directory)){
          export_message("Choose a valid export folder before saving.")
          shiny::showNotification(export_message(), type = "error")
          return(invisible(NULL))
        }
        filename <- value_or(input$export_filename, export_filename(feed_name))
        if(!grepl("\\.zip$", filename, ignore.case = TRUE)){
          filename <- paste0(filename, ".zip")
        }
        filename <- basename(filename)
        zipfile <- file.path(directory, filename)
        GTFSwizard::write_gtfs(dashboard_gtfs(), zipfile)
        export_message(paste("Saved:", normalizePath(zipfile, winslash = "/", mustWork = FALSE)))
        shiny::showNotification(export_message(), type = "message")
      }, error = function(error){
        message <- paste("Export failed:", conditionMessage(error))
        export_message(message)
        shiny::showNotification(message, type = "error")
      })
    })

    output$summary_cards <- shiny::renderUI({
      g <- edited_gtfs()
      agency_count <- if("agency_id" %in% names(g$agency)){
        length(unique(g$agency$agency_id))
      }else{
        nrow(g$agency)
      }
      shapes_count <- if(!is.null(g$shapes) && "shape_id" %in% names(g$shapes)){
        length(unique(g$shapes$shape_id))
      } else {
        0L
      }
      shiny::tagList(
        shiny::fluidRow(
          class = "gw-stats",
          shiny::column(2, stat_card("Routes", nrow(g$routes))),
          shiny::column(2, stat_card("Trips", nrow(g$trips))),
          shiny::column(2, stat_card("Stops", nrow(g$stops))),
          shiny::column(2, stat_card("Shapes", shapes_count)),
          shiny::column(2, stat_card("Service days", length(unique(g$dates_services$date)))),
          shiny::column(2, stat_card("Agencies", agency_count))
        ),
        shiny::div(
          class = "gw-card gw-feed-info",
          shiny::strong("Feed: "), feed_name
        )
      )
    })

    output$agency_table <- shiny::renderTable({
      edited_gtfs()$agency
    }, striped = TRUE, spacing = "xs")

    output$network_map <- leaflet::renderLeaflet({
      safe_output("Network map", {
        g <- dashboard_gtfs()
      stops_sf <- GTFSwizard::get_stops_sf(g$stops)
      stop_counts <- g$stop_times %>%
        dplyr::group_by(stop_id) %>%
        dplyr::summarise(trips = dplyr::n(), .groups = "drop")
      stops_sf <- stops_sf %>%
        dplyr::left_join(stop_counts, by = "stop_id") %>%
        dplyr::mutate(trips = tidyr::replace_na(trips, 0))
      stops_sf$.popup_name <- if("stop_name" %in% names(stops_sf)){
        as.character(stops_sf$stop_name)
      } else {
        as.character(stops_sf$stop_id)
      }
      has_shapes <- !is.null(g$shapes) && isTRUE(nrow(g$shapes) > 0L) &&
        all(c("shape_id", "shape_pt_lat", "shape_pt_lon") %in% names(g$shapes)) &&
        "shape_id" %in% names(g$trips)
      route_shapes <- NULL
      if(has_shapes){
        shapes_sf <- GTFSwizard::get_shapes_sf(g$shapes)
        route_shapes <- g$trips |>
          dplyr::select(route_id, shape_id) |>
          dplyr::filter(!is.na(shape_id), nzchar(as.character(shape_id))) |>
          dplyr::distinct() |>
          dplyr::left_join(shapes_sf, by = "shape_id") |>
          sf::st_as_sf()
        if(!nrow(route_shapes)){
          route_shapes <- NULL
        }
      }
      map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Light") %>%
        leaflet::addTiles(group = "OSM") %>%
        leaflet::addLayersControl(baseGroups = c("Light", "OSM"))
      if(!is.null(route_shapes)){
        routes <- sort(unique(route_shapes$route_id))
        route_colors <- if(length(routes) <= 20L){
          stats::setNames(gtfswizard_palette(length(routes)), routes)
        } else {
          stats::setNames(rep(gtfswizard_colors()[["blue"]], length(routes)), routes)
        }
        map <- map %>% leaflet::addPolylines(
          data = route_shapes,
          color = unname(route_colors[as.character(route_shapes$route_id)]),
          weight = 4,
          opacity = 0.88,
          popup = ~paste0("Route: ", route_id)
        )
      }
      map %>% leaflet::addCircleMarkers(
          data = stops_sf,
          radius = 2,
          stroke = FALSE,
          fillOpacity = 0.7,
          color = "#20a39e",
          popup = ~paste0(.popup_name, "<br>Trips: ", trips)
        )
      })
    })

    output$frequency_plot <- render_plot({
      safe_output("Frequency plot", GTFSwizard::plot_frequency(dashboard_gtfs()))
    }, interactive = FALSE)

    output$fleet_plot <- render_plot({
      safe_output("Fleet plot", {
      fleet <- GTFSwizard::get_fleet(plot_gtfs(), method = "by_hour") %>%
        dplyr::mutate(hour = as.numeric(hour)) %>%
        dplyr::filter(is.finite(hour), is.finite(fleet))

      ggplot2::ggplot(fleet, ggplot2::aes(hour, fleet, color = service_pattern, group = service_pattern)) +
        ggplot2::geom_line(linewidth = 1) +
        hour_scale(fleet$hour) +
        ggplot2::labs(
          title = "Scheduled Fleet by Hour",
          subtitle = "Each line is one service pattern",
          x = "Scheduled clock hour",
          y = "Simultaneously active scheduled trips (vehicles)",
          color = "Service pattern"
        ) +
        theme_gtfswizard()
      })
    })
    output$fleet_plot_ui <- shiny::renderUI({
      n <- length(unique(GTFSwizard::get_servicepattern(plot_gtfs())$service_pattern))
      plot_ui("fleet_plot", height = pattern_plot_height(n, 520L))
    })

    output$speed_plot <- render_plot({
      safe_output("Speed plot", {
      speed <- GTFSwizard::get_speeds(dashboard_gtfs(), method = "by_route") %>%
        dplyr::filter(
          is.finite(average.speed), is.finite(trips),
          is.finite(pattern_frequency)
        )
      ggplot2::ggplot(speed, ggplot2::aes(average.speed, weight = trips * pattern_frequency)) +
        ggplot2::geom_histogram(fill = gtfswizard_colors()[["blue"]], color = "white", bins = 25) +
        ggplot2::labs(
          title = "Scheduled Route Speed Distribution",
          subtitle = "Each observation is a route-service pattern summary",
          x = "Average scheduled speed (km/h)",
          y = "Weighted route-pattern observations"
        ) +
        theme_gtfswizard()
      })
    }, interactive = FALSE)

    output$headway_plot <- render_plot({
      safe_output("Headway plot", GTFSwizard::plot_headways(plot_gtfs()))
    })
    output$headway_plot_ui <- shiny::renderUI({
      n <- length(unique(GTFSwizard::get_servicepattern(plot_gtfs())$service_pattern))
      plot_ui("headway_plot", height = pattern_plot_height(n, 520L))
    })

    output$service_heatmap_plot <- render_plot({
      safe_output("Weekday-hour plot", GTFSwizard::plot_serviceheatmap(dashboard_gtfs()))
    })

    output$service_span_plot <- render_plot({
      safe_output("Service span plot", {
      GTFSwizard::plot_servicespan(
        plot_gtfs(),
        top_n = as.integer(value_or(input$service_span_top_n, 25L))
      )
      })
    })
    output$service_span_plot_ui <- shiny::renderUI({
      n <- value_or(input$service_span_top_n, 25L)
      plot_ui("service_span_plot", height = route_plot_height(n, 720L))
    })

    output$dwell_plot <- render_plot({
      safe_output("Dwell-time plot", {
      dwell_time <- GTFSwizard::get_dwelltimes(dashboard_gtfs(), method = "by_hour") %>%
        dplyr::filter(
          is.finite(average.dwelltime), is.finite(trips),
          is.finite(pattern_frequency)
        )
      ggplot2::ggplot(dwell_time, ggplot2::aes(average.dwelltime, weight = trips * pattern_frequency)) +
        ggplot2::geom_histogram(fill = gtfswizard_colors()[["coral"]], color = "white", bins = 25) +
        ggplot2::labs(
          title = "Scheduled Dwell Time Distribution",
          subtitle = "Each observation is a route-hour-service pattern summary",
          x = "Average scheduled dwell time (seconds)",
          y = "Weighted route-hour observations"
        ) +
        theme_gtfswizard()
      })
    }, interactive = FALSE)

    output$route_duration_plot <- render_plot({
      safe_output("Route-duration plot", {
      GTFSwizard::plot_routeduration(
        plot_gtfs(),
        top_n = as.integer(value_or(input$route_duration_top_n, 15L))
      )
      })
    }, interactive = FALSE)
    output$route_duration_plot_ui <- shiny::renderUI({
      n <- value_or(input$route_duration_top_n, 15L)
      plot_ui(
        "route_duration_plot",
        height = route_plot_height(n, 680L),
        interactive = FALSE
      )
    })

    output$service_supply_plot <- render_plot({
      safe_output("Service-supply plot", {
      GTFSwizard::plot_servicesupply(
        plot_gtfs(),
        top_n = as.integer(value_or(input$service_supply_top_n, 15L))
      )
      })
    }, interactive = FALSE)
    output$service_supply_plot_ui <- shiny::renderUI({
      n <- value_or(input$service_supply_top_n, 15L)
      plot_ui(
        "service_supply_plot",
        height = route_plot_height(n, 680L),
        interactive = FALSE
      )
    })

    output$calendar_plot <- render_plot({
      safe_output("Calendar plot", {
      fill <- if(isTRUE(input$calendar_fill_pattern)) "service_pattern" else "trips"
      GTFSwizard::plot_calendar(
        dashboard_gtfs(), facet_by_year = TRUE, fill = fill
      )
      })
    }, interactive = FALSE)
    output$calendar_plot_ui <- shiny::renderUI({
      plot_ui(
        "calendar_plot",
        height = calendar_plot_height(dashboard_gtfs()),
        interactive = FALSE
      )
    })

    planning_route_data <- shiny::reactive({
      planning_route_indicators(dashboard_gtfs(), top_n = Inf)
    })

    output$planning_system_table <- shiny::renderTable({
      tryCatch(
        planning_system_indicators(
          dashboard_gtfs(),
          route_indicators = planning_route_data()
        ),
        error = function(error){
          data.frame(Message = paste0(
            "Planning indicators skipped: ", conditionMessage(error)
          ))
        }
      )
    }, striped = TRUE, spacing = "xs")

    output$planning_route_table <- shiny::renderTable({
      tryCatch(
        utils::head(
          planning_route_data(),
          as.integer(value_or(input$planning_top_n, 30L))
        ),
        error = function(error){
          data.frame(Message = paste0(
            "Route indicators skipped: ", conditionMessage(error)
          ))
        }
      )
    }, striped = TRUE, spacing = "xs", digits = 1)

    output$corridor_plot <- render_plot({
      tryCatch(
        GTFSwizard::plot_corridor(
          dashboard_gtfs(),
          i = input$corridor_i,
          min_length = input$corridor_min_length
        ),
        error = function(error){
          if(grepl("no consecutive stop pairs", conditionMessage(error), fixed = TRUE)){
            shiny::validate(shiny::need(FALSE, "No consecutive stop pairs match the selected filters."))
          }
          skip_output("Corridor plot", error)
        }
      )
    }, interactive = FALSE)

    output$hubs_plot <- render_plot({
      safe_output("Hub plot", GTFSwizard::plot_hubs(dashboard_gtfs(), i = input$hubs_i))
    }, interactive = FALSE)

    output$route_table <- shiny::renderTable({
      g <- edited_gtfs()
      routes_summary <- g$routes %>%
        dplyr::left_join(
          g$trips %>%
            dplyr::group_by(route_id) %>%
            dplyr::summarise(trips = dplyr::n(), .groups = "drop"),
          by = "route_id"
        ) %>%
        dplyr::mutate(trips = tidyr::replace_na(trips, 0))
      routes_summary %>%
        dplyr::select(dplyr::any_of(c(
          "route_id", "route_short_name", "route_long_name", "trips"
        )))
    }, striped = TRUE, spacing = "xs")

    output$route_frequency_plot <- render_plot({
      safe_output("Route-frequency plot", {
      g <- plot_gtfs()
      GTFSwizard::plot_routefrequency(
        g,
        top_n = as.integer(value_or(input$route_frequency_top_n, 25L))
      )
      })
    })
    output$route_frequency_plot_ui <- shiny::renderUI({
      g <- plot_gtfs()
      routes <- min(nrow(g$routes), value_or(input$route_frequency_top_n, 25L))
      patterns <- length(unique(GTFSwizard::get_servicepattern(g)$service_pattern))
      plot_ui(
        "route_frequency_plot",
        height = route_pattern_plot_height(routes, patterns, 520L)
      )
    })
  }

  shiny::shinyApp(ui, server)
}

planning_system_indicators <- function(gtfs, route_indicators = NULL){
  finite_median <- function(x, digits = 1L){
    x <- x[is.finite(x)]
    if(length(x)) round(stats::median(x), digits) else NA_real_
  }
  if(is.null(route_indicators)){
    route_indicators <- planning_route_indicators(gtfs, top_n = Inf)
  }
  trip_schedule <- attr(route_indicators, "trip_schedule")
  trip_instances <- attr(route_indicators, "trip_instances")
  if(is.null(trip_schedule) || is.null(trip_instances)){
    trip_schedule <- planning_trip_schedule(gtfs)
    trip_instances <- planning_trip_instances(gtfs, trip_schedule)
  }
  patterns <- get_servicepattern(gtfs)
  durations <- trip_schedule |>
    dplyr::left_join(
      gtfs$trips[, c("trip_id", "service_id")], by = "trip_id"
    ) |>
    dplyr::left_join(patterns, by = "service_id") |>
    dplyr::group_by(service_pattern) |>
    dplyr::summarise(
      vehicle_hours = sum(duration, na.rm = TRUE) / 3600,
      .groups = "drop"
    )
  spans <- trip_schedule |>
    dplyr::left_join(
      gtfs$trips[, c("trip_id", "service_id")], by = "trip_id"
    ) |>
    dplyr::left_join(patterns, by = "service_id") |>
    dplyr::mutate(end = .data$.base_start + .data$duration) |>
    dplyr::group_by(service_pattern) |>
    dplyr::summarise(
      span_hours = (
        max(.data$end, na.rm = TRUE) -
          min(.data$.base_start, na.rm = TRUE)
      ) / 3600,
      .groups = "drop"
    )
  routes_per_stop <- gtfs$stop_times |>
    dplyr::select(trip_id, stop_id) |>
    dplyr::distinct() |>
    dplyr::left_join(
      gtfs$trips[, c("trip_id", "route_id")], by = "trip_id"
    ) |>
    dplyr::group_by(stop_id) |>
    dplyr::summarise(routes = dplyr::n_distinct(route_id), .groups = "drop")
  spacing <- planning_stop_spacing(gtfs)
  total_length <- planning_total_length(gtfs)
  peak_fleet <- planning_peak_fleet(trip_instances, trip_schedule)

  tibble::tibble(
    Indicator = c(
      "Median route headway", "Median commercial speed",
      "Median service span", "Median daily vehicle-hours",
      "Total length",
      "Peak scheduled fleet", "Median consecutive-stop spacing",
      "Median routes per stop", "Maximum routes at one stop",
      "Explicit transfer rules"
    ),
    Value = c(
      finite_median(route_indicators$average_headway_minutes),
      finite_median(route_indicators$average_speed_kmh),
      finite_median(spans$span_hours),
      finite_median(durations$vehicle_hours),
      total_length,
      peak_fleet,
      spacing,
      finite_median(routes_per_stop$routes),
      if(nrow(routes_per_stop)) max(routes_per_stop$routes) else NA_real_,
      if(is.null(gtfs$transfers)) 0 else nrow(gtfs$transfers)
    ),
    Unit = c(
      "minutes", "km/h", "hours", "vehicle-hours",
      "km", "vehicles", "meters", "routes", "routes", "rules"
    )
  )
}

planning_total_length <- function(gtfs){
  if(is.null(gtfs$shapes) || !"shape_id" %in% names(gtfs$trips)){
    return(NA_real_)
  }
  used_shapes <- unique(as.character(gtfs$trips$shape_id))
  used_shapes <- used_shapes[!is.na(used_shapes) & nzchar(used_shapes)]
  if(!length(used_shapes)){
    return(NA_real_)
  }
  shapes <- get_shapes_sf(gtfs$shapes) |>
    dplyr::filter(.data$shape_id %in% used_shapes)
  if(!nrow(shapes)){
    return(NA_real_)
  }
  round(sum(as.numeric(sf::st_length(latlon2epsg(shapes))), na.rm = TRUE) / 1000, 1)
}

planning_route_indicators <- function(gtfs, top_n = 30L){
  if(!is.infinite(top_n)){
    gw_assert_int(top_n, "top_n", lower = 1L)
  }
  gtfs <- ensure_shapes(ensure_wizardgtfs(gtfs))
  direction <- direction_field(gtfs$trips)
  join_fields <- c("route_id", direction, "service_pattern", "pattern_frequency")
  patterns <- get_servicepattern(gtfs)
  trip_schedule <- planning_trip_schedule(gtfs)
  trip_instances <- planning_trip_instances(gtfs, trip_schedule) |>
    dplyr::left_join(gtfs$trips, by = "trip_id") |>
    dplyr::left_join(patterns, by = "service_id")

  frequency <- trip_instances |>
    dplyr::group_by(dplyr::across(dplyr::all_of(join_fields))) |>
    dplyr::summarise(daily_trips = dplyr::n(), .groups = "drop")
  headways <- calculate_headway_rows(
    trip_instances, join_fields, "start_seconds"
  ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(join_fields))) |>
    dplyr::summarise(
      average_headway_minutes = mean(headway_minutes),
      .groups = "drop"
    )
  trip_metrics <- trip_schedule |>
    dplyr::left_join(gtfs$trips, by = "trip_id") |>
    dplyr::left_join(patterns, by = "service_id")
  durations <- trip_metrics |>
    dplyr::group_by(dplyr::across(dplyr::all_of(join_fields))) |>
    dplyr::summarise(
      average_duration_minutes = mean(duration, na.rm = TRUE) / 60,
      .groups = "drop"
    )
  shape_distances <- get_shapes_sf(gtfs$shapes)
  shape_distances$distance <- as.numeric(
    sf::st_length(latlon2epsg(shape_distances))
  )
  shape_distances <- sf::st_drop_geometry(shape_distances)
  speeds <- trip_metrics |>
    dplyr::left_join(
      shape_distances[, c("shape_id", "distance")],
      by = "shape_id"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(join_fields))) |>
    dplyr::summarise(
      average_speed_kmh = (
        mean(distance, na.rm = TRUE) / 1000
      ) / (
        mean(duration, na.rm = TRUE) / 3600
      ),
      .groups = "drop"
    )

  result <- frequency |>
    dplyr::left_join(
      headways,
      by = join_fields
    ) |>
    dplyr::left_join(
      speeds,
      by = join_fields
    ) |>
    dplyr::left_join(
      durations,
      by = join_fields
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$daily_trips), .data$route_id,
      dplyr::across(dplyr::all_of(direction_field(gtfs$trips)))
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        "route_id", direction_field(gtfs$trips), "service_pattern",
        "daily_trips", "average_headway_minutes", "average_speed_kmh",
        "average_duration_minutes"
      ))
    )
  attr(result, "trip_schedule") <- trip_schedule
  attr(result, "trip_instances") <- trip_instances[
    , c("trip_id", "start_seconds", "service_pattern", "pattern_frequency")
  ]
  if(is.infinite(top_n)) result else utils::head(result, top_n)
}

planning_trip_schedule <- function(gtfs){
  gtfs$stop_times |>
    dplyr::mutate(
      .arrival = gtfs_time_to_seconds(arrival_time),
      .departure = gtfs_time_to_seconds(departure_time)
    ) |>
    dplyr::group_by(trip_id) |>
    dplyr::summarise(
      .base_start = .departure[which.min(stop_sequence)],
      duration = .arrival[which.max(stop_sequence)] - .base_start,
      .groups = "drop"
    )
}

planning_trip_instances <- function(gtfs, trip_schedule){
  first <- trip_schedule[, c("trip_id", ".base_start")]
  frequency_trip <- if(is.null(gtfs$frequencies)){
    character()
  } else {
    unique(as.character(gtfs$frequencies$trip_id))
  }
  scheduled <- first[!first$trip_id %in% frequency_trip, , drop = FALSE]
  scheduled$start_seconds <- scheduled$.base_start
  generated <- lapply(seq_len(if(is.null(gtfs$frequencies)) 0L else nrow(gtfs$frequencies)), function(i){
    row <- gtfs$frequencies[i, ]
    start <- gtfs_time_to_seconds(row$start_time)
    end <- gtfs_time_to_seconds(row$end_time)
    tibble::tibble(
      trip_id = as.character(row$trip_id),
      .base_start = first$.base_start[match(row$trip_id, first$trip_id)],
      start_seconds = seq(start, end - 1, by = as.numeric(row$headway_secs))
    )
  })
  dplyr::bind_rows(scheduled, generated)
}

planning_peak_fleet <- function(trip_instances, trip_schedule){
  intervals <- trip_instances |>
    dplyr::left_join(
      trip_schedule[, c("trip_id", "duration")], by = "trip_id"
    )
  groups <- c("service_pattern", "pattern_frequency")
  starts <- intervals[, groups]
  starts$time <- intervals$start_seconds
  starts$net_fleet <- 1L
  ends <- intervals[, groups]
  ends$time <- intervals$start_seconds + intervals$duration
  ends$net_fleet <- -1L
  events <- dplyr::bind_rows(starts, ends) |>
    dplyr::filter(is.finite(time)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groups, "time")))) |>
    dplyr::summarise(
      net_fleet = sum(.data$net_fleet), .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(groups, "time")))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::mutate(fleet = cumsum(.data$net_fleet)) |>
    dplyr::ungroup()
  if(nrow(events)) max(events$fleet, na.rm = TRUE) else NA_real_
}

planning_stop_spacing <- function(gtfs){
  direction <- direction_field(gtfs$trips)
  representative_fields <- c("route_id", direction)
  if("shape_id" %in% names(gtfs$trips)){
    representative_fields <- c(representative_fields, "shape_id")
  }
  representative_trips <- gtfs$trips |>
    dplyr::group_by(dplyr::across(dplyr::all_of(representative_fields))) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(trip_id)
  calls <- gtfs$stop_times |>
    dplyr::filter(trip_id %in% representative_trips) |>
    dplyr::arrange(trip_id, stop_sequence) |>
    dplyr::left_join(
      gtfs$stops[, c("stop_id", "stop_lon", "stop_lat")],
      by = "stop_id"
    ) |>
    dplyr::group_by(trip_id) |>
    dplyr::mutate(
      distance = haversine_m(
        stop_lon, stop_lat, dplyr::lead(stop_lon), dplyr::lead(stop_lat)
      )
    ) |>
    dplyr::pull(distance)
  calls <- calls[is.finite(calls)]
  if(length(calls)) round(stats::median(calls), 1) else NA_real_
}
