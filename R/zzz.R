

.onLoad <- function(libname,pkgname){
  invisible(
    suppressMessages(
      suppressWarnings(
        
        lapply(
          c('sf', 'data.table', 'shiny', 'plotly', 'leaflet', 'leaflet.extras','tidyverse', 'hrbrthemes', 'stplanr'),
          function(x){
            invisible(
              suppressPackageStartupMessages(
                library(x,quietly = TRUE,character.only = TRUE)
              )
            )
          })
        
      )
    )
  )
  
  invisible(suppressMessages(suppressWarnings(
    try(detach("package:tidylog", unload = TRUE),silent = T)
  )))
  if(interactive())
    packageStartupMessage(GTFSwizard.StartupMessage())
  
}