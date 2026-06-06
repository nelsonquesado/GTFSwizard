gtfswizard_colors <- function(){
  c(
    ink = "#1B1B1B",
    blue = "#0072B2",
    teal = "#009E73",
    coral = "#D55E00",
    gold = "#E69F00",
    purple = "#CC79A7",
    sky = "#56B4E9",
    yellow = "#F0E442",
    gray = "#87939D",
    light = "#E8EDF0"
  )
}

gtfswizard_palette <- function(n){
  base <- unname(gtfswizard_colors()[c(
    "blue", "coral", "teal", "purple", "gold", "sky", "ink", "yellow"
  )])
  if(n <= length(base)){
    return(base[seq_len(n)])
  }
  grDevices::hcl.colors(n, palette = "Dynamic")
}

theme_gtfswizard <- function(base_size = 11){
  colors <- gtfswizard_colors()
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(color = colors[["ink"]]),
      plot.title = ggplot2::element_text(
        face = "bold", size = ggplot2::rel(1.25), margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        color = colors[["gray"]], margin = ggplot2::margin(b = 10)
      ),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = colors[["light"]], linewidth = 0.35
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(
        fill = "#F4F6F7", color = NA
      ),
      plot.margin = ggplot2::margin(10, 12, 10, 10)
    )
}

theme_gtfswizard_map <- function(base_size = 11){
  theme_gtfswizard(base_size) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

hour_scale <- function(hours){
  finite_hours <- hours[is.finite(hours)]
  maximum <- if(length(finite_hours)){
    max(24, finite_hours)
  } else {
    24
  }
  ggplot2::scale_x_continuous(
    breaks = seq(0, ceiling(maximum / 6) * 6, by = 6)
  )
}
