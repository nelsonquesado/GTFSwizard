---
title: GTFSwizard
---

<img class="gw-home-logo" align="right" src="https://raw.githubusercontent.com/OPATP/GTFSwizard/main/figs/GTFSwizard_logo.png" alt="GTFSwizard logo" width="150">

# GTFSwizard

Create, explore, analyze, edit, and export General Transit Feed Specification
(GTFS) Schedule feeds in R.

[![CRAN version](https://www.r-pkg.org/badges/version/GTFSwizard)](https://CRAN.R-project.org/package=GTFSwizard)
[![License: GPL-3](https://img.shields.io/cran/l/GTFSwizard.svg)](https://CRAN.R-project.org/package=GTFSwizard)

**Development 1.2.1** · **CRAN 1.2.0**

## Install

```r
# Stable release
install.packages("GTFSwizard")

# Development version
remotes::install_github(
  "OPATP/GTFSwizard@main"
)
```

## Work With GTFS

- **Create and validate:** build feeds from data frames with
  [`create_gtfs()`](reference/create_gtfs.html), or import an existing archive
  with [`read_gtfs()`](reference/read_gtfs.html).
- **Analyze service:** calculate frequency, headways, dwell time, duration,
  speed, fleet requirements, service span, corridors, and hubs.
- **Build scenarios:** filter routes, services, dates, stops, and time windows;
  then delay, split, merge, or edit scheduled trips.
- **Explore visually:** use [`explore_gtfs()`](reference/explore_gtfs.html) for
  interactive maps, planning indicators, service plots, editing, and export.

## Quick Start

The package includes compact rail and larger bus feeds, so the main workflows
can be tried without downloading external data.

```r
library(GTFSwizard)

gtfs <- for_rail_gtfs
summary(gtfs)

get_frequency(gtfs, method = "by_route")
plot_calendar(gtfs, fill = "service_pattern", facet_by_year = TRUE)
```

Launch the dashboard from an object, or omit the object to choose a GTFS zip
archive interactively.

```r
explore_gtfs(for_rail_gtfs)
# explore_gtfs()
```

<img class="gw-home-dashboard" src="https://raw.githubusercontent.com/OPATP/GTFSwizard/main/figs/exploregtfs.png" alt="GTFSwizard dashboard showing a transit network">

## Guides

- [Getting started](articles/getting-started.html)
- [Service analysis and visualization](articles/service-analysis.html)
- [Filtering, selecting, and editing](articles/filtering-editing.html)
- [Interactive dashboard](articles/explore-dashboard.html)
- [Complete function reference](reference/index.html)
- [Cheat sheet](https://github.com/OPATP/GTFSwizard/blob/main/inst/cheatsheet/GTFSwizard-cheatsheet.html)

## Citation

Use `citation("GTFSwizard")` to obtain the current package citation. Related
publications and project details are listed in the
[repository README](https://github.com/OPATP/GTFSwizard#readme).
