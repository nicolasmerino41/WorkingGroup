# Run after scripts/audit_NMRS.R. Only summary tables are loaded into R.
library(ggplot2)
library(sf)
library(dplyr)
plot_NMRS_coverage <- function() {
  tables <- file.path("outputs", "NMRS", "summaryTables")
  out <- file.path("outputs", "NMRS", "coveragePlots")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  read_summary <- function(name) read.csv(file.path(tables, paste0(name, ".csv")))
  annual <- read_summary("annual_coverage")
  species <- read_summary("species_coverage")
  cells <- read_summary("grid_period_coverage")
  overall <- read_summary("grid_overall_coverage")
  save_plot <- function(p, filename, width = 7, height = 4) {
    ggsave(file.path(out, filename), p, width = width, height = height,
           dpi = 300, bg = "white", device = grDevices::png, type = "cairo")
  }
  for (metric in c("records", "recorded_species", "grid_cells")) {
    p <- ggplot(annual, aes(x = year, y = .data[[metric]])) +
      geom_line() + geom_point(size = 1) + theme_classic(base_size = 12) +
      labs(x = "Starting year", y = gsub("_", " ", metric))
    save_plot(p, paste0("NMRS_annual_", metric, ".png"))
  }
  sp <- filter(species, tolower(taxon_rank) == "species")
  for (metric in c("records", "grid_cells")) {
    p <- ggplot(filter(sp, .data[[metric]] > 0), aes(x = .data[[metric]])) + geom_histogram(bins = 40,
      fill = "#287D8E", colour = "white") + scale_x_log10() +
      theme_classic(base_size = 12) +
      labs(x = paste(gsub("_", " ", metric), "per species (log scale)"), y = "Species")
    save_plot(p, paste0("NMRS_species_", metric, "_distribution.png"))
  }
  coords <- read.csv("data/grids/grid_square_coordinates_lookup.csv")
  centres <- coords %>% filter(resolution == 10) %>%
    group_by(grid) %>% summarise(long = mean(long), lat = mean(lat), .groups = "drop")
  unmatched <- anti_join(overall, centres, by = "grid")
  write.csv(unmatched, file.path(out, "unmapped_grid_cells.csv"), row.names = FALSE)
  if (nrow(unmatched)) warning(nrow(unmatched), " cells lack map coordinates; see unmapped_grid_cells.csv")
  land <- st_read("data/grids/ne_50m_land/ne_50m_land.shp", quiet = TRUE)
  land <- suppressWarnings(st_crop(land, xmin = -9, xmax = 2.5, ymin = 49.5, ymax = 61))
  land <- st_transform(land, 3035)
  map <- function(df, metric, label, limits = NULL, log_scale = FALSE) {
    df <- inner_join(df, centres, by = "grid")
    pts <- st_as_sf(df, coords = c("long", "lat"), crs = 4326) %>% st_transform(3035)
    ggplot() + geom_sf(data = land, fill = "grey95", colour = "grey65", linewidth = 0.15) +
      geom_sf(data = pts, aes(colour = .data[[metric]]), size = 0.8) +
      scale_colour_viridis_c(name = label, limits = limits,
                            trans = if (log_scale) "log10" else "identity") +
      coord_sf(datum = NA) + theme_void(base_size = 12)
  }
  save_plot(map(overall, "years_recorded", "Years recorded"),
            "NMRS_grid_years_recorded_all_years.png", 7, 8)
  for (start in sort(unique(cells$period_start))) {
    block <- filter(cells, period_start == start)
    period <- paste0(start, "-", unique(block$period_end))
    for (metric in c("records", "recorded_species", "years_recorded", "distinct_exact_dates")) {
      # Consistent scales across periods; log colour scale for record counts.
      limits <- range(cells[[metric]], na.rm = TRUE)
      save_plot(map(block, metric, gsub("_", "\n", metric), limits,
                    log_scale = metric == "records"),
                paste0("NMRS_", metric, "_", period, "_10km.png"), 7, 8)
    }
  }
  message("Coverage plots saved to ", out)
}
plot_NMRS_coverage()
