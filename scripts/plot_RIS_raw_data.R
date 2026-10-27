# Run from WorkingGroup.Rproj. Packages: sf, ggplot2, dplyr.
library(sf)
library(ggplot2)
library(dplyr)

out <- file.path("outputs", "RIS", "rawPlots")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
moths <- read.csv("data/RIS/MothCommunityData.csv", check.names = FALSE)
taxa <- setdiff(names(moths), c("year", "x", "y"))
community <- as.matrix(moths[taxa])
stopifnot(is.numeric(community), !anyNA(community), all(community >= 0))
# Includes all supplied taxa; unresolved groups and butterflies remain included.
moths$richness <- rowSums(community > 0)
site_years <- moths[c("year", "x", "y", "richness")]

# Coordinate magnitudes suggest British National Grid; confirm with source metadata.
point_crs <- 27700
land <- st_read("data/grids/ne_50m_land/ne_50m_land.shp", quiet = TRUE)
land <- suppressWarnings(st_crop(land, xmin = -9, xmax = 2.5,
                                ymin = 49.5, ymax = 61))
land <- st_transform(land, point_crs)
map_theme <- theme_void(base_size = 12) +
  theme(legend.position = "right", plot.background = element_rect(
    fill = "white", colour = NA))
map_points <- function(df, value, label, change = FALSE, limits = NULL) {
  pts <- st_as_sf(df, coords = c("x", "y"), crs = point_crs)
  p <- ggplot() +
    geom_sf(data = land, fill = "grey95", colour = "grey60", linewidth = 0.2) +
    geom_sf(data = pts, aes(colour = .data[[value]]), size = 2) +
    coord_sf(xlim = c(-50000, 700000), ylim = c(0, 1250000),
             datum = NA, expand = FALSE) + map_theme
  if (change) p + scale_colour_gradient2(name = label, low = "#2166AC",
    mid = "white", high = "#B2182B", midpoint = 0, limits = limits)
  else p + scale_colour_viridis_c(name = label, limits = limits)
}
save_plot <- function(p, filename, width = 7, height = 8) {
  ggsave(file.path(out, filename), p, width = width, height = height,
         dpi = 300, bg = "white", device = "png", type = "cairo")
}

# Sampling coverage.
coverage <- site_years %>% group_by(x, y) %>%
  summarise(years_sampled = n_distinct(year), .groups = "drop")
save_plot(map_points(coverage, "years_sampled", "Years sampled"),
          "RIS_sampling_locations_years_sampled_1990-2018.png")
annual <- site_years %>% count(year, name = "sites_sampled")
save_plot(ggplot(annual, aes(year, sites_sampled)) +
  geom_line() + geom_point() + theme_classic(base_size = 12) +
  labs(x = "Year", y = "Locations sampled"),
  "RIS_locations_sampled_per_year_1990-2018.png", 7, 4)

# Shared colour limits for selected-year richness maps.
years <- c(1990, 2000, 2010, 2018)
richness_limits <- c(0, max(site_years$richness[site_years$year %in% years]))
for (yr in years) {
  values <- filter(site_years, year == yr)
  save_plot(map_points(values, "richness", "Recorded taxa",
                       limits = richness_limits),
            paste0("RIS_recorded_taxon_richness_", yr, ".png"))
}

# Paired comparison: only locations observed in both years.
a <- filter(site_years, year == 1990)
b <- filter(site_years, year == 2010)
paired <- inner_join(a, b, by = c("x", "y"), suffix = c("_1990", "_2010"))
paired$richness_change <- paired$richness_2010 - paired$richness_1990
key <- function(x, y) paste(x, y, sep = "|")
ia <- match(key(paired$x, paired$y), key(moths$x, moths$y)[moths$year == 1990])
ib <- match(key(paired$x, paired$y), key(moths$x, moths$y)[moths$year == 2010])
pa <- community[moths$year == 1990, , drop = FALSE][ia, , drop = FALSE] > 0
pb <- community[moths$year == 2010, , drop = FALSE][ib, , drop = FALSE] > 0
# Jaccard dissimilarity: 0 = same composition, 1 = no shared taxa.
union_n <- rowSums(pa | pb)
paired$jaccard_dissimilarity <- ifelse(union_n > 0,
  1 - rowSums(pa & pb) / union_n, NA_real_)
change_limit <- max(1, abs(paired$richness_change))
save_plot(map_points(paired, "richness_change", "Taxon richness\n2010 minus 1990",
  change = TRUE, limits = c(-change_limit, change_limit)),
  "RIS_recorded_taxon_richness_change_2010_minus_1990_shared_locations.png")
save_plot(map_points(paired, "jaccard_dissimilarity", "Jaccard\ndissimilarity",
  limits = c(0, 1)),
  "RIS_community_dissimilarity_1990_vs_2010_shared_locations.png")

write.csv(site_years, file.path(out, "RIS_site_year_recorded_taxon_richness.csv"),
          row.names = FALSE)
write.csv(coverage, file.path(out, "RIS_location_sampling_coverage.csv"), row.names = FALSE)
write.csv(annual, file.path(out, "RIS_annual_sampling_coverage.csv"), row.names = FALSE)
write.csv(paired, file.path(out, "RIS_paired_1990_2010_richness_and_dissimilarity.csv"),
          row.names = FALSE)
message("Saved 8 plots and 4 summary tables to ", out,
        "; paired locations: ", nrow(paired))
# Descriptive plots: zeros are treated as non-detections; no effort/detection correction.
