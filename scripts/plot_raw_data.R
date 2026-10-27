# scripts/plot_raw_data.R
#
# Output: outputs/BTO/rawPlots/
#
# Breeding:
#   T1 = 1968-72
#   T2 = 1988-91
#   T3 = 2008-11
#
# Wintering:
#   T1 = 1980/81-1982/83
#   T3 = 2007/08-10/11
#   No T2 winter data are available.
#
# Richness = number of distinct species recorded per 10-km cell.
# All recorded statuses are included.
# Only taxonomic_rank == "Species" is retained.
#
# Coarse/undocumented resolutions are excluded and exported for inspection.
# Missing cell-periods remain NA, rather than being treated as zero.
# Changes are calculated only for cells with records in both periods.
#
# Packages:
# install.packages(c("sf", "readr", "dplyr", "ggplot2", "tibble"))

suppressPackageStartupMessages({
  library(sf)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(tibble)
})

# ---------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------

ROOT <- getwd()
DATA_DIR <- file.path(ROOT, "data", "BTO")
OUT_DIR <- file.path(ROOT, "outputs", "BTO", "rawPlots")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

required_files <- file.path(
  DATA_DIR,
  c(
    "distributions.csv",
    "species_lookup.csv"
  )
)

required_files <- c(required_files, file.path(ROOT, "data", "grids", "grid_square_coordinates_lookup.csv"))

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files)) {
  stop(
    "Run this script from the WorkingGroup project root.\nMissing:\n",
    paste(missing_files, collapse = "\n")
  )
}

# ---------------------------------------------------------------------
# Read data
# ---------------------------------------------------------------------

distributions <- read_csv(
  file.path(DATA_DIR, "distributions.csv"),
  show_col_types = FALSE
)

coordinates <- read_csv(
  file.path(ROOT, "data", "grids", "grid_square_coordinates_lookup.csv"),
  show_col_types = FALSE
)

species <- read_csv(
  file.path(DATA_DIR, "species_lookup.csv"),
  show_col_types = FALSE
)

distributions <- distributions %>%
  mutate(
    grid = toupper(trimws(grid)),
    season = toupper(trimws(season)),
    period = trimws(period)
  )

coordinates <- coordinates %>%
  mutate(grid = toupper(trimws(grid)))

species <- species %>%
  mutate(taxonomic_rank = trimws(taxonomic_rank))

cat("\nPeriods by season:\n")
print(with(distributions, table(season, period)))

cat("\nTaxonomic ranks:\n")
print(table(species$taxonomic_rank, useNA = "ifany"))

# ---------------------------------------------------------------------
# Define the actual atlas periods
# ---------------------------------------------------------------------

periods <- tribble(
  ~season, ~period,              ~slot, ~label,                  ~file_label,
  "B",     "1968-72",            "T1",  "1968–1972",              "T1_1968-1972",
  "B",     "1988-91",            "T2",  "1988–1991",              "T2_1988-1991",
  "B",     "2008-11",            "T3",  "2008–2011",              "T3_2008-2011",
  "W",     "1980/81-1982/83",    "T1",  "1980/81–1982/83",        "T1_1980-81_to_1982-83",
  "W",     "2007/08-10/11",      "T3",  "2007/08–2010/11",        "T3_2007-08_to_2010-11"
)

unknown_periods <- distributions %>%
  distinct(season, period) %>%
  anti_join(periods, by = c("season", "period"))

if (nrow(unknown_periods)) {
  print(unknown_periods)
  stop("Unexpected season/period combinations. Check these before plotting.")
}

write_csv(
  periods,
  file.path(OUT_DIR, "atlas_period_assignments.csv")
)

# ---------------------------------------------------------------------
# Handle spatial resolution
# ---------------------------------------------------------------------

excluded_resolution <- distributions %>%
  filter(is.na(resolution) | resolution != 10)

write_csv(
  excluded_resolution,
  file.path(OUT_DIR, "excluded_coarse_or_unknown_resolution_records.csv")
)

cat(
  "\nExcluding", nrow(excluded_resolution),
  "records with resolution other than 10 km.\n"
)

if (nrow(excluded_resolution)) {
  print(
    excluded_resolution %>%
      count(season, period, resolution, name = "records")
  )
}

# ---------------------------------------------------------------------
# Retain species-level records
# ---------------------------------------------------------------------

if (anyDuplicated(species$speccode)) {
  stop("Duplicate speccode entries in species_lookup.csv.")
}

unknown_species <- distributions %>%
  distinct(speccode) %>%
  anti_join(species, by = "speccode")

if (nrow(unknown_species)) {
  stop("Some speccode values are missing from species_lookup.csv.")
}

species_codes <- species %>%
  filter(tolower(taxonomic_rank) == "species") %>%
  select(speccode)

if (!nrow(species_codes)) {
  stop("No Species entries found in species_lookup.csv.")
}

excluded_taxa <- species %>%
  anti_join(species_codes, by = "speccode")

write_csv(
  excluded_taxa,
  file.path(OUT_DIR, "excluded_non_species_taxa.csv")
)

# Keep all valid 10-km rows for identifying cells with records.
records_10km <- distributions %>%
  filter(resolution == 10) %>%
  left_join(periods, by = c("season", "period"))

# Inspect the values without assuming that n_tenkms is binary.
print(
  records_10km %>%
    dplyr::count(n_tenkms, sort = TRUE),
  n = Inf
)

# Positive counts indicate a recorded occurrence.
# Missing counts cannot establish presence or absence.
records_10km <- records_10km %>%
  dplyr::filter(!is.na(n_tenkms), n_tenkms >= 0)

# Cells with a record are represented; completely missing cells are not
# assumed to have zero species.
coverage <- records_10km %>%
  distinct(season, slot, grid)

richness <- records_10km %>%
  filter(n_tenkms > 0) %>%
  semi_join(species_codes, by = "speccode") %>%
  distinct(season, slot, grid, speccode) %>%
  count(season, slot, grid, name = "richness")

richness <- coverage %>%
  left_join(richness, by = c("season", "slot", "grid")) %>%
  mutate(richness = coalesce(richness, 0L)) %>%
  left_join(
    periods %>% select(season, slot, period, label, file_label),
    by = c("season", "slot")
  )

missing_atlases <- periods %>%
  anti_join(
    richness %>% distinct(season, slot),
    by = c("season", "slot")
  )

if (nrow(missing_atlases)) {
  print(missing_atlases)
  stop("An expected atlas has no usable 10-km records.")
}

# ---------------------------------------------------------------------
# Build 10-km polygons from the supplied coordinate lookup
#
# This lookup includes Irish grids, which the OS British National Grid
# GeoPackage does not cover.
# ---------------------------------------------------------------------

coords_10km <- coordinates %>%
  filter(resolution == 10) %>%
  select(grid, order, long, lat) %>%
  arrange(grid, order)

if (anyNA(coords_10km)) {
  stop("Missing values in the 10-km coordinate lookup.")
}

coordinate_groups <- split(
  coords_10km,
  coords_10km$grid,
  drop = TRUE
)

polygon_list <- lapply(coordinate_groups, function(x) {
  x <- x[order(x$order), ]
  
  xy <- as.matrix(x[, c("long", "lat")])
  storage.mode(xy) <- "double"
  
  if (nrow(unique(xy)) < 3) {
    stop("Insufficient polygon vertices for grid ", x$grid[1])
  }
  
  if (!all(xy[1, ] == xy[nrow(xy), ])) {
    xy <- rbind(xy, xy[1, ])
  }
  
  st_polygon(list(xy))
})

grid <- st_sf(
  grid = names(coordinate_groups),
  geometry = st_sfc(polygon_list, crs = 4326)
) %>%
  st_make_valid() %>%
  st_transform(3035)

unmatched <- richness %>%
  distinct(grid) %>%
  anti_join(st_drop_geometry(grid), by = "grid")

if (nrow(unmatched)) {
  write_csv(
    unmatched,
    file.path(OUT_DIR, "unmatched_grid_references.csv")
  )
  
  write_csv(
    records_10km %>%
      dplyr::semi_join(unmatched, by = "grid"),
    file.path(OUT_DIR, "records_without_grid_polygons.csv")
  )
  
  warning(
    nrow(unmatched),
    " grid references have no polygon and will be excluded: ",
    paste(unmatched$grid, collapse = ", "),
    call. = FALSE
  )
  
  richness <- richness %>%
    dplyr::anti_join(unmatched, by = "grid")
}

# Consistent map extent: all cells recorded in at least one atlas.
grid <- grid %>%
  semi_join(richness %>% distinct(grid), by = "grid")

# ---------------------------------------------------------------------
# Calculate richness changes
# ---------------------------------------------------------------------

comparisons <- tribble(
  ~season, ~earlier, ~later,
  "B",     "T1",     "T2",
  "B",     "T2",     "T3",
  "B",     "T1",     "T3",
  "W",     "T1",     "T3"
)

changes <- bind_rows(lapply(seq_len(nrow(comparisons)), function(i) {
  s <- comparisons$season[i]
  early <- comparisons$earlier[i]
  late <- comparisons$later[i]
  
  a <- richness %>%
    filter(season == s, slot == early) %>%
    select(grid, richness_earlier = richness)
  
  b <- richness %>%
    filter(season == s, slot == late) %>%
    select(grid, richness_later = richness)
  
  result <- inner_join(a, b, by = "grid")
  
  if (!nrow(result)) {
    stop("No shared cells for ", s, ": ", late, " minus ", early)
  }
  
  result %>%
    mutate(
      season = s,
      earlier = early,
      later = late,
      change = richness_later - richness_earlier
    )
}))

write_csv(
  richness,
  file.path(OUT_DIR, "bird_richness_by_10km_cell.csv")
)

write_csv(
  changes,
  file.path(OUT_DIR, "bird_richness_changes_by_10km_cell.csv")
)

# ---------------------------------------------------------------------
# Plot settings
# ---------------------------------------------------------------------

season_names <- c(B = "Breeding", W = "Wintering")

richness_max <- max(1, richness$richness)
change_max <- max(1, abs(changes$change))

map_theme <- theme_void(base_size = 12) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 11, colour = "grey35"),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(12, 12, 12, 12)
  )

save_map <- function(p, filename) {
  ggsave(
    filename = file.path(OUT_DIR, filename),
    plot = p,
    width = 7,
    height = 9,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  
  message("Saved: ", filename)
}

# ---------------------------------------------------------------------
# Richness maps: three breeding, two wintering
# ---------------------------------------------------------------------

for (i in seq_len(nrow(periods))) {
  s <- periods$season[i]
  tt <- periods$slot[i]
  
  values <- richness %>%
    filter(season == s, slot == tt) %>%
    select(grid, richness)
  
  plot_data <- grid %>%
    left_join(values, by = "grid")
  
  p <- ggplot(plot_data) +
    geom_sf(aes(fill = richness), colour = NA) +
    scale_fill_viridis_c(
      name = "Recorded\nspecies",
      option = "C",
      limits = c(0, richness_max),
      na.value = "grey90"
    ) +
    coord_sf(datum = NA) +
    labs(
      title = paste(season_names[[s]], "bird richness"),
      subtitle = paste0(
        periods$slot[i], ": ", periods$label[i],
        " · 10 × 10 km"
      )
    ) +
    map_theme
  
  save_map(
    p,
    paste0(
      tolower(season_names[[s]]),
      "_richness_",
      periods$file_label[i],
      "_10km.png"
    )
  )
}

# ---------------------------------------------------------------------
# Change maps: three breeding, one wintering
# ---------------------------------------------------------------------

for (i in seq_len(nrow(comparisons))) {
  s <- comparisons$season[i]
  early <- comparisons$earlier[i]
  late <- comparisons$later[i]
  
  early_info <- periods %>%
    filter(season == s, slot == early)
  
  late_info <- periods %>%
    filter(season == s, slot == late)
  
  values <- changes %>%
    filter(
      season == s,
      earlier == early,
      later == late
    ) %>%
    select(grid, change)
  
  plot_data <- grid %>%
    left_join(values, by = "grid")
  
  p <- ggplot(plot_data) +
    geom_sf(aes(fill = change), colour = NA) +
    scale_fill_gradient2(
      name = "Species\nchange",
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-change_max, change_max),
      na.value = "grey90"
    ) +
    coord_sf(datum = NA) +
    labs(
      title = paste(season_names[[s]], "bird richness change"),
      subtitle = paste0(
        late, " − ", early, ": ",
        early_info$label, " → ", late_info$label
      )
    ) +
    map_theme
  
  save_map(
    p,
    paste0(
      tolower(season_names[[s]]),
      "_richness_change_",
      late_info$file_label,
      "_minus_",
      early_info$file_label,
      "_10km.png"
    )
  )
}

cat("\nDone: 9 PNG maps saved in outputs/BTO/rawPlots/.\n")
cat("Grey cells have no comparable recorded value.\n")
cat("Changes describe recorded richness, without detection correction.\n")