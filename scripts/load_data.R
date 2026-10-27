# Run from the WorkingGroup R project. Base R only.
files <- c("distributions", "distribution_changes",
           "grid_square_coordinates_lookup",
           "percent_benchmark_species_detected", "species_lookup")

for (name in files) {
  assign(name, read.csv(file.path("data", if (name == "grid_square_coordinates_lookup") "grids" else "BTO", paste0(name, ".csv")),
                        sep = ",", stringsAsFactors = FALSE))
}
rm(name)

# B = breeding / Britain; W = winter; I = Ireland (depending on column).
# Coordinates are polygon vertices, so multiple rows represent each grid cell.
# Keep spatial resolutions separate: the source contains 10, 20, 50 and -10.

summarise_records <- function(x) {
  codes <- unique(x$speccode)
  data.frame(
    records = nrow(x),
    grid_cells = length(unique(paste(x$island, x$grid, x$resolution))),
    taxa = length(codes),
    species = sum(species_lookup$speccode %in% codes &
                    species_lookup$taxonomic_rank == "Species")
  )
}

# Overall, breeding and winter coverage (all periods combined).
summary_overall <- rbind(
  cbind(season = "All", summarise_records(distributions)),
  cbind(season = "B", summarise_records(subset(distributions, season == "B"))),
  cbind(season = "W", summarise_records(subset(distributions, season == "W")))
)
rownames(summary_overall) <- NULL

# Grid counts by island and resolution (all periods combined).
summary_grids <- unique(distributions[c("island", "resolution", "grid")])
summary_grids <- aggregate(grid ~ island + resolution, summary_grids, length)
names(summary_grids)[3] <- "grid_cells"

# Coverage by atlas period, season, island and resolution.
groups <- split(distributions, interaction(
  distributions$period, distributions$season,
  distributions$island, distributions$resolution, drop = TRUE))
summary_coverage <- do.call(rbind, lapply(groups, function(x) {
  cbind(x[1, c("period", "season", "island", "resolution")],
        summarise_records(x))
}))
rownames(summary_coverage) <- NULL
rm(groups)

summary_files <- data.frame(
  dataset = files,
  rows = vapply(files, function(x) nrow(get(x)), integer(1)),
  columns = vapply(files, function(x) ncol(get(x)), integer(1))
)
summary_taxonomy <- as.data.frame(table(species_lookup$taxonomic_rank))
names(summary_taxonomy) <- c("taxonomic_rank", "taxa")
summary_status <- as.data.frame(with(distributions, table(season, status)))
summary_status <- subset(summary_status, Freq > 0)

print(summary_files, row.names = FALSE)
print(summary_overall, row.names = FALSE)
print(summary_grids, row.names = FALSE)
print(summary_coverage, row.names = FALSE)
print(summary_taxonomy, row.names = FALSE)
print(summary_status, row.names = FALSE)

# Non-records are not necessarily absences. Sampling completeness is provided
# separately in percent_benchmark_species_detected (percentages).
# See data/BTO/readme.txt for citation and CC-BY-NC licence.
