# scripts/audit_NMRS.R
#
# Run from WorkingGroup.Rproj.
# Install once: install.packages(c("DBI", "duckdb"))
#
# External storage:
#   D:/NMRS/merged/NMRS_merged.csv
#   D:/NMRS/audit/database/NMRS.duckdb
#   D:/NMRS/audit/database/temp/
#   D:/NMRS/audit/summaryTables/       [large audit tables]
#   D:/NMRS/audit/staging/            [temporary table exports]
#
# Repository:
#   outputs/NMRS/summaryTables/       [tables <= small_table_mb]
#   outputs/NMRS/audit_table_index.csv
#
# Source records are not cleaned or deduplicated.

audit_NMRS <- function(
    data_root = "D:/NMRS",
    csv = file.path(data_root, "merged", "NMRS_merged.csv"),
    repo_root = getwd(),
    period_width = 10L,
    memory_limit = "2GB",
    threads = 2L,
    small_table_mb = 10,
    force = FALSE
) {
  
  # -------------------------------------------------------------------
  # Validate configuration
  # -------------------------------------------------------------------
  
  if (
    length(period_width) != 1L ||
    is.na(period_width) ||
    !is.finite(period_width) ||
    period_width < 1 ||
    period_width != floor(period_width) ||
    period_width > .Machine$integer.max
  ) {
    stop("period_width must be one positive integer.")
  }
  
  if (
    length(threads) != 1L ||
    is.na(threads) ||
    !is.finite(threads) ||
    threads < 1 ||
    threads != floor(threads) ||
    threads > .Machine$integer.max
  ) {
    stop("threads must be one positive integer.")
  }
  
  if (
    length(small_table_mb) != 1L ||
    is.na(small_table_mb) ||
    !is.finite(small_table_mb) ||
    small_table_mb < 0
  ) {
    stop("small_table_mb must be one non-negative number.")
  }
  
  period_width <- as.integer(period_width)
  threads <- as.integer(threads)
  
  if (
    !requireNamespace("duckdb", quietly = TRUE) ||
    !requireNamespace("DBI", quietly = TRUE)
  ) {
    stop(
      'Install packages first: install.packages(c("DBI", "duckdb"))'
    )
  }
  
  if (!dir.exists(data_root)) {
    stop("NMRS data directory not found: ", data_root)
  }
  
  if (!file.exists(csv)) {
    stop("Merged CSV not found: ", csv)
  }
  
  data_root <- normalizePath(
    data_root,
    winslash = "/",
    mustWork = TRUE
  )
  
  csv <- normalizePath(
    csv,
    winslash = "/",
    mustWork = TRUE
  )
  
  repo_root <- normalizePath(
    repo_root,
    winslash = "/",
    mustWork = TRUE
  )
  
  # Avoid accidentally putting repository outputs inside the data folder.
  if (
    identical(tolower(repo_root), tolower(data_root)) ||
    startsWith(
      paste0(tolower(repo_root), "/"),
      paste0(tolower(data_root), "/")
    )
  ) {
    stop(
      "repo_root must point to the OneDrive WorkingGroup repository, ",
      "not D:/NMRS."
    )
  }
  
  # -------------------------------------------------------------------
  # Directories
  # -------------------------------------------------------------------
  
  repo_out <- file.path(repo_root, "outputs", "NMRS")
  tables <- file.path(repo_out, "summaryTables")
  
  external_out <- file.path(data_root, "audit")
  large_tables <- file.path(external_out, "summaryTables")
  staging_dir <- file.path(external_out, "staging")
  db_dir <- file.path(external_out, "database")
  temp_dir <- file.path(db_dir, "temp")
  
  for (directory in c(
    repo_out, tables, large_tables, staging_dir, db_dir, temp_dir
  )) {
    dir.create(
      directory,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    if (!dir.exists(directory)) {
      stop("Cannot create directory: ", directory)
    }
  }
  
  db_path <- file.path(db_dir, "NMRS.duckdb")
  
  # -------------------------------------------------------------------
  # DuckDB: database and spill files remain on D:
  # -------------------------------------------------------------------
  
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = db_path
  )
  
  on.exit(
    DBI::dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  quote <- function(x) {
    as.character(DBI::dbQuoteString(con, x))
  }
  
  DBI::dbExecute(
    con,
    paste0("SET memory_limit = ", quote(memory_limit))
  )
  
  DBI::dbExecute(
    con,
    paste0("SET threads = ", threads)
  )
  
  DBI::dbExecute(
    con,
    "SET preserve_insertion_order = false"
  )
  
  DBI::dbExecute(
    con,
    paste0(
      "SET temp_directory = ",
      quote(normalizePath(temp_dir, winslash = "/", mustWork = TRUE))
    )
  )
  
  # -------------------------------------------------------------------
  # Import only when the source has changed
  # -------------------------------------------------------------------
  
  info <- file.info(csv)
  
  fingerprint <- paste(
    csv,
    info$size,
    as.numeric(info$mtime),
    "schema_v1",
    sep = "|"
  )
  
  old <- if (DBI::dbExistsTable(con, "source_metadata")) {
    DBI::dbReadTable(con, "source_metadata")$fingerprint
  } else {
    character()
  }
  
  # Include every field referenced by the audit queries.
  required <- c(
    "NBN Atlas record ID",
    "Species ID (TVK)",
    "Scientific name",
    "Taxon Rank",
    "Year",
    "Event Date",
    "End date",
    "Day",
    "OSGR 10km",
    "Coordinate Uncertainty (m)",
    "Occurrence status",
    "Identification verification status",
    "Country",
    "Dataset name"
  )
  
  validate_schema <- function() {
    missing <- setdiff(
      required,
      DBI::dbListFields(con, "records")
    )
    
    if (length(missing)) {
      stop(
        "Missing expected columns: ",
        paste(missing, collapse = ", ")
      )
    }
  }
  
  if (
    force ||
    !identical(old, fingerprint) ||
    !DBI::dbExistsTable(con, "records")
  ) {
    message(
      "Importing CSV into DuckDB on D:. ",
      "Records remain on disk."
    )
    
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(
        con,
        paste0(
          "CREATE OR REPLACE TABLE records AS ",
          "SELECT * FROM read_csv(",
          quote(csv),
          ", header=true, all_varchar=true, strict_mode=true)"
        )
      )
      
      validate_schema()
      
      # Reject imports made while the source file was changing.
      info_after <- file.info(csv)
      
      if (
        !identical(info$size, info_after$size) ||
        !identical(as.numeric(info$mtime), as.numeric(info_after$mtime))
      ) {
        stop(
          "The merged CSV changed during import. ",
          "Finish merging before rerunning the audit."
        )
      }
      
      DBI::dbWriteTable(
        con,
        "source_metadata",
        data.frame(fingerprint = fingerprint),
        overwrite = TRUE
      )
    })
    
  } else {
    message("Merged CSV unchanged; reusing database on D:.")
    validate_schema()
  }
  
  # No cleaning or deduplication of source records.
  # Derived fields are used only for summarisation.
  DBI::dbExecute(con, '
    CREATE OR REPLACE VIEW audit_records AS
    SELECT
      *,
      try_cast("Year" AS INTEGER) AS record_year,
      try_cast("Event Date" AS DATE) AS start_date,
      try_cast("End date" AS DATE) AS end_date,
      nullif(trim("OSGR 10km"), \'\') AS cell,
      nullif(trim("Species ID (TVK)"), \'\') AS taxon_id,
      lower(trim("Taxon Rank")) = \'species\' AS is_species
    FROM records
  ')
  
  # -------------------------------------------------------------------
  # Export on D:, then move only small results into the repository
  # -------------------------------------------------------------------
  
  index <- data.frame(
    table = character(),
    storage = character(),
    path = character(),
    size_MB = numeric(),
    stringsAsFactors = FALSE
  )
  
  index_path <- file.path(repo_out, "audit_table_index.csv")
  
  save_query <- function(name, sql) {
    message("Summarising: ", name)
    
    staged <- tempfile(
      pattern = paste0(name, "_"),
      tmpdir = staging_dir,
      fileext = ".csv"
    )
    
    # DuckDB streams the result to disk; R does not load the result table.
    DBI::dbExecute(
      con,
      paste0(
        "COPY (", sql, ") TO ", quote(staged),
        " (FORMAT CSV, HEADER TRUE, NULL '')"
      )
    )
    
    bytes <- file.info(staged)$size
    small <- bytes <= small_table_mb * 1024^2
    
    destination <- file.path(
      if (small) tables else large_tables,
      paste0(name, ".csv")
    )
    
    alternate <- file.path(
      if (small) large_tables else tables,
      paste0(name, ".csv")
    )
    
    # On Windows, rename does not reliably overwrite existing files.
    if (file.exists(destination)) {
      if (unlink(destination) != 0L) {
        stop(
          "Cannot replace audit table: ", destination,
          "\nNew export remains at: ", staged
        )
      }
    }
    
    if (small) {
      # Copy across drives only after confirming this is a small file.
      if (!file.copy(staged, destination, overwrite = FALSE)) {
        stop(
          "Cannot save small audit table: ", destination,
          "\nExport remains at: ", staged
        )
      }
      
      if (!identical(file.info(destination)$size, bytes)) {
        stop(
          "Audit-table copy has an unexpected size: ", destination,
          "\nOriginal export remains at: ", staged
        )
      }
      
      unlink(staged)
      
    } else {
      # Both locations are on D:, so no large copy enters OneDrive.
      if (!file.rename(staged, destination)) {
        stop(
          "Cannot move large audit table: ", destination,
          "\nExport remains at: ", staged
        )
      }
    }
    
    # Remove a stale copy if the table changed storage category.
    if (file.exists(alternate)) {
      if (unlink(alternate) != 0L) {
        warning("Could not remove stale audit-table copy: ", alternate)
      }
    }
    
    entry <- data.frame(
      table = name,
      storage = if (small) "repository" else "external",
      path = normalizePath(
        destination,
        winslash = "/",
        mustWork = TRUE
      ),
      size_MB = round(bytes / 1024^2, 4),
      stringsAsFactors = FALSE
    )
    
    index <<- rbind(index, entry)
    
    # Updated after each successful export, including partial audit runs.
    write.csv(
      index,
      index_path,
      row.names = FALSE,
      na = ""
    )
    
    message(
      "  Saved to ", entry$storage,
      ": ", entry$path
    )
    
    invisible(destination)
  }
  
  # -------------------------------------------------------------------
  # Audit tables
  # -------------------------------------------------------------------
  
  save_query("overview", '
    SELECT
      count(*) AS records,
      count(DISTINCT "NBN Atlas record ID") AS unique_record_ids,
      count(*) FILTER (
        WHERE nullif("NBN Atlas record ID", \'\') IS NULL
      ) AS missing_record_ids,
      count(DISTINCT taxon_id) AS taxa,
      count(DISTINCT taxon_id) FILTER (
        WHERE is_species
      ) AS species,
      count(DISTINCT cell) AS grid_cells_10km,
      min(record_year) AS first_start_year,
      max(record_year) AS last_start_year,
      count(*) FILTER (
        WHERE cell IS NULL
      ) AS missing_grid10,
      count(*) FILTER (
        WHERE record_year IS NULL
      ) AS missing_or_invalid_year,
      count(*) FILTER (
        WHERE end_date > start_date
      ) AS date_interval_records,
      count(*) FILTER (
        WHERE year(end_date) > record_year
      ) AS multi_year_records
    FROM audit_records
  ')
  
  save_query("duplicate_record_ids", '
    SELECT
      "NBN Atlas record ID" AS record_id,
      count(*) AS copies
    FROM records
    WHERE nullif("NBN Atlas record ID", \'\') IS NOT NULL
    GROUP BY 1
    HAVING count(*) > 1
    ORDER BY copies DESC
  ')
  
  save_query("annual_coverage", '
    SELECT
      record_year AS year,
      count(*) AS records,
      count(DISTINCT taxon_id) FILTER (
        WHERE is_species
      ) AS recorded_species,
      count(DISTINCT cell) AS grid_cells,
      count(*) FILTER (
        WHERE year(end_date) > record_year
      ) AS multi_year_records
    FROM audit_records
    GROUP BY 1
    ORDER BY 1
  ')
  
  save_query("species_coverage", '
    SELECT
      taxon_id,
      "Scientific name" AS scientific_name,
      "Taxon Rank" AS taxon_rank,
      count(*) AS records,
      count(DISTINCT cell) AS grid_cells,
      count(DISTINCT record_year) AS years_recorded,
      min(record_year) AS first_year,
      max(record_year) AS last_year
    FROM audit_records
    GROUP BY 1, 2, 3
    ORDER BY records DESC
  ')
  
  for (field in c(
    "Taxon Rank",
    "Identification verification status",
    "Occurrence status",
    "Coordinate Uncertainty (m)",
    "Country",
    "Dataset name"
  )) {
    identifier <- as.character(
      DBI::dbQuoteIdentifier(con, field)
    )
    
    name <- paste0(
      "categories_",
      gsub("[^a-z0-9]+", "_", tolower(field))
    )
    
    save_query(
      name,
      paste0(
        "SELECT ", identifier, " AS category, ",
        "count(*) AS records ",
        "FROM records ",
        "GROUP BY 1 ",
        "ORDER BY records DESC"
      )
    )
  }
  
  period_sql <- paste0(
    "CAST(floor(record_year / ", period_width, ") * ",
    period_width, " AS INTEGER)"
  )
  
  DBI::dbExecute(
    con,
    paste0(
      "CREATE OR REPLACE VIEW period_records AS ",
      "SELECT *, ", period_sql, " AS period_start ",
      "FROM audit_records"
    )
  )
  
  save_query(
    "grid_period_coverage",
    paste0('
      SELECT
        cell AS grid,
        period_start,
        period_start + ', period_width - 1L, ' AS period_end,
        count(*) AS records,
        count(DISTINCT taxon_id) FILTER (
          WHERE is_species
        ) AS recorded_species,
        count(DISTINCT record_year) AS years_recorded,
        count(DISTINCT start_date) FILTER (
          WHERE nullif("Day", \'\') IS NOT NULL
            AND (end_date IS NULL OR end_date = start_date)
        ) AS distinct_exact_dates,
        count(*) FILTER (
          WHERE year(end_date) > record_year
        ) AS multi_year_records
      FROM period_records
      WHERE cell IS NOT NULL
        AND record_year IS NOT NULL
      GROUP BY 1, 2
      ORDER BY 2, 1
    ')
  )
  
  save_query("grid_overall_coverage", '
    SELECT
      cell AS grid,
      count(*) AS records,
      count(DISTINCT record_year) AS years_recorded,
      count(DISTINCT taxon_id) FILTER (
        WHERE is_species
      ) AS recorded_species
    FROM audit_records
    WHERE cell IS NOT NULL
    GROUP BY 1
    ORDER BY 1
  ')
  
  # -------------------------------------------------------------------
  # Small audit notes remain in the repository
  # -------------------------------------------------------------------
  
  writeLines(
    c(
      paste("Audit completed:", Sys.time()),
      paste("Source:", csv),
      paste("Source bytes:", info$size),
      paste("Source modified:", info$mtime),
      paste("DuckDB database:", db_path),
      paste("DuckDB temporary directory:", temp_dir),
      paste("Large audit tables:", large_tables),
      paste("Repository audit tables:", tables),
      paste("Small-table threshold (MiB):", small_table_mb),
      paste("Table-location index:", index_path),
      paste("Summary period width:", period_width),
      "All summaries retain duplicates; consult duplicate_record_ids.csv.",
      "Period assignment uses starting year, including uncertain date intervals.",
      "Recorded richness is not corrected for effort or detection.",
      paste(
        "Distinct exact dates measure recording activity,",
        "not standardised sampling visits."
      ),
      "Missing British grid references may include records outside Great Britain."
    ),
    file.path(tables, "audit_notes.txt")
  )
  
  message(
    "\nAudit complete.",
    "\nSmall tables: ", tables,
    "\nLarge tables: ", large_tables,
    "\nTable index: ", index_path,
    "\nDatabase: ", db_path
  )
  
  invisible(index)
}

audit_NMRS()