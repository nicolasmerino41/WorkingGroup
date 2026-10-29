# scripts/merge_NMRS.R
#
# Run from WorkingGroup.Rproj.
# Base R only; copies data in chunks to limit RAM use.
#
# Source CSVs: D:/NMRS/*.csv
# Merged CSV:  D:/NMRS/merged/NMRS_merged.csv
#
# All records and columns are preserved, including duplicates.
# Add future downloads directly to D:/NMRS and rerun.

merge_nmrs <- function(data_root = "D:/NMRS") {
  
  if (!dir.exists(data_root)) {
    stop("NMRS data directory not found: ", data_root)
  }
  
  input_dir <- normalizePath(
    data_root,
    winslash = "/",
    mustWork = TRUE
  )
  
  output_dir <- file.path(input_dir, "merged")
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (!dir.exists(output_dir)) {
    stop("Cannot create merged-data directory: ", output_dir)
  }
  
  # Only CSVs directly inside D:/NMRS are input files.
  # Subdirectories containing merged data or audit outputs are excluded.
  files <- sort(list.files(
    input_dir,
    pattern = "\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = FALSE
  ))
  
  files <- files[!file.info(files)$isdir]
  
  if (!length(files)) {
    stop("No source CSV files found directly inside ", input_dir)
  }
  
  output <- file.path(output_dir, "NMRS_merged.csv")
  temporary <- paste0(output, ".tmp")
  backup <- paste0(output, ".previous")
  
  if (file.exists(backup)) {
    stop(
      "Previous backup exists: ", backup,
      "\nInspect it before rebuilding; it may be from an interrupted run."
    )
  }
  
  if (file.exists(temporary)) {
    stop(
      "Temporary merged file already exists: ", temporary,
      "\nCheck that no merge is running, then remove this incomplete file ",
      "before retrying."
    )
  }
  
  # Validate all headers before writing the merged file.
  header_info <- function(path) {
    con <- file(path, "rb")
    on.exit(close(con))
    
    bytes <- readBin(con, "raw", n = 65536L)
    newline <- which(bytes == as.raw(10))[1]
    
    if (is.na(newline)) {
      stop("Missing or unexpectedly long header: ", path)
    }
    
    header <- sub(
      "\r$",
      "",
      rawToChar(bytes[seq_len(newline - 1L)])
    )
    
    header <- sub("^\ufeff", "", header)
    
    list(
      header = header,
      offset = newline
    )
  }
  
  headers <- lapply(files, header_info)
  reference <- headers[[1]]$header
  
  same <- vapply(
    headers,
    function(x) identical(x$header, reference),
    logical(1)
  )
  
  if (!all(same)) {
    stop(
      "Different CSV headers in: ",
      paste(basename(files[!same]), collapse = ", ")
    )
  }
  
  # Check that sources have not changed during the merge.
  source_before <- file.info(files)[, c("size", "mtime"), drop = FALSE]
  
  dest <- file(temporary, "wb")
  dest_open <- TRUE
  completed <- FALSE
  
  on.exit({
    if (dest_open) {
      try(close(dest), silent = TRUE)
    }
    
    # Remove incomplete output on a handled error.
    # A hard process crash may leave .tmp for manual inspection.
    if (!completed && file.exists(temporary)) {
      unlink(temporary)
    }
  }, add = TRUE)
  
  writeBin(charToRaw(paste0(reference, "\n")), dest)
  
  for (i in seq_along(files)) {
    message(
      "Merging ", i, "/", length(files),
      ": ", basename(files[i])
    )
    
    con <- file(files[i], "rb")
    
    tryCatch({
      seek(
        con,
        where = headers[[i]]$offset,
        origin = "start"
      )
      
      last_byte <- as.raw(10)
      
      repeat {
        chunk <- readBin(
          con,
          "raw",
          n = 8L * 1024L * 1024L
        )
        
        if (!length(chunk)) break
        
        writeBin(chunk, dest)
        last_byte <- tail(chunk, 1L)
      }
      
      if (last_byte != as.raw(10)) {
        writeBin(as.raw(10), dest)
      }
      
    }, finally = close(con))
  }
  
  close(dest)
  dest_open <- FALSE
  
  source_after <- file.info(files)[, c("size", "mtime"), drop = FALSE]
  
  if (!identical(source_before, source_after)) {
    stop(
      "A source CSV changed during merging. ",
      "Wait for downloads or other writes to finish, then rerun."
    )
  }
  
  if (file.exists(output)) {
    if (!file.rename(output, backup)) {
      stop(
        "Cannot replace the previous merged file. ",
        "Close applications using: ", output
      )
    }
  }
  
  if (!file.rename(temporary, output)) {
    restored <- TRUE
    
    if (file.exists(backup)) {
      restored <- file.rename(backup, output)
    }
    
    if (!restored) {
      stop(
        "Could not save the merged file or restore the previous file.",
        "\nPrevious data remain at: ", backup
      )
    }
    
    stop("Could not save merged file; the previous file was restored.")
  }
  
  completed <- TRUE
  
  if (file.exists(backup)) {
    if (unlink(backup) != 0L) {
      warning("Merged file saved, but old backup could not be removed: ", backup)
    }
  }
  
  # Small provenance file; kept beside the merged data.
  manifest <- data.frame(
    source = normalizePath(files, winslash = "/", mustWork = TRUE),
    bytes = source_before$size,
    modified = as.character(source_before$mtime),
    stringsAsFactors = FALSE
  )
  
  write.csv(
    manifest,
    file.path(output_dir, "merge_sources.csv"),
    row.names = FALSE,
    na = ""
  )
  
  message(
    "Saved ", length(files), " source files to:\n",
    output,
    "\nSize: ",
    round(file.info(output)$size / 1024^3, 3),
    " GB"
  )
  
  invisible(output)
}

merge_nmrs()