# md_settings()  --  project configuration constructor
#
# Replaces the per-project settings_*.R scripts.  Returns a named list
# (class "md_settings") that all md_* pipeline functions accept instead of
# relying on global variables.
#
# Required:
#   data_path  --  path to the Excel workbook that contains both the colinfo
#               sheet and the data sheet.
#
# Optional core parameters have sensible defaults matching the original script.
# Project-specific extras (extra data files, truncation windows, etc.) are
# passed as named arguments via `...` and are stored unchanged in the list.


#' Build a makedatar pipeline settings object
#'
#' @export
#' @param data_path       character  --  path to the Excel workbook
#' @param colinfo_sheet   character  --  sheet name for col_info  (default "colinfo")
#' @param data_sheet      character  --  sheet name for raw data   (default "data")
#' @param data_skip       integer    --  header rows to skip in data sheet (default 1L)
#' @param data_na         character  --  NA sentinel in data sheet (default "*")
#' @param colinfo_na      character  --  NA sentinel in colinfo    (default "NA")
#' @param output_dir      character  --  directory for output files (default "./output")
#' @param right_truncation numeric or NULL  --  follow-up cut-off in days; NULL
#'                        means no truncation (default NULL)
#' @param ...             additional project-specific key=value pairs stored
#'                        verbatim in the returned list
#' @return  a named list of class "md_settings"
md_settings <- function(data_path,
                         colinfo_sheet    = "colinfo",
                         data_sheet       = "data",
                         data_skip        = 1L,
                         data_na          = "*",
                         colinfo_na       = "NA",
                         output_dir       = "./output",
                         right_truncation = NULL,
                         ...) {
  if (missing(data_path) || !nzchar(trimws(data_path))) {
    stop("md_settings: 'data_path' is required and must be a non-empty string")
  }

  core <- list(
    data_path        = data_path,
    colinfo_sheet    = colinfo_sheet,
    data_sheet       = data_sheet,
    data_skip        = as.integer(data_skip),
    data_na          = data_na,
    colinfo_na       = colinfo_na,
    output_dir       = output_dir,
    right_truncation = right_truncation
  )

  extras <- list(...)
  s <- c(core, extras)
  class(s) <- "md_settings"
  s
}


#' Validate that a settings object is self-consistent and files exist
#'
#' @export
#' @param settings  an "md_settings" object from md_settings()
#' @param check_files logical  --  whether to verify that `data_path` exists on
#'                    disk (default TRUE)
#' @return  `settings` invisibly if all checks pass; stops with an informative
#'          error otherwise
md_settings_validate <- function(settings, check_files = TRUE) {
  if (!inherits(settings, "md_settings")) {
    stop("md_settings_validate: 'settings' must be created by md_settings()")
  }

  required_core <- c("data_path", "colinfo_sheet", "data_sheet",
                      "data_skip", "data_na", "colinfo_na", "output_dir")
  missing_keys  <- setdiff(required_core, names(settings))
  if (length(missing_keys) > 0L) {
    stop(sprintf("md_settings_validate: missing required keys: %s",
                 paste(missing_keys, collapse = ", ")))
  }

  if (!is.integer(settings$data_skip) || settings$data_skip < 0L) {
    stop("md_settings_validate: 'data_skip' must be a non-negative integer")
  }

  if (!is.null(settings$right_truncation)) {
    if (!is.numeric(settings$right_truncation) ||
        settings$right_truncation <= 0) {
      stop("md_settings_validate: 'right_truncation' must be a positive number or NULL")
    }
  }

  if (check_files && !file.exists(settings$data_path)) {
    stop(sprintf("md_settings_validate: data file not found: %s",
                 settings$data_path))
  }

  invisible(settings)
}


#' @export
print.md_settings <- function(x, ...) {
  cat("-- makedatar settings --\n")
  cat(sprintf("  data_path     : %s\n", x$data_path))
  cat(sprintf("  colinfo_sheet : %s\n", x$colinfo_sheet))
  cat(sprintf("  data_sheet    : %s\n", x$data_sheet))
  cat(sprintf("  data_skip     : %d\n", x$data_skip))
  cat(sprintf("  data_na       : %s\n", x$data_na))
  cat(sprintf("  colinfo_na    : %s\n", x$colinfo_na))
  cat(sprintf("  output_dir    : %s\n", x$output_dir))
  rt <- if (is.null(x$right_truncation)) "none" else
    sprintf("%.0f days (%.1f years)", x$right_truncation,
            x$right_truncation / 365)
  cat(sprintf("  right_truncation: %s\n", rt))
  extras <- setdiff(names(x),
                    c("data_path","colinfo_sheet","data_sheet","data_skip",
                      "data_na","colinfo_na","output_dir","right_truncation"))
  if (length(extras) > 0L) {
    cat("  extras:\n")
    for (k in extras) cat(sprintf("    %s : %s\n", k, x[[k]]))
  }
  invisible(x)
}


