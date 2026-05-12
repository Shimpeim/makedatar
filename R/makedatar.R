# md_make_data()  --  top-level pipeline function
#
# Wires all makedatar steps into a single reproducible call:
#
#   1. md_read_col_info()       --  parse the colinfo sheet
#   2. md_extract_var_roles()   --  extract role sub-tables
#   3. md_import_data()         --  import the data sheet
#   4. md_fill_demographics()   --  broadcast demographics; fill missing visit dates
#   5. md_apply_transform()     --  DSL-driven column derivation
#   6. md_apply_labels()        --  factor-encode original imported columns
#   7. md_apply_cutoff()        --  threshold-based discretisation
#   8. md_apply_labels()        --  factor-encode derived/cutoff columns
#   9. md_apply_strata()        --  row filter to allowed strata
#
# Returns an "md_result" list: $data, $col_info, $roles.
# No global variables are created or modified.


#' Run the full makedatar pipeline from an Excel workbook
#'
#' @export
#' @param settings    an "md_settings" object created by md_settings()
#' @param demo_vars   character vector  --  columns to broadcast per patient
#'                    (default: birth_date, gender, height, weight)
#' @param id_var      character(1) or NULL  --  subject-ID column for
#'                    md_fill_demographics().  NULL (default) auto-detects
#'                    the "subjid" ID column from \code{roles$id}.
#' @param fill_from   named character or NULL  --  passed to
#'                    md_fill_demographics() for date fallback
#'                    (default: \code{c(visit_date = "ic_date")})
#' @param verbose     logical  --  print step-level progress (default FALSE)
#' @return  a named list of class "md_result":
#'   \describe{
#'     \item{data}{data.frame  --  fully processed clinical dataset}
#'     \item{col_info}{data.frame  --  col_info as read and sorted}
#'     \item{roles}{list  --  named role sub-tables from md_extract_var_roles()}
#'   }
md_make_data <- function(settings,
                          demo_vars  = c("birth_date", "gender",
                                         "height", "weight"),
                          id_var     = NULL,
                          fill_from  = c(visit_date = "ic_date"),
                          verbose    = FALSE) {
  if (!inherits(settings, "md_settings")) {
    stop("md_make_data: 'settings' must be an md_settings object")
  }
  md_settings_validate(settings, check_files = TRUE)

  .log <- function(msg) if (verbose) message("[makedatar] ", msg)

  # -- Step 1: col_info -------------------------------------------------------
  .log("reading col_info ...")
  col_info <- md_read_col_info(settings$data_path,
                                sheet = settings$colinfo_sheet,
                                na    = settings$colinfo_na)

  # -- Step 2: role sub-tables ------------------------------------------------
  .log("extracting variable roles ...")
  roles <- md_extract_var_roles(col_info)

  # Resolve subject-ID column
  subj_id_col <- .resolve_id_var(id_var, roles)

  # -- Step 3: import raw data ------------------------------------------------
  .log("importing data sheet ...")
  data <- md_import_data(settings$data_path, col_info,
                          sheet = settings$data_sheet,
                          skip  = settings$data_skip,
                          na    = settings$data_na)

  # -- Step 4: fill demographics + visit dates --------------------------------
  .log("filling demographics ...")
  data <- md_fill_demographics(data,
                                vars      = demo_vars,
                                id_var    = subj_id_col,
                                fill_from = fill_from)

  # -- Step 5: transform (derive new columns via DSL) -------------------------
  .log(sprintf("applying %d transform(s) ...", nrow(roles$transform)))
  id_cols <- roles$id$col_name
  data    <- md_apply_transform(data, roles$transform, id_cols = id_cols)

  # -- Step 6: labels for original imported columns ---------------------------
  .log(sprintf("encoding %d label_orig column(s) ...", nrow(roles$label_orig)))
  data <- md_apply_labels(data, roles$label_orig)

  # -- Step 7: cutoff scoring -------------------------------------------------
  .log(sprintf("applying %d cutoff rule(s) ...", nrow(roles$cutoff)))
  data <- md_apply_cutoff(data, roles$cutoff)

  # -- Step 8: labels for derived columns -------------------------------------
  .log(sprintf("encoding %d label_derived column(s) ...", nrow(roles$label_derived)))
  data <- md_apply_labels(data, roles$label_derived)

  # -- Step 9: strata filter --------------------------------------------------
  .log(sprintf("applying %d strata filter(s) ...", nrow(roles$strata_in)))
  data <- md_apply_strata(data, roles$strata_in)

  .log(sprintf("done  --  %d rows x %d cols", nrow(data), ncol(data)))

  structure(
    list(data = data, col_info = col_info, roles = roles),
    class = "md_result"
  )
}


# --- Helpers -----------------------------------------------------------------

# Auto-detect or validate the subject-ID column.
.resolve_id_var <- function(id_var, roles) {
  if (!is.null(id_var)) return(id_var)

  subj_rows <- roles$id[!is.na(roles$id$ID) & roles$id$ID == "subjid", ]
  if (nrow(subj_rows) > 0L) return(subj_rows$col_name[1])

  # Fallback: first ID column, or the conventional name
  if (nrow(roles$id) > 0L) return(roles$id$col_name[1])
  "study_id"
}


# --- S3 methods for md_result ------------------------------------------------

#' @export
print.md_result <- function(x, ...) {
  cat("-- md_result --\n")
  cat(sprintf("  data    : %d rows x %d cols\n", nrow(x$data), ncol(x$data)))
  cat(sprintf("  col_info: %d variable definitions\n", nrow(x$col_info)))
  cat("  roles   :")
  role_counts <- vapply(x$roles, nrow, integer(1L))
  non_empty   <- role_counts[role_counts > 0L]
  if (length(non_empty) == 0L) {
    cat(" (all empty)\n")
  } else {
    cat("\n")
    for (nm in names(non_empty)) {
      cat(sprintf("    $%-16s %d row(s)\n", nm, non_empty[[nm]]))
    }
  }
  invisible(x)
}
