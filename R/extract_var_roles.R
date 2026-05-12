# =============================================================================
# md_extract_var_roles()
#
# Extracts typed variable-role sub-tables from a validated col_info data.frame.
# Replaces the twelve var.* assignments scattered through makedata_01.R.
#
# Return value  --  named list:
#
#   $id              col_name, ID                        [was: var.ID]
#   $exposure        col_name, exposure, timepoint       [was: var.exposure]       col_type=="text"
#   $exposure_conti  col_name, exposure, timepoint       [was: var.exposure.conti] col_type=="numeric"
#   $background      col_name, background, timepoint     [was: var.background]     col_type=="text"
#   $background_conti col_name, background, timepoint   [was: var.background.conti] col_type=="numeric"
#   $event           col_name, col_label, outcome, col_type  [was: var.event]      outcome %in% c("event","rank")
#   $time_to_event   col_name, col_type               [was: var.timetoevent]       outcome=="time"
#   $censored_time   col_name, col_type               [was: var.cens_timetoevent]  outcome=="censored.time"
#   $psmodel         col_name, col_type               [was: var.Psmodel]           Psmodel=="1"
#   $transform       transform, col_name, col_type   [was: var.trans]              transform non-NA & cutoff NA
#   $cutoff          col_name, transform, var.score, cutoff, col_type  [was: var.cutoff]
#   $label           orig_name, col_name, var.level, var.label, col_type  [was: var.label]
#   $label_orig      subset of $label where orig_name non-NA & col_type!="skip"    [was: var.label.orig_data]
#   $label_derived   subset of $label where orig_name NA & col_type!="skip"        [was: var.label.transformed_data]
#   $strata_in       col_name, strata_in              [was: var.strata_in]
# =============================================================================


#' Extract typed variable-role sub-tables from a col_info data.frame
#'
#' @param col_info A `data.frame` returned by [md_read_col_info()].
#'
#' @return A named list of data.frames, one per variable role.  All elements
#'   are always present; an empty data.frame (0 rows) is returned when no
#'   variables satisfy a role's filter.  See **Details** for the mapping to the
#'   original `var.*` objects.
#'
#' @details
#' | List element | Original object | Filter |
#' |---|---|---|
#' | `$id` | `var.ID` | `ID` non-NA |
#' | `$exposure` | `var.exposure` | `exposure` non-NA, `col_type == "text"` |
#' | `$exposure_conti` | `var.exposure.conti` | `exposure` non-NA, `col_type == "numeric"` |
#' | `$background` | `var.background` | `background` non-NA, `col_type == "text"` |
#' | `$background_conti` | `var.background.conti` | `background` non-NA, `col_type == "numeric"` |
#' | `$event` | `var.event` | `outcome %in% c("event", "rank")` |
#' | `$time_to_event` | `var.timetoevent` | `outcome == "time"` |
#' | `$censored_time` | `var.cens_timetoevent` | `outcome == "censored.time"` |
#' | `$psmodel` | `var.Psmodel` | `Psmodel == "1"` |
#' | `$transform` | `var.trans` | `transform` non-NA **and** `cutoff` NA |
#' | `$cutoff` | `var.cutoff` | `cutoff` non-NA |
#' | `$label` | `var.label` | `var.label` non-NA |
#' | `$label_orig` | `var.label.orig_data` | subset of `$label`: `orig_name` non-NA, `col_type != "skip"` |
#' | `$label_derived` | `var.label.transformed_data` | subset of `$label`: `orig_name` NA, `col_type != "skip"` |
#' | `$strata_in` | `var.strata_in` | `strata_in` non-NA |
#'
#' @examples
#' \dontrun{
#' col_info  <- md_read_col_info("workbook.xlsx")
#' var_roles <- md_extract_var_roles(col_info)
#' var_roles$id          # ID column names
#' var_roles$exposure    # categorical exposure variables
#' var_roles$transform   # variables requiring transformation
#' }
#'
#' @export
md_extract_var_roles <- function(col_info) {
  .assert_cols(col_info, .COL_INFO_REQUIRED, label = "col_info")

  # Ensure optional columns exist (md_read_col_info guarantees this, but
  # callers may pass hand-built data.frames in tests)
  for (col in .COL_INFO_OPTIONAL) {
    if (!col %in% colnames(col_info)) col_info[[col]] <- NA_character_
  }

  ci <- col_info  # shorthand

  list(

    # --- Identity variables ---------------------------------------------------
    id = .subset_cols(ci,
      !is.na(ci[["ID"]]),
      c("col_name", "ID")
    ),

    # --- Exposure variables ---------------------------------------------------
    exposure = .subset_cols(ci,
      !is.na(ci[["exposure"]]) & .eq_safe(ci[["col_type"]], "text"),
      c("col_name", "exposure", "timepoint")
    ),

    exposure_conti = .subset_cols(ci,
      !is.na(ci[["exposure"]]) & .eq_safe(ci[["col_type"]], "numeric"),
      c("col_name", "exposure", "timepoint")
    ),

    # --- Background / covariate variables ------------------------------------
    background = .subset_cols(ci,
      !is.na(ci[["background"]]) & .eq_safe(ci[["col_type"]], "text"),
      c("col_name", "background", "timepoint")
    ),

    background_conti = .subset_cols(ci,
      !is.na(ci[["background"]]) & .eq_safe(ci[["col_type"]], "numeric"),
      c("col_name", "background", "timepoint")
    ),

    # --- Outcome variables ----------------------------------------------------
    event = .subset_cols(ci,
      !is.na(ci[["outcome"]]) &
        ci[["outcome"]] %in% c("event", "rank"),
      c("col_name", "col_label", "outcome", "col_type")
    ),

    time_to_event = .subset_cols(ci,
      !is.na(ci[["outcome"]]) & .eq_safe(ci[["outcome"]], "time"),
      c("col_name", "col_type")
    ),

    censored_time = .subset_cols(ci,
      !is.na(ci[["outcome"]]) & .eq_safe(ci[["outcome"]], "censored.time"),
      c("col_name", "col_type")
    ),

    # --- Propensity-score model variables ------------------------------------
    psmodel = .subset_cols(ci,
      !is.na(ci[["Psmodel"]]) & .eq_safe(ci[["Psmodel"]], "1"),
      c("col_name", "col_type")
    ),

    # --- Transform variables (non-cutoff rows only) --------------------------
    # Note: when cutoff is non-NA, the transform column holds the SOURCE
    # variable name (not a DSL expression), so those rows are excluded here.
    transform = .subset_cols(ci,
      !is.na(ci[["transform"]]) & is.na(ci[["cutoff"]]),
      c("transform", "col_name", "col_type")
    ),

    # --- Cutoff-based discretisation variables --------------------------------
    cutoff = .subset_cols(ci,
      !is.na(ci[["cutoff"]]),
      c("col_name", "transform", "var.score", "cutoff", "col_type")
    ),

    # --- Factor-label variables -----------------------------------------------
    label = {
      lbl <- .subset_cols(ci,
        !is.na(ci[["var.label"]]),
        c("orig_name", "col_name", "var.level", "var.label", "col_type")
      )
      lbl
    },

    label_orig = {
      lbl <- .subset_cols(ci,
        !is.na(ci[["var.label"]]),
        c("orig_name", "col_name", "var.level", "var.label", "col_type")
      )
      .subset_rows(lbl,
        !is.na(lbl[["orig_name"]]) & !.eq_safe(lbl[["col_type"]], "skip")
      )
    },

    label_derived = {
      lbl <- .subset_cols(ci,
        !is.na(ci[["var.label"]]),
        c("orig_name", "col_name", "var.level", "var.label", "col_type")
      )
      .subset_rows(lbl,
        is.na(lbl[["orig_name"]]) & !.eq_safe(lbl[["col_type"]], "skip")
      )
    },

    # --- Stratum-inclusion filter variables -----------------------------------
    strata_in = .subset_cols(ci,
      !is.na(ci[["strata_in"]]),
      c("col_name", "strata_in")
    )
  )
}


# --- Private helpers ----------------------------------------------------------

# Row-and-column subset, always returning a data.frame (never a vector)
.subset_cols <- function(df, row_mask, cols) {
  df[row_mask, cols, drop = FALSE]
}

# Row subset preserving data.frame class
.subset_rows <- function(df, row_mask) {
  df[row_mask, , drop = FALSE]
}

# NA-safe string equality  --  avoids `==` producing NA for NA inputs
.eq_safe <- function(x, val) !is.na(x) & x == val
