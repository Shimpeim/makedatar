# Transform functions for md_apply_transform()
#
# All functions share the same calling convention used by the dispatcher:
#   - data    -- the full data.frame (always first, named)
#   - positional args from the DSL string
#   - id_cols -- character vector of ID column names (always named, last)
#   - ...     -- absorbs any extra named args silently
#
# The var.ID token in DSL strings is stripped before dispatch; id_cols is
# injected as a named argument by md_apply_transform().


# --- mf.bmi ------------------------------------------------------------------
# BMI: weight_kg / (height_cm / 100)^2
# DSL example: mf.bmi_func.(weight,height)

#' Compute BMI from weight (kg) and height (cm) columns
#' @param data data.frame
#' @param x1 column name -- weight in kg
#' @param x2 column name -- height in cm
#' @param ... ignored
#' @param id_cols character vector of ID column names
#' @return numeric vector
#' @export
mf.bmi <- function(data, x1, x2, ..., id_cols = character(0)) {
  as.numeric(data[[x1]]) / (as.numeric(data[[x2]]) / 100)^2
}


# --- mf.diff -----------------------------------------------------------------
# Difference: x1 - x2, joined on id_cols (NA rows of x2 produce NA result).
# DSL example: mf.diff_func.(post_fvc,pre_fvc)

#' Compute the difference between two columns, optionally joined on id_cols
#'
#' @param data data.frame
#' @param x1 column name -- minuend (the value to subtract from)
#' @param x2 column name -- subtrahend (the value to subtract); NA rows
#'   produce NA in the result
#' @param ... ignored
#' @param id_cols character vector of ID column names used as join key.
#'   If empty, a simple element-wise difference is returned.
#' @return numeric vector, same length as \code{nrow(data)}
#' @export
mf.diff <- function(data, x1, x2, ..., id_cols = character(0)) {
  id_and_x1 <- data[, c(id_cols, x1), drop = FALSE]
  id_and_x2 <- data[!is.na(data[[x2]]), c(id_cols, x2), drop = FALSE]
  if (length(id_cols) == 0L) {
    # No ID columns -- simple element-wise diff
    return(as.numeric(data[[x1]]) - as.numeric(data[[x2]]))
  }
  df <- merge(id_and_x1, id_and_x2, by = id_cols, all.x = TRUE, sort = FALSE)
  # Restore original row order
  df <- df[match(do.call(paste, data[, id_cols, drop = FALSE]),
                 do.call(paste, df[, id_cols, drop = FALSE])), ]
  as.numeric(df[[x1]]) - as.numeric(df[[x2]])
}


# --- mf.days_to_year ---------------------------------------------------------
# Convert a numeric-days column to whole years (floor).
# DSL example: mf.days_to_year_func.(age_days)

#' Convert a numeric days column to whole years (floor division)
#'
#' @param data data.frame
#' @param x1 column name -- numeric days value
#' @param ... ignored
#' @param id_cols character vector of ID column names
#' @return integer vector (floor of days / 365)
#' @export
mf.days_to_year <- function(data, x1, ..., id_cols = character(0)) {
  as.integer(floor(as.numeric(data[[x1]]) / 365))
}


# --- mf.pack_year ------------------------------------------------------------
# Pack-year = (cigarettes_per_day / 20) * years_smoked.
# Rows where smk_hist == non_smk_val are set to 0 (never-smokers).
# DSL example: mf.pack_year_func.(cig_per_day,smk_years)

#' Compute pack-years of cigarette smoking
#'
#' Pack-years = (cigarettes per day / 20) * years smoked.
#' Rows identified as never-smokers via \code{smk_hist} are forced to 0.
#'
#' @param data data.frame
#' @param x1 column name -- cigarettes smoked per day
#' @param x2 column name -- years smoked
#' @param smk_hist column name for the smoking-history flag
#'   (default \code{"smoking_hist"})
#' @param non_smk_val value in \code{smk_hist} that identifies never-smokers
#'   (default \code{"2"})
#' @param ... ignored
#' @param id_cols character vector of ID column names
#' @return numeric vector of pack-years
#' @export
mf.pack_year <- function(data, x1, x2,
                          smk_hist    = "smoking_hist",
                          non_smk_val = "2",
                          ...,
                          id_cols = character(0)) {
  res <- as.numeric(data[[x1]]) / 20 * as.numeric(data[[x2]])
  if (smk_hist %in% colnames(data)) {
    never <- !is.na(data[[smk_hist]]) & as.character(data[[smk_hist]]) == non_smk_val
    res[never] <- 0
  }
  res
}


# --- mf.sum ------------------------------------------------------------------
# Row-wise sum of two or more numeric columns.
# DSL example: mf.sum_func.(score_a,score_b,score_c)

#' Row-wise sum of two or more numeric columns
#'
#' @param data data.frame
#' @param ... column names to sum (passed as positional DSL arguments)
#' @param id_cols character vector of ID column names (excluded from summation)
#' @return character vector of row sums (converted via \code{as.character})
#' @export
mf.sum <- function(data, ..., id_cols = character(0)) {
  cols <- c(...)
  # strip id_cols from the list in case they leaked through
  cols <- setdiff(cols, id_cols)
  mat  <- vapply(cols, function(col) as.numeric(data[[col]]),
                 numeric(nrow(data)))
  as.character(rowSums(mat, na.rm = FALSE))
}


# --- mf.value_at_spec_visit --------------------------------------------------
# For each row return the value of `var.targ` recorded at the visit named
# `visit_name`, looked up via the join key `var.key`.
# DSL example: mf.value_at_spec_visit_func.(seg,redcap_event_name,Reg,study_id,var.ID)
#   (the `var.ID` token is stripped by the dispatcher -- id_cols is injected)

#' Look up the value of a variable at a specific visit
#'
#' For each row, returns the value of \code{var.targ} recorded at the visit
#' whose label equals \code{visit_name}, matched via the key column
#' \code{var.key}.  Duplicate keys at the target visit are resolved by
#' keeping the first occurrence.
#'
#' @param data data.frame
#' @param var.targ column name -- the target variable to look up
#' @param var.visit column name -- the visit-label column
#' @param visit_name character(1) -- the visit label to filter on
#' @param var.key column name -- the join key (e.g. subject ID)
#' @param ... ignored
#' @param id_cols character vector of ID column names
#' @return vector of looked-up values, same length as \code{nrow(data)}
#' @export
mf.value_at_spec_visit <- function(data, var.targ, var.visit, visit_name,
                                    var.key,
                                    ...,
                                    id_cols = character(0)) {
  at_visit <- data[!is.na(data[[var.visit]]) & data[[var.visit]] == visit_name,
                   c(var.key, var.targ), drop = FALSE]
  # Deduplicate lookup table on var.key (keep first occurrence)
  at_visit <- at_visit[!duplicated(at_visit[[var.key]]), , drop = FALSE]
  idx <- match(data[[var.key]], at_visit[[var.key]])
  at_visit[[var.targ]][idx]
}


# --- mf.wday -----------------------------------------------------------------
# Day-of-week (1 = Sunday ... 7 = Saturday) from a date column.
# DSL example: mf.wday_func.(visit_date)

#' Extract day of week from a date column
#'
#' Returns an integer (1 = Sunday, 7 = Saturday) via
#' \code{lubridate::wday()}.
#'
#' @param data data.frame
#' @param x column name -- a date column
#' @param ... ignored
#' @param id_cols character vector of ID column names
#' @return integer vector (1 = Sunday, 7 = Saturday)
#' @export
#' @importFrom lubridate wday
mf.wday <- function(data, x, ..., id_cols = character(0)) {
  lubridate::wday(data[[x]])
}


# --- mf.change_cutoff --------------------------------------------------------
# Change from a baseline visit: x1(current) - x1(baseline).
# x2       -- the visit-label column name
# study_id -- the subject-ID column name
# baseline -- the baseline visit label string
# DSL example: mf.change_cutoff_func.(percent_fvc,redcap_event_name,study_id,Reg)

#' Compute change from a baseline visit value
#'
#' For each row, subtracts the baseline-visit value of \code{x1} from the
#' current-row value of \code{x1}.  The baseline visit is identified by rows
#' where \code{x2} equals \code{baseline}.
#'
#' @param data data.frame
#' @param x1 column name -- the outcome variable
#' @param x2 column name -- the visit-label column
#' @param study_id column name -- the subject identifier
#' @param baseline character(1) -- the visit label for baseline
#' @param ... ignored
#' @param id_cols character vector of ID column names
#' @return numeric vector of change-from-baseline values
#' @export
mf.change_cutoff <- function(data, x1, x2, study_id, baseline,
                              ...,
                              id_cols = character(0)) {
  base_rows <- !is.na(data[[x2]]) & data[[x2]] == baseline &
               !is.na(data[[x1]])
  base_df <- data[base_rows, c(study_id, x1), drop = FALSE]
  colnames(base_df)[colnames(base_df) == x1] <- ".baseline"
  base_df <- base_df[!duplicated(base_df[[study_id]]), , drop = FALSE]
  idx <- match(data[[study_id]], base_df[[study_id]])
  as.numeric(data[[x1]]) - as.numeric(base_df$.baseline[idx])
}


# --- mf.text_parse -----------------------------------------------------------
# Evaluate an arbitrary R expression with columns of `data` in scope.
# x1 is the expression string (from the DSL / Excel cell).
# DSL example: mf.text_parse_func.(paste0(first_name,' ',last_name))

#' Evaluate an R expression string with data columns in scope
#'
#' Evaluates \code{x1} as an R expression using \code{with(data, ...)},
#' making all columns of \code{data} available by name.
#'
#' @param data data.frame
#' @param x1 character(1) -- an R expression string to evaluate
#' @param ... ignored
#' @param id_cols character vector of ID column names
#' @return result of evaluating the expression (typically a vector)
#' @export
mf.text_parse <- function(data, x1, ..., id_cols = character(0)) {
  with(data, eval(parse(text = x1)))
}
