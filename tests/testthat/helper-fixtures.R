# Shared test fixtures — sourced automatically by testthat before every test file.
#
# Modelled on the real template file:
#   makedata/template/NUCR019002_DATA_2024-11-12_1553.xlsx
#
# Key realities reflected here:
#   - ID column holds a TYPE LABEL ("subjid", "visit", "site"), not the col_name
#   - varseq = NA for some imported rows (placed at end after sort)
#   - Derived variables (orig_name = NA) can have transform DSL and/or var.label
#   - Transform DSL may contain R-object references as arguments (e.g. "var.ID")
#   - Label info lives on the SAME row as the variable, not a separate row
#   - All exposures in the real data are numeric  -> they go to $exposure_conti

# -----------------------------------------------------------------------------
# Primary fixture — 10 rows covering every role column
# -----------------------------------------------------------------------------
.fixture_col_info <- function() {
  data.frame(
    col_name  = c(
      "study_id",           # 1  ID: subjid,  varseq=1
      "redcap_event_name",  # 2  ID: visit,   varseq=2,   label_orig
      "site",               # 3  ID: site,    varseq=NA,  label_orig
      "blood_yn",           # 4  no role,     varseq=NA,  label_orig
      "blood_date",         # 5  no role,     varseq=NA
      "seg",                # 6  background,  varseq=10,  Psmodel
      "lymph",              # 7  background,  varseq=11,  Psmodel
      "seg.init",           # 8  exposure_conti, varseq=20, transform DSL, label_derived
      "lymph.init",         # 9  exposure_conti, varseq=21, transform DSL
      "seg_cat"             # 10 cutoff,      varseq=22
    ),
    orig_name = c(
      "record_id",
      "redcap_event_name",
      "redcap_data_access_group",
      "blood_yn",
      "blood_date",
      "seg",
      "lymph",
      NA,   # derived
      NA,   # derived
      NA    # derived
    ),
    col_label = c(
      "Subject ID", "Visit", "Site",
      "Blood draw", "Blood date",
      "Seg (%)", "Lymph (%)",
      "Seg at registration", "Lymph at registration",
      "Seg category"
    ),
    col_type  = c(
      "text", "text", "text",
      "text", "date",
      "numeric", "numeric",
      "numeric", "numeric",
      "numeric"
    ),
    varseq    = c(
      1L, 2L, NA_integer_,
      NA_integer_, NA_integer_,
      10L, 11L,
      20L, 21L,
      22L
    ),
    # --- Role columns ---------------------------------------------------------
    ID = c(
      "subjid", "visit", "site",
      NA, NA, NA, NA, NA, NA, NA
    ),
    exposure = c(
      NA, NA, NA, NA, NA, NA, NA,
      "seg.init", "lymph.init",   # numeric → $exposure_conti
      NA
    ),
    background = c(
      NA, NA, NA, NA, NA,
      "seg", "lymph",
      NA, NA, NA
    ),
    outcome = rep(NA_character_, 10),
    Psmodel = c(
      NA, NA, NA, NA, NA,
      "1", "1",
      NA, NA, NA
    ),
    # --- Annotation columns ---------------------------------------------------
    # transform: DSL string for transform step (cutoff=NA rows)
    #            OR source variable name for cutoff step (cutoff non-NA rows)
    transform = c(
      NA, NA, NA, NA, NA, NA, NA,
      "mf.value_at_spec_visit_func.(seg,redcap_event_name,Reg,study_id,var.ID)",
      "mf.value_at_spec_visit_func.(lymph,redcap_event_name,Reg,study_id,var.ID)",
      "seg"   # source var for the cutoff step — NOT a DSL string
    ),
    cutoff = c(
      NA, NA, NA, NA, NA, NA, NA,
      NA, NA,
      "{<40}{>=40}"
    ),
    var.score = c(
      NA, NA, NA, NA, NA, NA, NA,
      NA, NA,
      "{0}{1}"
    ),
    # var.level / var.label on the same row as the variable definition
    var.level = c(
      NA,
      "{Reg}{W4}{W12}",        # redcap_event_name: label_orig
      "{A}{B}",                # site:              label_orig, varseq=NA
      "{1}{0}",                # blood_yn:          label_orig, varseq=NA
      NA,
      NA, NA,
      "{low}{high}",           # seg.init:          label_derived (orig_name=NA)
      NA,
      NA
    ),
    var.label = c(
      NA,
      "{Registration}{Week 4}{Week 12}",
      "{Site A}{Site B}",
      "{Yes}{No}",
      NA,
      NA, NA,
      "{Low seg}{High seg}",
      NA,
      NA
    ),
    strata_in = rep(NA_character_, 10),
    timepoint = c(
      NA, NA, NA, NA, NA, NA, NA,
      "Reg", "Reg",
      NA
    ),
    stringsAsFactors = FALSE
  )
}


# Fixture with varseq out of order (tests that md_read_col_info sorts)
.fixture_col_info_unsorted <- function() {
  df <- .fixture_col_info()
  # Reverse only the non-NA varseq values; NA positions stay NA
  non_na <- !is.na(df[["varseq"]])
  df[["varseq"]][non_na] <- rev(df[["varseq"]][non_na])
  df
}


# Minimal col_info with only the four required columns (all optional cols absent)
.fixture_col_info_minimal <- function() {
  df <- .fixture_col_info()
  df[, .COL_INFO_REQUIRED, drop = FALSE]
}


# -----------------------------------------------------------------------------
# Long-format clinical data fixture (independent of col_info fixture)
# Two patients × three visits; demographics filled only at the Reg visit.
# -----------------------------------------------------------------------------
.fixture_long_data <- function() {
  data.frame(
    study_id   = c("P01","P01","P01", "P02","P02","P02"),
    visit_name = c("Reg","W4","W12",  "Reg","W4","W12"),
    visit_date = c(NA,   "2023-02-01","2023-03-01",
                   NA,   "2023-04-01","2023-05-01"),
    ic_date    = c("2023-01-01","2023-01-01","2023-01-01",
                   "2023-03-01","2023-03-01","2023-03-01"),
    birth_date = c("1980-01-01", NA, NA,
                   "1975-06-15", NA, NA),
    gender     = c("M", NA, NA,
                   "F", NA, NA),
    height     = c(170, NA, NA,
                   158, NA, NA),
    weight     = c(70,  NA, NA,
                   55,  NA, NA),
    fvc        = c(80, 78, 75,   65, 67, 63),
    stringsAsFactors = FALSE
  )
}
