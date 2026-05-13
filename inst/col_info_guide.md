# How to Build the `col_info` Sheet

`col_info` is the schema table that tells makedatar everything it needs to
know about every column in your study data: what it is called, what type it
holds, what role it plays in the analysis, and how to transform or label it.

It lives on a dedicated sheet (default name: `colinfo`) inside the same Excel
workbook that contains your data.  One row = one variable.

---

## 1. Sheet structure at a glance

| Column | Required? | Short purpose |
|---|---|---|
| `col_name` | **yes** | R name used everywhere downstream |
| `col_type` | **yes** | readxl import type |
| `varseq` | **yes** | column order in the output |
| `orig_name` | **yes** | source name in the data sheet (NA for derived) |
| `col_label` | no | human-readable description |
| `ID` | no | marks identifier columns |
| `exposure` | no | marks exposure (treatment/group) columns |
| `background` | no | marks covariate columns |
| `outcome` | no | marks outcome columns |
| `Psmodel` | no | marks propensity-score model covariates |
| `transform` | no | DSL string (or source variable) for derived columns |
| `cutoff` | no | threshold conditions for scoring a numeric variable |
| `var.score` | no | numeric scores paired with `cutoff` conditions |
| `var.level` | no | factor levels (internal codes) |
| `var.label` | no | factor labels (human-readable strings) |
| `strata_in` | no | allowed values for row-level filtering |
| `timepoint` | no | visit / timepoint annotation |

If an optional column is absent from the sheet entirely, makedatar adds it
automatically as an all-NA column.  You do **not** need to create columns you
will not use.

---

## 2. Required columns

### `col_name`

The name the variable will have in the R data frame.  Must be a valid R
identifier (no spaces; avoid starting with a digit).

```
col_name
--------
study_id
visit_date
fvc_pct
bmi
```

### `col_type`

Tells `readxl::read_excel()` how to parse each column.  Valid values:

| Value | Meaning |
|---|---|
| `text` | Import as character string |
| `numeric` | Import as double |
| `date` | Import as Date |
| `logical` | Import as TRUE/FALSE |
| `skip` | Column exists in the data sheet but must not be imported |

`skip` is useful when the spreadsheet has columns you want to physically
skip over (e.g. free-text notes) without removing them from the file.

### `varseq`

Integer.  Controls the left-to-right column order in the output data frame.
Assign sequential integers; gaps are fine.  Derived variables (those with
`orig_name` NA) can be placed wherever makes sense logically.

### `orig_name`

The column header as it appears in the **data sheet** after the skipped row
(the EDC/export name before renaming).

- Non-NA: the variable comes directly from the data sheet.
- **NA**: the variable is derived (created by `transform` or `cutoff`).

> **Rule of thumb:** if the column exists in your Excel data sheet, fill
> `orig_name`.  If makedatar will compute it from other columns, leave it NA.

---

## 3. Optional annotation columns

### `col_label`

A plain-text description shown in outputs and tables.  No special syntax.

```
col_label
---------
Patient ID
FVC % predicted
Body mass index
```

### `ID`

Marks a column as an identifier.  The most important value is `subjid`,
which makedatar uses to join demographics across visits.

```
ID
------
subjid
visit
```

Leave blank for all non-identifier columns.  You may have more than one ID
column (e.g. subject ID + event sequence number).

### `exposure`

Marks a column as an exposure (treatment / group) variable.  The cell value
is arbitrary text that names the exposure category — it is carried through
into `roles$exposure` and `roles$exposure_conti` for use in downstream
modelling scripts.

- `col_type == "text"` -> goes into `roles$exposure` (categorical)
- `col_type == "numeric"` -> goes into `roles$exposure_conti` (continuous)

```
exposure
--------
treatment_arm
```

### `background`

Same logic as `exposure` but for covariates / confounders.

- `col_type == "text"` -> `roles$background`
- `col_type == "numeric"` -> `roles$background_conti`

### `outcome`

Marks outcome columns.  Valid values:

| Value | Meaning | Roles table |
|---|---|---|
| `event` | Binary event indicator (0/1) | `roles$event` |
| `rank` | Ranked outcome | `roles$event` |
| `time` | Time-to-event (survival time) | `roles$time_to_event` |
| `censored.time` | Censored survival time | `roles$censored_time` |

### `Psmodel`

Set to `1` to include this variable in the propensity-score model covariate
set (`roles$psmodel`).  Leave blank to exclude.

### `timepoint`

Free-text annotation indicating which visit or timepoint the variable
belongs to.  Carried into `roles$exposure` and `roles$background` tables for
downstream filtering; makedatar itself does not act on this column.

---

## 4. Deriving new columns: `transform`

### 4a. Standard transform (DSL expression)

To create a derived variable, set `orig_name` to NA and write a DSL
expression in `transform`.  The grammar is:

```
<function_name>_func.(<arg1>,<arg2>,...)
```

makedatar dispatches to the corresponding `mf.*` function with `data` as the
first argument and the DSL args as positional arguments.

**Available functions**

| DSL function name | What it does |
|---|---|
| `mf.bmi` | `weight_kg / (height_cm / 100)^2` |
| `mf.diff` | `x1 - x2`, joined on ID columns |
| `mf.days_to_year` | Floor of `days / 365` as integer |
| `mf.pack_year` | `(cig_per_day / 20) * years_smoked` |
| `mf.sum` | Row-wise sum of two or more columns |
| `mf.value_at_spec_visit` | Look up a variable's value at a named visit |
| `mf.wday` | Day of week (1 = Sunday, 7 = Saturday) |
| `mf.change_cutoff` | Change from baseline visit |
| `mf.text_parse` | Evaluate an arbitrary R expression |

**The `var.ID` sentinel**

When a function needs the study ID column(s), write `var.ID` as a DSL
argument.  makedatar strips this token before dispatch and passes the actual
ID column names via `id_cols`.  Example:

```
mf.value_at_spec_visit_func.(seg,redcap_event_name,Reg,study_id,var.ID)
```

**Examples**

```
# Compute BMI; weight in col "weight_kg", height in "height_cm"
mf.bmi_func.(weight_kg,height_cm)

# Age in years from an age-in-days column
mf.days_to_year_func.(age_days)

# Change in FVC % from baseline visit "Reg"
mf.change_cutoff_func.(fvc_pct,redcap_event_name,study_id,Reg)

# Pack-years (uses default smk_hist="smoking_hist", non_smk_val="2")
mf.pack_year_func.(cig_per_day,smk_years)

# Sum of three symptom scores
mf.sum_func.(score_a,score_b,score_c)

# Concatenate two text columns
mf.text_parse_func.(paste0(first_name,' ',last_name))
```

The bare string `identity` (no `_func.()`) is also valid and means
"copy the column as-is".

### 4b. Cutoff source variable (used together with `cutoff`)

When `cutoff` is non-NA, the `transform` cell holds the **name of the
existing numeric column** to threshold — not a DSL expression.  See
Section 5.

---

## 5. Threshold scoring: `cutoff` and `var.score`

Used to discretise a continuous variable into a scored integer column.

**Brace-list syntax**

Both `cutoff` and `var.score` use the brace-list encoding: wrap each value
in `{ }` with no separator.

```
{value1}{value2}{value3}
```

**How it works**

makedatar evaluates each condition in `cutoff` against the source variable
`x` and multiplies the Boolean result by the corresponding score in
`var.score`.  The final value is the sum of all such products.

```
result = sum_j ( (x satisfies condition_j) * score_j )
```

NA in the source propagates as NA in the result.

**Condition syntax**

Each condition is an R suffix applied to `x`:

```
<40         ->  x < 40
>=40        ->  x >= 40
>=40 & x<60 ->  x >= 40 and x < 60   (note: repeat 'x' for compound)
```

**Example: two-category scoring (normal / abnormal FVC)**

```
col_name    orig_name   transform    cutoff           var.score   col_type
----------  ----------  -----------  ---------------  ----------  --------
fvc_grade   (NA)        fvc_pct      {>=80}{<80}      {0}{1}      numeric
```

Rows with `fvc_pct >= 80` get score 0; rows with `fvc_pct < 80` get score 1.

**Example: three-category GOLD staging**

```
col_name    transform   cutoff                      var.score   col_type
----------  ----------  --------------------------  ----------  --------
gold_stage  fvc_pct     {>=80}{>=50 & x<80}{<50}   {1}{2}{3}   numeric
```

> **Note:** conditions are evaluated independently and summed.  For mutually
> exclusive categories use non-overlapping ranges so only one condition fires
> per row.

---

## 6. Factor encoding: `var.level` and `var.label`

These two columns work as a pair and use brace-list syntax.

| Column | Meaning |
|---|---|
| `var.level` | The internal codes stored in the data (what R sees) |
| `var.label` | The human-readable strings shown in output |

**`var.level`**

- Non-NA: explicit brace-list of level codes.  The codes must match the
  actual values in the data column.
- **NA**: makedatar infers levels automatically from the column's unique
  non-NA values (sorted alphabetically / numerically).

**`var.label`**

Must always be filled when you want factor encoding.  The number of labels
must match the number of levels exactly.

**Examples**

```
# Explicit levels: numeric codes 1 / 2 mapped to "Male" / "Female"
col_name: gender
var.level: {1}{2}
var.label: {Male}{Female}

# Explicit levels: coded strings mapped to descriptive labels
col_name: smoking_hist
var.level: {1}{2}{3}
var.label: {Current smoker}{Never smoker}{Ex-smoker}

# Implicit levels (inferred from data): cutoff output 0 / 1 -> Normal / Abnormal
col_name: fvc_grade
var.level: (leave blank / NA)
var.label: {Normal}{Abnormal}
```

> **Tip:** For variables derived by `cutoff`, the output is always a numeric
> score (0, 1, 2, ...).  If you leave `var.level` blank, makedatar sorts the
> unique score values and matches them to your labels in ascending order.
> Supply explicit `var.level` values to make the mapping unambiguous.

---

## 7. Row filtering: `strata_in`

`strata_in` is a brace-list of the values that a column **must** contain
for a row to be kept.  Rows whose column value is not in the list are dropped.

```
col_name           strata_in
-----------------  --------------------
redcap_event_name  {Reg}{6M}{12M}
```

This keeps only rows where `redcap_event_name` is one of `Reg`, `6M`, or
`12M`.  Filtering is applied in the order rows appear in `col_info`, after
all transforms, labels, and cutoffs.

---

## 8. Complete worked examples

### Example A: a simple imported variable with labels

A binary sex variable stored as `"1"` / `"2"` in the EDC export:

| col_name | col_type | varseq | orig_name | col_label | background | var.level | var.label |
|---|---|---|---|---|---|---|---|
| gender | text | 5 | Sex | Sex | 1 | {1}{2} | {Male}{Female} |

### Example B: a derived BMI variable

BMI is calculated from existing `weight_kg` and `height_cm` columns:

| col_name | col_type | varseq | orig_name | col_label | transform |
|---|---|---|---|---|---|
| bmi | numeric | 20 | (blank/NA) | BMI (kg/m2) | mf.bmi_func.(weight_kg,height_cm) |

### Example C: a cutoff column derived from BMI

After BMI is computed, grade it into underweight / normal / overweight:

| col_name | col_type | varseq | orig_name | transform | cutoff | var.score | var.label |
|---|---|---|---|---|---|---|---|
| bmi_grade | numeric | 21 | (NA) | bmi | {<18.5}{>=18.5 & x<25}{>=25} | {0}{1}{2} | {Underweight}{Normal}{Overweight} |

> The `transform` column here is the **source column name** (`bmi`), not a
> DSL expression.

### Example D: visit-level value carried to all rows

Carry baseline FVC % (recorded at visit `"Reg"`) to all rows:

| col_name | col_type | varseq | orig_name | transform |
|---|---|---|---|---|
| fvc_pct_baseline | numeric | 30 | (NA) | mf.value_at_spec_visit_func.(fvc_pct,redcap_event_name,Reg,study_id,var.ID) |

---

## 9. Column interaction rules

The table below summarises which column combinations trigger which pipeline steps.

| Situation | `orig_name` | `transform` | `cutoff` | Pipeline step |
|---|---|---|---|---|
| Imported column, no derivation | non-NA | blank | blank | Step 3: imported as-is |
| Imported column, factor encoded | non-NA | blank | blank | Steps 3 + 6: imported then labelled |
| Derived via DSL function | NA | DSL string | blank | Step 5: `md_apply_transform()` |
| Discretised from numeric column | NA | source col name | non-NA | Step 7: `md_apply_cutoff()` |
| Discretised + factor encoded | NA | source col name | non-NA | Steps 7 + 8 |
| Row filter | any | blank | blank | Step 9: `md_apply_strata()` |

> A row with both `transform` (DSL) and `cutoff` non-NA is treated as a
> **cutoff row**: the `transform` cell is read as a source column name, not
> parsed as a DSL expression.

---

## 10. Common mistakes

| Mistake | Symptom | Fix |
|---|---|---|
| Comma inside a `mf.text_parse` expression | DSL parser splits on the comma, producing a wrong argument list | Avoid commas in the expression, or wrap the logic in a helper column first |
| `var.level` count != `var.label` count | Error: "N levels but M labels" | Count the `{}` pairs in both cells — they must match |
| `cutoff` condition count != `var.score` count | Error: "N conditions but M scores" | Count the `{}` pairs in both cells |
| `orig_name` non-NA for a derived column | makedatar tries to import a column that does not exist in the data sheet | Set `orig_name` to blank/NA for all derived columns |
| `col_type` mismatch | Numeric column imported as text, or vice versa | Check the readxl column type; `"skip"` silently drops the column |
| Using `var.ID` in a function that doesn't need it | Extra argument ignored (harmless) or error if function signature doesn't accept `...` | Only include `var.ID` for functions documented to use `id_cols` |
