# ============================================================
# NIS 2023 Core: Correctly load the HCUP fixed-width ASCII file
# and do an initial explore of cancer + VTE coding
#
# WHY NOT fread()/read_delim(): NIS_2023_Core.ASC has no field
# delimiter. Every discharge record is one 643-character line
# with each variable packed into a fixed set of byte positions
# (see data_specs/SASload_NIS_2023_Core.SAS, HCUP's own load
# program). Reading it as delimited text -- as an earlier draft
# of this script did with fread() -- doesn't error, it just
# silently misaligns every column past the first few. This
# script instead parses the byte positions straight out of the
# HCUP load program so they can't drift out of sync by hand.
#
# IMPORTANT SCOPE NOTE: NIS is discharge/administrative claims
# data. It has NO pharmacy/medication file, so anticoagulation
# (drug receipt) is not directly observable here -- see Section
# 5 for the only weak proxy available (IVC filter placement).
# ============================================================


# ----- 1. LOAD PACKAGES -----
library(tidyverse)


# ----- 2. PATHS -----
nis_file      <- "~/Downloads/NIS_2023_Core.ASC"
sas_load_file <- "data_specs/SASload_NIS_2023_Core.SAS"


# ----- 3. PARSE COLUMN LAYOUT FROM HCUP'S OWN LOAD PROGRAM -----
# Pulls @<byte position>, variable name, and informat straight out
# of the INPUT statement instead of hand-transcribing ~140 start/end
# positions (which is exactly the kind of manual-transcription bug
# this repo has caught before in other scripts).
parse_hcup_sas_load <- function(sas_path) {
  lines <- read_lines(sas_path)

  lrecl_line <- lines %>% str_subset("LRECL\\s*=") %>% pluck(1)
  lrecl <- as.integer(str_match(lrecl_line, "LRECL\\s*=\\s*([0-9]+)")[, 2])

  input_pattern <- "^\\s*@([0-9]+)\\s+(\\S+)\\s+(\\$?[A-Za-z0-9_]+)\\.\\s*$"
  parsed <- lines %>%
    str_subset(input_pattern) %>%
    str_match(input_pattern)

  start    <- as.integer(parsed[, 2])
  var      <- parsed[, 3]
  informat <- parsed[, 4]

  end <- c(start[-1] - 1L, lrecl)

  list(
    lrecl = lrecl,
    spec = tibble(
      var      = var,
      start    = start,
      end      = end,
      width    = end - start + 1L,
      type     = if_else(str_starts(informat, fixed("$")), "character", "numeric"),
      informat = informat
    )
  )
}

parsed_load <- parse_hcup_sas_load(sas_load_file)
col_spec <- parsed_load$spec
lrecl <- parsed_load$lrecl

# Sanity check: positions parsed from the load program must tile the
# record exactly against the LRECL declared in the INFILE statement
# (an independent source, not derived from the widths themselves),
# with no gaps, overlaps, or duplicate names, before trusting them to
# read millions of rows.
stopifnot(
  "Parsed column widths don't sum to the declared LRECL -- re-check the load program parse" =
    sum(col_spec$width) == lrecl,
  "Gap or overlap between consecutive parsed columns" =
    all(col_spec$end[-nrow(col_spec)] + 1L == col_spec$start[-1]),
  "Duplicate variable names parsed from load program" =
    !any(duplicated(col_spec$var))
)
cat("Parsed", nrow(col_spec), "columns from", sas_load_file,
    "spanning", sum(col_spec$width), "bytes.\n")


# ----- 4. LOAD THE FIXED-WIDTH FILE -----
col_types_str <- paste0(if_else(col_spec$type == "character", "c", "d"),
                        collapse = "")

nis <- read_fwf(
  nis_file,
  col_positions = fwf_positions(
    start     = col_spec$start,
    end       = col_spec$end,
    col_names = col_spec$var
  ),
  col_types = col_types_str
)

dx_cols <- col_spec$var[str_detect(col_spec$var, "^I10_DX[0-9]+$")]
pr_cols <- col_spec$var[str_detect(col_spec$var, "^I10_PR[0-9]+$")]

# Unused DX/PR slots are blank-padded in the ASCII file; read_fwf()
# reads them as "" rather than NA -- clean that up before matching.
nis <- nis %>%
  mutate(across(all_of(c(dx_cols, pr_cols)), ~ na_if(str_trim(.x), "")))

# --- Recode HCUP's numeric missing-value sentinels to NA ---
# HCUP informats (see PROC FORMAT INVALUE block in the load program)
# encode missing/invalid/inapplicable/not-available as all-9s/8s/6s/5s
# strings matched to each field's width (e.g. AGE is N3PF -> "-99",
# "-88", "-66" are missing codes, not real ages of -99). Collapsing
# all three/four subtypes to plain NA is fine for this exploratory
# pass; revisit if a specific analysis needs to distinguish "not
# applicable" from "unknown".
hcup_missing_codes <- list(
  N2PF    = c("-9", "-8", "-6", "-5"),
  N3PF    = c("-99", "-88", "-66"),
  N4PF    = c("-999", "-888", "-666"),
  N4P1F   = c("-9.9", "-8.8", "-6.6"),
  N5PF    = c("-9999", "-8888", "-6666"),
  N5P2F   = c("-9.99", "-8.88", "-6.66"),
  N6PF    = c("-99999", "-88888", "-66666"),
  N6P2F   = c("-99.99", "-88.88", "-66.66"),
  N7P2F   = c("-999.99", "-888.88", "-666.66"),
  N8PF    = c("-9999999", "-8888888", "-6666666"),
  N8P2F   = c("-9999.99", "-8888.88", "-6666.66"),
  N8P4F   = c("-99.9999", "-88.8888", "-66.6666"),
  N10PF   = c("-999999999", "-888888888", "-666666666"),
  N10P4F  = c("-9999.9999", "-8888.8888", "-6666.6666"),
  N10P5F  = c("-999.99999", "-888.88888", "-666.66666"),
  DATE10F = c("-999999999", "-888888888", "-666666666"),
  N11P7F  = c("-99.9999999", "-88.8888888", "-66.6666666"),
  N12P2F  = c("-99999999.99", "-88888888.88", "-66666666.66"),
  N12P5F  = c("-99999.99999", "-88888.88888", "-66666.66666"),
  N13PF   = c("-999999999999", "-888888888888", "-666666666666"),
  N15P2F  = c("-99999999999.99", "-88888888888.88", "-66666666666.66")
)

apply_hcup_missing <- function(df, col_spec, codes_lookup) {
  numeric_specs <- col_spec %>% filter(type == "numeric")
  for (i in seq_len(nrow(numeric_specs))) {
    var <- numeric_specs$var[i]
    informat <- numeric_specs$informat[i]
    codes <- codes_lookup[[informat]]
    if (is.null(codes)) {
      stop(sprintf(
        "No missing-value codes defined for informat '%s' (variable %s) -- add it to hcup_missing_codes before trusting this column.",
        informat, var
      ))
    }
    sentinel_values <- as.numeric(codes)
    df[[var]][df[[var]] %in% sentinel_values] <- NA
  }
  df
}

nis <- apply_hcup_missing(nis, col_spec, hcup_missing_codes)


# ----- 5. QUICK EXPLORE -----
dim(nis)
head(nis)
colnames(nis)
glimpse(nis)

nrow(nis)

nis %>% select(all_of(dx_cols)) %>% head()
nis %>% select(all_of(pr_cols)) %>% head()


# ----- 6. CANCER + VTE DIAGNOSIS CODE FLAGS -----
# Reference only -- narrow these regexes to your actual study
# definition before using them for a real analysis.
#
# Cancer: any code starting with "C" is a malignant neoplasm in
# ICD-10-CM (Chapter 2 spans C00-D49; every "C..." code, including
# the newer C4A/C7A/C7B/C88 additions, is malignant). This does NOT
# include D00-D09 (in situ) or D37-D48 (uncertain/unknown behavior)
# -- add those explicitly if your definition needs them.
#
# VTE: I26 (pulmonary embolism), I80 (phlebitis/thrombophlebitis),
# I82 (other venous embolism and thrombosis, incl. most DVT sites).
# Caveat: I80 includes superficial thrombophlebitis (I80.0-I80.3),
# which many VTE algorithms exclude since it isn't a "true" VTE --
# narrow to I80.1-I80.3/I80.8-I80.9 if that distinction matters for
# your outcome. I81 (portal vein thrombosis) and pregnancy-specific
# O22/O87 codes are deliberately NOT included by default here.
#
# Anticoagulation: NOT available. NIS has no medication/pharmacy
# file, so anticoagulant receipt cannot be observed directly. The
# only procedure-level proxy is IVC filter placement (ICD-10-PCS
# 06H0-, 06H3-, 06H4-), which signals a decision NOT to (or inability
# to) anticoagulate -- it is not evidence of anticoagulation itself.
cancer_regex <- "^C"
vte_regex    <- "^(I26|I80|I82)"
ivc_filter_regex <- "^06H[034]"

any_code_match <- function(df, cols, regex) {
  matches <- lapply(cols, function(col) {
    x <- df[[col]]
    x[is.na(x)] <- ""
    grepl(regex, x)
  })
  Reduce(`|`, matches)
}

nis <- nis %>%
  mutate(
    any_cancer_dx    = any_code_match(., dx_cols, cancer_regex),
    any_vte_dx       = any_code_match(., dx_cols, vte_regex),
    any_ivc_filter   = any_code_match(., pr_cols, ivc_filter_regex)
  )

cat("\n========== CANCER + VTE CODING SUMMARY ==========\n")
cat("Total discharge records:", nrow(nis), "\n")
cat("Records with any cancer diagnosis (C-code, any DX position):",
    sum(nis$any_cancer_dx), "\n")
cat("Records with any VTE diagnosis (I26/I80/I82, any DX position):",
    sum(nis$any_vte_dx), "\n")
cat("Records with both cancer AND VTE diagnosis:",
    sum(nis$any_cancer_dx & nis$any_vte_dx), "\n")
cat("Records with an IVC filter placement procedure:",
    sum(nis$any_ivc_filter), "\n")
