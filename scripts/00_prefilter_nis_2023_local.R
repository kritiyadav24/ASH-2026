# ============================================================
# RUN THIS LOCALLY (on your machine, not in the cloud session).
#
# Streams through the full NIS 2023 Core file via DuckDB (no need to
# load the whole multi-GB file into R memory) and writes out a small
# CSV containing only:
#   - the 18-50 / principal-diagnosis-cancer (C00-C97 excl. C44) cohort
#   - the columns the disparities analysis actually needs
#
# The output should be small enough to upload into the cloud session
# (likely tens of MB rather than GBs). Once uploaded, the main script
# (nis_2023_early_onset_cancer_disparities.R) can read it directly --
# just skip its Section 6 cohort-restriction steps since this file is
# already restricted.
#
# Usage:
#   1. Edit NIS_CORE_PATH below to point at your full NIS 2023 core CSV
#      (the one inside NIS_2023.zip / the NIS_2023 folder -- NOT
#      cc2023NIS.csv, which is the separate hospital-level file).
#   2. Rscript 00_prefilter_nis_2023_local.R
#   3. Upload the resulting nis_2023_cancer_cohort_prefiltered.csv
# ============================================================

# install.packages("duckdb")
library(duckdb)

NIS_CORE_PATH  <- "~/Downloads/NIS_2023/NIS_2023_Core.csv"   # <-- EDIT THIS
OUTPUT_PATH    <- "~/Downloads/nis_2023_cancer_cohort_prefiltered.csv"

con <- dbConnect(duckdb())

# Peek at the actual column names first -- NIS core file column names/
# capitalization can vary slightly by extract vendor/tool. Run this and
# check the output BEFORE trusting the query below; adjust column names
# in the SELECT/WHERE clauses if anything doesn't match.
peek <- dbGetQuery(con, sprintf(
  "SELECT * FROM read_csv_auto('%s', SAMPLE_SIZE=-1) LIMIT 1", NIS_CORE_PATH
))
cat("Columns found in the core file:\n")
print(names(peek))

# Build the cancer-diagnosis regex: C00-C97 excluding C44, matching on
# the undotted ICD-10-CM category code stored at the start of I10_DX1.
cancer_prefixes <- sprintf("C%02d", c(0:43, 45:97))
cancer_regex    <- paste0("^(", paste(cancer_prefixes, collapse = "|"), ")")

# Columns kept: demographics/exposure/covariates, all diagnosis and
# procedure columns (needed for complication/procedure flagging in the
# main script), and the survey-design variables. If your file's I10_DX/
# I10_PR column counts differ from 40/25, DuckDB will just return
# however many exist via the wildcard-style UNION below.
dx_cols <- grep("^I10_DX[0-9]+$", names(peek), value = TRUE)
pr_cols <- grep("^I10_PR[0-9]+$", names(peek), value = TRUE)

keep_cols <- c(
  "AGE", "FEMALE", "RACE", "PAY1", "ZIPINC_QRTL", "PL_NCHS",
  "LOS", "DIED", "TOTCHG", "NPR", "ORPROC", "ELECTIVE", "TRAN_IN",
  "HOSP_REGION", "HOSP_TEACH", "HOSP_BEDSIZE",
  "APRDRG_Risk_Mortality", "APRDRG_Severity",
  "HOSP_NIS", "NIS_STRATUM", "DISCWT", "YEAR",
  dx_cols, pr_cols
)
keep_cols <- intersect(keep_cols, names(peek))   # drop any that don't exist
missing_cols <- setdiff(c("AGE","FEMALE","RACE","PAY1","ZIPINC_QRTL","PL_NCHS",
                           "LOS","DIED","HOSP_NIS","NIS_STRATUM","DISCWT"),
                         names(peek))
if (length(missing_cols) > 0) {
  cat("\nWARNING -- these expected columns were NOT found, check names(peek) above:\n")
  print(missing_cols)
}

select_clause <- paste(sprintf('"%s"', keep_cols), collapse = ", ")

query <- sprintf("
  COPY (
    SELECT %s
    FROM read_csv_auto('%s', SAMPLE_SIZE=-1)
    WHERE YEAR = 2023
      AND AGE >= 18 AND AGE <= 50
      AND regexp_matches(I10_DX1, '%s')
  ) TO '%s' (HEADER, DELIMITER ',')
", select_clause, NIS_CORE_PATH, cancer_regex, OUTPUT_PATH)

dbExecute(con, query)

n_rows <- dbGetQuery(con, sprintf(
  "SELECT COUNT(*) AS n FROM read_csv_auto('%s')", OUTPUT_PATH
))
cat("\nWrote", n_rows$n, "rows to", OUTPUT_PATH, "\n")
cat("File size:\n")
system(sprintf("ls -lh %s", path.expand(OUTPUT_PATH)))

dbDisconnect(con, shutdown = TRUE)
