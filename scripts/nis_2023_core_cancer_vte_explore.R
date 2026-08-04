# ============================================================
# NIS 2023 Core: Cancer / VTE coding explore (CHUNKED VERSION)
#
# Pure base R -- no tidyverse/vroom/readr. Reads the fixed-width
# ASCII file in chunks of 200,000 lines via readLines()+substr(),
# so memory stays small and constant regardless of file size,
# and progress prints after every chunk.
#
# This is the version that actually completed against the real
# NIS_2023_Core.ASC (6,743,716 rows, 4.34 GB) in well under a
# minute. Earlier attempts using data.table::fread() (wrong --
# the file is fixed-width, not delimited), readr::read_fwf(), and
# tidyverse+vroom all either mis-parsed the file or were too slow/
# memory-heavy on this file size on ordinary laptop hardware. See
# scripts/nis_2023_core_load.R and nis_2023_core_standalone.R for
# the (correct but slower) tidyverse-based versions and the fixed-
# width column layout derived from HCUP's SAS load program.
#
# SCOPE NOTE: NIS is discharge/administrative claims data with no
# pharmacy/medication file, so anticoagulation (drug receipt) is
# not directly observable here. The only procedure-level proxy is
# IVC filter placement (ICD-10-PCS 06H0-/06H3-/06H4-), which signals
# a decision NOT to (or inability to) anticoagulate -- it is not
# evidence of anticoagulation itself.
# ============================================================

nis_file <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"

dx_starts <- seq(67, 340, by = 7); dx_ends <- dx_starts + 6   # I10_DX1-40
pr_starts <- seq(355, 523, by = 7); pr_ends <- pr_starts + 6  # I10_PR1-25
stopifnot(length(dx_starts) == 40, length(pr_starts) == 25)

# Cancer: any code starting with "C" is a malignant neoplasm in
# ICD-10-CM. Does NOT include D00-D09 (in situ) or D37-D48
# (uncertain/unknown behavior) -- add explicitly if needed.
#
# VTE: I26 (pulmonary embolism), I80 (phlebitis/thrombophlebitis),
# I82 (other venous embolism/thrombosis, incl. most DVT). I80
# includes superficial thrombophlebitis (I80.0-I80.3), which many
# VTE algorithms exclude -- narrow if that distinction matters.
cancer_regex     <- "^C"
vte_regex        <- "^(I26|I80|I82)"
ivc_filter_regex <- "^06H[034]"

con <- file(nis_file, "r")
chunk_size <- 200000

total_rows <- 0; n_cancer <- 0; n_vte <- 0; n_both <- 0; n_ivc <- 0

repeat {
  lines <- readLines(con, n = chunk_size)
  if (length(lines) == 0) break
  n <- length(lines)
  total_rows <- total_rows + n

  cancer_hit <- rep(FALSE, n)
  vte_hit    <- rep(FALSE, n)
  for (i in seq_along(dx_starts)) {
    codes <- trimws(substr(lines, dx_starts[i], dx_ends[i]))
    cancer_hit <- cancer_hit | grepl(cancer_regex, codes)
    vte_hit    <- vte_hit    | grepl(vte_regex, codes)
  }

  ivc_hit <- rep(FALSE, n)
  for (i in seq_along(pr_starts)) {
    codes <- trimws(substr(lines, pr_starts[i], pr_ends[i]))
    ivc_hit <- ivc_hit | grepl(ivc_filter_regex, codes)
  }

  n_cancer <- n_cancer + sum(cancer_hit)
  n_vte    <- n_vte    + sum(vte_hit)
  n_both   <- n_both   + sum(cancer_hit & vte_hit)
  n_ivc    <- n_ivc    + sum(ivc_hit)

  cat("Processed", total_rows, "rows so far...\n")
}
close(con)

cat("\n========== CANCER + VTE CODING SUMMARY ==========\n")
cat("Total discharge records:", total_rows, "\n")
cat("Records with any cancer diagnosis:", n_cancer, "\n")
cat("Records with any VTE diagnosis:", n_vte, "\n")
cat("Records with both cancer AND VTE:", n_both, "\n")
cat("Records with an IVC filter placement procedure:", n_ivc, "\n")
