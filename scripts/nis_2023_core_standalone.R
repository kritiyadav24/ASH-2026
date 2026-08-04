# ============================================================
# NIS 2023 Core: Load HCUP fixed-width ASCII file + initial
# cancer / VTE coding explore
#
# SELF-CONTAINED VERSION -- the column layout below
# (nis_2023_core_spec) was generated once from HCUP's official
# SAS load program for NIS 2023 Core and is pasted in here as
# data, so this is the only file you need.
#
# WHY NOT read.table()/fread(): NIS_2023_Core.ASC has no field
# delimiter. Every discharge record is one 643-character line
# with each variable packed into fixed byte positions. Reading
# it as delimited text does not error -- it just silently
# misaligns every column past the first few.
#
# SCOPE NOTE: NIS is discharge/administrative claims data with
# no pharmacy/medication file, so anticoagulation (drug receipt)
# is not directly observable here. See Section 6 for the only
# weak proxy available (IVC filter placement).
# ============================================================

library(tidyverse)
library(vroom)

# ----- 1. SET THIS to wherever your file actually is -----
nis_file <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"

# ----- 2. COLUMN LAYOUT (from HCUP's SASload_NIS_2023_Core.SAS) -----
nis_2023_core_spec <- tibble::tribble(
  ~var, ~start, ~end, ~type, ~informat,
  "HOSP_NIS", 1L, 5L, "numeric", "N5PF",
  "KEY_NIS", 6L, 15L, "numeric", "N10PF",
  "NIS_STRATUM", 16L, 19L, "numeric", "N4PF",
  "AGE", 20L, 22L, "numeric", "N3PF",
  "AGE_NEONATE", 23L, 24L, "numeric", "N2PF",
  "AMONTH", 25L, 26L, "numeric", "N2PF",
  "AWEEKEND", 27L, 28L, "numeric", "N2PF",
  "DIED", 29L, 30L, "numeric", "N2PF",
  "DISCWT", 31L, 41L, "numeric", "N11P7F",
  "DISPUNIFORM", 42L, 43L, "numeric", "N2PF",
  "DQTR", 44L, 45L, "numeric", "N2PF",
  "DRG", 46L, 48L, "numeric", "N3PF",
  "DRGVER", 49L, 50L, "numeric", "N2PF",
  "DRG_NoPOA", 51L, 53L, "numeric", "N3PF",
  "ELECTIVE", 54L, 55L, "numeric", "N2PF",
  "FEMALE", 56L, 57L, "numeric", "N2PF",
  "HCUP_ED", 58L, 60L, "numeric", "N3PF",
  "I10_BIRTH", 61L, 63L, "numeric", "N3PF",
  "I10_DELIVERY", 64L, 66L, "numeric", "N3PF",
  "I10_DX1", 67L, 73L, "character", "$CHAR7",
  "I10_DX2", 74L, 80L, "character", "$CHAR7",
  "I10_DX3", 81L, 87L, "character", "$CHAR7",
  "I10_DX4", 88L, 94L, "character", "$CHAR7",
  "I10_DX5", 95L, 101L, "character", "$CHAR7",
  "I10_DX6", 102L, 108L, "character", "$CHAR7",
  "I10_DX7", 109L, 115L, "character", "$CHAR7",
  "I10_DX8", 116L, 122L, "character", "$CHAR7",
  "I10_DX9", 123L, 129L, "character", "$CHAR7",
  "I10_DX10", 130L, 136L, "character", "$CHAR7",
  "I10_DX11", 137L, 143L, "character", "$CHAR7",
  "I10_DX12", 144L, 150L, "character", "$CHAR7",
  "I10_DX13", 151L, 157L, "character", "$CHAR7",
  "I10_DX14", 158L, 164L, "character", "$CHAR7",
  "I10_DX15", 165L, 171L, "character", "$CHAR7",
  "I10_DX16", 172L, 178L, "character", "$CHAR7",
  "I10_DX17", 179L, 185L, "character", "$CHAR7",
  "I10_DX18", 186L, 192L, "character", "$CHAR7",
  "I10_DX19", 193L, 199L, "character", "$CHAR7",
  "I10_DX20", 200L, 206L, "character", "$CHAR7",
  "I10_DX21", 207L, 213L, "character", "$CHAR7",
  "I10_DX22", 214L, 220L, "character", "$CHAR7",
  "I10_DX23", 221L, 227L, "character", "$CHAR7",
  "I10_DX24", 228L, 234L, "character", "$CHAR7",
  "I10_DX25", 235L, 241L, "character", "$CHAR7",
  "I10_DX26", 242L, 248L, "character", "$CHAR7",
  "I10_DX27", 249L, 255L, "character", "$CHAR7",
  "I10_DX28", 256L, 262L, "character", "$CHAR7",
  "I10_DX29", 263L, 269L, "character", "$CHAR7",
  "I10_DX30", 270L, 276L, "character", "$CHAR7",
  "I10_DX31", 277L, 283L, "character", "$CHAR7",
  "I10_DX32", 284L, 290L, "character", "$CHAR7",
  "I10_DX33", 291L, 297L, "character", "$CHAR7",
  "I10_DX34", 298L, 304L, "character", "$CHAR7",
  "I10_DX35", 305L, 311L, "character", "$CHAR7",
  "I10_DX36", 312L, 318L, "character", "$CHAR7",
  "I10_DX37", 319L, 325L, "character", "$CHAR7",
  "I10_DX38", 326L, 332L, "character", "$CHAR7",
  "I10_DX39", 333L, 339L, "character", "$CHAR7",
  "I10_DX40", 340L, 346L, "character", "$CHAR7",
  "I10_INJURY", 347L, 348L, "numeric", "N2PF",
  "I10_MULTINJURY", 349L, 350L, "numeric", "N2PF",
  "I10_NDX", 351L, 352L, "numeric", "N2PF",
  "I10_NPR", 353L, 354L, "numeric", "N2PF",
  "I10_PR1", 355L, 361L, "character", "$CHAR7",
  "I10_PR2", 362L, 368L, "character", "$CHAR7",
  "I10_PR3", 369L, 375L, "character", "$CHAR7",
  "I10_PR4", 376L, 382L, "character", "$CHAR7",
  "I10_PR5", 383L, 389L, "character", "$CHAR7",
  "I10_PR6", 390L, 396L, "character", "$CHAR7",
  "I10_PR7", 397L, 403L, "character", "$CHAR7",
  "I10_PR8", 404L, 410L, "character", "$CHAR7",
  "I10_PR9", 411L, 417L, "character", "$CHAR7",
  "I10_PR10", 418L, 424L, "character", "$CHAR7",
  "I10_PR11", 425L, 431L, "character", "$CHAR7",
  "I10_PR12", 432L, 438L, "character", "$CHAR7",
  "I10_PR13", 439L, 445L, "character", "$CHAR7",
  "I10_PR14", 446L, 452L, "character", "$CHAR7",
  "I10_PR15", 453L, 459L, "character", "$CHAR7",
  "I10_PR16", 460L, 466L, "character", "$CHAR7",
  "I10_PR17", 467L, 473L, "character", "$CHAR7",
  "I10_PR18", 474L, 480L, "character", "$CHAR7",
  "I10_PR19", 481L, 487L, "character", "$CHAR7",
  "I10_PR20", 488L, 494L, "character", "$CHAR7",
  "I10_PR21", 495L, 501L, "character", "$CHAR7",
  "I10_PR22", 502L, 508L, "character", "$CHAR7",
  "I10_PR23", 509L, 515L, "character", "$CHAR7",
  "I10_PR24", 516L, 522L, "character", "$CHAR7",
  "I10_PR25", 523L, 529L, "character", "$CHAR7",
  "I10_SERVICELINE", 530L, 532L, "numeric", "N3PF",
  "LOS", 533L, 537L, "numeric", "N5PF",
  "MDC", 538L, 539L, "numeric", "N2PF",
  "MDC_NoPOA", 540L, 541L, "numeric", "N2PF",
  "PAY1", 542L, 543L, "numeric", "N2PF",
  "PCLASS_ORPROC", 544L, 545L, "numeric", "N2PF",
  "PRDAY1", 546L, 548L, "numeric", "N3PF",
  "PRDAY2", 549L, 551L, "numeric", "N3PF",
  "PRDAY3", 552L, 554L, "numeric", "N3PF",
  "PRDAY4", 555L, 557L, "numeric", "N3PF",
  "PRDAY5", 558L, 560L, "numeric", "N3PF",
  "PRDAY6", 561L, 563L, "numeric", "N3PF",
  "PRDAY7", 564L, 566L, "numeric", "N3PF",
  "PRDAY8", 567L, 569L, "numeric", "N3PF",
  "PRDAY9", 570L, 572L, "numeric", "N3PF",
  "PRDAY10", 573L, 575L, "numeric", "N3PF",
  "PRDAY11", 576L, 578L, "numeric", "N3PF",
  "PRDAY12", 579L, 581L, "numeric", "N3PF",
  "PRDAY13", 582L, 584L, "numeric", "N3PF",
  "PRDAY14", 585L, 587L, "numeric", "N3PF",
  "PRDAY15", 588L, 590L, "numeric", "N3PF",
  "PRDAY16", 591L, 593L, "numeric", "N3PF",
  "PRDAY17", 594L, 596L, "numeric", "N3PF",
  "PRDAY18", 597L, 599L, "numeric", "N3PF",
  "PRDAY19", 600L, 602L, "numeric", "N3PF",
  "PRDAY20", 603L, 605L, "numeric", "N3PF",
  "PRDAY21", 606L, 608L, "numeric", "N3PF",
  "PRDAY22", 609L, 611L, "numeric", "N3PF",
  "PRDAY23", 612L, 614L, "numeric", "N3PF",
  "PRDAY24", 615L, 617L, "numeric", "N3PF",
  "PRDAY25", 618L, 620L, "numeric", "N3PF",
  "TRAN_IN", 621L, 622L, "numeric", "N2PF",
  "TRAN_OUT", 623L, 624L, "numeric", "N2PF",
  "YEAR", 625L, 628L, "numeric", "N4PF",
  "ZIPINC_QRTL", 629L, 630L, "numeric", "N2PF",
  "TOTCHG_2023", 631L, 640L, "numeric", "N10PF",
  "PL_NCHS2", 641L, 643L, "numeric", "N3PF"
)

lrecl <- 643L

stopifnot(
  "Embedded spec widths do not sum to 643 -- do not proceed" =
    sum(nis_2023_core_spec$end - nis_2023_core_spec$start + 1L) == lrecl,
  "Gap or overlap between consecutive columns" =
    all(nis_2023_core_spec$end[-nrow(nis_2023_core_spec)] + 1L ==
        nis_2023_core_spec$start[-1]),
  "Duplicate variable names in spec" =
    !any(duplicated(nis_2023_core_spec$var))
)
cat("Column layout OK:", nrow(nis_2023_core_spec), "columns,", lrecl, "bytes.\n")


# ----- 3. LOAD THE FIXED-WIDTH FILE -----
col_types_str <- paste0(
  if_else(nis_2023_core_spec$type == "character", "c", "d"),
  collapse = ""
)

nis <- vroom::vroom_fwf(
  nis_file,
  col_positions = vroom::fwf_positions(
    start     = nis_2023_core_spec$start,
    end       = nis_2023_core_spec$end,
    col_names = nis_2023_core_spec$var
  ),
  col_types = col_types_str,
  na = ""
)

dx_cols <- nis_2023_core_spec$var[str_detect(nis_2023_core_spec$var, "^I10_DX[0-9]+$")]
pr_cols <- nis_2023_core_spec$var[str_detect(nis_2023_core_spec$var, "^I10_PR[0-9]+$")]

# Unused DX/PR slots are blank-padded in the file -- clean to NA
nis <- nis %>%
  mutate(across(all_of(c(dx_cols, pr_cols)), ~ na_if(str_trim(.x), "")))


# ----- 4. RECODE HCUP MISSING-VALUE SENTINELS TO NA -----
# HCUP encodes missing/invalid/inapplicable/not-available as
# all-9s / all-8s / all-6s / all-5s strings sized to each field's
# width (e.g. a 3-digit field uses "-99"/"-88"/"-66"). These are
# NOT real negative values -- collapsing them all to NA here.
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

apply_hcup_missing <- function(df, spec, codes_lookup) {
  numeric_specs <- spec %>% filter(type == "numeric")
  for (i in seq_len(nrow(numeric_specs))) {
    var <- numeric_specs$var[i]
    informat <- numeric_specs$informat[i]
    codes <- codes_lookup[[informat]]
    if (is.null(codes)) {
      stop(sprintf(
        "No missing-value codes defined for informat '%s' (variable %s)",
        informat, var
      ))
    }
    sentinel_values <- as.numeric(codes)
    df[[var]][df[[var]] %in% sentinel_values] <- NA
  }
  df
}

nis <- apply_hcup_missing(nis, nis_2023_core_spec, hcup_missing_codes)


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
# ICD-10-CM. Does NOT include D00-D09 (in situ) or D37-D48
# (uncertain/unknown behavior) -- add explicitly if needed.
#
# VTE: I26 (pulmonary embolism), I80 (phlebitis/thrombophlebitis),
# I82 (other venous embolism/thrombosis, incl. most DVT). I80
# includes superficial thrombophlebitis (I80.0-I80.3), which many
# VTE algorithms exclude -- narrow if that distinction matters.
#
# Anticoagulation: NOT available in NIS (no pharmacy file). The
# only procedure-level proxy is IVC filter placement (ICD-10-PCS
# 06H0-/06H3-/06H4-), which signals a decision NOT to anticoagulate
# -- it is not evidence of anticoagulation itself.
cancer_regex     <- "^C"
vte_regex        <- "^(I26|I80|I82)"
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
    any_cancer_dx  = any_code_match(., dx_cols, cancer_regex),
    any_vte_dx     = any_code_match(., dx_cols, vte_regex),
    any_ivc_filter = any_code_match(., pr_cols, ivc_filter_regex)
  )

cat("\n========== CANCER + VTE CODING SUMMARY ==========\n")
cat("Total discharge records:", nrow(nis), "\n")
cat("Records with any cancer diagnosis:", sum(nis$any_cancer_dx), "\n")
cat("Records with any VTE diagnosis:", sum(nis$any_vte_dx), "\n")
cat("Records with both cancer AND VTE:", sum(nis$any_cancer_dx & nis$any_vte_dx), "\n")
cat("Records with an IVC filter placement procedure:", sum(nis$any_ivc_filter), "\n")
