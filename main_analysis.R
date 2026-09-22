# =============================================================================
# REPLICATION SCRIPT
# "The Decline of War Question: A Comparative Regional Perspective"
# Spagat & Thompson
#
# DATA FILE REQUIRED (same directory as this script):
#   ProjectMars_plus_GiblerMiller_final.xlsx      [sheet: "data"]
#
# 1,108 campaign-belligerent participations as supplied:
#   825 from Project Mars v1.1 (Lyall)
#   283 appended from Correlates of War, following Gibler & Miller's Table 11
#
# THREE ANALYTICAL PANELS, in increasing order of contestedness:
#   1. Project Mars only
#   2. Mars + Gibler-Miller additions excluding intra-state wars
#   3. Mars + Gibler-Miller additions
# The appended cases are attributed to Gibler & Miller rather than to the
# Correlates of War project. CoW takes no position on which of its wars were
# fought conventionally; Table 11 is Gibler & Miller's judgement applied to
# CoW's records, and naming it otherwise would lend it an authority neither
# dataset claims.
# Panel 2 holds the additions whose omission from Project Mars is not disputed
# on conventionality grounds. Panel 3 adds the CoW intra-state wars, whose
# status under Project Mars's conventionality criterion is exactly what is at
# issue between Lyall and Gibler & Miller. Presenting the panels in this order
# lets the reader see which results depend on the contested cases without the
# paper having to adjudicate the dispute.
#
# MILITARIZED INTERSTATE DISPUTES ARE EXCLUDED THROUGHOUT.
# Gibler & Miller's Table 11 includes 11 MIDs (29 belligerent rows). Including
# them would apply a 500-death floor to interstate conflict while everything
# else enters at CoW's 1,000-death war threshold, since no sub-1,000
# enumeration exists for intra-, extra- or non-state war. They are also a
# targeted subset rather than a census, and the 500-999 fatality band in MID
# data is the range Gibler, Miller and Little's own work documents as
# error-prone. They are filtered out here rather than deleted from the data
# file, so the archived dataset still corresponds to Table 11. Their inclusion
# makes no material difference to any result.
#
# The file carries two regional variables:
#   region        18 sub-regions, Project Mars rows only
#   region_group  the 8 analytical regions, populated for every row
# The analysis uses region_group throughout. Observations outside the eight
# regions (Central Asia, South Pacific) have region_group = NA and drop out.
#
# KIA thresholds use kialow only. The appended CoW rows carry a single
# fatality figure per belligerent, so kialow == kiahigh for them, whereas the
# Project Mars rows carry genuine low/high ranges. Screening on kiahigh would
# therefore test Mars cases on their high estimate and appended cases on a
# point estimate, filtering out appended cases for a reason that has nothing
# to do with their size. The kiahigh panels have been removed.
#
# SECTIONS
#   0.  Load, validate
#   A.  Regional analysis - Poisson and quasi-Poisson, by panel
#   B.  Panel comparison - primary inference
#   C.  Europe decomposition, by panel
#   D.  Merged Europe, by panel
#   E.  Belligerents spanning regions
#   F.  Overdispersion tests, by panel
#   G.  Break-point sensitivity grid, by panel
#   H.  Break-point sensitivity figures, by panel
#   I.  Publication tables
# =============================================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(gt)
library(AER)

DATA_FILE <- "ProjectMars_plus_GiblerMiller_final.xlsx"

# =============================================================================
# 0. LOAD AND VALIDATE
# =============================================================================

cat("\n=== LOADING DATA ===\n")
df_raw <- read_excel(DATA_FILE, sheet = "data")
cat("Rows in file:", nrow(df_raw), "\n")

# --- drop Militarized Interstate Disputes (see header note) ------------------
is_mid <- !is.na(df_raw$cow_dataset) & df_raw$cow_dataset == "MIDB 5.0"
cat("MID rows excluded:", sum(is_mid),
    "| distinct MID cases:", length(unique(df_raw$cow_war_no[is_mid])), "\n")
df <- df_raw[!is_mid, ]
cat("Rows retained:", nrow(df), "\n")

REGIONS <- c("Europe", "Middle East + N Africa", "E Asia + SE Asia", "E. Europe",
             "S. Asia", "Sub-Saharan Africa", "N America", "Latin America & Caribbean")

PANELS <- c("Project Mars",
            "Project Mars + non-civil-war additions",
            "Project Mars + all additions")
PANEL_KEYS <- c("mars", "gm_nointra", "gm_all")
names(PANEL_KEYS) <- PANELS

pre_start  <- min(df$yrstart, na.rm = TRUE); pre_end  <- 1950
post_start <- 1951;                          post_end <- max(df$yrstart, na.rm = TRUE)
pre_years  <- pre_end  - pre_start  + 1
post_years <- post_end - post_start + 1
all_years  <- data.frame(yrstart = pre_start:post_end)

cat("Pre-1950: ", pre_start, "-", pre_end,  "(", pre_years,  "years)\n")
cat("Post-1950:", post_start, "-", post_end, "(", post_years, "years)\n")

# --- validation: fail loudly rather than silently analysing the wrong thing ---
stopifnot(all(df$region_group[!is.na(df$region_group)] %in% REGIONS))
stopifnot(all(df$source %in% c("Project Mars", "Gibler-Miller")))
cat("\nSource split:      Project Mars", sum(df$source == "Project Mars"),
    "| Gibler-Miller",     sum(df$source == "Gibler-Miller"), "\n")
cat("In the 8 regions: ", sum(!is.na(df$region_group)),
    "| outside:",          sum(is.na(df$region_group)), "\n")

span <- df %>% filter(!is.na(region_group), !is.na(statename)) %>%
  group_by(statename) %>% summarise(n_reg = n_distinct(region_group), .groups = "drop") %>%
  filter(n_reg > 1)
cat("Belligerents in more than one analytical region:", nrow(span), "\n")
if (nrow(span) > 0) { cat("  !! expected 0 -- investigate before trusting results\n"); print(span) }

# --- panel construction ------------------------------------------------------
# Note the explicit !is.na() guard: cow_dataset is NA on every Project Mars row,
# and a bare inequality test would return NA and silently drop those rows.
panel_data <- function(panel) {
  if (panel == PANELS[1]) return(df[df$source == "Project Mars", ])
  if (panel == PANELS[2]) {
    keep <- df$source == "Project Mars" |
            (!is.na(df$cow_dataset) & df$cow_dataset != "Intra-State v4.1")
    return(df[keep, ])
  }
  df
}

cat("\nPanel sizes (all rows / rows in the eight regions):\n")
for (p in PANELS) {
  d <- panel_data(p)
  cat(sprintf("  %-50s %5d / %5d\n", p, nrow(d), sum(!is.na(d$region_group))))
}

# =============================================================================
# KIA THRESHOLDS  (quartiles of non-zero Project Mars kialow values)
# =============================================================================

mars_low <- df$kialow[df$source == "Project Mars" & !is.na(df$kialow) & df$kialow > 0]
thr_low  <- setNames(quantile(mars_low, c(.25, .50, .75)), c("Q1", "Q2", "Q3"))
cat("\nkialow Q1/Q2/Q3:", round(thr_low), "\n")

# Appended rows with no CoW fatality figure carry kialow = NA and therefore
# drop out of every KIA-threshold table. That is intended: the threshold cannot
# be evaluated for them. They are retained in the unfiltered table, where the
# absence of a published figure is not a reason to exclude a war that CoW has
# already coded above its own 1,000-death threshold.
chk <- df %>% filter(source == "Gibler-Miller") %>%
  summarise(n = n(), low = sum(!is.na(kialow)))
cat("Appended rows:", chk$n, "| kialow present:", chk$low,
    "| no fatality figure:", chk$n - chk$low, "\n")

# =============================================================================
# STATISTICAL FUNCTIONS
# =============================================================================

poisson_decline_test <- function(n_pre, n_post, t_pre, t_post) {
  rate_pre <- n_pre / t_pre; rate_post <- n_post / t_post
  actual <- if (rate_pre > 0) 100 * (1 - rate_post / rate_pre) else NA
  k_dec <- NA; p_dec <- NA; k_inc <- NA; p_inc <- NA
  if (rate_pre > 0) {
    for (k in c(1, .9, .8, .7, .6, .5, .4, .3, .2, .1, 0)) {
      if (k == 0 && n_post > 0) next
      lp <- k * rate_pre * t_post
      pv <- if (lp == 0) ifelse(n_post == 0, 1, 0) else ppois(n_post, lp)
      if (pv < 0.05) { k_dec <- k; p_dec <- pv }
    }
    if (rate_post > rate_pre) {
      for (k in c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.5, 3)) {
        lp <- k * rate_pre * t_post
        if (lp == 0) next
        pv <- 1 - ppois(n_post, lp)
        if (pv < 0.05) { k_inc <- k; p_inc <- pv }
      }
    }
  }
  list(n_pre = n_pre, n_post = n_post, rate_pre = rate_pre, rate_post = rate_post,
       actual_decline = actual, smallest_k_rejected = k_dec, p_value_decline = p_dec,
       largest_k_rejected_increase = k_inc, p_value_increase = p_inc)
}

poisson_power <- function(n_pre, t_pre, t_post, ref = c(30, 50), alpha = 0.05) {
  rate_pre <- n_pre / t_pre
  nm <- paste0("power_", ref, "pct")
  if (is.na(rate_pre) || rate_pre == 0) return(setNames(rep(NA, length(ref)), nm))
  c_crit <- qpois(alpha, rate_pre * t_post) - 1L
  if (c_crit < 0) return(setNames(rep(0, length(ref)), nm))
  setNames(sapply(ref, function(p) ppois(c_crit, (1 - p / 100) * rate_pre * t_post)), nm)
}

annual_counts <- function(data) {
  data %>% group_by(yrstart) %>% summarise(count = n(), .groups = "drop") %>%
    right_join(all_years, by = "yrstart") %>%
    mutate(count = ifelse(is.na(count), 0L, as.integer(count))) %>% arrange(yrstart)
}

# Dispersion from a constant-mean model of the annual series. Note that a
# genuine shift in the mean at the break point therefore contributes to the
# estimate, making the resulting intervals conservative. get_dispersion_period()
# below nets the shift out; Section F reports both so the difference is visible.
get_dispersion <- function(data) {
  if (nrow(data) == 0) return(NA_real_)
  a <- annual_counts(data)
  if (sum(a$count) == 0) return(NA_real_)
  fit <- glm(count ~ 1, data = a, family = poisson)
  sum(residuals(fit, type = "pearson")^2) / fit$df.residual
}

get_dispersion_period <- function(data) {
  if (nrow(data) == 0) return(NA_real_)
  a <- annual_counts(data) %>% mutate(period = ifelse(yrstart <= pre_end, "pre", "post"))
  if (length(unique(a$period)) < 2 || sum(a$count) == 0) return(NA_real_)
  fit <- glm(count ~ period, data = a, family = poisson)
  sum(residuals(fit, type = "pearson")^2) / fit$df.residual
}

# The test works on the log rate ratio, which is undefined when either period
# count is zero. Such cells are flagged not estimable rather than falling
# through to a null result: printing "No sig. change" against a region that
# recorded no post-1950 war at all asserts something the test never evaluated.
# The Haldane-Anscombe correction (adding 0.5 to each count) makes the ratio
# computable but not informative here -- the 1/0.5 term dominates the standard
# error, so the interval never rejects -- which is why the cell is marked
# rather than patched.
quasi_poisson_test <- function(n_pre, n_post, t_pre, t_post, dispersion) {
  if (n_pre == 0 || n_post == 0 || is.na(dispersion))
    return(list(rate_pre = n_pre / t_pre, rate_post = n_post / t_post,
                actual_decline = NA, dispersion = dispersion, rr_lower = NA, rr_upper = NA,
                smallest_k_rejected = NA, largest_k_rejected_increase = NA,
                estimable = FALSE))
  rate_pre <- n_pre / t_pre; rate_post <- n_post / t_post
  log_rr <- log(rate_post / rate_pre)
  se     <- sqrt(dispersion) * sqrt(1 / n_pre + 1 / n_post)
  hi <- exp(log_rr + 1.645 * se); lo <- exp(log_rr - 1.645 * se)
  list(rate_pre = rate_pre, rate_post = rate_post,
       actual_decline = 100 * (1 - rate_post / rate_pre), dispersion = dispersion,
       rr_lower = lo, rr_upper = hi,
       smallest_k_rejected         = if (!is.na(hi) && hi < 1) hi else NA,
       largest_k_rejected_increase = if (!is.na(lo) && lo > 1) lo else NA,
       estimable = TRUE)
}

# Qualitative verdict. Used for like-for-like comparison between the Poisson and
# quasi-Poisson results; comparing the formatted label strings does not work,
# because the two formatters word the null case differently.
# isFALSE(NULL) is FALSE, so calls that pass a bare Poisson result list -- which
# carries no estimable field -- are unaffected.
sig_category <- function(res) {
  if (isFALSE(res$estimable))                        "n/a"
  else if (!is.na(res$smallest_k_rejected))          "decline"
  else if (!is.na(res$largest_k_rejected_increase))  "increase"
  else                                               "none"
}

# A bound that rounds to zero means the test rejects no-change without
# establishing any substantive lower bound. Labelling it ">=0%" reads as an
# error, so it is named explicitly.
label_bound <- function(res, style = c("short", "long")) {
  style <- match.arg(style)
  if (isFALSE(res$estimable))
    return(if (style == "short") "not est.*" else "Not estimable*")
  if (!is.na(res$smallest_k_rejected)) {
    pct <- round((1 - res$smallest_k_rejected) * 100)
    if (pct == 0) return(if (style == "short") "sig dec (<10%)" else "Sig. decline (bound <10%)")
    return(if (style == "short") sprintf(">=%d%% dec", pct) else sprintf("\u2265%d%% decline", pct))
  }
  if (!is.na(res$largest_k_rejected_increase)) {
    pct <- round((res$largest_k_rejected_increase - 1) * 100)
    if (pct == 0) return(if (style == "short") "sig inc (<10%)" else "Sig. increase (bound <10%)")
    return(if (style == "short") sprintf(">=%d%% inc", pct) else sprintf("\u2265%d%% increase", pct))
  }
  if (style == "short") "No sig chg" else "No sig. change"
}

apply_kia <- function(data, kia_thresh) {
  if (is.null(kia_thresh)) return(data)
  data[!is.na(data$kialow) & data$kialow >= kia_thresh, ]
}

# Shared table styling, so the three table families cannot drift apart.
# rows_per_group is passed in rather than assumed, since the decomposition table
# has three rows per panel and the regional tables have eight.
# Widths are proportional, not absolute. gt converts px at 96 dpi, so a 900px
# table becomes 9.4 inches and runs off the page in Word; pct() lets Word fit the
# table to the margins while preserving the column proportions.
style_table <- function(tbl, title, subtitle, note, rows_per_group, n_groups,
                        has_groups = TRUE) {
  stripe <- unlist(lapply(seq_len(n_groups), function(j)
    seq(2, rows_per_group, 2) + (j - 1) * rows_per_group))
  tbl <- tbl %>%
    tab_header(title = md(title), subtitle = subtitle) %>%
    tab_source_note(source_note = md(note)) %>%
    tab_style(list(cell_fill("#2C3E50"), cell_text(color = "white", weight = "bold", size = px(13))),
              cells_title(groups = "title")) %>%
    tab_style(list(cell_fill("#2C3E50"), cell_text(color = "#BDC3C7", size = px(11))),
              cells_title(groups = "subtitle")) %>%
    tab_style(list(cell_fill("#34495E"), cell_text(color = "white", weight = "bold", size = px(11))),
              cells_column_labels()) %>%
    tab_style(cell_fill("#FAFAFA"), cells_body(rows = stripe)) %>%
    tab_style(cell_text(size = px(11)), cells_body()) %>%
    tab_style(cell_text(size = px(9), color = "#7F8C8D"), cells_source_notes()) %>%
    tab_options(table.width = pct(100), data_row.padding = px(5),
      table.border.top.style = "solid", table.border.top.width = px(2), table.border.top.color = "#2C3E50",
      table.border.bottom.style = "solid", table.border.bottom.width = px(2), table.border.bottom.color = "#2C3E50",
      row_group.border.top.style = "solid", row_group.border.top.width = px(1), row_group.border.top.color = "#BDC3C7",
      column_labels.border.bottom.style = "solid", column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "#BDC3C7")
  # Row-group styling is only valid where the table actually has row groups;
  # Tables 4 and 5 are ungrouped and gt errors on cells_row_groups() there.
  if (has_groups)
    tbl <- tbl %>% tab_style(
      list(cell_fill("#ECF0F1"), cell_text(weight = "bold", size = px(11), color = "#2C3E50")),
      cells_row_groups())
  tbl
}

# Numbered legends, as the journal requires "Table 1" to appear in the legend
# itself rather than only in the text.
TBL <- function(n, title) paste0("**Table ", n, ".** ", title)

# Every table is registered here as it is built, keyed by its number, so the
# combined document at the end cannot drift out of step with the individual
# files. The build order across sections is 1, 4, 2, 5, 3, so the registry is
# sorted before assembly rather than appended to in sequence.
TABLES <- list(); SUPP_TABLES <- list()

# gt's RTF output needs three corrections before it is fit for a manuscript.
# It sets the section to landscape; it writes the body in Courier New; and it
# fixes the type at 10pt. All three are cosmetic defaults rather than anything
# we asked for, so they are patched in the RTF text itself.
rtf_fix <- function(txt) {
  txt <- gsub("\\lndscpsxn", "", txt, fixed = TRUE)
  txt <- sub("Courier New;", "Times New Roman;", txt, fixed = TRUE)
  gsub("\\fs20", "\\fs22", txt, fixed = TRUE)
}

write_rtf <- function(tbl, path) {
  writeLines(rtf_fix(as.character(gt::as_rtf(tbl))), path, useBytes = TRUE)
  cat("Saved:", path, "\n")
}

# gt writes each table as a complete RTF document, which cannot simply be
# concatenated. The split point is the first \trowd -- the start of the first
# table row -- and NOT the first \pard, which falls inside that row. Splitting
# at \pard leaves the row opener in the shared preamble, so every table joins
# onto the first one as a continuation, and because that opening row carries
# \trhdr (repeat as a header on each page) the first table's title reappears at
# the top of every page. Bodies are rejoined with a closed-out paragraph and an
# explicit page break, which a page break inside a cell would not give.
combine_rtf <- function(tbls, path) {
  if (length(tbls) == 0) return(invisible(NULL))
  parts  <- lapply(tbls, function(t) rtf_fix(as.character(gt::as_rtf(t))))
  starts <- vapply(parts, function(s) regexpr("\\trowd", s, fixed = TRUE), integer(1))
  if (any(starts < 0)) {
    warning("combine_rtf: no table-row marker found; combined file not written.")
    return(invisible(NULL))
  }
  preamble <- substr(parts[[1]], 1, starts[[1]] - 1)
  bodies <- mapply(function(s, i) sub("\\}[[:space:]]*$", "", substr(s, i, nchar(s))),
                   parts, starts, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  writeLines(paste0(preamble, paste(bodies, collapse = "\n\\pard\\plain\\par\\page\n"), "}"),
             path, useBytes = TRUE)
  cat("Saved:", path, "(", length(tbls), "tables )\n")
}

PANEL_NOTE <- paste0(
  "Additions are the cases Gibler & Miller (2022, 2023) argue meet Project Mars's ",
  "own inclusion criteria. The second panel adds those that are not civil wars; ",
  "the third adds the COW intra-state wars, which Lyall excludes as insufficiently ",
  "conventional. Militarized interstate disputes are excluded throughout. ")

# =============================================================================
# SECTION A: REGIONAL ANALYSIS - POISSON AND QUASI-POISSON
# =============================================================================
# Both tests are computed for every panel. The quasi-Poisson is the primary
# inference reported in the text; the Poisson columns are retained because the
# power calculation is defined under the Poisson model and is therefore an
# upper bound on power under the quasi-Poisson.

cat("\n\n=== SECTION A: REGIONAL ANALYSIS ===\n")

analyze_panel <- function(panel, kia_thresh = NULL, ref = c(30, 50)) {
  data <- apply_kia(panel_data(panel), kia_thresh)
  out <- lapply(REGIONS, function(g) {
    gd <- data[!is.na(data$region_group) & data$region_group == g, ]
    n_pre  <- sum(gd$yrstart <= pre_end, na.rm = TRUE)
    n_post <- sum(gd$yrstart >  pre_end, na.rm = TRUE)
    disp   <- get_dispersion(gd)
    res    <- poisson_decline_test(n_pre, n_post, pre_years, post_years)
    resq   <- quasi_poisson_test(n_pre, n_post, pre_years, post_years, disp)
    pwr    <- poisson_power(n_pre, pre_years, post_years, ref)
    data.frame(panel = panel, region = g, n_total = nrow(gd),
               n_pre = n_pre, n_post = n_post,
               rate_pre = res$rate_pre, rate_post = res$rate_post,
               actual_decline = res$actual_decline,
               dispersion = disp,
               smallest_k_rejected = res$smallest_k_rejected,
               p_value_decline = res$p_value_decline,
               largest_k_rejected_increase = res$largest_k_rejected_increase,
               p_value_increase = res$p_value_increase,
               qp_smallest_k_rejected = resq$smallest_k_rejected,
               qp_largest_k_rejected_increase = resq$largest_k_rejected_increase,
               qp_rr_lower = resq$rr_lower, qp_rr_upper = resq$rr_upper,
               qp_estimable = resq$estimable,
               min_proven    = label_bound(res),
               min_proven_qp = label_bound(resq),
               verdict_pois  = sig_category(res),
               verdict_qp    = sig_category(resq),
               agree = if (!resq$estimable) "n/a"
                       else if (sig_category(res) == sig_category(resq)) "yes" else "DIFFERS",
               power_30pct = unname(pwr["power_30pct"]),
               power_50pct = unname(pwr["power_50pct"]),
               stringsAsFactors = FALSE)
  })
  res_df <- do.call(rbind, out)
  tl <- if (!is.null(kia_thresh)) paste0(" | kialow >= ", round(kia_thresh)) else " | no KIA filter"
  cat("\n---", panel, tl, "---\n")
  cat(sprintf("%-28s %5s %5s %5s %9s %7s %16s %16s %6s\n",
              "Region", "N", "Npre", "Npost", "Change%", "Disp.",
              "Poisson", "Quasi-Poisson", "Same?"))
  cat(strrep("-", 118), "\n")
  for (i in seq_len(nrow(res_df))) {
    r <- res_df[i, ]
    cat(sprintf("%-28s %5d %5d %5d %+8.1f%% %7.3f %16s %16s %6s\n",
                r$region, r$n_total, r$n_pre, r$n_post,
                ifelse(is.na(r$actual_decline), 0, r$actual_decline),
                ifelse(is.na(r$dispersion), NA_real_, r$dispersion),
                r$min_proven, r$min_proven_qp, r$agree))
  }
  res_df
}

res_base <- list(); res_kialow <- list()
for (p in PANELS) {
  key <- PANEL_KEYS[[p]]
  res_base[[key]] <- analyze_panel(p)
  for (tn in names(thr_low))
    res_kialow[[paste0(tn, "_", key)]] <- analyze_panel(p, thr_low[[tn]])
}
for (nm in names(res_base))   write.csv(res_base[[nm]],   paste0("results_base_", nm, ".csv"), row.names = FALSE)
for (nm in names(res_kialow)) write.csv(res_kialow[[nm]], paste0("results_kialow_", nm, ".csv"), row.names = FALSE)
cat("\nSection A CSVs saved.\n")

# =============================================================================
# SECTION B: PANEL COMPARISON (PRIMARY INFERENCE)
# =============================================================================
# The headline question: which regional verdicts depend on which cases are
# added. Global rows use the pooled series for each panel.

cat("\n\n=== SECTION B: PANEL COMPARISON, QUASI-POISSON ===\n\n")

row_for <- function(panel, label, d) {
  n_pre  <- sum(d$yrstart <= pre_end, na.rm = TRUE)
  n_post <- sum(d$yrstart >  pre_end, na.rm = TRUE)
  disp   <- get_dispersion(d)
  rp <- poisson_decline_test(n_pre, n_post, pre_years, post_years)
  rq <- quasi_poisson_test(n_pre, n_post, pre_years, post_years, disp)
  data.frame(panel = panel, region = label, n_pre = n_pre, n_post = n_post,
             actual_change = round(rq$actual_decline, 1), dispersion = round(disp, 3),
             poisson = label_bound(rp), quasi_poisson = label_bound(rq),
             verdict_pois = sig_category(rp), verdict_qp = sig_category(rq),
             qp_estimable = rq$estimable,
             same = if (!rq$estimable) "n/a"
                    else if (sig_category(rp) == sig_category(rq)) "yes" else "DIFFERS",
             stringsAsFactors = FALSE)
}

qp_results <- data.frame()
for (p in PANELS) {
  d <- panel_data(p); d <- d[!is.na(d$region_group), ]
  qp_results <- rbind(qp_results, row_for(p, "GLOBAL", d))
  for (g in REGIONS)
    qp_results <- rbind(qp_results, row_for(p, g, d[d$region_group == g, ]))
}

for (p in PANELS) {
  cat("---", p, "---\n")
  cat(sprintf("%-28s %5s %5s %9s %7s  %-18s %-18s %s\n",
              "Region", "Npre", "Npost", "Change%", "Disp.", "Poisson", "Quasi-Poisson", "Same?"))
  cat(strrep("-", 108), "\n")
  sub <- qp_results[qp_results$panel == p, ]
  for (i in seq_len(nrow(sub))) {
    r <- sub[i, ]
    cat(sprintf("%-28s %5d %5d %+8.1f%% %7.3f  %-18s %-18s %s\n",
                r$region, r$n_pre, r$n_post, r$actual_change, r$dispersion,
                r$poisson, r$quasi_poisson, r$same))
  }
  cat("\n")
}
write.csv(qp_results, "quasi_poisson_results.csv", row.names = FALSE)

# --- which verdicts move between panels (quasi-Poisson) ----------------------
cat("--- Verdict changes relative to Project Mars only (quasi-Poisson) ---\n")
base_v <- qp_results[qp_results$panel == PANELS[1], c("region", "verdict_qp")]
names(base_v)[2] <- "base_qp"
for (p in PANELS[-1]) {
  sub <- merge(qp_results[qp_results$panel == p, c("region", "verdict_qp")], base_v, by = "region")
  # A cell the test cannot evaluate is not a verdict change; excluded so an
  # undefined log rate ratio cannot masquerade as a substantive shift.
  na_rows <- sub$verdict_qp == "n/a" | sub$base_qp == "n/a"
  chg <- sub[!na_rows & sub$verdict_qp != sub$base_qp, ]
  cat("\n", p, ":\n", sep = "")
  if (nrow(chg) == 0) cat("  no region changes verdict\n")
  else for (i in seq_len(nrow(chg)))
    cat(sprintf("  %-28s %s -> %s\n", chg$region[i], chg$base_qp[i], chg$verdict_qp[i]))
  if (any(na_rows))
    cat(sprintf("  (not estimable, excluded: %s)\n",
                paste(sub$region[na_rows], collapse = ", ")))
}
cat("\n")

# =============================================================================
# SECTION C: EUROPE DECOMPOSITION
# =============================================================================

cat("\n\n=== SECTION C: EUROPE DECOMPOSITION ===\n\n")

decomp <- data.frame()
for (p in PANELS) {
  base <- panel_data(p); base <- base[!is.na(base$region_group), ]
  sets <- list(
    "All participations"              = base,
    "Excl. European belligerents"     = base[base$region_group != "Europe", ],
    "Excl. all European belligerents" = base[!(base$region_group %in% c("Europe", "E. Europe")), ])
  cat("---", p, "---\n")
  for (lab in names(sets)) {
    d <- sets[[lab]]
    n_pre <- sum(d$yrstart <= pre_end, na.rm = TRUE); n_post <- sum(d$yrstart > pre_end, na.rm = TRUE)
    disp  <- get_dispersion(d)
    res   <- quasi_poisson_test(n_pre, n_post, pre_years, post_years, disp)
    resp  <- poisson_decline_test(n_pre, n_post, pre_years, post_years)
    cat(sprintf("%-32s N=%4d  pre=%4d (%.3f/yr)  post=%4d (%.3f/yr)  disp=%.2f  change=%+.1f%%  %s\n",
                lab, nrow(d), n_pre, res$rate_pre, n_post, res$rate_post, disp,
                ifelse(is.na(res$actual_decline), 0, res$actual_decline), label_bound(res)))
    decomp <- rbind(decomp, data.frame(panel = p, comparison = lab, N = nrow(d),
      n_pre = n_pre, n_post = n_post,
      rate_pre = round(res$rate_pre, 3), rate_post = round(res$rate_post, 3),
      dispersion = round(disp, 3), actual_change = round(res$actual_decline, 1),
      result_pois = label_bound(resp, "long"), result = label_bound(res, "long"),
      stringsAsFactors = FALSE))
  }
  cat("\n")
}
write.csv(decomp, "europe_decomposition_qp.csv", row.names = FALSE)

# --- TABLE 1: global decomposition ---------------------------------------------
# The pre- and post-1950 counts are dropped in favour of the annual rates, which
# carry the same information over a fixed span and leave room for the change and
# dispersion columns to sit on one line.
tbl1 <- decomp %>%
  transmute(dataset = panel, comparison,
            rate_pre = sprintf("%.2f", rate_pre),
            rate_post = sprintf("%.2f", rate_post),
            actual_chg = sprintf("%+.1f%%", -actual_change),
            dispersion = sprintf("%.2f", dispersion),
            min_proven = result_pois, min_proven_qp = result) %>%
  gt(groupname_col = "dataset") %>%
  cols_label(comparison = "Sample",
             rate_pre = md("Pre-1950 rate<br>(per yr)"), rate_post = md("Post-1950 rate<br>(per yr)"),
             actual_chg = md("Actual<br>Change"), dispersion = md("\u03c6"),
             min_proven = md("Min. Proven<br>(Poisson)"),
             min_proven_qp = md("Min. Proven<br>(Quasi-Poisson)")) %>%
  cols_align("left", columns = c(comparison, min_proven, min_proven_qp)) %>%
  cols_align("center", columns = c(rate_pre, rate_post, actual_chg, dispersion)) %>%
  cols_width(comparison ~ pct(24), rate_pre ~ pct(10), rate_post ~ pct(10),
             actual_chg ~ pct(12), dispersion ~ pct(8),
             min_proven ~ pct(17), min_proven_qp ~ pct(19))
tbl1 <- style_table(tbl1,
  TBL(1, "The Global Decline in Conventional War Participation and its European Component, 1800\u20132011"),
  "Campaign-belligerent participations, eight analytical regions",
  paste0("Participations are assigned to the belligerent's home region, so ",
         "\"excluding European belligerents\" removes European states' participations ",
         "wherever fought, not wars fought in Europe. ", PANEL_NOTE,
         "Min. Proven Change = lower bound of the one-sided 95% CI; ",
         "the quasi-Poisson column is the inference reported in the text, and ",
         "standard Poisson is reported for comparability with the existing literature. ",
         "\u03c6 is the estimated dispersion parameter."),
  rows_per_group = 3, n_groups = length(PANELS))
TABLES[["1"]] <- tbl1
write_rtf(tbl1, "table1_global_decomposition.rtf")
cat("Saved: table1_global_decomposition.rtf\n")

# =============================================================================
# SECTION D: MERGED EUROPE
# =============================================================================

cat("\n\n=== SECTION D: MERGED EUROPE ===\n\n")

merged_regions <- c("Europe (merged)", setdiff(REGIONS, c("Europe", "E. Europe")))
merged_results <- data.frame()
for (p in PANELS) {
  m <- panel_data(p); m <- m[!is.na(m$region_group), ]
  m$merged_group <- ifelse(m$region_group %in% c("Europe", "E. Europe"),
                           "Europe (merged)", m$region_group)
  cat("---", p, "---\n")
  cat(sprintf("%-28s %5s %5s %5s %9s %7s  %s\n", "Region", "N", "Npre", "Npost", "Change%", "Disp.", "Result"))
  cat(strrep("-", 88), "\n")
  for (g in merged_regions) {
    gd <- m[m$merged_group == g, ]
    n_pre <- sum(gd$yrstart <= pre_end, na.rm = TRUE); n_post <- sum(gd$yrstart > pre_end, na.rm = TRUE)
    if (n_pre + n_post == 0) next
    disp <- get_dispersion(gd)
    res  <- quasi_poisson_test(n_pre, n_post, pre_years, post_years, disp)
    cat(sprintf("%-28s %5d %5d %5d %+8.1f%% %7.3f  %s\n", g, nrow(gd), n_pre, n_post,
                ifelse(is.na(res$actual_decline), 0, res$actual_decline), disp, label_bound(res)))
    merged_results <- rbind(merged_results, data.frame(panel = p, region = g, n_total = nrow(gd),
      n_pre = n_pre, n_post = n_post, actual_change = round(res$actual_decline, 1),
      dispersion = round(disp, 3), result = label_bound(res), stringsAsFactors = FALSE))
  }
  cat("\n")
}
write.csv(merged_results, "merged_europe_results_qp.csv", row.names = FALSE)

# =============================================================================
# SECTION E: BELLIGERENTS SPANNING REGIONS
# =============================================================================
# Reported at the level of the eight analytical regions, which is where the
# analysis happens. A belligerent whose sub-region varies within one analytical
# region has no effect on any result and is not a coding problem.

cat("\n\n=== SECTION E: BELLIGERENTS SPANNING REGIONS ===\n\n")

analytical <- df %>% filter(!is.na(statename), !is.na(region_group)) %>%
  group_by(statename) %>%
  summarise(regions = paste(sort(unique(region_group)), collapse = " | "),
            n_regions = n_distinct(region_group), n_obs = n(),
            year_min = min(yrstart, na.rm = TRUE), year_max = max(yrstart, na.rm = TRUE),
            .groups = "drop") %>% filter(n_regions > 1) %>% arrange(desc(n_obs))
cat("At the level of the eight analytical regions:\n")
if (nrow(analytical) == 0) cat("  none\n") else print(as.data.frame(analytical))

subreg <- df %>% filter(!is.na(statename), !is.na(region), source == "Project Mars") %>%
  group_by(statename) %>%
  summarise(sub_regions = paste(sort(unique(region)), collapse = ", "),
            n_sub = n_distinct(region),
            groups = paste(sort(unique(region_group)), collapse = " | "),
            n_obs = n(), .groups = "drop") %>% filter(n_sub > 1) %>% arrange(desc(n_obs))
cat("\nAt the level of the 18 sub-regions (Project Mars rows; informational only):\n")
if (nrow(subreg) == 0) cat("  none\n") else print(as.data.frame(subreg))
write.csv(analytical, "regional_migration.csv", row.names = FALSE)

# =============================================================================
# SECTION F: OVERDISPERSION
# =============================================================================

cat("\n\n=== SECTION F: OVERDISPERSION ===\n\n")
cat("phi(const) is estimated from a constant-mean model and so absorbs any real\n")
cat("shift at the break point; phi(period) nets that shift out. We report the\n")
cat("former in the text because it is the more conservative of the two.\n\n")

run_disp <- function(data, label) {
  if (nrow(data) == 0) { cat(sprintf("%-28s %11s\n", label, "no data")); return(invisible(NULL)) }
  a  <- annual_counts(data)
  dt <- dispersiontest(glm(count ~ 1, data = a, family = poisson), alternative = "greater")
  cat(sprintf("%-28s %11.3f %11.3f %9.4f  %s\n", label, get_dispersion(data),
              get_dispersion_period(data), dt$p.value,
              ifelse(dt$p.value < 0.05, "OVERDISPERSED", "Poisson OK")))
}
for (p in PANELS) {
  d <- panel_data(p); d <- d[!is.na(d$region_group), ]
  cat("---", p, "---\n")
  cat(sprintf("%-28s %11s %11s %9s  %s\n", "Region", "phi(const)", "phi(period)", "p-value", "Conclusion"))
  cat(strrep("-", 78), "\n")
  run_disp(d, "GLOBAL")
  for (g in REGIONS) run_disp(d[d$region_group == g, ], g)
  cat("\n")
}

# =============================================================================
# SECTION G: BREAK-POINT SENSITIVITY
# =============================================================================

cat("\n\n=== SECTION G: BREAK-POINT SENSITIVITY ===\n\n")

break_years <- seq(1900, 1960, by = 5)

grid_row <- function(d, B, disp, label, panel) {
  t_pre <- B - pre_start + 1; t_post <- post_end - B
  n_pre <- sum(d$yrstart <= B, na.rm = TRUE); n_post <- sum(d$yrstart > B, na.rm = TRUE)
  if (n_pre + n_post == 0) return(NULL)
  res <- quasi_poisson_test(n_pre, n_post, t_pre, t_post, disp)
  data.frame(panel = panel, break_year = B, region = label, n_pre = n_pre, n_post = n_post,
             pct_change = if (!is.na(res$actual_decline)) -res$actual_decline else NA,
             sig_cat = c(decline = "Significant decline", increase = "Significant increase",
                         none = "Not significant",
                         "n/a" = "Not estimable")[[sig_category(res)]],
             stringsAsFactors = FALSE)
}

region_grid <- data.frame(); match_counts <- data.frame()
for (p in PANELS) {
  d <- panel_data(p); d <- d[!is.na(d$region_group), ]
  region_disp <- sapply(REGIONS, function(g) get_dispersion(d[d$region_group == g, ]))
  global_disp <- get_dispersion(d)
  region_grid <- rbind(region_grid, do.call(rbind, c(
    lapply(break_years, function(B) grid_row(d, B, global_disp, "GLOBAL", p)),
    unlist(lapply(REGIONS, function(g) lapply(break_years, function(B)
      grid_row(d[d$region_group == g, ], B, region_disp[[g]], g, p))), recursive = FALSE))))

  # The baseline is DERIVED from the 1950 quasi-Poisson run rather than
  # hardcoded, so it cannot drift from the inference actually used. The 1950
  # column therefore scores 8/8 by construction, which is the correct
  # reference point.
  baseline_direction <- sapply(REGIONS, function(g) {
    gd <- d[d$region_group == g, ]
    res <- quasi_poisson_test(sum(gd$yrstart <= pre_end, na.rm = TRUE),
                              sum(gd$yrstart >  pre_end, na.rm = TRUE),
                              pre_years, post_years, region_disp[[g]])
    sig_category(res)
  })
  cat("---", p, "---\n")
  cat("Baseline pattern at 1950 (quasi-Poisson):\n")
  for (g in REGIONS) cat(sprintf("  %-28s %s\n", g, baseline_direction[[g]]))
  cat("\nRegions matching the 1950 baseline, by break year:\n")
  for (B in break_years) {
    m <- 0; tot <- 0
    for (g in REGIONS) {
      row <- region_grid[region_grid$panel == p & region_grid$break_year == B & region_grid$region == g, ]
      if (nrow(row) == 0) next
      tot <- tot + 1
      dir <- if (grepl("estimable", row$sig_cat)) "n/a" else
             if (grepl("decline", row$sig_cat)) "decline" else
             if (grepl("increase", row$sig_cat)) "increase" else "none"
      if (dir == baseline_direction[[g]]) m <- m + 1
    }
    cat(sprintf("  %-6d %d / %d%s\n", B, m, tot, ifelse(B == 1950, "   <- baseline", "")))
    match_counts <- rbind(match_counts, data.frame(panel = p, break_year = B,
      matched = m, total = tot, stringsAsFactors = FALSE))
  }
  cat("\n")
}
write.csv(region_grid, "breakpoint_grid_qp.csv", row.names = FALSE)
write.csv(match_counts, "breakpoint_match_counts.csv", row.names = FALSE)

# --- TABLE 4: break-point stability across panels ------------------------------
# The three per-panel figures made this comparison a matter of flipping between
# eight-facet grids. The count is the thing the text actually claims, so it is
# reported as a count; the two augmented-panel figures move to supplementary.
mc <- match_counts
mc$cell <- sprintf("%d / %d", mc$matched, mc$total)
wide <- data.frame(break_year = break_years)
for (pl in PANELS) {
  sub <- mc[mc$panel == pl, ]
  wide[[PANEL_KEYS[[pl]]]] <- sub$cell[match(break_years, sub$break_year)]
}
tbl4 <- wide %>% gt() %>%
  cols_label(break_year = "Break year",
             mars = "Project Mars",
             gm_nointra = "+ non-civil-war additions",
             gm_all = "+ all additions") %>%
  cols_align("center", columns = c(mars, gm_nointra, gm_all)) %>%
  cols_align("left", columns = break_year) %>%
  cols_width(break_year ~ pct(17), mars ~ pct(25), gm_nointra ~ pct(32), gm_all ~ pct(26))
tbl4 <- style_table(tbl4,
  TBL(4, "Sensitivity of the Regional Pattern to the Choice of Break Year"),
  "Regions retaining their 1950 classification, of eight",
  paste0("Each cell counts the regions whose quasi-Poisson verdict at the stated break year ",
         "matches the verdict that region receives at 1950. The 1950 row is 8 / 8 by ",
         "construction and is the reference point. ", PANEL_NOTE,
         "Figure 1 shows the underlying per-region results for the Project Mars panel; ",
         "the corresponding figures for the two augmented panels are in the supplementary materials."),
  rows_per_group = nrow(wide), n_groups = 1, has_groups = FALSE)
TABLES[["4"]] <- tbl4
write_rtf(tbl4, "table4_breakpoint_stability.rtf")
cat("Saved: table4_breakpoint_stability.rtf\n")

# =============================================================================
# SECTION H: BREAK-POINT FIGURE
# =============================================================================
# Saved at print size (5 x 7 in) so the journal does not shrink it: all text
# stays at 7-8 pt, above De Gruyter's 6 pt minimum. Title and subtitle are
# omitted because the caption in the manuscript carries that information.
# If the journal's text width is narrower than 5 in, reduce FIG_W to match.

cat("\n\n=== SECTION H: FIGURE ===\n\n")

FIG_W <- 5
FIG_H <- 7

labs_map <- c("Europe"                    = "Europe",
              "E. Europe"                 = "Eastern Europe",
              "Latin America & Caribbean" = "Latin America & Caribbean",
              "E Asia + SE Asia"          = "East & Southeast Asia",
              "Middle East + N Africa"    = "Middle East & North Africa",
              "S. Asia"                   = "South Asia",
              "Sub-Saharan Africa"        = "Sub-Saharan Africa",
              "N America"                 = "North America")

sig_levels <- c("Significant decline", "Not significant", "Significant increase")

# region_grid holds all three panels; the figure shows the Project Mars panel only.
panel_col <- intersect(c("panel", "dataset", "sample", "spec"), names(region_grid))
if (length(panel_col) == 1) {
  cat("Panels in region_grid:", paste(unique(region_grid[[panel_col]]), collapse = " | "), "\n")
  pm_label <- grep("mars", unique(region_grid[[panel_col]]), ignore.case = TRUE, value = TRUE)
  pm_label <- pm_label[!grepl("addition|gibler|civil", pm_label, ignore.case = TRUE)]
  stopifnot(length(pm_label) == 1)
  pd <- region_grid[region_grid$region != "GLOBAL" & region_grid[[panel_col]] == pm_label, ]
} else {
  stop("Can't find the panel column in region_grid. Run names(region_grid) and filter on it.")
}
stopifnot(!any(duplicated(pd[, c("region", "break_year")])))
pd$region_label <- factor(labs_map[pd$region], levels = labs_map[REGIONS])
pd$sig_cat      <- factor(pd$sig_cat, levels = sig_levels)

p <- ggplot(pd, aes(x = break_year, y = pct_change, colour = sig_cat, shape = sig_cat)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
  geom_vline(xintercept = 1950, linetype = "dashed", colour = "black",
             linewidth = 0.3, alpha = 0.6) +
  geom_line(aes(group = 1), colour = "#AAAAAA", linewidth = 0.4) +
  geom_point(size = 1.8, stroke = 0.8) +
  facet_wrap(~region_label, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = seq(1900, 1960, by = 10)) +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  scale_colour_manual(values = c("Significant decline"  = "#1A5276",
                                 "Not significant"      = "#808080",
                                 "Significant increase" = "#922B21"),
                      breaks = sig_levels, drop = FALSE, name = NULL) +
  scale_shape_manual(values = c("Significant decline"  = 19,
                                "Not significant"      = 1,
                                "Significant increase" = 19),
                     breaks = sig_levels, drop = FALSE, name = NULL) +
  labs(x = "Break year", y = "% change in rate (post vs. pre)") +
  theme_bw(base_size = 8) +
  theme(strip.background = element_rect(fill = "#ECF0F1"),
        strip.text       = element_text(face = "bold", size = 8),
        axis.title       = element_text(size = 8),
        axis.text        = element_text(size = 7),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        legend.text      = element_text(size = 8),
        legend.position  = "bottom",
        legend.margin    = margin(0, 0, 0, 0),
        panel.grid.minor = element_blank(),
        plot.margin      = margin(4, 6, 4, 4))

ggsave("breakpoint_sensitivity.png", p, width = FIG_W, height = FIG_H,
       units = "in", dpi = 600, bg = "white")
ggsave("breakpoint_sensitivity.pdf", p, width = FIG_W, height = FIG_H,
       units = "in", bg = "white")
cat("Saved: breakpoint_sensitivity.png and .pdf (", FIG_W, "x", FIG_H, "in )\n")

# =============================================================================
# SECTION I: PUBLICATION TABLES
# =============================================================================
# Three row groups per table, ordered Mars only -> Mars plus uncontested
# additions -> Mars plus all additions. Both the Poisson and quasi-Poisson
# bounds are shown; the quasi-Poisson is the inference reported in the text.

cat("\n\n=== SECTION I: PUBLICATION TABLES ===\n\n")

format_rows <- function(res_df) {
  res_df %>% mutate(
    dataset    = panel,
    actual_chg = sprintf("%+.1f%%", -actual_decline),
    min_proven = sapply(seq_len(n()), function(i) {
      label_bound(list(smallest_k_rejected = smallest_k_rejected[i],
                       largest_k_rejected_increase = largest_k_rejected_increase[i]), "long")
    }),
    min_proven_qp = sapply(seq_len(n()), function(i) {
      label_bound(list(smallest_k_rejected = qp_smallest_k_rejected[i],
                       largest_k_rejected_increase = qp_largest_k_rejected_increase[i],
                       estimable = qp_estimable[i]), "long")
    }),
    power_30  = ifelse(is.na(power_30pct), NA_real_, round(power_30pct, 2)),
    # Power bears on the interpretation of non-rejections, so the flag fires only
    # on null rows. Flagging a significant result for low power says something
    # different (effect-size inflation) and would be read as the same caution.
    low_power = !is.na(power_30pct) & power_30pct < 0.80 & verdict_qp == "none") %>%
    select(dataset, region, n_total, n_pre, n_post, actual_chg,
           min_proven, min_proven_qp, power_30, low_power)
}

make_table <- function(res_list, kia_label, outfile) {
  td  <- bind_rows(lapply(res_list, format_rows))
  lpr <- which(td$low_power)
  n_r <- length(REGIONS)
  # Zebra striping, derived from the number of panels and regions rather than
  # hardcoded, so adding or removing a panel cannot silently break it.
  stripe <- unlist(lapply(seq_along(res_list), function(j)
    seq(2, n_r, 2) + (j - 1) * n_r))
  has_ne <- any(grepl("Not estimable", td$min_proven_qp))
  note <- paste0(
    "Campaign-belligerent participations. Project Mars (Lyall 2020). ", PANEL_NOTE,
    "Pre-1950: 1800\u20131950 (", pre_years, " yr); ",
    "Post-1950: 1951\u20132011 (", post_years, " yr). ",
    "Significance: one-sided tests (p<0.05); the quasi-Poisson column is the ",
    "inference reported in the text, and standard Poisson is reported for ",
    "comparability with the existing literature. ",
    "Min. Proven Change = lower bound of the one-sided 95% CI. ")
  if (has_ne) note <- paste0(note,
    "\\*Not estimable: the quasi-Poisson interval is built on the log rate ratio, ",
    "which is undefined when a period count is zero. The Poisson column governs in ",
    "those cells; where it shows a significant decline the decline is real but cannot ",
    "be quantified under the conservative inference, and where it too shows no ",
    "significant change the pre-1950 count is too small to support any test. ")
  note <- paste0(note,
    "Power (30%) is computed under the Poisson model. Overdispersion in these series ",
    "(\u03c6 \u2248 1.3 to 4.7) substantially reduces the power actually available, so these ",
    "figures are an upper bound. ",
    "*Italicised values mark non-significant results with power below 0.80, the ",
    "conventional standard; there, the absence of a detected change is not evidence ",
    "that no change occurred.*")
  tbl <- td %>% select(-low_power) %>% gt(groupname_col = "dataset") %>%
    cols_label(region = "Region", n_total = "N",
               n_pre = md("Pre-1950<br>*N*"), n_post = md("Post-1950<br>*N*"),
               actual_chg = md("Actual<br>Change"),
               min_proven = md("Min. Proven<br>Poisson"),
               min_proven_qp = md("Min. Proven<br>Quasi-Poisson"),
               power_30 = md("Power<br>(30%)")) %>%
    cols_align("left",   columns = c(region, min_proven, min_proven_qp)) %>%
    cols_align("center", columns = c(n_total, n_pre, n_post, actual_chg, power_30)) %>%
    cols_width(region ~ pct(20), n_total ~ pct(6), n_pre ~ pct(8), n_post ~ pct(8),
               actual_chg ~ pct(12), min_proven ~ pct(17), min_proven_qp ~ pct(20),
               power_30 ~ pct(9)) %>%
    tab_style(cell_text(style = "italic"), cells_body(columns = power_30, rows = lpr))
  tbl <- style_table(tbl,
    TBL(2, "Regional Patterns in Conventional War Participation, 1800\u20132011"),
    kia_label, note, rows_per_group = n_r, n_groups = length(res_list))
  TABLES[["2"]] <<- tbl
  write_rtf(tbl, outfile)
  invisible(tbl)
}

make_table(lapply(PANEL_KEYS, function(k) res_base[[k]]),
           "No fatality threshold \u2014 all campaign-belligerent participations",
           "table2_regional.rtf")

# --- TABLE 5: fatality thresholds, compressed ---------------------------------
# The three full threshold tables (three panels x eight regions each) are replaced
# by one row per region and one column per threshold. Reporting the same 24 cells
# three times over made the reader compare across pages what can be read across a
# row, and the pattern that matters -- signal degrading as the sample shrinks --
# is only visible when the thresholds sit side by side.
threshold_table <- function(panel_key, panel_label, outfile, label) {
  cols <- list(none = res_base[[panel_key]])
  for (tn in names(thr_low)) cols[[tn]] <- res_kialow[[paste0(tn, "_", panel_key)]]
  td <- data.frame(region = REGIONS, stringsAsFactors = FALSE)
  for (nm in names(cols)) {
    r <- cols[[nm]]
    td[[nm]] <- sapply(REGIONS, function(g) {
      i <- which(r$region == g)
      label_bound(list(smallest_k_rejected = r$qp_smallest_k_rejected[i],
                       largest_k_rejected_increase = r$qp_largest_k_rejected_increase[i],
                       estimable = r$qp_estimable[i]), "long")
    })
  }
  has_ne <- any(grepl("Not estimable", unlist(td[-1])))
  # PANEL_NOTE describes the second and third panels, so it belongs only on the
  # supplementary versions; on the Project Mars table it would explain panels the
  # reader cannot see.
  note <- paste0(
    "Quasi-Poisson minimum proven change for each region at each fatality threshold. ",
    "Thresholds are the quartiles of the non-zero Project Mars kialow values. ",
    "Panel: ", panel_label, ". ",
    if (panel_key == "mars")
      "The corresponding results for the two augmented panels are in the supplementary materials. "
    else PANEL_NOTE,
    "Min. Proven Change = lower bound of the one-sided 95% CI. ")
  if (has_ne) note <- paste0(note,
    "\\*Not estimable: the interval is built on the log rate ratio, which is undefined ",
    "when a period count is zero. ")
  note <- paste0(note,
    "Cell entries thin as the threshold rises because sample sizes fall, not because ",
    "warfare behaves differently among larger conflicts.")
  tbl <- td %>% gt() %>%
    cols_label(region = "Region", none = "No threshold",
               Q1 = md(paste0("\u2265", round(thr_low[["Q1"]]), " KIA")),
               Q2 = md(paste0("\u2265", format(round(thr_low[["Q2"]]), big.mark = ","), " KIA")),
               Q3 = md(paste0("\u2265", format(round(thr_low[["Q3"]]), big.mark = ","), " KIA"))) %>%
    cols_align("left", columns = everything()) %>%
    cols_width(region ~ pct(20), none ~ pct(20), Q1 ~ pct(20), Q2 ~ pct(20), Q3 ~ pct(20))
  tbl <- style_table(tbl,
    TBL(label, "Regional Results by Conflict Size, 1800\u20132011"),
    panel_label, note, rows_per_group = length(REGIONS), n_groups = 1,
    has_groups = FALSE)
  if (panel_key == "mars") TABLES[[as.character(label)]] <<- tbl
  else                     SUPP_TABLES[[as.character(label)]] <<- tbl
  write_rtf(tbl, outfile)
}
threshold_table("mars", "Project Mars", "table5_thresholds.rtf", 5)
threshold_table("gm_nointra", "Project Mars + non-civil-war additions",
                "supp_thresholds_gm_nointra.rtf", "S1")
threshold_table("gm_all", "Project Mars + all additions",
                "supp_thresholds_gm_all.rtf", "S2")

# =============================================================================
# SECTION J: COLLAPSING CAMPAIGNS TO WARS
# =============================================================================
# Belligerent-level disaggregation is a precondition of the regional question:
# a war-level record says a war occurred and where, not who fought in it, so it
# cannot answer whether a given state has fought less often. Disaggregating
# further, to campaigns within wars, is a separate and contestable judgement.
# This section collapses each belligerent's multiple campaign entries within a
# war to a single participation, keeping the earliest start year, and reruns the
# regional test. If the verdicts hold, the contestable half of the
# disaggregation costs nothing to defend.

cat("\n\n=== SECTION J: WAR-BELLIGERENT UNIT (CAMPAIGNS COLLAPSED) ===\n\n")

collapse_campaigns <- function(d) {
  d$warkey <- ifelse(d$source == "Project Mars",
                     paste0("M", d$warcode), paste0("G", d$cow_war_no))
  d %>% group_by(warkey, statename) %>%
    slice_min(yrstart, n = 1, with_ties = FALSE) %>% ungroup() %>% as.data.frame()
}

collapse_results <- data.frame()
for (p in PANELS) {
  full <- panel_data(p); full <- full[!is.na(full$region_group), ]
  coll <- collapse_campaigns(full)
  cat("---", p, "---\n")
  cat(sprintf("campaign-belligerent rows: %d | war-belligerent rows: %d (%.0f%% retained)\n",
              nrow(full), nrow(coll), 100 * nrow(coll) / nrow(full)))
  cat(sprintf("%-28s %5s %5s %9s %7s  %-18s %-18s %s\n",
              "Region", "Npre", "Npost", "Change%", "Disp.", "Campaign unit", "War unit", "Same?"))
  cat(strrep("-", 104), "\n")
  for (g in c("GLOBAL", REGIONS)) {
    fd <- if (g == "GLOBAL") full else full[full$region_group == g, ]
    cd <- if (g == "GLOBAL") coll else coll[coll$region_group == g, ]
    rf <- quasi_poisson_test(sum(fd$yrstart <= pre_end), sum(fd$yrstart > pre_end),
                             pre_years, post_years, get_dispersion(fd))
    rc <- quasi_poisson_test(sum(cd$yrstart <= pre_end), sum(cd$yrstart > pre_end),
                             pre_years, post_years, get_dispersion(cd))
    same <- if (!rc$estimable || !rf$estimable) "n/a"
            else if (sig_category(rf) == sig_category(rc)) "yes" else "DIFFERS"
    cat(sprintf("%-28s %5d %5d %+8.1f%% %7.3f  %-18s %-18s %s\n",
                g, sum(cd$yrstart <= pre_end), sum(cd$yrstart > pre_end),
                ifelse(is.na(rc$actual_decline), 0, rc$actual_decline),
                get_dispersion(cd), label_bound(rf), label_bound(rc), same))
    collapse_results <- rbind(collapse_results, data.frame(
      panel = p, region = g,
      n_pre_camp = sum(fd$yrstart <= pre_end), n_post_camp = sum(fd$yrstart > pre_end),
      n_pre_war  = sum(cd$yrstart <= pre_end), n_post_war  = sum(cd$yrstart > pre_end),
      change_war = round(rc$actual_decline, 1), dispersion_war = round(get_dispersion(cd), 3),
      result_camp = label_bound(rf), result_war = label_bound(rc), same = same,
      stringsAsFactors = FALSE))
  }
  cat("\n")
}
write.csv(collapse_results, "campaign_collapse_qp.csv", row.names = FALSE)

# =============================================================================
# SECTION K: EUROPEAN PARTICIPATIONS -- AT HOME AND ABROAD
# =============================================================================
# Participations are assigned to the belligerent's HOME region. The claim that
# European states have ceased to fight anywhere, rather than merely that Europe
# has been free of war, requires knowing where those participations were fought.
# Lyall's own regional dummies code the location of the campaign, so crossing
# them with our home-region coding separates European belligerents fighting in
# Europe from European belligerents fighting elsewhere. Project Mars rows only:
# the appended cases do not carry Lyall's variables.

cat("\n\n=== SECTION K: EUROPEAN PARTICIPATIONS AT HOME AND ABROAD ===\n\n")

LOC <- c(weurope = "W. Europe", eeurop = "E. Europe", lamerica = "Latin America",
         ssafrica = "Sub-Saharan Africa", asia = "Asia", nafrme = "N. Africa / Mid. East",
         namerica = "N. America")

eur <- df[df$source == "Project Mars" & !is.na(df$region_group) &
          df$region_group %in% c("Europe", "E. Europe"), ]
eur_w <- eur[eur$region_group == "Europe", ]   # Western European belligerents

cat("Where Western European belligerents fought (Lyall campaign-location codes):\n")
cat(sprintf("%-24s %10s %10s\n", "Campaign location", "pre-1950", "post-1950"))
cat(strrep("-", 46), "\n")
loc_tab <- data.frame()
for (v in names(LOC)) {
  a <- sum(eur_w[[v]][eur_w$yrstart <= pre_end], na.rm = TRUE)
  b <- sum(eur_w[[v]][eur_w$yrstart >  pre_end], na.rm = TRUE)
  cat(sprintf("%-24s %10d %10d\n", LOC[[v]], a, b))
  # The change is computable for every location, so the column is filled rather
  # than stubbed; only the interval is reserved for the two aggregated series,
  # which is why the bound column has been removed from the table entirely.
  ch <- if (a > 0) sprintf("%+.1f%%", 100 * ((b / post_years) / (a / pre_years) - 1)) else "n/a"
  loc_tab <- rbind(loc_tab, data.frame(grp = "Campaign location", cat = LOC[[v]],
    n_pre = a, n_post = b, chg = ch, stringsAsFactors = FALSE))
}
unclass_pre  <- sum(rowSums(eur_w[eur_w$yrstart <= pre_end, names(LOC)], na.rm = TRUE) == 0)
unclass_post <- sum(rowSums(eur_w[eur_w$yrstart >  pre_end, names(LOC)], na.rm = TRUE) == 0)
cat(sprintf("%-24s %10d %10d\n", "(no location code)", unclass_pre, unclass_post))

# In-Europe vs outside-Europe, tested separately
eur_w$in_europe <- (rowSums(eur_w[, c("weurope", "eeurop")], na.rm = TRUE) > 0)
cat("\nSeparate tests, Western European belligerents:\n")
cat(sprintf("%-28s %5s %5s %9s %7s  %s\n", "Series", "Npre", "Npost", "Change%", "Disp.", "Result"))
cat(strrep("-", 82), "\n")
euro_split <- data.frame()
for (lab in c("Fought in Europe", "Fought outside Europe")) {
  s <- eur_w[eur_w$in_europe == (lab == "Fought in Europe"), ]
  np <- sum(s$yrstart <= pre_end); nq <- sum(s$yrstart > pre_end)
  dp <- get_dispersion(s)
  r  <- quasi_poisson_test(np, nq, pre_years, post_years, dp)
  cat(sprintf("%-28s %5d %5d %+8.1f%% %7.3f  %s\n", lab, np, nq,
              ifelse(is.na(r$actual_decline), 0, r$actual_decline), dp, label_bound(r)))
  euro_split <- rbind(euro_split, data.frame(series = lab, n_pre = np, n_post = nq,
    rate_pre = round(np / pre_years, 3), rate_post = round(nq / post_years, 3),
    change = round(r$actual_decline, 1), dispersion = round(dp, 3),
    result = label_bound(r), stringsAsFactors = FALSE))
  loc_tab <- rbind(loc_tab, data.frame(grp = "Aggregated series", cat = lab,
    n_pre = np, n_post = nq, chg = sprintf("%+.1f%%", -r$actual_decline),
    stringsAsFactors = FALSE))
  if (lab == "Fought in Europe") euro_bound_in <- label_bound(r, "long")
  else                           euro_bound_out <- label_bound(r, "long")
}

# --- TABLE 3: European participations at home and abroad -----------------------
tbl3 <- loc_tab %>% gt(groupname_col = "grp") %>%
  cols_label(cat = "", n_pre = md("Pre-1950<br>*N*"), n_post = md("Post-1950<br>*N*"),
             chg = md("Actual<br>Change")) %>%
  cols_align("left", columns = cat) %>%
  cols_align("center", columns = c(n_pre, n_post, chg)) %>%
  cols_width(cat ~ pct(46), n_pre ~ pct(18), n_post ~ pct(18), chg ~ pct(18))
tbl3 <- style_table(tbl3,
  TBL(3, "Where Western European Belligerents Fought, Before and After 1950"),
  "Project Mars participations, home region Europe",
  paste0("Home region is the belligerent's; campaign location is Lyall's own regional ",
         "coding. A participation counts as fought in Europe if the campaign is coded ",
         "in Western or Eastern Europe. Project Mars rows only, since the appended cases ",
         "do not carry Lyall's location variables. Tested separately under quasi-Poisson, ",
         "the minimum proven decline is ", euro_bound_in, " for participations fought in Europe and ",
         euro_bound_out, " for those fought elsewhere."),
  rows_per_group = 7, n_groups = 1)
TABLES[["3"]] <- tbl3
write_rtf(tbl3, "table3_europe_home_abroad.rtf")
cat("\nSaved: table3_europe_home_abroad.rtf\n")
write.csv(euro_split, "europe_home_abroad_qp.csv", row.names = FALSE)
cat("\nNote: a participation is 'fought in Europe' if Lyall codes the campaign in\n")
cat("Western or Eastern Europe. Both series are belligerent-level counts for\n")
cat("Western European belligerents only.\n")

# =============================================================================
# COMBINED TABLES DOCUMENT
# =============================================================================
# One file with every table in numerical order, each starting a new page, as the
# journal asks for tables on separate pages. Built from the same gt objects that
# produced the individual files, so the two cannot disagree.

cat("\n\n=== COMBINED TABLES ===\n\n")
combine_rtf(TABLES[order(as.numeric(names(TABLES)))], "all_tables.rtf")
combine_rtf(SUPP_TABLES[order(names(SUPP_TABLES))], "supplementary_tables.rtf")

# =============================================================================
cat("\n\n", strrep("=", 77), "\nALL ANALYSES COMPLETE\n", strrep("=", 77), "\n\n", sep = "")
cat("Data file:", DATA_FILE, "\n")
cat("MIDs excluded:", sum(is_mid), "rows\n\nOutputs:\n")
cat("  Combined: all_tables.rtf (Tables 1-5, one per page)\n")
cat("            supplementary_tables.rtf (Tables S1-S2)\n")
cat("  Separate: table1_global_decomposition.rtf, table2_regional.rtf,\n")
cat("            table3_europe_home_abroad.rtf, table4_breakpoint_stability.rtf,\n")
cat("            table5_thresholds.rtf\n")
cat("  Supp:     supp_thresholds_gm_nointra.rtf, supp_thresholds_gm_all.rtf,\n")
cat("            supp_figure_breakpoint_[gm_nointra/gm_all].tif\n")
cat("  Figure:   figure1_breakpoint_mars.tif\n")
cat("  CSVs:     quasi_poisson_results.csv, europe_decomposition_qp.csv,\n")
cat("            campaign_collapse_qp.csv, europe_home_abroad_qp.csv,\n")
cat("            merged_europe_results_qp.csv, breakpoint_grid_qp.csv,\n")
cat("            regional_migration.csv, results_base_*.csv,\n")
cat("            results_kialow_Q[1-3]_[mars/gm_nointra/gm_all].csv\n")
