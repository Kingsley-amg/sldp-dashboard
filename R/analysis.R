# =============================================================================
# SLDP pilot: statistical analysis for the dashboard (objectives O1 to O7)
# Author: Kingsley Amegah, Data Scientist / Health Data Specialist
#
# Same methods as SLDP_monitoring_analysis.Rmd. Sourced after R/pipeline.R.
# Every result is an aggregate (estimate, CI, p-value, count), with readable
# column names and term labels, ready to be shown on the public dashboard.
# Each block is wrapped so that thin early-pilot data skips a model rather
# than stopping the build; the dashboard then explains what is still needed.
# =============================================================================
suppressPackageStartupMessages({ library(lme4); library(survival) })

fmt_p   <- function(p) ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
fmt_ci  <- function(lo, hi, d = 2) sprintf(paste0("%.", d, "f to %.", d, "f"), lo, hi)
prop_ci <- function(x, n) { if (is.na(n) || n == 0) return(c(NA, NA, NA)); b <- binom.test(x, n); 100 * c(x / n, b$conf.int) }
prop_txt <- function(x, n) { v <- prop_ci(x, n); if (is.na(v[1])) "Not enough data" else sprintf("%.1f%% (%.1f to %.1f)", v[1], v[2], v[3]) }
tbl <- function(df) list(columns = names(df), rows = unname(lapply(seq_len(nrow(df)), function(i) unname(as.list(df[i, , drop = FALSE])))))
pending <- function(id, objective, title, method, need) list(id = id, objective = objective, title = title, method = method,
                                                                status = "pending", note = need)
done <- function(id, objective, title, method, table, finding, chart = NULL)
  list(id = id, objective = objective, title = title, method = method, status = "ok", table = tbl(table),
       finding = gsub("p = <0.001", "p < 0.001", finding, fixed = TRUE), chart = chart)

term_label <- function(t) {
  lab <- c("age_group5-14" = "Age 5-14 years (vs under 5)", "age_group15-24" = "Age 15-24 years (vs under 5)",
           "age_group25-44" = "Age 25-44 years (vs under 5)", "age_group45+" = "Age 45+ years (vs under 5)",
           "sexmale" = "Male (vs female)", "cem_siteTRUE" = "CEM site (vs non-CEM site)",
           "(Intercept)" = "Intercept (Day 0, SLDP not given)", "day3" = "Day 3 vs Day 0",
           "sldp_givenTRUE" = "SLDP given (at Day 0)", "age_years" = "Age (per year)",
           "day3:sldp_givenTRUE" = "Extra Day 3 change when SLDP given", "sldpSLDP not given" = "SLDP not given (vs given)")
  unname(ifelse(t %in% names(lab), lab[t], t))
}
A <- list()

# ---- O1. Coverage: mixed-effects logistic regression -----------------------
A$o1_cov <- tryCatch({
  d <- reg |> filter(screen_status == "eligible", !is.na(age_group), !is.na(sex)) |>
    mutate(given = as.integer(sldp_given), age_group = droplevels(age_group))
  if (nrow(d) < 50 || n_distinct(d$given) < 2 || n_distinct(d$facility) < 3) stop("need")
  m <- suppressWarnings(glmer(given ~ age_group + sex + cem_site + (1 | facility), data = d, family = binomial,
                              control = glmerControl(optimizer = "bobyqa")))
  co <- summary(m)$coefficients; vf <- as.numeric(VarCorr(m)$facility); icc <- vf / (vf + pi^2 / 3)
  t <- tibble(Factor = term_label(rownames(co)), `Odds ratio` = round(exp(co[, 1]), 2),
              `95% CI` = fmt_ci(exp(co[, 1] - 1.96 * co[, 2]), exp(co[, 1] + 1.96 * co[, 2])), `p-value` = fmt_p(co[, 4]),
              lo = exp(co[, 1] - 1.96 * co[, 2]), hi = exp(co[, 1] + 1.96 * co[, 2]))[-1, ]
  sig <- t$Factor[co[-1, 4] < 0.05]
  done("o1_cov", "O1 Coverage", "Factors associated with receiving SLDP among eligible patients",
       sprintf("Mixed-effects logistic regression, random intercept for facility (%d eligible patients, %d facilities).", nrow(d), n_distinct(d$facility)),
       t |> select(-lo, -hi),
       paste0(if (length(sig)) paste0("Significant at 5%: ", paste(sig, collapse = "; "), ". ") else "No factor is significantly associated with receiving SLDP at the 5% level. ",
              sprintf("Facility intraclass correlation = %.3f: about %.0f%% of the variation in receiving SLDP lies between facilities, so facility practice matters.", icc, 100 * icc)),
       chart = list(type = "forest", null = 1, label = "Odds ratio",
                    points = lapply(seq_len(nrow(t)), function(i) list(name = t$Factor[i], est = t$`Odds ratio`[i], lo = round(t$lo[i], 3), hi = round(t$hi[i], 3)))))
}, error = function(e) pending("o1_cov", "O1 Coverage", "Factors associated with receiving SLDP among eligible patients",
                               "Mixed-effects logistic regression, random intercept for facility.", "Runs once there are at least 50 eligible patients across 3 or more facilities."))

# ---- O1. Coverage by subgroup with exact CIs --------------------------------
A$o1_sub <- tryCatch({
  sub_cov <- function(var, nm) reg |> filter(screen_status == "eligible") |> group_by(Level = as.character(.data[[var]])) |>
    summarise(Eligible = n(), `Given SLDP` = sum(sldp_given), .groups = "drop") |> mutate(Subgroup = nm, .before = 1)
  t <- bind_rows(sub_cov("district_lbl", "District"), sub_cov("age_group", "Age group"), sub_cov("sex", "Sex"), sub_cov("act_type_lbl", "ACT given")) |>
    mutate(Level = stringr::str_to_sentence(coalesce(Level, "Not recorded")),
           `Coverage (95% CI)` = purrr::map2_chr(`Given SLDP`, Eligible, prop_txt))
  if (nrow(t) == 0) stop("need")
  done("o1_sub", "O1 Coverage", "SLDP coverage among eligible patients by subgroup",
       "Proportion given SLDP with exact (Clopper-Pearson) 95% confidence intervals.", t,
       "Subgroups whose interval sits clearly below the others are the ones to target in supervision.")
}, error = function(e) pending("o1_sub", "O1 Coverage", "SLDP coverage by subgroup", "Exact binomial CIs.", "No eligible patients yet."))

# ---- O3. Screening agreement (Cohen's kappa) ---------------------------------
A$o3_kappa <- tryCatch({
  d <- reg |> filter(!is.na(screen_status), !is.na(provider_eligibility)) |> mutate(calc = screen_status == "eligible", clin = provider_eligibility == "yes")
  if (nrow(d) < 10) stop("need")
  tab <- table(factor(d$calc, c(FALSE, TRUE)), factor(d$clin, c(FALSE, TRUE))); n <- sum(tab)
  po <- sum(diag(tab)) / n; pe <- sum(rowSums(tab) * colSums(tab)) / n^2; k <- (po - pe) / (1 - pe)
  se <- sqrt(po * (1 - po) / (n * (1 - pe)^2))
  strength <- cut(k, c(-Inf, 0.2, 0.4, 0.6, 0.8, Inf), labels = c("slight", "fair", "moderate", "substantial", "almost perfect"))
  t <- tibble(Measure = c("Patients compared", "Observed agreement", "Cohen's kappa (95% CI)",
                          "Clinician said eligible, form said not", "Clinician said not eligible, form said eligible"),
              Value = c(n, sprintf("%.1f%%", 100 * po), sprintf("%.3f (%s)", k, fmt_ci(k - 1.96 * se, k + 1.96 * se, 3)),
                        tab["FALSE", "TRUE"], tab["TRUE", "FALSE"]) |> as.character())
  done("o3_kappa", "O3 Screening and dosing", "Agreement between calculated eligibility and the clinician's decision",
       "Cohen's kappa with asymptotic 95% CI.", t,
       sprintf("Kappa %.2f indicates %s agreement (Landis and Koch). Overrides in either direction should be reviewed against the exclusion criteria.", k, strength))
}, error = function(e) pending("o3_kappa", "O3 Screening and dosing", "Screening agreement", "Cohen's kappa.", "Needs at least 10 patients with both decisions recorded."))

# ---- O3. Dose per kg by age group -------------------------------------------
A$o3_dose <- tryCatch({
  d <- reg |> filter(!is.na(mg_per_kg)); if (nrow(d) < 5) stop("need")
  t <- d |> group_by(`Age group` = as.character(age_group)) |>
    summarise(Patients = n(), `Median mg/kg` = round(median(mg_per_kg), 3),
              `Interquartile range` = sprintf("%.2f to %.2f", quantile(mg_per_kg, .25), quantile(mg_per_kg, .75)),
              `Within 0.15-0.4 mg/kg` = prop_txt(sum(between(mg_per_kg, SLDP_DOSE_LOW, SLDP_DOSE_HIGH)), n()),
              `Above range` = sum(mg_per_kg > SLDP_DOSE_HIGH), `Below range` = sum(mg_per_kg < SLDP_DOSE_LOW), .groups = "drop")
  kw <- if (n_distinct(d$age_group) > 1) kruskal.test(mg_per_kg ~ age_group, data = d)$p.value else NA
  done("o3_dose", "O3 Screening and dosing", "SLDP dose received per kilogram, by age group",
       "Median and IQR of mg base per kg; Kruskal-Wallis test across age groups.", t,
       sprintf("Overall median %.3f mg/kg against a 0.25 target. Difference between age groups: Kruskal-Wallis p = %s. Doses outside the range point to the dosing band used for that age.",
               median(d$mg_per_kg), fmt_p(kw)))
}, error = function(e) pending("o3_dose", "O3 Screening and dosing", "Dose per kg", "Median, IQR.", "Needs at least 5 doses with a weight."))

# ---- O4. Adverse event incidence and risk ratio ------------------------------
cohort_pat <- patients |> filter(cem_in)
A$o4_rr <- tryCatch({
  tab <- with(cohort_pat, table(factor(sldp_given, c(TRUE, FALSE)), factor(any_event, c(TRUE, FALSE))))
  if (!(all(rowSums(tab) > 0) && all(tab[, 1] > 0))) stop("need")
  r1 <- tab[1, 1] / sum(tab[1, ]); r0 <- tab[2, 1] / sum(tab[2, ])
  se <- sqrt(1 / tab[1, 1] - 1 / sum(tab[1, ]) + 1 / tab[2, 1] - 1 / sum(tab[2, ]))
  rr <- r1 / r0; lo <- exp(log(rr) - 1.96 * se); hi <- exp(log(rr) + 1.96 * se); fp <- fisher.test(tab)$p.value
  sae <- with(cohort_pat, tapply(any_serious, sldp_given, sum))
  t <- tibble(Group = c("SLDP given", "SLDP not given"), Patients = as.integer(rowSums(tab)), `With an adverse event` = as.integer(tab[, 1]),
              `Incidence (95% CI)` = c(prop_txt(tab[1, 1], sum(tab[1, ])), prop_txt(tab[2, 1], sum(tab[2, ]))),
              `With a serious event` = as.integer(c(sae[["TRUE"]] %||% 0, sae[["FALSE"]] %||% 0)))
  done("o4_rr", "O4 Safety", "Adverse event incidence in the CEM cohort, SLDP given vs not given",
       "Cumulative incidence per patient with exact 95% CI; risk ratio with log-normal CI; Fisher's exact test.", t,
       sprintf("Risk ratio %.2f (95%% CI %s), Fisher p = %s. %s", rr, fmt_ci(lo, hi), fmt_p(fp),
               if (lo > 1) "Patients given SLDP had a higher event risk: review the events in detail." else "No evidence of excess adverse events with SLDP."),
       chart = list(type = "forest", null = 1, label = "Risk ratio", points = list(list(name = "SLDP given vs not given", est = round(rr, 3), lo = round(lo, 3), hi = round(hi, 3)))))
}, error = function(e) pending("o4_rr", "O4 Safety", "Adverse event incidence and risk ratio", "Risk ratio, Fisher's test.", "Needs enrolled patients in both groups and at least one event in each."))

# ---- O4. Haemoglobin Day 0 to Day 3 -----------------------------------------
A$o4_hb <- tryCatch({
  p <- patients |> filter(!is.na(hb_d0), !is.na(hb_d3)); if (nrow(p) < 5) stop("need")
  wt <- wilcox.test(p$hb_d3, p$hb_d0, paired = TRUE, exact = FALSE)
  tt <- t.test(p$hb_d3, p$hb_d0, paired = TRUE)
  t <- tibble(Measure = c("Patients with both values", "Median Hb Day 0 (g/dL)", "Median Hb Day 3 (g/dL)", "Mean change (95% CI)",
                          "Fall of 2 g/dL or more", "Wilcoxon signed-rank p-value"),
              Value = c(nrow(p), sprintf("%.1f", median(p$hb_d0)), sprintf("%.1f", median(p$hb_d3)),
                        sprintf("%.2f g/dL (%s)", mean(p$hb_change), fmt_ci(tt$conf.int[1], tt$conf.int[2])),
                        prop_txt(sum(p$hb_drop_alert), nrow(p)), fmt_p(wt$p.value)) |> as.character())
  done("o4_hb", "O4 Safety", "Haemoglobin change from Day 0 to Day 3",
       "Paired comparison: Wilcoxon signed-rank test and paired t-test CI for the mean change.", t,
       sprintf("Mean change %.2f g/dL. %d patient(s) had a fall of 2 g/dL or more, each of which needs a Form 3 report and clinical review.", mean(p$hb_change), sum(p$hb_drop_alert)))
}, error = function(e) pending("o4_hb", "O4 Safety", "Haemoglobin change Day 0 to Day 3", "Wilcoxon signed-rank.", "Needs at least 5 patients with Hb on Day 0 and Day 3."))

A$o4_hb_lmm <- tryCatch({
  d <- cem |> filter(visit_type %in% c("d0", "d3"), !is.na(hb)) |> left_join(reg |> select(patient_id, age_years, sex, sldp_given), by = "patient_id") |>
    mutate(day3 = as.integer(visit_type == "d3"))
  if (nrow(d) < 30 || n_distinct(d$patient_id) < 15 || n_distinct(d$sldp_given) < 2) stop("need")
  m <- suppressWarnings(lmer(hb ~ day3 * sldp_given + age_years + sex + (1 | patient_id), data = d)); co <- summary(m)$coefficients
  pz <- 2 * pnorm(-abs(co[, 3]))
  t <- tibble(Term = term_label(rownames(co)), `Estimate (g/dL)` = round(co[, 1], 3),
              `95% CI` = fmt_ci(co[, 1] - 1.96 * co[, 2], co[, 1] + 1.96 * co[, 2], 3), `p-value (normal approx.)` = fmt_p(pz))
  it <- which(rownames(co) == "day3:sldp_givenTRUE")
  done("o4_hb_lmm", "O4 Safety", "Linear mixed model of haemoglobin (Day 0 and Day 3)",
       sprintf("Linear mixed model with a random intercept per patient, adjusted for age and sex (%d measurements, %d patients). Uses patients with only one value as well.", nrow(d), n_distinct(d$patient_id)),
       t, if (length(it)) sprintf("The interaction term (%.2f g/dL, p = %s) tests whether the Day 3 change differs when SLDP was given; a significant negative value would suggest SLDP-related haemolysis.", co[it, 1], fmt_p(pz[it])) else "")
}, error = function(e) pending("o4_hb_lmm", "O4 Safety", "Linear mixed model of haemoglobin", "LMM, patient random intercept.", "Needs 30 Hb values from 15 or more patients in both groups."))

# ---- O4. Time to first adverse event: Kaplan-Meier and Cox -------------------
tte <- cohort_pat |> filter(!is.na(day0)) |>
  mutate(event_time = as.numeric(first_ae_date - day0), censor_time = pmin(as.numeric(coalesce(last_contact, day0) - day0), 28),
         status = as.integer(!is.na(event_time) & event_time >= 0 & event_time <= 28),
         time = if_else(status == 1, pmax(event_time, 0.5), pmax(censor_time, 0.5)),
         sldp = factor(if_else(sldp_given, "SLDP given", "SLDP not given")))
A$o4_km <- tryCatch({
  if (nrow(tte) < 20 || sum(tte$status) < 3) stop("need")
  km <- survfit(Surv(time, status) ~ sldp, data = tte); s <- summary(km, censored = TRUE)
  curves <- tibble(group = sub("^sldp=", "", as.character(s$strata)), time = s$time, ci = 1 - s$surv, lo = 1 - s$upper, hi = 1 - s$lower)
  lr <- if (n_distinct(tte$sldp) == 2) survdiff(Surv(time, status) ~ sldp, data = tte) else NULL
  lr_p <- if (!is.null(lr)) 1 - pchisq(lr$chisq, length(lr$n) - 1) else NA
  last <- curves |> group_by(Group = group) |> slice_tail(n = 1) |> ungroup()
  t <- last |> transmute(Group, `Cumulative incidence by Day 28` = sprintf("%.1f%%", 100 * ci), `95% CI` = sprintf("%.1f%% to %.1f%%", 100 * lo, 100 * hi)) |>
    left_join(tte |> group_by(Group = as.character(sldp)) |> summarise(Patients = n(), Events = sum(status)), by = "Group")
  done("o4_km", "O4 Safety", "Time to first adverse event within 28 days (Kaplan-Meier)",
       "Kaplan-Meier cumulative incidence from Day 0; patients without an event censored at last completed contact; log-rank test.", t,
       sprintf("Log-rank p = %s.", fmt_p(lr_p)),
       chart = list(type = "km", label = "Cumulative incidence", series = lapply(split(curves, curves$group), function(g)
         list(name = g$group[1], time = c(0, g$time), est = round(c(0, g$ci), 4), lo = round(c(0, g$lo), 4), hi = round(c(0, g$hi), 4))) |> unname()))
}, error = function(e) pending("o4_km", "O4 Safety", "Time to first adverse event (Kaplan-Meier)", "Kaplan-Meier, log-rank.", "Needs at least 20 followed patients and 3 events."))

A$o4_cox <- tryCatch({
  if (nrow(tte) < 20 || sum(tte$status) < 3 || n_distinct(tte$sldp) < 2) stop("need")
  m <- suppressWarnings(coxph(Surv(time, status) ~ sldp + age_years + sex, data = tte)); co <- summary(m)$coefficients; ci <- summary(m)$conf.int
  ph <- tryCatch(cox.zph(m)$table["GLOBAL", "p"], error = function(e) NA)
  t <- tibble(Factor = term_label(rownames(co)), `Hazard ratio` = round(ci[, 1], 2), `95% CI` = fmt_ci(ci[, 3], ci[, 4]), `p-value` = fmt_p(co[, 5]))
  done("o4_cox", "O4 Safety", "Cox proportional hazards model for a first adverse event",
       "Cox regression adjusted for age and sex; proportional hazards checked with Schoenfeld residuals.", t,
       sprintf("Proportional hazards check: global Schoenfeld test p = %s. %s", fmt_p(ph),
               if (!is.na(ph) && ph < 0.05) "The assumption may not hold; read the hazard ratios with caution and rely on the Kaplan-Meier curves." else "The assumption holds."),
       chart = list(type = "forest", null = 1, label = "Hazard ratio", points = lapply(seq_len(nrow(t)), function(i)
         list(name = t$Factor[i], est = round(ci[i, 1], 3), lo = round(ci[i, 3], 3), hi = round(ci[i, 4], 3)))))
}, error = function(e) pending("o4_cox", "O4 Safety", "Cox model for a first adverse event", "Cox regression.", "Needs at least 20 followed patients, 3 events and both groups."))

# ---- O5. Retention and contact attempts --------------------------------------
A$o5_ret <- tryCatch({
  d <- patients |> filter(cem_in, !is.na(day0), day0 + 33 < TODAY); if (nrow(d) == 0) stop("need")
  t <- tibble(Measure = c("Patients whose Day 28 window has closed", "Completed Day 28 (95% CI)", "Withdrawn", "Lost to follow-up", "Died"),
              Value = c(nrow(d), prop_txt(sum(d$d28_done), nrow(d)), sum(d$exit_status == "withdrawn", na.rm = TRUE),
                        sum(d$exit_status == "lost", na.rm = TRUE), sum(d$exit_status == "died", na.rm = TRUE)) |> as.character())
  done("o5_ret", "O5 CEM performance", "Retention to Day 28", "Proportion completing Day 28 with exact 95% CI.", t,
       "Retention below 80% weakens the safety estimates; trace lost patients through the alternative contact.")
}, error = function(e) pending("o5_ret", "O5 CEM performance", "Retention to Day 28", "Exact binomial CI.", "Shown once the first Day 28 windows close (Day 0 + 33 days)."))

A$o5_att <- tryCatch({
  t <- cem |> filter(phone_visit) |> group_by(Attempt = pmin(coalesce(attempt_number, 1), 4)) |>
    summarise(Calls = n(), Reached = sum(contact_success == "yes", na.rm = TRUE), .groups = "drop") |>
    mutate(Attempt = if_else(Attempt == 4, "4 or more", as.character(Attempt)), `Success rate (95% CI)` = purrr::map2_chr(Reached, Calls, prop_txt))
  if (nrow(t) == 0) stop("need")
  tr <- cem |> filter(phone_visit) |> mutate(a = pmin(coalesce(attempt_number, 1), 4), s = contact_success == "yes")
  trend_p <- tryCatch(suppressWarnings(prop.trend.test(tapply(tr$s, tr$a, sum, na.rm = TRUE), tapply(tr$s, tr$a, length)))$p.value, error = function(e) NA)
  done("o5_att", "O5 CEM performance", "Telephone contact success by attempt number",
       "Proportion reached per attempt; chi-squared test for trend.", t,
       sprintf("Test for trend across attempts: p = %s. If later attempts still succeed often, keep calling before marking a patient lost.", fmt_p(trend_p)))
}, error = function(e) pending("o5_att", "O5 CEM performance", "Contact success by attempt", "Trend test.", "No telephone visits yet."))

# ---- O6. Parasitological follow-up -------------------------------------------
A$o6_gam <- tryCatch({
  d3 <- cem |> filter(visit_type == "d3", completed)
  g0 <- reg |> filter(micro_result %in% c("positive", "negative")) |> transmute(patient_id, g0 = coalesce(parasite_stages__gametocytes, 0L) == 1 & micro_result == "positive")
  g3 <- d3 |> filter(d3_microscopy_result %in% c("positive", "negative")) |> transmute(patient_id, g3 = coalesce(d3_microscopy_stages__gametocytes, 0L) == 1 & d3_microscopy_result == "positive")
  g <- inner_join(g0, g3, by = "patient_id"); if (nrow(g) < 10) stop("need")
  mp <- tryCatch(mcnemar.test(table(factor(g$g0, c(TRUE, FALSE)), factor(g$g3, c(TRUE, FALSE))))$p.value, error = function(e) NA)
  pos <- d3 |> filter(d3_repeat_test_done == "yes")
  t <- tibble(Measure = c("Day 3 repeat tests", "Day 3 microscopy positive (95% CI)", "Patients with microscopy on Day 0 and Day 3",
                          "Gametocyte carriage Day 0 (95% CI)", "Gametocyte carriage Day 3 (95% CI)", "McNemar test p-value"),
              Value = c(nrow(pos), prop_txt(sum(pos$d3_microscopy_result == "positive", na.rm = TRUE), nrow(pos)), nrow(g),
                        prop_txt(sum(g$g0), nrow(g)), prop_txt(sum(g$g3), nrow(g)), fmt_p(mp)) |> as.character())
  done("o6_gam", "O6 Parasitology", "Day 3 positivity and gametocyte carriage, Day 0 vs Day 3",
       "Paired proportions; McNemar test for the change in gametocyte carriage.", t,
       sprintf("Gametocyte carriage went from %.0f%% on Day 0 to %.0f%% on Day 3 (McNemar p = %s). %s",
               100 * mean(g$g0), 100 * mean(g$g3), fmt_p(mp),
               if (mean(g$g3) < mean(g$g0)) "A fall is consistent with the transmission-blocking aim of SLDP." else "Carriage did not fall: check microscopy quality and dosing."))
}, error = function(e) pending("o6_gam", "O6 Parasitology", "Gametocyte carriage Day 0 vs Day 3", "McNemar test.", "Needs at least 10 patients with microscopy on both days."))

# ---- O7. Missingness in key fields -------------------------------------------
A$o7_miss <- tryCatch({
  chk <- tibble::tribble(~Form, ~Field, ~var,
    "Form 1 Registration", "Weight", "weight", "Form 1 Registration", "Phone number", "phone", "Form 1 Registration", "Birth year", "birth_year",
    "Form 1 Registration", "ACT type", "act_type", "Form 1 Registration", "SLDP status", "sldp_status", "Form 1 Registration", "SLDP dose", "sldp_dose_mg",
    "Form 1 Registration", "CEM consent", "cem_consent", "Form 2 Day 0/3 visits", "Haemoglobin", "hb", "Form 2 Day 0/3 visits", "Temperature", "temperature",
    "Form 3 ADR/SAE", "Onset date", "event_onset", "Form 3 ADR/SAE", "Intensity", "intensity", "Form 3 ADR/SAE", "Seriousness", "serious", "Form 3 ADR/SAE", "Outcome", "outcome")
  t <- chk |> mutate(`Missing (%)` = purrr::map2_dbl(Form, var, function(f, v) {
      d <- switch(substr(f, 1, 6), "Form 1" = reg, "Form 2" = cem |> filter(visit_type %in% c("d0", "d3"), completed), "Form 3" = adr)
      # Only count a field as missing where the form actually asks it
      if (v == "sldp_dose_mg") d <- d |> filter(sldp_given)
      if (v == "cem_consent")  d <- d |> filter(cem_site)
      if (v == "act_type")     d <- d |> filter(pf_positive == "yes")
      if (!v %in% names(d) || nrow(d) == 0) NA_real_ else round(100 * mean(is.na(d[[v]])), 1) })) |> select(-var) |> arrange(desc(`Missing (%)`))
  done("o7_miss", "O7 Data quality", "Missing values in key fields", "Share of records with the field empty, counted only where the form asks the question (SLDP dose among patients given SLDP, CEM consent at CEM sites, ACT type among Pf-positive patients).", t,
       "Fields above 5% missing should be raised with the facilities concerned.")
}, error = function(e) pending("o7_miss", "O7 Data quality", "Missing values", "Percent missing.", "No records yet."))

analysis_out <- unname(A)
