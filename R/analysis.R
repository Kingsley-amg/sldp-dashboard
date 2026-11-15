# =============================================================================
# SLDP pilot: statistical analysis for the dashboard
# Author: Kingsley Amegah, Data Scientist / Health Data Specialist
#
# Follows the objectives and analysis plan of the CEM protocol:
#   Obj 1  Incidence and severity of adverse events after SLDP
#   Obj 2  Change in haemoglobin after treatment (Day 0 to Day 3)
#   Obj 3  Safety in subgroups (children, anaemic, possible G6PD deficiency)
#   Obj 4  Risk factors for adverse events and for a fall in Hb
#   Signal detection, reporting timeliness and retention (programme quality)
# Sourced after R/pipeline.R. Every result is an aggregate (estimate, CI,
# p-value, count). Each block is wrapped so that thin early data skips a test
# rather than stopping the build; the dashboard then says what is still needed.
# =============================================================================
suppressPackageStartupMessages(library(survival))

fmt_p   <- function(p) ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
pp      <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "p < 0.001", sprintf("p = %.3f", p)))
fmt_ci  <- function(lo, hi, d = 2) sprintf(paste0("%.", d, "f to %.", d, "f"), lo, hi)
prop_ci <- function(x, n) { if (is.na(n) || n == 0) return(c(NA, NA, NA)); b <- binom.test(x, n); 100 * c(x / n, b$conf.int) }
prop_txt <- function(x, n) { v <- prop_ci(x, n); if (is.na(v[1])) "No data" else sprintf("%.1f%% (%.1f to %.1f)", v[1], v[2], v[3]) }
rate_ci <- function(x, pt, per = 1000) { if (is.na(x) || pt <= 0) return(c(NA, NA, NA)); ci <- poisson.test(x, pt)$conf.int; per * c(x / pt, ci) }
rate_txt <- function(x, pt, per = 1000) { v <- rate_ci(x, pt, per); if (is.na(v[1])) "No data" else sprintf("%.2f (%.2f to %.2f)", v[1], v[2], v[3]) }
tbl <- function(df) list(columns = names(df), rows = unname(lapply(seq_len(nrow(df)), function(i) unname(as.list(df[i, , drop = FALSE])))))
pending <- function(id, objective, title, method, need) list(id = id, objective = objective, title = title, method = method, status = "pending", note = need)
done <- function(id, objective, title, method, table, finding, chart = NULL)
  list(id = id, objective = objective, title = title, method = method, status = "ok", table = tbl(table),
       finding = gsub("p = <0.001", "p < 0.001", finding, fixed = TRUE), chart = chart)
safely <- function(id, objective, title, method, need, expr) tryCatch(expr, error = function(e) {
  if (!identical(conditionMessage(e), "need")) log_msg("analysis ", id, ": ", conditionMessage(e))
  pending(id, objective, title, method, need) })
need <- function(ok) if (!isTRUE(ok)) stop("need")
forest <- function(t, label = "Odds ratio", null = 1)
  list(type = "forest", null = null, label = label,
       points = lapply(seq_len(nrow(t)), function(i) list(name = t$Factor[i], est = round(t$est[i], 3), lo = round(t$lo[i], 3), hi = round(t$hi[i], 3))))
# Test of association for a 2-level outcome across groups: Fisher's exact when small, chi-square otherwise
assoc_p <- function(g, y) { tab <- table(g, y); if (nrow(tab) < 2 || ncol(tab) < 2) return(NA)
  if (any(suppressWarnings(chisq.test(tab))$expected < 5)) fisher.test(tab, simulate.p.value = TRUE, B = 5000)$p.value else chisq.test(tab)$p.value }

OBJ1 <- "Obj 1 Incidence and severity"; OBJ2 <- "Obj 2 Haemoglobin change"; OBJ3 <- "Obj 3 Subgroups"; OBJ4 <- "Obj 4 Risk factors"
OBJ5 <- "Signal detection"; OBJ6 <- "Programme quality"

ch <- cohort |>
  mutate(sex = factor(sex, c("female", "male"), c("Female", "Male")),
         act = factor(act_type, c("al", "asaq", "ap", "dhap"), c("AL", "ASAQ", "AS-pyronaridine", "DHA-PPQ")),
         mgkg_band = cut(mg_per_kg, c(-Inf, 0.2, 0.3, 0.4, Inf), right = FALSE, labels = c("<0.20 mg/kg", "0.20-0.29", "0.30-0.39", "0.40+ mg/kg")),
         dose_basis_lbl = factor(dose_basis, c("sldp_table", "both", "act_table", "neither"), c("SLDP weight table", "Both tables agree", "ACT age/weight table", "Neither table")),
         hx_haem = factor(hx_haemolysis, c("no", "yes"), c("No", "Yes")),
         followed = person_days > 0)
A <- list()

# ============================================================================
# OBJECTIVE 1: incidence and severity of adverse events
# ============================================================================
A$o1_inc <- safely("o1_inc", OBJ1, "Incidence of adverse events after SLDP (CEM cohort)",
  "Cumulative incidence (participants with at least one AE / participants followed) with exact binomial 95% CI; incidence rate per 1,000 person-days with exact Poisson 95% CI.",
  "Runs once 20 or more CEM participants have follow-up.", {
  d <- ch |> filter(followed); need(nrow(d) >= 20)
  ev <- cem_events |> filter(patient_id %in% d$patient_id)
  pt <- sum(d$person_days)
  row <- function(lbl, xp, xe) tibble(Measure = lbl, Participants = xp, `Cumulative incidence (95% CI)` = prop_txt(xp, nrow(d)),
                                       Events = xe, `Rate per 1,000 person-days (95% CI)` = rate_txt(xe, pt))
  pids <- function(f) n_distinct(ev$patient_id[f])
  t <- bind_rows(row("Any adverse event", pids(rep(TRUE, nrow(ev))), nrow(ev)),
                 row("Mild", pids(ev$intensity == "mild"), sum(ev$intensity == "mild")),
                 row("Moderate", pids(ev$intensity == "moderate"), sum(ev$intensity == "moderate")),
                 row("Severe", pids(ev$intensity == "severe"), sum(ev$intensity == "severe")),
                 row("Serious (SAE)", pids(ev$serious), sum(ev$serious)),
                 row("Primaquine suspected", pids(ev$sus_sldp), sum(ev$sus_sldp)),
                 row("Signs of haemolysis (dark urine, jaundice or Hb drop)", sum(d$haemolysis), NA_integer_),
                 row("Hospitalised", pids(ev$hospitalised), sum(ev$hospitalised)),
                 row("Died", pids(ev$death), sum(ev$death)))
  t$`Rate per 1,000 person-days (95% CI)`[t$Measure == "Signs of haemolysis (dark urine, jaundice or Hb drop)"] <- NA
  ci <- prop_ci(n_distinct(ev$patient_id), nrow(d))
  done("o1_inc", OBJ1, "Incidence of adverse events after SLDP (CEM cohort)",
       sprintf("%d participants followed for %s person-days (median %.0f days each). Exact binomial and Poisson confidence intervals.",
               nrow(d), format(round(pt), big.mark = ","), median(d$person_days)),
       t, sprintf("%.1f%% of participants (95%% CI %.1f to %.1f) reported at least one adverse event; %d serious event(s) and %d death(s) so far.",
                  ci[1], ci[2], ci[3], sum(ev$serious), sum(ev$death)))
})

A$o1_signs <- safely("o1_signs", OBJ1, "Adverse events by sign or symptom", "Participants with each sign (one event can have several signs), exact 95% CI.",
  "Runs once there are adverse events.", {
  need(nrow(cem_events) > 0)
  d <- ch |> filter(followed)
  s <- cem_events |> filter(patient_id %in% d$patient_id) |> select(patient_id, signs, serious) |>
    tidyr::separate_rows(signs, sep = " ") |> filter(signs != "", !is.na(signs)) |>
    group_by(code = signs) |> summarise(Participants = n_distinct(patient_id), Events = n(), Serious = sum(serious), .groups = "drop") |>
    mutate(Sign = coalesce(lbl("cem", "adr_sign", code), code), `% of cohort (95% CI)` = purrr::map_chr(Participants, prop_txt, n = nrow(d))) |>
    arrange(desc(Participants))
  need(nrow(s) > 0)
  done("o1_signs", OBJ1, "Adverse events by sign or symptom", sprintf("Denominator: %d CEM participants followed.", nrow(d)),
       s |> select(Sign, Participants, Events, Serious, `% of cohort (95% CI)`),
       sprintf("Most frequent: %s. Signs of haemolysis (dark urine, jaundice, Hb drop) are tracked separately in Objective 2.", paste(head(s$Sign, 3), collapse = ", ")),
       chart = list(type = "bar", label = "Participants", x = s$Sign, y = s$Participants))
})

A$o1_time <- safely("o1_time", OBJ1, "Time to first adverse event (Kaplan-Meier)", "Kaplan-Meier estimate of the cumulative probability of a first AE over the 28 days after primaquine; censored at last contact or exit.",
  "Runs once 20 participants have follow-up and at least 3 have an AE.", {
  d <- ch |> filter(followed) |> mutate(time = pmax(if_else(any_ae, pmax(coalesce(ae_time, 0), 0), person_days), 0.01), status = as.integer(any_ae))
  need(nrow(d) >= 20 && sum(d$status) >= 3)
  f <- survfit(Surv(time, status) ~ 1, data = d); s <- summary(f, times = c(1, 3, 7, 14, 28), extend = TRUE)
  t <- tibble(Day = s$time, `At risk` = s$n.risk, `Cumulative AE probability` = sprintf("%.1f%%", 100 * (1 - s$surv)),
              `95% CI` = fmt_ci(100 * (1 - s$upper), 100 * (1 - s$lower), 1))
  done("o1_time", OBJ1, "Time to first adverse event (Kaplan-Meier)", sprintf("%d participants, %d with a first AE.", nrow(d), sum(d$status)), t,
       sprintf("By Day 3, %s of participants had a first adverse event; by Day 28, %s. Most events start early, in line with the primaquine exposure window.",
               t$`Cumulative AE probability`[2], t$`Cumulative AE probability`[5]),
       chart = list(type = "km", label = "Cumulative probability of a first AE (%)", x = f$time, y = round(100 * (1 - f$surv), 2),
                    lo = round(100 * (1 - f$upper), 2), hi = round(100 * (1 - f$lower), 2)))
})

# ============================================================================
# OBJECTIVE 2: haemoglobin change Day 0 to Day 3
# ============================================================================
hbd <- ch |> filter(!is.na(hb_d0), !is.na(hb_d3))
A$o2_hb <- safely("o2_hb", OBJ2, "Change in haemoglobin from Day 0 to Day 3",
  "Mean change with 95% CI, paired t-test and Wilcoxon signed-rank test; proportion with a fall of 2 g/dL or more (exact CI).",
  "Runs once 10 participants have both Day 0 and Day 3 Hb.", {
  need(nrow(hbd) >= 10)
  tt <- t.test(hbd$hb_d3, hbd$hb_d0, paired = TRUE); wx <- suppressWarnings(wilcox.test(hbd$hb_d3, hbd$hb_d0, paired = TRUE))
  nd <- sum(hbd$hb_drop); g3 <- sum(hbd$hb_change <= -3)
  t <- tibble(Measure = c("Participants with paired Hb", "Mean Hb Day 0 (SD)", "Mean Hb Day 3 (SD)", "Mean change (95% CI)", "Median change (IQR)",
                          "Paired t-test", "Wilcoxon signed-rank test", "Fall of 2 g/dL or more (95% CI)", "Fall of 3 g/dL or more (95% CI)", "Day 3 Hb below 7 g/dL"),
              Value = c(nrow(hbd), sprintf("%.2f (%.2f)", mean(hbd$hb_d0), sd(hbd$hb_d0)), sprintf("%.2f (%.2f)", mean(hbd$hb_d3), sd(hbd$hb_d3)),
                        sprintf("%.2f g/dL (%s)", tt$estimate, fmt_ci(tt$conf.int[1], tt$conf.int[2])),
                        sprintf("%.1f (%.1f to %.1f)", median(hbd$hb_change), quantile(hbd$hb_change, .25), quantile(hbd$hb_change, .75)),
                        sprintf("t = %.2f, %s", tt$statistic, pp(tt$p.value)), pp(wx$p.value),
                        sprintf("%d (%s)", nd, prop_txt(nd, nrow(hbd))), sprintf("%d (%s)", g3, prop_txt(g3, nrow(hbd))), as.character(sum(hbd$hb_d3 < 7))))
  br <- seq(floor(min(hbd$hb_change)) - 0.5, ceiling(max(hbd$hb_change)) + 0.5, by = 0.5)
  h <- cut(hbd$hb_change, br, right = TRUE)   # bins (a, b]: a fall of exactly 2.0 sits in the red bin
  done("o2_hb", OBJ2, "Change in haemoglobin from Day 0 to Day 3", sprintf("%d participants with Hb measured on Day 0 and Day 3.", nrow(hbd)), t,
       sprintf("Hb changed by a mean of %.2f g/dL (95%% CI %s; paired t-test %s). %d participant(s) (%s) had a fall of 2 g/dL or more, the protocol safety threshold.",
               tt$estimate, fmt_ci(tt$conf.int[1], tt$conf.int[2]), pp(tt$p.value), nd, sprintf("%.1f%%", 100 * nd / nrow(hbd))),
       chart = list(type = "hist", label = "Participants", x = head(br, -1) + 0.25, y = as.integer(table(h)), ref = -HB_DROP_ALERT, xlab = "Hb change Day 0 to Day 3 (g/dL)"))
})

A$o2_model <- safely("o2_model", OBJ2, "What predicts the change in Hb? (linear regression)",
  "Multiple linear regression of Hb change (Day 3 minus Day 0) on age group, sex, baseline Hb, primaquine mg/kg and G6PD status.",
  "Runs once 30 participants have paired Hb.", {
  d <- hbd |> filter(!is.na(age_group), !is.na(sex), !is.na(mg_per_kg)) |> mutate(age_group = droplevels(age_group),
       g6 = droplevels(factor(ifelse(g6pd_grp %in% "Deficient", "Deficient", "Normal / unknown"), c("Normal / unknown", "Deficient"))))
  need(nrow(d) >= 30)
  d$mgkg10 <- d$mg_per_kg * 10
  fml <- hb_change ~ age_group + sex + hb_d0 + mgkg10
  if (n_distinct(d$g6) > 1 && sum(d$g6 == "Deficient") >= 2) fml <- update(fml, . ~ . + g6)
  m <- lm(fml, data = d); co <- summary(m)$coefficients; ci <- confint(m)
  lab <- c("age_group5-14" = "Age 5-14 (vs under 5)", "age_group15 and over" = "Age 15+ (vs under 5)", sexMale = "Male (vs female)",
           hb_d0 = "Baseline Hb (per g/dL)", mgkg10 = "Primaquine dose (per 0.1 mg/kg)", g6Deficient = "G6PD deficient (vs normal/unknown)")
  t <- tibble(Factor = unname(coalesce(lab[rownames(co)], rownames(co))), est = co[, 1], lo = ci[, 1], hi = ci[, 2], p = co[, 4])[-1, ]
  out <- t |> transmute(Factor, `Difference in Hb change (g/dL)` = round(est, 2), `95% CI` = fmt_ci(lo, hi), `p-value` = fmt_p(p))
  sig <- t$Factor[t$p < 0.05]
  done("o2_model", OBJ2, "What predicts the change in Hb? (linear regression)", sprintf("%d participants; R-squared %.2f.", nrow(d), summary(m)$r.squared), out,
       if (length(sig)) paste0("Associated with the size of the Hb change at the 5% level: ", paste(sig, collapse = "; "), ".") else "No factor is significantly associated with the Hb change at the 5% level.",
       chart = forest(t, "Difference in Hb change (g/dL)", 0))
})

# ============================================================================
# OBJECTIVE 3: safety in subgroups (age, baseline Hb, G6PD, sex, ACT, dose)
# ============================================================================
A$o3_sub <- safely("o3_sub", OBJ3, "Adverse events and Hb fall by subgroup",
  "Per subgroup: participants, % with any AE, % with a serious AE, mean Hb change and % with a fall of 2 g/dL or more (exact CIs). p-values: chi-square or Fisher's exact test (AE) and Kruskal-Wallis test (Hb change).",
  "Runs once 20 CEM participants have follow-up.", {
  d <- ch |> filter(followed); need(nrow(d) >= 20)
  one <- function(var, nm) {
    dd <- d |> mutate(g = as.character(.data[[var]])) |> filter(!is.na(g))
    if (n_distinct(dd$g) < 2) return(NULL)
    p_ae <- assoc_p(dd$g, dd$any_ae)
    hb <- dd |> filter(!is.na(hb_change))
    p_hb <- if (n_distinct(hb$g) > 1 && nrow(hb) >= 10) kruskal.test(hb_change ~ g, data = hb)$p.value else NA
    dd |> group_by(Level = g) |>
      summarise(Participants = n(), `Any AE` = prop_txt(sum(any_ae), n()), `Serious AE` = prop_txt(sum(any_serious), n()),
                `Mean Hb change` = if (sum(!is.na(hb_change))) sprintf("%.2f (n=%d)", mean(hb_change, na.rm = TRUE), sum(!is.na(hb_change))) else "No data",
                `Hb fall of 2+` = prop_txt(sum(hb_drop), sum(!is.na(hb_change))), .groups = "drop") |>
      mutate(Subgroup = nm, `p (AE)` = fmt_p(p_ae), `p (Hb)` = fmt_p(p_hb), .before = 1)
  }
  t <- bind_rows(one("age_group", "Age group"), one("sex", "Sex"), one("hb_grade_d0", "Baseline Hb (anaemia grade)"), one("g6pd_grp", "G6PD status"),
                 one("hx_haem", "History of haemolysis"), one("act", "ACT given"), one("mgkg_band", "Primaquine dose"), one("dose_basis_lbl", "Dosing table used"),
                 one("district_lbl", "District"))
  need(nrow(t) > 0)
  t$`p (AE)`[duplicated(t$Subgroup)] <- ""; t$`p (Hb)`[duplicated(t$Subgroup)] <- ""
  sig <- unique(t$Subgroup[suppressWarnings(as.numeric(sub("<", "", t$`p (AE)`))) < 0.05 & t$`p (AE)` != ""])
  done("o3_sub", OBJ3, "Adverse events and Hb fall by subgroup", sprintf("%d CEM participants followed. Protocol subgroups: children (under 5, 5-14), anaemic at baseline, possible G6PD deficiency.", nrow(d)), t,
       if (length(sig)) paste0("AE frequency differs significantly (p < 0.05) by: ", paste(sig, collapse = "; "), ". Check the intervals before acting: early numbers are small.")
       else "No subgroup shows a statistically significant difference in AE frequency so far. Intervals are wide in the early pilot.")
})

A$o3_child <- safely("o3_child", OBJ3, "Children under 5 compared with older participants",
  "Risk ratio for any AE and for a fall in Hb of 2 g/dL or more, with 95% CI (log method) and Fisher's exact test.",
  "Runs once there are at least 5 children under 5 and 5 older participants.", {
  d <- ch |> filter(followed, !is.na(age_group)) |> mutate(u5 = age_group == "Under 5")
  need(sum(d$u5) >= 5 && sum(!d$u5) >= 5)
  rr <- function(y, g) { a <- sum(y & g); b <- sum(g); c <- sum(y & !g); e <- sum(!g)
    if (a == 0 || c == 0) return(c(NA, NA, NA, fisher.test(table(g, y))$p.value))
    r <- (a / b) / (c / e); se <- sqrt(1 / a - 1 / b + 1 / c - 1 / e); c(r, exp(log(r) - 1.96 * se), exp(log(r) + 1.96 * se), fisher.test(table(g, y))$p.value) }
  h <- d |> filter(!is.na(hb_change))
  r1 <- rr(d$any_ae, d$u5); r2 <- if (nrow(h) >= 10 && sum(h$u5) >= 3) rr(h$hb_drop, h$u5) else c(NA, NA, NA, NA)
  t <- tibble(Outcome = c("Any adverse event", "Hb fall of 2 g/dL or more"),
              `Under 5` = c(prop_txt(sum(d$any_ae & d$u5), sum(d$u5)), prop_txt(sum(h$hb_drop & h$u5), sum(h$u5))),
              `5 and over` = c(prop_txt(sum(d$any_ae & !d$u5), sum(!d$u5)), prop_txt(sum(h$hb_drop & !h$u5), sum(!h$u5))),
              `Risk ratio (95% CI)` = c(ifelse(is.na(r1[1]), "Not estimable", sprintf("%.2f (%s)", r1[1], fmt_ci(r1[2], r1[3]))),
                                        ifelse(is.na(r2[1]), "Not estimable", sprintf("%.2f (%s)", r2[1], fmt_ci(r2[2], r2[3])))),
              `p (Fisher)` = fmt_p(c(r1[4], r2[4])))
  done("o3_child", OBJ3, "Children under 5 compared with older participants", sprintf("%d under 5, %d aged 5 and over.", sum(d$u5), sum(!d$u5)), t,
       "Children under 5 get the smallest primaquine doses and are a protocol priority subgroup; a risk ratio clearly above 1 would need review by the safety team.")
})

# ============================================================================
# OBJECTIVE 4: risk factors
# ============================================================================
or_table <- function(m) { co <- summary(m)$coefficients
  lab <- c("age_group5-14" = "Age 5-14 (vs under 5)", "age_group15 and over" = "Age 15+ (vs under 5)", sexMale = "Male (vs female)",
           anaemic_d0TRUE = "Anaemic at Day 0 (vs not)", mg_per_kg = "Primaquine dose (per 0.1 mg/kg)", g6Deficient = "G6PD deficient (vs normal/unknown)",
           actOther = "ACT other than AL (vs AL)", hx = "History of haemolysis", hb_d0 = "Baseline Hb (per g/dL)")
  b <- co[, 1]; se <- co[, 2]; sc <- ifelse(rownames(co) == "mg_per_kg", 0.1, 1)
  ok <- se < 5   # a huge standard error means (quasi-)separation: too few events in that group to estimate
  tibble(Factor = unname(coalesce(lab[rownames(co)], rownames(co))), est = ifelse(ok, exp(b * sc), NA), lo = ifelse(ok, exp((b - 1.96 * se) * sc), NA),
         hi = ifelse(ok, exp((b + 1.96 * se) * sc), NA), p = ifelse(ok, co[, 4], NA))[-1, ] }
or_out <- function(t) t |> transmute(Factor, `Odds ratio` = ifelse(is.na(est), "Not estimable (too few events)", sprintf("%.2f", est)),
                                     `95% CI` = ifelse(is.na(est), "", fmt_ci(lo, hi)), `p-value` = fmt_p(p))
rf_data <- ch |> filter(followed, !is.na(age_group), !is.na(sex)) |>
  mutate(age_group = droplevels(age_group), act = factor(ifelse(act %in% "AL", "AL", "Other"), c("AL", "Other")),
         g6 = factor(ifelse(g6pd_grp %in% "Deficient", "Deficient", "Normal / unknown"), c("Normal / unknown", "Deficient")))
fit_logit <- function(d, y) {
  terms <- c("age_group", "sex", "anaemic_d0")
  if (sum(!is.na(d$mg_per_kg)) == nrow(d)) terms <- c(terms, "mg_per_kg")
  if (n_distinct(d$act) > 1 && min(table(d$act)) >= 5) terms <- c(terms, "act")
  if (sum(d$g6 == "Deficient") >= 3) terms <- c(terms, "g6")
  terms <- terms[vapply(terms, function(v) n_distinct(d[[v]]) > 1, logical(1))]
  glm(reformulate(terms, y), data = d, family = binomial) }
A$o4_ae <- safely("o4_ae", OBJ4, "Risk factors for any adverse event (logistic regression)",
  "Multivariable logistic regression: any AE during follow-up on age group, sex, anaemia at Day 0, primaquine mg/kg, ACT type and G6PD status. Odds ratios with Wald 95% CI.",
  "Runs once there are 40 participants followed and 10 with an AE (about 10 events per factor are needed for stable estimates).", {
  d <- rf_data; need(nrow(d) >= 40 && sum(d$any_ae) >= 10)
  m <- suppressWarnings(fit_logit(d, "any_ae")); t <- or_table(m)
  sig <- t$Factor[!is.na(t$p) & t$p < 0.05]
  done("o4_ae", OBJ4, "Risk factors for any adverse event (logistic regression)", sprintf("%d participants, %d with an AE.", nrow(d), sum(d$any_ae)),
       or_out(t),
       if (length(sig)) paste0("Independently associated with an AE (p < 0.05): ", paste(sig, collapse = "; "), ".") else "No factor is independently associated with an AE at the 5% level so far.",
       chart = forest(t |> filter(!is.na(est))))
})
A$o4_hb <- safely("o4_hb", OBJ4, "Risk factors for a fall in Hb of 2 g/dL or more (logistic regression)",
  "Logistic regression with the same factors, among participants with paired Hb. Exact logistic methods are preferable while events are rare.",
  "Runs once 40 participants have paired Hb and at least 5 had a fall of 2 g/dL or more.", {
  d <- rf_data |> filter(!is.na(hb_change)); need(nrow(d) >= 40 && sum(d$hb_drop) >= 5)
  m <- suppressWarnings(fit_logit(d, "hb_drop")); t <- or_table(m)
  sig <- t$Factor[!is.na(t$p) & t$p < 0.05]
  done("o4_hb", OBJ4, "Risk factors for a fall in Hb of 2 g/dL or more (logistic regression)", sprintf("%d participants, %d with a fall of 2 g/dL or more.", nrow(d), sum(d$hb_drop)),
       or_out(t),
       if (length(sig)) paste0("Independently associated with a fall of 2 g/dL or more (p < 0.05): ", paste(sig, collapse = "; "), ".") else "No factor is independently associated with a fall of 2 g/dL or more at the 5% level so far.",
       chart = forest(t |> filter(!is.na(est))))
})
A$o4_cox <- safely("o4_cox", OBJ4, "Time to first AE by age group (Cox regression)",
  "Cox proportional hazards model for time to first AE (age group, sex, anaemia at Day 0); hazard ratios with 95% CI.",
  "Runs once 40 participants are followed and 10 have an AE.", {
  d <- rf_data |> mutate(time = pmax(if_else(any_ae, pmax(coalesce(ae_time, 0), 0), person_days), 0.01), status = as.integer(any_ae))
  need(nrow(d) >= 40 && sum(d$status) >= 10)
  m <- coxph(Surv(time, status) ~ age_group + sex + anaemic_d0, data = d, ties = "efron"); s <- summary(m)
  lab <- c("age_group5-14" = "Age 5-14 (vs under 5)", "age_group15 and over" = "Age 15+ (vs under 5)", sexMale = "Male (vs female)", anaemic_d0TRUE = "Anaemic at Day 0 (vs not)")
  t <- tibble(Factor = unname(coalesce(lab[rownames(s$conf.int)], rownames(s$conf.int))), est = s$conf.int[, 1], lo = s$conf.int[, 3], hi = s$conf.int[, 4], p = s$coefficients[, 5])
  ph <- tryCatch(cox.zph(m)$table["GLOBAL", 3], error = function(e) NA)
  done("o4_cox", OBJ4, "Time to first AE by age group (Cox regression)", sprintf("%d participants, %d events. Proportional hazards test (global) p = %s.", nrow(d), sum(d$status), fmt_p(ph)),
       t |> transmute(Factor, `Hazard ratio` = round(est, 2), `95% CI` = fmt_ci(lo, hi), `p-value` = fmt_p(p)),
       "Hazard ratios above 1 mean an earlier and more frequent first AE than the reference group.", chart = forest(t, "Hazard ratio"))
})

# ============================================================================
# SIGNAL DETECTION: proportional reporting ratio (both reporting routes)
# ============================================================================
A$sig_prr <- safely("sig_prr", OBJ5, "Signal check: signs reported more often when primaquine is suspected",
  "Proportional reporting ratio (PRR) over all events (CEM and Form 3): share of events with a sign when primaquine is a suspected medicine, against events where it is not. Signal if PRR >= 2, chi-square >= 4 and 3 or more cases (Evans criteria). Causality is assessed separately by the FDA Technical Advisory Committee (WHO-UMC scale).",
  "Runs once there are 10 events with primaquine suspected and 5 without.", {
  ev <- bind_rows(cem_events, adr_events) |> select(event_id, signs, sus_sldp)
  need(sum(ev$sus_sldp) >= 10 && sum(!ev$sus_sldp) >= 5)
  long <- ev |> tidyr::separate_rows(signs, sep = " ") |> filter(signs != "", !is.na(signs)) |> distinct(event_id, signs, sus_sldp)
  nA <- sum(ev$sus_sldp); nB <- sum(!ev$sus_sldp)
  t <- long |> group_by(code = signs) |> summarise(a = sum(sus_sldp), c = sum(!sus_sldp), .groups = "drop") |>
    mutate(b = nA - a, d = nB - c, prr = (a / nA) / ((c + 0.5) / (nB + 0.5)),
           se = sqrt(1 / pmax(a, 0.5) - 1 / nA + 1 / (c + 0.5) - 1 / (nB + 0.5)), lo = prr * exp(-1.96 * se), hi = prr * exp(1.96 * se),
           chi = purrr::pmap_dbl(list(a, b, c, d), function(a, b, c, d) suppressWarnings(chisq.test(matrix(c(a, b, c, d), 2), correct = TRUE)$statistic)),
           signal = prr >= 2 & chi >= 4 & a >= 3, Sign = coalesce(lbl("cem", "adr_sign", code), code)) |> arrange(desc(prr))
  out <- t |> transmute(Sign, `Primaquine suspected` = a, `Not suspected` = c, PRR = round(prr, 2), `95% CI` = fmt_ci(lo, hi), `Chi-square` = round(chi, 1),
                        Signal = if_else(signal, "Yes: review", "No"))
  done("sig_prr", OBJ5, "Signal check: signs reported more often when primaquine is suspected", sprintf("%d events with primaquine suspected, %d without.", nA, nB), out,
       if (any(t$signal)) paste0("Possible signal(s) for review by the safety team: ", paste(t$Sign[t$signal], collapse = ", "), ".") else "No sign meets the signal criteria so far.")
})

# ============================================================================
# PROGRAMME QUALITY: retention, contact success, reporting timeliness, Day 3 parasitology
# ============================================================================
A$q_retention <- safely("q_retention", OBJ6, "Retention in CEM follow-up (Kaplan-Meier)",
  "Kaplan-Meier estimate of staying in follow-up to Day 28; exits are withdrawal, loss to follow-up (2 missed scheduled contacts) and death. Participants still in their window are censored.",
  "Runs once 20 participants have follow-up.", {
  vs <- visit_status |> group_by(patient_id) |> summarise(ltfu = any(n_missed >= LTFU_MISSED), .groups = "drop")
  d <- ch |> filter(followed) |> left_join(vs, by = "patient_id") |>
    mutate(exit = (!is.na(exit_status) & exit_status %in% c("withdrawn", "lost", "died")) | coalesce(ltfu, FALSE),
           time = pmax(as.numeric(pmin(coalesce(exit_date, last_contact, day0 + 28), day0 + 28, TODAY, na.rm = TRUE) - day0), 0.01),
           time = if_else(coalesce(exit_status, "") == "completed", 28, time))
  need(nrow(d) >= 20)
  f <- survfit(Surv(time, as.integer(exit)) ~ 1, data = d); s <- summary(f, times = c(3, 7, 14, 28), extend = TRUE)
  t <- tibble(Day = s$time, `Still in follow-up` = s$n.risk, Retention = sprintf("%.1f%%", 100 * s$surv), `95% CI` = fmt_ci(100 * s$lower, 100 * s$upper, 1))
  done("q_retention", OBJ6, "Retention in CEM follow-up (Kaplan-Meier)", sprintf("%d participants, %d exits (withdrawn, lost or died).", nrow(d), sum(d$exit)), t,
       sprintf("Estimated retention to Day 28: %s. The protocol defines loss to follow-up as 2 unsuccessful scheduled follow-ups.", t$Retention[4]),
       chart = list(type = "km", label = "Retention (%)", x = f$time, y = round(100 * f$surv, 2), lo = round(100 * f$lower, 2), hi = round(100 * f$upper, 2)))
})
A$q_timely <- safely("q_timely", OBJ6, "Reporting timeliness against the FDA deadlines",
  "Days from onset to the CEM focal person being informed (Form 2) or to the report date (Form 3). Target: serious within 7 days, others within 28 days.",
  "Runs once there are adverse events with dates.", {
  ev <- bind_rows(cem_events, adr_events) |> filter(!is.na(report_lag)); need(nrow(ev) > 0)
  t <- ev |> mutate(Type = if_else(serious, "Serious (7-day target)", "Non-serious (28-day target)")) |> group_by(Source = source, Type) |>
    summarise(Events = n(), `Median days (IQR)` = sprintf("%.0f (%.0f to %.0f)", median(report_lag), quantile(report_lag, .25), quantile(report_lag, .75)),
              `On time (95% CI)` = prop_txt(sum(on_time), n()), Late = sum(!on_time), .groups = "drop")
  done("q_timely", OBJ6, "Reporting timeliness against the FDA deadlines", sprintf("%d events with an onset and a report date.", nrow(ev)), t,
       sprintf("%d of %d events (%.0f%%) were reported within the deadline.", sum(ev$on_time), nrow(ev), 100 * mean(ev$on_time)))
})
A$q_d3par <- safely("q_d3par", OBJ6, "Day 3 parasitology (repeat test)", "Share of Day 3 repeat tests still positive for P. falciparum, with exact 95% CI.",
  "Runs once there are Day 3 repeat tests.", {
  d <- visits |> filter(visit == "d3", done, g_repeat_test %in% "yes"); need(nrow(d) >= 5)
  pos <- sum(d$g_rdt %in% "pf_pos" | d$g_species %in% c("pf", "mixed"), na.rm = TRUE)
  t <- tibble(Measure = c("Day 3 visits", "Repeat test done", "Still P. falciparum positive (95% CI)"),
              Value = c(sum(visits$visit == "d3" & visits$done), nrow(d), sprintf("%d (%s)", pos, prop_txt(pos, nrow(d)))))
  done("q_d3par", OBJ6, "Day 3 parasitology (repeat test)", "Among Day 3 visits where the test was repeated.", t,
       "Positivity on Day 3 reflects ACT response; persistent positivity should be followed up clinically.")
})

analysis_out <- unname(A)
log_msg("analysis: ", sum(vapply(analysis_out, function(x) x$status == "ok", logical(1))), " of ", length(analysis_out), " analyses ran")
