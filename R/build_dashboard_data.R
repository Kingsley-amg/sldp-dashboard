# =============================================================================
# SLDP pilot, Greater Accra: build the dashboard data
# Author: Kingsley Amegah, Data Scientist / Health Data Specialist
#
# Runs the shared pipeline (R/pipeline.R) and the analysis (R/analysis.R), then writes
#   docs/data/dashboard.json   PUBLIC. Aggregates only: counts by facility, week and
#                              category. No names, phone numbers, Patient IDs, Event IDs
#                              or free text. A safety net refuses to publish otherwise.
#   <SLDP_RAW_DIR>/queue.json  PRIVATE. The patient follow-up queue (names, phones).
#                              Never written into docs/: tools/encrypt_queue.mjs encrypts
#                              it with the QUEUE_PASSWORD secret into docs/data/queue.enc.json.
#
# Run locally:  Rscript R/build_dashboard_data.R                     (needs KOBO_TOKEN)
#               SLDP_USE_CACHE=true Rscript R/build_dashboard_data.R (reuses the last pull)
# =============================================================================
params <- list(
  use_cache   = tolower(Sys.getenv("SLDP_USE_CACHE", "false")) == "true",
  drop_test   = tolower(Sys.getenv("SLDP_INCLUDE_TEST", "false")) != "true",   # repo variable SLDP_INCLUDE_TEST=true shows training records
  pilot_start = if (nzchar(Sys.getenv("SLDP_PILOT_START"))) Sys.getenv("SLDP_PILOT_START") else "2026-10-01",   # repo variable SLDP_PILOT_START
  kobo_server = Sys.getenv("KOBO_SERVER", "https://eu.kobotoolbox.org"))
OUT_JSON <- Sys.getenv("SLDP_OUT_JSON", file.path("docs", "data", "dashboard.json"))
IS_DEMO  <- tolower(Sys.getenv("SLDP_DEMO", "false")) == "true"   # TRUE only for the synthetic-data preview

source(file.path("R", "pipeline.R"))
source(file.path("R", "analysis.R"))
QUEUE_JSON <- Sys.getenv("SLDP_QUEUE_JSON", file.path(DIR_RAW, "queue.json"))

wk   <- function(d) format(lubridate::floor_date(d, "week", week_start = 1), "%Y-%m-%d")
cols <- function(df) if (is.null(df) || nrow(df) == 0) list() else as.list(as.data.frame(df, stringsAsFactors = FALSE))
chr  <- function(x) as.character(x)
known_fac <- function(f) as.character(ifelse(!is.na(f) & f %in% fac_map$facility, f, "unknown"))

# ---- Facilities (all pilot sites, with targets) ----------------------------------
facilities <- fac_map |> transmute(code = facility, label = facility_lbl, district = coalesce(district_lbl, "Unknown"), cem_site,
                                   target_total, target_month) |>
  bind_rows(tibble(code = "unknown", label = "Facility not recorded", district = "Unknown", cem_site = FALSE, target_total = NA_real_, target_month = NA_real_))

# ============================================================================
# 1. Enrolment, screening and treatment cascade (Form 1)
# ============================================================================
r <- reg |> mutate(f = known_fac(facility), w = wk(registration_date), age = coalesce(chr(age_group), "Not recorded"), sex = coalesce(sex, "unknown"))
reg_cube <- r |> mutate(el_ = eligible, pf_ = pf) |> group_by(f, w, age, sex) |>
  summarise(registered = n(), tested = sum(test_done %in% "yes"), pf = sum(pf_), eligible = sum(el_), act = sum(el_ & act_given),
            accepted = sum(el_ & act_given & pq_accepted %in% "yes"), given = sum(sldp_given), cem_offered = sum(sldp_given & cem_site),
            enrolled = sum(cem_in), dose_ok = sum(dose_ok), dose_flag = sum(sldp_given & dose_flag %in% "yes"), dot = sum(sldp_given & dot %in% "yes"),
            counselled = sum(sldp_given & counselled %in% "yes"), .groups = "drop")
not_elig <- r |> filter(!eligible) |> count(f, w, reason = not_elig_reason, name = "n")
refusal  <- r |> filter(pq_accepted %in% "no") |> select(f, w, pq_not_given_reasons) |> tidyr::separate_rows(pq_not_given_reasons, sep = " ") |>
  mutate(reason = coalesce(lbl("registration", "refusal", pq_not_given_reasons), "Not recorded")) |> count(f, w, reason, name = "n")
not_disp <- r |> filter(pq_given %in% "no" | (eligible & !act_given)) |>
  mutate(reason = if_else(pq_given %in% "no", paste("Primaquine:", coalesce(lbl("registration", "pq_nd", pq_nd_reason), "not recorded")),
                          paste("ACT:", coalesce(lbl("registration", "act_nd", act_not_dispensed_reason), "not recorded")))) |> count(f, w, reason, name = "n")
tests <- r |> filter(test_done %in% "yes") |> count(f, w, test = coalesce(lbl("registration", "test", test_type), "Not recorded"), pf, name = "n")
act_cube <- r |> filter(act_given) |> count(f, w, act = coalesce(lbl("registration", "act", act_type), "Not recorded"), name = "n")
dose <- r |> filter(sldp_given) |>
  mutate(basis = recode(coalesce(dose_basis, "none"), sldp_table = "SLDP weight table", both = "Both tables agree", act_table = "ACT table",
                        neither = "Neither table", none = "Not recorded"),
         band = coalesce(chr(weight_band), "Not recorded"), dose = coalesce(lbl("registration", "pq_dose", pq_dose), "Not recorded"),
         mgkg = pmin(floor(mg_per_kg / 0.05) * 0.05, 0.6))
dose_basis <- dose |> count(f, w, basis, name = "n")
dose_band  <- dose |> count(f, band, dose, name = "n")
mgkg_hist  <- dose |> filter(!is.na(mgkg)) |> count(f, bin = round(mgkg, 2), name = "n")
enrol_day  <- r |> filter(cem_in) |> count(f, d = format(registration_date), name = "n")   # cumulative enrolment against the target

# ============================================================================
# 2. Follow-up (Form 2 schedule)
# ============================================================================
vs <- visit_status |> mutate(f = known_fac(facility), w = wk(day0))
fu_cube <- vs |> count(f, w, visit, status, name = "n")
fu_time <- vs |> filter(done, !is.na(days_late_done)) |>
  count(f, visit, timing = case_when(days_late_done < -1 ~ "Early", days_late_done <= grace ~ "On time", TRUE ~ "Late"), name = "n")
overdue_age <- vs |> filter(status %in% c("Overdue", "Missed")) |>
  count(f, visit, age = chr(cut(days_overdue, c(-Inf, 3, 7, 14, Inf), labels = c("1-3 days", "4-7 days", "8-14 days", "Over 14 days"))), name = "n")
calls <- visits |> filter(phone, visit != "unscheduled") |> mutate(f = known_fac(facility), w = wk(visit_date)) |>
  count(f, w, visit, result = if_else(h_success %in% "yes", "Reached", coalesce(lbl("cem", "callfail", h_fail_reason), "Not reached")), name = "n")
attempts_needed <- visits |> filter(phone, visit %in% c("d7", "d14", "d28")) |> group_by(patient_id, visit) |>
  summarise(f = known_fac(first(facility)), reached = any(done), tries = n(), .groups = "drop") |>
  count(f, visit, tries = pmin(tries, 4), reached, name = "n")
contacts_week <- visits |> mutate(f = known_fac(facility), w = wk(visit_date)) |> count(f, w, visit, done, name = "n")
ltfu_flag <- visit_status |> group_by(patient_id) |> summarise(ltfu = any(n_missed >= LTFU_MISSED), .groups = "drop")
exits_cube <- cohort |> left_join(ltfu_flag, by = "patient_id") |>
  mutate(f = known_fac(facility),
         status = case_when(exit_status %in% "died" ~ "Died", exit_status %in% "withdrawn" ~ "Withdrawn", exit_status %in% "lost" ~ "Lost to follow-up (closed)",
                            exit_status %in% "completed" ~ "Completed Day 28", coalesce(ltfu, FALSE) ~ "Meets LTFU definition (not closed)", TRUE ~ "In follow-up")) |>
  count(f, status, name = "n")
consent_cube <- reg |> filter(cem_in) |> left_join(d0v |> select(patient_id, consent_d0), by = "patient_id") |>
  count(f = known_fac(facility), consent = recode(coalesce(consent_d0, "none"), yes = "Consented", no = "Declined at Day 0", none = "No Day 0 form yet"), name = "n")

# ============================================================================
# 3. Safety (CEM events, Form 2) and spontaneous reports (Form 3)
# ============================================================================
ev_all <- bind_rows(cem_events, adr_events)
if (nrow(ev_all) == 0) ev_all <- tibble(event_id = character(), source = character(), patient_id = character(), facility = character(), onset = as.Date(character()),
                                        signs = character(), intensity = character(), serious = logical(), outcome = character(), criteria = character(),
                                        sus_sldp = logical(), sus_act = logical(), resolved = logical(), hospitalised = logical(), death = logical(),
                                        report_lag = numeric(), on_time = logical(), last_record = as.Date(character()))
ev_all <- ev_all |> mutate(f = known_fac(facility), w = wk(onset), src = if_else(source == "CEM (Form 2)", "cem", "spont"),
                           outcome = coalesce(lbl("cem", "outcome", outcome), outcome), intensity = stringr::str_to_sentence(intensity))
ev_cube <- ev_all |> count(f, w, src, intensity, serious, outcome, sus_sldp, sus_act, resolved, hospitalised, death, name = "n")
ev_signs <- ev_all |> select(f, src, serious, signs) |> tidyr::separate_rows(signs, sep = " ") |> filter(!is.na(signs), signs != "") |>
  count(f, src, serious, sign = coalesce(lbl("cem", "adr_sign", signs), signs), name = "n")
ev_crit <- ev_all |> filter(serious) |> select(f, src, criteria) |> tidyr::separate_rows(criteria, sep = " ") |> filter(!is.na(criteria), criteria != "") |>
  count(f, src, criterion = coalesce(lbl("cem", "sae_criteria", criteria), criteria), name = "n")
ev_timely <- ev_all |> filter(!is.na(report_lag)) |> count(f, src, serious, on_time, name = "n")
day0s <- bind_rows(cohort |> select(patient_id, day0), reg |> transmute(patient_id, day0 = pq_date)) |> filter(!is.na(day0)) |> distinct(patient_id, .keep_all = TRUE)
ev_onset <- ev_all |> left_join(day0s, by = "patient_id") |> mutate(day = pmin(pmax(as.numeric(onset - day0), 0), 28)) |> filter(!is.na(day)) |> count(f, src, day, name = "n")
ev_open <- ev_all |> filter(!resolved) |>
  count(f, src, serious, age = chr(cut(as.numeric(TODAY - last_record), c(-Inf, 7, 14, Inf), labels = c("Updated in last 7 days", "8-14 days ago", "Over 14 days ago"))), name = "n")
spont <- adr |> filter(report_type %in% "new") |>
  count(f = known_fac(facility), through = recode(coalesce(reported_through, "unknown"), cem_site_not_enrolled = "CEM site, not enrolled in CEM",
                                                  non_cem = "Non-CEM facility", unknown = "Not recorded"), name = "n")
# Safety cohort cube: lets the page recompute incidence and Hb by subgroup under any filter
safety_cube <- cohort |> mutate(f = known_fac(facility), age = coalesce(chr(age_group), "Not recorded"), hbg = coalesce(chr(hb_grade_d0), "No Day 0 Hb"),
                                g6 = coalesce(chr(g6pd_grp), "Not recorded"), sex = coalesce(sex, "unknown")) |>
  group_by(f, age, sex, hbg, g6) |>
  summarise(n = n(), pdays = round(sum(person_days), 1), ae = sum(any_ae), sae = sum(any_serious), haem = sum(haemolysis),
            hb_pairs = sum(!is.na(hb_change)), hb_drop = sum(hb_drop), hb_sum = round(sum(hb_change, na.rm = TRUE), 2), hosp = sum(hosp_any), .groups = "drop")
hb_hist <- cohort |> filter(!is.na(hb_change)) |> count(f = known_fac(facility), bin = pmax(pmin(ceiling(hb_change * 2) / 2, 3), -5), name = "n")
hb_grades <- cohort |> filter(!is.na(lab_grade_d0)) |>
  mutate(g3 = case_when(is.na(hb_d3) ~ "No Day 3 Hb", hb_d3 >= 10 ~ "10 g/dL or more", hb_d3 >= 8 ~ "8 to 9.9 g/dL", TRUE ~ "Below 8 g/dL")) |>
  count(f = known_fac(facility), d0 = chr(hb_grade_d0), d3 = g3, name = "n")
symptoms <- visits |> filter(done, visit != "d0") |>
  transmute(f = known_fac(facility), visit, dark_urine = yes(g_dark_urine) | yes(h_dark_urine) | yes(u_dark_urine), jaundice = yes(g_jaundice) | yes(h_jaundice) | yes(u_jaundice),
            pallor = yes(g_pallor) | yes(h_pallor) | yes(u_pallor), fatigue = yes(g_fatigue) | yes(h_fatigue) | yes(u_fatigue),
            hospitalised = yes(g_hospitalised) | yes(h_hospitalised) | yes(u_hospitalised)) |>
  group_by(f, visit) |> summarise(contacts = n(), across(dark_urine:hospitalised, sum), .groups = "drop")

# ============================================================================
# 4. Data quality
# ============================================================================
dq <- issue_log |> count(f = known_fac(facility), form, check, severity, name = "n")
subs_week <- bind_rows(reg |> transmute(form = "Form 1", f = facility, d = submission_date),
                       visits |> transmute(form = "Form 2", f = facility, d = submission_date),
                       adr |> transmute(form = "Form 3", f = facility, d = submission_date)) |>
  filter(!is.na(d)) |> count(f = known_fac(f), w = wk(d), form, name = "n")
entry_lag <- bind_rows(reg |> transmute(form = "Form 1", f = facility, lag = as.numeric(submission_date - registration_date)),
                       visits |> transmute(form = "Form 2", f = facility, lag = as.numeric(submission_date - visit_date))) |>
  filter(!is.na(lag)) |> count(f = known_fac(f), form, lag = chr(cut(lag, c(-Inf, 0, 1, 3, 7, Inf), labels = c("Same day", "1 day", "2-3 days", "4-7 days", "Over 7 days"))), name = "n")
fld <- function(df, form, vars) purrr::map_dfr(names(vars), function(v) tibble(form = form, f = known_fac(df$facility), field = vars[[v]], missing = is.na(df[[v]]))) |>
  group_by(form, f, field) |> summarise(n = n(), missing = sum(missing), .groups = "drop")
completeness <- bind_rows(fld(reg, "Form 1", c(weight = "Weight", phone = "Phone number", age_months = "Age")),
                          fld(reg |> filter(sldp_given), "Form 1", c(pq_date = "Primaquine date and time", pq_dose = "Primaquine dose")),
                          fld(reg |> filter(cem_in), "Form 1", c(alt_contact_phone = "Alternative contact phone")),
                          fld(visits |> filter(visit == "d0", done), "Form 2", c(f_hb = "Day 0 Hb", g6pd_status = "G6PD status")),
                          fld(visits |> filter(visit == "d3", done), "Form 2", c(g_hb = "Day 3 Hb")))
versions <- bind_rows(reg |> count(form = "Form 1", version = `__version__`), visits |> count(form = "Form 2", version = `__version__`),
                      adr |> count(form = "Form 3", version = `__version__`))

# ============================================================================
# 5. Queue: public counts, and the private list (names and phones)
# ============================================================================
queue_cube <- vs |> filter(status %in% c("Due now", "Overdue", "Upcoming (7 days)")) |> count(f, visit, status, name = "n")
blank_q <- function(d) d   # columns kept identical across the queue categories
call_back <- visit_status |> filter(!done, n_fail > 0, status %in% c("Due now", "Overdue")) |> transmute(patient_id, visit)
ltfu_ids <- visit_status |> filter(n_missed >= LTFU_MISSED, is.na(exit_status)) |> distinct(patient_id) |> pull()
q_vis <- visit_status |> filter(status %in% c("Due now", "Overdue", "Upcoming (7 days)"), !patient_id %in% ltfu_ids) |>
  left_join(call_back |> mutate(cb = TRUE), by = c("patient_id", "visit")) |>
  transmute(category = if_else(coalesce(cb, FALSE), paste(status, "(call back)"), status), patient_id, name = trimws(paste(coalesce(first_name, ""), coalesce(last_name, ""))),
            phone = phone_final, alt_name = alt_contact_name, alt_phone = alt_contact_phone, facility = facility_lbl, district = district_lbl,
            visit = label, mode, due = format(due), window_end = format(window_end), days_overdue, attempts = n_fail, last_attempt = format(last_attempt),
            next_attempt = format(next_attempt), missed = n_missed,
            flags = paste0(if_else(any_serious, "Serious AE; ", ""), if_else(hb_drop, "Hb fall of 2+; ", ""), if_else(n_missed >= 1, paste0(n_missed, " missed contact(s); "), "")))
pinfo <- reg |> select(patient_id, first_name, last_name, phone_final, alt_contact_name, alt_contact_phone, facility_lbl, district_lbl)
q_ev <- cem_events |> filter(!resolved) |> left_join(pinfo, by = "patient_id") |>
  transmute(category = "Open adverse event", patient_id, name = trimws(paste(coalesce(first_name, ""), coalesce(last_name, ""))), phone = phone_final,
            alt_name = alt_contact_name, alt_phone = alt_contact_phone, facility = facility_lbl, district = district_lbl,
            visit = paste0("Update event ", event_id, " (", intensity, if_else(serious, ", serious", ""), ")"), mode = "Form 2 update",
            due = format(last_record + 7), window_end = NA_character_, days_overdue = pmax(as.numeric(TODAY - (last_record + 7)), 0), attempts = NA_integer_,
            last_attempt = format(last_record), next_attempt = NA_character_, missed = NA_integer_, flags = if_else(serious, "Serious AE; ", ""))
q_d0 <- cohort |> filter(!coalesce(d0_done, FALSE)) |>
  transmute(category = "No Day 0 CEM form", patient_id, name = trimws(paste(coalesce(first_name, ""), coalesce(last_name, ""))), phone = phone_final,
            alt_name = alt_contact_name, alt_phone = alt_contact_phone, facility = facility_lbl, district = district_lbl, visit = "Day 0", mode = "In person",
            due = format(day0), window_end = format(day0 + 1), days_overdue = pmax(as.numeric(TODAY - day0 - 1), 0), attempts = 0L,
            last_attempt = NA_character_, next_attempt = NA_character_, missed = 0L, flags = "")
q_ltfu <- visit_status |> filter(n_missed >= LTFU_MISSED, is.na(exit_status)) |> distinct(patient_id, .keep_all = TRUE) |>
  transmute(category = "Meets LTFU definition", patient_id, name = trimws(paste(coalesce(first_name, ""), coalesce(last_name, ""))), phone = phone_final,
            alt_name = alt_contact_name, alt_phone = alt_contact_phone, facility = facility_lbl, district = district_lbl,
            visit = "Confirm and record the disposition in Form 2", mode = "Telephone", due = NA_character_, window_end = NA_character_, days_overdue = NA_real_,
            attempts = NA_integer_, last_attempt = NA_character_, next_attempt = NA_character_, missed = n_missed, flags = paste0(n_missed, " missed contacts; "))
q_order <- c("Overdue", "Due now", "Open adverse event", "No Day 0 CEM form", "Meets LTFU definition", "Upcoming (7 days)")
queue <- bind_rows(q_vis, q_ev, q_d0 |> filter(!patient_id %in% q_vis$patient_id[q_vis$visit == "Day 0"]), q_ltfu) |>
  mutate(flags = sub("; $", "", flags), name = na_if(name, "")) |>
  arrange(match(sub(" \\(call back\\)", "", category), q_order), desc(days_overdue), due)
dir.create(dirname(QUEUE_JSON), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(list(generated_at = format(Sys.time(), "%Y-%m-%d %H:%M", tz = LOCAL_TZ), today = format(TODAY), is_demo = IS_DEMO, rows = queue),
                     QUEUE_JSON, auto_unbox = TRUE, na = "null", null = "null", dataframe = "rows")
log_msg("queue: ", nrow(queue), " rows written to ", QUEUE_JSON, " (private; encrypt with tools/encrypt_queue.mjs)")

# ============================================================================
# 6. Write the public JSON (column-wise to keep it small)
# ============================================================================
out <- list(
  meta = list(generated_at = format(Sys.time(), "%Y-%m-%d %H:%M", tz = LOCAL_TZ), pulled_at = raw$registration$pulled_at %||% NA,
              today = format(TODAY), pilot_start = format(PILOT_START), is_demo = IS_DEMO, includes_test = !DROP_TEST,
              test_excluded = as.list(if (DROP_TEST) n_test else n_test * 0),
              protocol = list(sample_size = ENROL_TOTAL, enrol_months = ENROL_MONTHS, hb_drop = HB_DROP_ALERT, serious_days = SERIOUS_REPORT_D,
                              other_days = OTHER_REPORT_D, ltfu_missed = LTFU_MISSED),
              visit_schedule = cols(VISIT_SCHEDULE),
              counts = list(registered = nrow(reg), cohort = nrow(cohort), contacts = nrow(visits), cem_events = nrow(cem_events),
                            spont_reports = nrow(adr), spont_events = nrow(adr_events), queue = nrow(queue))),
  facilities = cols(facilities),
  reg = cols(reg_cube), not_elig = cols(not_elig), refusal = cols(refusal), not_disp = cols(not_disp), tests = cols(tests), act = cols(act_cube),
  dose_basis = cols(dose_basis), dose_band = cols(dose_band), mgkg_hist = cols(mgkg_hist), enrol_day = cols(enrol_day),
  fu = cols(fu_cube), fu_time = cols(fu_time), overdue_age = cols(overdue_age), calls = cols(calls), attempts = cols(attempts_needed),
  contacts_week = cols(contacts_week), exits = cols(exits_cube), consent = cols(consent_cube),
  ev = cols(ev_cube), ev_signs = cols(ev_signs), ev_crit = cols(ev_crit), ev_timely = cols(ev_timely), ev_onset = cols(ev_onset), ev_open = cols(ev_open),
  spont = cols(spont), safety = cols(safety_cube), hb_hist = cols(hb_hist), hb_grades = cols(hb_grades), symptoms = cols(symptoms),
  queue = cols(queue_cube),
  analysis = analysis_out,
  dq = cols(dq), subs_week = cols(subs_week), entry_lag = cols(entry_lag), completeness = cols(completeness), versions = cols(versions))

# Last safety net: refuse to publish if any identifier slipped in
txt <- jsonlite::toJSON(out, auto_unbox = TRUE, na = "null", digits = 4)
if (grepl("GA-[0-9A-F]{8}|RPT-|VIS-|\\+?233[0-9]{9}|\\b0[235][0-9]{8}\\b", txt)) stop("Identifier pattern found in dashboard JSON; not publishing.")
nm <- unique(na.omit(c(reg$first_name, reg$last_name))); nm <- nm[nchar(nm) >= 3]
hit <- nm[vapply(nm, function(n) grepl(paste0("\"", n, "\""), txt, fixed = TRUE), logical(1))]
if (length(hit)) stop("A patient name appears in the dashboard JSON; not publishing.")
dir.create(dirname(OUT_JSON), recursive = TRUE, showWarnings = FALSE)
writeLines(txt, OUT_JSON)
log_msg("wrote ", OUT_JSON, " (", round(file.size(OUT_JSON) / 1024, 1), " KB)")
