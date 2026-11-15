# =============================================================================
# SLDP pilot, Greater Accra: shared data pipeline (forms revised November 2026)
# Author: Kingsley Amegah, Data Scientist / Health Data Specialist
#
# Pulls the three Kobo forms (Form 1 Patient Register, Form 2 CEM Form,
# Form 3 ADR/SAE Report for patients not enrolled in CEM), cleans and
# validates them, and links them into analysis tables:
#   reg          one row per registered patient (Form 1)
#   visits       one row per CEM contact (Form 2)
#   cem_records  one row per adverse-event block recorded in Form 2
#   cem_events   one row per CEM adverse event (first record + latest update)
#   adr          one row per Form 3 report;  adr_events  one row per Form 3 event
#   cohort       one row per CEM participant (consented), with Hb, events, exits
#   visit_status one row per participant x scheduled visit (Day 0 to Day 28)
#   issue_log    data-quality checks
# Sourced by R/build_dashboard_data.R. Expects a `params` list to exist.
# =============================================================================
pkgs <- c("httr2", "jsonlite", "dplyr", "tidyr", "purrr", "stringr", "tibble", "lubridate")
invisible(lapply(pkgs, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))
KOBO_SERVER <- params$kobo_server
KOBO_TOKEN  <- Sys.getenv("KOBO_TOKEN")      # from .Renviron or the GitHub secret, never typed here

ASSETS <- c(registration = "a58q4HmKUXX6C28cUNVUd9",   # Form 1 Patient Register
            cem          = "azukUtLnBWCCZRmP6bVSm8",   # Form 2 CEM Form (Days 0, 3, 7, 14, 28)
            adr          = "aM6nCEVRheMGbVAKDGUT8i")   # Form 3 ADR/SAE Report (patients not enrolled in CEM)

USE_CACHE   <- isTRUE(params$use_cache)
DROP_TEST   <- isTRUE(params$drop_test)
LOCAL_TZ    <- "Africa/Accra"
TODAY       <- if (nzchar(Sys.getenv("SLDP_TODAY"))) as.Date(Sys.getenv("SLDP_TODAY")) else as.Date(lubridate::with_tz(Sys.time(), LOCAL_TZ))
PILOT_START <- as.Date(params$pilot_start)

# ---- Protocol constants (CEM protocol, Greater Accra SLDP pilot) -------------------
HB_DROP_ALERT     <- 2        # g/dL fall Day 0 to Day 3 = safety signal
SERIOUS_REPORT_D  <- 7        # FDA: serious ADRs reported within 7 days
OTHER_REPORT_D    <- 28       # FDA: other ADRs within 28 days
LTFU_MISSED       <- 2        # lost to follow-up after 2 unsuccessful scheduled follow-ups
ENROL_MONTHS      <- 5        # 5 months of enrolment, 1,500 participants in 10 sites
ENROL_TOTAL       <- 1500
# Scheduled contacts: offset from Day 0 and grace (days) before a visit counts as overdue (same as Form 2)
VISIT_SCHEDULE <- tibble::tribble(
  ~visit, ~offset, ~grace, ~mode,        ~label,
  "d0",   0,       1,      "In person",  "Day 0",
  "d3",   3,       2,      "In person",  "Day 3",
  "d7",   7,       3,      "Telephone",  "Day 7",
  "d14",  14,      3,      "Telephone",  "Day 14",
  "d28",  28,      5,      "Telephone",  "Day 28")
# 5-month enrolment targets per CEM site (protocol slide "Study Sites Enrolment Target")
SITE_TARGETS <- tibble::tribble(
  ~facility_label,                     ~target_total, ~target_month,
  "Abokobi Health Centre",             93,  19,
  "Adabraka Polyclinic",               163, 33,
  "Ashaiman Municipal Hospital",       78,  16,
  "Civil Service Polyclinic",          48,  10,
  "Community 22 Polyclinic",           41,  8,
  "Dome Community Clinic",             583, 117,
  "Ga East Municipal Hospital",        134, 27,
  "Greater Accra Regional Hospital",   72,  14,
  "La Polyclinic",                     78,  16,
  "University Hospital, Legon",        210, 42)

DIR_RAW <- Sys.getenv("SLDP_RAW_DIR", file.path("data", "raw"))   # raw snapshots hold names: never committed
dir.create(DIR_RAW, recursive = TRUE, showWarnings = FALSE)

log_msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
`%||%`  <- function(a, b) if (is.null(a)) b else a

# ============================================================================
# API
# ============================================================================
kobo_request <- function(url) {
  if (!nzchar(KOBO_TOKEN)) stop("KOBO_TOKEN is not set. Add it to .Renviron (local) or the repository secrets (GitHub).")
  request(url) |>
    req_headers(Authorization = paste("Token", KOBO_TOKEN), Accept = "application/json") |>
    req_retry(max_tries = 4, backoff = function(i) 2^i) |>
    req_timeout(120) |> req_perform() |> resp_body_json(simplifyVector = FALSE)
}
kobo_pull_submissions <- function(uid, page_size = 1000) {
  url <- sprintf("%s/api/v2/assets/%s/data.json?limit=%d", KOBO_SERVER, uid, page_size)
  records <- list()
  while (!is.null(url)) { page <- kobo_request(url); records <- c(records, page$results); url <- page[["next"]] }
  records
}
kobo_pull_form <- function(uid) {
  a <- kobo_request(sprintf("%s/api/v2/assets/%s.json", KOBO_SERVER, uid))
  list(name = a$name, version = a$deployed_version_id, survey = a$content$survey, choices = a$content$choices)
}
load_all_forms <- function() {
  out <- list()
  for (form in names(ASSETS)) {
    latest <- file.path(DIR_RAW, paste0(form, "_latest.json"))
    if (USE_CACHE) {
      if (!file.exists(latest)) stop("No cached snapshot for ", form, " in ", DIR_RAW)
      out[[form]] <- jsonlite::read_json(latest, simplifyVector = FALSE)
    } else {
      snap <- list(pulled_at = format(Sys.time(), tz = LOCAL_TZ, usetz = TRUE),
                   form = kobo_pull_form(ASSETS[[form]]), records = kobo_pull_submissions(ASSETS[[form]]))
      jsonlite::write_json(snap, latest, auto_unbox = TRUE, null = "null")
      out[[form]] <- snap
    }
  }
  out
}
raw <- load_all_forms()

# ============================================================================
# FLATTEN
# Kobo keys carry the group path ("site/patient_id"); XLSForm names are unique,
# so the last segment is a safe column name. Repeats become child tables.
# ============================================================================
short_name <- function(x) sub(".*/", "", x)
scalar_chr <- function(v) if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[[1]])
flatten_record <- function(rec) {
  keep <- rec[!vapply(rec, is.list, logical(1))]
  keep <- lapply(keep, scalar_chr); names(keep) <- short_name(names(keep))
  keep <- keep[!duplicated(names(keep))]
  tibble::as_tibble(keep)
}
flatten_repeat <- function(records, repeat_name) {
  purrr::map_dfr(records, function(rec) {
    key <- names(rec)[short_name(names(rec)) == repeat_name]
    if (length(key) == 0 || !is.list(rec[[key[1]]])) return(NULL)
    purrr::imap_dfr(rec[[key[1]]], function(item, i) {
      if (!is.list(item)) return(NULL)
      item <- item[!vapply(item, is.list, logical(1))]
      item <- lapply(item, scalar_chr); names(item) <- short_name(names(item)); item <- item[!duplicated(names(item))]
      tibble::as_tibble(item) |> mutate(`_parent` = as.character(rec[["_id"]]), item_no = i, .before = 1)
    })
  })
}
to_table <- function(s) if (length(s$records) == 0) tibble() else purrr::map_dfr(s$records, flatten_record)
ensure_cols <- function(df, cols) { for (c in setdiff(cols, names(df))) df[[c]] <- rep(NA_character_, nrow(df)); df }

# ---- Codebook: choice labels, select_multiple dummies --------------------------------
ch_val <- function(c, k) { v <- c[[k]]; if (is.list(v)) v <- v[[1]]; if (is.null(v)) NA_character_ else as.character(v) }
build_codebook <- function(form) {
  q <- purrr::map_dfr(form$survey, function(r) tibble(name = r$name %||% r$`$autoname` %||% NA_character_,
                                                     type = r$type %||% NA_character_,
                                                     list_name = r$select_from_list_name %||% NA_character_)) |>
    filter(!is.na(name), type %in% c("select_one", "select_multiple"))
  ch <- purrr::map_dfr(form$choices, function(c) tibble(list_name = ch_val(c, "list_name"), code = ch_val(c, "name"),
                                                       label = coalesce(ch_val(c, "label"), ch_val(c, "name")),
                                                       district = ch_val(c, "district"), cem = ch_val(c, "cem")))
  list(questions = q, choices = ch)
}
cb <- lapply(raw, function(s) build_codebook(s$form))
lbl <- function(form, list_name, codes) { ch <- cb[[form]]$choices |> filter(.data$list_name == !!list_name); ch$label[match(codes, ch$code)] }
has_code <- function(x, code) stringr::str_detect(coalesce(x, ""), paste0("(^|\\s)", code, "($|\\s)"))

# ---- Typing helpers -----------------------------------------------------------------
to_num  <- function(x) suppressWarnings(as.numeric(x))
to_date <- function(x) suppressWarnings(as.Date(substr(x, 1, 10)))
to_dt   <- function(x) suppressWarnings(lubridate::ymd_hms(x, tz = LOCAL_TZ, quiet = TRUE))
yes     <- function(x) !is.na(x) & x == "yes"
is_test <- function(x) stringr::str_detect(coalesce(x, ""), stringr::regex("^\\s*test\\b", ignore_case = TRUE))
first_non_na <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[[1]] else x[NA_integer_][1] }
safe_min <- function(x) { x <- x[!is.na(x)]; if (length(x)) min(x) else x[NA_integer_][1] }
last_val <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[[length(x)]] else x[NA_integer_][1] }
safe_max <- function(x) { x <- x[!is.na(x)]; if (length(x)) max(x) else x[NA_integer_][1] }

# ---- Facility lookup (every pilot site, from the Form 1 choices) ------------------------
districts <- cb$registration$choices |> filter(list_name == "district") |> select(district = code, district_lbl = label)
fac_map <- cb$registration$choices |> filter(list_name == "facility") |>
  transmute(facility = code, facility_lbl = label, district = .data$district, cem_site = cem %in% "yes") |>
  left_join(districts, by = "district") |>
  left_join(SITE_TARGETS, by = c("facility_lbl" = "facility_label")) |>
  mutate(target_total = if_else(cem_site, target_total, NA_real_), target_month = if_else(cem_site, target_month, NA_real_))

issues <- list()
add_issue <- function(df, form, check, severity) {
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
  issues[[length(issues) + 1]] <<- tibble(form = form, facility = as.character(df[["facility"]] %||% NA), check = check, severity = severity,
                                          patient_id = as.character(df[["patient_id"]] %||% NA))
}

# ============================================================================
# FORM 1: PATIENT REGISTER
# ============================================================================
reg_raw <- to_table(raw$registration) |>
  ensure_cols(c("_id", "_uuid", "_submission_time", "__version__", "start_time", "end_time", "district", "facility", "facility_cem",
                "registration_date", "patient_id", "first_name", "last_name", "phone", "age_unit", "age_value", "age_months", "age_years",
                "sex", "nhis_status", "weight", "test_done", "test_type", "rdt_result", "micro_result", "other_test_result", "severity",
                "micro_species", "parasite_density", "stages_seen", "pf_positive", "pregnant", "breastfeeding_u1", "pq_allergy",
                "severe_anaemia", "dark_urine", "jaundice", "eligibility", "clinician_eligible", "clinician_disagrees", "hx_haemolysis",
                "act_dispensed", "act_not_dispensed_reason", "act_type", "act_dose", "act_brand", "act_first_oral", "act_start_date",
                "pq_accepted", "pq_not_given_reasons", "pq_given", "pq_nd_reason", "pq_brand", "rec_dose", "act_rec_dose", "pq_strength",
                "pq_dose", "pq_dose_mg", "mg_per_kg", "dose_flag", "dose_basis", "dose_confirm", "dot", "dot_reasons", "pq_with_act",
                "with_food_ans", "vomit30", "dose_repeated", "pq_datetime", "pq_date", "counselled", "other_meds_any", "cem_enrolled",
                "phone_final", "alt_contact_name", "alt_contact_phone", "cem_card_done", "adr_today", "adr_signs", "adr_serious",
                "adr_serious_criteria", "entry_name"))
test_ids <- reg_raw |> filter(is_test(first_name) | is_test(last_name)) |> pull(patient_id) |> unique()
n_test <- c(form1 = length(test_ids))

reg <- reg_raw |>
  { \(d) if (DROP_TEST) filter(d, !patient_id %in% test_ids) else d }() |>
  mutate(submission_time = to_dt(`_submission_time`), submission_date = as.Date(submission_time),
         registration_date = to_date(registration_date), pq_date = to_date(pq_date), act_start_date = to_date(act_start_date),
         age_months = to_num(age_months), age_years = age_months / 12, weight = to_num(weight),
         pq_dose_mg = to_num(pq_dose_mg), mg_per_kg = to_num(mg_per_kg), parasite_density = to_num(parasite_density),
         pf = yes(pf_positive), eligible = eligibility %in% "eligible", sldp_given = yes(pq_given), cem_site = yes(facility_cem),
         cem_in = yes(cem_enrolled), act_given = yes(act_dispensed),
         age_group = cut(age_years, c(-Inf, 5, 15, Inf), right = FALSE, labels = c("Under 5", "5-14", "15 and over")),
         age_band = cut(age_years, c(-Inf, 5, 15, 25, 45, Inf), right = FALSE, labels = c("<5", "5-14", "15-24", "25-44", "45+")),
         weight_band = cut(weight, c(-Inf, 10, 15, 30, 60, Inf), right = FALSE, labels = c("<10 kg", "10-14 kg", "15-29 kg", "30-59 kg", "60+ kg")),
         reg_week = lubridate::floor_date(registration_date, "week", week_start = 1),
         # Why the patient was not eligible (first criterion met, in screening order)
         not_elig_reason = case_when(eligible ~ NA_character_,
                                     test_done %in% "no" ~ "Not tested (presumptive)",
                                     !pf ~ "Not P. falciparum positive",
                                     !is.na(age_months) & age_months < 12 ~ "Under 1 year",
                                     yes(pregnant) | pregnant %in% "not_sure" ~ "Pregnant / not sure",
                                     yes(breastfeeding_u1) | breastfeeding_u1 %in% "not_sure" ~ "Breastfeeding infant under 1",
                                     yes(pq_allergy) | pq_allergy %in% "not_sure" ~ "Allergy to primaquine",
                                     yes(severe_anaemia) | severe_anaemia %in% "not_sure" ~ "Severe anaemia",
                                     yes(dark_urine) | dark_urine %in% "not_sure" ~ "Dark urine",
                                     yes(jaundice) | jaundice %in% "not_sure" ~ "Jaundice",
                                     TRUE ~ "Other / not recorded"),
         # Where each eligible patient left the SLDP cascade
         sldp_stage = case_when(!eligible ~ "Not eligible", !act_given ~ "No ACT dispensed", pq_accepted %in% "no" ~ "Declined primaquine",
                                pq_given %in% "no" ~ "Not dispensed", sldp_given ~ "Given", TRUE ~ "Not recorded"),
         dose_ok = sldp_given & dose_flag %in% "no",
         facility = coalesce(facility, "unknown")) |>
  left_join(fac_map |> select(facility, facility_lbl, district_lbl), by = "facility")

add_issue(reg |> group_by(patient_id) |> filter(n() > 1) |> ungroup(), "Form 1", "Patient ID registered more than once", "Critical")
add_issue(reg |> filter(registration_date > TODAY | registration_date < PILOT_START), "Form 1", "Registration date outside the pilot period", "Major")
add_issue(reg |> filter(sldp_given, is.na(pq_date)), "Form 1", "Primaquine given but no date and time", "Critical")
add_issue(reg |> filter(sldp_given, dose_flag %in% "yes"), "Form 1", "Primaquine dose does not match either dosing table (confirmed by officer)", "Major")
add_issue(reg |> filter(cem_in, is.na(phone_final)), "Form 1", "CEM participant without a phone number", "Major")
add_issue(reg |> filter(sldp_given, clinician_disagrees %in% "yes"), "Form 1", "Clinician disagreed with the screening result", "Minor")
reg <- reg |> arrange(submission_time) |> distinct(patient_id, .keep_all = TRUE)

# ============================================================================
# FORM 2: CEM FORM
# ============================================================================
sym <- c("dark_urine", "pallor", "jaundice", "fatigue", "dizziness", "fever", "nausea", "vomiting", "rash")
visits_raw <- to_table(raw$cem) |>
  ensure_cols(c("_id", "_uuid", "_submission_time", "__version__", "patient_id", "reg_found", "day0_manual", "day0_date", "facility",
                "district", "sex", "age_months", "consent_refused", "visit_record_id", "visit_date", "visit", "consent", "interviewer",
                "mode", "days_since_day0", "timing_off", "prior_completed", "diabetes", "hypertension", "g6pd_status", "chronic_illness",
                "current_meds", "other_condition", "f_symptoms", "f_temp", "f_pulse", "f_rr", "f_pallor", "f_jaundice", "f_clinical_grade",
                "f_hb", "f_lab_grade", "f_grade", "f_status", "g_full_act", "g_new_symptoms", "g_dark_urine", "g_pallor", "g_fatigue",
                "g_jaundice", "g_palpitations", "g_hospitalised", "g_new_meds", "g_hb", "g_lab_grade", "g_clinical_grade", "d0_hb",
                "hb_change", "hb_alert", "g_repeat_test", "g_test_type", "g_rdt", "g_species", "g_stages", "g_status",
                "h_success", "h_attempt", "h_fail_reason", "h_withdrawing", "h_next_attempt", paste0("h_", sym), "h_hospitalised",
                "h_new_meds", "h_ae", "h_referred", "h_status", "h_d28_outcome", "u_reason", paste0("u_", sym), "u_hb", "u_hb_change",
                "u_hospitalised", "u_ae", "u_referred", "u_status", "status_now", "m_status", "m_date", "ae_flag", "ae_any", "completed",
                "review_ok", "f_status_date", "g_status_date", "h_status_date", "u_status_date"))
n_test["form2"] <- sum(visits_raw$patient_id %in% test_ids | is_test(visits_raw$interviewer))
visits <- visits_raw |>
  { \(d) if (DROP_TEST) filter(d, !patient_id %in% test_ids, !is_test(interviewer)) else d }() |>
  mutate(submission_time = to_dt(`_submission_time`), submission_date = as.Date(submission_time),
         visit_date = to_date(visit_date), day0_date = to_date(day0_date), m_date = to_date(m_date), h_next_attempt = to_date(h_next_attempt),
         status_date = coalesce(to_date(f_status_date), to_date(g_status_date), to_date(h_status_date), to_date(u_status_date)),
         across(c(f_hb, g_hb, d0_hb, hb_change, u_hb, u_hb_change, days_since_day0, f_temp, f_lab_grade, h_attempt), to_num),
         done = yes(completed), phone = visit %in% c("d7", "d14", "d28") | (visit == "unscheduled" & mode %in% "phone"),
         facility = coalesce(facility, "unknown")) |>
  left_join(fac_map |> select(facility, facility_lbl), by = "facility")

# Adverse-event blocks recorded inside Form 2 (one row per block)
ae_items <- flatten_repeat(raw$cem$records, "ae_event")
cem_records <- if (nrow(ae_items) == 0) tibble() else ae_items |>
  ensure_cols(c("ae_event_id", "ae_type_saved", "ae_prev_id", "ae_signs", "ae_onset", "ae_day", "ae_stopped", "ae_stop_date", "ae_mgmt",
                "ae_intensity", "ae_outcome", "ae_serious", "ae_criteria", "ae_focal_informed", "ae_focal_date", "ae_cm_any", "ae_cm_n",
                "ae_p_n", "ae_p1_which", "ae_p2_which", "ae_p3_which", "ae_p_which")) |>
  inner_join(visits |> select(`_parent` = `_id`, patient_id, facility, visit, visit_date, submission_time), by = "_parent") |>
  filter(!is.na(ae_event_id), ae_event_id != "") |>
  mutate(onset = to_date(ae_onset), stop_date = to_date(ae_stop_date), focal_date = to_date(ae_focal_date),
         which_all = paste(coalesce(ae_p1_which, ""), coalesce(ae_p2_which, ""), coalesce(ae_p3_which, ""), coalesce(ae_p_which, "")))
EMPTY_EVENTS <- tibble(event_id = character(), source = character(), patient_id = character(), facility = character(), onset = as.Date(character()),
  signs = character(), first_record = as.Date(character()), n_records = integer(), n_updates = integer(), intensity = character(), serious = logical(),
  criteria = character(), outcome = character(), stopped = character(), stop_date = as.Date(character()), informed_date = as.Date(character()),
  last_record = as.Date(character()), sus_sldp = logical(), sus_act = logical(), sus_other = logical(), hospitalised = logical(), transfused = logical(),
  death = logical(), report_lag = numeric(), report_target = numeric(), on_time = logical(), resolved = logical())
summarise_events <- function(d, source) {
  if (is.null(d) || nrow(d) == 0) return(EMPTY_EVENTS)
  d |> arrange(event_id, record_date, submission_time) |> group_by(event_id) |>
    summarise(source = source, patient_id = first(patient_id), facility = first(facility),
              onset = first_non_na(onset[type == "new"]) %||% first_non_na(onset), signs = first_non_na(signs[type == "new"]),
              first_record = safe_min(record_date), n_records = n(), n_updates = sum(type == "update"),
              intensity = c("mild", "moderate", "severe")[suppressWarnings(max(c(0, match(intensity, c("mild", "moderate", "severe"))), na.rm = TRUE))][1],
              serious = any(serious %in% "yes"), criteria = paste(unique(unlist(stringr::str_split(na.omit(criteria), " "))), collapse = " "),
              outcome = last_val(outcome), stopped = last_val(stopped), stop_date = safe_max(stop_date),
              informed_date = safe_min(informed_date), last_record = safe_max(record_date),
              sus_sldp = any(has_code(which_all, "sldp")), sus_act = any(has_code(which_all, "act")), sus_other = any(has_code(which_all, "other")),
              hospitalised = any(has_code(criteria, "hospitalisation") | has_code(criteria, "prolonged_hosp") | has_code(mgmt, "admitted")),
              transfused = any(has_code(mgmt, "transfused")), .groups = "drop") |>
    mutate(intensity = coalesce(intensity, "Not recorded"), outcome = coalesce(outcome, "Not recorded"),
           death = has_code(criteria, "death") | outcome == "D",
           report_lag = as.numeric(coalesce(informed_date, first_record) - onset),
           report_target = if_else(serious, SERIOUS_REPORT_D, OTHER_REPORT_D), on_time = !is.na(report_lag) & report_lag <= report_target,
           resolved = outcome %in% c("R1", "S", "D"))
}
cem_events <- summarise_events(if (nrow(cem_records)) cem_records |>
  transmute(event_id = ae_event_id, patient_id, facility, type = coalesce(ae_type_saved, "new"), onset, signs = ae_signs, record_date = visit_date,
            submission_time, intensity = ae_intensity, serious = ae_serious, criteria = ae_criteria, outcome = ae_outcome, stopped = ae_stopped,
            stop_date, informed_date = focal_date, which_all, mgmt = ae_mgmt) else NULL, "CEM (Form 2)")

add_issue(visits |> filter(!patient_id %in% reg$patient_id), "Form 2", "CEM contact for a Patient ID not in Form 1", "Critical")
add_issue(visits |> filter(visit_date > TODAY), "Form 2", "Visit date in the future", "Major")
add_issue(visits |> filter(timing_off %in% "yes", visit != "unscheduled"), "Form 2", "Visit recorded outside its scheduled window", "Minor")
add_issue(visits |> filter(done, visit != "unscheduled") |> group_by(patient_id, visit) |> filter(n() > 1) |> ungroup(), "Form 2", "Same scheduled visit completed more than once", "Major")
add_issue(visits |> filter(visit == "d0", done, is.na(f_hb)), "Form 2", "Day 0 visit without Hb", "Major")
add_issue(visits |> filter(visit == "d3", done, is.na(g_hb)), "Form 2", "Day 3 visit without Hb", "Major")
add_issue(visits |> filter(!is.na(day0_manual)), "Form 2", "Day 0 date entered by hand (patient not found on device)", "Minor")
if (nrow(cem_events)) {
  add_issue(cem_events |> filter(serious, !on_time), "Form 2", "Serious event: focal person informed later than 7 days after onset", "Critical")
  add_issue(cem_events |> filter(!resolved, last_record < TODAY - 14), "Form 2", "Unresolved event with no update for over 14 days", "Major")
}

# ============================================================================
# FORM 3: ADR/SAE REPORT (patients not enrolled in CEM)
# ============================================================================
adr_raw <- to_table(raw$adr) |>
  ensure_cols(c("_id", "_uuid", "_submission_time", "__version__", "patient_id", "reg_found", "r_cem_enrolled", "report_type",
                "existing_event_id", "event_id", "facility", "district", "sex", "age_years", "day0_date", "reported_through", "m_first_name",
                "m_cem", "a_pregnancy", "b_signs", "b_onset", "b_day", "b_stopped", "b_stop_date", "b_mgmt", "b_intensity", "c_outcome",
                "s_serious", "s_criteria", "e_any", "f_name", "f_profession", "f_report_date", "report_lag", "cem_block"))
prod_items <- flatten_repeat(raw$adr$records, "product")
n_test["form3"] <- sum(adr_raw$patient_id %in% test_ids | is_test(adr_raw$m_first_name) | is_test(adr_raw$f_name))
adr <- adr_raw |>
  { \(d) if (DROP_TEST) filter(d, !patient_id %in% test_ids, !is_test(m_first_name), !is_test(f_name)) else d }() |>
  mutate(submission_time = to_dt(`_submission_time`), submission_date = as.Date(submission_time),
         onset = to_date(b_onset), report_date = to_date(f_report_date), stop_date = to_date(b_stop_date), age_years = to_num(age_years),
         facility = coalesce(facility, "unknown"), cem_participant = yes(r_cem_enrolled) | yes(m_cem)) |>
  left_join(fac_map |> select(facility, facility_lbl, cem_site), by = "facility")
if (nrow(prod_items)) {
  w <- prod_items |> ensure_cols("p_which") |> group_by(`_parent`) |> summarise(which_all = paste(na.omit(p_which), collapse = " "), .groups = "drop")
  adr <- adr |> left_join(w, by = c("_id" = "_parent"))
}
adr <- ensure_cols(adr, "which_all")
adr_events <- summarise_events(if (nrow(adr)) adr |>
  transmute(event_id, patient_id, facility, type = coalesce(report_type, "new"), onset, signs = b_signs, record_date = report_date, submission_time,
            intensity = b_intensity, serious = s_serious, criteria = s_criteria, outcome = c_outcome, stopped = b_stopped, stop_date,
            informed_date = report_date, which_all = coalesce(which_all, ""), mgmt = b_mgmt) |> filter(!is.na(event_id)) else NULL, "Spontaneous (Form 3)")
if (nrow(adr_events)) adr_events <- adr_events |>
  left_join(adr |> filter(report_type %in% "new") |> group_by(event_id) |>
              summarise(through = first(reported_through), pregnant = any(a_pregnancy %in% "pregnant"), .groups = "drop"), by = "event_id")

add_issue(adr |> filter(cem_participant), "Form 3", "Form 3 report for a CEM participant (should be in Form 2)", "Critical")
add_issue(adr |> filter(!reg_found %in% "yes"), "Form 3", "Patient not found in Form 1 (entered by hand)", "Minor")
add_issue(adr |> filter(report_type %in% "update", !existing_event_id %in% adr$event_id[adr$report_type %in% "new"]), "Form 3", "Update refers to an event with no new report", "Major")
if (nrow(adr_events)) add_issue(adr_events |> filter(!on_time), "Form 3", "Reported after the FDA deadline (7 days serious, 28 days other)", "Major")

# ============================================================================
# CEM COHORT (one row per participant) and VISIT SCHEDULE
# ============================================================================
d0v <- visits |> filter(visit == "d0") |> arrange(submission_time) |> group_by(patient_id) |>
  summarise(consent_d0 = last(consent), d0_done = any(done), hb_d0 = last_val(f_hb), lab_grade_d0 = last_val(f_lab_grade),
            clin_grade_d0 = last_val(f_clinical_grade), g6pd = last_val(g6pd_status), day0_manual = last_val(to_date(day0_manual)),
            .groups = "drop")
d3v <- visits |> filter(visit == "d3", done) |> arrange(submission_time) |> group_by(patient_id) |>
  summarise(hb_d3 = last_val(g_hb), d3_date = last(visit_date), .groups = "drop")
contact <- visits |> group_by(patient_id) |>
  summarise(last_contact = safe_max(visit_date[done]), n_contacts = sum(done),
            haem_sign = any(yes(g_dark_urine) | yes(g_jaundice) | yes(h_dark_urine) | yes(h_jaundice) | yes(u_dark_urine) | yes(u_jaundice)),
            hosp_any = any(yes(g_hospitalised) | yes(h_hospitalised) | yes(u_hospitalised)),
            refused = any(visit == "d0" & consent %in% "no"), .groups = "drop")
exits <- visits |>
  mutate(ex = case_when(h_withdrawing %in% "yes" ~ "withdrawn", m_status %in% c("withdrawn", "lost", "died", "completed") ~ m_status,
                        status_now %in% "withdrew" ~ "withdrawn", status_now %in% c("lost", "died") ~ status_now, TRUE ~ NA_character_),
         ex_date = coalesce(m_date, status_date, visit_date)) |>
  filter(!is.na(ex)) |> arrange(ex_date) |> group_by(patient_id) |>
  summarise(exit_status = if (any(ex == "died")) "died" else if (any(ex %in% c("withdrawn", "lost"))) first(ex[ex %in% c("withdrawn", "lost")]) else first(ex),
            exit_date = safe_min(ex_date[ex != "completed"]) %||% NA, .groups = "drop")
ev_pat <- if (nrow(cem_events)) cem_events |> group_by(patient_id) |>
  summarise(n_events = n(), any_serious = any(serious), first_onset = safe_min(onset), death_event = any(death),
            haem_event = any(has_code(signs, "dark_urine") | has_code(signs, "jaundice") | has_code(signs, "hb_drop")), .groups = "drop") else
  tibble(patient_id = character(), n_events = integer(), any_serious = logical(), first_onset = as.Date(character()), death_event = logical(), haem_event = logical())

cohort <- reg |> filter(cem_in) |>
  left_join(d0v, by = "patient_id") |> left_join(d3v, by = "patient_id") |> left_join(contact, by = "patient_id") |>
  left_join(exits, by = "patient_id") |> left_join(ev_pat, by = "patient_id") |>
  mutate(refused = coalesce(refused, FALSE), day0 = coalesce(pq_date, day0_manual),
         n_events = coalesce(n_events, 0L), any_ae = n_events > 0, any_serious = coalesce(any_serious, FALSE),
         hb_change = hb_d3 - hb_d0, hb_drop = !is.na(hb_change) & hb_change <= -HB_DROP_ALERT,
         haemolysis = coalesce(haem_sign, FALSE) | coalesce(haem_event, FALSE) | hb_drop,
         hosp_any = coalesce(hosp_any, FALSE),
         anaemic_d0 = !is.na(lab_grade_d0) & lab_grade_d0 >= 1,
         hb_grade_d0 = factor(case_when(is.na(lab_grade_d0) ~ NA_character_, lab_grade_d0 == 0 ~ "Normal", lab_grade_d0 == 1 ~ "Mild (grade 1)",
                                        lab_grade_d0 == 2 ~ "Moderate (grade 2)", TRUE ~ "Severe (grade 3-4)"),
                              c("Normal", "Mild (grade 1)", "Moderate (grade 2)", "Severe (grade 3-4)")),
         g6pd_grp = factor(case_when(g6pd %in% "normal" ~ "Normal", g6pd %in% "deficient" ~ "Deficient", g6pd %in% "unknown" ~ "Unknown / not tested",
                                     TRUE ~ NA_character_), c("Normal", "Deficient", "Unknown / not tested")),
         follow_end = pmin(coalesce(exit_date, as.Date(Inf)), coalesce(last_contact, day0), day0 + 28, TODAY, na.rm = TRUE),
         person_days = pmax(as.numeric(follow_end - day0), 0.5),
         ae_time = as.numeric(first_onset - day0)) |>
  filter(!refused)

attempts <- visits |> filter(visit %in% VISIT_SCHEDULE$visit) |> group_by(patient_id, visit) |>
  summarise(done_date = safe_min(visit_date[done]), n_fail = sum(!done), done = any(done), last_attempt = safe_max(visit_date),
            next_attempt = safe_max(h_next_attempt), declined = any(h_fail_reason %in% "declined"), .groups = "drop")
visit_status <- cohort |>
  select(patient_id, facility, facility_lbl, district_lbl, day0, exit_status, exit_date, first_name, last_name, phone_final, alt_contact_name,
         alt_contact_phone, any_serious, hb_drop) |>
  tidyr::crossing(VISIT_SCHEDULE) |>
  left_join(attempts, by = c("patient_id", "visit")) |>
  mutate(done = coalesce(done, FALSE), n_fail = coalesce(n_fail, 0L), due = day0 + offset, window_end = due + grace,
         status = case_when(done ~ "Completed",
                            is.na(day0) ~ "No Day 0 date",
                            !is.na(exit_status) & exit_status != "completed" & (is.na(exit_date) | due > exit_date) ~ "Exited",
                            TODAY < due - 7 ~ "Not yet due",
                            TODAY < due ~ "Upcoming (7 days)",
                            TODAY <= window_end ~ "Due now",
                            TRUE ~ "Overdue"),
         days_late_done = as.numeric(done_date - due), on_time = done & days_late_done <= grace & days_late_done >= -1,
         days_overdue = if_else(status == "Overdue", as.numeric(TODAY - window_end), NA_real_)) |>
  group_by(patient_id) |> arrange(offset, .by_group = TRUE) |>
  # A pending visit is "missed" once the next scheduled visit is already due; misses count towards loss to follow-up
  mutate(next_due = lead(due), missed = status == "Overdue" & !is.na(next_due) & TODAY >= next_due,
         n_missed = sum(missed & visit != "d0"), status = if_else(missed, "Missed", status)) |> ungroup()

issue_log <- bind_rows(tibble(form = character(), facility = character(), check = character(), severity = character(), patient_id = character()),
                       bind_rows(issues))
if (nrow(cohort)) {
  add_issue(cohort |> filter(!coalesce(d0_done, FALSE), day0 < TODAY - 2), "Linkage", "CEM participant with no Day 0 CEM form after 2 days", "Major")
  add_issue(visit_status |> filter(n_missed >= LTFU_MISSED, is.na(exit_status)) |> distinct(patient_id, .keep_all = TRUE), "Linkage",
            "Meets the protocol definition of lost to follow-up (2 missed contacts) but not closed", "Major")
  issue_log <- bind_rows(issue_log |> slice(0), bind_rows(issues))
}
log_msg("pipeline: ", nrow(reg), " registered, ", nrow(cohort), " CEM participants, ", nrow(visits), " contacts, ",
        nrow(cem_events), " CEM events, ", nrow(adr), " Form 3 reports; TEST excluded: ", paste(names(n_test), n_test, collapse = ", "))
