# =============================================================================
# SLDP pilot, Greater Accra: shared data pipeline
# Author: Kingsley Amegah, Data Scientist / Health Data Specialist
#
# Pulls the three Kobo forms, cleans and validates them and links them into
# patient, visit and event tables. Same logic as SLDP_monitoring_analysis.Rmd.
# Sourced by R/build_dashboard_data.R (GitHub Action) to produce the
# aggregate-only dashboard data. Expects a `params` list to exist.
# =============================================================================
pkgs <- c("httr2", "jsonlite", "dplyr", "tidyr", "purrr", "stringr", "tibble", "lubridate")
invisible(lapply(pkgs, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))
KOBO_SERVER <- params$kobo_server
KOBO_TOKEN  <- Sys.getenv("KOBO_TOKEN")      # read from .Renviron, never typed here

# Asset UIDs of the three projects (from the project URL in Kobo)
ASSETS <- c(registration = "a58q4HmKUXX6C28cUNVUd9",   # Form 1, v2026110601
            cem          = "azukUtLnBWCCZRmP6bVSm8",   # Form 2, v2026110604
            adr          = "aM6nCEVRheMGbVAKDGUT8i")   # Form 3, v2026110603

USE_CACHE   <- isTRUE(params$use_cache)
DROP_TEST   <- isTRUE(params$drop_test)
LOCAL_TZ    <- "Africa/Accra"
TODAY       <- as.Date(lubridate::with_tz(Sys.time(), LOCAL_TZ))
PILOT_START <- as.Date(params$pilot_start)

# Clinical and operational thresholds (keep in line with the forms and protocol)
SLDP_TARGET_MG_PER_KG <- 0.25
SLDP_DOSE_LOW         <- 0.15
SLDP_DOSE_HIGH        <- 0.40
SLDP_MAX_MG           <- 15
HB_DROP_ALERT         <- 2      # g/dL fall from Day 0 = safety signal
ADR_RECONCILE_DAYS    <- 2      # Hb alert / new event should have a Form 3 report within this
ADR_REPORT_LAG_TARGET <- 2      # target days from onset to report
MIN_INTERVIEW_MIN     <- 3
MAX_INTERVIEW_MIN     <- 120

# Scheduled visits: offset from Day 0 and grace window (days) before "overdue"
VISIT_SCHEDULE <- tibble::tribble(
  ~visit_type, ~offset, ~grace, ~mode,
  "d0",  0, 1, "In person",
  "d3",  3, 2, "In person",
  "d7",  7, 3, "Telephone",
  "d14", 14, 3, "Telephone",
  "d28", 28, 5, "Telephone")

RUN_LABEL <- format(TODAY, "%Y-%m-%d")
# Raw snapshots hold names and phone numbers: they go to a folder that is never committed
DIR_RAW <- Sys.getenv("SLDP_RAW_DIR", file.path("data", "raw"))
dir.create(DIR_RAW, recursive = TRUE, showWarnings = FALSE)

PAL <- c("#2F5D7C", "#B4612F", "#6E8797", "#C9A227", "#4F7F5F", "#8E3B22")

log_msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
`%||%`  <- function(a, b) if (is.null(a)) b else a

# Helpers used throughout the report
pct <- function(n, d) {                     # percentage, one decimal, vector-safe
  r <- round(100 * n / d, 1); r[rep_len(is.na(d) | d <= 0, length(r))] <- NA_real_; r
}
ci_txt <- function(x, n) {                  # "x% (lo-hi)" with exact binomial 95% CI
  if (is.na(n) || n == 0) return(NA_character_)
  b <- binom.test(x, n); sprintf("%.1f%% (%.1f-%.1f)", 100 * x / n, 100 * b$conf.int[1], 100 * b$conf.int[2])
}
safe_fit <- function(expr, label) {                 # models never stop the report early in the pilot
  tryCatch(withCallingHandlers(expr, warning = function(w) {
    log_msg("model warning (", label, "): ", conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) { log_msg("model skipped (", label, "): ", conditionMessage(e)); NULL })
}


# ============================================================================
# API---------------------------------------------------------------------
# ============================================================================
# One authenticated GET with retries; returns parsed JSON
kobo_request <- function(url) {
  if (!nzchar(KOBO_TOKEN)) stop("KOBO_TOKEN is not set. Add it to your .Renviron file.")
  request(url) |>
    req_headers(Authorization = paste("Token", KOBO_TOKEN), Accept = "application/json") |>
    req_retry(max_tries = 4, backoff = function(i) 2^i) |>
    req_timeout(120) |> req_perform() |> resp_body_json(simplifyVector = FALSE)
}
# All submissions of one project, following the `next` page link
kobo_pull_submissions <- function(uid, page_size = 1000) {
  url <- sprintf("%s/api/v2/assets/%s/data.json?limit=%d", KOBO_SERVER, uid, page_size)
  records <- list()
  while (!is.null(url)) { page <- kobo_request(url); records <- c(records, page$results); url <- page[["next"]] }
  records
}
# The form definition: survey rows and choice lists (for labels)
kobo_pull_form <- function(uid) {
  a <- kobo_request(sprintf("%s/api/v2/assets/%s.json", KOBO_SERVER, uid))
  list(name = a$name, version = a$deployed_version_id, survey = a$content$survey, choices = a$content$choices)
}
load_all_forms <- function() {
  out <- list()
  for (form in names(ASSETS)) {
    latest <- file.path(DIR_RAW, paste0(form, "_latest.json"))
    if (USE_CACHE) {
      if (!file.exists(latest)) stop("No cached snapshot for ", form, ". Knit once with use_cache = FALSE.")
      out[[form]] <- jsonlite::read_json(latest, simplifyVector = FALSE)
    } else {
      snap <- list(pulled_at = format(Sys.time(), tz = LOCAL_TZ, usetz = TRUE),
                   form = kobo_pull_form(ASSETS[[form]]), records = kobo_pull_submissions(ASSETS[[form]]))
      stamp <- format(Sys.time(), "%Y%m%d_%H%M")
      jsonlite::write_json(snap, file.path(DIR_RAW, sprintf("%s_%s.json", form, stamp)), auto_unbox = TRUE, null = "null")
      jsonlite::write_json(snap, latest, auto_unbox = TRUE, null = "null")
      out[[form]] <- snap
    }
  }
  out
}
raw <- load_all_forms()

# ---- Flatten JSON into tables --------------------------------------------------
# Kobo keys carry the group path ("site/patient_id"); XLSForm names are unique,
# so the last path segment is a safe column name. Repeats become child tables.
short_name <- function(x) sub(".*/", "", x)
is_repeat  <- function(v) is.list(v) && length(v) > 0 && all(vapply(v, function(e) is.list(e) && !is.null(names(e)), logical(1)))
flatten_record <- function(rec) {
  keep <- rec[!vapply(rec, is.list, logical(1))]
  keep <- lapply(keep, function(v) if (is.null(v)) NA_character_ else as.character(v))
  names(keep) <- short_name(names(keep)); tibble::as_tibble(keep)
}
flatten_repeat <- function(records, repeat_name) {
  purrr::map_dfr(records, function(rec) {
    key <- names(rec)[short_name(names(rec)) == repeat_name]
    if (length(key) == 0 || !is_repeat(rec[[key]])) return(NULL)
    purrr::imap_dfr(rec[[key]], function(item, i) {
      item <- lapply(item, function(v) if (is.null(v)) NA_character_ else as.character(v))
      names(item) <- short_name(names(item))
      tibble::as_tibble(item) |> mutate(parent_id = as.character(rec[["_id"]]), item_no = i, .before = 1)
    })
  })
}
to_table <- function(s) if (length(s$records) == 0) tibble() else purrr::map_dfr(s$records, flatten_record)
ensure_cols <- function(df, cols) { for (c in setdiff(cols, names(df))) df[[c]] <- NA_character_; df }

reg_raw <- to_table(raw$registration); cem_raw <- to_table(raw$cem); adr_raw <- to_table(raw$adr)
cem_meds <- flatten_repeat(raw$cem$records, "current_medicines")
adr_meds <- flatten_repeat(raw$adr$records, "suspected_drugs")

# ---- Codebook: labels and select_multiple dummies -------------------------------
build_codebook <- function(form) {
  q <- purrr::map_dfr(form$survey, function(r) tibble(name = r$name %||% NA_character_, type = r$type %||% NA_character_,
                                                     list_name = r$select_from_list_name %||% NA_character_)) |>
    filter(!is.na(name), type %in% c("select_one", "select_multiple"))
  ch <- purrr::map_dfr(form$choices, function(c) { lab <- c$label; if (is.list(lab)) lab <- lab[[1]]
    tibble(list_name = c$list_name, code = as.character(c$name), label = as.character(lab %||% c$name)) })
  list(questions = q, choices = ch)
}
apply_codebook <- function(df, cb) {
  for (i in seq_len(nrow(cb$questions))) {
    v <- cb$questions$name[i]; if (!v %in% names(df)) next
    opts <- cb$choices |> filter(list_name == cb$questions$list_name[i])
    if (cb$questions$type[i] == "select_one") {
      df[[paste0(v, "_lbl")]] <- opts$label[match(df[[v]], opts$code)]
    } else for (code in opts$code) {
      col <- paste0(v, "__", code)
      df[[col]] <- as.integer(stringr::str_detect(coalesce(df[[v]], ""), paste0("(^|\\s)", code, "($|\\s)")))
      df[[col]][is.na(df[[v]])] <- NA_integer_
    }
  }
  df
}
cb_reg <- build_codebook(raw$registration$form); cb_cem <- build_codebook(raw$cem$form); cb_adr <- build_codebook(raw$adr$form)


# ============================================================================
# CLEANING----------------------------------------------------------------
# ============================================================================
issues <- list()
add_issue <- function(df, form, check, severity, value_col = NULL) {
  if (nrow(df) == 0) return(invisible(NULL))
  issues[[length(issues) + 1]] <<- tibble(
    form = form, record_id = as.character(df[["_id"]]), patient_id = as.character(df[["patient_id"]] %||% NA),
    facility = as.character(df[["facility"]] %||% NA), submitted = as.character(df[["_submission_time"]] %||% NA),
    check = check, severity = severity,
    value = if (!is.null(value_col)) as.character(df[[value_col]]) else NA_character_)
}
ID_PATTERN <- "^GA-[0-9A-F]{8}$"; EVENT_PATTERN <- "^GA-[0-9A-F]{8}-E[0-9A-F]{6}$|^AE-"
to_num  <- function(x) suppressWarnings(as.numeric(x))
to_date <- function(x) suppressWarnings(as.Date(substr(x, 1, 10)))
to_dt   <- function(x) suppressWarnings(lubridate::ymd_hms(x, tz = LOCAL_TZ, quiet = TRUE))
yes     <- function(x) !is.na(x) & x == "yes"
n_dropped_test <- c()

# Steps common to all forms: types, IDs, metadata, training records, re-uploads
clean_common <- function(df, form, name_col = NULL, note_col = NULL) {
  # Works on empty forms too (pilot start): the columns exist, so every step below runs on zero rows
  df <- ensure_cols(df, c("_id", "_uuid", "_submission_time", "start_time", "end_time", "patient_id", "__version__"))
  df <- df |> mutate(`_id` = as.character(`_id`), submission_time = to_dt(`_submission_time`),
                     submission_date = as.Date(submission_time), start_dt = to_dt(start_time), end_dt = to_dt(end_time),
                     duration_min = as.numeric(difftime(end_dt, start_dt, units = "mins")),
                     patient_id = toupper(stringr::str_squish(patient_id)), form_version = `__version__`)
  if (DROP_TEST) {
    t <- rep(FALSE, nrow(df))
    if (!is.null(name_col)) t <- t | stringr::str_detect(coalesce(df[[name_col]], ""), "^TEST\\b")
    if (!is.null(note_col)) t <- t | stringr::str_detect(coalesce(df[[note_col]], ""), "^TEST RECORD")
    n_dropped_test[form] <<- sum(t); df <- df[!t, , drop = FALSE]
  }
  add_issue(df |> group_by(`_uuid`) |> filter(n() > 1) |> ungroup(), form, "Same submission uploaded more than once (kept latest)", "Minor")
  df <- df |> arrange(desc(submission_time)) |> distinct(`_uuid`, .keep_all = TRUE)
  add_issue(df |> filter(!is.na(patient_id), !stringr::str_detect(patient_id, ID_PATTERN)), form, "Patient ID not in GA-XXXXXXXX format", "Critical", "patient_id")
  df
}

# ---- Form 1: registration ----
# Patient IDs of training registrations (name starts with TEST or comment starts with TEST RECORD),
# used to drop their follow-up visits and ADR reports too, even when those notes do not say TEST
test_ids <- if (nrow(reg_raw) == 0) character(0) else ensure_cols(reg_raw, c("patient_name", "comments", "patient_id")) |>
  filter(stringr::str_detect(coalesce(patient_name, ""), "^TEST\\b") | stringr::str_detect(coalesce(comments, ""), "^TEST RECORD")) |>
  pull(patient_id) |> stringr::str_squish() |> toupper() |> unique()

reg <- reg_raw |>
  ensure_cols(c("district","facility","facility_cem","registration_date","patient_name","birth_year","patient_id","phone",
                "collector","sex","age_months","weight","test_type","rdt_result","micro_result","micro_species","density",
                "parasite_stages","severity","pf_positive","pq_allergy","pregnant","breastfeeding_under1","severe_anaemia",
                "haemolysis","other_exclusion","screen_status","provider_eligibility","act_type","sldp_status","no_sldp_reason",
                "sldp_dose_mg","sldp_date","sldp_dot","with_food","counselled","sldp_refusal_specific","cem_consent",
                "cem_enrolled","comments","start_time","end_time","_submission_time","_uuid","__version__","device_id",
                "patient_insured","alternative_contact_phone")) |>
  clean_common("Form 1", name_col = "patient_name", note_col = "comments") |>
  mutate(registration_date = to_date(registration_date), sldp_date = to_date(sldp_date),
         birth_year = to_num(birth_year), age_months = to_num(age_months), age_years = age_months / 12,
         weight = to_num(weight), sldp_dose_mg = to_num(sldp_dose_mg),
         sldp_given = sldp_status == "given",
         mg_per_kg = if_else(sldp_given & weight > 0, sldp_dose_mg / weight, NA_real_),
         cem_site = facility_cem == "yes", cem_in = yes(cem_consent) & yes(cem_enrolled),
         age_group = cut(age_years, c(-Inf, 5, 15, 25, 45, Inf), right = FALSE, labels = c("<5", "5-14", "15-24", "25-44", "45+")),
         reg_week = lubridate::floor_date(registration_date, "week", week_start = 1)) |>
  apply_codebook(cb_reg)

add_issue(reg |> filter(registration_date > TODAY | registration_date < PILOT_START), "Form 1", "Registration date outside pilot period or in the future", "Major", "registration_date")
add_issue(reg |> filter(sldp_given & is.na(sldp_date)), "Form 1", "SLDP given but no SLDP date", "Critical")
add_issue(reg |> filter(sldp_given & sldp_date < registration_date), "Form 1", "SLDP date before registration", "Major", "sldp_date")
add_issue(reg |> filter(sldp_given & (sldp_dose_mg <= 0 | sldp_dose_mg > SLDP_MAX_MG)), "Form 1", "SLDP dose outside 0-15 mg", "Critical", "sldp_dose_mg")
add_issue(reg |> filter(sldp_given & is.na(weight)), "Form 1", "SLDP given without a weight", "Major")
add_issue(reg |> filter(!is.na(mg_per_kg) & (mg_per_kg < SLDP_DOSE_LOW | mg_per_kg > SLDP_DOSE_HIGH)), "Form 1", "SLDP dose outside 0.15-0.4 mg/kg", "Major", "mg_per_kg")
add_issue(reg |> filter(sldp_given & screen_status == "not_eligible"), "Form 1", "SLDP given although screening result was 'not eligible'", "Critical", "screen_status")
add_issue(reg |> filter(sldp_given & age_months < 12), "Form 1", "SLDP given to a child under 12 months", "Critical", "age_months")
add_issue(reg |> filter(cem_in & is.na(phone)), "Form 1", "CEM-enrolled patient has no phone number", "Major")
add_issue(reg |> filter(!is.na(duration_min) & (duration_min < MIN_INTERVIEW_MIN | duration_min > MAX_INTERVIEW_MIN)), "Form 1", "Interview duration implausible", "Minor", "duration_min")
# Duplicates: same ID twice; or probably the same person under two IDs
add_issue(reg |> group_by(patient_id) |> filter(n() > 1) |> ungroup(), "Form 1", "Patient ID registered more than once", "Critical")
reg <- reg |> mutate(name_key = stringr::str_to_lower(stringr::str_replace_all(coalesce(patient_name, ""), "[^A-Za-z ]", "")) |>
                       stringr::str_squish() |> stringr::str_split(" ") |> purrr::map_chr(~ paste(sort(.x), collapse = " ")))
add_issue(reg |> filter(name_key != "") |> group_by(name_key, birth_year, sex, facility) |> filter(n_distinct(patient_id) > 1) |> ungroup(),
          "Form 1", "Probable duplicate person (same name, birth year, sex, facility; different IDs)", "Major")
reg <- reg |> arrange(submission_time) |> distinct(patient_id, .keep_all = TRUE)

# ---- Form 2: CEM visits ----
cem <- cem_raw |>
  ensure_cols(c("district","facility","patient_id","registration_matches","visit_type","visit_date","contact_mode","day0_date",
                "baseline_source","days_since_baseline","scheduled_offset","contact_success","attempt_number","contact_failure",
                "dark_urine_d3","temperature","jaundice","fbc_status","hb","hb_alert","dark_urine","pallor_report",
                "jaundice_report","fatigue","dizziness","fever","nausea","vomiting","rash","ae_reported","ae_kind",
                "linked_existing_ae_id","participant_status","status_date","d3_repeat_test_done","d3_rdt_result",
                "d3_microscopy_result","d3_microscopy_stages","assessment_completed","focal_person","phone_notes",
                "baseline_notes","exam_notes","start_time","end_time","_submission_time","_uuid","__version__","device_id")) |>
  clean_common("Form 2", note_col = "phone_notes") |>
  mutate(visit_date = to_date(visit_date), day0_date = to_date(day0_date), status_date = to_date(status_date),
         days_since_baseline = to_num(days_since_baseline), scheduled_offset = to_num(scheduled_offset),
         timing_deviation = days_since_baseline - scheduled_offset, attempt_number = to_num(attempt_number),
         across(c(hb, temperature), to_num), completed = assessment_completed == "yes",
         phone_visit = visit_type %in% c("d7", "d14", "d28") | (visit_type == "unscheduled" & contact_mode == "phone"),
         reg_matched = to_num(registration_matches) == 1) |>
  apply_codebook(cb_cem)
if (DROP_TEST) cem <- cem |> filter(!stringr::str_detect(coalesce(baseline_notes, ""), "^TEST RECORD"),
                                     !stringr::str_detect(coalesce(exam_notes, ""), "^TEST RECORD"))
# Training cascade: also drop visits of patients registered as TEST records
if (DROP_TEST) cem <- cem |> filter(!patient_id %in% test_ids)
add_issue(cem |> filter(!reg_matched | !patient_id %in% reg$patient_id), "Form 2", "Visit not linked to a registration (orphan visit)", "Critical")
add_issue(cem |> filter(visit_date > TODAY), "Form 2", "Visit date in the future", "Major", "visit_date")
add_issue(cem |> filter(!is.na(day0_date) & visit_date < day0_date), "Form 2", "Visit before baseline date", "Major", "visit_date")
add_issue(cem |> filter(baseline_source == "manual"), "Form 2", "Baseline date entered manually (verify against treatment record)", "Minor", "day0_date")
add_issue(cem |> filter(!is.na(timing_deviation) & abs(timing_deviation) > 7 & visit_type != "unscheduled"), "Form 2", "Visit more than 7 days from schedule", "Minor", "timing_deviation")
add_issue(cem |> filter(!is.na(hb) & (hb < 3 | hb > 20)), "Form 2", "Haemoglobin outside 3-20 g/dL (check units)", "Major", "hb")
add_issue(cem |> filter(completed, visit_type != "unscheduled") |> group_by(patient_id, visit_type) |> filter(n() > 1) |> ungroup(),
          "Form 2", "More than one completed visit of the same scheduled type", "Major", "visit_type")
cem <- cem |> left_join(reg |> select(patient_id, reg_facility = facility), by = "patient_id")
add_issue(cem |> filter(!is.na(reg_facility) & facility != reg_facility), "Form 2", "Visit facility differs from registration facility (confirm transfer)", "Minor", "facility")

# ---- Form 3: ADR / SAE ----
adr <- adr_raw |>
  ensure_cols(c("district","facility","report_date","patient_id","registration_matches","event_record_type","permanent_ae_id",
                "event_onset","event_description","intensity","serious","serious_criteria","outcome","resolved_date",
                "hospitalized","suspected_study_drugs","pv_submitted","reporter_name","start_time","end_time",
                "_submission_time","_uuid","__version__","device_id")) |>
  clean_common("Form 3", note_col = "event_description") |>
  mutate(report_date = to_date(report_date), event_onset = to_date(event_onset), resolved_date = to_date(resolved_date),
         report_lag_days = as.numeric(report_date - event_onset), is_serious = serious == "yes",
         reg_matched = to_num(registration_matches) == 1, permanent_ae_id = toupper(stringr::str_squish(permanent_ae_id))) |>
  apply_codebook(cb_adr)
# Training cascade: also drop ADR reports of patients registered as TEST records
if (DROP_TEST) adr <- adr |> filter(!patient_id %in% test_ids)
add_issue(adr |> filter(!reg_matched), "Form 3", "ADR patient not found in registration (reconcile)", "Major")
add_issue(adr |> filter(!stringr::str_detect(coalesce(permanent_ae_id, ""), EVENT_PATTERN)), "Form 3", "Event ID missing or malformed", "Critical", "permanent_ae_id")
add_issue(adr |> filter(stringr::str_detect(coalesce(permanent_ae_id, ""), "^GA-") & substr(permanent_ae_id, 1, 11) != patient_id), "Form 3", "Event ID does not start with this Patient ID", "Critical", "permanent_ae_id")
add_issue(adr |> filter(event_record_type == "update" & !permanent_ae_id %in% adr$permanent_ae_id[adr$event_record_type == "new"]), "Form 3", "Update refers to an event with no 'new' report", "Major", "permanent_ae_id")
add_issue(adr |> filter(is_serious & !yes(pv_submitted)), "Form 3", "Serious event not yet submitted to pharmacovigilance", "Critical")
add_issue(adr |> filter(!is.na(report_lag_days) & report_lag_days > ADR_REPORT_LAG_TARGET), "Form 3", paste0("Reported more than ", ADR_REPORT_LAG_TARGET, " days after onset"), "Minor", "report_lag_days")

# ---- Cross-form reconciliation ----
add_issue(cem |> filter(ae_kind %in% c("ongoing", "changed") & !toupper(linked_existing_ae_id) %in% adr$permanent_ae_id), "Form 2", "Ongoing/changed event ID not found in Form 3 reports", "Major", "linked_existing_ae_id")
first_adr <- adr |> group_by(patient_id) |> summarise(first_adr_report = min(report_date, na.rm = TRUE), .groups = "drop")
add_issue(cem |> filter(ae_kind == "new" | hb_alert == "yes") |> left_join(first_adr, by = "patient_id") |>
            filter(is.na(first_adr_report) | first_adr_report > visit_date + ADR_RECONCILE_DAYS),
          "Form 2", paste0("New event or Hb alert without a Form 3 report within ", ADR_RECONCILE_DAYS, " days"), "Critical")


# ============================================================================
# LINKAGE-----------------------------------------------------------------
# ============================================================================
# ---- Event level: one row per Event ID (first report + latest update) ----
events <- adr |> filter(!is.na(permanent_ae_id)) |> arrange(permanent_ae_id, report_date, submission_time) |>
  group_by(event_id = permanent_ae_id) |>
  summarise(patient_id = first(patient_id), facility = first(facility),
            onset = suppressWarnings(min(event_onset, na.rm = TRUE)),
            first_report = min(report_date, na.rm = TRUE), n_reports = n(), description = first(event_description),
            max_intensity = c("mild", "moderate", "severe")[suppressWarnings(max(match(intensity, c("mild", "moderate", "severe")), na.rm = TRUE))],
            ever_serious = any(is_serious, na.rm = TRUE), criteria = paste(unique(na.omit(serious_criteria)), collapse = " "),
            final_outcome = last(outcome), sldp_suspected = any(stringr::str_detect(coalesce(suspected_study_drugs, ""), "sldp")),
            act_suspected = any(stringr::str_detect(coalesce(suspected_study_drugs, ""), "\\bact\\b")),
            hospitalised = any(hospitalized == "yes", na.rm = TRUE), pv_submitted = any(yes(pv_submitted)), .groups = "drop") |>
  mutate(onset = if_else(is.infinite(onset), as.Date(NA), onset), report_lag = as.numeric(first_report - onset))

# ---- Visit level: expected vs done for every enrolled patient ----
manual_day0 <- cem |> filter(baseline_source == "manual", !is.na(day0_date)) |> group_by(patient_id) |>
  summarise(manual_day0 = first(day0_date), .groups = "drop")
cohort <- reg |> filter(cem_in) |> left_join(manual_day0, by = "patient_id") |> mutate(day0 = coalesce(sldp_date, manual_day0))
add_issue(cohort |> filter(is.na(day0)), "Linkage", "Enrolled patient with no baseline date (no SLDP date and no manual baseline)", "Major")

visit_attempts <- cem |> filter(visit_type %in% VISIT_SCHEDULE$visit_type) |> group_by(patient_id, visit_type) |>
  summarise(n_attempts_failed = sum(!completed, na.rm = TRUE), completed = any(completed, na.rm = TRUE),
            completed_date = suppressWarnings(min(visit_date[completed], na.rm = TRUE)), last_attempt = max(visit_date, na.rm = TRUE), .groups = "drop") |>
  mutate(completed_date = if_else(is.infinite(completed_date), as.Date(NA), completed_date))
exits <- cem |> filter(participant_status %in% c("withdrawn", "lost", "died")) |> group_by(patient_id) |>
  summarise(exit_status = first(participant_status), exit_date = suppressWarnings(min(coalesce(status_date, visit_date), na.rm = TRUE)), .groups = "drop") |>
  mutate(exit_date = if_else(is.infinite(exit_date), as.Date(NA), exit_date))
lv <- c("Completed", "Due now", "Overdue", "Not yet due", "No baseline date", "Exited")
visit_status <- cohort |> select(patient_id, facility, facility_lbl, district, patient_name, phone, alternative_contact_phone, day0) |>
  tidyr::crossing(VISIT_SCHEDULE) |> mutate(due_date = day0 + offset, window_end = due_date + grace) |>
  left_join(visit_attempts, by = c("patient_id", "visit_type")) |> left_join(exits, by = "patient_id") |>
  mutate(completed = coalesce(completed, FALSE),
         status = case_when(completed ~ "Completed", is.na(day0) ~ "No baseline date",
                            !is.na(exit_date) & due_date > exit_date ~ "Exited",
                            TODAY < due_date ~ "Not yet due", TODAY <= window_end ~ "Due now", TRUE ~ "Overdue"),
         status = factor(status, lv), days_late_done = as.numeric(completed_date - due_date),
         visit_type = factor(visit_type, VISIT_SCHEDULE$visit_type))

# ---- Patient level ----
hb_wide <- cem |> filter(visit_type %in% c("d0", "d3"), !is.na(hb)) |> arrange(submission_time) |>
  group_by(patient_id, visit_type) |> summarise(hb = last(hb), .groups = "drop") |>
  tidyr::pivot_wider(names_from = visit_type, values_from = hb, names_prefix = "hb_") |> ensure_cols(c("hb_d0", "hb_d3")) |>
  mutate(across(c(hb_d0, hb_d3), as.numeric))
symptom_vars <- c("dark_urine", "pallor_report", "jaundice_report", "fatigue", "dizziness", "fever", "nausea", "vomiting", "rash")
patients <- reg |> left_join(manual_day0, by = "patient_id") |> mutate(day0 = coalesce(sldp_date, manual_day0)) |>
  left_join(hb_wide, by = "patient_id") |>
  left_join(cem |> group_by(patient_id) |> summarise(haemolysis_sign = any(yes(dark_urine) | yes(jaundice_report) | yes(dark_urine_d3) | yes(jaundice)),
                                                     last_contact = suppressWarnings(max(visit_date[completed], na.rm = TRUE)),
                                                     first_form2_ae = suppressWarnings(min(visit_date[yes(ae_reported)], na.rm = TRUE)), .groups = "drop"), by = "patient_id") |>
  left_join(events |> group_by(patient_id) |> summarise(n_events = n(), any_serious = any(ever_serious),
                                                        first_onset = suppressWarnings(min(onset, na.rm = TRUE)), .groups = "drop"), by = "patient_id") |>
  left_join(visit_status |> filter(visit_type == "d28") |> select(patient_id, d28_done = completed), by = "patient_id") |>
  left_join(exits, by = "patient_id") |>
  mutate(across(c(last_contact, first_form2_ae, first_onset), ~ if_else(is.infinite(.x), as.Date(NA), .x)),
         n_events = coalesce(n_events, 0L), any_event = n_events > 0 | !is.na(first_form2_ae),
         any_serious = coalesce(any_serious, FALSE), hb_change = hb_d3 - hb_d0,
         hb_drop_alert = !is.na(hb_change) & -hb_change >= HB_DROP_ALERT,
         first_ae_date = pmin(first_onset, first_form2_ae, na.rm = TRUE), d28_done = coalesce(d28_done, FALSE))

issue_template <- tibble(form = character(), record_id = character(), patient_id = character(), facility = character(),
                         submitted = character(), check = character(), severity = character(), value = character())
issue_log <- bind_rows(issue_template, bind_rows(issues)) |> mutate(severity = factor(severity, c("Critical", "Major", "Minor"))) |> arrange(severity, form, facility, patient_id)


