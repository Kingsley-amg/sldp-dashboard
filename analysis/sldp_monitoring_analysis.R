# =============================================================================
#  SLDP PILOT, GREATER ACCRA: DATA MONITORING AND ANALYSIS PIPELINE
# =============================================================================
#
#  Author  : Kingsley Amegah (Data Scientist / Health Data Specialist)
#  Purpose : Pull the three linked KoboToolbox forms directly from the Kobo API,
#            clean and validate them, link them into patient-, visit- and
#            event-level datasets, produce operational monitoring outputs, and
#            run the analyses that answer the programme's objectives.
#
#  Forms (KoboToolbox EU server, https://eu.kobotoolbox.org)
#    Form 1  Patient Registration, Eligibility Screening and Treatment  (v2026110601)
#    Form 2  Cohort Event Monitoring (CEM) Follow-up                     (v2026110604)
#    Form 3  ADR / SAE Reporting                                         (v2026110603)
#
#  Programme objectives addressed (edit to match the approved protocol)
#    O1  Coverage: proportion of eligible Pf patients who received SLDP, and
#        the factors associated with receiving it.
#    O2  Acceptability: refusal and non-administration, with patients' reasons;
#        directly observed therapy (DOT).
#    O3  Screening and dosing quality: agreement between calculated and
#        clinician eligibility; dose in mg/kg against the 0.25 mg/kg target.
#    O4  Safety: adverse event incidence, haemoglobin change Day 0 to Day 3,
#        haemolysis signs, serious adverse events, time to first event.
#    O5  CEM performance: follow-up completeness, timeliness, retention to Day 28.
#    O6  Parasitological follow-up: Day 3 positivity and gametocyte carriage.
#    O7  Data quality and system performance (linkage, duplicates, timeliness).
#
#  HOW TO RUN
#    1. Get your Kobo API token: Kobo > Account settings > Security > API key.
#    2. Store it outside this script, in your user .Renviron file:
#           usethis::edit_r_environ()          # opens the file
#           KOBO_TOKEN=paste_your_token_here   # add this line, save, restart R
#       Never write the token in this script or share it.
#    3. Check the CONFIGURATION section below (asset UIDs, dates, thresholds).
#    4. Run the whole script:  source("sldp_monitoring_analysis.R")
#       Outputs are written to ./outputs/<run date>/.
#    5. No internet? Set USE_CACHE <- TRUE to re-use the last downloaded data.
#
#  OUTPUTS
#    outputs/<date>/SLDP_monitoring_<date>.xlsx   monitoring indicators, overdue
#                                                 visit list, data issue log,
#                                                 ADR line list, analysis tables
#    outputs/<date>/figures/*.png                 monitoring and analysis charts
#    outputs/<date>/analysis_datasets.rds         cleaned, linked datasets
#    data/raw/*.json                              raw API snapshots (audit trail)
#
#  Requires R 4.1 or later (uses the native pipe |>).
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------
# Install any missing package once, then load everything quietly.
required_packages <- c(
  "httr2",     # API requests to KoboToolbox
  "jsonlite",  # reading and writing JSON snapshots
  "dplyr", "tidyr", "purrr", "stringr", "tibble", "forcats",  # data wrangling
  "lubridate", # dates
  "ggplot2", "scales",  # charts
  "openxlsx",  # Excel output for the data manager and supervisors
  "survival",  # Kaplan-Meier and Cox models
  "broom",     # tidy model output
  "lme4"       # mixed-effects models (facility clustering, repeated Hb)
)
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
invisible(lapply(required_packages, function(p) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))


# -----------------------------------------------------------------------------
# 1. CONFIGURATION: the only section that normally needs editing
# -----------------------------------------------------------------------------
KOBO_SERVER <- "https://eu.kobotoolbox.org"   # Kobo EU server (kpi)
KOBO_TOKEN  <- Sys.getenv("KOBO_TOKEN")        # read from .Renviron, never typed here

# Asset UIDs of the three projects (from the project URL in Kobo)
ASSETS <- c(
  registration = "a58q4HmKUXX6C28cUNVUd9",   # Form 1
  cem          = "azukUtLnBWCCZRmP6bVSm8",   # Form 2
  adr          = "aM6nCEVRheMGbVAKDGUT8i"    # Form 3
)

USE_CACHE      <- FALSE          # TRUE = use the last saved snapshot, no download
DROP_TEST      <- TRUE           # drop training records (names or notes starting TEST)
LOCAL_TZ       <- "Africa/Accra"
TODAY          <- as.Date(lubridate::with_tz(Sys.time(), LOCAL_TZ))
PILOT_START    <- as.Date("2026-10-01")   # first possible registration date

# Clinical and operational thresholds (keep in line with the forms and protocol)
SLDP_TARGET_MG_PER_KG <- 0.25
SLDP_DOSE_LOW         <- 0.15     # mg/kg soft lower bound used by Form 1
SLDP_DOSE_HIGH        <- 0.40     # mg/kg soft upper bound used by Form 1
SLDP_MAX_MG           <- 15       # absolute maximum dose
HB_DROP_ALERT         <- 2        # g/dL fall from Day 0 that defines a safety signal
ADR_RECONCILE_DAYS    <- 2        # an Hb alert should have a Form 3 report within this
ADR_REPORT_LAG_TARGET <- 2        # days from onset to report (timeliness indicator)
MIN_INTERVIEW_MIN     <- 3        # Form 1 interviews shorter than this are flagged
MAX_INTERVIEW_MIN     <- 120      # ... and longer than this

# Scheduled CEM visits: day offset from Day 0 and the grace window (days) after
# the due date before a visit counts as overdue.
VISIT_SCHEDULE <- tibble::tribble(
  ~visit_type, ~offset, ~grace, ~mode,
  "d0",          0,      1,     "In person",
  "d3",          3,      2,     "In person",
  "d7",          7,      3,     "Telephone",
  "d14",        14,      3,     "Telephone",
  "d28",        28,      5,     "Telephone"
)

# Folders
RUN_LABEL <- format(TODAY, "%Y-%m-%d")
DIR_RAW   <- file.path("data", "raw")
DIR_OUT   <- file.path("outputs", RUN_LABEL)
DIR_FIG   <- file.path(DIR_OUT, "figures")
for (d in c(DIR_RAW, DIR_OUT, DIR_FIG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# A quiet, consistent chart theme for every figure
theme_set(
  theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          plot.title.position = "plot",
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
)
PAL <- c("#2F5D7C", "#B4612F", "#6E8797", "#C9A227", "#4F7F5F", "#8E3B22")

log_msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)


# -----------------------------------------------------------------------------
# 2. PULL DATA FROM KOBOTOOLBOX
# -----------------------------------------------------------------------------
# kobo_request(): one authenticated GET with automatic retries for flaky
# networks. Returns the parsed JSON as an R list.
kobo_request <- function(url) {
  if (!nzchar(KOBO_TOKEN)) {
    stop("KOBO_TOKEN is not set. Add it to your .Renviron file (see HOW TO RUN).")
  }
  request(url) |>
    req_headers(Authorization = paste("Token", KOBO_TOKEN),
                Accept = "application/json") |>
    req_retry(max_tries = 4, backoff = function(i) 2^i) |>
    req_timeout(120) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
}

# kobo_pull_submissions(): every submission of one project, page by page.
# The v2 data endpoint returns at most `limit` records per page plus a `next`
# link; we follow `next` until it is empty.
kobo_pull_submissions <- function(uid, page_size = 1000) {
  url <- sprintf("%s/api/v2/assets/%s/data.json?limit=%d", KOBO_SERVER, uid, page_size)
  records <- list()
  while (!is.null(url)) {
    page    <- kobo_request(url)
    records <- c(records, page$results)
    url     <- page[["next"]]
    log_msg("  ", length(records), " of ", page$count, " records")
  }
  records
}

# kobo_pull_form(): the form definition (survey rows and choice lists), used to
# turn codes into labels and to know which questions are select_multiple.
kobo_pull_form <- function(uid) {
  asset <- kobo_request(sprintf("%s/api/v2/assets/%s.json", KOBO_SERVER, uid))
  list(name = asset$name, version = asset$deployed_version_id,
       survey = asset$content$survey, choices = asset$content$choices)
}

# Download (or reload) all three forms. Each download is saved as a dated
# snapshot so that every report can be reproduced from exactly the same data.
load_all_forms <- function() {
  out <- list()
  for (form in names(ASSETS)) {
    latest <- file.path(DIR_RAW, paste0(form, "_latest.json"))
    if (USE_CACHE) {
      if (!file.exists(latest)) stop("No cached snapshot for ", form, ". Run once with USE_CACHE <- FALSE.")
      log_msg("Reading cached ", form)
      out[[form]] <- jsonlite::read_json(latest, simplifyVector = FALSE)
    } else {
      log_msg("Downloading ", form, " (", ASSETS[[form]], ")")
      snap <- list(pulled_at = format(Sys.time(), tz = LOCAL_TZ, usetz = TRUE),
                   form = kobo_pull_form(ASSETS[[form]]),
                   records = kobo_pull_submissions(ASSETS[[form]]))
      stamp <- format(Sys.time(), "%Y%m%d_%H%M")
      jsonlite::write_json(snap, file.path(DIR_RAW, sprintf("%s_%s.json", form, stamp)),
                           auto_unbox = TRUE, null = "null")
      jsonlite::write_json(snap, latest, auto_unbox = TRUE, null = "null")
      out[[form]] <- snap
    }
  }
  out
}

raw <- load_all_forms()


# -----------------------------------------------------------------------------
# 3. FLATTEN THE JSON INTO TABLES
# -----------------------------------------------------------------------------
# Kobo keys carry the group path ("site/patient_id", "monitor/review/..."). XLSForm
# names are unique within a form, so the last path segment is a safe column name.
short_name <- function(x) sub(".*/", "", x)

# Is this value a repeat group (a list of named lists)?
is_repeat <- function(v) is.list(v) && length(v) > 0 && all(vapply(v, function(e) is.list(e) && !is.null(names(e)), logical(1)))

# One record -> one row of scalar fields (repeats, attachments, tags removed).
flatten_record <- function(rec) {
  keep <- rec[!vapply(rec, is.list, logical(1))]
  keep <- lapply(keep, function(v) if (is.null(v)) NA_character_ else as.character(v))
  names(keep) <- short_name(names(keep))
  tibble::as_tibble(keep)
}

# One repeat group across all records -> a long child table keyed by parent _id.
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

to_table <- function(snapshot) {
  recs <- snapshot$records
  if (length(recs) == 0) return(tibble())
  purrr::map_dfr(recs, flatten_record)
}

reg_raw  <- to_table(raw$registration)
cem_raw  <- to_table(raw$cem)
adr_raw  <- to_table(raw$adr)
cem_meds <- flatten_repeat(raw$cem$records, "current_medicines")   # Form 2 Day 0 medicines
adr_meds <- flatten_repeat(raw$adr$records, "suspected_drugs")     # Form 3 other medicines
log_msg("Rows: registration ", nrow(reg_raw), " | CEM visits ", nrow(cem_raw), " | ADR reports ", nrow(adr_raw))

# Guarantee that every column the pipeline uses exists, even before the first
# record of that kind arrives (for example no SAE yet means no 'stabilized').
ensure_cols <- function(df, cols) {
  for (c in setdiff(cols, names(df))) df[[c]] <- NA_character_
  df
}


# -----------------------------------------------------------------------------
# 4. CODEBOOK: labels for coded answers
# -----------------------------------------------------------------------------
# Build a lookup of (question -> choice list) and (list, code -> label) from the
# form definition, then add readable *_lbl columns and 0/1 dummies for every
# select_multiple question (one column per option, e.g. parasite_stages__gametocytes).
build_codebook <- function(form) {
  q <- purrr::map_dfr(form$survey, function(r) {
    tibble(name = r$name %||% NA_character_,
           type = r$type %||% NA_character_,
           list_name = r$select_from_list_name %||% NA_character_)
  }) |> filter(!is.na(name), type %in% c("select_one", "select_multiple"))
  ch <- purrr::map_dfr(form$choices, function(c) {
    lab <- c$label; if (is.list(lab)) lab <- lab[[1]]
    tibble(list_name = c$list_name, code = as.character(c$name), label = as.character(lab %||% c$name))
  })
  list(questions = q, choices = ch)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

apply_codebook <- function(df, cb) {
  for (i in seq_len(nrow(cb$questions))) {
    v <- cb$questions$name[i]; if (!v %in% names(df)) next
    opts <- cb$choices |> filter(list_name == cb$questions$list_name[i])
    if (cb$questions$type[i] == "select_one") {
      df[[paste0(v, "_lbl")]] <- opts$label[match(df[[v]], opts$code)]
    } else {
      for (code in opts$code) {
        df[[paste0(v, "__", code)]] <- as.integer(stringr::str_detect(
          coalesce(df[[v]], ""), paste0("(^|\\s)", stringr::fixed(code), "($|\\s)") |> as.character()))
        df[[paste0(v, "__", code)]][is.na(df[[v]])] <- NA_integer_
      }
    }
  }
  df
}

cb_reg <- build_codebook(raw$registration$form)
cb_cem <- build_codebook(raw$cem$form)
cb_adr <- build_codebook(raw$adr$form)


# -----------------------------------------------------------------------------
# 5. CLEANING
# -----------------------------------------------------------------------------
# Every problem found is written to one issue log, so the data manager gets a
# single query list instead of scattered warnings.
issues <- list()
add_issue <- function(df, form, check, severity, value_col = NULL, detail = NULL) {
  if (nrow(df) == 0) return(invisible(NULL))
  issues[[length(issues) + 1]] <<- tibble(
    form = form,
    record_id = as.character(df[["_id"]]),
    patient_id = as.character(df[["patient_id"]] %||% NA),
    facility = as.character(df[["facility"]] %||% NA),
    submitted = as.character(df[["_submission_time"]] %||% NA),
    check = check,
    severity = severity,   # "Critical" = wrong data or lost linkage; "Major"; "Minor"
    value = if (!is.null(value_col)) as.character(df[[value_col]]) else NA_character_,
    detail = detail %||% NA_character_
  )
}

ID_PATTERN    <- "^GA-[0-9A-F]{8}$"
EVENT_PATTERN <- "^GA-[0-9A-F]{8}-E[0-9A-F]{6}$|^AE-"

to_num  <- function(x) suppressWarnings(as.numeric(x))
to_date <- function(x) suppressWarnings(as.Date(substr(x, 1, 10)))
to_dt   <- function(x) suppressWarnings(lubridate::ymd_hms(x, tz = LOCAL_TZ, quiet = TRUE))
yes     <- function(x) !is.na(x) & x == "yes"

# Common steps for any form: types, IDs, metadata, training records, exact resubmissions
clean_common <- function(df, form, name_col = NULL, note_col = NULL) {
  # Works on empty forms too (pilot start): the columns exist, so every step below runs on zero rows
  df <- ensure_cols(df, c("_id", "_uuid", "_submission_time", "start_time", "end_time", "patient_id", "__version__"))
  df <- df |>
    mutate(
      `_id` = as.character(`_id`),
      submission_time = to_dt(`_submission_time`),
      submission_date = as.Date(submission_time),
      start_dt = to_dt(start_time), end_dt = to_dt(end_time),
      duration_min = as.numeric(difftime(end_dt, start_dt, units = "mins")),
      patient_id = toupper(stringr::str_squish(patient_id)),
      form_version = `__version__`
    )
  # Training records are removed before any indicator is calculated
  if (DROP_TEST) {
    is_test <- rep(FALSE, nrow(df))
    if (!is.null(name_col) && name_col %in% names(df)) is_test <- is_test | stringr::str_detect(coalesce(df[[name_col]], ""), "^TEST\\b")
    if (!is.null(note_col) && note_col %in% names(df)) is_test <- is_test | stringr::str_detect(coalesce(df[[note_col]], ""), "^TEST RECORD")
    if (any(is_test)) log_msg(form, ": dropped ", sum(is_test), " training records")
    df <- df[!is_test, , drop = FALSE]
  }
  # The same submission (same instance UUID) uploaded twice: keep the latest
  dup_uuid <- df |> group_by(`_uuid`) |> filter(n() > 1) |> ungroup()
  add_issue(dup_uuid, form, "Same submission uploaded more than once (kept latest)", "Minor")
  df <- df |> arrange(desc(submission_time)) |> distinct(`_uuid`, .keep_all = TRUE)
  # Patient ID format
  add_issue(df |> filter(!is.na(patient_id), !stringr::str_detect(patient_id, ID_PATTERN)),
            form, "Patient ID not in GA-XXXXXXXX format", "Critical", "patient_id")
  df
}

# ---- 5a. Form 1: registration --------------------------------------------------
reg <- reg_raw |>
  ensure_cols(c("district","facility","facility_cem","registration_date","patient_name","birth_year",
                "patient_id","phone","collector","sex","age_unit","age_value","age_months","weight",
                "test_type","rdt_result","micro_result","micro_species","density","parasite_stages",
                "diagnosis","severity","pf_positive","pq_allergy","pregnant","breastfeeding_under1",
                "severe_anaemia","haemolysis","other_exclusion","screen_status","provider_eligibility",
                "provider_reason","act_type","act_start","act_dot","sldp_status","no_sldp_reason",
                "sldp_dose_mg","tablet_strength","tablet_count","sldp_date","sldp_dot","with_food",
                "counselled","sldp_refusal_specific","sldp_dot_reasons","cem_consent","cem_enrolled",
                "ae_at_visit","comments","start_time","end_time","_submission_time","_uuid","__version__","device_id",
                "facility_accepts_insurance","patient_insured","insurance_type")) |>
  clean_common("Form 1", name_col = "patient_name", note_col = "comments") |>
  mutate(
    registration_date = to_date(registration_date),
    sldp_date   = to_date(sldp_date),
    act_start   = to_date(act_start),
    birth_year  = to_num(birth_year),
    age_months  = to_num(age_months),
    age_years   = age_months / 12,
    weight      = to_num(weight),
    density     = to_num(density),
    sldp_dose_mg = to_num(sldp_dose_mg),
    tablet_count = to_num(tablet_count),
    sldp_given  = sldp_status == "given",
    mg_per_kg   = if_else(sldp_given & weight > 0, sldp_dose_mg / weight, NA_real_),
    cem_site    = facility_cem == "yes",
    cem_in      = yes(cem_consent) & yes(cem_enrolled),
    age_group   = cut(age_years, c(-Inf, 5, 15, 25, 45, Inf), right = FALSE,
                      labels = c("<5", "5-14", "15-24", "25-44", "45+")),
    reg_week    = lubridate::floor_date(registration_date, "week", week_start = 1)
  ) |>
  apply_codebook(cb_reg)

# Range and logic checks that the form already enforces are re-run here, because
# records edited in Kobo or entered on older form versions can bypass them.
add_issue(reg |> filter(registration_date > TODAY | registration_date < PILOT_START),
          "Form 1", "Registration date outside pilot period or in the future", "Major", "registration_date")
add_issue(reg |> filter(sldp_given & is.na(sldp_date)), "Form 1", "SLDP given but no SLDP date", "Critical")
add_issue(reg |> filter(sldp_given & sldp_date < registration_date), "Form 1", "SLDP date before registration", "Major", "sldp_date")
add_issue(reg |> filter(sldp_given & (sldp_dose_mg <= 0 | sldp_dose_mg > SLDP_MAX_MG)),
          "Form 1", "SLDP dose outside 0-15 mg", "Critical", "sldp_dose_mg")
add_issue(reg |> filter(sldp_given & is.na(weight)), "Form 1", "SLDP given without a weight", "Major")
add_issue(reg |> filter(!is.na(mg_per_kg) & (mg_per_kg < SLDP_DOSE_LOW | mg_per_kg > SLDP_DOSE_HIGH)),
          "Form 1", "SLDP dose outside 0.15-0.4 mg/kg", "Major", "mg_per_kg")
add_issue(reg |> filter(sldp_given & screen_status == "not_eligible"),
          "Form 1", "SLDP given although screening result was 'not eligible'", "Critical", "screen_status")
add_issue(reg |> filter(sldp_given & age_months < 12), "Form 1", "SLDP given to a child under 12 months", "Critical", "age_months")
add_issue(reg |> filter(cem_in & is.na(phone)), "Form 1", "CEM-enrolled patient has no phone number", "Major")
add_issue(reg |> filter(!is.na(duration_min) & (duration_min < MIN_INTERVIEW_MIN | duration_min > MAX_INTERVIEW_MIN)),
          "Form 1", "Interview duration implausible", "Minor", "duration_min")

# Duplicate registrations: (a) the same Patient ID twice, (b) probably the same
# person registered twice with different IDs (same normalised name, birth year,
# sex and facility). (b) is a query for the data manager, not an automatic merge.
dup_id <- reg |> group_by(patient_id) |> filter(n() > 1) |> ungroup()
add_issue(dup_id, "Form 1", "Patient ID registered more than once", "Critical")
reg <- reg |>
  mutate(name_key = stringr::str_to_lower(stringr::str_replace_all(coalesce(patient_name, ""), "[^A-Za-z ]", "")) |>
           stringr::str_squish() |> stringr::str_split(" ") |> purrr::map_chr(~ paste(sort(.x), collapse = " ")))
probable_dup <- reg |> filter(name_key != "") |>
  group_by(name_key, birth_year, sex, facility) |> filter(n_distinct(patient_id) > 1) |> ungroup()
add_issue(probable_dup, "Form 1", "Probable duplicate person (same name, birth year, sex, facility; different IDs)", "Major")
# For analysis keep the earliest registration of each Patient ID
reg <- reg |> arrange(submission_time) |> distinct(patient_id, .keep_all = TRUE)

# ---- 5b. Form 2: CEM visits ----------------------------------------------------
cem <- cem_raw |>
  ensure_cols(c("district","facility","patient_id","registration_matches","linked_sldp_date","visit_type",
                "visit_date","contact_mode","inperson_completed","day0_date","baseline_source",
                "days_since_baseline","scheduled_offset","visit_deviation_reason","contact_success",
                "attempt_number","contact_failure","next_attempt","g6pd","presenting_symptoms","act_complete",
                "new_symptoms","dark_urine_d3","tachycardia_d3","temperature","pulse","pallor","jaundice",
                "clinical_anaemia","fbc_status","hb","baseline_hb","hb_decline","hb_alert","dark_urine",
                "pallor_report","jaundice_report","fatigue","dizziness","fever","nausea","vomiting","rash",
                "hospitalized_phone","hospitalized_d3","referred_phone","ae_reported","ae_kind",
                "linked_existing_ae_id","linked_event_patient","participant_status","status_date","day28_outcome",
                "d3_repeat_test_done","d3_malaria_test_type","d3_rdt_result","d3_microscopy_result",
                "d3_microscopy_species","d3_microscopy_stages","d3_parasite_density","d0_filter_paper_collected",
                "d3_filter_paper_collected","assessment_completed","is_scheduled","visit_key","visit_record_id",
                "focal_person","phone_notes","baseline_notes","exam_notes","inperson_incomplete_reason",
                "start_time","end_time","_submission_time","_uuid","__version__","device_id","linked_facility",
                "linked_registered_facility")) |>
  clean_common("Form 2", note_col = "phone_notes") |>
  mutate(
    visit_date   = to_date(visit_date),
    day0_date    = to_date(day0_date),
    status_date  = to_date(status_date),
    next_attempt = to_date(next_attempt),
    days_since_baseline = to_num(days_since_baseline),
    scheduled_offset    = to_num(scheduled_offset),
    timing_deviation    = days_since_baseline - scheduled_offset,
    attempt_number      = to_num(attempt_number),
    across(c(hb, baseline_hb, hb_decline, temperature, pulse, d3_parasite_density), to_num),
    completed  = assessment_completed == "yes",
    phone_visit = visit_type %in% c("d7", "d14", "d28") | (visit_type == "unscheduled" & contact_mode == "phone"),
    reg_matched = to_num(registration_matches) == 1
  ) |>
  apply_codebook(cb_cem)
# Drop the extra training visits that belong to training patients
if (DROP_TEST) cem <- cem |> filter(!stringr::str_detect(coalesce(baseline_notes, ""), "^TEST RECORD"),
                                     !stringr::str_detect(coalesce(exam_notes, ""), "^TEST RECORD"))

add_issue(cem |> filter(!reg_matched | !patient_id %in% reg$patient_id),
          "Form 2", "Visit not linked to a registration (orphan visit)", "Critical")
add_issue(cem |> filter(visit_date > TODAY), "Form 2", "Visit date in the future", "Major", "visit_date")
add_issue(cem |> filter(!is.na(day0_date) & visit_date < day0_date), "Form 2", "Visit before baseline date", "Major", "visit_date")
add_issue(cem |> filter(baseline_source == "manual"), "Form 2", "Baseline date entered manually (verify against treatment record)", "Minor", "day0_date")
add_issue(cem |> filter(!is.na(timing_deviation) & abs(timing_deviation) > 7 & visit_type != "unscheduled"),
          "Form 2", "Visit more than 7 days from schedule", "Minor", "timing_deviation")
add_issue(cem |> filter(!is.na(hb) & (hb < 3 | hb > 20)), "Form 2", "Haemoglobin outside 3-20 g/dL (check units)", "Major", "hb")
add_issue(cem |> filter(!is.na(temperature) & (temperature < 34 | temperature > 42)), "Form 2", "Temperature implausible", "Minor", "temperature")
dup_visit <- cem |> filter(completed, visit_type != "unscheduled") |> group_by(patient_id, visit_type) |> filter(n() > 1) |> ungroup()
add_issue(dup_visit, "Form 2", "More than one completed visit of the same scheduled type", "Major", "visit_type")

# Facility mismatch between the visit and the registration (transfer or error)
cem <- cem |> left_join(reg |> select(patient_id, reg_facility = facility), by = "patient_id")
add_issue(cem |> filter(!is.na(reg_facility) & facility != reg_facility),
          "Form 2", "Visit facility differs from registration facility (confirm transfer)", "Minor", "facility")

# ---- 5c. Form 3: ADR / SAE reports ---------------------------------------------
adr <- adr_raw |>
  ensure_cols(c("district","facility","report_date","patient_id","registration_matches","event_record_type",
                "existing_ae_id","permanent_ae_id","event_registry_patient","report_sex","report_age_months",
                "event_onset","event_description","intensity","serious","serious_criteria","outcome",
                "resolved_date","action_taken","hospitalized","suspected_study_drugs","other_suspected",
                "stabilized","transfused","referred","pv_submitted","pv_submission_date","reporter_name",
                "start_time","end_time","_submission_time","_uuid","__version__","device_id")) |>
  clean_common("Form 3", note_col = "event_description") |>
  mutate(
    report_date   = to_date(report_date),
    event_onset   = to_date(event_onset),
    resolved_date = to_date(resolved_date),
    pv_submission_date = to_date(pv_submission_date),
    report_lag_days = as.numeric(report_date - event_onset),
    report_age_months = to_num(report_age_months),
    is_serious = serious == "yes",
    reg_matched = to_num(registration_matches) == 1,
    permanent_ae_id = toupper(stringr::str_squish(permanent_ae_id))
  ) |>
  apply_codebook(cb_adr)
if (DROP_TEST) adr <- adr |> filter(!stringr::str_detect(coalesce(event_description, ""), "^TEST RECORD"))

add_issue(adr |> filter(!reg_matched), "Form 3", "ADR patient not found in registration (reconcile)", "Major")
add_issue(adr |> filter(!stringr::str_detect(coalesce(permanent_ae_id, ""), EVENT_PATTERN)),
          "Form 3", "Event ID missing or malformed", "Critical", "permanent_ae_id")
add_issue(adr |> filter(!is.na(permanent_ae_id) & stringr::str_detect(permanent_ae_id, "^GA-") &
                          substr(permanent_ae_id, 1, 11) != patient_id),
          "Form 3", "Event ID does not start with this Patient ID", "Critical", "permanent_ae_id")
add_issue(adr |> filter(event_record_type == "update" & !permanent_ae_id %in% adr$permanent_ae_id[adr$event_record_type == "new"]),
          "Form 3", "Update refers to an event with no 'new' report", "Major", "permanent_ae_id")
add_issue(adr |> filter(is_serious & (is.na(pv_submitted) | pv_submitted != "yes")),
          "Form 3", "Serious event not yet submitted to pharmacovigilance", "Critical")
add_issue(adr |> filter(!is.na(report_lag_days) & report_lag_days > ADR_REPORT_LAG_TARGET),
          "Form 3", paste0("Reported more than ", ADR_REPORT_LAG_TARGET, " days after onset"), "Minor", "report_lag_days")

# Form 2 events recorded as ongoing/changed whose Event ID is unknown to Form 3
add_issue(cem |> filter(ae_kind %in% c("ongoing", "changed") & !toupper(linked_existing_ae_id) %in% adr$permanent_ae_id),
          "Form 2", "Ongoing/changed event ID not found in Form 3 reports", "Major", "linked_existing_ae_id")
# Form 2 new events and Hb alerts that never got a Form 3 report
adr_by_patient <- adr |> group_by(patient_id) |> summarise(first_adr_report = min(report_date, na.rm = TRUE), .groups = "drop")
cem_needs_adr <- cem |> filter((ae_kind == "new") | hb_alert == "yes") |>
  left_join(adr_by_patient, by = "patient_id") |>
  filter(is.na(first_adr_report) | first_adr_report > visit_date + ADR_RECONCILE_DAYS)
add_issue(cem_needs_adr, "Form 2", paste0("New event or Hb alert without a Form 3 report within ", ADR_RECONCILE_DAYS, " days"), "Critical")

issue_template <- tibble(form = character(), record_id = character(), patient_id = character(), facility = character(),
                         submitted = character(), check = character(), severity = character(), value = character())
issue_log <- bind_rows(issue_template, bind_rows(issues)) |>
  mutate(severity = factor(severity, c("Critical", "Major", "Minor"))) |>
  arrange(severity, form, facility, patient_id)
log_msg("Data issues found: ", nrow(issue_log), " (", sum(issue_log$severity == "Critical"), " critical)")


# -----------------------------------------------------------------------------
# 6. LINKAGE: patient-, visit- and event-level analysis datasets
# -----------------------------------------------------------------------------
# 6a. Event level: one row per Event ID, combining the first report and the
#     latest update so that the final outcome and "ever serious" are known.
events <- adr |>
  filter(!is.na(permanent_ae_id)) |>
  arrange(permanent_ae_id, report_date, submission_time) |>
  group_by(event_id = permanent_ae_id) |>
  summarise(
    patient_id     = first(patient_id),
    facility       = first(facility),
    onset          = min(event_onset, na.rm = TRUE),
    first_report   = min(report_date, na.rm = TRUE),
    last_report    = max(report_date, na.rm = TRUE),
    n_reports      = n(),
    description    = first(event_description),
    max_intensity  = c("mild", "moderate", "severe")[max(match(intensity, c("mild", "moderate", "severe")), na.rm = TRUE)],
    ever_serious   = any(is_serious, na.rm = TRUE),
    criteria       = paste(unique(na.omit(serious_criteria)), collapse = " "),
    final_outcome  = last(outcome),
    resolved_date  = last(resolved_date),
    sldp_suspected = any(stringr::str_detect(coalesce(suspected_study_drugs, ""), "sldp")),
    act_suspected  = any(stringr::str_detect(coalesce(suspected_study_drugs, ""), "\\bact\\b")),
    hospitalised   = any(hospitalized == "yes", na.rm = TRUE),
    pv_submitted   = any(pv_submitted == "yes", na.rm = TRUE),
    report_lag     = as.numeric(first_report - onset),
    .groups = "drop"
  ) |>
  mutate(onset = if_else(is.infinite(onset), as.Date(NA), onset))

# 6b. Visit level: scheduled visit status per enrolled patient (expected vs done)
#     Day 0 for each patient = SLDP date, else the manual baseline from Form 2.
manual_day0 <- cem |> filter(baseline_source == "manual", !is.na(day0_date)) |>
  group_by(patient_id) |> summarise(manual_day0 = first(day0_date), .groups = "drop")
cohort <- reg |> filter(cem_in) |>
  left_join(manual_day0, by = "patient_id") |>
  mutate(day0 = coalesce(sldp_date, manual_day0))
add_issue_rows <- cohort |> filter(is.na(day0))
add_issue(add_issue_rows, "Linkage", "Enrolled patient with no baseline date (no SLDP date and no manual baseline)", "Major")
issue_template <- tibble(form = character(), record_id = character(), patient_id = character(), facility = character(),
                         submitted = character(), check = character(), severity = character(), value = character())
issue_log <- bind_rows(issue_template, bind_rows(issues)) |> mutate(severity = factor(severity, c("Critical", "Major", "Minor"))) |>
  arrange(severity, form, facility, patient_id)

visit_attempts <- cem |> filter(visit_type %in% VISIT_SCHEDULE$visit_type) |>
  group_by(patient_id, visit_type) |>
  summarise(n_submissions = n(), n_attempts_failed = sum(!completed, na.rm = TRUE),
            completed = any(completed, na.rm = TRUE),
            completed_date = suppressWarnings(min(visit_date[completed], na.rm = TRUE)),
            last_attempt = max(visit_date, na.rm = TRUE), .groups = "drop") |>
  mutate(completed_date = if_else(is.infinite(completed_date), as.Date(NA), completed_date))

visit_status <- cohort |>
  select(patient_id, facility, district, patient_name, phone, day0) |>
  tidyr::crossing(VISIT_SCHEDULE) |>
  mutate(due_date = day0 + offset, window_end = due_date + grace) |>
  left_join(visit_attempts, by = c("patient_id", "visit_type")) |>
  mutate(
    completed = coalesce(completed, FALSE),
    status = case_when(
      completed ~ "Completed",
      is.na(day0) ~ "No baseline date",
      TODAY < due_date ~ "Not yet due",
      TODAY <= window_end ~ "Due now",
      TRUE ~ "Overdue"
    ),
    days_late_done = as.numeric(completed_date - due_date),
    status = factor(status, c("Completed", "Due now", "Overdue", "Not yet due", "No baseline date")),
    visit_type = factor(visit_type, VISIT_SCHEDULE$visit_type)
  )
# Patients who already left the cohort (withdrawn, lost, died) are not chased
exits <- cem |> filter(participant_status %in% c("withdrawn", "lost", "died")) |>
  group_by(patient_id) |> summarise(exit_status = first(participant_status),
            exit_date = suppressWarnings(min(coalesce(status_date, visit_date), na.rm = TRUE)), .groups = "drop") |>
  mutate(exit_date = if_else(is.infinite(exit_date), as.Date(NA), exit_date))
visit_status <- visit_status |> left_join(exits, by = "patient_id") |>
  mutate(status = if_else(!is.na(exit_status) & status %in% c("Due now", "Overdue") & due_date > exit_date,
                          factor("Exited", levels = c(levels(status), "Exited")), factor(status, levels = c(levels(status), "Exited"))))

# 6c. Patient level: one row per registered patient with follow-up and safety
hb_wide <- cem |> filter(visit_type %in% c("d0", "d3"), !is.na(hb)) |>
  arrange(submission_time) |> group_by(patient_id, visit_type) |> summarise(hb = last(hb), .groups = "drop") |>
  tidyr::pivot_wider(names_from = visit_type, values_from = hb, names_prefix = "hb_")
hb_wide <- ensure_cols(hb_wide, c("hb_d0", "hb_d3")) |> mutate(across(c(hb_d0, hb_d3), as.numeric))

symptom_vars <- c("dark_urine", "pallor_report", "jaundice_report", "fatigue", "dizziness", "fever", "nausea", "vomiting", "rash")
haemolysis_signs <- cem |>
  mutate(haem_sign = yes(dark_urine) | yes(jaundice_report) | yes(dark_urine_d3) | yes(jaundice)) |>
  group_by(patient_id) |> summarise(any_haemolysis_sign = any(haem_sign), .groups = "drop")

patient_events <- events |> group_by(patient_id) |>
  summarise(n_events = n(), any_serious = any(ever_serious), first_onset = suppressWarnings(min(onset, na.rm = TRUE)), .groups = "drop") |>
  mutate(first_onset = if_else(is.infinite(first_onset), as.Date(NA), first_onset))
form2_ae <- cem |> filter(yes(ae_reported)) |> group_by(patient_id) |>
  summarise(first_form2_ae = min(visit_date), .groups = "drop")
last_contact <- cem |> filter(completed) |> group_by(patient_id) |> summarise(last_contact = max(visit_date), .groups = "drop")
d28 <- visit_status |> filter(visit_type == "d28") |> select(patient_id, d28_done = completed)

patients <- reg |>
  left_join(manual_day0, by = "patient_id") |>
  mutate(day0 = coalesce(sldp_date, manual_day0)) |>
  left_join(hb_wide, by = "patient_id") |>
  left_join(haemolysis_signs, by = "patient_id") |>
  left_join(patient_events, by = "patient_id") |>
  left_join(form2_ae, by = "patient_id") |>
  left_join(last_contact, by = "patient_id") |>
  left_join(d28, by = "patient_id") |>
  left_join(exits, by = "patient_id") |>
  mutate(
    n_events = coalesce(n_events, 0L),
    any_event = n_events > 0 | !is.na(first_form2_ae),
    any_serious = coalesce(any_serious, FALSE),
    hb_change = hb_d3 - hb_d0,
    hb_drop_alert = !is.na(hb_change) & -hb_change >= HB_DROP_ALERT,
    first_ae_date = pmin(first_onset, first_form2_ae, na.rm = TRUE),
    d28_done = coalesce(d28_done, FALSE)
  )
log_msg("Linked datasets: ", nrow(patients), " patients, ", nrow(cem), " visits, ", nrow(events), " events")


# -----------------------------------------------------------------------------
# 7. MONITORING INDICATORS
# -----------------------------------------------------------------------------
# Percentage with one decimal; works for a vector of counts over one denominator
pct <- function(n, d) {
  r <- round(100 * n / d, 1)
  r[rep_len(is.na(d) | d <= 0, length(r))] <- NA_real_
  r
}

# Exact binomial 95% CI, returned as a "x% (lo-hi)" string for tables
ci_txt <- function(x, n) {
  if (is.na(n) || n == 0) return(NA_character_)
  b <- binom.test(x, n)
  sprintf("%.1f%% (%.1f-%.1f)", 100 * x / n, 100 * b$conf.int[1], 100 * b$conf.int[2])
}

# 7a. Headline indicators for the current run
n_reg   <- nrow(reg)
n_pf    <- sum(reg$pf_positive == "yes", na.rm = TRUE)
n_elig  <- sum(reg$screen_status == "eligible", na.rm = TRUE)
n_given <- sum(reg$sldp_given, na.rm = TRUE)
n_elig_given <- sum(reg$sldp_given & reg$screen_status == "eligible", na.rm = TRUE)
n_cem_site <- sum(reg$cem_site, na.rm = TRUE)
n_enrolled <- sum(reg$cem_in, na.rm = TRUE)
vs_due <- visit_status |> filter(status %in% c("Completed", "Overdue", "Due now"), due_date <= TODAY)

headline <- tibble::tribble(
  ~domain, ~indicator, ~value,
  "Registration", "Patients registered", as.character(n_reg),
  "Registration", "P. falciparum confirmed (Pf status yes)", ci_txt(n_pf, n_reg),
  "Coverage", "Screened eligible", ci_txt(n_elig, n_reg),
  "Coverage", "SLDP given among eligible", ci_txt(n_elig_given, n_elig),
  "Coverage", "SLDP given (all registered)", ci_txt(n_given, n_reg),
  "Coverage", "SLDP directly observed (among given)", ci_txt(sum(yes(reg$sldp_dot) & reg$sldp_given), n_given),
  "Dosing", "Dose within 0.15-0.4 mg/kg (among given with weight)", ci_txt(sum(between(reg$mg_per_kg, SLDP_DOSE_LOW, SLDP_DOSE_HIGH), na.rm = TRUE), sum(!is.na(reg$mg_per_kg))),
  "CEM", "Registered at CEM sites", as.character(n_cem_site),
  "CEM", "Consented and enrolled (among CEM-site patients)", ci_txt(n_enrolled, n_cem_site),
  "CEM", "Scheduled visits due to date that are completed", ci_txt(sum(vs_due$status == "Completed"), nrow(vs_due)),
  "CEM", "Visits currently overdue", as.character(sum(visit_status$status == "Overdue")),
  "CEM", "Day 28 completed (among patients past Day 28 + grace)", ci_txt(sum(visit_status$visit_type == "d28" & visit_status$status == "Completed" & visit_status$window_end < TODAY), sum(visit_status$visit_type == "d28" & visit_status$window_end < TODAY)),
  "Safety", "Adverse events reported (distinct events)", as.character(nrow(events)),
  "Safety", "Serious adverse events", as.character(sum(events$ever_serious)),
  "Safety", "Hb fall >= 2 g/dL (among Day 0 and Day 3 Hb pairs)", ci_txt(sum(patients$hb_drop_alert), sum(!is.na(patients$hb_change))),
  "Safety", "SAEs submitted to pharmacovigilance", ci_txt(sum(events$ever_serious & events$pv_submitted), sum(events$ever_serious)),
  "Safety", "Events reported within 2 days of onset", ci_txt(sum(events$report_lag <= ADR_REPORT_LAG_TARGET, na.rm = TRUE), sum(!is.na(events$report_lag))),
  "Data quality", "Critical data issues open", as.character(sum(issue_log$severity == "Critical")),
  "Data quality", "CEM visits linked to a registration", ci_txt(sum(cem$reg_matched, na.rm = TRUE), nrow(cem)),
  "Data quality", "CEM visits using a manual baseline", ci_txt(sum(cem$baseline_source == "manual", na.rm = TRUE), nrow(cem)),
  "Data quality", "ADR reports linked to a registration", ci_txt(sum(adr$reg_matched, na.rm = TRUE), nrow(adr))
)

# 7b. Facility scorecard: one row per facility, the table supervisors act on
facility_scorecard <- reg |>
  group_by(district = district_lbl, facility = facility_lbl, cem_site) |>
  summarise(registered = n(),
            eligible = sum(screen_status == "eligible", na.rm = TRUE),
            sldp_given_eligible = sum(sldp_given & screen_status == "eligible", na.rm = TRUE),
            dot_pct = pct(sum(yes(sldp_dot) & sldp_given), sum(sldp_given)),
            enrolled = sum(cem_in, na.rm = TRUE), .groups = "drop") |>
  mutate(coverage_pct = pct(sldp_given_eligible, eligible)) |>
  left_join(visit_status |> filter(due_date <= TODAY) |> left_join(reg |> select(patient_id, facility_lbl), by = "patient_id") |>
              group_by(facility = facility_lbl) |>
              summarise(visits_due = sum(status %in% c("Completed", "Overdue", "Due now")),
                        visits_done = sum(status == "Completed"),
                        overdue = sum(status == "Overdue"), .groups = "drop") |>
              mutate(followup_pct = pct(visits_done, visits_due)), by = "facility") |>
  left_join(issue_log |> left_join(reg |> select(facility, facility_lbl) |> distinct(), by = "facility") |>
              group_by(facility = facility_lbl) |> summarise(critical_issues = sum(severity == "Critical"), all_issues = n(), .groups = "drop"),
            by = "facility") |>
  mutate(across(c(critical_issues, all_issues, overdue), ~ coalesce(.x, 0L))) |>
  arrange(district, desc(registered))

# 7c. Action lists for the field: overdue and due visits with contact details
overdue_list <- visit_status |>
  filter(status %in% c("Overdue", "Due now")) |>
  left_join(reg |> select(patient_id, alternative_contact_phone = any_of("alternative_contact_phone")), by = "patient_id") |>
  transmute(status, district, facility, patient_id, patient_name, phone,
            visit = toupper(as.character(visit_type)), mode, due_date, window_end,
            days_overdue = pmax(0, as.numeric(TODAY - window_end)),
            failed_attempts = coalesce(n_attempts_failed, 0L), last_attempt) |>
  arrange(desc(status == "Overdue"), desc(days_overdue), facility)

# 7d. Collector and device activity (who submits what, when, how fast)
activity <- bind_rows(
  reg |> transmute(form = "Form 1", who = collector, date = submission_date, duration_min, device = as.character(device_id)),
  cem |> transmute(form = "Form 2", who = focal_person, date = submission_date, duration_min, device = as.character(device_id)),
  adr |> transmute(form = "Form 3", who = reporter_name, date = submission_date, duration_min, device = as.character(device_id))
)
collector_summary <- activity |>
  group_by(form, who) |>
  summarise(submissions = n(), active_days = n_distinct(date), first = min(date), last = max(date),
            median_duration_min = round(median(duration_min, na.rm = TRUE), 1), .groups = "drop") |>
  arrange(form, desc(submissions))

# 7e. Form versions in use: old versions on devices miss later fixes
version_use <- bind_rows(
  reg |> count(form = "Form 1", form_version),
  cem |> count(form = "Form 2", form_version),
  adr |> count(form = "Form 3", form_version)
)

# 7f. SAE line list for the pharmacovigilance focal person
sae_list <- events |> filter(ever_serious) |>
  left_join(reg |> select(patient_id, sex, age_years, sldp_given, act_type_lbl), by = "patient_id") |>
  select(event_id, patient_id, facility, onset, first_report, report_lag, description, criteria,
         final_outcome, hospitalised, pv_submitted, sex, age_years, sldp_given, act_type_lbl)

# 7g. Monitoring charts
save_plot <- function(p, file, w = 9, h = 5) ggsave(file.path(DIR_FIG, file), p, width = w, height = h, dpi = 150, bg = "white")

if (nrow(reg) > 0) {
  # Weekly registrations and cumulative enrolment
  p <- reg |> count(reg_week, cem_in) |>
    mutate(group = if_else(cem_in, "Enrolled in CEM", "Registered, not in CEM")) |>
    ggplot(aes(reg_week, n, fill = group)) + geom_col() +
    scale_fill_manual(values = PAL[c(1, 3)]) + scale_x_date(date_labels = "%d %b") +
    labs(title = "Weekly registrations", x = "Week starting", y = "Patients", fill = NULL)
  save_plot(p, "M01_weekly_registrations.png")

  # SLDP coverage among eligible patients by facility
  p <- facility_scorecard |> filter(eligible > 0) |>
    ggplot(aes(reorder(facility, coverage_pct), coverage_pct, fill = cem_site)) + geom_col() +
    geom_text(aes(label = paste0(sldp_given_eligible, "/", eligible)), hjust = -0.1, size = 3) +
    coord_flip() + scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 25)) +
    scale_fill_manual(values = PAL[c(3, 1)], labels = c("Non-CEM", "CEM site")) +
    labs(title = "SLDP coverage among eligible patients", x = NULL, y = "% given SLDP", fill = NULL)
  save_plot(p, "M02_coverage_by_facility.png", h = 7)
}

if (nrow(visit_status) > 0) {
  # Follow-up status of every scheduled visit
  p <- visit_status |> count(visit_type, status) |>
    ggplot(aes(visit_type, n, fill = status)) + geom_col() +
    scale_fill_manual(values = c(Completed = PAL[1], `Due now` = PAL[4], Overdue = PAL[6],
                                 `Not yet due` = "#C9D2D8", `No baseline date` = PAL[2], Exited = "#8A8F94")) +
    labs(title = "CEM follow-up status by scheduled visit", x = "Visit", y = "Patients", fill = NULL)
  save_plot(p, "M03_followup_status.png")
}

if (nrow(cem) > 0) {
  # Timeliness: days early (-) or late (+) against the schedule
  p <- cem |> filter(completed, visit_type != "unscheduled", !is.na(timing_deviation)) |>
    ggplot(aes(factor(visit_type, VISIT_SCHEDULE$visit_type), timing_deviation)) +
    geom_hline(yintercept = 0, colour = "grey60") + geom_boxplot(fill = "#DCE7EE", outlier.colour = PAL[2]) +
    labs(title = "Visit timing against schedule", x = "Visit", y = "Days early (-) / late (+)")
  save_plot(p, "M04_visit_timeliness.png")

  # Telephone contact success by attempt number
  p <- cem |> filter(phone_visit) |>
    mutate(attempt = if_else(contact_success == "yes", "Successful", coalesce(contact_failure_lbl, "Failed"))) |>
    count(visit_type, attempt) |>
    ggplot(aes(factor(visit_type, c(VISIT_SCHEDULE$visit_type, "unscheduled")), n, fill = attempt)) + geom_col() +
    scale_fill_manual(values = PAL) + labs(title = "Telephone contact outcomes", x = "Visit", y = "Calls", fill = NULL)
  save_plot(p, "M05_phone_contact.png")
}

if (nrow(issue_log) > 0) {
  p <- issue_log |> count(check, severity) |>
    ggplot(aes(reorder(stringr::str_trunc(check, 60), n), n, fill = severity)) + geom_col() + coord_flip() +
    scale_fill_manual(values = c(Critical = PAL[6], Major = PAL[2], Minor = PAL[3])) +
    labs(title = "Data issues by type", x = NULL, y = "Records", fill = NULL)
  save_plot(p, "M06_data_issues.png", w = 11, h = 7)
}


# -----------------------------------------------------------------------------
# 8. ANALYSIS: answering the programme objectives
# -----------------------------------------------------------------------------
# Models are wrapped in safe_fit() so the script keeps running early in the
# pilot, when there are too few records for a model to converge.
safe_fit <- function(expr, label) {
  tryCatch(expr, error = function(e) { log_msg("  model skipped (", label, "): ", conditionMessage(e)); NULL },
           warning = function(w) { log_msg("  model warning (", label, "): ", conditionMessage(w)); suppressWarnings(eval.parent(substitute(expr))) })
}
results <- list()

# ---- O1. Coverage --------------------------------------------------------------
# Table 1: patient characteristics by whether SLDP was given
table1 <- reg |>
  mutate(sldp = if_else(sldp_given, "SLDP given", "SLDP not given")) |>
  group_by(sldp) |>
  summarise(n = n(),
            age_median_iqr = sprintf("%.0f (%.0f-%.0f)", median(age_years, na.rm = TRUE), quantile(age_years, .25, na.rm = TRUE), quantile(age_years, .75, na.rm = TRUE)),
            female_pct = pct(sum(sex == "female", na.rm = TRUE), n()),
            under5_pct = pct(sum(age_years < 5, na.rm = TRUE), n()),
            microscopy_confirmed_pct = pct(sum(micro_result == "positive", na.rm = TRUE), n()),
            severe_malaria_pct = pct(sum(severity == "severe", na.rm = TRUE), n()),
            cem_site_pct = pct(sum(cem_site, na.rm = TRUE), n()),
            insured_pct = pct(sum(yes(patient_insured)), n()), .groups = "drop")
results$table1 <- table1

coverage_by <- function(var) {
  reg |> filter(screen_status == "eligible") |>
    group_by(level = as.character(.data[[var]])) |>
    summarise(eligible = n(), given = sum(sldp_given), .groups = "drop") |>
    mutate(variable = var, coverage = purrr::map2_chr(given, eligible, ci_txt), .before = 1)
}
results$coverage <- purrr::map_dfr(c("district_lbl", "age_group", "sex", "act_type_lbl", "cem_site"), coverage_by)

# Factors associated with receiving SLDP among eligible patients. A mixed-effects
# logistic model with a random intercept for facility accounts for clustering of
# practice within facilities. Odds ratios with 95% Wald CIs.
elig <- reg |> filter(screen_status == "eligible", !is.na(age_group), !is.na(sex)) |>
  mutate(given = as.integer(sldp_given), age_group = droplevels(age_group))
m_cov <- NULL
if (nrow(elig) >= 50 && length(unique(elig$given)) == 2 && n_distinct(elig$facility) >= 3) {
  m_cov <- safe_fit(glmer(given ~ age_group + sex + cem_site + (1 | facility), data = elig, family = binomial,
                          control = glmerControl(optimizer = "bobyqa")), "coverage glmer")
}
if (!is.null(m_cov)) {
  co <- summary(m_cov)$coefficients
  results$coverage_model <- tibble(term = rownames(co), OR = exp(co[, 1]),
                                   lo = exp(co[, 1] - 1.96 * co[, 2]), hi = exp(co[, 1] + 1.96 * co[, 2]), p = co[, 4]) |>
    filter(term != "(Intercept)") |> mutate(across(c(OR, lo, hi), ~ round(.x, 2)), p = signif(p, 2))
  results$coverage_icc <- tibble(facility_ICC = round(as.numeric(VarCorr(m_cov)$facility) / (as.numeric(VarCorr(m_cov)$facility) + pi^2 / 3), 3))
}

# ---- O2. Acceptability ---------------------------------------------------------
results$not_given_reasons <- reg |> filter(sldp_status == "not_given") |>
  count(reason = coalesce(no_sldp_reason_lbl, "Not recorded"), sort = TRUE) |> mutate(pct = pct(n, sum(n)))
refusal_cols <- grep("^sldp_refusal_specific__", names(reg), value = TRUE)
results$refusal_reasons <- if (length(refusal_cols)) {
  reg |> filter(no_sldp_reason == "refusal") |> summarise(across(all_of(refusal_cols), ~ sum(.x, na.rm = TRUE))) |>
    tidyr::pivot_longer(everything(), names_to = "reason", values_to = "n") |>
    mutate(reason = sub("sldp_refusal_specific__", "", reason)) |> arrange(desc(n))
} else tibble()
dot_cols <- grep("^sldp_dot_reasons__", names(reg), value = TRUE)
results$dot <- tibble(given = n_given, observed = sum(yes(reg$sldp_dot) & reg$sldp_given),
                      with_food = sum(yes(reg$with_food) & reg$sldp_given), counselled = sum(yes(reg$counselled) & reg$sldp_given)) |>
  mutate(observed_pct = pct(observed, given), food_pct = pct(with_food, given), counselled_pct = pct(counselled, given))

# ---- O3. Screening agreement and dosing ----------------------------------------
# Agreement between the calculated eligibility and the clinician's decision
# (eligible vs not), with Cohen's kappa.
agree <- reg |> filter(!is.na(screen_status), !is.na(provider_eligibility)) |>
  mutate(calc = screen_status == "eligible", clin = provider_eligibility == "yes")
kappa <- function(a, b) {
  tab <- table(factor(a, c(FALSE, TRUE)), factor(b, c(FALSE, TRUE))); n <- sum(tab)
  if (n == 0) return(NA_real_); po <- sum(diag(tab)) / n; pe <- sum(rowSums(tab) * colSums(tab)) / n^2
  if (pe == 1) return(NA_real_); (po - pe) / (1 - pe)
}
results$screening_agreement <- tibble(
  n = nrow(agree), agreement_pct = pct(sum(agree$calc == agree$clin), nrow(agree)),
  kappa = round(kappa(agree$calc, agree$clin), 3),
  clinician_overrode_to_eligible = sum(!agree$calc & agree$clin),
  clinician_overrode_to_not = sum(agree$calc & !agree$clin))
results$screening_status <- reg |> count(screen_status) |> mutate(pct = pct(n, sum(n)))
results$exclusions <- reg |> summarise(across(c(pq_allergy, pregnant, breastfeeding_under1, severe_anaemia, haemolysis, other_exclusion),
                                              ~ sum(.x == "yes", na.rm = TRUE))) |>
  tidyr::pivot_longer(everything(), names_to = "exclusion", values_to = "n_yes")

dose <- reg |> filter(!is.na(mg_per_kg))
results$dosing <- dose |> group_by(age_group) |>
  summarise(n = n(), median_mg_kg = round(median(mg_per_kg), 3),
            iqr = sprintf("%.2f-%.2f", quantile(mg_per_kg, .25), quantile(mg_per_kg, .75)),
            within_range_pct = pct(sum(between(mg_per_kg, SLDP_DOSE_LOW, SLDP_DOSE_HIGH)), n()),
            above_range = sum(mg_per_kg > SLDP_DOSE_HIGH), below_range = sum(mg_per_kg < SLDP_DOSE_LOW), .groups = "drop")
if (nrow(dose) > 0) {
  p <- ggplot(dose, aes(mg_per_kg)) +
    annotate("rect", xmin = SLDP_DOSE_LOW, xmax = SLDP_DOSE_HIGH, ymin = -Inf, ymax = Inf, fill = "#DCE7EE", alpha = 0.7) +
    geom_histogram(binwidth = 0.025, fill = PAL[1], colour = "white") +
    geom_vline(xintercept = SLDP_TARGET_MG_PER_KG, linetype = 2, colour = PAL[2]) +
    labs(title = "SLDP dose per kilogram", subtitle = "Shaded: accepted range 0.15-0.4 mg/kg; dashed: 0.25 mg/kg target",
         x = "mg base per kg", y = "Patients")
  save_plot(p, "A01_dose_mg_per_kg.png")
}

# ---- O4. Safety ------------------------------------------------------------------
# Adverse event incidence per 100 treated patients (exact 95% CI), by SLDP status.
# Restricted to CEM-enrolled patients, the cohort with active follow-up, so that
# the denominator is known.
cohort_pat <- patients |> filter(cem_in)
inc <- cohort_pat |> group_by(sldp = if_else(sldp_given, "SLDP given", "SLDP not given")) |>
  summarise(patients = n(), with_event = sum(any_event), with_sae = sum(any_serious), .groups = "drop") |>
  mutate(event_incidence = purrr::map2_chr(with_event, patients, ci_txt),
         sae_incidence = purrr::map2_chr(with_sae, patients, ci_txt))
results$ae_incidence <- inc
# Risk ratio of any event, SLDP given vs not (log-binomial approximation via 2x2)
rr_tab <- with(cohort_pat, table(factor(sldp_given, c(TRUE, FALSE)), factor(any_event, c(TRUE, FALSE))))
if (all(dim(rr_tab) == 2) && all(rowSums(rr_tab) > 0) && rr_tab[2, 1] > 0 && rr_tab[1, 1] > 0) {
  r1 <- rr_tab[1, 1] / sum(rr_tab[1, ]); r0 <- rr_tab[2, 1] / sum(rr_tab[2, ])
  se <- sqrt(1 / rr_tab[1, 1] - 1 / sum(rr_tab[1, ]) + 1 / rr_tab[2, 1] - 1 / sum(rr_tab[2, ]))
  results$ae_risk_ratio <- tibble(RR = round(r1 / r0, 2), lo = round(exp(log(r1 / r0) - 1.96 * se), 2),
                                  hi = round(exp(log(r1 / r0) + 1.96 * se), 2),
                                  fisher_p = signif(fisher.test(rr_tab)$p.value, 2))
}
results$events_by_type <- events |> count(max_intensity, ever_serious, final_outcome)
results$events_suspected <- events |> summarise(events = n(), sldp_suspected = sum(sldp_suspected), act_suspected = sum(act_suspected))

# Haemoglobin Day 0 -> Day 3: paired change, Wilcoxon signed-rank test and the
# proportion with a fall >= 2 g/dL; then a linear mixed model of Hb over the two
# visits with a patient random intercept (uses patients with one value too).
hb_pairs <- patients |> filter(!is.na(hb_d0), !is.na(hb_d3))
if (nrow(hb_pairs) >= 5) {
  wt <- safe_fit(wilcox.test(hb_pairs$hb_d3, hb_pairs$hb_d0, paired = TRUE, exact = FALSE), "wilcoxon")
  results$hb_change <- tibble(
    pairs = nrow(hb_pairs), median_d0 = median(hb_pairs$hb_d0), median_d3 = median(hb_pairs$hb_d3),
    mean_change = round(mean(hb_pairs$hb_change), 2), sd_change = round(sd(hb_pairs$hb_change), 2),
    fall_ge2 = ci_txt(sum(-hb_pairs$hb_change >= HB_DROP_ALERT), nrow(hb_pairs)),
    wilcoxon_p = if (!is.null(wt)) signif(wt$p.value, 2) else NA)
  p <- hb_pairs |> mutate(id = row_number(), alert = -hb_change >= HB_DROP_ALERT) |>
    tidyr::pivot_longer(c(hb_d0, hb_d3), names_to = "visit", values_to = "hb") |>
    mutate(visit = if_else(visit == "hb_d0", "Day 0", "Day 3")) |>
    ggplot(aes(visit, hb, group = id, colour = alert)) + geom_line(alpha = 0.5) + geom_point(size = 1) +
    scale_colour_manual(values = c(`FALSE` = "#9AA8B2", `TRUE` = PAL[6]), labels = c("Fall < 2 g/dL", "Fall >= 2 g/dL")) +
    labs(title = "Haemoglobin, Day 0 to Day 3", x = NULL, y = "Hb (g/dL)", colour = NULL)
  save_plot(p, "A02_hb_day0_day3.png", w = 6, h = 5)
}
hb_long <- cem |> filter(visit_type %in% c("d0", "d3"), !is.na(hb)) |>
  left_join(reg |> select(patient_id, age_years, sex, sldp_given), by = "patient_id") |>
  mutate(day3 = as.integer(visit_type == "d3"))
if (nrow(hb_long) >= 30 && n_distinct(hb_long$patient_id) >= 15) {
  m_hb <- safe_fit(lmer(hb ~ day3 * sldp_given + age_years + sex + (1 | patient_id), data = hb_long), "Hb lmer")
  if (!is.null(m_hb)) {
    co <- summary(m_hb)$coefficients
    results$hb_model <- tibble(term = rownames(co), estimate = round(co[, 1], 3), se = round(co[, 2], 3),
                               lo = round(co[, 1] - 1.96 * co[, 2], 3), hi = round(co[, 1] + 1.96 * co[, 2], 3))
  }
}

# Symptom prevalence at each telephone visit (heat map of % reporting "yes")
sym <- cem |> filter(phone_visit, contact_success == "yes") |>
  select(visit_type, any_of(symptom_vars)) |>
  tidyr::pivot_longer(-visit_type, names_to = "symptom", values_to = "ans") |>
  group_by(visit_type, symptom) |> summarise(n = sum(!is.na(ans)), yes = sum(ans == "yes", na.rm = TRUE), .groups = "drop") |>
  mutate(pct = pct(yes, n))
results$symptoms <- sym
if (nrow(sym) > 0) {
  p <- ggplot(sym, aes(factor(visit_type, c("d7", "d14", "d28", "unscheduled")), symptom, fill = pct)) + geom_tile(colour = "white") +
    geom_text(aes(label = paste0(pct, "%")), size = 3) + scale_fill_gradient(low = "#F7F5F0", high = PAL[2]) +
    labs(title = "Symptoms reported at telephone follow-up", x = "Visit", y = NULL, fill = "% yes")
  save_plot(p, "A03_symptom_heatmap.png", w = 7, h = 5)
}

# Time to first adverse event within 28 days of Day 0: Kaplan-Meier by SLDP
# status and a Cox model adjusted for age and sex. Patients without an event are
# censored at their last completed contact (or Day 28, whichever is earlier).
tte <- cohort_pat |> filter(!is.na(day0)) |>
  mutate(event_time = as.numeric(first_ae_date - day0),
         censor_time = pmin(as.numeric(coalesce(last_contact, day0) - day0), 28),
         status = as.integer(!is.na(event_time) & event_time >= 0 & event_time <= 28),
         time = if_else(status == 1, pmax(event_time, 0.5), pmax(censor_time, 0.5)),
         sldp = factor(if_else(sldp_given, "SLDP given", "SLDP not given")))
if (nrow(tte) >= 20 && sum(tte$status) >= 3) {
  km <- survfit(Surv(time, status) ~ sldp, data = tte)
  results$km_28d <- broom::tidy(km) |> group_by(strata) |> slice_tail(n = 1) |> ungroup() |>
    transmute(strata = sub("^sldp=", "", strata), cumulative_incidence = round(1 - estimate, 3), lo = round(1 - conf.high, 3), hi = round(1 - conf.low, 3))
  km_df <- broom::tidy(km) |> mutate(strata = sub("^sldp=", "", strata))
  p <- ggplot(km_df, aes(time, 1 - estimate, colour = strata)) + geom_step() +
    scale_colour_manual(values = PAL) + scale_y_continuous(labels = scales::percent) +
    labs(title = "Cumulative incidence of a first adverse event", x = "Days since Day 0", y = "Cumulative incidence", colour = NULL)
  save_plot(p, "A04_time_to_first_ae.png")
  if (n_distinct(tte$sldp) == 2) {
    cox <- safe_fit(coxph(Surv(time, status) ~ sldp + age_years + sex, data = tte), "cox")
    if (!is.null(cox)) results$cox <- broom::tidy(cox, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(across(where(is.numeric), ~ round(.x, 3)))
  }
}

# ---- O5. CEM performance -----------------------------------------------------------
results$followup_by_visit <- visit_status |> filter(due_date <= TODAY) |>
  group_by(visit_type) |>
  summarise(due = sum(status %in% c("Completed", "Overdue", "Due now")), completed = sum(status == "Completed"),
            on_time = sum(status == "Completed" & days_late_done <= grace, na.rm = TRUE), .groups = "drop") |>
  mutate(completion = purrr::map2_chr(completed, due, ci_txt), on_time_pct = pct(on_time, completed))
results$retention <- patients |> filter(cem_in, !is.na(day0), day0 + 28 + 5 < TODAY) |>
  summarise(eligible_for_d28 = n(), completed_d28 = sum(d28_done), withdrawn = sum(exit_status == "withdrawn", na.rm = TRUE),
            lost = sum(exit_status == "lost", na.rm = TRUE), died = sum(exit_status == "died", na.rm = TRUE)) |>
  mutate(retention = ci_txt(completed_d28, eligible_for_d28))
results$contact_attempts <- cem |> filter(phone_visit) |>
  group_by(attempt = pmin(coalesce(attempt_number, 1), 4)) |>
  summarise(calls = n(), success = sum(contact_success == "yes", na.rm = TRUE), .groups = "drop") |>
  mutate(success_pct = pct(success, calls))

# ---- O6. Parasitological follow-up -------------------------------------------------
d3 <- cem |> filter(visit_type == "d3", completed)
results$day3_positivity <- d3 |> filter(d3_repeat_test_done == "yes") |>
  summarise(tested = n(),
            rdt_pos = sum(d3_rdt_result %in% c("pf", "other"), na.rm = TRUE),
            micro_pos = sum(d3_microscopy_result == "positive", na.rm = TRUE)) |>
  mutate(micro_positivity = ci_txt(micro_pos, tested))
# Gametocyte carriage by microscopy: Day 0 (registration) vs Day 3, paired,
# McNemar test. Gametocytes are a stage option in both forms.
g0 <- reg |> transmute(patient_id, gam_d0 = coalesce(parasite_stages__gametocytes, 0L) == 1 & micro_result == "positive",
                       micro_d0 = micro_result %in% c("positive", "negative"))
g3 <- d3 |> filter(d3_microscopy_result %in% c("positive", "negative")) |>
  transmute(patient_id, gam_d3 = coalesce(d3_microscopy_stages__gametocytes, 0L) == 1 & d3_microscopy_result == "positive")
gam <- inner_join(g0 |> filter(micro_d0), g3, by = "patient_id")
if (nrow(gam) >= 10) {
  tab <- table(factor(gam$gam_d0, c(TRUE, FALSE)), factor(gam$gam_d3, c(TRUE, FALSE)))
  results$gametocytes <- tibble(pairs = nrow(gam), d0_carriage = ci_txt(sum(gam$gam_d0), nrow(gam)),
                                d3_carriage = ci_txt(sum(gam$gam_d3), nrow(gam)),
                                mcnemar_p = tryCatch(signif(mcnemar.test(tab)$p.value, 2), error = function(e) NA))
}

# ---- O7. Data quality summary ------------------------------------------------------
results$issue_summary <- issue_log |> count(form, severity, check, sort = TRUE)
completeness <- function(df, form, vars) {
  vars <- intersect(vars, names(df))
  tibble(form = form, variable = vars, missing_pct = purrr::map_dbl(vars, ~ pct(sum(is.na(df[[.x]])), nrow(df))))
}
results$missingness <- bind_rows(
  completeness(reg, "Form 1", c("weight", "phone", "birth_year", "rdt_result", "act_type", "sldp_status", "sldp_date", "sldp_dose_mg", "cem_consent")),
  completeness(cem |> filter(visit_type %in% c("d0", "d3"), completed), "Form 2 (Day 0/3)", c("hb", "temperature", "pallor", "jaundice")),
  completeness(adr, "Form 3", c("event_onset", "intensity", "serious", "outcome", "action_taken"))
) |> arrange(desc(missing_pct))


# -----------------------------------------------------------------------------
# 9. EXPORT
# -----------------------------------------------------------------------------
wb <- createWorkbook()
hdr <- createStyle(textDecoration = "bold", fgFill = "#1C2B36", fontColour = "#F3F1EC", wrapText = TRUE)
add_sheet <- function(name, df, note = NULL) {
  addWorksheet(wb, name)
  start <- 1
  if (!is.null(note)) { writeData(wb, name, note, startRow = 1); start <- 3 }
  if (is.null(df) || nrow(df) == 0) df <- tibble(note = "No records yet")
  writeData(wb, name, as.data.frame(df), startRow = start, headerStyle = hdr)
  freezePane(wb, name, firstActiveRow = start + 1)
  setColWidths(wb, name, cols = seq_len(ncol(df)), widths = "auto")
}
add_sheet("Headline", headline, paste("SLDP pilot monitoring, data pulled", raw$registration$pulled_at %||% RUN_LABEL))
add_sheet("Facility scorecard", facility_scorecard)
add_sheet("Overdue visits", overdue_list, "Patients to contact today. Overdue = past the grace window; Due now = within it.")
add_sheet("Data issues", issue_log, "One row per problem. Critical first. Resolve in Kobo and re-run.")
add_sheet("SAE line list", sae_list)
add_sheet("Events", events)
add_sheet("Collectors", collector_summary)
add_sheet("Form versions", version_use, "Old versions on a device mean that device has not updated its forms.")
for (nm in names(results)) add_sheet(substr(paste0("A_", nm), 1, 31), results[[nm]])
xlsx_file <- file.path(DIR_OUT, sprintf("SLDP_monitoring_%s.xlsx", RUN_LABEL))
saveWorkbook(wb, xlsx_file, overwrite = TRUE)

saveRDS(list(registration = reg, cem_visits = cem, adr_reports = adr, events = events, patients = patients,
             visit_status = visit_status, cem_medicines = cem_meds, adr_medicines = adr_meds,
             issue_log = issue_log, results = results),
        file.path(DIR_OUT, "analysis_datasets.rds"))

log_msg("Done. Workbook: ", xlsx_file)
log_msg("Figures: ", DIR_FIG)
print(headline, n = Inf)
