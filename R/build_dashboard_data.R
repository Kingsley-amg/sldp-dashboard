# =============================================================================
# SLDP pilot, Greater Accra: build the public dashboard data
# Author: Kingsley Amegah, Data Scientist / Health Data Specialist
#
# Runs the shared pipeline (R/pipeline.R), then writes docs/data/dashboard.json.
#
# PRIVACY RULE: the JSON is published on a public web page, so it holds
# AGGREGATES ONLY: counts by facility, week, visit and category. No names,
# phone numbers, Patient IDs, Event IDs, dates of birth or free text ever
# leave this script. The raw Kobo snapshots stay in SLDP_RAW_DIR, which is
# outside the published folder and listed in .gitignore.
#
# Run locally:  Rscript R/build_dashboard_data.R          (needs KOBO_TOKEN)
#               SLDP_USE_CACHE=true Rscript R/build_dashboard_data.R
# =============================================================================

params <- list(
  use_cache   = tolower(Sys.getenv("SLDP_USE_CACHE", "false")) == "true",
  drop_test   = TRUE,
  pilot_start = Sys.getenv("SLDP_PILOT_START", "2026-10-01"),
  kobo_server = Sys.getenv("KOBO_SERVER", "https://eu.kobotoolbox.org"))
OUT_JSON <- Sys.getenv("SLDP_OUT_JSON", file.path("docs", "data", "dashboard.json"))
IS_DEMO  <- tolower(Sys.getenv("SLDP_DEMO", "false")) == "true"   # TRUE only for the mock-data preview

source(file.path("R", "pipeline.R"))
source(file.path("R", "analysis.R"))
log_msg("pipeline done: ", nrow(reg), " patients, ", nrow(cem), " visits, ", nrow(adr), " ADR reports")

# ---- Lookups -----------------------------------------------------------------
wk  <- function(d) format(lubridate::floor_date(d, "week", week_start = 1), "%Y-%m-%d")
# Full facility list from the Form 1 choices sheet (every pilot site, with its
# district and CEM flag), so sites with no records yet still appear in the
# dashboard filters. Facilities seen in the data but missing from the list are added.
ch_val <- function(c, k) { v <- c[[k]]; if (is.list(v)) v <- v[[1]]; if (is.null(v)) NA_character_ else as.character(v) }
form_choices <- purrr::map_dfr(raw$registration$form$choices, function(c)
  tibble(list_name = ch_val(c, "list_name"), code = ch_val(c, "name"), label = ch_val(c, "label"),
         district_code = ch_val(c, "district"), cem = ch_val(c, "cem")))
district_names <- form_choices |> filter(list_name == "district") |> select(district_code = code, district = label)
fac_form <- form_choices |> filter(list_name == "facility") |> left_join(district_names, by = "district_code") |>
  transmute(code, label = coalesce(label, code), district = coalesce(district, "Unknown"), cem_site = cem %in% "yes")
fac_seen <- bind_rows(
  reg |> transmute(code = facility, label = facility_lbl, district = district_lbl, cem_site),
  cem |> transmute(code = facility, label = facility_lbl, district = district_lbl, cem_site = TRUE),
  adr |> transmute(code = facility, label = facility_lbl, district = district_lbl, cem_site = NA)) |>
  filter(!is.na(code), !code %in% fac_form$code) |> group_by(code) |>
  summarise(label = coalesce(first(na.omit(label)), first(code)), district = coalesce(first(na.omit(district)), "Unknown"),
            cem_site = any(cem_site, na.rm = TRUE), .groups = "drop")
fac_map <- bind_rows(fac_form, fac_seen) |> arrange(district, label)
log_msg("facility list: ", nrow(fac_form), " from the form, ", nrow(fac_seen), " extra from the data")
fac_code <- function(x) coalesce(as.character(x), "unknown")

# ---- 1. Registration cube: facility x week -----------------------------------
reg_week <- reg |> filter(!is.na(registration_date)) |>
  group_by(f = fac_code(facility), w = wk(registration_date)) |>
  summarise(registered = n(),
            pf = sum(pf_positive == "yes", na.rm = TRUE),
            eligible = sum(screen_status == "eligible", na.rm = TRUE),
            not_eligible = sum(screen_status == "not_eligible", na.rm = TRUE),
            given = sum(sldp_given, na.rm = TRUE),
            given_elig = sum(sldp_given & screen_status == "eligible", na.rm = TRUE),
            dot = sum(sldp_given & yes(sldp_dot), na.rm = TRUE),
            food = sum(sldp_given & yes(with_food), na.rm = TRUE),
            counselled = sum(sldp_given & yes(counselled), na.rm = TRUE),
            dose_n = sum(!is.na(mg_per_kg)),
            dose_ok = sum(between(mg_per_kg, SLDP_DOSE_LOW, SLDP_DOSE_HIGH), na.rm = TRUE),
            cem_site_pts = sum(cem_site, na.rm = TRUE),
            enrolled = sum(cem_in, na.rm = TRUE),
            female = sum(sex == "female", na.rm = TRUE),
            under5 = sum(age_years < 5, na.rm = TRUE),
            agree_n = sum(!is.na(screen_status) & !is.na(provider_eligibility)),
            agree = sum((screen_status == "eligible") == (provider_eligibility == "yes"), na.rm = TRUE),
            .groups = "drop")

# Coverage among eligible by subgroup (age group, sex)
cov_group <- bind_rows(
  reg |> filter(screen_status == "eligible") |> group_by(f = fac_code(facility), level = as.character(age_group)) |>
    summarise(eligible = n(), given = sum(sldp_given), .groups = "drop") |> mutate(dim = "Age group"),
  reg |> filter(screen_status == "eligible") |> group_by(f = fac_code(facility), level = stringr::str_to_sentence(sex)) |>
    summarise(eligible = n(), given = sum(sldp_given), .groups = "drop") |> mutate(dim = "Sex")) |>
  mutate(level = coalesce(level, "Not recorded"))

# Why SLDP was not given; refusal reasons (select_multiple)
reasons <- reg |> filter(sldp_status == "not_given") |>
  count(f = fac_code(facility), reason = coalesce(no_sldp_reason_lbl, "Not recorded"), name = "n")
rc <- grep("^sldp_refusal_specific__", names(reg), value = TRUE)
refusal_labels <- cb_reg$choices |> filter(list_name %in% cb_reg$questions$list_name[cb_reg$questions$name == "sldp_refusal_specific"])
refusal <- if (length(rc)) reg |> filter(no_sldp_reason == "refusal") |> group_by(f = fac_code(facility)) |>
  summarise(across(all_of(rc), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
  tidyr::pivot_longer(-f, names_to = "code", values_to = "n") |> mutate(code = sub("sldp_refusal_specific__", "", code)) |>
  left_join(refusal_labels |> select(code, label), by = "code") |> transmute(f, reason = coalesce(label, code), n) |> filter(n > 0) else tibble()

# Dose per kg histogram, 0.025 mg/kg bins
dose_hist <- reg |> filter(!is.na(mg_per_kg)) |>
  mutate(bin = round(floor(pmin(mg_per_kg, 0.6) / 0.025) * 0.025, 3)) |> count(f = fac_code(facility), bin, name = "n")

# ---- 2. Follow-up (current snapshot) -----------------------------------------
followup <- visit_status |> count(f = fac_code(facility), visit = as.character(visit_type), status = as.character(status), name = "n")
on_time <- visit_status |> filter(status == "Completed") |>
  group_by(f = fac_code(facility), visit = as.character(visit_type)) |>
  summarise(completed = n(), on_time = sum(days_late_done <= grace, na.rm = TRUE), .groups = "drop")
overdue_age <- visit_status |> filter(status == "Overdue") |>
  mutate(days = as.numeric(TODAY - window_end),
         band = cut(days, c(-Inf, 3, 7, 14, Inf), labels = c("1-3 days", "4-7 days", "8-14 days", "Over 14 days"))) |>
  count(f = fac_code(facility), visit = as.character(visit_type), band = as.character(band), name = "n")
visits_week <- cem |> filter(completed, !is.na(visit_date)) |>
  count(f = fac_code(facility), w = wk(visit_date), visit = visit_type, name = "n")
phone <- cem |> filter(phone_visit) |>
  mutate(outcome = if_else(contact_success == "yes", "Reached", coalesce(contact_failure_lbl, "Not reached"))) |>
  count(f = fac_code(facility), visit = visit_type, outcome, name = "n")
retention <- patients |> filter(cem_in, !is.na(day0), day0 + 33 < TODAY) |>
  group_by(f = fac_code(facility)) |>
  summarise(closed = n(), d28 = sum(d28_done), withdrawn = sum(exit_status == "withdrawn", na.rm = TRUE),
            lost = sum(exit_status == "lost", na.rm = TRUE), died = sum(exit_status == "died", na.rm = TRUE), .groups = "drop")

# ---- 3. Safety ---------------------------------------------------------------
ev <- events |> mutate(w = wk(coalesce(onset, first_report)))
events_agg <- ev |> count(f = fac_code(facility), w, intensity = coalesce(max_intensity, "Not recorded"),
                          serious = ever_serious, outcome = coalesce(final_outcome, "Not recorded"), name = "n")
sae_agg <- ev |> group_by(f = fac_code(facility)) |>
  summarise(events = n(), serious = sum(ever_serious), serious_pv = sum(ever_serious & pv_submitted),
            hospitalised = sum(hospitalised), lag_n = sum(!is.na(report_lag)),
            lag_ok = sum(report_lag <= ADR_REPORT_LAG_TARGET, na.rm = TRUE),
            sldp_suspected = sum(sldp_suspected), .groups = "drop")
incidence <- patients |> filter(cem_in) |>
  group_by(f = fac_code(facility), arm = if_else(sldp_given, "SLDP given", "SLDP not given")) |>
  summarise(patients = n(), with_event = sum(any_event), with_sae = sum(any_serious),
            haemolysis = sum(haemolysis_sign, na.rm = TRUE), .groups = "drop")
hb <- patients |> filter(!is.na(hb_change)) |>
  group_by(f = fac_code(facility)) |>
  summarise(pairs = n(), alert = sum(hb_drop_alert), sum_d0 = sum(hb_d0), sum_d3 = sum(hb_d3), .groups = "drop")
hb_hist <- patients |> filter(!is.na(hb_change)) |>
  mutate(bin = pmax(pmin(floor(hb_change / 0.5) * 0.5, 3), -5)) |> count(f = fac_code(facility), bin, name = "n")
symptoms <- cem |> filter(phone_visit, contact_success == "yes") |>
  select(f = facility, visit = visit_type, any_of(symptom_vars)) |> mutate(f = fac_code(f)) |>
  tidyr::pivot_longer(-c(f, visit), names_to = "symptom", values_to = "ans") |>
  group_by(f, visit, symptom) |> summarise(n = sum(!is.na(ans)), yes = sum(ans == "yes", na.rm = TRUE), .groups = "drop")
d3_test <- cem |> filter(visit_type == "d3", completed, d3_repeat_test_done == "yes") |>
  group_by(f = fac_code(facility)) |>
  summarise(tested = n(), micro_n = sum(d3_microscopy_result %in% c("positive", "negative")),
            micro_pos = sum(d3_microscopy_result == "positive", na.rm = TRUE), .groups = "drop")

# ---- 4. Data quality ---------------------------------------------------------
dq <- issue_log |> count(f = fac_code(facility), form, check, severity = as.character(severity), name = "n")
linkage <- cem |> group_by(f = fac_code(facility)) |>
  summarise(visits = n(), matched = sum(reg_matched, na.rm = TRUE), manual = sum(baseline_source == "manual", na.rm = TRUE), .groups = "drop")
subs_week <- bind_rows(reg |> transmute(form = "Form 1 Registration", f = facility, d = submission_date),
                       cem |> transmute(form = "Form 2 CEM follow-up", f = facility, d = submission_date),
                       adr |> transmute(form = "Form 3 ADR/SAE", f = facility, d = submission_date)) |>
  filter(!is.na(d)) |> count(f = fac_code(f), w = wk(d), form, name = "n")
versions <- bind_rows(reg |> count(form = "Form 1 Registration", version = form_version),
                      cem |> count(form = "Form 2 CEM follow-up", version = form_version),
                      adr |> count(form = "Form 3 ADR/SAE", version = form_version))

# ---- 5. Write JSON -----------------------------------------------------------
# Data frames go out column-wise ({col: [values]}) to keep the file small.
cols <- function(df) if (is.null(df) || nrow(df) == 0) list() else as.list(as.data.frame(df))
out <- list(
  meta = list(generated_at = format(Sys.time(), "%Y-%m-%d %H:%M", tz = LOCAL_TZ), pulled_at = raw$registration$pulled_at %||% NA,
              today = format(TODAY), pilot_start = format(PILOT_START), is_demo = IS_DEMO,
              thresholds = list(dose_low = SLDP_DOSE_LOW, dose_high = SLDP_DOSE_HIGH, dose_target = SLDP_TARGET_MG_PER_KG,
                                hb_drop = HB_DROP_ALERT, report_lag = ADR_REPORT_LAG_TARGET),
              visit_schedule = cols(VISIT_SCHEDULE)),
  facilities = cols(fac_map),
  reg_week = cols(reg_week), cov_group = cols(cov_group), reasons = cols(reasons), refusal = cols(refusal), dose_hist = cols(dose_hist),
  followup = cols(followup), on_time = cols(on_time), overdue_age = cols(overdue_age), visits_week = cols(visits_week),
  phone = cols(phone), retention = cols(retention),
  events = cols(events_agg), sae = cols(sae_agg), incidence = cols(incidence), hb = cols(hb), hb_hist = cols(hb_hist),
  symptoms = cols(symptoms), d3_test = cols(d3_test),
  analysis = analysis_out,
  dq = cols(dq), linkage = cols(linkage), subs_week = cols(subs_week), versions = cols(versions))

# Last safety net: refuse to publish if any identifier slipped in
txt <- jsonlite::toJSON(out, auto_unbox = TRUE, na = "null", digits = 4)
if (grepl("GA-[0-9A-F]{8}|RPT-|\\+?233[0-9]{9}|\\b0[235][0-9]{8}\\b", txt)) stop("Identifier pattern found in dashboard JSON; not publishing.")
dir.create(dirname(OUT_JSON), recursive = TRUE, showWarnings = FALSE)
writeLines(txt, OUT_JSON)
log_msg("wrote ", OUT_JSON, " (", round(file.size(OUT_JSON) / 1024, 1), " KB)")
