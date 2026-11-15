# SLDP Pilot Monitoring Dashboard

Live monitoring and analysis dashboard for the single low-dose primaquine (SLDP) pilot in Greater Accra, built on the three linked KoboToolbox forms (registration, CEM follow-up and ADR/SAE reporting).

Developed by **Kingsley Amegah**, Data Scientist / Health Data Specialist.

**Live dashboard:** https://kingsley-amg.github.io/sldp-dashboard/

## What it shows

| Tab | Content |
|---|---|
| Overview | Headline indicators, "needs attention" alerts, CEM enrolment against the 1,500 target, weekly activity, today's follow-up queue (counts), facility scorecard |
| Enrolment vs targets | Each CEM site against its 5-month and monthly target, expected by today, last 28 days, projection at 5 months |
| Screening and treatment | Cascade from registration to CEM enrolment, reasons not eligible, refusal reasons, not dispensed, testing and positivity, age and sex |
| Dosing | Dosing table followed, mg/kg distribution, dose by weight band, DOT, counselling, ACT type |
| Follow-up | Status of every scheduled contact (Day 0 to 28), missed contacts, loss to follow-up, call outcomes, calls needed, timeliness, overdue ageing |
| Safety (CEM) | Adverse events from Form 2: incidence and rate, intensity, seriousness, signs, onset day, outcomes, Hb change, haemolysis signs, open events, reporting timeliness, protocol subgroups |
| Spontaneous reports | Form 3 reports for patients not enrolled in CEM |
| Statistical analysis | The protocol objectives: AE incidence (exact CIs, Kaplan-Meier), Hb change (paired t-test, Wilcoxon, regression), subgroups (children, anaemic, G6PD), risk factors (logistic and Cox regression), signal detection (PRR), retention and timeliness |
| Data quality | Automatic checks by severity, submissions per week, entry delay, completeness, form versions |
| Patient queue (locked) | Participants due, overdue, call-backs, open adverse events, missing Day 0 forms and those meeting the loss-to-follow-up definition, with names and phone numbers. Encrypted; opens with the queue password |
| Methods | Data flow, definitions and statistical methods |

Filters: period, district, facility and site type.

## Privacy

The public page holds **aggregate figures only**. Names, phone numbers, Patient IDs and Event IDs never go into `docs/data/dashboard.json`, and the build stops if an identifier pattern turns up.

The patient queue is the one place with names and phone numbers. R writes it to the runner's temporary folder; `tools/encrypt_queue.mjs` encrypts it with the `QUEUE_PASSWORD` secret (AES-256-GCM, key from PBKDF2-SHA256 with 310,000 iterations) into `docs/data/queue.enc.json`. The browser decrypts it only when the right password is typed; the password is never sent anywhere. Without the secret, no queue is published. Use a long password (at least 12 characters, a passphrase is best), share it only with the CEM team, and change the secret if someone leaves the team.

## How it updates

`.github/workflows/update-dashboard.yml` runs every 6 hours, on each push to `main`, and on demand:

1. `R/pipeline.R` downloads the three forms from the Kobo API, removes TEST records, then cleans, validates and links them (patients, CEM contacts, adverse events, Form 3 reports, the visit schedule).
2. `R/analysis.R` runs the statistical analysis for the protocol objectives.
3. `R/build_dashboard_data.R` writes `docs/data/dashboard.json` (aggregates) and the private queue.
4. `tools/encrypt_queue.mjs` encrypts the queue into `docs/data/queue.enc.json`.
5. `docs/` is deployed to GitHub Pages. The page (`docs/index.html`) does all filtering in the browser.

Until the `KOBO_TOKEN` secret is set, the site shows demonstration data, with a banner saying so.

## One-time setup

1. **Kobo token:** Settings > Secrets and variables > Actions > New repository secret. Name `KOBO_TOKEN`, value: your Kobo API key (Kobo > Account settings > Security).
2. **Queue password:** same place, new secret named `QUEUE_PASSWORD`. At least 10 characters.
3. **Pilot start date:** Settings > Secrets and variables > Actions > Variables > New repository variable `SLDP_PILOT_START` (for example `2026-12-01`). It sets the enrolment clock for "expected by today". Default: 2026-10-01.
4. **Pages:** Settings > Pages > Build and deployment > Source: **GitHub Actions**.
5. **First update:** Actions > Update SLDP dashboard > Run workflow.

## Run locally

```r
# KOBO_TOKEN in ~/.Renviron
Rscript R/build_dashboard_data.R        # writes docs/data/dashboard.json and data/raw/queue.json (private)
python3 -m http.server -d docs 8000     # open http://localhost:8000
```

Test without Kobo, using invented data in the exact Kobo format:

```bash
python3 tools/make_demo_data.py --forms <folder with the three XLSForms> --out /tmp/sldp_raw --today 2026-11-15
SLDP_USE_CACHE=true SLDP_RAW_DIR=/tmp/sldp_raw SLDP_TODAY=2026-11-15 SLDP_DEMO=true Rscript R/build_dashboard_data.R
QUEUE_PASSWORD=choose-a-password node tools/encrypt_queue.mjs /tmp/sldp_raw/queue.json docs/data/queue.enc.json
```

Site targets and protocol constants (Hb threshold, reporting deadlines, visit windows) are at the top of `R/pipeline.R`.
