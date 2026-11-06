# SLDP Pilot Monitoring Dashboard

Live monitoring and analysis dashboard for the single low-dose primaquine (SLDP) pilot in Greater Accra, built on the three linked KoboToolbox forms (registration, CEM follow-up and ADR/SAE reporting).

Developed by **Kingsley Amegah**, Data Scientist / Health Data Specialist.

**Live dashboard:** https://kingsley-amg.github.io/sldp-dashboard/

## What it shows

| Tab | Content |
|---|---|
| Overview | Headline indicators, weekly registrations, coverage trend, facility scorecard |
| Coverage and dosing | Coverage by facility and subgroup, reasons not given, refusal reasons, dose per kg |
| Follow-up | Visit status (Day 0 to Day 28), overdue ageing, weekly activity, telephone outcomes, retention |
| Safety | Adverse events, SAEs, pharmacovigilance reporting, Hb change, symptoms, Day 3 positivity |
| Data quality | Automatic checks by severity, linkage, submissions per week, form versions |
| Statistical analysis | Mixed-effects logistic regression, Cohen's kappa, risk ratio, Wilcoxon and LMM for Hb, Kaplan-Meier, Cox model, retention, McNemar test, missingness |
| Methods and definitions | Every indicator defined |

You can filter by period, district, facility and site type.

## Privacy

The page is public, so it holds **aggregate figures only**. Names, phone numbers, Patient IDs, Event IDs and free text never leave the build script, and the script refuses to publish if any identifier pattern turns up. Raw Kobo downloads are written to the runner's temporary folder and are never committed or published.

## How it updates

`.github/workflows/update-dashboard.yml` runs every 6 hours, on each push to `main`, and on demand:

1. `R/pipeline.R` downloads the three forms from the Kobo API, then cleans, validates and links them. This is the same logic as `analysis/SLDP_monitoring_analysis.Rmd`.
2. `R/analysis.R` fits the statistical models.
3. `R/build_dashboard_data.R` writes `docs/data/dashboard.json`, which contains aggregates only.
4. `docs/` is deployed to GitHub Pages. The page (`docs/index.html`) does all filtering in the browser.

Until the `KOBO_TOKEN` secret is set, the site shows demonstration data, with a banner saying so.

## One-time setup

1. **Add the Kobo token:** Settings > Secrets and variables > Actions > New repository secret. Name it `KOBO_TOKEN` and paste your Kobo API key as the value (Kobo > Account settings > Security).
2. **Turn on Pages:** Settings > Pages > Build and deployment > Source: **GitHub Actions**.
3. **Run the first update:** Actions > Update SLDP dashboard > Run workflow.

## Run locally

```r
# KOBO_TOKEN in ~/.Renviron
Rscript R/build_dashboard_data.R        # writes docs/data/dashboard.json
python3 -m http.server -d docs 8000     # open http://localhost:8000
```

To change the targets used for the status colours, edit `TARGETS` at the top of the script in `docs/index.html`.
