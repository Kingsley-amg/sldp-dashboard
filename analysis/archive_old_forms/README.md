# Archived: analysis scripts for the earlier form versions

These two files were written for the forms as they stood on 6 November 2026
(field names such as `patient_name`, `sldp_status`, `visit_type`). The forms were
revised in mid-November 2026 (Form 2 now holds the adverse-event blocks for CEM
participants; Form 3 is only for patients not enrolled in CEM), so these scripts no
longer match the Kobo data. They are kept for reference only.

The current analysis is `R/pipeline.R` + `R/analysis.R`, which the dashboard runs.
To work with the cleaned tables yourself in R:

```r
params <- list(use_cache = FALSE, drop_test = TRUE, pilot_start = "2026-10-01",
               kobo_server = "https://eu.kobotoolbox.org")
source("R/pipeline.R")   # reg, visits, cem_events, adr, adr_events, cohort, visit_status, issue_log
source("R/analysis.R")   # analysis_out (list of results)
```
