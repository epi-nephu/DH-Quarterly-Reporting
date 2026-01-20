# DH Quarterly Reporting
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 2.1, 12/01/2026

# Data extraction from PHAR for DH Quarterly Reporting

################################################################################
# Austin proxy shenanigans
################################################################################
# Note: useProxy = 1 when in Austin offices, useProxy = 0 when WFH
con <- DBI::dbConnect(odbc::odbc(), "PHAR", useProxy = 0)

################################################################################
# Extract case and outbreak data from PHAR
################################################################################
# Select all NEPHU notifications since the start of integration
# Exclude conditions not managed by LPHUs
# Separate legionellosis into longbeachae vs. pneumophila/others
phar_nephu <- DBI::dbGetQuery(con,
                              glue::glue("SELECT * FROM dh_public_health.phess_release.caseevents
                                          WHERE EVENT_DATE >= DATE '{integration_start}'
                                          AND EVENT_DATE <= DATE '{quarter_end}'
                                          AND (ASSIGNED_LPHU = 'North Eastern')")) %>% 
  janitor::clean_names() %>% 
  #
  dplyr::filter(event_type %in% c("Case", "Contact/exposed person")) %>%
  #
  dplyr::filter(!condition %in% c("Anaphylaxis",
                                  "Blood lead greater than 5 ug/dL",
                                  "Human biosecurity report",
                                  "Tuberculosis")) %>% 
  #
  dplyr::mutate(condition = dplyr::case_when(
    organism_cause == "Legionella longbeachae" ~ "Legionella longbeachae",
    condition == "Legionellosis"               ~ "Legionella pneumophila",
    TRUE ~ condition)) %>%
  #
  dplyr::select(phess_id = event_id,
                condition,
                event_type,
                event_date,
                event_classification,
                lga,
                assigned_lphu,
                indigenous_status,
                investigation_status,
                investigation_completed_date,
                reinvestigation_status,
                reinvestigation_status_date)

# Select all notifications signed out to LPHUs for follow-up during reporting period
phar_signouts <- DBI::dbGetQuery(con,
                                 glue::glue("SELECT * FROM dh_public_health.phess_release.caseepiactions
                                             WHERE EPI_TICKED_FOR_FOLLOW_UP_UNIT = 'Local Public Health Unit'")) %>% 
  janitor::clean_names() %>% 
  #
  dplyr::select(phess_id = event_id,
                datetime_signed_dh = epi_noted_when,
                datetime_signed_lphu = epi_ticked_acknowledged_when)

# Select risk factor information for Model 1 incident calculations
phar_risk <- DBI::dbGetQuery(con,
                             glue::glue("SELECT * FROM dh_public_health.phess_release.caseexposures")) %>% 
  janitor::clean_names() %>% 
  #
  dplyr::select(phess_id = event_id,
                pep_cab = antibiotic_prophylaxis_recommended_for_contacts,
                pep_vax = immunisation_recommended_for_contacts,
                travel_overseas = risk_factors_travel_overseas_country_specify) %>% 
  #
  dplyr::mutate(travel_overseas = dplyr::case_when(
    !is.na(travel_overseas) ~ "Yes",
    TRUE ~ NA_character_)) %>% 
  #
  dplyr::distinct(phess_id, .keep_all = TRUE)

phar_occupation <- DBI::dbGetQuery(con,
                                   glue::glue("SELECT * FROM dh_public_health.phess_release.caseworkstudycare")) %>% 
  janitor::clean_names() %>% 
  #
  dplyr::select(phess_id = event_id,
                work_status = work_study_care,
                occupation) %>% 
  #
  dplyr::distinct(phess_id, .keep_all = TRUE)

# Select NEPHU outbreaks for Model 1 incident calculations
phar_outbreaks <- DBI::dbGetQuery(con,
                                  glue::glue("SELECT * FROM dh_public_health.phess_release.outbreakevents")) %>% 
  janitor::clean_names() %>% 
  #
  dplyr::filter(exposure_site_lga %in% nephu_lgas | assigned_lphu == "North Eastern")

# Close the PHAR connection
DBI::dbDisconnect(con)

################################################################################
# Data wrangling - cases
################################################################################
# Read in conditions reference list
reference_conditions <- readxl::read_xlsx(paste0(here::here(), "/Data/ConditionsReferenceList", ".xlsx"),
                                          sheet     = 1,
                                          guess_max = min(100000, Inf)) %>%
  janitor::clean_names() %>% 
  #
  dplyr::select(condition,
                short_name,
                reporting_group) %>% 
  #
  dplyr::distinct(condition, .keep_all = TRUE)

# Join condition and risk factor information to case data
cases_nephu <- phar_nephu %>% 
  dplyr::left_join(phar_signouts, by = "phess_id") %>%
  #
  dplyr::mutate(event_date = lubridate::ymd(event_date),
                #
                date_signed_dh   = lubridate::date(datetime_signed_dh),
                date_signed_lphu = lubridate::date(datetime_signed_lphu)) %>%
  #
  dplyr::left_join(reference_conditions, by = "condition") %>% 
  dplyr::left_join(phar_risk, by = "phess_id") %>% 
  dplyr::left_join(phar_occupation, by = "phess_id")

################################################################################
# Data wrangling - outbreaks
################################################################################
# Outbreaks completed/closed during reporting period
outbreaks_nephu <- phar_outbreaks %>% 
  dplyr::mutate(investigation_completed_date = lubridate::ymd(investigation_completed_date)) %>% 
  #
  dplyr::filter(investigation_completed_date >= quarter_start & investigation_completed_date <= quarter_end)

