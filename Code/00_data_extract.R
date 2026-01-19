# DH Quarterly Reporting
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 2.1, 19/01/2026

# Data extraction from PHAR for DH Quarterly Reporting

################################################################################
# Austin proxy shenanigans
################################################################################
# Note: useProxy = 1 when in Austin offices, useProxy = 0 when WFH
con <- DBI::dbConnect(odbc::odbc(), "PHAR", useProxy = 1)

################################################################################
# Extract case and outbreak data from PHAR
################################################################################
# Select all NEPHU notifications during reporting period
# Exclude conditions not managed by LPHUs and FBWB illness
# Separate legionellosis into longbeachae vs. pneumophila/others
phar_nephu <- DBI::dbGetQuery(con,
                              glue::glue("SELECT * FROM dh_public_health.phess_release.caseevents
                                          WHERE EVENT_DATE >= DATE '{lookback_start}'
                                          AND EVENT_DATE <= DATE '{quarter_end}'
                                          AND (LGA IN ({lga_name_sql})
                                               OR ASSIGNED_LPHU = 'North Eastern')")) %>% 
  janitor::clean_names() %>% 
  #
  dplyr::filter(event_type %in% c("Case", "Contact/exposed person")) %>%
  #
  dplyr::filter(!condition %in% c("Food-borne or water-borne illness",
                                  "Anaphylaxis",
                                  "Blood lead greater than 5 ug/dL",
                                  "Tuberculosis",
                                  "Non-notifiable")) %>% 
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
# Wrangle outbreak data
################################################################################
outbreaks_nephu <- phar_outbreaks %>% 
  dplyr::mutate(event_date = lubridate::ymd(event_date)) %>% 
  dplyr::filter(event_date >= quarter_start & event_date <= quarter_end)

################################################################################
# Wrangle case data
################################################################################
# Restrict to notifications signed out by DH during reporting period
cases_nephu <- phar_nephu %>%
  dplyr::left_join(phar_signouts, by = "phess_id") %>%
  #
  dplyr::mutate(date_signed_dh   = lubridate::date(datetime_signed_dh),
                date_signed_lphu = lubridate::date(datetime_signed_lphu)) %>%
  #
  dplyr::filter(date_signed_dh >= quarter_start & date_signed_dh <= quarter_end) 

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
cases_nephu <- cases_nephu %>% 
  dplyr::left_join(reference_conditions, by = "condition") %>% 
  dplyr::left_join(phar_risk, by = "phess_id") %>% 
  dplyr::left_join(phar_occupation, by = "phess_id")

################################################################################
# Subset urgent notifications
################################################################################
urgent_nephu <- cases_nephu %>% 
  dplyr::filter(condition %in% conditions_urgent)





# Dedup - completion and Aboriginal status indicators - keep one row per case
# Completion - investigation_completed_date or reinvestigation_status_date in reporting period
cases_nephu %>% 
  dplyr::arrange(phess_id, date_signed_dh) %>%
  #
  dplyr::distinct(phess_id, .keep_all = TRUE)

# Some more data wrangling - response within 24hr indicator - need to dedup and keep first signout
cases_nephu <- cases_nephu %>% 
  dplyr::mutate(datetime_signed_dh   = lubridate::ymd_hms(datetime_signed_dh),
                datetime_signed_lphu = lubridate::ymd_hms(datetime_signed_lphu),
                #
                week_day = lubridate::wday(datetime_signed_dh, label = TRUE),
                #
                response_minutes = as.numeric(datetime_signed_lphu - datetime_signed_dh),
                response_hours   = as.numeric(response_minutes / 60),
                response_days    = as.numeric(response_hours / 24),
                #
                response_24hr = dplyr::case_when(
                  week_day == "Fri" & response_hours <= 72 ~ "Within 24hr",
                  week_day != "Fri" & response_hours <= 24 ~ "Within 24hr",
                  TRUE ~ "More than 24hr")) %>% 
  #
  dplyr::arrange(condition,
                 event_date)



