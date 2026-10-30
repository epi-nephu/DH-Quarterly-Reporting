# DH Quarterly Reporting
# Author: Alana Little, NEPHU (alana.little@austin.org.au)

# Setup and extract data from PHAR for DH Quarterly Reporting

################################################################################
# Load packages
################################################################################
# Install the pacman package if you don't already have it
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               janitor,
               lubridate,
               here,
               odbc,
               DBI,
               glue,
               rmarkdown,
               knitr,
               kableExtra,
               writexl)

################################################################################
# Define dates for reporting period
################################################################################
integration_start <- lubridate::ymd("2022-06-30")

quarter_start <- lubridate::ymd("2026-07-01")
quarter_end   <- lubridate::ymd("2026-09-30")

reporting_period <- "2027 Q1"

################################################################################
# Define constants
################################################################################
# NEPHU LGAs
nephu_lgas <- c("Banyule (C)", "Boroondara (C)", "Darebin (C)", "Hume (C)",
                "Knox (C)", "Manningham (C)", "Maroondah (C)", "Nillumbik (S)",
                "Whitehorse (C)", "Whittlesea (C)", "Yarra (C)", "Yarra Ranges (S)")

lga_name_sql <- glue::glue_collapse(glue::glue("'{nephu_lgas}'"), sep = ", ")

# Urgent conditions (FBWB disease excluded)
conditions_urgent <- c("Anthrax", 
                       "Botulism", 
                       "Candida auris", 
                       "Cholera",
                       "Diphtheria", 
                       #"Food-borne or water-borne illness",
                       "Haemolytic Uraemic Syndrome", 
                       "Haemophilus influenzae type B infection", 
                       "Hepatitis A",
                       "Invasive Group A Streptococcus",
                       "Japanese encephalitis",
                       "Legionellosis",
                       "Listeriosis",
                       "Lyssavirus - Australian Bat Lyssavirus",
                       "Lyssavirus - other",
                       "Measles",
                       "Meningococcal infection",
                       "Middle East Respiratory Syndrome (MERS)",
                       "Mpox",
                       "Murray Valley encephalitis virus infection",
                       "Paratyphoid",
                       "Plague",
                       "Poliomyelitis",
                       "Rabies",
                       "Severe Acute Respiratory Syndrome (SARS)",
                       "Smallpox",
                       "Tularaemia",
                       "Typhoid",
                       "Viral haemorrhagic fevers",
                       "Yellow fever")

# TRA conditions
conditions_tra <- c("Carbapenemase producing acinetobacter",
                    "Carbapenemase producing enterobacteriaceae",
                    "Carbapenemase producing pseudomonas",
                    "VanA Vancomycin resistant enterococcus")

# Lyssaviruses
conditions_lyssa <- c("Lyssavirus - Australian Bat Lyssavirus",
                      "Lyssavirus - other",
                      "Rabies")

################################################################################
# Austin proxy shenanigans
################################################################################
# Note: useProxy = 1 when in Austin offices, useProxy = 0 when WFH
proxy_status <- dplyr::case_when(Sys.getenv("http_proxy") == "http://server340v.armc.org.au:8080" ~ 1,
                                 TRUE ~ 0)

# Connect to PHAR
con <- DBI::dbConnect(odbc::odbc(), "PHAR", useProxy = proxy_status, EnableQueryResultDownload = 0)

################################################################################
# Extract case and outbreak data from PHAR
################################################################################
# Select all NEPHU notifications since the start of integration
# Exclude conditions not managed by LPHUs
# Separate legionellosis into longbeachae vs. pneumophila/others
phar_nephu <- DBI::dbGetQuery(con,
                              glue::glue("SELECT * FROM dh_public_health.phess_release.caseevents_nrt
                                          WHERE EVENT_DATE >= DATE '{integration_start}'
                                          AND EVENT_DATE <= DATE '{quarter_end}'")) %>% 
  janitor::clean_names() %>% 
  #
  dplyr::filter(event_type %in% c("Case", "Contact/exposed person")) %>%
  #
  dplyr::filter(lga %in% nephu_lgas | assigned_lphu == "North Eastern") %>% 
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
                atsi_status_collection_attempted,
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

# Select NEPHU outbreaks
phar_outbreaks <- DBI::dbGetQuery(con,
                                  glue::glue("SELECT * FROM dh_public_health.phess_release.outbreakevents")) %>% 
  janitor::clean_names() %>% 
  #
  dplyr::filter(exposure_site_lga %in% nephu_lgas | assigned_lphu == "North Eastern")

# Close the PHAR connection
DBI::dbDisconnect(con)

################################################################################
# Data wrangling
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

# Join condition and signout information to case data
cases_nephu <- phar_nephu %>% 
  dplyr::left_join(phar_signouts, by = "phess_id") %>%
  #
  dplyr::mutate(event_date = lubridate::ymd(event_date),
                #
                date_signed_dh   = lubridate::date(datetime_signed_dh),
                date_signed_lphu = lubridate::date(datetime_signed_lphu)) %>%
  #
  dplyr::left_join(reference_conditions, by = "condition") 

