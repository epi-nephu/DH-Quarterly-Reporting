# DH Quarterly Reporting
# Author: Alana Little, NEPHU (alana.little@austin.org.au)
# Version 2.1, 12/01/2026

# Setup for DH Quarterly Reporting

################################################################################
# Packages
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

quarter_start  <- lubridate::ymd("2025-10-01")
quarter_end    <- lubridate::ymd("2025-12-31")

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

# Model 1 incidents - specified urgent conditions
incident_urgent <- c("Measles", 
                     "Meningococcal infection", 
                     "Mpox")

# Model 1 incidents - specified locally acquired conditions 
incident_local <- c("Typhoid", 
                    "Paratyphoid", 
                    "Hepatitis A", 
                    "Legionella pneumophila", 
                    "Japanese encephalitis", 
                    "Murray Valley encephalitis virus infection", 
                    "Dengue virus infection", 
                    "Malaria", 
                    "Yellow fever")

# Model 1 incidents - specified high consequence conditions 
incident_rare <- c("Anthrax",
                   "Avian Influenza in humans", 
                   "Botulism", 
                   "Cholera", 
                   "Diphtheria",
                   "Haemolytic Uraemic Syndrome", 
                   "Lyssavirus - Australian Bat Lyssavirus", 
                   "Lyssavirus - other",
                   "Middle East Respiratory Syndrome (MERS)", 
                   "Plague", 
                   "Poliomyelitis", 
                   "Rabies",
                   "Severe Acute Respiratory Syndrome (SARS)", 
                   "Smallpox", 
                   "Tularaemia", 
                   "Viral haemorrhagic fevers")

# Model 1 incidents - overseas acquired specified enteric cases who work in high-risk occupation
incident_highrisk <- c("Hepatitis A", 
                       "Paratyphoid", 
                       "Shigellosis",
                       "Typhoid")

