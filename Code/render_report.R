# DH Quarterly Reporting
# Author: Alana Little, NEPHU (alana.little@austin.org.au)

# Load packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(here,
               tidyverse,
               rmarkdown,
               quarto,
               fs)

# Update file name for output html
outname <- paste0(reporting_period,
                  " DH Quarterly Reporting",
                  ".html")

# Render SitRep html - change input file as needed
quarto::quarto_render(
  input = "Code/quarterly_reporting.qmd",
  output_file = outname
)

# Move html from Code to Output folder
source_path <- paste0(here::here(), "/Code/", outname)
dest_folder <- paste0(here::here(), "/Output")

fs::file_move(path = source_path, new_path = file.path(dest_folder, basename(source_path)))

# Copy html to Secure Communicable Diseases folder
