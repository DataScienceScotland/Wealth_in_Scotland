

# Prep data
source("R/01_import_SAS_datasets.R", encoding = "UTF-8")
source("R/02_clean_household_datasets.R", encoding = "UTF-8")
source("R/02_clean_person_datasets.R", encoding = "UTF-8")
source("R/03_make_tidy_datasets.R", encoding = "UTF-8")


# Produce charts and tables for report
source("R/04_make_charts1.R", encoding = "UTF-8")
source("R/04_make_charts2.R", encoding = "UTF-8")
source("R/04_make_charts3.R", encoding = "UTF-8")
source("R/04_make_charts4.R", encoding = "UTF-8")
source("R/04_make_charts5.R", encoding = "UTF-8")

# Produce spreadsheet
source("R/05_make_spreadsheet.R", encoding = "UTF-8")
source("R/06_spreadsheet_postprocessing.R", encoding = "UTF-8")

# Render website
rmarkdown::clean_site(input = "./website")
rmarkdown::render_site(input = "./website",
                       output_format = 'bookdown::html_document2',
                       encoding = 'UTF-8')

