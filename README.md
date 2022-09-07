# Code used to create Wealth in Scotland official statistics report

Contains the R and R Markdown code for the Wealth in Scotland 2006-2020 report at https://data.gov.scot/wealth


## Notes

- the datasets required to produce the charts and tables are not included in this repository, same for the intermediate .rds datasets created in the R scripts
- the logo is also not included in this repository

## Files

### Project file

- R_wealth_report.Rproj

### Helper scripts

- R/00_colours.R
- R/00_functions.R
- R/00_strings.R

### Data prep scripts

- R/01_import_SAS_datasets.R
- R/02_clean_household_datasets.R
- R/02_clean_person_datasets.R
- R/03_make_tidy_datasets.R

### Produce charts and tables for report

- R/04_make_charts1.R
- R/04_make_charts2.R
- R/04_make_charts3.R
- R/04_make_charts4.R
- R/04_make_charts5.R

### Produce spreadsheet

- R/05_make_spreadsheet.R
- R/06_spreadsheet_postprocessing.R

### To run all of the above and render the website

- R/07_create_website.R

Each of the scripts above can also be run independently once the previous script has been run before at some point and saved an output to the data folder.

### Website files - content

- website/index.Rmd

- website/_chapters/_chapter00.Rmd
- website/_chapters/_chapter01.Rmd
- website/_chapters/_chapter02.Rmd
- website/_chapters/_chapter03.Rmd
- website/_chapters/_chapter04.Rmd
- website/_chapters/_chapter05.Rmd
- website/_chapters/_chapter06.Rmd

- website/accessibility.Rmd

### Website files - structure

- website/_template.html
- website/_site.yml
- website/_navbar.html
- website/_footer.html
- website/styles22.css


### VBA files for Excel postprocessing

- Run Excel Macro.vbs
- vba_for_postprocessing.xlsm




