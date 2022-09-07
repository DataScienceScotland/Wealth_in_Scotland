library(haven)
library(readxl)

waves <- c("w1", "w2", "w3", "w4", "w5", "r5", "r6", "r7")

# Person-level -----------------------------------------------------------------
# Person-level SAS datasets are large (> 1 GB), therefore download to data 
# folder before importing

filenames_p <- paste0("data/", c("was_wave_1_person_v4",
                                 "was_wave_2_person_v5",
                                 "was_wave_3_person_v6",
                                 "was_wave_4_person_v3",
                                 "was_wave_5_person_v3",
                                 "was_round_5_person", 
                                 "was_round_6_person_v2",
                                 "was_round_7_person"), 
                      ".sas7bdat")

# Read in files individually and save as RDS

saveRDS(read_sas(filenames_p[1]), "data/files_person_w1.rds")
saveRDS(read_sas(filenames_p[2]), "data/files_person_w2.rds")
saveRDS(read_sas(filenames_p[3]), "data/files_person_w3.rds")
saveRDS(read_sas(filenames_p[4]), "data/files_person_w4.rds")
saveRDS(read_sas(filenames_p[5]), "data/files_person_w5.rds")
saveRDS(read_sas(filenames_p[6]), "data/files_person_r5.rds")
saveRDS(read_sas(filenames_p[7]), "data/files_person_r6.rds") 
saveRDS(read_sas(filenames_p[8]), "data/files_person_r7.rds")

# Household-level --------------------------------------------------------------

# Path, file names
path <- # -- path redacted --

filenames_h <- paste0(path, c("was_wave_1_hhold_v4",
                              "was_wave_2_hhold_v5",
                              "was_wave_3_hhold_v6",
                              "was_wave_4_hhold_v3",
                              "was_wave_5_hhold_v4",
                              "was_round_5_hhold", 
                              "was_round_6_hhold_v2",
                              "was_round_7_hhold"), 
                      ".sas7bdat")

# Import SAS dataset (takes long)
files_h <- lapply(filenames_h[7:8], read_sas)

# Name list items
names(files_h) <- waves[7:8]

# Add to existing hh files
files_h_existing <- readRDS("data/files_household.rds")
files_h_existing[[waves[7]]] <- files_h[[waves[7]]]
files_h_existing[[waves[8]]] <- files_h[[waves[8]]]

# Save in RDS format
saveRDS(files_h_existing, "data/files_household.rds")

# Clear workspace
remove(files_h, files_h_existing)


# Problem debt -----------------------------------------------------------------

# Import SAS dataset 
saveRDS(read_sas(" -- path redacted -- "),
        "data/files_problemdebt.rds")


# Inflation index --------------------------------------------------------------

inflationindex <- read_xlsx("data/inflationindex.xlsx")
saveRDS(inflationindex, "data/inflationindex.rds")


# Clear workspace
rm(list = ls())
