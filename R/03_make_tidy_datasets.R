# Prelims ----------------------------------------------------------------------

library(tidyverse)
library(naniar)

source("R/00_functions.R")

hhld <- readRDS("data/clean_household.rds")
pers <- readRDS("data/clean_person.rds")
inflationindex <- readRDS("data/inflationindex.rds")
problemdebt <- readRDS("data/files_problemdebt.rds")

# Remove variable labels to prevent warning messages when joining datasets
attr(hhld$hhserial, "label") <- NULL
attr(pers$hhserial, "label") <- NULL


# Household dataset ------------------------------------------------------------

# Add variables 

hhld <- hhld %>%
  
  # add hrpsex to R6 dataset - chech if same issue for R7
  left_join(select(pers, wave, hhserial, hrpsex) %>%
              filter(wave == "r6") %>%
              rename(sex_r6 = hrpsex) %>%
              unique(), 
            by = c("wave", "hhserial")) %>%
  mutate(hrpsex = ifelse(wave == "r6", sex_r6, hrpsex)) %>%
  select(-sex_r6) %>%
  
  # add number of under 14s
  left_join(select(pers, wave, hhserial, dvage) %>%
              group_by(wave, hhserial) %>%
              summarise(u14s = sum(dvage < 14)),
            by = c("wave", "hhserial")) %>%
  
  # add equivalence factor
  mutate(numdepch = ifelse(numdepch < u14s, u14s, numdepch),
         dvhsize = ifelse(dvhsize < numdepch + numadult, numdepch + numadult, dvhsize),
         equ = 0.67 + (dvhsize - u14s - 1) * 0.33 + u14s * 0.2) %>%
  
  # add problem debt flag to w3 and w4 datasets
  left_join(problemdebt %>%
              filter(wave == "w3") %>%
              rename(hhpd_w3 = hhpd),
            by = c("wave", "hhserial")) %>%
  left_join(problemdebt %>%
              filter(wave == "w4") %>%
              rename(hhpd_w4 = hhpd),
            by = c("wave", "hhserial")) %>%
  mutate(hhpd = case_when(wave == "w3" ~ hhpd_w3,
                          wave == "w4" ~ hhpd_w4,
                          TRUE ~ hhpd)) %>%
  select(-hhpd_w3, -hhpd_w4) %>%

  # add numeric wave variable
  mutate(wavenum = case_when(wave == "w1" ~ 1,
                             wave == "w2" ~ 2,
                             wave == "w3" ~ 3,
                             wave == "w4" ~ 4,
                             wave == "w5" ~ 5,
                             wave == "r5" ~ 6,
                             wave == "r6" ~ 7,
                             wave == "r7" ~ 8),
         # add deflators
         infl = case_when(wave == "w1" ~ inflationindex[[1]],
                          wave == "w2" ~ inflationindex[[2]],
                          wave == "w3" ~ inflationindex[[3]],
                          wave == "w4" ~ inflationindex[[4]],
                          wave == "w5" ~ inflationindex[[5]],
                          wave == "r5" ~ inflationindex[[6]],
                          wave == "r6" ~ inflationindex[[7]],
                          wave == "r7" ~ inflationindex[[8]])) %>%
  
  # add household disability status
  left_join(pers %>%
              mutate(disabled = ifelse(lsill == 1 & illlim == 1, 1, 0)) %>%
              group_by(wave, hhserial) %>%
              summarise(disabled_hh = max(disabled)),
            by = c("wave", "hhserial")) %>%
  
  # add premiumbond data
  left_join(pers %>% filter(fnsav1 == 2,
                            dvfnsval > 0) %>%
              group_by(wave, hhserial) %>%
              summarise(premiumbonds = sum(dvfnsval, na.rm = TRUE)),
            by = c("wave", "hhserial")) %>%
  mutate(premiumbonds = ifelse(is.na(premiumbonds), 0, premiumbonds)) %>%
  
  # recode variables for financial resilience (set missing codes to 0)
  mutate(dvcacrval = negtozero(dvcacrval), # current accounts in credit
         dvsaval = negtozero(dvsaval) ,  # savings accounts
         dvisaval = negtozero(dvisaval), # ISAs
         dvfshukv = negtozero(dvfshukv), # UK shares
         dvfeshares = negtozero(dvfeshares), # employee shares
         dvfcollv = negtozero(dvfcollv), # unit and investment trusts
         dvfshosv = negtozero(dvfshosv), # overseas shares
         dvcaodval = negtozero(dvcaodval), # Current account overdrafts
         totarr_excmort = negtozero(totarr_excmort), # All arrears excl. mortgage arrears
         marrsv1 = negtozero(marrsv1), # Mortgage arrears
         marrsv2 = negtozero(marrsv2), # Mortgage arrears
         marrsv3 = negtozero(marrsv3)) %>% # Mortgage arrears
  
  # calculate liquid assets
  mutate(liquid_assets = dvcacrval + dvsaval + dvisaval + dvfshukv + dvfeshares +
           dvfcollv + dvfshosv + premiumbonds - dvcaodval - totarr_excmort - 
           marrsv1 - marrsv2 - marrsv3) %>%
  
  # add factor levels
  mutate(gor = factor(gor, levels = c(1:12), 
                      labels = c(rep("England", 10), "Wales", "Scotland")),
         
         wave = factor(wave, levels = c("w1", "w2", "w3", "w4", "w5", "r5", 
                                        "r6", "r7"), 
                       ordered = TRUE),
         
         hholdtype = factor(hholdtype, levels = c(1:10),
                            labels = c("Single pensioner",
                                       "Single working-age",
                                       "Pensioner couple",
                                       "Working-age couple",
                                       "Pensioner couple",
                                       "Couple with children",
                                       "Other",
                                       "Lone parent",
                                       rep("Other", 2))),
         
         hrpsex = factor(hrpsex, levels = c(1, 2), labels = c("Male", "Female")),
         
         hrpedlevel = factor(hrpedlevel, levels = c(-9:-6, 1:4),
                             labels = c(rep("DK/NA", 4), 
                                        "Degree-level or above",
                                        rep("Other qualification", 2), 
                                        "No qualification")),
         hrpdvage9 = factor(hrpdvage9, levels = c(1:9),
                            labels = c("0-15", rep("16-34", 2), "35-44", "45-54",
                            "55-64", "65-74", rep("75+", 2))),
         
         hrpethnic = factor(hrpethnic, levels = c(-9:-6, 1:15),
                            labels = c(rep("DK/NA", 4),
                                       "White British",
                                       rep("Minority ethnic", 14))),
         
         hrpdvecact = factor(hrpdvecact, levels = c(0:9),
                             labels = c("Ineligible / missing",
                                        "Employed",
                                        "Self-employed",
                                        rep("Inactive/Unemployed", 5),
                                        "Retired",
                                        "Inactive/Unemployed")),
         
         hrpdvmrdf = factor(hrpdvmrdf, levels = c(-9:-6, 1:9),
                            labels = c(rep("DK/NA", 4),
                                       "Married",
                                       "Cohabiting",
                                       "Single",
                                       "Widowed",
                                       "Divorced",
                                       "Divorced",
                                       "Cohabiting",
                                       "Married",
                                       "Divorced")),
         
         tenure = factor(tenure, levels = c(-9:-6, 1:7),
                         labels = c(rep("DK/NA", 4),
                                    'Owning outright',
                                    rep('Buying with mortgage', 2),
                                    rep('Private renting', 3),
                                    'Social renting')),
         
         urindsc = factor(urindsc, levels = c(1, 2), 
                          labels = c("Urban", "Rural")),
         
         disabled_hh = factor(disabled_hh, levels = c(0, 1),
                              labels = c("No-one disabled",
                                         "Someone disabled")),
         
         children_hh = ifelse(numdepch >= 1, "With children", "No children"),
         have_pro = ifelse(dvproperty > 0, 1, 0),
         have_pen = ifelse(penwlth > 0, 1, 0))

# Person dataset ---------------------------------------------------------------

pers <- pers %>%
  
  # Add key variables from household dataset
  mutate(wave = factor(wave, levels = c("w1", "w2", "w3", "w4", "w5", "r5", 
                                        "r6", "r7"), 
                       ordered = TRUE)) %>%
  left_join(select(hhld, wave, hhserial, gor, wgt, wgt_halfsample, wavenum, 
                   infl, netequincann, psu, stratum),
            by = c("wave", "hhserial")) %>%
  
  # split pension wealth variable into 'in payment' and 'saving'
  mutate(savpen = totpen - dvpinpval,
         paypen = dvpinpval) %>%
  
  # add factor levels
  mutate(dvage9 = factor(dvage9, levels = c(1:9),
                         labels = c("0-15", rep("16-34", 2), "35-44", "45-54",
                                       "55-64", "65-74", rep("75+", 2))),
         sex = factor(sex, levels = c(1, 2), labels = c("Men", "Women")),
         dvmrdf = factor(dvmrdf, levels = seq(1, 9, 1), 
                         labels = c("Married", 
                                    "Cohabiting", 
                                    "Single", 
                                    "Widowed", 
                                    "Divorced", 
                                    "Divorced",
                                    "Cohabiting", 
                                    "Married", # Civil Partnership
                                    "Divorced")),
         disabled = case_when(lsill == 1 & illlim == 1 ~ "Disabled",
                              TRUE ~ "Not disabled"),
         disabled = factor(disabled),
         ethnic = factor(ethnic, levels = c(-9:-6, 1:15),
                         labels = c(rep("DK/NA", 4),
                                    "White British",
                                    rep("Minority ethnic", 14))),
         edlevel = factor(edlevel, levels = c(seq(-9, -6, 1), seq(1, 4, 1)),
                          labels = c(rep("DK/NA", 4), 
                                     "Degree-level or above",
                                     rep("Other qualification", 2), 
                                     "No qualification")),
         ecact = factor(ecact, levels = c(0:9),
                        labels = c("Ineligible / missing",
                                   "Employed",
                                   "Self-employed",
                                   "Unemployed",
                                   "Inactive - student",
                                   "Inactive - other",
                                   "Inactive - other",
                                   "Inactive - disabled",
                                   "Retired",
                                   "Inactive - other")),
         religion = factor(religion, levels = c(seq(-9, -6, 1), seq(1, 8, 1)),
                           labels = c(rep("DK/NA", 4),
                                      "Christian",
                                      "Other religion",
                                      "Other religion",
                                      "Other religion",
                                      "Other religion",
                                      "Other religion",
                                      "Other religion",
                                      "No religion")),
         sid = factor(sid, levels = c(seq(-9, -6, 1), seq(1, 4, 1)),
                      labels = c(rep("DK/NA", 4),
                                 "Hetero / straight",
                                 "Other",
                                 "Other",
                                 "Other")),
         incdrop = factor(incdrop, levels = c(seq(-9, -6, 1), seq(1, 6, 1)),
                          labels = c(rep("DK/NA", 4),
                                     "Less than one week",
                                     "One week or more, but less than one month",
                                     "One month or more, but less than three months",
                                     "Three months or more, but less than six months",
                                     "Six months or more, but less than 12 months",
                                     "Twelve months or more"),
                          ordered = TRUE),
         incdrop = fct_explicit_na(incdrop, "DK/NA")) %>%

  select(-hrpsex, -fnsav1, -dvfnsval)

# Check for missing data -------------------------------------------------------

missing_h <- hhld %>%
  group_by(wave) %>%
  filter(gor == "Scotland") %>%
  miss_var_summary() %>%
  filter(n_miss >= 1) %>%
  mutate(pct_miss = paste0(round(pct_miss, 2), "%"))

missing_p <- pers %>%
  group_by(wave) %>%
  filter(gor == "Scotland") %>%
  miss_var_summary() %>%
  filter(n_miss >= 1) %>%
  mutate(pct_miss = paste0(round(pct_miss, 2), "%"))

# Save all ---------------------------------------------------------------------

saveRDS(hhld, "data/tidy_household.rds")
saveRDS(pers, "data/tidy_person.rds")

rm(list = ls())
