# Clean imported datasets: rename and recategorise variables etc.

library(tidyverse)

waves <- factor(c("w1", "w2", "w3", "w4", "w5", "r5", "r6", "r7"),
                levels = c("w1", "w2", "w3", "w4", "w5", "r5", "r6", "r7"),
                ordered = TRUE)

clean <- vector("list", length(waves))
names(clean) <- waves

# W1 ---------------------------------------------------------------------------

wave <- "w1"

file <- readRDS(paste0("data/files_person_", wave, ".rds"))
colnames(file) <- tolower(colnames(file))

file <- file %>%
  mutate(wave = wave,
         hhserial = hhserialw1,
         dvage = dvagew1,
         dvage9 = case_when(dvage %in% c(0:15) ~ 1,
                            dvage %in% c(16:24) ~ 2,
                            dvage %in% c(25:34) ~ 3,
                            dvage %in% c(35:44) ~ 4,
                            dvage %in% c(45:54) ~ 5,
                            dvage %in% c(55:64) ~ 6,
                            dvage %in% c(65:74) ~ 7,
                            dvage %in% c(75:84) ~ 8,
                            dvage >= 85 ~ 9),
         isdep = isdepw1,
         sex = sexw1,
         
         # Only needed for R6 as it's missing in the R6 hhld dataset
         hrpsex = NA,
         
         dvmrdf = dvmrdfw1,
         lsill = lsillw1,
         illlim = illlimw1,
         ethnic = ethnicw1,
         religion = religionw1,
         sid = NA,
         edlevel = edlevelw1,
         ecact = NA,
         persprox = persproxw1,
         
         # Premium bonds
         fnsav1 = fnsav1w1,
         dvfnsval = dvfnsvalw1,
         
         # Individual wealth
         totpen = totpenw1,
         dvpinpval = dvpinpvalw1,
         p_net_fin = NA,
         p_phys = NA,
         p_net_prop = NA,
         p_totwlth = NA,
         
         incdrop = NA) %>%
  
  select(wave, hhserial, 
         dvage, dvage9, isdep, sex, hrpsex, dvmrdf, lsill, illlim, ethnic, religion, sid, edlevel, ecact,
         persprox,
         fnsav1, dvfnsval,
         totpen, dvpinpval, p_net_fin, p_phys, p_net_prop, p_totwlth, 
         incdrop)

clean[[wave]] <- file

# W2 ---------------------------------------------------------------------------

wave <- "w2"

file <- readRDS(paste0("data/files_person_", wave, ".rds"))
colnames(file) <- tolower(colnames(file))

file <- file %>%
  mutate(wave = wave,
         hhserial = hhserialw2,
         dvage = dvagew2,
         dvage9 = dvage9w2,
         isdep = isdepw2,
         sex = sexw2,
         
         # Only needed as it's missing in the R6 hhld dataset
         hrpsex = NA,
         
         dvmrdf = dvmrdfw2,
         lsill = lsillw2,
         illlim = illlimw2,
         ethnic = ethnicw2,
         religion = religionw2,
         sid = NA,
         edlevel = edlevelw2,
         ecact = NA,
         persprox = persproxw2,
         
         # Premium bonds
         fnsav1 = fnsav1w2,
         dvfnsval = dvfnsvalw2,
         
         # Individual wealth
         totpen = totpenw2,
         dvpinpval = dvpinpvalw2,
         p_net_fin = NA,
         p_phys = NA,
         p_net_prop = NA,
         p_totwlth = NA,
         
         # Current question wording started in W4
         incdrop = NA) %>%
  
  select(wave, hhserial, 
         dvage, dvage9, isdep, sex, hrpsex, dvmrdf, lsill, illlim, ethnic, religion, sid, edlevel, ecact,
         persprox,
         fnsav1, dvfnsval,
         totpen, dvpinpval, p_net_fin, p_phys, p_net_prop, p_totwlth, 
         incdrop)

clean[[wave]] <- file

# W3 ---------------------------------------------------------------------------

wave <- "w3"

file <- readRDS(paste0("data/files_person_", wave, ".rds"))
colnames(file) <- tolower(colnames(file))

file <- file %>%
  mutate(wave = wave,
         hhserial = hhserialw3,
         dvage = dvagew3,
         dvage9 = dvage9w3,
         isdep = isdepw3,
         sex = sexw3,
         
         # Only needed as it's missing in the R6 hhld dataset
         hrpsex = NA,
         
         dvmrdf = dvmrdfw3,
         lsill = lsillw3,
         illlim = illlimw3,
         ethnic = ethnicw3,
         religion = religionw3,
         sid = NA,
         edlevel = edlevelw3,
         ecact = NA,
         persprox = persproxw3,
         
         # Premium bonds
         fnsav1 = fnsav1w3,
         dvfnsval = dvfnsvalw3,
         
         # Individual wealth
         totpen = totpenw3,
         dvpinpval = dvpinpvalw3,
         p_net_fin = NA,
         p_phys = NA,
         p_net_prop = NA,
         p_totwlth = p_totwlthw3,
         
         # Current question wording started in W4
         incdrop = NA) %>%
  
  select(wave, hhserial, 
         dvage, dvage9, isdep, sex, hrpsex, dvmrdf, lsill, illlim, ethnic, religion, sid, edlevel, ecact,
         persprox,
         fnsav1, dvfnsval,
         totpen, dvpinpval, p_net_fin, p_phys, p_net_prop, p_totwlth, 
         incdrop)

clean[[wave]] <- file

# W4 ---------------------------------------------------------------------------

wave <- "w4"

file <- readRDS(paste0("data/files_person_", wave, ".rds"))
colnames(file) <- tolower(colnames(file))

file <- file %>%
  mutate(wave = wave,
         hhserial = hhserialw4,
         dvage = dvagew4,
         dvage9 = dvage9w4,
         isdep = isdepw4,
         sex = sexw4,
         
         # Only needed as it's missing in the R6 hhld dataset
         hrpsex = NA,
         
         dvmrdf = dvmrdfw4,
         lsill = lsillw4,
         illlim = illlimw4,
         ethnic = ethnicw4,
         religion = religionw4,
         sid = NA,
         edlevel = edlevelw4,
         ecact = NA,
         persprox = persproxw4,
         
         # Premium bonds
         fnsav1 = fnsav1w4,
         dvfnsval = dvfnsvalw4,
         
         # Individual wealth
         totpen = totpenw4,
         dvpinpval = dvpinpvalw4,
         p_net_fin = p_net_finw4,
         p_phys = p_physw4,
         p_net_prop = p_net_propw4,
         p_totwlth = p_totwlthw4,
         
         incdrop = incdropw4) %>%
  
  select(wave, hhserial, 
         dvage, dvage9, isdep, sex, hrpsex, dvmrdf, lsill, illlim, ethnic, religion, sid, edlevel, ecact,
         persprox,
         fnsav1, dvfnsval,
         totpen, dvpinpval, p_net_fin, p_phys, p_net_prop, p_totwlth, 
         incdrop)

clean[[wave]] <- file

# W5 ---------------------------------------------------------------------------

wave <- "w5"

file <- readRDS(paste0("data/files_person_", wave, ".rds"))
colnames(file) <- tolower(colnames(file))

file <- file %>%
  mutate(wave = wave,
         hhserial = hhserialw5,
         dvage = dvagew5,
         dvage9 = dvage9w5,
         isdep = isdepw5,
         sex = sexw5,
         
         # Only needed as it's missing in the R6 hhld dataset
         hrpsex = NA,
         
         dvmrdf = dvmrdfw5,
         lsill = lsillw5,
         illlim = illlimw5,
         ethnic = ethnicw5,
         religion = religionw5,
         sid = sidw5,
         edlevel = edlevelw5,
         ecact = dvecactw5,
         persprox = persproxw5,
         
         # Premium bonds
         fnsav1 = fnsav1w5,
         dvfnsval = dvfnsvalw5,
         
         # Individual wealth
         totpen = totpenw5,
         dvpinpval = dvpinpvalw5,
         p_net_fin = p_net_finw5,
         p_phys = p_physw5,
         p_net_prop = p_net_propw5,
         p_totwlth = p_totwlthw5,
         
         incdrop = incdropw5) %>%
  
  select(wave, hhserial, 
         dvage, dvage9, isdep, sex, hrpsex, dvmrdf, lsill, illlim, ethnic, religion, sid, edlevel, ecact,
         persprox,
         fnsav1, dvfnsval,
         totpen, dvpinpval, p_net_fin, p_phys, p_net_prop, p_totwlth, 
         incdrop)

clean[[wave]] <- file

# R5 ---------------------------------------------------------------------------

wave <- "r5"

file <- readRDS(paste0("data/files_person_", wave, ".rds"))
colnames(file) <- tolower(colnames(file))

file <- file %>%
  mutate(wave = wave,
         hhserial = hhserialr5,
         dvage = dvager5,
         dvage9 = dvage9r5,
         isdep = case_when(is.na(isdepw5) ~ isdepw4,
                           TRUE ~ isdepw5),
         sex = sexr5,
         
         # Only needed as it's missing in the R6 hhld dataset
         hrpsex = NA,
         
         dvmrdf = dvmrdfr5,
         lsill = case_when(is.na(lsillw5) ~ lsillw4,
                           TRUE ~ lsillw5),
         illlim = case_when(is.na(illlimw5) ~ illlimw4,
                            TRUE ~ illlimw5),
         ethnic = case_when(is.na(ethnicw5) ~ ethnicw4,
                             TRUE ~ ethnicw5),
         religion = case_when(is.na(religionw5) ~ religionw4,
                              TRUE ~ religionw5),
         sid = case_when(is.na(sidw5) ~ sidw4,
                         TRUE ~ sidw5),
         edlevel = edlevelr5,
         ecact = dvecactr5,
         persprox = case_when(is.na(persproxw5) ~ persproxw4,
                              TRUE ~ persproxw5),
         
         # Premium bonds
         fnsav1 = case_when(is.na(fnsav1w5) ~ fnsav1w4,
                            TRUE ~ fnsav1w5),
         dvfnsval = dvfnsvalr5,
         
         # Individual wealth
         totpen = totpenr5,
         dvpinpval = dvpinpvalr5,
         p_net_fin = p_net_finr5,
         p_phys = p_physr5,
         p_net_prop = p_net_propr5,
         p_totwlth = p_totwlthr5,
         
         incdrop = incdropr5) %>%
  
  select(wave, hhserial, 
         dvage, dvage9, isdep, sex, hrpsex, dvmrdf, lsill, illlim, ethnic, religion, sid, edlevel, ecact,
         persprox,
         fnsav1, dvfnsval,
         totpen, dvpinpval, p_net_fin, p_phys, p_net_prop, p_totwlth, 
         incdrop)

clean[[wave]] <- file

# R6 ---------------------------------------------------------------------------

wave <- "r6"

file <- readRDS(paste0("data/files_person_", wave, ".rds"))
colnames(file) <- tolower(colnames(file))

file <- file %>%
  mutate(wave = wave,
         hhserial = hhserialr6,
         dvage = dvager6,
         dvage9 = dvage9r6,
         isdep = case_when(is.na(isdepw6) ~ isdepw5,
                           TRUE ~ isdepw6),
         sex = sexr6,
         
         # Only needed as it's missing in the R6 hhld dataset
         hrpsex = case_when(is.na(hrpsexw6) ~ hrpsexw5,
                            TRUE ~ hrpsexw6),
         
         dvmrdf = dvmrdfr6,
         lsill = case_when(is.na(lsillw6) ~ lsillw5,
                           TRUE ~ lsillw6),
         illlim = case_when(is.na(illlimw6) ~ illlimw5,
                            TRUE ~ illlimw6),
         ethnic = case_when(is.na(dvethnicw6) ~ ethnicw5,
                            TRUE ~ dvethnicw6),
         religion = case_when(is.na(dvreligw6) ~ religionw5,
                              TRUE ~ dvreligw6),
         sid = case_when(is.na(sidw6) ~ sidw5,
                         TRUE ~ sidw6),
         edlevel = edlevelr6,
         ecact = dvecactr6,
         persprox = case_when(is.na(persproxw6) ~ persproxw5,
                              TRUE ~ persproxw6),
         
         # Premium bonds
         fnsav1 = fnsav1w6, # fnsav1w5 not in dataset!
         dvfnsval = dvfnsvalr6,
         
         # Individual wealth
         totpen = totpenr6,
         dvpinpval = dvpinpvalr6,
         p_net_fin = p_net_finr6,
         p_phys = p_physr6,
         # p_net_propr6 is missing for some reason 
         p_net_prop = p_net_mainr6 + p_net_othpropr6,
         p_totwlth = p_totwlthr6,
         
         incdrop = incdropr6) %>%
  
  select(wave, hhserial, 
         dvage, dvage9, isdep, sex, hrpsex, dvmrdf, lsill, illlim, ethnic, religion, sid, edlevel, ecact,
         persprox,
         fnsav1, dvfnsval,
         totpen, dvpinpval, p_net_fin, p_phys, p_net_prop, p_totwlth, 
         incdrop)

clean[[wave]] <- file


# R7 ---------------------------------------------------------------------------

wave <- "r7"

file <- readRDS(paste0("data/files_person_", wave, ".rds"))
colnames(file) <- tolower(colnames(file))

file <- file %>%
  mutate(wave = wave,
         hhserial = hhserialr7,
         dvage = dvager7,
         dvage9 = dvage9r7,
         isdep = isdepr7,
         sex = sexr7,
         
         # Only needed for R6 as it's missing in the R6 hhld dataset
         hrpsex = NA,
         
         dvmrdf = dvmrdfr7,
         lsill = lsillr7,
         illlim = illlimr7,
         ethnic = dvethnicr7,
         religion = dvreligr7,
         sid = sidr7,
         edlevel = edlevelr7,
         ecact = dvecactr7,
         persprox = persproxr7,
         
         # Premium bonds
         fnsav1 = fnsav1r7,
         dvfnsval = dvfnsvalr7,
         
         # Individual wealth
         totpen = totpenr7,
         dvpinpval = dvpinpvalr7,
         p_net_fin = p_net_finr7,
         p_phys = p_physr7,
         p_net_prop = p_net_propr7,
         p_totwlth = p_totwlthr7,
         
         incdrop = incdropr7) %>%
  
  select(wave, hhserial, 
         dvage, dvage9, isdep, sex, hrpsex, dvmrdf, lsill, illlim, ethnic, religion, sid, edlevel, ecact,
         persprox,
         fnsav1, dvfnsval,
         totpen, dvpinpval, p_net_fin, p_phys, p_net_prop, p_totwlth, 
         incdrop)

clean[[wave]] <- file

# Combine into single data frame and save --------------------------------------
saveRDS(do.call(rbind, clean), "data/clean_person.rds")

rm(list = ls())

