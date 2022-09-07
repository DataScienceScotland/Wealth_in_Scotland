# prelims ----------------------------------------------------------------------

# Clean imported datasets: rename and recategorise variables etc.

library(tidyverse)

waves <- factor(c("w1", "w2", "w3", "w4", "w5", "r5", "r6", "r7"),
                levels = c("w1", "w2", "w3", "w4", "w5", "r5", "r6", "r7"),
                ordered = TRUE)

clean <- vector("list", length(waves))
names(clean) <- waves

files <- readRDS("data/files_household.rds")

# Property wealth detail (should add up to total net property wealth)

# DVhvalueW6	Value of main residence
# DVHseValW6_sum	Total value of other houses
# DVBltValW6_sum 	Total value of buy to let houses
# DVBlDValW6_sum 	Total value of buildings
# DVLUKValW6_sum 	Total value of UK land
# DVLOSValW6_sum 	Total value of overseas land
# DVOPrValW6_sum	Total value of other property
# TotMortW6	Total mortgage on main residence
# DVEqRelValR6	Total value of equity release
# DVHseDebtW6_sum 	Total debt houses not main residence
# DVBLtDebtW6_sum 	Total debt buy to let houses
# DVBldDebtW6_sum 	Total debt buildings
# DVLUKDebtW6_sum 	Total debt UK land
# DVLOSDebtW6_sum 	Total debt overseas land
# DVOPrDebtW6_sum 	Total debt other property

# W1 ---------------------------------------------------------------------------

wave <- "w1"

file <- files[[wave]]
colnames(file) <- tolower(colnames(file))

file <- file %>%
  
  mutate(wave = wave,
         hhserial = hhserialw1,
         gor = gorw1,
         wgt = xs_wgtw1,
         
         psu = NA,
         stratum = NA,
         
         # Wave 1 only: Physical wealth data was only recorded from half the 
         # sample (17,316 households). For aggregate physical wealth, their 
         # weights need to be grossed up to the whole population using a “rating 
         # up factor” = Weighted total population / weighted half sample 
         # population: 24,583,701/13,901,282
         wgt_halfsample = ifelse(gcpreamw1 == 1, wgt * 24583701 / 13901282, 0),
         
         # Main wealth variables
         totwlth = totwlthw1,
         finwlth = hfinwntw1_sum,
         phywlth = hphysww1,
         prowlth = hpropww1,
         penwlth = totpenw1_sum,
         
         # Problem debt (unmanageable debt)
         hhpd = NA,
         
         # variables required to calculate financial resilience / vulnerability
         netequincann = NA,
         dvcacrval = dvcacrvalw1_sum,
         dvsaval = dvsavalw1_sum,
         dvisaval = dvisavalw1_sum,
         dvfshukv = dvfshukvw1_sum,
         dvfeshares = dvfesharesw1_sum,
         dvfcollv = dvfcollvw1_sum,
         dvfshosv = dvfshosvw1_sum,
         dvcaodval = dvcaodvalw1_sum,
         
         totarr_excmort = NA,
         marrsv1 = NA,
         marrsv2 = NA,
         marrsv3 = NA,
         hhtotrep = NA,
         
         # Home ownership and tenure
         dvproperty = dvpropertyw1,
         tenure = case_when(ten1w1 %in% c(4:5) & dvprirntw1 == 2 ~ 7,
                            TRUE ~ ten1w1),
         
         # Property detail
         dvhvalue = dvhvaluew1,
         dvhseval = dvhsevalw1,
         dvbltval = NA, 	
         dvbldval = dvbldvalw1,
         dvlukval = dvlukvalw1, 	
         dvlosval = dvlosvalw1,
         dvoprval = dvoprvalw1,	
         totmort = totmortw1,
         dveqrelval = dveqrelvalw1,
         dvhsedebt = dvhsedebtw1,
         dvbltdebt = NA,
         dvblddebt = dvblddebtw1,
         dvlukdebt = dvlukdebtw1,
         dvlosdebt = dvlosdebtw1,
         dvoprdebt = dvoprdebtw1,
         
         # Household characteristics
         numdepch = numdepchw1,
         numadult = numadultw1,
         dvhsize = dvhsizew1,
         hholdtype = hholdtypew1,
         hrpdvecact = NA,
         hrpdvage = hrpdvagew1,
         hrpdvage9 = hrpdvage9w1,
         hrpsex = hrpsexw1,
         hrpdvmrdf = hrpdvmrdfw1,
         hrpethnic = hrpethnicw1,
         hrpedlevel = hrpedlevelw1,
         
         # Area characteristics
         urindsc = urindw1) %>%
  
  select(wave, hhserial, gor, wgt, wgt_halfsample,
         
         numdepch, numadult, dvhsize, hholdtype, 
         hrpsex, hrpedlevel, hrpdvage, hrpdvage9, hrpethnic, hrpdvecact, hrpdvmrdf,
         
         urindsc,
         
         totwlth, finwlth, phywlth, prowlth, penwlth,
         
         hhpd,
         
         dvproperty, tenure,
         dvhvalue, dvhseval, dvbltval, dvbldval, dvlukval, dvlosval, dvoprval, 
         totmort, dveqrelval, dvhsedebt, dvbltdebt, dvblddebt, dvlukdebt, dvlosdebt, dvoprdebt,
         
         netequincann, dvcacrval, dvsaval, dvisaval, dvfshukv, dvfeshares, 
         dvfcollv, dvfshosv, dvcaodval, totarr_excmort, marrsv1, marrsv2, 
         marrsv3, hhtotrep,
         
         psu, stratum)

clean[[wave]] <- file

# W2 ---------------------------------------------------------------------------

wave <- "w2"

file <- files[[wave]]
colnames(file) <- tolower(colnames(file))

file <- file %>%
  
  mutate(wave = wave,
         hhserial = hhserialw2,
         gor = gorw2,
         wgt = xs_calwgtw2,
         wgt_halfsample = NA,
         
         psu = NA,
         stratum = NA,
         
         # Main wealth variables
         totwlth = totwlthw2,
         finwlth = hfinwntw2_sum,
         phywlth = hphysww2,
         prowlth = hpropww2,
         penwlth = totpenw2_aggr,
         
         # Problem debt (unmanageable debt)
         hhpd = NA,
         
         # variables required to calculate financial resilience / vulnerability
         netequincann = NA,
         dvcacrval = dvcacrvalw2_aggr,
         dvsaval = dvsavalw2_aggr,
         dvisaval = dvisavalw2_aggr,
         dvfshukv = dvfshukvw2_aggr,
         dvfeshares = dvfesharesw2_aggr,
         dvfcollv = dvfcollvw2_aggr,
         dvfshosv = dvfshosvw2_aggr,
         dvcaodval = dvcaodvalw2_aggr,
         totarr_excmort = totarr_excmortw2_aggr,
         marrsv1 = marrsvw2,
         marrsv2 = marrsv2w2,
         marrsv3 = marrsv3w2,
         hhtotrep = NA,
         
         # Home ownership and tenure
         dvproperty = dvpropertyw2,
         tenure = case_when(ten1w2 %in% c(4:5) & dvprirntw2 == 2 ~ 7,
                            TRUE ~ ten1w2),
         
         # Property detail
         dvhvalue = dvhvaluew2,
         dvhseval = dvhsevalw2,
         dvbltval = dvbltvalw2, 	
         dvbldval = dvbldvalw2,
         dvlukval = dvlukvalw2, 	
         dvlosval = dvlosvalw2,
         dvoprval = dvoprvalw2,	
         totmort = totmortw2,
         dveqrelval = dveqrelvalw2,
         dvhsedebt = dvhsedebtw2,
         dvbltdebt = dvbltdebtw2,
         dvblddebt = dvblddebtw2,
         dvlukdebt = dvlukdebtw2,
         dvlosdebt = dvlosdebtw2,
         dvoprdebt = dvoprdebtw2,
         
                  # Household characteristics
         numdepch = numdepch_hhw2,
         numadult = numadultw2,
         dvhsize = dvhsizew2,
         hholdtype = hholdtypew2,
         hrpdvecact = NA,
         hrpdvage = hrpdvagew2,
         hrpdvage9 = hrpdvage9w2,
         hrpsex = hrpsexw2,
         hrpdvmrdf = hrpdvmrdfw2,
         hrpethnic = hrpethnicw2,
         hrpedlevel = hrpedlevelw2,
         
         # Area characteristics
         urindsc = case_when(as.numeric(urindscw2) %in% c(1:5) ~ 1,
                             as.numeric(urindscw2) %in% c(6:8) ~ 2)) %>%
  
  select(wave, hhserial, gor, wgt, wgt_halfsample,
         
         numdepch, numadult, dvhsize, hholdtype, 
         hrpsex, hrpedlevel, hrpdvage, hrpdvage9, hrpethnic, hrpdvecact, hrpdvmrdf,
         
         urindsc,
         
         totwlth, finwlth, phywlth, prowlth, penwlth,
         
         hhpd,
         
         dvproperty, tenure,
         dvhvalue, dvhseval, dvbltval, dvbldval, dvlukval, dvlosval, dvoprval, 
         totmort, dveqrelval, dvhsedebt, dvbltdebt, dvblddebt, dvlukdebt, dvlosdebt, dvoprdebt,
         
         netequincann, dvcacrval, dvsaval, dvisaval, dvfshukv, dvfeshares, 
         dvfcollv, dvfshosv, dvcaodval, totarr_excmort, marrsv1, marrsv2, 
         marrsv3, hhtotrep,
         
         psu, stratum)

clean[[wave]] <- file

# W3 ---------------------------------------------------------------------------

wave <- "w3"

file <- files[[wave]]
colnames(file) <- tolower(colnames(file))

file <- file %>%
  
  mutate(wave = wave,
         hhserial = hhserialw3,
         gor = gorw3,
         wgt = w3xswgt,
         wgt_halfsample = NA,
         
         psu = NA,
         stratum = NA,
         
         # Main wealth variables
         totwlth = totwlthw3,
         finwlth = hfinwntw3_sum,
         phywlth = hphysww3,
         prowlth = hpropww3,
         penwlth = totpenw3_aggr,
         
         # Problem debt (unmanageable debt)
         hhpd = NA,
         
         # variables required to calculate financial resilience / vulnerability
         netequincann = NA,
         dvcacrval = dvcacrvalw3_aggr,
         dvsaval = dvsavalw3_aggr,
         dvisaval = dvisavalw3_aggr,
         dvfshukv = dvfshukvw3_aggr,
         dvfeshares = dvfesharesw3_aggr,
         dvfcollv = dvfcollvw3_aggr,
         dvfshosv = dvfshosvw3_aggr,
         dvcaodval = dvcaodvalw3_aggr,
         totarr_excmort = totarr_excmortw3_aggr,
         marrsv1 = marrsvw3,
         marrsv2 = marrsv2w3,
         marrsv3 = marrsv3w3,
         hhtotrep = NA,
         
         # Home ownership and tenure
         dvproperty = dvpropertyw3,
         tenure = case_when(ten1w3 %in% c(4:5) & dvprirntw3 == 2 ~ 7,
                            TRUE ~ ten1w3),
         
         # Property detail
         dvhvalue = dvhvaluew3,
         dvhseval = dvhsevalw3_sum,
         dvbltval = dvbltvalw3_sum, 	
         dvbldval = dvbldvalw3_sum,
         dvlukval = dvlukvalw3_sum, 	
         dvlosval = dvlosvalw3_sum,
         dvoprval = dvoprvalw3_sum,	
         totmort = totmortw3,
         dveqrelval = dveqrelvalw3,
         dvhsedebt = dvhsedebtw3_sum,
         dvbltdebt = dvbltdebtw3_sum,
         dvblddebt = dvblddebtw3_sum,
         dvlukdebt = dvlukdebtw3_sum,
         dvlosdebt = dvlosdebtw3_sum,
         dvoprdebt = dvoprdebtw3_sum,
         
         # Household characteristics
         numdepch = numdepchw3,
         numadult = numadultw3,
         dvhsize = dvhsizew3,
         hholdtype = hholdtypew3,
         hrpdvecact = NA,
         hrpdvage = hrpdvagew3,
         hrpdvage9 = hrpdvage9w3,
         hrpsex = hrpsexw3,
         hrpdvmrdf = hrpdvmrdfw3,
         hrpethnic = hrpethnicw3,
         hrpedlevel = hrpedlevelw3,
         
         # Area characteristics
         urindsc = case_when(as.numeric(urindscw3) %in% c(1:5) ~ 1,
                             as.numeric(urindscw3) %in% c(6:8) ~ 2)) %>%
  
  select(wave, hhserial, gor, wgt, wgt_halfsample,
         
         numdepch, numadult, dvhsize, hholdtype, 
         hrpsex, hrpedlevel, hrpdvage, hrpdvage9, hrpethnic, hrpdvecact, hrpdvmrdf,
         
         urindsc,
         
         totwlth, finwlth, phywlth, prowlth, penwlth,
         
         hhpd,
         
         dvproperty, tenure,
         dvhvalue, dvhseval, dvbltval, dvbldval, dvlukval, dvlosval, dvoprval, 
         totmort, dveqrelval, dvhsedebt, dvbltdebt, dvblddebt, dvlukdebt, dvlosdebt, dvoprdebt,
         
         netequincann, dvcacrval, dvsaval, dvisaval, dvfshukv, dvfeshares, 
         dvfcollv, dvfshosv, dvcaodval, totarr_excmort, marrsv1, marrsv2, 
         marrsv3, hhtotrep,
         
         psu, stratum)

clean[[wave]] <- file

# W4 ---------------------------------------------------------------------------

wave <- "w4"

file <- files[[wave]]
colnames(file) <- tolower(colnames(file))

file <- file %>%
  
  mutate(wave = wave,
         hhserial = hhserialw4,
         gor = gorw4,
         wgt = w4xshhwgt,
         wgt_halfsample = NA,
         
         psu = NA,
         stratum = NA,
         
         # Main wealth variables
         totwlth = totwlthw4,
         finwlth = hfinwntw4_sum,
         phywlth = hphysww4,
         prowlth = hpropww4,
         penwlth = totpenw4_aggr,
         
         # Problem debt (unmanageable debt)
         hhpd = NA,
         
         # variables required to calculate financial resilience / vulnerability
         netequincann = NA,
         dvcacrval = dvcacrvalw4_aggr,
         dvsaval = dvsavalw4_aggr,
         dvisaval = dvisavalw4_aggr,
         dvfshukv = dvfshukvw4_aggr,
         dvfeshares = dvfesharesw4_aggr,
         dvfcollv = dvfcollvw4_aggr,
         dvfshosv = dvfshosvw4_aggr,
         dvcaodval = dvcaodvalw4_aggr,
         totarr_excmort = totarr_excmortw4_aggr,
         marrsv1 = marrsv1w4,
         marrsv2 = marrsv2w4,
         marrsv3 = marrsv3w4,
         hhtotrep = NA,
         
         # Home ownership and tenure
         dvproperty = dvpropertyw4,
         tenure = case_when(ten1w4 %in% c(4:5) & dvprirntw4 == 2 ~ 7,
                            TRUE ~ ten1w4),

         # Property detail
         dvhvalue = dvhvaluew4,
         dvhseval = dvhsevalw4_sum,
         dvbltval = dvbltvalw4_sum, 	
         dvbldval = dvbldvalw4_sum,
         dvlukval = dvlukvalw4_sum, 	
         dvlosval = dvlosvalw4_sum,
         dvoprval = dvoprvalw4_sum,	
         totmort = totmortw4,
         dveqrelval = dveqrelvalw4,
         dvhsedebt = dvhsedebtw4_sum,
         dvbltdebt = dvbltdebtw4_sum,
         dvblddebt = dvblddebtw4_sum,
         dvlukdebt = dvlukdebtw4_sum,
         dvlosdebt = dvlosdebtw4_sum,
         dvoprdebt = dvoprdebtw4_sum,
         
         # Household characteristics
         numdepch = numdepchw4,
         numadult = numadultw4,
         dvhsize = dvhsizew4,
         hholdtype = hholdtypew4,
         hrpdvecact = hrpdvecactw4,
         hrpdvage = hrpdvagew4,
         hrpdvage9 = hrpdvage9w4,
         hrpsex = hrpsexw4,
         hrpdvmrdf = hrpdvmrdfw4,
         hrpethnic = hrpethnicw4,
         hrpedlevel = hrpedlevelw4,
         
         # Area characteristics
         urindsc = case_when(as.numeric(urindscw4) %in% c(1:5) ~ 1,
                             as.numeric(urindscw4) %in% c(6:8) ~ 2)) %>%
  
  select(wave, hhserial, gor, wgt, wgt_halfsample,
         
         numdepch, numadult, dvhsize, hholdtype, 
         hrpsex, hrpedlevel, hrpdvage, hrpdvage9, hrpethnic, hrpdvecact, hrpdvmrdf,
         
         urindsc,
         
         totwlth, finwlth, phywlth, prowlth, penwlth,
         
         hhpd,
         
         dvproperty, tenure,
         dvhvalue, dvhseval, dvbltval, dvbldval, dvlukval, dvlosval, dvoprval, 
         totmort, dveqrelval, dvhsedebt, dvbltdebt, dvblddebt, dvlukdebt, dvlosdebt, dvoprdebt,
         
         netequincann, dvcacrval, dvsaval, dvisaval, dvfshukv, dvfeshares, 
         dvfcollv, dvfshosv, dvcaodval, totarr_excmort, marrsv1, marrsv2, 
         marrsv3, hhtotrep,
         
         psu, stratum)

clean[[wave]] <- file

# W5 ---------------------------------------------------------------------------

wave <- "w5"

file <- files[[wave]]
colnames(file) <- tolower(colnames(file))

file <- file %>%
  
  mutate(wave = wave,
         hhserial = hhserialw5,
         gor = gorw5,
         wgt = w5xshhwgt,
         wgt_halfsample = NA,
         
         psu = NA,
         stratum = NA,
         
         # Main wealth variables
         totwlth = totwlthw5,
         finwlth = hfinwntw5_sum,
         phywlth = hphysww5,
         prowlth = hpropww5,
         penwlth = totpenw5_aggr,
         
         # Problem debt (unmanageable debt)
         hhpd = hhpdw5,
         
         # variables required to calculate financial resilience / vulnerability
         netequincann = netequincannw5,
         dvcacrval = dvcacrvalw5_aggr,
         dvsaval = dvsavalw5_aggr,
         dvisaval = dvisavalw5_aggr,
         dvfshukv = dvfshukvw5_aggr,
         dvfeshares = dvfesharesw5_aggr,
         dvfcollv = dvfcollvw5_aggr,
         dvfshosv = dvfshosvw5_aggr,
         dvcaodval = dvcaodvalw5_aggr,
         totarr_excmort = totarr_excmortw5_aggr,
         marrsv1 = marrsv1w5,
         marrsv2 = marrsv2w5,
         marrsv3 = marrsv3w5,
         hhtotrep = hhtotrepw5_sum,
         
         # Home ownership and tenure
         dvproperty = dvpropertyw5,
         tenure = case_when(ten1w5 %in% c(4:5) & dvprirntw5 == 2 ~ 7,
                            TRUE ~ ten1w5),
         
         # Property detail
         dvhvalue = dvhvaluew5,
         dvhseval = dvhsevalw5_sum,
         dvbltval = dvbltvalw5_sum, 	
         dvbldval = dvbldvalw5_sum,
         dvlukval = dvlukvalw5_sum, 	
         dvlosval = dvlosvalw5_sum,
         dvoprval = dvoprvalw5_sum,	
         totmort = totmortw5,
         dveqrelval = dveqrelvalw5,
         
         dvhsedebt = dvhsedebtw5_sum,
         dvbltdebt = dvbltdebtw5_sum,
         dvblddebt = dvblddebtw5_sum,
         dvlukdebt = dvlukdebtw5_sum,
         dvlosdebt = dvlosdebtw5_sum,
         dvoprdebt = dvoprdebtw5_sum,
         
         # Household characteristics
         numdepch = numdepchw5,
         numadult = numadultw5,
         dvhsize = dvhsizew5,
         hholdtype = hholdtypew5,
         hrpdvecact = hrpdvecactw5,
         hrpdvage = hrpdvagew5,
         hrpdvage9 = hrpdvage9w5,
         hrpsex = hrpsexw5,
         hrpdvmrdf = hrpdvmrdfw5,
         hrpethnic = hrpethnicw5,
         hrpedlevel = hrpedlevelw5,
         
         # Area characteristics
         urindsc = case_when(as.numeric(urindscw5) %in% c(1:5) ~ 1,
                             as.numeric(urindscw5) %in% c(6:8) ~ 2)) %>%
  
  select(wave, hhserial, gor, wgt, wgt_halfsample,
         
         numdepch, numadult, dvhsize, hholdtype, 
         hrpsex, hrpedlevel, hrpdvage, hrpdvage9, hrpethnic, hrpdvecact, hrpdvmrdf,
         
         urindsc,
         
         totwlth, finwlth, phywlth, prowlth, penwlth,
         
         hhpd,
         
         dvproperty, tenure,
         dvhvalue, dvhseval, dvbltval, dvbldval, dvlukval, dvlosval, dvoprval, 
         totmort, dveqrelval, dvhsedebt, dvbltdebt, dvblddebt, dvlukdebt, dvlosdebt, dvoprdebt,
         
         netequincann, dvcacrval, dvsaval, dvisaval, dvfshukv, dvfeshares, 
         dvfcollv, dvfshosv, dvcaodval, totarr_excmort, marrsv1, marrsv2, 
         marrsv3, hhtotrep,
         
         psu, stratum)

clean[[wave]] <- file

# R5 ---------------------------------------------------------------------------

wave <- "r5"

file <- files[[wave]]
colnames(file) <- tolower(colnames(file))

file <- file %>%
  
  mutate(wave = wave,
         hhserial = hhserialr5,
         gor = gorr5,
         
         # Missing value is region code 2
         gor = ifelse(is.na(gor), 2, gor),
         
         wgt = r5xshhwgt,
         wgt_halfsample = NA,
         
         psu = NA,
         stratum = NA,
         
         # Main wealth variables
         totwlth = totwlthr5,
         finwlth = hfinwntr5_sum,
         phywlth = hphyswr5,
         prowlth = hpropwr5,
         penwlth = totpenr5_aggr,
         
         # Problem debt (unmanageable debt)
         hhpd = hhpdr5,
         
         # variables required to calculate financial resilience / vulnerability
         netequincann = netequincannr5,
         dvcacrval = dvcacrvalr5_aggr,
         dvsaval = dvsavalr5_aggr,
         dvisaval = dvisavalr5_aggr,
         dvfshukv = dvfshukvr5_aggr,
         dvfeshares = dvfesharesr5_aggr,
         dvfcollv = dvfcollvr5_aggr,
         dvfshosv = dvfshosvr5_aggr,
         dvcaodval = dvcaodvalr5_aggr,
         totarr_excmort = totarr_excmortr5_aggr,
         marrsv1 = case_when(is.na(marrsv1w5) ~ marrsv1w4,
                             TRUE ~ marrsv1w5),
         marrsv2 = case_when(is.na(marrsv2w5) ~ marrsv2w4,
                             TRUE ~ marrsv2w5),
         marrsv3 = case_when(is.na(marrsv3w5) ~ marrsv3w4,
                             TRUE ~ marrsv3w5),
         hhtotrep = hhtotrepr5_sum,
         
         # Home ownership and tenure
         dvproperty = dvpropertyr5,
         dvprirnt = case_when(is.na(dvprirntw5) ~ dvprirntw4,
                              TRUE ~ dvprirntw5),
         tenure = case_when(ten1r5_i %in% c(4:5) & dvprirnt == 2 ~ 7,
                            TRUE ~ ten1r5_i),
         
         # Property detail
         dvhvalue = dvhvaluer5,
         dvhseval = dvhsevalr5_sum,
         dvbltval = dvbltvalr5_sum, 	
         dvbldval = dvbldvalr5_sum,
         dvlukval = dvlukvalr5_sum, 	
         dvlosval = dvlosvalr5_sum,
         dvoprval = dvoprvalr5_sum,	
         totmort = totmortr5,
         dveqrelval = dveqrelvalr5,
         
         dvhsedebt = dvhsedebtr5_sum,
         dvbltdebt = dvbltdebtr5_sum,
         dvblddebt = dvblddebtr5_sum,
         dvlukdebt = dvlukdebtr5_sum,
         dvlosdebt = dvlosdebtr5_sum,
         dvoprdebt = dvoprdebtr5_sum,
         
         # Household characteristics
         numdepch = case_when(is.na(numdepchw5) ~ numdepchw4,
                              TRUE ~ numdepchw5),
         numadult = case_when(is.na(numadultw5) ~ numadultw4,
                              TRUE ~ numadultw5),
         dvhsize = case_when(is.na(dvhsizew5) ~ dvhsizew4,
                              TRUE ~ dvhsizew5),
         hholdtype = hholdtyper5,
         hrpdvecact = hrpdvecactr5,
         hrpdvage = hrpdvager5,
         hrpdvage9 = hrpdvage9r5,
         hrpsex = case_when(is.na(hrpsexw5) ~ hrpsexw4,
                            TRUE ~ hrpsexw5),
         hrpdvmrdf = case_when(is.na(hrpdvmrdfw5) ~ hrpdvmrdfw4,
                               TRUE ~ hrpdvmrdfw5),
         hrpethnic = case_when(is.na(hrpethnicw5) ~ hrpethnicw4,
                               TRUE ~ hrpethnicw5),
         hrpedlevel = case_when(is.na(hrpedlevelw5) ~ hrpedlevelw4,
                                TRUE ~ hrpedlevelw5),
         
         # Area characteristics
         urindsc = case_when(is.na(as.numeric(urindscw5)) ~ as.numeric(urindscw4),
                             TRUE ~ as.numeric(urindscw5)),
         urindsc = case_when(urindsc %in% c(1:5) ~ 1,
                             urindsc %in% c(6:8) ~ 2)) %>%
  
  select(wave, hhserial, gor, wgt, wgt_halfsample,
         
         numdepch, numadult, dvhsize, hholdtype, 
         hrpsex, hrpedlevel, hrpdvage, hrpdvage9, hrpethnic, hrpdvecact, hrpdvmrdf,
         
         urindsc,
         
         totwlth, finwlth, phywlth, prowlth, penwlth,
         
         hhpd,
         
         dvproperty, tenure,
         dvhvalue, dvhseval, dvbltval, dvbldval, dvlukval, dvlosval, dvoprval, 
         totmort, dveqrelval, dvhsedebt, dvbltdebt, dvblddebt, dvlukdebt, dvlosdebt, dvoprdebt,
         
         netequincann, dvcacrval, dvsaval, dvisaval, dvfshukv, dvfeshares, 
         dvfcollv, dvfshosv, dvcaodval, totarr_excmort, marrsv1, marrsv2, 
         marrsv3, hhtotrep,
         
         psu, stratum)

clean[[wave]] <- file

# R6 ---------------------------------------------------------------------------

wave <- "r6"

file <- files[[wave]]
colnames(file) <- tolower(colnames(file))

file <- file %>%
  
  mutate(wave = wave,
         hhserial = hhserialr6,
         gor = gorr6,
         wgt = r6xshhwgt,
         wgt_halfsample = NA,
         
         psu = NA,
         stratum = NA,
         
         # Main wealth variables
         totwlth = totwlthr6,
         finwlth = hfinwntr6_sum,
         phywlth = hphyswr6,
         prowlth = hpropwr6,
         penwlth = totpenr6_aggr,
         
         # Problem debt (unmanageable debt)
         hhpd = hhpdr6,
         
         # variables required to calculate financial resilience / vulnerability
         netequincann = netequincann_r6,
         dvcacrval = dvcacrvalr6_aggr,
         dvsaval = dvsavalr6_aggr,
         dvisaval = dvisavalr6_aggr,
         dvfshukv = dvfshukvr6_aggr,
         dvfeshares = dvfesharesr6_aggr,
         dvfcollv = dvfcollvr6_aggr,
         dvfshosv = dvfshosvr6_aggr,
         dvcaodval = dvcaodvalr6_aggr,
         totarr_excmort = totarr_excmortr6_aggr,
         marrsv1 = case_when(is.na(marrsv1w6) ~ marrsv1w5,
                             TRUE ~ marrsv1w6),
         marrsv2 = case_when(is.na(marrsv2w6) ~ marrsv2w5,
                             TRUE ~ marrsv2w6),
         marrsv3 = NA,
         hhtotrep = hhtotrepr6_sum,
         
         # Home ownership and tenure
         dvproperty = dvpropertyr6,
         dvprirnt = case_when(is.na(dvprirntw6) ~ dvprirntw5,
                              TRUE ~ dvprirntw6),
         tenure = case_when(ten1r6_i %in% c(4:5) & dvprirnt == 2 ~ 7,
                            TRUE ~ ten1r6_i),
         
         # Property detail
         dvhvalue = dvhvaluer6,
         dvhseval = dvhsevalr6_sum,
         dvbltval = dvbltvalr6_sum, 	
         dvbldval = dvbldvalr6_sum,
         dvlukval = dvlukvalr6_sum, 	
         dvlosval = dvlosvalr6_sum,
         dvoprval = dvoprvalr6_sum,	
         totmort = totmortr6,
         dveqrelval = dveqrelvalr6,
         
         dvhsedebt = dvhsedebtr6_sum,
         dvbltdebt = dvbltdebtr6_sum,
         dvblddebt = dvblddebtr6_sum,
         dvlukdebt = dvlukdebtr6_sum,
         dvlosdebt = dvlosdebtr6_sum,
         dvoprdebt = dvoprdebtr6_sum,
         
         # Household characteristics
         numdepch = case_when(is.na(numdepchw6) ~ numdepchw5,
                              TRUE ~ numdepchw6),
         numadult = case_when(is.na(numadultw6) ~ numadultw5,
                              TRUE ~ numadultw6),
         dvhsize = case_when(is.na(dvhsizew6) ~ dvhsizew5,
                              TRUE ~ dvhsizew6),
         hholdtype = hholdtyper6,
         hrpdvecact = hrpdvecactr6,
         hrpdvage = hrpdvager6,
         hrpdvage9 = hrpdvage9r6,
         hrpsex = NA, # In R6, HRPSex is in person level dataset!
         hrpdvmrdf = case_when(is.na(hrpdvmrdfw6) ~ hrpdvmrdfw5,
                               TRUE ~ hrpdvmrdfw6),
         hrpethnic = case_when(is.na(hrpethnicw6) ~ hrpethnicw5,
                               TRUE ~ hrpethnicw6),
         hrpedlevel = case_when(is.na(hrpedlevelw6) ~ hrpedlevelw5,
                                TRUE ~ hrpedlevelw6),
         
         # Area characteristics
         urindsc = case_when(is.na(urindscw6) ~ as.numeric(urindscw5),
                             TRUE ~ urindscw6),
         urindsc = case_when(urindsc %in% c(1:5) ~ 1,
                             urindsc %in% c(6:8) ~ 2)) %>%
  
  select(wave, hhserial, gor, wgt, wgt_halfsample,
         
         numdepch, numadult, dvhsize, hholdtype, 
         hrpsex, hrpedlevel, hrpdvage, hrpdvage9, hrpethnic, hrpdvecact, hrpdvmrdf,
         
         urindsc,
         
         totwlth, finwlth, phywlth, prowlth, penwlth,
         
         hhpd,
         
         dvproperty, tenure,
         dvhvalue, dvhseval, dvbltval, dvbldval, dvlukval, dvlosval, dvoprval, 
         totmort, dveqrelval, dvhsedebt, dvbltdebt, dvblddebt, dvlukdebt, dvlosdebt, dvoprdebt,
         
         netequincann, dvcacrval, dvsaval, dvisaval, dvfshukv, dvfeshares, 
         dvfcollv, dvfshosv, dvcaodval, totarr_excmort, marrsv1, marrsv2, 
         marrsv3, hhtotrep,
         
         psu, stratum)

clean[[wave]] <- file

# R7 ---------------------------------------------------------------------------

wave <- "r7"

file <- files[[wave]]
colnames(file) <- tolower(colnames(file))

file <- file %>%
  
  mutate(wave = wave,
         hhserial = hhserialr7,
         gor = gorr7,
         wgt = r7xshhwgt,
         wgt_halfsample = NA,
         
         psu = newpsu,
         
         # Main wealth variables
         totwlth = totwlthr7,
         finwlth = hfinwntr7_sum,
         phywlth = hphyswr7,
         prowlth = hpropwr7,
         penwlth = totpenr7_aggr,
         
         # Problem debt (unmanageable debt)
         hhpd = hhpdr7,
         
         # variables required to calculate financial resilience / vulnerability
         netequincann = netequincann_bhcr7,
         dvcacrval = dvcacrvalr7_aggr,
         dvsaval = dvsavalr7_aggr,
         dvisaval = dvisavalr7_aggr,
         dvfshukv = dvfshukvr7_aggr,
         dvfeshares = dvfesharesr7_aggr,
         dvfcollv = dvfcollvr7_aggr,
         dvfshosv = dvfshosvr7_aggr,
         dvcaodval = dvcaodvalr7_aggr,
         totarr_excmort = totarr_excmortr7_aggr,
         
         marrsv1 = marrsv1r7,
         marrsv2 = marrsv2r7,
         marrsv3 = marrsv3r7,
         hhtotrep = hhtotrepr7_sum,
         
         # Home ownership and tenure
         dvproperty = dvpropertyr7,
         dvprirnt = dvprirntr7,
         tenure = case_when(ten1r7_i %in% c(4:5) & dvprirnt == 2 ~ 7,
                            TRUE ~ ten1r7_i),
         
         # Property detail
         dvhvalue = dvhvaluer7,
         dvhseval = dvhsevalr7_sum,
         dvbltval = dvbltvalr7_sum, 	
         dvbldval = dvbldvalr7_sum,
         dvlukval = dvlukvalr7_sum, 	
         dvlosval = dvlosvalr7_sum,
         dvoprval = dvoprvalr7_sum,	
         totmort = totmortr7,
         dveqrelval = dveqrelvalr7,
         
         dvhsedebt = dvhsedebtr7_sum,
         dvbltdebt = dvbltdebtr7_sum,
         dvblddebt = dvblddebtr7_sum,
         dvlukdebt = dvlukdebtr7_sum,
         dvlosdebt = dvlosdebtr7_sum,
         dvoprdebt = dvoprdebtr7_sum,
         
         # Household characteristics
         numdepch = numdepchr7,
         numadult = numadultr7,
         dvhsize = dvhsizer7,
         hholdtype = hholdtyper7,
         hrpdvecact = hrpdvecactr7,
         hrpdvage = hrpdvager7,
         hrpdvage9 = hrpdvage9r7,
         hrpsex = hrpsexr7,
         hrpdvmrdf = hrpdvmrdfr7,
         hrpethnic = hrpethnicr7,
         hrpedlevel = hrpedlevelr7,
         
         # Area characteristics
         urindsc = urindscr7,
         urindsc = case_when(urindsc %in% c(1:5) ~ 1,
                             urindsc %in% c(6:8) ~ 2)) %>%
  
  select(wave, hhserial, gor, wgt, wgt_halfsample,
         
         numdepch, numadult, dvhsize, hholdtype, 
         hrpsex, hrpedlevel, hrpdvage, hrpdvage9, hrpethnic, hrpdvecact, hrpdvmrdf,
         
         urindsc,
         
         totwlth, finwlth, phywlth, prowlth, penwlth,
         
         hhpd,
         
         dvproperty, tenure,
         dvhvalue, dvhseval, dvbltval, dvbldval, dvlukval, dvlosval, dvoprval, 
         totmort, dveqrelval, dvhsedebt, dvbltdebt, dvblddebt, dvlukdebt, dvlosdebt, dvoprdebt,
         
         netequincann, dvcacrval, dvsaval, dvisaval, dvfshukv, dvfeshares, 
         dvfcollv, dvfshosv, dvcaodval, totarr_excmort, marrsv1, marrsv2, 
         marrsv3, hhtotrep,
         
         psu, stratum)

clean[[wave]] <- file

# Combine into single data frame and save
saveRDS(do.call(rbind, clean), "data/clean_household.rds")

rm(list = ls())
