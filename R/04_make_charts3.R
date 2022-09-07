
# Prelims ----------------------------------------------------------------------

library(tidyverse)
library(ggiraph)
library(scales)
library(Hmisc)
library(analysistools)
library(ggrepel)

source("R/00_functions.R", encoding = "UTF-8")
source("R/00_colours.R")

hhld <- readRDS("data/tidy_household.rds") %>% 
  filter(gor == "Scotland",
         wave != "w5")

pers <- readRDS("data/tidy_person.rds") %>% 
  filter(gor == "Scotland",
         wave != "w5")

tidyhbai <- readRDS("data/tidyhbai.rds") 

mytheme <- theme_grey() +
  theme(text = element_text(colour = SGgrey, size = 14),
        line = element_line(colour = SGgrey,
                            linetype = 1,
                            lineend = 2,
                            size = 0.5),
        
        plot.title = element_text(hjust = 0, colour = SGgrey, size = 12),
        plot.caption = element_text(hjust = 1),
        plot.title.position = "plot",
        
        legend.position = "top",
        legend.title = element_blank(),
        legend.key = element_blank(),
        
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        axis.line.x = element_line(),
        axis.ticks.length = unit(2, "pt"),
        axis.ticks.y = element_blank(),
        
        axis.title = element_blank())

theme_set(mytheme)

periods <- c("2006-2008", "2008-2010", "2010-2012", "2012-2014", "2014-2016", 
             "2016-2018", "2018-2020")

charts3 <- list()
tables3 <- list()

# 3_1 Median fin wealth by decile ----------------------------------------------

deciles <- hhld %>%
  group_by(wavenum) %>%
  mutate(decile = getdeciles(finwlth, weights = wgt)) %>%
  select(wavenum, hhserial, decile)
  
df <- hhld %>%
  left_join(deciles, 
            by = c("wavenum", "hhserial")) %>%
  group_by(wavenum, decile) %>%
  summarise(value = wtd.median(finwlth*infl, weights = wgt),
            age = wtd.median(hrpdvage, weights = wgt),
            n = comma2(n())) %>%
  mutate(x = factor(decile),
         y = roundGBP(value),
         period = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods),
         tooltip = paste0("Decile ", x, ": ", fmtGBP(y)),
         data_id = x) %>%
  ungroup()

charts3$chart_1 <- df %>%
  filter(wavenum == max(wavenum)) %>%
  ggplot(aes(x = x, y = y, tooltip = tooltip, data_id = data_id)) +
  geom_col_interactive(fill = SGblue) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  add_source()

tables3$table_1$median <- df %>%
  filter(wavenum == max(wavenum)) %>%
  select(period, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Period = period) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_1$age <- df %>%
  filter(wavenum == max(wavenum)) %>%
  select(period, x, age) %>%
  rename(Period = period) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_1$sample <- df %>%
  filter(wavenum == max(wavenum)) %>%
  select(period, x, n) %>%
  rename(Period = period) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_2 Median fin wealth by decile over time -----------------------------------

df <- hhld %>%
  left_join(deciles, 
            by = c("wavenum", "hhserial")) %>%
  group_by(wavenum, decile) %>%
  summarise(value = wtd.median(finwlth*infl, weights = wgt),
            age = wtd.median(hrpdvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(key = factor(decile),
         y = roundGBP(value),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods, ordered = TRUE),
         label = case_when(x == min(x) ~ paste0("Decile ", key, ": ", fmtGBP(y)),
                           TRUE ~ fmtGBP(y)),
         tooltip = paste0("Decile ", key, ": ", fmtGBP(y), " (", x, ")"),
         data_id = paste(x, key)) %>%
  select(x, y, age, n, key, label, tooltip, data_id)

total <- hhld %>%
  group_by(wavenum) %>%
  summarise(value = wtd.median(finwlth*infl, weights = wgt),
            age = wtd.median(hrpdvage, weights = wgt),
            n = comma2(n())) %>%
  mutate(key = "All",
         y = roundGBP(value),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods, ordered = TRUE)) %>%
  select(x, y, age, n, key)

charts3$chart_2 <- df %>%   filter(key != "All") %>%   linechart() +
  scale_color_manual(values = SGmix10) +
  add_labels(df) +
  scale_x_discrete(expand = expansion(add = c(2.5, 1.5)),
                   breaks = c(periods[1], periods[length(periods)])) +
  add_source()

tables3$table_2$median <- df %>%
  select(key, x, y) %>%
  rbind(total %>% select(x, y, key)) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Decile = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_2$age <- df %>%
  select(key, x, age) %>%
  rbind(total %>% select(x, key, age)) %>%
  rename(Decile = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_2$sample <- df %>%
  select(key, x, n) %>%
  rbind(total %>% select(x, n, key)) %>%
  rename(Decile = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_3 Financial vulnerability over time ----------------------------------------

# Average poverty lines for each wave from HBAI data
# Note that inflation adjustment is not required
povline <- tidyhbai %>%
  filter(yearn >= 13) %>%
  group_by(yearn) %>%
  summarise(median = wtd.median(s_oe_bhc, weights = gs_newpp)) %>%
  mutate(povline = 0.6 * median,
         wave = case_when(yearn <= 14 ~ "w1",
                          yearn <= 16 ~ "w2",
                          yearn <= 18 ~ "w3",
                          yearn <= 20 ~ "w4",
                          yearn <= 22 ~ "r5",
                          yearn <= 24 ~ "r6",
                          yearn <= 26 ~ "r7"),
         wave = factor(wave, levels = c("w1", "w2", "w3", "w4", "w5", "r5", 
                                        "r6", "r7"),
                       ordered = TRUE)) %>%
  group_by(wave) %>%
  summarise(povline = mean(povline))

inc_deciles <- hhld %>%
  filter(wavenum >= 5) %>%
  group_by(wavenum) %>%
  mutate(decile = getdeciles(netequincann, weights = wgt)) %>%
  select(wavenum, decile, hhserial)

# Vulnerability dataset to be used for all charts in this section
vulnerability <- hhld %>%
  left_join(inc_deciles, 
            by = c("wavenum", "hhserial")) %>%
  left_join(povline, by = "wave") %>%
  mutate(povline_monthly =  365/12 * povline / 7,
         
         # monthly repayments only available since w5
         hhtotrep = ifelse(wavenum <= 4, 0, hhtotrep),
         
         # assets need to cover equivalised poverty line equivalent amount 
         # plus monthly repayments
         months_above_povline = liquid_assets / (equ * povline_monthly + hhtotrep),
         
         # binary vulnerability measure
         vulnerable = ifelse(months_above_povline < 1, 1, 0),
         
         # detailed vulnerability measure
         months_above_povline = case_when(months_above_povline > 12 ~ "> 12 months",
                                          months_above_povline > 6 ~ "6-12 months",
                                          months_above_povline > 3 ~ "3-5 months",
                                          months_above_povline > 1 ~ "1-2 months",
                                          months_above_povline > 0 ~ "< 1 month",
                                          TRUE ~ "In debt already"),
         months_above_povline = factor(months_above_povline,
                                       levels = c("In debt already",
                                                  "< 1 month",
                                                  "1-2 months",
                                                  "3-5 months",
                                                  "6-12 months",
                                                  "> 12 months")),
         months_above_povline = fct_rev(months_above_povline)) 

df <- vulnerability %>%
  mutate(key = months_above_povline) %>%
  group_by(wavenum, key) %>%
  summarise(people = sum(wgt),
            n = n(),
            age = wtd.median(hrpdvage, weights = wgt)) %>%
  mutate(share = people/sum(people)) %>%
  ungroup() %>%
  group_by(wavenum) %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = round2(share, 2),
         n = sum(n),
         n = comma2(n),
         label = case_when(x == min(x) ~ paste0(key, ": ", percent2(y)),
                           TRUE ~ percent2(y)),
         tooltip = paste0(paste0(key, ": ", percent2(y)), " (", x, ")"),
         data_id = paste(x, key)) %>%
  ungroup()

charts3$chart_3 <- df %>% 
  ggplot(aes(x = x, y = y, fill = key, tooltip = tooltip, data_id = data_id)) +
  geom_col_interactive(position = "fill", colour = "white") +
  scale_fill_manual(values = SGmix6) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(breaks = c(periods[1], periods[length(periods)])) +
  theme(legend.position = "right") +
  labs(caption = "Source: Wealth and Assets Survey; Family Resources Survey")

tables3$table_3$share <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename("Period of time" = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_3$sample <- df %>%
  select(key, x, n) %>%
  pivot_wider(values_from = n, names_from = x) %>%
  mutate(Group = "All") %>%
  select(Group, everything()) %>%
  select(-key) %>%
  head(1L)

# Income drop question

df <- pers %>%
  filter(persprox == 1,
         incdrop != "DK/NA",
         wavenum >= 4) %>%
  group_by(wavenum, incdrop) %>%
  summarise(adults = sum(wgt),
            n = n()) %>%
  group_by(wavenum) %>%
  mutate(share = adults/sum(adults),
         share = percent2(share)) %>%
  ungroup() %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8),
                    labels = periods),
         Period = incdrop) %>%
  arrange(desc(Period))

tables3$table_3$incdrop <- df %>%
  select(Period, x, share) %>%
  pivot_wider(names_from = x, values_from = share) %>%
  rename("Period of time" = Period)

tables3$table_3$incdrop_sample <- df %>%
  group_by(x) %>%
  summarise(n = comma2(sum(n)),
            Period = "All") %>%
  select(Period, x, n) %>%
  pivot_wider(names_from = x, values_from = n) 

# 3_4 FV: Hhld type ------------------------------------------------------------

df <- vulnerability %>%
  mutate(key = hholdtype) %>%
  summarise_data(measure = "vulnerable")

charts3$chart_4 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix7) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3.5, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(caption = "Source: Wealth and Assets Survey; Family Resources Survey")

tables3$table_4$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_4$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_4$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_4$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_5 FV: HRP Employment -------------------------------------------------------

df <- vulnerability %>%
  mutate(key = hrpdvecact) %>%
  filter(wavenum >= 4) %>%
  summarise_data(measure = "vulnerable")

charts3$chart_5 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix4) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3, 1)),
                   breaks = c(periods[4], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(caption = "Source: Wealth and Assets Survey; Family Resources Survey")

tables3$table_5$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_5$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_5$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_5$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_6 FV: Tenure ---------------------------------------------------------------

df <- vulnerability %>%
  mutate(key = tenure) %>%
  filter(key != "DK/NA") %>%
  summarise_data(measure = "vulnerable")

charts3$chart_6 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix4) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(caption = "Source: Wealth and Assets Survey; Family Resources Survey")

tables3$table_6$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_6$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_6$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_6$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_7 FV: Income quintile ------------------------------------------------------

df <- vulnerability %>%
  mutate(key = case_when(decile <= 2 ~ "1st (lowest) household income quintile",
                         decile <= 4 ~ "2nd",
                         decile <= 6 ~ "3rd",
                         decile <= 8 ~ "4th",
                         TRUE ~ "5th (highest) household income quintile")) %>%
  filter(wavenum >= 5) %>%
  summarise_data(measure = "vulnerable")

charts3$chart_7 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix5) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3, 1)),
                   breaks = c(periods[5], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(caption = "Source: Wealth and Assets Survey; Family Resources Survey")

tables3$table_7$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_7$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_7$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_7$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_8 FV: HRP Education --------------------------------------------------------

df <- vulnerability %>%
  mutate(key = hrpedlevel) %>%
  filter(key != "DK/NA") %>%
  summarise_data(measure = "vulnerable")

charts3$chart_8 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix3) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3.5, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(caption = "Source: Wealth and Assets Survey; Family Resources Survey")

tables3$table_8$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_8$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_8$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_8$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_9 FV: HRP Age --------------------------------------------------------------

df <- vulnerability %>%
  mutate(key = hrpdvage9) %>%
  summarise_data(measure = "vulnerable")

charts3$chart_9 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix6) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(1.2, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(caption = "Source: Wealth and Assets Survey; Family Resources Survey")

tables3$table_9$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_9$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_9$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_9$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_10 FV: HRP Marital ----------------------------------------------------------

df <- vulnerability %>%
  mutate(key = hrpdvmrdf) %>%
  summarise_data(measure = "vulnerable")

charts3$chart_10 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix5) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(caption = "Source: Wealth and Assets Survey; Family Resources Survey")

tables3$table_10$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_10$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_10$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_10$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_11 FV: Children in hhld -----------------------------------------------------

df <- vulnerability %>%
  mutate(key = children_hh) %>%
  summarise_data(measure = "vulnerable")

charts3$chart_11 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(caption = "Source: Wealth and Assets Survey; Family Resources Survey")

tables3$table_11$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_11$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_11$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_11$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_12 FV: Disabled hhld members ------------------------------------------------

df <- vulnerability %>%
  mutate(key = disabled_hh) %>%
  summarise_data(measure = "vulnerable")

charts3$chart_12 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(caption = "Source: Wealth and Assets Survey; Family Resources Survey")

tables3$table_12$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_12$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_12$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_12$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)


# 3_13 FV: HRP Sex --------------------------------------------------------------

df <- vulnerability %>%
  mutate(key = hrpsex) %>%
  #filter(hholdtype != "Lone parent") %>%
  summarise_data(measure = "vulnerable")

charts3$chart_13 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(1.5, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(caption = "Source: Wealth and Assets Survey; Family Resources Survey")

tables3$table_13$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_13$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_13$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_13$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_14 FV: Urban/rural ----------------------------------------------------------

df <- vulnerability %>%
  mutate(key = urindsc) %>%
  filter(!is.na(key)) %>%
  summarise_data(measure = "vulnerable")

charts3$chart_14 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(caption = "Source: Wealth and Assets Survey; Family Resources Survey")

tables3$table_14$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_14$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_14$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_14$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_15 FV: HRP Ethnic -----------------------------------------------------------

df <- vulnerability %>%
  mutate(key = hrpethnic) %>%
  filter(key != "DK/NA") %>%
  summarise_data(measure = "vulnerable")

charts3$chart_15 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2.5, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(caption = "Source: Wealth and Assets Survey; Family Resources Survey")

tables3$table_15$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_15$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_15$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_15$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)


# Unmanageable debt ------------------------------------------------------------

inc_deciles <- hhld %>%
  filter(wavenum >= 5) %>%
  group_by(wavenum) %>%
  mutate(decile = getdeciles(netequincann, weights = wgt)) %>%
  select(wavenum, decile, hhserial)

debt <- hhld %>%
  filter(wavenum >= 3) %>%
  left_join(inc_deciles, by = c("wavenum", "hhserial"))

# 3_16 UD: All -----------------------------------------------------------

df <- debt %>%
  mutate(key = "All") %>%
  summarise_data(measure = "hhpd", rounding = 3) %>%
  mutate(label = paste0(format(y * 100), "%"),
         tooltip = paste0(label, " (", x, ")"),
         data_id = x,
         # hacky fix to undo doubling of sample size in summarise_data function
         n = comma(as.numeric(str_remove(n, ","))/2))

charts3$chart_16 <- df %>%
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(df) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.1)) +
  add_source()

tables3$table_16$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = paste0(format(y * 100), "%")) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_16$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_16$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_17 UD: HRP Employment ------------------------------------------------------

df <- debt %>%
  mutate(key = hrpdvecact) %>%
  filter(wavenum >= 4) %>%
  summarise_data(measure = "hhpd", rounding = 3)

charts3$chart_17 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix4) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[4], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.25)) +
  add_source()

tables3$table_17$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = paste0(format(y * 100), "%")) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_17$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_17$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_17$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_18 UD: Hhld type -----------------------------------------------------------

df <- debt %>%
  mutate(key = hholdtype) %>%
  summarise_data(measure = "hhpd", rounding = 3)

charts3$chart_18 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix7) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2.5, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.25)) +
  add_source()

tables3$table_18$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = paste0(format(y * 100), "%")) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_18$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_18$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_18$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_19 UD: Tenure --------------------------------------------------------------

df <- debt %>%
  mutate(key = tenure) %>%
  filter(key != "DK/NA") %>%
  summarise_data(measure = "hhpd", rounding = 3)

charts3$chart_19 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix4) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2.5, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.25)) +
  add_source()

tables3$table_19$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = paste0(format(y * 100), "%")) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_19$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_19$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_19$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_20 UD: HRP ethnic ----------------------------------------------------------

df <- debt %>%
  mutate(key = hrpethnic) %>%
  filter(key != "DK/NA") %>%
  summarise_data(measure = "hhpd", rounding = 3)

charts3$chart_20 <- df %>%   
  filter(key != "All") %>%   
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.25)) +
  add_source()

tables3$table_20$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = paste0(format(y * 100), "%")) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_20$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_20$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_20$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_21 UD: HRP Age -------------------------------------------------------------

df <- debt %>%
  mutate(key = hrpdvage9) %>%
  summarise_data(measure = "hhpd", rounding = 3)

charts3$chart_21 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix6) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(1.2, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.25)) +
  add_source()

tables3$table_21$rate <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, y) %>%
  mutate(y = paste0(format(y * 100), "%")) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_21$composition <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_21$age <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_21$sample <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_22 UD: Children ------------------------------------------------------------

df <- debt %>%
  mutate(key = children_hh) %>%
  summarise_data(measure = "hhpd", rounding = 3)

charts3$chart_22 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.25)) +
  add_source()

tables3$table_22$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = paste0(format(y * 100), "%")) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_22$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_22$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_22$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)


# 3_23 UD: Income quintile -----------------------------------------------------

df <- debt %>%
  mutate(key = case_when(decile <= 2 ~ "1st (lowest) household income quintile",
                         decile <= 4 ~ "2nd",
                         decile <= 6 ~ "3rd",
                         decile <= 8 ~ "4th",
                         TRUE ~ "5th (highest) household income quintile")) %>%
  filter(wavenum >= 5) %>%
  summarise_data(measure = "hhpd", rounding = 3)

charts3$chart_23 <- df %>%   
  filter(key != "All") %>%   
  linechart() +
  scale_color_manual(values = SGmix5) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3, 1)),
                   breaks = c(periods[5], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.25)) +
  add_source()

tables3$table_23$rate <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, y) %>%
  mutate(y = paste0(format(y * 100), "%")) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_23$composition <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_23$age <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_23$sample <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_24 UD: HRP Marital ---------------------------------------------------------

df <- debt %>%
  mutate(key = hrpdvmrdf) %>%
  rbind(., debt %>% mutate(key = "All")) %>%
  summarise_data(measure = "hhpd", rounding = 3)

charts3$chart_24 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix5) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(1.5, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.25)) +
  add_source()

tables3$table_24$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = paste0(format(y * 100), "%")) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_24$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_24$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_24$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)


# 3_25 UD: Disabled ------------------------------------------------------------

df <- debt %>%
  mutate(key = disabled_hh) %>%
  summarise_data(measure = "hhpd", rounding = 3)

charts3$chart_25 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2.5, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.25)) +
  add_source()

tables3$table_25$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = paste0(format(y * 100), "%")) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_25$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_25$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_25$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_26 UD: HRP Education -------------------------------------------------------

df <- debt %>%
  mutate(key = hrpedlevel) %>%
  filter(key != "DK/NA") %>%
  summarise_data(measure = "hhpd", rounding = 3)

charts3$chart_26 <- df %>%   
  filter(key != "All") %>%   
  linechart() +
  scale_color_manual(values = SGmix3) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2.5, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.25)) +
  add_source()

tables3$table_26$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = paste0(format(y * 100), "%")) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_26$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_26$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_26$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 3_27 UD: HRP Sex -------------------------------------------------------------

df <- debt %>%
  mutate(key = hrpsex) %>%
  #filter(hholdtype != "Lone parent") %>%
  summarise_data(measure = "hhpd", rounding = 3)

charts3$chart_27 <- df %>%   
  filter(key != "All") %>%   
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(1.5, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.25)) +
  add_source()

tables3$table_27$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = paste0(format(y * 100), "%")) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_27$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_27$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_27$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)


# 3_28 UD: Urban/rural ---------------------------------------------------------

df <- debt %>%
  mutate(key = urindsc) %>%
  filter(!is.na(key)) %>%
  summarise_data(measure = "hhpd", rounding = 3)

charts3$chart_28 <- df %>%   
  filter(key != "All") %>%   
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.25)) +
  add_source()

tables3$table_28$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = paste0(format(y * 100), "%")) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables3$table_28$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables3$table_28$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables3$table_28$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)



# Save all ---------------------------------------------------------------------

saveRDS(charts3, "data/charts3.rds")
saveRDS(tables3, "data/tables3.rds")

rm(list = ls())
