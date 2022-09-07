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
         wave != "w5",
         dvage9 != "0-15",
         isdep != 1) %>%
  mutate(anypen = ifelse(totpen > 0, 1, 0))

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

charts5 <- list()
tables5 <- list()

# 5_1 Median individual pension wealth by decile -------------------------------

deciles <- pers %>%
  filter(totpen > 0) %>%
  group_by(wavenum) %>%
  mutate(decile = getdeciles(totpen, weights = wgt)) %>%
  select(wavenum, hhserial, decile)

df <- pers %>%
  filter(totpen > 0) %>%
  left_join(deciles, 
            by = c("wavenum", "hhserial")) %>%
  group_by(wavenum, decile) %>%
  summarise(value = wtd.median(totpen*infl, weights = wgt),
            age = wtd.median(dvage, weights = wgt),
            n = comma2(n())) %>%
  mutate(x = factor(decile),
         y = roundGBP(value),
         period = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods),
         tooltip = paste0("Decile ", x, ": ", fmtGBP(y)),
         data_id = x) %>%
  ungroup()

charts5$chart_1 <- df %>%
  filter(wavenum == max(wavenum)) %>%
  ggplot(aes(x = x, y = y, tooltip = tooltip, data_id = data_id)) +
  geom_col_interactive(fill = SGblue) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  add_source()

tables5$table_1$median <- df %>%
  filter(wavenum == max(wavenum)) %>%
  select(period, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Period = period) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_1$age <- df %>%
  filter(wavenum == max(wavenum)) %>%
  select(period, x, age) %>%
  rename(Period = period) %>%
  pivot_wider(values_from = age, names_from = x)

tables5$table_1$sample <- df %>%
  filter(wavenum == max(wavenum)) %>%
  select(period, x, n) %>%
  rename(Period = period) %>%
  pivot_wider(values_from = n, names_from = x)

# 5_2 Median pension wealth by decile over time --------------------------------

df <- pers %>%
  filter(totpen > 0) %>%
  left_join(deciles, 
            by = c("wavenum", "hhserial")) %>%
  group_by(wavenum, decile) %>%
  summarise(value = wtd.median(totpen*infl, weights = wgt),
            age = wtd.median(dvage, weights = wgt),
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

total <- pers %>%
  filter(totpen > 0) %>%
  group_by(wavenum) %>%
  summarise(value = wtd.median(totpen*infl, weights = wgt),
            age = wtd.median(dvage, weights = wgt),
            n = comma2(n())) %>%
  mutate(key = "All",
         y = roundGBP(value),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods, ordered = TRUE)) %>%
  select(x, y, age, n, key)

charts5$chart_2 <- linechart(df) +
  scale_color_manual(values = SGmix10) +
  add_labels(df) +
  scale_x_discrete(expand = expansion(add = c(2.5, 1.5)),
                   breaks = c(periods[1], periods[length(periods)])) +
  add_source()

tables5$table_2$median <- df %>%
  select(key, x, y) %>%
  rbind(total %>% select(key, x, y)) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Decile = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_2$age <- df %>%
  select(key, x, age) %>%
  rbind(total %>% select(key, x, age)) %>%
  rename(Decile = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables5$table_2$sample <- df %>%
  select(key, x, n) %>%
  rbind(total %>% select(key, x, n)) %>%
  rename(Decile = key) %>%
  pivot_wider(values_from = n, names_from = x)


# 5_3 Saving for a pension by age and sex --------------------------------------

df <- pers %>%
  filter(wavenum == max(wavenum)) %>%
  mutate(anysavpen = ifelse(savpen > 0, 1, 0)) %>%
  group_by(dvage9, sex) %>%
  mutate(age = wtd.median(dvage, weights = wgt),
         n = n()) %>%
  group_by(dvage9, sex, anysavpen) %>%
  summarise(people = sum(wgt),
            age = max(age),
            n = comma2(max(n))) %>%
  group_by(dvage9, sex) %>%
  mutate(rate = people/sum(people),
         rate = round2(rate, 2)) %>%
  filter(anysavpen == 1) %>%
  group_by(sex) %>%
  mutate(composition = people/sum(people)) %>%
  ungroup() %>%
  mutate(x = dvage9, 
         y = rate,
         composition = percent2(composition),
         key = sex,
         tooltip = paste0(key, ", ", x, ": ", percent2(rate)),
         data_id = paste(x, key)) %>%
  select(x, y, composition, n, age, key, tooltip, data_id)

charts5$chart_3 <- df %>% 
  ggplot(aes(x = x, y = y, tooltip = tooltip, data_id = data_id)) +
  geom_col_interactive(fill = SGblue) +
  facet_wrap(~key) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  add_source()

tables5$table_3$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_3$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 5_4 Saving for a pension by sex over time ------------------------------------

df <- pers %>%
  mutate(anysavpen = ifelse(savpen > 0, 1, 0)) %>%
  group_by(wavenum, sex) %>%
  mutate(age = wtd.median(dvage, weights = wgt),
         n = n()) %>%
  group_by(wavenum, sex, anysavpen) %>%
  summarise(people = sum(wgt),
            age = max(age),
            n = comma2(max(n))) %>%
  group_by(wavenum, sex) %>%
  mutate(rate = people/sum(people),
         rate = round2(rate, 2)) %>%
  filter(anysavpen == 1) %>%
  group_by(wavenum) %>%
  mutate(composition = people/sum(people)) %>%
  ungroup() %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods, ordered = TRUE), 
         y = rate,
         composition = percent2(composition),
         key = sex,
         tooltip = paste0(key, ": ", percent2(rate), " (", x, ")"),
         data_id = paste(x, key),
         label = case_when(x == min(x) ~ paste0(key, ": ", percent2(rate)),
                           x == max(x) ~ percent2(rate))) %>%
  select(x, y, composition, n, age, key, label, tooltip, data_id)

charts5$chart_4 <- linechart(df) +
  scale_colour_manual(values = SGmix2) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.7)) +
  add_labels(df) +
  add_source()

tables5$table_4$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_4$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables5$table_4$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 5_5 Pensions in payment by age and sex ---------------------------------------

df <- pers %>%
  filter(wavenum == max(wavenum)) %>%
  mutate(anypaypen = ifelse(paypen > 0, 1, 0)) %>%
  group_by(dvage9, sex) %>%
  mutate(age = wtd.median(dvage, weights = wgt),
         n = n()) %>%
  group_by(dvage9, sex, anypaypen) %>%
  summarise(people = sum(wgt),
            age = max(age),
            n = comma2(max(n))) %>%
  group_by(dvage9, sex) %>%
  mutate(rate = people/sum(people),
         rate = round2(rate, 2)) %>%
  filter(anypaypen == max(anypaypen)) %>%
  group_by(sex) %>%
  mutate(people = ifelse(anypaypen == 0, 0, people),
         rate = ifelse(anypaypen == 0, 0, rate),
         composition = people/sum(people)) %>%
  ungroup() %>%
  mutate(x = dvage9, 
         y = rate,
         composition = percent2(composition),
         key = sex,
         tooltip = paste0(key, ", ", x, ": ", percent2(rate)),
         data_id = paste(x, key)) %>%
  select(x, y, composition, n, age, key, tooltip, data_id)

charts5$chart_5 <- df %>% 
  ggplot(aes(x = x, y = y, tooltip = tooltip, data_id = data_id)) +
  geom_col_interactive(fill = SGblue) +
  facet_wrap(~key) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  add_source()

tables5$table_5$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_5$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 5_6 Pensions in payment by age and sex over time -----------------------------

df <- pers %>%
  mutate(anypaypen = ifelse(paypen > 0, 1, 0)) %>%
  group_by(wavenum, sex) %>%
  mutate(age = wtd.median(dvage, weights = wgt),
         n = n()) %>%
  group_by(wavenum, sex, anypaypen) %>%
  summarise(people = sum(wgt),
            age = max(age),
            n = comma2(max(n))) %>%
  group_by(wavenum, sex) %>%
  mutate(rate = people/sum(people),
         rate = round2(rate, 2)) %>%
  filter(anypaypen == max(anypaypen)) %>%
  group_by(wavenum) %>%
  mutate(people = ifelse(anypaypen == 0, 0, people),
         rate = ifelse(anypaypen == 0, 0, rate),
         composition = people/sum(people)) %>%
  ungroup() %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods, ordered = TRUE), 
         y = rate,
         composition = percent2(composition),
         key = sex,
         tooltip = paste0(key, ": ", percent2(rate), " (", x, ")"),
         data_id = paste(x, key),
         label = case_when(x == min(x) ~ paste0(key, ": ", percent2(rate)),
                           x == max(x) ~ percent2(rate))) %>%
  select(x, y, composition, n, age, key, label, tooltip, data_id)

charts5$chart_6 <- linechart(df) +
  scale_colour_manual(values = SGmix2) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 0.7)) +
  add_labels(df) +
  add_source()

tables5$table_6$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_6$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables5$table_6$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# Any pension wealth by various characteristics --------------------------------
# split by in payment / not in payment??

# 5_7 Income quintile ----------------------------------------------------------

df <- pers %>%
  filter(wavenum >= 5) %>%
  left_join(hhld %>%
              filter(wavenum >= 5) %>%
              group_by(wavenum) %>%
              mutate(decile = getdeciles(netequincann, weights = wgt)) %>%
              select(wavenum, decile, hhserial),
            by = c("wavenum", "hhserial")) %>%
  mutate(key = case_when(decile <= 2 ~ "1st (lowest) household income quintile",
                         decile <= 4 ~ "2nd",
                         decile <= 6 ~ "3rd",
                         decile <= 8 ~ "4th",
                         TRUE ~ "5th (highest) household income quintile")) %>%
  summarise_data(measure = "anypen")

charts5$chart_7 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix5) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3.5, 1)),
                   breaks = c(periods[5], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables5$table_7$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_7$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables5$table_7$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables5$table_7$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)


# 5_8 Education ----------------------------------------------------------------

df <- pers %>%
  filter(edlevel != "DK/NA") %>%
  mutate(key = edlevel) %>%
  summarise_data(measure = "anypen")

charts5$chart_8 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix3) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3.5, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables5$table_8$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_8$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables5$table_8$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables5$table_8$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 5_9 Employment ---------------------------------------------------------------

df <- pers %>%
  filter(wavenum >= 5,
         ecact != "Ineligible / missing") %>%
  mutate(key = ecact) %>%
  summarise_data(measure = "anypen")

charts5$chart_9 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix7) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3.5, 1)),
                   breaks = c(periods[5], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables5$table_9$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_9$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables5$table_9$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables5$table_9$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 5_10 Orientation --------------------------------------------------------------

df <- pers %>%
  filter(persprox == 1,
         sid != "DK/NA") %>%
  mutate(key = sid) %>%
  summarise_data(measure = "anypen")

charts5$chart_10 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[5], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables5$table_10$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_10$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables5$table_10$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables5$table_10$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 5_11 Marital -----------------------------------------------------------------

df <- pers %>%
  mutate(key = dvmrdf) %>%
  summarise_data(measure = "anypen")

charts5$chart_11 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix5) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables5$table_11$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_11$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables5$table_11$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables5$table_11$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 5_12 Age ---------------------------------------------------------------------

df <- pers %>%
  mutate(key = dvage9) %>%
  summarise_data(measure = "anypen")

charts5$chart_12 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix6) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 2)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables5$table_12$rate <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_12$composition <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables5$table_12$age <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables5$table_12$sample <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 5_13 Religion ----------------------------------------------------------------

df <- pers %>%
  filter(persprox == 1,
         religion != "DK/NA") %>%
  mutate(key = religion) %>%
  summarise_data(measure = "anypen")

charts5$chart_13 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix3) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2.5, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables5$table_13$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_13$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables5$table_13$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables5$table_13$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 5_14 Ethnicity ---------------------------------------------------------------

df <- pers %>%
  filter(persprox == 1,
         ethnic != "DK/NA") %>%
  mutate(key = ethnic) %>%
  summarise_data(measure = "anypen")

charts5$chart_14 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2.5, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables5$table_14$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_14$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables5$table_14$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables5$table_14$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 5_15 Disability -------------------------------------------------------------- 

df <- pers %>%
  mutate(key = disabled) %>%
  summarise_data(measure = "anypen")

charts5$chart_15 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables5$table_15$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_15$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables5$table_15$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables5$table_15$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 5_16 Sex ---------------------------------------------------------------------

df <- pers %>%
  mutate(key = sex) %>%
  summarise_data(measure = "anypen")

charts5$chart_16 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables5$table_16$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables5$table_16$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables5$table_16$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables5$table_16$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)





# Save all ---------------------------------------------------------------------

saveRDS(charts5, "data/charts5.rds")
saveRDS(tables5, "data/tables5.rds")