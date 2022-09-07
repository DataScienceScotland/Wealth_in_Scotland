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

charts4 <- list()
tables4 <- list()

# 4_1 Median prop wealth by decile ----------------------------------------------

deciles <- hhld %>%
  group_by(wavenum) %>%
  filter(have_pro == 1) %>%
  mutate(decile = getdeciles(prowlth, weights = wgt)) %>%
  select(wavenum, hhserial, decile)

df <- hhld %>%
  filter(have_pro == 1) %>%
  left_join(deciles, 
            by = c("wavenum", "hhserial")) %>%
  group_by(wavenum, decile) %>%
  summarise(value = wtd.median(prowlth*infl, weights = wgt),
            age = wtd.median(hrpdvage, weights = wgt),
            n = comma2(n())) %>%
  mutate(x = factor(decile),
         y = roundGBP(value),
         period = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods),
         tooltip = paste0("Decile ", x, ": ", fmtGBP(y)),
         data_id = x) %>%
  ungroup()

charts4$chart_1 <- df %>%
  filter(wavenum == max(wavenum)) %>%
  ggplot(aes(x = x, y = y, tooltip = tooltip, data_id = data_id)) +
  geom_col_interactive(fill = SGblue) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  add_source()

tables4$table_1$median <- df %>%
  filter(wavenum == max(wavenum)) %>%
  select(period, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Period = period) %>%
  pivot_wider(values_from = y, names_from = x)

tables4$table_1$age <- df %>%
  filter(wavenum == max(wavenum)) %>%
  select(period, x, age) %>%
  rename(Period = period) %>%
  pivot_wider(values_from = age, names_from = x)

tables4$table_1$sample <- df %>%
  filter(wavenum == max(wavenum)) %>%
  select(period, x, n) %>%
  rename(Period = period) %>%
  pivot_wider(values_from = n, names_from = x)

# 4_2 Median prop wealth by decile over time -----------------------------------

df <- hhld %>%
  filter(have_pro == 1) %>%
  left_join(deciles, 
            by = c("wavenum", "hhserial")) %>%
  group_by(wavenum, decile) %>%
  summarise(value = wtd.median(prowlth*infl, weights = wgt),
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
  filter(have_pro == 1) %>%
  group_by(wavenum) %>%
  summarise(value = wtd.median(prowlth*infl, weights = wgt),
            age = wtd.median(hrpdvage, weights = wgt),
            n = comma2(n())) %>%
  mutate(key = "All",
         y = roundGBP(value),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods, ordered = TRUE)) %>%
  select(x, y, age, n, key)

charts4$chart_2 <- linechart(df) +
  scale_color_manual(values = SGmix10) +
  add_labels(df) +
  scale_x_discrete(expand = expansion(add = c(2.5, 1.5)),
                   breaks = c(periods[1], periods[length(periods)])) +
  add_source()

tables4$table_2$median <- df %>%
  select(key, x, y) %>%
  rbind(total %>% select(key, x, y)) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Decile = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables4$table_2$age <- df %>%
  select(key, x, age) %>%
  rbind(total %>% select(key, x, age)) %>%
  rename(Decile = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables4$table_2$sample <- df %>%
  select(key, x, n) %>%
  rbind(total %>% select(key, x, n)) %>%
  rename(Decile = key) %>%
  pivot_wider(values_from = n, names_from = x)

# Any property wealth ----------------------------------------------------------

hhld %>%
  mutate(key = "All",
         key = as.character(key)) %>%
  rbind(., hhld %>% mutate(key = "All")) %>%
  summarise_data(measure = "have_pro")

# 4_3 Hhld type ----------------------------------------------------------------

df <- hhld %>%
  mutate(key = hholdtype) %>%
  summarise_data(measure = "have_pro")

charts4$chart_3 <- df %>%      
  filter(key != "All") %>%      
  linechart() +
  scale_color_manual(values = SGmix7) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3.5, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables4$table_3$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables4$table_3$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables4$table_3$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables4$table_3$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 4_4 Income quintile ----------------------------------------------------------

df <- hhld %>%
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
  summarise_data(measure = "have_pro")

charts4$chart_4 <- df %>%      
  filter(key != "All") %>%      
  linechart() +
  scale_color_manual(values = SGmix5) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3, 1)),
                   breaks = c(periods[5], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables4$table_4$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables4$table_4$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables4$table_4$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables4$table_4$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 4_5 HRP Employment -----------------------------------------------------------

df <- hhld %>%
  filter(wavenum >= 4) %>%
  mutate(key = hrpdvecact) %>%
  summarise_data(measure = "have_pro")

charts4$chart_5 <- df %>%      filter(key != "All") %>%      linechart() +
  scale_color_manual(values = SGmix4) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3, 1)),
                   breaks = c(periods[4], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables4$table_5$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables4$table_5$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables4$table_5$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables4$table_5$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 4_6 HRP Education ------------------------------------------------------------

df <- hhld %>%
  filter(hrpedlevel != "DK/NA") %>%
  mutate(key = hrpedlevel) %>%
  summarise_data(measure = "have_pro")

charts4$chart_6 <- df %>%      filter(key != "All") %>%      linechart() +
  scale_color_manual(values = SGmix3) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3.5, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables4$table_6$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables4$table_6$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables4$table_6$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables4$table_6$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 4_7 HRP Marital --------------------------------------------------------------

df <- hhld %>%
  mutate(key = hrpdvmrdf) %>%
  summarise_data(measure = "have_pro")

charts4$chart_7 <- df %>%      filter(key != "All") %>%      linechart() +
  scale_color_manual(values = SGmix5) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables4$table_7$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables4$table_7$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables4$table_7$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables4$table_7$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 4_8 HRP Age ------------------------------------------------------------------

df <- hhld %>%
  mutate(key = hrpdvage9) %>%
  summarise_data(measure = "have_pro")

charts4$chart_8 <- df %>%      
  filter(key != "All") %>%  
  linechart() +
  scale_color_manual(values = SGmix6) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(1.2, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables4$table_8$rate <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables4$table_8$composition <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables4$table_8$age <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables4$table_8$sample <- df %>%
  arrange(as.character(key)) %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 4_9 Disabled hhld members ----------------------------------------------------

df <- hhld %>%
  mutate(key = disabled_hh) %>%
  summarise_data(measure = "have_pro")

charts4$chart_9 <- df %>%      filter(key != "All") %>%      linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2.5, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables4$table_9$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables4$table_9$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables4$table_9$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables4$table_9$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)


# 4_10 HRP Ethnic --------------------------------------------------------------

df <- hhld %>%
  filter(hrpethnic != "DK/NA") %>%
  mutate(key = hrpethnic) %>%
  summarise_data(measure = "have_pro")

charts4$chart_10 <- df %>%      filter(key != "All") %>%      linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2.5, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables4$table_10$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables4$table_10$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables4$table_10$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables4$table_10$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 4_11 HRP Sex -----------------------------------------------------------------

df <- hhld %>%
  mutate(key = hrpsex) %>%
  #filter(hholdtype != "Lone parent") %>%
  summarise_data(measure = "have_pro")

charts4$chart_11 <- df %>%      filter(key != "All") %>%      linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(1.5, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables4$table_11$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables4$table_11$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables4$table_11$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables4$table_11$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 4_12 Urban/rural -------------------------------------------------------------

df <- hhld %>%
  filter(!is.na(urindsc)) %>%
  mutate(key = urindsc) %>%
  summarise_data(measure = "have_pro")

charts4$chart_12 <- df %>%      filter(key != "All") %>%      linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables4$table_12$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables4$table_12$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables4$table_12$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables4$table_12$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 4_13 Children in hhld --------------------------------------------------------

df <- hhld %>%
  mutate(key = children_hh) %>%
  summarise_data(measure = "have_pro")

charts4$chart_13 <- df %>%      filter(key != "All") %>%      linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 1)) +
  add_source()

tables4$table_13$rate <- df %>%
  select(key, x, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables4$table_13$composition <- df %>%
  select(key, x, composition) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = composition, names_from = x)

tables4$table_13$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables4$table_13$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# Save all ---------------------------------------------------------------------

saveRDS(charts4, "data/charts4.rds")
saveRDS(tables4, "data/tables4.rds")
