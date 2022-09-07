
# Prelims ----------------------------------------------------------------------

library(tidyverse)
library(ggiraph)
library(scales)
library(Hmisc)
library(analysistools)
library(ggrepel)

source("R/00_functions.R", encoding = "UTF-8")
source("R/00_colours.R")

# use full W1 sample for fin, pro, pen
hhld_full_w1 <- readRDS("data/tidy_household.rds") %>% 
  filter(gor == "Scotland",
         wave != "w5")

# exclude W1 half sample where physical wealth q's weren't asked
hhld <- hhld_full_w1 %>% 
  filter(!(wave == "w1" & wgt_halfsample == 0))

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

types <- c("Financial", "Physical", "Property", "Pension")
periods <- c("2006-2008", "2008-2010", "2010-2012", "2012-2014", "2014-2016", 
             "2016-2018", "2018-2020")


col_types <- c("Financial" = SGmix4[4], "Physical" = SGmix4[3], 
               "Property" = SGmix4[2], "Pension" = SGmix4[1],
               "Total" = SGmix4[1], "All" = SGmix4[1])

charts1 <- list()
tables1 <- list()

# 1_1 Wealth over time ---------------------------------------------------------

df <- hhld %>%
  group_by(wavenum) %>%
  summarise(value = wtd.median(totwlth * infl, weights = wgt),
            n = comma2(n())) %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         key = "All",
         y = roundGBP(value),
         label = stringi::stri_enc_toutf8(fmtGBP(y)),
         tooltip = stringi::stri_enc_toutf8(paste0("Median wealth: ", 
                                                   fmtGBP(y), " (", x, ")")),
         data_id = paste(x, key))

charts1$chart_1 <- linechart(df) +
  scale_y_continuous(limits = c(2E4, 5E5)) +
  scale_x_discrete(expand = c(0.15, 0.15)) +
  add_labels(df)  +
  scale_color_manual(values = col_types) +
  add_source()

tables1$table_1$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables1$table_1$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 1_2 Median wealth by wealth type ---------------------------------------------

df <- hhld %>%
  filter(wavenum == max(wavenum)) %>%
  summarise(Financial = wtd.median(finwlth, weights = wgt),
            Physical = wtd.median(phywlth, weights = ifelse(wavenum == 1, wgt_halfsample, wgt)),
            Property = wtd.median(prowlth, weights = wgt),
            Pension = wtd.median(penwlth, weights = wgt),
            n = comma2(n()),
            n_w1_phys = sum(wgt_halfsample > 0)) %>%
  pivot_longer(cols = types) %>%
  mutate(x = factor(name, levels = types),
         x = fct_rev(x),
         y = roundGBP(value),
         label = stringi::stri_enc_toutf8(fmtGBP(y)),
         tooltip = stringi::stri_enc_toutf8(paste0("Median ", tolower(x), " wealth:\n", 
                                                   fmtGBP(y)))) %>%
  select(x, y, n, label, tooltip)

charts1$chart_2 <- df %>%
  ggplot(aes(x = x, y = y, fill = x, 
             label = label,
             tooltip = tooltip,
             data_id = x)) +
  geom_col_interactive(show.legend = FALSE) +
  geom_text(colour = SGgrey, 
            nudge_y = 6000) +
  scale_fill_manual(values = col_types) + 
  scale_y_continuous(labels = comma_format(prefix = "£"),
                     limits = c(0, 100000)) +
  coord_flip() +
  add_source()

tables1$table_2$median <- df %>%
  select(x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Type = x,
         Median = y)

tables1$table_2$sample <- df %>%
  select(x, n) %>%
  rename(Type = x,
         Sample = n)

# 1_3 Median wealth by wealth type over time -----------------------------------

df <- hhld_full_w1 %>%
  group_by(wavenum) %>%
  summarise(Financial = wtd.median(finwlth * infl, weights = wgt),
            Physical = wtd.median(phywlth * infl, weights = ifelse(wavenum == 1, wgt_halfsample, wgt)),
            Property = wtd.median(prowlth * infl, weights = wgt),
            Pension = wtd.median(penwlth * infl, weights = wgt),
            n = n(),
            n_w1_phys = sum(wgt_halfsample > 0)) %>%
  pivot_longer(cols = types) %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         n = ifelse(wavenum == 1 & name == "Physical", n_w1_phys, n),
         n = comma2(n),
         key = factor(name, levels = types),
         key = fct_rev(key),
         y = roundGBP(value),
         label = case_when(wavenum == min(wavenum) ~ paste0(key, " wealth: ", stringi::stri_enc_toutf8(fmtGBP(y))),
                           TRUE ~ stringi::stri_enc_toutf8(fmtGBP(y))),
         tooltip = stringi::stri_enc_toutf8(paste0("Median ", tolower(key), " wealth: ", 
                                                   fmtGBP(y), " (", x, ")")),
         data_id = paste(x, key)) %>%
  select(x, y, key, n, label, tooltip, data_id)

charts1$chart_3 <- linechart(df) +
  scale_colour_manual(values = col_types) + 
  scale_x_discrete(expand = expansion(add = c(3, 1.5)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(-2E4, 1E5)) +
  add_labels(df) +
  add_source()

tables1$table_3$median <- df %>%
  select(x, y, key) %>%
  mutate(y = fmtGBP(y)) %>%
  pivot_wider(names_from = x, values_from = y) %>%
  rename(Type = key)

tables1$table_3$sample <- df %>%
  select(x, n, key) %>%
  pivot_wider(names_from = x, values_from = n) %>%
  rename(Type = key)

# 1_4 Median wealth by wealth type and age -------------------------------------

df <- hhld %>%
  filter(wavenum == max(wavenum)) %>%
  group_by(hrpdvage9) %>%
  summarise(Financial = wtd.median(finwlth, weights = wgt),
            Physical = wtd.median(phywlth, weights = wgt),
            Property = wtd.median(prowlth, weights = wgt),
            Pension = wtd.median(penwlth, weights = wgt),
            Total = wtd.median(totwlth, weights = wgt),
            n = comma2(n())) %>%
  pivot_longer(c(types, "Total"), names_to = "type") %>%
  mutate(type = factor(type, levels = c(types, "Total")),
         x = fct_rev(hrpdvage9),
         y = roundGBP(value),
         label = stringi::stri_enc_toutf8(fmtGBP(y)),
         tooltip = stringi::stri_enc_toutf8(paste0("Household head aged ", 
                                                   x, "\nMedian ", 
                                                   tolower(type), " wealth: ", 
                                                   fmtGBP(y))))

charts1$chart_4 <- df %>%
  filter(type != "Total") %>%
  ggplot(aes(x = x, y = y, fill = type, 
             label = label, tooltip = tooltip, data_id = paste(x, type))) +
  geom_col_interactive(show.legend = FALSE) +
  geom_text(colour = SGgrey, nudge_y = 18000) +
  scale_fill_manual(values = col_types) + 
  coord_flip() +
  scale_y_continuous(labels = comma_format(prefix = "£"),
                     limits = c(0, 200000)) +
  facet_wrap("type", ncol = 1) +
  add_source()

tables1$table_4$median <- df %>%
  select(type, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Age = x) %>%
  pivot_wider(values_from = y, names_from = type)

tables1$table_4$sample <- df %>%
  select(type, x, n) %>%
  rename(Age = x) %>%
  pivot_wider(values_from = n, names_from = type)

# Save all ---------------------------------------------------------------------

saveRDS(charts1, "data/charts1.rds")
saveRDS(tables1, "data/tables1.rds")

rm(list = ls())
