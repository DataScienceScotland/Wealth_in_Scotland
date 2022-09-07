
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

pers <- readRDS("data/tidy_person.rds") %>% 
  filter(gor == "Scotland",
         wave != "w5",
         
         # exclude W1 half sample where physical wealth q's weren't asked
         !(wave == "w1" & wgt_halfsample == 0))

tidyhbai <- readRDS("data/tidyhbai.rds") %>% filter(gvtregn == "Scotland")

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
               "All" = SGmix4[1])

charts2 <- list()
tables2 <- list()


# 2_1 Income and wealth dist ---------------------------------------------------

# HBAI income using last 2 years (note it's hhld-level to be consistent with wealth analysis)

hbai <- tidyhbai %>%
  filter(yearn >= 25) %>%
  select(s_oe_bhc, gs_newbu) %>%
  mutate(vingt = 1) %>%
  arrange(s_oe_bhc)

vgt = wtd.quantile(hbai$s_oe_bhc, 
                   weights = hbai$gs_newbu, 
                   probs = seq(0.02, 1, 0.02))

for (k in 1:dim(hbai)[1]) {
  for (i in 1:49) {
    if (hbai$s_oe_bhc[k] > vgt[i] & hbai$s_oe_bhc[k] <= vgt[i + 1])
    {hbai$vingt[k] <- i + 1}
  }
}

income_shares <- hbai %>%
  group_by(vingt) %>%
  summarise(inc = sum(s_oe_bhc*gs_newbu),
            n = n()) %>%
  mutate(share = inc/sum(inc)) %>%
  select(-inc) %>%
  mutate(type = "Income")

# Wealth shares

wealth_shares <- hhld %>% 
  filter(wavenum == max(wavenum)) %>%
  select(totwlth, wgt) %>%
  mutate(vingt = 1,
         totwlth = ifelse(totwlth < 0, 0, totwlth)) %>%
  arrange(totwlth)

vgt = wtd.quantile(wealth_shares$totwlth, 
                   weights = wealth_shares$wgt, 
                   probs = seq(0.02, 1, 0.02))

for (k in 1:dim(wealth_shares)[1]) {
  for (i in 1:49) {
    if (wealth_shares$totwlth[k] > vgt[i] & wealth_shares$totwlth[k] <= vgt[i + 1]) 
    {wealth_shares$vingt[k] <- i + 1}
  }
}

wealth_shares <- wealth_shares %>%
  group_by(vingt) %>%
  summarise(wlth = sum(totwlth*wgt),
            n = n()) %>%
  mutate(share = wlth/sum(wlth)) %>%
  select(-wlth) %>%
  mutate(type = "Wealth")

df <- rbind(income_shares, wealth_shares) %>%
  mutate(Vigintile = vingt,
         vingt = ifelse(vingt == 0, 1, 2*vingt),
         tooltip = paste0(type, " share: ", round2(share*100, 1), 
                          "% at percentiles ", vingt - 1, " to ", vingt),
         data_id = paste(vingt, type)) 

charts2$chart_1 <- df %>%
  ggplot(aes(x = vingt, y = share, fill = type, tooltip = tooltip, 
             data_id = data_id)) +
  geom_col_interactive(position = position_dodge(width = 1), width = 1) +
  scale_fill_manual(values = c(SGmix4[3], SGmix4[1])) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(1, 20, 40, 60, 80, 100)) +
  labs(caption = "Source: Wealth and Assets Survey and Family Resources Survey")

tables2$table_1$median <- df %>%
  select(Vigintile, share, type) %>%
  rename("Vigintile (2% band)" = Vigintile) %>%
  mutate(share = sprintf("%1.1f%%", round2(share*100, 1))) %>%
  pivot_wider(values_from = share, names_from = type)

tables2$table_1$sample <- df %>%
  group_by(type) %>%
  summarise(Group = "All",
            n = comma2(sum(n))) %>%
  pivot_wider(values_from = n, names_from = type)

# 2_2 Median wealth by decile --------------------------------------------------

deciles <- hhld %>%
  group_by(wavenum) %>%
  mutate(decile = analysistools::getdeciles(totwlth, weights = wgt)) %>% 
  select(wavenum, hhserial, decile)

df <- hhld %>%
  left_join(deciles, by = c("wavenum", "hhserial")) %>%
  group_by(wavenum, decile) %>%
  summarise(value = wtd.median(totwlth*infl, weights = wgt),
            age = wtd.median(hrpdvage, weights = wgt),
            n = comma2(n())) %>%
  mutate(x = factor(decile),
         y = roundGBP(value),
         period = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods),
         tooltip = paste0("Decile ", x, ": ", fmtGBP(y)),
         data_id = x) %>%
  ungroup()

charts2$chart_2 <- df %>%
  filter(wavenum == max(wavenum)) %>%
  ggplot(aes(x = x, y = y, tooltip = tooltip, data_id = data_id)) +
  geom_col_interactive(fill = SGblue) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  add_source()

tables2$table_2$median <- df %>%
  filter(wavenum == max(wavenum)) %>%
  select(period, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Period = period) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_2$age <- df %>%
  filter(wavenum == max(wavenum)) %>%
  select(period, x, age) %>%
  rename(Period = period) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_2$sample <- df %>%
  filter(wavenum == max(wavenum)) %>%
  select(period, x, n) %>%
  rename(Period = period) %>%
  pivot_wider(values_from = n, names_from = x)

# 2_3 Median wealth by decile over time ----------------------------------------

df <- hhld %>%
  left_join(deciles, 
            by = c("wavenum", "hhserial")) %>%
  group_by(wavenum, decile) %>%
  summarise(value = wtd.median(totwlth*infl, weights = wgt),
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
  summarise(value = wtd.median(totwlth*infl, weights = wgt),
            age = wtd.median(hrpdvage, weights = wgt),
            n = comma2(n())) %>%
  mutate(key = "All",
         y = roundGBP(value),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods, ordered = TRUE)) %>%
  select(x, y, age, n, key)

charts2$chart_3 <- linechart(df) +
  scale_color_manual(values = SGmix10) +
  add_labels(df) +
  scale_x_discrete(expand = expansion(add = c(2.5, 1.5)),
                   breaks = c(periods[1], periods[length(periods)])) +
  add_source()

tables2$table_3$median <- df %>%
  select(key, x, y) %>%
  rbind(total %>% select(key, x, y)) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Decile = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_3$age <- df %>%
  select(key, x, age) %>%
  rbind(total %>% select(key, x, age)) %>%
  rename(Decile = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_3$sample <- df %>%
  select(key, x, n) %>%
  rbind(total %>% select(key, x, n)) %>%
  rename(Decile = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 2_4 Age distribution by wealth decile ----------------------------------------

df <- hhld %>%
  left_join(deciles, by = c("wavenum", "hhserial")) %>%
  filter(wavenum == max(wavenum)) %>%
  group_by(decile, hrpdvage9) %>%
  summarise(value = sum(wgt),
            n = n()) %>%
  group_by(decile) %>%
  mutate(x = factor(decile, levels = seq(1, 10, 1)),
         key = hrpdvage9,
         y = value/sum(value),
         tooltip = paste0(percent2(y), " of decile ", decile, " households are aged ", key),
         data_id = paste(decile, key))

charts2$chart_4 <- df %>%
  ggplot(aes(x = x, y = y, fill = key, tooltip = tooltip, 
             data_id = data_id)) +
  geom_col_interactive(position = "fill", colour = "white") +
  scale_fill_manual(values = SGmix6) +
  scale_y_continuous(labels = percent_format()) +
  theme(legend.position = "right",
        legend.title = element_text()) +
  labs(fill = "Age") +
  add_source()

tables2$table_4$share <- df %>%
  select(decile, key, y) %>%
  mutate(y = percent2(y)) %>%
  rename(Age = key) %>%
  pivot_wider(values_from = y, names_from = decile, values_fill = list(y = "0%"))

tables2$table_4$sample <- df %>%
  select(decile, key, n) %>%
  group_by(decile) %>%
  summarise(n = sum(n, na.rm = TRUE)) %>%
  mutate(Group = "All",
         n = comma2(n)) %>%
  pivot_wider(values_from = n, names_from = decile) 

# 2_5 Gini ---------------------------------------------------------------------

df <- hhld %>%
  group_by(wavenum) %>% 
  summarise(value = get_gini(totwlth, weights = wgt),
            n = comma2(n())) %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = round2(value, 3),
         label = percent2(y),
         tooltip = paste0(label, " (", x, ")"),
         data_id = x,
         key = "All")

charts2$chart_5 <- linechart(df) +
  add_labels(df) +
  scale_y_continuous(labels = percent_format(1),
                     limits = c(0.5, 0.7))  +
  scale_color_manual(values = col_types) +
  add_source()

tables2$table_5$measure <- df %>%
  select(x, y) %>%
  mutate(y = percent2(y),
         Measure = "Gini") %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_5$sample <- df %>%
  select(x, n, key) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 2_6 Palma --------------------------------------------------------------------

df <- hhld %>%
  group_by(wavenum) %>% 
  get_decshares(var = "totwlth", weight = "wgt", inflator = "infl") %>%
  summarise(value = share[10]/sum(share[1:4])) %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = round2(value, 1),
         label = y,
         tooltip = paste0(label, " (", x, ")"),
         data_id = x,
         key = "All") 

charts2$chart_6 <- linechart(df) +
  add_labels(df) +
  scale_y_continuous(limits = c(4, 16))  +
  scale_color_manual(values = col_types) +
  add_source()

tables2$table_6$measure <- df %>%
  select(x, y) %>%
  mutate(Measure = "Palma") %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_6$sample <- hhld %>%
  group_by(wavenum) %>%
  summarise(n = comma2(n())) %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         Group = "All") %>%
  select(-wavenum) %>%
  pivot_wider(values_from = n, names_from = x)


# 2_7 Shares -------------------------------------------------------------------

shares <- hhld %>%
  group_by(wavenum) %>%
  mutate(dec9 = wtd.quantile(totwlth, probs = 0.9, weights = wgt),
         dec4 = wtd.quantile(totwlth, probs = 0.4, weights = wgt),
         dec = case_when(totwlth > dec9 ~ "Top 10%",
                         totwlth <= dec4 ~ "Bottom 40%",
                         TRUE ~ "Middle 50%")) %>%
  group_by(dec, add = TRUE) %>%
  summarise(share = sum(totwlth*wgt),
            n = comma2(n())) %>%
  group_by(wavenum) %>%
  mutate(value = share/sum(share),
         y = round2(value, 3)) %>%
  select(wavenum, value, y, dec) %>%
  ungroup()

top1 <- hhld %>%
  group_by(wavenum) %>%
  mutate(dec99 = wtd.quantile(totwlth, probs = 0.99, weights = wgt),
         top1 = ifelse(totwlth > dec99, 1, 0)) %>%
  group_by(top1, add = TRUE) %>%
  summarise(top1_sum = sum(totwlth*wgt),
            n = comma2(n())) %>%
  group_by(wavenum) %>%
  summarise(value = top1_sum[2]/sum(top1_sum)) %>%
  mutate(y = round2(value, 3),
         dec = "Top 1%")

df <- rbind(shares, top1) %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods, 
                    ordered = TRUE),
         dec = factor(dec, levels = c("Bottom 40%", "Middle 50%", "Top 10%", 
                                      "Top 1%"), ordered = TRUE),
         tooltip = paste0("The ", tolower(dec), " households had ", percent2(y), " of total wealth in ", x),
         data_id = paste(x, dec)) %>%
  arrange(dec)

charts2$chart_7 <- ggplot(df, aes(x = x, y = value, group = dec, fill = dec)) +
  geom_hline_interactive(yintercept = 0.4, colour = SGmix4[1], 
                         tooltip = "40% line", data_id = "hline") +
  geom_hline_interactive(yintercept = 0.5, colour = SGmix4[2], 
                         tooltip = "50% line", data_id = "hline") +
  geom_hline_interactive(yintercept = 0.1, colour = SGmix4[3], 
                         tooltip = "10% line", data_id = "hline") +
  geom_hline_interactive(yintercept = 0.01, colour = SGmix4[4], 
                         tooltip = "1% line", data_id = "hline") +
  geom_col_interactive(aes(tooltip = tooltip,
                           data_id = data_id),
                       position = "dodge") +
  scale_fill_manual(values = SGmix4) +
  scale_y_continuous(labels = percent_format(1),
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
  add_source()

tables2$table_7$measure <- df %>%
  select(x, y, dec) %>%
  mutate(y = percent2(y)) %>%
  rename("Income group" = dec) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_7$sample <- hhld %>%
  group_by(wavenum) %>%
  summarise(n = comma2(n())) %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         Group = "All") %>%
  select(-wavenum) %>%
  pivot_wider(values_from = n, names_from = x)

# 2_8 Gap ----------------------------------------------------------------------

df <- hhld %>%
  group_by(wavenum) %>%
  mutate(dec9 = wtd.quantile(totwlth, weights = wgt, probs = 0.9),
         dec4 = wtd.quantile(totwlth, weights = wgt, probs = 0.4),
         dec = case_when(totwlth <= dec4 ~ 4,
                         totwlth > dec9 ~ 10)) %>%
  group_by(dec, add = TRUE) %>%
  summarise(totwlth_median = wtd.median(totwlth, weights = wgt),
            infl = infl[1]) %>%
  filter(!is.na(dec)) %>%
  group_by(wavenum) %>%
  summarise(value = infl[1] * (totwlth_median[2] - totwlth_median[1])) %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label = comma(value, scale = 1E-6, accuracy = 0.1, prefix = "£", suffix = " million"),
         tooltip = fmtGBP(roundGBP(value)),
         data_id = x,
         key = "All")

charts2$chart_8 <- linechart(df) +
  add_labels(df) +
  scale_y_continuous(labels = comma_format(prefix = "£"),
                     limits = c(1E6, 2E6)) +
  scale_x_discrete(expand = c(0.2, 0.2),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_color_manual(values = col_types) +
  add_source()

tables2$table_8$measure <- df %>%
  select(x, y) %>%
  mutate(y = fmtGBP(y),
         Measure = "Wealth gap") %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_8$sample <- hhld %>%
  group_by(wavenum) %>%
  summarise(n = comma2(n())) %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         Group = "All") %>%
  select(-wavenum) %>%
  pivot_wider(values_from = n, names_from = x)


# 2_9 Gini by type --------------------------------------------------------

phy <- hhld %>%
  group_by(wavenum) %>%
  summarise(value = get_gini(phywlth, weights = ifelse(wave == "w1", wgt_halfsample, wgt)),
            n = n(),
            n_w1 = sum(wgt_halfsample > 0)) %>%
  mutate(key = "Physical",
         n = ifelse(wavenum == 1, n_w1, n),
         n = comma2(n)) %>%
  select(-n_w1)

# use full W1 sample for fin, pro, pen
fin <- hhld_full_w1 %>%
  group_by(wavenum) %>%
  summarise(value = get_gini(finwlth, weights = wgt),
            n = comma2(n())) %>%
  mutate(key = "Financial")

pro <- hhld_full_w1 %>%
  group_by(wavenum) %>%
  summarise(value = get_gini(prowlth, weights = wgt),
            n = comma2(n())) %>%
  mutate(key = "Property")

pen <- hhld_full_w1 %>%
  group_by(wavenum) %>%
  summarise(value = get_gini(penwlth, weights = wgt),
            n = comma2(n())) %>%
  mutate(key = "Pension")

tot <- hhld %>%
  group_by(wavenum) %>%
  summarise(value = get_gini(totwlth, weights = wgt),
            n = comma2(n())) %>%
  mutate(key = "Total")


df <- rbind(phy, fin, pro, pen, tot) %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = round2(value, 3),
         label = percent2(y),
         label = ifelse(x == min(x), paste0(key, ": ", label), label),
         tooltip = paste0(key, ": ", percent2(y), " (", x, ")"),
         data_id = paste0(x, key),
         key = factor(key, levels = c("Financial", 
                                      "Pension", 
                                      "Property", 
                                      "Physical", 
                                      "Total"), ordered = TRUE)) %>% 
  arrange(key)

charts2$chart_9 <- df %>% 
  filter(key != "Total") %>% 
  linechart() +
  add_labels(df) +
  scale_x_discrete(expand = expansion(mult = c(0.25, 0.1))) +
  scale_y_continuous(limits = c(0.3, 1))  +
  scale_color_manual(values = col_types) +
  add_source()

tables2$table_9$measure <- df %>%
  select(key, x, y)  %>%
  mutate(y = percent2(y)) %>%
  rename(Type = key) %>%
  pivot_wider(values_from = y, names_from = x) 

tables2$table_9$sample <- df %>%
  select(key, x, n, y) %>%
  select(-y) %>%
  rename(Type = key) %>%
  pivot_wider(values_from = n, names_from = x)

# ___Individual wealth by individual characteristics ---------------------------

# 2_10 Age ----------------------------------------------------------------------

df <- pers %>%
  mutate(key = dvage9,
         key = as.character(key)) %>%
  bind_rows(., pers %>% mutate(key = as.character("All"))) %>%
  filter(wavenum >= 3) %>%
  group_by(wavenum, key) %>%
  summarise(value = wtd.median(p_totwlth*infl, weights = wgt),
            age = wtd.median(dvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label =  case_when(x == min(x) ~ paste0(key, ": ", fmtGBP(y)),
                            x == max(x) ~ paste0(fmtGBP(y), " (", key, ")"),
                            TRUE ~ fmtGBP(y)),
         tooltip = paste0(paste0(key, ": ", fmtGBP(y)), " (", x, ")"),
         data_id = paste(x, key))


charts2$chart_10 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix7) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(1.5, 1.5)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 3E5)) +
  add_source()

tables2$table_10$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_10$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_10$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 2_11 Employment --------------------------------------------------------------

df <- pers %>%
  mutate(key = ecact,
         key = as.character(key)) %>%
  bind_rows(., pers %>% 
              filter(ecact != "Ineligible / missing") %>% 
              mutate(key = as.character("All"))) %>%
  filter(wavenum >= 5,
         isdep != 1,
         key != "Ineligible / missing") %>%
  group_by(wavenum, key) %>%
  summarise(value = wtd.median(p_totwlth*infl, weights = wgt),
            age = wtd.median(dvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(key = fct_reorder2(key, desc(wavenum), value),
         key = fct_relevel(key, "All", after = 10L),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label =  case_when(x == min(x) ~ paste0(key, ": ", fmtGBP(y)),
                            TRUE ~ fmtGBP(y)),
         tooltip = paste0(paste0(key, ": ", fmtGBP(y)), " (", x, ")"),
         data_id = paste(x, key)) %>%
  arrange(key)


charts2$chart_11 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix7) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2.5, 2)),
                   breaks = c(periods[5], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 3E5)) +
  add_source()

tables2$table_11$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_11$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_11$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)


# 2_12 Education ---------------------------------------------------------------

df <- pers %>%
  mutate(key = edlevel,
         key = as.character(key)) %>%
  bind_rows(., pers %>% 
              filter(edlevel != "DK/NA") %>% 
              mutate(key = as.character("All"))) %>%
  filter(wavenum >= 3,
         isdep != 1,
         key != "DK/NA") %>%
  group_by(wavenum, key) %>%
  summarise(value = wtd.median(p_totwlth*infl, weights = wgt),
            age = wtd.median(dvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(key = fct_reorder2(key, desc(wavenum), value),
         key = fct_relevel(key, "All", after = 10L),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label =  case_when(x == min(x) ~ paste0(key, ": ", fmtGBP(y)),
                            TRUE ~ fmtGBP(y)),
         tooltip = paste0(paste0(key, ": ", fmtGBP(y)), " (", x, ")"),
         data_id = paste(x, key)) %>%
  arrange(key)


charts2$chart_12 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix3) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 3E5)) +
  add_source()

tables2$table_12$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_12$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_12$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)



# 2_13 Marital -----------------------------------------------------------------

df <- pers %>%
  mutate(key = dvmrdf,
         key = as.character(key)) %>%
  bind_rows(., pers %>% mutate(key = as.character("All"))) %>%
  filter(wavenum >= 3,
         isdep != 1) %>%
  group_by(wavenum, key) %>%
  summarise(value = wtd.median(p_totwlth*infl, weights = wgt),
            age = wtd.median(dvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(key = fct_reorder2(key, desc(wavenum), value),
         key = fct_relevel(key, "All", after = 10L),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label =  case_when(x == min(x) ~ paste0(key, ": ", fmtGBP(y)),
                            TRUE ~ fmtGBP(y)),
         tooltip = paste0(paste0(key, ": ", fmtGBP(y)), " (", x, ")"),
         data_id = paste(x, key)) %>%
  arrange(key)

charts2$chart_13 <- df %>%
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix5) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1.6)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 3E5)) +
  add_source()

tables2$table_13$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_13$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_13$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 2_14 Sexual orientation ------------------------------------------------------

df <- pers %>%
  mutate(key = sid,
         key = as.character(key)) %>%
  bind_rows(., pers %>% 
              filter(sid != "DK/NA") %>% 
              mutate(key = as.character("All"))) %>%
  filter(wavenum >= 6,
         dvage >= 16,
         persprox == 1,
         key != "DK/NA") %>%
  group_by(wavenum, key) %>%
  summarise(value = wtd.median(p_totwlth*infl, weights = wgt),
            age = wtd.median(dvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(key = fct_reorder2(key, desc(wavenum), value),
         key = fct_relevel(key, "All", after = 10L),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label =  case_when(x == min(x) ~ paste0(key, ": ", fmtGBP(y)),
                            TRUE ~ fmtGBP(y)),
         tooltip = paste0(paste0(key, ": ", fmtGBP(y)), " (", x, ")"),
         data_id = paste(x, key)) %>%
  arrange(key)


charts2$chart_14 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 0.8)),
                   breaks = c(periods[5], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 3E5)) +
  add_source()

tables2$table_14$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_14$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_14$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 2_15 Religion ----------------------------------------------------------------

df <- pers %>%
  mutate(key = religion,
         key = as.character(key)) %>%
  bind_rows(., pers %>% 
              filter(religion != "DK/NA") %>% 
              mutate(key = as.character("All"))) %>%
  filter(wavenum >= 3,
         isdep != 1,
         persprox == 1,
         key != "DK/NA") %>%
  group_by(wavenum, key) %>%
  summarise(value = wtd.median(p_totwlth*infl, weights = wgt),
            age = wtd.median(dvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(key = fct_reorder2(key, desc(wavenum), value),
         key = fct_relevel(key, "All", after = 10L),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label =  case_when(x == min(x) ~ paste0(key, ": ", fmtGBP(y)),
                            TRUE ~ fmtGBP(y)),
         tooltip = paste0(paste0(key, ": ", fmtGBP(y)), " (", x, ")"),
         data_id = paste(x, key)) %>%
  arrange(key)


charts2$chart_15 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix3) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 3E5)) +
  add_source()

tables2$table_15$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_15$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_15$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 2_16 Ethnicity ---------------------------------------------------------------

df <- pers %>%
  mutate(key = ethnic,
         key = as.character(key)) %>%
  bind_rows(., pers %>% 
              filter(ethnic != "DK/NA") %>% 
              mutate(key = as.character("All"))) %>%
  filter(wavenum >= 3,
         isdep != 1,
         persprox == 1,
         key != "DK/NA") %>%
  group_by(wavenum, key) %>%
  summarise(value = wtd.median(p_totwlth*infl, weights = wgt),
            age = wtd.median(dvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(key = fct_reorder2(key, desc(wavenum), value),
         key = fct_relevel(key, "All", after = 10L),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label =  case_when(x == min(x) ~ paste0(key, ": ", fmtGBP(y)),
                            TRUE ~ fmtGBP(y)),
         tooltip = paste0(paste0(key, ": ", fmtGBP(y)), " (", x, ")"),
         data_id = paste(x, key)) %>%
  arrange(key)


charts2$chart_16 <- df %>%
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 3E5)) +
  add_source()

tables2$table_16$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_16$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_16$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)




# 2_17 Disability --------------------------------------------------------------

df <- pers %>%
  mutate(key = disabled,
         key = as.character(key)) %>%
  bind_rows(., pers %>% mutate(key = as.character("All"))) %>%
  filter(wavenum >= 3,
         isdep != 1) %>%
  group_by(wavenum, key) %>%
  summarise(value = wtd.median(p_totwlth*infl, weights = wgt),
            age = wtd.median(dvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(key = fct_reorder2(key, desc(wavenum), value),
         key = fct_relevel(key, "All", after = 10L),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label =  case_when(x == min(x) ~ paste0(key, ": ", fmtGBP(y)),
                            TRUE ~ fmtGBP(y)),
         tooltip = paste0(paste0(key, ": ", fmtGBP(y)), " (", x, ")"),
         data_id = paste(x, key)) %>%
  arrange(key)

charts2$chart_17 <- df %>%
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 3E5)) +
  add_source()

tables2$table_17$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_17$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_17$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 2_18 Sex ---------------------------------------------------------------------

df <- pers %>%
  mutate(key = sex,
         key = as.character(key)) %>%
  bind_rows(., pers %>% mutate(key = as.character("All"))) %>%
  filter(wavenum >= 3,
         isdep != 1) %>%
  group_by(wavenum, key) %>%
  summarise(value = wtd.median(p_totwlth*infl, weights = wgt),
            age = wtd.median(dvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(key = fct_reorder2(key, desc(wavenum), value),
         key = fct_relevel(key, "All", after = 10L),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label =  case_when(x == min(x) ~ paste0(key, ": ", fmtGBP(y)),
                            TRUE ~ fmtGBP(y)),
         tooltip = paste0(paste0(key, ": ", fmtGBP(y)), " (", x, ")"),
         data_id = paste(x, key)) %>%
  arrange(key)


charts2$chart_18 <- df %>%
  filter(key != "All") %>%
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1.2)),
                   breaks = c(periods[3], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 3E5)) +
  add_source()

tables2$table_18$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_18$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_18$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)




# ___Hhld wealth by hhld characteristics ---------------------------------------

# 2_19 Income quintile ---------------------------------------------------------

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
  bind_rows(., hhld %>% 
              filter(wavenum >= 5) %>% 
              mutate(key = as.character("All"))) %>%
  group_by(wavenum, key) %>%
  summarise(value = wtd.median(totwlth*infl, weights = wgt),
            age = wtd.median(hrpdvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(key = fct_reorder2(key, desc(wavenum), value),
         key = fct_relevel(key, "All", after = 10L),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label =  case_when(x == min(x) ~ paste0(key, ": ", fmtGBP(y)),
                            TRUE ~ fmtGBP(y)),
         tooltip = paste0(paste0(key, ": ", fmtGBP(y)), " (", x, ")"),
         data_id = paste(x, key))  %>%
  arrange(key)

charts2$chart_19 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix5) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(3.5, 1)),
                   breaks = c(periods[5], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 8.5E5)) +
  add_source()

tables2$table_19$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_19$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_19$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)


# 2_20 Household type ----------------------------------------------------------

df <- hhld %>%
  mutate(key = hholdtype,
         key = as.character(key)) %>%
  bind_rows(., hhld %>% mutate(key = as.character("All"))) %>%
  group_by(wavenum, key) %>%
  summarise(value = wtd.median(totwlth*infl, weights = wgt),
            age = wtd.median(hrpdvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(key = fct_reorder2(key, desc(wavenum), value),
         key = fct_relevel(key, "All", after = 10L),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label =  case_when(x == min(x) ~ paste0(key, ": ", fmtGBP(y)),
                            TRUE ~ fmtGBP(y)),
         tooltip = paste0(paste0(key, ": ", fmtGBP(y)), " (", x, ")"),
         data_id = paste(x, key))  %>%
  arrange(key)

charts2$chart_20 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix7) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(4, 1.5)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 8.5E5)) +
  add_source()

tables2$table_20$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_20$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_20$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 2_21 Tenure ------------------------------------------------------------------

df <- hhld %>%
  mutate(key = tenure,
         key = as.character(key)) %>%
  bind_rows(., hhld %>% 
              filter(tenure != "DK/NA") %>% 
              mutate(key = as.character("All"))) %>%
  filter(key != "DK/NA") %>%
  group_by(wavenum, key) %>%
  summarise(value = wtd.median(totwlth*infl, weights = wgt),
            age = wtd.median(hrpdvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(key = fct_reorder2(key, desc(wavenum), value),
         key = fct_relevel(key, "All", after = 10L),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label =  case_when(x == min(x) ~ paste0(key, ": ", fmtGBP(y)),
                            TRUE ~ fmtGBP(y)),
         tooltip = paste0(paste0(key, ": ", fmtGBP(y)), " (", x, ")"),
         data_id = paste(x, key))  %>%
  arrange(key)

charts2$chart_21 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix4) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(4.5, 1.5)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 6E5)) +
  add_source()

tables2$table_21$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_21$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_21$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)

# 2_22 Urban-rural -------------------------------------------------------------

df <- hhld %>%
  mutate(key = urindsc,
         key = as.character(key)) %>%
  bind_rows(., hhld %>% mutate(key = as.character("All"))) %>%
  filter(!is.na(urindsc)) %>%
  group_by(wavenum, key) %>%
  summarise(value = wtd.median(totwlth*infl, weights = wgt),
            age = wtd.median(hrpdvage, weights = wgt),
            n = comma2(n())) %>%
  ungroup() %>%
  mutate(key = fct_reorder2(key, desc(wavenum), value),
         key = fct_relevel(key, "All", after = 10L),
         x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                    ordered = TRUE),
         y = roundGBP(value),
         label =  case_when(x == min(x) ~ paste0(key, ": ", fmtGBP(y)),
                            TRUE ~ fmtGBP(y)),
         tooltip = paste0(paste0(key, ": ", fmtGBP(y)), " (", x, ")"),
         data_id = paste(x, key))  %>%
  arrange(key)

charts2$chart_22 <- df %>% 
  filter(key != "All") %>% 
  linechart() +
  scale_color_manual(values = SGmix2) +
  add_labels(filter(df, key != "All")) +
  scale_x_discrete(expand = expansion(add = c(2, 1.5)),
                   breaks = c(periods[1], periods[length(periods)])) +
  scale_y_continuous(limits = c(0, 6E5)) +
  add_source()

tables2$table_22$median <- df %>%
  select(key, x, y) %>%
  mutate(y = fmtGBP(y)) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = y, names_from = x)

tables2$table_22$age <- df %>%
  select(key, x, age) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = age, names_from = x)

tables2$table_22$sample <- df %>%
  select(key, x, n) %>%
  rename(Group = key) %>%
  pivot_wider(values_from = n, names_from = x)




# Save all ---------------------------------------------------------------------

saveRDS(charts2, "data/charts2.rds")
saveRDS(tables2, "data/tables2.rds")

rm(list = ls())

