library(tidyverse)
library(dplyr)

df <- load(url("https://github.com/codymg/other_projects/blob/master/data/ucdp-prio-acd-181.Rdata?raw=true")

glimpse(df)

df$type_of_conflict <- as.character(df$type_of_conflict)

dat <- df %>%
        select(type_of_conflict, year) %>%
        group_by(type_of_conflict, year) %>%
        mutate(num_conflict = n()) %>%
        distinct(year, .keep_all = TRUE) %>%
        spread(year, num_conflict) %>%
        replace(., is.na(.), 0) %>%
        ungroup() %>%
        mutate(., conflict_type = 
                 recode(type_of_conflict, 
                        "1" = "Colonial or Imperial Conflicts",
                        "2" = "Conflicts Between States", 
                        "3" = "Civil Conflicts", 
                        "4" = "Civil Conflicts with Foreign State Intervention")) %>%
        select(., conflict_type, 2:73)
           
plot_dat <- df %>%
  select(type_of_conflict, year) %>%
  group_by(type_of_conflict, year) %>%
  mutate(num_conflict = n()) %>%
  distinct(year, .keep_all = TRUE) %>%
  replace(., is.na(.), 0) %>%
  ungroup() %>%
  mutate(., conflict_type = 
           recode(type_of_conflict, 
                  "1" = "Colonial or Imperial Conflicts",
                  "2" = "Conflicts Between States", 
                  "3" = "Civil Conflicts", 
                  "4" = "Civil Conflicts with Foreign State Intervention")) %>%
  select(year, conflict_type, num_conflict)

missing_years <- data.frame(year = c(1947, 1947, 1949, 1956, 1975), #filling these in with 0 to get rid of white area space in plot
                               conflict_type = c("Conflicts Between States", 
                                                 "Civil Conflicts with Foreign State Intervention",
                                                 "Civil Conflicts with Foreign State Intervention",
                                                 "Civil Conflicts with Foreign State Intervention",
                                                 "Colonial or Imperial Conflicts"), 
                               num_conflict = c(0,0,0,0,0))

plot_dat <- bind_rows(plot_dat, missing_years)


plot_dat %>%
  ggplot(aes(x = year, y = num_conflict, fill = conflict_type)) +
  geom_area(position = "stack") +
  scale_fill_manual(name = "Conflict Type", 
                    label = c("Civil Conflicts", 
                              "Civil Conflicts with\nForeign State Intervention", 
                              "Colonial or Imperial Conflicts", 
                              "Conflicts Between States"), 
                    values = c("yellow", "darkred", "darkgreen", "darkblue")) +
  theme_bw() +
  labs(title = "State-Based Conflicts Since 1946",
       subtitle = "UCDP/PRIO Armed Conflict Data",
       y = "Number of Conflicts\n (25+ Battle Deaths)",
       x = "Year")
