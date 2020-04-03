library(foreign)
library(janitor)
library(tidyverse)

exp2 <- read.dta("raw-data/accentsexp2.dta") %>% 
  clean_names()

exp2_cleaned <- exp2 %>% 
  
  # Drop NAs for partyid & ideo, as original table does.
  
  drop_na(partyid7, ideology) %>% 
  separate(ppincimp, c("income", "dis1", "dis2"), " ")  %>% 
  select(-dis1, -dis2) %>% 
  mutate(income = recode(income, "Less" = "$2500"),
         income = parse_number(income) / 1000) %>% 
  mutate(years_educ = case_when(grepl("HIGH SCHOOL GRADUATE", ppeduc) ~ 12,
                                grepl("Some college", ppeduc) ~ 14,
                                grepl("Professional", ppeduc) ~ 20,
                                grepl("Bachelor", ppeduc) ~ 16,
                                grepl("Master", ppeduc) ~ 17,
                                grepl("12th grade", ppeduc) ~ 12,
                                grepl("Assoc", ppeduc) ~ 14,
                                grepl("11th", ppeduc) ~ 11,
                                grepl("7th", ppeduc) ~ 7.5,
                                grepl("9th", ppeduc) ~ 9,
                                grepl("10th", ppeduc) ~ 10,
                                grepl("No form", ppeduc) ~ 0,
                                grepl("5th", ppeduc) ~ 5.5)
  ) %>%
  mutate(online = recode(ppnet, "Yes" = 1,
                         "No" = 0)) %>% 
  mutate(employed = case_when(grepl("Not working", ppwork) ~ 0,
                              TRUE ~ 1)) %>% 
  mutate(conservative = case_when(grepl("Extremely lib", ideology) ~ 1,
                                  grepl("Slightly lib", ideology) ~ 3,
                                  grepl("Extremely con", ideology) ~ 7,
                                  grepl("Slightly con", ideology) ~ 5,
                                  grepl("Moderate", ideology) ~ 4,
                                  grepl("Lib", ideology) ~ 2,
                                  grepl("Con", ideology) ~ 6,
                                  TRUE ~ -1)
  ) %>% 
  filter(conservative != -1) %>% 
  mutate(republican = case_when(grepl("Not Strong Rep", partyid7) ~ 6,
                                grepl("Not Strong Dem", partyid7) ~ 2,
                                grepl("Leans Rep", partyid7) ~ 5,
                                grepl("Leans Dem", partyid7) ~ 3,
                                grepl("Independent", partyid7) ~ 4,
                                grepl("Rep", partyid7) ~ 7,
                                grepl("Dem", partyid7) ~ 1)
  ) %>% 
  
  # Recode black & other race to numerical.
  
  mutate(black = case_when(grepl("Black", ppethm) ~ 1,
                           TRUE ~ 0)
  ) %>% 
  mutate(other = case_when(grepl("Other", ppethm) ~ 1,
                           TRUE ~ 0)
  ) %>% 
  select(income, years_educ, online, employed, conservative,
         republican, black, other, ppage, ppethm)

write_rds(exp2_cleaned, "clean-data/exp2_cleaned.rds")
