library(foreign)
library(janitor)
library(tidyverse)

exp1 <- read.dta("raw-data/accentsexp1.dta") %>% 
  clean_names()

exp1_cleaned <- exp1 %>% 
  
  # Since published data only gives income ranges, take lower bound as income value.
  # Based on min/max in table, parse <$5000 as 2500 & >$175000 as 250000.
  # Convert scale to thousands.
  
  separate(ppincimp, c("income", "dis1", "dis2"), " ")  %>% 
  select(-dis1, -dis2) %>% 
  mutate(income = recode(income, "Less" = "$2500"),
         income = parse_number(income) / 1000
         ) %>% 
  
  # Recode Q7, Q8, Q9, Q10, Q12, & Q13 using numerical scale.
  
  mutate(q7 = recode(q7, "Strongly support" = 4,
                     "Somewhat support" = 3,
                     "Somewhat oppose" = 2,
                     "Strongly oppose" = 1,
                     "Refused" = -1)
         ) %>%
  
  mutate(q8 = recode(q8, "Increased a lot" = 5,
                     "Increased a little" = 4,
                     "Left the same" = 3,
                     "Decreased a little" = 2,
                     "Decreased a lot" = 1, 
                     "Refused" = -1)
         ) %>% 
  mutate(q9 = recode(q9, "Strongly agree" = 1,
                     "Somewhat agree" = 2,
                     "Somewhat disagree" = 3,
                     "Strongly disagree" = 4,
                     "Refused" = -1)
         ) %>% 
  mutate(q10 = recode(q10, "Strongly agree" = 4,
                      "Somewhat agree" = 3,
                      "Somewhat disagree" = 2,
                      "Strongly disagree" = 1,
                      "Refused" = -1)
         ) %>% 
  mutate(q12a = recode(q12a, "Most people can be trusted" = 1,
                       "You can't be too careful in dealing with people" = 0,
                       "Refused" = -1)
         ) %>% 
  mutate(q13 = recode(q13, "Not at all Likely" = 4,
                      "Somewhat Likely" = 3,
                      "Very Likely" = 2,
                      "Extremely Likely" = 1,
                      "Refused" = -1)
         ) %>% 
  
  # Data only gives educ attainment, not years. Recode into 
  # years & make some inferences about vague cases. Remember
  # to go broad -> narrow w/case_when().
  
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
                                grepl("No form", ppeduc) ~ 0)
         ) %>% 
  
  # Derive regression variables from condition
  
  mutate(dark = case_when(grepl("Dark", condition) ~ 1,
                          TRUE ~ 0),
         accented = case_when(grepl("accented", condition) ~ 1,
                              TRUE ~ 0),
         video = case_when(grepl("Control", condition) ~ 0,
                           TRUE ~ 1),
         spanish = case_when(grepl("Spanish", condition) ~ 1,
                             TRUE ~ 0),
         english = case_when(grepl("clear English", condition) ~ 1,
                             TRUE ~ 0)
         ) %>% 
  
  # Recode ppnet & ppwork to numerical.
  # Recode ideo7 & repub to numerical scale.
  
  mutate(online = recode(ppnet, "Yes" = 1,
                         "No" = 0)) %>% 
  mutate(employed = case_when(grepl("Not working", ppwork) ~ 0,
                              TRUE ~ 1)) %>% 
  mutate(conservative = case_when(grepl("Extremely lib", ideo7) ~ 1,
                                  grepl("Slightly lib", ideo7) ~ 3,
                                  grepl("Extremely con", ideo7) ~ 7,
                                  grepl("Slightly con", ideo7) ~ 5,
                                  grepl("Moderate", ideo7) ~ 4,
                                  grepl("Lib", ideo7) ~ 2,
                                  grepl("Con", ideo7) ~ 6,
                                  TRUE ~ -1)
         ) %>% 
  filter(conservative != -1) %>% 
  mutate(republican = case_when(grepl("Not Strong Rep", party7) ~ 6,
                                grepl("Not Strong Dem", party7) ~ 2,
                                grepl("Leans Rep", party7) ~ 5,
                                grepl("Leans Dem", party7) ~ 3,
                                grepl("Independent", party7) ~ 4,
                                grepl("Rep", party7) ~ 7,
                                grepl("Dem", party7) ~ 1)
         ) %>% 
  
  # Recode black, other race, & male to numerical.
  
  mutate(black = case_when(grepl("Black", ppethm) ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(other = case_when(grepl("Other", ppethm) ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(male = recode(ppgender, "Male" = 1,
                       "Female" = 0)) %>% 
  select(q7, q8, q9, q10, q12a, q13, dark, accented, video, spanish, english, income, years_educ, online, employed, conservative,
         republican, black, other, male, ppage, ppethm)

write_rds(exp1_cleaned, "clean-data/exp1_cleaned.rds")
