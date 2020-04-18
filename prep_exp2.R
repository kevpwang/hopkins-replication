library(foreign)
library(janitor)
library(tidyverse)

exp2 <- read.dta("raw-data/accentsexp2.dta") %>% 
  clean_names()

# Same processing as exp1 but slightly different questions.

exp2_cleaned <- exp2 %>% 
  
  # Drop NAs for partyid & ideo, as original table does.
  
  drop_na(partyid7, ideology) %>% 
  
  # Remove respondents who left too early or did not take the survey in one sitting.

  separate(ppincimp, c("income", "dis1", "dis2"), " ")  %>% 
  select(-dis1, -dis2) %>% 
  mutate(income = recode(income, "Less" = "$2500"),
         income = parse_number(income) / 1000) %>% 
  
  # Recode Q7, Q8, Q9, Q10, Q11, Q12, & Q14 using numerical scale where
  # higher numbers represent more inclusive attitudes.
  
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
  mutate(q11 = recode(q11, "Strongly agree" = 1,
                      "Somewhat agree" = 2,
                      "Somewhat disagree" = 3,
                      "Strongly disagree" = 4,
                      "Refused" = -1)
  ) %>% 
  mutate(q12 = recode(q12, "Strongly agree" = 4,
                      "Somewhat agree" = 3,
                      "Somewhat disagree" = 2,
                      "Strongly disagree" = 1,
                      "Refused" = -1)
  ) %>% 
  mutate(q14 = recode(q14, "Every Day" = 5,
                      "At Least Once a Week" = 4,
                      "1-3 Times Each Month" = 3,
                      "Less than Once a Month" = 2,
                      "Never or Almost Never" = 1,
                      "Refused" = -1)
  ) %>% 
  
  # Same educ. recoding as exp1.
  
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
  
  # Derive treatment vars from assigned_video.
  
  mutate(accented = case_when(assigned_video == "Video J" ~ 1,
                              TRUE ~ 0),
         spanish = case_when(assigned_video == "Video H" ~ 1,
                             TRUE ~ 0),
         english = case_when(assigned_video == "Video I" ~ 1,
                             TRUE ~ 0)
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
  mutate(democratic = case_when(grepl("Not Strong Rep", partyid7) ~ 2,
                                grepl("Not Strong Dem", partyid7) ~ 6,
                                grepl("Leans Rep", partyid7) ~ 3,
                                grepl("Leans Dem", partyid7) ~ 5,
                                grepl("Independent", partyid7) ~ 4,
                                grepl("Rep", partyid7) ~ 1,
                                grepl("Dem", partyid7) ~ 7)
         ) %>% 
  mutate(republican = -1 * democratic + 8) %>% 
  mutate(black = case_when(grepl("Black", ppethm) ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(other = case_when(grepl("Other", ppethm) ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(male = recode(ppgender, "Male" = 1,
                       "Female" = 0)) %>% 
  select(q7, q8, q9, q10, q11, q12, q14, accented, english, spanish, income, years_educ, online, employed, conservative,
         democratic, republican, black, other, male, ppage, ppethm)

write_rds(exp2_cleaned, "clean-data/exp2_cleaned.rds")
