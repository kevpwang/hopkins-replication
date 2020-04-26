library(gt)
library(broom)
library(stargazer)
library(ggpubr)
library(sjPlot)
library(janitor)
library(rstanarm)
library(tidyverse)

#### Set directory to main repo ####

setwd(".")

#### Read cleaned data ####

exp1_cleaned <- read_rds("clean-data/exp1_cleaned.rds") %>% 
  clean_names()
exp2_cleaned <- read_rds("clean-data/exp2_cleaned.rds") %>% 
  clean_names()

#### Table 1 ####

# Regression analysis for Q7 path to citizenship
# Hopkins excludes Hispanics; sensible to exclude 'refused'

exp1_q7 <- exp1_cleaned %>% 
  filter(ppethm != "Hispanic") %>% 
  filter(q7 != -1)

q7_model <- lm(q7 ~ dark + accented + spanish + no_video + years_educ + conservative + republican + 
                 black + male, data = exp1_q7)

# Reg table with stargazer 
# header = FALSE to get rid of stargazer tag

stargazer(q7_model,
          title = "OLS Regression of Support for a Pathway to Naturalization on Indicators of Treatment
          Group Status and Other Independent Variables",
          digits = 3,
          header = FALSE)

#### Figure 2 ####

# Pull coefs from regression

iv_list <- c("accented", "spanish", "no_video")
parameters <- tibble(condition = c("Accented English", "Fluent Spanish", "No Video (Control)"),
                     coef = map_dbl(iv_list, ~filter(tidy(q7_model), term == .x) %>% pull(estimate)),
                     sd = map_dbl(iv_list, ~filter(tidy(q7_model), term == .x) %>% pull(std.error))
)

# Plot effects for relevant groups
# 68% & 95% confints with geom_errorbar()

ggplot(parameters, aes(x = condition, y = coef)) +
  geom_col(alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymin = coef - sd, ymax = coef + sd), size = 1, width = 0) +
  geom_errorbar(aes(ymin = coef - 2 * sd, ymax = coef + 2 * sd), width = 0) +
  scale_y_continuous(limits = c(-0.4, 0.4)) +
  labs(
    title = "Support Pathway to Naturalization",
    subtitle = "Subjects who heard broken English display significantly more pro-immigrant attitudes",
    x = "Treatment",
    y = "Difference from Fluent English"
  )

#### Figure 3 ####

# Six models: 7, 8 (legal imm), 9 (generic threat), 10 (strengthens), 12a (trust),
# 13 (job threat).
# Separately filter for 'refused' for each Q.

exp1_q8 <- exp1_cleaned %>% 
  filter(ppethm != "Hispanic") %>% 
  filter(q8 != -1)
q8_model <- lm(q8 ~ dark + accented + spanish + no_video + years_educ + conservative + republican + 
                       black + male, data = exp1_q8)

exp1_q9 <- exp1_cleaned %>% 
  filter(ppethm != "Hispanic") %>% 
  filter(q9 != -1)
q9_model <- lm(q9 ~ dark + accented + spanish + no_video + years_educ + conservative + republican + 
                       black + male, data = exp1_q9)

exp1_q10 <- exp1_cleaned %>% 
  filter(ppethm != "Hispanic") %>% 
  filter(q10 != -1)
q10_model <- lm(q10 ~ dark + accented + spanish + no_video + years_educ + conservative + republican + 
                        black + male, data = exp1_q10)

# Supposed to be logistic (in Appendix), but coefs in Fig 3 are from linreg

exp1_q12a <- exp1_cleaned %>% 
  filter(ppethm != "Hispanic") %>% 
  drop_na(q12a) %>% 
  filter(q12a != -1)
q12a_model <- lm(q12a ~ dark + accented + spanish + no_video + years_educ + conservative + republican + 
                         black + male, data = exp1_q12a)

exp1_q13 <- exp1_cleaned %>% 
  filter(ppethm != "Hispanic") %>% 
  filter(q13 != -1)
q13_model <- lm(q13 ~ dark + accented + spanish + no_video + years_educ + conservative + republican + 
                        black + male, data = exp1_q13)

# Make each graph & combine with ggarrange()
# Same procedure as Fig 2

iv_list <- c("accented", "spanish", "no_video", "dark")
parameters <- tibble(condition = c("Accent", "Span.", "No Video", "Dark"),
                     coef = map_dbl(iv_list, ~filter(tidy(q7_model), term == .x) %>% pull(estimate)),
                     sd = map_dbl(iv_list, ~filter(tidy(q7_model), term == .x) %>% pull(std.error))
                     ) %>%
  mutate(condition = factor(condition, levels = condition))

q7_plot <- ggplot(parameters, aes(x = condition, y = coef)) +
  geom_col(alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymin = coef - sd, ymax = coef + sd), size = 1, width = 0) +
  geom_errorbar(aes(ymin = coef - 2 * sd, ymax = coef + 2 * sd), width = 0) +
  scale_y_continuous(limits = c(-0.4, 0.4)) +
  labs(
    title = "Support Pathway",
    x = "Treatment",
    y = "Difference from LS Fluent English"
  ) +
  theme(plot.title = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 5)
        )

parameters <- tibble(condition = c("Accent", "Span.", "No Video", "Dark"),
                     coef = map_dbl(iv_list, ~filter(tidy(q8_model), term == .x) %>% pull(estimate)),
                     sd = map_dbl(iv_list, ~filter(tidy(q8_model), term == .x) %>% pull(std.error))
                     ) %>% 
  mutate(condition = factor(condition, levels = condition))

q8_plot <- ggplot(parameters, aes(x = condition, y = coef)) +
  geom_col(alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymin = coef - sd, ymax = coef + sd), size = 1, width = 0) +
  geom_errorbar(aes(ymin = coef - 2 * sd, ymax = coef + 2 * sd), width = 0) +
  scale_y_continuous(limits = c(-0.4, 0.4)) +
  labs(
    title = "Support Increased Immigration",
    x = "Treatment",
    y = "Difference from LS Fluent English"
    ) +
  theme(plot.title = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 5)
        )

parameters <- tibble(condition = c("Accent", "Span.", "No Video", "Dark"),
                     coef = map_dbl(iv_list, ~filter(tidy(q9_model), term == .x) %>% pull(estimate)),
                     sd = map_dbl(iv_list, ~filter(tidy(q9_model), term == .x) %>% pull(std.error))
                     ) %>% 
  mutate(condition = factor(condition, levels = condition))

q9_plot <- ggplot(parameters, aes(x = condition, y = coef)) +
  geom_col(alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymin = coef - sd, ymax = coef + sd), size = 1, width = 0) +
  geom_errorbar(aes(ymin = coef - 2 * sd, ymax = coef + 2 * sd), width = 0) +
  scale_y_continuous(limits = c(-0.4, 0.3), breaks = c(-0.4, -0.2, 0.1, 0.2, 0.3)) +
  labs(
    title = "Don't Feel Threatened",
    x = "Treatment",
    y = "Difference from LS Fluent English"
    ) +
  theme(plot.title = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 5)
        )

parameters <- tibble(condition = c("Accent", "Span.", "No Video", "Dark"),
                     coef = map_dbl(iv_list, ~filter(tidy(q10_model), term == .x) %>% pull(estimate)),
                     sd = map_dbl(iv_list, ~filter(tidy(q10_model), term == .x) %>% pull(std.error))
                     ) %>% 
  mutate(condition = factor(condition, levels = condition))

q10_plot <- ggplot(parameters, aes(x = condition, y = coef)) +
  geom_col(alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymin = coef - sd, ymax = coef + sd), size = 1, width = 0) +
  geom_errorbar(aes(ymin = coef - 2 * sd, ymax = coef + 2 * sd), width = 0) +
  scale_y_continuous(limits = c(-0.3, 0.3), breaks = c(-0.3, -0.1, 0.1, 0.3)) +
  labs(
    title = "Strengthen American Society",
    x = "Treatment",
    y = "Difference from LS Fluent English"
    ) +
  theme(plot.title = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 5)
        )

parameters <- tibble(condition = c("Accent", "Span.", "No Video", "Dark"),
                     coef = map_dbl(iv_list, ~filter(tidy(q12a_model), term == .x) %>% pull(estimate)),
                     sd = map_dbl(iv_list, ~filter(tidy(q12a_model), term == .x) %>% pull(std.error))
                     ) %>% 
  mutate(condition = factor(condition, levels = condition))

q12a_plot <- ggplot(parameters, aes(x = condition, y = coef)) +
  geom_col(alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymin = coef - sd, ymax = coef + sd), size = 1, width = 0) +
  geom_errorbar(aes(ymin = coef - 2 * sd, ymax = coef + 2 * sd), width = 0) +
  scale_y_continuous(limits = c(-0.3, 0.3), breaks = c(-0.3, -0.1, 0.1, 0.3)) +
  labs(
    title = "Social Trust",
    x = "Treatment",
    y = "Difference from LS Fluent English"
    ) +
  theme(plot.title = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 5)
        )

parameters <- tibble(condition = c("Accent", "Span.", "No Video", "Dark"),
                     coef = map_dbl(iv_list, ~filter(tidy(q13_model), term == .x) %>% pull(estimate)),
                     sd = map_dbl(iv_list, ~filter(tidy(q13_model), term == .x) %>% pull(std.error))
                     ) %>% 
  mutate(condition = factor(condition, levels = condition))

q13_plot <- ggplot(parameters, aes(x = condition, y = coef)) +
  geom_col(alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymin = coef - sd, ymax = coef + sd), size = 1, width = 0) +
  geom_errorbar(aes(ymin = coef - 2 * sd, ymax = coef + 2 * sd), width = 0) +
  scale_y_continuous(limits = c(-0.3, 0.3), breaks = c(-0.3, -0.1, 0.1, 0.3)) +
  labs(
    title = "Don't Take Jobs",
    x = "Treatment",
    y = "Difference from LS Fluent English"
    ) +
  theme(plot.title = element_text(size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 5)
        )

ggarrange(q7_plot, q12a_plot, q9_plot, q10_plot, q13_plot, q8_plot,
          ncol = 3, nrow = 2)

#### Figure 4 ####

# Data from second survey, same path to citizenship question
# Could not replicate index of 6 Qs bc. no method specified for creating index

exp2_q7 <- exp2_cleaned %>% 
  filter(ppethm != "Hispanic") %>% 
  filter(q7 != -1)

q7_model <- lm(q7 ~ accented + spanish + years_educ + conservative + democratic + 
                       male + black, data = exp2_q7)

# Same procedure as previous

iv_list <- c("accented", '(Intercept)', "spanish")
parameters <- tibble(condition = c("Accent", "English", "Spanish"),
                     coef = map_dbl(iv_list, ~filter(tidy(q7_model), term == .x) %>% pull(estimate)),
                     sd = map_dbl(iv_list, ~filter(tidy(q7_model), term == .x) %>% pull(std.error))
) %>% 
  mutate(condition = factor(condition, levels = condition))

# Set English coef to 0 bc. it is baseline

parameters[2,2] <- 0

ggplot(parameters, aes(x = condition, y = coef)) +
  geom_col(alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymin = coef - sd, ymax = coef + sd), size = 1, width = 0) +
  geom_errorbar(aes(ymin = coef - 2 * sd, ymax = coef + 2 * sd), width = 0) +
  scale_y_continuous(limits = c(-0.2, 0.3), breaks = c(-0.2, -0.1, 0.1, 0.2, 0.3)) +
  labs(
    title = "Support Pathway",
    subtitle = paste("mean = ", round(mean(exp2_q7$q7), 1), 
                     ", SD = ", round(sd(exp2_q7$q7), 1), sep = ""),
    x = "Treatment",
    y = "Difference from control (English)"
  )

#### App C, Table 1 ####

# Summary statistics table

exp1_tab1 <- exp1_cleaned %>% 
  filter(ppethm != "Hispanic") %>% 
  select(income, years_educ, online, employed, conservative,
         republican, black, other, ppage)

exp2_tab1 <- exp2_cleaned %>% 
  filter(ppethm != "Hispanic") %>% 
  select(income, years_educ, online, employed, conservative,
         republican, black, other, ppage)

table <- tibble(labels = c("Income", "Years of Education", "Online", "Employed",
                           "Conservative", "Republica", "Black", "Other", "Age"),
                mean1 = colMeans(exp1_tab1),
                sd1 = apply(exp1_tab1, 2, sd),
                min1 = apply(exp1_tab1, 2, min),
                max1 = apply(exp1_tab1, 2, max),
                mean2 = colMeans(exp2_tab1),
                sd2 = apply(exp2_tab1, 2, sd),
                min2 = apply(exp2_tab1, 2, min),
                max2 = apply(exp2_tab1, 2, max)
)
table %>% 
  gt() %>% 
  tab_spanner(
    label = "August 2010 Experiment",
    columns = vars(mean1, sd1, min1, max1)
  ) %>% 
  tab_spanner(
    label = "January 2011 Experiment",
    columns = vars(mean2, sd2, min2, max2)
  ) %>% 
  fmt_number(
    decimals = 2,
    columns = vars(mean1, sd1, min1, max1, 
                   mean2, sd2, min2, max2)
  ) %>% 
  cols_label(
    labels = "",
    mean1 = "Mean",
    sd1 = "SD",
    min1 = "Min.",
    max1 = "Max.",
    mean2 = "Mean",
    sd2 = "SD",
    min2 = "Min.",
    max2 = "Max.",
  )

#### App C, Table 2 ####

# Regression table for Q7, 8, 9 using IVs in App C Table 2.

q7_model <- lm(q7 ~ dark + accented + spanish + no_video + years_educ + conservative + republican + 
                 black + male, data = exp1_q7)

tab_model(q7_model, q8_model, q9_model,
          show.se = TRUE,
          show.ci = FALSE,
          show.r2 = FALSE,
          digits = 3,
          dv.labels = c("Pathway", "Legal Immig.", "Generic Threat"),
          pred.labels = c("Intercept", "Accented", "English", "Spanish", "Dark Skin",
                          "Education", "Conservative Ideo.", "Republican ID",
                          "Black", "Male"))

#### App C, Table 3 ####

# Regression table for Q10, 12a, 13
# Q12a model in table is logistic, but coefs in Fig 3 are linreg

q12a_model <- glm(q12a ~ accented + english + spanish + dark + years_educ + conservative + republican + 
                         black + male, family = "binomial", data = exp1_q12a)

tab_model(q10_model, q12a_model, q13_model,
          show.se = TRUE,
          show.ci = FALSE,
          show.r2 = FALSE,
          transform = NULL,
          digits = 3,
          dv.labels = c("Strengthen America", "Social Trust", "Job Threat"),
          pred.labels = c("Intercept", "Accented", "English", "Spanish", "Dark Skin",
                          "Education", "Conservative Ideo.", "Republican ID",
                          "Black", "Male"))

#### App C, Table 4 ####

q7_model <- lm(q7 ~ accented + spanish + years_educ + conservative + democratic + 
                       male + black, data = exp2_q7)

tab_model(q7_model,
          show.se = TRUE,
          show.ci = FALSE,
          show.r2 = FALSE,
          digits = 3,
          dv.labels = "Pathway",
          pred.labels = c("Intercept", "Accented", "Spanish", "Education", "Conservative Ideo.", 
                          "Democratic ID", "Male", "Black"))