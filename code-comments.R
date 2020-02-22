# There is no published code associated with the paper, presumably because 
# most of the statistical procedures are fairly straightforward. In lieu of comments
# on actual code, I outline the statistical tests used in the paper and discuss how they 
# might be replicated in R.

library(foreign)
library(tidyverse)

##### AUGUST 2010 SURVEY #####

# Read in the data from a Stata file format (.dta).

accents_1 <- read.dta("raw-data/accentsexp1.dta")

# After viewing video, path to citizenship question administered (`Q7`). Answers range
# from "strongly oppose" (1) to "strongly support" (4).

# Word form appears in accents_1; should recode() to numerical.
# Treatment appears as single line, e.g. "Light-skinned image, Spanish language". Use 
# separate() & pivot this column, i.e. create new columns w/each value as
# treatment & binary-coded values.

# Linear model with `Q7` as response var and skin tone, accented ENG, fluent ESP
# treatment with video, yrs of educ, conservatism, partisanship, race (white/black), 
# and gender (M/F) as explanatory var. This model will also give mean & SD of 
# distribution of `Q7`.
# Assuming data is properly wrangled above, lm() will suffice for this. "Benchmark"
# is light skin & fluent ENG.

# Two-sided p-value calculations for each explanatory var using ANOVA. Looks like
# this can be done with aov().

# Figure 2 is a variant of dot-and-whisker displaying selected coefficients and
# their errors. Use ggplot() & geom_boxplot() for this.

# Calculate mean diff between accented & fluent ENG, controlling for skin tone. Determine
# 95% interval.

# Considering effect of accented ENG on Q1-5. Since probability of significant
# finding increases w/no. o/dependent vars, Hopkins follows "free step-down
# resampling method", which presents both two-sided p-values & two-sided
# p-values corrected for six individual hypothesis tests between accented &
# fluent ENG. Learn how to do this in R.

# For each question, linear model w/same treatment & identity indicators 
# as first model. Same dot-and-whisker plots produced for language & skin
# tone coefficients.
# Calculate mean & SD for each dependent value (answer value 1-4).

# Re-do linear models above, differentiating bet. respondents in/out of
# labor force.

# Mechanical Turk results do not appear to be published? Look around more
# and email Hopkins. Calculate p-values for differences in answers to
# immigrant's presumed education level, difficulty of ENG, effort at ENG,
# whether immigrant is typical.

##### JANUARY 2011 SURVEY #####

# Read in data from same format as previous.

accents_2 <- read.dta("raw-data/accentsexp2.dta")

# No skin tone treatment or no-video control group.
# Calculate accent perception for each language treatment to
# check that fluent ENG not misunderstood as interpreter.
# Eliminate low-quality responses from sample according to footnote 110.
# Same regression model and p-value calculations as previous survey.

# Construct "composite index" of numerical answers to six other
# immig. Qs (incl. two new Qs since previous survey). Is this simply
# mean/median answer?

#Calculate 95% interval for mean diff. between accented & fluent ENG
# as previously done.