# Packages ---------------------------------------------------------------
library(tidyverse)
library(parameters)
library(rstanarm)
library(marginaleffects)
library(datawizard)

source("scripts//99-custom-functions.R")

# Data -------------------------------------------------------------------
# 1982 High School and Beyond Survey, USA
schools <- read_rds("data/school_math.rds")

# Příprava dat -----------------------------------------------------------
schools <- demean(schools, ~ses, by = ~school_id)
# demean() je zkratka pro:
# schools |>
#   mutate(ses_between = mean(ses),
#          ses_within = ses - ses_between,
#         .by = school_id)

# Between vs Within efekt SES na matematickou gramotnost -----------------

## Random Intercept model - skoly mohou rozdilny promerny SES, ale vztah
## Mezi individualnim SES a znalostmi je napric skolami stejny
# options(mc.cores = 4)
# m4 <- stan_glmer(
#   math ~ ses_within + ses_between + sex + minority + (1 | school_id),
#   data = schools
# )

m4 <- read_rds("models/schools-random-intercept.rds")

plot_sample_schools(m4) # vztah mezi individualnim SES a individualnim skore na 5 nahodnych skolach
parameters(m4) |> plot()

## Random Slopes model - skaly maji rozdilny prumerny SES a vztah
## mezi individualnim SES a znalostmi se muze lisit skola od skoly
# options(mc.cores = 4)
# m5 <- stan_glmer(
#   math ~ ses_within + ses_between + sex + minority + (1 + ses_within | school_id),
#   data = schools
# )

m5 <- read_rds("models/schools-random-slopes.rds")

plot_sample_schools(m5) # vztah mezi individualnim SES a individualnim skore na 5 nahodnych skolach
parameters(m5) |> plot()
