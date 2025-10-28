# Packages ---------------------------------------------------------------
library(tidyverse)
library(marginaleffects)
library(rstanarm)
source("scripts/99-custom-functions.R")

# Data -------------------------------------------------------------------
kraje <- read_rds("data/kraje-krimi-population.rds")
kraje_sample <- read_rds("data//kraje-krimi.rds") #simulovana data!

# Odhad prevalence kriminality v krajich ---------------------------------

## Complete Pooling (kraje jsou zamenitelne)
m1 <- glm(crime_experience ~ 1, data = kraje_sample, family = binomial())
summary(m1)

plot_predictions(m1, condition = "region") +
  scale_y_continuous(limits = c(0, 1)) +
  geom_point(mapping = aes(x = region, y = prev), data = kraje, color = "red") +
  coord_flip() +
  labs(title = "Complete Pooling")

compute_errors(model = m1, title = "No Pooling")

## No Pooling (kraje jsou unikatni)
m2 <- glm(crime_experience ~ region, data = kraje_sample, family = binomial())
summary(m2)
predictions(m2, newdata = datagrid(region = unique))

plot_predictions(m2, condition = "region") +
  scale_y_continuous(limits = c(0, 1)) +
  geom_point(mapping = aes(x = region, y = prev), data = kraje, color = "red") +
  coord_flip() +
  labs(title = "No Pooling")

compute_errors(model = m2, title = "Complete Pooling")

## Partial Pooling (kraje jsou pribuzne) - Bayesiansky
m3 <- stan_glmer(
  crime_experience ~ (1 | region),
  data = kraje_sample,
  family = binomial()
)

summary(m3)

plot_predictions(m3, condition = "region") +
  scale_y_continuous(limits = c(0, 1)) +
  geom_point(mapping = aes(x = region, y = prev), data = kraje, color = "red") +
  coord_flip() +
  labs(title = "Partial Pooling")

compute_errors(model = m3, title = "Partial Pooling")

# Srovnani chyby odhadu --------------------------------------------------
bind_rows(
  compute_errors(model = m1, title = "No Pooling"),
  compute_errors(model = m2, title = "Complete Pooling"),
  compute_errors(model = m3, title = "Partial Pooling"),
)
