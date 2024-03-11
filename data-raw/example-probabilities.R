library(tidyverse)
library(measr)
library(here)

exm_dat <- read_rds(here("data-raw", "example-data.rds"))

msr_dat <- exm_dat$data |>
  pivot_wider(names_from = item_id, values_from = score)

model <- measr_dcm(msr_dat, qmatrix = exm_dat$q_matrix, resp_id = "resp_id",
                   type = "lcdm", attribute_structure = "unconstrained",
                   method = "optim", backend = "rstan")

probs <- predict(model) |>
  pluck("attribute_probabilities") |>
  pivot_wider(names_from = attribute, values_from = probability)


dcm_probs <- list(
  att1 = list(estimate = probs$att1,
              truth = exm_dat$resp_profiles$att1),
  att2 = list(estimate = probs$att2,
              truth = exm_dat$resp_profiles$att2),
  att3 = list(estimate = probs$att3,
              truth = exm_dat$resp_profiles$att3)
)


usethis::use_data(dcm_probs, overwrite = TRUE)
