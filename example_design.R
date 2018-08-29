# install.packages("DeclareDesign")

library(DeclareDesign)

# Model ---------------------------------------------------------
population <- declare_population(
   N = 2100,
   noise = rnorm(N),
   age = sample(18:75, N, replace = TRUE),
   young = as.numeric(age < 40))

potential_outcomes <- declare_potential_outcomes(
  Y_Z_0 = noise,
  Y_Z_1 = Y_Z_0 + 0.25 + 0.1 * young)

# Inquiry -------------------------------------------------------
estimand_ate  <- declare_estimand(
  ATE = mean(Y_Z_1 - Y_Z_0))
estimand_cate_diff <- declare_estimand(
  CATE_diff = mean(Y_Z_1[young == 1] - Y_Z_0[young == 1]) -
              mean(Y_Z_1[young == 0] - Y_Z_0[young == 0]))

# Data Strategy -------------------------------------------------
sampling <- declare_sampling(n = 250)
assignment <- declare_assignment(m = 125)

# Answer Strategy -----------------------------------------------
estimator_ate <- declare_estimator(Y ~ Z,
                                   estimand = estimand_ate,
                                   model = difference_in_means,
                                   label = "estimator_ate")

estimator_cate <- declare_estimator(Y ~ Z * young,
                                    estimand = estimand_cate_diff,
                                    model = lm_robust,
                                    term = "Z:young",
                                    label = "estimator_cate")

# Design ---------------------------------------------------------
design <-
  population +
  potential_outcomes +
  estimand_ate +
  estimand_cate_diff +
  sampling +
  assignment +
  declare_reveal() +
  estimator_ate +
  estimator_cate

# Diagnosands ----------------------------------------------------
diagnosands <- declare_diagnosands(
  select = c(bias, power, mean_estimate)
)

diagnosis <- diagnose_design(
  design, diagnosands = diagnosands,
  sims = 100, bootstrap_sims = FALSE
)

diagnosis

