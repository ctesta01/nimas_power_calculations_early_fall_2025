

simulate_data <- function(n_obs = 100L, t_max = 3L,
                          effect_of_A_on_L1 = -1,
                          effect_of_A_on_L2 = -1,
                          effect_of_A_on_Y = -22) {

  # 1) Build the DAG
  dag <- DAG.empty() +

    # Baseline covariates at t = 0
    node("L1", t = 0, distr = "rbern", prob = 0.3) +  # comorbidity1 at baseline
    node("L2", t = 0, distr = "rbern", prob = 0.4) +  # comorbidity2 at baseline

    # Strata for Subgroup-Specific Effects
    node("V", t = 0, distr = "rbern", prob = 0.5)  # sex

  # Add time-varying covariates
  for (t in 1:(t_max-1)) {

    # add the time-varying covariates
    dag <- add.nodes(dag, node(
      name = "L1",
      t = t,
      distr = "rbern",
      prob =
        plogis(5 * L1[t-1] - 2.5 + # center of {0, 5} is 2.5
               L2[t-1] - 0.5 +     # center of {0, 1} is 0.5
                 ifelse(t > 1,      # effect of A on L1 for times t > 1
                   effect_of_A_on_L1 * A[t-1],
                   0
                 )
               )
    ))

    dag <- add.nodes(dag, node(
      name = "L2",
      t = t,
      distr = "rbern",
      prob = plogis(L1[t-1] - 0.5 + # center of {0, 1} is 0.5
                    6 * L2[t-1] - 3 + # center of {0, 6} is 3
                    ifelse(t > 1,    # effect of A on L2 for times t > 1
                        effect_of_A_on_L2 * A[t-1],
                        0)
      )))

    # add time-varying treatment/exposure
    dag <- add.nodes(dag, node(
      name = "A",
      t = t,
      distr = "rpois",
      lambda =  L1[t] * .5 +
        L2[t] * .75 + # prior covariates affect treatment
        ifelse(t > 1, A[t - 1], 0) # continuation of treatment
      ))
  }

  dag <- add.nodes(dag, node(
    name = "Y",
    t = t_max,
    distr = "rbern",
    prob = plogis(
      10 * L1[t_max-1] +
      12 * L2[t_max-1] +
      effect_of_A_on_Y * A[t_max-1]
      # sum(sapply(
      #   1:(t_max-1), function(t) {
      #   effect_of_A_on_Y[t] * A[t]
      # }))
    )))

  dag_set <- set.DAG(dag, verbose = FALSE)
  dat <- sim(dag_set, n = n_obs)

  return(dat)
}


# notes to self:
# V doesn't do anything yet ...
# not sure how to quantify the minimum detectable effect of A on Y
# ... risk difference?
# .... is there a standardized effect size like Cohen's D or similar to use?
#
