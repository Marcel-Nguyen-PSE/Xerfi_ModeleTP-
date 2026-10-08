library(dplyr)

d <- as.data.frame(read_excel(
  "base_tp_regressions_trimestrielle.xlsx",
  sheet = "Donnees_regression"
))

d <- d[order(d$annee, d$trimestre), ]

d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    tp_lag1 = lag(tp_volume_trimestriel, 1),
    tp_lag2 = lag(tp_volume_trimestriel, 2),

    marches_lag1 = lag(marches_tp_volume, 1),
    marches_lag2 = lag(marches_tp_volume, 2),

    terrassement_lag2 = lag(ica_terrassement, 2),
    routes_lag4 = lag(ica_routes, 4),
    rail_lag4 = lag(ica_voies_ferrees, 4),
    fluides_lag4 = lag(ica_reseaux_fluides, 4),
    elec_lag4 = lag(ica_reseaux_elec_telecom, 4),

    ouvrages_art_lag2 = lag(ica_ouvrages_art, 2),
    tunnels_lag2 = lag(ica_tunnels, 2),
    maritime_lag2 = lag(ica_maritime_fluvial, 2),
    forages_lag2 = lag(ica_forages_sondages, 2),

    swi_lag1 = lag(swi_trimestriel, 1),

    fbcf_apu_lag1 = lag(fbcf_apu, 1),
    fbcf_snf_lag1 = lag(fbcf_snf, 1),

    tp01_lag1 = lag(tp01_trimestriel, 1)
  )

candidates <- c(
  "marches_lag1",
  "marches_lag2",
  "terrassement_lag2",
  "routes_lag4",
  "rail_lag4",
  "fluides_lag4",
  "elec_lag4",
  "ouvrages_art_lag2",
  "tunnels_lag2",
  "maritime_lag2",
  "forages_lag2",
  "swi_lag1",
  "fbcf_apu_lag1",
  "fbcf_snf_lag1",
  "tp01_lag1"
)

train <- d |> filter(annee < 2022)
test  <- d |> filter(annee >= 2022)


results_A <- data.frame()

# Benchmark
m <- lm(
  tp_volume_trimestriel ~
    tp_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

pred <- predict(m, newdata = test)

results_A <- rbind(
  results_A,
  data.frame(
    variables = "AR(1)",
    n_variables = 0,
    RMSE = sqrt(mean(
      (test$tp_volume_trimestriel - pred)^2,
      na.rm = TRUE
    ))
  )
)


# ------------------------------------------------------------
# Models with 1 additional predictor
# ------------------------------------------------------------

for (v1 in candidates) {

  formula_model <- as.formula(
    paste(
      "tp_volume_trimestriel ~ tp_lag1 +",
      v1,
      "+ regime_fntp_2016 + covid_2020_t2"
    )
  )

  m <- lm(
    formula_model,
    data = train
  )

  pred <- predict(
    m,
    newdata = test
  )

  rmse <- sqrt(mean(
    (test$tp_volume_trimestriel - pred)^2,
    na.rm = TRUE
  ))

  results_A <- rbind(
    results_A,
    data.frame(
      variables = v1,
      n_variables = 1,
      RMSE = rmse
    )
  )
}


# ------------------------------------------------------------
# Models with 2 additional predictors
# ------------------------------------------------------------

comb2 <- combn(
  candidates,
  2
)

for (j in 1:ncol(comb2)) {

  v1 <- comb2[1, j]
  v2 <- comb2[2, j]

  formula_model <- as.formula(
    paste(
      "tp_volume_trimestriel ~ tp_lag1 +",
      v1, "+", v2,
      "+ regime_fntp_2016 + covid_2020_t2"
    )
  )

  m <- lm(
    formula_model,
    data = train
  )

  pred <- predict(
    m,
    newdata = test
  )

  rmse <- sqrt(mean(
    (test$tp_volume_trimestriel - pred)^2,
    na.rm = TRUE
  ))

  results_A <- rbind(
    results_A,
    data.frame(
      variables = paste(v1, "+", v2),
      n_variables = 2,
      RMSE = rmse
    )
  )
}


# ------------------------------------------------------------
# Models with 3 additional predictors
# ------------------------------------------------------------

comb3 <- combn(
  candidates,
  3
)

for (j in 1:ncol(comb3)) {

  v1 <- comb3[1, j]
  v2 <- comb3[2, j]
  v3 <- comb3[3, j]

  formula_model <- as.formula(
    paste(
      "tp_volume_trimestriel ~ tp_lag1 +",
      v1, "+", v2, "+", v3,
      "+ regime_fntp_2016 + covid_2020_t2"
    )
  )

  m <- lm(
    formula_model,
    data = train
  )

  pred <- predict(
    m,
    newdata = test
  )

  rmse <- sqrt(mean(
    (test$tp_volume_trimestriel - pred)^2,
    na.rm = TRUE
  ))

  results_A <- rbind(
    results_A,
    data.frame(
      variables = paste(v1, "+", v2, "+", v3),
      n_variables = 3,
      RMSE = rmse
    )
  )
}


# Best models when TP lag1 is available
results_A <- results_A |>
  arrange(RMSE)

head(results_A, 20)


# ============================================================
# 5. SCENARIO B:
# TP lag1 unavailable -> use TP lag2
# ============================================================

results_B <- data.frame()


# Benchmark
m <- lm(
  tp_volume_trimestriel ~
    tp_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

pred <- predict(
  m,
  newdata = test
)

results_B <- rbind(
  results_B,
  data.frame(
    variables = "TP lag2",
    n_variables = 0,
    RMSE = sqrt(mean(
      (test$tp_volume_trimestriel - pred)^2,
      na.rm = TRUE
    ))
  )
)


# ------------------------------------------------------------
# 1 additional predictor
# ------------------------------------------------------------

for (v1 in candidates) {

  formula_model <- as.formula(
    paste(
      "tp_volume_trimestriel ~ tp_lag2 +",
      v1,
      "+ regime_fntp_2016 + covid_2020_t2"
    )
  )

  m <- lm(
    formula_model,
    data = train
  )

  pred <- predict(
    m,
    newdata = test
  )

  rmse <- sqrt(mean(
    (test$tp_volume_trimestriel - pred)^2,
    na.rm = TRUE
  ))

  results_B <- rbind(
    results_B,
    data.frame(
      variables = v1,
      n_variables = 1,
      RMSE = rmse
    )
  )
}


# ------------------------------------------------------------
# 2 additional predictors
# ------------------------------------------------------------

for (j in 1:ncol(comb2)) {

  v1 <- comb2[1, j]
  v2 <- comb2[2, j]

  formula_model <- as.formula(
    paste(
      "tp_volume_trimestriel ~ tp_lag2 +",
      v1, "+", v2,
      "+ regime_fntp_2016 + covid_2020_t2"
    )
  )

  m <- lm(
    formula_model,
    data = train
  )

  pred <- predict(
    m,
    newdata = test
  )

  rmse <- sqrt(mean(
    (test$tp_volume_trimestriel - pred)^2,
    na.rm = TRUE
  ))

  results_B <- rbind(
    results_B,
    data.frame(
      variables = paste(v1, "+", v2),
      n_variables = 2,
      RMSE = rmse
    )
  )
}


# ------------------------------------------------------------
# 3 additional predictors
# ------------------------------------------------------------

for (j in 1:ncol(comb3)) {

  v1 <- comb3[1, j]
  v2 <- comb3[2, j]
  v3 <- comb3[3, j]

  formula_model <- as.formula(
    paste(
      "tp_volume_trimestriel ~ tp_lag2 +",
      v1, "+", v2, "+", v3,
      "+ regime_fntp_2016 + covid_2020_t2"
    )
  )

  m <- lm(
    formula_model,
    data = train
  )

  pred <- predict(
    m,
    newdata = test
  )

  rmse <- sqrt(mean(
    (test$tp_volume_trimestriel - pred)^2,
    na.rm = TRUE
  ))

  results_B <- rbind(
    results_B,
    data.frame(
      variables = paste(v1, "+", v2, "+", v3),
      n_variables = 3,
      RMSE = rmse
    )
  )
}


# Best models when TP lag1 is unavailable
results_B <- results_B |>
  arrange(RMSE)

head(results_B, 20)
