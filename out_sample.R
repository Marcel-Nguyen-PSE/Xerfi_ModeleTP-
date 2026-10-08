library(readxl)

d <- as.data.frame(read_excel(
  "base_tp_regressions_trimestrielle.xlsx",
  sheet = "Donnees_regression"
))

d <- d[order(d$annee, d$trimestre), ]

# Common sample for all four models
variables <- c(
  "tp_volume_trimestriel",
  "tp_volume_lag1",
  "ica_terrassement",
  "ica_routes",
  "ica_voies_ferrees",
  "ica_reseaux_fluides",
  "ica_reseaux_elec_telecom",
  "swi_trimestriel",
  "fbcf_apu",
  "fbcf_snf",
  "regime_fntp_2016",
  "covid_2020_t2",
  "trimestre"
)

z <- d[complete.cases(d[, variables]), ]

# 1. Persistence + seasonal and structural controls
m1 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    regime_fntp_2016 +
    covid_2020_t2 +
    factor(trimestre),
  data = z
)

# 2. Add sector activity
m2 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement +
    ica_routes +
    ica_voies_ferrees +
    ica_reseaux_fluides +
    ica_reseaux_elec_telecom +
    regime_fntp_2016 +
    covid_2020_t2 +
    factor(trimestre),
  data = z
)

# 3. Add soil moisture
m3 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement +
    ica_routes +
    ica_voies_ferrees +
    ica_reseaux_fluides +
    ica_reseaux_elec_telecom +
    swi_trimestriel +
    regime_fntp_2016 +
    covid_2020_t2 +
    factor(trimestre),
  data = z
)

# 4. Add public and corporate investment
m4 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement +
    ica_routes +
    ica_voies_ferrees +
    ica_reseaux_fluides +
    ica_reseaux_elec_telecom +
    swi_trimestriel +
    fbcf_apu +
    fbcf_snf +
    regime_fntp_2016 +
    covid_2020_t2 +
    factor(trimestre),
  data = z
)

# Detailed results
summary(m1)
summary(m2)
summary(m3)
summary(m4)

# Compare fit
comparison <- data.frame(
  model = c("m1_persistence", "m2_sectors", "m3_weather", "m4_investment"),
  observations = c(nobs(m1), nobs(m2), nobs(m3), nobs(m4)),
  adjusted_R2 = c(
    summary(m1)$adj.r.squared,
    summary(m2)$adj.r.squared,
    summary(m3)$adj.r.squared,
    summary(m4)$adj.r.squared
  ),
  AIC = c(AIC(m1), AIC(m2), AIC(m3), AIC(m4)),
  BIC = c(BIC(m1), BIC(m2), BIC(m3), BIC(m4))
)

comparison[order(comparison$BIC), ]

# Do the additional variables improve fit jointly?
anova(m1, m2)
anova(m2, m3)
anova(m3, m4)

# Persistence coefficient and confidence interval
coef(m3)["tp_volume_lag1"]
confint(m3, "tp_volume_lag1")

# Diagnostic plots for m3
par(mfrow = c(2, 2))
plot(m3)
par(mfrow = c(1, 1))

# Remaining quarterly residual autocorrelation
acf(
  residuals(m3),
  lag.max = 12,
  main = "Residual autocorrelation: m3"
)

# Best model according to AIC
best_model <- m2

# Actual and fitted values on the estimation sample
results <- data.frame(
  year = z$annee,
  quarter = z$trimestre,
  actual = z$tp_volume_trimestriel,
  predicted = as.numeric(fitted(best_model))
)

results <- results[order(results$year, results$quarter), ]

# Quarterly time axis
results$time <- results$year + (results$quarter - 1) / 4

# Actual versus fitted volume
plot(
  results$time,
  results$actual,
  type = "l",
  col = "black",
  lwd = 2,
  ylim = range(c(results$actual, results$predicted)),
  xlab = "Year",
  ylab = "TP volume index",
  main = "Actual vs fitted TP volume — m2"
)

lines(
  results$time,
  results$predicted,
  col = "blue",
  lwd = 2,
  lty = 2
)

legend(
  "topleft",
  legend = c("Actual", "Fitted"),
  col = c("black", "blue"),
  lwd = 2,
  lty = c(1, 2),
  bty = "n"
)

# In-sample prediction errors
results$error <- results$actual - results$predicted

RMSE <- sqrt(mean(results$error^2))
MAE <- mean(abs(results$error))

data.frame(RMSE = RMSE, MAE = MAE)

# View actual and fitted values
results[, c("year", "quarter", "actual", "predicted", "error")]


# Start from the complete quarterly dataset
d <- as.data.frame(d)
d <- d[order(d$annee, d$trimestre), ]

# Identify the previous calendar quarter
quarter_id <- 4 * d$annee + d$trimestre
previous <- match(quarter_id - 1, quarter_id)

# Lag the sector variables to avoid using current-quarter activity
d$terrassement_lag1 <- d$ica_terrassement[previous]
d$routes_lag1 <- d$ica_routes[previous]
d$fer_lag1 <- d$ica_voies_ferrees[previous]
d$fluides_lag1 <- d$ica_reseaux_fluides[previous]
d$elec_lag1 <- d$ica_reseaux_elec_telecom[previous]

variables <- c(
  "tp_volume_trimestriel", "tp_volume_lag1",
  "terrassement_lag1", "routes_lag1", "fer_lag1",
  "fluides_lag1", "elec_lag1",
  "regime_fntp_2016", "covid_2020_t2", "trimestre"
)

p <- d[complete.cases(d[, variables]), ]

train <- p[p$annee <= 2021, ]
test <- p[p$annee >= 2022 & p$annee <= 2025, ]

# Benchmark: persistence with seasonal and structural controls
m1_train <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    regime_fntp_2016 +
    covid_2020_t2 +
    factor(trimestre),
  data = train
)

# Forecasting version of m2: sector variables are lagged
m2_train <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    terrassement_lag1 +
    routes_lag1 +
    fer_lag1 +
    fluides_lag1 +
    elec_lag1 +
    regime_fntp_2016 +
    covid_2020_t2 +
    factor(trimestre),
  data = train
)

# Out-of-sample predictions
test$pred_persistence <- test$tp_volume_lag1
test$pred_m1 <- predict(m1_train, newdata = test)
test$pred_m2 <- predict(m2_train, newdata = test)

# Prediction errors
error_persistence <- test$tp_volume_trimestriel - test$pred_persistence
error_m1 <- test$tp_volume_trimestriel - test$pred_m1
error_m2 <- test$tp_volume_trimestriel - test$pred_m2

# Lower RMSE and MAE are better
comparison <- data.frame(
  model = c("Persistence", "m1", "m2_lagged_sectors"),
  RMSE = c(
    sqrt(mean(error_persistence^2)),
    sqrt(mean(error_m1^2)),
    sqrt(mean(error_m2^2))
  ),
  MAE = c(
    mean(abs(error_persistence)),
    mean(abs(error_m1)),
    mean(abs(error_m2))
  )
)

comparison[order(comparison$RMSE), ]

# Plot actual versus predicted values
time <- test$annee + (test$trimestre - 1) / 4

matplot(
  time,
  cbind(
    test$tp_volume_trimestriel,
    test$pred_persistence,
    test$pred_m1,
    test$pred_m2
  ),
  type = "l",
  col = c("black", "grey60", "blue", "red"),
  lty = c(1, 3, 2, 2),
  lwd = c(2, 1, 2, 2),
  xlab = "Year",
  ylab = "TP volume index",
  main = "Out-of-sample predictions: 2022–2025"
)

legend(
  "topleft",
  legend = c("Actual", "Persistence", "m1", "m2: lagged sectors"),
  col = c("black", "grey60", "blue", "red"),
  lty = c(1, 3, 2, 2),
  lwd = c(2, 1, 2, 2),
  bty = "n"
)

# Inspect individual predictions
test[, c(
  "annee", "trimestre", "tp_volume_trimestriel",
  "pred_persistence", "pred_m1", "pred_m2"
)]


# Baseline: persistence + controls
m_base <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    regime_fntp_2016 +
    covid_2020_t2 +
    factor(trimestre),
  data = train
)

# Baseline + earthworks
m_terrassement <- update(
  m_base, . ~ . + terrassement_lag1
)

# Baseline + roads
m_routes <- update(
  m_base, . ~ . + routes_lag1
)

# Baseline + railways
m_fer <- update(
  m_base, . ~ . + fer_lag1
)

# Baseline + fluid networks
m_fluides <- update(
  m_base, . ~ . + fluides_lag1
)

# Baseline + electricity / telecom networks
m_elec <- update(
  m_base, . ~ . + elec_lag1
)

# Out-of-sample predictions
test$pred_base <- predict(m_base, newdata = test)
test$pred_terrassement <- predict(m_terrassement, newdata = test)
test$pred_routes <- predict(m_routes, newdata = test)
test$pred_fer <- predict(m_fer, newdata = test)
test$pred_fluides <- predict(m_fluides, newdata = test)
test$pred_elec <- predict(m_elec, newdata = test)

# Prediction errors
e_base <- test$tp_volume_trimestriel - test$pred_base
e_terrassement <- test$tp_volume_trimestriel - test$pred_terrassement
e_routes <- test$tp_volume_trimestriel - test$pred_routes
e_fer <- test$tp_volume_trimestriel - test$pred_fer
e_fluides <- test$tp_volume_trimestriel - test$pred_fluides
e_elec <- test$tp_volume_trimestriel - test$pred_elec

# Compare forecast performance
comparison <- data.frame(
  model = c(
    "Baseline",
    "Baseline + earthworks",
    "Baseline + roads",
    "Baseline + railways",
    "Baseline + fluid networks",
    "Baseline + electricity/telecom"
  ),
  RMSE = c(
    sqrt(mean(e_base^2)),
    sqrt(mean(e_terrassement^2)),
    sqrt(mean(e_routes^2)),
    sqrt(mean(e_fer^2)),
    sqrt(mean(e_fluides^2)),
    sqrt(mean(e_elec^2))
  ),
  MAE = c(
    mean(abs(e_base)),
    mean(abs(e_terrassement)),
    mean(abs(e_routes)),
    mean(abs(e_fer)),
    mean(abs(e_fluides)),
    mean(abs(e_elec))
  )
)

# Positive values mean improvement over the baseline
comparison$RMSE_gain_pct <-
  100 * (comparison$RMSE[1] - comparison$RMSE) / comparison$RMSE[1]

comparison[order(comparison$RMSE), ]

# Plot RMSE: lower is better
barplot(
  comparison$RMSE,
  names.arg = c("Baseline", "Earthworks", "Roads",
                "Railways", "Fluids", "Elec/telecom"),
  col = c("grey60", rep("steelblue", 5)),
  ylab = "Out-of-sample RMSE",
  main = "Does adding one sector improve prediction?",
  cex.names = 0.8
)

abline(h = comparison$RMSE[1], col = "red", lty = 2)

# Inspect individual model coefficients
summary(m_terrassement)
summary(m_routes)
summary(m_fer)
summary(m_fluides)
summary(m_elec)




library(dplyr)

# Sort the data chronologically first
d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    ica_terrassement_lag1 = lag(ica_terrassement, 1),
    ica_routes_lag1 = lag(ica_routes, 1),
    ica_voies_ferrees_lag1 = lag(ica_voies_ferrees, 1),
    ica_reseaux_fluides_lag1 = lag(ica_reseaux_fluides, 1),
    ica_reseaux_elec_telecom_lag1 = lag(ica_reseaux_elec_telecom, 1)
  )

# Forecast model: use last quarter's sector indicators
m2_lag <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement_lag1 +
    ica_routes_lag1 +
    ica_voies_ferrees_lag1 +
    ica_reseaux_fluides_lag1 +
    ica_reseaux_elec_telecom_lag1 +
    regime_fntp_2016 +
    covid_2020_t2 +
    factor(trimestre),
  data = d
)

summary(m2_lag)



library(dplyr)

# --------------------------------------------------
# 1. Sort data and create lagged ICA variables
# --------------------------------------------------

d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    ica_terrassement_lag1 = lag(ica_terrassement, 1),
    ica_routes_lag1 = lag(ica_routes, 1),
    ica_voies_ferrees_lag1 = lag(ica_voies_ferrees, 1),
    ica_reseaux_fluides_lag1 = lag(ica_reseaux_fluides, 1),
    ica_reseaux_elec_telecom_lag1 = lag(ica_reseaux_elec_telecom, 1)
  )


# --------------------------------------------------
# 2. Train / test split
# --------------------------------------------------

train <- d |>
  filter(annee < 2022)

test <- d |>
  filter(annee >= 2022)


# --------------------------------------------------
# 3. AR(1) benchmark
# --------------------------------------------------

m_ar <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    regime_fntp_2016 +
    covid_2020_t2 +
    factor(trimestre),
  data = train
)


# --------------------------------------------------
# 4. AR(1) + lagged sector variables
# --------------------------------------------------

m_sector <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement_lag1 +
    ica_routes_lag1 +
    ica_voies_ferrees_lag1 +
    ica_reseaux_fluides_lag1 +
    ica_reseaux_elec_telecom_lag1 +
    regime_fntp_2016 +
    covid_2020_t2 +
    factor(trimestre),
  data = train
)


# --------------------------------------------------
# 5. Out-of-sample predictions
# --------------------------------------------------

test$pred_ar <- predict(
  m_ar,
  newdata = test
)

test$pred_sector <- predict(
  m_sector,
  newdata = test
)


# --------------------------------------------------
# 6. Forecast accuracy
# --------------------------------------------------

rmse_ar <- sqrt(
  mean(
    (test$tp_volume_trimestriel - test$pred_ar)^2,
    na.rm = TRUE
  )
)

rmse_sector <- sqrt(
  mean(
    (test$tp_volume_trimestriel - test$pred_sector)^2,
    na.rm = TRUE
  )
)

mae_ar <- mean(
  abs(
    test$tp_volume_trimestriel - test$pred_ar
  ),
  na.rm = TRUE
)

mae_sector <- mean(
  abs(
    test$tp_volume_trimestriel - test$pred_sector
  ),
  na.rm = TRUE
)

comparison <- data.frame(
  model = c("AR(1)", "AR(1) + sector ICA"),
  RMSE = c(rmse_ar, rmse_sector),
  MAE = c(mae_ar, mae_sector)
)

print(comparison)


# --------------------------------------------------
# 7. Time variable for graph
# --------------------------------------------------

test$time <- test$annee + (test$trimestre - 1) / 4


# --------------------------------------------------
# 8. Out-of-sample prediction graph
# --------------------------------------------------

plot(
  test$time,
  test$tp_volume_trimestriel,
  type = "l",
  col = "black",
  lwd = 2,
  ylim = range(
    c(
      test$tp_volume_trimestriel,
      test$pred_ar,
      test$pred_sector
    ),
    na.rm = TRUE
  ),
  xlab = "Year",
  ylab = "TP volume index",
  main = "Out-of-sample forecasts: 2022-2025"
)

lines(
  test$time,
  test$pred_ar,
  col = "blue",
  lwd = 2,
  lty = 2
)

lines(
  test$time,
  test$pred_sector,
  col = "red",
  lwd = 2,
  lty = 2
)

legend(
  "topleft",
  legend = c(
    "Actual",
    "AR(1)",
    "AR(1) + lagged sectors"
  ),
  col = c(
    "black",
    "blue",
    "red"
  ),
  lty = c(1, 2, 2),
  lwd = 2,
  bty = "n"
)


# Benchmark
m_ar <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# AR + roads
m_route <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_routes_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# AR + earthworks
m_terr <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# AR + rail
m_rail <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_voies_ferrees_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# AR + water networks
m_eau <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_reseaux_fluides_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# AR + electricity / telecom
m_elec <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_reseaux_elec_telecom_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# --------------------------------------------------
# 1. Benchmark AR(1)
# --------------------------------------------------

m_ar <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 2. AR(1) + roads
# --------------------------------------------------

m_route <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_routes_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 3. AR(1) + earthworks
# --------------------------------------------------

m_terr <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 4. AR(1) + rail
# --------------------------------------------------

m_rail <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_voies_ferrees_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 5. AR(1) + water / fluid networks
# --------------------------------------------------

m_eau <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_reseaux_fluides_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 6. AR(1) + electricity / telecom
# --------------------------------------------------

m_elec <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_reseaux_elec_telecom_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 7. Out-of-sample predictions
# --------------------------------------------------

test$pred_ar <- predict(m_ar, newdata = test)
test$pred_route <- predict(m_route, newdata = test)
test$pred_terr <- predict(m_terr, newdata = test)
test$pred_rail <- predict(m_rail, newdata = test)
test$pred_eau <- predict(m_eau, newdata = test)
test$pred_elec <- predict(m_elec, newdata = test)


# --------------------------------------------------
# 8. Compare predictive performance
# --------------------------------------------------

comparison <- data.frame(
  model = c(
    "AR(1)",
    "AR + roads",
    "AR + earthworks",
    "AR + rail",
    "AR + water",
    "AR + electricity"
  ),

  RMSE = c(
    sqrt(mean((test$tp_volume_trimestriel - test$pred_ar)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_route)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_terr)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_rail)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_eau)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_elec)^2, na.rm = TRUE))
  ),

  MAE = c(
    mean(abs(test$tp_volume_trimestriel - test$pred_ar), na.rm = TRUE),
    mean(abs(test$tp_volume_trimestriel - test$pred_route), na.rm = TRUE),
    mean(abs(test$tp_volume_trimestriel - test$pred_terr), na.rm = TRUE),
    mean(abs(test$tp_volume_trimestriel - test$pred_rail), na.rm = TRUE),
    mean(abs(test$tp_volume_trimestriel - test$pred_eau), na.rm = TRUE),
    mean(abs(test$tp_volume_trimestriel - test$pred_elec), na.rm = TRUE)
  )
)

comparison <- comparison[order(comparison$RMSE), ]

print(comparison)


# --------------------------------------------------
# 9. Summaries
# --------------------------------------------------

summary(m_ar)
summary(m_route)
summary(m_terr)
summary(m_rail)
summary(m_eau)
summary(m_elec)






library(dplyr)

# --------------------------------------------------
# 1. Create ICA lags
# --------------------------------------------------

d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    ica_routes_lag1 = lag(ica_routes, 1),
    ica_routes_lag2 = lag(ica_routes, 2),
    ica_routes_lag4 = lag(ica_routes, 4),

    ica_terrassement_lag1 = lag(ica_terrassement, 1),
    ica_terrassement_lag2 = lag(ica_terrassement, 2),
    ica_terrassement_lag4 = lag(ica_terrassement, 4),

    ica_voies_ferrees_lag1 = lag(ica_voies_ferrees, 1),
    ica_voies_ferrees_lag2 = lag(ica_voies_ferrees, 2),
    ica_voies_ferrees_lag4 = lag(ica_voies_ferrees, 4),

    ica_reseaux_fluides_lag1 = lag(ica_reseaux_fluides, 1),
    ica_reseaux_fluides_lag2 = lag(ica_reseaux_fluides, 2),
    ica_reseaux_fluides_lag4 = lag(ica_reseaux_fluides, 4),

    ica_reseaux_elec_telecom_lag1 = lag(ica_reseaux_elec_telecom, 1),
    ica_reseaux_elec_telecom_lag2 = lag(ica_reseaux_elec_telecom, 2),
    ica_reseaux_elec_telecom_lag4 = lag(ica_reseaux_elec_telecom, 4)
  )


# --------------------------------------------------
# 2. Train / test split
# --------------------------------------------------

train <- d |>
  filter(annee < 2022)

test <- d |>
  filter(annee >= 2022)


# --------------------------------------------------
# 3. Benchmark
# --------------------------------------------------

m_ar <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 4. ROADS
# --------------------------------------------------

m_route_l1 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_routes_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

m_route_l2 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_routes_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

m_route_l4 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_routes_lag4 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 5. EARTHWORKS
# --------------------------------------------------

m_terr_l1 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

m_terr_l2 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

m_terr_l4 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement_lag4 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 6. RAIL
# --------------------------------------------------

m_rail_l1 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_voies_ferrees_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

m_rail_l2 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_voies_ferrees_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

m_rail_l4 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_voies_ferrees_lag4 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 7. WATER / FLUID NETWORKS
# --------------------------------------------------

m_eau_l1 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_reseaux_fluides_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

m_eau_l2 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_reseaux_fluides_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

m_eau_l4 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_reseaux_fluides_lag4 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 8. ELECTRICITY / TELECOM
# --------------------------------------------------

m_elec_l1 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_reseaux_elec_telecom_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

m_elec_l2 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_reseaux_elec_telecom_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

m_elec_l4 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_reseaux_elec_telecom_lag4 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 9. Predictions
# --------------------------------------------------

test$pred_ar <- predict(m_ar, newdata = test)

test$pred_route_l1 <- predict(m_route_l1, newdata = test)
test$pred_route_l2 <- predict(m_route_l2, newdata = test)
test$pred_route_l4 <- predict(m_route_l4, newdata = test)

test$pred_terr_l1 <- predict(m_terr_l1, newdata = test)
test$pred_terr_l2 <- predict(m_terr_l2, newdata = test)
test$pred_terr_l4 <- predict(m_terr_l4, newdata = test)

test$pred_rail_l1 <- predict(m_rail_l1, newdata = test)
test$pred_rail_l2 <- predict(m_rail_l2, newdata = test)
test$pred_rail_l4 <- predict(m_rail_l4, newdata = test)

test$pred_eau_l1 <- predict(m_eau_l1, newdata = test)
test$pred_eau_l2 <- predict(m_eau_l2, newdata = test)
test$pred_eau_l4 <- predict(m_eau_l4, newdata = test)

test$pred_elec_l1 <- predict(m_elec_l1, newdata = test)
test$pred_elec_l2 <- predict(m_elec_l2, newdata = test)
test$pred_elec_l4 <- predict(m_elec_l4, newdata = test)


# --------------------------------------------------
# 10. RMSE comparison
# --------------------------------------------------

comparison_lags <- data.frame(
  model = c(
    "AR(1)",

    "Route lag1",
    "Route lag2",
    "Route lag4",

    "Earthworks lag1",
    "Earthworks lag2",
    "Earthworks lag4",

    "Rail lag1",
    "Rail lag2",
    "Rail lag4",

    "Water lag1",
    "Water lag2",
    "Water lag4",

    "Electricity lag1",
    "Electricity lag2",
    "Electricity lag4"
  ),

  RMSE = c(
    sqrt(mean((test$tp_volume_trimestriel - test$pred_ar)^2, na.rm = TRUE)),

    sqrt(mean((test$tp_volume_trimestriel - test$pred_route_l1)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_route_l2)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_route_l4)^2, na.rm = TRUE)),

    sqrt(mean((test$tp_volume_trimestriel - test$pred_terr_l1)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_terr_l2)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_terr_l4)^2, na.rm = TRUE)),

    sqrt(mean((test$tp_volume_trimestriel - test$pred_rail_l1)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_rail_l2)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_rail_l4)^2, na.rm = TRUE)),

    sqrt(mean((test$tp_volume_trimestriel - test$pred_eau_l1)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_eau_l2)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_eau_l4)^2, na.rm = TRUE)),

    sqrt(mean((test$tp_volume_trimestriel - test$pred_elec_l1)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_elec_l2)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_elec_l4)^2, na.rm = TRUE))
  )
)

comparison_lags <- comparison_lags |>
  arrange(RMSE)

print(comparison_lags)



# Best current model
m_best <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# Earthworks lag 2 + lag 4
m_terr_24 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement_lag2 +
    ica_terrassement_lag4 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# Add long-lag water
m_terr_water <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement_lag2 +
    ica_reseaux_fluides_lag4 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# Add long-lag electricity
m_terr_elec <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement_lag2 +
    ica_reseaux_elec_telecom_lag4 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# Earthworks + water + electricity
m_combined <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    ica_terrassement_lag2 +
    ica_reseaux_fluides_lag4 +
    ica_reseaux_elec_telecom_lag4 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# Predictions
test$pred_best <- predict(m_best, newdata = test)
test$pred_terr24 <- predict(m_terr_24, newdata = test)
test$pred_terr_water <- predict(m_terr_water, newdata = test)
test$pred_terr_elec <- predict(m_terr_elec, newdata = test)
test$pred_combined <- predict(m_combined, newdata = test)

# Compare RMSE
comparison2 <- data.frame(
  model = c(
    "AR(1)",
    "Earthworks lag2",
    "Earthworks lag2 + lag4",
    "Earthworks lag2 + water lag4",
    "Earthworks lag2 + electricity lag4",
    "Earthworks lag2 + water lag4 + electricity lag4"
  ),

  RMSE = c(
    sqrt(mean((test$tp_volume_trimestriel - test$pred_ar)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_best)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_terr24)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_terr_water)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_terr_elec)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_combined)^2, na.rm = TRUE))
  )
)

comparison2 <- comparison2 |>
  arrange(RMSE)

print(comparison2)


library(dplyr)



d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    marche_tp_volume_lag1 = lag(marches_tp_volume, 1),
    marche_tp_volume_lag2 = lag(marches_tp_volume, 2),
    marche_tp_volume_lag4 = lag(marches_tp_volume, 4)
  )

train <- d |> filter(annee < 2022)
test  <- d |> filter(annee >= 2022)

# AR benchmark
m_ar <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# Markets lag 1
m_marche_l1 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    marche_tp_volume_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# Markets lag 2
m_marche_l2 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    marche_tp_volume_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# Markets lag 4
m_marche_l4 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    marche_tp_volume_lag4 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

# Markets lag 1 + lag 2
m_marche_12 <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    marche_tp_volume_lag1 +
    marche_tp_volume_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

test$pred_ar <- predict(m_ar, newdata = test)

test$pred_marche_l1 <- predict(m_marche_l1, newdata = test)
test$pred_marche_l2 <- predict(m_marche_l2, newdata = test)
test$pred_marche_l4 <- predict(m_marche_l4, newdata = test)
test$pred_marche_12 <- predict(m_marche_12, newdata = test)

comparison_marche <- data.frame(
  model = c(
    "AR(1)",
    "Markets lag1",
    "Markets lag2",
    "Markets lag4",
    "Markets lag1 + lag2"
  ),
  RMSE = c(
    sqrt(mean((test$tp_volume_trimestriel - test$pred_ar)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_marche_l1)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_marche_l2)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_marche_l4)^2, na.rm = TRUE)),
    sqrt(mean((test$tp_volume_trimestriel - test$pred_marche_12)^2, na.rm = TRUE))
  )
)

comparison_marche[order(comparison_marche$RMSE), ]

m_best_combined <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    marches_volume_lag1 +
    marches_volume_lag2 +
    ica_terrassement_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

test$pred_best_combined <- predict(
  m_best_combined,
  newdata = test
)

rmse_best_combined <- sqrt(
  mean(
    (test$tp_volume_trimestriel - test$pred_best_combined)^2,
    na.rm = TRUE
  )
)

rmse_best_combined

library(dplyr)

# --------------------------------------------------
# 1. Create lags of Marchés TP
# --------------------------------------------------

d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    marches_lag1 = lag(marches_tp_volume, 1),
    marches_lag2 = lag(marches_tp_volume, 2),
    marches_lag3 = lag(marches_tp_volume, 3),
    marches_lag4 = lag(marches_tp_volume, 4)
  )

train_m <- d |>
  filter(annee < 2022)

test_m <- d |>
  filter(annee >= 2022)


# --------------------------------------------------
# 2. AR(1)
# --------------------------------------------------

m_markets_ar1 <- lm(
  marches_tp_volume ~
    marches_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train_m
)


# --------------------------------------------------
# 3. AR(2)
# --------------------------------------------------

m_markets_ar2 <- lm(
  marches_tp_volume ~
    marches_lag1 +
    marches_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train_m
)


# --------------------------------------------------
# 4. AR(1) + seasonal quarterly lag
# --------------------------------------------------

m_markets_ar4 <- lm(
  marches_tp_volume ~
    marches_lag1 +
    marches_lag4 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train_m
)


# --------------------------------------------------
# 5. AR(1) + lag2 + lag4
# --------------------------------------------------

m_markets_full <- lm(
  marches_tp_volume ~
    marches_lag1 +
    marches_lag2 +
    marches_lag4 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train_m
)


# --------------------------------------------------
# 6. Out-of-sample predictions
# --------------------------------------------------

test_m$pred_naive <- test_m$marches_lag1

test_m$pred_ar1 <- predict(
  m_markets_ar1,
  newdata = test_m
)

test_m$pred_ar2 <- predict(
  m_markets_ar2,
  newdata = test_m
)

test_m$pred_ar4 <- predict(
  m_markets_ar4,
  newdata = test_m
)

test_m$pred_full <- predict(
  m_markets_full,
  newdata = test_m
)


# --------------------------------------------------
# 7. Compare RMSE
# --------------------------------------------------

comparison_markets <- data.frame(
  model = c(
    "Naive",
    "AR(1)",
    "AR(2)",
    "AR(1) + lag4",
    "AR(1) + lag2 + lag4"
  ),

  RMSE = c(
    sqrt(mean(
      (test_m$marches_tp_volume - test_m$pred_naive)^2,
      na.rm = TRUE
    )),

    sqrt(mean(
      (test_m$marches_tp_volume - test_m$pred_ar1)^2,
      na.rm = TRUE
    )),

    sqrt(mean(
      (test_m$marches_tp_volume - test_m$pred_ar2)^2,
      na.rm = TRUE
    )),

    sqrt(mean(
      (test_m$marches_tp_volume - test_m$pred_ar4)^2,
      na.rm = TRUE
    )),

    sqrt(mean(
      (test_m$marches_tp_volume - test_m$pred_full)^2,
      na.rm = TRUE
    ))
  )
)

comparison_markets <- comparison_markets |>
  arrange(RMSE)

print(comparison_markets)

library(dplyr)

# --------------------------------------------------
# 1. Sort data and create Marchés lags
# --------------------------------------------------

d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    marches_volume_lag1 = lag(marches_tp_volume, 1),
    marches_volume_lag2 = lag(marches_tp_volume, 2),
    marches_volume_lag4 = lag(marches_tp_volume, 4)
  )

train <- d |> filter(annee < 2022)
test  <- d |> filter(annee >= 2022)


# --------------------------------------------------
# 2. Model used to forecast Marchés
# --------------------------------------------------

m_markets <- lm(
  marches_tp_volume ~
    marches_volume_lag1 +
    marches_volume_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 3. Out-of-sample forecast of Marchés
# --------------------------------------------------

test$marches_hat <- predict(
  m_markets,
  newdata = test
)

# For TP_t, we need Marchés_(t-1)
# Here we use the forecast made for the previous quarter
test <- test |>
  arrange(annee, trimestre) |>
  mutate(
    marches_hat_lag1 = lag(marches_hat, 1)
  )


# --------------------------------------------------
# 4. TP benchmark: AR(1)
# --------------------------------------------------

m_tp_ar <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 5. TP model estimated with historical Marchés
# --------------------------------------------------

m_tp_markets <- lm(
  tp_volume_trimestriel ~
    tp_volume_lag1 +
    marches_volume_lag1 +
    marches_volume_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 6. Prediction using ACTUAL Marchés
# This is the previous best model
# --------------------------------------------------

test$pred_tp_actual_markets <- predict(
  m_tp_markets,
  newdata = test
)


# --------------------------------------------------
# 7. Prediction using FORECASTED Marchés
# --------------------------------------------------

test_forecast <- test

# Replace Marchés_(t-1) by our forecast
test_forecast$marches_volume_lag1 <-
  test_forecast$marches_hat_lag1

test$pred_tp_forecasted_markets <- predict(
  m_tp_markets,
  newdata = test_forecast
)


# --------------------------------------------------
# 8. AR prediction
# --------------------------------------------------

test$pred_tp_ar <- predict(
  m_tp_ar,
  newdata = test
)


# --------------------------------------------------
# 9. Compare on EXACTLY the same observations
# --------------------------------------------------

evaluation <- test |>
  filter(
    !is.na(pred_tp_ar),
    !is.na(pred_tp_actual_markets),
    !is.na(pred_tp_forecasted_markets)
  )

comparison_two_stage <- data.frame(
  model = c(
    "AR(1)",
    "TP + actual Marchés",
    "TP + forecasted Marchés"
  ),

  RMSE = c(
    sqrt(mean(
      (evaluation$tp_volume_trimestriel -
         evaluation$pred_tp_ar)^2
    )),

    sqrt(mean(
      (evaluation$tp_volume_trimestriel -
         evaluation$pred_tp_actual_markets)^2
    )),

    sqrt(mean(
      (evaluation$tp_volume_trimestriel -
         evaluation$pred_tp_forecasted_markets)^2
    ))
  ),

  MAE = c(
    mean(abs(
      evaluation$tp_volume_trimestriel -
        evaluation$pred_tp_ar
    )),

    mean(abs(
      evaluation$tp_volume_trimestriel -
        evaluation$pred_tp_actual_markets
    )),

    mean(abs(
      evaluation$tp_volume_trimestriel -
        evaluation$pred_tp_forecasted_markets
    ))
  )
)

comparison_two_stage[order(comparison_two_stage$RMSE), ]
 
library(dplyr)

# ============================================================
# 1. PREPARE THE DATA
# ============================================================

d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    tp_lag1 = lag(tp_volume_trimestriel, 1),
    marches_lag1 = lag(marches_tp_volume, 1),
    marches_lag2 = lag(marches_tp_volume, 2)
  )


# ============================================================
# 2. ROLLING OUT-OF-SAMPLE FORECAST
#
# At each quarter:
# - estimate models using past data only
# - forecast Marchés
# - use forecasted Marchés to forecast TP
# ============================================================

results <- data.frame()

start <- which(d$annee >= 2021)[1]


for (i in start:nrow(d)) {

  # Skip if TP is missing
  if (is.na(d$tp_volume_trimestriel[i])) next


  # ----------------------------------------------------------
  # A. Data available before quarter t
  # ----------------------------------------------------------

  train_tp <- d[1:(i - 1), ]

  # For forecasting Marchés_(t-1),
  # we only use information up to t-2
  train_marches <- d[1:(i - 2), ]


  # ----------------------------------------------------------
  # B. Forecast Marchés_(t-1)
  # ----------------------------------------------------------

  model_marches <- lm(
    marches_tp_volume ~
      marches_lag1 +
      marches_lag2 +
      regime_fntp_2016 +
      covid_2020_t2,
    data = train_marches
  )

  forecast_marches <- predict(
    model_marches,
    newdata = d[i - 1, ]
  )


  # ----------------------------------------------------------
  # C. AR(1) benchmark for TP
  # ----------------------------------------------------------

  model_ar <- lm(
    tp_volume_trimestriel ~
      tp_lag1 +
      regime_fntp_2016 +
      covid_2020_t2,
    data = train_tp
  )

  forecast_ar <- predict(
    model_ar,
    newdata = d[i, ]
  )


  # ----------------------------------------------------------
  # D. TP model with Marchés
  # ----------------------------------------------------------

  model_tp <- lm(
    tp_volume_trimestriel ~
      tp_lag1 +
      marches_lag1 +
      marches_lag2 +
      regime_fntp_2016 +
      covid_2020_t2,
    data = train_tp
  )


  # Create row for quarter t
  new_tp <- d[i, ]

  # Replace actual Marchés_(t-1)
  # by our forecast of Marchés_(t-1)
  new_tp$marches_lag1 <- forecast_marches


  forecast_tp <- predict(
    model_tp,
    newdata = new_tp
  )


  # ----------------------------------------------------------
  # E. Save results
  # ----------------------------------------------------------

  results <- rbind(
    results,
    data.frame(
      annee = d$annee[i],
      trimestre = d$trimestre[i],

      actual = d$tp_volume_trimestriel[i],

      forecast_ar = forecast_ar,
      forecast_marches = forecast_tp
    )
  )
}


# ============================================================
# 3. REMOVE MISSING PREDICTIONS
# ============================================================

results <- results |>
  filter(
    !is.na(actual),
    !is.na(forecast_ar),
    !is.na(forecast_marches)
  )


# ============================================================
# 4. COMPARE FORECAST ACCURACY
# ============================================================

rmse_ar <- sqrt(
  mean((results$actual - results$forecast_ar)^2)
)

rmse_marches <- sqrt(
  mean((results$actual - results$forecast_marches)^2)
)


mae_ar <- mean(
  abs(results$actual - results$forecast_ar)
)

mae_marches <- mean(
  abs(results$actual - results$forecast_marches)
)


comparison <- data.frame(
  model = c(
    "AR(1)",
    "Forecasted Marchés"
  ),

  RMSE = c(
    rmse_ar,
    rmse_marches
  ),

  MAE = c(
    mae_ar,
    mae_marches
  )
)

print(comparison)


# ============================================================
# 5. GRAPH
# ============================================================

results$time <-
  results$annee +
  (results$trimestre - 1) / 4


plot(
  results$time,
  results$actual,
  type = "l",
  col = "black",
  lwd = 2,
  ylim = range(
    results$actual,
    results$forecast_ar,
    results$forecast_marches
  ),
  xlab = "Year",
  ylab = "TP volume",
  main = "Rolling out-of-sample forecast"
)

lines(
  results$time,
  results$forecast_ar,
  col = "blue",
  lwd = 2,
  lty = 2
)

lines(
  results$time,
  results$forecast_marches,
  col = "red",
  lwd = 2,
  lty = 2
)

legend(
  "topleft",
  legend = c(
    "Actual",
    "AR(1)",
    "Forecasted Marchés"
  ),
  col = c(
    "black",
    "blue",
    "red"
  ),
  lty = c(1, 2, 2),
  lwd = 2,
  bty = "n"
)


# ============================================================
# 6. SIMPLE BOOTSTRAP
#
# Ask:
# "How often does the Marchés model beat AR(1)?"
# ============================================================

results$error_ar <-
  results$actual - results$forecast_ar

results$error_marches <-
  results$actual - results$forecast_marches


set.seed(123)

B <- 5000

rmse_gain <- numeric(B)


for (b in 1:B) {

  # Resample forecast quarters
  sample_id <- sample(
    1:nrow(results),
    replace = TRUE
  )

  rmse_ar_b <- sqrt(
    mean(results$error_ar[sample_id]^2)
  )

  rmse_marches_b <- sqrt(
    mean(results$error_marches[sample_id]^2)
  )

  # Positive number = Marchés is better
  rmse_gain[b] <- rmse_ar_b - rmse_marches_b
}


# ============================================================
# 7. BOOTSTRAP RESULTS
# ============================================================

mean(rmse_gain > 0)

quantile(
  rmse_gain,
  c(0.025, 0.975)
)

mean(rmse_gain)


set.seed(123)

B <- 5000
block_length <- 4
n <- nrow(results)

rmse_gain_block <- numeric(B)

for (b in 1:B) {

  ids <- integer(0)

  while (length(ids) < n) {

    start <- sample(
      1:(n - block_length + 1),
      1
    )

    ids <- c(
      ids,
      start:(start + block_length - 1)
    )
  }

  ids <- ids[1:n]

  rmse_ar_b <- sqrt(
    mean(results$error_ar[ids]^2)
  )

  rmse_marches_b <- sqrt(
    mean(results$error_marches[ids]^2)
  )

  rmse_gain_block[b] <-
    rmse_ar_b - rmse_marches_b
}

mean(rmse_gain_block > 0)

quantile(
  rmse_gain_block,
  c(0.025, 0.975)
)

mean(rmse_gain_block)


library(dplyr)

# --------------------------------------------------
# 1. Create lags
# --------------------------------------------------

d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    marches_lag1 = lag(marches_tp_volume, 1),
    marches_lag2 = lag(marches_tp_volume, 2),

    terr_lag1 = lag(ica_terrassement, 1),
    route_lag1 = lag(ica_routes, 1),
    rail_lag1 = lag(ica_voies_ferrees, 1),
    eau_lag1 = lag(ica_reseaux_fluides, 1),
    elec_lag1 = lag(ica_reseaux_elec_telecom, 1)
  )

train <- d |> filter(annee < 2022)
test  <- d |> filter(annee >= 2022)


# --------------------------------------------------
# 2. Marchés model 1: own dynamics only
# --------------------------------------------------

m_marche_ar <- lm(
  marches_tp_volume ~
    marches_lag1 +
    marches_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 3. Marchés model 2: sector variables only
# --------------------------------------------------

m_marche_sector <- lm(
  marches_tp_volume ~
    terr_lag1 +
    route_lag1 +
    rail_lag1 +
    eau_lag1 +
    elec_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 4. Marchés model 3: AR + sectors
# --------------------------------------------------

m_marche_full <- lm(
  marches_tp_volume ~
    marches_lag1 +
    marches_lag2 +
    terr_lag1 +
    route_lag1 +
    rail_lag1 +
    eau_lag1 +
    elec_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 5. Out-of-sample Marchés predictions
# --------------------------------------------------

test$pred_marche_ar <- predict(
  m_marche_ar,
  newdata = test
)

test$pred_marche_sector <- predict(
  m_marche_sector,
  newdata = test
)

test$pred_marche_full <- predict(
  m_marche_full,
  newdata = test
)


# --------------------------------------------------
# 6. Compare Marchés forecast accuracy
# --------------------------------------------------

comparison_marche <- data.frame(
  model = c(
    "Marchés AR(2)",
    "Sector variables",
    "AR(2) + sectors"
  ),

  RMSE = c(
    sqrt(mean(
      (test$marches_tp_volume - test$pred_marche_ar)^2,
      na.rm = TRUE
    )),

    sqrt(mean(
      (test$marches_tp_volume - test$pred_marche_sector)^2,
      na.rm = TRUE
    )),

    sqrt(mean(
      (test$marches_tp_volume - test$pred_marche_full)^2,
      na.rm = TRUE
    ))
  )
)

comparison_marche[order(comparison_marche$RMSE), ]


library(dplyr)

d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    tp_lag2 = lag(tp_volume_trimestriel, 2),

    marches_lag1 = lag(marches_tp_volume, 1),
    marches_lag2 = lag(marches_tp_volume, 2),

    terr_lag1 = lag(ica_terrassement, 1),
    route_lag1 = lag(ica_routes, 1),
    rail_lag1 = lag(ica_voies_ferrees, 1),
    water_lag1 = lag(ica_reseaux_fluides, 1),
    elec_lag1 = lag(ica_reseaux_elec_telecom, 1)
  )

train <- d |> filter(annee < 2022)
test  <- d |> filter(annee >= 2022)


# --------------------------------------------------
# 1. Benchmark: last available TP is t-2
# --------------------------------------------------

m_tp_lag2 <- lm(
  tp_volume_trimestriel ~
    tp_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 2. TP lag2 + Marchés
# --------------------------------------------------

m_markets <- lm(
  tp_volume_trimestriel ~
    tp_lag2 +
    marches_lag1 +
    marches_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 3. TP lag2 + Marchés + sector information
# --------------------------------------------------

m_markets_sector <- lm(
  tp_volume_trimestriel ~
    tp_lag2 +
    marches_lag1 +
    marches_lag2 +
    terr_lag1 +
    route_lag1 +
    water_lag1 +
    elec_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# 4. Sector variables only
# --------------------------------------------------

m_sector <- lm(
  tp_volume_trimestriel ~
    tp_lag2 +
    terr_lag1 +
    route_lag1 +
    water_lag1 +
    elec_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# --------------------------------------------------
# Predictions
# --------------------------------------------------

test$pred_lag2 <- predict(
  m_tp_lag2,
  newdata = test
)

test$pred_markets <- predict(
  m_markets,
  newdata = test
)

test$pred_markets_sector <- predict(
  m_markets_sector,
  newdata = test
)

test$pred_sector <- predict(
  m_sector,
  newdata = test
)


# --------------------------------------------------
# Compare RMSE
# --------------------------------------------------

comparison_no_tp_lag1 <- data.frame(
  model = c(
    "TP lag2",
    "TP lag2 + Markets",
    "TP lag2 + Markets + sectors",
    "TP lag2 + sectors"
  ),

  RMSE = c(
    sqrt(mean(
      (test$tp_volume_trimestriel - test$pred_lag2)^2,
      na.rm = TRUE
    )),

    sqrt(mean(
      (test$tp_volume_trimestriel - test$pred_markets)^2,
      na.rm = TRUE
    )),

    sqrt(mean(
      (test$tp_volume_trimestriel - test$pred_markets_sector)^2,
      na.rm = TRUE
    )),

    sqrt(mean(
      (test$tp_volume_trimestriel - test$pred_sector)^2,
      na.rm = TRUE
    ))
  )
)

comparison_no_tp_lag1[order(comparison_no_tp_lag1$RMSE), ]


library(dplyr)

# ============================================================
# 1. PREPARE DATA
# ============================================================

d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    # TP lags
    tp_lag1 = lag(tp_volume_trimestriel, 1),
    tp_lag2 = lag(tp_volume_trimestriel, 2),

    # Marchés lags
    marches_lag1 = lag(marches_tp_volume, 1),
    marches_lag2 = lag(marches_tp_volume, 2),

    # Best sector variable found previously
    earthworks_lag2 = lag(ica_terrassement, 2)
  )


# ============================================================
# 2. TRAIN / TEST SPLIT
# ============================================================

train <- d |>
  filter(annee < 2022)

test <- d |>
  filter(annee >= 2022)


# ============================================================
# 3. FORECAST MARCHÉS
#
# We use the parsimonious AR(2) model for Marchés
# ============================================================

m_marches <- lm(
  marches_tp_volume ~
    marches_lag1 +
    marches_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)

test$pred_marches <- predict(
  m_marches,
  newdata = test
)

# For TP_t we need predicted Marchés_(t-1)
test <- test |>
  arrange(annee, trimestre) |>
  mutate(
    pred_marches_lag1 = lag(pred_marches, 1)
  )


# ============================================================
# SCENARIO A
# TP_(t-1) IS AVAILABLE
# ============================================================


# ------------------------------------------------------------
# A1. AR(1) benchmark
# ------------------------------------------------------------

m_A_ar <- lm(
  tp_volume_trimestriel ~
    tp_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# ------------------------------------------------------------
# A2. AR(1) + observed Marchés
# ------------------------------------------------------------

m_A_markets <- lm(
  tp_volume_trimestriel ~
    tp_lag1 +
    marches_lag1 +
    marches_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# ------------------------------------------------------------
# A3. AR(1) + Earthworks lag2
# ------------------------------------------------------------

m_A_earth <- lm(
  tp_volume_trimestriel ~
    tp_lag1 +
    earthworks_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# ------------------------------------------------------------
# Predictions
# ------------------------------------------------------------

test$A_ar <- predict(
  m_A_ar,
  newdata = test
)

test$A_markets_observed <- predict(
  m_A_markets,
  newdata = test
)


# ------------------------------------------------------------
# A4. AR(1) + FORECASTED Marchés
# ------------------------------------------------------------

test_A_forecast <- test

test_A_forecast$marches_lag1 <-
  test_A_forecast$pred_marches_lag1

test$A_markets_forecast <- predict(
  m_A_markets,
  newdata = test_A_forecast
)


test$A_earth <- predict(
  m_A_earth,
  newdata = test
)


# ============================================================
# SCENARIO B
# TP_(t-1) IS NOT AVAILABLE
# LAST OBSERVED TP = TP_(t-2)
# ============================================================


# ------------------------------------------------------------
# B1. TP lag2 benchmark
# ------------------------------------------------------------

m_B_lag2 <- lm(
  tp_volume_trimestriel ~
    tp_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# ------------------------------------------------------------
# B2. TP lag2 + observed Marchés
# ------------------------------------------------------------

m_B_markets <- lm(
  tp_volume_trimestriel ~
    tp_lag2 +
    marches_lag1 +
    marches_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# ------------------------------------------------------------
# B3. TP lag2 + Earthworks lag2
# ------------------------------------------------------------

m_B_earth <- lm(
  tp_volume_trimestriel ~
    tp_lag2 +
    earthworks_lag2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = train
)


# ------------------------------------------------------------
# Predictions
# ------------------------------------------------------------

test$B_lag2 <- predict(
  m_B_lag2,
  newdata = test
)

test$B_markets_observed <- predict(
  m_B_markets,
  newdata = test
)


# ------------------------------------------------------------
# B4. TP lag2 + FORECASTED Marchés
# ------------------------------------------------------------

test_B_forecast <- test

test_B_forecast$marches_lag1 <-
  test_B_forecast$pred_marches_lag1

test$B_markets_forecast <- predict(
  m_B_markets,
  newdata = test_B_forecast
)


test$B_earth <- predict(
  m_B_earth,
  newdata = test
)


# ============================================================
# 4. SCENARIO A:
# COMPARE ALL MODELS ON SAME OBSERVATIONS
# ============================================================

eval_A <- test |>
  filter(
    !is.na(tp_volume_trimestriel),
    !is.na(A_ar),
    !is.na(A_markets_observed),
    !is.na(A_markets_forecast),
    !is.na(A_earth)
  )


comparison_A <- data.frame(

  model = c(
    "AR(1)",
    "AR(1) + observed Markets",
    "AR(1) + forecasted Markets",
    "AR(1) + Earthworks lag2"
  ),

  RMSE = c(

    sqrt(mean(
      (eval_A$tp_volume_trimestriel -
         eval_A$A_ar)^2
    )),

    sqrt(mean(
      (eval_A$tp_volume_trimestriel -
         eval_A$A_markets_observed)^2
    )),

    sqrt(mean(
      (eval_A$tp_volume_trimestriel -
         eval_A$A_markets_forecast)^2
    )),

    sqrt(mean(
      (eval_A$tp_volume_trimestriel -
         eval_A$A_earth)^2
    ))
  ),

  MAE = c(

    mean(abs(
      eval_A$tp_volume_trimestriel -
        eval_A$A_ar
    )),

    mean(abs(
      eval_A$tp_volume_trimestriel -
        eval_A$A_markets_observed
    )),

    mean(abs(
      eval_A$tp_volume_trimestriel -
        eval_A$A_markets_forecast
    )),

    mean(abs(
      eval_A$tp_volume_trimestriel -
        eval_A$A_earth
    ))
  )
)

comparison_A <- comparison_A |>
  arrange(RMSE)

print("SCENARIO A: previous-quarter TP available")
print(comparison_A)


# ============================================================
# 5. SCENARIO B:
# COMPARE ALL MODELS ON SAME OBSERVATIONS
# ============================================================

eval_B <- test |>
  filter(
    !is.na(tp_volume_trimestriel),
    !is.na(B_lag2),
    !is.na(B_markets_observed),
    !is.na(B_markets_forecast),
    !is.na(B_earth)
  )


comparison_B <- data.frame(

  model = c(
    "TP lag2",
    "TP lag2 + observed Markets",
    "TP lag2 + forecasted Markets",
    "TP lag2 + Earthworks lag2"
  ),

  RMSE = c(

    sqrt(mean(
      (eval_B$tp_volume_trimestriel -
         eval_B$B_lag2)^2
    )),

    sqrt(mean(
      (eval_B$tp_volume_trimestriel -
         eval_B$B_markets_observed)^2
    )),

    sqrt(mean(
      (eval_B$tp_volume_trimestriel -
         eval_B$B_markets_forecast)^2
    )),

    sqrt(mean(
      (eval_B$tp_volume_trimestriel -
         eval_B$B_earth)^2
    ))
  ),

  MAE = c(

    mean(abs(
      eval_B$tp_volume_trimestriel -
        eval_B$B_lag2
    )),

    mean(abs(
      eval_B$tp_volume_trimestriel -
        eval_B$B_markets_observed
    )),

    mean(abs(
      eval_B$tp_volume_trimestriel -
        eval_B$B_markets_forecast
    )),

    mean(abs(
      eval_B$tp_volume_trimestriel -
        eval_B$B_earth
    ))
  )
)

comparison_B <- comparison_B |>
  arrange(RMSE)

print("SCENARIO B: previous-quarter TP unavailable")
print(comparison_B)


# ============================================================
# 6. GRAPH A
# TP_(t-1) AVAILABLE
# ============================================================

eval_A$time <-
  eval_A$annee +
  (eval_A$trimestre - 1) / 4

plot(
  eval_A$time,
  eval_A$tp_volume_trimestriel,
  type = "l",
  col = "black",
  lwd = 2,
  ylim = range(
    c(
      eval_A$tp_volume_trimestriel,
      eval_A$A_ar,
      eval_A$A_markets_observed,
      eval_A$A_markets_forecast
    )
  ),
  xlab = "Year",
  ylab = "TP volume",
  main = "Out-of-sample forecast — TP lag1 available"
)

lines(
  eval_A$time,
  eval_A$A_ar,
  col = "blue",
  lwd = 2,
  lty = 2
)

lines(
  eval_A$time,
  eval_A$A_markets_observed,
  col = "darkgreen",
  lwd = 2,
  lty = 2
)

lines(
  eval_A$time,
  eval_A$A_markets_forecast,
  col = "red",
  lwd = 2,
  lty = 2
)

legend(
  "topleft",
  legend = c(
    "Actual",
    "AR(1)",
    "Observed Markets",
    "Forecasted Markets"
  ),
  col = c(
    "black",
    "blue",
    "darkgreen",
    "red"
  ),
  lty = c(1, 2, 2, 2),
  lwd = 2,
  bty = "n"
)


# ============================================================
# 7. GRAPH B
# TP_(t-1) NOT AVAILABLE
# ============================================================

eval_B$time <-
  eval_B$annee +
  (eval_B$trimestre - 1) / 4

plot(
  eval_B$time,
  eval_B$tp_volume_trimestriel,
  type = "l",
  col = "black",
  lwd = 2,
  ylim = range(
    c(
      eval_B$tp_volume_trimestriel,
      eval_B$B_lag2,
      eval_B$B_markets_observed,
      eval_B$B_markets_forecast
    )
  ),
  xlab = "Year",
  ylab = "TP volume",
  main = "Out-of-sample forecast — TP lag1 unavailable"
)

lines(
  eval_B$time,
  eval_B$B_lag2,
  col = "blue",
  lwd = 2,
  lty = 2
)

lines(
  eval_B$time,
  eval_B$B_markets_observed,
  col = "darkgreen",
  lwd = 2,
  lty = 2
)

lines(
  eval_B$time,
  eval_B$B_markets_forecast,
  col = "red",
  lwd = 2,
  lty = 2
)

legend(
  "topleft",
  legend = c(
    "Actual",
    "TP lag2",
    "Observed Markets",
    "Forecasted Markets"
  ),
  col = c(
    "black",
    "blue",
    "darkgreen",
    "red"
  ),
  lty = c(1, 2, 2, 2),
  lwd = 2,
  bty = "n"
)


library(dplyr)

# ============================================================
# 1. PREPARE DATA
# ============================================================

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


# ============================================================
# 2. CANDIDATE VARIABLES
# ============================================================

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


# ============================================================
# 3. FIXED TRAIN / TEST SPLIT FIRST
#
# This is only the first screening step.
# ============================================================

train <- d |> filter(annee < 2022)
test  <- d |> filter(annee >= 2022)


# ============================================================
# 4. SCENARIO A:
# TP lag1 is available
#
# Test models with 0, 1, 2 or 3 additional predictors.
# Do not go beyond 3 initially because sample is small.
# ============================================================

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


library(dplyr)

results <- data.frame()

start <- which(d$annee >= 2021)[1]


for (i in start:nrow(d)) {

  if (is.na(d$tp_volume_trimestriel[i])) next

  train <- d[1:(i - 1), ]


  # --------------------------------------------------
  # Benchmark AR(1)
  # --------------------------------------------------

  m0 <- lm(
    tp_volume_trimestriel ~
      tp_lag1 +
      regime_fntp_2016 +
      covid_2020_t2,
    data = train
  )


  # --------------------------------------------------
  # Best fixed-sample model
  # --------------------------------------------------

  m1 <- lm(
    tp_volume_trimestriel ~
      tp_lag1 +
      marches_lag1 +
      routes_lag4 +
      forages_lag2 +
      regime_fntp_2016 +
      covid_2020_t2,
    data = train
  )


  # --------------------------------------------------
  # Markets lag1 + drilling lag2
  # --------------------------------------------------

  m2 <- lm(
    tp_volume_trimestriel ~
      tp_lag1 +
      marches_lag1 +
      forages_lag2 +
      regime_fntp_2016 +
      covid_2020_t2,
    data = train
  )


  # --------------------------------------------------
  # Markets lag1 + lag2 + drilling lag2
  # --------------------------------------------------

  m3 <- lm(
    tp_volume_trimestriel ~
      tp_lag1 +
      marches_lag1 +
      marches_lag2 +
      forages_lag2 +
      regime_fntp_2016 +
      covid_2020_t2,
    data = train
  )


  # --------------------------------------------------
  # Markets lag1 + lag2
  # --------------------------------------------------

  m4 <- lm(
    tp_volume_trimestriel ~
      tp_lag1 +
      marches_lag1 +
      marches_lag2 +
      regime_fntp_2016 +
      covid_2020_t2,
    data = train
  )


  # --------------------------------------------------
  # Forecast quarter i
  # --------------------------------------------------

  p0 <- predict(m0, newdata = d[i, ])
  p1 <- predict(m1, newdata = d[i, ])
  p2 <- predict(m2, newdata = d[i, ])
  p3 <- predict(m3, newdata = d[i, ])
  p4 <- predict(m4, newdata = d[i, ])


  results <- rbind(
    results,
    data.frame(
      annee = d$annee[i],
      trimestre = d$trimestre[i],
      actual = d$tp_volume_trimestriel[i],

      AR1 = p0,
      best_fixed = p1,
      markets_forages = p2,
      markets12_forages = p3,
      markets12 = p4
    )
  )
}


# --------------------------------------------------
# Keep common observations
# --------------------------------------------------

results <- results |>
  filter(
    complete.cases(
      actual,
      AR1,
      best_fixed,
      markets_forages,
      markets12_forages,
      markets12
    )
  )


# --------------------------------------------------
# Compare rolling RMSE
# --------------------------------------------------

comparison <- data.frame(

  model = c(
    "AR(1)",
    "Markets1 + Roads4 + Drilling2",
    "Markets1 + Drilling2",
    "Markets1 + Markets2 + Drilling2",
    "Markets1 + Markets2"
  ),

  RMSE = c(

    sqrt(mean(
      (results$actual - results$AR1)^2
    )),

    sqrt(mean(
      (results$actual - results$best_fixed)^2
    )),

    sqrt(mean(
      (results$actual - results$markets_forages)^2
    )),

    sqrt(mean(
      (results$actual - results$markets12_forages)^2
    )),

    sqrt(mean(
      (results$actual - results$markets12)^2
    ))
  ),

  MAE = c(

    mean(abs(
      results$actual - results$AR1
    )),

    mean(abs(
      results$actual - results$best_fixed
    )),

    mean(abs(
      results$actual - results$markets_forages
    )),

    mean(abs(
      results$actual - results$markets12_forages
    )),

    mean(abs(
      results$actual - results$markets12
    ))
  )
)

comparison <- comparison |>
  arrange(RMSE)

print(comparison)


library(dplyr)

# ============================================================
# 1. PREPARE HISTORICAL DATA
# ============================================================

d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    tp_lag1 = lag(tp_volume_trimestriel, 1),
    tp_lag2 = lag(tp_volume_trimestriel, 2),

    routes_lag4 = lag(ica_routes, 4),
    tunnels_lag2 = lag(ica_tunnels, 2),
    swi_lag1 = lag(swi_trimestriel, 1),

    # variables for forecasting SWI and tunnels
    swi_lag4 = lag(swi_trimestriel, 4),
    tunnels_lag1 = lag(ica_tunnels, 1)
  )


# Keep information available through 2026 Q2
hist <- d |>
  filter(
    annee < 2026 |
    (annee == 2026 & trimestre <= 2)
  )


# ============================================================
# 2. ESTIMATE FINAL TP MODELS
# ============================================================

# Scenario A:
# previous-quarter TP is available
m_A <- lm(
  tp_volume_trimestriel ~
    tp_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = hist
)


# Scenario B:
# previous-quarter TP unavailable
m_B <- lm(
  tp_volume_trimestriel ~
    tp_lag2 +
    routes_lag4 +
    tunnels_lag2 +
    swi_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = hist
)


# ============================================================
# 3. MODELS FOR FUTURE EXTERNAL VARIABLES
#
# Needed because SWI and tunnels will eventually also
# be unknown for 2027 forecasts.
# ============================================================

m_swi <- lm(
  swi_trimestriel ~
    swi_lag1 +
    swi_lag4,
  data = hist
)

m_tunnels <- lm(
  ica_tunnels ~
    tunnels_lag1,
  data = hist
)


# ============================================================
# 4. CREATE FUTURE QUARTERS
# ============================================================

future <- data.frame(
  annee = c(2026, 2026, 2027, 2027),
  trimestre = c(3, 4, 1, 2),

  regime_fntp_2016 = 1,
  covid_2020_t2 = 0,

  pred_A = NA_real_,
  pred_B = NA_real_,

  pred_swi = NA_real_,
  pred_tunnels = NA_real_
)


# ============================================================
# 5. CREATE WORKING SERIES
# ============================================================

tp_A <- hist$tp_volume_trimestriel
tp_B <- hist$tp_volume_trimestriel

routes <- hist$ica_routes
tunnels <- hist$ica_tunnels
swi <- hist$swi_trimestriel


# ============================================================
# 6. FORECAST 2026 Q3 -> 2027 Q2
# ============================================================

for (i in 1:4) {

  # ----------------------------------------------------------
  # A. FORECAST SWI
  # ----------------------------------------------------------

  new_swi <- data.frame(
    swi_lag1 = tail(swi, 1),
    swi_lag4 = swi[length(swi) - 3]
  )

  future$pred_swi[i] <- predict(
    m_swi,
    newdata = new_swi
  )

  swi <- c(
    swi,
    future$pred_swi[i]
  )


  # ----------------------------------------------------------
  # B. FORECAST TUNNELS
  # ----------------------------------------------------------

  new_tunnel <- data.frame(
    tunnels_lag1 = tail(tunnels, 1)
  )

  future$pred_tunnels[i] <- predict(
    m_tunnels,
    newdata = new_tunnel
  )

  tunnels <- c(
    tunnels,
    future$pred_tunnels[i]
  )


  # ----------------------------------------------------------
  # C. SCENARIO A
  #
  # TP_t depends on TP_(t-1)
  #
  # After 2026 Q3, forecasts are used recursively.
  # ----------------------------------------------------------

  new_A <- data.frame(
    tp_lag1 = tail(tp_A, 1),
    regime_fntp_2016 = 1,
    covid_2020_t2 = 0
  )

  future$pred_A[i] <- predict(
    m_A,
    newdata = new_A
  )

  tp_A <- c(
    tp_A,
    future$pred_A[i]
  )


  # ----------------------------------------------------------
  # D. SCENARIO B
  #
  # TP_t depends on TP_(t-2)
  # + route t-4
  # + tunnels t-2
  # + SWI t-1
  # ----------------------------------------------------------

  new_B <- data.frame(

    tp_lag2 =
      tp_B[length(tp_B) - 1],

    routes_lag4 =
      routes[length(routes) - 3],

    tunnels_lag2 =
      tunnels[length(tunnels) - 2],

    swi_lag1 =
      swi[length(swi) - 1],

    regime_fntp_2016 = 1,
    covid_2020_t2 = 0
  )

  future$pred_B[i] <- predict(
    m_B,
    newdata = new_B
  )

  tp_B <- c(
    tp_B,
    future$pred_B[i]
  )


  # ----------------------------------------------------------
  # E. Extend routes
  #
  # No future route forecast is needed yet because lag4 means
  # 2026Q3-2027Q2 only require 2025Q3-2026Q2 routes.
  # Add NA placeholders to preserve indexing.
  # ----------------------------------------------------------

  routes <- c(routes, NA)
}


# ============================================================
# 7. SHOW FORECASTS
# ============================================================

forecast_table <- future |>
  select(
    annee,
    trimestre,
    pred_A,
    pred_B
  ) |>
  rename(
    `AR1_previous_TP_available` = pred_A,
    `TP_lag2_bridge_model` = pred_B
  )

print(forecast_table)


# ============================================================
# 8. CREATE GRAPH
# ============================================================

future$time <-
  future$annee +
  (future$trimestre - 1) / 4

historical_plot <- hist |>
  filter(
    annee >= 2024
  ) |>
  mutate(
    time = annee + (trimestre - 1) / 4
  )


plot(
  historical_plot$time,
  historical_plot$tp_volume_trimestriel,
  type = "l",
  col = "black",
  lwd = 2,

  xlim = c(
    min(historical_plot$time),
    max(future$time)
  ),

  ylim = range(
    c(
      historical_plot$tp_volume_trimestriel,
      future$pred_A,
      future$pred_B
    ),
    na.rm = TRUE
  ),

  xlab = "Year",
  ylab = "TP volume index",
  main = "TP forecasts: 2026 Q3 - 2027 Q2"
)


# Connect historical series to forecasts
last_time <- tail(historical_plot$time, 1)
last_tp <- tail(
  historical_plot$tp_volume_trimestriel,
  1
)


lines(
  c(last_time, future$time),
  c(last_tp, future$pred_A),
  col = "blue",
  lwd = 2,
  lty = 2
)


lines(
  c(last_time, future$time),
  c(last_tp, future$pred_B),
  col = "red",
  lwd = 2,
  lty = 2
)


points(
  future$time,
  future$pred_A,
  col = "blue",
  pch = 16
)

points(
  future$time,
  future$pred_B,
  col = "red",
  pch = 16
)


legend(
  "topleft",
  legend = c(
    "Actual TP",
    "Scenario A: TP lag1 available",
    "Scenario B: TP lag1 unavailable"
  ),
  col = c(
    "black",
    "blue",
    "red"
  ),
  lty = c(1, 2, 2),
  lwd = 2,
  bty = "n"
)




library(dplyr)

# ============================================================
# 1. PREPARE DATA
# ============================================================

d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    tp_lag1 = lag(tp_volume_trimestriel, 1),
    tp_lag2 = lag(tp_volume_trimestriel, 2),

    routes_lag1 = lag(ica_routes, 1),
    routes_lag4 = lag(ica_routes, 4),

    tunnels_lag1 = lag(ica_tunnels, 1),
    tunnels_lag2 = lag(ica_tunnels, 2),

    swi_lag1 = lag(swi_trimestriel, 1),
    swi_lag4 = lag(swi_trimestriel, 4)
  )


# ============================================================
# 2. DATA AVAILABLE THROUGH 2026 Q2
# ============================================================

hist <- d |>
  filter(
    annee < 2026 |
    (annee == 2026 & trimestre <= 2)
  )


# ============================================================
# 3. TP MODELS
# ============================================================

# Scenario A: previous-quarter TP available
m_A <- lm(
  tp_volume_trimestriel ~
    tp_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = hist
)


# Scenario B: previous-quarter TP unavailable
m_B <- lm(
  tp_volume_trimestriel ~
    tp_lag2 +
    routes_lag4 +
    tunnels_lag2 +
    swi_lag1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = hist
)


# ============================================================
# 4. MODELS FOR VARIABLES NEEDED BY SCENARIO B
# ============================================================

m_routes <- lm(
  ica_routes ~ routes_lag1,
  data = hist
)

m_tunnels <- lm(
  ica_tunnels ~ tunnels_lag1,
  data = hist
)

m_swi <- lm(
  swi_trimestriel ~
    swi_lag1 +
    swi_lag4,
  data = hist
)

# fallback if lag4 is unavailable
m_swi_ar1 <- lm(
  swi_trimestriel ~ swi_lag1,
  data = hist
)


# ============================================================
# 5. ADD FUTURE QUARTERS
# ============================================================

future_rows <- data.frame(
  annee = c(2026, 2026, 2027, 2027),
  trimestre = c(3, 4, 1, 2)
)

ext <- bind_rows(
  hist,
  future_rows
) |>
  arrange(annee, trimestre)

ext$regime_fntp_2016[
  is.na(ext$regime_fntp_2016)
] <- 1

ext$covid_2020_t2[
  is.na(ext$covid_2020_t2)
] <- 0


# ============================================================
# 6. FILL MISSING / FUTURE ROUTES, TUNNELS AND SWI
# ============================================================

for (i in 5:nrow(ext)) {

  # ROUTES
  if (
    is.na(ext$ica_routes[i]) &&
    !is.na(ext$ica_routes[i - 1])
  ) {

    ext$ica_routes[i] <- predict(
      m_routes,
      newdata = data.frame(
        routes_lag1 = ext$ica_routes[i - 1]
      )
    )
  }


  # TUNNELS
  if (
    is.na(ext$ica_tunnels[i]) &&
    !is.na(ext$ica_tunnels[i - 1])
  ) {

    ext$ica_tunnels[i] <- predict(
      m_tunnels,
      newdata = data.frame(
        tunnels_lag1 = ext$ica_tunnels[i - 1]
      )
    )
  }


  # SWI
  if (is.na(ext$swi_trimestriel[i])) {

    if (
      !is.na(ext$swi_trimestriel[i - 1]) &&
      !is.na(ext$swi_trimestriel[i - 4])
    ) {

      ext$swi_trimestriel[i] <- predict(
        m_swi,
        newdata = data.frame(
          swi_lag1 = ext$swi_trimestriel[i - 1],
          swi_lag4 = ext$swi_trimestriel[i - 4]
        )
      )

    } else if (
      !is.na(ext$swi_trimestriel[i - 1])
    ) {

      ext$swi_trimestriel[i] <- predict(
        m_swi_ar1,
        newdata = data.frame(
          swi_lag1 = ext$swi_trimestriel[i - 1]
        )
      )
    }
  }
}


# ============================================================
# 7. CREATE TWO TP FORECAST CHAINS
# ============================================================

tp_A <- ext$tp_volume_trimestriel
tp_B <- ext$tp_volume_trimestriel

ext$pred_A <- NA_real_
ext$pred_B <- NA_real_


future_id <- which(
  (ext$annee == 2026 & ext$trimestre >= 3) |
  ext$annee == 2027
)


# ============================================================
# 8. FORECAST TP
# ============================================================

for (i in future_id) {

  # ----------------------------------------------------------
  # Scenario A
  # TP_t depends on TP_(t-1)
  # ----------------------------------------------------------

  ext$pred_A[i] <- predict(
    m_A,
    newdata = data.frame(
      tp_lag1 = tp_A[i - 1],
      regime_fntp_2016 = 1,
      covid_2020_t2 = 0
    )
  )

  tp_A[i] <- ext$pred_A[i]


  # ----------------------------------------------------------
  # Scenario B
  # TP_t depends on:
  # TP_(t-2)
  # Routes_(t-4)
  # Tunnels_(t-2)
  # SWI_(t-1)
  # ----------------------------------------------------------

  ext$pred_B[i] <- predict(
    m_B,
    newdata = data.frame(
      tp_lag2 = tp_B[i - 2],
      routes_lag4 = ext$ica_routes[i - 4],
      tunnels_lag2 = ext$ica_tunnels[i - 2],
      swi_lag1 = ext$swi_trimestriel[i - 1],
      regime_fntp_2016 = 1,
      covid_2020_t2 = 0
    )
  )

  tp_B[i] <- ext$pred_B[i]
}


# ============================================================
# 9. FORECAST TABLE
# ============================================================

forecast_table <- ext |>
  filter(
    (annee == 2026 & trimestre >= 3) |
    annee == 2027
  ) |>
  select(
    annee,
    trimestre,
    pred_A,
    pred_B
  )

print(forecast_table)

# ============================================================
# 10. PLOT
# ============================================================

ext$time <-
  ext$annee +
  (ext$trimestre - 1) / 4

historical <- ext |>
  filter(
    annee >= 2024,
    !is.na(tp_volume_trimestriel)
  )

future_plot <- ext |>
  filter(
    (annee == 2026 & trimestre >= 3) |
    annee == 2027
  )

plot(
  historical$time,
  historical$tp_volume_trimestriel,
  type = "l",
  col = "black",
  lwd = 2,

  xlim = c(
    min(historical$time),
    max(future_plot$time)
  ),

  ylim = range(
    c(
      historical$tp_volume_trimestriel,
      future_plot$pred_A,
      future_plot$pred_B
    ),
    na.rm = TRUE
  ),

  xlab = "Year",
  ylab = "TP volume index",
  main = "TP forecasts: 2026 Q3 - 2027 Q2"
)


last_time <- tail(historical$time, 1)
last_tp <- tail(
  historical$tp_volume_trimestriel,
  1
)


# Scenario A
lines(
  c(last_time, future_plot$time),
  c(last_tp, future_plot$pred_A),
  col = "blue",
  lwd = 2,
  lty = 2
)


# Scenario B
lines(
  c(last_time, future_plot$time),
  c(last_tp, future_plot$pred_B),
  col = "red",
  lwd = 2,
  lty = 2
)


points(
  future_plot$time,
  future_plot$pred_A,
  col = "blue",
  pch = 16
)

points(
  future_plot$time,
  future_plot$pred_B,
  col = "red",
  pch = 16
)


legend(
  "topleft",
  legend = c(
    "Actual TP",
    "Scenario A: TP lag1 available",
    "Scenario B: TP lag1 unavailable"
  ),
  col = c(
    "black",
    "blue",
    "red"
  ),
  lty = c(1, 2, 2),
  lwd = 2,
  bty = "n"
)

# ============================================================
# 7. CREATE FORECAST COLUMNS
# ============================================================

tp_A <- ext$tp_volume_trimestriel
tp_B <- ext$tp_volume_trimestriel

ext$pred_A <- NA_real_
ext$lower_A <- NA_real_
ext$upper_A <- NA_real_

ext$pred_B <- NA_real_
ext$lower_B <- NA_real_
ext$upper_B <- NA_real_


future_id <- which(
  (ext$annee == 2026 & ext$trimestre >= 3) |
  ext$annee == 2027
)


# ============================================================
# 8. FORECAST TP WITH 95% PREDICTION INTERVALS
# ============================================================

for (i in future_id) {

  # ----------------------------------------------------------
  # SCENARIO A
  # TP_(t-1) available
  # ----------------------------------------------------------

  new_A <- data.frame(
    tp_lag1 = tp_A[i - 1],
    regime_fntp_2016 = 1,
    covid_2020_t2 = 0
  )

  forecast_A <- predict(
    m_A,
    newdata = new_A,
    interval = "prediction",
    level = 0.95
  )

  ext$pred_A[i]  <- forecast_A[1, "fit"]
  ext$lower_A[i] <- forecast_A[1, "lwr"]
  ext$upper_A[i] <- forecast_A[1, "upr"]

  # Recursive point forecast
  tp_A[i] <- ext$pred_A[i]


  # ----------------------------------------------------------
  # SCENARIO B
  # TP_(t-1) unavailable
  # ----------------------------------------------------------

  new_B <- data.frame(
    tp_lag2 = tp_B[i - 2],
    routes_lag4 = ext$ica_routes[i - 4],
    tunnels_lag2 = ext$ica_tunnels[i - 2],
    swi_lag1 = ext$swi_trimestriel[i - 1],
    regime_fntp_2016 = 1,
    covid_2020_t2 = 0
  )

  forecast_B <- predict(
    m_B,
    newdata = new_B,
    interval = "prediction",
    level = 0.95
  )

  ext$pred_B[i]  <- forecast_B[1, "fit"]
  ext$lower_B[i] <- forecast_B[1, "lwr"]
  ext$upper_B[i] <- forecast_B[1, "upr"]

  # Recursive point forecast
  tp_B[i] <- ext$pred_B[i]
}


# ============================================================
# 9. FORECAST TABLE
# ============================================================

forecast_table <- ext |>
  filter(
    (annee == 2026 & trimestre >= 3) |
    annee == 2027
  ) |>
  select(
    annee,
    trimestre,
    pred_A,
    lower_A,
    upper_A,
    pred_B,
    lower_B,
    upper_B
  )

print(forecast_table)

# ============================================================
# 1. CREATE INTERVAL COLUMNS
# ============================================================

ext$pred_A  <- NA_real_
ext$lower_A <- NA_real_
ext$upper_A <- NA_real_

ext$pred_B  <- NA_real_
ext$lower_B <- NA_real_
ext$upper_B <- NA_real_


# Working TP series
tp_A <- ext$tp_volume_trimestriel
tp_B <- ext$tp_volume_trimestriel


# Future rows
future_id <- which(
  (ext$annee == 2026 & ext$trimestre >= 3) |
  ext$annee == 2027
)


# ============================================================
# 2. FORECAST SCENARIO A AND B
# ============================================================

for (i in future_id) {

  # ----------------------------------------------------------
  # Scenario A
  # ----------------------------------------------------------

  new_A <- data.frame(
    tp_lag1 = tp_A[i - 1],
    regime_fntp_2016 = 1,
    covid_2020_t2 = 0
  )

  forecast_A <- predict(
    m_A,
    newdata = new_A,
    interval = "prediction",
    level = 0.95
  )

  ext$pred_A[i]  <- forecast_A[1, "fit"]
  ext$lower_A[i] <- forecast_A[1, "lwr"]
  ext$upper_A[i] <- forecast_A[1, "upr"]

  tp_A[i] <- ext$pred_A[i]


  # ----------------------------------------------------------
  # Scenario B
  # ----------------------------------------------------------

  new_B <- data.frame(
    tp_lag2 = tp_B[i - 2],
    routes_lag4 = ext$ica_routes[i - 4],
    tunnels_lag2 = ext$ica_tunnels[i - 2],
    swi_lag1 = ext$swi_trimestriel[i - 1],
    regime_fntp_2016 = 1,
    covid_2020_t2 = 0
  )

  forecast_B <- predict(
    m_B,
    newdata = new_B,
    interval = "prediction",
    level = 0.95
  )

  ext$pred_B[i]  <- forecast_B[1, "fit"]
  ext$lower_B[i] <- forecast_B[1, "lwr"]
  ext$upper_B[i] <- forecast_B[1, "upr"]

  tp_B[i] <- ext$pred_B[i]
}


# ============================================================
# 3. CREATE FORECAST TABLE
# ============================================================

forecast_table <- ext |>
  filter(
    (annee == 2026 & trimestre >= 3) |
    annee == 2027
  ) |>
  select(
    annee,
    trimestre,
    pred_A,
    lower_A,
    upper_A,
    pred_B,
    lower_B,
    upper_B
  )

print(forecast_table)

# ============================================================
# 10. PLOT WITH 95% PREDICTION INTERVALS
# ============================================================

ext$time <-
  ext$annee +
  (ext$trimestre - 1) / 4

historical <- ext |>
  filter(
    annee >= 2024,
    !is.na(tp_volume_trimestriel)
  )

future_plot <- ext |>
  filter(
    (annee == 2026 & trimestre >= 3) |
    annee == 2027
  )


plot(
  historical$time,
  historical$tp_volume_trimestriel,
  type = "l",
  col = "black",
  lwd = 2,

  xlim = c(
    min(historical$time),
    max(future_plot$time)
  ),

  ylim = range(
    c(
      historical$tp_volume_trimestriel,
      future_plot$lower_A,
      future_plot$upper_A,
      future_plot$lower_B,
      future_plot$upper_B
    ),
    na.rm = TRUE
  ),

  xlab = "Year",
  ylab = "TP volume index",
  main = "TP forecasts with 95% prediction intervals"
)


last_time <- tail(historical$time, 1)
last_tp <- tail(historical$tp_volume_trimestriel, 1)


# ------------------------------------------------------------
# Prediction interval Scenario A
# ------------------------------------------------------------

polygon(
  c(
    future_plot$time,
    rev(future_plot$time)
  ),
  c(
    future_plot$lower_A,
    rev(future_plot$upper_A)
  ),
  border = NA,
  col = rgb(0, 0, 1, 0.12)
)


# ------------------------------------------------------------
# Prediction interval Scenario B
# ------------------------------------------------------------

polygon(
  c(
    future_plot$time,
    rev(future_plot$time)
  ),
  c(
    future_plot$lower_B,
    rev(future_plot$upper_B)
  ),
  border = NA,
  col = rgb(1, 0, 0, 0.12)
)


# ------------------------------------------------------------
# Point forecasts
# ------------------------------------------------------------

lines(
  c(last_time, future_plot$time),
  c(last_tp, future_plot$pred_A),
  col = "blue",
  lwd = 2,
  lty = 2
)

lines(
  c(last_time, future_plot$time),
  c(last_tp, future_plot$pred_B),
  col = "red",
  lwd = 2,
  lty = 2
)


points(
  future_plot$time,
  future_plot$pred_A,
  col = "blue",
  pch = 16
)

points(
  future_plot$time,
  future_plot$pred_B,
  col = "red",
  pch = 16
)


legend(
  "topleft",
  legend = c(
    "Actual TP",
    "Scenario A forecast",
    "Scenario B forecast"
  ),
  col = c(
    "black",
    "blue",
    "red"
  ),
  lty = c(1, 2, 2),
  lwd = 2,
  bty = "n"
)

# ============================================================
# 1. HISTORICAL OUT-OF-SAMPLE FORECAST ERRORS
# ============================================================

error_A <- results$actual - results$AR1

error_B <- results_B_roll$actual -
  results_B_roll$routes_tunnels_swi


# Remove missing values
error_A <- error_A[!is.na(error_A)]
error_B <- error_B[!is.na(error_B)]


# ============================================================
# 2. EMPIRICAL ERROR QUANTILES
# ============================================================

# 95% interval
q_A_95 <- quantile(
  error_A,
  c(0.025, 0.975)
)

q_B_95 <- quantile(
  error_B,
  c(0.025, 0.975)
)

# 80% interval
q_A_80 <- quantile(
  error_A,
  c(0.10, 0.90)
)

q_B_80 <- quantile(
  error_B,
  c(0.10, 0.90)
)


# ============================================================
# 3. BUILD FUTURE INTERVALS
# ============================================================

future_plot$A_lower_95 <-
  future_plot$pred_A + q_A_95[1]

future_plot$A_upper_95 <-
  future_plot$pred_A + q_A_95[2]

future_plot$B_lower_95 <-
  future_plot$pred_B + q_B_95[1]

future_plot$B_upper_95 <-
  future_plot$pred_B + q_B_95[2]


future_plot$A_lower_80 <-
  future_plot$pred_A + q_A_80[1]

future_plot$A_upper_80 <-
  future_plot$pred_A + q_A_80[2]

future_plot$B_lower_80 <-
  future_plot$pred_B + q_B_80[1]

future_plot$B_upper_80 <-
  future_plot$pred_B + q_B_80[2]


# ============================================================
# 4. PLOT
# ============================================================

plot(
  historical$time,
  historical$tp_volume_trimestriel,
  type = "l",
  col = "black",
  lwd = 2,

  xlim = c(
    min(historical$time),
    max(future_plot$time)
  ),

  ylim = range(
    historical$tp_volume_trimestriel,
    future_plot$A_lower_95,
    future_plot$A_upper_95,
    future_plot$B_lower_95,
    future_plot$B_upper_95,
    na.rm = TRUE
  ),

  xlab = "Year",
  ylab = "TP volume index",
  main = "TP forecasts based on rolling out-of-sample errors"
)


# ------------------------------------------------------------
# 80% uncertainty bands
# ------------------------------------------------------------

polygon(
  c(
    future_plot$time,
    rev(future_plot$time)
  ),
  c(
    future_plot$A_lower_80,
    rev(future_plot$A_upper_80)
  ),
  border = NA,
  col = rgb(0, 0, 1, 0.12)
)

polygon(
  c(
    future_plot$time,
    rev(future_plot$time)
  ),
  c(
    future_plot$B_lower_80,
    rev(future_plot$B_upper_80)
  ),
  border = NA,
  col = rgb(1, 0, 0, 0.12)
)


# ------------------------------------------------------------
# 95% limits as dotted lines
# ------------------------------------------------------------

lines(
  future_plot$time,
  future_plot$A_lower_95,
  col = "blue",
  lty = 3
)

lines(
  future_plot$time,
  future_plot$A_upper_95,
  col = "blue",
  lty = 3
)

lines(
  future_plot$time,
  future_plot$B_lower_95,
  col = "red",
  lty = 3
)

lines(
  future_plot$time,
  future_plot$B_upper_95,
  col = "red",
  lty = 3
)


# ------------------------------------------------------------
# Forecast paths
# ------------------------------------------------------------

lines(
  c(last_time, future_plot$time),
  c(last_tp, future_plot$pred_A),
  col = "blue",
  lwd = 2,
  lty = 2
)

lines(
  c(last_time, future_plot$time),
  c(last_tp, future_plot$pred_B),
  col = "red",
  lwd = 2,
  lty = 2
)


points(
  future_plot$time,
  future_plot$pred_A,
  col = "blue",
  pch = 16
)

points(
  future_plot$time,
  future_plot$pred_B,
  col = "red",
  pch = 16
)


legend(
  "topleft",
  legend = c(
    "Actual TP",
    "Scenario A",
    "Scenario B",
    "80% empirical interval",
    "95% empirical limits"
  ),
  col = c(
    "black",
    "blue",
    "red",
    "grey",
    "grey"
  ),
  lty = c(
    1,
    2,
    2,
    1,
    3
  ),
  lwd = c(
    2,
    2,
    2,
    6,
    1
  ),
  bty = "n"
)



library(dplyr)

# ============================================================
# 1. PREPARE DATA
# ============================================================

d <- d |>
  arrange(annee, trimestre) |>
  mutate(
    tp_lag1 = lag(tp_volume_trimestriel, 1),
    tp_lag2 = lag(tp_volume_trimestriel, 2),

    routes_lag4 = lag(ica_routes, 4),
    tunnels_lag2 = lag(ica_tunnels, 2),
    swi_lag1 = lag(swi_trimestriel, 1)
  )


# ============================================================
# 2. ROLLING OUT-OF-SAMPLE FORECASTS
# ============================================================

rolling <- data.frame()

start <- which(d$annee >= 2021)[1]


for (i in start:nrow(d)) {

  if (
    is.na(d$tp_volume_trimestriel[i]) |
    is.na(d$tp_lag1[i]) |
    is.na(d$tp_lag2[i]) |
    is.na(d$routes_lag4[i]) |
    is.na(d$tunnels_lag2[i]) |
    is.na(d$swi_lag1[i])
  ) next


  # Use only observations before quarter i
  train <- d[1:(i - 1), ]


  # ----------------------------------------------------------
  # Scenario A: TP_(t-1) available
  # ----------------------------------------------------------

  m_A_roll <- lm(
    tp_volume_trimestriel ~
      tp_lag1 +
      regime_fntp_2016 +
      covid_2020_t2,
    data = train
  )

  pred_A_roll <- predict(
    m_A_roll,
    newdata = d[i, ]
  )


  # ----------------------------------------------------------
  # Scenario B: TP_(t-1) unavailable
  # ----------------------------------------------------------

  m_B_roll <- lm(
    tp_volume_trimestriel ~
      tp_lag2 +
      routes_lag4 +
      tunnels_lag2 +
      swi_lag1 +
      regime_fntp_2016 +
      covid_2020_t2,
    data = train
  )

  pred_B_roll <- predict(
    m_B_roll,
    newdata = d[i, ]
  )


  # ----------------------------------------------------------
  # Save
  # ----------------------------------------------------------

  rolling <- rbind(
    rolling,
    data.frame(
      annee = d$annee[i],
      trimestre = d$trimestre[i],
      actual = d$tp_volume_trimestriel[i],
      pred_A = pred_A_roll,
      pred_B = pred_B_roll
    )
  )
}


# ============================================================
# 3. FORECAST ERRORS
# ============================================================

rolling$error_A <-
  rolling$actual - rolling$pred_A

rolling$error_B <-
  rolling$actual - rolling$pred_B


# Check performance
sqrt(mean(rolling$error_A^2))
sqrt(mean(rolling$error_B^2))


# ============================================================
# 4. EMPIRICAL 80% AND 95% INTERVALS
# ============================================================

q_A_80 <- quantile(
  rolling$error_A,
  c(0.10, 0.90),
  na.rm = TRUE
)

q_A_95 <- quantile(
  rolling$error_A,
  c(0.025, 0.975),
  na.rm = TRUE
)


q_B_80 <- quantile(
  rolling$error_B,
  c(0.10, 0.90),
  na.rm = TRUE
)

q_B_95 <- quantile(
  rolling$error_B,
  c(0.025, 0.975),
  na.rm = TRUE
)


# ============================================================
# 5. APPLY TO FUTURE FORECASTS
# ============================================================

future_plot$A_lower_80 <-
  future_plot$pred_A + q_A_80[1]

future_plot$A_upper_80 <-
  future_plot$pred_A + q_A_80[2]

future_plot$A_lower_95 <-
  future_plot$pred_A + q_A_95[1]

future_plot$A_upper_95 <-
  future_plot$pred_A + q_A_95[2]


future_plot$B_lower_80 <-
  future_plot$pred_B + q_B_80[1]

future_plot$B_upper_80 <-
  future_plot$pred_B + q_B_80[2]

future_plot$B_lower_95 <-
  future_plot$pred_B + q_B_95[1]

future_plot$B_upper_95 <-
  future_plot$pred_B + q_B_95[2]


# See results
future_plot |>
  select(
    annee,
    trimestre,
    pred_A,
    A_lower_80,
    A_upper_80,
    A_lower_95,
    A_upper_95,
    pred_B,
    B_lower_80,
    B_upper_80,
    B_lower_95,
    B_upper_95
  )


# ============================================================
# PLOT FUTURE FORECASTS + EMPIRICAL INTERVALS
# ============================================================

plot(
  historical$time,
  historical$tp_volume_trimestriel,
  type = "l",
  col = "black",
  lwd = 2,

  xlim = c(
    min(historical$time),
    max(future_plot$time)
  ),

  ylim = range(
    c(
      historical$tp_volume_trimestriel,
      future_plot$A_lower_95,
      future_plot$A_upper_95,
      future_plot$B_lower_95,
      future_plot$B_upper_95
    ),
    na.rm = TRUE
  ),

  xlab = "Year",
  ylab = "TP volume index",
  main = "TP forecasts: 2026 Q3 - 2027 Q2"
)


# ============================================================
# 1. 80% INTERVAL - SCENARIO A
# ============================================================

polygon(
  c(
    future_plot$time,
    rev(future_plot$time)
  ),
  c(
    future_plot$A_lower_80,
    rev(future_plot$A_upper_80)
  ),
  col = rgb(0, 0, 1, 0.12),
  border = NA
)


# ============================================================
# 2. 80% INTERVAL - SCENARIO B
# ============================================================

polygon(
  c(
    future_plot$time,
    rev(future_plot$time)
  ),
  c(
    future_plot$B_lower_80,
    rev(future_plot$B_upper_80)
  ),
  col = rgb(1, 0, 0, 0.12),
  border = NA
)


# ============================================================
# 3. 95% LIMITS - SCENARIO A
# ============================================================

lines(
  future_plot$time,
  future_plot$A_lower_95,
  col = "blue",
  lty = 3,
  lwd = 1
)

lines(
  future_plot$time,
  future_plot$A_upper_95,
  col = "blue",
  lty = 3,
  lwd = 1
)


# ============================================================
# 4. 95% LIMITS - SCENARIO B
# ============================================================

lines(
  future_plot$time,
  future_plot$B_lower_95,
  col = "red",
  lty = 3,
  lwd = 1
)

lines(
  future_plot$time,
  future_plot$B_upper_95,
  col = "red",
  lty = 3,
  lwd = 1
)


# ============================================================
# 5. POINT FORECAST - SCENARIO A
# ============================================================

lines(
  c(last_time, future_plot$time),
  c(last_tp, future_plot$pred_A),
  col = "blue",
  lwd = 2,
  lty = 2
)

points(
  future_plot$time,
  future_plot$pred_A,
  col = "blue",
  pch = 16
)


# ============================================================
# 6. POINT FORECAST - SCENARIO B
# ============================================================

lines(
  c(last_time, future_plot$time),
  c(last_tp, future_plot$pred_B),
  col = "red",
  lwd = 2,
  lty = 2
)

points(
  future_plot$time,
  future_plot$pred_B,
  col = "red",
  pch = 16
)


# ============================================================
# 7. LEGEND
# ============================================================

legend(
  "topleft",

  legend = c(
    "Actual TP",
    "Scenario A forecast",
    "Scenario B forecast",
    "95% empirical bounds"
  ),

  col = c(
    "black",
    "blue",
    "red",
    "grey40"
  ),

  lty = c(
    1,
    2,
    2,
    3
  ),

  lwd = c(
    2,
    2,
    2,
    1
  ),

  bty = "n"
)


library(dplyr)

set.seed(123)

# ============================================================
# 1. HISTORICAL DATA AVAILABLE THROUGH 2026 Q2
# ============================================================

hist <- d |>
  arrange(annee, trimestre) |>
  filter(
    annee < 2026 |
    (annee == 2026 & trimestre <= 2)
  )

n_hist <- nrow(hist)


# ============================================================
# 2. NUMBER OF MONTE CARLO SIMULATIONS
# ============================================================

B <- 5000
H <- 4

quarters <- c(
  "2026 Q3",
  "2026 Q4",
  "2027 Q1",
  "2027 Q2"
)


# ============================================================
# 3. RESIDUAL STANDARD DEVIATIONS
# ============================================================

sigma_A <- sigma(m_A)
sigma_B <- sigma(m_B)

sigma_swi <- sigma(m_swi)
sigma_tunnels <- sigma(m_tunnels)


# ============================================================
# 4. STORAGE FOR SIMULATED TP PATHS
# ============================================================

sim_A <- matrix(
  NA_real_,
  nrow = B,
  ncol = H
)

sim_B <- matrix(
  NA_real_,
  nrow = B,
  ncol = H
)


# ============================================================
# 5. MONTE CARLO LOOP
# ============================================================

for (b in 1:B) {

  # ----------------------------------------------------------
  # Historical series
  # ----------------------------------------------------------

  tp_A <- hist$tp_volume_trimestriel
  tp_B <- hist$tp_volume_trimestriel

  swi_sim <- hist$swi_trimestriel
  tunnel_sim <- hist$ica_tunnels

  routes <- hist$ica_routes


  # ----------------------------------------------------------
  # Forecast four quarters recursively
  # ----------------------------------------------------------

  for (h in 1:H) {

    i <- n_hist + h


    # ========================================================
    # A. SIMULATE FUTURE SWI
    # ========================================================

    new_swi <- data.frame(
      swi_lag1 = swi_sim[i - 1],
      swi_lag4 = swi_sim[i - 4]
    )

    mean_swi <- predict(
      m_swi,
      newdata = new_swi
    )

    swi_new <-
      mean_swi +
      rnorm(
        1,
        mean = 0,
        sd = sigma_swi
      )

    swi_sim[i] <- swi_new


    # ========================================================
    # B. SIMULATE FUTURE TUNNELS
    # ========================================================

    new_tunnel <- data.frame(
      tunnels_lag1 = tunnel_sim[i - 1]
    )

    mean_tunnel <- predict(
      m_tunnels,
      newdata = new_tunnel
    )

    tunnel_new <-
      mean_tunnel +
      rnorm(
        1,
        mean = 0,
        sd = sigma_tunnels
      )

    tunnel_sim[i] <- tunnel_new


    # ========================================================
    # C. SCENARIO A
    #
    # TP_t = f(TP_t-1)
    # ========================================================

    new_A <- data.frame(
      tp_lag1 = tp_A[i - 1],
      regime_fntp_2016 = 1,
      covid_2020_t2 = 0
    )

    mean_A <- predict(
      m_A,
      newdata = new_A
    )

    tp_new_A <-
      mean_A +
      rnorm(
        1,
        mean = 0,
        sd = sigma_A
      )

    tp_A[i] <- tp_new_A

    sim_A[b, h] <- tp_new_A


    # ========================================================
    # D. SCENARIO B
    #
    # TP_t =
    # f(
    #   TP_t-2,
    #   routes_t-4,
    #   tunnels_t-2,
    #   SWI_t-1
    # )
    # ========================================================

    new_B <- data.frame(

      tp_lag2 =
        tp_B[i - 2],

      routes_lag4 =
        routes[i - 4],

      tunnels_lag2 =
        tunnel_sim[i - 2],

      swi_lag1 =
        swi_sim[i - 1],

      regime_fntp_2016 = 1,
      covid_2020_t2 = 0
    )

    mean_B <- predict(
      m_B,
      newdata = new_B
    )

    tp_new_B <-
      mean_B +
      rnorm(
        1,
        mean = 0,
        sd = sigma_B
      )

    tp_B[i] <- tp_new_B

    sim_B[b, h] <- tp_new_B
  }
}


# ============================================================
# 6. MONTE CARLO FORECAST DISTRIBUTIONS
# ============================================================

forecast_mc <- data.frame(

  quarter = quarters,

  # Scenario A
  A_median = apply(
    sim_A,
    2,
    median,
    na.rm = TRUE
  ),

  A_lower80 = apply(
    sim_A,
    2,
    quantile,
    probs = 0.10,
    na.rm = TRUE
  ),

  A_upper80 = apply(
    sim_A,
    2,
    quantile,
    probs = 0.90,
    na.rm = TRUE
  ),

  A_lower95 = apply(
    sim_A,
    2,
    quantile,
    probs = 0.025,
    na.rm = TRUE
  ),

  A_upper95 = apply(
    sim_A,
    2,
    quantile,
    probs = 0.975,
    na.rm = TRUE
  ),


  # Scenario B
  B_median = apply(
    sim_B,
    2,
    median,
    na.rm = TRUE
  ),

  B_lower80 = apply(
    sim_B,
    2,
    quantile,
    probs = 0.10,
    na.rm = TRUE
  ),

  B_upper80 = apply(
    sim_B,
    2,
    quantile,
    probs = 0.90,
    na.rm = TRUE
  ),

  B_lower95 = apply(
    sim_B,
    2,
    quantile,
    probs = 0.025,
    na.rm = TRUE
  ),

  B_upper95 = apply(
    sim_B,
    2,
    quantile,
    probs = 0.975,
    na.rm = TRUE
  )
)

print(forecast_mc)


# ============================================================
# 7. TIME AXIS
# ============================================================

forecast_mc$time <- c(
  2026.50,
  2026.75,
  2027.00,
  2027.25
)

historical_plot <- hist |>
  filter(annee >= 2024) |>
  mutate(
    time = annee +
      (trimestre - 1) / 4
  )

last_time <- tail(
  historical_plot$time,
  1
)

last_tp <- tail(
  historical_plot$tp_volume_trimestriel,
  1
)


# ============================================================
# 8. BASE PLOT
# ============================================================

plot(
  historical_plot$time,
  historical_plot$tp_volume_trimestriel,

  type = "l",
  col = "black",
  lwd = 2,

  xlim = c(
    2024,
    2027.3
  ),

  ylim = range(
    c(
      historical_plot$tp_volume_trimestriel,
      forecast_mc$A_lower95,
      forecast_mc$A_upper95,
      forecast_mc$B_lower95,
      forecast_mc$B_upper95
    ),
    na.rm = TRUE
  ),

  xlab = "Year",
  ylab = "TP volume index",

  main = "TP forecasts — Monte Carlo simulation"
)


# ============================================================
# 9. SCENARIO A - 95% BAND
# ============================================================

polygon(
  c(
    forecast_mc$time,
    rev(forecast_mc$time)
  ),

  c(
    forecast_mc$A_lower95,
    rev(forecast_mc$A_upper95)
  ),

  col = rgb(
    0,
    0,
    1,
    0.08
  ),

  border = NA
)


# ============================================================
# 10. SCENARIO A - 80% BAND
# ============================================================

polygon(
  c(
    forecast_mc$time,
    rev(forecast_mc$time)
  ),

  c(
    forecast_mc$A_lower80,
    rev(forecast_mc$A_upper80)
  ),

  col = rgb(
    0,
    0,
    1,
    0.18
  ),

  border = NA
)


# ============================================================
# 11. SCENARIO B - 95% BAND
# ============================================================

polygon(
  c(
    forecast_mc$time,
    rev(forecast_mc$time)
  ),

  c(
    forecast_mc$B_lower95,
    rev(forecast_mc$B_upper95)
  ),

  col = rgb(
    1,
    0,
    0,
    0.08
  ),

  border = NA
)


# ============================================================
# 12. SCENARIO B - 80% BAND
# ============================================================

polygon(
  c(
    forecast_mc$time,
    rev(forecast_mc$time)
  ),

  c(
    forecast_mc$B_lower80,
    rev(forecast_mc$B_upper80)
  ),

  col = rgb(
    1,
    0,
    0,
    0.18
  ),

  border = NA
)


# ============================================================
# 13. MEDIAN FORECAST PATH - SCENARIO A
# ============================================================

lines(
  c(
    last_time,
    forecast_mc$time
  ),

  c(
    last_tp,
    forecast_mc$A_median
  ),

  col = "blue",
  lwd = 2,
  lty = 2
)


# ============================================================
# 14. MEDIAN FORECAST PATH - SCENARIO B
# ============================================================

lines(
  c(
    last_time,
    forecast_mc$time
  ),

  c(
    last_tp,
    forecast_mc$B_median
  ),

  col = "red",
  lwd = 2,
  lty = 2
)


points(
  forecast_mc$time,
  forecast_mc$A_median,
  col = "blue",
  pch = 16
)

points(
  forecast_mc$time,
  forecast_mc$B_median,
  col = "red",
  pch = 16
)


# ============================================================
# 15. LEGEND
# ============================================================

legend(
  "topleft",

  legend = c(
    "Actual TP",
    "Scenario A median",
    "Scenario B median",
    "80% simulated interval",
    "95% simulated interval"
  ),

  col = c(
    "black",
    "blue",
    "red",
    "grey50",
    "grey75"
  ),

  lty = c(
    1,
    2,
    2,
    NA,
    NA
  ),

  lwd = c(
    2,
    2,
    2,
    NA,
    NA
  ),

  pch = c(
    NA,
    NA,
    NA,
    15,
    15
  ),

  pt.cex = c(
    NA,
    NA,
    NA,
    2,
    2
  ),

  bty = "n"
)



# ============================================================
# 1. TIME AXIS FOR ROLLING OOS FORECASTS
# ============================================================

rolling$time <-
  rolling$annee +
  (rolling$trimestre - 1) / 4


# Keep recent rolling forecasts only for readability
rolling_plot <- rolling |>
  filter(annee >= 2023)


# ============================================================
# 2. FUTURE MONTE CARLO TIME AXIS
# ============================================================

forecast_mc$time <- c(
  2026.50,  # 2026 Q3
  2026.75,  # 2026 Q4
  2027.00,  # 2027 Q1
  2027.25   # 2027 Q2
)


# ============================================================
# 3. BASE PLOT: ACTUAL HISTORICAL TP
# ============================================================

plot(
  rolling_plot$time,
  rolling_plot$actual,
  type = "l",
  col = "black",
  lwd = 2,

  xlim = c(
    min(rolling_plot$time),
    max(forecast_mc$time)
  ),

  ylim = range(
    c(
      rolling_plot$actual,
      rolling_plot$pred_A,
      rolling_plot$pred_B,
      forecast_mc$A_lower95,
      forecast_mc$A_upper95,
      forecast_mc$B_lower95,
      forecast_mc$B_upper95
    ),
    na.rm = TRUE
  ),

  xlab = "Year",
  ylab = "TP volume index",

  main = "Rolling out-of-sample and future TP forecasts"
)


# ============================================================
# 4. HISTORICAL ROLLING OOS FORECASTS
# ============================================================

# Scenario A
lines(
  rolling_plot$time,
  rolling_plot$pred_A,
  col = "blue",
  lwd = 1.5,
  lty = 3
)

# Scenario B
lines(
  rolling_plot$time,
  rolling_plot$pred_B,
  col = "red",
  lwd = 1.5,
  lty = 3
)


# ============================================================
# 5. FUTURE 95% MONTE CARLO BANDS
# ============================================================

# Scenario A
polygon(
  c(
    forecast_mc$time,
    rev(forecast_mc$time)
  ),
  c(
    forecast_mc$A_lower95,
    rev(forecast_mc$A_upper95)
  ),
  col = rgb(0, 0, 1, 0.08),
  border = NA
)

# Scenario B
polygon(
  c(
    forecast_mc$time,
    rev(forecast_mc$time)
  ),
  c(
    forecast_mc$B_lower95,
    rev(forecast_mc$B_upper95)
  ),
  col = rgb(1, 0, 0, 0.08),
  border = NA
)


# ============================================================
# 6. FUTURE 80% MONTE CARLO BANDS
# ============================================================

# Scenario A
polygon(
  c(
    forecast_mc$time,
    rev(forecast_mc$time)
  ),
  c(
    forecast_mc$A_lower80,
    rev(forecast_mc$A_upper80)
  ),
  col = rgb(0, 0, 1, 0.18),
  border = NA
)

# Scenario B
polygon(
  c(
    forecast_mc$time,
    rev(forecast_mc$time)
  ),
  c(
    forecast_mc$B_lower80,
    rev(forecast_mc$B_upper80)
  ),
  col = rgb(1, 0, 0, 0.18),
  border = NA
)


# ============================================================
# 7. CONNECT LAST ACTUAL VALUE TO FUTURE FORECAST
# ============================================================

last_time <- tail(
  rolling_plot$time,
  1
)

last_actual <- tail(
  rolling_plot$actual,
  1
)


# Scenario A future median
lines(
  c(
    last_time,
    forecast_mc$time
  ),
  c(
    last_actual,
    forecast_mc$A_median
  ),
  col = "blue",
  lwd = 2,
  lty = 2
)


# Scenario B future median
lines(
  c(
    last_time,
    forecast_mc$time
  ),
  c(
    last_actual,
    forecast_mc$B_median
  ),
  col = "red",
  lwd = 2,
  lty = 2
)


# ============================================================
# 8. FUTURE FORECAST POINTS
# ============================================================

points(
  forecast_mc$time,
  forecast_mc$A_median,
  col = "blue",
  pch = 16
)

points(
  forecast_mc$time,
  forecast_mc$B_median,
  col = "red",
  pch = 16
)


# ============================================================
# 9. MARK START OF TRUE FUTURE FORECAST
# ============================================================

abline(
  v = 2026.5,
  lty = 3,
  col = "grey40"
)


# ============================================================
# 10. LEGEND
# ============================================================

legend(
  "topleft",

  legend = c(
    "Actual TP",
    "Scenario A rolling OOS",
    "Scenario B rolling OOS",
    "Scenario A future median",
    "Scenario B future median",
    "80% Monte Carlo interval",
    "95% Monte Carlo interval"
  ),

  col = c(
    "black",
    "blue",
    "red",
    "blue",
    "red",
    "grey50",
    "grey75"
  ),

  lty = c(
    1,
    3,
    3,
    2,
    2,
    NA,
    NA
  ),

  lwd = c(
    2,
    1.5,
    1.5,
    2,
    2,
    NA,
    NA
  ),

  pch = c(
    NA,
    NA,
    NA,
    NA,
    NA,
    15,
    15
  ),

  bty = "n"
)


library(dplyr)

# ============================================================
# 1. TIME AXIS
# ============================================================

# Rolling OOS forecasts
rolling <- rolling |>
  mutate(
    time = annee + (trimestre - 1) / 4
  )

# Keep only recent rolling period for readability
rolling_plot <- rolling |>
  filter(annee >= 2023)

# Monte Carlo future forecasts
forecast_mc$time <- c(
  2026.50,  # 2026 Q3
  2026.75,  # 2026 Q4
  2027.00,  # 2027 Q1
  2027.25   # 2027 Q2
)

# Historical actual TP up to 2026 Q2
historical_plot <- hist |>
  filter(annee >= 2023) |>
  mutate(
    time = annee + (trimestre - 1) / 4
  )

last_time <- tail(historical_plot$time, 1)
last_tp   <- tail(historical_plot$tp_volume_trimestriel, 1)


# ============================================================
# 2. OPTIONAL: TABLE OF FUTURE MONTE CARLO FORECASTS
# ============================================================

forecast_table <- data.frame(
  quarter   = c("2026 Q3", "2026 Q4", "2027 Q1", "2027 Q2"),

  A_median  = forecast_mc$A_median,
  A_low80   = forecast_mc$A_lower80,
  A_up80    = forecast_mc$A_upper80,
  A_low95   = forecast_mc$A_lower95,
  A_up95    = forecast_mc$A_upper95,

  B_median  = forecast_mc$B_median,
  B_low80   = forecast_mc$B_lower80,
  B_up80    = forecast_mc$B_upper80,
  B_low95   = forecast_mc$B_lower95,
  B_up95    = forecast_mc$B_upper95
)

print(round(forecast_table, 2))


# ============================================================
# 3. BASE PLOT
# ============================================================

plot(
  historical_plot$time,
  historical_plot$tp_volume_trimestriel,
  type = "l",
  col = "black",
  lwd = 2,

  xlim = c(min(historical_plot$time), max(forecast_mc$time)),

  ylim = range(
    c(
      historical_plot$tp_volume_trimestriel,
      rolling_plot$pred_A,
      rolling_plot$pred_B,
      forecast_mc$A_lower95,
      forecast_mc$A_upper95,
      forecast_mc$B_lower95,
      forecast_mc$B_upper95
    ),
    na.rm = TRUE
  ),

  xlab = "Year",
  ylab = "TP volume index",
  main = "TP forecasts: rolling OOS + Monte Carlo"
)


# ============================================================
# 4. ROLLING OUT-OF-SAMPLE FORECASTS
# ============================================================

# Scenario A rolling OOS
lines(
  rolling_plot$time,
  rolling_plot$pred_A,
  col = "blue",
  lwd = 1.5,
  lty = 3
)

# Scenario B rolling OOS
lines(
  rolling_plot$time,
  rolling_plot$pred_B,
  col = "red",
  lwd = 1.5,
  lty = 3
)


# ============================================================
# 5. MONTE CARLO FUTURE INTERVALS
# ============================================================

# ----- Scenario A: 95% band
polygon(
  c(forecast_mc$time, rev(forecast_mc$time)),
  c(forecast_mc$A_lower95, rev(forecast_mc$A_upper95)),
  col = rgb(0, 0, 1, 0.08),
  border = NA
)

# ----- Scenario A: 80% band
polygon(
  c(forecast_mc$time, rev(forecast_mc$time)),
  c(forecast_mc$A_lower80, rev(forecast_mc$A_upper80)),
  col = rgb(0, 0, 1, 0.18),
  border = NA
)

# ----- Scenario B: 95% band
polygon(
  c(forecast_mc$time, rev(forecast_mc$time)),
  c(forecast_mc$B_lower95, rev(forecast_mc$B_upper95)),
  col = rgb(1, 0, 0, 0.08),
  border = NA
)

# ----- Scenario B: 80% band
polygon(
  c(forecast_mc$time, rev(forecast_mc$time)),
  c(forecast_mc$B_lower80, rev(forecast_mc$B_upper80)),
  col = rgb(1, 0, 0, 0.18),
  border = NA
)


# ============================================================
# 6. MONTE CARLO FUTURE MEDIAN PATHS
# ============================================================

# Scenario A future median
lines(
  c(last_time, forecast_mc$time),
  c(last_tp, forecast_mc$A_median),
  col = "blue",
  lwd = 2,
  lty = 2
)

points(
  forecast_mc$time,
  forecast_mc$A_median,
  col = "blue",
  pch = 16
)

# Scenario B future median
lines(
  c(last_time, forecast_mc$time),
  c(last_tp, forecast_mc$B_median),
  col = "red",
  lwd = 2,
  lty = 2
)

points(
  forecast_mc$time,
  forecast_mc$B_median,
  col = "red",
  pch = 16
)


# ============================================================
# 7. SEPARATION LINE: FUTURE FORECAST STARTS AT 2026 Q3
# ============================================================

abline(
  v = 2026.50,
  col = "grey40",
  lty = 3
)


# ============================================================
# 8. LEGEND
# ============================================================

legend(
  "topleft",
  legend = c(
    "Actual TP",
    "Scenario A rolling OOS",
    "Scenario B rolling OOS",
    "Scenario A Monte Carlo median",
    "Scenario B Monte Carlo median",
    "80% Monte Carlo interval",
    "95% Monte Carlo interval"
  ),
  col = c(
    "black",
    "blue",
    "red",
    "blue",
    "red",
    "grey50",
    "grey75"
  ),
  lty = c(
    1,
    3,
    3,
    2,
    2,
    NA,
    NA
  ),
  lwd = c(
    2,
    1.5,
    1.5,
    2,
    2,
    NA,
    NA
  ),
  pch = c(
    NA,
    NA,
    NA,
    16,
    16,
    15,
    15
  ),
  pt.cex = c(
    NA,
    NA,
    NA,
    1,
    1,
    2,
    2
  ),
  bty = "n"
)

