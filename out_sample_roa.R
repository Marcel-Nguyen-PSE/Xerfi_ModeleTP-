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
