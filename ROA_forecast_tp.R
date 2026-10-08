library(dplyr)

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

hist <- d |>
  filter(
    annee < 2026 |
    (annee == 2026 & trimestre <= 2)
  )

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

m_swi_ar1 <- lm(
  swi_trimestriel ~ swi_lag1,
  data = hist
)

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

tp_A <- ext$tp_volume_trimestriel
tp_B <- ext$tp_volume_trimestriel

ext$pred_A <- NA_real_
ext$pred_B <- NA_real_


future_id <- which(
  (ext$annee == 2026 & ext$trimestre >= 3) |
  ext$annee == 2027
)

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
