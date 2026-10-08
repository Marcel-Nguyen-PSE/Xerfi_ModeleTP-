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

set.seed(123)

hist <- d |>
  arrange(annee, trimestre) |>
  filter(
    annee < 2026 |
    (annee == 2026 & trimestre <= 2)
  )

n_hist <- nrow(hist)

B <- 5000
H <- 4

quarters <- c(
  "2026 Q3",
  "2026 Q4",
  "2027 Q1",
  "2027 Q2"
)

sigma_A <- sigma(m_A)
sigma_B <- sigma(m_B)

sigma_swi <- sigma(m_swi)
sigma_tunnels <- sigma(m_tunnels)

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

for (b in 1:B) {

  tp_A <- hist$tp_volume_trimestriel
  tp_B <- hist$tp_volume_trimestriel

  swi_sim <- hist$swi_trimestriel
  tunnel_sim <- hist$ica_tunnels

  routes <- hist$ica_routes

  for (h in 1:H) {

    i <- n_hist + h

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
    # SCENARIO A
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
    # SCENARIO B
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

