library(dplyr)
library(readxl)

d <- as.data.frame(read_excel(
  "base_tp_regressions_trimestrielle.xlsx",
  sheet = "Donnees_regression"
))

d <- d[order(d$annee, d$trimestre), ]

z <- d[
  complete.cases(
    d[, c(
      "tp_volume_trimestriel",
      "marches_tp_volume",
      "tp01_trimestriel",
      "ica_terrassement",
      "ica_routes",
      "ica_voies_ferrees",
      "ica_reseaux_fluides",
      "ica_reseaux_elec_telecom",
      "ica_ouvrages_art",
      "ica_tunnels",
      "ica_maritime_fluvial",
      "ica_forages_sondages",
      "swi_trimestriel",
      "fbcf_snf",
      "fbcf_apu",
      "regime_fntp_2016",
      "covid_2020_t2"
    )]
  ),
]

pca_data <- z[, c(
  "ica_terrassement",
  "ica_routes",
  "ica_voies_ferrees",
  "ica_reseaux_fluides",
  "ica_reseaux_elec_telecom"
)]

pca_tp <- prcomp(
  pca_data,
  center = TRUE,
  scale. = TRUE
)

summary(pca_tp)

pca_tp$rotation

z$PC1 <- pca_tp$x[, 1]
z$PC2 <- pca_tp$x[, 2]
z$PC3 <- pca_tp$x[, 3]
z$PC4 <- pca_tp$x[, 4]
z$PC5 <- pca_tp$x[, 5]


m_pc1 <- lm(
  tp_volume_trimestriel ~
    marches_tp_volume +
    PC1 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = z
)

m_pc2 <- lm(
  tp_volume_trimestriel ~
    marches_tp_volume +
    PC1 + PC2 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = z
)

m_pc3 <- lm(
  tp_volume_trimestriel ~
    marches_tp_volume +
    PC1 + PC2 + PC3 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = z
)

m_pc4 <- lm(
  tp_volume_trimestriel ~
    marches_tp_volume +
    PC1 + PC2 + PC3 + PC4 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = z
)

m_pc5 <- lm(
  tp_volume_trimestriel ~
    marches_tp_volume +
    PC1 + PC2 + PC3 + PC4 + PC5 +
    regime_fntp_2016 +
    covid_2020_t2,
  data = z
)

comparison_pc <- data.frame(
  model = c(
    "PC1",
    "PC1 + PC2",
    "PC1 + PC2 + PC3",
    "PC1 + PC2 + PC3 + PC4",
    "All 5 PCs",
    "Original ICA variables"
  ),

  Adjusted_R2 = c(
    summary(m_pc1)$adj.r.squared,
    summary(m_pc2)$adj.r.squared,
    summary(m_pc3)$adj.r.squared,
    summary(m_pc4)$adj.r.squared,
    summary(m_pc5)$adj.r.squared
  ),

  RMSE = c(
    sqrt(mean(residuals(m_pc1)^2)),
    sqrt(mean(residuals(m_pc2)^2)),
    sqrt(mean(residuals(m_pc3)^2)),
    sqrt(mean(residuals(m_pc4)^2)),
    sqrt(mean(residuals(m_pc5)^2))
  ),

  AIC = c(
    AIC(m_pc1),
    AIC(m_pc2),
    AIC(m_pc3),
    AIC(m_pc4),
    AIC(m_pc5)
  ),

  BIC = c(
    BIC(m_pc1),
    BIC(m_pc2),
    BIC(m_pc3),
    BIC(m_pc4),
    BIC(m_pc5)
  )
)

summary(pca_tp)
summary(m_pc3)
pca_tp$rotation



