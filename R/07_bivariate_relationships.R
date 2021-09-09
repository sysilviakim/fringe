source(here::here("R", "utilities.R"))
load(here("data", "tidy", "cong-filtered.Rda"))

# Define deviations from party mean ============================================
df <- cong_filtered %>%
  bind_rows(.id = "office") %>%
  group_by(party) %>%
  mutate(
    ## Because abs(df$nominate_dim1) is higher for REPs,
    ## Using just abs(df$nominate_dim1) does not make sense
    mean_dwnom1 = mean(nominate_dim1, na.rm = TRUE),
    ## Deviations from the party's average
    ## If Rep average is 0.4 and candidate dwnom1 is 0.7, 0.3
    ## If Dem average is -0.4 and candidate dwnom1 is -0.7, also 0.3
    dw = case_when(
      party == "REPUBLICAN" ~ nominate_dim1 - mean_dwnom1,
      party == "DEMOCRAT" ~ -(nominate_dim1 - mean_dwnom1)
    ),
    pci2020 = pci2020 / 1000,
    vs = candidatevotes / totalvotes,
    safe = case_when(
      party == "DEMOCRAT" ~ PVI,
      party == "REPUBLICAN" ~ -PVI
    )
  ) %>%
  filter(party != "INDEPENDENT")

## p = 0.1
ks.test(
  df %>% filter(party == "DEMOCRAT" & !is.na(dw)) %>% .$dw,
  df %>% filter(party == "REPUBLICAN" & !is.na(dw)) %>% .$dw
)

# Bivariate relations: DW-NOMINATE =============================================
cor.test(df$min, df$dw) ## n.s.
cor.test(df$mean, df$dw) ## n.s.
cor.test(df$max, df$dw) ## n.s.

broom_custom(lm(min ~ dw, data = df))
broom_custom(lm(mean ~ dw, data = df))
broom_custom(lm(max ~ dw, data = df))

# Bivariate relations: safety based on PVI measure =============================
cor.test(df$min, df$safe) ## 0.071
cor.test(df$mean, df$safe) ## 0.126
cor.test(df$max, df$safe) ## 0.117

broom_custom(lm(min ~ safe, data = df))
broom_custom(lm(mean ~ safe, data = df))
broom_custom(lm(max ~ safe, data = df))

# Adding covariates ============================================================
broom_custom(lm(min ~ dw + office + party + inc + pci2020 + safe, data = df))
broom_custom(lm(mean ~ dw + office + party + inc + pci2020 + safe, data = df))
broom_custom(lm(max ~ dw + office + party + inc + pci2020 + safe, data = df))

## Stargazer export: with and without dwnom1
lm1 <- lm(min ~ office + party + inc + pci2020 + safe, data = df)
lm2 <- lm(min ~ office + party + inc + pci2020 + safe + dw, data = df)
lm3 <- lm(mean ~ office + party + inc + pci2020 + safe, data = df)
lm4 <- lm(mean ~ office + party + inc + pci2020 + safe + dw, data = df)
lm5 <- lm(max ~ office + party + inc + pci2020 + safe, data = df)
lm6 <- lm(max ~ office + party + inc + pci2020 + safe + dw, data = df)

stargazer(
  lm1, lm2, lm3, lm4, lm5, lm6,
  covariate.labels = c(
    "Senate", "Republican", "Incumbent", "Open Seat", 
    "State-level Per Capita Income in 2020 (1,000 USD)",
    "Electoral Safety Based On Cook PVI",
    "Ideological Extremity Based On DW-NOMINATE"
  ),
  se = starprep(lm1, lm2, lm3, lm4, lm5, lm6, se_type = "stata"),
  omit = "Constant", header = FALSE, model.numbers = FALSE,
  float = FALSE, omit.stat = c("f", "ser"), star.cutoffs = c(0.05, 0.01, 0.001),
  out = here("tab", "pred_summ.tex")
)

# Scatterplots =================================================================
scatter_custom(df, "nominate_dim1", xlab = "DW-NOMINATE (Dim. 1)")
scatter_custom(df, "dw", xlab = "Ideological Extremity Based On DW-NOMINATE")
scatter_custom(
  df, "pci2020", xlab = "State-level Per Capita Income in 2020 (1,000 USD)"
)
scatter_custom(df, "vs", xlab = "2020 General Vote Share")
scatter_custom(df, "safe", xlab = "Electoral Safety Based On Cook PVI")

broom_custom(lm(dw ~ office + party, data = df))
