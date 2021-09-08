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
    dev_dwnom1 = abs(nominate_dim1 - mean_dwnom1),
    pci2020 = pci2020 / 1000,
    vs = candidatevotes / totalvotes
  )
  
# Bivariate relations: DW-NOMINATE =============================================
cor.test(df$min, df$dev_dwnom1) ## n.s.
cor.test(df$mean, df$dev_dwnom1) ## n.s.
cor.test(df$max, df$dev_dwnom1) ## n.s.

broom_custom(lm(min ~ dev_dwnom1, data = df))
broom_custom(lm(mean ~ dev_dwnom1, data = df))
broom_custom(lm(max ~ dev_dwnom1, data = df))

# Adding covariates ============================================================
## Only (1) Republican, (2) state-level income
broom_custom(lm(min ~ dev_dwnom1 + office + party + inc + pci2020, data = df))
broom_custom(lm(mean ~ dev_dwnom1 + office + party + inc + pci2020, data = df))
broom_custom(lm(max ~ dev_dwnom1 + office + party + inc + pci2020, data = df))

# Scatterplots =================================================================
scatter_custom(df, "nominate_dim1", xlab = "DW-NOMINATE (Dim. 1)")
scatter_custom(
  df, "pci2020", xlab = "State-level Per Capital Income (1,000 USD)"
)
scatter_custom(df, "vs", xlab = "2020 General Vote Share")

broom_custom(lm(dev_dwnom1 ~ office + party, data = df))
