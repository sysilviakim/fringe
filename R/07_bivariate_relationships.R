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

ggplot(df, aes(x = dw, y = mean)) + 
  geom_point()

# Bivariate relations: safety based on PVI measure =============================
cor.test(df$min, df$safe) ## 0.071
cor.test(df$mean, df$safe) ## 0.126
cor.test(df$max, df$safe) ## 0.117

## Stargazer export: with and without dwnom1
cov_bench <- " ~ office + party + inc + pci2020 + safe + actblue + winred"
lm1 <- lm(as.formula(paste("min", cov_bench)), data = df)
lm2 <- lm(as.formula(paste("min", cov_bench, " + party * dw")), data = df)
lm3 <- lm(as.formula(paste("mean", cov_bench)), data = df)
lm4 <- lm(as.formula(paste("mean", cov_bench, " + party * dw")), data = df)
lm5 <- lm(as.formula(paste("max", cov_bench)), data = df)
lm6 <- lm(as.formula(paste("max", cov_bench, " + party * dw")), data = df)

stargazer(
  lm1, lm2, lm3, lm4, lm5, lm6,
  covariate.labels = c(
    "Senate", "Republican Party", "Incumbent", "Open Seat",
    "State-level Average Per Capita Income (1,000 USD, 2020)",
    "Electoral Safety Based On Cook PVI", "Used ActBlue", "Used WinRed",
    "Ideological Extremity Based On DW-NOMINATE",
    "Ideological Extremity $\\times$ Republican Party"
  ),
  dep.var.labels = c("min", "min", "mean", "mean", "max", "max"),
  se = starprep(lm1, lm2, lm3, lm4, lm5, lm6, se_type = "stata"),
  omit = "Constant", header = FALSE, model.numbers = FALSE,
  float = FALSE, omit.stat = c("f", "ser"), star.cutoffs = c(0.05, 0.01, 0.001),
  out = here("tab", "pred_summ.tex")
)

# Scatterplots =================================================================
scatter_custom(df, "nominate_dim1", xlab = "DW-NOMINATE (Dim. 1)")
scatter_custom(df, "dw", xlab = "Ideological Extremity Based On DW-NOMINATE")
scatter_custom(
  df, "pci2020",
  xlab = "State-level Per Capita Income in 2020 (1,000 USD)"
)
scatter_custom(df, "vs", xlab = "2020 General Vote Share")
scatter_custom(df, "safe", xlab = "Electoral Safety Based On Cook PVI")

broom_custom(lm(dw ~ office + party, data = df))

# Sanders legacy Dems: higher extremism? =======================================
temp <- df %>%
  filter(party == "DEMOCRAT" & !is.na(sanders))

temp %>%
  group_by(sanders) %>%
  summarise(dwnom1 = mean(nominate_dim1, na.rm = TRUE))

## Caveat: only 17 obs vs. rest
t.test(nominate_dim1 ~ sanders, data = temp, alternative = "greater")
ks.test(
  temp %>% filter(sanders == 0) %>% .$nominate_dim1,
  temp %>% filter(sanders == 1) %>% .$nominate_dim1
)

# Note that safety and extremism are highly correlated =========================
cor.test(df$dw, df$safe)
plot(df$dw, df$safe)

# By party/platform Top 5 ======================================================
p1 <- top5(
  df %>% filter(party == "DEMOCRAT" & actblue == 1),
  ggtitle = "Democrat, Used ActBlue"
)
p2 <- top5(
  df %>% filter(party == "DEMOCRAT" & actblue == 0),
  ggtitle = "Democrat, Did Not Use ActBlue"
)
p3 <- top5(
  df %>% filter(party == "REPUBLICAN" & winred == 1),
  ggtitle = "Republican, Used WinRed"
)
p4 <- top5(
  df %>% filter(party == "REPUBLICAN" & winred == 0),
  ggtitle = "Republican, Did Not Use WinRed"
)

library(patchwork)

pdf(here("fig", "congress_by_party_platform_top5.pdf"), width = 7, height = 6)
(pdf_default(p1) | pdf_default(p2)) /
  (pdf_default(p3) | pdf_default(p4))
dev.off()
