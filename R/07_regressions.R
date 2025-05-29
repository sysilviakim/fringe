source(here::here("R", "utilities.R"))
load(here("data", "tidy", "cong-filtered.Rda"))

# Define deviations from party mean ============================================
df <- cong_filtered %>%
  bind_rows(.id = "office") %>%
  filter(party != "INDEPENDENT") %>%
  mutate(
    dw = case_when(
      party == "REPUBLICAN" ~ nominate_dim1,
      party == "DEMOCRAT" ~ -nominate_dim1
    ),
    pci2020 = pci2020 / 1000,
    vs = candidatevotes / totalvotes,
    safe = case_when(
      party == "DEMOCRAT" ~ PVI,
      party == "REPUBLICAN" ~ -PVI
    )
  ) %>%
  mutate(Party = simple_cap(tolower(party))) %>%
  mutate(platform = actblue + winred)

assert_that(all(df$platform < 2))
table(df$office, useNA = "ifany") ## 823 house, 141 senate

## p = 0.1366
ks.test(
  df %>% filter(party == "DEMOCRAT" & !is.na(dw)) %>% .$dw,
  df %>% filter(party == "REPUBLICAN" & !is.na(dw)) %>% .$dw
)

df %>%
  select(dw, nominate_dim1, inc, office, cand_name, url) %>%
  filter(!is.na(nominate_dim1)) %>%
  arrange(nominate_dim1) %>%
  View()

# Bivariate relations: DW-NOMINATE =============================================
cor.test(df$min, df$dw) ## n.s.
cor.test(df$mean, df$dw) ## n.s.
cor.test(df$max, df$dw) ## n.s.

# ggplot export ================================================================
p <- ggplot(df, aes(x = dw, y = mean)) +
  geom_point(aes(colour = Party)) +
  scale_color_manual(values = c("#0571b0", "#ca0020")) +
  xlab("Ideological Extremity") +
  ylab("Average of Suggested Amounts") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black") +
  facet_wrap(~Party)

pdf(here("fig", "scatter_dw_mean.pdf"), width = 4.5, height = 3.5)
print(plot_nolegend(pdf_default(p)))
dev.off()

p <- ggplot(df, aes(x = Party, y = mean)) +
  geom_boxplot(aes(colour = Party), notch = TRUE) +
  scale_color_manual(values = c("#0571b0", "#ca0020")) +
  xlab("Party") +
  ylab("Average of Suggested Amounts")

pdf(here("fig", "box_party_mean.pdf"), width = 4.5, height = 3.5)
print(plot_nolegend(pdf_default(p)))
dev.off()

# Bivariate relations: safety based on PVI measure =============================
cor.test(df$min, df$safe) ## 0.03056
cor.test(df$mean, df$safe) ## 0.0001156
cor.test(df$max, df$safe) ## 0.0003556

## Stargazer export: with and without dwnom1
lm_bunch <- function(df,
                     cov_bench =
                       "office + party + platform + inc + pci2020 + safe") {
  lm1 <- lm(as.formula(paste("min ~ ", "dw + ", cov_bench)), data = df)
  lm2 <- lm(as.formula(paste("min ~ ", "party * dw + ", cov_bench)), data = df)
  lm3 <- lm(as.formula(paste("mean ~ ", "dw + ", cov_bench)), data = df)
  lm4 <- lm(as.formula(paste("mean ~ ", "party * dw + ", cov_bench)), data = df)
  lm5 <- lm(as.formula(paste("max ~ ", "dw + ", cov_bench)), data = df)
  lm6 <- lm(as.formula(paste("max ~ ", "party * dw + ", cov_bench)), data = df)
  return(list(lm1, lm2, lm3, lm4, lm5, lm6))
}

stargazer_wrapper <-
  function(lm_list,
           out,
           cov_labels = c(
             "Ideological Extremity",
             "Senate", "Republican", "Used ActBlue/WinRed",
             "Incumbent", "Open Seat",
             "State Avg. Per Capita Income (1,000 USD, 2020)",
             "Electoral Safety",
             "Republican $\\times$ Ideological Extremity"
           )) {
    stargazer(
      lm_list,
      covariate.labels = cov_labels,
      dep.var.labels = c("Min.", "Min.", "Mean", "Mean", "Max.", "Max."),
      se = starprep(lm_list, se_type = "stata"),
      omit = "Constant",
      header = FALSE,
      model.numbers = FALSE,
      float = FALSE,
      omit.stat = c("f", "ser"),
      star.cutoffs = c(0.05, 0.01, 0.001),
      out = out,
      dep.var.caption =
        "Dependent Variable: Summary Statistics of Suggested Amounts"
    )
  }

stargazer_wrapper(lm_bunch(df), here("tab", "pred_summ_3vars_only_dw.tex"))
stargazer_wrapper(
  lm_bunch(
    df %>% filter(office == "senate"),
    cov_bench = "party + platform + inc + pci2020 + safe"
  ),
  here("tab", "pred_summ_3vars_only_dw_senate.tex"),
  cov_labels = c(
    "Ideological Extremity", "Republican", "Used ActBlue/WinRed",
    "Incumbent", "Open Seat", "State Avg. Per Capita Income (1,000 USD, 2020)",
    "Electoral Safety", "Republican $\\times$ Ideological Extremity"
  )
)
stargazer_wrapper(
  lm_bunch(
    df %>% filter(office == "house"),
    cov_bench = "party + platform + inc + pci2020 + safe"
  ),
  here("tab", "pred_summ_3vars_only_dw_house.tex"),
  cov_labels = c(
    "Ideological Extremity", "Republican", "Used ActBlue/WinRed",
    "Incumbent", "Open Seat", "State Avg. Per Capita Income (1,000 USD, 2020)",
    "Electoral Safety", "Republican $\\times$ Ideological Extremity"
  )
)

# Scatterplots =================================================================
df <- df %>% select(-Party)
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
  ggtitle = "Democrat, Used ActBlue", fill = "#0571B0"
)
p2 <- top5(
  df %>% filter(party == "DEMOCRAT" & actblue == 0),
  ggtitle = "Democrat, Did Not Use ActBlue", fill = "#92C5DE"
)
p3 <- top5(
  df %>% filter(party == "REPUBLICAN" & winred == 1),
  ggtitle = "Republican, Used WinRed", fill = "#CA0020"
)
p4 <- top5(
  df %>% filter(party == "REPUBLICAN" & winred == 0),
  ggtitle = "Republican, Did Not Use WinRed", fill = "#F4A582"
)

pdf(here("fig", "congress_by_party_platform_top5.pdf"), width = 7, height = 6)
(pdf_default(p1) | pdf_default(p2)) /
  (pdf_default(p3) | pdf_default(p4))
dev.off()

# Who's missing the suggestions? ===============================================
df <- df %>% mutate(missing_suggestions = case_when(is.na(min) ~ 1, TRUE ~ 0))

df %>%
  group_by(missing_suggestions, party) %>%
  summarise(
    nominate = mean(nominate_dim1, na.rm = TRUE),
    n = n()
  )

temp <- df %>%
  filter(party == "REPUBLICAN") %>%
  filter(!is.na(dw))
t.test(nominate_dim1 ~ missing_suggestions, data = temp)

# By incumbency status =========================================================
df %>%
  group_by(party, inc) %>%
  summarise(
    min = mean(min, na.rm = TRUE),
    mean = mean(mean, na.rm = TRUE),
    max = mean(max, na.rm = TRUE)
  )
