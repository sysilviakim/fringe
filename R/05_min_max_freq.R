source(here::here("R", "utilities.R"))

# No need to import raw data ===================================================
categories <- c("winred", "rightus", "actblue")
load(here("data/tidy/portfolio_summ_platforms.Rda"))

# No-prompt links ==============================================================
categories %>%
  set_names(., .) %>%
  map(
    ~ nrow(dl[[.x]] %>% filter(is.na(amount) & !is.na(url) & url != "")) /
      nrow(dl[[.x]] %>% filter(!is.na(url) & url != "")) * 100
  ) %>%
  unlist()
# winred rightus actblue 
#      0       0       0

# Single-prompt links ==========================================================
categories %>%
  set_names(., .) %>%
  map(
    ~ nrow(
      dl[[.x]] %>%
        filter(!is.na(amount) & !grepl("-", amount) & !is.na(url) & url != "")
    ) /
      nrow(dl[[.x]] %>% filter(!is.na(url) & url != "")) * 100
  ) %>%
  unlist()
#    winred   rightus   actblue
# 0.2070393 0.0000000 0.8990713

# Restricted to federal elections
categories %>%
  set_names(., .) %>%
  map(
    ~ nrow(
      dl[[.x]] %>%
        filter(
          !is.na(amount) & !grepl("-", amount) &
            !is.na(url) & url != "" & grepl("us |pres", class)
        )
    ) /
      nrow(
	    dl[[.x]] %>% filter(!is.na(url) & url != "" & grepl("us |pres", class))
	  ) * 100
  ) %>%
  unlist()
#    winred   rightus   actblue
# 0.0000000 0.0000000 0.3996427

# Others
categories %>%
  set_names(., .) %>%
  map(
    ~ nrow(
      dl[[.x]] %>%
        filter(
          !is.na(amount) & !grepl("-", amount) &
            !is.na(url) & url != "" & !grepl("us |pres", class)
        )
    ) /
      nrow(
	    dl[[.x]] %>% filter(!is.na(url) & url != "" & !grepl("us |pres", class))
	  ) * 100
  ) %>%
  unlist()
#   winred  rightus  actblue
# 2.631579 0.000000 1.059102

# Multi-entity cases for ActBlue? 0.927305 vs. 0.3641329 so about 2.5 more time
# for single-entity fundraisers, but very little anyway
nrow(
  dl$actblue %>%
    filter(
      !is.na(amount) & !grepl("-", amount) &
        !is.na(url) & url != "" & multi_entity == 0
    )
) /
  nrow(dl$actblue %>% filter(!is.na(url) & url != "" & multi_entity == 0)) * 100
# [1] 0.927305

nrow(
  dl$actblue %>% filter(
    !is.na(amount) & !grepl("-", amount) &
      !is.na(url) & url != "" & multi_entity > 0
  )
) /
  nrow(dl$actblue %>% filter(!is.na(url) & url != "" & multi_entity > 0)) * 100
# [1] 0.3641329

# Modal menu percentage ========================================================
prop(dl$actblue, "amount", sort = TRUE, head = 1)
prop(dl$winred, "amount", sort = TRUE, head = 1)
prop(dl$rightus, "amount", sort = TRUE, head = 1)

# Frequency of maximum value by platform =======================================
max(dl$actblue$max) # 250,000
min(dl$actblue$max) # 1
prop(dl$actblue, "max", sort = TRUE, head = 5)
# max
# 1000  500  250  100 2800
# 57.7  9.7  6.2  5.4  2.2

max(dl$winred$max) # 250,000
min(dl$winred$max) # 1
prop(dl$winred, "max", sort = TRUE, head = 5)

max(dl$rightus$max)
min(dl$rightus$max)
prop(dl$rightus, "max", sort = TRUE, head = 5)

# Frequency of maximum value by platform =======================================
max(dl$actblue$min)
min(dl$actblue$min)
prop(dl$actblue, "min", sort = TRUE, head = 5)
