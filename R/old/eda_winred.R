categories <- c("winred") # , "rightus", "actblue"
source("R/01_data_import.R")

# Any empty URLs? ==============================================================
temp <- df_ls$winred %>% filter(is.na(url))

# Yes, and 3 with completely empty data <--- very minor candidates
# e.g., https://ballotpedia.org/A._Wayne_Johnson
setdiff(temp$name, df_ls$winred %>% filter(!is.na(url)) %>% .$name)
# [1] "Matthew Burril"             "Wayne Johnson"
# [3] "Janet Barresi for Congress"

# Does WinRed directory contain changing URLs or are they static? ==============

temp <- df_ls$winred %>%
  group_by(name, race, url) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(name, race) %>%
  filter(n() > 1)

# 78.1% (649 out of 831) entities change URL link at some point in time
round(
  length(unique(temp$name)) / length(unique(df_ls$winred$name)) * 100,
  digits = 1
)

# Does any WinRed entity have multiple URLs in the same day? ===================
temp <- df_ls$winred %>%
  group_by(name, race, url, date) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(name, race, date) %>%
  filter(n() > 1)

# Oh yikes, yes e.g., Michelle Steel, Young Kim, Lindsey Graham
nrow(temp)

# Any entities across multiple races? ==========================================
temp <- df_ls$winred %>%
  group_by(name, race) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(name) %>%
  filter(n() > 1)

# Oh yikes, yes; couple errors, couple real deal
# Alison Hayden CA-16 should be changed to CA-15
# Jimmy Rodriguez really ran for both AZ and VT: keep only VT
# https://ballotpedia.org/Jimmy_Rodriguez
# Liz Marty May and Matthew Morris is just a matter of at-large districts
# SD-1 ---> SD-AL, DE-NaN ---> DE-AL
# Scott Perry should be PA-10 instead of PA-4 (previous held position)
# Vern Buchanan should be FL-16 instead of FL-13 (previous held position)
nrow(temp)

# Any amount changes within same URL? ==========================================
wide_date <- df_ls$winred %>%
  # To kick out duplicates
  mutate(url = gsub("\\?sc=winred-directory", "", url)) %>%
  mutate(name = gsub(" for Congress", "", name)) %>%
  mutate(
    race = ifelse(name == "Alison Hayden" & race == "CA-16", "CA-15", race),
    race = ifelse(name == "Liz Marty May" & race == "SD-1", "SD-AL", race),
    race = ifelse(name == "Matthew Morris" & race == "DE-NaN", "DE-AL", race),
    race = ifelse(name == "Scott Perry" & race == "PA-4", "PA-10", race),
    race = ifelse(name == "Vern Buchanan" & race == "FL-13", "FL-16", race)
  ) %>%
  filter(!(name == "Jimmy Rodriguez" & race == "AZ-8")) %>%
  filter(!is.na(url)) %>%
  pivot_wider(
    names_from = date,
    values_from = portfolio,
    names_prefix = "date_",
    values_fn = list
  ) %>%
  clean_names() %>%
  arrange(name, url)

# 49: same entity, multiple URLs
sum(duplicated(wide_date$name))

# Amount changes within URL: entities/portfolio ================================
temp <- cross2(
  wide_date %>% select(-name, -race, -url) %>% names(),
  seq(nrow(wide_date))
) %>%
  map(
    ~ {
      out <- list()
      out[[paste0("row_", str_pad(.x[[2]], width = 3, pad = "0"))]] <-
        wide_date[.x[[2]], .x[[1]], drop = TRUE]
      return(out)
    }
  )

menu_list <- seq(nrow(wide_date)) %>%
  map(
    ~ unique(
      temp[
        seq(1, length(temp), by = (ncol(wide_date) - 3))[.x]:
          seq(
            ncol(wide_date) - 3, length(temp),
            by = (ncol(wide_date) - 3)
          )[.x]
      ]
    ) %>%
      bind_rows() %>%
      .[, 1, drop = TRUE]
  ) %>%
  map(~ Filter(Negate(is.null), .x)) %>%
  map(~ Filter(function(x) any(!is.na(x)), .x))

menu_freq <- menu_list %>%
  map(length) %>%
  unlist()

# 77 cases have portfolio change within the same URL
which(menu_freq > 1)
length(which(menu_freq > 1))
menu_list[which(menu_freq > 1)]

# 3 cases have no amount associated with URL: e.g., a Thank You page after loss 
which(menu_freq < 1)
length(which(menu_freq < 1))
wide_date[which(menu_freq < 1), "url"]

# Alan Swain: checked with screenshots
swain <- wide_date[8, ] %>%
  select(-name, -race, -url) %>% 
  map(~ paste(unlist(.x), collapse = "-")) %>% 
  unlist() %>%
  .[. != ""]

# Best set of code to extract portfolio
seq(unique(rleid(swain))) %>%
  set_names(., .) %>%
  map(
    ~ tibble(
      min = names(swain)[min(which(.x == rleid(swain)))],
      max = names(swain)[max(which(.x == rleid(swain)))],
      amount = wide_date[
        8, names(swain)[min(which(.x == rleid(swain)))], drop = TRUE
      ] %>% 
        unlist() %>%
        paste0(collapse = "-")
    )
  ) %>%
  bind_rows(.id = "seq") %>%
  mutate_at(vars("min", "max"), ~ ymd(gsub("date|_", "", .x)))
