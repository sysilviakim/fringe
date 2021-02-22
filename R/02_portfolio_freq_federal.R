categories <- c("president", "senate", "house")
source(here::here("R", "01_data_import.R"))

# All federal records, regardless of the data generating process ===============
dl <- df_ls %>%
  imap(
    ~ {
      if (.y == "president") {
        ex <- c("last_name", "url", "year")
      } else if (.y == "senate") {
        ex <- c("state", "last_name", "url", "year")
      } else {
        ex <- c("state", "state_cd", "last_name", "url", "year")
      }
      .x %>%
        ungroup() %>%
        mutate(year = 2020) %>%
        filter(!is.na(url) & url != "") %>%
        portfolio_summ(exclude_cols = ex, order_vars = ex)
    }
  )
save(dl, file = here("data/tidy/portfolio_summ_federal.Rda"))

# Keep only the first URL per date =============================================
dl <- df_ls %>%
  imap(
    ~ {
      if (.y == "president") {
        ex <- c("last_name", "url", "year")
      } else if (.y == "senate") {
        ex <- c("last_name", "url", "year", "state")
      } else {
        ex <- c("last_name", "url", "year", "state", "state_cd")
      }
      .x %>%
        ungroup() %>%
        mutate(year = 2020) %>%
        filter(!is.na(url) & url != "") %>%
        # Don't filter for !is.na(portfolio) just yet
        group_by(across(c(setdiff(ex, "url"), "date"))) %>%
        # First URL recorded for a given date/amount
        filter(url == first(url)) %>%
        group_by(across(c(setdiff(ex, "url"), "date", "portfolio"))) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(
          url = case_when(
            last_name == "de Blasio" & grepl("\\|", url) ~
              gsub("\\|.*?$", "", url),
            TRUE ~ url
          )
        ) %>%
        portfolio_summ(exclude_cols = ex, order_vars = ex)
    }
  )
save(dl, file = here("data/tidy/portfolio_summ_federal_first_only.Rda"))
