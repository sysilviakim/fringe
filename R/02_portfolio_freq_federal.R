categories <- c("president", "senate", "house")
source(here::here("R", "01_scraped_data_import.R"))

# Anomalies ====================================================================
df_ls <- df_ls %>%
  map(
    ~ .x %>%
      mutate(
        # de Blasio in presidential race
        # Young Kim in house race
        # Probably happened because I used lists at some point
        url = gsub(" .*?$|\\|.*?$", "", url),
      )
  )

df_ls$senate <- df_ls$senate %>%
  mutate(
    state = case_when(
      last_name == "Rubio" & state == "" ~ "FL",
      last_name == "Jordan" & state == "CA" ~ "ID",
      last_name == "Murphy" & state == "CA" ~ "LA",
      TRUE ~ state
    )
  )

# All federal records, regardless of the data generating process ===============
dl <- df_ls %>%
  imap(
    ~ {
      ex <- fed_exception_vars(.y)
      .x %>%
        ungroup() %>%
        mutate(year = 2020) %>%
        filter(!is.na(url) & url != "") %>%
        group_split(across(all_of(ex))) %>%
        map_dfr(~ portfolio_summ(.x, order_vars = ex)) %>%
        arrange(across(c(setdiff(all_of(ex), "url"), "min")))
    }
  )

dl %>% map(~ assert_that(all(.x$min <= .x$max)))
save(dl, file = here("data/tidy/portfolio_summ_federal.Rda"))

# Keep only the first URL per date =============================================
dl <- df_ls %>%
  imap(
    ~ {
      ex <- fed_exception_vars(.y)
      .x %>%
        ungroup() %>%
        mutate(year = 2020, portfolio = as.numeric(portfolio)) %>%
        filter(!is.na(url) & url != "") %>%
        # Don't filter for !is.na(portfolio) just yet
        group_by(across(c(setdiff(all_of(ex), "url"), "date"))) %>%
        # First URL recorded for a given date/amount
        filter(url == first(url)) %>%
        group_by(across(c(setdiff(all_of(ex), "url"), "date", "portfolio"))) %>%
        # This enforces reordering from smallest number, however
        slice(1) %>%
        ungroup() %>%
        group_split(across(all_of(ex))) %>%
        map_dfr(~ portfolio_summ(.x, order_vars = ex)) %>%
        arrange(across(c(setdiff(all_of(ex), "url"), "min")))
    }
  )

dl %>% map(~ assert_that(all(.x$min <= .x$max)))
save(dl, file = here("data/tidy/portfolio_summ_federal_first_only.Rda"))

# Create summary statistics ====================================================
dl <- dl %>%
  map(
    ~ .x %>%
      mutate(
        amount = case_when(
          amount == "0-1-2-3" ~ NA_character_,
          amount == "-999" ~ NA_character_,
          TRUE ~ amount
        )
      ) %>%
      mutate(
        amount = case_when(
          last_name == "Sewell" & grepl("2020-", amount) ~
            gsub("2020-", "20.20-", amount),
          TRUE ~ amount
        )
      )
  ) %>%
  map(summ_calc_fxn) %>%
  imap(
    ~ left_join(
      .x,
      df_ls[[.y]] %>%
        select(
          c(
            starts_with("state"), ends_with("name"),
            FEC_ID_cand, party, incumbent, contains("class")
          )
        ) %>%
        dedup() %>%
        filter(!is.na(incumbent))
    )
  )

save(dl, file = here("data/tidy/portfolio_summ_federal_final_2020.Rda"))

# Create figures ===============================================================
dl %>%
  imap(
    ~ {
      ex <- fed_exception_vars(.y)
      .x <- portfolio_na_fig_label(.x)

      if (.y != "president") {
        .x <- .x %>%
          group_by(across(all_of(ex))) %>%
          filter(as.Date("2020-11-01") < last(max)) %>%
          arrange(across(all_of(ex))) %>%
          # This must be changed later; delete NA records if nested btw
          # valid records
          group_by(across(c(all_of(ex), "amount"))) %>%
          filter(
            !(n_distinct(amount) > 1 & amount == "No\nSuggested\nAmounts")
          ) %>%
          slice(1) %>%
          filter(!grepl("2022", url))
      }

      p <- prop(.x, "amount", sort = TRUE, head = 5, print = FALSE) %>%
        unlist() %>%
        set_names(., nm = names(.)) %>%
        imap(~ tibble(label = .y, freq = as.numeric(.x))) %>%
        bind_rows() %>%
        mutate(
          label = gsub("-", "\n", label),
          label = factor(
            label,
            levels = gsub(
              "-", "\n",
              names(prop(.x, "amount", sort = TRUE, head = 5, print = FALSE))
            )
          )
        ) %>%
        ggplot(aes(x = label, y = freq)) +
        geom_bar(stat = "identity") +
        xlab(NULL) +
        ylab("Percentage (%)") +
        scale_y_continuous(limits = c(0, 50))

      pdf(
        here("fig", paste0("portfolio_freq_top_5_", .y, ".pdf")),
        width = 3, height = 3
      )
      print(pdf_default(p))
      dev.off()
    }
  )
