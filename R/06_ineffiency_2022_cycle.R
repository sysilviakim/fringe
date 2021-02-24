source(here::here("R", "utilities.R"))
categories <- c("senate", "house")

df_ls <- categories %>%
  set_names(., .) %>%
  map(
    ~ read_fst(here(paste0("data/tidy/", .x, "_2022.fst"))) %>%
      filter(!is.na(party)) %>%
      arrange(across(c(starts_with("state"), ends_with("name"), "date"))) %>%
      filter(incumbent == TRUE) %>%
      mutate(url = gsub(" .*?$|\\|.*?$", "", url)) %>%
      filter(last_name != "Loeffler") %>%
      dedup()
  )

df_ls %>%
  map(~ assert_that(max(.x$date, na.rm = TRUE) > as.Date("2021-01-01")))

# Note that Greene, LaTurner, and Mace's WinRed links were not scraped
# due to a flaw in the scraper (when both Anedot and WinRed existed, 
# scraped only Anedot)

# Create portfolio summaries ===================================================
dl <- df_ls %>%
  imap(
    ~ {
      ex <- fed_exception_vars(.y)
      .x %>%
        ungroup() %>%
        # Take out for now to feed into portfolio_summ
        select(
          -FEC_ID_cand, -party, -first_name, -incumbent, -contains("class")
        ) %>%
        mutate(year = 2022, portfolio = as.numeric(portfolio)) %>%
        filter(!is.na(url) & url != "") %>%
        # Don't filter for !is.na(portfolio) just yet
        group_by(across(c(setdiff(ex, "url"), "date"))) %>%
        # First URL recorded for a given date/amount
        filter(url == first(url)) %>%
        group_by(across(c(setdiff(ex, "url"), "date", "portfolio"))) %>%
        # This enforces reordering from smallest number, however
        slice(1) %>%
        ungroup() %>%
        portfolio_summ(order_vars = ex)
    }
  )
save(dl, file = here("data/tidy/portfolio_summ_federal_first_only_2022.Rda"))

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

dl %>% map(~ assert_that(all(!is.na(.x$incumbent))))

# Did candidates shift their maximum amounts to 2,900, if previously 2,800? ====
temp <- dl %>%
  map(
    ~ .x %>%
      group_by(across(c(contains("state"), contains("name"), "party"))) %>%
      # e.g. Loeffler "thank you" page after loss
      filter(!is.na(amount)) %>%
      summarise(
        orig_2800 = ifelse(sum(ineff_2800, na.rm = TRUE) > 0, TRUE, FALSE),
        adjust = ifelse(
          sum(ineff_2900, na.rm = TRUE) > 0 & sum(ineff_2800, na.rm = TRUE) > 0,
          "Adjusted \\$2,800 to \\$2,900",
          "Did Not Adjust \\$2,800 Maximum"
        ),
        winred = ifelse(any(grepl("winred", url)), "WinRed", "Other"),
        actblue = ifelse(any(grepl("actblue", url)), "ActBlue", "Other")
      ) %>%
      mutate(winred = factor(winred, levels = c("WinRed", "Other")))
  )

# Compare by party =============================================================
# Oh yikes!
tab <- cross2(c("Dem", "Rep"), categories) %>%
  map(
    ~ prop(
      temp[[.x[[2]]]] %>% filter(orig_2800 == TRUE & party == .x[[1]]),
      c("adjust"), print = FALSE
    ) %>%
      rowid_matrix_to_df() %>%
      rename(
        !!as.name(paste0(.x[[1]], " (", simple_cap(.x[[2]]), ")")) := value
      )
  ) %>%
  Reduce(left_join, .) %>%
  rename(" " = rownames) %>%
  xtable()

highlight_xtable(
  tab, 
  file = here("tab", "max_adjust_congress_2022.tex"),
  booktabs = TRUE, include.rownames = FALSE, floating = FALSE
)

# Conditional probability by platform ==========================================
tab <- cross2(c("Dem", "Rep"), categories) %>%
  map(
    ~ prop(
      temp[[.x[[2]]]] %>% filter(orig_2800 == TRUE & party == .x[[1]]),
      c("adjust", ifelse(.x[[1]] == "Rep", "winred", "actblue")), print = FALSE
    ) %>%
      rowid_matrix_to_df() %>%
      mutate(type = paste0(.x[[1]], " (", simple_cap(.x[[2]]), ")"))
  )

highlight_xtable(
  tab, 
  file = here("tab", "max_adjust_congress_2022.tex"),
  booktabs = TRUE, include.rownames = FALSE, floating = FALSE
)


