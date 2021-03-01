source(here::here("R", "utilities.R"))
categories <- c("senate", "house")

df_ls <- categories %>%
  set_names(., .) %>%
  map(
    ~ read_fst(here(paste0("data/tidy/", .x, "_2022.fst"))) %>%
      filter(!is.na(party)) %>%
      arrange(across(c(starts_with("state"), ends_with("name"), "date"))) %>%
      # Don't care about incumbents
      filter(incumbent == TRUE & last_name != "Loeffler") %>%
      mutate(url = gsub(" .*?$|\\|.*?$", "", url)) %>%
      # Multiple entity or irrelevant one time links
      filter(
        !grepl("8-races", url) & !grepl("official-trump-duncan-yard-signs", url)
      ) %>%
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
        mutate(year = 2022, portfolio = as.numeric(portfolio)) %>%
        filter(!is.na(url) & url != "") %>%
        group_split(across(all_of(ex))) %>%
        map_dfr(~ portfolio_summ(.x, order_vars = ex)) %>%
        arrange(across(c(setdiff(all_of(ex), "url"), "min")))
    }
  )

dl %>% map(~ assert_that(all(.x$min <= .x$max)))

# Manual corrections ===========================================================
dl$house <- dl$house %>%
  mutate(
    max = case_when(
      last_name == "Greene" &
        url == "https://secure.winred.com/marjorie-greene-for-congress/donate" &
        amount == "25-50-100-250-500-1000-2800" ~
      # https://web.archive.org/web/20210131090143/https://secure.winred.com/marjorie-greene-for-congress/donate
      as.Date("2021-01-31"),
      TRUE ~ max
    ),
    min = case_when(
      last_name == "Greene" &
        url == "https://secure.winred.com/marjorie-greene-for-congress/donate" &
        amount == "25-50-100-250-500-1000-2900" ~
      # https://web.archive.org/web/20210205002844/https://secure.winred.com/marjorie-greene-for-congress/donate
      as.Date("2021-02-05"),
      TRUE ~ min
    )
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
save(dl, file = here("data/tidy/portfolio_summ_federal_final_2022.Rda"))
