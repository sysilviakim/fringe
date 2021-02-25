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
        # Don't filter for !is.na(portfolio) just yet
        # group_by(across(c(setdiff(ex, "url"), "date"))) %>%
        # First URL recorded for a given date/amount
        # filter(url == first(url)) %>%
        # group_by(across(c(setdiff(ex, "url"), "date", "portfolio"))) %>%
        # This enforces reordering from smallest number, however
        # slice(1) %>%
        # ungroup() %>%
        portfolio_summ(order_vars = ex)
    }
  )

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
          "Adjusted $2,800 to $2,900",
          "Did Not Adjust $2,800 Maximum"
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
      c("adjust"),
      print = FALSE
    ) %>%
      rowid_matrix_to_df() %>%
      rename(
        !!as.name(paste0(.x[[1]], " (", simple_cap(.x[[2]]), ")")) := value
      )
  ) %>%
  Reduce(left_join, .) %>%
  rename(" " = rownames) %>%
  xtable()

print(
  tab,
  file = here("tab", "max_adjust_congress_by_party_2022.tex"),
  booktabs = TRUE, include.rownames = FALSE, floating = FALSE
)

# Conditional probability by platform ==========================================
# aka how much of this is a platform effect?
temp %>%
  imap(
    ~ .x %>%
      filter(
        orig_2800 == TRUE & party == "Rep" &
          winred == "WinRed" & grepl("Did", adjust)
      )
  )

temp %>%
  imap(
    ~ .x %>%
      filter(
        orig_2800 == TRUE & party == "Dem" &
          actblue == "ActBlue" & grepl("Did", adjust)
      )
  )

tab <- cross2(c("Dem", "Rep"), categories) %>%
  map(
    ~ prop(
      temp[[.x[[2]]]] %>% filter(orig_2800 == TRUE & party == .x[[1]]),
      c("adjust", ifelse(.x[[1]] == "Rep", "winred", "actblue")),
      print = FALSE
    ) %>%
      rowid_matrix_to_df() %>%
      mutate(type = paste0(.x[[1]], " (", simple_cap(.x[[2]]), ")")) %>%
      select(rownames, type, everything())
  )

out <- bind_cols(
  tab[[1]] %>% select(-type) %>% rename(" " = rownames),
  tab[[2]] %>% select(-rownames, -type),
  tab[[3]] %>% select(-rownames, -type),
  tab[[4]] %>% select(-rownames, -type),
  .name_repair = "minimal"
) %>%
  xtable(align = "llll|ll|ll|ll")

addtorow <- list(
  pos = list(-1),
  command = paste0(
    tab %>%
      map_chr(
        ~ paste0(
          "& \\multicolumn{2}{c}{", .x$type[1], "}",
          collapse = ""
        )
      ) %>%
      paste0(collapse = ""),
    "\\\\"
  )
)

print(
  out,
  add.to.row = addtorow, hline.after = c(0),
  file = here("tab", "max_adjust_congress_by_platform_2022.tex"),
  booktabs = TRUE, include.rownames = FALSE, floating = FALSE
)
