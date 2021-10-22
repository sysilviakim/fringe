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

# Missing Senate candidates from data collection ===============================
## Joy Felicia Slade ---> no website, no contribution records
## Annette Davis Jackson ---> no contribution records
## Isakson/Rand Paul retired, McCain died, Shelby had no contribution link
## Bunch of minor candidates in GA/LA

senate_supp <- list(
  tibble(
    ## https://web.archive.org/web/20200614012238/https://deborahforgeorgia.com/
    last_name = "Jackson",
    state = "GA",
    url =
      "https://secure.anedot.com/deborah-jackson-for-us-senate/donate",
    min = "2020-06-14",
    max = "2020-11-09",
    amount = "101-250-500-1000-2800"
  ),
  tibble(
    ## https://web.archive.org/web/20200919042402/https://www.jamesiajames4ussenate.com/
    ## https://web.archive.org/web/20200612032710/https://secure.actblue.com/donate/jamesia-james-for-us-senate-1
    last_name = "James",
    state = "GA",
    url =
      "https://secure.actblue.com/donate/jamesia-james-for-us-senate-1",
    min = "2020-06-12",
    max = "2020-11-09",
    amount = "25-100-250-1000"
  ),
  tibble(
    ## https://web.archive.org/web/20200201143141/www.graysonforga.com
    ## https://web.archive.org/web/20200131112135/https://www.graysonforga.com/donate
    last_name = "Grayson",
    state = "GA",
    url = "https://www.graysonforga.com/donate",
    min = "2020-01-31",
    max = "2020-11-09",
    amount = "1-5-10-25-50-100-250-1000-2500"
  ),
  tibble(
    ## https://web.archive.org/web/20200601222403/http://kandisstaylor.com/
    ## https://web.archive.org/web/20200816201409/https://secure.winred.com/kandiss-taylor-for-u-s-senate/donate
    last_name = "Taylor",
    state = "GA",
    url = "https://secure.winred.com/kandiss-taylor-for-u-s-senate/donate",
    min = "2020-06-01",
    max = "2020-11-09",
    amount = "25-50-100-250-500-1000-2800"
  ),
  tibble(
    ## https://web.archive.org/web/20200305150203/https://tarverforsenate.com/
    ## https://secure.ngpvan.com/J78LV9GDlEOs5GbaeHPe-A2 ---> lost
    ## ActBlue is second of two links, but NGP VAN amounts will never be known!
    ## https://web.archive.org/web/20200915231925/https://secure.actblue.com/donate/tarver-for-senate-1
    last_name = "Tarver",
    state = "GA",
    url = "https://secure.actblue.com/donate/tarver-for-senate-1",
    min = "2020-07-07",
    max = "2020-11-09",
    amount = "2800-1000-500-250-100-25-10"
  )
) %>%
  bind_rows() %>%
  mutate(
    min = as.Date(min),
    max = as.Date(max),
    seq = 1,
    seq_url = 1,
    year = "2020"
  )

dl$senate <- bind_rows(dl$senate, senate_supp)

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
  map(summ_calc_fxn)

## Unlike 2022 data, I did not have candidate characteristics in the data, 
## so skip merging

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
