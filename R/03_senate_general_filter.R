source(here::here("R", "utilities.R"))
load(here("data", "tidy", "mit-tidy.Rda"))
load(here("data/tidy/portfolio_summ_federal_first_only.Rda"))

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
    amount = "101-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20200919042402/https://www.jamesiajames4ussenate.com/
    ## https://web.archive.org/web/20200612032710/https://secure.actblue.com/donate/jamesia-james-for-us-senate-1
    last_name = "James",
    state = "GA",
    url = 
      "https://secure.anedot.com/deborah-jackson-for-us-senate/donate",
    min = "2020-06-12",
    max = "2020-11-09",
    amount = "25-100-250-1000",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20200201143141/www.graysonforga.com
    ## https://web.archive.org/web/20200131112135/https://www.graysonforga.com/donate
    last_name = "Grayson",
    state = "GA",
    url = "https://www.graysonforga.com/donate",
    min = "2020-01-31",
    max = "2020-11-09",
    amount = "1-5-10-25-50-100-250-1000-2500",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20200601222403/http://kandisstaylor.com/
    ## https://web.archive.org/web/20200816201409/https://secure.winred.com/kandiss-taylor-for-u-s-senate/donate
    last_name = "Taylor",
    state = "GA",
    url = "https://www.graysonforga.com/donate",
    min = "2020-06-01",
    max = "2020-11-09",
    amount = "25-50-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
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
    amount = "2800-1000-500-250-100-25-10",
    seq_url = 1,
    seq = 1
  )
) %>%
  bind_rows() %>%
  mutate(
    min = as.Date(min), 
    max = as.Date(max),
    last_name = tolower(last_name)
  )

senate <- mit$senate %>%
  mutate(
    last_name = case_when(
      gsub(",|\\.", "", tolower(word(candidate, -1, -1))) == "jr" ~ 
        gsub(",|\\.", "", tolower(word(candidate, -2, -2))),
      candidate == "CATHERINE CORTEZ MASTO" ~ "cortez masto",
      candidate == "MERAV BEN DAVID" ~ "ben-david",
      candidate == "CHRIS VAN HOLLEN" ~ "van hollen",
      TRUE ~ gsub(",|\\.", "", tolower(word(candidate, -1, -1)))
    )
  ) %>%
  select(last_name, everything()) %>%
  left_join(
    ., dl$senate %>%
      mutate(
        year = as.numeric(year),
        last_name = tolower(last_name)
      ) %>%
      select(-year) %>%
      filter() %>%
      ## correct error
      mutate(
        state = case_when(
          last_name == "jordan" & state == "CA" ~ "ID",
          last_name == "murphy" & state == "CA" ~ "LA",
          TRUE ~ state
        )
      ) %>%
      bind_rows(., senate_supp)
  )

## Check for missing values
senate %>% filter(is.na(url)) %>% View()
