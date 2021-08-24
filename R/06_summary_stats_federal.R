source(here::here("R", "utilities.R"))

# Import congressional data ====================================================
congress <- list(
  senate = loadRData(here("data", "tidy", "senate-merged.Rda")),
  house = loadRData(here("data", "tidy", "house-merged.Rda"))
) %>%
  map(summ_calc_fxn) %>%
  imap(
    ~ {
      out <- .x %>%
        select(-seq_url, -contains("office")) %>%
        select(
          last_name, contains("state"), party, inc, contains("year"), amount, 
          min_date, max_date, seq, min, max, mean, median, q1, q3, first, last,
          choices, contains("ineff"), sanders, everything()
        ) %>%
        filter(!is.na(url)) %>%
        mutate(
          max_date = case_when(
            max_date >= as.Date("2020-11-06") ~ as.Date("2020-11-06"),
            TRUE ~ max_date
          )
        ) %>%
        mutate(duration = max_date - min_date)
      if (.y == "house") {
        out <- out %>% arrange(state, state_cd, last_name, seq, min)
      } else {
        out <- out %>% arrange(state, last_name, seq, min)
      }
      return(out)
    }
  )

# View(congress$senate)
# congress$senate %>% filter(is.na(amount)) %>% View()

# Which Senate candidates *don't* have links live in November? =================

## Kamala Harris, of course

## Chuck Grassley: 
## https://web.archive.org/web/20201007075445/https://grassleyworks.com/
## *Literally* didn't have a contribution link on his website for long
## Not sure why...
## WinRed link was live, of course

## Pat Toomey's WinRed link went down(!) mid September, and is still(!!) down
## Screenshot evidence exists, although not snapshoted frequently at WayBack

congress$senate %>%
  group_by(state, last_name) %>%
  filter(
    !(lubridate::month(max(max_date, na.rm = TRUE)) >= 11 & 
        lubridate::year(max(max_date, na.rm = TRUE)) == 2020)
  ) %>%
  View()

# Which House candidates *don't* have links live in November? ==================
## razzoli stopped donation link before Nov; kick from sample
congress$house %>%
  group_by(state, state_cd, last_name) %>%
  filter(
    !(lubridate::month(max(max_date, na.rm = TRUE)) >= 11 & 
        lubridate::year(max(max_date, na.rm = TRUE)) == 2020)
  ) %>%
  View()

# One link per candidate =======================================================
cong_filtered <- congress %>%
  imap(
    ~ ## Start/end date may be misleading + weird deviations can't be verified;
      ## See for example Curran (not screenshot at the same date; 
      ## Oct 21, 2020 anomaly lost)
      ## First, must be grouped by amount/URL
      {
        out <- .x %>%
          ## Irrespective of the URL, if amount is the same, treat as same
          group_by(last_name, amount) %>%
          filter(
            lubridate::month(max(max_date, na.rm = TRUE)) >= 11 & 
              lubridate::year(max(max_date, na.rm = TRUE)) == 2020
          ) %>%
          mutate(
            duration = max(max_date) - min(min_date),
            min_date = min(min_date),
            max_date = max(max_date)
          ) %>%
          slice(n()) %>%
          filter(
            ## First check for latest solicitation sets or longest-duration sets
            duration == max(duration, na.rm = TRUE) | 
              max_date == max(max_date, na.rm = TRUE)
          )
        if (.y == "house") {
          out <- out %>% arrange(state, state_cd, last_name, seq, min)
        } else {
          out <- out %>% arrange(state, last_name, seq, min)
        }
        return(out)
      }
  )

# cong_filtered$senate %>%
#   group_by(state, last_name) %>%
#   filter(n() > 1) %>%
#   select(duration, everything()) %>%
#   View()

# Manual deduplicating =========================================================
cong_filtered$senate <- cong_filtered$senate %>%
  filter(!(is.na(amount) & last_name == "lewis" & state == "MN")) %>%
  filter(!(is.na(amount) & last_name == "gardner" & state == "CO")) %>%
  filter(!(is.na(amount) & last_name == "graham" & state == "SC")) %>%
  filter(!(as.numeric(duration) < 3 & last_name == "mcsally"))

cong_filtered$house <- cong_filtered$house %>%
  filter(!(is.na(amount) & last_name == "taylor" & state_cd == "VA-02")) %>%
  filter(!(is.na(amount) & last_name == "collins" & state_cd == "TX-32")) %>%
  filter(!(is.na(amount) & last_name == "teague" & state_cd == "TX-09")) %>%
  filter(!(is.na(amount) & last_name == "hunt" & state_cd == "TX-07")) %>%
  filter(!(is.na(amount) & last_name == "cooper" & state_cd == "TN-05")) %>%
  filter(!(is.na(amount) & last_name == "parnell" & state_cd == "PA-17")) %>%
  filter(!(is.na(amount) & last_name == "cooper" & state_cd == "TN-05")) %>%
  ## barnette did change to 25-50-250-500-750-1000, but this was Nov 5, 
  ## so will count the previous entries
  filter(!(as.numeric(duration) == 0 & last_name == "barnette")) %>%
  filter(!(is.na(amount) & last_name == "kean" & state_cd == "NJ-07")) %>%
  filter(!(is.na(amount) & last_name == "gottheimer" & state_cd == "NJ-05")) %>%
  filter(!(is.na(amount) & last_name == "mowers" & state_cd == "NH-01")) %>%
  filter(!(is.na(amount) & last_name == "ives" & state_cd == "IL-06")) %>%
  filter(!(is.na(amount) & last_name == "barragan" & state_cd == "CA-44")) %>%
  filter(!(is.na(amount) & last_name == "garamendi" & state_cd == "CA-03")) %>%
  ## splash page vs. more stable tab link for baldwin-kennedy
  ## take longer link
  filter(!(as.numeric(duration) == 0 & last_name == "cookingham")) %>%
  group_by(state_cd, last_name) %>%
  filter(duration == max(duration))

# Are no-prompt links not coding mistakes? =====================================
## Corrections coded into filter scripts

cong_filtered$senate %>% 
  filter(is.na(amount)) %>% 
  arrange(desc(candidatevotes)) %>% 
  select(url, everything()) %>%
  View()

cong_filtered$house %>% 
  filter(is.na(amount)) %>% 
  arrange(desc(candidatevotes)) %>% 
  select(url, everything()) %>%
  View()


