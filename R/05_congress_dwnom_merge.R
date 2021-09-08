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

# Import DW-NOMINATE ===========================================================
dwnom <- read_csv(here("data/raw/HSall_members.csv")) 
dwnom_sliced <- dwnom %>%
  filter(chamber != "President" & congress > 110) %>%
  group_by(bioname, state_abbrev) %>%
  filter(congress == max(congress)) %>%
  separate(bioname, into = c("last_name", "first_name_dwnom"), sep = ", ") %>%
  mutate(
    last_name = tolower(last_name),
    first_name_dwnom = tolower(first_name_dwnom)
  ) %>%
  select(congress, last_name, first_name_dwnom, everything()) %>%
  rowwise() %>%
  mutate(
    state_cd = paste(
      state_abbrev, 
      case_when(
        str_pad(district_code, 2, pad = "0") == "00" ~ "0",
        TRUE ~ str_pad(district_code, 2, pad = "0")
      ), 
      sep = "-"
    ),
    party_dwnom1 = case_when(
      party_code == 100 ~ "DEMOCRAT",
      party_code == 200 ~ "REPUBLICAN",
      party_code == 328 ~ "INDEPENDENT"
    )
  ) %>%
  select(
    ## ICPSR legacy infos
    -state_icpsr, -icpsr, -district_code, -occupancy, -last_means, -party_code
  ) %>%
  rename(state = state_abbrev) %>%
  mutate(
    state_cd = case_when(
      state_cd == "AK-01" ~ "AK-0",
      state_cd == "DE-01" ~ "DE-0",
      state_cd == "ND-01" ~ "ND-0",
      state_cd == "VT-01" ~ "VT-0",
      state_cd == "WY-01" ~ "WY-0",
      TRUE ~ state_cd
    ),
    last_name = gsub("á", "a", last_name),
    last_name = gsub("í", "i", last_name),
    last_name = case_when(
      last_name == "jackson lee" & state_cd == "TX-18" ~ "lee",
      last_name == "dean" & state_cd == "PA-04" ~ "dean cunnane",
      last_name == "watson coleman" & state_cd == "NJ-12" ~ "coleman",
      TRUE ~ last_name
    )
  )

# Merge with DW-NOMINATE (Senate) ==============================================
congress$senate <- left_join(
  congress$senate, dwnom_sliced
)

## New candidates, lost challengers
unmatched_senate <- congress$senate %>%
  filter(is.na(nominate_dim1)) %>%
  group_by(last_name, state) %>%
  slice(1)

unmatched_senate %>% View()
nrow(unmatched_senate) / nrow(congress$senate) ## 15%

# Merge with DW-NOMINATE (House) ===============================================
congress$house <- left_join(
  congress$house, dwnom_sliced
)

## New candidates, lost challengers
assert_that(
  congress$house %>%
    filter(is.na(nominate_dim1) & inc == "INCUMBENT") %>%
    nrow() == 0
)

unmatched_house <- congress$house %>%
  filter(is.na(nominate_dim1)) %>%
  group_by(last_name, state_cd) %>% 
  slice(1)

unmatched_house %>% View()
nrow(unmatched_house) / nrow(congress$house) ## 29%

# Save =========================================================================
save(congress, file = here("data", "tidy", "congress-merged.Rda"))
