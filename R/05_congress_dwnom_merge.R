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
  filter(chamber != "President" & congress > 115) %>%
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
  ungroup() %>%
  select(
    ## ICPSR legacy infos
    -state_icpsr, -icpsr, -district_code, -occupancy, -last_means, -party_code
  ) %>%
  rename(state = state_abbrev) %>%
  mutate(
    state_cd = case_when(
      state_cd == "AK-01" ~ "AK-0",
      state_cd == "DE-01" ~ "DE-0",
      state_cd == "MT-01" ~ "MT-0",
      state_cd == "ND-01" ~ "ND-0",
      state_cd == "SD-01" ~ "SD-0",
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

# Before merging, check for duplicates =========================================
dwnom_sliced %>% 
  group_by(last_name, state, state_cd, chamber) %>% 
  filter(n() > 1) %>%
  arrange(last_name) %>%
  View()

## amash, dingell, levin, and mitchell
dwnom_sliced <- dwnom_sliced %>%
  filter(!(last_name == "amash" & party_dwnom1 == "REPUBLICAN")) %>%
  filter(!(last_name == "mitchell" & party_dwnom1 == "REPUBLICAN"))

congress$senate %>% 
  group_by(last_name, cand_id, state) %>% 
  slice(1) %>% 
  group_by(last_name, state) %>%
  filter(n() > 1) %>%
  arrange(last_name) %>%
  select(cand_id, cand_name, everything()) %>%
  View()

congress$senate <- congress$senate %>%
  ## 2010 candidate
  filter(cand_id != "S4GA11053") %>%
  ## Duplicate Paula Jean, due to changing ID from S8WV00119 to S0WV00215
  filter(cand_id != "S8WV00119") %>%
  ## Setti Warren is a 2012 candidate
  filter(cand_id != "S2MA00139") %>%
  ## 2018 candidate
  filter(cand_id != "S8UT00259") %>%
  ## Duplicate Derrick Earl Grayson, due to changing ID from
  ## S4GA11236 to S0GA00658
  filter(cand_id != "S4GA11236")
  
congress$house %>% 
  group_by(last_name, cand_id, state_cd) %>% 
  slice(1) %>% 
  group_by(last_name, state_cd) %>%
  filter(n() > 1) %>%
  arrange(last_name) %>%
  select(cand_id, cand_name, everything()) %>%
  View()

congress$house <- congress$house %>%
  ## Ken Calvert has two registrations: H2CA37023 is the real one, not H0CA42209
  filter(cand_id != "H0CA42209") %>%
  ## Joe Cunningham, not Logan
  filter(cand_id != "H0SC01378") %>%
  ## Moe Davis and Jim Davis in the same race, AND only Moe Davis general cand.
  filter(cand_id != "H0NC11183") %>%
  ## Mike Garcia's previous record (H0CA25162) has been kicked out of FEC rec.
  filter(cand_id != "H0CA25162") %>%
  ## Duplicate Rey Gonzalez, same amounts
  filter(cand_id != "H6TX34031") %>%
  ## Duplicate Juan Hidalgo, H8CA51062 is the right one
  filter(cand_id != "H6CA51066") %>%
  ## Gina Jones, not Cecil Jones
  filter(cand_id != "H0TX23227") %>%
  ## Duplicate Mark Leyva, H0IN01333 is the right one
  filter(cand_id != "H8IN01096") %>%
  ## Duplicate Nate McMurray, H8NY27176 is the right one
  filter(cand_id != "H0NY27157") %>%
  ## Duplicate Raul Ruiz, uhh... H2CA36439 has larger numbers
  filter(cand_id != "H0CA36177") %>%
  ## Duplicate Mike Siegel, H8TX10110 is the right one
  filter(cand_id != "H0TX10208") %>%
  ## Duplicate Christy Smith, H0CA25154 is the right one
  filter(cand_id != "H0CA25253")

# Merge with DW-NOMINATE (Senate) ==============================================
## Joined by last_name and state
congress$senate <- left_join(
  congress$senate, dwnom_sliced %>% filter(chamber == "Senate")
)

## New candidates, lost challengers
unmatched_senate <- congress$senate %>%
  filter(is.na(nominate_dim1)) %>%
  group_by(last_name, state) %>%
  slice(1)

unmatched_senate %>% View()
nrow(unmatched_senate) / nrow(congress$senate) ## 15%

# Merge with DW-NOMINATE (House) ===============================================
## Joined by last_name and state
congress$house <- left_join(
  congress$house, dwnom_sliced %>% filter(chamber == "House")
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

# Sanity check =================================================================
congress %>% 
  bind_rows(.id = "office") %>%
  group_by(party) %>%
  summarise(
    min = min(nominate_dim1, na.rm = TRUE), 
    mean = mean(nominate_dim1, na.rm = TRUE),
    max = max(nominate_dim1, na.rm = TRUE)
  )

congress %>% 
  bind_rows(.id = "office") %>%
  ggplot(aes(x = nominate_dim1, group = party, colour = party, fill = party)) + 
  geom_histogram()

# Add in state-GDPs from the Bureau of Economic Analysis =======================
## https://apps.bea.gov/iTable/iTable.cfm?reqid=70&step=1&acrdn=2
bea <- read_csv(here("data", "raw", "bea-2020-gdp.csv"), skip = 4) %>%
  clean_names() %>%
  mutate(
    across(where(is.character), ~ tolower(trimws(gsub("\\*", "", .x)))),
    geo_fips = as.numeric(geo_fips) / 1000
  ) %>%
  filter(
    geo_name != "united states" & 
      description == "per capita personal income (dollars) 2/"
  ) %>%
  select(-description, -line_code) %>%
  rename(
    q1_income = x2020_q1, q2_income = x2020_q2,
    q3_income = x2020_q3, q4_income = x2020_q4
  ) %>%
  left_join(., state_df %>% select(geo_fips = stfips, state = stabb)) %>%
  select(-geo_fips, -geo_name) %>%
  ## Per capita income across all 2020 quarters
  mutate(pci2020 = (q1_income + q2_income + q3_income + q4_income) / 4) %>%
  select(state, everything()) %>%
  filter(!is.na(state))

congress <- congress %>%
  imap(~ left_join(.x, bea))

# Also add Cook PVI ============================================================
## Since it's already built from solicitRwinred... 
load(here("data", "tidy", "cook_pvi.Rda"))

congress$senate <- left_join(
  congress$senate, 
  cook_pvi %>% filter(office == "S") %>% select(-office) %>% select(-state_cd)
)
assert_that(!any(is.na(congress$senate$PVI)))

congress$house <- left_join(
  congress$house, 
  cook_pvi %>% filter(office == "H") %>% select(-office)
)
assert_that(!any(is.na(congress$house$PVI)))

# Save =========================================================================
save(congress, file = here("data", "tidy", "congress-merged.Rda"))
