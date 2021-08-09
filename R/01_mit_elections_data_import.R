source(here::here("R", "utilities.R"))

# Clean MIT House elections data ===============================================
## County-level returns from the MEDSL Election Returns Dataverse (MIT)
## https://dataverse.harvard.edu/dataverse/medsl_election_returns

## Create state_cd variable for House, filter no-write-in candidates
house <- 
  read_delim(here("data", "raw", "1976-2020-house.tab"), delim = "\t") %>%
  filter(
    year == 2020 & writein == FALSE & !grepl("WRITE-IN", party) & 
      !(candidate %in% 
          c("OTHERS", "SCATTERING", "OVER VOTES", "UNDER VOTES", "VOID"))
  ) %>%
  select(-state, -state_cen, -state_ic, -state_fips) %>%
  mutate(
    state_cd = paste(state_po, str_pad(district, 2, pad = "0"), sep = "-")
  ) %>%
  select(-state_po, -district) %>%
  select(state_cd, everything()) %>%
  mutate(candidate = gsub("\u0093|\u0094", "", candidate)) %>%
  mutate(candidate = gsub("\\\\", "", candidate))

## Kick out no variation columns
house <- house %>%
  select(
    - {
      house %>%
        map_dbl(~ length(table(.x, useNA = "ifany"))) %>%
        {which(. == 1)} %>%
        names()
    }
  )

## fusion_ticket (e.g. Dem and Working Families) TRUE cases merge
house <- bind_rows(
  house %>% filter(fusion_ticket == FALSE),
  house %>% 
    filter(fusion_ticket == TRUE) %>%
    group_by(fusion_ticket, state_cd, candidate) %>%
    mutate(candidatevotes = sum(candidatevotes)) %>%
    filter(party %in% c("DEMOCRAT", "REPUBLICAN"))
) %>%
  select(-fusion_ticket) %>%
  arrange(state_cd, desc(candidatevotes))

## "Typical" cases of two-party candidates (one each) or uncontested races
## https://en.wikipedia.org/wiki/Third-party_and_independent_members_of_the_United_States_House_of_Representatives
## Amash and Mitchell both did not run
house <- house %>%
  filter(party == "DEMOCRAT" | party == "REPUBLICAN")

## Exceptions: DC and LA
house %>%
  group_by(state_cd) %>%
  group_split() %>%
  map_dbl(nrow) %>% 
  {which(. > 2)}

# Clean MIT House elections data ===============================================
senate <- 
  read_delim(here("data", "raw", "1976-2020-senate.tab"), delim = "\t") %>%
  filter(
    year >= 2016 & writein == FALSE & !grepl("WRITE-IN", party_detailed) & 
      !(candidate %in% 
          c("OTHERS", "SCATTERING", "OVER VOTES", "UNDER VOTES", "VOID"))
  ) %>%
  select(-state, -state_cen, -state_ic, -state_fips) %>%
  select(state = state_po, everything()) %>%
  mutate(candidate = gsub("\u0093|\u0094", "", candidate)) %>%
  mutate(candidate = gsub("\\\\", "", candidate)) %>%
  ## Really minor candidates
  filter(candidatevotes > 1000) %>%
  ## Error: missing parties
  mutate(
    party = case_when(
      candidate == "CYNTHIA M. LUMMIS" & is.na(party_detailed) ~ "REPUBLICAN",
      candidate == "MERAV BEN DAVID" & is.na(party_detailed) ~ "DEMOCRAT",
      TRUE ~ party_detailed
    )
  ) %>%
  select(-party_detailed, -party_simplified) %>%
  select(state, special, candidate, party, everything())

## Kick out no variation columns
## "unofficial" column TRUE for Rounds vs. Ahlers in SD? Not sure why
senate <- senate %>%
  select(
    - {
      senate %>%
        map_dbl(~ length(table(.x, useNA = "ifany"))) %>%
        {which(. == 1)} %>%
        names()
    }
  ) %>%
  select(-unofficial)

## In the 2020 election, Sanders and King did not run
senate <- senate %>%
  filter(
    party == "DEMOCRAT" | party == "REPUBLICAN" | 
      candidate == "ANGUS S. KING, JR." | candidate == "BERNIE SANDERS" | 
      candidate == "AMY KLOBUCHAR" ## Marked DEMOCRATIC-FARMER-LABOR
  ) %>%
  arrange(state, desc(candidatevotes)) %>%
  mutate(
    party = case_when(
      party == "DEMOCRATIC-FARMER-LABOR" ~ "DEMOCRAT",
      TRUE ~ party
    )
  )

## Exceptions are GA and LA, both due to runoff
senate %>%
  group_by(state) %>%
  group_split() %>%
  map_dbl(nrow) %>% 
  {which(. > 2)}

mit <- list(
  senate = senate,
  house = house
)

save(mit, file = here("data", "tidy", "mit-tidy.Rda"))
