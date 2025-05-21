source(here::here("R", "utilities.R"))

# All 2020 committees ==========================================================
if (!file.exists(here("data/tidy/fec_cand_summ.Rda"))) {
  ## Format not consistent prior to 2008
  fec_cand_summ <- seq(2008, 2020, by = 2) %>%
    set_names(., .) %>%
    map(
      ~ {
        fname <- paste0(
          "https://cg-519a459a-0ea3-42c2-b7bc-fa1143481f74.",
          "s3-us-gov-west-1.amazonaws.com/bulk-downloads/", .x, "/",
          paste0("candidate_summary_", .x, ".csv")
        )
        download.file(
          fname,
          destfile = here("data", "raw", paste0("fec_cand_summ_", .x, ".csv"))
        )
        read_csv(fname, col_types = cols(.default = "c")) %>%
          clean_names()
      }
    )
  save(fec_cand_summ, file = here("data/tidy/fec_cand_summ.Rda"))
} else {
  load(here("data/tidy/fec_cand_summ.Rda"))
}

# Bind rows and kick out nonzero variance variables ============================
df <- fec_cand_summ %>%
  bind_rows(.id = "year") %>%
  ## Send image to back
  select(-link_image, everything()) %>%
  mutate() %>%
  ## Bring more important variables to front
  select(
    cand_id, cand_name, cand_office,
    incumbent = cand_incumbent_challenger_open_seat,
    everything()
  )

# By-year changes? =============================================================
temp <- df %>%
  group_by(cand_id) %>%
  mutate(cand_name = last(cand_name)) %>%
  rowwise() %>%
  mutate(status = paste(cand_office, incumbent)) %>%
  select(cand_id, cand_name, status, year) %>%
  ungroup() %>%
  group_by(cand_id) %>%
  pivot_wider(names_from = year, values_from = status)

# For now, take only the basic characteristics =================================
fec_cand_summ_2020 <- df %>%
  filter(
    (year >= 2014 & grepl("^S", cand_id)) | 
      (year == 2020 & grepl("^H", cand_id)) | 
      ## Darrell Issa
      cand_id == "H0CA48024" |
      ## Sara Jacobs (why not captured in 2020?)
      cand_id == "H8CA49074" |
      ## Byron Donalds
      cand_id == "H2FL14186"
  ) %>%
  group_by(cand_id) %>% 
  # Just take the last snapshot, which is (if it exists) 2020
  slice(n()) %>%
  select(
    cand_id, cand_name,
    office = cand_office, inc = incumbent, state = cand_office_st,
    cd = cand_office_dist, party = cand_party_affiliation
  ) %>%
  dedup() %>%
  group_by(cand_id) %>%
  slice(n())

# Save =========================================================================
save(fec_cand_summ_2020, file = here("data", "tidy", "fec_cand_summ_2020.Rda"))
