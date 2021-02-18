source(here::here("R", "utilities.R"))

root_url <- "https://www.fec.gov/files/bulk-downloads/"
if (!dir.exists(here("data", "raw", "fec"))) {
  dir.create(here("data", "raw", "fec"))
}

fec_candidates <- seq(2008, 2022, by = 2) %>%
  map_dfr(
    ~ {
      url <- paste0(root_url, .x, "/candidate_summary_", .x, ".csv")
      if (
        !file.exists(here("data/raw/fec", paste0("cand_summ_", .x, ".csv")))) {
        download.file(
          url,
          destfile = here("data/raw/fec", paste0("cand_summ_", .x, ".csv")),
          mode = "wb"
        )
      }
      read_csv(
        here("data/raw/fec", paste0("cand_summ_", .x, ".csv")),
        na = "", col_types = cols(.default = "c")
      ) %>%
        clean_names() %>%
        mutate(across(everything(), tolower)) %>%
        mutate(
          cand_name_2 = trimws(gsub("\\.", "", cand_name)),
          cand_name_2 = trimws(
            gsub(
              paste0(
                c("sr", "jr", "ii", "iii", "iv") %>%
                  map(~ c(paste0(", ", .x, "$"), paste0(", ", .x, ","))) %>%
                  unlist() %>%
                  paste(collapse = "|"),
                "| md, phd |md, phd$| dds, ms |dds, ms$"
              ),
              "", cand_name_2
            )
          )
        ) %>%
        separate(
          col = "cand_name_2", into = c("last_name", "first_name"),
          sep = ", "
        ) %>%
        select(cand_id, last_name, first_name, cand_state, everything()) %>%
        mutate(
          # 9-digit and 5-digit mixed
          cand_zip5 = str_sub(cand_zip, 1, 5),
          first_name = gsub(" md$", "", first_name)
        ) %>%
        select(-link_image, -cand_zip, -cand_name, everything()) %>%
        mutate(
          cand_id = toupper(cand_id),
          cand_state = toupper(cand_state)
        )
    }
  )

save(fec_candidates, file = here("data", "tidy", "fec_candidates.Rda"))
