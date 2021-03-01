source(here::here("R", "utilities.R"))
load(here("data/tidy/portfolio_summ_federal_final_2022.Rda"))

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
        ~ paste0("& \\multicolumn{2}{c}{", .x$type[1], "}", collapse = "")
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
