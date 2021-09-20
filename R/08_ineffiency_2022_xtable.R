source(here::here("R", "utilities.R"))
load(here("data/tidy/portfolio_summ_federal_final_2022.Rda"))

# Did candidates shift their maximum amounts to 2,900, if previously 2,800? ====
temp <- dl %>%
  map(
    ~ .x %>%
      group_by(across(c(contains("state"), contains("name"), "party"))) %>%
      # e.g. Loeffler "thank you" page after loss
      filter(!is.na(amount)) %>%
      mutate(
        adjusted_date = case_when(
          grepl("2900", amount) ~ min_date,
          TRUE ~ NA_Date_
        )
      ) %>%
      summarise(
        orig_2800 = ifelse(sum(ineff_2800, na.rm = TRUE) > 0, TRUE, FALSE),
        adjust = ifelse(
          sum(ineff_2900, na.rm = TRUE) > 0 & sum(ineff_2800, na.rm = TRUE) > 0,
          "Adjusted $2,800 to $2,900",
          "Did Not Adjust $2,800 Maximum"
        ),
        # e.g. Bryan Steil
        # added 5,800 soon after 2,900:
        # first date shifted to 2,900 should be the first one by date
        adjusted_date = min(adjusted_date, na.rm = TRUE),
        winred = ifelse(any(grepl("winred", url)), "WinRed", "Other"),
        actblue = ifelse(any(grepl("actblue", url)), "ActBlue", "Other")
      ) %>%
      mutate(
        party = factor(party, levels = c("Dem", "Rep", "Ind")),
        actblue = factor(actblue, levels = c("ActBlue", "Other")),
        winred = factor(winred, levels = c("WinRed", "Other")),
        adjusted_date = case_when(
          is.infinite(adjusted_date) ~ as.Date("2021-01-31"),
          TRUE ~ adjusted_date
        )
      ) %>%
      ungroup()
  )

## Sanity check: difference is those without any prompts
dl %>% map_dbl(~ nrow(.x %>% group_by(fec_id_cand) %>% slice(1)))
temp %>% map_dbl(nrow)

# Compare by party =============================================================
# Oh yikes!
tab <- cross2(c("Dem", "Rep"), c("senate", "house")) %>%
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
tab <- cross2(c("Dem", "Rep"), c("senate", "house")) %>%
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

# When did the shift happen? ===================================================
# Conditional on switching, right after the FEC announcement on Feb 2, 2021
temp %>%
  map(~ .x %>% filter(grepl("Adjusted", adjust))) %>%
  map(
    ~ list(
      Dem = prop(
        .x %>% filter(party == "Dem"), "adjusted_date",
        sort = TRUE, head = 3, print = FALSE
      ),
      Rep = prop(
        .x %>% filter(party == "Rep"), "adjusted_date",
        sort = TRUE, head = 3, print = FALSE
      )
    )
  )

max_date <- temp %>% 
  map_dbl(~ max(.x$adjusted_date)) %>% 
  max() %>% 
  as.Date(., origin = "1970-01-01")

# Visualize it: Senate/House by party ==========================================
p <- temp %>%
  map(
    ~ {
      p <- .x %>%
        filter(orig_2800 == TRUE) %>%
        mutate(
          adjust = case_when(
            grepl("Did Not", adjust) ~ 0,
            TRUE ~ 1
          )
        ) %>%
        group_by(party, adjusted_date) %>%
        summarise(adjust = sum(adjust), n = n()) %>%
        pivot_wider(
          id_cols = adjusted_date, names_from = party, values_from = c(adjust, n)
        ) %>%
        mutate(across(where(is.integer), ~ as.numeric(.x))) %>%
        mutate(
          across(
            where(is.numeric), 
            ~ case_when(is.na(.x) ~ 0, TRUE ~ .x)
          )
        ) %>%
        mutate(
          adjust_Dem = adjust_Dem / sum(n_Dem),
          adjust_Rep = adjust_Rep / sum(n_Rep)
        ) %>%
        pivot_longer(
          cols = c(adjust_Dem, adjust_Rep), names_to = "Party", values_to = "v"
        ) %>%
        mutate(Party = gsub("adjust_", "", Party)) %>%
        group_by(Party) %>%
        arrange(adjusted_date) %>%
        mutate(cm = cumsum(v))
      
      if (max_date > max(p$adjusted_date)) {
        ## Unify the endpoint
        temp <- p %>% 
          filter(adjusted_date == max(adjusted_date)) %>%
          mutate(adjusted_date = max_date)
        
        p <- bind_rows(p, temp)
      }
      
      p <- p %>%
        ggplot(aes(x = adjusted_date, y = cm, colour = Party, linetype = Party)) +
        geom_line() +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        scale_colour_manual(values = c("Dem" = "#74add1", "Rep" = "#d73027")) +
        xlab("Date Adjusted to $2,900") +
        ylab("Cumulative %") + 
        scale_x_date(breaks = "1 month", date_labels = "%b")
    }
  )

p %>%
  imap(
    ~ {
      pdf(
        here("fig", paste0("adjust_2900_", .y, ".pdf")),
        width = 3.5, height = 2.5
      )
      print(
        pdf_default(.x) +
          theme(
            legend.position = c(.8, .3),
            legend.background = element_blank()
          )
      )
      dev.off()
    }
  )

# Visualize it: Within party by platform =======================================
p <- temp %>%
  bind_rows(.id = "Race") %>%
  filter(orig_2800 == TRUE) %>%
  mutate(
    adjust = case_when(
      grepl("Did Not", adjust) ~ 0,
      TRUE ~ 1
    ),
    platform = case_when(
      winred == "WinRed" ~ "WinRed",
      actblue == "ActBlue" ~ "ActBlue",
      TRUE ~ "Other"
    ),
    platform = factor(platform, levels = c("WinRed", "ActBlue", "Other"))
  ) %>%
  arrange(adjusted_date) %>%
  group_split(party) %>%
  `names<-`({.} %>% map(~ .x$party[1]) %>% unlist()) %>%
  imap(
    ~ {
      p <- .x %>%
        droplevels() %>%
        group_by(platform, adjusted_date) %>%
        summarise(adjust = sum(adjust), n = n()) %>%
        ungroup() %>%
        pivot_wider(
          id_cols = adjusted_date, names_from = platform, 
          values_from = c(adjust, n)
        ) %>%
        mutate(across(where(is.integer), ~ as.numeric(.x))) %>%
        mutate(
          across(
            where(is.numeric), 
            ~ case_when(is.na(.x) ~ 0, TRUE ~ .x)
          )
        )
      
      if (.y == "Dem") {
        p <- p %>%
          mutate(
            adjust_ActBlue = adjust_ActBlue / sum(n_ActBlue),
            adjust_Other = adjust_Other / sum(n_Other)
          )
      } else {
        p <- p %>%
          mutate(
            adjust_WinRed = adjust_WinRed / sum(n_WinRed),
            adjust_Other = adjust_Other / sum(n_Other)
          )
      }
      
      p <- p %>%
        select(-contains("n_")) %>%
        pivot_longer(
          cols = matches("adjust_"),
          names_to = "Platform", values_to = "v"
        ) %>%
        mutate(Platform = gsub("adjust_", "", Platform)) %>%
        group_by(Platform) %>%
        arrange(adjusted_date) %>%
        mutate(cm = cumsum(v)) %>%
        ggplot(
          aes(x = adjusted_date, y = cm, colour = Platform, linetype = Platform)
        ) +
        geom_line()
      
      if (.y == "Dem") {
        p <- p +
          scale_linetype_manual(
            values = c("ActBlue" = "solid", "Other" = "twodash")
          ) +
          scale_colour_manual(
            values = c("ActBlue" = "#74add1", "Other" = "gray")
          )
      } else {
        p <- p +
          scale_linetype_manual(
            values = c("WinRed" = "solid", "Other" = "twodash")
          ) +
          scale_colour_manual(
            values = c("WinRed" = "#d73027", "Other" = "gray")
          )
      }
      p <- p +
        labs(colour = "Platform", group = "Platform", linetype = "Platform") +
        xlab("Date Adjusted to $2,900") +
        ylab("Cumulative %") +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + 
        scale_x_date(breaks = "1 month", date_labels = "%b")
    }
  )

p %>%
  imap(
    ~ {
      pdf(
        here("fig", paste0("adjust_2900_", .y, ".pdf")),
        width = 3.5, height = 2.5
      )
      print(
        pdf_default(.x) +
          theme(
            legend.position = c(.8, .3),
            legend.background = element_blank()
          )
      )
      dev.off()
    }
  )
