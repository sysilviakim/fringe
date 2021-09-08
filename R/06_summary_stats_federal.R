source(here::here("R", "utilities.R"))
load(here("data", "tidy", "congress-merged.Rda"))

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

# Histograms ===================================================================
cong <- cong_filtered %>% bind_rows(.id = "office") %>%
  mutate(office = simple_cap(office)) %>%
  group_by(office) %>%
  mutate(
    min_min = min(min, na.rm = TRUE),
    min_med = median(min, na.rm = TRUE),
    min_max = max(min, na.rm = TRUE),
    mean_min = min(mean, na.rm = TRUE),
    mean_med = median(mean, na.rm = TRUE),
    mean_max = max(mean, na.rm = TRUE),
    max_min = min(max, na.rm = TRUE),
    max_med = median(max, na.rm = TRUE),
    max_max = max(max, na.rm = TRUE)
  )
  
hist_list <- c("min", "max", "mean") %>%
  set_names(., .) %>%
  imap(
    ~ ggplot(aes(x = !!as.name(.x)), data = cong) +
      geom_histogram(bins = 60) + 
      facet_wrap(~ office) + 
      scale_x_continuous(labels = scales::comma) + 
      scale_y_continuous(labels = scales::comma) + 
      scale_y_continuous(
        labels = scales::comma,
        limits = c(0, 575), breaks = seq(0, 575, by = 100)
      )
  )

hist_list %>%
  imap(
    ~ {
      pdf(
        here("fig", paste0("hist_congress_2020_", .y, ".pdf")),
        width = 5, height = 2
      )
      print(pdf_default(.x) + theme(axis.title = element_blank()))
      dev.off()
    }
  )

ks.test(cong_filtered$senate$mean, cong_filtered$house$mean)
# Two-sample Kolmogorov-Smirnov test
# 
# data:  cong_filtered$senate$mean and cong_filtered$house$mean
# D = 0.056141, p-value = 0.8428
# alternative hypothesis: two-sided

# Top 5 frequent figures =======================================================
c("senate", "house") %>%
  map(
    ~ {
      p <- prop(
        portfolio_na_fig_label(cong_filtered[[.x]]) %>%
          group_by(url, amount) %>%
          slice(1),
        "amount",
        sort = TRUE, head = 5, print = FALSE
      ) %>%
        unlist() %>%
        set_names(., nm = names(.)) %>%
        imap(~ tibble(label = .y, freq = as.numeric(.x))) %>%
        bind_rows() %>%
        mutate(
          label = gsub(
            "-", "\n",
            paste0(label, ifelse(.x == "senate", "", "\n"))),
          label = factor(
            label,
            levels = gsub(
              "-", "\n",
              paste0(
                names(
                  prop(
                    portfolio_na_fig_label(cong_filtered[[.x]]), "amount", 
                    sort = TRUE, head = 5, print = FALSE
                  )
                ),
                ifelse(.x == "senate", "", "\n")
              )
            )
          )
        ) %>%
        ggplot(aes(x = label, y = freq)) +
        geom_bar(stat = "identity") +
        # xlab("\nSolicitation Amounts, Top 5, Right.us Directory") +
        xlab(NULL) +
        ylab("Percentage (%)") +
        scale_y_continuous(limits = c(0, 50)) 
      pdf(
        here("fig", paste0("portfolio_freq_top_5_", .x, "_2020.pdf")),
        width = 3, height = 3
      )
      print(pdf_default(p))
      dev.off()
    }
  )

# Summary statistics ===========================================================
summ <- c(senate = "senate", house = "house") %>%
  imap_dfr(
    ~ cong_filtered[[.x]] %>%
      ## i.e., everything
      mutate(cd = "00") %>%
      group_by(cd) %>%
      summarise(
        empty_prop = sum(is.na(amount)) / n(),
        # single = sum(!is.na(amount) & !grepl("-", amount)) / n(),
        amount = Mode(amount, na.rm = TRUE),
        min_median = median(min, na.rm = TRUE), 
        max_median = median(max, na.rm = TRUE), 
        mean_median = median(mean, na.rm = TRUE),
        ineff_2700 = sum(ineff_2700 == 1, na.rm = TRUE) / n(),
        max_out = sum(ineff_2800 == 1, na.rm = TRUE) / n(),
        beyond_max = sum(beyond_max == 1, na.rm = T) / n(),
        sanders = sum(sanders == 1, na.rm = TRUE) / n()
        # min_mode = Mode(min, na.rm = TRUE), 
        # max_mode = Mode(max, na.rm = TRUE), 
        # mean_mode = Mode(mean, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      select(-cd),
    .id = "office"
  ) %>%
  mutate(
    across(
      c("empty_prop", "ineff_2700", "max_out", "beyond_max", "sanders"),
      ~ scales::percent(.x, accuracy = 0.1)
    ),
    across(
      contains("median"),
      ~ formatC(.x, format = "f", digits = 1, big.mark = ",")
    )
  )
summ

xtab_df <- as_tibble(t(summ), rownames = "var") %>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ] %>%
  select(office, Senate = senate, House = house) %>%
  mutate(
    office = case_when(
      office == "empty_prop" ~ "No Defaults (%)",
      office == "single" ~ "One Default (%)",
      office == "ineff_2700" ~ "Did Not Adjust $2,700",
      office == "max_out" ~ "Used $2,800 (Individual Maximum)",
      office == "beyond_max" ~ "Solicited More Than $2,800",
      office == "sanders" ~ "Sanders Heritage ($27)",
      office == "min_median" ~ "Median of Minimum Value Solicited",
      office == "max_median" ~ "Median of Maximum Value Solicited",
      office == "mean_median" ~ "Median of Mean Value Solicited",
      TRUE ~ office
    )
  )
xtab_df

which_var <- which(xtab_df$office == "amount")
assert_that(xtab_df$Senate[which_var] == xtab_df$House[which_var])
addtorow <- list(
  pos = list(which_var - 1),
  command = paste0(
    "Mode Solicitation Set & \\multicolumn{2}{c}{ ",
    xtab_df$Senate[which_var],
    " }", " \\\\
  "
  )
)

print(
  xtable(
    xtab_df %>% 
      filter(office != "amount") %>%
      select(` ` = office, everything()),
    align = "llrr"
  ),
  add.to.row = addtorow, hline.after = c(-1, 0, 4, nrow(xtab_df) - 1),
  file = here("tab", "congress_desc_2020.tex"),
  booktabs = TRUE, include.rownames = FALSE, floating = FALSE
)

