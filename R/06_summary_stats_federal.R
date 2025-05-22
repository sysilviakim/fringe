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
            ## Originally,
            ## checked for latest solicitation sets or longest-duration sets
            ## Now just looking for max_date
            ## duration == max(duration, na.rm = TRUE) | 
              max_date == max(max_date, na.rm = TRUE) & 
                max_date > as.Date("2020-11-01") & 
                duration > 10
          ) %>%
          ## Platform used
          mutate(
            actblue = case_when(grepl("actblue", url) ~ 1, TRUE ~ 0),
            winred = case_when(grepl("winred", url) ~ 1, TRUE ~ 0)
          )
          
        if (.y == "house") {
          out <- out %>% arrange(state, state_cd, last_name, seq, min)
        } else {
          out <- out %>% arrange(state, last_name, seq, min)
        }
        return(out)
      }
  )

cong_filtered$senate %>%
  group_by(state, last_name) %>%
  filter(n() > 1) %>%
  select(duration, everything()) %>%
  View()

cong_filtered$house %>%
  group_by(state_cd, last_name) %>%
  filter(n() > 1) %>%
  select(duration, everything()) %>%
  View()

# Manual deduplicating =========================================================
cong_filtered$senate <- cong_filtered$senate %>%
  filter(!(is.na(amount) & last_name == "lewis" & state == "MN")) %>%
  filter(!(is.na(amount) & last_name == "gardner" & state == "CO")) %>%
  filter(!(is.na(amount) & last_name == "graham" & state == "SC")) %>%
  filter(!(as.numeric(duration) < 3 & last_name == "mcsally")) %>%
  group_by(state, last_name) %>%
  filter(duration == max(duration)) %>%
  ## this is the main donate tab for lieberman
  ## see https://web.archive.org/web/20200816201450/https://secure.actblue.com/donate/lieberman-om?refcode=website-header&amount=25
  filter(
    !(
      last_name == "lieberman" & state == "GA" &
        url != "https://secure.actblue.com/donate/lieberman-om"
    )
  )

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
  filter(!(is.na(amount) & last_name == "hartzler" & state_cd == "MO-04")) %>%
  filter(!(is.na(amount) & last_name == "long" & state_cd == "MO-07")) %>%
  filter(!(is.na(amount) & last_name == "jones" & state_cd == "TX-23")) %>%
  ## splash page vs. more stable tab link for baldwin-kennedy
  ## take longer link
  filter(!(as.numeric(duration) == 0 & last_name == "cookingham")) %>%
  group_by(state_cd, last_name) %>%
  filter(duration == max(duration)) %>%
  filter(
    !(
      last_name == "bilirakis" & state_cd == "FL-12" &
        url != "https://secure.winred.com/bilirakis-for-congress/donate"
    ) & 
      !(
        last_name == "bourdeaux" & state_cd == "GA-07" &
          url != "https://secure.actblue.com/donate/carolyn_website"
      ) & 
      !(
        last_name == "newman" & state_cd == "IL-03" &
          url != "https://secure.actblue.com/donate/marienewmanforcongress"
      ) & 
      ## Now Hank Linderman's tricky; while other candidates' donate tab are 
      ## more visible/salient, Hank Linderman... see
      ## https://web.archive.org/web/20201107211145/https://hank4ky.com/
      ## I'll choose the center button
      !(
        last_name == "linderman" & state_cd == "KY-02" &
          url != "https://actblue.com/donate/hank4ky"
      ) & 
      !(
        last_name == "parrott" & state_cd == "MD-06" & 
          url != "https://www.parrott2020.org/donate-page"
      ) & 
      !(
        last_name == "mcclain" & state_cd == "MI-10" & 
          url != "https://transaxt.com/Donate/ZU3MNE/LisaMcClainforCongress/"
      ) & 
      !(
        last_name == "stefanik" & state_cd == "NY-21" &
          url != "https://secure.winred.com/elisestefanik/pg-site-donate"
      ) & 
      !(
        last_name == "gilbert" & state_cd == "TX-01" &
          url != "https://secure.actblue.com/donate/hank-newweb"
      ) & 
      !(
        last_name == "crenshaw" & state_cd == "TX-02" &
          url != "https://secure.winred.com/crenshawforcongress/support"
      )
  )

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

## Sanity check for one link
assert_that(
  cong_filtered$senate %>% 
    group_by(state, last_name) %>%
    filter(n() > 1) %>%
    nrow() == 0
)

assert_that(
  cong_filtered$house %>% 
    group_by(state_cd, last_name) %>%
    filter(n() > 1) %>%
    nrow() == 0
)

# Save =========================================================================
save(cong_filtered, file = here("data", "tidy", "cong-filtered.Rda"))

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

# Summary statistics by chamber and party ======================================
summ <- cross2(c("senate", "house"), c("DEMOCRAT", "REPUBLICAN")) %>%
  map_dfr(
    ~ cong_filtered[[.x[[1]]]] %>%
      filter(party == .x[[2]]) %>%
      ## i.e., everything
      mutate(cd = "00") %>%
      group_by(cd) %>%
      summarise(
        empty_prop = sum(is.na(amount)) / n(),
        amount = Mode(amount, na.rm = TRUE),
        min_median = median(min, na.rm = TRUE), 
        mean_median = median(mean, na.rm = TRUE),
        max_median = median(max, na.rm = TRUE), 
        main_platform = sum(actblue == 1 | winred == 1, na.rm = TRUE) / n(),
        ineff_2700 = sum(ineff_2700 == 1, na.rm = TRUE) / n(),
        max_out = sum(ineff_2800 == 1, na.rm = TRUE) / n(),
        beyond_max = sum(beyond_max == 1, na.rm = T) / n(),
        sanders = sum(sanders == 1, na.rm = TRUE) / n()
      ) %>%
      ungroup() %>%
      select(-cd) %>%
      mutate(
        office = paste0(
          simple_cap(tolower(.x[[2]])), ", ", simple_cap(.x[[1]])
        )
      )
  ) %>%
  mutate(
    across(
      c("empty_prop", "ineff_2700", "max_out", 
        "beyond_max", "sanders", "main_platform"),
      ~ scales::percent(.x, accuracy = 0.1)
    ),
    across(
      contains("median"),
      ~ formatC(.x, format = "f", digits = 1, big.mark = ",")
    )
  ) %>%
  select(office, everything())
summ

xtab_df <- as_tibble(t(summ), rownames = "var") %>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ] %>%
  mutate(
    office = case_when(
      office == "empty_prop" ~ "No Defaults",
      office == "ineff_2700" ~ "Did Not Adjust $2,700",
      office == "max_out" ~ "Used $2,800 (Legal Max.)",
      office == "beyond_max" ~ "Solicited More Than $2,800",
      office == "sanders" ~ "Sanders Heritage ($27)",
      office == "min_median" ~ "Median of Min. USD Solicited",
      office == "max_median" ~ "Median of Max. USD Solicited",
      office == "mean_median" ~ "Median of Mean USD Solicited",
      office == "amount" ~ "Mode of Solicitation Set",
      office == "main_platform" ~ "Used Major Platform",
      TRUE ~ office
    )
  )
xtab_df

print(
  xtable(xtab_df %>%  select(` ` = office, everything()), align = "llrrrr"),
  hline.after = c(-1, 0, 5, nrow(xtab_df)),
  file = here("tab", "congress_by_chamber_party_desc_2020.tex"),
  booktabs = TRUE, include.rownames = FALSE, floating = FALSE
)
