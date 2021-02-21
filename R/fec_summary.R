source(here::here("R", "utilities.R"))

# Scrape FEC summaries from official website ===================================
# Extracted after looking at `Network` at the Elements
# https://www.fec.gov/data/raising-bythenumbers/?election_year=1980

if (!file.exists("data/tidy/fec_summary.Rda")) {
  fec_summary <- cross2(c("P", "S", "H"), seq(1980, 2020, by = 4)) %>%
    map(
      ~ paste0(
        "https://api.open.fec.gov/v1/candidates/totals/by_office/by_party/?",
        "api_key=28Y8q8XFocq8yhKfBzzhUJXjFj2JHCZzIv4P2KIK&office=", .x[[1]], 
        "&", "election_year=", .x[[2]], "&",
        "is_active_candidate=true&per_page=20&sort_null_only=false&",
        "sort_hide_null=false&sort_nulls_last=false&page=1"
      ) %>%
        read_html() %>%
        html_text() %>%
        fromJSON() %>%
        .$results
    ) %>%
    bind_rows() %>%
    mutate(
      office = factor(
        office, levels = c("P", "S", "H"), 
        labels = c("President", "Senate", "House")
      ),
      party = factor(
        party, levels = c("DEM", "REP", "Other"), 
        labels = c("Dem", "Rep", "Other")
      )
    ) %>%
    arrange(office, desc(election_year))
  save(fec_summary, file = "data/tidy/fec_summary.Rda")
}

# Create ggplot objects ========================================================
p <- 
  cross2(c("President", "Senate", "House"), c("receipts", "disbursements")) %>%
  map(
    ~ fec_summary %>%
      filter(office == .x[[1]]) %>%
      ggplot() +
      geom_line(
        aes(
          x = election_year, y = !!as.name(paste0("total_", .x[[2]])) / 1e6,
          group = party, color = party, linetype = party
        )
      ) + 
      scale_x_continuous(
        breaks = seq(1980, 2020, by = 4), 
        labels = function(x) paste0("'", substr(x, 3, 4))
      ) + 
      scale_y_continuous(
        limits = c(0, 3300), breaks = seq(0, 3300, 500), 
        labels = scales::comma
      ) + 
      xlab("Year") + 
      ylab(
        paste0(
          if_else(grepl("dis", .x[[2]]), "Disbursements", "Receipts"), 
          " (1 million USD)"
        )
      ) + 
      labs(group = "Party", color = "Party", linetype = "Party") + 
      scale_color_manual(
        values = c("Dem" = "#377eb8", "Rep" = "#e41a1c", "Other" = "#984ea3")
      )
  )

# Export: plots look similar, should use only one ==============================
pdf("fig/fec_summary_1980_2020_receipts.pdf", width = 8, height = 4)
grid_arrange_shared_legend(
  pdf_default(p[[1]]) + xlab(""), 
  pdf_default(p[[2]]) + ylab(""), 
  pdf_default(p[[3]]) + ylab("") + xlab("")
)
dev.off()

pdf("fig/fec_summary_1980_2020_disbursements.pdf", width = 8, height = 4)
grid_arrange_shared_legend(
  pdf_default(p[[4]]) + xlab(""), 
  pdf_default(p[[5]]) + ylab(""), 
  pdf_default(p[[6]]) + ylab("") + xlab("")
)
dev.off()
