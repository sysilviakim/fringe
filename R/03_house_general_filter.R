source(here::here("R", "utilities.R"))
load(here("data", "tidy", "mit-tidy.Rda"))
load(here("data", "tidy", "portfolio_summ_federal_first_only.Rda"))
load(here("data", "tidy", "fec_cand_summ_2020.Rda"))

# Merge MIT data and scraped data ==============================================
house <- mit$house %>%
  mutate(
    candidate = trimws(gsub("\\s+", " ", candidate)),
    last_name = case_when(
      gsub(",|\\.", "", tolower(word(candidate, -1, -1))) %in%
        c("jr", "sr", "ii", "iii", "iv") ~
        gsub(",|\\.", "", tolower(word(candidate, -2, -2))),
      TRUE ~ gsub(",|\\.", "", tolower(word(candidate, -1, -1)))
    ),
    last_name = gsub("á", "a", last_name),
    last_name = gsub("ñ", "n", last_name),
    last_name = gsub("í", "i", last_name),
    last_name = gsub("ó", "o", last_name),
    last_name = case_when(
      last_name == "bull" & candidate == "LYNNETTE GREY BULL" ~ 
        "grey bull",
      last_name == "rochester" & candidate == "LISA BLUNT ROCHESTER" ~
        "blunt rochester",
      last_name == "cunnane" & candidate == "MADELEINE DEAN CUNNANE" ~ 
        "dean cunnane",
      last_name == "beutler" & candidate == "JAIME HERRERA BEUTLER" ~ 
        "herrera beutler",
      last_name == "schultz" & candidate == "DEBBIE WASSERMAN SCHULTZ" ~ 
        "wasserman schultz",
      last_name == "drew" & candidate == "JEFFERSON VAN DREW" ~ 
        "van drew",
      last_name == "luna" & candidate == "ANNA PAULINA LUNA" ~ 
        "paulina luna",
      last_name == "orden" & candidate == "DERRICK VAN ORDEN" ~ 
        "van orden",
      last_name == "duyne" & candidate == "BETH VAN DUYNE" ~ 
        "van duyne",
      last_name == "santos" & candidate == "GEORGE A. D. SANTOS" ~ 
        "devolder-santos",
      last_name == "water" & candidate == "KYLE VAN DE WATER" ~ 
        "van de water",
      last_name == "isla" & candidate == "MICHELLE DE LA ISLA" ~ 
        "de la isla",
      last_name == "holmes" & candidate == "MICHELLE GARCIA HOLMES" ~ 
        "garcia holmes",
      last_name == "small" & candidate == "XOCHITL TORRES SMALL" ~ 
        "torres small",
      last_name == "cruz-hernandez" & 
        candidate == "MONICA DE LA CRUZ-HERNANDEZ" ~ 
        "de la cruz hernandez",
      last_name == "pearson" & candidate == "ERIKA STOTTS PEARSON" ~ 
        "stotts-pearson",
      last_name == "stanton-king" & candidate == "ANGELA STANTON-KING" ~ 
        "stanton king",
      last_name == "fuente" & candidate == "RICARDO RICK DE LA FUENTE" ~ 
        "de la fuente",
      last_name == "ausdal" & candidate == "KEVIN VAN AUSDAL" ~ 
        "van ausdal",
      last_name == "ezammudeen" & candidate == "JOHSIE CRUZ EZAMMUDEEN" ~ 
        "cruz",
      last_name == "rodgers" & candidate == "CATHY MCMORRIS RODGERS" ~ 
        "mcmorris rodgers",
      TRUE ~ last_name
    ),
    state_cd = gsub("-00", "-0", state_cd)
  ) %>%
  select(last_name, everything()) %>%
  filter(!grepl("DC", state_cd)) %>%
  left_join(
    ., dl$house %>%
      mutate(
        year = as.numeric(year),
        last_name = tolower(last_name)
      ) %>%
      select(-year)
  ) %>%
  mutate(
    ## if no scraped data, still need to create cd
    state = case_when(
      is.na(state) ~ gsub("[[:digit:]]|-", "", state_cd),
      TRUE ~ state
    )
  ) %>%
  mutate(
    min = case_when(
      last_name == "petel" & state_cd == "CA-14"  ~ as.Date("2020-06-07"),
      last_name == "schakowsky" & state_cd == "IL-09" ~ as.Date("2019-12-05"),
      TRUE ~ min
    ),
    max = case_when(
      last_name == "petel" & state_cd == "CA-14" ~ as.Date("2020-11-09"),
      last_name == "ausdal" & state_cd == "GA-14"  ~ as.Date("2020-11-26"),
      last_name == "londrigan" & state_cd == "IL-13"  ~ as.Date("2020-11-06"),
      last_name == "brady" & state_cd == "MA-09"  ~ as.Date("2020-11-12"),
      ## razzoli actually paused Anedot receipts before Nov
      ## https://web.archive.org/web/20200401000000*/https://secure.anedot.com/mark-razzoli-for-cong/home
      last_name == "christensen" & state_cd == "OR-01"  ~ as.Date("2020-12-01"),
      last_name == "collick" & state_cd == "VA-03"  ~ as.Date("2020-11-09"),
      last_name == "dunn" & state_cd == "FL-02" ~ as.Date("2020-11-09"),
      TRUE ~ max
    ),
    amount = case_when(
      last_name == "petel" & state_cd == "CA-14" ~ 
        "25-50-100-200-500-1000-1500-2800",
      ## error in data collection; traceback
      ## https://web.archive.org/web/20200517161104/https://secure.winred.com/nealdunn/donate
      last_name == "dunn" & state_cd == "FL-02" ~ "250-500-1000-2800-5600",
      ## Weird link; not archived
      ## https://web.archive.org/web/20201005050648/https://schakowsky.bsd.net/page/contribute/default?donate_page_KEY=15
      last_name == "schakowsky" & state_cd == "IL-09" ~ "10-50-250-2800-25-100-1000",
      ## https://web.archive.org/web/20201111201429/https://secure.ngpvan.com/Kc_4TB6OfUqzi_V_cUHm6g2
      ## NGP VAN scrape error
      last_name == "tonko" & state_cd == "NY-20" ~ "10-50-100-500-1000-2800",
      TRUE ~ amount
    )
  ) %>%
  ## For Campa-Najjar, delete fr_q3 and fr_q4 links
  filter(
    !(last_name == "campa-najjar" & state_cd == "CA-50" &
        grepl("fr_q3|fr_q4|surv_q4", url))
  )

## Check for missing values
house %>%
  filter(is.na(url)) %>%
  arrange(desc(candidatevotes)) %>%
  View()

# Merge with FEC candidate summary =============================================
house <- left_join(
  house,
  fec_cand_summ_2020 %>%
    filter(office == "H") %>%
    mutate(last_name = trimws(tolower(word(cand_name, 1, 1, sep = ",")))) %>%
    rename(party_fec = party) %>%
    mutate(
      cd = case_when(
        cand_name == "ISSA, DARRELL" & cd == "49" ~ "50",
        ## https://ballotpedia.org/Sara_Jacobs
        cand_name == "JACOBS, SARA" & cd == "49" ~ "53",
        TRUE ~ cd
      )
    ) %>%
    mutate(state_cd = gsub("-00", "-0", paste(state, cd, sep = "-"))) %>%
    mutate(
      last_name = case_when(
        last_name == "mcardle schulman" & 
          cand_name == "MCARDLE SCHULMAN, MAUREEN B MRS" ~ "mcardle-schulman",
        last_name == "kennedy" & cand_name == "KENNEDY, RONDA" ~ 
          "baldwin-kennedy",
        last_name == "walorski swihart" & 
          cand_name == "WALORSKI SWIHART, JACKIE" ~ 
          "walorski",
        last_name == "leger" & cand_name == "LEGER, TERESA" ~ 
          "fernandez",
        last_name == "dean" & cand_name == "DEAN, MADELEINE" ~ 
          "dean cunnane",
        last_name == "arenholz" & cand_name == "ARENHOLZ, ASHLEY HINSON" ~ 
          "hinson",
        TRUE ~ last_name
      )
    ) %>%
    bind_rows(
      ## https://ballotpedia.org/George_McDermott missing
      ., tibble(
        cand_id = NA,
        cand_name = "MCDERMOTT, GEORGE",
        office = "H",
        inc = "CHALLENGER",
        state = "MD",
        cd = "04",
        party_fec = "REP",
        last_name = "mcdermott",
        state_cd = "MD-04"
      )
    )
)

## Must hold true
## assert_that(house %>% filter(is.na(cd)) %>% nrow() == 0)
## This no longer holds; 
## Brandon Martin (AZ-02) is missing from FEC data
## https://www.fec.gov/data/candidate/H8AZ02201/?cycle=2020
## Same for JEANNINE LEE LAKE
## https://www.fec.gov/data/candidate/H8IN06202/
## LEE ANN DUGAS
## GEORGE AD SANTOS
## MAUREEN MCARDLE SCHULMAN
## CHARLOTTE BERGMANN

# No solicitations link: resolve errors ========================================
## Plus manual inputs from PayPal links, recorded on the setup file
house <- house %>%
  mutate(
    amount = case_when(
      last_name == "huffman" & amount == "-999" ~
        "5",
      last_name == "guest" & amount == "-999" ~
        "100",
      last_name == "schakowsky" & amount == "-999" ~
        "10-25-50-100-250-1000-2800",
      last_name == "smith" & amount == "-999" ~
        "25-50-100-200-500-1500",
      last_name == "tonko" & amount == "-999" ~
        "10-50-100-500-1000-2800",
      last_name == "kelly" & amount == "-999" ~
        "10-25-50-100-200-250-500-1000-2800-5600",
      last_name == "cleaver" & amount == "-999" ~
        "25-50-100-200-500-1500",
      last_name == "oberweis" & amount == "-999" ~
        "35-50-100-250-500-1000-2800-5600",
      last_name == "maryott" & amount == "-999" ~
        "2800-1000-500-250-100-50",
      last_name == "spenser" & amount == "-999" ~
        "12-25-50-101-500-100-1500-2800",
      last_name == "parrott" & amount == "-999" ~
        "6-14-25-50-100-250-500-1000-2800-75-125",
      last_name == "costa" & amount == "-999" ~
        "25-50-100-250-1000-2800",
      last_name == "kumar" & amount == "-999" ~
        "1000-2800-5600",
      last_name == "bradley" & amount == "-999" ~
        "5-10-25-50-100-250-500-1000-2800",
      last_name == "briscoe" & amount == "-999" ~
        "199-200-250-500-1000-2700",
      last_name == "weber" & amount == "-999" ~
        "10-20.20-50-100-1000-2800",
      last_name == "garcia" & amount == "-999" ~
        "20-35-50-100-500-2500",
      last_name == "reed" & amount == "-999" ~
        "1-5-15-25-100-250-500-1000-1776-2700-5400",
      last_name == "thorpe" & amount == "-999" ~
        "35-75-125-250",
      last_name == "wood" & amount == "-999" ~
        "5-10-25-50-100-250-500-2800-5600",
      last_name == "gonzalez" & amount == "-999" ~
        "25-50-100-250-500-1000-1500-2000-2700",
      last_name == "armendariz-jackson" & amount == "-999" ~
        "100-500-2800-5600",
      last_name == "bish" & amount == "-999" ~
        "25-100-500-2800",
      last_name == "patrick" & amount == "-999" ~
        "	10-50-100",
      last_name == "hogg" & amount == "-999" ~
        "20-35-50-100-500-2500",
      last_name == "50-100-200-500-1000" & amount == "-999" ~
        "santiago-cano",
      last_name == "pennie" & amount == "-999" ~
        "10-25-50-100-250-500-1000-2800",
      last_name == "piterman" & amount == "-999" ~
        "10-100-1000",
      last_name == "mitris" & amount == "-999" ~
        "250-500-1000",
      last_name == "beeler" & amount == "-999" ~
        "5-25-100",
      TRUE ~ amount
    )
  )

# Party mismatch resolve =======================================================
table(house$party, house$party_fec)

save(house, file = here("data", "tidy", "house-merged.Rda"))
