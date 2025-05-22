categories <- c("senate", "house")
source(here::here("R", "scraped_data_import.R"))
rm(df_raw)
source(here::here("R", "scraped_data_import.R")) ## run it twice
gc(reset = TRUE)

# Anomalies ====================================================================
df_ls <- df_ls %>%
  map(
    ~ .x %>%
      mutate(
        # de Blasio in presidential race
        # Young Kim in house race
        # Probably happened because I used lists at some point
        url = gsub(" .*?$|\\|.*?$", "", url),
      )
  )

df_ls$senate <- df_ls$senate %>%
  mutate(
    state = case_when(
      last_name == "Rubio" & state == "" ~ "FL",
      last_name == "Jordan" & state == "CA" ~ "ID",
      last_name == "Murphy" & state == "CA" ~ "LA",
      TRUE ~ state
    )
  )

# All federal records, regardless of the data generating process ===============
dl <- df_ls %>%
  imap(
    ~ {
      ex <- fed_exception_vars(.y)
      .x %>%
        ungroup() %>%
        mutate(year = 2020) %>%
        filter(!is.na(url) & url != "") %>%
        group_split(across(all_of(ex))) %>%
        map_dfr(~ portfolio_summ(.x, order_vars = ex)) %>%
        arrange(across(c(setdiff(all_of(ex), "url"), "min")))
    }
  )

dl %>% map(~ assert_that(all(.x$min <= .x$max)))
save(dl, file = here("data/tidy/portfolio_summ_federal.Rda"))

# Keep only the first URL per date =============================================
dl <- df_ls %>%
  imap(
    ~ {
      ex <- fed_exception_vars(.y)
      .x %>%
        ungroup() %>%
        mutate(year = 2020, portfolio = as.numeric(portfolio)) %>%
        filter(!is.na(url) & url != "") %>%
        # Don't filter for !is.na(portfolio) just yet
        group_by(across(c(setdiff(all_of(ex), "url"), "date"))) %>%
        # First URL recorded for a given date/amount
        filter(url == first(url)) %>%
        group_by(across(c(setdiff(all_of(ex), "url"), "date", "portfolio"))) %>%
        # This enforces reordering from smallest number, however
        slice(1) %>%
        ungroup() %>%
        group_split(across(all_of(ex))) %>%
        map_dfr(~ portfolio_summ(.x, order_vars = ex)) %>%
        arrange(across(c(setdiff(all_of(ex), "url"), "min")))
    }
  )

dl %>% map(~ assert_that(all(.x$min <= .x$max)))
save(dl, file = here("data/tidy/portfolio_summ_not_supped.Rda"))

# Missing House candidates from data collection ================================
## Dale Mensing ---> no contribution link
## Analilia Joya ---> no contribution link
## Lindsay Holliday ---> no contribution link
## Tommy Hanson ---> no contribution link
## Lee Ann Dugas ---> no contribution link
## David Schilling ---> no record in setup file
##                      https://ballotpedia.org/David_Schilling ---> nothing!
##                      https://www.facebook.com/David-Schilling-for-U-S-
##                      Representative-District-2-100437218506904/
## Glenn Harris ---> no contribution link
## Sheldon Vincent ---> no record in setup file
##                      https://ballotpedia.org/Sheldon_Vincent_Sr. ---> no need
## Ben Gibson ---> no contribution link
## Allen Guillory ---> no contribution link
## Jesse Lagarde ---> no contribution link
## George McDermott ---> no contribution link
## Anthony Rogers ---> no contribution link
## Megan Rezabek ---> no contribution link
## Daryl Farrow ---> no contribution link
## Jason Mushnik ---> no contribution link
## Brian Kelly ---> no contribution link
## Garfield Wallace ---> no contribution link
## Frank Lucas ---> no contribution link
## David Torres ---> no contribution link
## Christopher Finley ---> no contribution link
## Randy Weber ---> no contribution link
## Ricardo Rick De la Fuente ---> no contribution link

house_supp <- list(
  tibble(
    ## https://web.archive.org/web/20201031182324/https://connect.clickandpledge.com/w/Form/7a862fb4-4953-4fef-b11b-b53c533d08e1
    last_name = "rogers",
    state_cd = "AL-03",
    url = "https://connect.clickandpledge.com/w/Form/7a862fb4-4953-4fef-b11b-b53c533d08e1",
    min = "2020-10-31",
    max = "2020-11-05",
    amount = "20-30-40-50",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201102035144/https://secure.piryx.com/donate/ysLubgUF/Womack-for-Congress/
    last_name = "womack",
    state_cd = "AR-03",
    url =
      "https://secure.piryx.com/donate/ysLubgUF/Womack-for-Congress/",
    min = "2020-11-02",
    max = "2020-11-05",
    amount = "25-50-100-250-500-1000",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201024233231/https://secure.anedot.com/buzz-patterson/donate
    last_name = "patterson",
    state_cd = "CA-07",
    url =
      "https://secure.anedot.com/buzz-patterson/donate",
    min = "2020-10-24",
    max = "2020-11-06",
    amount = "11200-5600-2800-1000-500-250-99-50-5",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20200807220031/https://secure.anedot.com/ronda-kennedy-for-congress/website-splash
    last_name = "baldwin-kennedy",
    state_cd = "CA-26",
    url =
      "https://secure.anedot.com/ronda-kennedy-for-congress/website-splash",
    min = "2020-08-07",
    max = "2020-11-06",
    amount = "100-250-500-1000-2800-5600",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201020174816/https://secure.winred.com/rondakennedy/donate
    last_name = "baldwin-kennedy",
    state_cd = "CA-26",
    url =
      "https://secure.winred.com/rondakennedy/donate",
    min = "2020-10-20",
    max = "2020-11-13",
    amount = "25-50-100-250-500-1000-2800",
    seq_url = 2,
    seq = 2
  ),
  tibble(
    ## https://web.archive.org/web/20201103071415/https://erincruz.revv.co/takebackthehouse
    last_name = "cruz",
    state_cd = "CA-36",
    url =
      "https://erincruz.revv.co/takebackthehouse",
    min = "2020-11-03",
    max = "2020-11-06",
    amount = "20-36-100-250-360-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201031082417/https://secure.actblue.com/donate/georgette-gomez-for-congress-1
    last_name = "gomez",
    state_cd = "CA-53",
    url =
      "https://secure.actblue.com/donate/georgette-gomez-for-congress-1",
    min = "2020-10-31",
    max = "2020-11-03",
    amount = "10-50-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201023120919/https://secure.anedot.com/shane-bolling-campaign/donate
    last_name = "bolling",
    state_cd = "CO-01",
    url =
      "https://secure.anedot.com/shane-bolling-campaign/donate",
    min = "2020-10-23",
    max = "2020-11-13",
    amount = "-999",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101133231/https://secure.actblue.com/donate/dmb-2020
    last_name = "mitsch bush",
    state_cd = "CO-03",
    url =
      "https://secure.actblue.com/donate/dmb-2020",
    min = "2020-11-01",
    max = "2020-12-05",
    amount = "25-50-100-250-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101182721/https://secure.campaignsolutions.com/kenbuck/list/proc/donation1/?initiativekey=V0T9TNVLFGX5
    last_name = "buck",
    state_cd = "CO-04",
    url =
      "https://secure.campaignsolutions.com/kenbuck/list/proc/donation1/?initiativekey=V0T9TNVLFGX5",
    min = "2020-11-01",
    max = "2020-11-13",
    amount = "25-50-75-100-250-500-1000-2800-5600",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101151051/https://secure.actblue.com/donate/lisa-blunt-rochester-for-congress-1
    last_name = "blunt rochester",
    state_cd = "DE-0",
    url =
      "https://secure.actblue.com/donate/lisa-blunt-rochester-for-congress-1",
    min = "2020-11-01",
    max = "2020-12-05",
    amount = "25-50-125-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201102061149/https://secure.winred.com/annapaulinaluna/web
    last_name = "paulina luna",
    state_cd = "FL-13",
    url =
      "https://secure.winred.com/AnnaPaulinaLuna/web",
    min = "2020-11-02",
    max = "2020-11-07",
    amount = "10-25-50-100-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101123622/https://secure.actblue.com/donate/cohn-for-congress-website
    last_name = "cohn",
    state_cd = "FL-15",
    url =
      "https://secure.actblue.com/donate/cohn-for-congress-website",
    min = "2020-11-01",
    max = "2020-11-05",
    amount = "100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201025001720/https://secure.anedot.com/steube/donate
    last_name = "steube",
    state_cd = "FL-17",
    url =
      "https://secure.anedot.com/steube/donate",
    min = "2020-10-25",
    max = "2020-11-15",
    amount = "250-500-1000-2800-5600",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101025706/https://secure.actblue.com/donate/dwsdefault
    last_name = "wasserman schultz",
    state_cd = "FL-23",
    url =
      "https://secure.actblue.com/donate/dwsdefault",
    min = "2020-11-01",
    max = "2020-11-14",
    amount = "5-25-50-100-1000-2800-5600",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201025002243/https://secure.anedot.com/carlaforcongress/direct
    last_name = "spalding",
    state_cd = "FL-23",
    url =
      "https://secure.anedot.com/carlaforcongress/direct",
    min = "2020-10-25",
    max = "2020-11-15",
    amount = "1000-500-250-99-50-5",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20200825162402/http://www.kevinvanausdal.com/donate
    last_name = "van ausdal",
    state_cd = "GA-14",
    url =
      "https://www.kevinvanausdal.com/donate",
    min = "2020-08-25",
    max = "2020-09-30",
    amount = "20-50-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101045134/https://secure.actblue.com/donate/cindyaxneforcongress
    last_name = "axne",
    state_cd = "IA-03",
    url =
      "https://secure.actblue.com/donate/cindyaxneforcongress",
    min = "2020-11-01",
    max = "2020-11-13",
    amount = "10-25-100-250-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201113020644/https://secure.winred.com/mark-leyva-for-congress/donate
    last_name = "leyva",
    state_cd = "IN-01",
    url =
      "https://secure.winred.com/mark-leyva-for-congress/donate",
    min = "2020-11-01",
    max = "2020-11-13",
    amount = "25-50-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101085407/https://secure.actblue.com/donate/michelle-for-kansas-website
    last_name = "de la isla",
    state_cd = "KS-02",
    url =
      "https://secure.actblue.com/donate/michelle-for-kansas-website",
    min = "2020-11-01",
    max = "2020-11-17",
    amount = "25-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101105224/https://secure.actblue.com/donate/kennyhoustonforcongress
    last_name = "houston",
    state_cd = "LA-04",
    url =
      "https://secure.actblue.com/donate/kennyhoustonforcongress",
    min = "2020-11-01",
    max = "2020-11-13",
    amount = "25-100-250-1000",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101032654/https://secure.actblue.com/donate/Hilliardforcongress
    last_name = "hilliard",
    state_cd = "MI-04",
    url =
      "https://secure.actblue.com/donate/Hilliardforcongress",
    min = "2020-11-01",
    max = "2020-11-12",
    amount = "10-25-50-100-500-1000",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101022004/https://secure.actblue.com/donate/angiecraig2018
    last_name = "craig",
    state_cd = "MN-02",
    url =
      "https://secure.actblue.com/donate/angiecraig2018",
    min = "2020-11-01",
    max = "2020-11-05",
    amount = "3-25-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101064211/https://secure.actblue.com/donate/dorothydotbenford4congress
    last_name = "benford",
    state_cd = "MS-03",
    url =
      "https://secure.actblue.com/donate/dorothydotbenford4congress",
    min = "2020-11-01",
    max = "2020-11-12",
    amount = "25-100-250-1000",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201005085245/https://actions.ngpvan.com/v1/Forms/C6jeZnRxfUWonMSV5wyOVA2
    last_name = "price",
    state_cd = "NC-04",
    url =
      "https://actions.ngpvan.com/v1/Forms/C6jeZnRxfUWonMSV5wyOVA2",
    min = "2020-10-05",
    max = "2020-11-11",
    amount = "5-10-25-50-100",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201027085901/https://secure.winred.com/van-drew-for-congress/donate
    last_name = "van drew",
    state_cd = "NJ-02",
    url =
      "https://secure.winred.com/van-drew-for-congress/donate",
    min = "2020-10-27",
    max = "2020-11-02",
    amount = "5-10-25-50-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20200625011909/https://secure.actblue.com/donate/founding-donor-nj-04
    last_name = "schmid",
    state_cd = "NJ-04",
    url =
      "https://secure.actblue.com/donate/founding-donor-nj-04",
    min = "2020-06-25",
    max = "2020-11-01",
    amount = "25-100-250-500-1000-2800-5600",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20200711121540/https://secure.anedot.com/mark-razzoli-for-cong/home
    last_name = "razzoli",
    state_cd = "NJ-12",
    url =
      "https://secure.anedot.com/mark-razzoli-for-cong/home",
    min = "2020-07-11",
    max = "2020-08-17",
    amount = "3-12-76-125",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101144924/https://secure.anedot.com/garcia-holmes-for-congress/donate
    last_name = "garcia holmes",
    state_cd = "NM-01",
    url =
      "https://secure.anedot.com/garcia-holmes-for-congress/donate",
    min = "2020-11-01",
    max = "2020-11-15",
    amount = "-999",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101051048/https://secure.actblue.com/donate/xochitl-torres-small
    last_name = "torres small",
    state_cd = "NM-02",
    url =
      "https://secure.actblue.com/donate/xochitl-torres-small",
    min = "2020-11-01",
    max = "2020-11-12",
    amount = "25-50-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101095601/https://secure.actblue.com/donate/teresa-for-nm
    last_name = "fernandez",
    state_cd = "NM-03",
    url =
      "https://secure.actblue.com/donate/teresa-for-nm",
    min = "2020-11-01",
    max = "2020-11-12",
    amount = "10-25-50-100-250-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20200926090421/https://secure.winred.com/george-santos-for-congress/donate
    last_name = "devolder-santos",
    state_cd = "NY-03",
    url =
      "https://secure.winred.com/george-santos-for-congress/donate",
    min = "2020-09-26",
    max = "2020-11-11",
    amount = "25-50-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201023221741/https://www.maureen4congress.com/donate/
    last_name = "mcardle-schulman",
    state_cd = "NY-17",
    url =
      "https://www.maureen4congress.com/donate/",
    min = "2020-10-23",
    max = "2020-11-11",
    amount = "25-50-100-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101095601/https://secure.winred.com/van-de-water-for-congress/donate
    last_name = "van de water",
    state_cd = "NY-19",
    url =
      "https://secure.winred.com/van-de-water-for-congress/donate",
    min = "2020-11-01",
    max = "2020-11-11",
    amount = "25-50-100-250-500-1000-2900",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201005091836/https://secure.piryx.com/donate/Gt0eTiC0/Latta-for-Congress/
    last_name = "latta",
    state_cd = "OH-05",
    url =
      "https://secure.piryx.com/donate/Gt0eTiC0/Latta-for-Congress/",
    min = "2020-10-05",
    max = "2020-11-09",
    amount = "35-50-100-250-500-1000-2500",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201022061129/https://secure.winred.com/campaign-to-elect-chris-christensen-for-congress/donate
    last_name = "christensen",
    state_cd = "OR-01",
    url =
      "https://secure.winred.com/campaign-to-elect-chris-christensen-for-congress/donate",
    min = "2020-10-22",
    max = "2020-10-31",
    amount = "25-50-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101071151/https://secure.actblue.com/donate/mad4pawebsite
    last_name = "dean cunnane",
    state_cd = "PA-04",
    url =
      "https://secure.actblue.com/donate/mad4pawebsite",
    min = "2020-11-01",
    max = "2020-12-05",
    amount = "5-25-50-100-250-500-1000",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101022242/https://secure.actblue.com/donate/depasquale-for-pa-1
    last_name = "depasquale",
    state_cd = "PA-10",
    url =
      "https://secure.actblue.com/donate/depasquale-for-pa-1",
    min = "2020-11-01",
    max = "2021-05-09",
    amount = "25-50-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101095710/https://secure.actblue.com/donate/blair-pn
    last_name = "walshingham",
    state_cd = "TN-01",
    url =
      "https://secure.actblue.com/donate/blair-pn",
    min = "2020-11-01",
    max = "2020-11-05",
    amount = "50-100-200-250-500-1000",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101041352/https://erikastottspearson.com/donate/
    last_name = "stotts-pearson",
    state_cd = "TN-08",
    url =
      "https://erikastottspearson.com/donate/",
    min = "2020-11-01",
    max = "2020-11-09",
    amount = "-999",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20200817065315/https://secure.anedot.com/monica-de-la-cruz-hernandez-for-congress/m4congress
    last_name = "de la cruz hernandez",
    state_cd = "TX-15",
    url =
      "https://secure.anedot.com/monica-de-la-cruz-hernandez-for-congress/m4congress",
    min = "2020-08-17",
    max = "2020-11-05",
    amount = "-999",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101193757/https://secure.winred.com/beth-van-duyne-for-congress/support-today/
    last_name = "van duyne",
    state_cd = "TX-24",
    url =
      "https://secure.winred.com/beth-van-duyne-for-congress/support-today/",
    min = "2020-11-01",
    max = "2020-11-06",
    amount = "50-100-250-500-1000-2800-5600",
    seq_url = 1,
    seq = 1
  ),
  # Discovered that is not a general election candidate
  # tibble(
  #   ## https://web.archive.org/web/20201109213004/https://secure.anedot.com/jim-davis-for-congress/donate
  #   last_name = "davis",
  #   state_cd = "NC-11",
  #   url =
  #     "https://secure.anedot.com/jim-davis-for-congress/donate",
  #   min = "2020-04-20",
  #   max = "2020-11-06",
  #   amount = "20.20-50-100-250-500-2800",
  #   seq_url = 1,
  #   seq = 1
  # ),
  tibble(
    ## https://web.archive.org/web/20201101091111/https://secure.actblue.com/donate/spanberger_website
    last_name = "spanberger",
    state_cd = "VA-07",
    url =
      "https://secure.actblue.com/donate/spanberger_website",
    min = "2020-11-01",
    max = "2020-11-06",
    amount = "50-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201013125910/https://secure.winred.com/jaime-for-congress/website_donation_page
    last_name = "herrera beutler",
    state_cd = "WA-03",
    url =
      "https://secure.winred.com/jaime-for-congress/website_donation_page",
    min = "2020-10-13",
    max = "2020-11-05",
    amount = "25-50-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201012105133/https://secure.winred.com/cathy-mcmorris-rodgers/donate
    last_name = "mcmorris rodgers",
    state_cd = "WA-05",
    url =
      "https://secure.winred.com/cathy-mcmorris-rodgers/donate",
    min = "2020-10-12",
    max = "2020-11-03",
    amount = "10-25-50-100-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201212194533/https://secure.anedot.com/friends-of-doug-basler/basler4congress
    last_name = "basler",
    state_cd = "WA-09",
    url =
      "https://secure.anedot.com/friends-of-doug-basler/basler4congress",
    min = "2020-01-01",
    max = "2020-12-12",
    amount = "25-50-100-250-500-1000-2800-5600",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101093732/https://secure.actblue.com/donate/roger-website
    last_name = "polack",
    state_cd = "WI-01",
    url =
      "https://secure.actblue.com/donate/roger-website",
    min = "2020-11-01",
    max = "2020-12-12",
    amount = "5-10-25-50-100-250-500",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201027085548/https://secure.winred.com/van-orden-for-congress/home
    last_name = "van orden",
    state_cd = "WI-03",
    url =
      "https://secure.winred.com/van-orden-for-congress/home",
    min = "2020-10-27",
    max = "2020-11-03",
    amount = "25-50-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101035630/https://secure.actblue.com/donate/tricia-website
    last_name = "zunker",
    state_cd = "WI-07",
    url =
      "https://secure.actblue.com/donate/tricia-website",
    min = "2020-11-01",
    max = "2020-11-22",
    amount = "10-25-50-100-250-500-1000",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101114410/https://secure.actblue.com/donate/grey-bull-for-congress-1
    last_name = "grey bull",
    state_cd = "WY-0",
    url =
      "https://secure.actblue.com/donate/grey-bull-for-congress-1",
    min = "2020-11-01",
    max = "2020-11-03",
    amount = "25-100-250-1000",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201006195841/https://johsiecruz2020.com/
    last_name = "cruz",
    state_cd = "GA-04",
    url = NA, ## paypal
    min = "2019-12-12",
    max = "2020-11-03",
    amount = NA,
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101025655/https://www.bobgoodforcongress.com/
    last_name = "good",
    state_cd = "VA-05",
    url = "https://secure.anedot.com/good-for-congress/donate", 
    min = "2020-05-14",
    max = "2020-11-03",
    amount = "10-25-100-250-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201101011336/https://www.fallonforcongress.com/
    last_name = "fallon",
    state_cd = "TX-04",
    url = "https://secure.anedot.com/fallonforcongress/donate", 
    min = "2020-08-11",
    max = "2020-11-03",
    amount = "10-25-50-100-250-500-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  tibble(
    ## https://web.archive.org/web/20201111030319/https://secure.actblue.com/donate/carolyn_website
    last_name = "bordeaux",
    state_cd = "GA-07",
    url = "https://secure.actblue.com/donate/carolyn_website",
    min = "2019-04-28",
    max = "2020-11-03",
    amount = "25-50-100-250-1000-2800",
    seq_url = 1,
    seq = 1
  ),
  ## John Moolenaar is particular
  ## See https://web.archive.org/web/20201112221153/https://www.johnmoolenaarforcongress.com/donate
  ## In this case, it's more valid to add all these strings together
  tibble(
    last_name = "moolenaar",
    state_cd = "MI-04",
    url = "https://www.johnmoolenaarforcongress.com/donate",
    min = "2019-10-01",
    max = "2020-11-03",
    amount = "3-25-50-100-250-500-750-1000-1500-2000-2800",
    seq_url = 1,
    seq = 1
  )
) %>%
  bind_rows() %>%
  mutate(
    min = as.Date(min),
    max = as.Date(max),
    last_name = tolower(last_name)
  )

# Missing Senate candidates from data collection ===============================
## Joy Felicia Slade ---> no website, no contribution records
## Annette Davis Jackson ---> no contribution records
## Isakson/Rand Paul retired, McCain died, Shelby had no contribution link
## Bunch of minor candidates in GA/LA

senate_supp <- list(
  tibble(
    ## https://web.archive.org/web/20200614012238/https://deborahforgeorgia.com/
    last_name = "Jackson",
    state = "GA",
    url =
      "https://secure.anedot.com/deborah-jackson-for-us-senate/donate",
    min = "2020-06-14",
    max = "2020-11-09",
    amount = "101-250-500-1000-2800"
  ),
  tibble(
    ## https://web.archive.org/web/20200919042402/https://www.jamesiajames4ussenate.com/
    ## https://web.archive.org/web/20200612032710/https://secure.actblue.com/donate/jamesia-james-for-us-senate-1
    last_name = "James",
    state = "GA",
    url =
      "https://secure.actblue.com/donate/jamesia-james-for-us-senate-1",
    min = "2020-06-12",
    max = "2020-11-09",
    amount = "25-100-250-1000"
  ),
  tibble(
    ## https://web.archive.org/web/20200201143141/www.graysonforga.com
    ## https://web.archive.org/web/20200131112135/https://www.graysonforga.com/donate
    last_name = "Grayson",
    state = "GA",
    url = "https://www.graysonforga.com/donate",
    min = "2020-01-31",
    max = "2020-11-09",
    amount = "1-5-10-25-50-100-250-1000-2500"
  ),
  tibble(
    ## https://web.archive.org/web/20200601222403/http://kandisstaylor.com/
    ## https://web.archive.org/web/20200816201409/https://secure.winred.com/kandiss-taylor-for-u-s-senate/donate
    last_name = "Taylor",
    state = "GA",
    url = "https://secure.winred.com/kandiss-taylor-for-u-s-senate/donate",
    min = "2020-06-01",
    max = "2020-11-09",
    amount = "25-50-100-250-500-1000-2800"
  ),
  tibble(
    ## https://web.archive.org/web/20200305150203/https://tarverforsenate.com/
    ## https://secure.ngpvan.com/J78LV9GDlEOs5GbaeHPe-A2 ---> lost
    ## ActBlue is second of two links, but NGP VAN amounts will never be known!
    ## https://web.archive.org/web/20200915231925/https://secure.actblue.com/donate/tarver-for-senate-1
    last_name = "Tarver",
    state = "GA",
    url = "https://secure.actblue.com/donate/tarver-for-senate-1",
    min = "2020-07-07",
    max = "2020-11-09",
    amount = "2800-1000-500-250-100-25-10"
  )
) %>%
  bind_rows() %>%
  mutate(
    min = as.Date(min),
    max = as.Date(max),
    seq = 1,
    seq_url = 1,
    year = "2020"
  )

# Bind missing data and save ===================================================
dl$house <- bind_rows(dl$house, house_supp)
dl$senate <- bind_rows(dl$senate, senate_supp)
save(dl, file = here("data/tidy/portfolio_summ_federal_first_only.Rda"))

# Create summary statistics ====================================================
dl <- dl %>%
  map(
    ~ .x %>%
      mutate(
        amount = case_when(
          amount == "0-1-2-3" ~ NA_character_,
          amount == "-999" ~ NA_character_,
          TRUE ~ amount
        )
      ) %>%
      mutate(
        amount = case_when(
          last_name == "Sewell" & grepl("2020-", amount) ~
            gsub("2020-", "20.20-", amount),
          TRUE ~ amount
        )
      )
  ) %>%
  map(summ_calc_fxn)

## Unlike 2022 data, I did not have candidate characteristics in the data, 
## so skip merging

save(dl, file = here("data/tidy/portfolio_summ_federal_final_2020.Rda"))
