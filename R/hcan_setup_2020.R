source(here::here("R", "utilities.R"))

# AL: Alabama (last updated Oct 4, 2020)======================================
# AL-01 (last updated Oct 4, 2020) -------------------------------------------
# Carl won vs Hightower, Pringle, Lambert, Castorani. (Rep)
# Averhart won vs. Gardner, Collins.
# Incumbent Byrne lost in Senate primary.
cand_house_2020 <- list(
  tibble(
    first_name = "Jerry Lee",
    last_name = "Carl",
    state_cd = "AL-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jerry Carl for Congress",
    campaign_website = "https://jerrycarlforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/jerry-carl-for-congress/donate",
    FEC_ID_cmte = "C00697789",
    FEC_ID_cand = "H0AL01055"
  ),
  tibble(
    first_name = "James",
    last_name = "Averhart",
    state_cd = "AL-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Campaign to Elect James Averhart",
    campaign_website = "https://jamesaverhart.com/",
    # 25-100-250-1000
    actblue_link = "James-Averhart-1",
    anedot_link = NA,
    # 10-20.20-50-100-250-500
    other_link = "https://jamesaverhart.com/donate/",
    FEC_ID_cmte = "C00708867",
    FEC_ID_cand = "H0AL01097"
  ),
  # AL-02 (last updated Oct 4, 2020) -------------------------------------------
  # Roby not running for reelection: open seat
  # Coleman vs. Taylor
  # vs. Moore vs. King vs. Hasdorff vs. Brown vs. Rogers vs. Dismukes
  # vs. Mathis vs. Harvey-Hall
  # --- Dismukes dropped out on Oct 30, 2019
  # Moore won for Rep and Harvey-Hall for Dems.
  tibble(
    first_name = "Barry", # Felix Barry, goes by Barry
    last_name = "Moore",
    state_cd = "AL-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Barry Moore For Congress",
    campaign_website = "https://barrymooreforcongress.com",
    # No amounts suggested, could be a great baseline
    # Change documented to WinRed on Feb 27, 2020
    # At least from Feb 25, 2020
    # now prompts to 25-50-100-250-500-1000-2800 (true of Sep )
    actblue_link = NA,
    # anedot_link = "barry-moore-for-congress/donate",
    anedot_link = NA,
    other_link = "https://secure.winred.com/BarryMoore/donate",
    FEC_ID_cmte = c("C00720375", "C00639096"),
    FEC_ID_cand = "H8AL02171"
  ),
  tibble(
    first_name = "Phyllis",
    last_name = "Harvey-Hall",
    state_cd = "AL-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Campaign to Elect Phyllis Harvey-Hall",
    # Website is http://harveyhall2congress.com but down
    campaign_website = "https://harveyhall2congress.com/",
    actblue_link = c(
      "phyllis2020",
      "campaign-to-elect-phyllis-harvey-hall-2"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00728873",
    FEC_ID_cand = "H0AL02202"
  ),
  tibble(
    first_name = "Jeff",
    last_name = "Coleman",
    state_cd = "AL-02", party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Coleman for Congress",
    campaign_website = "https://jeffcolemanal.com",
    anedot_link = NA,
    other_link = "https://secure.winred.com/coleman-for-congress/donate/",
    FEC_ID_cmte = "C00715698",
    FEC_ID_cand = "H0AL02145"
  ),
  tibble(
    first_name = "Martha",
    last_name = "Roby",
    state_cd = "AL-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://roby.house.gov",
    campaign_name = "Martha Roby for Congress",
    campaign_website = "https://www.martharoby.com",
    actblue_link = NA,
    # No amounts suggested. Could be a great baseline.
    anedot_link = "mrobyforcongress/homepage",
    other_link = NA,
    FEC_ID_cmte = "C00462143",
    FEC_ID_cand = "H0AL02087"
  ),
  tibble(
    first_name = "Jessica",
    last_name = "Taylor",
    state_cd = "AL-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jessica Taylor for Congress",
    campaign_website = "https://jessicatayloral.com",
    actblue_link = NA,
    anedot_link = NA,
    # 8400? Really?
    other_link = "https://secure.winred.com/jessica-taylor-for-congress/donate",
    FEC_ID_cmte = "C00723759",
    FEC_ID_cand = "H0AL02178"
  ),
  tibble(
    first_name = "Troy",
    last_name = "King",
    state_cd = "AL-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Troy King for Congress",
    campaign_website = "http://www.troyking4congress.com",
    actblue_link = NA,
    anedot_link = "troy-king-for-congress/pgdonate",
    other_link = NA,
    FEC_ID_cmte = "C00718411",
    FEC_ID_cand = "H0AL02152"
  ),
  # Dropped out on Oct 30, 2019
  tibble(
    first_name = "Will",
    last_name = "Dismukes",
    state_cd = "AL-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Will Dismukes for Congress",
    campaign_website = "https://www.willdismukes.com/home/",
    actblue_link = NA,
    anedot_link = "will-dismukes-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00714337",
    FEC_ID_cand = "H0AL02137"
  ),
  tibble(
    first_name = "Terri",
    last_name = "Hasdorff",
    state_cd = "AL-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Terri for Congress",
    campaign_website = "https://www.terriforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/TerriHasdorff/donate",
    FEC_ID_cmte = "C00726109",
    FEC_ID_cand = "H0AL02194"
  ),
  tibble(
    first_name = "Thomas",
    last_name = "Brown",
    state_cd = "AL-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    # Website is https://thomaswbrownjrforcongress.com but cannot be reached
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00723478",
    FEC_ID_cand = "H0AL02160"
  ),
  tibble(
    first_name = "Robert",
    last_name = "Rogers",
    state_cd = "AL-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bob Rogers for U.S. Congress",
    campaign_website = "https://www.bobrogersforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://politics.raisethemoney.com/brogersgXeMNdMr_bLOcfdP4roxwA?",
      "iframe=true"
    ),
    FEC_ID_cmte = "C00725721",
    FEC_ID_cand = "H6AL02159"
  ),
  tibble(
    first_name = "Nathan",
    last_name = "Mathis",
    state_cd = "AL-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nathan Mathis for Congress",
    # Very interesting history behind this politician, but no donation link
    campaign_website = "https://www.mathis2020.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00726034",
    FEC_ID_cand = "H6AL02167"
  ),
  # AL-03 (last updated Oct 4, 2020) -------------------------------------------
  # Rogers vs. Thompson. Cannot find Thompson website
  # Rogers vs. Winfrey. Looks like Thompson dropped out.
  tibble(
    first_name = "Mike",
    last_name = "Rogers",
    state_cd = "AL-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://mikerogers.house.gov",
    campaign_name = "Mike Rogers For Congress",
    campaign_website = "http://www.mikerogersforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # clickandpledge: 10-25-50-100-250.
    # Now 20-30-40-50 (Sep 4, 2020)
    # other_link = "https://co.clickandpledge.com/sp/d1/default.aspx?wid=34425",
    # Essentially same links, but just to make the change clear...
    other_link = paste0(
      "https://connect.clickandpledge.com/w/Form/",
      "7a862fb4-4953-4fef-b11b-b53c533d08e1"
    ),
    FEC_ID_cmte = "C00367862",
    FEC_ID_cand = "H2AL03032"
  ),
  tibble(
    first_name = "Adia",
    last_name = "Winfrey",
    state_cd = "AL-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Committee to Elect Dr. Adia Winfrey",
    campaign_website = "https://www.winfrey4congress.com/",
    actblue_link = "winfrey4congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00669010",
    FEC_ID_cand = "H8AL03062"
  ),
  # AL-04 (last updated Oct 4, 2020) -------------------------------------------
  # Robert Aderholt was uncontested (no candidates registered as of Sep 24, '19)
  # Now Aderholt vs. Neighbors
  tibble(
    first_name = "Robert",
    last_name = "Aderholt",
    state_cd = "AL-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://aderholt.house.gov",
    campaign_name = "Robert Aderholt for Congress",
    campaign_website = "https://aderholtforcongress.org/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed Sep 4, 2020. At least from Aug 16, 2020
    other_link = c(
      "https://transaxt.com/Donate/6HKPUY/RobertAderholtforCongress/",
      paste0(
        "https://secure.winred.com/",
        "robert-aderholt-for-congress/reelect-robert-aderholt/"
      )
    ),
    FEC_ID_cmte = "C00313247",
    FEC_ID_cand = "H6AL04098"
  ),
  tibble(
    first_name = "Ricky",
    last_name = "Neighbors",
    state_cd = "AL-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rick Neighbors for Congress",
    campaign_website = "https://neighbors4congress.com/",
    actblue_link = "neighborshelpingneighbors",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00729764", "C00648329"),
    FEC_ID_cand = "H8AL04086"
  ),
  # AL-05 (last updated Oct 4, 2020) -------------------------------------------
  # Brooks won vs. Lewis in Rep primary. No Dem candidate
  tibble(
    first_name = "Mo",
    last_name = "Brooks",
    state_cd = "AL-05",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://brooks.house.gov",
    campaign_name = "Mo Brooks for Congress",
    campaign_website = "https://mobrooksforcongress.com",
    actblue_link = NA,
    anedot_link = "mobrooks/donate",
    other_link = NA,
    FEC_ID_cmte = "C00464149",
    FEC_ID_cand = "H0AL05163"
  ),
  # AL-06 (last updated Oct 4, 2020) -------------------------------------------
  # Palmer vs Allen vs. Pellegrino. Allen withdrew. No Dem candidates.
  tibble(
    first_name = "Gary",
    last_name = "Palmer",
    state_cd = "AL-06",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://palmer.house.gov",
    campaign_name = "",
    campaign_website = "https://palmerforalabama.com",
    actblue_link = NA,
    anedot_link = NA,
    # At least startinf from Feb 19, 2020
    other_link = "https://secure.winred.com/gary-palmer-for-congress/donate",
    FEC_ID_cmte = "C00551374",
    FEC_ID_cand = "H4AL06098"
  ),
  tibble(
    first_name = "Kaynen",
    last_name = "Pellegrino",
    state_cd = "AL-06",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Pellegrino for Alabama",
    # "https://pellegrinoforalabama.com",
    # website no longer active. Found a wixsite link through his fb.
    campaign_website =
      "https://kaynenpellegrino20.wixsite.com/kaynenpellegrino-1",
    actblue_link = NA,
    anedot_link = NA,
    # 10-15-25-50-100-250-500-1000-1500
    # from his prev website no longer active
    # The GoFundMe: no prompts
    other_link = "https://www.gofundme.com/f/pellegrino-for-congress-2020",
    FEC_ID_cmte = "C00696765",
    FEC_ID_cand = "H0AL06112"
  ),
  # AL-07 (last updated Oct 4, 2020) -------------------------------------------
  # Terri Sewell was uncontested (no candidates registered as of Sep 24, 2019)
  # Now Sewell vs. Stokes but Stokes withdrew. No Rep candidate.
  tibble(
    first_name = "Terri",
    last_name = "Sewell",
    state_cd = "AL-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://sewell.house.gov",
    campaign_name = "Terri Sewell for Congress",
    campaign_website = "https://sewellforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/7910491657490861056",
    # 20.20-35-50-100-500-2800
    FEC_ID_cmte = "C00458976",
    FEC_ID_cand = "H0AL07086"
  ),
  # AK: Alaska (at-large district; last checked Oct 4, 2020) ===================
  # Don Young was uncontested (no candidates registered as of Oct 5, 2019)
  # Now Young vs. Galvin vs. Lamb
  # Young won vs. Nelsen vs. Heikes (Rep) and Galvin won vs. Hibler and Tubatuk
  tibble(
    first_name = "Don",
    last_name = "Young",
    state_cd = "AK-0",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://donyoung.house.gov",
    campaign_name = "Alaskans for Don Young",
    campaign_website = "http://www.alaskansfordonyoung.com/",
    actblue_link = NA,
    # iframe does not show up in Chrome or Firefox
    # anedot_link = "alaskans-for-don-young/general-fund-c6063f677c6",
    # Change documented to WinRed on Mar 21, 2020
    # from 10-25-50-100-250-500-1000-2000-2700 to 5-10-25-50-100-250-500
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/alaskans-for-don-young-inc/ak4donyoung2020",
    FEC_ID_cmte = "C00012229",
    FEC_ID_cand = "H6AK00045"
  ),
  tibble(
    first_name = "Alyse",
    last_name = "Galvin",
    state_cd = "AK-0",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Alyse for Alaska",
    campaign_website = "https://www.alyse4alaska.com",
    actblue_link = "galvinforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00665711",
    FEC_ID_cand = "H8AK00140"
  ),
  # Cannot find Thomas Lamb
  # AZ: Arizona (last updated Oct 8, 2020) =====================================
  # AZ-01 (last updated Oct 8, 2020) -------------------------------------------
  # O'Halleran vs. Shedd vs. Putzova vs. Taylor vs. Mcguire
  # Mcguire seems to be a former state senator but no donation page up
  # O'Halleran vs Shedd in General.
  tibble(
    first_name = "Tom",
    last_name = "O'Halleran",
    state_cd = "AZ-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://ohalleran.house.gov",
    campaign_name = "Tom O'Halleran for Congress",
    campaign_website = "https://www.tomohalleran.com/",
    actblue_link = "ohalleran",
    # This link with donate/* works but website leads to the following link
    # https://secure.actblue.com/contribute/page/ohalleran
    # Same amount, same background
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00582890",
    FEC_ID_cand = "H6AZ01199"
  ),
  tibble(
    first_name = "Tiffany",
    last_name = "Shedd",
    state_cd = "AZ-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tiffany Shedd for Congress",
    campaign_website = "https://www.sheddforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/tiffany-shedd-for-congress/donate",
    # "https://tiffany-shedd-for-congress.revv.co/donate",
    FEC_ID_cmte = c("C00657155", "C00702415"),
    FEC_ID_cand = "H8AZ01237"
  ),
  # Reidhead lost but archived:
  # https://secure.anedot.com/reidhead-for-congress/donate
  # On Jul 10, 2020, 25-50-100-500-1000-2800
  tibble(
    first_name = "Eva",
    last_name = "Putzova",
    state_cd = "AZ-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Eva Putzova for Congress",
    campaign_website = "https://evaforcongress.com/",
    actblue_link = "web-main", # Wow!
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00694091",
    FEC_ID_cand = "H0AZ01358"
  ),
  tibble(
    first_name = "Chris",
    last_name = "Taylor",
    state_cd = "AZ-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chris Taylor for Arizona",
    campaign_website = "https://www.taylorforarizona.com/about",
    actblue_link = NA,
    anedot_link = "chris-taylor-for-arizona/donation",
    other_link = NA,
    FEC_ID_cmte = "C00707471",
    FEC_ID_cand = "H0AZ01366"
  ),
  # AZ-02 (last updated Oct 8, 2020) -------------------------------------------
  # Kirkpatrick vs Martin in General
  tibble(
    first_name = "Ann",
    last_name = "Kirkpatrick",
    state_cd = "AZ-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://kirkpatrick.house.gov/",
    campaign_name = "Kirkpatrick for Congress",
    campaign_website = "https://www.kirkpatrickforcongress.com/",
    actblue_link = "ms_kirkpatrick_homepage",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00651042",
    FEC_ID_cand = "H8AZ01104"
  ),
  tibble(
    first_name = "Brandon Ray",
    last_name = "Martin",
    state_cd = "AZ-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Martin for Freedom",
    campaign_website = "https://www.brandonmartinforcongress.com/",
    actblue_link = NA,
    # Simply linking "martinforfreedom" is not sufficient.
    anedot_link = c(
      "martinforfreedom/pg-e-trump-endorsement",
      "martinforfreedom/donate"
    ),
    other_link = NA,
    FEC_ID_cmte = "C00698373",
    FEC_ID_cand = "H0AZ02190"
  ),
  # Let's... just scrape what remains.
  tibble(
    first_name = "Noran Eric",
    last_name = "Ruden",
    state_cd = "AZ-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Noran Eric Ruden for Congress",
    campaign_website = "https://rudenforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://secure.winred.com/ruden-for-congress/donate",
    other_link = paste0(
      "https://web.archive.org/web/20200816174526/",
      "https://secure.winred.com/ruden-for-congress/donate"
    ),
    # Down but 10-35-100-250-500-1000-2800 on Aug 16, 2020
    FEC_ID_cmte = "C00734087",
    FEC_ID_cand = "H0AZ02265"
  ),
  # Only archived post drop-out, so lost
  tibble(
    first_name = "Shay",
    last_name = "Stautz",
    state_cd = "AZ-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shay Stautz for Congress",
    campaign_website = "http://shaystautz.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710582",
    FEC_ID_cand = "H0AZ02232"
  ),
  # Joseph Morgan had an Anedot: https://secure.anedot.com/jm4cd2/donate
  # No prompts
  # AZ-03 (last updated Oct 8, 2020) -------------------------------------------
  # Raul Grijalva is uncontested (no candidates registered as of Sep 24, 2019)
  # Grijalva vs. Wood for General
  tibble(
    first_name = "Raul",
    last_name = "Grijalva",
    state_cd = "AZ-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://grijalva.house.gov",
    campaign_name = "A whole lof of people for Grijalva",
    campaign_website = "https://www.standwithraul.com/",
    actblue_link = "rg-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00374058",
    FEC_ID_cand = "H2AZ07070"
  ),
  tibble(
    first_name = "Daniel",
    last_name = "Wood",
    state_cd = "AZ-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Daniel Wood for US Congress",
    campaign_website = "https://www.danielwood4congress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/danielwood4congress-com",
    FEC_ID_cmte = "C00725697",
    FEC_ID_cand = "H0AZ03461"
  ),
  # AZ-04 (last updated Oct 8, 2020) -------------------------------------------
  # Gosar vs DiSanto for General. Robinson withdrew before general elec.
  tibble(
    first_name = "Paul",
    last_name = "Gosar",
    state_cd = "AZ-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://gosar.house.gov",
    campaign_name = "Paul Gosar for Congress",
    campaign_website = "https://drpaulgosar.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/paul-gosar-for-congress/donate",
    FEC_ID_cmte = "C00461806",
    FEC_ID_cand = "H0AZ01259"
  ),
  tibble(
    first_name = "Delina",
    last_name = "DiSanto",
    state_cd = "AZ-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Delina DiSanto for Congress",
    campaign_website = "https://www.delinadisanto.com",
    actblue_link = "delina-disanto-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00672154",
    FEC_ID_cand = "H8AZ04165"
  ),
  # WinRed is lost and not archived---shame.
  # Anedot, which is linked from
  # https://web.archive.org/web/20191211194234/
  # https://www.annemarieforcongress.com/donate
  # is still live with no prompts
  tibble(
    first_name = "Anne Marie",
    last_name = "Ward",
    state_cd = "AZ-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Anne Marie Ward for Congress",
    campaign_website = "https://www.annemarieforcongress.com/",
    actblue_link = NA,
    anedot_link = "anne-marie-ward-for-congress/donate",
    other_link = "https://secure.winred.com/annemarieward/donate",
    FEC_ID_cmte = "C00725689",
    FEC_ID_cand = "H0AZ04535"
  ),
  tibble(
    first_name = "Emily",
    last_name = "Robinson",
    state_cd = "AZ-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Emily Robinson for Congress",
    campaign_website = "http://2020robinson.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://politics.raisethemoney.com/ijz5sjkv1wq3c3azpekwrg",
    FEC_ID_cmte = "C00717025",
    FEC_ID_cand = "H0AZ04527"
  ),
  # AZ-05 (last updated Oct 8, 2020) -------------------------------------------
  # Biggs vs. Greene for General.
  tibble(
    first_name = "Andy",
    last_name = "Biggs",
    state_cd = "AZ-05",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://biggs.house.gov",
    campaign_name = "Biggs for Congress",
    campaign_website = "https://biggsforcongress.com",
    actblue_link = NA,
    anedot_link = "biggs-for-congress/c61e8937910dff1246031",
    other_link = NA,
    FEC_ID_cmte = "C00610451",
    FEC_ID_cand = "H6AZ05083"
  ),
  tibble(
    first_name = "Joan",
    last_name = "Greene",
    state_cd = "AZ-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joan Greene for Congress",
    campaign_website = "https://www.joangreeneaz.com/",
    actblue_link = "joangreenewebsite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00640938",
    FEC_ID_cand = "H8AZ05154"
  ),
  # AZ-06 (last updated Oct 8, 2020) -------------------------------------------
  # Schweikert vs. Tipirneni in General.
  tibble(
    first_name = "David",
    last_name = "Schweikert",
    state_cd = "AZ-06",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://schweikert.house.gov/",
    campaign_name = "David Schweikert for Congress",
    campaign_website = "https://davidschweikert.com/",
    actblue_link = NA,
    anedot_link = "friendsofdavidschweikert/donate",
    other_link = NA,
    FEC_ID_cmte = c("C00540617", "C00440727"),
    FEC_ID_cand = "H4AZ06045"
  ),
  tibble(
    first_name = "Hiral Vyas",
    last_name = "Tipirneni",
    state_cd = "AZ-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hiral for Congress",
    campaign_website = "https://hiralforcongress.com",
    actblue_link = "hiraltipirneni",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00649897",
    FEC_ID_cand = "H8AZ08083"
  ),
  tibble(
    first_name = "Anita",
    last_name = "Malik",
    state_cd = "AZ-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Anita for Arizona",
    campaign_website = "https://anitaforarizona.com",
    actblue_link = "anita-malik",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00655134",
    FEC_ID_cand = "H8AZ06103"
  ),
  # AZ-07 (last updated Oct 8, 2020) -------------------------------------------
  # Gallego vs. Barnett
  tibble(
    first_name = "Ruben",
    last_name = "Gallego",
    state_cd = "AZ-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://rubengallego.house.gov/",
    campaign_name = "Ruben Gallego for Arizona",
    campaign_website = "https://gallegoforarizona.com/",
    actblue_link = "suppportrubengallego", # Not typo
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00558627",
    FEC_ID_cand = "H4AZ07043"
  ),
  tibble(
    first_name = "Josh",
    last_name = "Barnett",
    state_cd = "AZ-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Josh Barnett for Arizona Congress",
    campaign_website = "https://www.barnettforaz.com/",
    actblue_link = NA,
    anedot_link = "josh-barnett-for-arizona-congress",
    # Combination of SVGs and PayPal that are difficult to scrape...
    # 10-15-25-50-100
    # other_link = "https://www.barnettforaz.com/donate",
    # Switched to Anedot
    other_link = NA,
    FEC_ID_cmte = "C00702332",
    FEC_ID_cand = "H0AZ07082"
  ),
  # withdrew before primary
  tibble(
    first_name = "Chris",
    last_name = "Hindle",
    state_cd = "AZ-07",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Chris Hindle for Congress 2020",
    campaign_website = "https://hindle2020.com/",
    actblue_link = NA,
    anedot_link = "chris-hindle-for-congress-2020/donate",
    other_link = NA,
    FEC_ID_cmte = "C00713081",
    FEC_ID_cand = "H0AZ07090"
  ),
  # AZ-08 (last updated Oct 8, 2020) -------------------------------------------
  # Lesko vs. Muscato
  # vs. Olsen vs. Musselwhite vs. Rodriguez vs. Sawdy
  # Lesko vs. Muscato for gen election.
  tibble(
    first_name = "Debbie",
    last_name = "Lesko",
    state_cd = "AZ-08",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://lesko.house.gov",
    campaign_name = "Debbie Lesko for Congress",
    campaign_website = "http://debbieleskoforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/DebbieLesko/donate",
    FEC_ID_cmte = "C00663914",
    FEC_ID_cand = "H8AZ08158"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Muscato",
    state_cd = "AZ-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Muscato for Congress",
    campaign_website = "https://www.muscato4congress.com",
    actblue_link = "muscato4congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00703124",
    FEC_ID_cand = "H0AZ08072"
  ),
  tibble(
    first_name = "Bob", # Robert, goes by Bob
    last_name = "Olsen",
    state_cd = "AZ-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bob Olsen for Congress",
    campaign_website = "https://www.bobolsen2020.com",
    actblue_link = "1564895484423189104843489168", # ???
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00715748",
    FEC_ID_cand = "H0AZ08080"
  ),
  tibble(
    first_name = "Bob",
    last_name = "Musselwhite",
    state_cd = "AZ-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bob Musselwhite for Congress",
    campaign_website = "https://bobmusselwhiteforcongress.com",
    actblue_link = "bob-musselwhite-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00673194",
    FEC_ID_cand = "H8AZ08232"
  ),
  tibble(
    first_name = "Jimmy",
    last_name = "Rodriguez",
    state_cd = "AZ-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jimmy 4 Congress",
    campaign_website = "https://jimmy4congress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/jimmy-rodriguez-for-congress/donate",
    FEC_ID_cmte = "C00732875",
    FEC_ID_cand = "H0AZ08098"
  ),
  tibble(
    first_name = "Steven",
    last_name = "Sawdy",
    state_cd = "AZ-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sawdy for Arizona",
    campaign_website = "https://sawdyforarizona.com",
    actblue_link = "s4azlaunch",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00736181",
    FEC_ID_cand = "H8AZ08208"
  ),
  # AZ-09 (last updated Oct 8, 2020) -------------------------------------------
  # Santon vs. Giles for General Election.
  tibble(
    first_name = "Greg",
    last_name = "Stanton",
    state_cd = "AZ-09",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://stanton.house.gov/",
    campaign_name = "Stanton for Congress",
    campaign_website = "https://www.stantonforarizona.com/",
    actblue_link = "campaignlaunch2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00657304",
    FEC_ID_cand = "H8AZ09040"
  ),
  tibble(
    first_name = "David Victor",
    last_name = "Giles",
    state_cd = "AZ-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dave Giles for Congress",
    campaign_website = "https://www.davegilesforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts. This is a new platform...
    other_link =
      "https://simplecheckout.authorize.net/payment/CatalogPayment.aspx",
    FEC_ID_cmte = "C00572602",
    FEC_ID_cand = "H6AZ09010"
  ),
  tibble(
    first_name = "Nicholas",
    last_name = "Tutora",
    state_cd = "AZ-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tutora for Congress",
    campaign_website = "https://tutora2020.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts, no particular platform
    other_link = "https://tutora2020.com/donate",
    FEC_ID_cmte = "C00723288",
    FEC_ID_cand = "H0AZ09021"
  ),
  # AR: Arkansas (last updated Oct 25, 2020) ===================================
  # AR-01 (last updated Oct 25, 2020) ------------------------------------------
  # Rick Crawford is uncontested (no candidates registered as of Oct 5, 2019)
  tibble(
    first_name = "Rick",
    last_name = "Crawford",
    state_cd = "AR-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://crawford.house.gov",
    campaign_name = "Crawford for Congress",
    campaign_website = "https://www.meetrickcrawford.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Same as Wheeler and Morgan. Not scraping well (iframe)
    other_link = "https://politics.raisethemoney.com/ccrawford",
    FEC_ID_cmte = "C00462374",
    FEC_ID_cand = "H0AR01083"
  ),
  # AR-02 (last updated Oct 25, 2020) ------------------------------------------
  # French Hill is uncontested (no candidates registered as of Oct 5, 2019)
  # Hill vs Elliott for General.
  tibble(
    first_name = "French",
    last_name = "Hill",
    state_cd = "AR-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://hill.house.gov",
    campaign_name = "French Hill for Congress",
    campaign_website = "https://www.electfrench.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Same as Wheeler and Morgan. Not scraping well (iframe)
    # 10-25-50-100-250-500-1000-2800 to 25-50-100-250-500-1000-2000-2800
    other_link =
      # "https://politics.raisethemoney.com/mpsxflhxxj3xhbfwquclw",
      "https://politics.raisethemoney.com/fhillazL9AmiyKOwSyThL6AmWPg",
    FEC_ID_cmte = "C00551275",
    FEC_ID_cand = "H4AR02141"
  ),
  tibble(
    first_name = "Joyce Ann",
    last_name = "Elliott",
    state_cd = "AR-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joyce Elliott for Congress",
    campaign_website = "https://www.joyceelliott.com/",
    actblue_link = "websit",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00726943",
    FEC_ID_cand = "H0AR02206"
  ),
  # AR-03 (last updated Oct 25, 2020) ------------------------------------------
  # Womack vs. Williams
  tibble(
    first_name = "Steve",
    last_name = "Womack",
    state_cd = "AR-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://womack.house.gov",
    campaign_name = "Womack for Congress",
    campaign_website = "https://www.womackforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Still piryx
    other_link =
      "https://secure.piryx.com/donate/ysLubgUF/Womack-for-Congress/",
    FEC_ID_cmte = "C00477745",
    FEC_ID_cand = "H0AR03055"
  ),
  tibble(
    first_name = "Celeste",
    last_name = "Williams",
    state_cd = "AR-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Campaign to Elect Celeste Williams",
    campaign_website = "https://www.celestewilliamsforarkansas.com",
    actblue_link = "celesteforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00709667",
    FEC_ID_cand = "H0AR03105"
  ),
  # AR-04 (last updated Jul 11, 2021) ------------------------------------------
  # Westerman vs. Redmond vs. Hanson
  # Cannot find Redmond nor Hanson
  # Westerman vs. Hanson vs. Gilbert in General Elections.
  # Gilbert is on the ballot but cannot find campaign website.
  tibble(
    first_name = "Bruce",
    last_name = "Westerman",
    state_cd = "AR-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://westerman.house.gov",
    campaign_name = "Westerman For Congress",
    campaign_website = "https://www.brucewesterman.com",
    actblue_link = NA,
    anedot_link = NA,
    # now directs to winred link
    other_link = "https://secure.winred.com/westerman-for-congress/donate",
    # "https://politics.raisethemoney.com/westermanar",
    FEC_ID_cmte = "C00548180",
    FEC_ID_cand = "H4AR04048"
  ),
  tibble(
    first_name = "William",
    last_name = "Hanson",
    state_cd = "AR-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "William Hanson For Congress",
    campaign_website = "https://williamhansonforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-1000-2800
    other_link = "https://secure.numero.ai/contribute/WilliamHanson",
    FEC_ID_cmte = "C00717439",
    FEC_ID_cand = "H0AR04087"
  ),
  tibble(
    first_name = "Frank",
    last_name = "Gilbert",
    state_cd = "AR-04",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Frank Gilbert For Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00740530",
    FEC_ID_cand = "H0AR04095"
  ),
  # CA: California (last updated Jul 15, 2021) =================================
  # CA-01 ---------------------------------------------------------------------
  # LaMalfa vs. Denney for General Election
  tibble(
    first_name = "Doug",
    last_name = "LaMalfa",
    state_cd = "CA-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://lamalfa.house.gov",
    campaign_name = "Doug LaMalfa For Congress",
    campaign_website = "http://douglamalfa.com/",
    actblue_link = NA,
    anedot_link = NA,
    # uses efundraising connections, not sure about scrape potential but uses:
    # 10-25-50-100-500-5600
    other_link = "https://www.efundraisingconnections.com/c/lamalfaforcongress/",
    FEC_ID_cmte = "C00548180",
    FEC_ID_cand = "H2CA02142"
  ),
  tibble(
    first_name = "Audrey",
    last_name = "Denney",
    state_cd = "CA-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Audrey Denney For Congress",
    campaign_website = "https://audreyforcongress.com/",
    actblue_link = "web_button",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00664409",
    FEC_ID_cand = "H8CA01257"
  ),
  # CA-02 ---------------------------------------------------------------------
  # Jared Huffman is uncontested (no candidates registered as of Sep 24, 2019)
  # Dale Mensing opponent in General
  tibble(
    first_name = "Jared",
    last_name = "Huffman",
    state_cd = "CA-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://huffman.house.gov",
    campaign_name = "Jared Huffman for Congress",
    campaign_website = "https://www.jaredhuffman.com/",
    actblue_link = NA,
    anedot_link = NA,
    # No suggested amounts. Could be a good baseline.
    # still no suggested about but the box for amount fills initially at 5.00 for me. (9/11/2020)
    other_link = "https://act.myngp.com/Forms/5698309771284056064",
    FEC_ID_cmte = "C00536680",
    FEC_ID_cand = "H2CA06259"
  ),
  tibble(
    first_name = "Dale Kenneth",
    last_name = "Mensing",
    state_cd = "CA-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dale K Mensing for Congress",
    campaign_website = "https://www.theflamingarrow.com/",
    actblue_link = NA,
    anedot_link = NA,
    # could not find any campaign donation links.
    # I see on FEC website that he has colected a total of $991.
    other_link = NA,
    FEC_ID_cmte = "C00543553",
    FEC_ID_cand = "H4CA02098"
  ),
  # CA-03 ---------------------------------------------------------------------
  tibble(
    first_name = "John",
    last_name = "Garamendi",
    state_cd = "CA-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://garamendi.house.gov",
    campaign_name = "Garamendi for Congress",
    campaign_website = "https://www.garamendi.org/",
    actblue_link = NA,
    anedot_link = NA,
    # 20-35-50-100-500-2800
    other_link = "https://www.garamendi.org/contribute",
    FEC_ID_cmte = "C00462697",
    FEC_ID_cand = "H0CA10149"
  ),
  tibble(
    first_name = "Tamika",
    last_name = "Hamilton",
    state_cd = "CA-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tamika Hamilton for Congress",
    campaign_website = "https://votetamika.org/",
    actblue_link = NA,
    anedot_link = "tamika-hamilton-for-congress/contribute",
    other_link = NA,
    FEC_ID_cmte = "C00706374",
    FEC_ID_cand = "H0CA03128"
  ),
  # CA-04 ---------------------------------------------------------------------
  tibble(
    first_name = "Thomas",
    last_name = "McClintock",
    state_cd = "CA-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://mcclintock.house.gov",
    campaign_name = "McClintock For Congress",
    campaign_website = "https://tommcclintock.com//",
    actblue_link = NA,
    anedot_link = NA,
    # uses efundraising connections, not sure about scrape potential but uses:
    # 10-25-50-100-500-5400
    other_link = "https://www.efundraisingconnections.com/c/TomMcClintock/",
    FEC_ID_cmte = "C00446815",
    FEC_ID_cand = "H8CA04152"
  ),
  tibble(
    first_name = "Brynne",
    last_name = "Kennedy",
    state_cd = "CA-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brynne Kennedy For Congress",
    campaign_website = "https://brynneforcongress.com/",
    actblue_link = "brynne_website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00704650",
    FEC_ID_cand = "H0CA04175"
  ),
  # CA-05 ---------------------------------------------------------------------
  tibble(
    first_name = "Mike",
    last_name = "Thompson",
    state_cd = "CA-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://mikethompson.house.gov",
    campaign_name = "Mike Thompson for Congress",
    campaign_website = "https://www.mikethompsonforcongress.com//",
    actblue_link = NA,
    anedot_link = NA,
    # uses fastaction with following amounts:
    # 20-35-50-100-500-2800
    other_link = "https://www.mikethompsonforcongress.com/donate",
    FEC_ID_cmte = "C00326363",
    FEC_ID_cand = "H8CA01109"
  ),
  tibble(
    first_name = "Scott",
    last_name = "Giblin",
    state_cd = "CA-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Giblin 4 Congress",
    campaign_website = "https://www.giblin4congress.com/",
    actblue_link = NA,
    anedot_link = "giblin-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00714592",
    FEC_ID_cand = "H0CA05107"
  ),
  # CA-06 ---------------------------------------------------------------------
  tibble(
    first_name = "Doris",
    last_name = "Matsui",
    state_cd = "CA-06",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://matsui.house.gov",
    campaign_name = "Matsui for Congress",
    campaign_website = "https://www.matsuiforcongress.com/",
    actblue_link = "matsui-web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Christine",
    last_name = "Bish",
    state_cd = "CA-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bish for Congress 2020",
    campaign_website = "https://bishforcongress.org/",
    actblue_link = NA,
    anedot_link = NA,
    # fundhero but 25-100-500-2800
    other_link = "https://donate.fundhero.io/bishforcongress",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-07 ---------------------------------------------------------------------
  tibble(
    first_name = "Amerish",
    last_name = "Bera",
    state_cd = "CA-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://bera.house.gov",
    campaign_name = "Bera for Congress",
    campaign_website = "https://beraforcongress.com/",
    actblue_link = "bera-homepage",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Buzz",
    last_name = "Patterson",
    state_cd = "CA-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Buzz Patterson for Congress",
    campaign_website = "https://buzz4congress.com/",
    actblue_link = NA,
    # $11,200 (!!!!) - captioned double maximum contribution per couple!
    anedot_link = "buzz-patterson/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-08 ---------------------------------------------------------------------
  tibble(
    first_name = "Christine",
    last_name = "Bubser",
    state_cd = "CA-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chris Bubser for Congress",
    campaign_website = "https://www.chrisbubser.com/",
    actblue_link = "chris-bubser-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Jay",
    last_name = "Obernolte",
    state_cd = "CA-08",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jay Obernolte for Congress 2020",
    campaign_website = "http://electjay.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/JayObernolte/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-09 ---------------------------------------------------------------------
  # Jerry McNerney is uncontested (no candidates registered as of Sep 24, 2019)
  tibble(
    first_name = "Jerry",
    last_name = "McNerney",
    state_cd = "CA-09",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://mcnerney.house.gov",
    campaign_name = "Jerry McNerney for Congress",
    campaign_website = "https://jerrymcnerney.org/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://secure.ngpvan.com/VHSW6b1ewk-YWY8XloCLZw2",
    # I see "https://secure.ngpvan.com/VHSW6b1ewk-YWY8XloCLZw2"
    # but maybe I can understand this in the meeting:
    other_link = "https://secure.ngpvan.com/v1/Forms/VHSW6b1ewk-YWY8XloCLZw2",
    FEC_ID_cmte = "C00398644",
    FEC_ID_cand = "H4CA11081"
  ),
  tibble(
    first_name = "Antonio",
    last_name = "Amador",
    state_cd = "CA-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tony Amador for Congress",
    campaign_website = "http://www.amadorforcongress.net/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-2800
    other_link = "https://www.efundraisingconnections.com/c/Amador/",
    FEC_ID_cmte = "C00728618",
    FEC_ID_cand = "H0CA09125"
  ),
  # CA-10 -----------------------------------------
  # Harder vs. Howze
  # vs. Elliott vs. Sousa Livengood
  # vs. Denham vs. Dosstt vs. Barkley vs. Blevins
  # Harder vs Howze in General
  tibble(
    first_name = "Josh",
    last_name = "Harder",
    state_cd = "CA-10",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://harder.house.gov",
    campaign_name = "Josh Harder for Congress",
    campaign_website = "https://www.harderforcongress.com",
    actblue_link = "jharder",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00639146",
    FEC_ID_cand = "H8CA10126"
  ),
  tibble(
    first_name = "Ted",
    last_name = "Howze",
    state_cd = "CA-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ted Howze for Congress",
    campaign_website = "https://tedhowze.com",
    actblue_link = NA,
    anedot_link = "howzeforcongress/website",
    other_link = NA,
    FEC_ID_cmte = "C00681544",
    FEC_ID_cand = "H8CA10209"
  ),
  tibble(
    first_name = "Bob",
    last_name = "Elliott",
    state_cd = "CA-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bob Elliott for Congress",
    campaign_website = "https://bobelliottforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/BobElliott/donate",
    FEC_ID_cmte = "C00706333",
    FEC_ID_cand = "H0CA10271"
  ),
  tibble(
    first_name = "Marla",
    last_name = "Sousa Livengood", # Just goes as Livengood
    state_cd = "CA-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marla Livengood for Congress",
    campaign_website = "http://www.marlalivengood.com",
    actblue_link = NA,
    anedot_link = "livengoodforcongress/donate2020",
    other_link = NA,
    FEC_ID_cmte = "C00703363",
    FEC_ID_cand = "H0CA10263"
  ),
  tibble(
    first_name = "Jeff",
    last_name = "Denham",
    state_cd = "CA-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Denham for Congress",
    campaign_website = "https://jeffdenham.com",
    actblue_link = NA,
    anedot_link = NA,
    # Revv website
    other_link = "https://secure.jeffdenham.com/donate",
    FEC_ID_cmte = "C00473272",
    FEC_ID_cand = "H0CA19173"
  ),
  # Charles Dossett withdrew
  tibble(
    first_name = "Mike",
    last_name = "Barkley",
    state_cd = "CA-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    # Website is http://www.mjbarkl.com/run.htm but is down
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00495507",
    FEC_ID_cand = "H2CA00096"
  ),
  tibble(
    first_name = "Ryan",
    last_name = "Blevins",
    state_cd = "CA-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ryan Blevins for Congress",
    campaign_website = "https://www.blevinsforcongress.com",
    actblue_link = "ryan-blevins",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00722298",
    FEC_ID_cand = "H0CA10289"
  ),
  # CA-11 ---------------------------------------------------------------------
  # Mark DeSaulnier is uncontested (no candidates registered as of Sep 24, '19)
  tibble(
    first_name = "Mark",
    last_name = "DeSaulnier",
    state_cd = "CA-11",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://desaulnier.house.gov",
    campaign_name = "Mark DeSaulnier For Congress",
    campaign_website = "http://desaulnierforcongress.com/",
    actblue_link = "desaulnier2016",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00554709",
    FEC_ID_cand = "H0CA10073"
  ),
  tibble(
    first_name = "Nisha",
    last_name = "Sharma",
    state_cd = "CA-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nisha For Congress",
    campaign_website = "https://www.nishaforcongress.com/",
    actblue_link = NA,
    anedot_link = "nisha-sharma-for-congress/contribute",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-12 ---------------------------------------------------------------------
  tibble(
    first_name = "Nancy",
    last_name = "Pelosi",
    state_cd = "CA-12",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://pelosi.house.gov",
    campaign_name = "Nancy Pelosi for Congress",
    campaign_website = "https://www.pelosiforcongress.org/",
    actblue_link = c(
      "npdefault",
      "npads2020",
      "nancy-pelosi-2"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00213512",
    FEC_ID_cand = "H8CA05035"
  ),
  tibble(
    first_name = "Shahid",
    last_name = "Buttar",
    state_cd = "CA-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shahid Buttar for Congress",
    campaign_website = "https://shahidforchange.us/",
    actblue_link = "shahidforchange",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-13 ---------------------------------------------------------------------
  # Lee vs. Piterman
  tibble(
    first_name = "Barbara",
    last_name = "Lee",
    state_cd = "CA-13",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://lee.house.gov",
    campaign_name = "Barbara Lee for Congress",
    campaign_website = "https://barbaraleeforcongress.org/",
    actblue_link = "generalform",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00331769",
    FEC_ID_cand = "H8CA09060"
  ),
  tibble(
    first_name = "Nikka",
    last_name = "Piterman",
    state_cd = "CA-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.nikka2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 10-100-1000
    # still same amounts (sept 11)
    other_link = paste0(
      "https://www.paypal.com/donate/?token=dlNjwfvxD9FGk2ccOf4oA73lXFLRRmkl",
      "_42JyGF4NN_Y2xu_MnBruyANke9FCwRVsL1CT0&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00708214",
    FEC_ID_cand = "H0CA13127"
  ),
  # CA-14 ---------------------------------------------------------------------
  # Jackie Speier was uncontested (no candidates registered as of Sep 24, 2019)
  # Speier vs. Taylor
  # Goodrow has suspended campaign
  tibble(
    first_name = "Jackie",
    last_name = "Speier",
    state_cd = "CA-14",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://speier.house.gov",
    campaign_name = "Jackie Speier for Congress",
    campaign_website = "https://www.jackiespeier.org/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/7640768585564293120",
    FEC_ID_cmte = "C00443705",
    FEC_ID_cand = "H8CA12171"
  ),
  tibble(
    first_name = "Eric",
    last_name = "Taylor",
    state_cd = "CA-14",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Taylor2020",
    campaign_website = "https://erictaylor.house/",
    actblue_link = NA,
    anedot_link = NA,
    # Um... his own contribution platform
    # 3-28-50-100-250
    other_link = "https://erictaylor.house/contribute",
    FEC_ID_cmte = "C00443705",
    FEC_ID_cand = "H8CA12171"
  ),
  tibble(
    first_name = "Ran",
    last_name = "Petel",
    state_cd = "CA-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Petel 4 Congress",
    campaign_website = "https://petel4congress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://petel4congress.com/donations/donation-form",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-15 ---------------------------------------------------------------------
  # Eric Swalwell is a former presidential candidate
  # Swalwell vs. Wahab
  # vs. Wieckowski vs. Liu vs. Hayden vs. Intal
  # Wieckowski dropped out, cannot find Hayden nor Intal
  # Liu campaign no formal website https://www.facebook.com/PeterLiuCAGov18/
  tibble(
    first_name = "Eric",
    last_name = "Swalwell",
    state_cd = "CA-15",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://swalwell.house.gov",
    campaign_name = "Swalwell for Congress",
    campaign_website = "https://ericswalwell.com/",
    actblue_link = "eric-swalwell-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00502294",
    FEC_ID_cand = "H2CA15094"
  ),
  tibble(
    first_name = "Aisha",
    last_name = "Wahab",
    state_cd = "CA-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Aisha Wahab for Congress",
    campaign_website = "https://aishawahab.com/",
    actblue_link = "ca15",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00702076",
    FEC_ID_cand = "H0CA15213"
  ),
  tibble(
    first_name = "Alison",
    last_name = "Hayden",
    state_cd = "CA-15",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Alison for Congress 2020",
    campaign_website = "https://alison-hayden-for-congress1.constantcontactsites.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/alisonhayden/",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-16 ---------------------------------------------------------------------
  tibble(
    first_name = "Jim",
    last_name = "Costa",
    state_cd = "CA-16",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://costa.house.gov/",
    campaign_name = "Jim Costa for Congress",
    campaign_website = "https://www.jimcosta.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-1000-2800
    other_link = "https://www.jimcosta.com/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Kevin",
    last_name = "Cookingham",
    state_cd = "CA-16",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cookingham for Congress",
    campaign_website = "https://cookinghamforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 199-250-500-1000-2800-5600-11200!!!!
    other_link = "https://www.efundraisingconnections.com/c/Cookingham/",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-17 ---------------------------------------------------------------------
  tibble(
    first_name = "Ro", # Rohit but goes by Ro
    last_name = "Khanna",
    state_cd = "CA-17",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://khanna.house.gov/",
    campaign_name = "Ro for Congress",
    campaign_website = "https://www.rokhanna.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 27-100-500-1000-2800-5600
    other_link = "https://www.rokhanna.com/contribute",
    FEC_ID_cmte = "C00503185",
    FEC_ID_cand = "H4CA12055"
  ),
  tibble(
    first_name = "Ritesh",
    last_name = "Tandon",
    state_cd = "CA-17",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tandon for Congress",
    campaign_website = "https://www.tandonforcongress.com/",
    actblue_link = NA,
    anedot_link = "tandon-for-congress/donate",
    other_link = "https://www.tandonforcongress.com/donation/",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-18 ---------------------------------------------------------------------
  tibble(
    first_name = "Anna",
    last_name = "Eshoo",
    state_cd = "CA-18",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://eshoo.house.gov/",
    campaign_name = "Anna Eshoo",
    campaign_website = "https://annaeshoo4congress.com/",
    actblue_link = "annaeshoo",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Rishi",
    last_name = "Kumar",
    state_cd = "CA-18",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kumar for Congress",
    campaign_website = "https://rishikumar.com/",
    actblue_link = NA,
    anedot_link = NA,
    # fastaction link 1000-2800-5600
    other_link = "https://rishikumar.com/contribute",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-19 ---------------------------------------------------------------------
  # Lofgren vs. Cruz vs. Torres
  # Issaq withdrew before the primary election on March 3, 2020.
  tibble(
    first_name = "Zoe",
    last_name = "Lofgren",
    state_cd = "CA-19",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://lofgren.house.gov/",
    campaign_name = "Zoe Lofgren",
    campaign_website = "https://zoelofgren.com/",
    actblue_link = c("zoe-lofgren-3", "zl-website"), # zl-website link appears as popup
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00289603",
    FEC_ID_cand = "H4CA16049"
  ),
  # # Originally CA-39 filed but perhaps a mistake?
  tibble(
    first_name = "Ignacio",
    last_name = "Cruz",
    state_cd = "CA-19",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ignacio Cruz for Congress",
    campaign_website = "https://cruzusa.org/",
    actblue_link = NA,
    anedot_link = NA,
    # I don't even know how this site works and not scraping it now
    # 5-25-50-100-200-500-1000-2000-2800
    # It is timing out as well as of Sep 22, 2019
    other_link = NA,
    FEC_ID_cmte = "C00703793",
    FEC_ID_cand = "H0CA39114"
  ),
  tibble(
    first_name = "Ivan",
    last_name = "Torres",
    state_cd = "CA-19",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ivan Torres 2020",
    campaign_website = "https://ivantorres2020.com/",
    actblue_link = "ivantorres2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00708404",
    FEC_ID_cand = "H0CA19207"
  ),
  tibble(
    first_name = "Justin",
    last_name = "Aguilera",
    state_cd = "CA-19",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Justin Aguilera for Congress",
    campaign_website = "https://www.justinaguilera.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 100-250-500-1000-2800-5600
    other_link = "https://www.efundraisingconnections.com/c/JustinAguilera/",
    FEC_ID_cmte = "C00683532",
    FEC_ID_cand = "H8CA19051"
  ),
  # CA-20 ---------------------------------------------------------------------
  # Panetta was uncontested as of Sep 17, 2019
  # Now Panetta vs. Costa (Filed Sep 22, 2019)
  # Panetta vs Gorman for General
  tibble(
    first_name = "Jimmy",
    last_name = "Panetta",
    state_cd = "CA-20",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://panetta.house.gov",
    campaign_name = "Jimmy Panetta for Congress",
    campaign_website = "https://www.jimmypanetta.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/-3426693289772775424",
    FEC_ID_cmte = "C00592154",
    FEC_ID_cand = "H6CA20152"
  ),
  tibble(
    first_name = "Jeff",
    last_name = "Gorman",
    state_cd = "CA-20",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jeff Gorman for Congress",
    campaign_website = "https://jeffforcongress.com/",
    actblue_link = NA,
    anedot_link = "jeff-gorman-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00722363",
    FEC_ID_cand = "H0CA20122"
  ),
  # Why both incumbents?!
  # I think this may be an FEC error. Jim Costa is CA-16.
  tibble(
    first_name = "Jim",
    last_name = "Costa",
    state_cd = "CA-20",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://costa.house.gov",
    campaign_name = "Jim Costa for Congress",
    campaign_website = "http://www.jimcosta.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://cm.aristotle.com/d/costa1",
    FEC_ID_cmte = "C00391029",
    FEC_ID_cand = "H4CA20082"
  ),
  # CA-21 ---------------------------------------------------------------------
  # Cox vs. Valadao
  tibble(
    first_name = "TJ",
    last_name = "Cox",
    state_cd = "CA-21",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://cox.house.gov",
    campaign_name = "TJ Cox for Congress",
    campaign_website = "https://tjcoxforcongress.com",
    actblue_link = c("cox_web_2018", "tj-homepage"), # now has tj-homepage
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00648956",
    FEC_ID_cand = "H8CA10167"
  ),
  tibble(
    first_name = "David",
    last_name = "Valadao",
    state_cd = "CA-21",
    party = "Rep",
    incumbent = FALSE, # Formerly, yes.
    office_website = NA,
    campaign_name = "Valadao for Congress",
    campaign_website = "https://valadaoforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/valadao-for-congress/donate",
    FEC_ID_cmte = "C00499392",
    FEC_ID_cand = "H2CA20094"
  ),
  # CA-22 ---------------------------------------------------------------------
  # Nunes vs. Arballo vs. Bliatout vs. Rezvani vs. Garcia
  # Cannot find Garcia
  tibble(
    first_name = "Devin",
    last_name = "Nunes",
    state_cd = "CA-22",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://nunes.house.gov",
    campaign_name = "The Devin Nunes Campaign",
    campaign_website = "https://www.devinnunes.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Nov 26, 2019 (same in Sept, 2020)
    # 20-50-250-500-1500-2800-5600 to 25-50-100-250-500-1000-2800
    other_link = paste0(
      "https://secure.winred.com/devin-nunes-campaign-committee/",
      "nunes-home-page"
    ),
    # other_link = "https://cm.aristotle.com/d/DevinNunes",
    # other_link = NA,
    FEC_ID_cmte = "C00370056",
    FEC_ID_cand = "H8CA20059"
  ),
  tibble(
    first_name = "Phil",
    last_name = "Arballo",
    state_cd = "CA-22",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Phil Arballo for Congress",
    # The main page actually yields errors
    campaign_website = "https://www.philarballo.com/",
    actblue_link = c(
      "arb_monthly_website", "arb_monthly_logo", "arb_monthly_social"
    ), # first link is what is in his website
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00708479",
    FEC_ID_cand = "H0CA22102"
  ),
  tibble(
    first_name = "Bobby",
    last_name = "Bliatout",
    state_cd = "CA-22",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bobby Bliatout For Congress, 2020",
    campaign_website = "https://bobbybliatout.com/",
    actblue_link = "bobby-bliatout-for-congress--2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00708529",
    FEC_ID_cand = "H8CA22154"
  ),
  tibble(
    first_name = "Dary",
    last_name = "Rezvani Sarabi", # Goes as Dary Rezvani
    state_cd = "CA-22",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dary for Congress",
    campaign_website = "https://daryforcongress.com/",
    actblue_link = "darywebsite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00707547",
    FEC_ID_cand = "H0CA22094"
  ),
  # CA-23 ---------------------------------------------------------------------
  # McCarthy vs. Mangone
  tibble(
    first_name = "Kevin",
    last_name = "McCarthy",
    state_cd = "CA-23",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://kevinmccarthy.house.gov",
    campaign_name = "Kevin McCarthy for Congress",
    campaign_website = "https://www.mccarthyforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/kevin-mccarthy/contribute",
    FEC_ID_cmte = "C00420935",
    FEC_ID_cand = "H6CA22125"
  ),
  tibble(
    first_name = "Kim",
    last_name = "Mangone",
    state_cd = "CA-23",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.mangoneforcongress.com/",
    # Has both ActBlue and non-ActBlue donation links(!)
    # Non-ActBlue: 25-50-100-250-1000-2500
    # ActBlue: 25-100-250-1000
    # Absolutely no idea why there's a difference
    # his campaign website has it now direct link to the actblue link. (09.2020)
    actblue_link = "mangoneforcongress.com",
    anedot_link = NA,
    other_link = "https://www.mangoneforcongress.com/donations",
    FEC_ID_cmte = "C00709550",
    FEC_ID_cand = "H0CA23167"
  ),
  # CA-24 ---------------------------------------------------------------------
  tibble(
    first_name = "Salud",
    last_name = "Carbajal",
    state_cd = "CA-24",
    party = "Dem",
    incumbent = FALSE,
    office_website = "https://carbajal.house.gov/",
    campaign_name = "Salud Carbajal for Congress",
    campaign_website = "https://saludcarbajal.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 2700-1000-500-100-50-25 (in reverse for interesting reason)
    other_link = "https://act.myngp.com/Forms/2166643238495455232",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Andy",
    last_name = "Caldwell",
    state_cd = "CA-24",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Andy Caldwell for Congress",
    campaign_website = "https://andycaldwell2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 20.01-25-50-100-500-2800
    other_link = "https://www.efundraisingconnections.com/c/AndyCaldwell/",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-25 (last updated Apr 2, 2020) ------------------------------------------
  # Garcia vs. Smith is the General
  # Hill resigned among scandal on Oct 27, 2019, but webpage/actblue still live
  tibble(
    first_name = "Katie",
    last_name = "Hill",
    state_cd = "CA-25",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://katiehill.house.gov",
    campaign_name = "Katie Hill for Congress",
    campaign_website = "https://www.katiehillforcongress.com/",
    actblue_link = "katie-hill-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00634212",
    FEC_ID_cand = "H8CA25074"
  ),
  tibble(
    first_name = "Mike",
    last_name = "Garcia",
    state_cd = "CA-25",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mike Garcia for Congress",
    campaign_website = "https://www.electmikegarcia.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.efundraisingconnections.com/c/MikeGarcia/",
    FEC_ID_cmte = c("C00701102", "C00702456"),
    FEC_ID_cand = "H0CA25105"
  ),
  tibble(
    first_name = "Christy",
    last_name = "Smith",
    state_cd = "CA-25",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Christy for Congress",
    campaign_website = "https://www.christyforcongress.org",
    actblue_link = "christy-for-congress-1-web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00725101",
    FEC_ID_cand = "H0CA25154"
  ),
  # Uygur (endorsement from Sanders rescinded?) lost primary, website is down
  tibble(
    first_name = "Angela",
    last_name = "Underwood Jacobs",
    state_cd = "CA-25",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Angela Underwood Jacobs for Congress",
    campaign_website = "https://www.angelaforyou.com/about",
    actblue_link = NA,
    anedot_link = "aunderwood/donate",
    other_link = NA,
    FEC_ID_cmte = "C00701946",
    FEC_ID_cand = "H0CA25113"
  ),
  tibble(
    first_name = "Steve",
    last_name = "Knight",
    state_cd = "CA-25",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Steve Knight for Congress",
    campaign_website = "http://steveknight.org",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://www.efundraisingconnections.com/c/SteveKnightCongress/",
    FEC_ID_cmte = "C00554014",
    FEC_ID_cand = "H4CA25123"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Cripe", # Had typo as Cripes. Correct folder later
    state_cd = "CA-25",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Cripe for Congress",
    campaign_website = "https://www.markcripe.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.efundraisingconnections.com/c/MarkCripe/",
    FEC_ID_cmte = "C00704957",
    FEC_ID_cand = "H0CA25121"
  ),
  tibble(
    first_name = "Anibal",
    last_name = "Valdez-Ortega",
    state_cd = "CA-25",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Valdez-Ortega for Congress",
    campaign_website = "https://valdezortegaforcongress.org",
    actblue_link = "valdez-ortega-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00729590",
    FEC_ID_cand = "H0CA25212"
  ),
  tibble(
    first_name = "David",
    last_name = "Rudnick",
    state_cd = "CA-25",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rudnick for Congress",
    campaign_website = "http://rudnickforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.efundraisingconnections.com/c/MarkCripe/",
    FEC_ID_cmte = "C00719047",
    FEC_ID_cand = "H0CA25139"
  ),
  # Still no website for Papadopoulos; lost primary anyway.
  # Valladares running for 38th Assembly
  tibble(
    first_name = "David",
    last_name = "Lozano",
    state_cd = "CA-25",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Lozano for Congress",
    campaign_website = "https://lozanoforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.efundraisingconnections.com/c/DavidLozano/",
    FEC_ID_cmte = "C00729798",
    FEC_ID_cand = "H0CA25220"
  ),
  tibble(
    first_name = "Robert",
    last_name = "Cooper",
    state_cd = "CA-25",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Robert Cooper for Congress",
    campaign_website = "https://cooper4congress.org",
    actblue_link = "robert-cooper-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00730184",
    FEC_ID_cand = "H0CA25238"
  ),
  tibble(
    first_name = "Frederick David",
    last_name = "Rudnick",
    state_cd = "CA-25",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Rudnick for Congress",
    campaign_website = "https://rudnickforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/david-rudnick-for-congress",
    FEC_ID_cmte = "C00719047",
    FEC_ID_cand = "H0CA25139"
  ),
  tibble(
    first_name = "Kenneth",
    last_name = "Jenks",
    state_cd = "CA-25",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kenneth Jenks for Congress",
    campaign_website = "https://www.kennethjenksforcongress.com",
    actblue_link = NA,
    anedot_link = "jenks2020/contribute",
    other_link = NA,
    FEC_ID_cmte = "C00728915",
    FEC_ID_cand = "H0CA25188"
  ),
  tibble(
    first_name = "Getro",
    last_name = "Elize",
    state_cd = "CA-25",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Getro Elize for Congress",
    campaign_website = "https://getroelize.com",
    actblue_link = "getro-elize-campaign",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00728774",
    FEC_ID_cand = "H0CA25196"
  ),
  # Lackey website down
  # CA-26 ---------------------------------------------------------------------
  # Brownley vs. Salas vs. Kennedy
  tibble(
    first_name = "Julia",
    last_name = "Brownley",
    state_cd = "CA-26",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://juliabrownley.house.gov",
    campaign_name = "Julia Brownley for Congress",
    campaign_website = "https://juliabrownley.com/",
    actblue_link = "brownley-homepage",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00513077",
    FEC_ID_cand = "H2CA00120"
  ),
  tibble(
    first_name = "Robert",
    last_name = "Salas",
    state_cd = "CA-26",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    # Nothing to scrape now if this is a GoFundMe stage
    other_link =
      "https://www.gofundme.com/f/testing-the-waters-for-congress-salas",
    FEC_ID_cmte = "C00718817",
    FEC_ID_cand = "H0CA26137"
  ),
  tibble(
    first_name = "Ronda",
    last_name = "Baldwin-Kennedy", # Baldwin-Kennedy, goes by Kennedy
    state_cd = "CA-26",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ronda Kennedy for Congress 2020",
    campaign_website = "https://rondakennedy.com/",
    actblue_link = NA,
    anedot_link = NA, # "ronda-kennedy-for-congress/website-splash",
    # has winred link now
    other_link = "https://secure.winred.com/rondakennedy/donate",
    FEC_ID_cmte = "C00708073",
    FEC_ID_cand = "H0CA26145"
  ),
  # CA-27 ---------------------------------------------------------------------
  # Chu vs. Nalbandian
  tibble(
    first_name = "Judy",
    last_name = "Chu",
    state_cd = "CA-27",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://chu.house.gov",
    campaign_name = "Judy Chu for Congress",
    campaign_website = "https://www.judychu.org/",
    actblue_link = "judy-chu-1", # Choose main entity
    anedot_link = NA,
    # This is a very unique ActBlue setup... it is not like the typical site.
    # There are no suggested amounts (again, a possibly good baseline)
    # But it lists "Who's supporting Judy Chu"
    # all of which seem to be links that are supposed to have expired but not
    other_link = "https://secure.actblue.com/entity/fundraisers/21630",
    # the link that appears now is the link above this line from the campaign website
    # --- (09.2020)
    # other_link = NA,
    FEC_ID_cmte = "C00458125",
    FEC_ID_cand = "H0CA32101"
  ),
  tibble(
    first_name = "Johnny",
    last_name = "Nalbandian",
    state_cd = "CA-27",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Johnny Nalbandian for Congress",
    campaign_website = "http://votejohnny.us/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://give.victorypassport.com/votejohnny/contribute",
    # (09.2020) - now has winred link:
    other_link = "https://secure.winred.com/johnny-for-congress/donate",
    # paste0(
    #   "https://mystique.victorypassport.com/pages/johnny/",
    #   "standard-donation-no-upsell?location=https%3A%2F%2F",
    #   "give.victorypassport.com%2Fvotejohnny%2Fcontribute&location_url=",
    #   "aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5jb20vdm90ZWpvaG5ueS9jb250",
    #   "cmlidXRlLyM%3D"
    # ),
    FEC_ID_cmte = "C00651547",
    FEC_ID_cand = "H0CA27127"
  ),
  # CA-28 ---------------------------------------------------------------------
  # Schiff vs. Barbosa vs. Agbede vs. Hollis vs. Girl vs. Early
  tibble(
    first_name = "Adam",
    last_name = "Schiff",
    state_cd = "CA-28",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Adam Schiff for Congress",
    campaign_website = "https://adamschiff.com/",
    actblue_link = c(
      "as-website-signup",
      "adam-schiff-2",
      "mar2020_as_bullock",
      "mar2020_as",
      "feb2020_as_pelosi", # Split with Pelosi
      "feb2020_as",
      "jan2020_as",
      "dec2019_as",
      "nov2019_as",
      "oct2019_as",
      "sep2019_as",
      "aug2019_as",
      "jul2019_as",
      "jun2019_as",
      "may2019_as",
      "apr2019_as",
      "feb2020_as_crow", # Split with Crow
      "jan2020_as_mucarsel-powell", # Split with Debbie Mucarsel-Powell
      "feb2020_as_mcbath_underwood" # Split with McBath, Underwood
    ),
    anedot_link = NA,
    other_link = c(
      # The banner links to NGP VAN not ActBlue
      "https://secure.ngpvan.com/J2P5_0GDBU-HlKyPpIAdVA2"
      # The below is gone as of Jan 21, 2020
      # "https://act.myngp.com/Forms/6665982682203687424"
      # This other_link that is not commented is the link from his campaign website
      # --- in 09.2020.
    ),
    FEC_ID_cmte = "C00343871",
    FEC_ID_cand = "H0CA27085"
  ),
  tibble(
    first_name = "Jennifer",
    last_name = "Barbosa",
    state_cd = "CA-28",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jennifer Barbosa For Congress",
    campaign_website = "https://barbosaforcongress.com/",
    actblue_link = NA,
    # Interesting. Proclaimed Independent on FEC but uses Anedot!
    anedot_link = "barbosaforcongress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00699926",
    FEC_ID_cand = "H0CA28141"
  ),
  tibble(
    # According to Ballotpedia was 2016 Senate candidate
    # and 2018 and 2014 Governor candidate
    # No donation link
    first_name = "Akinyemi",
    last_name = "Agbede",
    state_cd = "CA-28",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Agbede for United States Congress 2020",
    campaign_website = "https://agbedeforuscongress.com/about/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00698282",
    FEC_ID_cand = "H0CA28133"
  ),
  tibble(
    first_name = "Jon",
    last_name = "Hollis",
    state_cd = "CA-28",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John Hollis 4 Congress",
    campaign_website = "https://www.hollisforcongress.com/",
    # No donation/contribution link found (mostly empty page)
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://donorbox.org/embed/jon-hollis-for-congress?",
      "default_interval=o&hide_donation_meter=true"
    ),
    FEC_ID_cmte = "C00710129",
    FEC_ID_cand = "H0CA28158"
  ),
  tibble(
    first_name = "Maebe",
    last_name = "Girl",
    state_cd = "CA-28",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Maebe A. Girl for Congress",
    campaign_website = "https://maebeagirlforcongress.org/",
    actblue_link = "maebeagirl",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00709501",
    FEC_ID_cand = "H0CA28174"
  ),
  tibble(
    first_name = "Eric",
    last_name = "Early",
    state_cd = "CA-28",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Eric Early for Congress 2020",
    campaign_website = "https://ericearly.com/",
    actblue_link = NA,
    anedot_link = "eric-early-for-congress-2020/donate",
    other_link = NA,
    FEC_ID_cmte = "C00711432",
    FEC_ID_cand = "H0CA28182"
  ),
  # CA-29 ---------------------------------------------------------------------
  # Cardenas vs. Duenas
  tibble(
    first_name = "Tony",
    last_name = "Cardenas",
    state_cd = "CA-29",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://cardenas.house.gov",
    campaign_name = "Tony Cardenas for Congress",
    campaign_website = "https://www.tonycardenasforcongress.com/",
    actblue_link = "gocardenas",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00498873",
    FEC_ID_cand = "H2CA28113"
  ),
  tibble(
    first_name = "Angelica",
    last_name = "Duenas",
    state_cd = "CA-29",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.angelica4congress.com/",
    actblue_link = "angelica",
    anedot_link = NA,
    # Has both ActBlue and (two) non-ActBlue donation links
    # NationBuilder --- so it is not uniquely Republican!
    # Only actblue link in campaign website (09.2020)
    other_link = c(
      "https://www.angelica4congress.com/donatenb",
      "https://secure.democracyengine.com/AngelicaDuenas"
    ),
    FEC_ID_cmte = "C00697391",
    FEC_ID_cand = "H8CA29100"
  ),
  # CA-30 ---------------------------------------------------------------------
  # Sherman vs. D'Orazio vs. Collin vs. Berina
  # I can't find this independent Susan Collin(s) for CA-30 (not Maine Senator)
  tibble(
    first_name = "Brad",
    last_name = "Sherman",
    state_cd = "CA-30",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://sherman.house.gov",
    campaign_name = "Sherman for Congress",
    campaign_website = "http://bradsherman.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-500-1000-2800
    other_link = "https://act.myngp.com/Forms/-4762048638316508416",
    FEC_ID_cmte = "C00308742",
    FEC_ID_cand = "H6CA24113"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Reed",
    state_cd = "CA-30",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Reed for Congress",
    campaign_website = "http://www.markreedforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 1- 5-15-25-100-250-500-1000-1776-2700-5400 (interesting numbers here)
    other_link = "http://www.markreedforcongress.com/donate/",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Royce",
    last_name = "D'Orazio",
    state_cd = "CA-30",
    party = "NPA",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    # Um... no fundraising nor any webpage found (Arcade owner??)
    campaign_website = "https://twitter.com/roycedorazio?lang=en",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00706523",
    FEC_ID_cand = "H0CA30147"
  ),
  tibble(
    first_name = "CJ",
    last_name = "Berina",
    state_cd = "CA-30",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "CJ Berina for Congress",
    campaign_website = "http://www.cjforcongress2020.com/",
    actblue_link = "cj-berina-for-congress-2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00716548",
    FEC_ID_cand = "H0CA30162"
  ),
  # CA-31 ---------------------------------------------------------------------
  # Pete Aguilar is uncontested (no candidates registered as of Sep 24, 2019)
  tibble(
    first_name = "Pete",
    last_name = "Aguilar",
    state_cd = "CA-31",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://aguilar.house.gov",
    campaign_name = "Pete Aguilar for Congrss",
    campaign_website = "https://peteaguilar.com/",
    actblue_link = "pa4c",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00510461",
    FEC_ID_cand = "H2CA31125"
  ),
  tibble(
    first_name = "Agnes",
    last_name = "Gibboney",
    state_cd = "CA-31",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Agnes Ginoney for Congress",
    campaign_website = "https://www.agnesgibboney.com/",
    actblue_link = NA,
    anedot_link = "agnes-gibboney-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-32 -----------------------------------------
  tibble(
    first_name = "Grace",
    last_name = "Napolitano",
    state_cd = "CA-32",
    party = "Dem",
    incumbent = FALSE,
    office_website = "https://napolitano.house.gov/",
    campaign_name = "Napolitano for Congress",
    campaign_website = "https://www.napolitanoforcongress.com/",
    actblue_link = "grace-flores-napolitano-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Joshua",
    last_name = "Scott",
    state_cd = "CA-32",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joshua Scott for Congress",
    campaign_website = "https://votejoshuascott.org/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/JoshuaScott/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # # Napolitano vs. Gonzales vs. Quintero vs. Lutz vs. Kashifalghita
  # # Mary Ann Lutz--can't find website
  # # https://www.facebook.com/MaryAnnLutzforCongress/
  # # Andre Quintero's PCC seems to be a relic of 2018
  tibble(
    first_name = "Grace",
    last_name = "Napolitano",
    state_cd = "Dem",
    party = "CA-32",
    incumbent = TRUE,
    office_website = "https://napolitano.house.gov",
    campaign_name = "Grace F. Napolitano for Congress",
    campaign_website = "https://www.napolitanoforcongress.com/",
    actblue_link = "grace-flores-napolitano-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00334706",
    FEC_ID_cand = "H8CA34068"
  ),
  tibble(
    first_name = "Meshal",
    last_name = "Kashifalghita", # Goes as Kash
    state_cd = "CA-32",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kash for Congress",
    campaign_website = "https://kashforcongress.com",
    actblue_link = "meshal-kash-for-congress-1#",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00713362",
    FEC_ID_cand = "H0CA32341"
  ),
  tibble(
    first_name = "Emmanuel",
    last_name = "Gonzales",
    state_cd = "CA-32",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Emanuel Gonzales for Congress",
    campaign_website = "https://www.emanuelgonzales.com/",
    actblue_link = "emanforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00685743",
    FEC_ID_cand = "H0CA32333"
  ),
  tibble(
    first_name = "Joshua",
    last_name = "Scott",
    state_cd = "CA-32",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joshua Scott for Congress",
    campaign_website = "https://votejoshuascott.org",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/JoshuaScott/donate",
    FEC_ID_cmte = "C00682179",
    FEC_ID_cand = "H8CA32088"
  ),
  # Andre Quintero's PCC seems to be a relic of 2018
  tibble(
    first_name = "Mary Ann",
    last_name = "Lutz",
    state_cd = "CA-32",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mary Ann Lutz for Congress",
    campaign_website = "https://www.facebook.com/MaryAnnLutzforCongress/",
    actblue_link = "maryannlutz",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00624346",
    FEC_ID_cand = "H8CA32062"
  ),
  tibble(
    first_name = "Raul Ali",
    last_name = "Madrigal",
    state_cd = "CA-32",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = paste0(
      "https://www.facebook.com/pages/category/Social-Service/",
      "Raul-Ali-Madrigal-113942263413910/"
    ),
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00734780",
    FEC_ID_cand = "H0CA32358"
  ),
  # CA-33 ---------------------------------------------------------------------
  # Lieu vs. Fricke vs. Bradley
  tibble(
    first_name = "Ted",
    last_name = "Lieu",
    state_cd = "CA-33",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://lieu.house.gov",
    campaign_name = "",
    campaign_website = "http://www.tedlieu.com/",
    # Now has actblue link. (09.2020)
    actblue_link = "ted-lieu-5",
    anedot_link = NA,
    # Rare non-ActBlue donation link
    # NationBuilder is not limited to Republicans
    other_link = NA, # "https://tedlieu.nationbuilder.com/contribute1",
    FEC_ID_cmte = "C00556506",
    FEC_ID_cand = "H4CA33119"
  ),
  tibble(
    first_name = "Brandon",
    last_name = "Fricke",
    state_cd = "CA-33",
    party = "NPA",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Fricke for Congress",
    campaign_website = "https://www.brandonfricke.com/",
    actblue_link = NA,
    anedot_link = "fricke-for-congress/c781177eb882f95a12e3b",
    other_link = NA,
    FEC_ID_cmte = "C00718643",
    FEC_ID_cand = "H0CA33208"
  ),
  tibble(
    first_name = "James P.",
    last_name = "Bradley",
    state_cd = "CA-33",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "James P. Bradley for Congress",
    campaign_website = "https://bradleycongress.com/",
    actblue_link = NA,
    # anedot_link = "/ames-p-bradley-for-us-congress/donate"
    # No amount prompts, and also a WinRed.
    anedot_link = NA,
    other_link = "https://donorbox.org/james-p-bradley-for-us-congress",
    # now has a donorbox link. 5-10-25-50-100-250-500-100-2800 (09.2020)
    # "https://secure.winred.com/james-p-bradley-for-us-congress/donate",
    FEC_ID_cmte = "C00695791",
    FEC_ID_cand = "H0CA48156"
  ),
  # CA-34 ---------------------------------------------------------------------
  # Gomez vs. Motiwalla vs. Pittman vs. Kim
  # Shannel Pittman has no website: See https://twitter.com/ElectShannel
  tibble(
    first_name = "Jimmy",
    last_name = "Gomez",
    state_cd = "CA-34",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://gomez.house.gov",
    campaign_name = "Jimmy Gomez for Congress",
    campaign_website = "https://jimmygomezforcongress.com/",
    actblue_link = "jimmygomezforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00629659",
    FEC_ID_cand = "H8CA34266"
  ),
  tibble(
    first_name = "Frances Yasmeen",
    last_name = "Motiwalla",
    state_cd = "CA-34",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.fym2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    # No idea what this platform is.
    # 20.20-50-100-250-500-800-1000-1400-2000-2800
    other_link = "https://www.fym2020.com/readytosupport",
    FEC_ID_cmte = "C00706671",
    FEC_ID_cand = "H0CA34115"
  ),
  tibble(
    first_name = "David",
    last_name = "Kim",
    state_cd = "CA-34",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Kim for Congress",
    # Supposed to be constructed by Sep 2020
    # See https://twitter.com/davidkim2020
    campaign_website = "www.DavidKim2020.com",
    # added the latter in (09.2020 but both have same amounts)
    # ie. 5-10-22-50-100-500-2800
    actblue_link = c("david-kim-2020-1", "davidkimweb"),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00713115",
    FEC_ID_cand = "H0CA34131"
  ),
  # CA-35 ---------------------------------------------------------------------
  # Norma Torres is uncontested (no candidates registered as of Sep 24, 2019)
  # Torres vs Cargile General
  tibble(
    first_name = "Norma",
    last_name = "Torres",
    state_cd = "CA-35",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://torres.house.gov",
    campaign_name = "Norma Torres for Congress",
    campaign_website = "https://normatorres.com/",
    actblue_link = "norma-torres-4",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00557652",
    FEC_ID_cand = "H4CA35031"
  ),
  tibble(
    first_name = "Mike",
    last_name = "Cargile",
    state_cd = "CA-35",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cargile for Congress",
    campaign_website = "https://www.cargileforcongress.com/",
    actblue_link = NA,
    anedot_link = "cargile-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-36 ---------------------------------------------------------------------
  tibble(
    first_name = "Raul",
    last_name = "Ruiz",
    state_cd = "CA-36",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://ruiz.house.gov",
    campaign_name = "Dr. Raul Ruiz for Congress",
    campaign_website = "https://drraulruiz.com/",
    actblue_link = "contributeonline", # but defaults to amount 5.
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00557652",
    FEC_ID_cand = "H4CA35031"
  ),
  tibble(
    first_name = "Erin",
    last_name = "Cruz",
    state_cd = "CA-36",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Erin Cruz for Congress",
    campaign_website = "https://erincruz.org/",
    actblue_link = NA,
    anedot_link = NA,
    # 20-36-100-250-360-500-1000-2800
    other_link = "https://erincruz.revv.co/takebackthehouse",
    FEC_ID_cmte = "C00714048",
    FEC_ID_cand = "H0CA36193"
  ),
  # CA-37 ---------------------------------------------------------------------
  # Karen Bass was uncontested as of Sep 17, 2019
  # Now Bass vs. Webber (Filed Sep 17, 2019)
  tibble(
    first_name = "Karen",
    last_name = "Bass",
    state_cd = "CA-37",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://bass.house.gov",
    campaign_name = "Karen Bass for Congress",
    campaign_website = "https://karenbass.com/",
    actblue_link = "karenbass2018",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00476523",
    FEC_ID_cand = "H0CA33117"
  ),
  tibble(
    first_name = "Errol",
    last_name = "Webber",
    state_cd = "CA-37",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Errol Webber For Congress",
    campaign_website = "https://www.webber2020.com/",
    actblue_link = NA,
    # winred link now
    anedot_link = NA, # "errol-webber-for-congress/donate",
    other_link = "https://secure.winred.com/errolwebber/donate",
    FEC_ID_cmte = "C00719864",
    FEC_ID_cand = "H0CA37167"
  ),
  # CA-38 ---------------------------------------------------------------------
  # Linda Sanchez is uncontested (no candidates registered as of Sep 24, 2019)
  tibble(
    first_name = "Linda",
    last_name = "Sanchez",
    state_cd = "CA-38",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://lindasanchez.house.gov/",
    campaign_name = "Vote for Linda",
    campaign_website = "https://voteforlinda.com/",
    actblue_link = "ls_support",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00384057",
    FEC_ID_cand = "H2CA39078"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Tolar",
    state_cd = "CA-38",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Michael Tolar for Congress",
    campaign_website = "http://www.michaeltolar4congress.com/",
    # a 4.20 mmount there among other (seemingly) random amounts.
    actblue_link = "michaeltolar4congress.com",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-39 (last updated Oct 25, 2020) ------------------------------------------
  # Cisneros vs. Kim vs. Chen vs. Cox
  # Jay Chen's PCC seems to be lingering after dropping out of 2018
  # No active recipients in ActBlue or NGP VAN https://chenforcongress.com/
  tibble(
    first_name = "Gil",
    last_name = "Cisneros",
    state_cd = "CA-39",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://cisneros.house.gov/",
    campaign_name = "Gil Cisneros for Congress",
    campaign_website = "https://cisnerosforcongress.com/",
    # in sept 2018 links to first of the 3 in the campaign website
    actblue_link = c(
      "cisnerosweb2018" # ,
      # "springbreak-3-16-20",
      # "gcc-q1-2020"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00650648",
    FEC_ID_cand = "H8CA39174"
  ),
  tibble(
    first_name = "Young",
    last_name = "Kim",
    state_cd = "CA-39",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Young Kim for Congress",
    campaign_website = "https://www.youngkimforcongress.com/",
    actblue_link = NA,
    # Not sure what, but the UI interface for
    # https://secure.anedot.com/youngkimcd39/ and
    # https://secure.anedot.com/youngkimcd39/c9624a7de18cb394c814a
    # differ. Same amounts though.
    # Back again
    # as of 09.2020, same anedot link through her website.
    anedot_link = "youngkimcd39/c9624a7de18cb394c814a",
    # E-Pac link is Not just specific to Young Kim but also
    # Michelle Steel (CA-45), Amanda Makki (FL-13), Salazar (FL-27),
    # Handel (GA-06), Ashley Hinson (IA-01), Chele Farley (NY-18),
    # Beth Duyne (TX-24), Nicole (NY-11), Collins (TX-31), Nancy Mace (SC-01)
    other_link = c(
      # Change documented to WinRed on May 5, 2020
      "https://secure.winred.com/young-kim-for-congress/website",
      "https://secure.winred.com/young-kim-for-congress/thank-you",
      "https://secure.winred.com/young-kim-for-congress/search/" # Google
      # Team Pages
      # "https://secure.winred.com/team/fa0dc477-maga-majority-takers",
      # "https://secure.winred.com/team/30d76c8b-make-california-great-again",
      # "https://secure.winred.com/e-pac/donate-now"
    ),
    FEC_ID_cmte = "C00665638",
    FEC_ID_cand = "H8CA39240"
  ),
  tibble(
    first_name = "Steve",
    last_name = "Cox",
    state_cd = "CA-39",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cox for Congress",
    campaign_website = "http://www.voteforcox.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented from democracyengine to donorbox
    # from 100-50-25-10 to 10-27-50-100
    # other_link = "https://secure.democracyengine.com/SteveCox",
    other_link = "https://donorbox.org/voteforcox",
    FEC_ID_cmte = "C00698357",
    FEC_ID_cand = "H8CA39190"
  ),
  # CA-40 ---------------------------------------------------------------------
  # Roybal-Allard vs. Cortes Barragan vs. Rivera vs. Metzger vs. Felix
  tibble(
    first_name = "Lucille",
    last_name = "Roybal-Allard",
    state_cd = "CA-40",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://roybal-allard.house.gov",
    campaign_name = "U.S. Congresswoman Lucille Roybal Allard",
    campaign_website = "http://www.lucilleforcongress.org/",
    # Separately found on ActBlue
    actblue_link = "lucille-roybal-allard-2",
    anedot_link = NA,
    # NGP VAN, but says no longer accepting online contributions (what?)
    # other_link = paste0(
    #   "https://services.myngp.com/ngponlineservices/RedirectPage.aspx?",
    #   "Y=V%2f7FzMjOcK31BMkUYEfMEsC7bHiprTWcRg4Bjd7YK06rJgClt%2bxC5It7P",
    #   "dqZDePLs2gIdQN8BNs%3d",
    # ),
    # 09.2020 shows this other link:
    other_link = "https://act.myngp.com/Forms/2018377069518326784",
    FEC_ID_cmte = "C00259143",
    FEC_ID_cand = "H2CA33048"
  ),
  tibble(
    first_name = "Antonio",
    last_name = "Delgado",
    state_cd = "CA-40",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Antonio Delgado for Congress",
    campaign_website = "https://delgadoforcongress.com/",
    actblue_link = "adfc_website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00633859",
    FEC_ID_cand = "H8NY19181"
  ),
  tibble(
    first_name = "Rodolfo",
    last_name = "Cortez Barragan",
    state_cd = "CA-40",
    party = "Green",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dr. Barragan for Congress",
    campaign_website = "https://rodolfo2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Donorbox is not limited to Republicans
    other_link = "https://donorbox.org/dr-rodolfo-cortes-barragan-for-congress",
    FEC_ID_cmte = "C00664185",
    FEC_ID_cand = "H8CA40206"
  ),
  tibble(
    first_name = "Juan",
    last_name = "Rivera",
    state_cd = "CA-40",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Juan Rivera for Congress",
    # Not loading at present, hence cannot find donation/contribution link
    campaign_website = "https://www.joinrivera.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00698704",
    FEC_ID_cand = "H0CA40104"
  ),
  tibble(
    first_name = "Orion",
    last_name = "Metzger",
    state_cd = "CA-40",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Metzger for Congress",
    campaign_website = "http://www.metzgerforcongress.com/",
    actblue_link = "metzger",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710301",
    FEC_ID_cand = "H0CA40112"
  ),
  tibble(
    first_name = "Anthony",
    last_name = "Felix",
    state_cd = "CA-40",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Anthony Felix Jr. for Congress 2020",
    campaign_website = "https://2020af.com/",
    actblue_link = "anthony-felix-jr--for-congress-2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711382",
    FEC_ID_cand = "H0CA40120"
  ),
  # CA-41 ------------------------------------------
  # Takano vs. Williams vs. Smith
  tibble(
    first_name = "Mark",
    last_name = "Takano",
    state_cd = "CA-41",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://takano.house.gov",
    campaign_name = "Mark Takano Teacher for Congress",
    campaign_website = "https://marktakano.com/",
    actblue_link = "mark-takano-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00498667",
    FEC_ID_cand = "H2CA43245"
  ),
  tibble(
    first_name = "Grace",
    last_name = "Williams",
    state_cd = "CA-41",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dr. Grace I Williams for Congress 2020",
    campaign_website = "https://www.drgraceforcongress.com/about/",
    actblue_link = "dr--grace-i-williams-for-congress-2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00698191",
    FEC_ID_cand = "H0CA49113"
  ),
  tibble(
    first_name = "Aja",
    last_name = "Smith",
    state_cd = "CA-41",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Aja Smith for Congress",
    campaign_website = "https://www.ajasmithforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://www.efundraisingconnections.com/c/AjaSmithforCongress/",
    FEC_ID_cmte = c("C00719070", "C00635854"),
    FEC_ID_cand = "H8CA41170"
  ),
  # CA-42 ---------------------------------------------------------------------
  tibble(
    first_name = "Ken",
    last_name = "Calvert",
    state_cd = "CA-42",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://calvert.house.gov/",
    campaign_name = "Ken Calvert for Congress",
    campaign_website = "https://www.calvertforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-75-100-250-500-1000-2800-5600
    other_link = paste0(
      "https://secure.campaignsolutions.com/calvert/list/proc/donation1/?",
      "InitiativeKey=LL39MZ8HTQXD"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "William",
    last_name = "O'Mara",
    state_cd = "CA-42",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Liam O'Mara for Congress",
    campaign_website = "https://www.liamomara.org/",
    actblue_link = "liam-o-mara-for-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-43 ---------------------------------------------------------------------
  # Waters vs. Navarro vs. Collins
  tibble(
    first_name = "Maxine",
    last_name = "Waters",
    state_cd = "CA-43",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://waters.house.gov",
    campaign_name = "Waters for Congress",
    # The website domain has expired! What?!
    # Now Live (09.2020)
    campaign_website = "http://www.maxinewatersforcongress.com/",
    # the latter appears in her website popup (09.2020)
    actblue_link = c("maxine-waters-2", "reclaimourtime"),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00167585",
    FEC_ID_cand = "H4CA23011"
  ),
  tibble(
    first_name = "Omar",
    last_name = "Navarro",
    state_cd = "CA-43",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Omar Navarro for Congress",
    # Empty site... I don't understand, he's quite popular on Twitter
    # Lots of receipts on FEC. Where's the contribution site?
    # https://twitter.com/RealOmarNavarro
    campaign_website = "https://www.omarnavarro.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Donation link is 404
    other_link = "https://www.omarnavarro.com/donate",
    FEC_ID_cmte = "C00592048",
    FEC_ID_cand = "H6CA44145"
  ),
  tibble(
    first_name = "Joe",
    last_name = "Collins",
    state_cd = "CA-43",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joe Collins for Congress",
    campaign_website = "https://www.joecollinsforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      paste0(
        "https://secure.joecollinsforcongress.com/donation1/?",
        "InitiativeKey=VE0D2VWDBDVQ"
      ),
      "https://donorbox.org/embed/joe-collins-for-congress"
    ),
    FEC_ID_cmte = "C00705236",
    FEC_ID_cand = "H0CA43108"
  ),
  # CA-44 ---------------------------------------------------------------------
  # Barragan vs. Nettles vs. McDonald
  # Cannot find Annette McDonald's campaign site (but she has 1k donation)
  # Barragan vs Joya in general.
  tibble(
    first_name = "Nanette",
    last_name = "Barragan",
    state_cd = "CA-44",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://barragan.house.gov",
    campaign_name = "Nanette Barragan for Congress",
    campaign_website = "http://barraganforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/5351103858949031936",
    FEC_ID_cmte = "C00577353",
    FEC_ID_cand = "H6CA44103"
  ),
  tibble(
    first_name = "Analilia",
    last_name = "Joya",
    state_cd = "CA-44",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joya for Congress",
    campaign_website = "https://www.analiliajoya.com/home-1",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00735050",
    FEC_ID_cand = "H4CA44157"
  ),
  tibble(
    first_name = "Aaron",
    last_name = "Nettles",
    state_cd = "CA-44",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nettles for Congress",
    campaign_website = "https://nettlesforcongress.org/",
    actblue_link = NA,
    anedot_link = NA,
    # PayPal. No prompt. Could serve as a baseline.
    other_link = paste0(
      "https://www.paypal.com/donate/?token=eFK_AOW-BBCCdw6gTF1F4r",
      "-uHY44WjLnp_7GR-Ige9I27lNDYlK50Bw7Jg59YGvqIkHrw0&country.x=US&locale.x="
    ),
    FEC_ID_cmte = "C00719435",
    FEC_ID_cand = "H0CA44163"
  ),
  # CA-45 (last updated Oct 25, 2020) ------------------------------------------
  # Porter vs. Sedgwick
  # vs. Sparks vs. Raths vs. Huang vs. Gennawey
  # vs. Pai vs. Gonzales vs. Woolworth vs. Forde vs. Walters vs. Furin
  # Gennawey, Pai, Woolworth dropped out
  # Forde seems to have lost in 2018 and not have been active either:
  # https://www.forde.com/ is empty
  # Walters campaign is not active. See https://www.mimiwalters.com/
  # Originally contribution site was
  # https://www.efundraisingconnections.com/c/MimiWaltersCongress/
  tibble(
    first_name = "Katie",
    last_name = "Porter",
    state_cd = "CA-45",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://porter.house.gov/",
    campaign_name = "",
    campaign_website = "https://katieporter.com/",
    # campagin website leads to the last of the links
    actblue_link = c(
      # "kp18",
      # "kppostelection",
      # "aoc-katieporter-email-2020313-2",
      "wb-1020",
      "wb-0920",
      "wb-0820",
      "sm-0920",
      "ads-gs-dd-launch-2020q3",
      "ads-fb-dd-launch-gead",
      "bluerevforkp"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00636571",
    FEC_ID_cand = "H8CA45130"
  ),
  tibble(
    first_name = "Greg",
    last_name = "Raths",
    state_cd = "CA-45",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Greg Raths for Congress 2020",
    campaign_website = "https://gregraths.com/",
    actblue_link = NA,
    # Well, at least it's specified "Maximum Couple Contribution"
    anedot_link =
      "greg-raths-for-congress-d8ac9809-2d1a-463c-8bf0-310798f12831/contribute",
    other_link = NA,
    FEC_ID_cmte = "C00700559",
    FEC_ID_cand = "H4CA45089"
  ),
  tibble(
    first_name = "Don",
    last_name = "Sedgwick",
    state_cd = "CA-45",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sedgwick for Congress",
    campaign_website = "https://sedgwickforcongress.com/",
    actblue_link = NA,
    anedot_link = "sedgwick/donate",
    other_link = NA,
    FEC_ID_cmte = "C00698670",
    FEC_ID_cand = "H0CA45087"
  ),
  tibble(
    first_name = "Rhonda",
    last_name = "Furin",
    state_cd = "CA-45",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rhonda Furin for Congress",
    campaign_website = "https://rhondafurin.com",
    actblue_link = NA,
    anedot_link = "furin-for-congress/website-splash",
    other_link = NA,
    FEC_ID_cmte = "C00724161",
    FEC_ID_cand = "H0CA45152"
  ),
  tibble(
    first_name = "Lisa",
    last_name = "Sparks",
    state_cd = "CA-45",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dr. Lisa Sparks for Congress",
    campaign_website = "https://www.drlisasparks.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented by WinRed on Feb 29, 2020
    # from 10-25-50-100-250-1000-2800
    #   to 10-25-50-100-250-1000-2800 (same amounts)
    other_link = paste0(
      # "https://mystique.victorypassport.com/pages/sparks/website",
      # "?location=https%3A%2F%2Fgive.victorypassport.com",
      # "%2Fsparks%2Fwebsite&location_url=",
      # "aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5jb20vc3BhcmtzL3dlYnN",
      # "pdGUvP3JlY3VycmluZz10cnVlJnV0bV9tZWRpdW09d2Vic2l0ZSZ1dG1fc2",
      # "91cmNlPWhlYWRlciZ1dG1fY29udGVudD1jb250cmlidXRlIw%3D%3D"
      "https://secure.winred.com/drlisasparks/website"
    ),
    FEC_ID_cmte = "C00706051",
    FEC_ID_cand = "H0CA45129"
  ),
  tibble(
    first_name = "Christopher",
    last_name = "Gonzales",
    state_cd = "CA-45",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gonzales for Congress",
    campaign_website = "https://gonzalesforcongress.com",
    actblue_link = NA,
    anedot_link = "gonzales-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00727636",
    FEC_ID_cand = "H0CA45160"
  ),
  tibble(
    first_name = "Peggy",
    last_name = "Huang",
    state_cd = "CA-45",
    party = "Rep",
    incumbent = FALSE,
    office_website = "",
    campaign_name = "Peggy Huang for Congress",
    campaign_website = "https://www.peggyforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.efundraisingconnections.com/c/Peggy/",
    FEC_ID_cmte = "C00700807",
    FEC_ID_cand = "H0CA45095"
  ),
  # Gennawey dropped out
  tibble(
    first_name = "Ray",
    last_name = "Gennawey",
    state_cd = "CA-45",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ray for Congress",
    campaign_website = "https://rayforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://transaxt.com/Donate/QKTDQY/RayGennaweyforCongress//",
    FEC_ID_cmte = "C00702209",
    FEC_ID_cand = "H0CA45103"
  ),
  # Woolworth dropped out
  tibble(
    first_name = "Brenton",
    last_name = "Woolworth",
    state_cd = "CA-45",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Woolworth for Congress",
    campaign_website = "https://www.woolworthforcongress.com/",
    actblue_link = NA,
    anedot_link = "woolworth-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00705160",
    FEC_ID_cand = "H0CA45111"
  ),
  # Forde dropped out
  # CA-46 ---------------------------------------------------------------------
  # Correa vs. Waters vs. Mendiola
  tibble(
    first_name = "Lou",
    last_name = "Correa",
    state_cd = "CA-46",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://correa.house.gov/",
    campaign_name = "Lou Correa for Congress",
    campaign_website = "https://www.loucorrea.com/",
    actblue_link = "lc",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00578302",
    FEC_ID_cand = "H6CA46116"
  ),
  tibble(
    first_name = "James",
    last_name = "Waters",
    state_cd = "CA-46",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "James Waters for Congress",
    campaign_website = "https://www.watersforcongress.com/",
    actblue_link = NA,
    anedot_link = "james-waters-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00708669",
    FEC_ID_cand = "H0CA46077"
  ),
  tibble(
    first_name = "Pablo",
    last_name = "Mendiolea",
    state_cd = "CA-46",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Pablo Mendiolea for Congress",
    campaign_website = "https://www.vote4pablo.com/",
    actblue_link = NA,
    anedot_link = NA,
    # CampaignPartner.com
    # Rare non-ActBlue donation link
    other_link = "https://www.vote4pablo.com/contribute",
    FEC_ID_cmte = "C00717082",
    FEC_ID_cand = "H0CA46085"
  ),
  # CA-47 ---------------------------------------------------------------------
  # Lowenthal vs. Moua vs. West vs. Roshan
  # Cannot find Sou Moua
  tibble(
    first_name = "Alan",
    last_name = "Lowenthal",
    state_cd = "CA-47",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://lowenthal.house.gov/",
    campaign_name = "Alan Lowenthal for U.S. Congress",
    campaign_website = "https://alanlowenthal.nationbuilder.com/",
    actblue_link = "lowenthal4congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00498212",
    FEC_ID_cand = "H2CA00104"
  ),
  tibble(
    first_name = "Amy Phan",
    last_name = "West",
    state_cd = "CA-47",
    party = "Rep",
    incumbent = TRUE,
    office_website = "Amy Phan West for Congress",
    campaign_name = "",
    campaign_website = "https://amyphanwestforcongress.com/",
    actblue_link = NA,
    anedot_link = "amy-phan-west-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00712646",
    FEC_ID_cand = "H0CA47141"
  ),
  tibble(
    first_name = "Shawn",
    last_name = "Roshan",
    state_cd = "CA-47",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Shawn Roshan for Congress 2020",
    campaign_website = "https://www.shawnroshan.com/",
    # No donation/contribution link found
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00715714",
    FEC_ID_cand = "H0CA47158"
  ),
  tibble(
    first_name = "John",
    last_name = "Briscoe",
    state_cd = "CA-47",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Briscoe for Congress",
    campaign_website = "https://johnbriscoe.us/",
    actblue_link = NA,
    anedot_link = NA,
    # "I get a pause link but without any vpns."
    other_link = "https://johnbriscoe.us/donations",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-48 ---------------------------------------------------------------------
  # Rouda vs. Steel
  # vs. Baugh vs. Burley vs. Sturges vs. Engels vs. Keirstead
  # Keirstead seems rollover from last cycle
  tibble(
    first_name = "Harley",
    last_name = "Rouda",
    state_cd = "CA-48",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://rouda.house.gov/",
    campaign_name = "Harley Rouda for Congress",
    campaign_website = "https://harleyforcongress.com/",
    actblue_link = "stand-with-harley-rouda",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00633982",
    FEC_ID_cand = "H8CA48035"
  ),
  tibble(
    first_name = "Michelle",
    last_name = "Steel",
    state_cd = "CA-48",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Michelle Steel for Congress",
    campaign_website = "https://www.michellesteelca.com/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://give.victorypassport.com/steel/website/",
    # Actual frame:
    # Now a winred link
    other_link = "https://secure.winred.com/MichelleSteel/steel-website/",
    # paste0(
    # "https://mystique.victorypassport.com/pages/steel/website",
    # "?location=https%3A%2F%2F", "give.victorypassport.com",
    # "%2Fsteel%2Fwebsite&location_url=",
    # "aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5jb20vc3RlZWwvd2Vic2l0ZS8",
    # "%2FcmVjdXJyaW5nPXRydWUmdXRtX21lZGl1bT13ZWJzaXRlJnV0bV9zb3VyY2U9",
    # "aGVhZGVyJnV0bV9jb250ZW50PWNvbnRyaWJ1dGUj",
    FEC_ID_cmte = "C00704981",
    FEC_ID_cand = "H0CA48198"
  ),
  tibble(
    first_name = "Scott",
    last_name = "Baugh",
    state_cd = "CA-48",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Scott Baugh for Congress",
    campaign_website = "https://scottbaughforcongress.com/",
    actblue_link = NA,
    anedot_link = "baugh/contribute",
    other_link = NA,
    FEC_ID_cmte = "C00607432",
    FEC_ID_cand = "H6CA48245"
  ),
  tibble(
    first_name = "Brian",
    last_name = "Burley",
    state_cd = "CA-48",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brian Burley For Congress",
    campaign_website = "https://www.brianburley.com",
    actblue_link = NA,
    anedot_link = NA,
    # PDI? Interesting platform.
    other_link = "https://www.brianburley.com/donate",
    FEC_ID_cmte = "C00700187",
    FEC_ID_cand = "H0CA48172"
  ),
  tibble(
    first_name = "Christopher",
    last_name = "Engels",
    state_cd = "CA-48",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Christopher Engels for Congress 2020",
    campaign_website = "https://electengels.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/ChristopherEngels/donate",
    FEC_ID_cmte = "C00714170",
    FEC_ID_cand = "H0CA48206"
  ),
  # CA-49 ---------------------------------------------------------------------
  # Levin vs. Maryott vs. Fortin vs. Knoblock
  # Cannot find Knoblock for Congress
  tibble(
    first_name = "Mike",
    last_name = "Levin",
    state_cd = "CA-49",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://mikelevin.house.gov",
    campaign_name = "Mike Levin for Congress",
    campaign_website = "https://mikelevin.org/",
    actblue_link = "mikelevin",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00634253",
    FEC_ID_cand = "H8CA49058"
  ),
  tibble(
    first_name = "Brian",
    last_name = "Maryott",
    state_cd = "CA-49",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brian Maryott for Congress",
    campaign_website = "https://maryottforcongress.com/",
    actblue_link = NA,
    anedot_link = NA, # "maryottforcongress/donate",
    # other_link = "https://maryottforcongress.com/contribute",
    # This is anedot link in disguise, as was the Weld presidential campaign
    # only leads to this now:
    other_link = "https://maryottforcongress.com/contribute",
    FEC_ID_cmte = "C00666859",
    FEC_ID_cand = "H8CA49108"
  ),
  tibble(
    first_name = "Mara",
    last_name = "Fortin",
    state_cd = "CA-49",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mara for California",
    campaign_website = "https://maraforcalifornia.com/about/",
    # No donation/contribution link, mostly empty site
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00708511",
    FEC_ID_cand = "H0CA49139"
  ),
  # CA-50 (last updated Mar 1, 2020) ------------------------------------------
  # Hunter resigned; open seat
  # Issa vs. DeMaio vs. Campa-Najjar
  # vs. Wilske vs. Jones vs. Calderon vs. Wells vs. Balkin vs. Abed
  # vs. Edick vs. Rahn vs. Horvath vs. Ota vs. Duncan
  # --- Wilske   withdrew to support Issa
  # --- Calderon withdrew
  # --- Wells    withdrew to support Issa
  # --- Balkin   withdrew to support Campa-Najjar
  # --- Abed     withdrew to support Issa
  # --- Edick    withdrew
  # --- Rhan     withdrew to support Issa
  # --- Duncan   withdrew
  tibble(
    first_name = "Duncan",
    last_name = "Hunter",
    state_cd = "CA-50",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://hunter.house.gov",
    campaign_name = "Duncan D. Hunter for Congress",
    campaign_website = "http://hunterforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://spark.widgetmakr.com/render/",
      "ee7bb282-e056-4cc2-94c6-a8ab813a2320"
    ),
    FEC_ID_cmte = "C00433524",
    FEC_ID_cand = "H8CA52052"
  ),
  # There's also supposed to be a Nate Wilkins but not yet in FEC
  # https://natewilkins.org/home
  # https://secure.winred.com/natewilkins/support-nate
  tibble(
    first_name = "Darrell",
    last_name = "Issa",
    state_cd = "CA-50",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Darrell Issa for Congress",
    campaign_website = "https://www.darrellissa.com/",
    actblue_link = NA,
    anedot_link = "darrellissa/donate",
    # This is available on WinRed directory, but website leads to Anedot
    other_link = "https://secure.winred.com/darrellissa/donate",
    FEC_ID_cmte = "C00721332",
    FEC_ID_cand = "H0CA50178"
  ),
  tibble(
    first_name = "Carl",
    last_name = "DeMaio",
    state_cd = "CA-50",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Carl DeMaio for Congress",
    campaign_website = "https://carldemaio.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Revv link
    other_link = "https://secure.carldemaio.com/contribute?utm_source=web",
    FEC_ID_cmte = "C00715029",
    FEC_ID_cand = "H0CA50145"
  ),
  tibble(
    first_name = "Ammar",
    last_name = "Campa-Najjar", # Goes as Campa?
    state_cd = "CA-50",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ammar Campa-Najjar for Congress",
    campaign_website = "https://www.campacampaign.com/",
    # FIRST ACTBLUE APPEARS IN THE CAMPAIGN WEBSITE (09.2020)
    actblue_link = c(
      "campacampaignfundraiser",
      "campacampaign",
      "ms_ammar_fr_q32019_afc-a",
      "ms_ammar_surv_q42019",
      "ms_ammar_fr_q42019"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00635888",
    FEC_ID_cand = "H8CA50098"
  ),
  # Wilske dropped out to support Issa
  tibble(
    first_name = "Larry",
    last_name = "Wilske",
    state_cd = "CA-50",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wilske for Congress",
    campaign_website = "https://larrywilske.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.larrywilske.com/donate",
    FEC_ID_cmte = "C00696187",
    FEC_ID_cand = "H0CA50111"
  ),
  tibble(
    first_name = "Brian",
    last_name = "Jones",
    state_cd = "CA-50",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brian Jones for Congress",
    campaign_website = "https://www.electbrianjones.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.efundraisingconnections.com/c/BrianJones/",
    FEC_ID_cmte = "C00720599",
    FEC_ID_cand = "H8CA52037"
  ),
  # # Wells dropped out to support Issa
  tibble(
    first_name = "Bill",
    last_name = "Wells",
    state_cd = "CA-50",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wells for Congress",
    campaign_website = "http://www.wellsforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://www.efundraisingconnections.com/c/MayorBillWellsforCongress/",
    FEC_ID_cmte = "C00694075",
    FEC_ID_cand = "H0CA50103"
  ),
  tibble(
    first_name = "Helen",
    last_name = "Horvath",
    state_cd = "CA-50",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Helen Horvath for Congress",
    campaign_website = "https://www.horvathcampaign.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Not sure how to scrape just yet. 25-100-250-500-1000-other.
    # Now changed to 25-75-100-250-500-1000-other.
    other_link = "https://www.horvathcampaign.com/donate",
    FEC_ID_cmte = "C00717207",
    FEC_ID_cand = "H0CA50152"
  ),
  tibble(
    first_name = "Henry",
    last_name = "Ota",
    state_cd = "CA-50",
    party = "Non",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ota for Congress",
    campaign_website = "https://www.henryota2020.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://politics.raisethemoney.com/hota",
    FEC_ID_cmte = "C00730283",
    FEC_ID_cand = "H0CA50186"
  ),
  # CA-51 ---------------------------------------------------------------------
  # Juan Vargas is uncontested (no candidates registered as of Sep 24, 2019)
  # now (09.2020) vs Hidaglo in General.
  tibble(
    first_name = "Juan",
    last_name = "Vargas",
    state_cd = "CA-51",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://vargas.house.gov",
    campaign_name = "Juan Vargas for Congress",
    campaign_website = "https://votevargas.com/",
    actblue_link = NA,
    anedot_link = NA,
    # I see that efundraising is also not limited to Republicans
    other_link = "https://www.efundraisingconnections.com/c/JuanVargas/",
    FEC_ID_cmte = "C00497321",
    FEC_ID_cand = "H2CA50026"
  ),
  tibble(
    first_name = "Juan",
    last_name = "Hidalgo",
    state_cd = "CA-51",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hidalgo for Congress",
    campaign_website = "https://hidalgoforcongress.com/",
    actblue_link = NA,
    anedot_link = "hidalgo-for-congress-2020-50f108b5-d7af-4f22-ab54-9081042dc86c/general-donations",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-52 ---------------------------------------------------------------------
  tibble(
    first_name = "Scott",
    last_name = "Peters",
    state_cd = "CA-52",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://scottpeters.house.gov/",
    campaign_name = "Scott Peters for Congress",
    campaign_website = "https://scottpeters.com/",
    actblue_link = "sp2018",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "James",
    last_name = "DeBello",
    state_cd = "CA-52",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "DeBello for Congress",
    campaign_website = "https://www.debellocongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 100-250-500-1000-2800-5600
    other_link = "https://www.efundraisingconnections.com/c/DeBello",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CA-53 ---------------------------------------------------------------------
  tibble(
    first_name = "Sara",
    last_name = "Jacobs",
    state_cd = "CA-53",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sara Jacobs for Congress",
    campaign_website = "https://sarajacobsforca.com",
    actblue_link = "sara-jacobs-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00719559",
    FEC_ID_cand = "H0CA53115"
  ),
  tibble(
    first_name = "Georgette",
    last_name = "Gomez",
    state_cd = "CA-53",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Georgette Gomez for Congress",
    campaign_website = "https://georgettegomez.org",
    actblue_link = "georgette-gomez-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00719112",
    FEC_ID_cand = "H0CA53107"
  ),
  # CO: Colorado (last updated Jul 11, 2021) ===================================
  # CO-01 (last updated Jul 11, 2021) ------------------------------------------
  # Degette vs. Bolling vs. Furey vs. Fiorino vs. Kok vs. Duran vs. Winters
  tibble(
    first_name = "Diana",
    last_name = "Degette",
    state_cd = "CO-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://degette.house.gov",
    campaign_name = "Diana DeGette for Congress",
    campaign_website = "https://degette.com",
    actblue_link = "diana-degette-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00311639",
    FEC_ID_cand = "H6CO01141"
  ),
  tibble(
    first_name = "Shane",
    last_name = "Bolling",
    state_cd = "CO-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shane 4 Colorado",
    campaign_website = "https://shane4co.com/",
    actblue_link = NA,
    anedot_link = "shane-bolling-campaign/donate",
    other_link = NA,
    FEC_ID_cmte = "C00748848",
    FEC_ID_cand = "H0CO02233"
  ),
  tibble(
    first_name = "Kyle",
    last_name = "Furey",
    state_cd = "CO-01",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kyle Furey for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "NA",
    FEC_ID_cand = "H0CO01185"
  ),
  tibble(
    first_name = "Paul",
    last_name = "Fiorino",
    state_cd = "CO-01",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Fiorino for Colorado",
    campaign_website = "https://fiorinoforcolorado.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "NA",
    FEC_ID_cand = "H0CO01193"
  ),
  tibble(
    first_name = "Jan",
    last_name = "Kok",
    state_cd = "CO-01",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jan Kok for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "NA",
    FEC_ID_cand = "H0CO01177"
  ),
  tibble(
    first_name = "Crisanta",
    last_name = "Duran",
    state_cd = "CO-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Crisanta Duran for Congress",
    campaign_website = "https://duranforcongress.com",
    actblue_link = "colorado01",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00697540",
    FEC_ID_cand = "H0CO01136"
  ),
  tibble(
    first_name = "Charlie Madison",
    last_name = "Winters",
    state_cd = "CO-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Charlie Madisoni Winters for Colorado",
    # Nothing effectively launched yet
    campaign_website = "https://twitter.com/extantcmw?lang=en",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00698324",
    FEC_ID_cand = "H0CO01144"
  ),
  # Meghan Pratschler website is down
  # "Joey Camp is running for U.S. President, U.S. Senate -Colo. and
  # U.S. Rep. -Colo, Dist. 1 as a satirical spoiler." So not entered.
  # Cannot find Shane Bolling
  # CO-02 (last updated Jul 11, 2021) ------------------------------------------
  # Neguse vs. Matyi as of Oct 5, 2019
  # Now Neguse vs. Winn vs. Matyi vs. Johnson vs. Cutcher
  # vs. Atkinson vs. Swing
  tibble(
    first_name = "Joe",
    last_name = "Neguse",
    state_cd = "CO-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://neguse.house.gov",
    campaign_name = "Joe Neguse for Congress",
    campaign_website = "https://www.joeneguseforcongress.com",
    actblue_link = "website-jn",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00648253",
    FEC_ID_cand = "H8CO02160"
  ),
  tibble(
    first_name = "Charles",
    last_name = "Winn",
    state_cd = "CO-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Winn for Colorado",
    campaign_website = "https://www.winnforcolorado.com/",
    actblue_link = NA,
    # Hidden Anedot
    anedot_link = "winn-for-colorado/donate",
    other_link = "https://www.winnforcolorado.com/contribute",
    FEC_ID_cmte = "C00735373",
    FEC_ID_cand = "H0CO02225"
  ),
  tibble(
    first_name = "Thom",
    last_name = "Atkinson",
    state_cd = "CO-02",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Statesman for Freedom",
    campaign_website = "https://www.statesman4freedom.us/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0CO02241"
  ),
  tibble(
    first_name = "Gary",
    last_name = "Swing",
    state_cd = "CO-02",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gary Swing for Congress",
    campaign_website = "https://theswingvote.wixsite.com/unity",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00280552",
    FEC_ID_cand = "H2CO02080"
  ),
  tibble(
    first_name = "Alex",
    last_name = "Johnson",
    state_cd = "CO-02",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Alex Johnson for Colorado",
    campaign_website = "https://www.alexjohnson4co.com/",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?token=thCIeKwhPwe_-_r5wS_uhoZNFzYN",
      "r265Rv0tiUaSm-TA0Z66ocXttqlqYYCMNaUYlbFYqW&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00721423",
    FEC_ID_cand = "H0CO02209"
  ),
  tibble(
    first_name = "William",
    last_name = "Cutcher",
    state_cd = "CO-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "William Cutcher Campaign",
    campaign_website = "https://wcutcher.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/william-cutcher-campaign/donate",
    FEC_ID_cmte = "C00725572",
    FEC_ID_cand = "H0CO02217"
  ),
  # Cannot find Mark Matyi
  # CO-03 (last updated Jul 1, 2020) ------------------------------------------
  # Tipton vs. Mitsch Bush vs. Routledge vs. Valdez vs. Moser
  # Cannot find Moser
  tibble(
    first_name = "Scott",
    last_name = "Tipton",
    state_cd = "CO-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://tipton.house.gov",
    campaign_name = "Scott Tipton for Congress",
    campaign_website = "https://www.votetipton.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://winred.revv.co/scott-tipton-for-congress/contribute",
    FEC_ID_cmte = "C00470757",
    FEC_ID_cand = "H6CO03139"
  ),
  tibble(
    first_name = "Diane",
    last_name = "Mitsch Bush",
    state_cd = "CO-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Diane for Colorado",
    campaign_website = "https://dianeforcolorado.com",
    actblue_link = "dmb-2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00707638", "C00649459"),
    FEC_ID_cand = "H8CO03192"
  ),
  tibble(
    first_name = "James",
    last_name = "Iacino",
    state_cd = "CO-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "James for Colorado",
    campaign_website = "https://jamesforcolorado.com/",
    actblue_link = "james_website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00723338",
    FEC_ID_cand = "H0CO03157"
  ),
  tibble(
    first_name = "Lauren",
    last_name = "Boebert",
    state_cd = "CO-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lauren Boebert for Congress",
    campaign_website = "https://laurenforcolorado.com/",
    actblue_link = NA,
    anedot_link = "lauren-boebert-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00728238",
    FEC_ID_cand = "H0CO03165"
  ),
  tibble(
    first_name = "Root",
    last_name = "Routledge",
    state_cd = "CO-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Root For Citizen Congressman",
    campaign_website = "http://rootforcitizencongressman.org",
    actblue_link = "root-for-citizen-congressman-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00670943",
    FEC_ID_cand = "H8CO03234"
  ),
  # Changed to House District campaign 62
  tibble(
    first_name = "Donald",
    last_name = "Valdez",
    state_cd = "CO-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Donald Valdez Congress",
    campaign_website = "https://www.valdezforco.com",
    actblue_link = "donald-valdez-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00712034",
    FEC_ID_cand = "H0CO03132"
  ),
  tibble(
    first_name = "Critter", # Christopher, goes by Critter
    last_name = "Milton",
    state = "CO",
    party = "Uni",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Critter Milton for Colorado",
    campaign_website = "https://crittermilton.com",
    # Changed to Unity Party
    actblue_link = "crittermilton",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00719823",
    FEC_ID_cand = "H0CO03173"
  ),
  # Cannot find Robert Moser
  # CO-04 (last updated Jul 1, 2021) ------------------------------------------
  # Buck vs. McCorkle vs. Griffith vs. Ireland
  tibble(
    first_name = "Kenneth",
    last_name = "Buck",
    state_cd = "CO-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://buck.house.gov",
    campaign_name = "Buck for Colorado",
    campaign_website = "http://www.buckforcolorado.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://secure.campaignsolutions.com/kenbuck/list/proc/donation1/?",
      "initiativekey=V0T9TNVLFGX5"
    ),
    FEC_ID_cmte = "C00573378",
    FEC_ID_cand = "H4CO04090"
  ),
  tibble(
    first_name = "Ike", # Issac
    last_name = "McCorkle",
    state_cd = "CO-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "McCorkle For Colorado",
    campaign_website = "https://ike4co.com",
    actblue_link = "mccorkleforcolorado",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00697334",
    FEC_ID_cand = "H0CO04197"
  ),
  tibble(
    first_name = "Bruce",
    last_name = "Griffith",
    state_cd = "CO-04",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bruce Griffith for Colorado",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H6CO04178"
  ),
  tibble(
    first_name = "Laura",
    last_name = "Ireland",
    state_cd = "CO-04",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Laura Ireland for Colorado",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0CO04221"
  ),
  tibble(
    first_name = "Melanie",
    last_name = "Tomitsch",
    state_cd = "CO-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tomitsch 4 Congress",
    campaign_website = "https://twitter.com/mdtomitsch",
    actblue_link = "mdt4c",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00742304",
    FEC_ID_cand = "H0CO04213"
  ),
  # Cannot find campaign website for McGannon
  # CO-05 (last updated Jul 11, 2021) ------------------------------------------
  # Lamborn vs. Freeland vs. Duffett
  tibble(
    first_name = "Doug",
    last_name = "Lamborn",
    state_cd = "CO-05",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://lamborn.house.gov",
    campaign_name = "lamborn-for-congress/donate",
    campaign_website = "https://www.lambornforcongress.org",
    actblue_link = NA,
    # This wasn't recorded, for some reason (yikes)
    anedot_link = "lamborn-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00420745",
    FEC_ID_cand = "H6CO05159"
  ),
  tibble(
    first_name = "Jillian",
    last_name = "Freeland",
    state_cd = "CO-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jillian Freeland for Congress",
    campaign_website = "https://jillianfreeland.com",
    actblue_link = NA,
    anedot_link = NA,
    # 20-35-50-100-500-2800
    other_link = "https://secure.ngpvan.com/v1/Forms/3Bc6vk22YEOEgD_IvEjdfw2",
    FEC_ID_cmte = "C00700997",
    FEC_ID_cand = "H0CO05129"
  ),
  tibble(
    first_name = "Ed",
    last_name = "Duffett",
    state_cd = "CO-05",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ed Duffett for Congress",
    campaign_website = "https://www.edduffettforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0CO05160"
  ),
  tibble(
    first_name = "Marcus",
    last_name = "Murphy",
    state_cd = "CO-05",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marcus Murphy for Congress",
    campaign_website = "https://www.facebook.com/MurphyForCongressCO5/",
    actblue_link = NA,
    anedot_link = NA,
    # Could not find any donation link
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # Withdrew from primary, ActBlue already withdrawn
  tibble(
    first_name = "Ryan",
    last_name = "Lucas",
    state_cd = "CO-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jillian Freeland for Congress",
    campaign_website = "https://ryanlucasforcolorado.com/",
    actblue_link = "lucascd5",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00733428",
    FEC_ID_cand = "H0CO05145"
  ),
  tibble(
    first_name = "Brandon",
    last_name = "Bocchino",
    state_cd = "CO-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brandon for Colorado",
    campaign_website = "https://www.brandon4colorado.com/",
    actblue_link = "brandon-for-colorado-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00729814",
    FEC_ID_cand = "H0CO05137"
  ),
  tibble(
    first_name = "Marcus",
    last_name = "Murphy",
    state_cd = "CO-05",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marcus Murphy for Congress",
    campaign_website = "https://www.facebook.com/MurphyForCongressCO5/",
    actblue_link = NA,
    anedot_link = NA,
    # could not find any donation link
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CO-06 (last updated Jul 11, 2021) ------------------------------------------
  # Crow vs. Stockham vs. House vs. Olsen Szemler
  # As much attention as House is getting (former CO GOP chairman)
  # can't find his website...
  tibble(
    first_name = "Jason",
    last_name = "Crow",
    state_cd = "CO-06",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://crow.house.gov",
    campaign_name = "Jason Crow for Congress",
    campaign_website = "https://jasoncrowforcongress.com/",
    # Not sure when switched, but between Feb 11 and Jun 16
    # 25-50-100-250-500-2800 to 5-10-25-50-100-250-500
    actblue_link = "crow-website",
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/8904834646960245248",
    FEC_ID_cmte = "C00637363",
    FEC_ID_cand = "H8CO06229"
  ),
  tibble(
    first_name = "John Francis",
    last_name = "Szemler",
    state_cd = "CO-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.johnszemlerforcolorado.com",
    actblue_link = NA,
    anedot_link = NA,
    # No amount prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?token=hY451WPRma-yRaaKSIxAdwaMWdL4Fw9X",
      "t2ppoN4GdUR3fYA10b8uWTDFzR9lFmAmQaMgJG&country.x=US&locale.x=&Z3JncnB0="
    ),
    FEC_ID_cmte = "C00719146",
    FEC_ID_cand = "H0CO06127"
  ),
  tibble(
    first_name = "Jaimie",
    last_name = "Kulikowski",
    state_cd = "CO-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jaimie Lynn Kulikowski for Congress",
    campaign_website = "https://jaimielynn.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 100-50-10
    other_link = paste0(
      "https://www.paypal.com/donate/?token=84Mb4UtlCQUDdPljeAbtdf0N2P",
      "pQYlz4_CYKOTyMpmEVkdtJLWea8x0_tcgwb5Ae6z8-pW&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = c("C00721084", "C00687756"),
    FEC_ID_cand = "H8CO06294"
  ),
  tibble(
    first_name = "Steven",
    last_name = "House",
    state_cd = "CO-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "House for CO-06",
    campaign_website = "https://houseforcolorado.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/stevehouse/donate",
    FEC_ID_cmte = c("C00717868", "C00702506"),
    FEC_ID_cand = "H0CO06119"
  ),
  tibble(
    first_name = "Norm",
    last_name = "Olsen",
    state_cd = "CO-06",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Norm Olsen for Congress",
    campaign_website = "http://norm4liberty.org/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = "H2CO02122"
  ),
  # Cannot find Ryan Gonzalez
  # CO-07 (last updated Jul 11, 2021) ------------------------------------------
  # Perlmutter vs. Stockham vs. Biles vs. Olstza vs. Clay
  tibble(
    first_name = "Ed",
    last_name = "Perlmutter",
    state_cd = "CO-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://perlmutter.house.gov",
    campaign_name = "Perlmutter for Colorado",
    campaign_website = "https://perlmutterforcolorado.com",
    actblue_link = "perlmutterforcolorado",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00410639",
    FEC_ID_cand = "H6CO07023"
  ),
  tibble(
    first_name = "Nathan",
    last_name = "Clay",
    state_cd = "CO-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nathan Clay For Congress",
    campaign_website = "https://nathanclayforcongress.com",
    # Interesting choices... 10-27-54-95-270-540-2700
    actblue_link = "ncfc",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00637710",
    FEC_ID_cand = "H8CO07052"
  ),
  tibble(
    first_name = "Casper",
    last_name = "Stockham",
    state_cd = "CO-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Casper for Colorado",
    campaign_website = "https://casperforcolorado.com/",
    actblue_link = NA,
    anedot_link = NA,
    # It's own website
    # https://casperforcolorado.com/
    # Difficult to scrape, work later: 2800-1000-500-100-50-35
    other_link = "https://politics.raisethemoney.com/hipvtk9lopqvrdicigeuzw",
    FEC_ID_cmte = "C00710855",
    FEC_ID_cand = "H0CO06101"
  ),
  tibble(
    first_name = "Ken",
    last_name = "Biles",
    state_cd = "CO-07",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ken for US House",
    campaign_website = "http://ken4colorado.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "http://ken4colorado.com/donate-1/",
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0CO07109"
  ),
  tibble(
    first_name = "David",
    last_name = "Olszta",
    state_cd = "CO-07",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Olszta for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0CO07117"
  ),
  tibble(
    first_name = "Anthony",
    last_name = "Malgieri",
    state_cd = "CO-07",
    party = "Non",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Anthony Malgieri For Congress",
    campaign_website = "https://anthonymalgieri.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 10-20-50
    other_link = paste0(
      "https://www.paypal.com/donate/?token=4urvdHvDZvZxVPUjWe9hHV9_M",
      "RFwUGXobteTzzbiHH9wz17Z3cXR-MHkW_SwYNeJIeWAwG&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00710855",
    FEC_ID_cand = "H0CO06101"
  ),
  # CT: Connecticut (last updated Jul 11, 2021) ==============================
  # CT-01 ---------------------------------------------------------------------
  # John Larson is uncontested (no candidates registered as of Sep 24, 2019)
  # Now Mary Fay in the race.
  tibble(
    first_name = "John",
    last_name = "Larson",
    state_cd = "CT-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://larson.house.gov",
    campaign_name = "Larson for Congress",
    campaign_website = "http://larsonforcongress.org/",
    actblue_link = "jbl",
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/6151618692714137600",
    FEC_ID_cmte = "C00330142",
    FEC_ID_cand = "H8CT01046"
  ),
  tibble(
    first_name = "Mary",
    last_name = "Fay",
    state_cd = "CT-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mary Fay 4 Congress",
    campaign_website = "https://www.maryfay4congress.com/",
    actblue_link = NA,
    anedot_link = "mary-fay-4-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CT-02 ---------------------------------------------------------------------
  tibble(
    first_name = "Joe",
    last_name = "Courtney",
    state_cd = "CT-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://courtney.house.gov/",
    campaign_name = "Joe Courtney for Congress",
    campaign_website = "https://www.joecourtney.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.ngpvan.com/v1/Forms/JYMNKz-V9EOuJh3orF2Uvg2",
    FEC_ID_cmte = "C00410233",
    FEC_ID_cand = "H2CT02112"
  ),
  tibble(
    first_name = "Justin",
    last_name = "Anderson",
    state_cd = "CT-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Justin Anderson For Congress",
    campaign_website = "https://www.justinandersonforcongress.com/",
    actblue_link = NA,
    # empty box, no options
    anedot_link = "justin-anderson-for-congress/donate/",
    other_link = NA,
    FEC_ID_cmte = "C00713826",
    FEC_ID_cand = "H0CT02207"
  ),
  tibble(
    first_name = "Cassandra",
    last_name = "Martineau",
    state_cd = "CT-02",
    party = "Grn",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cass for Congress",
    campaign_website = "https://www.cass4congress.rocks/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0CT02215"
  ),
  # CT-03 ---------------------------------------------------------------------
  # Rosa DeLauro is uncontested (no candidates registered as of Sep 24, 2019)
  tibble(
    first_name = "Rosa",
    last_name = "DeLauro",
    state_cd = "CT-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://delauro.house.gov",
    campaign_name = "Rosa DeLauro for Congress",
    campaign_website = "http://www.rosadelauro.com/",
    actblue_link = "rosa-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00238865",
    FEC_ID_cand = "H0CT03072"
  ),
  tibble(
    first_name = "Margaret",
    last_name = "Streicker",
    state_cd = "CT-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Stericker for Congress",
    campaign_website = "https://streicker2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/streicker-for-congress-inc/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Justin",
    last_name = "Paglino",
    state_cd = "CT-03",
    party = "Gre",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Justin for All",
    campaign_website = "https://justin4all.org/",
    actblue_link = NA,
    # only box link, no amounts
    anedot_link = "justin4all/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # CT-04 ---------------------------------------------------------------------
  tibble(
    first_name = "Jim",
    last_name = "Himes",
    state_cd = "CT-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://himes.house.gov/",
    campaign_name = "Jim Himes for Congress",
    campaign_website = "https://www.himesforcongress.com/",
    actblue_link = "stand-with-jim-himes",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00434191",
    FEC_ID_cand = "H8CT04172"
  ),
  tibble(
    first_name = "Jonathan",
    last_name = "Riddle",
    state_cd = "CT-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jonathan Riddle for Congress",
    campaign_website = "https://www.riddleforcongress.com/",
    actblue_link = NA,
    anedot_link = "riddle-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00715508",
    FEC_ID_cand = "H0CT04195"
  ),
  tibble(
    first_name = "Brian",
    last_name = "Merlen",
    state_cd = "CT-04",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vote Merlen",
    campaign_website = "https://www.votemerlen.com/",
    actblue_link = NA,
    anedot_link = "together-health/donate",
    other_link = NA,
    FEC_ID_cmte = "C00744722",
    FEC_ID_cand = "H0CT04211"
  ),
  # CT-05 ---------------------------------------------------------------------
  tibble(
    first_name = "Jahana",
    last_name = "Hayes",
    state_cd = "CT-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://hayes.house.gov/",
    campaign_name = "Jahana Hayes for Congress",
    campaign_website = "https://jahanahayes.com/",
    actblue_link = "jh19web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "David X.",
    last_name = "Sullivan",
    state_cd = "CT-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David X. Sullivan for Congress",
    campaign_website = "https://davidxsullivan.com/",
    actblue_link = NA,
    anedot_link = "sullivan-for-congress/davidxsullivanforuscongresswebe",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # DE: Delaware (at-large district; last updated Oct 8, 2020) =================
  # Blunt Rochester vs. Murphy
  tibble(
    first_name = "Lisa",
    last_name = "Blunt Rochester",
    state_cd = "DE-0",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://bluntrochester.house.gov",
    campaign_name = "Lisa Blunt Rochester for Congress",
    campaign_website = "https://lisabluntrochester.com/",
    actblue_link = "lisa-blunt-rochester-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00590778",
    FEC_ID_cand = "H6DE00206"
  ),
  tibble(
    first_name = "Lee",
    last_name = "Murphy",
    state_cd = "DE-0",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Lee Murphy for U.S. Congress",
    campaign_website = "https://gomurph.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      "https://secure.winred.com/gomurph/donate",
      "https://donorbox.org/embed/gomurph"
    ),
    FEC_ID_cmte = c("C00709584", "C00679696"),
    FEC_ID_cand = "H8DE01044"
  ),
  tibble(
    first_name = "Matthew",
    last_name = "Morris",
    state_cd = "DE-0",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Matthew Morris For US House Delaware At Large",
    campaign_website = "https://www.matthewmorrisfordelaware.com",
    actblue_link = NA,
    # No prompts
    anedot_link = "friends-of-matthew-morris/donate",
    other_link = NA,
    FEC_ID_cmte = "C00743401",
    FEC_ID_cand = "H0DE01066"
  ),
  # DC: Washington DC (last updated Jul 11, 2021) ==============================
  # At-large nonvoting district
  # Eleanor Holmes Norton was uncontested as of Sep 17, 2019
  # Now Norton vs. Hynes vs. Musa vs. Lowery vs. Krucoff
  tibble(
    first_name = "Eleanor Holmes",
    last_name = "Norton",
    state_cd = "DC-0",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://norton.house.gov",
    campaign_name = "",
    # Down at the moment.
    # website still down as of Sept. 2020
    campaign_website = "http://www.nortonforcongress.org/",
    actblue_link = "eleanor-holmes-norton-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00244335",
    FEC_ID_cand = "H0DC00058"
  ),
  tibble(
    first_name = "Patrick",
    last_name = "Hynes",
    state_cd = "DC-0",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Patrick Hynes for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0DC00264"
  ),
  tibble(
    first_name = "Omari",
    last_name = "Musa",
    state_cd = "DC-0",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Omari Musa for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0DC00280"
  ),
  tibble(
    first_name = "Amir",
    last_name = "Lowery",
    state_cd = "DC-0",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Amir Lowery for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0DC00272"
  ),
  tibble(
    first_name = "David",
    last_name = "Krucoff",
    state_cd = "DC-0",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Krucoff for Congress",
    campaign_website = "https://www.krucoffforcongress.com",
    actblue_link = NA,
    anedot_link = "krucoff-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00719930",
    FEC_ID_cand = "H0DC01015"
  ),
  tibble(
    first_name = "John C.",
    last_name = "Cheeks",
    state_cd = "DC-0",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John C. Cheeks for Congress",
    # website down
    campaign_website = "http://johnccheeks.com/",
    actblue_link = NA,
    anedot_link = NA,
    # cannot find donation links
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL: Florida (last updated Jul 11, 2021) =======================================
  # FL-01 ---------------------------------------------------------------------
  # Romagnano seems to have withdrawn
  tibble(
    first_name = "Matt",
    last_name = "Gaetz",
    state_cd = "FL-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://gaetz.house.gov",
    campaign_name = "Gaetz for Congress",
    campaign_website = "https://mattgaetz.com",
    actblue_link = NA,
    anedot_link = NA,
    # 5-10-20.20-50-100-500-2800-5600
    other_link = "https://gaetzforcongress.revv.co/donate",
    FEC_ID_cmte = "C00612432",
    FEC_ID_cand = "H6FL01119"
  ),
  tibble(
    first_name = "Phil",
    last_name = "Ehr",
    state_cd = "FL-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Phil Ehr for U.S. Congress",
    campaign_website = "https://www.ehrforcongress.com",
    actblue_link = c(
      "ehr-for-congress-wb",
      "ehr-for-congress-tw"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00698993",
    FEC_ID_cand = "H8FL01107"
  ),
  tibble(
    first_name = "John",
    last_name = "Mills",
    state_cd = "FL-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John Mills for Congress",
    campaign_website = "http://johnmillsforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://www.paypal.com/donate/?token=kY-7W7N57ve-AklK4ZkikfNz5k-E5",
      "ikHjfSgCWt1XJk7YB3RSxvE6oM6KZfmZd_Hf17ypm&country.x=US&locale.x=US",
      "&Z3JncnB0="
    ),
    FEC_ID_cmte = "C00565366",
    FEC_ID_cand = "H6FL01143"
  ),
  tibble(
    first_name = "Albert",
    last_name = "Oram",
    state_cd = "FL-01",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "ORAM for Congress",
    campaign_website = "https://www.oramforcongress.org/",
    actblue_link = NA,
    anedot_link = NA,
    # link does not work
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-02 ---------------------------------------------------------------------
  # Cannot find Anderson
  # Kim O'Connor a write-in candidate vs incumbent Dunn
  tibble(
    first_name = "Neal",
    last_name = "Dunn",
    state_cd = "FL-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://dunn.house.gov",
    campaign_name = "Neal Dunn for Congress",
    campaign_website = "https://www.nealdunn.com/home",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      paste0(
        "https://spark.widgetmakr.com/widget/render/",
        "bc027835-dca5-4dc9-b691-cdb648713f67"
      ),
      "https://secure.winred.com/nealdunn/donate"
    ),
    FEC_ID_cmte = "C00582304",
    FEC_ID_cand = "H6FL02208"
  ),
  tibble(
    first_name = "Kim",
    last_name = "O'Connor",
    state_cd = "FL-02",
    party = "Ind",
    incumbent = FALSE,
    office_website = "Write-In Kim",
    campaign_website = "https://www.writeinkim.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H4FL02013"
  ),
  # FL-03 ---------------------------------------------------------------------
  # Ted Yoho is retiring(!)
  # St. George vs. Sapp vs. Cammack vs. Chase vs. Wells
  # vs. Rollins vs. Chamberlin vs. Engelbrecht vs. Dodds vs. Christensen
  # vs. Theus vs. Wells vs. Giunn vs. Braddy vs. Peters vs. Rowe
  # Cammack vs. Christensen for General
  tibble(
    first_name = "Kat",
    last_name = "Cammack",
    state_cd = "FL-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kat for Congress",
    campaign_website = "https://www.katforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/katcammack/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Adam",
    last_name = "Christensen",
    state_cd = "FL-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Adam J Christensen for Congress",
    campaign_website = "https://www.forthemanynotjustme.com/",
    actblue_link = "forthemanynotjustme.com",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-04 ---------------------------------------------------------------------
  # Rutherford vs. Deegan vs. Koniz(write-in)
  tibble(
    first_name = "John",
    last_name = "Rutherford",
    state_cd = "FL-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://rutherford.house.gov/",
    campaign_name = "John Rutherford for COngress",
    campaign_website = "https://johnrutherfordforcongress.com/",
    actblue_link = NA,
    anedot_link = "citizens-for-john-rutherford/rutherfordforcd4/",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Donna",
    last_name = "Deegan",
    state_cd = "FL-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Donna Deegan for Congress",
    campaign_website = "https://donnadeeganforcongress.com/",
    actblue_link = "donnadeegan",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Gary",
    last_name = "Koniz",
    state_cd = "FL-04",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Koniz for Congress",
    campaign_website = "https://www.garykonizforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # empty box, no amounts.
    other_link = "https://www.garykonizforcongress.com/donate.html",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-05 ---------------------------------------------------------------------
  tibble(
    first_name = "Al",
    last_name = "Lawson",
    state_cd = "FL-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://lawson.house.gov/",
    campaign_name = "Al Lawson for Congress",
    campaign_website = "https://www.allawson.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 10-20-35-50-100-500-1000-2800
    # other_link = "https://www.allawson.com/donate/",
    other_link = "https://secure.ngpvan.com/v1/Forms/-JGiuva5hk6bvLoBN7nHcw2",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Gary",
    last_name = "Adler",
    state_cd = "FL-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gary Adler for Congress",
    campaign_website = "https://electadler.com/",
    actblue_link = NA,
    anedot_link = "gary-adler-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-06 ---------------------------------------------------------------------
  # Grayson is write-in candidate. (but ex-congressman) but cant find.
  tibble(
    first_name = "Michael",
    last_name = "Waltz",
    state_cd = "FL-06",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://waltz.house.gov/",
    campaign_name = "Michael Waltz for US Congress",
    campaign_website = "https://michaelwaltz.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/michaelwaltz/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Clint",
    last_name = "Curtis",
    state_cd = "FL-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Clint Curtis for Congress",
    campaign_website = "http://clintcurtis4congress.com/",
    actblue_link = "clint-curtis-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Alan",
    last_name = "Grayson",
    state_cd = "FL-06",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Alan Grayson for Congress",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-07 ---------------------------------
  tibble(
    first_name = "Stephanie",
    last_name = "Murphy",
    state_cd = "FL-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://murphy.house.gov",
    campaign_name = "Stephanie Murphy for Congress",
    campaign_website = "http://www.stephaniemurphyforcongress.com",
    actblue_link = "murphy-homepage",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00620443",
    FEC_ID_cand = "H6FL07140"
  ),
  tibble(
    first_name = "Leo",
    last_name = "Valentin",
    state_cd = "FL-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Leo Valentin for Congress",
    campaign_website = "https://www.leovalentin.com",
    actblue_link = NA,
    anedot_link = "leo-valentin-for-congress/florida7",
    other_link = NA,
    FEC_ID_cmte = "C00726000",
    FEC_ID_cand = "H0FL07143"
  ),
  tibble(
    first_name = "William",
    last_name = "Garlington",
    state_cd = "FL-07",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bill Garlington for Congress",
    campaign_website = "https://www.billgarlingtonforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 50-100-250-500-1000-2800-5600-11,200 (max-couples' donation)
    other_link = paste0(
      "https://www.billgarlingtonforcongress.com",
      "/campaigns/donation/donate/"
    ),
    FEC_ID_cmte = c("C00740860", "C00740852"),
    FEC_ID_cand = "H0FL07192"
  ),
  tibble(
    first_name = "Yukong",
    last_name = "Zhao",
    state_cd = "FL-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Yukong Zhao for Congress",
    campaign_website = "https://yukongzhaoforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/YukongZhao/donate",
    FEC_ID_cmte = "C00726596",
    FEC_ID_cand = "H0FL07150"
  ),
  tibble(
    first_name = "Richard",
    last_name = "Goble",
    state_cd = "FL-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Goble for Congress",
    campaign_website = "https://gobleforcongress.com",
    actblue_link = NA,
    anedot_link = "richard-goble-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = c("C00723353", "C00723346"),
    FEC_ID_cand = "H0FL07135"
  ),
  # Janet Edwards withdrew
  tibble(
    first_name = "Mike",
    last_name = "Thibodeau",
    state_cd = "FL-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mike Thibodeau for Congress",
    campaign_website = "https://miketforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-1000-2800
    other_link =
      "https://www.donatebucket.com/colContributionForm.aspx?cid=NDIz",
    FEC_ID_cmte = "C00711465",
    FEC_ID_cand = "H0FL07127"
  ),
  # Armani Salado withdrew
  # Only FB for Thomas Delia/Jean Rochelle "Chelle" DiAngelus/Eduardo Mejias
  tibble(
    first_name = "Sean",
    last_name = "Jackson",
    state_cd = "FL-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sean Jackson for Congress",
    campaign_website = "http://seanjacksonforcongress.com/sean-p-jackson-home/",
    actblue_link = NA,
    anedot_link = "sean-jackson-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00738500",
    FEC_ID_cand = "H0FL07184"
  ),
  # FL-08 ---------------------------------------------------------------------
  tibble(
    first_name = "Bill",
    last_name = "Posey",
    state_cd = "FL-08",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://posey.house.gov/",
    campaign_name = "Bill Posey for US Congress",
    campaign_website = "https://billposey.com/",
    actblue_link = NA,
    anedot_link = "friends-of-bill-posey/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Jim",
    last_name = "Kennedy",
    state_cd = "FL-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jim Kennedy for Congress",
    campaign_website = "https://www.kennedyforflorida.com/",
    actblue_link = "jim-kennedy-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-09 ---------------------------------------------------------------------
  # cannot find Clay Hill (write-in Ind)
  tibble(
    first_name = "Darren",
    last_name = "Soto",
    state_cd = "FL-09",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://soto.house.gov",
    campaign_name = "Soto for Congress",
    campaign_website = "https://www.darrensoto.com/",
    actblue_link = "soto",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Bill",
    last_name = "Olson",
    state_cd = "FL-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bill Olson for Congress",
    campaign_website = "https://billolsonfl.com/",
    actblue_link = NA,
    anedot_link = "william-olson-for-congress/website",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-10 ---------------------------------------------------------------------
  # Demings vs. Troen
  # Demings vs. Francois for General
  tibble(
    first_name = "Val",
    last_name = "Demings",
    state_cd = "FL-10",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://demings.house.gov",
    campaign_name = "Val Demings for Congress",
    campaign_website = "http://www.valdemings.com/",
    actblue_link = "chief_val_demings",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00590489",
    FEC_ID_cand = "H2FL08063"
  ),
  tibble(
    first_name = "Kirk",
    last_name = "Troen",
    state_cd = "FL-10",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Troen For Congress",
    campaign_website = "https://troenforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # No suggested amounts. Could be a good baseline.
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "token=ICBO_id9yveCTU0rgHXWnYm6U-xcMtr-MvD4Kb575pQCPeEwDGKNQtNRcerJQmO",
      "w5N8bpG&country.x=US&locale.x="
    ),
    FEC_ID_cmte = "C00703215",
    FEC_ID_cand = "H0FL10089"
  ),
  tibble(
    first_name = "Vennia",
    last_name = "Francois",
    state_cd = "FL-10",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Vennia Francois For Congress",
    campaign_website = "https://venniaforcongress.com/",
    actblue_link = NA,
    anedot_link = "venniafrancois/web-donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-11 ---------------------------------------------------------------------
  tibble(
    first_name = "Daniel",
    last_name = "Webster",
    state_cd = "FL-11",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://webster.house.gov/",
    campaign_name = "Daniel Webster for Congress",
    campaign_website = "https://www.electwebster.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://secure.campaignsolutions.com/webster/",
      "donation1/default.aspx?InitiativeKey=8LI7KDO8RIOF"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Dana",
    last_name = "Cottrell",
    state_cd = "FL-11",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dana Cottrell for Congress",
    campaign_website = "https://dana2020.com/",
    actblue_link = "dcottrell2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-12 ---------------------------------------------------------------------
  tibble(
    first_name = "Gus",
    last_name = "Bilirakis",
    state_cd = "FL-12",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://bilirakis.house.gov/",
    campaign_name = "Bilirakis for Congress",
    campaign_website = "http://bilirakisforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      "https://secure.winred.com/bilirakis-for-congress/home-page",
      "https://secure.winred.com/bilirakis-for-congress/donate"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Kimberly",
    last_name = "Walker",
    state_cd = "FL-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kimberly Walker for Congress",
    campaign_website = "https://kimberlyforcongress.com/",
    actblue_link = "kimforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Michael Stevem",
    last_name = "Knezevich",
    state_cd = "FL-12",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Knezevich for Congress",
    campaign_website = "https://www.mskforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # paypal checkout with no suggested amounts
    other_link = paste0(
      "https://www.powr.io/apps/paypal-button/",
      "view?id=20626652&mode=page&transaction_id=7044118"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-13 ------------------------------------------
  # Crist vs. Makki
  # vs. Buck vs. Paulina Luna vs. Becker vs. Griffin vs. Newby vs. Sacramento
  # Crist vs Paulina Luna for General
  tibble(
    first_name = "Charlie",
    last_name = "Crist",
    state_cd = "FL-13",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://crist.house.gov",
    campaign_name = "Charlie Crist for Congress",
    campaign_website = "https://charliecrist.com",
    actblue_link = c(
      "charlie-crist-1",
      "crist-email-btn",
      "charlie-eoq"
    ),
    anedot_link = NA,
    # So this is a very atypical NVP VAN and I can't quite figure it out...
    # 20-250-500-1000-2800-5600
    # Sept 2020 amounts (looks like Stripe checkout):
    # 50-100-250-500-1000-2800
    other_link = "https://secure.numero.ai/contribute/CharlieCrist",
    FEC_ID_cmte = "C00590067",
    FEC_ID_cand = "H6FL13205"
  ),
  tibble(
    first_name = "Amanda",
    last_name = "Makki",
    state_cd = "FL-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.amandamakki.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://makkiforcongress.ichooseapex.com/donate/",
    FEC_ID_cmte = "C00708263",
    FEC_ID_cand = "H0FL13133"
  ),
  tibble(
    first_name = "George",
    last_name = "Buck",
    state_cd = "FL-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "George Buck for Congress",
    campaign_website = "https://georgebuckjr.com",
    actblue_link = NA,
    anedot_link = "georgebuck/main",
    other_link = NA,
    FEC_ID_cmte = "C00667808",
    FEC_ID_cand = "H8FL13136"
  ),
  tibble(
    first_name = "Anna",
    last_name = "Paulina Luna",
    state_cd = "FL-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Anna Paulina Luna for Congress",
    campaign_website = "https://www.voteannapaulina.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/AnnaPaulinaLuna/web",
    FEC_ID_cmte = "C00718239",
    FEC_ID_cand = "H0FL13158"
  ),
  tibble(
    first_name = "Matt",
    last_name = "Becker",
    state_cd = "FL-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Matt Becker for Congress",
    campaign_website = "https://www.mattbecker.org",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/MattBecker/donate",
    FEC_ID_cmte = "C00712489",
    FEC_ID_cand = "H0FL13141"
  ),
  tibble(
    first_name = "Sheila",
    last_name = "Griffin",
    state_cd = "FL-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sheila Griffin for Congress",
    campaign_website = "https://votesheilagriffin.com",
    actblue_link = NA,
    # No prompts
    anedot_link = "sheilagriffin/donate",
    other_link = NA,
    FEC_ID_cmte = "C00703579",
    FEC_ID_cand = "H0FL13125"
  ),
  tibble(
    first_name = "Sharon Barry",
    last_name = "Newby",
    state_cd = "FL-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sharon Barry Newby For Congress",
    campaign_website = "https://shari4congress.com",
    actblue_link = NA,
    anedot_link = "shari-barry-newby-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00724062",
    FEC_ID_cand = "H0FL13166"
  ),
  # Cannot find Sacramento
  # FL-14 ---------------------------------------------------------------------
  tibble(
    first_name = "Kathy",
    last_name = "Castor",
    state_cd = "FL-14",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://castor.house.gov",
    campaign_name = "Castor for Congress",
    campaign_website = "https://www.castorforcongress.com/",
    actblue_link = "kc_website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Christine",
    last_name = "Quinn",
    state_cd = "FL-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Christine Quinn for Congress",
    campaign_website = "https://win-quinn.com/",
    actblue_link = NA,
    anedot_link = "christine-quinn-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Robert",
    last_name = "Wunderlich",
    state_cd = "FL-14",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Robert Wunderlich for Congress",
    campaign_website = "https://www.wunderlichforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # paypal link - no suggested amt.
    other_link = paste0(
      "https://www.paypal.com/donate?token=N6SJqjn-XCR",
      "GD5G7hJjix4jpeM227Gh1NJ7iHz4_7MuCxlKobEBClj1W3b5YAvk57W4qctBKB066zMSK"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-15 ---------------------------------------------------------------------
  # Scott Franklin def. incumbent Spano
  # Franklin vs. Cohn general.
  tibble(
    first_name = "Scott",
    last_name = "Franklin",
    state_cd = "FL-15",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Scott Franklin for Congress",
    campaign_website = "https://www.votescottfranklin.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/scott-franklin-for-congress/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Alan",
    last_name = "Cohn",
    state_cd = "FL-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Alan Cohn for Congress",
    campaign_website = "https://alancohnforcongress.com/",
    actblue_link = "cohn-for-congress-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-16 ---------------------------------------------------------------------
  tibble(
    first_name = "Vern",
    last_name = "Buchanan",
    state_cd = "FL-16",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://buchanan.house.gov",
    campaign_name = "Vern Buchanan for Congress",
    campaign_website = "https://www.vernbuchanan.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/vernbuchanan/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Margaret",
    last_name = "Good",
    state_cd = "FL-16",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Margaret Good for Congress",
    campaign_website = "https://margaretgood.com/",
    actblue_link = "margaretgood_website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-17 ---------------------------------------------------------------------
  tibble(
    first_name = "Greg",
    last_name = "Steube",
    state_cd = "FL-17",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://steube.house.gov",
    campaign_name = "Greg Steube for Congress",
    campaign_website = "https://www.electgregsteube.com/",
    actblue_link = NA,
    anedot_link = "steube/donate",
    other_link = NA,
    FEC_ID_cmte = "C00671891",
    FEC_ID_cand = "H8FL17053"
  ),
  tibble(
    first_name = "Allen",
    last_name = "Ellison",
    state_cd = "FL-17",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ellison for Congress",
    campaign_website = "http://ellisonforcongress.com/",
    actblue_link = "allenellison2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0FL17159"
  ),
  # FL-18 ---------------------------------------------------------------------
  # Brian Mast was uncontested (no candidates registered as of Sep 23, 2019)
  # Now Mast vs. Vessio vs. Vazquez
  # Now Mast vs. Keith vs. Miller for general
  tibble(
    first_name = "Brian",
    last_name = "Mast",
    state_cd = "FL-18",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://mast.house.gov",
    campaign_name = "Mast for Congress",
    campaign_website = "https://www.mastforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/mast-for-congress/donate",
    FEC_ID_cmte = "C00632257",
    FEC_ID_cand = "H6FL18097"
  ),
  tibble(
    first_name = "Nick",
    last_name = "Vessio",
    state_cd = "FL-18",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vessio for Congress",
    campaign_website = "https://www.facebook.com/ritadan123/",
    # No donation page yet
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00721803",
    FEC_ID_cand = "H0FL18181"
  ),
  tibble(
    first_name = "Oz",
    last_name = "Vazquez",
    state_cd = "FL-18",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Oz Vazquez for Congress",
    campaign_website = "https://www.ozforcongress.com",
    actblue_link = "oz-homepage",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00721290",
    FEC_ID_cand = "H0FL18199"
  ),
  tibble(
    first_name = "Pam",
    last_name = "Keith",
    state_cd = "FL-18",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Pam Keith for Congress",
    campaign_website = "https://www.pamkeithfl.com/",
    actblue_link = "pamkeith-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "K. W.",
    last_name = "Miller",
    state_cd = "FL-18",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "KW Miller for Congress",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-1000-2000-2800
    other_link = "https://kwmillerforcongress.square.site/",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-19 ---------------------------------------------------------------------
  # Incumbent Rooney not seeking reelection
  tibble(
    first_name = "Byron",
    last_name = "Donalds",
    state_cd = "FL-19",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Byron Donalds for Congress",
    campaign_website = "https://www.byrondonalds.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/byrondonalds/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Cindy",
    last_name = "Banyai",
    state_cd = "FL-19",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cindy Banyai for Congress",
    campaign_website = "https://www.cindybanyai.com/",
    actblue_link = "cindy-banyai-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # cant find Dumornay (Ind)
  # FL-20 ---------------------------------------------------------------------
  tibble(
    first_name = "Alcee",
    last_name = "Hastings",
    state_cd = "FL-20",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://alceehastings.house.gov",
    campaign_name = "Hastings for Congress",
    campaign_website = "https://www.alceeforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # NGP VAN address
    other_link = "https://act.myngp.com/Forms/4586072895058345984",
    # "https://www.alceeforcongress.com/donate"
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Greg",
    last_name = "Musselwhite",
    state_cd = "FL-20",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Musselwhite for Congress",
    campaign_website = "https://musselwhiteforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/gregmusselwhite/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-21 ---------------------------------------------------------------------
  tibble(
    first_name = "Lois",
    last_name = "Frankel",
    state_cd = "FL-21",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://frankel.house.gov",
    campaign_name = "Frankel for Congress",
    campaign_website = "https://loisfrankelforcongress.com/",
    actblue_link = "main-page",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00494856",
    FEC_ID_cand = "H2FL14053"
  ),
  tibble(
    first_name = "Laura",
    last_name = "Loomer",
    state_cd = "FL-21",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Laura Loomer for Congress",
    campaign_website = "https://lauraloomerforcongress.com/",
    actblue_link = NA,
    anedot_link = "laura-loomer-for-congress-inc/highlightgeneral",
    other_link = NA,
    FEC_ID_cmte = "C00714543",
    FEC_ID_cand = "H0FL21078"
  ),
  tibble(
    first_name = "Charleston",
    last_name = "Malkemus",
    state_cd = "FL-21",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Charleston for Congress",
    campaign_website = "https://votecharleston.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00744615",
    FEC_ID_cand = "H0FL21177"
  ),
  # FL-22 ---------------------------------------------------------------------
  tibble(
    first_name = "Ted",
    last_name = "Deutch",
    state_cd = "FL-22",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://teddeutch.house.gov",
    campaign_name = "Ted Deutch for Congress",
    campaign_website = "https://www.tedforcongress.com/",
    actblue_link = "td-home",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Jim",
    last_name = "Pruden",
    state_cd = "FL-22",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jim Pruden for Congress",
    campaign_website = "https://www.jimprudenforcongress.com/",
    actblue_link = NA,
    anedot_link = "jim-pruden-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-23 ---------------------------------------------------------------------
  tibble(
    first_name = "Debbie",
    last_name = "Wasserman Schultz",
    state_cd = "FL-23",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://wassermanschultz.house.gov",
    campaign_name = "Debbie Wasserman Schultz for Congress",
    campaign_website = "https://debbiewassermanschultz.com/",
    actblue_link = "dwsdefault",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Carla",
    last_name = "Spalding",
    state_cd = "FL-23",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Carla Spalding for Congress",
    campaign_website = "https://carlaspaldingforcongress.com/",
    actblue_link = NA,
    anedot_link = "carlaforcongress/direct",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-24 ---------------------------------------------------------------------
  # Wilson vs. Olivo vs. Lehtola
  # Wilson vs Spicer vs Olivo vs Omega-Turner
  tibble(
    first_name = "Frederica",
    last_name = "Wilson",
    state_cd = "FL-24",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://wilson.house.gov",
    campaign_name = "Frederica Wilson for Congress",
    campaign_website = "http://www.fredericawilsonforcongress.com/",
    actblue_link = "fw-may18eom",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00460055",
    FEC_ID_cand = "H0FL17068"
  ),
  tibble(
    first_name = "Christine",
    last_name = "Olivo",
    state_cd = "FL-24",
    # registered as ind later
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Christine for Congress",
    campaign_website = "https://www.christineolivo.com/",
    actblue_link = NA, # "christineforcongress",
    anedot_link = "christine-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00691832",
    FEC_ID_cand = "H0FL23041"
  ),
  tibble(
    first_name = "Sakihah",
    last_name = "Lehtola",
    state_cd = "FL-24",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Sakinah For Congress",
    campaign_website = "http://www.sakinahforcongress.com",
    actblue_link = "sakinah-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00719153",
    FEC_ID_cand = "H0FL24197"
  ),
  tibble(
    first_name = "Lavern",
    last_name = "Spicer",
    state_cd = "FL-24",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lavern Spicer for Congress",
    campaign_website = "https://votelavernspicerforcongress.com/",
    actblue_link = NA,
    anedot_link = "lavern-spicer/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # cannot find omega-turner (Libertarian)
  # FL-25 ---------------------------------------------------------------------
  # Incumbent Diaz-Balart is uncontested.
  tibble(
    first_name = "Mario",
    last_name = "Diaz-Balart",
    state_cd = "FL-25",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://mariodiazbalart.house.gov/",
    campaign_name = "Mario Diaz-Balart for Congress",
    campaign_website = "https://mariodiazbalart.org/",
    actblue_link = NA,
    anedot_link = NA,
    # no suggested amount:
    other_link = "https://mariodiazbalart.org/donate/",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-26 ---------------------------------------------------------------------
  # Mucarsel-Powell vs. Vilarino vs. Blanco
  # Mucarsel-Powell vs. Gimenez
  tibble(
    first_name = "Debbie",
    last_name = "Mucarsel-Powell",
    state_cd = "FL-26",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Debbie for Congress",
    campaign_website = "https://www.debbiemucarselpowell.com",
    actblue_link = "dmpwebsitebutton",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00652065",
    FEC_ID_cand = "H8FL26039"
  ),
  tibble(
    first_name = "Irina",
    last_name = "Vilarino",
    state_cd = "FL-26",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Irina Vilarino for Congress",
    campaign_website = "https://irinavilarino.com",
    actblue_link = NA,
    anedot_link = "irina-vilarino-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00701144",
    FEC_ID_cand = "H0FL26010"
  ),
  tibble(
    first_name = "Omar",
    last_name = "Blanco",
    state_cd = "FL-26",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Blanco for Congress",
    campaign_website = "https://omarblanco.com",
    actblue_link = NA,
    anedot_link = "blanco-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00716225",
    FEC_ID_cand = "H0FL26028"
  ),
  tibble(
    first_name = "Carlos",
    last_name = "Gimenez",
    state_cd = "FL-26",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Carlos Gimenez for Congress",
    campaign_website = "https://carlosgimenezforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/carlos-gimenez-for-congress/donate/",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # FL-27 ---------------------------------------------------------------------
  # Cannot find Molina
  tibble(
    first_name = "Donna",
    last_name = "Shalala",
    state_cd = "FL-27",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://shalala.house.gov",
    campaign_name = "Donna Shalala for Congress",
    campaign_website = "https://donnashalala.com",
    actblue_link = "dshalala20",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00672311",
    FEC_ID_cand = "H8FL27193"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Hepburn",
    state_cd = "FL-27",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hepburn for Congress",
    campaign_website = "https://michaelhepburn.com",
    actblue_link = "the-committee-to-elect-michael-hepburn-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711275",
    FEC_ID_cand = "H8FL27011"
  ),
  tibble(
    first_name = "Maria Elvira",
    last_name = "Salazar",
    state_cd = "FL-27",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Salazar for Congress",
    campaign_website = "https://mariaelvirasalazar.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/salazar/donate",
    FEC_ID_cmte = c("C00714261", "C00671859"),
    FEC_ID_cand = "H8FL27185"
  ),
  # Website down as of Jan 21, 2020
  tibble(
    first_name = "Gabe",
    last_name = "Ferrer",
    state_cd = "FL-27",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elect Gabe Ferrer",
    campaign_website = "http://www.electgabeferrer.com",
    actblue_link = NA,
    anedot_link = NA,
    # 5-10-15-25-50
    other_link = "http://www.electgabeferrer.com",
    FEC_ID_cmte = "C00656447",
    FEC_ID_cand = "H8FL27110"
  ),
  tibble(
    first_name = "Juan",
    last_name = "Fiol",
    state_cd = "FL-27",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Juan Fiol for Congress",
    campaign_website = "https://www.fiolforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.campaigncontributions.net/81870/Donate-to-Juan-Fiol",
    FEC_ID_cmte = "C00715474",
    FEC_ID_cand = "H0FL27026"
  ),
  # GA: Georgia (last updated Jul 11, 2021) ===============================================================
  # GA-01 (last updated Oct 7, 2020) -------------------------------------------
  tibble(
    first_name = "Buddy",
    last_name = "Carter",
    state_cd = "GA-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://buddycarter.house.gov/",
    campaign_name = "Buddy Carter for Congress",
    campaign_website = "https://buddycarterforcongress.com/",
    actblue_link = NA,
    anedot_link = "buddycarter/donate",
    other_link = NA,
    FEC_ID_cmte = "C00543967",
    FEC_ID_cand = "H4GA01039"
  ),
  tibble(
    first_name = "Daniel",
    last_name = "Merritt",
    state_cd = "GA-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Merritt for Congress",
    campaign_website = "https://www.merrittforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # This has not been archived, and is lot, unfortunately
    other_link = "https://donorbox.org/embed/merritt-for-congress",
    FEC_ID_cmte = "C00701177",
    FEC_ID_cand = "H0GA01060"
  ),
  tibble(
    first_name = "Joyce",
    last_name = "Griggs",
    state_cd = "GA-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joyce Griggs for Congress",
    campaign_website = "https://griggsforcongress.com/",
    actblue_link = "joycegriggs2020-wb",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00741397",
    FEC_ID_cand = "H0GA01078"
  ),
  # GA-02 (last updated Oct 7, 2020) -------------------------------------------
  # Bishop Sanford was uncontested (no candidates registered as of Sep 23, 2019)
  # Now Bishop vs. Cole vs. Childs
  tibble(
    first_name = "Sanford",
    last_name = "Bishop",
    state_cd = "GA-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://bishop.house.gov",
    campaign_name = "Sanford Bishop for Congress",
    campaign_website = "https://sanfordbishop.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/-5599655099519138048",
    FEC_ID_cmte = "C00266940",
    FEC_ID_cand = "H2GA02031"
  ),
  tibble(
    first_name = "Donald",
    last_name = "Cole",
    state_cd = "GA-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Don Cole for Congress",
    campaign_website = "https://cole4congress.com/",
    # "http://doncole.com",
    actblue_link = NA,
    anedot_link = "don-cole-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00737692",
    FEC_ID_cand = "H0GA02258"
  ),
  tibble(
    first_name = "Vivian",
    last_name = "Childs",
    state_cd = "GA-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Childs for Congress",
    campaign_website = "https://vivianchilds.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/vivianchilds/donate",
    FEC_ID_cmte = "C00560359",
    FEC_ID_cand = "H4GA02078"
  ),
  # GA-03 (last updated Oct 7, 2020) -------------------------------------------
  # Drew Ferguson was uncontested (no candidates registered as of Sep 23, 2019)
  # Now Ferguson vs. Almonord
  tibble(
    first_name = "Drew",
    last_name = "Ferguson",
    state_cd = "GA-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://ferguson.house.gov",
    campaign_name = "Drew Ferguson for Congress",
    campaign_website =
      "https://fergusonforgeorgia.com/conservative-for-georgia/",
    actblue_link = NA,
    anedot_link = "drew-ferguson-for-congress/general-fund",
    other_link = NA,
    FEC_ID_cmte = "C00607838",
    FEC_ID_cand = "H6GA03113"
  ),
  tibble(
    first_name = "Val", # Valbrun, goes by Val
    last_name = "Almonord",
    state_cd = "GA-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Val Almonord for Congress",
    campaign_website = "https://almonordforcongress.com/",
    actblue_link = "val-almonord-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00743724", "C00741595", "C00741587"),
    FEC_ID_cand = "H0GA03074"
  ),
  # GA-04 ---------------------------------------------------------------------
  tibble(
    first_name = "Hank",
    last_name = "Johnson",
    state_cd = "GA-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://hankjohnson.house.gov/",
    campaign_name = "Committee to Elect Hank Johnson",
    campaign_website = "https://hankforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # NGP VAN link
    other_link = "https://act.myngp.com/Forms/2190422444976703488",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Joshie",
    last_name = "Cruz",
    state_cd = "GA-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joshie Cruz for Congress",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    # paypal donation link
    other_link = paste0(
      "https://www.paypal.com/donate?token=u_58PDl2-x942ZQQv",
      "yEqxZCT8j_2OLQHNx74SGsfdqj6oof80f8tP5IyktL-D54wKchELxlaP25XyFT0"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # GA-05 ---------------------------------------------------------------------
  # John Lewis is uncontested (no candidates registered as of Sep 23, 2019)
  # Following Lewis' death, Williams  vs. Stanton-King
  tibble(
    first_name = "John",
    last_name = "Lewis",
    state_cd = "GA-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://johnlewis.house.gov",
    campaign_name = "John Lewis for Congress",
    campaign_website = "https://www.johnlewisforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://www.johnlewisforcongress.com/donate",
    # Essentially NGP VAN
    other_link = "https://act.myngp.com/Forms/-6622449868794754560",
    FEC_ID_cmte = "C00202416",
    FEC_ID_cand = "H6GA05217"
  ),
  tibble(
    first_name = "Nikema",
    last_name = "Williams",
    state_cd = "GA-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nikema for Congress",
    campaign_website = "https://www.nikemaforcongress.com/",
    actblue_link = "nikemaforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00752584",
    FEC_ID_cand = "H0GA05301"
  ),
  tibble(
    first_name = "Angela",
    last_name = "Stanton King",
    state_cd = "GA-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Stanton King for Congress",
    campaign_website = "https://stantonkingforcongress.com/",
    actblue_link = NA,
    # no amount options in anedot
    anedot_link = "stanton-king-for-congress-inc/donate",
    other_link =
      "https://secure.winred.com/stanton-king-for-congress-inc/donate",
    FEC_ID_cmte = "C00741538",
    FEC_ID_cand = "H0GA05277"
  ),
  # GA-06 ---------------------------------------------------------------------
  # McBath vs. Greene vs. Handel vs. Beach vs. Rodden vs. Harrison
  # Cannot find Harrison website
  # McBath vs Handel in General
  tibble(
    first_name = "Lucy",
    last_name = "McBath",
    state_cd = "GA-06",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://mcbath.house.gov",
    campaign_name = "Lucia McBath for Congress",
    campaign_website = "https://lucyforcongress.com/home/",
    actblue_link = c("support-lucy", "0119twitter", "1019social"),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00672295",
    FEC_ID_cand = "H8GA06393"
  ),
  tibble(
    first_name = "",
    last_name = "",
    state_cd = "GA-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Marjorie",
    last_name = "Greene",
    state_cd = "GA-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marjorie Greene for Congress",
    campaign_website = "https://greene2020.com/",
    actblue_link = NA,
    anedot_link = "greene-for-congress-inc",
    other_link = NA,
    FEC_ID_cmte = "C00708289",
    FEC_ID_cand = "H0GA06192"
  ),
  tibble(
    first_name = "Karen",
    last_name = "Handel",
    state_cd = "GA-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Karen Handel for Congress",
    campaign_website = "https://karenhandel.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(paste0(
      "https://secure.victorypassport.com/pages/handel/main-donate-page?",
      "location=https%3A%2F%2Fkarenhandel.com%2Fdonate&location_url=",
      "aHR0cHM6Ly9rYXJlbmhhbmRlbC5jb20vZG9uYXRlLyM%3D"
    ), "https://secure.winred.com/handel-for-congress/donate"),
    FEC_ID_cmte = "C00633362",
    FEC_ID_cand = "H8GA06286"
  ),
  tibble(
    first_name = "Brandon",
    last_name = "Beach",
    state_cd = "GA-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brandon Beach for Congress",
    campaign_website = "http://beachforcongress.com/",
    actblue_link = NA,
    anedot_link = "beachforcongress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00694265",
    FEC_ID_cand = "H0GA06168"
  ),
  tibble(
    first_name = "Nicole",
    last_name = "Rodden",
    state_cd = "GA-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nicole Rodden for Congress",
    campaign_website = "https://www.nicoleroddenforcongress.com/home",
    actblue_link = NA,
    anedot_link = "nicole-rodden-for-congress/invest",
    other_link = NA,
    FEC_ID_cmte = "C00707612",
    FEC_ID_cand = "H0GA06184"
  ),
  # GA-07 ---------------------------------------------------------------------
  tibble(
    first_name = "Carolyn",
    last_name = "Bourdeaux",
    state_cd = "GA-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Carolyn For Congress",
    campaign_website = "https://www.carolyn4congress.com/",
    actblue_link = c(
      "carolyn_website",
      "cb-popup",
      "absocial19",
      "carolyn_june_eoq_social",
      "carolyn4congressq3"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Rich",
    last_name = "McCormick",
    state_cd = "GA-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Friends of McCormick",
    campaign_website = "https://www.richmccormick.us/",
    actblue_link = NA,
    anedot_link = "friends-of-mccormick/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # GA-08 ---------------------------------------------------------------------
  tibble(
    first_name = "Austin",
    last_name = "Scott",
    state_cd = "GA-08",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://austinscott.house.gov/",
    campaign_name = "Austin Scott for Congress",
    campaign_website = "http://scottforga.com/",
    actblue_link = NA,
    anedot_link = "austin-scott-for-congress/scott-for-ga",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Lindsay 'Doc'",
    last_name = "Holliday",
    state_cd = "GA-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lindsay Doc Holliday Campaign",
    campaign_website = "https://www.voteholliday.com/",
    # no donation links, from website: "I am not soliciting contributions.
    # Somehow, we in America must get the big money out of politics."
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Jimmy",
    last_name = "Cooper",
    state_cd = "GA-08",
    party = "Gre",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jimmy Cooper for Congress",
    campaign_website = "https://jimmycooperforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # only amount box, no options.
    other_link = paste0(
      "https://www.philantro.com/donate/954681362/givealways",
      "?uri=jimmycooperforcongress.com&suggested=&lang=en&pl",
      "d=&afl=&pf=false&s=true"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # GA-09 ---------------------------------------------------------------------
  # Doug Collins is uncontested (no candidates registered as of Sep 23, 2019)
  # Collins running for Senate
  tibble(
    first_name = "Doug",
    last_name = "Collins",
    state_cd = "GA-09",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://dougcollins.house.gov",
    campaign_name = "Collins for Congress",
    campaign_website = "https://www.collinsleads.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Now running for Senate
    other_link = "https://www.collinsleads.com/donate",
    # WinRed Twitter says there was
    # https://secure.winred.com/collins-for-congress/donate
    # But now it gives me a 404 error.
    FEC_ID_cmte = "C00502039",
    FEC_ID_cand = "H2GA09150"
  ),
  tibble(
    first_name = "Andrew",
    last_name = "Clyde",
    state_cd = "GA-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Austin Clyde for Congress",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = "andrew-clyde-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Devin",
    last_name = "Pandy",
    state_cd = "GA-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Devin Pandy for Congress",
    campaign_website = "https://devinpandyforcongress.com/main/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.ngpvan.com/v1/Forms/5-kba88sdkWRLB77FOaWeQ2",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # GA-10 ---------------------------------------------------------------------
  tibble(
    first_name = "Jody",
    last_name = "Hice",
    state_cd = "GA-10",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://hice.house.gov/",
    campaign_name = "Jody Hice for Congress",
    campaign_website = "https://www.jodyhice.com/",
    actblue_link = NA,
    anedot_link = "jody-hice-for-congress/jodyhiceforcongress",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Tabitha",
    last_name = "Johnson-Green",
    state_cd = "GA-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Johnson-Green for Congress",
    campaign_website = "http://www.tabitha2020.com/",
    actblue_link = "tabitha-johnson-green-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # GA-11 ---------------------------------------------------------------------
  tibble(
    first_name = "Barry",
    last_name = "Loudermilk",
    state_cd = "GA-11",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://loudermilk.house.gov/",
    campaign_name = "Loudermilk for Congress",
    campaign_website = "https://loudermilkforcongress.com/",
    actblue_link = NA,
    anedot_link = "loudermilk-for-congress/loudermilk",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Dana",
    last_name = "Barrett",
    state_cd = "GA-11",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dana Barrett for Congress",
    campaign_website = "https://www.electdanabarrett.com/",
    actblue_link = "dana-barrett-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # GA-12 ---------------------------------------------------------------------
  # Rick Allen was uncontested (no candidates registered as of Sep 23, 2019)
  # Now Allen vs. Johnson (statement of candidacy Oct 28, 2019)
  # vs. Steiner (statement of candidacy Oct 28, 2019)
  # Allen vs. Johnson vs. Keller (Ind) for general as well
  tibble(
    first_name = "Rick",
    last_name = "Allen",
    state_cd = "GA-12",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://allen.house.gov/",
    campaign_name = "Rich W. Allen for Congress",
    campaign_website = "https://www.rickwallen.com/",
    # Change documented to WinRed Nov 16, 2019
    # Looks like at least from Nov 15, 2019
    # 5-25-50-100-250-500-1000-2800 to 5-25-50-100-250-500-1000-2800 (same)
    # still has the WinRed site (Oct 6, 2020)
    actblue_link = NA,
    # anedot_link = "rick-allen-for-congress/donate",
    anedot_link = NA,
    other_link = "https://secure.winred.com/RickAllen/donate",
    FEC_ID_cmte = "C00504019",
    FEC_ID_cand = "H2GA12121"
  ),
  tibble(
    first_name = "Liz",
    last_name = "Johnson",
    state_cd = "GA-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Liz Johnson for Congress",
    campaign_website = "https://www.lizjohnsonforcongress.com/",
    actblue_link = "liz-johnson-for-congress-site",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00722991",
    FEC_ID_cand = "H0GA12059"
  ),
  tibble(
    first_name = "Dan",
    last_name = "Steiner",
    state_cd = "GA-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Daniel Steiner for Congress",
    campaign_website = "https://twitter.com/steinercongress",
    # No fundraising link yet
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00724740",
    FEC_ID_cand = "H0GA12067"
  ),
  tibble(
    first_name = "Donald",
    last_name = "Keller",
    state_cd = "GA-12",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Donald Keller for Congress",
    campaign_website = "https://keller4congress.com/",
    # cannot find donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # GA-13 ---------------------------------------------------------------------
  # Scott vs. Hites vs. Cowen
  tibble(
    first_name = "David",
    last_name = "Scott",
    state_cd = "GA-13",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://davidscott.house.gov/",
    campaign_name = "David Scott for Congress",
    campaign_website = "http://votedavidscott.com/",
    actblue_link = "david-scott-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Becky E.",
    last_name = "Hites",
    state_cd = "GA-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Becky E. Hites for Congress",
    campaign_website = "https://hitesforcongress.com/",
    actblue_link = NA,
    anedot_link = "hites-for-congress/hitesforcongress2020",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Martin",
    last_name = "Cowen",
    state_cd = "GA-13",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Martin Cowen for Congress",
    campaign_website = "http://www.cowenforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Paypal link with 5-10-25 amount suggestions
    other_link = paste0(
      "paypal.com/donate?token=0f3DvHu-hhQRnLe6jDtW6lQ_F4fS",
      "ic6i4lZzzM2-IK-jGs2PeHO46C-7-g8tQWwLJLL2BDHBeVFqAuZO"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # GA-14 ----------------------------------------------------------------------
  # Tom Graves was uncontested (no candidates registered as of Sep 23, 2019)
  # Just resigned
  tibble(
    first_name = "Tom",
    last_name = "Graves",
    state_cd = "GA-14",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://tomgraves.house.gov",
    campaign_name = "Graves for Congress",
    campaign_website = "https://gravesforcongress.org/",
    actblue_link = NA,
    anedot_link = "graves-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00462556",
    FEC_ID_cand = "H0GA09030"
  ),
  tibble(
    first_name = "Marjorie",
    last_name = "Greene",
    state_cd = "GA-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marjorie Greene for Congress",
    campaign_website = "https://www.marjorietaylorgreene.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/marjorie-greene-for-congress/donate",
    FEC_ID_cmte = "C00708289",
    FEC_ID_cand = "H0GA06192"
  ),
  # https://www.kevinvanausdal.com gone because withdrew
  tibble(
    first_name = "Kevin",
    last_name = "Van Ausdal",
    state_cd = "GA-14",
    party = "",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.kevinvanausdal.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Lost, but can find in the archives
    # 20-50-100-250-500-1000-2800
    other_link = "https://www.kevinvanausdal.com/donate",
    FEC_ID_cmte = "C00732669",
    FEC_ID_cand = "H0GA14097"
  ),
  tibble(
    first_name = "Jon",
    last_name = "Cowan",
    state_cd = "GA-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John Cowan for Congress",
    campaign_website = "https://cowanforcongress.com",
    actblue_link = NA,
    anedot_link = "john-cowan-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00734517",
    FEC_ID_cand = "H0GA14048"
  ),
  # Many other candidates. Will come back to this.
  # HI: Hawaii (last updated Jul 11, 2021) =====================================
  # HI-01 ---------------------------------------------------------------------
  # Ed Case is uncontested (no candidates registered as of Oct 5, 2019)
  # Case vs Curtis for General
  tibble(
    first_name = "Ed",
    last_name = "Case",
    state_cd = "HI-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://case.house.gov",
    campaign_name = "Ed Case for Congress",
    campaign_website = "https://www.edcase.com/",
    actblue_link = "case-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00680918",
    FEC_ID_cand = "H2HI02128"
  ),
  tibble(
    first_name = "Ron",
    last_name = "Curtis",
    state_cd = "HI-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ron Curtis for Hawaii",
    campaign_website = "https://roncurtis808.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/ron-curtis-for-hawaii/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # HI-02 ---------------------------------------------------------------------
  # Gabbard vs. Kahele vs. Hoomanawanui vs. Cornejo
  # Gabbard currently running for president
  # Kahale vs Aka
  tibble(
    first_name = "Tulsi",
    last_name = "Gabbard",
    state_cd = "HI-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Tulsi for Hawai'i",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00497396",
    FEC_ID_cand = "H2HI02508"
  ),
  tibble(
    first_name = "Kai",
    last_name = "Kahele",
    state_cd = "HI-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kai Kahele for Congress",
    campaign_website = "https://www.kaikahele.com",
    actblue_link = c(
      "kaikahele"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00694604",
    FEC_ID_cand = "H0HI02155"
  ),
  tibble(
    first_name = "Joe",
    last_name = "Akana",
    state_cd = "HI-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joe Akana for Congress",
    campaign_website = "https://www.joeakana.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/akana-for-congress/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Jonathan",
    last_name = "Hoomanawanui",
    state_cd = "HI-02",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jonathan Hoomanawanui for Congress",
    campaign_website = "http://jhoomanawanuicampaign.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://politics.raisethemoney.com/en/jhoomanawanuiUTKpln9NXzSkSq7Dml216g/",
    FEC_ID_cmte = "C00735332",
    FEC_ID_cand = "H0HI02197"
  ),
  tibble(
    first_name = "Ron",
    last_name = "Burrus",
    state_cd = "HI-02",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ron Burrus for Hawai'i",
    campaign_website = "https://www.rbd2hawaii.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0HI02338"
  ),
  # ID: Idaho (last updated Oct 5, 2020) ======================================
  # ID-01 ---------------------------------------------------------------------
  # Russ Fulcher is uncontested (no candidates registered as of Sep 23, 2019)
  # Fulcher vs. Soto vs. Evans
  tibble(
    first_name = "Russ",
    last_name = "Fulcher",
    state_cd = "ID-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://fulcher.house.gov",
    campaign_name = "Russ Fulcher for Idaho",
    campaign_website = "https://russfulcher.com/",
    actblue_link = NA,
    anedot_link = "russfulcher/donate",
    other_link = NA,
    FEC_ID_cmte = "C00648295",
    FEC_ID_cand = "H8ID01124"
  ),
  tibble(
    first_name = "Rudy",
    last_name = "Soto",
    state_cd = "ID-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rudy for Congress",
    campaign_website = "https://rudysoto.us/",
    actblue_link = "mostpeople",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Joe",
    last_name = "Evans",
    state_cd = "ID-01",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joe Evans For Congress",
    campaign_website = "https://idahojoe4congress.org/",
    actblue_link = NA,
    anedot_link = NA,
    # no suggested amounts, fund-raising website
    other_link = "https://fundly.com/joe-evans-for-congress",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # ID-02 ---------------------------------------------------------------------
  # Michael Simpson is uncontested (no candidates registered as of Oct 5, 2019)
  # Simpson vs. Swisher
  tibble(
    first_name = "Michael",
    last_name = "Simpson",
    state_cd = "ID-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://simpson.house.gov",
    campaign_name = "Simpson for Congress",
    campaign_website = "http://www.simpsonforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # added WinRed link
    other_link = c(
      "https://secure.winred.com/MichaelSimpson/donate",
      "https://cm.aristotle.com/d/Simpson"
    ),
    FEC_ID_cmte = "C00331397",
    FEC_ID_cand = "H8ID02064"
  ),
  tibble(
    first_name = "Aaron",
    last_name = "Swisher",
    state_cd = "ID-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Swisher for Idaho",
    campaign_website = "https://swisherforidaho.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.ngpvan.com/v1/Forms/WuiklsKYhk223Z9Pkldcyg2",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # IL: Illinois (last updated Jul 11, 2021) ===================================
  # IL-01 -----------------------------------------
  # Rush vs. White in general
  tibble(
    first_name = "Bobby",
    last_name = "Rush",
    state_cd = "IL-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://rush.house.gov",
    # I cannot find the website!!! What?!
    campaign_name = "Bobby L. Rush For Congress",
    campaign_website = "https://citizens4rush.com",
    actblue_link = c(
      "bobbylrush",
      "bobby-rush-2"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00257121",
    FEC_ID_cand = "H2IL01042"
  ),
  
  tibble(
    first_name = "Philanise",
    last_name = "White",
    state_cd = "IL-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Philanise White for Congress",
    campaign_website = "https://philaniseforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/PhilaniseWhite/donate",
    FEC_ID_cmte = "C00719187",
    FEC_ID_cand = "H0IL01194"
  ),
  # Withdrew
  # IL-02 -----------------------------------------
  # Kelly vs. Raborn vs. Morrow
  # Morrow was disqualified; previously a paypal with no prompt
  # Kelly vs. Raborn General
  tibble(
    first_name = "Robin",
    last_name = "Kelly",
    state_cd = "IL-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://robinkelly.house.gov",
    campaign_name = "Robin Kelly For Congress",
    campaign_website = "http://www.robinkellyforcongress.org/",
    actblue_link = NA,
    anedot_link = NA,
    # NGP link changed
    # NGP CHANGED AGAIN
    other_link = "https://act.myngp.com/Forms/3494079228662908928",
    # "https://act.myngp.com/Forms/7540217854190423040",
    # "https://act.myngp.com/Forms/1226798291258574848",
    FEC_ID_cmte = "C00539866",
    FEC_ID_cand = "H2IL02172"
  ),
  tibble(
    first_name = "Theresa",
    last_name = "Raborn",
    state_cd = "IL-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Raborn for Congress",
    campaign_website = "https://www.rabornforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-1000-2500
    # still same amounts
    other_link = "https://www.rabornforcongress.com/donate",
    FEC_ID_cmte = "C00705335",
    FEC_ID_cand = "H0IL02127"
  ),
  # IL-03 -----------------------------------------
  # Fricilone vs. Newman general
  tibble(
    first_name = "Dan",
    last_name = "Lipinski",
    state_cd = "IL-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://lipinski.house.gov",
    campaign_name = "",
    campaign_website = "http://lipinskiforcongress.com",
    actblue_link = c(
      # Change documented between ActBlue links
      "dl-website-button",
      "danlipinski"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00405431",
    FEC_ID_cand = "H4IL03077"
  ),
  tibble(
    first_name = "Marie",
    last_name = "Newman",
    state_cd = "IL-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marie Newman for Congress",
    campaign_website = "https://www.marienewmanforcongress.com",
    actblue_link = c(
      "marienewmanforcongress",
      "mn-su-rd", # signup page
      # "q1email",
      # "aoc-marie-email-2020306-1", # Ocasio-Cortez
      # "aoc-marie-email-2020315-1", # Ocasio-Cortez
      # "aoc-marie-email-2020316-1", # Ocasio-Cortez
      # "apem-20200312-momentum",    # Pressley
      # "apem-20200318-goal",        # Pressley
      "mn-em-2020",
      "mn-om-2020-rd"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00636670",
    FEC_ID_cand = "H8IL03102"
  ),
  tibble(
    first_name = "Rush",
    last_name = "Darwish",
    state_cd = "IL-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rush for Congress",
    campaign_website = "https://rush4congress.com/meet-rush/",
    actblue_link = "rush4congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00701896",
    FEC_ID_cand = "H0IL03109"
  ),
  # Matthew withdrew from race on Nov 12, 2019
  tibble(
    first_name = "Abe",
    last_name = "Matthew",
    state_cd = "IL-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00700831",
    FEC_ID_cand = "H0IL03091"
  ),
  tibble(
    first_name = "Mike",
    last_name = "Fricilone",
    state_cd = "IL-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Fricilone For Congress",
    # Website http://mikefricilone.com cannot be reached
    # it can be reached now
    # campaign_website = "https://twitter.com/FriciloneMike",
    campaign_website = "https://mikefricilone.com",
    actblue_link = NA,
    anedot_link = "fricilone-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00707679",
    FEC_ID_cand = "H0IL03117"
  ),
  tibble(
    first_name = "Catherine",
    last_name = "O'Shea",
    state_cd = "IL-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "O'Shea for Congress",
    campaign_website =
      "https://www.facebook.com/OShea-for-Congress-100374691472100/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00736645",
    FEC_ID_cand = "H0IL03125"
  ),
  # IL-04 -----------------------------------------
  # Chuy Garcia is uncontested (no candidates registered as of Sep 24, 2019)
  # Garcia vs Solorio Jr. General
  tibble(
    first_name = "Chuy", # Jesus Chuy, goes by Chuy
    last_name = "Garcia",
    state_cd = "IL-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://chuygarcia.house.gov",
    campaign_name = "Chuy for Congress",
    campaign_website = "https://chuyforcongress.com/",
    # "http://jesuschuygarcia.com/",
    actblue_link = c(
      "chuy-garcia-1",
      "jcg_web_new"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00661777",
    FEC_ID_cand = "H8IL04134"
  ),
  tibble(
    first_name = "Jesus",
    last_name = "Solorio",
    state_cd = "IL-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Solorio for Congress",
    campaign_website = "https://www.solorioforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/solorio-for-congress/imjesus",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # IL-05 -----------------------------------------
  # Quigley vs. Burns
  # Hanson vs. Quigley vs. Wilda
  tibble(
    first_name = "Mike",
    last_name = "Quigley",
    state_cd = "IL-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://quigley.house.gov/",
    campaign_name = "Mike Quigley for Congress",
    campaign_website = "https://quigleyforcongress.com/",
    actblue_link = "mq-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00457556",
    FEC_ID_cand = "H0IL05096"
  ),
  tibble(
    first_name = "Brian",
    last_name = "Burns",
    state_cd = "IL-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Burns for Congress",
    campaign_website = "https://www.burns2020.com/",
    actblue_link = c(
      "burnsusa",
      "burnscongress"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00715193", "C00715185"),
    FEC_ID_cand = "H0IL05351"
  ),
  tibble(
    first_name = "Tom",
    last_name = "Hanson",
    state_cd = "IL-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tommy Hanson for Congress",
    campaign_website = "https://www.facebook.com/tommyhansonforcongress/",
    # cannot find any website or donation links
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = "H8IL05073"
  ),
  tibble(
    first_name = "Tom",
    last_name = "Wilda",
    state_cd = "IL-05",
    party = "Gre",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tom Wilda for Congress",
    campaign_website = "https://www.tomforthefifth.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 5-25-50-100-250-1000-2500
    other_link = "https://www.tomforthefifth.com/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # IL-06 -----------------------------------------
  tibble(
    first_name = "Sean",
    last_name = "Casten",
    state_cd = "IL-06",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://casten.house.gov",
    campaign_name = "Casten for Congress",
    campaign_website = "https://castenforcongress.com",
    actblue_link = "castenwebsite2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00648493",
    FEC_ID_cand = "H8IL06139"
  ),
  tibble(
    first_name = "Jeanne",
    last_name = "Ives",
    state_cd = "IL-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jeanne for Congress",
    campaign_website = "https://www.jeanneforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/JeanneIves/donate",
    FEC_ID_cmte = "C00712794",
    FEC_ID_cand = "H0IL06060"
  ),
  tibble(
    first_name = "Bill",
    last_name = "Redpath",
    state_cd = "IL-06",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Redpath for Congress",
    campaign_website = "https://billredpath.com/",
    actblue_link = NA,
    anedot_link = NA,
    # paypal link with no amount suggestions
    other_link =
      paste0(
        "https://www.paypal.com/donate/?token=i4BlcjabN2UCv5n_",
        "kOzixGU6tm2WAuL0lKc4BJzE-I_drHOO6CfViIVPYVA1lr-OXkKCXEUzWEHOfMzO"
      ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Jay", # Gordon J. goes by Jay
    last_name = "Kinzler",
    state_cd = "IL-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kinzler for Congress",
    campaign_website = "https://kinzlerforcongress.org",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-250-500-1000-2800
    other_link = "https://kinzlerforcongress.org/donate/",
    FEC_ID_cmte = "C00721704",
    FEC_ID_cand = "H0IL06078"
  ),
  # Withdrew
  tibble(
    first_name = "Evelyn",
    last_name = "Sanguinetti",
    state_cd = "IL-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Evelyn for Congress",
    campaign_website = "https://www.evelyn2020.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://mystique.victorypassport.com/pages/citizens-for-evelyn-il-06/",
      "website?location=https%3A%2F%2Fgive.victorypassport.com%2F",
      "evelynforcongress%2Fwebsite&location_url=aHR0cHM6Ly9naXZlLnZpY3Rvcnlw",
      "YXNzcG9ydC5jb20vZXZlbHluZm9yY29uZ3Jlc3Mvd2Vic2l0ZS8%2FcmVjdXJyaW5nPXR",
      "ydWUmdXRtX3NvdXJjZT1zaXRlX2hlYWRlciM%3D"
    ),
    FEC_ID_cmte = "C00703488",
    FEC_ID_cand = "H0IL06052"
  ),
  tibble(
    first_name = "Robert",
    last_name = "Marshall",
    state_cd = "IL-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Committee to Elect Robert Marshall",
    campaign_website = "https://www.citizensforrobertmarshall.net",
    actblue_link = NA,
    anedot_link = "committee-to-elect-robert-marshall/donate",
    other_link = NA,
    FEC_ID_cmte = "C00583567",
    FEC_ID_cand = "H0IL08033"
  ),
  # IL-07 -----------------------------------------
  tibble(
    first_name = "Danny",
    last_name = "Davis",
    state_cd = "IL-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://davis.house.gov",
    campaign_name = "Re-Elect Congressman Danny K. Davis",
    # campaign website cannot be reached but the ActBlue link works.
    # (oct 6,2020)
    campaign_website = "https://dkdcongress.com",
    actblue_link = "danny-davis-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00172619",
    FEC_ID_cand = "H4IL07037"
  ),
  tibble(
    first_name = "Kristine",
    last_name = "Schanbacher",
    state_cd = "IL-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kristine Schanbacher for Congress",
    campaign_website = "http://votekristine.com",
    actblue_link = "votekristine",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710491",
    FEC_ID_cand = "H0IL07175"
  ),
  tibble(
    first_name = "Anthony",
    last_name = "Clark",
    state_cd = "IL-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Anthony Clark 2020 for Congress",
    campaign_website = "https://www.voteanthonyclark.com",
    actblue_link = "anthony-clark-2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00696757",
    FEC_ID_cand = "H8IL07103"
  ),
  tibble(
    first_name = "Kina",
    last_name = "Collins",
    state_cd = "IL-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kina Collins for Congress",
    campaign_website = "https://kinaforcongress.com",
    actblue_link = "kinacollins",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00706259",
    FEC_ID_cand = "H0IL07167"
  ),
  tibble(
    first_name = "Tracy",
    last_name = "Jennings",
    state_cd = "IL-07",
    # switched to Independent.
    party = "Ind", # "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tracy Jennings for Congress 2020",
    campaign_website = "https://tracyjenningscongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompt
    # Oct 6 2020, looks like the paypal link changed.
    # There are also Venmo and cashapp links.
    other_link =
      paste0(
        "https://www.paypal.com/donate?token=Si-EM0wls8zUr-Pj",
        "SMu7kmGjC1cgz0MYm7fj9f03Qrm7gIfpcCk_aCjX_GB4IqYzNwuGGwZjIErwapJQ"
      ),
    # paste0(
    # "https://www.paypal.com/donate/",
    # "?token=U5MUu-9pk_Fx246aOH6GdcItrDG23RLOybaXIPViyIhEIJa",
    # "ugZNagMN24ZgeJbWgAVJuDm&country.x=US&locale.x=US"
    # ),
    FEC_ID_cmte = "C00701110",
    FEC_ID_cand = "H0IL07159"
  ),
  tibble(
    first_name = "Craig",
    last_name = "Cameron",
    state_cd = "IL-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cameron for Congress",
    campaign_website = "https://cameron4congress.com/",
    actblue_link = NA,
    anedot_link = "cameron4congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # Craig Cameron website https://cameron4congress.com is empty
  # not anymore (Oct 6, 2020)
  # IL-08 -----------------------------------------
  # Raja Krishnamoorthi was uncontested
  # (no candidates registered as of Sep 24, 2019)
  # Now Krishnamoorthi vs. Hussain
  # Krishnamoorthi vs. Gabriel Nelson (Lib) in general
  tibble(
    first_name = "Raja",
    last_name = "Krishnamoorthi",
    state_cd = "IL-08",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://krishnamoorthi.house.gov",
    campaign_name = "Raja for Congress",
    campaign_website = "https://rajaforcongress.com/",
    # Changed from NGP Van to ActBlue
    actblue_link = "rajaforcongress",
    anedot_link = NA,
    other_link = NA,
    # other_link = "https://secure.ngpvan.com/v1/Forms/wpngNi1Vc0aC_6qmxeBMEg2",
    FEC_ID_cmte = "C00575092",
    FEC_ID_cand = "H6IL08147"
  ),
  tibble(
    first_name = "Inam",
    last_name = "Hussain",
    state_cd = "IL-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dr. Inam for Congress",
    campaign_website = "https://www.drinamforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # No Prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "token=zvxR9EbLvdVXAD38XtftxLT56GyUQFD9xp5TPtQTL",
      "vGpY3_9J5kTBCwicKEe1BgaZXQHGm&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00727412",
    FEC_ID_cand = "H0IL08140"
  ),
  tibble(
    first_name = "Preston",
    last_name = "Nelson",
    state_cd = "IL-08",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Preston Nelson for US Congress",
    campaign_website = "https://vote4nelson.com/",
    actblue_link = NA,
    anedot_link = NA,
    # gofundme kink, no amount suggestions.
    other_link =
      paste0(
        "https://www.gofundme.com/f/vote-4-nelson-preston-nelson-for-",
        "congress?utm_medium=copy_link&utm_source=customer&utm_campaig",
        "n=p_lico+share-sheet+expWdV"
      ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # IL-09 -----------------------------------------
  tibble(
    first_name = "Jan", # Janice, goes by Jan
    last_name = "Schakowsky",
    state_cd = "IL-09",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://schakowsky.house.gov",
    campaign_name = "Jan Schakowsky for Congress",
    campaign_website = "http://www.janschakowsky.org",
    actblue_link = NA,
    anedot_link = NA,
    # 10-50-250-2800-25-100-1000
    # Same amounts as October 6, 2020.
    other_link =
      "https://schakowsky.bsd.net/page/contribute/default?donate_page_KEY=15",
    FEC_ID_cmte = "C00327023",
    FEC_ID_cand = "H8IL09067"
  ),
  tibble(
    first_name = "Andrew",
    last_name = "Heldut", # Tarnasiewict-Heldut but goes by Heldut
    state_cd = "IL-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Heldut for Congress",
    campaign_website = "https://www.andrewheldut.com",
    actblue_link = "heldut2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711283",
    FEC_ID_cand = "H0IL09098"
  ),
  tibble(
    first_name = "Sargis",
    last_name = "Sangari",
    state_cd = "IL-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vote Sangari",
    campaign_website = "https://votesangari.com",
    actblue_link = NA,
    anedot_link = "votesangari/donate",
    other_link = NA,
    FEC_ID_cmte = "C00658450",
    FEC_ID_cand = "H8IL09190"
  ),
  # IL-10 -----------------------------------------
  tibble(
    first_name = "Brad", # Bradley Scott, goes by Brad
    last_name = "Schneider",
    state_cd = "IL-10",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://schneider.house.gov",
    campaign_name = "Brad Schneider for Congress",
    campaign_website = "http://www.schneiderforcongress.com",
    # Interesting amounts...
    actblue_link = "schneider-homepage-donate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00495952",
    FEC_ID_cand = "H2IL10068"
  ),
  tibble(
    first_name = "Val", # Valerie Ramirez, goes by Val
    last_name = "Mukherjee",
    state_cd = "IL-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "VAl for Congress",
    campaign_website = "https://www.valforcongress.com",
    actblue_link = NA,
    anedot_link = "val-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00707539",
    FEC_ID_cand = "H0IL10351"
  ),
  # Disqualified due to lack of signatures
  tibble(
    first_name = "Adam",
    last_name = "Broad",
    state_cd = "IL-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00717975",
    FEC_ID_cand = "H0IL10377"
  ),
  # Wang withdrew
  # IL-11 -----------------------------------------
  tibble(
    first_name = "Bill", # G. William, goes by Bill
    last_name = "Foster",
    state_cd = "IL-11",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://foster.house.gov",
    campaign_name = "Bill Foster for Congress",
    campaign_website = "https://billfoster.com",
    actblue_link = "bfwebsite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00435099",
    FEC_ID_cand = "H8IL14067"
  ),
  tibble(
    first_name = "Krishna",
    last_name = "Bansal",
    state_cd = "IL-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Krishna for Congress",
    campaign_website = "https://www.krishnaforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-250-500-1000-2800-5600
    other_link = "https://www.krishnaforcongress.com/donations/",
    FEC_ID_cmte = "C00721217",
    FEC_ID_cand = "H0IL11128"
  ),
  tibble(
    first_name = "Rachel",
    last_name = "Ventura",
    state_cd = "IL-11",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rachel Ventura For US Congress",
    campaign_website = "Rachel Ventura for Congress",
    actblue_link = "rachelventura",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00712679",
    FEC_ID_cand = "H0IL11102"
  ),
  tibble(
    first_name = "Rick",
    last_name = "Laib",
    state_cd = "IL-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rick Laib 2020",
    campaign_website = "https://www.ricklaib2020.com",
    # Cannot find donation link---yet he is the primary winner!
    # Added winred link - Oct 6, 2020
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/committee-to-elect-rick-laib/donate",
    FEC_ID_cmte = "C00715334",
    FEC_ID_cand = "H0IL11110"
  ),
  # IL-12 -----------------------------------------
  tibble(
    first_name = "Mike", # Michael, goes by Mike
    last_name = "Bost",
    state_cd = "IL-12",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://bost.house.gov",
    campaign_name = "Mike Bost for Congress",
    campaign_website = "https://bostforcongress.com",
    actblue_link = NA,
    anedot_link = "mike-bost-for-congress/mike-bost-for-congress",
    other_link = NA,
    FEC_ID_cmte = "C00546499",
    FEC_ID_cand = "H4IL12060"
  ),
  tibble(
    first_name = "Joel David",
    last_name = "Funk",
    state_cd = "IL-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joel Funk for Congress",
    campaign_website = "https://funkforcongress.com",
    actblue_link = "funk-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00714584",
    FEC_ID_cand = "H0IL12092"
  ),
  tibble(
    first_name = "Ray", # Raymond Carl, goes by Ray
    last_name = "Lenzi",
    state_cd = "IL-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ray Lenzi for Congress",
    campaign_website = "https://lenziforcongress.org",
    actblue_link = "raymond-lenzi-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00725564",
    FEC_ID_cand = "H0IL12100"
  ),
  # IL-13 -----------------------------------------
  # Davis vs. Londrigan vs. Smith
  # Davis vs Londrigan for general
  tibble(
    first_name = "Rodney",
    last_name = "Davis",
    state_cd = "IL-13",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://rodneydavis.house.gov",
    campaign_name = "Rodney for Congress",
    campaign_website = "https://electrodney.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/rodney-for-congress/donate",
    FEC_ID_cmte = "C00521948",
    FEC_ID_cand = "H2IL13120"
  ),
  tibble(
    first_name = "Betsy",
    last_name = "Dirksen Londrigan",
    state_cd = "IL-13",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Betsy Dirksen Londrigan for Congress",
    campaign_website = "https://www.betsydirksenlondrigan.com",
    actblue_link = "londriganweb2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00649483",
    FEC_ID_cand = "H8IL13150"
  ),
  tibble(
    first_name = "Stephanie",
    last_name = "Smith",
    state_cd = "IL-13",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Stefanie Smith for the People",
    campaign_website = "https://www.stefanie2020.com",
    actblue_link = "stefanie2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00723619",
    FEC_ID_cand = "H0IL13066"
  ),
  # IL-14 -----------------------------------------
  # Underwood vs. Oberweis in general
  # vs. Gradel vs. Rezin vs. Lauf vs. Marter vs. Evans vs. Quigley vs. Catella
  tibble(
    first_name = "Lauren",
    last_name = "Underwood",
    state_cd = "IL-14",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.underwoodforcongress.com",
    # Main is NGP VAN right now at 25-50-75-100-250-500
    # Change documented to ActBlue on Feb 29, 2020
    # from 25-50-75-100-250-500 to 25-100-250-500-1000-2800-5600
    # Currently the latter of the ActBlue links with:
    # 10-25-50-75-100-250-500 on Oct 6, 2020
    actblue_link = c(
      "lauren-underwood-for-congress-1",
      "lufc_web_october2020"
    ),
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/1159474659983231744",
    FEC_ID_cmte = "C00652719",
    FEC_ID_cand = "H8IL14174"
  ),
  tibble(
    first_name = "Jim",
    last_name = "Oberweis",
    state_cd = "IL-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jim Oberweis for Congress",
    campaign_website = "https://jim2020.com",
    actblue_link = NA,
    anedot_link = NA,
    # Not sure what format this is...
    # 35-50-100-250-500-1000-2800-5600
    # still includes the same amount
    other_link = "https://jim2020.com/donate/",
    FEC_ID_cmte = "C00696872",
    FEC_ID_cand = "H8IL14083"
  ),
  tibble(
    first_name = "Ted",
    last_name = "Gradel",
    state_cd = "IL-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gradel for Congress",
    campaign_website = "https://tedgradel.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://transaxt.com/Donate/US6RX6/TedGradelforCongress/",
    FEC_ID_cmte = "C00703736",
    FEC_ID_cand = "H0IL14163"
  ),
  tibble(
    first_name = "Sue",
    last_name = "Rezin",
    state_cd = "IL-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rezin for Congress",
    campaign_website = "https://rezinforcongress.com",
    actblue_link = NA,
    anedot_link = "rezin-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00711580",
    FEC_ID_cand = "H0IL14189"
  ),
  tibble(
    first_name = "Catalina",
    last_name = "Lauf",
    state_cd = "IL-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Catalina Lauf for Congress",
    campaign_website = "https://www.catalinaforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/catalina-lauf-for-congress-committee/donate",
    FEC_ID_cmte = "C00716597",
    FEC_ID_cand = "H0IL14197"
  ),
  tibble(
    first_name = "James",
    last_name = "Marter",
    state_cd = "IL-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marter for Congress",
    campaign_website = "https://marter4congress.us",
    actblue_link = NA,
    anedot_link = "marter-for-congress/main",
    other_link = NA,
    FEC_ID_cmte = "C00657361",
    FEC_ID_cand = "H8IL16153"
  ),
  tibble(
    first_name = "Jerry",
    last_name = "Evans",
    state_cd = "IL-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jerry Evans for Congress",
    campaign_website = "https://www.jerryforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/evans-for-congress/donate",
    FEC_ID_cmte = "C00723767",
    FEC_ID_cand = "H0IL14213"
  ),
  tibble(
    first_name = "Matt",
    last_name = "Quigley",
    state_cd = "IL-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Matt Quigley For Congress",
    campaign_website = "https://mattquigleyforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "token=6X8yWclTcd4KgS-4-HrFHOMkHyaCS0ZJofLnKSXmGY3n3yNUqN07B7",
      "MIJ2KOQIaYCwBfsm&country.x=US&locale.x="
    ),
    FEC_ID_cmte = "C00693556",
    FEC_ID_cand = "H0IL14148"
  ),
  tibble(
    first_name = "Anthony",
    last_name = "Catella",
    state_cd = "IL-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Catella for Congress",
    campaign_website = "https://catellaforcongress.com",
    # No donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00720458",
    FEC_ID_cand = "H0IL14205"
  ),
  # IL-15 -----------------------------------------
  # John Shimkus announced retirement on Nov 4, 2019
  # Miller vs. Duncan
  # vs. Ellington vs. Wolff vs. Gaither vs. Hursey
  # vs. Walker vs. Fuller vs. Bambenek vs. Gary
  tibble(
    first_name = "Mary",
    last_name = "Miller",
    state_cd = "IL-15",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mary Miller for Congress",
    campaign_website = "https://www.marymillerforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/mary-miller-for-congress/donate",
    FEC_ID_cmte = "C00723916",
    FEC_ID_cand = "H0IL15129"
  ),
  tibble(
    first_name = "Darren",
    last_name = "Duncan",
    state_cd = "IL-15",
    party = "",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Darren Duncan for Congress",
    campaign_website = "https://www.duncanforillinois.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      # Listed as https://winred.revv.co/darren-duncan-for-congress/commonsense
      # But essentially the same page
      "https://secure.winred.com/darren-duncan-for-congress/commonsense",
    FEC_ID_cmte = "C00729210",
    FEC_ID_cand = "H0IL15186"
  ),
  tibble(
    first_name = "Chuck", # Charles Richard, goes by Chuck
    last_name = "Ellington",
    state_cd = "IL-15",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dr. Chuck Ellington for Congress",
    campaign_website = "https://drchuckellingtonforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Paypal prompts for 25-50-100
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "token=49EsLeA53XAbgjXY9-jxANO7YMNPSNEMQekTdEKd9dy-HbQb8F",
      "CPsy5CurH8G4PbbuQKK0&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00730101",
    FEC_ID_cand = "H0IL15202"
  ),
  tibble(
    first_name = "Kerry", # Kerry Allen
    last_name = "Wolff",
    state_cd = "IL-15",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wolff for Congress",
    campaign_website = "https://kerrywolff.com",
    actblue_link = NA,
    anedot_link = "wolff-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00725028",
    FEC_ID_cand = "H0IL15145"
  ),
  tibble(
    first_name = "Kevin",
    last_name = "Gaither",
    state_cd = "IL-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kevin Gaither for IL-15 Congressional District",
    campaign_website = "https://gaither4il.com",
    actblue_link = "gaither4il",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00648675",
    FEC_ID_cand = "H8IL15072"
  ),
  tibble(
    first_name = "John",
    last_name = "Hursey",
    state_cd = "IL-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "The John W. Hursey Jr Campaign for Congress",
    campaign_website = "https://johnhurseyforcongress.com",
    actblue_link = "the-john-w--hursey-jr-campaign-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00712158",
    FEC_ID_cand = "H0IL15095"
  ),
  tibble(
    first_name = "Erika",
    last_name = "Weaver",
    state_cd = "IL-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Friends of Erica Weaver",
    campaign_website = "https://www.weaverforillinois.com/",
    actblue_link = "weaver-for-illinois-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # Alex Walker now running for State Rep 110th District
  # Lori Fuller withdrew
  # John Christian Bambenek withdrew
  # Kent Gray was disqualified
  # IL-16 -----------------------------------------
  # Kinzinger vs. Brzozowski
  tibble(
    first_name = "Adam",
    last_name = "Kinzinger",
    state_cd = "IL-16",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://kinzinger.house.gov",
    campaign_name = "Kinzinger for Congress",
    campaign_website = "https://www.electadam.com",
    actblue_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # at least form Jan 22, 2020
    # from 50-100-250-5000-1000-2800 to 25-50-100-250-500-1000-2800
    # anedot_link = "kinzinger-for-congress/adam-kinzinger-congress",
    # still has the same winRed link - Oct 6, 2020.
    anedot_link = NA,
    other_link = "https://secure.winred.com/adamkinzinger/donate",
    FEC_ID_cmte = "C00458877",
    FEC_ID_cand = "H0IL11052"
  ),
  tibble(
    first_name = "Dani",
    last_name = "Brzozowski",
    state_cd = "IL-16",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dani for Congress",
    campaign_website = "https://www.daniforillinois.com",
    actblue_link = c(
      "daniforillinois", # Oct 06, 2020
      "daniforillinoiswebsitepopup"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00718742",
    FEC_ID_cand = "H0IL16184"
  ),
  # IL-17 -----------------------------------------
  tibble(
    first_name = "Cheri",
    last_name = "Bustos",
    state_cd = "IL-17",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://bustos.house.gov",
    campaign_name = "Cheri Bustos for Congress",
    campaign_website = "https://www.cheribustos.com",
    actblue_link = "bustoshome",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00498568",
    FEC_ID_cand = "H2IL17071"
  ),
  tibble(
    first_name = "Esther Joy",
    last_name = "King",
    state_cd = "IL-17",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Esther for Congress",
    # updated campaign website on Oct 07, 2020
    campaign_website = "https://www.estherforcongress.com/",
    # "http://www.allynpolitical.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/estherforcongress/donate",
    FEC_ID_cmte = "C00716498",
    FEC_ID_cand = "H0IL17083"
  ),
  tibble(
    first_name = "Bill",
    last_name = "Fawell",
    state_cd = "IL-17",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bill Fawell for Congress",
    campaign_website = "http://www.electfawell.org/",
    # Cannot find donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00675728",
    FEC_ID_cand = "H4IL17077"
  ),
  # Swisher website is down
  # Farrell withdrew
  tibble(
    first_name = "Spanky",
    last_name = "Edwards",
    state_cd = "IL-17",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Spanky Edwards for Congress",
    campaign_website = "https://www.facebook.com/SpankyForCongress/",
    actblue_link = "spankyfedwards",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00725002",
    FEC_ID_cand = "H0IL17125"
  ),
  # IL-18 -----------------------------------------
  # Darin LaHood was uncontested (no candidates registered as of Sep 24, 2019)
  # Now LaHood vs. Haasis
  # Haasis withdrew on Dec 20, 2019; LaHood effectively uncontested
  # LaHood vs. Petrilli in general
  tibble(
    first_name = "Darin",
    last_name = "LaHood",
    state_cd = "IL-18",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://lahood.house.gov",
    campaign_name = "LaHood for Congress",
    campaign_website = "https://www.lahoodforcongress.com/",
    actblue_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from Dec 17, 2019
    # from 5-10-25-50-100-500-1000-2800
    #   to 5-10-25-50-100-500-1000-2800 (same amounts)
    # As of Oct 07, 2020 same abounts. with same WinRed link
    anedot_link = "lfc/donate",
    other_link = "https://secure.winred.com/DarinLaHood/donate",
    FEC_ID_cmte = "C00575050",
    FEC_ID_cand = "H6IL18088"
  ),
  tibble(
    first_name = "George",
    last_name = "Petrilli",
    state_cd = "IL-18",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Petrilli for Congress",
    campaign_website = "https://www.petrilliforcongress.com/",
    # it says "ActBlue - coming soon" on website
    # but only this paypal link with no suggestions works now.
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      paste0(
        "https://www.paypal.com/donate/?cmd=_donations&business=petril",
        "liforcongress@gmail.com&item_name=Petrilli%20for%20Congress&currency",
        "_code=USD&Z3JncnB0="
      ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # IN: Indiana Updated Jul 11, 2021 ===============================================================
  # IN-01 ---------------------------------------------------------------------
  # Pete Visclosky is uncontested (no candidates registered as of Sep 24, '19)
  # Visclosky retired, Mrvan vs Levya in General
  tibble(
    first_name = "Pete",
    last_name = "Visclosky",
    state_cd = "IN-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://visclosky.house.gov",
    campaign_name = "Visclosky for Congress",
    campaign_website = "https://www.viscloskyforcongress.us/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://www.viscloskyforcongress.us/contribute",
    # In reality NGP VAN
    other_link = "https://act.myngp.com/Forms/6437597269052164096",
    FEC_ID_cmte = "C00166504",
    FEC_ID_cand = "H4IN01012"
  ),
  tibble(
    first_name = "Frank",
    last_name = "Mrvan",
    state_cd = "IN-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Frank Marvan for Congress",
    campaign_website = "https://mrvanforcongress.com/",
    actblue_link = "veracitysite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Mark",
    last_name = "Leyva",
    state_cd = "IN-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Leyva for Congress",
    campaign_website = "https://markleyva.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/mark-leyva-for-congress/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # IN-02 ---------------------------------------------------------------------
  tibble(
    first_name = "Jackie",
    last_name = "Walorski",
    state_cd = "IN-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://walorski.house.gov/",
    campaign_name = "Walorski for Congress",
    campaign_website = "https://www.standwithjackie.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-1000-2800-5600
    other_link = "https://secure.standwithjackie.com/contribute/",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Pat",
    last_name = "Hackett",
    state_cd = "IN-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Pat Hackett for Congress",
    campaign_website = "https://pathackettforcongress.com/",
    actblue_link = "hackettwebsite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # IN-03 ---------------------------------------------------------------------
  tibble(
    first_name = "Jim",
    last_name = "Banks",
    state_cd = "IN-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://banks.house.gov/",
    campaign_name = "Jim Banks for Congress",
    campaign_website = "https://jimbanks.us/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/jim-banks-for-congress-inc/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Chip",
    last_name = "Coldiron",
    state_cd = "IN-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chip Coldiron for Congress",
    campaign_website = "https://www.coldiron4congress.com/",
    actblue_link = "coldiron4congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # IN-04 ---------------------------------------------------------------------
  # Jim Baird is uncontested (no candidates registered as of Sep 24, 2019)
  tibble(
    first_name = "Jim",
    last_name = "Baird",
    state_cd = "IN-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://baird.house.gov",
    campaign_name = "Jim Baird for Congress",
    campaign_website = "http://www.electjimbaird.com/",
    actblue_link = NA,
    anedot_link = NA,
    # still this victorypassport link
    other_link = paste0(
      "https://mystique.victorypassport.com/pages/baird/baird-donations?",
      "location=https%3A%2F%2Fgive.victorypassport.com%2Fbaird%2Fdonate&",
      "location_url=aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5jb20vYmFpcmQv",
      "ZG9uYXRlLyM%3D"
    ),
    FEC_ID_cmte = "C00662940",
    FEC_ID_cand = "H8IN04199"
  ),
  tibble(
    first_name = "Joe",
    last_name = "Mackey",
    state_cd = "IN-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joe 4 Hoosiers",
    campaign_website = "https://www.joe4hoosiers.com/",
    # interesting amounts
    actblue_link = "joe4hoosiers",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # IN-05 ------------------------------------------
  # Susan Brooks retired as of Jun 14, 2019; open seat
  tibble(
    first_name = "Christina",
    last_name = "Hale",
    state_cd = "IN-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Christina Hale for Congress",
    campaign_website = "https://haleforcongress.com",
    actblue_link = "haleweb2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711887",
    FEC_ID_cand = "H0IN05219"
  ),
  tibble(
    first_name = "Ken",
    last_name = "Tucker",
    state_cd = "IN-05",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "A Guy from Indiana",
    campaign_website = "https://aguyfromindiana.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0IN05342"
  ),
  tibble(
    first_name = "Chuck",
    last_name = "Dietzen",
    state_cd = "IN-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chuck Dietzen for Congress",
    campaign_website = "https://www.drchuckforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/chuck-dietzen-for-congress/donate",
    FEC_ID_cmte = "C00721571",
    FEC_ID_cand = "H0IN05250"
  ),
  tibble(
    first_name = "Beth",
    last_name = "Henderson",
    state_cd = "IN-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Beth for Indiana",
    campaign_website = "https://www.bethforindiana.com",
    actblue_link = NA,
    anedot_link = "beth-henderson-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00722439",
    FEC_ID_cand = "H0IN05276"
  ),
  tibble(
    first_name = "Kelly",
    last_name = "Mitchell",
    state_cd = "IN-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kelly Mitchell for Congress",
    campaign_website = "https://www.hoosiersforkelly.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/KellyMitchell/donate",
    FEC_ID_cmte = "C00717330",
    FEC_ID_cand = "H0IN05243"
  ),
  tibble(
    first_name = "Micah",
    last_name = "Beckwith",
    state_cd = "IN-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Beckwith for Congress",
    campaign_website = "https://www.beckwith4congress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "ttps://politics.raisethemoney.com/beckwith-for-congress?iframe=true",
    FEC_ID_cmte = "C00709881",
    FEC_ID_cand = "H0IN05201"
  ),
  tibble(
    first_name = "Dee", # Dierdre, goes by Dee
    last_name = "Thornton",
    state_cd = "IN-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Thornton for Congress",
    campaign_website = "http://www.deethorntonforcongress.com",
    actblue_link = "deethornton",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00664797",
    FEC_ID_cand = "H8IN05188"
  ),
  tibble(
    first_name = "Kent",
    last_name = "Abernathy",
    state_cd = "IN-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kent Abernathy for Congress",
    campaign_website = "https://kentabernathy.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/kent-abernathy-for-congress/donate",
    FEC_ID_cmte = "C00721985",
    FEC_ID_cand = "H0IN05268"
  ),
  # # Steve Braun withdrew
  tibble(
    first_name = "Jennifer",
    last_name = "Christie",
    state_cd = "IN-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jennifer for Indiana",
    campaign_website = "https://jenniferforindiana.com",
    actblue_link = "jenniferforindiana5th",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00666891",
    FEC_ID_cand = "H8IN05196"
  ),
  tibble(
    first_name = "Matthew",
    last_name = "Hook",
    state_cd = "IN-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.votethefuturenow.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link =
      "https://donorbox.org/embed/friends-of-matthew-hook-and-the-future",
    FEC_ID_cmte = "C00731232",
    FEC_ID_cand = "H0IN05318"
  ),
  tibble(
    first_name = "Andy", # Andrew James, goes by Andy
    last_name = "Jacobs",
    state_cd = "IN-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Andy Jacobs for Congress",
    campaign_website = "https://www.andyjacobsforcongress.com",
    actblue_link = "andyjacobsforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00714147",
    FEC_ID_cand = "H0IN05227"
  ),
  tibble(
    first_name = "Andrew",
    last_name = "Bales",
    state_cd = "IN-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Andrew Bales for Congress",
    campaign_website = "https://www.balesforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-1000-2500-other
    other_link = "https://www.balesforcongress.com/donate",
    FEC_ID_cmte = "C00725382",
    FEC_ID_cand = "H0IN05284"
  ),
  # Elle Kizik is a write-in campaign so can't find her
  tibble(
    first_name = "Victoria",
    last_name = "Spartz",
    state_cd = "IN-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Victoria Spartz for Congress",
    campaign_website = "https://www.spartzforcongress.com",
    actblue_link = NA,
    anedot_link = c(
      "victoria-spartz-for-congress/c9bf30bfad4acee52d688",
      "victoria-victory-fund/donate"
    ), # Oct 07, 2020
    other_link = NA,
    FEC_ID_cmte = "C00737767",
    FEC_ID_cand = "H0IN05326"
  ),
  tibble(
    first_name = "Carl",
    last_name = "Brizzi",
    state_cd = "IN-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vote Brizzi",
    campaign_website = "https://www.votebrizzi.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/vote-brizzi/donate",
    FEC_ID_cmte = "C00738195",
    FEC_ID_cand = "H0IN05334"
  ),
  tibble(
    first_name = "Allen",
    last_name = "Davidson",
    state_cd = "IN-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Allen R. Davidson for Congress",
    campaign_website = "https://www.ard4in05.com",
    # No online donation links
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00734509",
    FEC_ID_cand = "H4IN05112"
  ),
  # IN-06 ---------------------------------------------------------------------
  # Greg Pence is uncontested (no candidates registered as of Sep 24, 2019)
  # Pence vs Lake in general
  tibble(
    first_name = "Greg",
    last_name = "Pence",
    state_cd = "IN-06",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://pence.house.gov",
    campaign_name = "Greg Pence for Congress",
    campaign_website = "https://gregpenceforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from
    # from 25-50-100-250-500-1000-2800-5600
    #   to 25-50-100-250-500-1000-2800-5600 (I think it's the same website.)
    # other_link = "https://secure.gregpencevictory.com/donate/",
    # as of Oct 07, website leads to the other_link
    other_link = "https://secure.winred.com/gregpence/donate/",
    FEC_ID_cmte = "C00658401",
    FEC_ID_cand = "H8IN06129"
  ),
  tibble(
    first_name = "Jeannine Lee",
    last_name = "Lake",
    state_cd = "IN-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lake For Congress",
    campaign_website = "https://jeannineleelakeforcongress.com/",
    actblue_link = "jeannine-lake-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # IN-07 ---------------------------------------------------------------------
  tibble(
    first_name = "Andre",
    last_name = "Carson",
    state_cd = "IN-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://carson.house.gov",
    campaign_name = "Andre Carson for Congress",
    campaign_website = "https://andrecarson.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.ngpvan.com/v1/Forms/ZFgQB1NB_027aRtM8Xj6LQ2",
    FEC_ID_cmte = "C00658401",
    FEC_ID_cand = "H8IN06129"
  ),
  tibble(
    first_name = "Susan Marie",
    last_name = "Smith",
    state_cd = "IN-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Friends of Susan Marie Smith",
    campaign_website = "https://susanmariesmith.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/friends-of-susan-marie-smith/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # IN-08 ---------------------------------------------------------------------
  # Larry Bucshon is uncontested (no candidates registered as of Sep 24, 2019)
  tibble(
    first_name = "Larry",
    last_name = "Bucshon",
    state_cd = "IN-08",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://bucshon.house.gov",
    campaign_name = "Bucshon for Congress",
    campaign_website = "https://www.bucshonforcongress.com/",
    # Change documented to WinRed on Oct 16, 2019
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/bucshon-for-congress/donate",
    FEC_ID_cmte = "C00468256",
    FEC_ID_cand = "H0IN08114"
  ),
  tibble(
    first_name = "Thomasina",
    last_name = "Marsili",
    state_cd = "IN-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Thomasina for Congress",
    campaign_website = "https://www.hopein8.com/",
    actblue_link = "hope-in-8-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00724310",
    FEC_ID_cand = "H0IN08213"
  ),
  tibble(
    first_name = "James",
    last_name = "Rodenberger",
    state_cd = "IN-08",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "James Rodenberger for Congress",
    campaign_website = "https://jamesrodenberger.com/index.html",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0IN08221"
  ),
  # IN-09 ---------------------------------------------------------------------
  tibble(
    first_name = "Trey",
    last_name = "Hollingsworth",
    state_cd = "IN-09",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://hollingsworth.house.gov",
    campaign_name = "Trey Hollingsworth for Congress",
    campaign_website = "http://www.votefortrey.com/",
    actblue_link = NA,
    anedot_link = "treyhollingsworth/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Andy",
    last_name = "Ruff",
    state_cd = "IN-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Andy Ruff for Congress",
    campaign_website = "https://www.andyruff.org/",
    actblue_link = "andyruff",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # IA: Iowa (last updated Oct 8, 2020) =======================================
  # IA-01 ---------------------------------------------------------------------
  # Finkenauer vs. Hinson vs. Hansen vs. White
  # Cannot find Hansen nor White
  # Finkenauer vs Hinson general
  tibble(
    first_name = "Abby",
    last_name = "Finkenauer",
    state_cd = "IA-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://finkenauer.house.gov",
    campaign_name = "Finkenauer for Congress",
    campaign_website = "https://www.abbyfinkenauer.com",
    actblue_link = c("abby-finkenauer-2018", "AF-website-link"),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00637074",
    FEC_ID_cand = "H8IA01094"
  ),
  tibble(
    first_name = "Ashley",
    last_name = "Hinson",
    state_cd = "IA-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ashley Hinson for Congress",
    campaign_website = "https://ashleyhinson.com",
    actblue_link = NA,
    anedot_link = "ashley-hinson-for-congress/donate",
    other_link = "https://secure.winred.com/ashleyhinson/donate",
    FEC_ID_cmte = "C00706267",
    FEC_ID_cand = "H0IA01174"
  ),
  # IA-02 ------------------------------------------
  # No incumbents: open seat.
  # Hart vs. Miller-Meeks
  # vs. Schilling vs. Everly vs. Phillips vs. Tessler vs. Kedley
  # --- Tessler withdrew on May  3, 2019
  # --- Kedley  withdrew on Jun 22, 2019
  # Hart vs. Miller-Meeks in General elections.
  tibble(
    first_name = "Rita",
    last_name = "Hart",
    state_cd = "IA-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rita Hart for Congress",
    campaign_website = "https://www.ritahart.com",
    # second link is what is in website now (oct 8, 2020)
    actblue_link = c("website-rh", "rita-hart-for-iowa-1"),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00706457",
    FEC_ID_cand = "H0IA02156"
  ),
  tibble(
    first_name = "Mariannette",
    last_name = "Miller-Meeks",
    state_cd = "IA-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "http://millermeeks2020.com",
    actblue_link = NA,
    # New format: need work. 10-25-50-100-250-1000-2800
    # other_link = "http://millermeeks2020.com/donate/",
    # Change documented to Anedot on Mar 1, 2020
    # to 5-25-50-100-250-500-1000-2800
    # same link with
    anedot_link = "miller-meeks-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00558825",
    FEC_ID_cand = "H0IA02180"
  ),
  tibble(
    first_name = "Bobby",
    last_name = "Schilling",
    state_cd = "IA-02", # Formerly IL
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bobby for Iowa",
    campaign_website = "https://bobbyforiowa.com",
    actblue_link = NA,
    anedot_link = "bobby-for-iowa/donate",
    other_link = NA,
    FEC_ID_cmte = "C00711457",
    FEC_ID_cand = "H0IA02164"
  ),
  tibble(
    first_name = "Steven",
    last_name = "Everly",
    state_cd = "IA-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Everly for Iowa",
    campaign_website = "https://www.everlyforiowa.com/about",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.everlyforiowa.com/donate", # NationBuilder
    FEC_ID_cmte = "C00712315",
    FEC_ID_cand = "H0IA02206"
  ),
  tibble(
    first_name = "Rick", # Ricky Lee, goes by Rick
    last_name = "Phillips",
    state_cd = "IA-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rick Phillips for U.S. House of Representatives IA-02",
    campaign_website = "https://theswampfox.net",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://electrickphillips.nationbuilder.com/",
      "donate_to_rick_phillips_campaign"
    ),
    FEC_ID_cmte = "C00726752",
    FEC_ID_cand = "H0IA02198"
  ),
  # Tessler withdrew on May 3, 2019
  # Kedley withdrew on Jun 24, 2019
  # IA-03 ---------------------------------------------------------------------
  # Cannot find Schoenblatt nor Huss
  # Axne vs. Young general
  tibble(
    first_name = "Cindy",
    last_name = "Axne",
    state_cd = "IA-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://axne.house.gov",
    campaign_name = "Cindy Axne for Congress",
    campaign_website = "https://cindyaxneforcongress.com",
    actblue_link = "cindyaxneforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00646844",
    FEC_ID_cand = "H8IA03124"
  ),
  tibble(
    first_name = "David",
    last_name = "Young",
    state_cd = "IA-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Young for Iowa",
    campaign_website = "https://youngforiowa.com",
    actblue_link = NA,
    anedot_link = NA,
    # What was Young using before WinRed? Shame I won't find out...
    other_link = "https://secure.winred.com/davidyoung/donate",
    FEC_ID_cmte = "C00545616",
    FEC_ID_cand = "H4IA03115"
  ),
  tibble(
    first_name = "Bill",
    last_name = "Schafer",
    state_cd = "IA-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Schafer for Congress",
    campaign_website = "https://schaferforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://mystique.victorypassport.com/pages/schafer/",
      "schafer-for-congress-2?location=https%3A%2F%2Fgive.victorypassport.com",
      "%2Fschafer%2Fjoin-our-team&location_url=aHR0cHM6Ly9naXZlLnZpY3RvcnlwYX",
      "NzcG9ydC5jb20vc2NoYWZlci9qb2luLW91ci10ZWFtLyM%3D"
    ),
    FEC_ID_cmte = "C00709014",
    FEC_ID_cand = "H0IA03204"
  ),
  tibble(
    first_name = "",
    last_name = "",
    state_cd = "IA-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "",
    last_name = "",
    state_cd = "IA-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # IA-04 ---------------------------------------------------------------------
  # King vs. Feenstra vs. Richards vs. Taylor vs. Scholten vs. Krull vs. Reeder
  # Cannot find Krull
  # Feenstra vs Scholten General
  tibble(
    first_name = "Steve",
    last_name = "King",
    state_cd = "IA-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://steveking.house.gov",
    campaign_name = "",
    campaign_website = "https://www.steveking.com",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Jun 3, 2020
    other_link =
      "https://secure.winred.com/king-for-congress/donate",
    #   "https://donorbox.org/embed/support-king-for-congress?wmode=opaque",
    FEC_ID_cmte = "C00373563",
    FEC_ID_cand = "H2IA05072"
  ),
  tibble(
    first_name = "Randall",
    last_name = "Feenstra",
    state_cd = "IA-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Feenstra for Congress",
    campaign_website = "https://www.feenstraforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed Jun 3, 2020
    other_link = # paste0(
      "https://secure.winred.com/feenstraforcongress/donate",
    # "https://secure.victorypassport.com/pages/feenstra/donations-iframe?",
    # "location=https%3A%2F%2Fwww.feenstraforcongress.com%2Fdonate&location",
    # "_url=aHR0cHM6Ly93d3cuZmVlbnN0cmFmb3Jjb25ncmVzcy5jb20vZG9uYXRlLyM%3D"
    # ),
    FEC_ID_cmte = "C00693663",
    FEC_ID_cand = "H0IA04145"
  ),
  tibble(
    first_name = "Bret",
    last_name = "Richards",
    state_cd = "IA-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Richards for Congress",
    campaign_website = "https://richardsforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/embed/richardsforcongress?a=b",
    FEC_ID_cmte = "C00694208",
    FEC_ID_cand = "H0IA04129"
  ),
  tibble(
    first_name = "Jeremy",
    last_name = "Taylor",
    state_cd = "IA-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Taylor for Congress",
    campaign_website = "https://www.jeremytaylorforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed Jun 3, 2020
    other_link = "https://secure.winred.com/JeremyTaylor/donate",
    # "https://jtaylor.ichooseapex.com/donate/",
    FEC_ID_cmte = "C00694687",
    FEC_ID_cand = "H0IA04137"
  ),
  tibble(
    first_name = "J.D.",
    last_name = "Scholten",
    state_cd = "IA-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "J.D. Scholten For Congress",
    campaign_website = "https://www.scholten4iowa.com",
    actblue_link = "jdslaunch",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00650622",
    FEC_ID_cand = "H8IA04106"
  ),
  tibble(
    first_name = "Steve",
    last_name = "Reeder",
    state_cd = "IA-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Reeder for Congress",
    campaign_website = "https://reederforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/reeder-for-congress/donate",
    FEC_ID_cmte = "C00720607",
    FEC_ID_cand = "H0IA04152"
  ),
  # KS: Kansas (last updated Jul 11, 2021) =====================================
  # KS-01 ---------------------------------------------------------------------
  # Can't find Soetaert
  # Mann vs Barnett general
  tibble(
    first_name = "Tracey",
    last_name = "Mann",
    state_cd = "KS-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tracey Mann for Congress",
    campaign_website = "https://www.traceymann.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/traceymannforcongress/donate",
    FEC_ID_cmte = "C00460659",
    FEC_ID_cand = "H0KS01123"
  ),
  tibble(
    first_name = "Kali",
    last_name = "Barnett",
    state_cd = "KS-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kali Barnett for Kansas",
    campaign_website = "https://kaliforkansas.com/",
    actblue_link = "blu_kali_websitehomepage",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00714535",
    FEC_ID_cand = "H0KS01172"
  ),
  tibble(
    first_name = "Bill",
    last_name = "Clifford",
    state_cd = "KS-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dr. Bill Clifford for Congress",
    campaign_website = "https://www.cliffordforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/billclifford/donate",
    FEC_ID_cmte = "C00719278",
    FEC_ID_cand = "H0KS01198"
  ),
  tibble(
    first_name = "Brandon",
    last_name = "Williams",
    state_cd = "KS-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brandon 2020 for US Congress",
    campaign_website = "https://www.facebook.com/Brandon2020Kansas/",
    actblue_link = "brandon2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00719526",
    FEC_ID_cand = "H0KS01206"
  ),
  # KS-02 ---------------------------------------------------------------------
  tibble(
    first_name = "Jake",
    last_name = "LaTurner",
    state_cd = "KS-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "LaTurner for Congress",
    campaign_website = "https://www.jakelaturner.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/kansans-for-laturner/donate-sd",
    FEC_ID_cmte = "C00718346",
    FEC_ID_cand = "H0KS02188"
  ),
  tibble(
    first_name = "Michelle",
    last_name = "De La Isla",
    state_cd = "KS-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Michelle for Kansas",
    campaign_website = "https://www.michelleforkansas.com/",
    actblue_link = "michelle-for-kansas-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00732792",
    FEC_ID_cand = "H0KS02196"
  ),
  tibble(
    first_name = "Robert",
    last_name = "Garrard",
    state_cd = "KS-02",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Robert Garrard for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H8KS02108"
  ),
  # KS-03 ---------------------------------------------------------------------
  # Davids vs Adkins General
  tibble(
    first_name = "Sharice",
    last_name = "Davids",
    state_cd = "KS-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.shariceforcongress.com/home",
    actblue_link = "sharice-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Amanda",
    last_name = "Adkins",
    state_cd = "KS-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Amanda Adkins for Congress",
    # website cannot be reached as of Oct 8, 2020
    campaign_website = "https://amandaadkins2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    # the winred link still works, however.
    other_link = "https://secure.winred.com/amandaadkins/donate2020",
    FEC_ID_cmte = "C00717876",
    FEC_ID_cand = "H0KS03236"
  ),
  # tibble(
  #   first_name = "Sara Hart",
  #   last_name = "Weir",
  #   state_cd = "KS-03",
  #   party = "Rep",
  #   incumbent = TRUE,
  #   office_website = NA,
  #   campaign_name = "Sara Hart Weir for Congress",
  #   campaign_website = "https://www.saraforkansas.com",
  #   actblue_link = NA,
  #   anedot_link = NA,
  #   other_link = "https://secure.winred.com/SaraHartWeir/donate",
  #   FEC_ID_cmte = "C00714089",
  #   FEC_ID_cand = "H0KS03228"
  # ),
  # KS-04 ---------------------------------------------------------------------
  # Ron Estes is uncontested (no candidates registered as of Sep 24, 2019)
  # Estes vs Lombard General
  tibble(
    first_name = "Ron",
    last_name = "Estes",
    state_cd = "KS-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://estes.house.gov",
    campaign_name = "Ron Estes for Congress",
    campaign_website = "https://estesforcongress.com/",
    actblue_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from Dec 19, 2019
    # from 25-50-100-250-500-1000-2800-5600
    #   to 25-50-100-250-500-1000-2800-5600 (same amounts)
    anedot_link = "ron-estes-congress/cb09546621e8243a43fbf",
    other_link = "https://secure.winred.com/ronestes/donate",
    FEC_ID_cmte = "C00632067",
    FEC_ID_cand = "H8KS04112"
  ),
  tibble(
    first_name = "Laura",
    last_name = "Lombard",
    state_cd = "KS-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Laura for Kansas",
    campaign_website = "https://lauralombardforkansas.com/",
    actblue_link = "lauraforkansas2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # KY: Kentucky (last updated Jul 11, 2021) ===================================
  # KY-01 ------------------------------------------
  # James Comer is uncontested (no candidates registered as of Sep 24, 2019)
  tibble(
    first_name = "James",
    last_name = "Comer",
    state_cd = "KY-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://comer.house.gov",
    campaign_name = "Comer for Congress",
    campaign_website = "https://jamescomer.com/",
    actblue_link = NA,
    anedot_link = c(
      "comer-for-congress/comer-congress",
      "comer-for-congress/re-elect-james-comer"
    ),
    other_link = NA,
    FEC_ID_cmte = "C00588764",
    FEC_ID_cand = "H6KY01110"
  ),
  tibble(
    first_name = "James",
    last_name = "Rhodes",
    state_cd = "KY-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rhodes for Congress",
    campaign_website = "https://rhodesforcongress.com/",
    actblue_link = "rhodesforcongress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # KY-02 ------------------------------------------
  # Brett Guthrie was uncontested (no candidates registered as of Sep 24, 2019)
  # Now Guthrie vs. Linderman vs. Perry vs. Tribble
  tibble(
    first_name = "Brett",
    last_name = "Guthrie",
    state_cd = "KY-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://guthrie.house.gov",
    campaign_name = "Guthrie for Congress",
    campaign_website = "https://vote.brettguthrie.com/",
    # "https://brettguthrie.com/",
    actblue_link = NA,
    anedot_link = "brett-guthrie-for-congress/general-fund-cbc2bf72fab",
    other_link = NA,
    FEC_ID_cmte = "C00445023",
    FEC_ID_cand = "H8KY02031"
  ),
  tibble(
    first_name = "Hank",
    last_name = "Linderman",
    state_cd = "KY-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hank for KY",
    campaign_website = "https://www.hank4ky.com/",
    actblue_link = c("hank4ky-1", "hank4ky"),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00445023",
    FEC_ID_cand = "H8KY02031"
  ),
  tibble(
    first_name = "Robert",
    last_name = "Perry",
    state_cd = "KY-02",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Perry for Kentucky",
    campaign_website = "https://www.perry4ky.com/",
    actblue_link = NA,
    anedot_link = "perry-for-kentucky/donate",
    other_link = NA,
    FEC_ID_cmte = "C00729087",
    FEC_ID_cand = "H0KY02095"
  ),
  # cannot find website for Lewis Carter (Populist party)
  tibble(
    first_name = "Lewis",
    last_name = "Carter",
    state_cd = "KY-02",
    party = "Pop",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # Cannot find Poet Tribble
  # KY-03 ------------------------------------------
  # John Yarmuth was uncontested (no candidates registered as of Sep 24, 2019)
  # Now Yarmuth vs. Craven vs. Serrano vs. Palazzo vs. Eddings
  tibble(
    first_name = "John",
    last_name = "Yarmuth",
    state_cd = "KY-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://yarmuth.house.gov",
    campaign_name = "Yarmuth for Congress",
    campaign_website = "https://www.yarmuthforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://www.yarmuthforcongress.com/contribute",
    # In reality NGP VAN
    # NGP VPN address changed. - Oct 13
    other_link = "https://secure.ngpvan.com/v1/Forms/zlW8zr2BKEa_YQBu8Pin8A2",
    # "https://act.myngp.com/Forms/6048279317232945920",
    FEC_ID_cmte = "C00419630",
    FEC_ID_cand = "H6KY03124"
  ),
  tibble(
    first_name = "Mike",
    last_name = "Craven",
    state_cd = "KY-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mike Craven for Congress",
    campaign_website = "https://www.mikecravenforuscongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-150-250-500-1000-1500-2000-2700
    other_link = "https://www.mikecravenforuscongress.com/contribute",
    FEC_ID_cmte = "C00745026",
    FEC_ID_cand = "H8KY03187"
  ),
  tibble(
    first_name = "Gerardo",
    last_name = "Serrano",
    state_cd = "KY-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gerardo Serrano for Congress",
    campaign_website = "http://serrano4congress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts (perhaps entering all personal details will show?)
    other_link = "https://serrano4congress.com/donate/",
    FEC_ID_cmte = "C00743922",
    FEC_ID_cand = "H0KY03267"
  ),
  tibble(
    first_name = "Rhonda",
    last_name = "Palazzo",
    state_cd = "KY-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rhonda Palazzo for Congress",
    campaign_website = "https://www.rhondaforcongress.com/",
    # "https://www.rhondapalazzoforcongress.com/",
    actblue_link = NA,
    anedot_link = "rhonda-palazzo-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00737635",
    FEC_ID_cand = "H0KY03259"
  ),
  tibble(
    first_name = "Waymen",
    last_name = "Eddings",
    state_cd = "KY-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "http://www.waymeneddings.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-1000-2800
    other_link = "http://www.waymeneddings.com/product/donate/",
    FEC_ID_cmte = "C00736116",
    FEC_ID_cand = "H0KY03242"
  ),
  # KY-04 ------------------------------------------
  tibble(
    first_name = "Thomas",
    last_name = "Massie",
    state_cd = "KY-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://massie.house.gov/",
    campaign_name = "Thomas Massie for Congress",
    campaign_website = "https://www.thomasmassie.com/",
    actblue_link = NA,
    # "https://secure.thomasmassie.com/donate"
    # Picture is slightly different but underlying data is the same
    anedot_link = "thomas-massie-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00509729",
    FEC_ID_cand = "H2KY04121"
  ),
  # McMurtry must have been a major player, but website is now gone
  # Thankfully, was archived and could infer anedot address, still live
  tibble(
    first_name = "Todd",
    last_name = "McMurtry",
    state_cd = "KY-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "McMurtry for Congress",
    campaign_website = NA, # "http://mcmurtryforcongress.com/",
    actblue_link = NA,
    anedot_link = "mcmurtry-for-congress/website-splash",
    other_link = NA,
    FEC_ID_cmte = "C00734053",
    FEC_ID_cand = "H0KY04133"
  ),
  tibble(
    first_name = "Alexandra",
    last_name = "Owensby",
    state_cd = "KY-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Alexandra Owensby for US Congress KY-4",
    campaign_website = "https://www.amowensby.com/",
    actblue_link = "owensby",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00709204",
    FEC_ID_cand = "H0KY04117"
  ),
  tibble(
    first_name = "Shannon",
    last_name = "Fabert",
    state_cd = "KY-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shannon Fabert for the 4th",
    campaign_website = "https://fabert2020.org/",
    actblue_link = "fabertforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00718106",
    FEC_ID_cand = "H0KY04125"
  ),
  # KY-05 ------------------------------------------
  tibble(
    first_name = "Hal", # Harold Dallas, goes by Hal
    last_name = "Rogers",
    state_cd = "KY-05",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Hal Rogers for Congress",
    campaign_website = "https://congressmanhalrogers.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/halrogers/donate",
    FEC_ID_cmte = "C00116632",
    FEC_ID_cand = "H0KY05015"
  ),
  tibble(
    first_name = "Matthew",
    last_name = "Best",
    state_cd = "KY-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Best for Kentucky",
    campaign_website = "https://bestforky.com/",
    actblue_link = "best-for-kentucky-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # Hibbitts website gone
  # Cannot find Matthew Ryan Best
  # Cannot find John Wells
  # KY-06 ------------------------------------------
  # Barr vs. Hicks vs. Harris vs. Young
  tibble(
    first_name = "Andy", # Garland Andy, goes by Andy
    last_name = "Barr",
    state_cd = "KY-06",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://barr.house.gov",
    campaign_name = "Andy Barr for Congress",
    campaign_website = "https://andybarrforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/andybarrforcongress/donate",
    FEC_ID_cmte = "C00467571",
    FEC_ID_cand = "H0KY06104"
  ),
  tibble(
    first_name = "Josh",
    last_name = "Hicks",
    state_cd = "KY-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Josh Hicks for Congress",
    campaign_website = "https://www.joshhicksforcongress.com",
    actblue_link = "joshhicks",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00714279",
    FEC_ID_cand = "H0KY06179"
  ),
  tibble(
    first_name = "Frank",
    last_name = "Harris",
    state_cd = "KY-06",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Frank Hairrs for Congress",
    campaign_website = "https://www.frankharris4congress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H8KY06222"
  ),
  tibble(
    first_name = "Daniel",
    last_name = "Kemph",
    state_cd = "KY-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Daniel Kemph for Congress",
    campaign_website = "http://www.kemph2020.com/",
    actblue_link = "kemph2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00717660",
    FEC_ID_cand = "H8KY06180"
  ),
  tibble(
    first_name = "Geoff",
    last_name = "Young",
    state_cd = "KY-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Geoff Young for Kentucky",
    campaign_website = "https://www.young4ky.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/geoff-young-for-kentucky/donate",
    FEC_ID_cmte = c("C00710152", "C00599514"),
    FEC_ID_cand = "H4KY06163"
  ),
  # LA: Louisiana (last updated Oct 14/15,2020) ==================================
  # LA-01 ---------------------------------------------------------------------
  # Steve Scalise is uncontested (no candidates registered as of Sep 24, 2019)
  tibble(
    first_name = "Steve",
    last_name = "Scalise",
    state_cd = "LA-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://scalise.house.gov",
    campaign_name = "Scalise for Congress",
    campaign_website = "https://stevescalise.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # at least from Jan 7, 2020
    # from 10-25-50-100-250-1000-2800 to 35-50-100-250-500-1000-2800
    other_link = paste0(
      # "https://secure.victorypassport.com/pages/scalise/",
      # "scalise-home-page-embed?location=https%3A%2F%2Fstevescalise.com%2F",
      # "donate&location_url=aHR0cHM6Ly9zdGV2ZXNjYWxpc2UuY29tL2RvbmF0ZS8j"
      "https://secure.winred.com/stevescalise/home"
    ),
    FEC_ID_cmte = "C00394957",
    FEC_ID_cand = "H0LA01087"
  ),
  tibble(
    first_name = "Lee Ann",
    last_name = "Dugas",
    state_cd = "LA-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dugas for Congress",
    campaign_website = "https://twitter.com/LeeDugas2001",
    # cannot find payments at all
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # LA-02 ---------------------------------------------------------------------
  # Cedric Richmond is uncontested (no candidates registered as of Sep 24, '19)
  tibble(
    first_name = "Cedric",
    last_name = "Richmond",
    state_cd = "LA-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://richmond.house.gov",
    campaign_name = "Richmond for Congress",
    campaign_website = "https://cedricrichmond.com/",
    actblue_link = "cedric-richmond-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00451336",
    FEC_ID_cand = "H8LA02054"
  ),
  # Cannot find website or payment info for Glenn Harris
  tibble(
    first_name = "Glenn",
    last_name = "Harris",
    state_cd = "LA-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Belden",
    last_name = "Batiste",
    state_cd = "LA-02",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Belden Batiste for Congress",
    campaign_website = "https://belden4congress.com/",
    # no payment info
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Colby",
    last_name = "James",
    state_cd = "LA-02",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Colby James for Congress",
    campaign_website = "https://colby4congress.squarespace.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 10-50-100 but cannot figure out how to get fund_misc to work for it.
    other_link = "https://donorbox.org/embed/committee-to-elect-colby-james",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # cannot find Sheldon C. Vincent, Sr. or David Schilling (both Rep)
  # LA-03 -----------------------------------------
  # Higgins vs. Anderson vs. Thomas
  tibble(
    first_name = "Clay Captain",
    last_name = "Higgins",
    state_cd = "LA-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Captain Higgins for Congress",
    campaign_website = "https://captainhiggins.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/clayhiggins/donate",
    FEC_ID_cmte = "C00617662",
    FEC_ID_cand = "H6LA03148"
  ),
  tibble(
    first_name = "Rob",
    last_name = "Anderson",
    state_cd = "LA-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rob for Louisiana",
    campaign_website = "https://www.robandersonforcongress.com",
    actblue_link = "robmob",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00731380", "C00668913"),
    FEC_ID_cand = "H8LA03060"
  ),
  tibble(
    first_name = "Braylon",
    last_name = "Harris",
    state_cd = "LA-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Braylon Harris for Louisiana",
    campaign_website = "https://harris4uscongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.ngpvan.com/v1/Forms/SmKLOj5DxkiO8Y96aSoDVQ2",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Brandon",
    last_name = "LeLeux",
    state_cd = "LA-03",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brandon LeLeux for Congress",
    campaign_website = "https://brandonleleuxforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # paypal link, only a box to enter.
    other_link =
      "https://www.paypal.com/donate/?cmd=_s-xclick&hosted_button_id=UP5EU9UCQGH62",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Verone",
    last_name = "Thomas",
    state_cd = "LA-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Verone for Congress",
    campaign_website = "https://twitter.com/verone4congress",
    # Cannot find donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00716993", "C00675421"),
    FEC_ID_cand = "H8LA03086"
  ),
  
  # LA-04 ---------------------------------------------------------------------
  # Mike Johnson is uncontested (no candidates registered as of Sep 24, 2019)
  tibble(
    first_name = "Mike",
    last_name = "Johnson",
    state_cd = "LA-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://mikejohnson.house.gov",
    campaign_name = "Mike Johnson for Louisiana",
    campaign_website = "https://www.mikejohnsonforlouisiana.com/",
    actblue_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from Dec 19, 2019
    # 25-50-100-250-1000-2800 to 25-50-100-250-500-1000-2800
    anedot_link = "mikejohnson/donate",
    other_link = "https://secure.winred.com/MikeJohnson/donate",
    FEC_ID_cmte = "C00608695",
    FEC_ID_cand = "H6LA04138"
  ),
  tibble(
    first_name = "Ben",
    last_name = "Gibson",
    state_cd = "LA-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ben Gibson for Congress",
    campaign_website = "https://bengibsonforcongress.com/",
    # cannot find contribution links
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Kenny",
    last_name = "Houston",
    state_cd = "LA-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kenny Houston for Congress",
    campaign_website = "https://kennyhoustonforcongress.com/",
    actblue_link = "/kennyhoustonforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Ryan",
    last_name = "Trundle",
    state_cd = "LA-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ryan Trundle for Congress",
    campaign_website = "https://www.ryantrundle.com/",
    actblue_link = "ryan-trundle-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # LA-05 ---------------------------------------------------------------------
  # Ralph Abraham was uncontested as of Sep 17, 2019
  # Now Abraham is running for Governor: https://abrahamforgovernor.com/
  # Abraham vs. Pierrottie, but cannot find Pierottie
  # Filed Sep 20, 2019 so probably needs time
  tibble(
    first_name = "Ralph",
    last_name = "Abraham",
    state_cd = "LA-05",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://abraham.house.gov",
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Candy",
    last_name = "Christophe",
    state_cd = "LA-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Candy Christophe for COngress",
    campaign_website = "https://www.candychristophe4congress.com/",
    actblue_link = "candy-christophe-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Jesse",
    last_name = "Lagarde",
    state_cd = "LA-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lagarde for Louisiana",
    campaign_website =
      "https://www.facebook.com/Lagarde-for-Louisiana-109622654175460/",
    # no donation links
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Allen",
    last_name = "Guillory",
    state_cd = "LA-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Allen Guillory for Congress",
    campaign_website =
      "https://www.facebook.com/Allen-Guillory-for-Congressmen-107722267707259/",
    # no donation links
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Lance",
    last_name = "Harris",
    state_cd = "LA-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lance Harris for Congress",
    campaign_website = "http://electlance.com/",
    actblue_link = NA,
    anedot_link = "lance-harris-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Luke",
    last_name = "Letlow",
    state_cd = "LA-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "LetLow for Congress",
    campaign_website = "https://lukeletlow.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/letlow-for-congress/letlowforcongress",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Martin",
    last_name = "Lemelle",
    state_cd = "LA-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lemelle for Congress",
    campaign_website = "https://martinlemelle.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 5-20.20-50-100-250-500-1000-2800
    other_link = "https://secure.numero.ai/contribute/MartinLemelle",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Matt",
    last_name = "Hasty",
    state_cd = "LA-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Matt Hasty for US Congress",
    campaign_website = "https://www.geauxhasty.com/",
    # "https://www.facebook.com/Matt-Hasty-for-US-Congress-111253797316410/",
    actblue_link = NA,
    anedot_link = "matt-hasty-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Phillip",
    last_name = "Snowden",
    state_cd = "LA-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Committee to Elect Phillip Snowden",
    campaign_website = "https://www.phillipsnowdencampaign.com/",
    actblue_link = "phillip-snowden-campaign-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Scotty",
    last_name = "Robinson",
    state_cd = "LA-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Scotty Robinson for Congress",
    campaign_website = "https://scottyforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/scotty-robinson-for-congress/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # LA-06 ---------------------------------------------------------------------
  # Garret Graves is uncontested (no candidates registered as of Sep 24, 2019)
  tibble(
    first_name = "Garret",
    last_name = "Graves",
    state_cd = "LA-06",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://garretgraves.house.gov",
    campaign_name = "Garret Graves for Congress",
    campaign_website = "https://www.garretgraves.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://garretgraves.ichooseapex.com/donate/",
    FEC_ID_cmte = "C00558486",
    FEC_ID_cand = "H4LA06153"
  ),
  tibble(
    first_name = "Dartanyon A.",
    last_name = "Williams",
    state_cd = "LA-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dartanyon A. Williams for US Congress",
    campaign_website = "https://www.daw4us.com/",
    actblue_link = "dartanyon-a-williams",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Shannon",
    last_name = "Sloan",
    state_cd = "LA-06",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shannon Sloan for Congress",
    campaign_website = "https://www.facebook.com/sloanforliberty/",
    # cannot find donation links
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Richard",
    last_name = "Torregano",
    state_cd = "LA-06",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://rpt501.com/",
    # cannot find donation links
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # ME: Maine (last update October 15, 2020====================================
  # ME-01  ------------------------------------------
  # Pingree vs. Allen
  tibble(
    first_name = "Chellie",
    last_name = "Pingree",
    state_cd = "ME-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://pingree.house.gov",
    campaign_name = "Chellie Pingree for Congress",
    campaign_website = "https://www.chelliepingree.com/",
    actblue_link = "pingree",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00433391",
    FEC_ID_cand = "H8ME01120"
  ),
  tibble(
    first_name = "Jay",
    last_name = "Allen",
    state_cd = "ME-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jay Allen for Congress",
    campaign_website = "https://www.jayallenformaine.com",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Feb 12, 2020
    # from 5-10-25-50-100-250-500-1000-2000-2800
    #   to      25-50-100-250-500-1000-2800
    # other_link = "https://politics.raisethemoney.com/jayallen?iframe=true",
    other_link = "https://secure.winred.com/JayAllen/donate",
    FEC_ID_cmte = "C00715532",
    FEC_ID_cand = "H0ME01119"
  ),
  # ME-02  ------------------------------------------
  # Golden vs. Brakey
  # vs. Crafts vs. Bennett vs. Hiatt (Hiatt withdrew)
  tibble(
    first_name = "Jared",
    last_name = "Golden",
    state_cd = "ME-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://golden.house.gov",
    campaign_name = "Jared Golden for Congress",
    campaign_website = "https://jaredgoldenforcongress.com/",
    actblue_link = "jaredforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00653816",
    FEC_ID_cand = "H8ME02185"
  ),
  tibble(
    first_name = "Eric",
    last_name = "Brakey",
    state_cd = "ME-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brakey for Congress",
    campaign_website = "https://ericbrakey.com/",
    actblue_link = NA,
    anedot_link = "brakey-for-congress/donate",
    # other_link = "https://ericbrakey.com/donate",
    other_link = NA,
    FEC_ID_cmte = "C00713958",
    FEC_ID_cand = "H0ME02067"
  ),
  tibble(
    first_name = "Dale",
    last_name = "Crafts",
    state_cd = "ME-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dale Crafts for Congress",
    campaign_website = "https://dalecraftsforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/dale-crafts-for-congress/contribute",
    FEC_ID_cmte = "C00722454",
    FEC_ID_cand = "H0ME02083"
  ),
  tibble(
    first_name = "Adrienne",
    last_name = "Bennett",
    state_cd = "ME-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Adrienne for Congress",
    campaign_website = "https://www.adrienne2020.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/AdrienneBennett/donate",
    FEC_ID_cmte = "C00722108",
    FEC_ID_cand = "H0ME02075"
  ),
  # Hiatts withdrew on Dec 19, 2020
  # MD: Maryland (last updated Oct 15 020) ====================================
  # MD-01 ---------------------------------------------------------------------
  # Andrew Harris was uncontested as of Sep 16, 2019
  # Now Harris vs. Delgado (registered Sep 16, 2019)
  tibble(
    first_name = "Andy",
    last_name = "Harris",
    state_cd = "MD-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://harris.house.gov",
    campaign_name = "Andy Harris for Congress",
    campaign_website = "https://www.andyharris.com/",
    actblue_link = NA,
    anedot_link = "andy-harris-for-congress/andy-harris-for-congress",
    other_link = NA,
    FEC_ID_cmte = "C00435974",
    FEC_ID_cand = "H8MD01094"
  ),
  tibble(
    first_name = "Jorge",
    last_name = "Delgado",
    state_cd = "MD-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Delgado for Congress",
    campaign_website = "http://delgado4congress.com/",
    actblue_link = NA,
    # No suggested amounts. Could be a good baseline.
    anedot_link = "delgado4congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00719658",
    FEC_ID_cand = "H0MD01216"
  ),
  
  tibble(
    first_name = "Mia",
    last_name = "Mason",
    state_cd = "MD-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mia Mason for Congress",
    campaign_website = "mia4maryland",
    actblue_link = "mia4maryland",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MD-02 ---------------------------------------------------------------------
  tibble(
    first_name = "Dutch",
    last_name = "Ruppersberger",
    state_cd = "MD-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://ruppersberger.house.gov/",
    campaign_name = "Dutch Ruppersberger for Congress",
    campaign_website = "https://www.dutchforcongress.com/",
    actblue_link = "dutch-web-donate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Johnny",
    last_name = "Salling",
    state_cd = "MD-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Salling for Congress",
    campaign_website = "http://www.sallingforcongress.com/",
    actblue_link = NA,
    anedot_link = "salling-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Jeff",
    last_name = "Northcott",
    state_cd = "MD-02",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jeff Northcott for Congress",
    campaign_website = "https://www.facebook.com/jeff4congress/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MD-03 ---------------------------------------------------------------------
  tibble(
    first_name = "John",
    last_name = "Sarbanes",
    state_cd = "MD-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://sarbanes.house.gov/",
    campaign_name = "Sarbanes for Congress",
    campaign_website = "https://www.johnsarbanes.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/5146769949154344960",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Charles",
    last_name = "Anthony",
    state_cd = "MD-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Charles Anthony for Congress",
    campaign_website = "https://charlesanthonyforcongress.com/",
    actblue_link = NA,
    anedot_link = "charlesanthonyforcongress/contribute",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MD-04 ---------------------------------------------------------------------
  # Brown vs. Bryant
  tibble(
    first_name = "Anthony",
    last_name = "Brown",
    state_cd = "MD-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Anthony Brown for Congress",
    campaign_website = "https://anthonybrown.com/",
    # act blue link on Oct 15, 2020.
    # but the ngp link also works.
    actblue_link = "anthonybrownwebsite",
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/3029234798871707648",
    FEC_ID_cmte = "C00574640",
    FEC_ID_cand = "H6MD04209"
  ),
  tibble(
    first_name = "Shelia",
    last_name = "Bryant",
    state_cd = "MD-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shelia Bryant For Congress",
    campaign_website = "https://www.bryantforcongress.com/",
    actblue_link = "bryant-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00708917",
    FEC_ID_cand = "H0MD04145"
  ),
  tibble(
    first_name = "George",
    last_name = "McDermott",
    state_cd = "MD-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "George McDermott for Congress",
    campaign_website = "http://www.georgemcdermott.com/",
    # cannot find donation links.
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MD-05 ---------------------------------------------------------------------
  tibble(
    first_name = "Steny",
    last_name = "Hoyer",
    state_cd = "MD-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://hoyer.house.gov",
    campaign_name = "Hoyer for Congress",
    campaign_website = "https://www.hoyerforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # NGP VAN, but the usual act.myngp.com does not work...
    # 35-50-100-500-2500-2800
    # changed to NGP link
    other_link = "https://act.myngp.com/Forms/8000653924697966336",
    # "https://www.hoyerforcongress.com/contribute",
    FEC_ID_cmte = "C00140715",
    FEC_ID_cand = "H2MD05155"
  ),
  tibble(
    first_name = "McKayla",
    last_name = "Wilkes",
    state_cd = "MD-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "McKayla Wilkes for Congress",
    campaign_website = "https://www.mckayla2020.com",
    actblue_link = c(
      "-mckaylaforcongress-1#",
      "mckaylawilkes"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00698563",
    FEC_ID_cand = "H0MD05183"
  ),
  tibble(
    first_name = "Briana",
    last_name = "Urbina",
    state_cd = "MD-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Urbina for Congress",
    campaign_website = "https://www.urbinaforus.com",
    actblue_link = "briana-urbina",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00705087",
    FEC_ID_cand = "H0MD05191"
  ),
  tibble(
    first_name = "Kevin", # Darryl Kevin Jones, goes by Kevin
    last_name = "Jones",
    state_cd = "MD-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kevin Jones for Congress",
    campaign_website = "https://www.kevinjones2020.com",
    actblue_link = "kevin-jones-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00716746",
    FEC_ID_cand = "H0MD05209"
  ),
  tibble(
    first_name = "Lee",
    last_name = "Havis",
    state_cd = "MD-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Havis for Congress",
    campaign_website = "https://havisforcongress.com",
    actblue_link = NA,
    anedot_link = "havis-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00721902",
    FEC_ID_cand = "H0MD05217"
  ),
  tibble(
    first_name = "Chris",
    last_name = "Palombi",
    state_cd = "MD-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Palombi for Congress",
    campaign_website = "https://www.chrispalombi.com/",
    actblue_link = NA,
    anedot_link = "palombi-for-congress/donate",
    other_link =
      "https://secure.winred.com/palombi-for-congress/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Rashad",
    last_name = "Lloyd",
    state_cd = "MD-05",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rashad Lloyd for MD-05",
    campaign_website = "",
    # not taking any donations.
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MD-06 ---------------------------------------------------------------------
  tibble(
    first_name = "David",
    last_name = "Trone",
    state_cd = "MD-06",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://trone.house.gov/",
    campaign_name = "David Trone for Congress",
    campaign_website = "https://www.davidtrone.com/",
    actblue_link = "david_trone",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Neil",
    last_name = "Parrott",
    state_cd = "MD-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Parrott for Congress",
    campaign_website = "https://www.parrottforcongress.org/homepage",
    # main website has a diffrent method but also has option to pay thru Winred
    # In website donation page:
    # 6-14-25-50-100-250-500-1000-2800-75-125 in that order.
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      "https://www.parrott2020.org/donate-page",
      "https://secure.winred.com/NeilParrott/donate"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MD-07 ---------------------------------------------------------------------
  # Elijah Cummings is uncontested (no candidates registered as of Sep 24, '19)
  # Mfume is the incumbent after Cummings passed away.
  # vs. Klacik for general
  tibble(
    first_name = "Elija",
    last_name = "Cummings",
    state_cd = "MD-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://cummings.house.gov",
    campaign_name = "Cummings for Congress",
    campaign_website = "https://www.cummingsforcongress.com/",
    actblue_link = "elijah-cummings-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00310318",
    FEC_ID_cand = "H6MD07160"
  ),
  tibble(
    first_name = "Kweisi",
    last_name = "Mfume",
    state_cd = "MD-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://mfume.house.gov/",
    campaign_name = "Mfume for Congress",
    campaign_website = "https://mfumeforcongress.com/",
    actblue_link = "mfumeforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Kim",
    last_name = "Klacik",
    state_cd = "MD-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "https://kimkforcongress.com/",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = "kim-klacik-for-congress/donate",
    other_link = "https://secure.winred.com/kimklacik/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MD-08 ---------------------------------------------------------------------
  # Jamie Raskin is uncontested (no candidates registered as of Sep 24, 2019)
  tibble(
    first_name = "Jamie",
    last_name = "Raskin",
    state_cd = "MD-08",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://raskin.house.gov",
    campaign_name = "Jamie Raskin for Congress",
    campaign_website = "https://jamieraskin.com/",
    # looks like recently they removed actBlue for NGP link. (oct 15)
    actblue_link = c("rockthehouseraskin", "donatetojamieraskin"),
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/2890878353571973120",
    FEC_ID_cmte = "C00575126)",
    FEC_ID_cand = "H6MD08457"
  ),
  tibble(
    first_name = "Gregory",
    last_name = "Coll",
    state_cd = "MD-08",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gregory Coll for Congress",
    campaign_website = "https://www.gregorycoll.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/gregory-coll-for-congress/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MA: Massachusetts (last updated Jul 11, 2021) =========================
  # MA-01 ---------------------------------------------------------------------
  # Richard Neal faces no major party opposition
  tibble(
    first_name = "Richard",
    last_name = "Neal",
    state_cd = "MA-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://neal.house.gov/",
    campaign_name = "Neal for Congress",
    campaign_website = "https://www.nealforcongress.com/",
    actblue_link = "richardneal14",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MA-02 ---------------------------------------------------------------------
  # McGovern vs. Miller but Miller dropped out
  # Jim McGovern is uncontested (no candidates registered as of Sep 26, 2019)
  # McGovern vs Lovvorn.
  tibble(
    first_name = "Jim",
    last_name = "McGovern",
    state_cd = "MA-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://mcgovern.house.gov",
    campaign_name = "Jim McGovern for Congress",
    campaign_website = "https://www.jimmcgovern.com/",
    # Default suggests 15
    actblue_link = "stand-with-jim-mcgovern",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00285171",
    FEC_ID_cand = "H4MA03022"
  ),
  tibble(
    first_name = "Tracy",
    last_name = "Lovvorn",
    state_cd = "MA-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tracy Lovvorn for Congress",
    campaign_website = "https://tracyforcongress.com/",
    actblue_link = NA,
    # anedot default on campaign website.
    anedot_link = "tlovvfc/donate",
    other_link = "https://secure.winred.com/tracy-lovvorn-for-congress/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MA-03 ---------------------------------------------------------------------
  # Lori Trahan is uncontested (no candidates registered as of Sep 26, 2019)
  # Trahan uncontested
  tibble(
    first_name = "Lori",
    last_name = "Trahan",
    state_cd = "MA-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://trahan.house.gov",
    campaign_name = "Lori Trahan for Congress",
    campaign_website = "https://loritrahan.com/",
    actblue_link = "loritrahan", # Defaults to 5
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00655647",
    FEC_ID_cand = "H8MA03106"
  ),
  # MA-04 ---------------------------------------------------------------------
  # Open seat
  # Jake Auchincloss	vs Julie Hall	General election
  tibble(
    first_name = "Jesse",
    last_name = "Mermell",
    state_cd = "MA-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jesse Mermell for Congress",
    campaign_website = "https://jessemermell.com",
    actblue_link = "jm-donate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00721282",
    FEC_ID_cand = "H0MA04184"
  ),
  tibble(
    first_name = "Jake",
    last_name = "Auchincloss",
    state_cd = "MA-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jake Auchincloss for Congress",
    campaign_website = "https://www.jakeforma.com/",
    actblue_link = "jake-auchincloss-for-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Julie",
    last_name = "Hall",
    state_cd = "MA-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Julie Hall for Congress",
    campaign_website = "https://hallforcongress.com/",
    actblue_link = NA,
    # website defaults to anedot.
    anedot_link = "committee-to-elect-julie-hall/webdonate",
    other_link =
      "https://secure.winred.com/committee-to-elect-julie-hall/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MA-05 ---------------------------------------------------------------------
  tibble(
    first_name = "Katherine",
    last_name = "Clark",
    state_cd = "MA-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://katherineclark.house.gov/",
    campaign_name = "Katherine Clark for Congress",
    campaign_website = "https://www.katherineclark.org/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/9009862872284923904",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Caroline",
    last_name = "Colarusso",
    state_cd = "MA-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Colarusso for Congress",
    campaign_website = "https://colarussoforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/caroline-colarusso-for-congress/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MA-06 ---------------------------------------------------------------------
  # Seth Moulton is a former presidential candidate
  # Moulton vs. Moran
  tibble(
    first_name = "Seth",
    last_name = "Moulton",
    state_cd = "MA-06",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://moulton.house.gov",
    campaign_name = "Seth Moulton for Congress",
    campaign_website = "https://sethmoulton.com",
    # This is Moulton Leadership Fund, and not his PCC
    # still links to the same leadershio fund. (Oct 2020)
    actblue_link = "moultonlf2019",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00547240",
    FEC_ID_cand = "H4MA06090"
  ),
  tibble(
    first_name = "John Paul",
    last_name = "Moran",
    state_cd = "MA-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John Paul Moran for Congress",
    campaign_website = "https://johnpaulmoran.com/",
    actblue_link = NA,
    anedot_link =
      "stand-with-john-paul-ce4276d0-9455-4439-9549-e7ed4e270ef3/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MA-07 -----------------------------------------
  # Ayanna Pressley is uncontested (no candidates registered as of Feb 20, '20)
  # 20.53 in the main screen. Interesting! Says it is average cont. this year
  # Pressley unopposed for General elections
  tibble(
    first_name = "Ayanna",
    last_name = "Pressley",
    state_cd = "MA-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://pressley.house.gov",
    campaign_name = "Ayanna Pressley for Congress",
    campaign_website = "https://ayannapressley.com/",
    actblue_link = c(
      "ayannapressleysite1", # from her website/
      "ads-google-ayanna-q2-20",
      "ads-google-ayanna-q1-20",
      "ayanna_swag",
      # "apem-20200313-goal",
      # "apem-20200309-goal",            # 13.02
      # "apem-20200308-goal",
      # "apem-20200229-goal",
      # "apem-20200228-goal",
      # "apem-20200227-pou",
      # "apem-20200226-goal",            # 7.95
      # "apem-20200222-goal",
      # "apem-20200221-pou",             # Split with Power of Us
      # "apem-20200220-warren-pressley", # Split with Warren
      # "apem-20200219-debate",
      # "apem-20200216-pou",
      # "apem-20200214-goal",
      # "apem-20200213-corruption",
      # "apem-20200209-wfp",
      # "apem-20200204-sotu-pm",
      # "apem-20200130-credit",
      # "apem-20200129-deadline",        # Split with leadership PAC
      # "apem-20200128-deadline",
      # "apem-20200127-iowa",
      # "apem-20200123",
      # "apem-20200122-deadline",
      # "apem-20200113-pm",
      # "apem-20200113",
      # "stand-with-ilhan",
      # "apem-201912-eoy",
      # "apem-20191228-justice",
      # "ayanna_jessica",
      # "apem-20191216-goal",
      # "apem-20191214-goal",
      # "apem-nov19-eom",
      # "apem-catching-up",
      # "change-cant-wait",
      # "warren-pressley",
      # "our-movement-for-environmental-justice",
      # "ap-ema-october",
      # "congressional-endorsements",
      # "impeach-the-occupant-of-the-white-house",
      # "our-movement-for-racial-justice",
      # "end-abusive-debt-collection",
      # "impeach-kavanaugh",
      # "repeal-hyde",
      # "stand-with-aoc",
      # "end-of-quarter-goal",
      # "fight-back-against-the-nra-squad?",
      # "ap-ema-september",
      # "september2019-goal",
      "ayanna-pressley"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00667741",
    FEC_ID_cand = "H8MA07032"
  ),
  # MA-08 ---------------------------------------------------------------------
  # Lynch uncontested.
  tibble(
    first_name = "Stephen",
    last_name = "Lynch",
    state_cd = "MA-08",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://lynch.house.gov/",
    campaign_name = "Stephen F Lynch for Congress",
    campaign_website = "https://www.stephenflynch.com/",
    actblue_link = "stephen-lynch-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MA-09 ---------------------------------------------------------------------
  # Bill Keating is uncontested (no candidates registered as of Sep 26, 2019)
  # Keating vs. Brady vs. Manley in General.
  tibble(
    first_name = "Bill",
    last_name = "Keating",
    state_cd = "MA-09",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://keating.house.gov",
    campaign_name = "Bill Keating For Congress",
    campaign_website = "http://www.keatingforcongress.com/",
    actblue_link = "bill-keating",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00479063",
    FEC_ID_cand = "H0MA10082"
  ),
  tibble(
    first_name = "Helen",
    last_name = "Brady",
    state_cd = "MA-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Give 'Em Helen",
    campaign_website = "https://www.giveemhelen.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00750125",
    FEC_ID_cand = "H0MA09076"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Manley",
    state_cd = "MA-09",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Coach Manley for Congress",
    campaign_website = "https://coachmanley2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0MA09084"
  ),
  # MI: Michigan (last updated Jul 11, 2021) ==============================================================
  # MI-01 ---------------------------------------------------------------------
  tibble(
    first_name = "Jack",
    last_name = "Bergman",
    state_cd = "MI-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://bergman.house.gov/",
    campaign_name = "Bergman for Congress",
    campaign_website = "https://bergmanforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/jackbergman/contribute",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Dana",
    last_name = "Ferguson",
    state_cd = "MI-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ferguson for Congress",
    campaign_website = "https://www.fergusonformichigan1.com/",
    actblue_link = "friends-of-dana-ferguson-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Ben",
    last_name = "Boren",
    state_cd = "MI-01",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Boren for Liberty",
    campaign_website = "https://borenforliberty.com/",
    actblue_link = NA,
    anedot_link = "ben-boren-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MI-02 ---------------------------------------------------------------------
  tibble(
    first_name = "Bill",
    last_name = "Huizenga",
    state_cd = "MI-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://huizenga.house.gov/",
    campaign_name = "Huizenga for Congress",
    campaign_website = "https://huizengaforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/huizenga-for-congress/contribute",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Bryan",
    last_name = "Berghoef",
    state_cd = "MI-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bryan Berghoef for Congress",
    campaign_website = "https://www.bryanberghoef.com/",
    actblue_link = "berghoef-cmgwebsite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Jean-Michel",
    last_name = "Creviere",
    state_cd = "MI-02",
    party = "Gre",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.2020creviere.com/",
    # Cannot find donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Max",
    last_name = "Riekse",
    state_cd = "MI-02",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Max Riekse for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0MI02227"
  ),
  # cannot find Gerald Van Sickle (Taxpayers)
  # MI-03 ---------------------------------------------------------------------
  tibble(
    first_name = "Justin",
    last_name = "Amash",
    state_cd = "MI-03",
    party = "Ind",
    incumbent = TRUE,
    office_website = "https://amash.house.gov/",
    campaign_name = "Justin Amash for Congress",
    campaign_website = "https://www.justinamash.com/",
    actblue_link = NA,
    anedot_link = "justinamash/donate",
    other_link = NA,
    FEC_ID_cmte = "C00476291",
    FEC_ID_cand = "H0MI03126"
  ),
  tibble(
    first_name = "Jim",
    last_name = "Lower",
    state_cd = "MI-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jim Lower for Congress",
    campaign_website = "https://votelower.com/",
    # Change documented to WinRed on Nov 18, 2019
    # at least from Nov 12, 2019
    actblue_link = NA,
    # No reference points. Could be a good baseline.
    # anedot_link = "jim-lower-for-congress/donate",
    anedot_link = NA,
    other_link = "https://secure.winred.com/jim-lower-for-congress/donate-cvm",
    # 25-50-100-250-500-1000-2800
    FEC_ID_cmte = "C00707430",
    FEC_ID_cand = "H0MI03274"
  ),
  tibble(
    first_name = "Nick",
    last_name = "Colvin",
    state_cd = "MI-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Nick Colvin for Michigan",
    campaign_website = "https://www.colvinformichigan.com/",
    actblue_link = "nickcolvin-web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00709519",
    FEC_ID_cand = "H0MI03282"
  ),
  tibble(
    first_name = "Peter",
    last_name = "Meijer",
    state_cd = "MI-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Peter Meijer",
    campaign_website = "https://www.votemeijer.com/",
    actblue_link = NA,
    anedot_link = NA,
    # defaults to 5.
    other_link = "https://secure.winred.com/peter-meijer/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Hillary",
    last_name = "Scholten",
    state_cd = "MI-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hillary Scholten for Congress",
    campaign_website = "https://www.hillaryscholten.com/",
    actblue_link = "helphillary#",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MI-04 ---------------------------------------------------------------------
  tibble(
    first_name = "John",
    last_name = "Moolenaar",
    state_cd = "MI-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://moolenaar.house.gov/",
    campaign_name = "John Moolenaar for Congress",
    campaign_website = "https://www.johnmoolenaarforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 2 options in the website. First any amount, vs second: 500+.
    other_link = c(
      "https://secure.winred.com/johnmoolenaar/donate",
      "https://secure.winred.com/johnmoolenaar/donate-hd"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Jerry",
    last_name = "Hilliard",
    state_cd = "MI-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jerry Hillard for Congress",
    campaign_website = "https://www.jerryhilliardforcongress.com/",
    actblue_link = "Hilliardforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Amy",
    last_name = "Seplr",
    state_cd = "MI-04",
    party = "Gre",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Amy Seplr for Congress",
    campaign_website = "https://slepr2020.wixsite.com/website",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://www.paypal.com/donate/?cmd=_donations&business=slepr2020@g",
      "mail.com&item_name=Amy%20Slepr%20for%20Congress%C2%A02020&curren",
      "cy_code=USD"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "David",
    last_name = "Canny",
    state_cd = "MI-04",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Canny for US Congress",
    campaign_website = "https://www.facebook.com/DavidC4congress/",
    # cannot find donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MI-05 ---------------------------------------------------------------------
  tibble(
    first_name = "Dan",
    last_name = "Kildee",
    state_cd = "MI-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://dankildee.house.gov/",
    campaign_name = "",
    # cannot find campaign "website"
    campaign_website = "https://www.facebook.com/RepDanKildee",
    actblue_link = "dan-kildee-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00499947",
    FEC_ID_cand = "H2MI05119"
  ),
  tibble(
    first_name = "Tim",
    last_name = "Kelly",
    state_cd = "MI-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tim Kelly for Congress",
    campaign_website = "https://www.timkellyforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Nationbuilder: 25-50-100-250-500-1000
    other_link = c(
      "https://www.timkellyforcongress.com/donate",
      "https://secure.winred.com/timkelly/donate"
    ),
    FEC_ID_cmte = "C00733568",
    FEC_ID_cand = "H0MI05162"
  ),
  tibble(
    first_name = "Kathy",
    last_name = "Goodwin",
    state_cd = "MI-05",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kathy Goodwin for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H8MI05124"
  ),
  # MI-06 ---------------------------------------------------------------------
  # Cannot find Tidwell, Gray withdrew on Jul 6, 2018
  tibble(
    first_name = "Fred",
    last_name = "Upton",
    state_cd = "MI-06",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://upton.house.gov",
    campaign_name = "Upton for All of Us",
    campaign_website = "https://www.fredupton.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/fred-upton/website_donation_2019",
    FEC_ID_cmte = "C00200584",
    FEC_ID_cand = "H6MI04113"
  ),
  tibble(
    first_name = "Jon",
    last_name = "Hoadley",
    state_cd = "MI-06",
    party = "Den",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jon Hoadley for Congress",
    campaign_website = "http://www.jonhoadley.com",
    # How do some candidates secure these generic links?!
    actblue_link = "website-form",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00701599",
    FEC_ID_cand = "H0MI06152"
  ),
  tibble(
    first_name = "Jeff",
    last_name = "DePoy",
    state_cd = "MI-06",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jeff for the 6th",
    campaign_website = "https://www.facebook.com/jefffor6th/",
    # can't find donation links
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # cannot find John Lawrence (Green Party)
  # MI-07 ------------------------------------------
  # Walberg vs. Driskell vs. Smith vs. Branscum vs. Hall
  tibble(
    first_name = "Timothy",
    last_name = "Walberg",
    state_cd = "MI-07",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://walberg.house.gov",
    campaign_name = "Tim Walberg for Congress",
    campaign_website = "https://www.walbergforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/TimWalberg/web",
    FEC_ID_cmte = "C00390724",
    FEC_ID_cand = "H4MI07103"
  ),
  tibble(
    first_name = "Gretchen",
    last_name = "Driskell",
    state_cd = "MI-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gretchen Driskell for Congress",
    campaign_website = "https://votegretchen.com",
    actblue_link = c(
      "gd-website",
      "gd-googled2d-dec2019"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00729947",
    FEC_ID_cand = "H0MI07150"
  ),
  # Christopher Bartholomew Smith withdrew
  # Sam Branscum withdrew
  tibble(
    first_name = "Ryan",
    last_name = "Hall",
    state_cd = "MI-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ryan Hall for Congress",
    campaign_website = "https://www.ryanhallforcongress.com",
    actblue_link = "ryanhallforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00727321",
    FEC_ID_cand = "H0MI07143"
  ),
  # MI-08 ---------------------------------------------------------------------
  # Elissa Slotkin was uncontested (no candidates registered as of Sep 24, '19)
  # Now Slotkin vs. Snyder
  tibble(
    first_name = "Elissa",
    last_name = "Slotkin",
    state_cd = "MI-08",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://slotkin.house.gov",
    campaign_name = "Elissa for Congress",
    campaign_website = "https://elissaforcongress.com/",
    actblue_link = "slotkinforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00650150",
    FEC_ID_cand = "H8MI08102"
  ),
  # https://www.politico.com/story/2019/09/25/gop-cashes-in-on-impeachment-1510714
  # This is not targeted towards Snyder
  tibble(
    first_name = "Nikki",
    last_name = "Snyder",
    state_cd = "MI-08",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Nikki for Congress",
    campaign_website = "http://nikkiforus.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://transaxt.com/Donate/TAMAH2/NikkiSnyderforCongress/",
    FEC_ID_cmte = c("C00721118", "C00721100"),
    FEC_ID_cand = "H0MI08125"
  ),
  tibble(
    first_name = "Paul",
    last_name = "Junge",
    state_cd = "MI-08",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Paul Junge for Congress",
    campaign_website = "https://pauljunge.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/PaulJunge/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Joe",
    last_name = "Hartman",
    state_cd = "MI-08",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "http://joe4congress.vote/?i=1",
    # no donation info
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MI-09 ---------------------------------------------------------------------
  # Andy Levin is uncontested (no candidates registered as of Sep 26, 2019)
  tibble(
    first_name = "Andy",
    last_name = "Levin",
    state_cd = "MI-09",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://andylevin.house.gov",
    campaign_name = "Andy Levin for Congress",
    campaign_website = "https://www.andylevinforcongress.com/",
    actblue_link = "al-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00662619",
    FEC_ID_cand = "H8MI09118"
  ),
  tibble(
    first_name = "Charles",
    last_name = "Langworthy",
    state_cd = "MI-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Langworthy for COngress",
    campaign_website = "https://www.langworthyforcongress.com/",
    actblue_link = NA,
    anedot_link = "langworthy-for-congress/donate",
    other_link = "https://secure.winred.com/langworthy-for-congress/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Mike",
    last_name = "Saliba",
    state_cd = "MI-09",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.facebook.com/ElectMikeSaliba/",
    # Cannot find donation info.
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Andrea",
    last_name = "Kirby",
    state_cd = "MI-09",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.linkedin.com/in/andrea-kirby-836741a5/",
    # Cannot find donation info.
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MI-10 ---------------------------------------------------------------------
  # Paul Mitchell is retiring and webpage seems already down
  # Cannot find Jowske
  tibble(
    first_name = "Kimberly",
    last_name = "Bizon",
    state_cd = "MI-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elect Kimberly Bizon for U.S. Congress",
    campaign_website = "https://www.kimberlybizonforcongress.com",
    actblue_link = c(
      "people-communities-first", # thru website now
      "committee-to-elect-kimberly-bizon-1"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00666594",
    FEC_ID_cand = "H8MI10108"
  ),
  tibble(
    first_name = "Kelly",
    last_name = "Noland",
    state_cd = "MI-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kelly Noland for Congress",
    campaign_website = "https://www.kellynoland2020.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://secure.campaigncontributions.net/80390/",
      "Contribute-to-KELLY-NOLAND-for-CLARITY-in-2020"
    ),
    FEC_ID_cmte = "C00710673",
    FEC_ID_cand = "H0MI10238"
  ),
  tibble(
    first_name = "Richard",
    last_name = "Piwko",
    state_cd = "MI-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Richard A Piwko for Congress",
    campaign_website = "https://rapiwkoforcongress.com",
    actblue_link = NA,
    anedot_link = "richard-a-piwko-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00713941",
    FEC_ID_cand = "H0MI10246"
  ),
  tibble(
    first_name = "Brandon",
    last_name = "Mikula",
    state_cd = "MI-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brandon Mikula For Congress",
    campaign_website = "https://brandonmikulaforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # No suggested amounts anyway
    other_link = paste0(
      "https://www.paypal.com/donate/?token=adaXuNtTMgF4DDtJ9qj7cXhhdhjqUvPp",
      "LuDwNRDcdyTiW0PjRWS8bn6aB-US2jLQBmEykW&country.x=US&locale.x="
    ),
    FEC_ID_cmte = "C00714840",
    FEC_ID_cand = "H0MI10253"
  ),
  tibble(
    first_name = "Lisa",
    last_name = "McClain",
    state_cd = "MI-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lisa McClain for Congress",
    campaign_website = "https://lisamcclainforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      c(
        "https://transaxt.com/Donate/ZU3MNE/LisaMcClainforCongress/", # website
        "https://secure.winred.com/lisa-mcclain-for-congress/donate"
      ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MI-11 ---------------------------------------------------------------------
  tibble(
    first_name = "Haley",
    last_name = "Stevens",
    state_cd = "MI-11",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://stevens.house.gov/",
    campaign_name = "Haley Stevens for Congress",
    campaign_website = "https://www.haleystevensforcongress.com/",
    actblue_link = "hs_website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00638650",
    FEC_ID_cand = "H8MI11254"
  ),
  tibble(
    first_name = "Eric",
    last_name = "Esshaki",
    state_cd = "MI-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Esshaki for Congress",
    campaign_website = "https://www.ericesshaki.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/esshaki-for-congress/website-donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Leonard",
    last_name = "Schwartz",
    state_cd = "MI-11",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Leonard Schwartz for Congress",
    campaign_website = "http://www.leonardschwartz.us/",
    actblue_link = NA,
    anedot_link = NA,
    # leads to this link but it leads to a 404.
    other_link = "http://www.leonardschwartz.us/donate.html",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MI-12 ---------------------------------------------------------------------
  tibble(
    first_name = "Debbie",
    last_name = "Dingell",
    state_cd = "MI-12",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://debbiedingell.house.gov",
    campaign_name = "Debbie Dingell for Congress",
    campaign_website = "http://debbiedingellforcongress.com/",
    actblue_link = "debbie-dingell-1", # actblue as of Oct 18, 2020
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/3736861790810669056",
    FEC_ID_cmte = "C00558213",
    FEC_ID_cand = "H4MI12079"
  ),
  tibble(
    first_name = "Jeff",
    last_name = "Jones",
    state_cd = "MI-12",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jeff Jones for Congress",
    campaign_website = "https://www.facebook.com/JeffJones4Congress/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://www.paypal.com/donate/?cmd=_s-xclick&",
      "hosted_button_id=ACFV55976HUH6&source=url"
    ),
    FEC_ID_cmte = c("C00756171", "C00611269"),
    FEC_ID_cand = "H6MI12264"
  ),
  tibble(
    first_name = "Gary",
    last_name = "Walkowicz",
    state_cd = "MI-12",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gary Walkowicz for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H8MI07039"
  ),
  # MI-13 ---------------------------------------------------------------------
  # Tlaib vs. Patterson
  # but can't find Patterson. Instead... https://patterson2020.us/
  # President?! Maybe de-facto uncontested
  tibble(
    first_name = "Rashida",
    last_name = "Tlaib",
    state_cd = "MI-13",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://tlaib.house.gov",
    campaign_name = "Rashida for Congress",
    campaign_website = "https://www.rashidaforcongress.com/",
    actblue_link = c(
      "2020reelection_homepage", # Oct 19,2020 link
      "rashidaforcongresshomepage",
      "rashidaforcongress",
      "pccc_2018_eoq1_4",
      "bernie-rashida-tlaib"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00668608",
    FEC_ID_cand = "H8MI13250"
  ),
  tibble(
    first_name = "David",
    last_name = "Dudenhoefer",
    state_cd = "MI-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Dudenhoefer for Congress",
    campaign_website = "https://www.dude4congress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/the-committee-to-elect-david-dudenhoefer/donate",
    FEC_ID_cmte = "C00728386",
    FEC_ID_cand = "H0MI13471"
  ),
  tibble(
    first_name = "D. Etta",
    last_name = "Wilcoxon",
    state_cd = "MI-13",
    party = "Gre",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.dettawilcoxon.org/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.dettawilcoxon.org/donate",
    FEC_ID_cmte = c("C00761098", "C00685412"),
    FEC_ID_cand = "H8MI13342"
  ),
  tibble(
    first_name = "Articia",
    last_name = "Bomer",
    state_cd = "MI-13",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Articia Bomer for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00750968", "C00745950"),
    FEC_ID_cand = "H0MI13505"
  ),
  tibble(
    first_name = "Sam",
    last_name = "Johnson",
    state_cd = "MI-13",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sam Johnson for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = "H4MI07012"
  ),
  # MI-14 ---------------------------------------------------------------------
  tibble(
    first_name = "Brenda",
    last_name = "Lawrence",
    state_cd = "MI-14",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://lawrence.house.gov/",
    campaign_name = "Brenda Lawrence for Congress",
    campaign_website = "http://brendalawrence.com/",
    actblue_link = "brendal",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Robert Vance",
    last_name = "Patrick",
    state_cd = "MI-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "R. Vance Patrick for Congress",
    campaign_website = "https://rvancepatrick.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/r-vance-patrick-4-congress",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Lisa",
    last_name = "Giola",
    state_cd = "MI-14",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.lisaforliberty.org/",
    # cannot find donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Philip",
    last_name = "Kolody",
    state_cd = "MI-14",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Philip Kolody for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H8MI14118"
  ),
  # cannot find Clyde Shabazz (Green) and Philip Kolody (Working Class)
  # MN: Minnesota (last updated Jul 12, 2021) ==============================
  # MN-01 ---------------------------------------------------------------------
  tibble(
    first_name = "Jim",
    last_name = "Hagedorn",
    state_cd = "MN-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://hagedorn.house.gov/",
    campaign_name = "Jim Hagedorn for Congress",
    campaign_website = "https://www.jimhagedorn.org/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/jimhagedorn/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Dan",
    last_name = "Feehan",
    state_cd = "MN-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dan Feehan for Congress",
    campaign_website = "https://www.danfeehan.com/",
    actblue_link = "feehan-site",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Bill",
    last_name = "Rood",
    state_cd = "MN-01",
    party = "Gra",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://roodforcongress.com/",
    # cannot find donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MN-02 -----------------------------------------
  # Craig vs. Olson vs. Zeiler vs. Kistner vs. Barr
  tibble(
    first_name = "Angie", # Angela Dawn, goes by Angie
    last_name = "Craig",
    state_cd = "MN-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://craig.house.gov",
    campaign_name = "Angie Craig for Congress",
    campaign_website = "https://angiecraig.com/",
    actblue_link = "angiecraig2018",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00575209",
    FEC_ID_cand = "H6MN02131"
  ),
  tibble(
    first_name = "Rick",
    last_name = "Olson",
    state_cd = "MN-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rick Olson for Congress",
    campaign_website = "https://www.rickolsonforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-1000-2800-5600
    other_link = "https://www.rickolsonforcongress.com/donate",
    FEC_ID_cmte = "C00719039",
    FEC_ID_cand = "H0MN02118"
  ),
  tibble(
    first_name = "Kerry",
    last_name = "Zeiler",
    state_cd = "MN-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kerry for Congress",
    campaign_website = "https://www.kerry4congress.org/",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "token=y5G1SpJ14ffgmNlwHSTRHjOG14eBtHTyI-XrtQBkmK1V52dcaNvwa5Phdb",
      "creKtsoWPe20&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00731281",
    FEC_ID_cand = "H0MN02126"
  ),
  tibble(
    first_name = "Tyler",
    last_name = "Kistner",
    state_cd = "MN-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tyler Kistner for Congress",
    campaign_website = "https://www.tylerkistnerforcongress.com//",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/tylerkistner/donate",
    FEC_ID_cmte = "C00732925",
    FEC_ID_cand = "H0MN02134"
  ),
  tibble(
    first_name = "Regina",
    last_name = "Barr",
    state_cd = "MN-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Regina Barr for Congress",
    campaign_website = "https://reginaforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/regina-for-congress/donate",
    FEC_ID_cmte = "C00734863",
    FEC_ID_cand = "H0MN02142"
  ),
  tibble(
    first_name = "Adam",
    last_name = "Weeks",
    state_cd = "MN-02",
    party = "Leg", # "Legalize Marijuana Now"
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Adam Weeks for Congress",
    campaign_website = "https://www.weeks2020.com/",
    actblue_link = NA,
    anedot_link = "weeks-2020/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Martin",
    last_name = "Schulte",
    state_cd = "MN-02",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Martin Schulte for Congress",
    campaign_website = "https://www.martinvschulte.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0MO02262"
  ),
  # MN-03 ---------------------------------------------------------------------
  tibble(
    first_name = "Kendall",
    last_name = "Qualls",
    state_cd = "MN-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kendall for Congress",
    campaign_website = "https://kendallforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/kendall-for-congress/donate",
    FEC_ID_cmte = "C00713461",
    FEC_ID_cand = "H0MN03124"
  ),
  tibble(
    first_name = "Dean",
    last_name = "Phillips",
    state_cd = "MN-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://phillips.house.gov/",
    campaign_name = "Dean Phillips for Congress",
    campaign_website = "https://phillipsforcongress.org/",
    actblue_link = "pfc-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Leonard",
    last_name = "Steinman",
    state_cd = "MN-03",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Leonard Steinman for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0MO04151"
  ),
  # MN-04 ---------------------------------------------------------------------
  tibble(
    first_name = "Betty",
    last_name = "McCollum",
    state_cd = "MN-04",
    party = "Dem", # Democratic-Farmer-Labor to be exact
    incumbent = TRUE,
    office_website = "https://mccollum.house.gov",
    campaign_name = "McCollum for Congress",
    campaign_website = "https://www.mccollumforcongress.com/",
    actblue_link = "betty-mccollum-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00354688",
    FEC_ID_cand = "H0MN04049"
  ),
  
  tibble(
    first_name = "David",
    last_name = "Sandbeck",
    state_cd = "MN-04",
    party = "Dem", # Democratic-Farmer-Labor to be exact
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sandbeck for Congress",
    campaign_website = "https://sandbeckforcongress.com/",
    actblue_link = "sandbeck",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00694976",
    FEC_ID_cand = "H0MN04171"
  ),
  tibble(
    first_name = "Travis",
    last_name = "Ekbom",
    state_cd = "MN-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Travis Ekbom for Congress",
    campaign_website = "https://travis2020.com/",
    actblue_link = NA,
    anedot_link = "campaign-fund-of-travis-ekbom/donate",
    other_link = NA,
    FEC_ID_cmte = "C00712331",
    FEC_ID_cand = "H0MN04189"
  ),
  tibble(
    first_name = "Gene",
    last_name = "Rechtzigel",
    state_cd = "MN-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gene For Congress",
    campaign_website = "http://www.geneforpeople.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://www.paypal.com/donate?token=Hlgg7Jl_25AuJT",
      "uQptsJpOdWd67wJ6HlyodDEcFsa1-sRmdpLLn5h5tVTPDASRqTR0XuLZQj9Ce6Rq6o"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Sue",
    last_name = "Sindt",
    state_cd = "MN-04",
    party = "Gra", # Grassroots
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sue Sindt for Congress-District 4",
    campaign_website = "https://sindtforcongress.com/",
    # cannot find donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MN-05 ---------------------------------------------------------------------
  # Omar vs. Miller
  # vs. Whaley vs. Stella vs. Johnson vs. Kelley
  # Cannot find Miller
  tibble(
    first_name = "Ilhan",
    last_name = "Omar",
    state_cd = "MN-05",
    party = "Dem", # DFL to be exact. Democratic-Farmer-Labor
    incumbent = TRUE,
    office_website = "https://omar.house.gov",
    campaign_name = "Ilhan for Congress",
    campaign_website = "https://www.ilhanomar.com/",
    actblue_link = c(
      "ilhan_for_congress",
      "ilhan-for-congress",
      "estfbmnio"
      # "io-community",
      # "io-pandemic-profiteers",
      # "ilhan-pac-io",
      # "io-progressive",
      # "io-super-tuesday",
      # "io-march",
      # "io-march-mid-month",
      # "io-march-eoq",
      # "io-february-eom",
      # "io-february-mid-month",
      # "io-february",
      # "2020janiomidmonth",
      # "2020ioquarter1",
      # "2019eoqio",       # Called for 15.95 at one time
      # "quarter4",        # Called for 13.37 at one time
      # "aoc-ilhan-rally", # Split with AOC
      # "io-cancel-student-debt?",
      # "io-people-first",
      # "io-digital-outreach",
      # "io-minimum-wage",
      # "io-medicare-for-all",
      # "io-emergency-fund",
      # "io-emergency-response"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00680934",
    FEC_ID_cand = "H8MN05239"
  ),
  # Website down as of Jan 21, 2020
  # Twitter is live but Facebook also down
  tibble(
    first_name = "Brent",
    last_name = "Whaley",
    state_cd = "MN-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brent Whaley for Congress",
    campaign_website = "https://www.brentwhaleyforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # NationBuilder
    other_link = "https://www.brentwhaleyforcongress.com/contribute",
    FEC_ID_cmte = "C00708701",
    FEC_ID_cand = "H0MN05152"
  ),
  tibble(
    first_name = "Danielle",
    last_name = "Stella",
    state_cd = "MN-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Stella 2020",
    campaign_website = "https://www.stella2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/embed/stella2020",
    FEC_ID_cmte = "C00709493",
    FEC_ID_cand = "H0MN05160"
  ),
  tibble(
    first_name = "Lacy",
    last_name = "Johnson",
    state_cd = "MN-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lacy Johnson for Congress",
    campaign_website = "https://www.lacyjohnson.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      "https://secure.winred.com/lacy-johnson-for-congress/donate"
      # Same link!
      # "https://secure.winred.com/lacy-johnson-for-congress/donate-now-x"
    ),
    FEC_ID_cmte = "C00711689",
    FEC_ID_cand = "H0MN05178"
  ),
  tibble(
    first_name = "Chris",
    last_name = "Kelley",
    state_cd = "MN-05",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chris Kelley for Congress",
    campaign_website = "http://www.votechriskelley.com/",
    actblue_link = NA,
    anedot_link = "chris-kelley-for-congress/donate-page",
    other_link = NA,
    FEC_ID_cmte = "C00713214",
    FEC_ID_cand = "H0MN05186"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Moore",
    state_cd = "MN-05",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Michael Moore for Congress",
    campaign_website = "https://www.usrepmoore.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0MN05293"
  ),
  # MN-06 ---------------------------------------------------------------------
  tibble(
    first_name = "Tom",
    last_name = "Emmer",
    state_cd = "MN-06",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://emmer.house.gov",
    campaign_name = "Emmer for Congress",
    campaign_website = "https://emmerforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/emmer-for-congress",
    FEC_ID_cmte = "C00545749",
    FEC_ID_cand = "H4MN06087"
  ),
  tibble(
    first_name = "Tawnja",
    last_name = "Zahradka",
    state_cd = "MN-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tawnja For Congress",
    campaign_website = "https://www.tawnjaforcongress.com/",
    actblue_link = "tawnja-for-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MN-07 ---------------------------------------------------------------------
  # Novak has no contribution page, and I cannot find Collis
  tibble(
    first_name = "Collin",
    last_name = "Peterson",
    state_cd = "MN-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://collinpeterson.house.gov",
    campaign_name = "Collin Peterson for Congress",
    campaign_website = "http://www.petersonforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/-2140327635745175808",
    FEC_ID_cmte = "C00253187",
    FEC_ID_cand = "H2MN07014"
  ),
  tibble(
    first_name = "Dave",
    last_name = "Hughes",
    state_cd = "MN-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hughes for Congress",
    campaign_website = "https://www.hughesforcongress.us/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.hughesforcongress.us/donate",
    FEC_ID_cmte = "C00610071",
    FEC_ID_cand = "H6MN07304"
  ),
  tibble(
    first_name = "Jayesun",
    last_name = "Sherman",
    state_cd = "MN-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jayesun Sherman for Congress",
    campaign_website = "https://www.jayesun4uscongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://www.jayesun4uscongress.com/checkout/donate?",
      "donatePageId=5a4d1d6bc8302599db015bfc"
    ),
    FEC_ID_cmte = "C00664862",
    FEC_ID_cand = "H8MN07102"
  ),
  tibble(
    first_name = "Stephen",
    last_name = "Emery",
    state_cd = "MN-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Conservative. Democrat. For Minnesota",
    campaign_website = "https://stephenaemery.org/",
    actblue_link = NA,
    anedot_link = "emery/donate",
    other_link = NA,
    FEC_ID_cmte = "C00679464",
    FEC_ID_cand = "H0MN07075"
  ),
  tibble(
    first_name = "Michelle",
    last_name = "Fischbach",
    state_cd = "MN-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Fischbach for Congress",
    campaign_website = "http://www.fischbachforcongress.com/",
    # Change documented to WinRed
    # 5-10-25-50-100-250-500-1000-2800 to 5-10-25-50-100-250
    # Now 5-10-25-50-100-250-500-1000-2800 as before
    # Now includes 5600 as well (Oct 19, 2020)
    actblue_link = NA,
    # anedot_link = "fischbach-for-congress/home",
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/MichelleFischbach/website-donations",
    FEC_ID_cmte = "C00717959",
    FEC_ID_cand = "H0MN07091"
  ),
  tibble(
    first_name = "Rae Hart",
    last_name = "Anderson",
    state_cd = "MN-07",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rae Hart Anderson for Congress",
    campaign_website = "https://www.twitter.com/GodisHere2day4U",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0MN07141"
  ),
  tibble(
    first_name = "Slater",
    last_name = "Johnson",
    state_cd = "MN-07",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Slater Johnson for Congress",
    campaign_website = "https://www.facebook.com/Slater-Johnson-for-District-7-109208377567307/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0MN07166"
  ),
  # MN-08 ---------------------------------------------------------------------
  tibble(
    first_name = "Pete",
    last_name = "Stauber",
    state_cd = "MN-08",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://stauber.house.gov/",
    campaign_name = "Pete Stauber for Congress",
    campaign_website = "https://petestauberforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/pete-stauber-for-congress/donate-today",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Quinn",
    last_name = "Nystorm",
    state_cd = "MN-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Quinn Nystorm for Congress",
    campaign_website = "https://quinnforcongress.com/",
    actblue_link = "quinn-nystrom-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Judith",
    last_name = "Schwartzbacker",
    state_cd = "MN-08",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Judith Schwartzbacker for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0MN08180"
  ),
  # MS: Mississippi (last updated Jul 12, 2021) ===============================
  # MS-01 ---------------------------------------------------------------------
  # Trent Kelly is uncontested (no candidates registered as of Sep 26, 2019)
  tibble(
    first_name = "Trent",
    last_name = "Kelly",
    state_cd = "MS-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://kelly.house.gov",
    campaign_name = "Kelly for Mississippi",
    campaign_website = "https://www.kellyformississippi.com/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://trentkelly.nationbuilder.com/donation",
    # Change documented to WinRed on Oct 8, 2019
    # 25-50-100-250-1000-2500 to 25-50-100-250-500-1000-2800
    # Highest amount increase
    # these might effectively be same links.
    other_link = c(
      "https://secure.winred.com/TrentKelly/donate",
      "https://winred.revv.co/TrentKelly/donate"
    ),
    FEC_ID_cmte = "C00573980",
    FEC_ID_cand = "H6MS01131"
  ),
  tibble(
    first_name = "Antonia",
    last_name = "Eliason",
    state_cd = "MS-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Antonia for Mississippi",
    campaign_website = "https://antonia2020.com/",
    actblue_link = "antonia4ms1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MS-02 ---------------------------------------------------------------------
  # Bennie Thompson is uncontested (no candidates registered as of Sep 26, '19)
  tibble(
    first_name = "Bennie",
    last_name = "Thompson",
    state_cd = "MS-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://benniethompson.house.gov",
    campaign_name = "Bennie Thompson for Congress",
    campaign_website = "http://benniethompson.com",
    actblue_link = "bennie-thompson-3",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00279851",
    FEC_ID_cand = "H4MS02068"
  ),
  tibble(
    first_name = "Brian",
    last_name = "Flowers",
    state_cd = "MS-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Flowers for Congress",
    campaign_website = "http://flowersforcongress.com/",
    # uses the language of "invest"
    actblue_link = NA,
    anedot_link = "flowers-for-congress/donate",
    other_link = "https://secure.winred.com/flowers-for-congress/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MS-03 ---------------------------------------------------------------------
  # Michael Guest is uncontested (no candidates registered as of Sep 26, 2019)
  tibble(
    first_name = "Michael",
    last_name = "Guest",
    state_cd = "MS-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://guest.house.gov",
    campaign_name = "Michael Guest for Congress",
    campaign_website = "https://michaelguest.ms/",
    actblue_link = NA,
    anedot_link = NA,
    # $100 is subtly suggested but no others and not scrapable
    # $2,700 instead of $2,800
    # Could be a good baseline...
    # winred through winred directory
    other_link = c(
      "https://michaelguest.ms/donate/",
      " https://secure.winred.com/michaelguest/donate"
    ),
    FEC_ID_cmte = "C00665752",
    FEC_ID_cand = "H8MS03125"
  ),
  tibble(
    first_name = "Dorothy",
    last_name = "Benford",
    state_cd = "MS-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dorothy Dot Benford for Congress",
    campaign_website = "https://dorothydotbenfordforcongress.rocks/",
    actblue_link = "dorothydotbenford4congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MS-04 ---------------------------------------------------------------------
  # Palazzo won Republican Primary and faces no democratic opposition
  tibble(
    first_name = "Steven",
    last_name = "Palazzo",
    state_cd = "MS-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://palazzo.house.gov/",
    campaign_name = "Palazzo for Congress",
    campaign_website = "https://palazzoforcongress.com/",
    actblue_link = NA,
    anedot_link = "palazzo-for-congress/general-fund-c2c76715b06",
    other_link = "https://secure.winred.com/stevenpalazzo/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MO: Missouri ==============================================================
  # MO-01 (last updated Jul 12, 2020) ------------------------------------------
  # Incumbent Clay defeated by Cori Bush
  # Bush vs. Rogers
  tibble(
    first_name = "William Lacy",
    last_name = "Clay",
    state_cd = "MO-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://lacyclay.house.gov",
    campaign_name = "",
    campaign_website = "http://www.lacyclay.org",
    actblue_link = c(
      # Change within ActBlue; already paused but from source
      # 10-25-50-100-250-5000-1000
      "clay2020",
      "william-lacy-clay--jr-2"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00346080",
    FEC_ID_cand = "H0MO01066"
  ),
  tibble(
    first_name = "Cori",
    last_name = "Bush",
    state_cd = "MO-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cori Bush for Congress",
    campaign_website = "https://coribush.org",
    actblue_link = "supportcori2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00638767",
    FEC_ID_cand = "H8MO01143"
  ),
  tibble(
    first_name = "Anthony",
    last_name = "Rogers",
    state_cd = "MO-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Anthony Rogers 2020",
    # Website is down
    campaign_website = "https://www.anthonyrogers2020.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00742650",
    FEC_ID_cand = "H0MO01207"
  ),
  tibble(
    first_name = "Alex",
    last_name = "Furman",
    state_cd = "MO-01",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Alex Furman for Congress",
    campaign_website = "https://alexforus.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://alexforus.com/donate.html",
    FEC_ID_cmte = "C00754507",
    FEC_ID_cand = "H0MO01215"
  ),
  # MO-02 (last updated Oct 25, 2020) ------------------------------------------
  # Ann Wagner is uncontested (no candidates registered as of Oct 3, 2019)
  # Now Wagner vs. Schupp
  tibble(
    first_name = "Ann",
    last_name = "Wagner",
    state_cd = "MO-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://wagner.house.gov",
    campaign_name = "Ann Wagner for Congress",
    campaign_website = "https://annwagner.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/ann-wagner/donate",
    FEC_ID_cmte = "C00495846",
    FEC_ID_cand = "H2MO02102"
  ),
  tibble(
    first_name = "Jill",
    last_name = "Schupp",
    state_cd = "MO-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Schupp for Congress",
    campaign_website = "https://www.jillschupp.com",
    actblue_link = "schuppwebsite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00729558",
    FEC_ID_cand = "H0MO02254"
  ),
  # MO-03 ---------------------------------------------------------------------
  tibble(
    first_name = "Blaine",
    last_name = "Luetkemeyer",
    state_cd = "MO-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://luetkemeyer.house.gov",
    campaign_name = "Blaine for Congress",
    campaign_website = "https://www.blaineforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/blaine-for-congress/website",
    FEC_ID_cmte = c("C00458679", "C00447748"),
    FEC_ID_cand = "H8MO09153"
  ),
  tibble(
    first_name = "Katy",
    last_name = "Geppert",
    state_cd = "MO-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://katygeppertforcongress.com",
    # Not a standalone actblue: divided btw 14 groups. Um... 11k out of this?
    actblue_link = "oustwallstreetflunkies",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00665216",
    FEC_ID_cand = "H8MO03198"
  ),
  tibble(
    first_name = "Megan",
    last_name = "Rezabek",
    state_cd = "MO-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Megan Rezabek for Congress",
    campaign_website = "https://www.meganrezabekforcongress.com/",
    # From: https://www.meganrezabekforcongress.com/get-involved
    # I will not be accepting donations. Thank you for understanding
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # cannot find Leonard Steinman (Lib)
  # MO-04 ---------------------------------------------------------------------
  tibble(
    first_name = "Vicky",
    last_name = "Hartzler",
    state_cd = "MO-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://hartzler.house.gov/",
    campaign_name = "Vicky Hartzler for Congress",
    campaign_website = "https://www.vickyhartzler.com/",
    actblue_link = NA,
    anedot_link = NA,
    # empty box on website. WinRed is from WinRed directory.
    other_link = c(
      "https://www.vickyhartzler.com/donate",
      "https://secure.winred.com/vickyhartzler/donate"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Lindsey",
    last_name = "Simmons",
    state_cd = "MO-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lindsey Simmons for US Congress",
    campaign_website = "https://lindseysimmons.com/",
    actblue_link = "calledtoserve",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Steven",
    last_name = "Koonse",
    state_cd = "MO-04",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    # cannot find additional info on Steven Koonse
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MO-05 ---------------------------------------------------------------------
  tibble(
    first_name = "Emanuel",
    last_name = "Cleaver",
    state_cd = "MO-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://cleaver.house.gov/",
    campaign_name = "Cleaver for Congress",
    campaign_website = "https://www.cleaverforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      paste0(
        "https://www.campaigncontribution.com/payment/contribution",
        "/info/57fe9544-1eb0-4626-bbf2-786b57def75f"
      ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Ryan",
    last_name = "Derks",
    state_cd = "MO-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ryan Derks for Congress",
    campaign_website = "https://ryanderks.com/",
    actblue_link = NA,
    anedot_link = "derks-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Robin",
    last_name = "Dominick",
    state_cd = "MO-05",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Robin Dominick for Missouri",
    campaign_website = "https://www.robindominick.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 10-20-25-50
    other_link = "https://www.robindominick.com/donations",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # MO-06 ---------------------------------------------------------------------
  tibble(
    first_name = "Sam",
    last_name = "Graves",
    state_cd = "MO-06",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://graves.house.gov/",
    campaign_name = "Graves for Congress",
    campaign_website = "https://gravesforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/samgraves/donate-2020",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Gena",
    last_name = "Ross",
    state_cd = "MO-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dr. Gena Ross for US Congress",
    campaign_website = "https://www.rossforuscongress.net/",
    actblue_link = "rossforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # cannot find Jim Higgins (Lib)
  # MO-07 ---------------------------------------------------------------------
  tibble(
    first_name = "Billy",
    last_name = "Long",
    state_cd = "MO-07",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://long.house.gov/",
    campaign_name = "Billy Long for Congress",
    campaign_website = "https://www.billylongforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # First link is the one through website. Second through WinRed directory
    other_link =
      c(
        "https://www.billylongforcongress.com/donate",
        "https://secure.winred.com/billy-long-for-congress/donate-now"
      ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Teresa",
    last_name = "Montseny",
    state_cd = "MO-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Teresa Montseny for Congress",
    campaign_website = "http://www.montsenyforcongress.com/",
    actblue_link = "montseny-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Kevin",
    last_name = "Craig",
    state_cd = "MO-07",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Craig for Congress",
    campaign_website = "http://www.kevincraig.us/",
    # not taking campaign donations
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Audrey",
    last_name = "Richards",
    state_cd = "MO-07",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Audrey Richards for Congress",
    campaign_website = "https://www.audreyformissouri.com/",
    # not taking campaign donations
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=3DURD982K4672&source=url",
    FEC_ID_cmte = "C00753541",
    FEC_ID_cand = "H0MO07279"
  ),
  # MO-08 ------------------------------------------
  # Smith vs. Ellis
  tibble(
    first_name = "Jason",
    last_name = "Smith",
    state_cd = "MO-08",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://jasonsmith.house.gov",
    campaign_name = "Jason Smith for Congress",
    campaign_website = "https://electjasonsmith.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/JasonSmith/donate",
    FEC_ID_cmte = "C00541862",
    FEC_ID_cand = "H4MO08162"
  ),
  tibble(
    first_name = "Kathy", # Kathryn, goes by Kathy
    last_name = "Ellis", # "Gail",
    state_cd = "MO-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Katy Ellis for Congress",
    campaign_website = "https://www.ellisforcongress.com",
    actblue_link = "ellisforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00701730", "C00639450"),
    FEC_ID_cand = "H8MO08098"
  ),
  tibble(
    first_name = "Tom",
    last_name = "Schmitz",
    state_cd = "MO-08",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tom Schmitz for Liberty",
    campaign_website = "https://www.tomschmitzforliberty.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0MO08285"
  ),
  # MT: Montana (at-large district; last updated Oct 25, 2020) =================
  # Gianforte vs. Williams
  # vs. Rosendale vs. Winter vs. Stapleton vs. Dooling vs. Rains vs. Johnson
  # vs. Lamm
  tibble(
    first_name = "Kathleen",
    last_name = "Williams",
    state_cd = "MT-0",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Williams for Montana",
    campaign_website = "https://kathleenformontana.com/",
    actblue_link = "1904-web-kw",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00701748", "C00659177"),
    FEC_ID_cand = "H8MT01232"
  ),
  tibble(
    first_name = "Matt",
    last_name = "Rosendale",
    state_cd = "MT-0",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Matt Rosendale for Montana",
    campaign_website = "https://www.mattformontana.com/",
    actblue_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from Jan 22, 2020
    # 10-25-50-100-250-500-1000-2800-5600
    # to 10-25-50-100-250-500-1000-2800-5600 (same amounts)
    anedot_link = "mattformontana/home-donate",
    other_link = "https://secure.winred.com/MattRosendale/donate",
    FEC_ID_cmte = "C00548289",
    FEC_ID_cand = "H4MT00050"
  ),
  # Running for governor
  tibble(
    first_name = "Greg",
    last_name = "Gianforte",
    state_cd = "MT-0",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://gianforte.house.gov",
    campaign_name = "Greg for Montana",
    campaign_website = "https://gregformontana.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to Revv on Mar 1, 2020
    # from 10-25-50-75-100-250-500-710-1420 (!?!)
    #   to 45-50-100-250-500-710-1000-1420 (!?!!)
    # Maximum contribution is $710 per person/per election
    # or $1,420 for an individual and $2,840 per couple.
    # Contributions above $710 will be attributed to donor and spouse
    # if allowable. State Law requires us to gather the name, address,
    # occupation and employer of anyone contributing more than $35.
    # Contributions from Corporations are prohibited.
    other_link = paste0(
      # "https://donate.gregformontana.com/donation1/?",
      # "initiativekey=NL6WQARTEXG5&rev=80"
      "https://greg-for-montana.revv.co/grassroots-donate2"
    ),
    FEC_ID_cmte = "C00631945",
    FEC_ID_cand = "H8MT01182"
  ),
  tibble(
    first_name = "Tom", # Thomas, goes by Tom
    last_name = "Winter",
    state_cd = "MT-0",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tom Winter for Montana",
    campaign_website = "https://www.winterformontana.com/",
    actblue_link = "tomwinter",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00700963",
    FEC_ID_cand = "H0MT00090"
  ),
  tibble(
    first_name = "Corey",
    last_name = "Stapleton", # Currently Montana SoS
    state_cd = "MT-0",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://coreystapleton.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Mar 1, 2020
    # But because previous link was Revv, essentially same thing
    # other_link = "https://coreystapleton.revv.co/donate",
    other_link = "https://secure.winred.com/CoreyStapleton/donate",
    FEC_ID_cmte = "C00709915",
    FEC_ID_cand = "H4MT01033"
  ),
  tibble(
    first_name = "Joe",
    last_name = "Dooling",
    state_cd = "MT-0",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joe Dooling for Congress",
    campaign_website = "http://www.joedooling.com/",
    actblue_link = NA,
    anedot_link = "joe-dooling-for-u-s-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00709923",
    FEC_ID_cand = "H0MT00108"
  ),
  tibble(
    first_name = "Debra",
    last_name = "Lamm",
    state_cd = "MT-0",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Debra Lamm for Congress",
    campaign_website = "https://debralamm.com/",
    actblue_link = NA,
    # Change documented to WinRed on Mar 1, 2020
    # from 2800-1000-500-250-100-50
    #   to 25-50-100-250-500-1000-2800
    # anedot_link = "lamm-for-congress/donate",
    aneodot_link = NA,
    other_link = "https://secure.winred.com/debralamm/donate",
    FEC_ID_cmte = "C00718585",
    FEC_ID_cand = "H0MT01098"
  ),
  # Rains withdrew
  tibble(
    first_name = "Matt",
    last_name = "Rains",
    state_cd = "MT-0",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rains for Montana",
    campaign_website = "https://rainsformontana.com/",
    actblue_link = "rains_website_donate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710509",
    FEC_ID_cand = "H0MT00116"
  ),
  tibble(
    first_name = "Tim",
    last_name = "Johnson",
    state_cd = "MT-0",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.timjohnsonformontana.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 5-10-20
    other_link = paste0(
      "https://www.paypal.com/donate/?token=eiK6K_UZKTWdbyEh7YwpkXbn2",
      "vMAbr-ZfZbOnQWg79dEiLJ4fYsSjo3OKc84pFtvL5V_FG&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00711002",
    FEC_ID_cand = "H0MT01080"
  ),
  # There's only FB for John Evankovich
  # NE: Nebraska ==============================================================
  # NE-01 ---------------------------------------------------------------------
  tibble(
    first_name = "Jeff",
    last_name = "Fortenberry",
    state_cd = "NE-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://fortenberry.house.gov",
    campaign_name = "Jeff Fortenberry for United States Congress",
    campaign_website = "https://www.supportfort.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/JeffFortenberry/donate",
    FEC_ID_cmte = "C00395467",
    FEC_ID_cand = "H4NE01064"
  ),
  tibble(
    first_name = "Barbara",
    last_name = "Ramsey",
    state_cd = "NE-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Babs Ramsey for Congress",
    campaign_website = "https://www.babsforcongress.com",
    actblue_link = "babsforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00716506",
    FEC_ID_cand = "H0NE01112"
  ),
  tibble(
    first_name = "Kate",
    last_name = "Bolz",
    state_cd = "NE-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kate Bolz for Congress",
    campaign_website = "https://katebolz.com",
    actblue_link = "kate-bolz",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00721472",
    FEC_ID_cand = "H0NE01120"
  ),
  tibble(
    first_name = "Dennis",
    last_name = "Grace",
    state_cd = "NE-01",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dennis Grace for US Congress",
    campaign_website = "https://electdennisgrace.com/",
    actblue_link = NA,
    anedot_link = NA,
    # paypal donate $20 option
    other_link =
      paste0(
        "https://www.paypal.com/webapps/hermes?token=7FG9",
        "0794678349615&useraction=commit&rm=0&mfid=1603399349485_2b1966b50b7f4"
      ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # NE-02 ---------------------------------------------------------------------
  tibble(
    first_name = "Don",
    last_name = "Bacon",
    state_cd = "NE-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://bacon.house.gov",
    campaign_name = "Don Bacon for Congress",
    campaign_website = "https://donjbacon.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/donbacon/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Kara",
    last_name = "Eastman",
    state_cd = "NE-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kara Eastman for Congress",
    campaign_website = "https://www.eastmanforcongress.com/",
    actblue_link = "eastmanforcongressweb",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Tyler",
    last_name = "Schaeffer",
    state_cd = "NE-02",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tyler Schaeffer for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0NC11282"
  ),
  # NE-03 ---------------------------------------------------------------------
  tibble(
    first_name = "Adrian",
    last_name = "Smith",
    state_cd = "NE-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://adriansmith.house.gov/",
    campaign_name = "Adrian Smith for Congress",
    campaign_website = "https://www.joinadrian.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/AdrianSmith/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Mark",
    last_name = "Elworth",
    state_cd = "NE-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Elworth Jr for US Representatives Nebraska D3",
    # cannot find a website but this is the twitter.
    campaign_website = "https://twitter.com/markelworthjr",
    # found searching for a ActBlue link
    actblue_link = "mark-elworth-jr-for-us-representatives-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # cannot find Dustin C. Hobbs (Lib)
  # NV: Nevada ================================================================
  # NV-01 ---------------------------------------------------------------------
  # Dina Titus is uncontested (no candidates registered as of Sep 26, 2019)
  tibble(
    first_name = "Dina",
    last_name = "Titus",
    state_cd = "NV-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://titus.house.gov",
    campaign_name = "Titus for Congress",
    campaign_website = "http://www.dinatitus.com/",
    actblue_link = "titus-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00499467",
    FEC_ID_cand = "H8NV03036"
  ),
  tibble(
    first_name = "Joyce",
    last_name = "Bentley",
    state_cd = "NV-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joyce Bentley for Congress",
    campaign_website = "http://joycebentley.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      paste0(
        "https://www.paypal.com/donate?token=r7huJ8jHl7vLDbK",
        "FH8nB2Otn2cCDGX4azat8BkozvxiPWm-Z1OPJibkb4CklCKS-gE2rGdQnSAqtkZoW"
      ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Robert",
    last_name = "Van Strawder",
    state_cd = "NV-01",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Robert Van Strawder, Jr for Congress",
    campaign_website = "https://www.vegasdonforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 10-50-100-500-1000
    other_link =
      "https://www.vegasdonforcongress.com/i-wish-to-make-a-donation/",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Kamau",
    last_name = "Bakari",
    state_cd = "NV-01",
    party = "IAP", # Independent American Party
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elect Kamau Bakari",
    campaign_website = "https://electbakari.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 50-100-250-500 on both links but former says "minimum $5" before default
    # while the latter "Maximum $2,700".
    other_link = c(
      "https://secure.qgiv.com/for/5fkfc",
      "https://secure.qgiv.com/for/ekb"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Joseph",
    last_name = "Maridon",
    state_cd = "NV-01",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joseph Maridon for Congress",
    campaign_website = "https://www.josephmaridon4nevada.com/",
    # cannot find donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # NV-02 ---------------------------------------------------------------------
  tibble(
    first_name = "Mark",
    last_name = "Amodei",
    state_cd = "NV-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://amodei.house.gov",
    campaign_name = "Amodei for Nevada",
    campaign_website = "https://amodeifornevada.com/",
    actblue_link = NA,
    anedot_link = "amodei-for-nevada/caeb00811fac21d5219dd",
    # Actually, the link says send it by post now!
    # Oct 2020: gives option to send it by mail
    other_link = NA,
    FEC_ID_cmte = "C00496760",
    FEC_ID_cand = "H2NV02395"
  ),
  tibble(
    first_name = "Clint",
    last_name = "Koble",
    state_cd = "NV-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Koble for Congress",
    campaign_website = "https://www.kobleforcongress.com/",
    actblue_link = "clintkoble-q32019",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00666420",
    FEC_ID_cand = "H8NV02095"
  ),
  tibble(
    first_name = "Patricia",
    last_name = "Ackerman",
    state_cd = "NV-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Patricia Ackerman for Congress",
    campaign_website = "https://www.ackerman4congress.com/",
    actblue_link = "ackerman4congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Janine",
    last_name = "Hansen",
    state_cd = "NV-02",
    party = "IAP", # Independent American Party
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Janine Hansen for Congress",
    campaign_website = "https://www.janinehansen.com/",
    # no donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # cannot find Richard J. Dunn III	(Ind)
  # NV-03 -----------------------------------------
  tibble(
    first_name = "Susie",
    last_name = "Lee",
    state_cd = "NV-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://susielee.house.gov",
    campaign_name = "Susie Lee for Congress",
    campaign_website = "https://susieleeforcongress.com/",
    actblue_link = "susielee",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00655613",
    FEC_ID_cand = "H6NV04020"
  ),
  tibble(
    first_name = "Dan",
    last_name = "Schwartz",
    state_cd = "NV-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dan Schwartz for Congress",
    campaign_website = "https://dan4nevada.com",
    actblue_link = NA,
    anedot_link = "friends-of-dan-schwartz/website-splash",
    other_link = NA,
    FEC_ID_cmte = "C00711945",
    FEC_ID_cand = "H0NV03116"
  ),
  tibble(
    first_name = "Dan",
    last_name = "Rodimer",
    state_cd = "NV-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dan Rodimer for Congress",
    campaign_website = "https://danrodimer.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/DanRodimer/donate",
    FEC_ID_cmte = "C00717322",
    FEC_ID_cand = "H0NV03132"
  ),
  # Can only find instagarm for Zachary Walkerlieb at
  # https://www.instagram.com/zachfornevada/
  tibble(
    first_name = "Zach", # Zachary, goes by Zach
    last_name = "Walkerlieb",
    state_cd = "NV-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00703025",
    FEC_ID_cand = "H0NV03108"
  ),
  tibble(
    first_name = "Tiger",
    last_name = "Helgelien",
    state_cd = "NV-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tiger for Congress",
    campaign_website = "http://www.tigerforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/tigerforcongress/donate",
    FEC_ID_cmte = "C00713313",
    FEC_ID_cand = "H0NV03124"
  ),
  tibble(
    first_name = "Corwin",
    last_name = "Newberry",
    state_cd = "NV-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Newberry for Congress",
    campaign_website = "http://newberryforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "token=GAivzUcaWwNA3hC5mIgA-lrpbQYMuijjU7Vk0fb2O_3PTdj",
      "wssLKhivXUAvXhNKPcNgLiW&country.x=US&locale.x=US&Z3JncnB0="
    ),
    FEC_ID_cmte = "C00724013",
    FEC_ID_cand = "H0NV03140"
  ),
  tibble(
    first_name = "Steve",
    last_name = "Brown",
    state_cd = "NV-03",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Steve Brown for Nevada",
    campaign_website = "https://www.stevefornevada.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://politics.raisethemoney.com/libertarian-",
      "party-of-clark-county-nvKVqiS0g-W-zIw-v_ZN2pDQ"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Ed",
    last_name = "Bridges",
    state_cd = "NV-03",
    party = "IAP",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ed Bridges for US Congress",
    campaign_website = "https://www.edbridgesforcongress.com/",
    # asking for $20 donations to be mailed.
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # Alex Pereszlenyi's website expired at https://ap4congress.com
  # Richard Hart seems residue of unsuccessful campaign in 2018
  # NV-04 ---------------------------------------------------------------------
  tibble(
    first_name = "Steven",
    last_name = "Horsford",
    state_cd = "NV-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://horsford.house.gov",
    campaign_name = "Steven Horsford for Congress",
    campaign_website = "https://www.stevenhorsford.com",
    # actblue link changed to "shwebsite" but amounts are the same.
    actblue_link = c("teamsh", "shwebsite"),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00668228",
    FEC_ID_cand = "H2NV04011"
  ),
  tibble(
    first_name = "Jim",
    last_name = "Marchant",
    state_cd = "NV-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marchant For Congress",
    campaign_website = "https://jimmarchant.com",
    actblue_link = NA,
    anedot_link = "marchant/website-splash",
    other_link = "https://secure.winred.com/jimmarchant/donate",
    FEC_ID_cmte = "C00694711",
    FEC_ID_cand = "H0NV04015"
  ),
  tibble(
    first_name = "Sam",
    last_name = "Peters",
    state_cd = "NV-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sam Peters for Congress",
    campaign_website = "https://www.sampeters4congress.com",
    actblue_link = NA,
    anedot_link = "committee-to-elect-sam-peters/c22708c2710e1031741d9",
    other_link = NA,
    FEC_ID_cmte = "C00700898",
    FEC_ID_cand = "H0NV04023"
  ),
  tibble(
    first_name = "Charles",
    last_name = "NAvarro",
    state_cd = "NV-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Navarro for Nevada",
    campaign_website = "https://www.navarrofornevada.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/CharlesNavarro/donate",
    FEC_ID_cmte = "C00707661",
    FEC_ID_cand = "H0NV04031"
  ),
  tibble(
    first_name = "Randi",
    last_name = "Reed",
    state_cd = "NV-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Randi Reed for Congress",
    campaign_website = "https://www.randireednv.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/RandiReed/donate-grassroots/",
    FEC_ID_cmte = "C00714105",
    FEC_ID_cand = "H0NV04072"
  ),
  tibble(
    first_name = "Lisa Song",
    last_name = "Sutton",
    state_cd = "NV-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lisa Song Sutton for Congress",
    campaign_website = "https://www.lisa4nevada.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/lisasongsutton/donate_2020",
    FEC_ID_cmte = "C00711077",
    FEC_ID_cand = "H0NV04056"
  ),
  tibble(
    first_name = "Leo",
    last_name = "Blundo",
    state_cd = "NV-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Leo Blundo for Nevada",
    campaign_website = "https://www.blundofornevada.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/LeoBlundo/donate",
    FEC_ID_cmte = "C00713487",
    FEC_ID_cand = "H0NV04064"
  ),
  tibble(
    first_name = "Catherine",
    last_name = "Prato",
    state_cd = "NV-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nurse Prato for Nevada",
    campaign_website = "https://nurseprato.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/catherine-prato-for-congress/donate",
    FEC_ID_cmte = "C00715284",
    FEC_ID_cand = "H0NV04080"
  ),
  tibble(
    first_name = "Rebecca",
    last_name = "Wood",
    state_cd = "NV-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rebecca Wood for Congress",
    campaign_website = "https://rebeccawood.us",
    actblue_link = NA,
    anedot_link = NA,
    # This is a revv link
    other_link = "https://secure.rebeccawood.us/donate-now",
    FEC_ID_cmte = "C00728964",
    FEC_ID_cand = "H0NV04114"
  ),
  tibble(
    first_name = "Jonathan Royce",
    last_name = "Esteban",
    state_cd = "NV-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Esteban for Congress",
    campaign_website = "https://www.jonathanroyceesteban.com",
    actblue_link = NA,
    anedot_link = NA,
    # 1-4-20-50-100 This does not show until you enter an email
    other_link = paste0(
      "https://www.jonathanroyceesteban.com/checkout/donate?",
      "donatePageId=5db9ccc647be7351aee83ab5"
    ),
    FEC_ID_cmte = "C00709345",
    FEC_ID_cand = "H0NV04049"
  ),
  tibble(
    first_name = "Jonathan Roy",
    last_name = "Esteban",
    state_cd = "NV-04",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Esteban for Congress",
    campaign_website = "https://www.jonathanroyceesteban.com/",
    actblue_link = NA,
    anedot_link = "esteban-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Barry",
    last_name = "Rubinson",
    state_cd = "NV-04",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Barry Rubinson for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = "H0NV04171"
  ),
  # NH: New Hampshire ==============================
  # NH-01 ---------------------------------------------------------------------
  # Chris Pappas is uncontested (no candidates registered as of Oct 5, 2019)
  tibble(
    first_name = "Chris",
    last_name = "Pappas",
    state_cd = "NH-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://pappas.house.gov",
    campaign_name = "Chris Pappas for Congress",
    campaign_website = "https://www.chrispappas.org/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.ngpvan.com/v1/Forms/lZk2sknDFEaf2y9mFq1piQ2",
    FEC_ID_cmte = "C00660464",
    FEC_ID_cand = "H8NH01210"
  ),
  tibble(
    first_name = "Matt",
    last_name = "Mowers",
    state_cd = "NH-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mowers for Congress",
    campaign_website = "https://mattmowers.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/mowers-for-congress/donate",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Zachary",
    last_name = "Dumont",
    state_cd = "NH-01",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dumont for Congress",
    campaign_website = "https://www.dumontnh.com/",
    actblue_link = NA,
    anedot_link = "dumont/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # NH-02 ---------------------------------------------------------------------
  # Kuster vs. Negron
  tibble(
    first_name = "Annie",
    last_name = "Kuster",
    state_cd = "NH-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://kuster.house.gov",
    campaign_name = "Annie Kuster for Congress",
    campaign_website = "http://www.kusterforcongress.com",
    actblue_link = "annmclanekuster",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00462861",
    FEC_ID_cand = "H0NH02181"
  ),
  tibble(
    first_name = "Steve",
    last_name = "Negron",
    state_cd = "NH-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Steve Negron for Congress",
    campaign_website = "https://negron4congress.com",
    actblue_link = NA,
    # latter is the newer link
    anedot_link = c("negron/donate", "negron/looking_for_a_winner"),
    other_link =
      "https://secure.winred.com/steve-negron-for-congress/donate",
    FEC_ID_cmte = "C00657981",
    FEC_ID_cand = "H8NH02275"
  ),
  tibble(
    first_name = "Andrew",
    last_name = "Olding",
    state_cd = "NH-02",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Andrew Olding for NH",
    campaign_website = "https://olding4nh.com/",
    # cannot find donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # NJ: New Jersey ============================================================
  # NJ-01 ---------------------------------------------------------------------
  tibble(
    first_name = "Donald",
    last_name = "Norcross",
    state_cd = "NJ-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://norcross.house.gov/",
    campaign_name = "Donald Norcross for Congress",
    campaign_website = "https://www.donaldnorcrossforcongress.com/",
    actblue_link = "norcrossfornj",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Claire",
    last_name = "Gustafson",
    state_cd = "NJ-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Claire Gustafson for Congress",
    campaign_website = "https://gustafson2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/clairegustafson/donation",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # NJ-02 ---------------------------------------------------------------------
  tibble(
    first_name = "Jeff",
    last_name = "Van Drew",
    state_cd = "NJ-02",
    party = "Rep", # switched to Rep
    incumbent = TRUE,
    office_website = "https://vandrew.house.gov",
    campaign_name = "Van Drew for Congress",
    campaign_website = "https://vandrewforcongress.com/",
    # Woops, I accidentally put him down as a Democrat all this while... wait
    # He actually was a Democrat. Switched to Republican on Jan 7, 2020
    # Now this will be a headache in the dataset...
    # Has been on WinRed at least since Dec 20, 2019
    actblue_link = NA, # "vandrew",
    anedot_link = NA,
    other_link = "https://secure.winred.com/van-drew-for-congress/donate",
    FEC_ID_cmte = "C00661868",
    FEC_ID_cand = "H8NJ02166"
  ),
  tibble(
    first_name = "Bob",
    last_name = "Patterson",
    state_cd = "NJ-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bob Patterson for Congress",
    campaign_website = "https://bobpattersonforcongress.com/",
    actblue_link = NA,
    anedot_link = "patterson-for-congress/contribute",
    other_link = NA,
    FEC_ID_cmte = "C00590810",
    FEC_ID_cand = "H0NJ02148"
  ),
  tibble(
    first_name = "David",
    last_name = "Richter",
    state_cd = "NJ-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Richter for Congress",
    campaign_website = "https://richter2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://transaxt.com/Donate/MW7RRY/DavidRichterforCongressInc/",
    FEC_ID_cmte = "C00715466",
    FEC_ID_cand = "H0NJ02155"
  ),
  tibble(
    first_name = "Brian",
    last_name = "Fitzherbert",
    state_cd = "NJ-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Fitzherbert for Congress",
    campaign_website = "https://www.fitzherbertforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/BrianFitzherbert/donate",
    FEC_ID_cmte = "C00713818",
    FEC_ID_cand = "H8NJ02182"
  ),
  tibble(
    first_name = "Amy",
    last_name = "Kennedy",
    state_cd = "NJ-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Amy Kennedy for Congress",
    campaign_website = "https://amykennedyforcongress.com/",
    actblue_link = "alk_web200106-launch",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Jesse",
    last_name = "Ehrnstrom",
    state_cd = "NJ-02",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jesse Ehrnstrom For US Congress",
    campaign_website = "http://jesse4congress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 5-20-50
    other_link =
      paste0(
        "https://www.paypal.com/donate?token=M0bNgHPsgGkO-xPIoQ6MPZ",
        "jMCziyX_Fa-x3D9pV_r7qpZ3_l_5I182Lq1unI7B9GvQhEkbawTDa5zoqc"
      ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Jenna",
    last_name = "Harvey",
    state_cd = "NJ-02",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jenna Harvey for South Jersey",
    campaign_website = "http://jennaforsj.com/",
    # no donation links
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # NJ-03 -----------------------------------------
  # Andy Kim is uncontested (no candidates registered as of Sep 24, 2019)
  # Now Kim vs. Richter vs. Gibbs vs. Novak (last updated Feb 12, 2020)
  tibble(
    first_name = "Andy",
    last_name = "Kim",
    state_cd = "NJ-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://kim.house.gov",
    campaign_name = "Andy Kim for Congress",
    campaign_website = "https://andykimforcongress.com/",
    actblue_link = "ak_wsite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00648220",
    FEC_ID_cand = "H8NJ03206"
  ),
  # Actually, it turned out that he moved from NJ-02 to NJ-03
  # So I do have all the data.
  # https://www.pressofatlanticcity.com/news/richter-exits-nd-district-race-will-run-for-congress-in/article_9fd9082c-d83c-515e-b66e-03fa85a47584.html
  tibble(
    first_name = "David",
    last_name = "Richter",
    state_cd = "NJ-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Richter for Congress",
    campaign_website = "https://richter2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      # WinRed when registered info, but found previous link from Archives
      # "https://transaxt.com/Donate/MW7RRY/DavidRichterforCongressInc/",
      # Unfortunately, no longer active
      # https://web.archive.org/web/20200124124940/https://transaxt.com/Donate/MW7RRY/DavidRichterforCongressInc/
      # says 20-50-100-250-500-1000-2800-5600
      # New WinRed is also 20-50-100-250-500-1000-2800-5600
      "https://secure.winred.com/davidrichter/donate"
    ),
    FEC_ID_cmte = "C00715466",
    FEC_ID_cand = "H0NJ02155"
  ),
  tibble(
    first_name = "Kate",
    last_name = "Gibbs",
    state_cd = "NJ-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kate Gibbs for U.S. Congress",
    campaign_website = "https://katefornj.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/KateGibbs/donate",
    FEC_ID_cmte = "C00727958",
    FEC_ID_cand = "H0NJ03211"
  ),
  tibble(
    first_name = "John",
    last_name = "Novak",
    state_cd = "NJ-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John Novak Republican for Congress",
    campaign_website = "https://johnnovaknj.com/",
    actblue_link = NA,
    anedot_link = "novak-for-congress/donate",
    other_link = "https://johnnovaknj.com/donate/",
    FEC_ID_cmte = "C00726190",
    FEC_ID_cand = "H0NJ03203"
  ),
  tibble(
    first_name = "Martin",
    last_name = "Weber",
    state_cd = "NJ-03",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Martin Wever for Congress",
    campaign_website = "https://www.martinweberforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-250-500-1000
    other_link = "https://www.martinweberforcongress.com/contribute/",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Robert",
    last_name = "Shapiro",
    state_cd = "NJ-03",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Robert Shapiro for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H6NJ12235"
  ),
  # NJ-04 ---------------------------------------------------------------------
  tibble(
    first_name = "Chris",
    last_name = "Smith",
    state_cd = "NJ-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://chrissmith.house.gov/",
    campaign_name = "Friends of Chris Smith",
    campaign_website = "https://smith4nj.com/",
    actblue_link = NA,
    anedot_link = NA,
    # empty box type
    other_link = c(
      paste0(
        "https://www.campaigncontribution.com/payment/contri",
        "bution/info/6be33664-4500-45dd-a593-b2a47d017538"
      ),
      "https://secure.winred.com/chrissmith/donate"
    ),
    # but has a WinRed link in WinRed directory
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Stephanie",
    last_name = "Schmid",
    state_cd = "NJ-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Stephanie for New Jersey",
    campaign_website = "https://www.stephaniefornewjersey.com/",
    actblue_link = "founding-donor-nj-04",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Michael",
    last_name = "Rufo",
    state_cd = "NJ-04",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Michael Rufo for Congress",
    campaign_website = "https://runrufo2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://www.paypal.com/donate/?cmd=_s-xclick&hosted_",
      "button_id=J7UR2HCYAJNN4"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Andrew",
    last_name = "Pachuta",
    state_cd = "NJ-04",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Andrew Pachuta for Congress",
    campaign_website = "https://www.votepachuta.com/",
    actblue_link = NA,
    anedot_link = NA,
    # donation link just a Wix template; does not work
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Hank",
    last_name = "Schroeder",
    state_cd = "NJ-04",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vote Hank",
    campaign_website = "https://www.facebook.com/pg/votehanknj/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H6NJ04182"
  ),
  # NJ-05 -----------------------------------------
  tibble(
    first_name = "Josh",
    last_name = "Gottheimer",
    state_cd = "NJ-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://gottheimer.house.gov",
    campaign_name = "Josh Gottheimer for Congress",
    campaign_website = "https://josh4congress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 500-1000-2800-5600-11200 (!!!)
    # looks like he removed the 11200 option in the latest link
    # later link is latest.
    other_link = c(
      "https://act.myngp.com/Forms/5982318022785107968",
      "https://act.myngp.com/Forms/-6653157846444405760"
    ),
    FEC_ID_cmte = "C00573949",
    FEC_ID_cand = "H6NJ05171"
  ),
  tibble(
    first_name = "Mike",
    last_name = "Ghassali",
    state_cd = "NJ-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "MIke Ghassali for Congress",
    campaign_website = "https://ghassaliforcongress.com",
    actblue_link = NA,
    anedot_link = "mike-ghassali-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00710483",
    FEC_ID_cand = "H0NJ05190"
  ),
  tibble(
    first_name = "Frank",
    last_name = "Pallotta",
    state_cd = "NJ-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Pallotta for Congress",
    campaign_website = "https://www.pallottaforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/FrankPallotta/donate",
    FEC_ID_cmte = "C00706275",
    FEC_ID_cand = "H0NJ05174"
  ),
  tibble(
    first_name = "Arati",
    last_name = "Kreibich",
    state_cd = "NJ-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Arati for Congress",
    campaign_website = "https://www.aratiforcongress.com/",
    actblue_link = c("akfc"),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00713917",
    FEC_ID_cand = "H0NJ05208"
  ),
  tibble(
    first_name = "John",
    last_name = "McCann",
    state_cd = "NJ-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John McCann for Congress",
    campaign_website = "https://www.mccannforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://johnmccann.revv.co/donate?source_code=website",
    FEC_ID_cmte = c("C00699272", "C00661637"),
    FEC_ID_cand = "H8NJ05136"
  ),
  tibble(
    first_name = "Dana",
    last_name = "Dirisio",
    state_cd = "NJ-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dana A. Dirisio for Congress",
    campaign_website = "https://www.danafornj.com",
    actblue_link = NA,
    # No prompts
    anedot_link =
      "friends-of-dana-dirisio-172dee65-5287-4762-8604-05a60010d3a0",
    other_link = NA,
    FEC_ID_cmte = "C00727859",
    FEC_ID_cand = "H0NJ05216"
  ),
  tibble(
    first_name = "James",
    last_name = "Baldini",
    state_cd = "NJ-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "James Baldini for Congress",
    campaign_website = "https://www.jamesbaldiniforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-150-250-500-1000-1500-2000-2700
    other_link = "https://www.jamesbaldiniforcongress.com/contribute",
    FEC_ID_cmte = "C00705053",
    FEC_ID_cand = "H0NJ05166"
  ),
  tibble(
    first_name = "Hector",
    last_name = "Castillo",
    state_cd = "NJ-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://castilloforcongress2020.com",
    actblue_link = NA,
    anedot_link = NA,
    # No website donation form yet
    other_link = "https://castilloforcongress2020.com/contribute/",
    FEC_ID_cmte = "C00729640",
    FEC_ID_cand = "H0NJ05224"
  ),
  # Dalrymple withdrew
  # Cannot find Louis Vellucci (Ind)
  # NJ-06 ---------------------------------------------------------------------
  tibble(
    first_name = "Frank",
    last_name = "Pallone",
    state_cd = "NJ-06",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://pallone.house.gov/",
    campaign_name = "Pallone for Congress",
    campaign_website = "https://www.pallonefornewjersey.com/",
    actblue_link = "pallone4congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Christian",
    last_name = "Onuoha",
    state_cd = "NJ-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Christian Onuoha Election Committee",
    campaign_website = "https://www.christianfornj.com/",
    actblue_link = NA,
    anedot_link = NA,
    # a squarespace donation through website.
    # options are 10-25-50-100 after you enter email address.
    other_link =
      paste0(
        "https://secure.squarespace.com/checkout/donate?donatePageId=5ecc8",
        "11995a4d74aea67676d&ss_cid=edf464be-7b4b-4231-ac84-5ba180c28e78",
        "&ss_cvisit=1603868097604&ss_cvr=9cc107d3-7d3a-4075-a93e-aed4dc886",
        "250%7C1603868097515%7C1603868097515%7C1603868097515%7C1"
      ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # NJ-07 ---------------------------------------------------------------------
  tibble(
    first_name = "Tom",
    last_name = "Malinowski",
    state_cd = "NJ-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://malinowski.house.gov",
    campaign_name = "Tom Malinowski for Congress",
    campaign_website = "https://malinowskifornj.com/",
    actblue_link = "tpmwb",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00656686",
    FEC_ID_cand = "H8NJ07223"
  ),
  tibble(
    first_name = "Tom",
    last_name = "Kean",
    state_cd = "NJ-07",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Kean for Congress",
    campaign_website = "https://tomkean.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from Jan 22, 2020
    # from 10-25-50-100-250-1000-2800 to 10-25-50-100-250-1000-2800-5600
    other_link = paste0(
      # "https://mystique.victorypassport.com/pages/kean/grassroots?",
      # "location=https%3A%2F%2Fgive.victorypassport.com%2Fkean%2Fdonate&",
      # "location_url=aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5jb20va2Vhbi9kb25",
      # "hdGUvP3NvdXJjZT13ZWJzaXRlIw%3D%3D"
      "https://secure.winred.com/tomkean/donate"
    ),
    FEC_ID_cmte = "C00703058",
    FEC_ID_cand = "H0NJ07261"
  ),
  # NJ-08 ---------------------------------------------------------------------
  # Albio Sires is uncontested (no candidates registered as of Sep 24, 2019)
  tibble(
    first_name = "Albio",
    last_name = "Sires",
    state_cd = "NJ-08",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://sires.house.gov",
    # Uh... why can't I find the website?
    campaign_name = "Sires for Congress",
    campaign_website = "https://www.albiosiresnj.com/",
    # no donation links in website but found actblue one through google/
    actblue_link = "albio-sires-3",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00410753",
    FEC_ID_cand = "H6NJ13191"
  ),
  tibble(
    first_name = "Jason",
    last_name = "Mushnik",
    state_cd = "NJ-08",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    # cannot find campaign website.
    # https://www.ourcampaigns.com/CandidateDetail.html?CandidateID=443471 says
    # there are none.
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Dan",
    last_name = "Delaney",
    state_cd = "NJ-08",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dan Delaney for Congress",
    campaign_website = "http://dandelaneyforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # redirects to a paypal link with the amount selected on this page
    # 5-10-20-50-100
    other_link = "http://dandelaneyforcongress.com/donate/",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # NJ-09 ---------------------------------------------------------------------
  tibble(
    first_name = "Bill",
    last_name = "Pascrell",
    state_cd = "NJ-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bill Pascrell for Congress",
    campaign_website = "https://billpascrell.com/",
    actblue_link = "billpascrell",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Billy",
    last_name = "Prempeh",
    state_cd = "NJ-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Billy Prempeh for Congress",
    campaign_website = "https://billy2020nj.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://secure.winred.com/the-billy-prempeh-for-",
      "congress-committee-inc/donate"
    ),
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Chris",
    last_name = "Auriemma",
    state_cd = "NJ-09",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chris for Congress",
    campaign_website = "https://www.facebook.com/chrisforcongress9thdistrict/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00742874",
    FEC_ID_cand = "H0NJ09218"
  ),
  # NJ-10 (last updated Jul 12, 2021) ------------------------------------------
  tibble(
    first_name = "Donald",
    last_name = "Payne", # Jr.
    state_cd = "NJ-10",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://payne.house.gov",
    campaign_name = "Donald Payne Jr For Congress",
    campaign_website = "https://www.paynejr4congress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/4643211215818981376",
    FEC_ID_cmte = "C00519355",
    FEC_ID_cand = "H2NJ10154"
  ),
  tibble(
    first_name = "Jen", # Jennifer
    last_name = "Zinone",
    state_cd = "NJ-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jen Zinone for Congress",
    campaign_website = "https://jenzinone.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/jen-zinone/donate",
    FEC_ID_cmte = "C00740720",
    FEC_ID_cand = "H0NJ10232"
  ),
  tibble(
    first_name = "Akil",
    last_name = "Khalfani",
    state_cd = "NJ-10",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Akil Khalfani for Congress",
    campaign_website = "https://www.akilkhalfani.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://politics.raisethemoney.com/khalfani4congress",
    FEC_ID_cmte = "C00750406",
    FEC_ID_cand = "H0NJ10257"
  ),
  tibble(
    first_name = "Khaliah",
    last_name = "Fitchette",
    state_cd = "NJ-10",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Khaliah for Congress",
    campaign_website = "https://www.khaliahforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00758565",
    FEC_ID_cand = "H0NJ10240"
  ),
  tibble(
    first_name = "John",
    last_name = "Mirrione",
    state_cd = "NJ-10",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John for Congress",
    campaign_website = "https://johnmirrione.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://johnmirrione.com/donate",
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0NJ10265"
  ),
  # NJ-11 (last updated Oct 25, 2020) ------------------------------------------
  tibble(
    first_name = "Mikie",
    last_name = "Sherrill",
    state_cd = "NJ-11",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://sherrill.house.gov",
    campaign_name = "Mikie Sherrill for Congress",
    campaign_website = "https://www.mikiesherrill.com/",
    actblue_link = "mikie-sherrill",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00640003",
    FEC_ID_cand = "H8NJ11142"
  ),
  tibble(
    first_name = "Rosemary",
    last_name = "Becchi",
    state_cd = "NJ-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rosemary Becchi for Congress",
    campaign_website = "https://becchiforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/rosemarybecchi/donate",
    FEC_ID_cmte = "C00711572",
    FEC_ID_cand = "H0NJ07279"
  ),
  # Prijten has dropped out on May 14, 2019
  # Not sure Crook is running anymore, cannot find a valid website
  tibble(
    first_name = "Mark",
    last_name = "Washburne",
    state_cd = "NJ-11",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Washburne for Congress",
    campaign_website =
      "https://mwashburneforcongress.com/mark-washburne-for-congress",
    actblue_link = "mark-washburne-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00717801",
    FEC_ID_cand = "H8NJ11183"
  ),
  # NJ-12 (last updated Oct 25, 2020) ------------------------------------------
  tibble(
    first_name = "Bonnie Watson",
    last_name = "Coleman",
    state_cd = "NJ-12",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://watsoncoleman.house.gov",
    campaign_name = "Bonnie for Congress",
    campaign_website = "https://www.bonnieforcongress.com",
    actblue_link = "bonnie-watson-coleman-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00558437",
    FEC_ID_cand = "H4NJ12149"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Razzoli",
    state_cd = "NJ-12",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Razzoli for Congress",
    campaign_website = "https://www.markrazzoli.com",
    actblue_link = NA,
    # Anedot already lost, although registered as general election candidate?!
    # In the past it was 3-12-76-125-other
    # https://web.archive.org/web/20200711121540/
    # https://secure.anedot.com/mark-razzoli-for-cong/home
    anedot_link = "mark-razzoli-for-cong/home",
    other_link = NA,
    FEC_ID_cmte = "C00716829",
    FEC_ID_cand = "H0NJ12204"
  ),
  # NM: New Mexico (last updated Oct 25, 2020) =================================
  # NM-01 (last updated Oct 25, 2020) ------------------------------------------
  # Haaland vs. Kokinadis vs. Chick
  tibble(
    first_name = "Deb", # Debra goes by Deb
    last_name = "Haaland",
    state_cd = "NM-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://haaland.house.gov",
    campaign_name = "Deb for Congress",
    campaign_website = "https://debforcongress.com",
    actblue_link = c(
      # Change within ActBlue
      "firstphase20",
      "thirdphase"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00639054",
    FEC_ID_cand = "H8NM01331"
  ),
  tibble(
    first_name = "Michelle",
    last_name = "Garcia Holmes",
    state_cd = "NM-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Garcia Holmes for Congress",
    campaign_website = "https://www.garciaholmes4congress.com",
    actblue_link = NA,
    anedot_link = "garcia-holmes-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00724245",
    FEC_ID_cand = "H0NM01189"
  ),
  tibble(
    first_name = "Brett",
    last_name = "Kokinadis",
    state_cd = "NM-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brett Kokinadis for Congress",
    campaign_website = "https://imaginebrett.com",
    actblue_link = NA,
    anedot_link = "imagine-brett-for-congress/pitchin",
    other_link = NA,
    FEC_ID_cmte = "C00703546",
    FEC_ID_cand = "H0NM03094"
  ),
  tibble(
    first_name = "Cameron",
    last_name = "Chick",
    state_cd = "NM-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cameron Chick 4 Congress NM CD1",
    campaign_website = "https://www.cameronchick4congressnmcd1.com",
    actblue_link = NA,
    anedot_link = NA,
    # No amount prompts
    other_link = "https://paypal.me/ElectCameronChick4Co?locale.x=en_US",
    FEC_ID_cmte = "C00713651",
    FEC_ID_cand = "H0NM01171"
  ),
  # NM-02 (last updated Oct 25, 2020) ------------------------------------------
  # Torres Small vs. Herrell vs. Mathys vs. Chase
  tibble(
    first_name = "Xochitl",
    last_name = "Torres Small",
    state_cd = "NM-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://torressmall.house.gov/",
    campaign_name = "Xochitl Torres Small for Congress",
    campaign_website = "https://www.xochforcongress.com/",
    actblue_link = "xochitl-torres-small",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00666149",
    FEC_ID_cand = "H8NM02248"
  ),
  tibble(
    first_name = "Yvette",
    last_name = "Herrell",
    state_cd = "NM-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Yvette Herrell for Congress",
    campaign_website = "https://www.yvetteherrell.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/yvetteherrell/donate",
    FEC_ID_cmte = "C00655571",
    FEC_ID_cand = "H8NM02156"
  ),
  tibble(
    first_name = "Chris",
    last_name = "Mathys",
    state_cd = "NM-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mathys for Congress",
    campaign_website = "https://www.mathyscongress.com/",
    actblue_link = NA,
    anedot_link = "mathys-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00695965",
    FEC_ID_cand = "H0NM02211"
  ),
  tibble(
    first_name = "Claire",
    last_name = "Chase",
    state_cd = "NM-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "C",
    campaign_website = "https://www.clairechase.org/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from Nov 20, 2020
    # 50-100-250-500-1000-2800-5600 to 5-10-25-50-100-250-500
    # other_link = "https://clairechaseforcongress.ichooseapex.com/donate/",
    other_link = "https://secure.winred.com/ClaireChase/donate",
    FEC_ID_cmte = "C00717306",
    FEC_ID_cand = "H0NM02229"
  ),
  # NM-03 (last updated Oct 25, 2020) ------------------------------------------
  # Open seat: Plame vs. Serna
  # vs. Leger vs. Apodaca vs. Sanchez vs. Brown vs. Chick vs. McDonald
  # vs. Montoya
  # Apodaca withdrew on Aug 15, 2019, Chick moved to NM-01,
  # McDonald withdrew on May 2019,
  tibble(
    first_name = "Teresa",
    last_name = "Fernandez", # Goes by Leger Fernandez
    state_cd = "NM-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Teresa for All",
    campaign_website = "https://www.teresaforall.com",
    actblue_link = "teresa-for-nm",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00704049",
    FEC_ID_cand = "H0NM03102"
  ),
  tibble(
    first_name = "Alexis",
    last_name = "Johnson",
    state_cd = "NM-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Alexis Johnson for Congress",
    campaign_website = "https://alexisjohnsonnm.com",
    actblue_link = NA,
    anedot_link = c(
      "alexis-johnson-for-congress/website-splash",
      "alexis-johnson-for-congress/ca9e262c3e2141db746c4"
    ),
    other_link = NA,
    FEC_ID_cmte = "C00716480",
    FEC_ID_cand = "H0NM03201"
  ),
  tibble(
    first_name = "Valerie",
    last_name = "Plame",
    state_cd = "NM-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Valerie Plame for Congress",
    campaign_website = "https://valerieplameforcongress.com",
    actblue_link = c(
      "valerie-plame",
      "plame_190909undercover-d2d",
      "plame_october_email"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00705483",
    FEC_ID_cand = "H0NM03110"
  ),
  tibble(
    first_name = "Marco",
    last_name = "Serna",
    state_cd = "NM-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marco Serna for Congress",
    campaign_website = "https://www.marcoforcongress.com",
    actblue_link = "marcoforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00706325",
    FEC_ID_cand = "H0NM03136"
  ),
  tibble(
    first_name = "Joseph",
    last_name = "Sanchez",
    state_cd = "NM-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joseph Louis Sanchez for Congress",
    campaign_website = "http://josephsanchezforcongress.com",
    actblue_link = "joseph-sanchez-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00707620",
    FEC_ID_cand = "H0NM03144"
  ),
  tibble(
    first_name = "Audra",
    last_name = "Brown",
    state_cd = "NM-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Audra for New Mexico",
    campaign_website = "https://mailchi.mp/967f379fad35/imfromnm",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://audra4congress.revv.co/fight-for-new-mexico",
    FEC_ID_cmte = "C00711259",
    FEC_ID_cand = "H0NM03169"
  ),
  tibble(
    first_name = "Gavin",
    last_name = "Kaiser",
    state_cd = "NM-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kaiser for Constitutional Rights",
    campaign_website = "https://kaiserforcongress.com",
    actblue_link = "kaiser-for-constitutional-rights-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00703306",
    FEC_ID_cand = "H0NM03086"
  ),
  tibble(
    first_name = "Laura",
    last_name = "Montoya",
    state_cd = "NM-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Laura Montoya for NM",
    campaign_website = "http://lauramontoya4nm.com",
    actblue_link = "lauram4nm",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710756",
    FEC_ID_cand = "H0NM03151"
  ),
  # NY: New York (last updated Oct 24, 2020) ===================================
  # NY-01 (last updated Oct 24, 2020) ------------------------------------------
  tibble(
    first_name = "Lee",
    last_name = "Zeldin",
    state_cd = "NY-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Zeldin For Congress",
    campaign_website = "https://zeldinforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Change detected to WinRed Jun 30, 2020
    # 10-25-50-75-100-250-500-1000-2800-5600 to 25-50-100-250-500-1000-2800
    other_link = c(
      "https://secure.winred.com/lee-zeldin-for-congress/donate" # ,
      # "https://secure.zeldinforcongress.com/lee-fight-back"
    ),
    FEC_ID_cmte = "C00552547",
    FEC_ID_cand = "H8NY01148"
  ),
  tibble(
    first_name = "Nancy",
    last_name = "Goroff",
    state_cd = "NY-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nancy Goroff for Congress",
    campaign_website = "https://www.goroffforcongress.com",
    actblue_link = c(
      # Change within ActBlue
      "goroff-website-new",
      "goroff-website"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711564",
    FEC_ID_cand = "H0NY01186"
  ),
  tibble(
    first_name = "Perry",
    last_name = "Gershon",
    state_cd = "NY-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Perry Gershon for Congress",
    campaign_website = "https://www.perrygershon.com",
    actblue_link = "support-perry-gershon",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00701334", "C00650457"),
    FEC_ID_cand = "H8NY01155"
  ),
  tibble(
    first_name = "Bridget",
    last_name = "Fleming",
    state_cd = "NY-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bridget Fleming for Congress",
    campaign_website = "https://flemingfornewyork.com",
    actblue_link = "bridgetflemingforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00728832",
    FEC_ID_cand = "H0NY01202"
  ),
  tibble(
    first_name = "Greg", # Gregory-John, goes by Greg
    last_name = "Fischer",
    state_cd = "NY-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Greg Fischer for Congress, NY-1",
    campaign_website = "https://www.facebook.com/VoteForSuffolk/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00730051",
    FEC_ID_cand = "H0NY01210"
  ),
  #  Contribution page no longer found (Jun 30, 2020)
  tibble(
    first_name = "David",
    last_name = "Gokhshtein",
    state_cd = "NY-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Gokhshtein for House",
    campaign_website = "http://votegokhshtein.com",
    # Donation link is broken
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710426",
    FEC_ID_cand = "H0NY01178"
  ),
  # NY-02 (last updated Oct 24, 2020) ------------------------------------------
  # Peter King retired; open seat
  tibble(
    first_name = "Jackie", # Jacqueline, goes by Jackie
    last_name = "Gordon",
    state_cd = "NY-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jackie Gordon for Congress",
    campaign_website = "https://jackiegordonforcongress.com",
    actblue_link = "jackiegordonsite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00706549",
    FEC_ID_cand = "H0NY02192"
  ),
  tibble(
    first_name = "Andrew",
    last_name = "Garbarino",
    state_cd = "NY-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Garbarino For Congress",
    campaign_website = "https://www.garbarinoforny.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/garbarino-for-congress/donate/",
    FEC_ID_cmte = "C00729954",
    FEC_ID_cand = "H0NY02234"
  ),
  tibble(
    first_name = "Mike",
    last_name = "LiPetri",
    state_cd = "NY-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "LiPetri for Congress",
    campaign_website = "https://www.mikelipetri.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/MichaelLiPetri/donate",
    FEC_ID_cmte = "C00732768",
    FEC_ID_cand = "H0NY02267"
  ),
  # LaLota now running for State Senate;
  # Note that WinRed still has his name in their main directory
  # with https://secure.winred.com/nick-lalota-for-congress/contribute
  tibble(
    first_name = "Johanna",
    last_name = "Ellerup",
    state_cd = "NY-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Johanna for Congress 2020",
    campaign_website = "https://www.johannaforcongress2020.com",
    actblue_link = "johannaforcongress2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00728246",
    FEC_ID_cand = "H0NY02218"
  ),
  # Cannot find Trish Bergin
  tibble(
    first_name = "Robert",
    last_name = "Kudler",
    state_cd = "NY-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kudler for Congress",
    campaign_website = "https://www.kudlerforcongress.com",
    # Donate tab leads with WinRed directory, which does not have Kudler
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00732578",
    FEC_ID_cand = "H0NY02259"
  ),
  # Cannot find Kevin Gomez
  # Cannot find Nancy Hemendinger
  tibble(
    first_name = "Danny",
    last_name = "Ross",
    state_cd = "NY-02",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "danNY2 for Congress",
    campaign_website = "https://www.ross4ny.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/embed/dcr2o2o",
    FEC_ID_cmte = "C00699413",
    FEC_ID_cand = "H0NY03067"
  ),
  tibble(
    first_name = "Mike",
    last_name = "Sax",
    state_cd = "NY-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mike Sax Democrat for Congress",
    # Website under construction
    campaign_website = "https://friendsmikesax.com",
    actblue_link = "friends-of-mike-sax-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00691824",
    FEC_ID_cand = "H8NY02088"
  ),
  # NY-03 (last updated Jul 13, 2021) ------------------------------------------
  tibble(
    first_name = "Thomas",
    last_name = "Suozzi",
    state_cd = "NY-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://suozzi.house.gov",
    campaign_name = "Suozzi for Congress",
    campaign_website = "https://suozziforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Still NGP. $100 highlighted
    other_link = "https://act.myngp.com/Forms/8486978515454069504",
    FEC_ID_cmte = "C00607200",
    FEC_ID_cand = "H6NY03247"
  ),
  tibble(
    first_name = "George",
    last_name = "Devolder-Santos", # Goes by Santos
    state_cd = "NY-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "George Santos for Congress",
    campaign_website = "https://georgeforny.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/george-santos-for-congress/donate",
    FEC_ID_cmte = "C00721365",
    FEC_ID_cand = "H0NY03083"
  ),
  tibble(
    first_name = "Melanie",
    last_name = "D'Arrigo",
    state_cd = "NY-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Melanie D'Arrigo for U.S. Congress",
    campaign_website = "https://www.darrigo2020.com",
    actblue_link = c(
      "darrigoforny3", # At least from Apr 23
      "melaniedarrigo"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00712398",
    FEC_ID_cand = "H0NY03075"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Weinstock",
    state_cd = "NY-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Michael Weinstock for Congress",
    campaign_website = "https://weinstockforcongress.com",
    actblue_link = "weinstock",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00691709",
    FEC_ID_cand = "H0NY03059"
  ),
  tibble(
    first_name = "Howard",
    last_name = "Rabin",
    state_cd = "NY-03",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Howard Rabin for U.S. Congress",
    campaign_website = "https://howardrabinforuscongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00756577",
    FEC_ID_cand = "H0NY03091"
  ),
  # Sauberman withdrew
  # NY-04 (last updated Oct 24, 2020) ------------------------------------------
  tibble(
    first_name = "Kathleen",
    last_name = "Rice",
    state_cd = "NY-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://kathleenrice.house.gov",
    campaign_name = "Kathleen Rice for Congress",
    campaign_website = "https://www.kathleenrice.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/-8980169569814116608",
    FEC_ID_cmte = "C00555813",
    FEC_ID_cand = "H4NY04075"
  ),
  tibble(
    first_name = "Douglas",
    last_name = "Tuman",
    state_cd = "NY-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elect Tuman",
    campaign_website = "https://www.douglastuman.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Venmo, PayPal, credit card, cryptocurrency(!)
    # other_link = "https://www.douglastuman.com/donate",
    # Change documented to WinRed on Oct 24, 2020
    other_link = "https://secure.winred.com/douglastuman/donate",
    FEC_ID_cmte = "C00743955",
    FEC_ID_cand = "H0NY04156"
  ),
  # Can no longer find the Raiser campaign
  # Cannot find LeBlanc
  tibble(
    first_name = "Azeem",
    last_name = "Hussein",
    state_cd = "NY-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Azeem Hussein for Congress",
    campaign_website = "https://www.azeemhussein2020.com",
    actblue_link = "azeem",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00737841",
    FEC_ID_cand = "H0NY04131"
  ),
  # Dennehy campaign not accepting contributions(!)
  tibble(
    first_name = "Edward",
    last_name = "Dennehy",
    state_cd = "NY-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cindy Grosz for Congress",
    campaign_website = "https://www.thedennehys.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.thedennehys.com/no-donations",
    FEC_ID_cmte = "C00749218",
    FEC_ID_cand = "H0NY04164"
  ),
  tibble(
    first_name = "Cindy",
    last_name = "Grosz",
    state_cd = "NY-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cindy Grosz for Congress",
    campaign_website = "https://www.cindygrosz.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/cindy-grosz-for-congress/donate",
    FEC_ID_cmte = "C00740118",
    FEC_ID_cand = "H0NY04149"
  ),
  # NY-05 (last updated Oct 24, 2020) ------------------------------------------
  # Republican party primary cancelled
  tibble(
    first_name = "Gregory",
    last_name = "Meeks",
    state_cd = "NY-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://meeks.house.gov",
    campaign_name = "Gregory Meeks for Congress",
    campaign_website = "https://www.congressmangregorymeeks.com",
    actblue_link = "gregorymeeks",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00430991", "C00329375"),
    FEC_ID_cand = "H8NY06048"
  ),
  tibble(
    first_name = "Shaniyat",
    last_name = "Chowdhury",
    state_cd = "NY-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shaniyat Chowdhury for NY-5",
    campaign_website = "https://shaniyat2020.com",
    actblue_link = "shaniyat-chowdhury-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00704585",
    FEC_ID_cand = "H0NY05070"
  ),
  tibble(
    first_name = "Amit",
    last_name = "Lal",
    state_cd = "NY-05",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lal for Congress",
    campaign_website = "https://www.lalforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://politics.raisethemoney.com/lal-for-congress",
    FEC_ID_cmte = "C00716241",
    FEC_ID_cand = "H0NY05088"
  ),
  # NY-06 (last updated Oct 24, 2020) ------------------------------------------
  tibble(
    first_name = "Grace",
    last_name = "Meng",
    state_cd = "NY-06",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Grace for New York",
    campaign_website = "https://gracefornewyork.com",
    actblue_link = "grace-meng-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00516666",
    FEC_ID_cand = "H2NY06116"
  ),
  tibble(
    first_name = "Tom",
    last_name = "Zmich",
    state_cd = "NY-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Zmich for Congress",
    campaign_website = "http://www.zmich2020.com",
    actblue_link = NA,
    anedot_link = "zmich-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00731802",
    FEC_ID_cand = "H0NY06086"
  ),
  tibble(
    first_name = "Mel", # Melquiades, goes by Mel
    last_name = "Gagarin",
    state_cd = "NY-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mel for Progress",
    campaign_website = "https://melforprogress.com",
    actblue_link = "melforprogress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00712216",
    FEC_ID_cand = "H0NY06078"
  ),
  tibble(
    first_name = "Sandra",
    last_name = "Choi",
    state_cd = "NY-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sandra for Congress",
    campaign_website = "https://www.sandrachoi2020.com",
    actblue_link = "sandrachoi",
    anedot_link = NA,
    # At least from Apr 11, 2020
    other_link = "https://secure.numero.ai/contribute/sandrachoi",
    FEC_ID_cmte = "C00711168",
    FEC_ID_cand = "H0NY06060"
  ),
  tibble(
    first_name = "Matthew",
    last_name = "DiBono",
    state_cd = "NY-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "A Bold Choice for a Prosperous Tomorrow",
    # Cannot find official website
    # 25-50-100-300
    campaign_website =
      "https://www.crowdpac.com/campaigns/391396/matthew-c-dibono",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00704460",
    FEC_ID_cand = "H0NY06052"
  ),
  # NY-07 (last updated Jul 13, 2021) ------------------------------------------
  tibble(
    first_name = "Nydia",
    last_name = "Velazquez",
    state_cd = "NY-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://velazquez.house.gov",
    campaign_name = "Re-elect Nydia Velazquez",
    campaign_website = "https://www.reelectnydiavelazquez.com",
    actblue_link = "nv-web-donor",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00271312",
    FEC_ID_cand = "H2NY00010"
  ),
  # No FEC, Velazquez de facto uncontested
  tibble(
    first_name = "Brian",
    last_name = "Kelly",
    state_cd = "NY-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "http://www.brianwkelly.com",
    # Obviously, no donation link...
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # Now running for Assembly District 51
  tibble(
    first_name = "Avery",
    last_name = "Pereira",
    state_cd = "NY-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Avery Pereira for Congress",
    campaign_website = "https://www.averypereira.com",
    # Don't see a donation link in his website
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00695957",
    FEC_ID_cand = "H0NY07084"
  ),
  tibble(
    first_name = "Paperboy",
    last_name = "Prince",
    state_cd = "NY-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Paperboy Prince for Congress",
    campaign_website = "https://paperboyprince.com",
    actblue_link = "paperboy",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00727644",
    FEC_ID_cand = "H0NY07092"
  ),
  tibble(
    first_name = "Iroghama",
    last_name = "Omere",
    state_cd = "NY-07",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Omere for New York",
    campaign_website = "http://omerefornyd7.com",
    actblue_link = "omerefornyd7#",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00733055",
    FEC_ID_cand = "H0NY07100"
  ),
  tibble(
    first_name = "Gilbert",
    last_name = "Midonnet",
    state_cd = "NY-07",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gilbert Midonnet for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0NY07126"
  ),
  # NY-08 (last updated Oct 24, 2020) ------------------------------------------
  # Hakeem Jeffries was uncontested
  # (no candidates registered as of Sep 26, 2019)
  # Now Jeffries vs. Garcia, but virtually uncontested
  tibble(
    first_name = "Hakeem",
    last_name = "Jeffries",
    state_cd = "NY-08",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://jeffries.house.gov",
    campaign_name = "Hakeem Jeffries for Congress",
    campaign_website = "https://www.hakeemjeffries.com/",
    actblue_link = "jeffries-homepage-donate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00503052",
    FEC_ID_cand = "H2NY10092"
  ),
  # Not on FEC, cannot find website
  tibble(
    first_name = "Garfield",
    last_name = "Wallace",
    state_cd = "NY-08",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Garfield Wallace for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = NA
  ),
  tibble(
    first_name = "Abe",
    last_name = "Garcia",
    state_cd = "NY-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Abe J. Garcia for Congress",
    campaign_website = "https://twitter.com/friendsabe",
    # Cannot find any donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00724468",
    FEC_ID_cand = "H0NY08090"
  ),
  # NY-09 (last updated Oct 24, 2020) ------------------------------------------
  # Clarke vs. Bunkeddeko
  # vs. Hiller vs. Hubbard vs. James vs. Deutsch vs. Azumah vs. Gayot
  tibble(
    first_name = "Yvette",
    last_name = "Clarke",
    state_cd = "NY-09",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://clarke.house.gov",
    campaign_name = "Clarke for Congress",
    campaign_website = "https://www.voteyvette.com/",
    actblue_link = c(
      "ydc_donatepage", # At least from Jun 3, 2020
      "yvetteclarke-q32019",
      "yvetteclarke-q22019"
    ),
    anedot_link = NA,
    other_link = NA,
    # To make the lengths consistent
    FEC_ID_cmte = c("C00415331", "C00398941", ""),
    FEC_ID_cand = "H4NY11138"
  ),
  tibble(
    first_name = "Constantin", # Constantine? Mixed names on FEC
    last_name = "Jean-Pierre",
    state_cd = "NY-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Constantin for Congress",
    campaign_website = "https://www.cjpforny.com",
    actblue_link = NA,
    anedot_link = "constintin-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = c("C00743013", "C00743005"),
    FEC_ID_cand = "H0NY09122"
  ),
  tibble(
    first_name = "Adem",
    last_name = "Bunkeddeko",
    state_cd = "NY-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Adem Bunkeddeko for Congress",
    campaign_website = "https://ademforcongress.com/",
    actblue_link = "adem2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00713404", "C00656793"),
    FEC_ID_cand = "H8NY09158"
  ),
  tibble(
    first_name = "Mike",
    last_name = "Hiller",
    state_cd = "NY-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hiller for Congress",
    campaign_website = "https://www.michaelhillerforcongress.com/",
    actblue_link = "mikehiller",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00716324",
    FEC_ID_cand = "H0NY09106"
  ),
  tibble(
    first_name = "Alex",
    last_name = "Hubbard",
    state_cd = "NY-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Alex Hubbard for Congress",
    campaign_website = "https://www.hubbard2020.com/",
    actblue_link = "hubbard2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00704882",
    FEC_ID_cand = "H0NY09098"
  ),
  tibble(
    # Mixed up first/last names, woops.
    first_name = "Isiah",
    last_name = "James",
    state_cd = "NY-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Isiah James for Congress",
    campaign_website = "https://www.isiahforcongress.com/",
    actblue_link = "friends-of-isiah-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00695437",
    FEC_ID_cand = "H0NY09080"
  ),
  # Still can't find his website. Was on ballot though
  tibble(
    first_name = "Chaim",
    last_name = "Deutsch",
    state_cd = "NY-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    # Very well known, but cannot find website!
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00735548",
    FEC_ID_cand = "H0NY09114"
  ),
  tibble(
    first_name = "Joel",
    last_name = "Anabilah-Azumah", # Azumah
    state_cd = "NY-09",
    party = "Ref",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.joelazumah.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 10-   25-50-100-150-250-500-1000-1500 to
    # 10-15-25-50-100-    250-500-1000-1500
    other_link = c(
      "https://89360.campaignpartner.net/contribute" # ,
      # Same website
      # "http://www.joelazumah.com/contribute"
    ),
    FEC_ID_cmte = "C00613182",
    FEC_ID_cand = "H4NY09090"
  ),
  tibble(
    first_name = "Lutchi",
    last_name = "Gayot",
    state_cd = "NY-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lutchi Gayot for Congress",
    campaign_website = "https://www.teamgayot.com",
    actblue_link = "lutchi-gayot",
    anedot_link = NA,
    # 1-5-10-25-50-100
    other_link = "https://www.teamgayot.com/checkout/donate",
    FEC_ID_cmte = "C00673244",
    FEC_ID_cand = "H8NY09174"
  ),
  # NY-10 (last updated Oct 24, 2020) ------------------------------------------
  # Nadler vs. Boylan
  # vs. Lynch vs. Nigro vs. Herzog vs. Frankel vs. Hendricks
  # vs. Bernstein vs. Stevenson vs. Wyman
  tibble(
    first_name = "Jerry", # Jerrold
    last_name = "Nadler",
    state_cd = "NY-10",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://nadler.house.gov/",
    campaign_name = "Jerry Nadler for Congress",
    campaign_website = "https://www.jerrynadler.com/",
    actblue_link = "jn_website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00290825",
    FEC_ID_cand = "H2NY17071"
  ),
  tibble(
    first_name = "Cathy",
    last_name = "Bernstein",
    state_cd = "NY-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cathy Bernstein for Congress",
    campaign_website = "https://bernsteinforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://politics.raisethemoney.com/cbernstein",
    FEC_ID_cmte = "C00733170",
    FEC_ID_cand = "H0NY10112"
  ),
  tibble(
    first_name = "Lindsey",
    last_name = "Boylan",
    state_cd = "NY-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lindsey Boylan for Congress",
    campaign_website = "https://lindseyboylan.com/",
    actblue_link = "lindsey-boylan-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00703231",
    FEC_ID_cand = "H0NY10070"
  ),
  tibble(
    first_name = "Holly",
    last_name = "Lynch",
    state_cd = "NY-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Holly Lynch for Congress",
    campaign_website = "https://www.hollylynchny.org/meet-holly",
    actblue_link = "hlynch-site",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00658070",
    FEC_ID_cand = "H8NY10081"
  ),
  tibble(
    first_name = "Jeanne",
    last_name = "Nigro",
    state_cd = "NY-10",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nigro for Congress",
    campaign_website = "https://jeannenigro.com",
    actblue_link = NA,
    anedot_link = "nigro-for-congress/website-splash",
    other_link = NA,
    FEC_ID_cmte = "C00710657",
    FEC_ID_cand = "H0NY10088"
  ),
  tibble(
    first_name = "Jonathan",
    last_name = "Herzog",
    state_cd = "NY-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Herzog for Congress",
    campaign_website = "https://www.herzog2020.com",
    actblue_link = "herzog2020",
    anedot_link = NA,
    # Cryptocurrency(!) link
    other_link = "https://herzog2020.nyc/",
    FEC_ID_cmte = "C00714824",
    FEC_ID_cand = "H0NY10096"
  ),
  tibble(
    first_name = "Amanda",
    last_name = "Frankel",
    state_cd = "NY-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Amanda Frankel for Congress",
    campaign_website = "https://www.amandafrankel.com/",
    actblue_link = "amanda-frankel-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00702126",
    FEC_ID_cand = "H0NY10062"
  ),
  tibble(
    first_name = "Darryl",
    last_name = "Hentricks",
    state_cd = "NY-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hendricks 2020",
    campaign_website = "https://www.hendricks2020.com",
    actblue_link = "hendricks2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00695106",
    FEC_ID_cand = "H0NY10104"
  ),
  tibble(
    first_name = "Dylan",
    last_name = "Stevenson",
    state_cd = "NY-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Stevenson for Congress",
    campaign_website = "https://stevensonforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 18-54-90-150-360-720-1800-2800
    other_link = "https://stevensonforcongress.com/?page_id=374",
    FEC_ID_cmte = "C00737460",
    FEC_ID_cand = "H0NY10120"
  ),
  tibble(
    first_name = "Bob", # Robert Mark, goes by Bob
    last_name = "Wyman",
    state_cd = "NY-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bob Wyman for Congress (NY-10)",
    campaign_website = "http://wyman2020.us",
    actblue_link = "bob-wyman",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00739201",
    FEC_ID_cand = "H0NY10138"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Madrid",
    state_cd = "NY-10",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Madrid for US Congress",
    campaign_website = "http://wyman2020.us",
    actblue_link = NA,
    anedot_link = "michael-madrid-for-congress-ny-10/donate",
    other_link = NA,
    FEC_ID_cmte = "C00742817",
    FEC_ID_cand = "H0NY10146"
  ),
  # NY-11 (last updated Oct 24, 2020) ------------------------------------------
  tibble(
    first_name = "Max",
    last_name = "Rose",
    state_cd = "NY-11",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://maxrose.house.gov",
    campaign_name = "Max Rose for Congress",
    campaign_website = "https://www.maxroseforcongress.com/",
    # Switch from NGP to ActBlue; at least from May 16, 2020
    actblue_link = "mrfcwebsite",
    anedot_link = NA,
    other_link = NA,
    # other_link = "https://act.myngp.com/Forms/4260765953381239552",
    FEC_ID_cmte = "C00652248",
    FEC_ID_cand = "H8NY11113"
  ),
  tibble(
    first_name = "Nicole",
    last_name = "Malliotakis",
    state_cd = "NY-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nicole for New York",
    campaign_website = "https://nicolemalliotakis.com",
    # Change documented to WinRed on Nov 16, 2019
    # At least from Nov 13, 2019
    # 25-50-100-250-500-1000-2800-5600 to 25-50-100-250-500-1000-2800-5600
    # (same)
    actblue_link = NA,
    # anedot_link = "nicolefornewyork/donate",
    # other_link = "https://secure.nicolemalliotakis.com/donate"
    # Anedot link in disguise
    anedot_link = NA,
    other_link = "https://secure.winred.com/nicolemalliotakis/donate",
    FEC_ID_cmte = "C00694778",
    FEC_ID_cand = "H0NY11078"
  ),
  tibble(
    first_name = "Joe",
    last_name = "Caldarera", # Goes commonly by Joe Cal
    state_cd = "NY-11",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Joe Caldarera for Congress",
    campaign_website = "https://joecal.nyc",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/JoeCaldarera/welcome",
    FEC_ID_cmte = "C00724682",
    FEC_ID_cand = "H0NY11094"
  ),
  # Withdrew Dec 16, 2019 (YouTuber!?)
  tibble(
    first_name = "Joseph",
    last_name = "Saladino",
    state_cd = "NY-11",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Joseph Saladino for Congress",
    campaign_website = "https://saladinoforcongress.com/",
    actblue_link = NA,
    anedot_link = "saladino-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00704288",
    FEC_ID_cand = "H0NY11086"
  ),
  # NY-12 (last updated Jul 13, 2021) ------------------------------------------
  tibble(
    first_name = "Carolyn",
    last_name = "Maloney",
    state_cd = "NY-12",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://maloney.house.gov",
    campaign_name = "Carolyn Maloney for Congress",
    campaign_website = "https://www.carolynmaloney.com",
    actblue_link = c(
      # Change within ActBlue
      "cbm-rtw-crisisvideo",
      "cbm-website"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00273169",
    FEC_ID_cand = "H2NY14037"
  ),
  tibble(
    first_name = "Carlos",
    last_name = "Santiago-Cano",
    state_cd = "NY-12",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Carlos Santiago-Cano for Congress",
    campaign_website = "https://carlosforny.com",
    actblue_link = NA,
    anedot_link = NA,
    # 50-100-200-500-1000
    other_link = "https://carlosforny.com/donation/",
    FEC_ID_cmte = "C00759050",
    FEC_ID_cand = "H0NY12209"
  ),
  tibble(
    first_name = "Suraj",
    last_name = "Patel",
    state_cd = "NY-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Suraj Patel for Congress",
    campaign_website = "https://www.surajpatel.nyc",
    actblue_link = c(
      "sprecountfund2020",
      "surajpatel2020"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00720912",
    FEC_ID_cand = "H8NY12129"
  ),
  tibble(
    first_name = "Lauren",
    last_name = "Ashcraft",
    state_cd = "NY-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lauren Ashcraft for Congress",
    campaign_website = "https://laurenashcraft.com",
    actblue_link = "lauren-ashcraft-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00698068",
    FEC_ID_cand = "H0NY12175"
  ),
  tibble(
    first_name = "Peter",
    last_name = "Harrison",
    state_cd = "NY-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Peter for New York",
    campaign_website = "https://www.peterfornewyork.com",
    actblue_link = c(
      "peterjune",
      "petermay"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00713776",
    FEC_ID_cand = "H0NY12191"
  ),
  tibble(
    first_name = "Erica",
    last_name = "Vladimer",
    state_cd = "NY-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Erica for NY",
    campaign_website = "https://www.ericaforny.com",
    actblue_link = "ericaforny",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00708156",
    FEC_ID_cand = "H0NY12183"
  ),
  tibble(
    first_name = "Steven",
    last_name = "Kolln",
    state_cd = "NY-12",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kolln for Congress",
    campaign_website = "http://kollnforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0NY12217"
  ),
  # NY-13 (last updated Oct 24, 2020) ------------------------------------------
  # Espaillat vs. Keith
  # vs. Habersham vs. Lopez vs. Rodriguez vs. Morris-Perry vs. Gwinn
  # Cannot find Lopez website
  tibble(
    first_name = "Adriano",
    last_name = "Espaillat",
    state_cd = "NY-13",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://espaillat.house.gov",
    # Can't find an official website! What?!
    # Weird, now it's there, and by Wayback Machine, it was supposed
    # to have existed for years! Yet never came up on Google...
    # At least from May 29, 2020 and 2019 snapshot broken. So confusing
    campaign_name = "Espaillat for Congress",
    campaign_website = "https://adrianoespaillat.com/",
    # At least from May 8, 2019. Checked 1-6, all outdated
    actblue_link = "adriano-espaillat-7",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00701953", "C00635722", "C00593525", "C00518365"),
    FEC_ID_cand = "H2NY13096"
  ),
  tibble(
    first_name = "Love", # Lovelynn, goes by Love
    last_name = "Gwinn",
    state_cd = "NY-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lovelynn Gwinn for Congress",
    campaign_website = "https://lovefornewyork.com",
    actblue_link = NA,
    anedot_link = "love-for-new-york/donate",
    other_link = NA,
    FEC_ID_cmte = "C00740696",
    FEC_ID_cand = "H0NY13165"
  ),
  tibble(
    first_name = "James Felton",
    last_name = "Keith",
    state_cd = "NY-13",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "James Felton Keith for US Congress",
    campaign_website = "https://www.jamesfeltonkeith.com/",
    actblue_link = "jfkorganizers",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00700971",
    FEC_ID_cand = "H0NY13124"
  ),
  tibble(
    first_name = "Richard",
    last_name = "Habersham",
    state_cd = "NY-13",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Richard Habersham for US | For Congress",
    campaign_website = "https://www.habershamforcongress.com/",
    actblue_link = "electhabersham",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00701367",
    FEC_ID_cand = "H0NY13108"
  ),
  tibble(
    first_name = "Pedro",
    last_name = "Lopez",
    state_cd = "NY-13",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Pedro Lopez for Congress",
    campaign_website = "http://www.pedrolopezforcongress.com",
    actblue_link = "pedro-lopez-for-congress-2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00715870",
    FEC_ID_cand = "H0NY13116"
  ),
  tibble(
    first_name = "Ramon",
    last_name = "Rodriguez",
    state_cd = "NY-13",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ramon for Congress",
    campaign_website = "https://ramonforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = paste0(
      "https://www.paypal.com/webapps/",
      "shoppingcart?flowlogging_id=d457563d27c76&",
      "mfid=1585173710459_d457563d27c76#/checkout/openButton"
    ),
    FEC_ID_cmte = "C00728055",
    FEC_ID_cand = "H0NY13140"
  ),
  tibble(
    first_name = "Christopher",
    last_name = "Morris-Perry",
    state_cd = "NY-13",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Christopher Morris-Perry for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00737544",
    FEC_ID_cand = "H0NY13157"
  ),
  # NY-14 (last updated Oct 24, 2020) ------------------------------------------
  # Ocasio-Cortez vs. Papazian
  # vs. Hernandez vs. Tucker vs. Butler vs. Murray vs. Solano vs.
  # Cummings vs. Cruz
  # Cannot find Butler's 2020 one (13th 2018 one is paused)
  tibble(
    first_name = "Alexandria",
    last_name = "Ocasio-Cortez",
    state_cd = "NY-14",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://ocasio-cortez.house.gov",
    campaign_name = "Alexandria Ocasio-Cortez 2018",
    # I'm surprised that the website is still much 2018-centric.
    # campaign_website = "https://www.ocasio2018.com",
    campaign_website = "https://www.ocasiocortez.com",
    actblue_link = c(
      "ocasio2020", # previously ocasio2018
      "adopt-aoc",
      "alexandria-ocasio-cortez",
      # "aoc-oct2019",
      "aoc-ads-google-q4-2020",
      "aoc-ads-google-q3-2020",
      # "aoc-ads-google-q2-2020",
      # "aoc-ads-google-q1-2020",
      # "aoc-ads-facebook-june",
      # "aoc-ads-facebook-february-2020",
      # "ocasio-response",
      # "aoc-opponent-email-2020224-2",
      # "aoc-opponent-email-2020219-1",
      # "aoc-jessica-email-2020218-1",
      # "aoc-opponent-email-2020215-2",
      # "aoc-opponent-email-2020215-1",
      # "aoc-opponent-email-2020215-2",
      # "aoc-bernie-email-20200118",       # Split with Sanders
      # "aoc-ctc-email-2020221-4",         # Split with many
      # "aoc-ctc-email-2020222-1",         # Split with many
      # "aoc-ctc-email-2020223-1",         # Split with many
      # "aoc-opponent-email-2020225-1",
      # "aoc-ctc-email-202025-2",
      # "aoc-opponent-email-2020226-2",
      # "aoc-movement-email-2020226-2",
      # "aoc-opponent-email-2020228-1",
      # "aoc-opponent-email-2020229-1",
      # "aoc-opponent-email-2020229-3",
      # "aoc-ads-facebook-march-2020",
      # "aoc-ctc-email-2020301",
      # "aoc-ctc-email-2020302",
      # "aoc-ctc-email-2020303-3",
      # "aoc-jessica-email-2020303-2",
      # "aoc-bernie-2020305-1",
      # "aoc-bernie-2020309-1",
      # "aoc-movement-email-2020309-2",
      # "aoc-marchfec-email-2020311-1",
      # "aoc-marchfec-email-2020312-1",
      # "aoc-covid-email-20200401",
      # "aoc-markey",                      # Split with Markey
      "ocasio-campaign"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00639591",
    FEC_ID_cand = "H8NY15148"
  ),
  tibble(
    first_name = "John",
    last_name = "Cummings",
    state_cd = "NY-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cummings for Congress",
    campaign_website = "https://cummings2020.com/",
    actblue_link = NA,
    anedot_link = c(
      # Change within Anedot Oct 24, 2020
      # archive.org is down, cannot check
      "cummings-for-congress/donatege",
      "cummings-for-congress/c6273ae2b33122793e4b6",
      "cummings-for-congress/cc2c3c6648d4a772063cc"
    ),
    other_link = NA,
    FEC_ID_cmte = "C00712257",
    FEC_ID_cand = "H0NY14296"
  ),
  tibble(
    first_name = "Michelle",
    last_name = "Caruso-Cabrera",
    state_cd = "NY-14",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Caruso-Cabrera for Congress",
    # From Feb 12, 2020 (yikes! Key candidate missed). Both website/ActBlue
    campaign_website = "https://michellecc2020.com/",
    actblue_link = "caruso-cabrera-for-new-york-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00738336",
    FEC_ID_cand = "H0NY14346"
  ),
  tibble(
    first_name = "Scherie",
    last_name = "Murray",
    state_cd = "NY-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Scherie Murray for Congress",
    campaign_website = "https://scheriemurray.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/scherie-murray-for-congress/donate",
    FEC_ID_cmte = "C00712190",
    FEC_ID_cand = "H0NY14270"
  ),
  tibble(
    first_name = "Jineea",
    last_name = "Butler",
    state_cd = "NY-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jineea for Congress",
    campaign_website = "https://www.jineea.com",
    actblue_link = NA,
    anedot_link = "jineea-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00709758",
    FEC_ID_cand = "H0NY14262"
  ),
  tibble(
    first_name = "Badrun",
    last_name = "Khan",
    state_cd = "NY-14",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Badrun khan for Congress",
    campaign_website = "https://badrunkhan.com",
    actblue_link = "badrun-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00709527",
    FEC_ID_cand = "H0NY14304"
  ),
  tibble(
    first_name = "Fernando",
    last_name = "Cabrera",
    state_cd = "NY-14",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Fernando Cabrera For Congress",
    campaign_website = "https://www.cabreraforcongress.com",
    actblue_link = "cabrera-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00721753",
    FEC_ID_cand = "H0NY14338"
  ),
  tibble(
    first_name = "Antoine",
    last_name = "Tucker",
    state_cd = "NY-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tucker for Congress",
    campaign_website = "https://www.tucker.vote/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-500-1000-2800
    other_link = paste0(
      "https://secure.squarespace.com/checkout/donate?",
      "donatePageId=5cfa398a54b84800012a3a64"
    ),
    FEC_ID_cmte = "C00709162",
    FEC_ID_cand = "H0NY14254"
  ),
  tibble(
    first_name = "Miguel",
    last_name = "Hernandez",
    state_cd = "NY-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Miguel Hernandez for Congress",
    campaign_website = "https://www.miguelhernandezforcongress.org/",
    actblue_link = NA,
    # No prompts
    anedot_link = "miguel-hernandez-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00703769",
    FEC_ID_cand = "H0NY14239"
  ),
  tibble(
    first_name = "Ruth",
    last_name = "Papazian",
    state_cd = "NY-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ruth for Congress",
    campaign_website = "https://ruthforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 5-10-14-25-50-100-250-500-1000-2000-2800
    other_link = "https://papazian2020.nationbuilder.com/personal-donation",
    FEC_ID_cmte = "C00706234",
    FEC_ID_cand = "H0NY14247"
  ),
  tibble(
    first_name = "Israel Ortega", # I.O.C (!)
    last_name = "Cruz",
    state_cd = "NY-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Israel Ortega Cruz for Congress",
    campaign_website = "https://israelortegacruzforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Not associated with typical websites... not sure.
    # 1-5-10-35-50-100-250-500-1000-2800
    other_link = paste0(
      "https://israelortegacruzforcongress.com/donations/",
      "israel-ortega-cruz-for-congress-donation-form/"
    ),
    FEC_ID_cmte = "C00716316",
    FEC_ID_cand = "H0NY14312"
  ),
  tibble(
    first_name = "Rey",
    last_name = "Solano",
    state_cd = "NY-14",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Committee to Elect Rey Solano",
    campaign_website = "https://www.facebook.com/AmericaOverCommunism/",
    actblue_link = NA,
    anedot_link = NA,
    # No suggested amounts
    other_link = paste0(
      "https://www.gofundme.com/f/",
      "rey-solano-for-congress-america-over-communism"
    ),
    FEC_ID_cmte = "C00713073",
    FEC_ID_cand = "H0NY14288"
  ),
  tibble(
    first_name = "Jose",
    last_name = "Velazquez",
    state_cd = "NY-14",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jose Velazquez for Congress NY-14",
    campaign_website = "https://www.josevelazquez2020.com",
    # No donation link yet
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00720789",
    FEC_ID_cand = "H0NY14320"
  ),
  # Rally Robot is a trolling account
  # NY-15 (last updated Oct 24, 2020) ------------------------------------------
  # Serano retired; open seat
  tibble(
    first_name = "Ritchie",
    last_name = "Torres",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ritchie Torres for Congress",
    campaign_website = "https://www.torres.nyc",
    actblue_link = c(
      "rtorres-launch", # At least from Jun 5, 2020
      "ritchie-website"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00699744",
    FEC_ID_cand = "H0NY15160"
  ),
  tibble(
    first_name = "Patrick",
    last_name = "Delices",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Patrick Delices for Congress",
    campaign_website = "http://www.voteforpatrickdelices.com",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-150-250-500-1000-1500-200-2500
    other_link = "https://67320.campaignpartner.net/contribute",
    FEC_ID_cmte = "C00707075",
    FEC_ID_cand = "H0NY15202"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Blake",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Michael Blake for Congress",
    campaign_website = "https://www.blakeforcongress.nyc",
    actblue_link = "michaelblake-web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00703603",
    FEC_ID_cand = "H0NY15194"
  ),
  tibble(
    first_name = "Mellisa",
    last_name = "Mark-Viverito",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Melissa Mark-Viverito for the Bronx",
    campaign_website = "https://mmvforthebronx.com",
    actblue_link = "mmvbronx",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00715037",
    FEC_ID_cand = "H0NY15244"
  ),
  # https://www.nytimes.com/2020/06/09/nyregion/ruben-diaz-bronx-congress.html
  # Cannot find congress campaign site for Ruben Diaz
  tibble(
    first_name = "Ruben",
    last_name = "Diaz",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    # FB has http://www.rubendiaz.com/ but it's empty
    campaign_website = "https://bronx.com/rev-ruben-diaz-sr-for-congress/",
    # https://bronx.com/rev-ruben-diaz-sr-for-congress/
    actblue_link = "diaz2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00701862",
    FEC_ID_cand = "H0NY15186"
  ),
  tibble(
    first_name = "Ydanis",
    last_name = "Rodriguez",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ydanis for Congress",
    campaign_website = "https://www.ydanisforcongress.com",
    actblue_link = "ydanis-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00720417",
    FEC_ID_cand = "H0NY15269"
  ),
  tibble(
    first_name = "Tomas",
    last_name = "Ramos",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tomas for Congress",
    campaign_website = "https://www.tomasforcongress.com",
    actblue_link = c("tomasramos"), # , "ramos-launch"
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00700732",
    FEC_ID_cand = "H0NY15178"
  ),
  tibble(
    first_name = "Samelys",
    last_name = "Lopez",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = " Lopez for The People",
    campaign_website = "https://lopezforthepeople.com",
    actblue_link = c(
      "lftpwebsite",
      "lftpwebsite1"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00717256",
    FEC_ID_cand = "H0NY15277"
  ),
  tibble(
    first_name = "Marlene",
    last_name = "Cintron",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marlene Cintron for Congress",
    campaign_website = "https://www.marleneforthebronx.com",
    # "marlenecintron" is withdrawn but no formal signals
    actblue_link = "marlenecintron",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00715045",
    FEC_ID_cand = "H0NY15251"
  ),
  tibble(
    first_name = "Chivona",
    last_name = "Newsome",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chivona Newsome For Congress",
    campaign_website = "https://chivonanewsomeforcongress.com",
    actblue_link = "newsomeforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00703132",
    FEC_ID_cand = "H0NY15293"
  ),
  tibble(
    first_name = "Jonathan",
    last_name = "Ortiz",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ortiz for Congress",
    campaign_website = "https://www.ortiz2020.com",
    actblue_link = "ortiz2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00692012",
    FEC_ID_cand = "H0NY15152"
  ),
  tibble(
    first_name = "David",
    last_name = "Franks",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Franks Jr. for Congress",
    campaign_website = "https://davidpfranksjrforcongress.com",
    # Website not quite ready; no contribution link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00707281",
    FEC_ID_cand = "H0NY15210"
  ),
  # Rivera now running for State Senate
  tibble(
    first_name = "Frangell",
    last_name = "Basora",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Frangell Basora for New York",
    campaign_website = "https://www.frangellbasora2020.com",
    actblue_link = "frangellbasora2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00707901",
    FEC_ID_cand = "H0NY15236"
  ),
  tibble(
    first_name = "Julio",
    last_name = "Pabon",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Julio Pabon for Congress",
    campaign_website = "https://www.juliopabon.com",
    actblue_link = "bronxrevolution",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00732438",
    FEC_ID_cand = "H0NY15285"
  ),
  tibble(
    first_name = "Marlene",
    last_name = "Tapper",
    state_cd = "NY-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marlene Tapper for Congress",
    campaign_website = "https://www.marlenetapper.com/",
    actblue_link = "marlene-j--tapper-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00741926",
    FEC_ID_cand = "H0NY15301"
  ),
  # NY-16 (last updated Jul 13, 2021) ------------------------------------------
  # Engel vs. Bowman vs. Ghebreghiorgis vs. Fink vs. Ravelo vs. Belvin
  tibble(
    first_name = "Eliot",
    last_name = "Engel",
    state_cd = "NY-16",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://engel.house.gov",
    campaign_name = "Engel for Congress",
    campaign_website = "https://engelforcongress.com/",
    actblue_link = "engel2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00236513",
    FEC_ID_cand = "H8NY19058"
  ),
  tibble(
    first_name = "Jamaal",
    last_name = "Bowman",
    state_cd = "NY-16",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bowman for Congress",
    campaign_website = "https://www.bowmanforcongress.com",
    actblue_link = c(
      "jb-web-finalstretch", # At least from Jun 11, 2020
      "jamaal-ads-google",
      # "jamaal-launch",
      "jdjamaal-launch", # Justice Democrats: not sure if campaign initiated
      "jd-jamaal", # Justice Democrats: not sure if campaign initiated
      "pccc_2020_jamaalbowman_only_launchvideo"
      # "bowman-for-congress-nixon",
      # "bowman-for-congress-1",
      # "jb-email-20200504",
      # "jb-email-20200505",
      # "jb-email-20200506",
      # "jb-email-20200507",
      # "jb-email-20200508",
      # "jb-email-20200509",
      # "jb-email-20200510",
      # "jb-email-20200511",
      # "jb-email-20200512",
      # "jb-email-20200513",
      # "jb-email-20200514",
      # "jb-email-20200515",
      # "jb-email-20200516",
      # "jb-email-20200517",
      # "jb-email-20200518",
      # "jb-email-20200519",
      # "jb-email-20200520",
      # "jb-email-20200521",
      # "jb-email-20200522",
      # "jb-email-20200523",
      # "jb-email-20200524",
      # "jb-email-20200525",
      # "jb-email-20200526",
      # "jb-email-20200527",
      # "jb-email-20200528",
      # "jb-email-20200529",
      # "jb-email-20200530",
      # "jb-email-20200531",
      # "jb-email-20200601",
      # "jb-email-20200602",
      # "jb-email-20200603",
      # "jb-email-20200604",
      # "jb-email-20200605",
      # "jb-email-20200606",
      # "jb-email-20200607",
      # "jb-email-20200608",
      # "jb-email-20200609",
      # "jb-email-20200610",
      # "jb-email-20200611",
      # "jb-email-20200612",
      # "jb-email-20200613",
      # "jb-email-20200614",
      # "jb-email-20200615",
      # "jb-email-20200616",
      # "jb-email-20200617",
      # "jb-email-20200618",
      # "jb-email-20200619",
      # "jb-email-20200619",
      # "jb-email-20200619",
      # "jb-email-20200619",
      # "jb-email-20200620",
      # "jb-email-20200621",
      # "jb-email-20200622",
      # "jb-email-20200623",
      # "jb-email-20200624",
      # "jb-email-20200625",
      # "jb-email-20200626",
      # "jb-email-20200627",
      # "jb-popup-timeforachange2",
      # "closingad", # On Twitter May 26, 2020 and onward
      # "warren4bowman",
      # "aoc-jamaal-social",
      # "jb-515-social",
      # "jb-socialpush-20200523",
      # "jb-social-0531",
      # "jb-602-social",
      # "jb-603-social-fecdeadline",
      # "jb-606-social",
      # "jb-610-social",
      # "jb-611-social",
      # "jb-611-social2",
      # "jb-613-social",
      # "jb-614-social1",
      # "jb-614-social2",
      # "jb-615-social",
      # "jb-615-social2",
      # "jb-615-social3",
      # "jb-616-social",
      # "jb-616-social2",
      # "jb-616-social3",
      # "jb-617-social",
      # "jb-617-social2",
      # "jb-618-social",
      # "jb-618-social2",
      # "jb-618-social3",
      # "jb-618-social4",
      # "jb-619-social",
      # "jb-619-social2",
      # "jb-620-social",
      # "jb-620-social2",
      # "jb-620-social3",
      # "jb-620-social4",
      # "jb-621-social",
      # "jb-621-social2",
      # "jb-621-social3",
      # "jb-622-social",
      # "jb-622-social2",
      # "jb-623-social",
      # "jb-623-social2",
      # "jb-social-24",
      # "jb-629-social",
      # "jb-630-social",
      # "jb-630-social2"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00709196",
    FEC_ID_cand = "H0NY16143"
  ),
  # Yielded to Bowman but page is still live
  tibble(
    first_name = "Andom",
    last_name = "Ghebreghiorgis",
    state_cd = "NY-16",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Andom for NY",
    campaign_website = "https://www.andomforny.com",
    actblue_link = "andomforny",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00708941",
    FEC_ID_cand = "H0NY16135"
  ),
  tibble(
    first_name = "Chris",
    last_name = "Fink",
    state_cd = "NY-16",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chris Fink for Congress",
    campaign_website = "https://chrisfinkforcongress.com",
    actblue_link = "chrisfinkforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00726273",
    FEC_ID_cand = "H0NY16168"
  ),
  tibble(
    first_name = "Sammy", # Samuel, goes by Sammy
    last_name = "Ravelo",
    state_cd = "NY-16",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Samuel Ravelo for Congress",
    campaign_website = "https://www.sammyravelo.com",
    actblue_link = "sammy-ravelo-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710954",
    FEC_ID_cand = "H0NY16150"
  ),
  tibble(
    first_name = "Patrick",
    last_name = "McManus",
    state_cd = "NY-16",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "McManus for Congress",
    campaign_website = "https://www.mcmanusforcongress2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00759027",
    FEC_ID_cand = "H8NY15114"
  ),
  # Cannot find Kenny Belvin
  # NY-17 (last updated Oct 24, 2020) ------------------------------------------
  # Incumbent Nita Lowey just announced retirement (Oct 10, 2019); open seat
  # (her original ActBlue was nita-lowey-2, captured in file already)
  # Eisen vs. Buchwald
  # vs. Jones vs. Farkas vs. Fine vs. Carlucci vs. Parker vs. Hernandez
  # vs. Moreno-Casanova vs. Buchanan vs. Rodriguez-Wheeler vs. Katz
  # vs. Guin vs. Gottesfeld vs. Mcardle
  # Jarred Buchanan withdrew on Oct 16, 2019
  # Cannot find Moreno-Casanova, Katz, nor McArdle Schulman
  # Missed a key player Schleifer
  tibble(
    first_name = "Mondaire",
    last_name = "Jones",
    state_cd = "NY-17",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mondaire Jones for Congress",
    campaign_website = "https://mondaireforcongress.com",
    actblue_link = "mondaire",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711150",
    FEC_ID_cand = "H0NY17174"
  ),
  tibble(
    first_name = "Maureen",
    last_name = "Mcardle-Schulman",
    state_cd = "NY-17",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Maureen for Congress",
    campaign_website = "https://www.maureen4congress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-500-1000-2800
    other_link = "https://www.maureen4congress.com/donate/",
    FEC_ID_cmte = "C00737254",
    FEC_ID_cand = "H0NY17315"
  ),
  tibble(
    first_name = "Adam",
    last_name = "Schleifer",
    state_cd = "NY-17",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Adam Schleifer for Congress",
    campaign_website = "https://adamschleifer.com/",
    actblue_link = "adamforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00728600",
    FEC_ID_cand = "H0NY17273"
  ),
  tibble(
    first_name = "Evelyn",
    last_name = "Farkas",
    state_cd = "NY-17",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Evelyn Farkas Democrat for Congress",
    campaign_website = "https://evelynforny.com",
    actblue_link = "evelynfarkaswebsite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00727693",
    FEC_ID_cand = "H0NY17265"
  ),
  tibble(
    first_name = "David",
    last_name = "Buchwald",
    state_cd = "NY-17",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Buchwald for Congress",
    campaign_website = "https://www.davidbuchwaldforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # https://www.davidbuchwaldforcongress.com/donate
    # Hidden NGP
    other_link = "https://act.myngp.com/Forms/5205741614887077632",
    FEC_ID_cmte = "C00722983",
    FEC_ID_cand = "H0NY17190"
  ),
  tibble(
    first_name = "Josh",
    last_name = "Eisen",
    state_cd = "NY-17",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Eisen For Congress",
    campaign_website = "https://eisenforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/JoshEisen/donate",
    FEC_ID_cmte = "C00723437",
    FEC_ID_cand = "H0NY17208"
  ),
  tibble(
    first_name = "Allison",
    last_name = "Fine",
    state_cd = "NY-17",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Allison Fine for Congress",
    campaign_website = "https://www.allisonfine2020.com",
    actblue_link = "allison-fine-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00724997",
    FEC_ID_cand = "H0NY17240"
  ),
  tibble(
    first_name = "David",
    last_name = "Carlucci",
    state_cd = "NY-17",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Carlucci for Congress",
    campaign_website = "https://davidcarlucci.com",
    actblue_link = "carlucci2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00723791",
    FEC_ID_cand = "H0NY17224"
  ),
  tibble(
    first_name = "Catherine",
    last_name = "Parker",
    state_cd = "NY-17",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://catherineforcongress.com",
    # Gives a new pop-up. Uhh okay this is new, not sure it will screenshot
    actblue_link = "catherineforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00731422",
    FEC_ID_cand = "H0NY17281"
  ),
  tibble(
    first_name = "Asha Castleberry",
    last_name = "Hernandez",
    state_cd = "NY-17",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Asha for Congress",
    campaign_website = "https://www.ashaforcongress.com",
    actblue_link = "ashaforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00727040",
    FEC_ID_cand = "H0NY17257"
  ),
  # Cannot find Moreno-Casanova
  tibble(
    first_name = "Jo-Anna",
    last_name = "Rodriguez-Wheeler",
    state_cd = "NY-17",
    party = "",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://joannaforcongress.com",
    # Empty
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00723700",
    FEC_ID_cand = "H0NY17216"
  ),
  # Cannot find Katz
  tibble(
    first_name = "Delter",
    last_name = "Guin",
    state_cd = "NY-17",
    party = "",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://delterforthepeople.com",
    # Empty
    actblue_link = "delter-for-the-people-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00732636",
    FEC_ID_cand = "H0NY17299"
  ),
  tibble(
    first_name = "Yehudis",
    last_name = "Gottesfeld",
    state_cd = "NY-17",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Yehudis Gottesfeld for Congress",
    campaign_website = "https://yehudisforcongress.com",
    actblue_link = NA,
    anedot_link = "yehudisforcongress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00736363",
    FEC_ID_cand = "H0NY17307"
  ),
  # Cannot find McArdle Schulman
  # NY-18 (last updated Oct 24, 2020) ------------------------------------------
  tibble(
    first_name = "Sean",
    last_name = "Maloney",
    state_cd = "NY-18",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://seanmaloney.house.gov",
    campaign_name = "Sean Patrick Maloney for Congress",
    campaign_website = "https://seanmaloney.com/",
    actblue_link = c(
      "supportspm" # ,
      # "spmrapidresponse"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00512426",
    FEC_ID_cand = "H2NY22139"
  ),
  tibble(
    first_name = "Chele",
    last_name = "Farley",
    state_cd = "NY-18",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chele Farley for Congress",
    campaign_website = "http://www.chelefarley.com/",
    # Change documented to WinRed on Feb 27, 2020
    # At least from Jan 29, 2020
    # from 25-50-100-500-1000-2800-5600 to 25-50-100-500-1000-2800-5600
    # (same amounts)
    actblue_link = NA,
    # anedot_link = "chele-farley-for-congress-inc/donate",
    other_link = c(
      # At least from Jun 22, 2020
      "https://secure.winred.com/chelefarley/main_donate",
      "https://secure.winred.com/chelefarley/donate"
    ),
    FEC_ID_cmte = "C00701433",
    FEC_ID_cand = "H0NY18172"
  ),
  tibble(
    first_name = "Scott",
    last_name = "Smith",
    state_cd = "NY-18",
    party = "Lib",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Scott Smith for Congress",
    campaign_website = "https://mrsmithforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 5-10-25-50-100-200-500-1000-1500-2000-2500-2700
    # Hidden; revealed after email entry
    other_link = paste0(
      "https://secure.squarespace.com/checkout/donate?",
      "donatePageId=541e333de4b02b592c3c4748&ss_cid=3ceb164b-2edd-42fc-b03d-",
      "0a0367e58ff6&ss_cvisit=1569512817849&ss_cvr=c15261db-0b11-4552-8347-3",
      "b7674faf4eb%7C1569512819393%7C1569512819393%7C1569512819393%7C1"
    ),
    FEC_ID_cmte = "C00565465",
    FEC_ID_cand = "H4NY18109"
  ),
  # NY-19 (last updated Jul 13, 2021) ------------------------------------------
  tibble(
    first_name = "Antonio",
    last_name = "Delgado",
    state_cd = "NY-19",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://delgado.house.gov",
    campaign_name = "Antonio Delgado for Congress",
    campaign_website = "https://delgadoforcongress.com",
    actblue_link = "adfc_website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00633859",
    FEC_ID_cand = "H8NY19181"
  ),
  tibble(
    first_name = "Kyle",
    last_name = "Van De Water",
    state_cd = "NY-19",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kyle for Congress",
    campaign_website = "https://www.kyleforny19.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/van-de-water-for-congress/donate",
    FEC_ID_cmte = c("C00737650", "C00702803"),
    FEC_ID_cand = "H0NY19204"
  ),
  # Tony German has dropped out
  tibble(
    first_name = "Ola",
    last_name = "Hawatmeh",
    state_cd = "NY-19",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ola for Congress",
    campaign_website = "https://olaforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/olaforcongress/donate",
    FEC_ID_cmte = "C00714907",
    FEC_ID_cand = "H0NY19188"
  ),
  tibble(
    first_name = "Mike", # Michael, goes by Mike
    last_name = "Roth",
    state_cd = "NY-19",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mike Roth For The US Congress",
    # Mostly empty, no donation link
    campaign_website = "https://wewantstate.us/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00713768",
    FEC_ID_cand = "H0NY19170"
  ),
  tibble(
    first_name = "Kyle",
    last_name = "Van De Water",
    state_cd = "NY-19",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kyle Van De Water for Congress",
    campaign_website = "https://www.kyleforny19.com/",
    # Just FB, no donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00737650",
    FEC_ID_cand = "H0NY19204"
  ),
  tibble(
    first_name = "Steven",
    last_name = "Greenfield",
    state_cd = "NY-19",
    party = "Grn",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Steven Greenfield for Congress",
    campaign_website = "https://stevegreenfieldforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://stevegreenfieldforcongress.com/fund-me/",
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H2NY22097"
  ),
  # NY-20 (last updated Oct 24, 2020) ------------------------------------------
  # Tonko vs. Joy vs. Seney
  tibble(
    first_name = "Paul",
    last_name = "Tonko",
    state_cd = "NY-20",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://tonko.house.gov",
    campaign_name = "Paul Tonko for Congress",
    campaign_website = "https://paultonko.net",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.ngpvan.com/Kc_4TB6OfUqzi_V_cUHm6g2",
    FEC_ID_cmte = "C00450049",
    FEC_ID_cand = "H8NY21203"
  ),
  tibble(
    first_name = "Elizabeth",
    last_name = "Joy",
    state_cd = "NY-20",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = " Joy for NY",
    campaign_website = "http://joyforny.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/joy-for-ny/donate",
    FEC_ID_cmte = "C00701755",
    FEC_ID_cand = "H0NY20111"
  ),
  tibble(
    first_name = "Mike", # Michael
    last_name = "Seney",
    state_cd = "NY-20",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Seney for Congress",
    # https://seneyforcongress.com is down; cannot find donation link
    campaign_website = "https://twitter.com/MikeSeneyNY20",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00724815",
    FEC_ID_cand = "H0NY20129"
  ),
  # NY-21 (last updated Oct 18, 2020) ------------------------------------------
  # Stefanik vs. Cobb
  tibble(
    first_name = "Elise",
    last_name = "Stefanik",
    state_cd = "NY-21",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://stefanik.house.gov",
    campaign_name = "Elise for Congress",
    campaign_website = "http://www.eliseforcongress.com",
    # Anedot now, but others also exist (as of Nov 15, 2019)
    # Switched entirely to WinRed as of Nov 18, 2019
    # 5-10-25-50-100-250-500-1000-2800 to 5-10-25-50-100-250-500-1000-2800-5600
    # Addition of 5600 (I'm still curious about this...)
    actblue_link = NA,
    # anedot_link = c(
    #   "elise-for-congress/website-donations",
    #   "elise-for-congress/impeachment-trials"
    # ),
    # Split between Stefanik and NRSC though.
    other_link = c(
      "https://secure.winred.com/elisestefanik/pg-site-donate",
      "https://secure.winred.com/EliseStefanik/donate",
      "https://secure.winred.com/nrsc/nrsc-stefanik"
    ),
    FEC_ID_cmte = "C00547893",
    FEC_ID_cand = "H4NY21079"
  ),
  tibble(
    first_name = "Tedra",
    last_name = "Cobb",
    state_cd = "NY-21",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tedra Cobb for Congress",
    campaign_website = "https://www.tedracobb.com",
    actblue_link = "tedra-cobb-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00649061",
    FEC_ID_cand = "H8NY21245"
  ),
  # NY-22 (last updated Oct 17, 2020) ------------------------------------------
  tibble(
    first_name = "Anthony",
    last_name = "Brindisi",
    state_cd = "NY-22",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://brindisi.house.gov",
    campaign_name = "Anthony Brindisi for Congress",
    campaign_website = "https://brindisiforcongress.com/",
    actblue_link = "abfc2017",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00648725",
    FEC_ID_cand = "H8NY22151"
  ),
  tibble(
    first_name = "Claudia",
    last_name = "Tenney",
    state_cd = "NY-22",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Claudia Tenney For Congress",
    campaign_website = "https://claudiaforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/tenney-for-congress/donate",
    FEC_ID_cmte = c("C00632828", "C00561183"),
    FEC_ID_cand = "H4NY22051"
  ),
  tibble(
    first_name = "George",
    last_name = "Philips",
    state_cd = "NY-22",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "George Philips for Congress",
    campaign_website = "https://georgephillips.us/",
    actblue_link = NA,
    anedot_link = "phillips/donate",
    other_link = NA,
    FEC_ID_cmte = "C00697920",
    FEC_ID_cand = "H6NY22098"
  ),
  # Withdrew Jan 31, 2020
  tibble(
    first_name = "Steve",
    last_name = "Cornwell",
    state_cd = "NY-22",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cornwell for Congress",
    campaign_website = "https://cornwellforcongress.com/",
    actblue_link = NA,
    anedot_link = "cornwell-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00712067",
    FEC_ID_cand = "H0NY22083"
  ),
  # Now running for councilman
  tibble(
    first_name = "Franklin",
    last_name = "Sager",
    state_cd = "NY-22",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sager for America",
    # Website is now empty
    campaign_website = "https://sagerforamerica.org/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-100-500
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "token=pS0-wLVvirP3bsu6ZwiohILEy7ail6UCU0ae7sMMfjG6kfGakV9hPN5e",
      "-qDwWDP16Xyow0&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00694331",
    FEC_ID_cand = "H0NY22075"
  ),
  # NY-23 (last updated Oct 17, 2020) ------------------------------------------
  # Reed vs. Mitrano vs. Noren
  tibble(
    first_name = "Tom",
    last_name = "Reed",
    state_cd = "NY-23",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://reed.house.gov",
    campaign_name = "Tom Reed for Congress",
    campaign_website = "https://tomreedforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/tom-reed-for-congress/donate",
    FEC_ID_cmte = "C00464032",
    FEC_ID_cand = "H0NY29054"
  ),
  tibble(
    first_name = "Tracy",
    last_name = "Mitrano",
    state_cd = "NY-23",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tracy Mitrano for Congress",
    campaign_website = "https://mitrano2020.com",
    actblue_link = "mitrano2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00654525",
    FEC_ID_cand = "H8NY23183"
  ),
  tibble(
    first_name = "Scott",
    last_name = "Noren",
    state_cd = "NY-23",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Scott Noren For Congress",
    campaign_website = "https://scottnorenforcongress.com",
    actblue_link = "scott-noren-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00706093",
    FEC_ID_cand = "H0NY23123"
  ),
  # NY-24 (last updated Oct 17, 2020) ------------------------------------------
  tibble(
    first_name = "John",
    last_name = "Katko",
    state_cd = "NY-24",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://katko.house.gov",
    campaign_name = "John Katko for Congress",
    campaign_website = "https://www.johnkatkoforcongress.com",
    actblue_link = NA,
    anedot_link = "katko-for-congress/katko-website-donate",
    other_link = NA,
    FEC_ID_cmte = "C00556365",
    FEC_ID_cand = "H4NY24073"
  ),
  tibble(
    first_name = "Dana",
    last_name = "Balter",
    state_cd = "NY-24",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dana Balter for Congress",
    campaign_website = "https://electdanabalter.com",
    actblue_link = "balter-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00655183",
    FEC_ID_cand = "H8NY24090"
  ),
  tibble(
    first_name = "Francis",
    last_name = "Conole",
    state_cd = "NY-24",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Conole for Congress",
    campaign_website = "https://conoleforcongress.com",
    actblue_link = c(
      "fc-website", # Not sure when changed
      "fc-default"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00702191",
    FEC_ID_cand = "H0NY24063"
  ),
  # Misso withdrew on Mar 14, 2020
  tibble(
    first_name = "Roger",
    last_name = "Misso",
    state_cd = "NY-24",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Roger Misso for United States Congress",
    campaign_website = "https://rogermisso.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00701227",
    FEC_ID_cand = "H0NY24055"
  ),
  # NY-25 (last updated Oct 17, 2020) ------------------------------------------
  # Morelle vs. Wilt vs. Mitris vs. Wilson
  tibble(
    first_name = "Joe",
    last_name = "Morelle",
    state_cd = "NY-25",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://morelle.house.gov",
    campaign_name = "Joe Morelle for Congress",
    campaign_website = "https://www.votemorelle.com",
    actblue_link = c(
      "aa-mar20", # At least from Jun 3, 2020
      "morelle"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00675108",
    FEC_ID_cand = "H8NY25105"
  ),
  tibble(
    first_name = "George",
    last_name = "Mitris",
    state_cd = "NY-25",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.mitrisforcongress2020.com",
    actblue_link = NA,
    anedot_link = NA,
    # Prompts for 50-100-250
    # Now 250-500-1000
    other_link = paste0(
      "https://www.paypal.com/donate/?token=OiXNGXXAdwMJ39PcGuQinJumy",
      "__AIi3U-aVYqNYJnl-q65MOUvd1TJG-xFJ_jze-7omNAW&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00736736",
    FEC_ID_cand = "H0NY25094"
  ),
  tibble(
    first_name = "Robin",
    last_name = "Wilt",
    state_cd = "NY-25",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Robin Wilt for Congress",
    campaign_website = "https://www.wiltforcongress.com",
    actblue_link = "wilt-for-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00674788",
    FEC_ID_cand = "H8NY25113"
  ),
  tibble(
    first_name = "Kevin",
    last_name = "Wilson",
    state_cd = "NY-25",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kevin Wilson for Congress",
    campaign_website = "http://kevinwilsonforcongress.com",
    actblue_link = NA,
    anedot_link = "kevin-wilson-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00741439",
    FEC_ID_cand = "H0NY25102"
  ),
  # NY-26 (last updated Jul 13, 2021) ------------------------------------------
  # Higgins vs. Egriu vs. Raleigh vs. Donovan vs. Samsel
  # Cannot find Egriu (proper website)
  # https://ballotpedia.org/Emin_Eddie_Egriu
  tibble(
    first_name = "Brian",
    last_name = "Higgins",
    state_cd = "NY-26",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://higgins.house.gov",
    campaign_name = "Brian Higgins for Congress",
    campaign_website = "https://www.higginsforcongress.com",
    actblue_link = "brianhiggins",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00401034",
    FEC_ID_cand = "H4NY27076"
  ),
  tibble(
    first_name = "Ricky",
    last_name = "Donovan",
    state_cd = "NY-26",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Donovan for Congress",
    campaign_website = "https://www.facebook.com/donovan4congress/",
    actblue_link = NA,
    anedot_link = NA,
    # Mangled; not even a proper PayPal link
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "token=5SOqXyf2JV8koYPl770O8coFP1I3AZ_pl_YiVMjJ4ANoGTwA4FEv91BI",
      "YXk4g1JPqNYyem&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00740969",
    FEC_ID_cand = "H0NY26084"
  ),
  tibble(
    first_name = "Emin",
    last_name = "Egriu",
    state_cd = "NY-26",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Egriu For Congress",
    campaign_website = "http://egriuforcongress.com",
    actblue_link = "egriu-for-congress-2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00557223",
    FEC_ID_cand = "H0NY26068"
  ),
  tibble(
    first_name = "Mile",
    last_name = "Raleigh",
    state_cd = "NY-26",
    party = "Grn",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Raleigh For Congress",
    campaign_website = "https://www.raleighforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00739912",
    FEC_ID_cand = "H0NY26076"
  ),
  # Cannot find Ricky Donovan
  # James Samsel no website
  # NY-27 (last updated Oct 17, 2020) ------------------------------------------
  # Chris Collins just resigned; open seat
  # Jacobs vs. Parlato
  # vs. McMurray vs. Ortt vs. Whitmer vs. Smierciak vs. Mychajliw
  tibble(
    # I had switched last/first name. Sort out files later
    first_name = "Christopher",
    last_name = "Jacobs",
    state_cd = "NY-27",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jacobs for Congress",
    campaign_website = "https://www.jacobs4congress.com",
    actblue_link = NA,
    anedot_link = "jacobs-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00706788",
    FEC_ID_cand = "H0NY27090"
  ),
  tibble(
    first_name = "Nate",
    last_name = "McMurray",
    state_cd = "NY-27",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "McMurray for Congress",
    campaign_website = "https://votemcmurray.com",
    actblue_link = c(
      # Change within ActBlue documented Oct 17, 2020
      "mcmurray2020",
      "specialelectionny27", # At least from Apr 9, 2020
      "nate-mcmurray"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00666453",
    FEC_ID_cand = "H8NY27176"
  ),
  tibble(
    first_name = "Duane",
    last_name = "Whitmer",
    state_cd = "NY-27",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Duane Whitmer for Congress",
    campaign_website = "https://duanewhitmer.com",
    actblue_link = NA,
    anedot_link = "djw4congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00694547",
    FEC_ID_cand = "H0NY27082"
  ),
  tibble(
    first_name = "Beth",
    last_name = "Parlato",
    state_cd = "NY-27",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Beth for Congress",
    campaign_website = "https://bethforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # from 100-250-500-1000-2800 to 5-15-25-50-100-250-500-1000-2800
    # other_link = "https://bethforcongress.ichooseapex.com/donate/",
    other_link = "https://secure.winred.com/BethParlato/donate",
    FEC_ID_cmte = "C00713859",
    FEC_ID_cand = "H0NY27108"
  ),
  tibble(
    first_name = "Rob",
    last_name = "Ortt",
    state_cd = "NY-27",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rob Ortt for Congress",
    campaign_website = "https://www.orttforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/ortt-for-congress/donate",
    FEC_ID_cmte = "C00715847",
    FEC_ID_cand = "H0NY27116"
  ),
  tibble(
    first_name = "Frank",
    last_name = "Smierciak",
    state_cd = "NY-27",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Smierciak for Congress",
    campaign_website = "http://www.fcsforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts immediately, but may be hidden
    other_link = "http://www.fcsforcongress.com/donate",
    FEC_ID_cmte = "C00721415",
    FEC_ID_cand = "H0NY27124"
  ),
  tibble(
    first_name = "Stefan",
    last_name = "Mychajliw",
    state_cd = "NY-27",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Stefan for Congress",
    campaign_website = "https://votestefan.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/stefan-for-congress/donate",
    FEC_ID_cmte = "C00735811",
    FEC_ID_cand = "H0NY27132"
  ),
  # NC: North Carolina (last updated Jul 12, 2021) =============================
  # NC-01 (last updated Oct 17, 2020) ------------------------------------------
  # Butterfield vs. Smith vs. Nix vs. Carter vs. Baca
  tibble(
    first_name = "G.K.",
    last_name = "Butterfield",
    state_cd = "NC-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://butterfield.house.gov",
    campaign_name = "G.K. Butterfield for Congress",
    campaign_website = "http://www.gkbutterfield.com/home/",
    actblue_link = "gkbutterfield",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00401190",
    FEC_ID_cand = "H4NC01046"
  ),
  tibble(
    first_name = "Sandy",
    last_name = "Smith",
    state_cd = "NC-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sandy Smith for Congress",
    campaign_website = "https://sandysmithnc.com",
    actblue_link = NA,
    anedot_link = "sandysmithforcongress/website",
    other_link = NA,
    FEC_ID_cmte = "C00697250",
    FEC_ID_cand = "H0NC03149"
  ),
  tibble(
    first_name = "Michele",
    last_name = "Nix",
    state_cd = "NC-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Michele Nix - US Congress",
    campaign_website = "https://michelenix.com",
    actblue_link = NA,
    anedot_link = "michelenix/nix-donate",
    other_link = NA,
    FEC_ID_cmte = "C00697623",
    FEC_ID_cand = "H0NC03131"
  ),
  tibble(
    first_name = "DeAndre",
    last_name = "Carter",
    state_cd = "NC-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "DeAndre for Congress",
    campaign_website = "https://www.dacforcongress.com",
    actblue_link = "deandrecarter",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00694000",
    FEC_ID_cand = "H0NC01093"
  ),
  tibble(
    first_name = "Ethan",
    last_name = "Baca",
    state_cd = "NC-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ethan Baca for Congress",
    campaign_website = "https://www.ethanpbaca.com",
    actblue_link = NA,
    anedot_link = "ethan-baca-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00723445",
    FEC_ID_cand = "H0NC01101"
  ),
  # NC-02 (last updated Oct 17, 2020) ------------------------------------------
  # Holding vs. Cooper vs. Swenson vs. Butler vs. Johnson-Hostler
  # Cannot find Swenson, Johnson-Hostler running for Wake County Board of Edu
  tibble(
    first_name = "Deborah",
    last_name = "Ross",
    state_cd = "NC-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Deborah Ross for Congress",
    campaign_website = "https://deborahross.com",
    actblue_link = "deborah-ross-5",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00729277",
    FEC_ID_cand = "H0NC02125"
  ),
  tibble(
    first_name = "Alan",
    last_name = "Swain",
    state_cd = "NC-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Swain for Congress",
    campaign_website = "https://www.swainforcongress.com",
    actblue_link = NA,
    anedot_link = "swain-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00731885",
    FEC_ID_cand = "H0NC02158"
  ),
  tibble(
    first_name = "George",
    last_name = "Holding",
    state_cd = "NC-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://holding.house.gov",
    campaign_name = "George Holding for Congress",
    campaign_website = "https://georgeholdingforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://www.trailblz.info/HoldingGeorge/Donations.aspx?",
      "rdif=http%3a%2f%2fgeorgeholdingforcongress.com%2fthank-you%2f&ver=2"
    ),
    FEC_ID_cmte = "C00499236",
    FEC_ID_cand = "H2NC13110"
  ),
  # Dropped out Dec 10, 2019
  tibble(
    first_name = "Scott",
    last_name = "Cooper",
    state_cd = "NC-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Scott Cooper for Congress",
    campaign_website = "https://cooperfornc.com",
    actblue_link = "cooperfornc-landing",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00700922",
    FEC_ID_cand = "H0NC02083"
  ),
  tibble(
    first_name = "Jason",
    last_name = "Butler",
    state_cd = "NC-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jason Butler for Congress",
    campaign_website = "https://butlerfornc.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.ngpvan.com/AO7-01ecvEmFCo-Y3wX8fA2",
    FEC_ID_cmte = "C00714121",
    FEC_ID_cand = "H0NC02109"
  ),
  tibble(
    first_name = "Monika",
    last_name = "Johnson-Hostler",
    state_cd = "NC-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Monika for Congress",
    # Not http://www.monikaforschools.com
    campaign_website = "https://monikaforcongress.com",
    actblue_link = "monika-johnson-hostler-for-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00714972",
    FEC_ID_cand = "H0NC02117"
  ),
  # NC-03 (last updated Oct 17, 2020) ------------------------------------------
  # Mixed with special election candidates, I'm sure
  # Thomas, Johnson paused receipts (thomas_homepage, ike2019)
  # Websites down for Perry, Bew, Moore, De Luca, Cairns, Law, Nix, Outlaw,
  # Speciale, Baiko, Rollason, Reeves
  # Cannot find Rouse, Boyd, Cox, Humphrey
  # Michael Henry Tabor... but goes by Mike Payment? Paypal still exists, but..
  # See https://ballotpedia.org/North_Carolina%27s_3rd_Congressional_District_election,_2020
  tibble(
    first_name = "Greg Francis",
    last_name = "Murphy",
    state_cd = "NC-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://gregmurphy.house.gov",
    campaign_name = "Dr. Greg Murphy for Congress",
    campaign_website = "https://murphy4congress.com",
    actblue_link = NA,
    anedot_link = "murphy4congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00697649",
    FEC_ID_cand = "H0NC03172"
  ),
  tibble(
    first_name = "Daryl",
    last_name = "Farrow",
    state_cd = "NC-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elect Daryl Farrow for U.S. Congress",
    campaign_website = "https://dtfarrow.com",
    # Cannot find donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00752345",
    FEC_ID_cand = "H0NC03123"
  ),
  tibble(
    first_name = "Phil",
    last_name = "Shepard",
    state_cd = "NC-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Phil Shepard for Congress",
    campaign_website = "https://philshepard4congress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.piryx.com/donate/mvIi2zso/Phil-Shepard-for-Congress/",
    FEC_ID_cmte = "C00698555",
    FEC_ID_cand = "H0NC03230"
  ),
  tibble(
    first_name = "Chimer",
    last_name = "Clark",
    state_cd = "NC-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chimer Clark for Congress",
    campaign_website = "http://www.chimerclarkforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://secure.squarespace.com/checkout/donate?donatePageId=",
      "5c7ed805971a1847839049f9&ss_cid=9198eb64-ab41-4a44-a983-2af3",
      "f3835c0c&ss_cvisit=1570586694195&ss_cvr=bd06f10d-5e2d-4c87-9",
      "d21-c7262f8bc3e5%7C1570586695870%7C1570586695870%7C1570586695870%7C1"
    ),
    FEC_ID_cmte = "C00699777",
    FEC_ID_cand = "H0NC03289"
  ),
  # NC-04 (last updated Oct 17, 2020) ------------------------------------------
  # Price vs. Sarkar vs. Lockwood vs. Sobie vs. Brock vs. Loor
  # Cannot find Brock
  tibble(
    first_name = "David",
    last_name = "Price",
    state_cd = "NC-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://price.house.gov",
    campaign_name = "",
    campaign_website = "https://priceforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://actions.ngpvan.com/v1/Forms/C6jeZnRxfUWonMSV5wyOVA2",
    FEC_ID_cmte = "C00195628",
    FEC_ID_cand = "H6NC04037"
  ),
  # Not on FEC
  tibble(
    first_name = "Robert",
    last_name = "Thomas",
    state_cd = "NC-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://robertthomasforcongress.com",
    actblue_link = NA,
    anedot_link = "robert-thomas-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Debesh",
    last_name = "Sarkar",
    state_cd = "NC-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Debesh Sarkar 2020",
    campaign_website = "https://www.debeshsarkar2020.com",
    # Cannot find donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00733162", "C00733154"),
    FEC_ID_cand = "H0NC04204"
  ),
  # Dropped out
  tibble(
    first_name = "Curtis",
    last_name = "Sobie",
    state_cd = "NC-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Curtis J. Sobie for Congress",
    campaign_website = "https://www.sobie2020.us",
    actblue_link = "sobie2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00696625",
    FEC_ID_cand = "H0NC04162"
  ),
  tibble(
    first_name = "Daniel Ulysses",
    last_name = "Lockwood",
    state_cd = "NC-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Daniel Ulysses Lockwood for Congress",
    campaign_website = "https://www.lockwood4nc.com",
    actblue_link = "lockwood4nc",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00702159",
    FEC_ID_cand = "H0NC04170"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Judson",
    state_cd = "NC-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Judson for Congress",
    campaign_website = "https://markjudsonforcongress.org",
    actblue_link = "mark-judson-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711101",
    FEC_ID_cand = "H0NC04188"
  ),
  tibble(
    first_name = "Steve",
    last_name = "Von Loor", # Loor, goes by Von Loor
    state_cd = "NC-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Von Loor for U.S. Congress NC 4th",
    campaign_website = "https://www.steveavonloor.org",
    actblue_link = NA,
    anedot_link = "vonloor4congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00653535",
    FEC_ID_cand = "H8NC04090"
  ),
  # NC-05 (last updated Oct 17, 2020) ------------------------------------------
  # Foxx vs. Supin vs. Brown vs. Gregory
  tibble(
    first_name = "Virginia",
    last_name = "Foxx",
    state_cd = "NC-05",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://foxx.house.gov",
    campaign_name = "Virginia Foxx for Congress",
    campaign_website = "https://virginiafoxx.com",
    actblue_link = NA,
    anedot_link = "virginia-foxx-for-congress/the-general-fund-c2dc7f90233",
    other_link = NA,
    FEC_ID_cmte = "C00386748",
    FEC_ID_cand = "H4NC05146"
  ),
  tibble(
    first_name = "David Wilson",
    last_name = "Brown",
    state_cd = "NC-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Wilson Brown for Congress",
    campaign_website = "https://dwb4congress.com",
    actblue_link = "dwb4congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00649806",
    FEC_ID_cand = "H8NC10097"
  ),
  tibble(
    first_name = "Jeanne",
    last_name = "Supin",
    state_cd = "NC-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jeanne Supin for Congress",
    campaign_website = "http://jeannesupinforcongress.com",
    actblue_link = "jeanne-supin-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00707588",
    FEC_ID_cand = "H0NC05169"
  ),
  tibble(
    first_name = "Jeff",
    last_name = "Gregory",
    state_cd = "NC-05",
    party = "Con",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jeff Gregory For Congress",
    campaign_website = paste0(
      "https://www.facebook.com/pages/category/Politician/",
      "Jeff-Gregory-For-Congress-104867906212415/"
    ),
    # Don't see a donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00610444", "C00478933"),
    FEC_ID_cand = "H0NC10177"
  ),
  # NC-06 (last updated Oct 17, 2020) ------------------------------------------
  # Walker vs. Flynn vs. Bucardo
  # Walker is retiring
  tibble(
    first_name = "Kathy",
    last_name = "Manning",
    state_cd = "NC-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kathy Manning for Congress",
    campaign_website = "https://www.kathymanningfornc.com",
    actblue_link = "manning-site",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00662577",
    FEC_ID_cand = "H8NC13067"
  ),
  tibble(
    first_name = "Lee", # Joseph Lee
    last_name = "Haywood",
    state_cd = "NC-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lee Haywood for Congress",
    campaign_website = "https://www.leehaywood.com",
    actblue_link = NA,
    anedot_link = "lee-haywood-for-congress-committee/donate",
    other_link = NA,
    FEC_ID_cmte = "C00732651",
    FEC_ID_cand = "H0NC06241"
  ),
  tibble(
    first_name = "Mark", # Bradley Mark, goes by Mark
    last_name = "Walker",
    state_cd = "NC-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = "https://walker.house.gov",
    campaign_name = "Walker for NC",
    campaign_website = "https://walker4nc.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://mystique.victorypassport.com/pages/walker4nc/",
      "mark-walker-grassroots?location=https%3A%2F%2Fgive.victorypassport.com",
      "%2Fwalker4nc%2Fdonate&location_url=aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9",
      "ydC5jb20vd2Fsa2VyNG5jL2RvbmF0ZS8%2FcmVjdXJyaW5nPXRydWUmdXRtX21lZGl1bT1",
      "3ZWJzaXRlJnV0bV9zb3VyY2U9aG9tZXBhZ2UmdXRtX2NvbnRlbnQ9ZG9uYXRlX2hlYWRlc",
      "iM%3D"
    ),
    FEC_ID_cmte = "C00543231",
    FEC_ID_cand = "H4NC06052"
  ),
  tibble(
    first_name = "Angela",
    last_name = "Flynn",
    state_cd = "NC-06",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Angela Flynn for NC District 6",
    campaign_website = "https://www.angelaflynnforcongress.com",
    actblue_link = "flynn4congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00700740",
    FEC_ID_cand = "H0NC06191"
  ),
  tibble(
    first_name = "Jennyfer",
    last_name = "Bucardo",
    state_cd = "NC-06",
    party = "NPP",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jen for Congress",
    campaign_website = "https://www.jenforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 5-50-500
    other_link = paste0(
      "https://www.paypal.com/donate/?token=MTFo3e-LFbtvrm3xHk2dPCp3SjivdZVi",
      "-bj0lcjOgVPSoizsb43d7UMKA3jwZfY-8v8E3m&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00716159",
    FEC_ID_cand = "H0NC06209"
  ),
  # NC-07 (last updated Oct 17, 2020) ------------------------------------------
  # Rouzer vs. D'Abrosca vs. Judson
  tibble(
    first_name = "David",
    last_name = "Rouzer",
    state_cd = "NC-07",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://rouzer.house.gov",
    campaign_name = "Rouzer for Congress",
    campaign_website = "http://davidrouzer.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/DavidRouzer/donate",
    FEC_ID_cmte = "C00501643",
    FEC_ID_cand = "H2NC07096"
  ),
  tibble(
    first_name = "Chris", # Christopher
    last_name = "Ward",
    state_cd = "NC-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chris Ward for Congress",
    campaign_website = "https://www.chriswardforcongress.com",
    actblue_link = "chriswardforcongress2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00732040",
    FEC_ID_cand = "H0NC07140"
  ),
  tibble(
    first_name = "Pete",
    last_name = "D'Abrosca",
    state_cd = "NC-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Pete D'Abrosca for Congress",
    campaign_website = "https://petefornc.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/PeterDabrosca/donate",
    FEC_ID_cmte = "C00715300",
    FEC_ID_cand = "H0NC07124"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Judson",
    state_cd = "NC-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Judson for Congress",
    campaign_website = "https://markjudsonforcongress.org",
    actblue_link = "mark-judson-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711101",
    FEC_ID_cand = "H0NC04188"
  ),
  # NC-08 (last updated Oct 17, 2020) ------------------------------------------
  # Hudson vs. Timmons-Goodson vs. Walker
  tibble(
    first_name = "Richard",
    last_name = "Hudson",
    state_cd = "NC-08",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://hudson.house.gov",
    campaign_name = "Richard Hudson for Congress",
    campaign_website = "https://richardhudson.org",
    actblue_link = NA,
    # anedot_link = "richard-hudson-for-congress/c2bd423ccef7148804daa",
    anedot_link = NA,
    # Change documented to WinRed on Oct 17, 2020
    # At least from Aug 17, 2020
    other_link = "https://secure.winred.com/hudson-for-congress/main-donate",
    FEC_ID_cmte = "C00504522",
    FEC_ID_cand = "H2NC08185"
  ),
  tibble(
    first_name = "Pat", # Patricia, goes by Pat
    last_name = "Timmons-Goodson",
    state_cd = "NC-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Pat Timmons-Goodson for Congress",
    campaign_website = "https://www.timmonsgoodsonforcongress.com",
    actblue_link = c(
      "patricia-timmons-goodson-for-congress-1" # ,
      # "blu-patt-20q3-d2d-search"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00732800",
    FEC_ID_cand = "H0NC08254"
  ),
  # Huffman in the 8th district was a residue from 2018
  tibble(
    first_name = "Scott",
    last_name = "Huffman",
    state_cd = "NC-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Scott Huffman for Congress",
    campaign_website = "https://www.scotthuffman.com",
    actblue_link = "huffmanfornc",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00696492", "C00663229"),
    FEC_ID_cand = "H8NC08109"
  ),
  # Walker withdrew
  tibble(
    first_name = "Rodney",
    last_name = "Walker",
    state_cd = "NC-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Walker for Congress",
    campaign_website = "https://www.walkerforcongressnc.com",
    actblue_link = "ncwalker2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00707877",
    FEC_ID_cand = "H0NC08247"
  ),
  # NC-09 (last updated Oct 17, 2020) ------------------------------------------
  # McCready vs. Bishop (2019 Sep Special Election)
  # vs. Brown vs. Ridenhour vs. Rushing vs. Hull vs. Shubert vs.
  # Smith vs. Wiley vs. Cano
  # Can no longer find Rushing, Wiley, Blackwelder, Bishop
  # Pittenger seems to be rollover from 2018
  # McCready and Hull paused accepting donations, but who knows...
  tibble(
    first_name = "Dan",
    last_name = "Bishop",
    state_cd = "NC-09",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://danbishop.house.gov",
    campaign_name = "Bishop for Congress",
    campaign_website = "https://votedanbishop.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/dan-bishop/bishop-for-congress",
    FEC_ID_cmte = "C00699660",
    FEC_ID_cand = "H0NC09187"
  ),
  tibble(
    first_name = "Cynthia",
    last_name = "Wallace",
    state_cd = "NC-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cynthia Wallace for Congress",
    campaign_website = "https://www.cynthiawallace.com",
    actblue_link = "cynthia-wallace-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00731919",
    FEC_ID_cand = "H0NC09286"
  ),
  tibble(
    first_name = "Dan",
    last_name = "McCready",
    state_cd = "NC-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "McCready for Congress",
    campaign_website = "https://www.danmccready.com/",
    # "mccready-web", "dmc-august, No longer accepting contributions
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00641381",
    FEC_ID_cand = "H8NC09123"
  ),
  tibble(
    first_name = "Leigh",
    last_name = "Brown",
    state_cd = "NC-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Leigh Brown for Congress",
    campaign_website = "https://www.leighbrownforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://mystique.victorypassport.com/pages/leighbrown/",
      "donate-today?location=https%3A%2F%2Fgive.victorypassport.com%2",
      "Fleighbrown%2Fdonate-today&location_url=aHR0cHM6Ly9naXZlLnZpY3",
      "RvcnlwYXNzcG9ydC5jb20vbGVpZ2hicm93bi9kb25hdGUtdG9kYXkvIw%3D%3D"
    ),
    FEC_ID_cmte = "C00700641",
    FEC_ID_cand = "H0NC09203"
  ),
  tibble(
    first_name = "Matthew",
    last_name = "Ridenhour",
    state_cd = "NC-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Matthew Ridenhour for Congress",
    campaign_website = "https://voteridenhour.com",
    actblue_link = NA,
    anedot_link = "nc09/donate",
    other_link = NA,
    FEC_ID_cmte = "C00698449",
    FEC_ID_cand = "H0NC09161"
  ),
  tibble(
    first_name = "Stevie Rivenbark",
    last_name = "Hull", # Goes by Rivenbark
    state_cd = "NC-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ste",
    campaign_website = "http://stevieforcongress.com",
    actblue_link = NA,
    anedot_link = "stevie-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00700526",
    FEC_ID_cand = "H0NC09211"
  ),
  tibble(
    first_name = "Fern",
    last_name = "Shubert",
    state_cd = "NC-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shubert for US Congress",
    campaign_website = "https://www.shubertforuscongress.com",
    actblue_link = NA,
    anedot_link = "shubertforuscongress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00700500",
    FEC_ID_cand = "H0NC09195"
  ),
  tibble(
    first_name = "Allen", # Loran Allen, goes by Allen
    last_name = "Smith",
    state_cd = "NC-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Allen Smith for Congress",
    campaign_website = "https://www.allenforcongress.com/home",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.allenforcongress.com/donate",
    FEC_ID_cmte = "C00698001",
    FEC_ID_cand = "H0NC09153"
  ),
  tibble(
    first_name = "Christian", # Fernando Christian, goes by Christian
    last_name = "Cano",
    state_cd = "NC-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cano For Congress",
    campaign_website = "https://cano4nc.com",
    actblue_link = "cano-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00590307",
    FEC_ID_cand = "H6NC09184"
  ),
  tibble(
    first_name = "Raymond",
    last_name = "Gerber",
    state_cd = "NC-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gerber for Congress",
    campaign_website = "https://raygerber.com",
    actblue_link = NA,
    anedot_link = "gerber-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00720300",
    FEC_ID_cand = "H0NC09260"
  ),
  # NC-10 (last updated Oct 17, 2020) ------------------------------------------
  # McHenry vs. Collias vs. Wilson
  tibble(
    first_name = "Patrick",
    last_name = "McHenry",
    state_cd = "NC-10",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://mchenry.house.gov",
    campaign_name = "McHenry for Congress",
    campaign_website = "https://mchenryforcongress.com",
    # Change documented to WinRed on Oct 27, 2019
    # At least Oct 17 and beyond
    # 25-50-250-500-1000-2800-5600 to 100-250-500-1000-2800-5600
    # Actually, didn't switch the link until Nov 16, 2019 by mistake
    actblue_link = NA,
    # anedot_link = "mchenry-for-congress/donate",
    anedot_link = NA,
    other_link = "https://secure.winred.com/mchenry-for-congress/donate",
    FEC_ID_cmte = "C00393629",
    FEC_ID_cand = "H4NC10047"
  ),
  # Uncontested primary winner
  tibble(
    first_name = "David",
    last_name = "Parker",
    state_cd = "NC-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vote Parker",
    campaign_website = "http://www.voteparker.com",
    actblue_link = "vote-parker-1#",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00735092",
    FEC_ID_cand = "H0NC10193"
  ),
  # Withdrew
  tibble(
    first_name = "Gina",
    last_name = "Collias",
    state_cd = "NC-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gina Collias for Congress",
    campaign_website = "https://www.ginacolliasforcongress.com",
    actblue_link = "gina-collias-for-congress-campaign-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00668483",
    FEC_ID_cand = "H8NC10113"
  ),
  tibble(
    first_name = "David Wilson",
    last_name = "Brown",
    state_cd = "NC-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Wilson Brown for Congress",
    campaign_website = "https://dwb4congress.com",
    actblue_link = "dwb4congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00649806",
    FEC_ID_cand = "H8NC10097"
  ),
  # NC-11 (last updated Jul 12, 2021) ------------------------------------------
  # Meadows announced retirement on Dec 19, 2020; open seat
  # Driscoll vs. Bennett
  # vs. Davis vs. Woodsmall vs. Collias vs. OShea vs. Archerd
  # vs. Price vs. Wiley vs. Wiley (second is the same person; entered wrongly)
  tibble(
    first_name = "Madison", # David Madison, goes by Madison
    last_name = "Cawthorn",
    state_cd = "NC-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cawthorn for NC",
    campaign_website = "https://madisoncawthorn.com",
    actblue_link = NA,
    # From WinRed to Anedot documented Oct 17, 2020
    # from 25-50-100-250-500-1000-2800
    #   to 25-50-100-250-500-1000-2500
    anedot_link = "cawthorn-for-nc/cawthorn-for-nc-caca66a3eb4a5f39bf245",
    other_link = c(
      "https://secure.winred.com/cawthornfornc/fighter",
      "https://secure.winred.com/cawthornfornc/donate"
    ),
    FEC_ID_cmte = "C00732958",
    FEC_ID_cand = "H0NC11233"
  ),
  tibble(
    first_name = "Moe", # Morris D., goes by Moe
    last_name = "Davis",
    state_cd = "NC-11",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Moe Davis for Congress",
    campaign_website = "https://moedavisforcongress.com",
    actblue_link = c(
      # Change within ActBlue documented Oct 17, 2020
      # from 11-20.20-30-50-100-250-500
      #   to 10-25      -50-    250
      "moedavisweb",
      "moedavis4nc11"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00727800",
    FEC_ID_cand = "H0NC11175"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Meadows",
    state_cd = "NC-11",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://meadows.house.gov",
    campaign_name = "Mark Meadows for Congress",
    campaign_website = "https://meadowsforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://secure.meadowsforcongress.com/donation1/?",
      "InitiativeKey=HXYRANGJTRTF"
    ),
    FEC_ID_cmte = "C00503094",
    FEC_ID_cand = "H2NC11080"
  ),
  tibble(
    first_name = "Chuck", # Charles Wallace, goes by Chuck
    last_name = "Archerd",
    state_cd = "NC-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Archerd for Congress",
    campaign_website = "https://archerdforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://politics.raisethemoney.com/ArcherdForCongress",
    FEC_ID_cmte = "C00732347",
    FEC_ID_cand = "H0NC11209"
  ),
  tibble(
    first_name = "Dan",
    last_name = "Driscoll",
    state_cd = "NC-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Driscoll for Congress",
    campaign_website = "https://dandriscoll.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/dandriscoll/donate",
    FEC_ID_cmte = "C00732107",
    FEC_ID_cand = "H0NC11258"
  ),
  tibble(
    first_name = "Vance",
    last_name = "Patterson",
    state_cd = "NC-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vance Patterson for Congress",
    campaign_website = "https://www.vancenc.com",
    # Asks for check donations only, so no link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00736868",
    FEC_ID_cand = "H0NC10169"
  ),
  tibble(
    first_name = "Lynda",
    last_name = "Bennett",
    state_cd = "NC-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lynda Bennett for Congress",
    campaign_website = "https://lyndaforcongress.com",
    actblue_link = NA,
    anedot_link =
      "lynda-bennett-for-congress-4e3373a5-aa8b-4301-9ec7-8fb61dab9172/donate",
    other_link = NA,
    FEC_ID_cmte = "C00732099",
    FEC_ID_cand = "H0NC11191"
  ),
  # Dropped out and endorsed Cawthorn on Jan 28, 2020
  tibble(
    first_name = "Matthew",
    last_name = "Burril",
    state_cd = "NC-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = NA,
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00732685",
    FEC_ID_cand = "H0NC11217"
  ),
  tibble(
    first_name = "Joey",
    last_name = "Osborne",
    state_cd = "NC-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = NA,
    # Website is https://osborneforcongress.com but is down
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00735563",
    FEC_ID_cand = "H0NC11241"
  ),
  tibble(
    first_name = "Jim",
    last_name = "Davis",
    state_cd = "NC-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jim Davis for Congress",
    campaign_website = "https://jimdavisforcongress.com",
    actblue_link = NA,
    anedot_link = "jim-davis-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00733105",
    FEC_ID_cand = "H0NC11183"
  ),
  tibble(
    first_name = "Steve",
    last_name = "Woodsmall",
    state_cd = "NC-11",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Woodsmall for Congress",
    campaign_website = "https://woodsmall4nc.com",
    actblue_link = c(
      # Change in ActBlue link detected Feb 16, 2020
      "woodsmallwebsite",
      "woodsmall-for-congress-1",
      "stevemonthly"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00668558",
    FEC_ID_cand = "H8NC11129"
  ),
  tibble(
    first_name = "Gina",
    last_name = "Collias",
    state_cd = "NC-11",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gina Collias for Congress",
    campaign_website = "https://www.ginacolliasforcongress.com",
    actblue_link = "gina-collias-for-congress-campaign-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00668483",
    FEC_ID_cand = "H8NC10113"
  ),
  tibble(
    first_name = "Wayne", # Joseph Wayne, goes by Wayne
    last_name = "King",
    state_cd = "NC-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wayne King for Congress",
    campaign_website = "https://waynekingforcongress.com",
    actblue_link = NA,
    anedot_link = "waynekingforcongress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00732941",
    FEC_ID_cand = "H0NC11225"
  ),
  tibble(
    first_name = "Michael",
    last_name = "O'Shea",
    state_cd = "NC-11",
    party = "",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "O'Shea for Congress",
    campaign_website = "https://osheaforcongress.com",
    actblue_link = "osheaforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00724518",
    FEC_ID_cand = "H0NC11159"
  ),
  tibble(
    first_name = "Phillip",
    last_name = "Price",
    state_cd = "NC-11",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Phillip Price for Congress",
    campaign_website = "https://price4wnc.org",
    # Not the main link; main link is Stripe without any prompts
    actblue_link = "phillip-price-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00649277",
    FEC_ID_cand = "H8NC11103"
  ),
  # Cannot find Albert Wiley Jr.
  # NC-12 (last updated Oct 17, 2020) ------------------------------------------
  # Adams vs. Brewster vs. Davis
  # Roderick Davis withdrew. Brewster withdrew. No general Rep candidate
  tibble(
    first_name = "Alma",
    last_name = "Adams",
    state_cd = "NC-12",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://adams.house.gov",
    campaign_name = "Alma Adams for Congress",
    campaign_website = "http://almaadamsforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/2134836566127151104",
    FEC_ID_cmte = "C00546358",
    FEC_ID_cand = "H4NC12100"
  ),
  tibble(
    first_name = "Bill",
    last_name = "Brewster",
    state_cd = "NC-12",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bill Brewster for Congress",
    campaign_website = "http://brewsterforcongress.com",
    actblue_link = NA,
    anedot_link = "brewster/donate",
    other_link = NA,
    FEC_ID_cmte = "C00699363",
    FEC_ID_cand = "H0NC12124"
  ),
  tibble(
    first_name = "Tamara",
    last_name = "Zwinak",
    state_cd = "NC-12",
    party = "Grn",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tamara Zwinak for Congress",
    campaign_website = "https://ushouserace2020.blogspot.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0NC11274"
  ),
  tibble(
    first_name = "Tracey",
    last_name = "DeBruhl",
    state_cd = "NC-12",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "DeBruhl for Congress",
    campaign_website = "https://www.facebook.com/DeBruhl-For-Congress-105350877641380",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0NC11282"
  ),
  # NC-13 (last updated Oct 17, 2020) ------------------------------------------
  # Ted Budd was uncontested (no candidates registered as of Oct 8, 2019)
  # Now Budd vs. Huffman
  tibble(
    first_name = "Ted",
    last_name = "Budd",
    state_cd = "NC-13",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://budd.house.gov",
    campaign_name = "Ted Budd for Congress",
    campaign_website = "https://tedbudd.com/",
    actblue_link = NA,
    # anedot_link = "ted-budd-for-congress/general-fund-cca917c220e9ca1798a8b",
    anedot_link = NA,
    # Change documented to WinRed on Oct 17, 2020
    # At least from Aug 7, 2020
    other_link = "https://secure.winred.com/tedbudd/standwithted",
    FEC_ID_cmte = "C00614776",
    FEC_ID_cand = "H6NC13129"
  ),
  tibble(
    first_name = "Scott",
    last_name = "Huffman",
    state_cd = "NC-13",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Huffman for NC",
    campaign_website = "https://www.scotthuffman.com",
    actblue_link = "huffmanfornc",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00696492", "C00663229"),
    FEC_ID_cand = "H8NC08109"
  ),
  # ND: North Dakota (at-large district; last updated Oct 7, 2020) =============
  # Kelly Armstrong was uncontested (no candidates registered as of Oct 5, 2019)
  # Now Armstrong vs. Raknerud
  tibble(
    first_name = "Kelly",
    last_name = "Armstrong",
    state_cd = "ND-0",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://armstrong.house.gov",
    campaign_name = "Kelly Armstrong for Congress",
    campaign_website = "https://www.armstrongnd.com/",
    actblue_link = NA,
    anedot_link = "armstrongnd/armstrongnd",
    other_link = NA,
    FEC_ID_cmte = "C00670547",
    FEC_ID_cand = "H8ND00096"
  ),
  tibble(
    first_name = "Zachary",
    last_name = "Raknerud",
    state_cd = "ND-0",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Zach for North Dakota",
    campaign_website = "https://zachfornd.com",
    actblue_link = "zachfornd1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00736256",
    FEC_ID_cand = "H0ND01042"
  ),
  # Ohio (last updated Oct 17, 2020) ===========================================
  # OH-01 (last updated Oct 17, 2020) ------------------------------------------
  # Chabot vs. Schroder vs. Weaver vs. Foster vs. Kahn vs. Pratschler
  tibble(
    first_name = "Steve",
    last_name = "Chabot",
    state_cd = "OH-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://chabot.house.gov",
    campaign_name = "Steve Chabot for Congress",
    campaign_website = "https://stevechabot.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from Dec 20, 2019
    # from    25-50-75-100-250-500-1000-2800-5600
    #   to 10-25-50-100-250-1000-2800
    other_link = paste0(
      # "https://secure.campaignsolutions.com/stevechabot/donation1/?",
      # "InitiativeKey=LDIJUAD2FVUN"
      "https://secure.winred.com/steve-chabot/donate"
    ),
    FEC_ID_cmte = "C0030183",
    FEC_ID_cand = "H8OH01043"
  ),
  tibble(
    first_name = "Kate",
    last_name = "Schroder",
    state_cd = "OH-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kate for Congress",
    campaign_website = "https://kateforcongress.com/",
    actblue_link = c(
      # Change between ActBlue
      "k4c-site" # ,
      # "kateforcongress1"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711630",
    FEC_ID_cand = "H0OH01099"
  ),
  tibble(
    first_name = "Nikki",
    last_name = "Foster",
    state_cd = "OH-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nikki Foster for Ohio",
    campaign_website = "https://nikkifosteroh.com/",
    actblue_link = "nkf_monthly_intro",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710459",
    FEC_ID_cand = "H0OH01081"
  ),
  tibble(
    first_name = "Kevin",
    last_name = "Kahn",
    state_cd = "OH-01",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kahn4Congress",
    campaign_website = "https://kahn4congress.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = "https://kahn4congress.com/donate/",
    FEC_ID_cmte = "C00736942",
    FEC_ID_cand = "H0OH01115"
  ),
  # Pratschler withdrew
  # Dropped out
  tibble(
    first_name = "Laura Ann",
    last_name = "Weaver",
    state_cd = "OH-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Win with Weaver",
    campaign_website = "http://www.winwithweaver.com/",
    actblue_link = "winwithweaver",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00635342",
    FEC_ID_cand = "H8OH01084"
  ),
  # OH-02 (last updated Oct 17, 2020) ------------------------------------------
  # Brad Wenstrup was uncontested (no candidates registered as of Oct 8, 2019)
  # Now Wenstrup vs. Castle
  tibble(
    first_name = "Brad",
    last_name = "Wenstrup",
    state_cd = "OH-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://wenstrup.house.gov",
    campaign_name = "Wenstrup for Congress",
    campaign_website = "https://usabrad.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/usabrad/donate",
    FEC_ID_cmte = "C00497818",
    FEC_ID_cand = "H2OH02085"
  ),
  tibble(
    first_name = "Jaime",
    last_name = "Castle",
    state_cd = "OH-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jaime Castle for US Congress",
    campaign_website = "https://castleforuscongress.com",
    actblue_link = "friends-of-jaime-castle-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00728584",
    FEC_ID_cand = "H0OH02139"
  ),
  # OH-03 (last updated Oct 17, 2020) ------------------------------------------
  # Beatty vs. Harper vs. Moss vs. Littler vs. Richarson
  tibble(
    first_name = "Joyce",
    last_name = "Beatty",
    state_cd = "OH-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://beatty.house.gov",
    campaign_name = "Joyce Beatty for Congress",
    campaign_website = "https://beattyforcongress.com/",
    actblue_link = "joyce-beatty-3",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00507368",
    FEC_ID_cand = "H2OH03125"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Richardson",
    state_cd = "OH-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Richardson For Ohio's 3rd Congressional District",
    campaign_website = "https://www.richardsonforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      c(
        "https://secure.winred.com/mark-richardson-committee-to-elect/oh3" # ,
        # Goes to the same link
        # "https://secure.winred.com/mark-richardson-committee-to-elect/donate"
      ),
    FEC_ID_cmte = "C00736462",
    FEC_ID_cand = "H0OH03111"
  ),
  tibble(
    first_name = "Morgan",
    last_name = "Harper",
    state_cd = "OH-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Morgan Harper for Congress",
    campaign_website = "https://www.morganharper.org/",
    actblue_link = "morganharper",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710566",
    FEC_ID_cand = "H0OH03103"
  ),
  # Cannot find Nicholas Moss
  tibble(
    first_name = "Andrew",
    last_name = "Littler",
    state_cd = "OH-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Andrew Littler for Ohio",
    campaign_website = "http://www.andrew4ohio.org/",
    actblue_link = NA,
    anedot_link = NA,
    # 5-10-20-50 difficult to scrape, will deal later
    other_link = "https://andrew-littler-for-ohio.square.site/",
    FEC_ID_cmte = "C00706143",
    FEC_ID_cand = "H0OH03095"
  ),
  # OH-04 (last updated Oct 17, 2020) ------------------------------------------
  # Jordan vs. Freshour vs. Sites vs. Larsen vs. Gibbs
  tibble(
    first_name = "Jim",
    last_name = "Jordan",
    state_cd = "OH-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://jordan.house.gov",
    campaign_name = "Jim Jordan for Congress",
    campaign_website = "http://jimjordanforcongress.com/",
    # Change documented to Anedot(!) on Nov 16, 2019
    # No suggested amounts. Could be a good baseline (esp. with impeachment)
    # Originally was 10-25-50-100-250-500-1000-2800
    # Still Anedot, still no prompts/baseline
    actblue_link = NA,
    # Although Rightus reported donation website to be
    # https://secure.anedot.com/jim-jordan-for-congress/c5dc5e810b53c888dc1e2
    anedot_link = "jim-jordan-for-congress/donate",
    # other_link = NA,
    other_link = "https://secure.piryx.com/donate/2UhYCZvN/Jim-Jordan",
    FEC_ID_cmte = "C00416594",
    FEC_ID_cand = "H6OH04082"
  ),
  tibble(
    first_name = "Shannon",
    last_name = "Freshour",
    state_cd = "OH-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shannon Freshour for Congress",
    campaign_website = "https://www.shannon4ohio.com/",
    actblue_link = c(
      "shannon-4-ohio-committee-1" # ,
      # "adopt-oh-cd-04"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00712760",
    FEC_ID_cand = "H0OH04093"
  ),
  # No donation links, no FEC
  tibble(
    first_name = "Steve",
    last_name = "Perkins",
    state_cd = "OH-04",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://perkins4ohio.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "Jeffrey",
    last_name = "Sites",
    state_cd = "OH-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Jeffrey A. Sites for Congress",
    campaign_website = "https://sitesforcongress.com",
    actblue_link = c(
      "defeatjjordan",
      "jeffrey-a--sites-for-congress-1"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00712224",
    FEC_ID_cand = "H0OH04085"
  ),
  tibble(
    first_name = "Mike",
    last_name = "Larsen",
    state_cd = "OH-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Larsen for U.S. Congress",
    campaign_website = "https://www.ohiomike2020.com",
    actblue_link = "ohmike",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00721928",
    FEC_ID_cand = "H0OH04101"
  ),
  tibble(
    first_name = "Chris",
    last_name = "Gibbs",
    state_cd = "OH-04",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chris Gibbs for Congress",
    campaign_website = "http://www.chrisgibbsforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-1000-2500
    other_link = "https://donate.chrisgibbsohio.com/donate-launch",
    FEC_ID_cmte = "C00735282",
    FEC_ID_cand = "H0OH04119"
  ),
  # OH-05 (last updated Oct 17, 2020) ------------------------------------------
  # Latta vs. Rubando vs. Carrigan vs. Redinger
  tibble(
    first_name = "Bob",
    last_name = "Latta",
    state_cd = "OH-05",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://latta.house.gov",
    campaign_name = "Latta for Congress",
    campaign_website = "https://lattaforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Still piryx
    other_link = "https://secure.piryx.com/donate/Gt0eTiC0/Latta-for-Congress/",
    FEC_ID_cmte = "C00438697",
    FEC_ID_cand = "H8OH05036"
  ),
  tibble(
    first_name = "Nick",
    last_name = "Rubando",
    state_cd = "OH-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rubando for Congress",
    campaign_website = "https://nickrubando.com",
    actblue_link = "rubandoforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00707943",
    FEC_ID_cand = "H0OH05074"
  ),
  tibble(
    first_name = "M. Xavier", # Michael Xavier, goes by M. Xavier
    last_name = "Carrigan",
    state_cd = "OH-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Xavier Carrigan for US Congress",
    campaign_website = "https://www.mxc2020.com",
    actblue_link = "donate-mxc-2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00717892",
    FEC_ID_cand = "H0OH05082"
  ),
  tibble(
    first_name = "Gene",
    last_name = "Redinger",
    state_cd = "OH-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Redinger For Congress 2020",
    campaign_website = "https://redingerforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Empty
    other_link = "https://redingerforcongress.com/donate/",
    FEC_ID_cmte = "C00701300",
    FEC_ID_cand = "H0OH05090"
  ),
  # OH-06 (last updated Oct 17, 2020) ------------------------------------------
  # Johnson vs. Morgan
  # Roberts seem to be rollover from 2018 ---> nope.
  tibble(
    first_name = "Bill",
    last_name = "Johnson",
    state_cd = "OH-06",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://billjohnson.house.gov",
    campaign_name = "Johnson for Congress",
    campaign_website = "http://billjohnsonleads.com",
    actblue_link = NA,
    anedot_link = "johnson-for-congress/johnsonforcongress",
    other_link = NA,
    FEC_ID_cmte = "C00476820",
    FEC_ID_cand = "H0OH06189"
  ),
  tibble(
    first_name = "Shawna",
    last_name = "Roberts",
    state_cd = "OH-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shawna Roberts for Congress",
    campaign_website = "https://twitter.com/RobertsOhioD6",
    actblue_link = "shawnaroberts",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00670232",
    FEC_ID_cand = "H8OH06133"
  ),
  tibble(
    first_name = "Kenneth",
    last_name = "Morgan",
    state_cd = "OH-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kenneth Morgan for Congress",
    campaign_website = "https://www.kennethmorganserves.com",
    actblue_link = NA,
    anedot_link = NA,
    # 5-20-50-100
    other_link = "https://www.kennethmorganserves.com/donations/donation-form/",
    FEC_ID_cmte = "C00704056",
    FEC_ID_cand = "H0OH06221"
  ),
  # OH-07 (last updated Oct 17, 2020) ------------------------------------------
  # Bob Gibbs was uncontested (no candidates registered as of Oct 8, 2019)
  # Now Gibbs vs. Lape vs. Potter
  tibble(
    first_name = "Bob",
    last_name = "Gibbs",
    state_cd = "OH-07",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://gibbs.house.gov",
    campaign_name = "Bob Gibbs for Congress",
    campaign_website = "https://bobgibbsforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://politics.raisethemoney.com/gibbsforcongress/",
    FEC_ID_cmte = "C00466516",
    FEC_ID_cand = "H0OH18077"
  ),
  tibble(
    first_name = "Quentin",
    last_name = "Potter",
    state_cd = "OH-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Quentin Potter for Congress",
    campaign_website = "https://www.quentinpotterforcongress.com",
    actblue_link = "potter-for-oh-7th-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00737288",
    FEC_ID_cand = "H0OH07153"
  ),
  tibble(
    first_name = "Brandon",
    last_name = "Lape",
    state_cd = "OH-07",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lape for Ohio",
    campaign_website = "https://lapeforohio.com",
    # Cannot find donation page
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00734442",
    FEC_ID_cand = "H0OH07146"
  ),
  # OH-08 (last updated Oct 17, 2020) ------------------------------------------
  # Warren Davidson was uncontested (no candidates registered as of Oct 8, '19)
  # Now Davidson vs. Enoch vs. Guyette
  tibble(
    first_name = "Warren",
    last_name = "Davidson",
    state_cd = "OH-08",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://davidson.house.gov",
    campaign_name = "Davidson For Congress",
    campaign_website = "https://www.davidsonforcongress.com/",
    actblue_link = NA,
    # No suggested amounts. Could be a good baseline.
    anedot_link = "davidson-for-congress/general-fund-c190511c3c1ea845988ab",
    other_link = NA,
    FEC_ID_cmte = "C00600718",
    FEC_ID_cand = "H6OH08315"
  ),
  tibble(
    first_name = "Vanessa",
    last_name = "Enoch",
    state_cd = "OH-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Enoch for Congress",
    campaign_website = "https://enochforcongress.com",
    actblue_link = "electenochcampaign2020",
    anedot_link = NA,
    # ActBlue to NGP VAN?! Also, different amounts
    # At least from Aug 17, 2020: 3-5-10-20.20-50-100-250-500-1000-2800
    other_link = "https://secure.ngpvan.com/v1/Forms/Flr211G7LkmjVAKpFhxyAg2",
    FEC_ID_cmte = "C00667279",
    FEC_ID_cand = "H8OH08097"
  ),
  tibble(
    first_name = "Matthew",
    last_name = "Guyette",
    state_cd = "OH-08",
    party = "",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Guyette for Congress",
    campaign_website = "https://guyetteforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Prompt for 20 USD
    other_link = paste0(
      "https://www.paypal.com/webapps/hermes?",
      "token=5H811293AS002325B&useraction=commit&rm=0&",
      "mfid=1584835537586_e69d32d650928"
    ),
    FEC_ID_cmte = "C00726299",
    FEC_ID_cand = "H4OH08088"
  ),
  # OH-09 (last updated Oct 17, 2020) ------------------------------------------
  # Marcy Kaptur was uncontested (no candidates registered as of Oct 8, 2019)
  # Now Kaptur vs. Weber vs. Crist vs. Connors
  tibble(
    first_name = "Marcy",
    last_name = "Kaptur",
    state_cd = "OH-09",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://kaptur.house.gov/about",
    campaign_name = "Marcy Kaptur for Congress",
    campaign_website = "https://marcykaptur.com/",
    actblue_link = "marcykaptur",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00154625",
    FEC_ID_cand = "H2OH09031"
  ),
  tibble(
    first_name = "Rob", # Robert Marion, goes by Rob
    last_name = "Weber",
    state_cd = "OH-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rob Weber for Congress",
    campaign_website = "https://robweberforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/rob-weber-for-congress",
    FEC_ID_cmte = "C00734327",
    FEC_ID_cand = "H0OH09167"
  ),
  # Terry Crist withdrew
  tibble(
    first_name = "Tim", # Timothy, goes by Tim
    last_name = "Connors",
    state_cd = "OH-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tim Connors For Congress",
    campaign_website = "https://www.timconnorsforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 5-10-25-50-100-150-250-500-1000-1500-2000-2800
    other_link = "https://www.timconnorsforcongress.com/contribute",
    FEC_ID_cmte = "C00733931",
    FEC_ID_cand = "H0OH09159"
  ),
  # OH-10 (last updated Oct 17, 2020) ------------------------------------------
  # Mike Turner was uncontested (no candidates registered as of Oct 8, 2019)
  # Now Turner vs. Tims vs. Moyer vs. Flanders
  tibble(
    first_name = "Mike",
    last_name = "Turner",
    state_cd = "OH-10",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://turner.house.gov",
    campaign_name = "Citizens for Turner",
    campaign_website = "http://miketurner.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Leads to same link as before
    other_link = "https://secure.winred.com/turner/oh10",
    FEC_ID_cmte = "C00373001",
    FEC_ID_cand = "H2OH03067"
  ),
  tibble(
    first_name = "Desiree",
    last_name = "Tims",
    state_cd = "OH-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Desiree Tims for Congress",
    campaign_website = "https://timsforcongress.com",
    actblue_link = "friends-of-desiree-tims",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00713743",
    FEC_ID_cand = "H0OH10116"
  ),
  tibble(
    first_name = "Eric",
    last_name = "Moyer",
    state_cd = "OH-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Moyer 2020",
    campaign_website = "https://www.moyer2020.org",
    actblue_link = "moyer",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00729137",
    FEC_ID_cand = "H0OH10132"
  ),
  tibble(
    first_name = "Kathi",
    last_name = "Flanders",
    state_cd = "OH-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kathi Flanders For Congress",
    campaign_website = "http://kathiflanders.com",
    actblue_link = NA,
    anedot_link = NA,
    # Default 5
    other_link = "http://kathiflanders.com/donate/#Donate",
    FEC_ID_cmte = "C00731240",
    FEC_ID_cand = "H0OH10124"
  ),
  # OH-11 (last updated Oct 17, 2020) ------------------------------------------
  # Fudge vs. Gore vs. Schultz vs. Shabazz vs. Hood vs. Taylor vs. Bell
  tibble(
    first_name = "Marcia",
    last_name = "Fudge",
    state_cd = "OH-11",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://fudge.house.gov",
    campaign_name = "Marcia L. Fudge For Congress",
    campaign_website = "http://marcialfudge.com",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://secure.ngpvan.com/UzuWY0wU8EC3YlSdP5LoXg2",
    other_link = "https://secure.ngpvan.com/v1/Forms/UzuWY0wU8EC3YlSdP5LoXg2",
    FEC_ID_cmte = "C00454694",
    FEC_ID_cand = "H8OH11141"
  ),
  tibble(
    first_name = "Laverne",
    last_name = "Gore",
    state_cd = "OH-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Laverne Gore for Congress",
    campaign_website = "https://lavernegore4congress.org",
    actblue_link = NA,
    # Change documented to Anedot on Oct 17, 2020
    # At least from Aug 8, 2020
    anedot_link = "laverne-gore-for-congress/donation-target",
    # other_link = paste0(
    #   "https://www.paypal.com/donate/?",
    #   "token=AUEMRQs6xMI8ahF3egoICtiMtLg1zkRh9dOOTAQ39AvQs1M3tXJft4Ze0",
    #   "Ra0wcwfTka0UG&country.x=US&locale.x=US"
    # ),
    FEC_ID_cmte = "C00724609",
    FEC_ID_cand = "H0OH11098"
  ),
  tibble(
    first_name = "Jonah",
    last_name = "Schulz",
    state_cd = "OH-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Schulz for Congress",
    campaign_website = "https://www.schulzforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/embed/schulz-for-congress",
    FEC_ID_cmte = "C00725283",
    FEC_ID_cand = "H0OH11106"
  ),
  tibble(
    first_name = "Tariq",
    last_name = "Shabazz",
    state_cd = "OH-11",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tariq Shabazz for US Representative2020",
    campaign_website = "https://www.shabazzforusrep2020.com",
    actblue_link = "shabazzforusrep2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00728337",
    FEC_ID_cand = "H0OH11122"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Hood",
    state_cd = "OH-11",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "New Spirit 2020",
    campaign_website = "https://www.mikehood4congress.org",
    # No donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00727818",
    FEC_ID_cand = "H0OH11114"
  ),
  tibble(
    first_name = "Shalira",
    last_name = "Taylor",
    state_cd = "OH-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shalira for All",
    # Not http://www.st4oh.info: this is 2015
    campaign_website = "https://www.taylor4all.com",
    actblue_link = NA,
    anedot_link = NA,
    # 25-250-1000-2800
    other_link = "https://donate.fundhero.io/taylor-for-all",
    FEC_ID_cmte = "C00714188",
    FEC_ID_cand = "H0OH11080"
  ),
  tibble(
    first_name = "James Jerome",
    last_name = "Bell",
    state_cd = "OH-11",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "James Jerome Bell for Congress Ohio 11th District",
    campaign_website = "https://jamesjeromebell2020.com",
    actblue_link = NA,
    anedot_link = NA,
    # No amount prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?token=9xjWwsnXGYjqWEpvCa8cilI25lb",
      "S41ERe4B57vxwy25GTgqNS_5cH_kf1Y4oBpAV7hjQl0&country.x=US&locale.x="
    ),
    FEC_ID_cmte = c("C00704619", "C00647305"),
    FEC_ID_cand = "H8OH11281"
  ),
  # OH-12 (last updated Jul 13, 2021) -----------------------------------------
  # Balderson vs. Shearer vs. Bell vs. Weislogel vs. Mierzejewski
  tibble(
    first_name = "Troy", # William Troy, goes by Troy
    last_name = "Balderson",
    state_cd = "OH-12",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://balderson.house.gov",
    campaign_name = "Troy Balderson for Congress",
    campaign_website = "https://www.baldersonforcongress.com",
    actblue_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from Jan 22, 2020
    # from 10-25-50-100-250-500-1000-2000-2800
    #      to 25-50-100-250-500-1000-2000-2800
    anedot_link = NA,
    # anedot_link = "troy-balderson/donate",
    other_link =
      "https://secure.winred.com/balderson-for-congress/troybalderson",
    FEC_ID_cmte = "C00662650",
    FEC_ID_cand = "H8OH12180"
  ),
  tibble(
    first_name = "Alaina",
    last_name = "Shearer",
    state_cd = "OH-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Alaina Shearer for Congress",
    campaign_website = "https://www.alaina2020.com",
    actblue_link = "alaina-shearer-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00724047",
    FEC_ID_cand = "H0OH12179"
  ),
  # Benjamin Weislogel withdrew
  # Samuel Mierzejewski withdrew
  tibble(
    first_name = "Jenny",
    last_name = "Bell",
    state_cd = "OH-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jenny Bell 2020",
    campaign_website = "https://jennybell2020.us",
    actblue_link = "jennybell2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00717405",
    FEC_ID_cand = "H0OH12161"
  ),
  tibble(
    first_name = "John",
    last_name = "Stewart",
    state_cd = "OH-12",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elect John Stewart",
    campaign_website = "https://www.electjohnstewart.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00749135",
    FEC_ID_cand = "H0OH12229"
  ),
  # OH-13 (last updated Jul 13, 2021) ------------------------------------------
  # Tim Ryan was running for president
  # Ryan vs. Lyras vs. Hagan vs. Santos vs. Truex vs. Morckel
  tibble(
    first_name = "Tim",
    last_name = "Ryan",
    state_cd = "OH-13",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://timryan.house.gov",
    campaign_name = "Tim Ryan for Congress",
    campaign_website = "https://timryanforcongress.com/",
    actblue_link = c(
      "tim-ryan-for-congress" # ,
      # "tim-ryan-2", # Email: 13-25-50-100
      # "tr2002eom"   # Email: 13-25-50-100
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00373464",
    FEC_ID_cand = "H2OH17109"
  ),
  tibble(
    first_name = "Christina",
    last_name = "Hagan",
    state_cd = "OH-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hagan for Congress",
    campaign_website = "https://hagan2020.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/hagan-for-congress/donate",
    FEC_ID_cmte = "C00638411",
    FEC_ID_cand = "H8OH16108"
  ),
  tibble(
    first_name = "Lou", # Louis, goes by Lou
    last_name = "Lyras",
    state_cd = "OH-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lyras for Congress",
    campaign_website = "http://lyrasforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/embed/lou-lyras-for-congress-2020",
    FEC_ID_cmte = "C00682948",
    FEC_ID_cand = "H8OH13154"
  ),
  tibble(
    first_name = "Robert",
    last_name = "Santos",
    state_cd = "OH-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Robert Santos for Ohio",
    campaign_website = "https://www.robertsantos4ohio.com",
    actblue_link = NA,
    anedot_link = NA,
    # 20-50-100
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "token=BJpwepae50NNdcZ7fhXt9Rq0lJT0-9SBQhm5i2GGuEVOfSQex6i2",
      "-3HTFIlwM96eQF8MtG&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00724872",
    FEC_ID_cand = "H0OH13201"
  ),
  # Mostly empty website
  tibble(
    first_name = "Donald",
    last_name = "Truex",
    state_cd = "OH-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dontruex4ohio",
    campaign_website = "https://dontruex4ohio.wixsite.com/website",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00739318", "C00738807", "C00738799"),
    FEC_ID_cand = "H0OH13219"
  ),
  tibble(
    first_name = "Richard",
    last_name = "Morckel",
    state_cd = "OH-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Morckel 4 Congress",
    campaign_website = "https://morckel4congress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 5-10-20-50-100-200
    other_link = paste0(
      "https://fundrazr.com/campaigns/Morckel4Congress/pay?",
      "ref=sh_a8B5H3_ab_4Fomo9qQs1l4Fomo9qQs1l"
    ),
    FEC_ID_cmte = "C00714808",
    FEC_ID_cand = "H6OH13240"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Fricke",
    state_cd = "OH-13",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Fricke for Ohio",
    campaign_website = "https://www.facebook.com/mikefricke2020",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0OH13235"
  ),
  # OH-14 (last updated Oct 17, 2020) ------------------------------------------
  # David Joyce was uncontested (no candidates registered as of Oct 8, 2019)
  # Now Joyce vs. Mueri
  tibble(
    first_name = "David",
    last_name = "Joyce",
    state_cd = "OH-14",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://joyce.house.gov",
    campaign_name = "Dave Joyce For Congress",
    campaign_website = "https://joyceforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://joyceforcongress.revv.co/donate-today",
    # Change documented to WinRed (it was Revv anyway) Oct 17, 2020
    # At least from Aug 12, 2020
    other_link = "https://secure.winred.com/davejoyce/donate",
    FEC_ID_cmte = "C00527457",
    FEC_ID_cand = "H2OH14064"
  ),
  tibble(
    first_name = "Hillary O'Connor",
    last_name = "Mueri",
    state_cd = "OH-14",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hillary O'Connor Mueri for Ohio",
    campaign_website = "https://www.hillaryoconnormueri.com",
    actblue_link = "hom_website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00722637",
    FEC_ID_cand = "H0OH14084"
  ),
  # OH-15 (last updated Oct 17, 2020) ------------------------------------------
  # Stivers vs. Newby vs. Kilgore vs. Hunt
  tibble(
    first_name = "Steve",
    last_name = "Stivers",
    state_cd = "OH-15",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://stivers.house.gov",
    campaign_name = "Steve Stivers for Congress",
    campaign_website = "https://stevestivers.com",
    actblue_link = NA,
    anedot_link = NA,
    # Revv account
    other_link = "https://secure.stevestivers.com/donate",
    FEC_ID_cmte = "C00441352",
    FEC_ID_cand = "H8OH15076"
  ),
  tibble(
    first_name = "Joel",
    last_name = "Newby",
    state_cd = "OH-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Newby for Congress",
    campaign_website = "https://www.joelnewby.com",
    actblue_link = "newby-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00725754",
    FEC_ID_cand = "H0OH15149"
  ),
  tibble(
    first_name = "Daniel",
    last_name = "Kilgore",
    state_cd = "OH-15",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Daniel Kilgore for Ohio",
    campaign_website = "https://danielkilgoreforohio.com",
    actblue_link = "daniel-kilgore-for-ohio-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00712554",
    FEC_ID_cand = "H0OH15131"
  ),
  tibble(
    first_name = "Shelby",
    last_name = "Hunt",
    state_cd = "OH-15",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hunt for Congress",
    campaign_website = "https://huntforcongress.us",
    actblue_link = NA,
    anedot_link = NA,
    # 5-10-25-50-100
    other_link = "https://huntforcongress.us/support-new-leadership/",
    FEC_ID_cmte = "C00733212",
    FEC_ID_cand = "H0OH15156"
  ),
  # OH-16 (last updated Oct 17, 2020) ------------------------------------------
  # Anthony Gonzalez was uncontested
  # (no candidates registered as of Oct 8, 2019)
  # Now Gonzalez vs. Godfrey vs. Karpus
  tibble(
    first_name = "Anthony",
    last_name = "Gonzalez",
    state_cd = "OH-16",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://anthonygonzalez.house.gov",
    campaign_name = "Anthony Gonzalez For Congress",
    campaign_website = "https://www.voteanthonygonzalez.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Revv account
    other_link = "https://secure.voteanthonygonzalez.com/donate",
    FEC_ID_cmte = "C00654079",
    FEC_ID_cand = "H8OH16124"
  ),
  tibble(
    first_name = "Aaron",
    last_name = "Godfrey",
    state_cd = "OH-16",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Godfrey for Congress",
    campaign_website = "https://www.godfreyforcongress.com",
    actblue_link = "godfrey2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00639567",
    FEC_ID_cand = "H8OH16090"
  ),
  tibble(
    first_name = "Ronald",
    last_name = "Karpus",
    state_cd = "OH-16",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Karpus for Congress",
    campaign_website = "https://www.karpusforcongress.com",
    actblue_link = "karpus",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00727792",
    FEC_ID_cand = "H0OH16121"
  ),
  # Oklahoma (last updated Oct 8, 2020) ========================================
  # OK-01 (last updated Oct 4, 2020) -------------------------------------------
  # Kevin Hern was uncontested (no candidates registered as of Sep 26, 2019)
  # Now Hern vs. Asamoa-Caesar
  tibble(
    first_name = "Kevin",
    last_name = "Hern",
    state_cd = "OK-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://hern.house.gov",
    campaign_name = "Hern for Congress",
    campaign_website = "https://www.hernforcongress.com/",
    actblue_link = NA,
    anedot_link = "hern-for-congress/general-fund-ca4988f2603af750b16cd",
    other_link = NA,
    FEC_ID_cmte = "C00636092",
    FEC_ID_cand = "H8OK01157"
  ),
  tibble(
    first_name = "Kojo",
    last_name = "Asamoa-Caesar",
    state_cd = "OK-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://kojoforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.ngpvan.com/v1/Forms/HgIVj2JL8k2aJ9sKTMOS2w2",
    FEC_ID_cmte = "C00724336",
    FEC_ID_cand = "H0OK01147"
  ),
  # OK-02 (last updated Oct 8, 2020) -------------------------------------------
  # Markwayne Mullin was uncontested as of Sep 17, 2019
  # Now Mullin vs. Lanier (Filed Sep 17, 2019)
  # Lanier does not have a webpage yet, prev. Collin County Judge candidate
  # https://ballotpedia.org/Danyell_Lanier
  tibble(
    first_name = "Markwayne",
    last_name = "Mullin",
    state_cd = "OK-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://mullin.house.gov",
    campaign_name = "Mullin for Congress",
    campaign_website = "https://www.mullinforcongress.com/",
    actblue_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from Nov 20, 2019
    # from 10-25-50-100-250-500-1000-2800
    #   to 10-25-50-100-250-500-1000-2800 (same amounts)
    # anedot_link = "mullin-for-congress/mullin-web-donations",
    other_link = c(
      # Change within WinRed, at least from Aug 16 (nothing archived after Apr)
      "https://secure.winred.com/markwaynemullin/splash",
      # On Jul 24, record of the second link:
      "https://secure.winred.com/markwaynemullin/trump",
      "https://secure.winred.com/MarkwayneMullin/donate"
    ),
    FEC_ID_cmte = "C00498345",
    FEC_ID_cand = "H2OK02083"
  ),
  tibble(
    first_name = "Danyell",
    last_name = "Lanier",
    state_cd = "OK-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Danyell for Congress",
    campaign_website = "https://www.danyellforcongress.com",
    actblue_link = "danyell-lanier-for-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00719831",
    FEC_ID_cand = "H0OK02210"
  ),
  tibble(
    first_name = "Joseph",
    last_name = "Silk",
    state_cd = "OK-02",
    party = "",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    # Not archived and lost, unfortunately
    other_link = "https://www.silkforcongress.com/donate",
    FEC_ID_cmte = "C00744037",
    FEC_ID_cand = "H0OK02228"
  ),
  # OK-03 (last updated Oct 8, 2020) -------------------------------------------
  tibble(
    first_name = "Frank",
    last_name = "Lucas",
    state_cd = "OK-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://lucas.house.gov",
    campaign_name = "",
    campaign_website = "https://frankdlucas.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00287912",
    FEC_ID_cand = "H4OK06056"
  ),
  tibble(
    first_name = "Zoe",
    last_name = "Midyett",
    state_cd = "OK-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Midyett for U.S. Congress",
    campaign_website = "https://www.midyett2020.com",
    actblue_link = "midyett-2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00749770",
    FEC_ID_cand = "H0OK03093"
  ),
  # OK-04 (last updated Jul 13, 2021) -------------------------------------------
  # Tom Cole was uncontested (no candidates registered as of Sep 26, 2019)
  # Now Cole vs. Brannon
  tibble(
    first_name = "Tom",
    last_name = "Cole",
    state_cd = "OK-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Tom Cole for Congress",
    campaign_website = "http://www.tomcoleforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Still, amazingly, trailbiz
    other_link = paste0(
      "https://www.trailblz.info/ColeTom/Donations.aspx?rdif=",
      "http%3a%2f%2fwww.tomcoleforcongress.com/thank_you&ver=2"
    ),
    FEC_ID_cmte = "C00379735",
    FEC_ID_cand = "H2OK04055"
  ),
  tibble(
    first_name = "Mary",
    last_name = "Brannon",
    state_cd = "OK-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.electbrannon.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "cmd=_donations&business=manager@electbrannon.com&",
      "item_name=Mary%20Brannon%20-%20RUNNING%20FOR%20OKLAHOMA%20",
      "CONGRESSIONAL%20DISTRICT%204&currency_code=USD"
    ),
    FEC_ID_cmte = "C00678060",
    FEC_ID_cand = "H8OK04094"
  ),
  tibble(
    first_name = "Bob",
    last_name = "White",
    state_cd = "OK-04",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bob White for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0OK04216"
  ),
  # OK-05 (last updated Oct 8, 2020) -------------------------------------------
  # Oakley for Oklahoma turned out to be for the state congressional district
  # See https://www.oakleyforok.com/
  tibble(
    first_name = "Kendra",
    last_name = "Horn",
    state_cd = "OK-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://horn.house.gov",
    campaign_name = "Kendraw Horn for Congress",
    campaign_website = "https://www.kendrahornforcongress.com/",
    actblue_link = c(
      "kendrahorn" # ,
      # "kh-popup"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00648915",
    FEC_ID_cand = "H8OK05109"
  ),
  tibble(
    first_name = "Terry",
    last_name = "Neese",
    state_cd = "OK-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Terry Neese for Congress",
    campaign_website = "https://neeseforcongress.com/",
    actblue_link = NA,
    # Again does not appear in Firefox or Chrome but in Opera? Not sure why...
    # Change documented to WinRed on Feb 27, 2020
    # At least from Feb 5, 2020
    # from 25-50-100-250-500-1000-2800-5600
    #   to 25-50-100-250-500-1000-2800
    anedot_link = "neese-for-congress/donate",
    other_link = "https://secure.winred.com/neese-for-congress/donate",
    FEC_ID_cmte = "C00704064",
    FEC_ID_cand = "H0OK05213"
  ),
  tibble(
    first_name = "Stephanie",
    last_name = "Bice",
    state_cd = "OK-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bice for Congress",
    campaign_website = "https://biceforcongress.com/",
    actblue_link = NA,
    # No suggested amounts. Could be a good baseline.
    anedot_link = "bice-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00703843",
    FEC_ID_cand = "H0OK05205"
  ),
  tibble(
    first_name = "Janet",
    last_name = "Barresi",
    state_cd = "OK-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Janet Barresi for Congress",
    campaign_website = "https://janet2020.com",
    actblue_link = NA,
    anedot_link = "janet-barresi-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00721357",
    FEC_ID_cand = "H0OK05254"
  ),
  tibble(
    first_name = "David",
    last_name = "Hill",
    state_cd = "OK-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Hill for Oklahoma",
    campaign_website = "https://davidhillforok.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/david-hill-for-oklahoma/donate-today",
    FEC_ID_cmte = "C00721092",
    FEC_ID_cand = "H0OK05247"
  ),
  tibble(
    first_name = "Merideth",
    last_name = "VanSant",
    state_cd = "OK-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "VanSant for Congress",
    campaign_website = "https://www.vansantforcongress.com",
    actblue_link = NA,
    anedot_link = "mvs2020/donate",
    other_link = NA,
    FEC_ID_cmte = "C00691196",
    FEC_ID_cand = "H0OK05262"
  ),
  tibble(
    first_name = "David",
    last_name = "Greene",
    state_cd = "OK-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Greene for Congress",
    campaign_website = "https://www.davidgreene.me/about/",
    actblue_link = NA,
    anedot_link = NA,
    # No suggested amounts.
    other_link = "https://www.davidgreene.me/donate/",
    FEC_ID_cmte = "C00699629",
    FEC_ID_cand = "H0OK05197"
  ),
  # Looks like Oakley withdrew
  # Looks like Dan Belcher withdrew
  tibble(
    first_name = "Miles",
    last_name = "Rahimi",
    state_cd = "OK-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Miles V Rahimi for Congress",
    campaign_website = "https://milesforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/miles-v-rahimi-for-congress",
    FEC_ID_cmte = "C00737205",
    FEC_ID_cand = "H0OK05270"
  ),
  # Oregon (last updated Oct 4, 2020) ==========================================
  # OR-01 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Suzanne",
    last_name = "Bonamici",
    state_cd = "OR-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://bonamici.house.gov",
    campaign_name = "Suzanne Bonamici for Congress",
    campaign_website = "https://www.bonamiciforcongress.com",
    actblue_link = "stand-with-suzanne-bonamici",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00500421",
    FEC_ID_cand = "H2OR01133"
  ),
  tibble(
    first_name = "Christopher",
    last_name = "Christensen",
    state_cd = "OR-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chris Christensen for US Congress",
    campaign_website = "https://cchrist2020.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://secure.winred.com/",
      "campaign-to-elect-chris-christensen-for-congress/donate"
    ),
    FEC_ID_cmte = "C00742031",
    FEC_ID_cand = "H0OR01194"
  ),
  tibble(
    first_name = "Amanda",
    last_name = "Siebe",
    state_cd = "OR-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Siebe 2020 for US House",
    campaign_website = "https://siebe2020.com",
    actblue_link = "siebe2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00717744",
    FEC_ID_cand = "H0OR01160"
  ),
  # Cannot find Hofscheier
  # Ricky Barajas website is down
  # Cannot find Bryan Tatum
  tibble(
    first_name = "Heidi",
    last_name = "Briones",
    state_cd = "OR-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Heidi Briones for Oregon District 1",
    # Website is down at https://heidi2020.com
    campaign_website = "https://twitter.com/HeidiBriones",
    actblue_link = "friends-of-heidi-briones-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00739045",
    FEC_ID_cand = "H0OR01186"
  ),
  # OR-02 (last updated Jul 13, 2021) -------------------------------------------
  # Greg Walden retired as of Oct 28, 2019; open seat
  tibble(
    first_name = "Cliff",
    last_name = "Bentz",
    state_cd = "OR-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cliff Bentz for Congress",
    campaign_website = "www.cliffbentz.com",
    actblue_link = NA,
    anedot_link = "cliff-bentz-for-congress/slug",
    other_link = NA,
    FEC_ID_cmte = "C00725465",
    FEC_ID_cand = "H0OR02127"
  ),
  tibble(
    first_name = "Alex",
    last_name = "Spenser",
    state_cd = "OR-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Spenser 2020",
    campaign_website = "http://spenser2020.com",
    actblue_link = NA,
    anedot_link = NA,
    # Rare Democrat Donorbox
    other_link = "https://donorbox.org/embed/spenser2020",
    FEC_ID_cmte = "C00732586",
    FEC_ID_cand = "H0OR02267"
  ),
  tibble(
    first_name = "Jimmy",
    last_name = "Crumpacker",
    state_cd = "OR-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Crumpacker for Congress",
    campaign_website = "https://crumpackerforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/crumpacker-for-congress/donate",
    FEC_ID_cmte = "C00731943",
    FEC_ID_cand = "H0OR02168"
  ),
  tibble(
    first_name = "Knute",
    last_name = "Buehler",
    state_cd = "OR-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Knute for Congress",
    campaign_website = "https://knutebuehler.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/knute-for-congress/contribute",
    FEC_ID_cmte = "C00730507",
    FEC_ID_cand = "H0OR02150"
  ),
  tibble(
    first_name = "Jason",
    last_name = "Atkinson",
    state_cd = "OR-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.jasonatkinsonforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Need to enter email to see; 25-100-500-1000-1500-2000-2800
    other_link = paste0(
      "https://www.jasonatkinsonforcongress.com/checkout/donate?",
      "donatePageId=5dea9f2dfb2743301f370efc"
    ),
    FEC_ID_cmte = "C00726786",
    FEC_ID_cand = "H0OR02135"
  ),
  tibble(
    first_name = "Jamie",
    last_name = "McLeod-Skinner",
    state_cd = "OR-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jamie for Oregon",
    campaign_website = "https://jamiefororegon.com",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-250-500-1000-2500
    other_link =
      "https://secure.c-esystems.com/jamiemcleodskinner/donation.aspx",
    FEC_ID_cmte = "C00649004",
    FEC_ID_cand = "H8OR02161"
  ),
  # Jeff Smith now running for governor
  # Isabella Tibbetts withdrew
  tibble(
    first_name = "Nik", # Nick, goes by Nik
    last_name = "Heuertz",
    state_cd = "OR-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nik for Congress",
    campaign_website = "https://nikforcongress.com",
    actblue_link = "nik-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00734418",
    FEC_ID_cand = "H0OR02176"
  ),
  tibble(
    first_name = "David",
    last_name = "Campbell",
    state_cd = "OR-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Campbell for Congress 2020",
    campaign_website = "https://campbellforcongress2020.com",
    actblue_link = NA,
    # No prompts
    anedot_link = "friends-of-david-r-campbell/donate",
    other_link = NA,
    FEC_ID_cmte = "C00736843",
    FEC_ID_cand = "H0OR02192"
  ),
  tibble(
    first_name = "Robert",
    last_name = "Werch",
    state_cd = "OR-02",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Robert Werch for Congress",
    campaign_website = "http://robertwerch.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "http://robertwerch.com/donate/",
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0OR02291"
  ),
  # Cannot find Mark Roberts
  # OR-03 (last updated Jul 13, 2021) -------------------------------------------
  tibble(
    first_name = "Earl",
    last_name = "Blumenauer",
    state_cd = "OR-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://blumenauer.house.gov",
    campaign_name = "Earl Blumenauer for Congress",
    campaign_website = "https://www.earlblumenauer.com",
    actblue_link = "stand-with-earl-blumenauer",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00307314",
    FEC_ID_cand = "H6OR03064"
  ),
  tibble(
    first_name = "Joanna",
    last_name = "Harbour",
    state_cd = "OR-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joanna Harbour for Congress",
    campaign_website = "https://joannaharbour.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/joanna-harbour-for-congress/donate",
    FEC_ID_cmte = "C00743203",
    FEC_ID_cand = "H0OR03174"
  ),
  tibble(
    first_name = "Albert",
    last_name = "Lee",
    state_cd = "OR-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Albert Lee for Congress",
    campaign_website = "https://albertlee2020.com",
    actblue_link = "albert-lee-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00701417",
    FEC_ID_cand = "H0OR03125"
  ),
  tibble(
    first_name = "Dane",
    last_name = "Wilcox",
    state_cd = "OR-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dane Wilcox for Congress",
    campaign_website = "https://wilcox2020.com",
    actblue_link = "dwforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00719328",
    FEC_ID_cand = "H0OR03158"
  ),
  tibble(
    first_name = "Tom", # Thomas Samuel, goes by Tom
    last_name = "Harrison",
    state_cd = "OR-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tom Harrison to Congress",
    campaign_website = "https://tomharrison4congress.com",
    actblue_link = NA,
    anedot_link = "tom-harrison-for-congress/retire-earl",
    other_link = NA,
    FEC_ID_cmte = c("C00681387", "C00684621"),
    FEC_ID_cand = "H8OR03102"
  ),
  tibble(
    first_name = "Josh",
    last_name = "Solomon",
    state_cd = "OR-03",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Josh Solomon for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0OR03216"
  ),
  # Ed Baker no activity for six months
  # https://www.facebook.com/edbakerforcongress/
  # Cannot find Gregory Aller
  # OR-04 (last updated Jul 13, 2021) -------------------------------------------
  tibble(
    first_name = "Peter",
    last_name = "Defazio",
    state_cd = "OR-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://defazio.house.gov",
    campaign_name = "Defazio for Congress",
    campaign_website = "https://www.defazioforcongress.org",
    actblue_link = "stand-with-peter-defazio",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00215905",
    FEC_ID_cand = "H6OR04047"
  ),
  tibble(
    first_name = "Alek",
    last_name = "Skarlatos",
    state_cd = "OR-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Alek for Oregon",
    campaign_website = "https://www.alekfororegon.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/alek-for-oregon/donate",
    FEC_ID_cmte = "C00715854",
    FEC_ID_cand = "H0OR04107"
  ),
  tibble(
    first_name = "Doyle",
    last_name = "Canning",
    state_cd = "OR-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Canning for Congress OR-04",
    campaign_website = "https://www.canningforcongress.com",
    actblue_link = "canning2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00707505",
    FEC_ID_cand = "H0OR04099"
  ),
  tibble(
    first_name = "Art",
    last_name = "Robinson",
    state_cd = "OR-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Art Robinson for Congress",
    campaign_website = "https://artforcongress.com",
    actblue_link = NA,
    anedot_link = "art-robinson-for-congress/artforcongress",
    other_link = NA,
    FEC_ID_cmte = "C00481341",
    FEC_ID_cand = "H0OR04081"
  ),
  tibble(
    first_name = "Cassidy",
    last_name = "Clausen",
    state_cd = "OR-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cassidy Clausen 4 Congress",
    campaign_website = "https://cassidyclausen4congress.com",
    # No donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00718429",
    FEC_ID_cand = "H0OR04115"
  ),
  tibble(
    first_name = "Nelson",
    last_name = "Ijih",
    state_cd = "OR-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nelson for Congress",
    campaign_website = "https://nelson2020.com",
    actblue_link = NA,
    anedot_link = "nelson-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00720185",
    FEC_ID_cand = "H0OR04123"
  ),
  tibble(
    first_name = "Daniel",
    last_name = "Hoffay",
    state_cd = "OR-04",
    party = "Grn",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Daniel Hoffay 2020",
    campaign_website = "http://danielhoffay2020.org/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0OR03208"
  ),
  # OR-05 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Kurt",
    last_name = "Schrader",
    state_cd = "OR-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://schrader.house.gov",
    campaign_name = "Kurt Schrader for Congress",
    campaign_website = "http://kurtschrader.com/",
    actblue_link = c(
      # Switch documented between ActBlue links on Feb 13, 2020
      "kurt_schrader",
      "kurtschrader"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00446906",
    FEC_ID_cand = "H8OR05107"
  ),
  tibble(
    first_name = "Amy Ryan",
    last_name = "Courser",
    state_cd = "OR-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Amy Ryan Courser for Congress 2020",
    campaign_website = "https://amyfororegon.com/",
    actblue_link = NA,
    # No suggested amounts
    anedot_link = "amy-ryan-courser-for-congress/ca7f2e90330e8fd8ef1b5",
    other_link = NA,
    FEC_ID_cmte = "C00717587",
    FEC_ID_cand = "H0OR05104"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Gamba",
    state_cd = "OR-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Gamba for Congress",
    campaign_website = "https://markgamba.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.ngpvan.com/v1/Forms/XQI0BEWQakCznGmAyJD96Q2",
    FEC_ID_cmte = "C00701490",
    FEC_ID_cand = "H0OR05096"
  ),
  tibble(
    first_name = "Joey",
    last_name = "Nations",
    state_cd = "OR-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nations for Congress",
    campaign_website = "https://www.joeynations.com/",
    actblue_link = NA,
    anedot_link = NA,
    # No suggested amounts
    # Change documented to WinRed on Feb 12, 2020
    other_link =
      # paste0(
      #   "https://www.paypal.com/donate/?token=gnKIc7iiekbGM7EfSW6mWxl3TNT-bfm",
      #   "BQ1akHGPZfj9qNgjZbwjfsnPHzAfsOdnN4l--kG&country.x=US&locale.x="
      # ),
      "https://secure.winred.com/JoeyNations/donatefightingfororegon",
    FEC_ID_cmte = "C00672477",
    FEC_ID_cand = "H8OR05206"
  ),
  tibble(
    first_name = "Angela",
    last_name = "Roman",
    state_cd = "OR-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Angela Roman for Congress",
    # Keeps giving me security risk warning, so taking it off scraping now
    campaign_website = NA, # "https://angelaromanforcongress.com/",
    # Change documented to WinRed Nov 16, 2019
    # at least from Nov 14, 2019
    # 25-50-100-250-500-1000-2800 to 25-50-100-250-500-1000-2800 (same)
    # It's back to WinRed as of Feb 13, 2020
    actblue_link = NA,
    anedot_link = "angelaromanforcongress/contribute",
    # anedot_link = NA,
    # Current link splits donations btw Roman and Trump though.
    other_link = "https://secure.winred.com/AngelaRoman/donate",
    FEC_ID_cmte = "C00700062",
    FEC_ID_cand = "H0OR05088"
  ),
  tibble(
    first_name = "Blair",
    last_name = "Reynolds",
    state_cd = "OR-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Blair Reynolds for Congress",
    campaign_website = "https://reynoldsfor.us/",
    actblue_link = "reynoldsforus",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00736355",
    FEC_ID_cand = "H0OR05112"
  ),
  # Pennsylvania (last updated Oct 4, 2020) ====================================
  # PA-01 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Brian",
    last_name = "Fitzpatrick",
    state_cd = "PA-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://fitzpatrick.house.gov",
    campaign_name = "Brian Fitzpatrick for Congress",
    campaign_website = "https://www.brianfitzpatrick.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/brian-fitzpatrick-for-congress/donate",
    FEC_ID_cmte = "C00607416",
    FEC_ID_cand = "H6PA08277"
  ),
  tibble(
    first_name = "Christina",
    last_name = "Finello",
    state_cd = "PA-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Christina Finello to Congress",
    campaign_website = "https://www.finelloforcongress.com",
    actblue_link = "christina-finello-web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00713693",
    FEC_ID_cand = "H0PA01325"
  ),
  # PA-02 (last updated Oct 4, 2020) -------------------------------------------
  # Brendan Boyle was uncontested (no candidates registered as of Sep 26, 2019)
  # Now Boyle vs. Torres
  tibble(
    first_name = "Brendan",
    last_name = "Boyle",
    state_cd = "PA-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://boyle.house.gov",
    campaign_name = "Congressman Brendan Boyle",
    campaign_website = "https://voteboyle.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.ngpvan.com/v1/Forms/V5zS2tQN7UGegndBJW-oKw2",
    FEC_ID_cmte = "C00543363",
    FEC_ID_cand = "H4PA13199"
  ),
  tibble(
    first_name = "David",
    last_name = "Torres",
    state_cd = "PA-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Torres for Congress 2020",
    campaign_website = "https://www.torres-2020.com",
    # No donation links visible
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00742015", "C00677344"),
    FEC_ID_cand = "H8PA02110"
  ),
  # PA-03 (last updated Oct 4, 2020) ------------------------------------------
  # Dwight Evans was uncontested (no candidates registered as of Sep 26, 2019)
  # Now Evans vs. Harvey
  tibble(
    first_name = "Dwight",
    last_name = "Evans",
    state_cd = "PA-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://evans.house.gov",
    campaign_name = "Dwight Evans for Congress",
    campaign_website = "https://www.dwightevans.com/",
    actblue_link = "dwight-evans-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00591065",
    FEC_ID_cand = "H6PA02171"
  ),
  # Not yet on FEC
  tibble(
    first_name = "Michael",
    last_name = "Harvey",
    state_cd = "PA-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Michael Harvey for Congress",
    campaign_website = "https://friendsofmichaelha.wixsite.com/mysite/about",
    actblue_link = NA,
    anedot_link = "runmikerun/donate",
    other_link = NA,
    FEC_ID_cmte = "", # Not available
    FEC_ID_cand = "H0PA03347"
  ),
  # PA-04 (last updated Oct 4, 2020) -------------------------------------------
  # Madeleine Dean was uncontested (no candidates registered as of Sep 24, '19)
  # Now Dean vs. Barnette vs. Tarshish
  # Cannot find Tarshish (independent)
  tibble(
    first_name = "Madeleine",
    last_name = "Dean Cunnane", # Goes by Dean
    state_cd = "PA-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "MAD 4 PA",
    campaign_website = "https://www.mad4pa.com/",
    # Change from NGP VAN to ActBlue
    actblue_link = "mad4pawebsite",
    anedot_link = NA,
    # other_link = "https://www.mad4pa.com/donate"
    # NGP VAN in disguise
    other_link = "https://act.myngp.com/Forms/5687575663157447168",
    FEC_ID_cmte = "C00670844",
    FEC_ID_cand = "H8PA04116"
  ),
  tibble(
    first_name = "Kathy",
    last_name = "Barnette",
    state_cd = "PA-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kathy Barnette for Congress",
    campaign_website = "https://kathybarnetteforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/kathybarnette/donate",
    FEC_ID_cmte = "C00734731",
    FEC_ID_cand = "H0PA04279"
  ),
  # PA-05 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Mary Gay",
    last_name = "Scanlon",
    state_cd = "PA-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://scanlon.house.gov",
    campaign_name = "Mary Gay Scanlon for Congress",
    campaign_website = "https://www.scanlonforcongress.com",
    actblue_link = "socialmgs",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00669358", "C00688051"),
    FEC_ID_cand = "H8PA07200"
  ),
  tibble(
    first_name = "Dasha",
    last_name = "Pruett",
    state_cd = "PA-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dasha for Congress",
    campaign_website = "https://www.dashaforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/dasha-for-congress/donate",
    FEC_ID_cmte = "C00732727",
    FEC_ID_cand = "H0PA05094"
  ),
  # PA-06 (last updated Oct 4, 2020) -------------------------------------------
  # Houlahan vs. Emmons
  # vs. Siedenbuehl vs. Stern vs. McHugh
  tibble(
    first_name = "Chrissy",
    last_name = "Houlahan",
    state_cd = "PA-06",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://houlahan.house.gov",
    campaign_name = "Chrissy Houlahan For U.S. Congress",
    campaign_website = "https://www.chrissyhoulahanforcongress.com/",
    actblue_link = "chrissy-houlahan-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00637371",
    FEC_ID_cand = "H8PA06087"
  ),
  tibble(
    first_name = "John",
    last_name = "Emmons",
    state_cd = "PA-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John Emmons for Congress",
    campaign_website = "https://www.emmonsforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/JohnEmmons/donate",
    FEC_ID_cmte = "C00713537",
    FEC_ID_cand = "H0PA06167"
  ),
  tibble(
    first_name = "Tom",
    last_name = "Siedenbuehl",
    state_cd = "PA-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Siedenbuehl for Congress",
    campaign_website = "https://www.tomforpa.com/",
    actblue_link = NA,
    anedot_link = NA,
    # NationBuilder
    # other_link = "https://www.tomforpa.com/contribute",
    # Change documented to WinREd on Feb 29, 2020
    # from Paypal prompts of 25-50-100-1000-2500
    #                     to 25-50-100-250-500-1000-2800
    other_link = "https://secure.winred.com/siedenbuehl-for-congress/support",
    FEC_ID_cmte = "C00701722",
    FEC_ID_cand = "H0PA06142"
  ),
  # Everett Stern withdrew
  tibble(
    first_name = "John",
    last_name = "McHugh",
    state_cd = "PA-06",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://johnhmchugh.com",
    actblue_link = NA,
    anedot_link = NA,
    # Prompts for 10-20-50
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "token=RxI5aRmSmr2rk0bMOSrfjIjq1vLF__MmrsbjtSvdXNJgpduJq9yVr",
      "WEfFRPTHu-lOl9H7G&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00720110",
    FEC_ID_cand = "H0PA06175"
  ),
  # PA-07 (last updated Oct 4, 2020) -------------------------------------------
  # Wild vs. Scheller vs. Browning vs. Connolly
  tibble(
    first_name = "Susan",
    last_name = "Wild",
    state_cd = "PA-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://wild.house.gov",
    campaign_name = "Susan Wild for Congress",
    campaign_website = "https://wildforcongress.com",
    actblue_link = c(
      # Change within ActBlue
      # from 25-50-100-250-500-1000-2800
      #   to 50-100-250-500-1000-2800
      # Between Jun 17 and Jun 18 (hey, very specific! Nice!)
      # view-source:https://web.archive.org/web/20200616081917/
      # https://wildforcongress.com/
      "susan-wild-for-congress-1",
      "susanwild"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00658567",
    FEC_ID_cand = "H8PA15229"
  ),
  tibble(
    first_name = "Lisa",
    last_name = "Scheller",
    state_cd = "PA-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Scheller for Congress",
    campaign_website = "https://schellerforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/scheller-for-congress-inc/donate",
    FEC_ID_cmte = "C00722892",
    FEC_ID_cand = "H0PA07132"
  ),
  tibble(
    first_name = "Dean",
    last_name = "Browning",
    state_cd = "PA-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Browning For Congress",
    campaign_website = "https://www.browningforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/deanbrowning/donate",
    FEC_ID_cmte = "C00664466",
    FEC_ID_cand = "H8PA15260"
  ),
  tibble(
    first_name = "Matt",
    last_name = "Connolly",
    state_cd = "PA-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Matt Connolly For Congress PA7",
    campaign_website = "https://www.mattforpa7.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://mattforpa7.revv.co/donate-today",
    FEC_ID_cmte = "C00730127",
    FEC_ID_cand = "H4PA17141"
  ),
  # PA-08 (last updated Oct 4, 2020) -------------------------------------------
  # Matthew Cartwright is uncontested
  # (no candidates registered as of Sep 26, 2019)
  tibble(
    first_name = "Matthew",
    last_name = "Cartwright",
    state_cd = "PA-08",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://cartwright.house.gov",
    campaign_name = "Cartwright for Congress",
    campaign_website = "https://www.cartwrightcongress.com/",
    actblue_link = "mattcartwright",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00509968",
    FEC_ID_cand = "H2PA17079"
  ),
  tibble(
    first_name = "Jim",
    last_name = "Bognet",
    state_cd = "PA-08",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bognet for Congress",
    campaign_website = "https://bognetforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/bognet-for-congress/donate",
    FEC_ID_cmte = c("C00735688", "C00702852"),
    FEC_ID_cand = "H0PA08221"
  ),
  # PA-09 (last updated Oct 4, 2020) -------------------------------------------
  # Daniel Meuser was uncontested as of Mar 1, 2020
  # Now Meuser vs. Wegman
  tibble(
    first_name = "Daniel",
    last_name = "Meuser",
    state_cd = "PA-09",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://meuser.house.gov",
    campaign_name = "Meuser for Congress",
    campaign_website = "https://www.meuserforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/DanMeuser/donate",
    FEC_ID_cmte = "C00654723",
    FEC_ID_cand = "H8PA10147"
  ),
  tibble(
    first_name = "Gary",
    last_name = "Wegman",
    state_cd = "PA-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gary Wegman for Congress",
    campaign_website = "https://garywegmanforcongress.com",
    actblue_link = "gary-wegman-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00614784",
    FEC_ID_cand = "H6PA16338"
  ),
  # PA-10 (last updated Oct 4, 2020) ------------------------------------------
  # Perry vs. DePasquale vs. Brier vs. Jeffries
  tibble(
    first_name = "Scott",
    last_name = "Perry",
    state_cd = "PA-10",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Patriots for Perry",
    campaign_website = "https://patriotsforperry.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/ScottPerry/donate",
    FEC_ID_cmte = "C00510164",
    FEC_ID_cand = "H2PA04135"
  ),
  tibble(
    first_name = "Eugene",
    last_name = "DePasquale",
    state_cd = "PA-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Eugene for Congress",
    campaign_website = "https://eugeneforcongress.com",
    actblue_link = c(
      # Change within ActBlue; was still old link on Sep 8, 2020
      # But changed by Sep 29, 2020
      "depasquale-for-pa-1-weekly",
      "depasquale-for-pa-1"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710533",
    FEC_ID_cand = "H0PA10110"
  ),
  tibble(
    first_name = "Tom", # Thomas F., goes by Tom
    last_name = "Brier",
    state_cd = "PA-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brier for Congress",
    campaign_website = "https://brierforcongress.com",
    actblue_link = "brier-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00697037",
    FEC_ID_cand = "H0PA10102"
  ),
  # Bobby Jeffries now running for State House
  # PA-11 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Lloyd",
    last_name = "Smucker",
    state_cd = "PA-11",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://smucker.house.gov",
    campaign_name = "Smucker for Congress",
    campaign_website = "http://www.electsmuckerpa.com",
    actblue_link = NA,
    anedot_link = "smucker-for-congress/smuckerforcongress",
    other_link = NA,
    FEC_ID_cmte = "C00599464",
    FEC_ID_cand = "H6PA16320"
  ),
  tibble(
    first_name = "Sarah",
    last_name = "Hammond",
    state_cd = "PA-11",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sarah Hammond for Congress PA-11",
    campaign_website = "https://www.sarahforus.com",
    actblue_link = "11united",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00721837",
    FEC_ID_cand = "H0PA11183"
  ),
  # Missed Paul Daigle: https://www.paulforpa.com/donate-1 not archived
  # PA-12 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Fred",
    last_name = "Keller",
    state_cd = "PA-12",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://keller.house.gov",
    campaign_name = "Fred Keller for Congress",
    campaign_website = "https://www.fredkellerforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://fredkeller.revv.co/donate",
    FEC_ID_cmte = "C00697052",
    FEC_ID_cand = "H0PA12181"
  ),
  tibble(
    first_name = "Lee",
    last_name = "Griffin",
    state_cd = "PA-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lee Griffin for Congress",
    campaign_website = "https://www.leegriffinforcongress.com",
    # Interesting portfolio of 12-24-36-48-72-120-240
    actblue_link = "leegriffinforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00742270",
    FEC_ID_cand = "H0PA12199"
  ),
  # Marc Friedenberg was for special election
  # PA-13 (last updated Oct 4, 2020) -------------------------------------------
  # John Joyce was uncontested (no candidates registered as of Sep 26, 2019)
  # Now Joyce vs. Rowley
  tibble(
    first_name = "John",
    last_name = "Joyce",
    state_cd = "PA-13",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Dr. John Joyce for Congress",
    campaign_website = "https://www.johnjoyceforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Still Victory Passport
    other_link = paste0(
      "https://mystique.victorypassport.com/pages/joyce/main-donation-page?",
      "location=https%3A%2F%2Fgive.victorypassport.com%2Fjoyce%2Fdonate-today",
      "&location_url=aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5jb20vam95Y2UvZG9u",
      "YXRlLXRvZGF5LyM%3D"
    ),
    FEC_ID_cmte = "C00674259",
    FEC_ID_cand = "H8PA13125"
  ),
  tibble(
    first_name = "Todd",
    last_name = "Rowley",
    state_cd = "PA-13",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Todd Rowley for Congress",
    campaign_website = "https://www.toddrowleyforcongress.com",
    actblue_link = "todd-rowley-for-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00723858",
    FEC_ID_cand = "H0PA13114"
  ),
  # PA-14 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Guy",
    last_name = "Reschenthaler",
    state_cd = "PA-14",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://reschenthaler.house.gov",
    campaign_name = "Guy for Congress",
    campaign_website = "https://www.guyforpa.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/GuyReschenthaler/donate",
    FEC_ID_cmte = "C00657833",
    FEC_ID_cand = "H8PA18199"
  ),
  tibble(
    first_name = "Bill", # William
    last_name = "Marx",
    state_cd = "PA-14",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bill Marx for Congress",
    campaign_website = "https://www.billmarxforcongress.org",
    actblue_link = "friends-to-elect-bill-marx-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00736504",
    FEC_ID_cand = "H0PA14138"
  ),
  # PA-15 (last updated Oct 4, 2020) -------------------------------------------
  # Glenn Thompson was uncontested (no candidates registered as of Sep 26, 2019)
  # Now Thompson vs. Williams
  tibble(
    first_name = "Glenn",
    last_name = "Thompson",
    state_cd = "PA-15",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://thompson.house.gov",
    campaign_name = "Congressman Glenn Thompson",
    campaign_website = "http://www.friendsofglennthompson.com/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = paste0(
    #   "https://spark.widgetmakr.com/widget/render/",
    #   "bc5797a3-fb34-4966-ab20-4f0310560d78?hostUrl=gtthompson.com",
    #   "%2Fcontribute.html"
    # ),
    # Change documented to WinRed on Oct 8, 2019 (probably changed on 6th)
    # 10-25-50-100-150-500-1000-2800, same amounts
    other_link = "https://secure.winred.com/GlennThompson/donate",
    FEC_ID_cmte = "C00444620",
    FEC_ID_cand = "H8PA05071"
  ),
  # Not on FEC yet
  tibble(
    first_name = "Robert",
    last_name = "Williams",
    state_cd = "PA-15",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://williamsfor15.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://politics.raisethemoney.com/rwilliamsa8r3vFcVJ-RUmu-rVSBtLg",
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # PA-16 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Mike", # Uh, filed as George J, but known as Mike
    last_name = "Kelly",
    state_cd = "PA-16",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://kelly.house.gov",
    campaign_name = "Mike Kelly for Congress",
    campaign_website = "https://www.mikekellyforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-200-250-500-1000-2800-5600
    other_link = "https://www.mikekellyforcongress.com/contribute/",
    FEC_ID_cmte = "C00474189",
    FEC_ID_cand = "H0PA03271"
  ),
  tibble(
    first_name = "Kristy",
    last_name = "Gnibus",
    state_cd = "PA-16",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kristy Gnibus for Congress",
    campaign_website = "https://www.kristyforcongress.com",
    # Nice link
    actblue_link = "new_web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00722314",
    FEC_ID_cand = "H0PA16034"
  ),
  # PA-17 (last updated Oct 4, 2020) ------------------------------------------
  tibble(
    first_name = "Conor",
    last_name = "Lamb",
    state_cd = "PA-17",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://lamb.house.gov",
    campaign_name = "Conor Lamb for Congress",
    campaign_website = "https://conorlamb.com/",
    actblue_link = "lamb-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00657411",
    FEC_ID_cand = "H8PA18181"
  ),
  tibble(
    first_name = "Richard",
    last_name = "Parnell",
    state_cd = "PA-17",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sean Parnell for Congress",
    campaign_website = "https://www.seanforcongress.co/",
    actblue_link = NA,
    anedot_link = NA,
    # Was WinRed from pretty much the beginning
    other_link =
      "https://secure.winred.com/americans-for-parnell-committee/donate",
    FEC_ID_cmte = "C00724914",
    FEC_ID_cand = "H0PA17115"
  ),
  tibble(
    first_name = "Scott",
    last_name = "Timko",
    state_cd = "PA-17",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Timko for Congress",
    campaign_website = "https://teamtimko.nationbuilder.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://teamtimko.nationbuilder.com/help_scott_with_your_donation",
    FEC_ID_cmte = "C00701086",
    FEC_ID_cand = "H0PA17099"
  ),
  tibble(
    first_name = "Brian",
    last_name = "Thomsen",
    state_cd = "PA-17",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Thomsen for Congress",
    campaign_website = "https://thomsenforcongress.com/",
    actblue_link = NA,
    anedot_link = "thomsen-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00721894",
    FEC_ID_cand = "H0PA17107"
  ),
  tibble(
    first_name = "Jesse",
    last_name = "Vodvarka",
    state_cd = "PA-17",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jesse Vodvarka for Congress",
    campaign_website = "http://jessejamesvodvarka.com/",
    # Not accepted donations
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00735720",
    FEC_ID_cand = "H0PA17123"
  ),
  # PA-18 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Michael",
    last_name = "Doyle",
    state_cd = "PA-18",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://doyle.house.gov",
    campaign_name = "Mike Doyle for Congress",
    campaign_website = "http://mikedoyleforcongress.com",
    actblue_link = "mike-doyle-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00290064",
    FEC_ID_cand = "H4PA18131"
  ),
  tibble(
    first_name = "Luke",
    last_name = "Negron",
    state_cd = "PA-18",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Negron 2020",
    campaign_website = "https://www.negron2020.com",
    actblue_link = NA,
    anedot_link = "negron-campaign-committee/donate",
    other_link = NA,
    FEC_ID_cmte = "C00735852",
    FEC_ID_cand = "H0PA18113"
  ),
  tibble(
    first_name = "Jerry", # Gerald
    last_name = "Dickinson",
    state_cd = "PA-18",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jerry Dickinson for Congress",
    campaign_website = "https://www.jerrydickinson.com",
    # Although down, 10-52-50-100-250
    actblue_link = "wbsitedonate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00701847",
    FEC_ID_cand = "H0PA18097"
  ),
  # Rhode Island (last updated Jul 13, 2021) ====================================
  # RI-01 (last updated Oct 4, 2020) -------------------------------------------
  # Cicilline vs. Wysocki (Rep -> Ind, Rep primary canceled)
  tibble(
    first_name = "David",
    last_name = "Cicilline",
    state_cd = "RI-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://cicilline.house.gov",
    campaign_name = "David Cicilline for Congress",
    campaign_website = "https://www.cicilline.com/",
    actblue_link = "david-cicilline-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00476564",
    FEC_ID_cand = "H0RI01073"
  ),
  tibble(
    first_name = "Fred",
    last_name = "Wysocki",
    state_cd = "RI-01",
    party = "Ind", # Was initially Republican
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Fred Wysocki for U.S. House",
    campaign_website = "https://www.fred4house.com/",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?token=gIhhvP8n77h2DphnynZuRE6FW5IonWma0",
      "511I1W3X2UsXQbUACp3QMY5iYnrku6t0xfL3W&country.x=US&locale.x=US&Z3JncnB0="
    ),
    FEC_ID_cmte = "C00683391",
    FEC_ID_cand = "H8RI01092"
  ),
  # RI-02 (last updated Oct 4, 2020) -------------------------------------------
  # Jim Langevin was uncontested (no candidates registered as of Oct 5, 2019)
  # Now Langevin vs. Lancia
  tibble(
    first_name = "Jim",
    last_name = "Langevin",
    state_cd = "RI-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://langevin.house.gov",
    campaign_name = "Langevin for Congress",
    campaign_website = "https://www.jimlangevin.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/8936116428386732032",
    FEC_ID_cmte = "C00344697",
    FEC_ID_cand = "H0RI02139"
  ),
  tibble(
    first_name = "Bob", # Robert
    last_name = "Lancia",
    state_cd = "RI-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lancia for Congress",
    campaign_website = "https://www.boblanciaforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/paid-for-by-lancia-for-congress/donate",
    FEC_ID_cmte = "C00738302",
    FEC_ID_cand = "H0RI02287"
  ),
  # South Carolina ============================================================
  # SC-01 (last updated Oct 4, 2020) -------------------------------------------
  # Cunningham vs. Landing
  # vs. Mace vs. Covert vs. Cunningham vs. Cox vs. Norris
  # Former rep Mark Sanford now in the presidential(!) race
  tibble(
    first_name = "Joe",
    last_name = "Cunningham",
    state_cd = "SC-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://cunningham.house.gov",
    campaign_name = "Joe Cunningham for Congress",
    campaign_website = "https://www.joecunninghamforcongress.com/",
    actblue_link = "joe-general",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00650507",
    FEC_ID_cand = "H8SC01116"
  ),
  tibble(
    first_name = "Nancy",
    last_name = "Mace",
    state_cd = "SC-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nancy Mace for Congress",
    campaign_website = "https://nancymace.org/",
    actblue_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from Feb 4
    # from 10-25-50-100-250-500-1000-2800-5600
    #   to 10-25-50-100-250-500-1000-2800-5600 (same amounts)
    # anedot_link = "nancy-mace-for-congress/donate",
    anedot_link = NA,
    other_link = "https://secure.winred.com/nancymace/donate",
    FEC_ID_cmte = "C00710103",
    FEC_ID_cand = "H0SC01394"
  ),
  tibble(
    first_name = "Kathy",
    last_name = "Landing",
    state_cd = "SC-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kathy Landing for Congress",
    campaign_website = "https://www.kathylanding.com/",
    actblue_link = NA,
    anedot_link = "kathy-landing-for-congress/2020",
    other_link = NA,
    FEC_ID_cmte = "C00708875",
    FEC_ID_cand = "H0SC01386"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Covert",
    state_cd = "SC-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Michael Covert for Congress",
    campaign_website = "https://mikecovertforcongress.com/",
    actblue_link = NA,
    anedot_link = "covert/website-splash",
    other_link = NA,
    FEC_ID_cmte = "C00693630",
    FEC_ID_cand = "H0SC01360"
  ),
  tibble(
    first_name = "Logan",
    last_name = "Cunningham",
    state_cd = "SC-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Logan Cunningham For Congress",
    # Uh... okay.
    campaign_website = "https://www.facebook.com/LoganCunninghamForCongress/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://logancunninghamforcongress.revv.co/logancunninghamforcongress",
    FEC_ID_cmte = "C00707133",
    FEC_ID_cand = "H0SC01378"
  ),
  tibble(
    first_name = "Chris",
    last_name = "Cox",
    state_cd = "SC-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chris Cox for Congress",
    campaign_website = "https://www.chriscox2020.com/",
    actblue_link = NA,
    anedot_link = "chris-cox-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00711390",
    FEC_ID_cand = "H0SC01402"
  ),
  tibble(
    first_name = "Phil",
    last_name = "Norris",
    state_cd = "SC-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Phil Norris for Congress",
    campaign_website = "http://philnorrisforcongress.org/",
    actblue_link = NA,
    anedot_link = NA,
    # Option defaults to 30
    other_link = "https://donorbox.org/embed/phil-norris-for-congress",
    FEC_ID_cmte = "C00712471",
    FEC_ID_cand = "H0SC01410"
  ),
  # SC-02 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Joe",
    last_name = "Wilson",
    state_cd = "SC-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://joewilson.house.gov",
    campaign_name = "Joe Wilson for Congress",
    campaign_website = "https://joemeansjobs.com",
    actblue_link = NA,
    anedot_link = NA,
    # other_link =
    #   "https://secure.piryx.com/donate/WzJc4e8g/joewilson/sitelink",
    # Change documented to WinRed on Oct 4, 2020
    
    other_link = "https://secure.winred.com/joewilson/donate",
    FEC_ID_cmte = "C00368522",
    FEC_ID_cand = "H2SC02059"
  ),
  tibble(
    first_name = "Adair",
    last_name = "Boroughs",
    state_cd = "SC-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Adair for Congress",
    campaign_website = "https://www.adairforcongress.com",
    actblue_link = c(
      # Change within ActBlue
      # from 5-10-25-50-100-250-500
      #   to 25-50-100-250-500-1000-2800
      "adair-for-congress",
      "adairforcongress"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00701052",
    FEC_ID_cand = "H0SC02061"
  ),
  # Dropped out
  tibble(
    first_name = "Lawrence",
    last_name = "Nathaniel",
    state_cd = "SC-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nathaniel for Congress",
    campaign_website = "https://www.nathaniel2020.com",
    actblue_link = "nathaniel2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00693903",
    FEC_ID_cand = "H0SC02079"
  ),
  tibble(
    first_name = "Viresh",
    last_name = "Sinha",
    state_cd = "SC-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vote Sinha",
    campaign_website = "http://votesinha.org",
    actblue_link = "vote-sinha-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00723221",
    FEC_ID_cand = "H0SC02087"
  ),
  # SC-03 (last updated Oct 4, 2020) -------------------------------------------
  # Jeff Duncan was uncontested (no candidates registered as of Sep 26, 2019)
  # Now duncan vs. Cleveland
  tibble(
    first_name = "Jeff",
    last_name = "Duncan",
    state_cd = "SC-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://jeffduncan.house.gov",
    campaign_name = "Jeff Duncan for Congress",
    campaign_website = "https://jeffduncan.com/home/",
    actblue_link = NA,
    anedot_link = "jeffduncan/donate",
    # Also has a "Donate crypto to Jeff Duncan for Congress". What.
    other_link = "https://bitpay.com/593644/donate",
    FEC_ID_cmte = "C00460550",
    FEC_ID_cand = "H0SC03077"
  ),
  tibble(
    first_name = "Hosea",
    last_name = "Cleveland",
    state_cd = "SC-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hosea Cleveland for Congress",
    campaign_website = "https://www.clevelandforcongress.com",
    actblue_link = "hosea-cleveland",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00616888",
    FEC_ID_cand = "H4SC03111"
  ),
  # SC-04 (last updated Jul 13, 2021) -------------------------------------------
  tibble(
    first_name = "William",
    last_name = "Timmons",
    state_cd = "SC-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://timmons.house.gov",
    campaign_name = "William Timmons for Congress",
    campaign_website = "https://www.votetimmons.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/williamtimmons/donate",
    FEC_ID_cmte = "C00668491",
    FEC_ID_cand = "H8SC04250"
  ),
  tibble(
    first_name = "Kim",
    last_name = "Nelson",
    state_cd = "SC-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kim Nelson for Congress",
    campaign_website = "https://www.kimnelsonforcongress.com",
    actblue_link = "kim-nelson-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710665",
    FEC_ID_cand = "H0SC04281"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Chandler",
    state_cd = "SC-04",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chandler for Congress",
    campaign_website = "https://mgchand39.wixsite.com/chandlerforcongress",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H2SC04121"
  ),
  # SC-05 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Ralph",
    last_name = "Norman",
    state_cd = "SC-05",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://norman.house.gov",
    campaign_name = "Ralph Norman for Congress",
    campaign_website = "https://www.electralphnorman.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/ralphnorman/contribute",
    FEC_ID_cmte = "C00633610",
    FEC_ID_cand = "H8SC05158"
  ),
  tibble(
    first_name = "Moe", # Mauricus, goes by Moe
    last_name = "Brown",
    state_cd = "SC-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Moe Brown for Congress",
    campaign_website = "https://www.moeforcongress.com",
    actblue_link = "moe-brown-for-congressq3",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00736694",
    FEC_ID_cand = "H0SC05056"
  ),
  # SC-06 (last updated Jul 13, 2021) -------------------------------------------
  # Jim Clyburn was uncontested (no candidates registered as of Sep 26, 2019)
  # Now Clyburn vs. McCollum vs. Hackett
  tibble(
    first_name = "Jim",
    last_name = "Clyburn",
    state_cd = "SC-06",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://clyburn.house.gov",
    campaign_name = "Jim Clyburn for U.S. Congress",
    campaign_website = "https://clyburnforcongress.com/",
    actblue_link = "jim-clyburn-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00255562",
    FEC_ID_cand = "H2SC02042"
  ),
  tibble(
    first_name = "John",
    last_name = "McCollum",
    state_cd = "SC-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John McCollum for Congress",
    campaign_website = "https://www.johnforsc.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = paste0(
      "https://www.paypal.com/donate?",
      "token=PhwuL_ASJctTWLU2Fnq443EVz5pU5brP0wxVLOBZKgWO2yhD2eVzsW1e",
      "PE9BJF9IFz-MRzdsYv6RtCDu"
    ),
    FEC_ID_cmte = "C00745877",
    FEC_ID_cand = "H0SC06237"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Hackett",
    state_cd = "SC-06",
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Hackett for Congress",
    campaign_website = "https://www.facebook.com/markhackettSC",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0SC06245"
  ),
  # SC-07 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Tom",
    last_name = "Rice",
    state_cd = "SC-07",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://rice.house.gov",
    campaign_name = "Tom Rice for U.S. Congress",
    campaign_website = "http://www.votetomrice.com",
    actblue_link = NA,
    anedot_link = "tom-rice-for-congress/general-fund-c9c0830d077f2b773c399",
    other_link = NA,
    FEC_ID_cmte = "C00506048",
    FEC_ID_cand = "H2SC07066"
  ),
  tibble(
    first_name = "Melissa",
    last_name = "Watson",
    state_cd = "SC-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Melissa Watson for Congress",
    campaign_website = "https://www.melissawatsonforcongress.com",
    actblue_link = "mww-rtw-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00732883",
    FEC_ID_cand = "H0SC07029"
  ),
  # South Dakota (at-large district; last updated Oct 4, 2020) =================
  # Johnson vs. Spawn vs. Raver vs. Wirth
  # Spawn withdrew and Wirth I cannot find
  tibble(
    first_name = "Dusty",
    last_name = "Johnson",
    state_cd = "SD-0",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://dustyjohnson.house.gov",
    campaign_name = "Dusty Johnson for Congress",
    campaign_website = "https://www.dustyjohnson.com",
    actblue_link = NA,
    # Change documented to WinRed on Mar 1, 2020
    # from 25-50-100-250-1000-2800
    #   to 25-50-100-250-1000-2800 (same amounts)
    # anedot_link = "friends-for-dusty-johnson/dusty",
    anedot_link = NA,
    other_link = "https://secure.winred.com/friends-of-dusty-johnson/donate",
    FEC_ID_cmte = "C00628917",
    FEC_ID_cand = "H8SD01055"
  ),
  # Dem Primary was cancelled: Rep vs. Lib
  tibble(
    first_name = "Randy",
    last_name = "Luallin",
    state_cd = "SD-0",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elect Randy Luallin for US Congress",
    campaign_website = "http://www.randyluallin.com",
    # No donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "", # Not available
    FEC_ID_cand = "H2CO02155"
  ),
  tibble(
    first_name = "Whitney",
    last_name = "Raver",
    state_cd = "SD-0",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Whitney Raver for Congress",
    campaign_website = "https://www.whitneyraverforcongress.org/",
    # ActBlue to NGP VAN?!
    actblue_link = "whitney-raver-for-congress-1",
    anedot_link = NA,
    other_link = "https://secure.ngpvan.com/77vpDb3SyECzH1KYQvpCaA2",
    FEC_ID_cmte = "C00715961",
    FEC_ID_cand = "H0SD01078"
  ),
  # # Ellee Spawn dropped out and arrested(!)
  tibble(
    first_name = "Brian",
    last_name = "Wirth",
    state_cd = "SD-0",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brian Wirth for Congress",
    campaign_website = "http://brianwirth605.com",
    actblue_link = "brian-wirth-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00691519",
    FEC_ID_cand = "H0SD01060"
  ),
  tibble(
    first_name = "Liz", # Elizabeth, goes by Liz
    last_name = "Marty May",
    state_cd = "SD-0",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Liz Marty May for Congress",
    campaign_website = "https://www.lizmayforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/liz-marty-may-for-congress/web",
    FEC_ID_cmte = "C00736561",
    FEC_ID_cand = "H0SD01094"
  ),
  # Tennessee (last updated Jul 13, 2021) =======================================
  # TN-01 (last updated Jul 13, 2021) -------------------------------------------
  tibble(
    first_name = "Blair",
    last_name = "Walshingham",
    state_cd = "TN-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Blair Walsingham for Congress",
    campaign_website = "https://blairforcongress.com",
    actblue_link = "blair-pn",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00739953",
    FEC_ID_cand = "H0TN01092"
  ),
  tibble(
    first_name = "Diana",
    last_name = "Harshbarger",
    state_cd = "TN-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Diana for Congress",
    campaign_website = "https://www.votediana.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/diana-for-congress/donate",
    FEC_ID_cmte = "C00741090",
    FEC_ID_cand = "H0TN01118"
  ),
  tibble(
    first_name = "Josh",
    last_name = "Gapp",
    state_cd = "TN-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Josh Gapp for Congress",
    campaign_website = "https://www.joshgapp.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = "https://www.joshgapp.com/donate",
    FEC_ID_cmte = "C00743732",
    FEC_ID_cand = "H0TN01142"
  ),
  # Missed Clark but archived
  tibble(
    first_name = "John",
    last_name = "Clark",
    state_cd = "TN-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John Clark for Cognress",
    campaign_website = "http://www.johnclarkforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.anedot.com/john-clark-for-congress/donate",
    FEC_ID_cmte = c("C00733907", "C00733881"),
    FEC_ID_cand = "H0TN01076"
  ),
  tibble(
    first_name = "Rusty", # Dewey Rusty Edward, goes by Rusty
    last_name = "Crowe",
    state_cd = "TN-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rusty Crowe to Congress",
    campaign_website = "https://rusty4congress.com",
    actblue_link = NA,
    anedot_link = "citizens-to-elect-rusty-crowe-to-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00738930",
    FEC_ID_cand = "H0TN01084"
  ),
  tibble(
    first_name = "Steve", # Stephen, goes by Steve
    last_name = "Darden",
    state_cd = "TN-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Steve Darden for Congress",
    campaign_website = "https://www.stevedarden.com/#",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-1000-2000-2800
    other_link = "https://www.stevedarden.com/donations/donate-now/",
    FEC_ID_cmte = "C00740589",
    FEC_ID_cand = "H0TN01100"
  ),
  tibble(
    first_name = "Timothy",
    last_name = "Hill",
    state_cd = "TN-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Timothy Hill for Congress",
    campaign_website = "https://www.timothyhillforcongress.com",
    actblue_link = NA,
    anedot_link =
      "timothy-hill-for-congress-888fb18c-b790-4c92-bc7a-2fee816773dd/donate",
    other_link = NA,
    FEC_ID_cmte = "C00743393",
    FEC_ID_cand = "H0TN01134"
  ),
  tibble(
    first_name = "David",
    last_name = "Hawk",
    state_cd = "TN-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Hawk for Tennessee",
    campaign_website = "https://www.davidhawkfortennessee.com",
    actblue_link = NA,
    anedot_link = NA,
    # Only has donate by mail
    other_link = "https://www.davidhawkfortennessee.com/donate",
    FEC_ID_cmte = "C00743591",
    FEC_ID_cand = "H0TN01159"
  ),
  tibble(
    first_name = "Chris",
    last_name = "Rowe",
    state_cd = "TN-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chris Rowe Democrat for Congress",
    campaign_website = "https://chrisroweforcongress.com",
    actblue_link = "chris-rowe-for-america-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00693739",
    FEC_ID_cand = "H0TN01043"
  ),
  tibble(
    first_name = "Steve",
    last_name = "Holder",
    state_cd = "TN-01",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Steve Holder for Congress",
    campaign_website = "https://steveholder44945532.wordpress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0TN01241"
  ),
  # Lost Jay Adkins
  # TN-02 (last updated Oct 4, 2020) -------------------------------------------
  # Tim Burchett was uncontested (no candidates registered as of Sep 26, 2019)
  # Now Burchett vs. Hoyos
  tibble(
    first_name = "Tim",
    last_name = "Burchett",
    state_cd = "TN-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://burchett.house.gov",
    campaign_name = "Burchett for Congress",
    campaign_website = "https://www.burchettforcongress.com/",
    actblue_link = NA,
    anedot_link = "timburchett/donate",
    other_link = NA,
    FEC_ID_cmte = "C00652149",
    FEC_ID_cand = "H8TN02119"
  ),
  tibble(
    first_name = "Renee",
    last_name = "Hoyos",
    state_cd = "TN-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Renee Hoyos for Congress",
    campaign_website = "https://hoyosforcongress.com",
    actblue_link = "renee-hoyos-for-congress-3",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00706820", "C00654988"),
    FEC_ID_cand = "H8TN02176"
  ),
  # TN-03 (last updated Oct 4, 2020) -------------------------------------------
  # Chuck Fleischmann is uncontested
  # (no candidates registered as of Sep 26, 2019)
  tibble(
    first_name = "Chuck",
    last_name = "Fleischmann",
    state_cd = "TN-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://fleischmann.house.gov",
    campaign_name = "Chuck Fleischmann for Congress",
    campaign_website = "https://www.chuckforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-1000-2700
    other_link = "https://www.chuckforcongress.com/donate",
    FEC_ID_cmte = "C00461822",
    FEC_ID_cand = "H0TN03254"
  ),
  tibble(
    first_name = "Meg",
    last_name = "Gorman",
    state_cd = "TN-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Meg for Congress",
    campaign_website = "https://www.meg2020.com",
    actblue_link = "meg2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00742395",
    FEC_ID_cand = "H0TN03452"
  ),
  # TN-04 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Scott",
    last_name = "DesJarlais",
    state_cd = "TN-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://desjarlais.house.gov",
    campaign_name = "DesJarlais for Congress",
    campaign_website = "http://scottdesjarlais.com",
    actblue_link = NA,
    anedot_link = "desjarlais-for-congress/general-fund-c20e80656b131663d1cf7",
    other_link = NA,
    FEC_ID_cmte = "C00464073",
    FEC_ID_cand = "H0TN04195"
  ),
  tibble(
    first_name = "Christopher",
    last_name = "Hale",
    state_cd = "TN-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Christopher Hale for Tennessee",
    campaign_website = "https://www.christopherhale.com",
    actblue_link = "christopherhale",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00678045",
    FEC_ID_cand = "H8TN04107"
  ),
  # TN-05 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Jim", # James, goes by Jim
    last_name = "Cooper",
    state_cd = "TN-05",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://cooper.house.gov",
    campaign_name = "Cooper for Congress",
    campaign_website = "https://www.cooperforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-2800
    other_link = "https://www.cooperforcongress.com/form/-8877775747779720448",
    FEC_ID_cmte = "C00376665",
    FEC_ID_cand = "H2TN05131"
  ),
  tibble(
    first_name = "Keeda",
    last_name = "Haynes",
    state_cd = "TN-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Keeda Haynes for Congress",
    campaign_website = "https://keedahaynes.com",
    # Although suspended, 25-100-250-1000
    actblue_link = "keeda-haynes-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00733311",
    FEC_ID_cand = "H0TN05382"
  ),
  tibble(
    first_name = "Justin",
    last_name = "Bautista-Jones", # Bautista-Jones, but simply Jones in campaign
    state_cd = "TN-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Justin Jones for Congress",
    campaign_website = "https://justinjones2020.com",
    # Although suspended, 3-16-27-50-100-500-1000
    actblue_link = "justin-jones-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00726646",
    FEC_ID_cand = "H0TN05333"
  ),
  tibble(
    first_name = "Joshua",
    last_name = "Rawlings",
    state_cd = "TN-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joshua Rawlings for Congress",
    campaign_website = "https://www.rawlings2020.com",
    actblue_link = "rawlings",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00718551",
    FEC_ID_cand = "H0TN05317"
  ),
  # No Republican candidate
  # TN-06 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "John",
    last_name = "Rose",
    state_cd = "TN-06",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://johnrose.house.gov",
    campaign_name = "John Rose for Tennessee",
    campaign_website = "https://johnrose.com",
    actblue_link = NA,
    # The issue of Anedot frames not working in certain browsers...
    # How will this affect donations?
    anedot_link = "john-rose-for-tennessee/donate",
    other_link = NA,
    FEC_ID_cmte = "C00652743",
    FEC_ID_cand = "H8TN06094"
  ),
  tibble(
    first_name = "Christopher",
    last_name = "Finley",
    state_cd = "TN-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Finley for Congress",
    campaign_website = paste0(
      "https://www.facebook.com/pages/category/Political-Candidate/",
      "Finley-for-Congress-224332224832975/"
    ),
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00754176",
    FEC_ID_cand = "H0TN06364"
  ),
  # TN-07 (last updated Oct 4, 2020) -------------------------------------------
  # Mark Green was uncontested (no candidates registered as of Oct 3, 2019)
  # Now Green vs. Sreepada
  tibble(
    first_name = "Mark",
    last_name = "Green",
    state_cd = "TN-07",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://markgreen.house.gov",
    campaign_name = "Dr. Mark Green for Congress",
    campaign_website = "https://www.markgreen4tn.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/MarkGreen/donate",
    FEC_ID_cmte = "C00658385",
    FEC_ID_cand = "H8TN07076"
  ),
  tibble(
    first_name = "Kiran",
    last_name = "Sreepada",
    state_cd = "TN-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kiran Sreepada for Congress",
    campaign_website = "https://www.kiranforcongress.com",
    actblue_link = "kiransreepada",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00721761",
    FEC_ID_cand = "H0TN07255"
  ),
  # TN-08 (last updated Oct 4, 2020) -------------------------------------------
  # David Kustoff was uncontested (no candidates registered as of Sep 26, 2019)
  # Now Kustoff vs. Pearson
  tibble(
    first_name = "David",
    last_name = "Kustoff",
    state_cd = "TN-08",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://kustoff.house.gov",
    campaign_name = "Kustoff for Congress",
    campaign_website = "https://www.kustoffforcongress.com/",
    actblue_link = NA,
    anedot_link = "kustoff-for-congress/kustoff-for-congress",
    other_link = NA,
    FEC_ID_cmte = c("C00614826", "C00376244"),
    FEC_ID_cand = "H2TN07103"
  ),
  tibble(
    first_name = "Erika",
    last_name = "Stotts-Pearson",
    state_cd = "TN-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Erika Stotts Pearson for Congress",
    campaign_website = "https://erikastottspearson.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = "https://erikastottspearson.com/donate/",
    FEC_ID_cmte = "C00684977",
    FEC_ID_cand = "H6TN08250"
  ),
  tibble(
    first_name = "James",
    last_name = "Hart",
    state_cd = "TN-08",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "James Hart for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00372722",
    FEC_ID_cand = "H2TN08077"
  ),
  tibble(
    first_name = "Jon",
    last_name = "Dillard",
    state_cd = "TN-08",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jon Dillard for Congress",
    campaign_website = "https://jondillard.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0TN08360"
  ),
  # TN-09 (last updated Oct 4, 2020) -------------------------------------------
  # Stephen Cohen was uncontested (no candidates registered as of Sep 26, 2019)
  # Now Cohen vs.
  tibble(
    first_name = "Steve",
    last_name = "Cohen",
    state_cd = "TN-09",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://cohen.house.gov",
    campaign_name = "Cohen for Congress",
    campaign_website = "http://cohenforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 20-35-50-100-500-2800
    other_link = "https://act.myngp.com/Forms/-2295579567298574336",
    FEC_ID_cmte = "C00422980",
    FEC_ID_cand = "H6TN09068"
  ),
  tibble(
    first_name = "Charlotte",
    last_name = "Bergmann",
    state_cd = "TN-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bergmann for Congress",
    campaign_website = "https://electbergmann.com",
    actblue_link = NA,
    anedot_link = "bergmann/donate",
    other_link = NA,
    FEC_ID_cmte = c("C00693143", "C00666685"),
    FEC_ID_cand = "H8TN09114"
  ),
  tibble(
    first_name = "Corey",
    last_name = "Strong",
    state_cd = "TN-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Corey Strong for Congress",
    campaign_website = "https://the901strong.com",
    actblue_link = "the901strong",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00713255",
    FEC_ID_cand = "H0TN09129"
  ),
  # Texas (last updated Oct 4, 2020) ===========================================
  # TX-01 (last updated Oct 4, 2020) -------------------------------------------
  # Brent Beal seems to be an unsuccessful candidate of 2018,
  # and no websites are available (Although there is 211.83 USD contributions
  # as of Jan 21, 2020)
  tibble(
    first_name = "Louie",
    last_name = "Gohmert",
    state_cd = "TX-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Louie Gohmert for Congress",
    campaign_website = "https://gohmert.com/",
    actblue_link = NA,
    # Was actually Anedot. Same amounts
    anedot_link = "gohmert/donate",
    # 50-100-250-500-1000-2800-5600
    # other_link = "https://gohmert.com/contribute/",
    other_link = NA,
    FEC_ID_cmte = "C00386532",
    FEC_ID_cand = "H4TX04039"
  ),
  tibble(
    first_name = "Hank",
    last_name = "Gilbert",
    state_cd = "TX-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hank Gilbert For Congress",
    campaign_website = "https://hankfortexas.com/",
    actblue_link = c(
      # Change documented within ActBlue
      "hank-newweb",
      # Was originally this (accidentally put for campaign names)
      "hank-gilbert-for-congress-1"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00718270",
    FEC_ID_cand = "H0TX01108"
  ),
  # TX-02 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Dan", #Daniel, goes by Dan
    last_name = "Crenshaw",
    state_cd = "TX-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://crenshaw.house.gov",
    campaign_name = "Crenshaw for Congress",
    campaign_website = "https://crenshawforcongress.com/",
    actblue_link = NA,
    # anedot_link = "crenshaw/donate",
    anedot_link = NA,
    # Change documented to WinRed on Oct 4, 2020
    # Not archived directly, but according to website,
    # was still Anedot by Feb 4 but turned to WinRed by Apr 22
    # WinRed went through complicated changes; carefully document
    other_link = c(
      "https://secure.winred.com/crenshawforcongress/support",
      # This used to be 2800-1000-500-250-100-50-35-15 on Jul 27, 2020
      # Now 5-15-35-50-100-250-5000-1000-2800, same as first link
      # (Just different text, same amounts I guess?)
      "https://secure.winred.com/crenshawforcongress/standwithdancrenshaw",
      # 35-50-100-250-500-1000-2800
      "https://secure.winred.com/crenshawforcongress/donate"
    ),
    FEC_ID_cmte = "C00660795",
    FEC_ID_cand = "H8TX02166"
  ),
  tibble(
    first_name = "Sima",
    last_name = "Ladjevardian",
    state_cd = "TX-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sima Ladjevardian for Texas",
    campaign_website = "https://www.simafortx.com",
    actblue_link = "simafortx-web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00730408",
    FEC_ID_cand = "H0TX02106"
  ),
  tibble(
    first_name = "Elisa",
    last_name = "Cardnell",
    state_cd = "TX-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Cardnell for Congress",
    campaign_website = "https://www.elisacardnell.com/",
    actblue_link = c(
      # Change within ActBlue, at least from May 10, 2020
      # But lost runoff
      "elisa4moms",
      "elisa-cardnell-for-congress-1"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00696146",
    FEC_ID_cand = "H0TX02080"
  ),
  # TX-03 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Van", # Nicholas Van, goes by Van
    last_name = "Taylor",
    state_cd = "TX-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://vantaylor.house.gov",
    campaign_name = "Van Taylor for Congress",
    campaign_website = "https://www.vantaylor.com",
    actblue_link = NA,
    # Does not properly appear in some browsers
    anedot_link = "vantaylorcampaign/donate",
    other_link = NA,
    FEC_ID_cmte = "C00653634",
    FEC_ID_cand = "H8TX03123"
  ),
  tibble(
    first_name = "Lulu", # Helane Lulu Sawsan, goes by Lulu
    last_name = "Seikaly",
    state_cd = "TX-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lulu Seikaly for Texas",
    campaign_website = "https://lulufortexas.com",
    actblue_link = "as-lulu-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C0072167",
    FEC_ID_cand = "H0TX03120"
  ),
  tibble(
    first_name = "Sean",
    last_name = "McCaffity",
    state_cd = "TX-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sean McCaffity for Congress",
    campaign_website = "https://www.seanmccaffity.com",
    actblue_link = "sm_newweb",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00715318",
    FEC_ID_cand = "H0TX03112"
  ),
  # Lorie Burch dropped out Apr 29, 2019
  # Tanner Do ran but lost primary on Marh 3, 2020; not archived
  tibble(
    first_name = "Al",
    last_name = "Robertson",
    state_cd = "TX-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Al Robertson for Congress TX3",
    campaign_website = "https://twitter.com/AlRobertsonTX03",
    actblue_link = "al-robertson-for-congress-tx3-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710376",
    FEC_ID_cand = "H0TX03104"
  ),
  tibble(
    first_name = "Roger",
    last_name = "Barone",
    state_cd = "TX-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://barone4congress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-150-250-500-1000-1500-2000-2700
    other_link = "https://barone4congress.com/contribute",
    FEC_ID_cmte = "C00637405",
    FEC_ID_cand = "H8TX03081"
  ),
  # TX-04 (last updated Jul 13, 2021) -------------------------------------------
  # John Ratcliffe was uncontested (no candidates registered as of Sep 26, 2019)
  # Became Director of national intelligence
  tibble(
    first_name = "Floyd",
    last_name = "McLendon",
    state_cd = "TX-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "McLendon for Congress",
    campaign_website = "https://www.mclendonforcongress.com",
    actblue_link = NA,
    anedot_link = "mclendon-for-congress/donate-homepage",
    other_link = NA,
    FEC_ID_cmte = "C00713933",
    FEC_ID_cand = "H0TX32079"
  ),
  tibble(
    first_name = "Russell",
    last_name = "Foster",
    state_cd = "TX-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Russell Foster for Congress",
    campaign_website = "https://foster4texas.com",
    actblue_link = "russellfostertx",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00722686",
    FEC_ID_cand = "H0TX04151"
  ),
  tibble(
    first_name = "John",
    last_name = "Ratcliffe",
    state_cd = "TX-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://ratcliffe.house.gov",
    campaign_name = "Ratcliffe for Congress",
    campaign_website = "https://ratcliffeforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = paste0(
    #   "https://spark.widgetmakr.com/widget/render/",
    #   "cd6250ae-f270-45d2-b44b-621380f06fd3?hostUrl=",
    #   "ratcliffeforcongress.com%2Fdonate%2F"
    # ),
    # Change documented to WinRed on Oct 8, 2019
    # 10-25-50-100-250-500-1000-2700, same amounts (hmm?!)
    # Now 10-25-50-100-250-1000-2800 as of Oct 27, 2019
    other_link = "https://secure.winred.com/JohnRatcliffe/donate",
    FEC_ID_cmte = "C00554113",
    FEC_ID_cand = "H4TX04153"
  ),
  tibble(
    first_name = "Lou",
    last_name = "Antonelli",
    state_cd = "TX-04",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lou Antonelli for Congress",
    campaign_website = "https://louantonelliforcongress.blogspot.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.powr.io/checkout_screen?unique_label=2d93d4a1_1599429793",
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0TX04250"
  ),
  tibble(
    first_name = "Pat",
    last_name = "Fallon",
    state_cd = "TX-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Fallon for Congress",
    campaign_website = "https://www.fallonforcongress.com/",
    actblue_link = NA,
    anedot_link = "fallonforcongress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00750307",
    FEC_ID_cand = "H0TX04219"
  ),
  # TX-05 (last updated Oct 4, 2020) -------------------------------------------
  # Lance Gooden is uncontested (no candidates registered as of Sep 26, 2019)
  tibble(
    first_name = "Lance",
    last_name = "Gooden",
    state_cd = "TX-05",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://gooden.house.gov",
    campaign_name = "Lance Gooden for Congress",
    campaign_website = "http://lancegooden.com/",
    actblue_link = NA,
    anedot_link = "lancegooden/donate",
    other_link = NA,
    FEC_ID_cmte = "C00662601",
    FEC_ID_cand = "H8TX05144"
  ),
  tibble(
    first_name = "Carolyn",
    last_name = "Salter",
    state_cd = "TX-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Carolyn Salter for Congress",
    campaign_website = "https://www.salterforcongress.com/home/",
    actblue_link = "as-carolyn-salter-for-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00734236",
    FEC_ID_cand = "H0TX05109"
  ),
  # TX-06 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Ron",
    last_name = "Wright",
    state_cd = "TX-06",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://wright.house.gov",
    campaign_name = "Wright for Congress",
    campaign_website = "https://wright4congress.com",
    actblue_link = NA,
    anedot_link = "ronwrightforcongress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00662171",
    FEC_ID_cand = "H8TX06233"
  ),
  tibble(
    first_name = "Stephen",
    last_name = "Daniel",
    state_cd = "TX-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Stephen Daniel for Congress",
    campaign_website = "https://www.stephendaniel.com",
    actblue_link = "sd-new-web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711739",
    FEC_ID_cand = "H0TX06099"
  ),
  # TX-07 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Lizzie Pannill", # Elizabeth
    last_name = "Fletcher",
    state_cd = "TX-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://fletcher.house.gov",
    campaign_name = "Fletcher for Congress",
    campaign_website = "http://www.lizziefletcher.com",
    actblue_link = "fletcherforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00640045",
    FEC_ID_cand = "H8TX07140"
  ),
  tibble(
    first_name = "Wesley",
    last_name = "Hunt",
    state_cd = "TX-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wesley Hunt for Texas",
    campaign_website = "https://wesleyfortexas.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/wesleyfortexas/donate/",
    FEC_ID_cmte = c("C00701003", "C00702886"),
    FEC_ID_cand = "H0TX07170"
  ),
  # TX-08 (last updated Jul 13, 2021) -------------------------------------------
  tibble(
    first_name = "Kevin",
    last_name = "Brady",
    state_cd = "TX-08",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Kevin Brady for Congress",
    campaign_website = "https://www.bradyforcongress.com",
    actblue_link = NA,
    anedot_link = "kevin-brady-for-congress/general-fund-cbd6700dbd2c89f877de6",
    other_link = NA,
    FEC_ID_cmte = "C00311043",
    FEC_ID_cand = "H6TX08100"
  ),
  tibble(
    first_name = "Liz", # Elizabeth
    last_name = "Hernandez",
    state_cd = "TX-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elizabeth Hernandez for Congress",
    campaign_website = "https://lizfortx8.com",
    actblue_link = "lizfortx8",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00731901",
    FEC_ID_cand = "H0TX08129"
  ),
  tibble(
    first_name = "Kirk",
    last_name = "Osborn",
    state_cd = "TX-08",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kirk Osborn for Congress",
    campaign_website = "https://www.kirkosbornforcongress.com",
    actblue_link = NA,
    # No prompts
    anedot_link = "kirk-osborn-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00733436",
    FEC_ID_cand = "H0TX08137"
  ),
  tibble(
    first_name = "Laura",
    last_name = "Jones",
    state_cd = "TX-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Laura Jones for Congress",
    campaign_website = "https://laurajonesforcongress.com",
    # Paused but 5-10-15-20-25-100
    actblue_link = "laurajonesforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00721597",
    FEC_ID_cand = "H0TX08145"
  ),
  tibble(
    first_name = "Chris",
    last_name = "Duncan",
    state_cd = "TX-08",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chris Duncan for Congress",
    campaign_website = "https://www.facebook.com/ChrisDuncanLPTexas/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H8TX08122"
  ),
  # TX-09 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Al",
    last_name = "Green",
    state_cd = "TX-09",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://algreen.house.gov",
    campaign_name = "Al Green for Congress",
    campaign_website = "https://algreen.org/",
    # Now has an ActBlue
    actblue_link = "al-green-website",
    anedot_link = NA,
    other_link =
      "https://secure.piryx.com/donate/8FdmG2VS/Congressman-Al-Green-Campaign/",
    FEC_ID_cmte = "C00578567",
    FEC_ID_cand = "H4TX09095"
  ),
  tibble(
    first_name = "Johnny",
    last_name = "Teague",
    state_cd = "TX-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Johnny Teague for Congress",
    campaign_website = "https://www.johnnyteague.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      # 25-50-100-250-1000-2800
      "https://www.johnnyteague.com/donate",
      # 10-50-100-250-1000-2800
      "https://donorbox.org/johnny-teague-for-congress"
    ),
    FEC_ID_cmte = "C00706242",
    FEC_ID_cand = "H0TX09176"
  ),
  tibble(
    first_name = "Jon",
    last_name = "Menefee",
    state_cd = "TX-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Menefee for Congress",
    campaign_website = "https://www.menefeeforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Oops, link was missing. 25-50-100-250-1000-2800
    other_link = "https://www.menefeeforcongress.com/donation",
    FEC_ID_cmte = "C00683656",
    FEC_ID_cand = "H0TX09168"
  ),
  tibble(
    first_name = "Melissa Mechelle",
    last_name = "Williams", # Goes as Wilson-Williams
    state_cd = "TX-09",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Melissa Mechelle Wilson (Williams) for Congress",
    campaign_website = "https://melissamechelle.org",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://politics.raisethemoney.com/mwilson",
    FEC_ID_cmte = "C00709865",
    FEC_ID_cand = "H0TX09184"
  ),
  # TX-10 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Michael",
    last_name = "McCaul",
    state_cd = "TX-10",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://mccaul.house.gov",
    campaign_name = "McCaul for Congress",
    campaign_website = "https://michaelmccaul.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/mccaul-for-congress/donate",
    FEC_ID_cmte = "C00392688",
    FEC_ID_cand = "H4TX10093"
  ),
  tibble(
    first_name = "Mike", #Michael, goes by Mike
    last_name = "Siegel",
    state_cd = "TX-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mike Siegel For Congress",
    campaign_website = "https://siegelfortexas.org",
    actblue_link = "siegel-for-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00662668",
    FEC_ID_cand = "H8TX10110"
  ),
  # Not on FEC yet; no contribution link
  tibble(
    first_name = "Roy",
    last_name = "Eriksen",
    state_cd = "TX-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Eriksen for Congress",
    campaign_website = "https://www.royeriksen.com",
    actblue_link = "siegel-for-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = NA
  ),
  # Lost Hutcheson
  tibble(
    first_name = "Shannon",
    last_name = "Hutcheson",
    state_cd = "TX-10",
    party = "",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://shannonhutcheson.com",
    # 10-25-50-100-250-1000-2800
    actblue_link = "shannon-hutcheson-for-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00707026",
    FEC_ID_cand = "H8TX10185"
  ),
  tibble(
    first_name = "Pritesh",
    last_name = "Gandhi",
    state_cd = "TX-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.gandhifortexas.com",
    # 10-50-100-250-500-1000-2800
    actblue_link = "gandhi-for-texas-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00698134",
    FEC_ID_cand = "H0TX10216"
  ),
  # TX-11 (last updated Oct 4, 2020) -------------------------------------------
  # Mike Conaway is retiring. Website is there but no donation link
  tibble(
    first_name = "August",
    last_name = "Pfluger",
    state_cd = "TX-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "August Pfluger for Congress",
    campaign_website = "https://www.augustpfluger.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/august-pfluger-for-congress/donate",
    FEC_ID_cmte = "C00719294",
    FEC_ID_cand = "H0TX11230"
  ),
  tibble(
    first_name = "Jon Mark",
    last_name = "Hogg",
    state_cd = "TX-11",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.jonmarkhogg.com",
    actblue_link = NA,
    anedot_link = NA,
    # 20-35-50-100-500-2500
    other_link = "https://secure.ngpvan.com/hVXyiw2qAU-OKjw3guh7Qg2",
    FEC_ID_cmte = "C00716415",
    FEC_ID_cand = "H0TX11198"
  ),
  # Lost Lacy and Batch
  tibble(
    first_name = "J. Ross",
    last_name = "Lacy",
    state_cd = "TX-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.jrosslacy.com",
    actblue_link = NA,
    # Unfortunately, not archived.
    anedot_link = "j-ross-lacy-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00715169",
    FEC_ID_cand = "H0TX11172"
  ),
  tibble(
    first_name = "Brandon",
    last_name = "Batch",
    state_cd = "TX-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.brandonbatch.com/",
    actblue_link = NA,
    # Unfortunately, not archived.
    anedot_link = "brandon-batch-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00721076",
    FEC_ID_cand = "H0TX11263"
  ),
  tibble(
    first_name = "JD",
    last_name = "Faircloth",
    state_cd = "TX-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "J. D. Faircloth For Congress",
    campaign_website = "https://fairclothforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-1000-2000-2800-5600
    other_link = "https://politics.raisethemoney.com/jfaircloth",
    FEC_ID_cmte = "C00723064",
    FEC_ID_cand = "H0TX11271"
  ),
  tibble(
    first_name = "James",
    last_name = "Berryhill",
    state_cd = "TX-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Berryhill for Congress",
    campaign_website = "https://jamieberryhillforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/JamieBerryhill/donate",
    FEC_ID_cmte = "C00725648",
    FEC_ID_cand = "H0TX11248"
  ),
  tibble(
    first_name = "Wesley",
    last_name = "Virdell",
    state_cd = "TX-11",
    party = "",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.virdellforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-250-1000-2500
    other_link = "https://www.virdellforcongress.com/donate",
    FEC_ID_cmte = "C00719260",
    FEC_ID_cand = "H0TX11222"
  ),
  tibble(
    first_name = "Wacey Alpha",
    last_name = "Cody",
    state_cd = "TX-11",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wacey Alpha Cody for Congress",
    campaign_website = "https://www.waceyalphacodyforcongress.com",
    actblue_link = NA,
    # No prompts
    anedot_link = "wacey-alpha-cody-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00698720",
    FEC_ID_cand = "H0TX11164"
  ),
  # TX-12 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Kay",
    last_name = "Granger",
    state_cd = "TX-12",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://kaygranger.house.gov",
    campaign_name = "",
    campaign_website = "https://kaygranger.com",
    actblue_link = NA,
    # Only shows up properly on Firefox
    anedot_link = "granger/donate",
    other_link = NA,
    FEC_ID_cmte = "C00310532",
    FEC_ID_cand = "H6TX12060"
  ),
  tibble(
    first_name = "Lisa",
    last_name = "Welch",
    state_cd = "TX-12",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lisa Welch for Texas Congress",
    campaign_website = "https://www.lisawelch.org",
    actblue_link = "drwelch",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00719310",
    FEC_ID_cand = "H0TX12238"
  ),
  tibble(
    first_name = "Trey",
    last_name = "Holcomb",
    state_cd = "TX-12",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Trey for Texas",
    campaign_website = "https://treyfortexas.vote",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://politics.raisethemoney.com/tholcomb",
    FEC_ID_cmte = "C00734541",
    FEC_ID_cand = "H0TX12261"
  ),
  tibble(
    first_name = "Chris",
    last_name = "Putnam",
    state_cd = "TX-12",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Putnam for Texas",
    campaign_website = "https://www.putnamfortexas.com",
    # Only gives "Donate via Mail" option. Wow!
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00720474",
    FEC_ID_cand = "C00310532"
  ),
  # TX-13 (last updated Oct 4, 2020) -------------------------------------------
  # Thornberry announced retirement on Sep 30; his contribution page is gone.
  # Now open seat
  tibble(
    first_name = "Gus",
    last_name = "Trujillo",
    state_cd = "TX-13",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gus For Congress",
    campaign_website = "https://gustrujillo.com",
    actblue_link = "gus",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00727990",
    FEC_ID_cand = "H0TX13152"
  ),
  tibble(
    first_name = "Ronny",
    last_name = "Jackson",
    state_cd = "TX-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Texans for Ronny Jackson",
    campaign_website = "https://ronnyjacksonfortexas13.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/texans-for-ronny-jackson/contribute",
    FEC_ID_cmte = "C00730531",
    FEC_ID_cand = "H0TX13228"
  ),
  # Lost Ekstrom for Congress
  # Donation link lost
  tibble(
    first_name = "Josh",
    last_name = "Winegarner",
    state_cd = "TX-13",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Texans for Ronny Jackson",
    campaign_website = "https://www.joshwinegarner.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://joshwinegarnerforcongress.ichooseapex.com/default.aspx",
    FEC_ID_cmte = "C00723536",
    FEC_ID_cand = "H0TX13129"
  ),
  tibble(
    first_name = "Greg",
    last_name = "Sagan",
    state_cd = "TX-13",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Greg Sagan for Congress",
    campaign_website = "https://gregsaganforcongress.org",
    actblue_link = "gregsagan2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00649392",
    FEC_ID_cand = "H8TX13163"
  ),
  # TX-14 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Randy",
    last_name = "Weber",
    state_cd = "TX-14",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://weber.house.gov",
    campaign_name = "",
    campaign_website = "https://randyweber.org",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00502229",
    FEC_ID_cand = "H2TX14149"
  ),
  tibble(
    first_name = "Adrienne",
    last_name = "Bell",
    state_cd = "TX-14",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Adrienne Bell for Congress",
    campaign_website = "https://www.voteforbell.com",
    actblue_link = "bellwebsite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00700534", "C00639872"),
    FEC_ID_cand = "H8TX14120"
  ),
  # TX-15 (last updated Oct 4, 2020) -------------------------------------------
  tibble(
    first_name = "Vicente",
    last_name = "Gonzalez",
    state_cd = "TX-15",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://gonzalez.house.gov",
    campaign_name = "Vicente Gonzalez for U.S. Congress",
    campaign_website = "http://www.vicentegonzalez.com",
    actblue_link = "vicente-gonzalez-for-u-s--congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00592659",
    FEC_ID_cand = "H6TX15162"
  ),
  tibble(
    first_name = "Monica",
    last_name = "De La Cruz Hernandez",
    state_cd = "TX-15",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Monica De La Cruz-Hernandez For Congress",
    campaign_website = "https://www.monicaforcongress.us",
    actblue_link = NA,
    # No prompts
    anedot_link = "monica-de-la-cruz-hernandez-for-congress/m4congress",
    other_link = NA,
    FEC_ID_cmte = "C00723072",
    FEC_ID_cand = "H0TX15124"
  ),
  tibble(
    first_name = "Ryan",
    last_name = "Krause",
    state_cd = "TX-15",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ryan Krause for Congress",
    campaign_website = "https://ryankrauseforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/embed/ryan-krause-for-congress",
    FEC_ID_cmte = "C00662841",
    FEC_ID_cand = "H0TX15108"
  ),
  # TX-16 (last updated Oct 3, 2020) -------------------------------------------
  # Escobar vs. Armendariz-Jackson
  tibble(
    first_name = "Veronica",
    last_name = "Escobar",
    state_cd = "TX-16",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://escobar.house.gov",
    campaign_name = "Veronica Escobar Democrat for Congress",
    campaign_website = "https://veronicaescobar.com/",
    actblue_link = "voteforveronica2018",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00653923",
    FEC_ID_cand = "H8TX16109"
  ),
  tibble(
    first_name = "Irene",
    last_name = "Armendariz-Jackson",
    state_cd = "TX-16",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Irene Armendariz-Jackson for Congress",
    campaign_website = "https://www.irenearmendarizjackson.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://donate.fundhero.io/irene-armendariz-jackson-for-congress",
    FEC_ID_cmte = "C00714378",
    FEC_ID_cand = "H0TX16106"
  ),
  tibble(
    first_name = "Sam",
    last_name = "Williams",
    state_cd = "TX-16",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Samuel Lee Williams For Congress",
    campaign_website = "https://samforep.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/SamuelLeeWilliams/donate",
    FEC_ID_cmte = "C00679720",
    FEC_ID_cand = "H8TX16166"
  ),
  tibble(
    first_name = "Anthony",
    last_name = "Aguero",
    state_cd = "TX-16",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Aguero For Texas",
    campaign_website = "https://www.facebook.com/AgueroForTexas/",
    # "http://aguerofortexas.us/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "token=-3qdr-cLL3lmGbj7TP6jIekG1D0bWqLrSQUAlTVsghKYYzuiYmgnTPxptAj",
      "AQFHztbV-g0&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00709360",
    FEC_ID_cand = "H0TX16098"
  ),
  tibble(
    first_name = "Jaime",
    last_name = "Arrriola", # So... it's Arriola but misspelled in FEC
    state_cd = "TX-16",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jaime for Congress",
    campaign_website = "https://www.jaime4congress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from Feb 21, 2020
    # from 5-10-20-50-100-200-500-1000-2000
    #   to 5-10-25-50-100-250-500
    other_link =
      # "https://app.campaignhq.com/campaigns/supportjaimeforcongress/pay",
      "https://secure.winred.com/JaimeArriola/votejaime",
    FEC_ID_cmte = "C00717231",
    FEC_ID_cand = "H0TX16114"
  ),
  # TX-17 (last updated Oct 3, 2020) -------------------------------------------
  # Flores vs. Kennedy but somehow, Flores is not listed on FEC! What?!
  # Sutton (Sep 28, 2019) and Sessions (announced Oct 3, not on FEC) joined
  tibble(
    first_name = "Bill",
    last_name = "Flores",
    state_cd = "TX-17",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://flores.house.gov",
    campaign_name = "Bill Flores for Congress",
    campaign_website = "https://www.billflores.com/",
    actblue_link = NA,
    anedot_link = "floresforcongress/contribute",
    other_link = NA,
    FEC_ID_cmte = "C00472241",
    FEC_ID_cand = "H0TX17104"
  ),
  tibble(
    first_name = "Rick",
    last_name = "Kennedy",
    state_cd = "TX-17",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rick Kennedy for Congress",
    campaign_website = "https://www.rickkennedyforcongress.com/",
    actblue_link = "rk4tx17",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00654640",
    FEC_ID_cand = "H8TX17149"
  ),
  tibble(
    first_name = "Trent",
    last_name = "Sutton",
    state_cd = "TX-17",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://suttonforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Need customization. 2500-1700-170-50. Interesting choice...
    other_link = "https://suttonforcongress.com/donate-2/",
    FEC_ID_cmte = "C00721175",
    FEC_ID_cand = "H0TX17120"
  ),
  tibble(
    first_name = "Pete",
    last_name = "Sessions",
    state_cd = "TX-17",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Pete Sessions for Congress",
    # Still shows that he is running for 32nd district
    # So links will change pretty soon, I reckon
    campaign_website = "https://www.petesessions.com",
    actblue_link = NA,
    # Also very well hidden
    anedot_link =
      "pete-sessions-for-congress/general-fund-c53196c67a5c038e68fbf",
    other_link = NA,
    FEC_ID_cmte = "C00303305",
    FEC_ID_cand = "H2TX03126"
  ),
  # TX-18 (last updated Oct 3, 2020) -------------------------------------------
  tibble(
    first_name = "Sheila Jackson",
    last_name = "Lee",
    state_cd = "TX-18",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://jacksonlee.house.gov",
    campaign_name = "Sheila Jackson Lee",
    campaign_website = "https://sheilajacksonlee18.com",
    actblue_link = "sheila-jackson-lee-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00287904",
    FEC_ID_cand = "H4TX18054"
  ),
  tibble(
    first_name = "Wendell",
    last_name = "Champion",
    state_cd = "TX-18",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wendell Champion for Congress",
    campaign_website = "https://champion2020.com",
    actblue_link = NA,
    anedot_link = "champion-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00714618",
    FEC_ID_cand = "H0TX18292"
  ),
  tibble(
    first_name = "Robert",
    last_name = "Cadena",
    state_cd = "TX-18",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cadena for Congress",
    campaign_website = "https://www.cadenaforcongress.com",
    actblue_link = NA,
    anedot_link = "cadenaforcongress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00730309",
    FEC_ID_cand = "H0TX18334"
  ),
  # Missed Marc Flores: http://www.marcfloresforcongress.com/
  tibble(
    first_name = "Marc",
    last_name = "Flores",
    state_cd = "TX-18",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marc Flores for Congress",
    campaign_website = "www.marcfloresforcongress.com",
    # Although empty, 25-100-250-1000
    actblue_link = "marcflores",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00730309",
    FEC_ID_cand = "H0TX18334"
  ),
  tibble(
    first_name = "Vince",
    last_name = "Duncan",
    state_cd = "TX-18",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vince Duncan for Congress",
    campaign_website = "https://www.vinceforus.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.paypal.com/cgi-bin/we%20back?cmd=_s-xclick&hosted_button_id=639ZSBXNMTF5N",
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H4TX18112"
  ),
  tibble(
    first_name = "Luke",
    last_name = "Spencer",
    state_cd = "TX-18",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Luke Spencer for Congress",
    campaign_website = "https://www.lukespencerforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00682286",
    FEC_ID_cand = "H8TX18196"
  ),
  # ActBlue marcflores
  # TX-19 (last updated Jul 13, 2021) -------------------------------------------
  # Jodey Arrington was uncontested
  # (no candidates registered as of Sep 26, 2019)
  tibble(
    first_name = "Jodey",
    last_name = "Arrington",
    state_cd = "TX-19",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://arrington.house.gov",
    campaign_name = "Jodey Arrington Republican for Congress",
    campaign_website = "http://jodeyarrington.com/",
    actblue_link = NA,
    # Still very difficult to see in some browsers
    # 25-50-100-250-500-1000-2000-2800
    anedot_link =
      "texans-for-jodey-arrington/general-fund-c1dd2430920e07d0c7a04",
    other_link = NA,
    FEC_ID_cmte = "C00588657",
    FEC_ID_cand = "H6TX19099"
  ),
  tibble(
    first_name = "Tom",
    last_name = "Watson",
    state_cd = "TX-19",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tom Watson for Congress",
    campaign_website = "https://tomwatsonforuscongress.com",
    actblue_link = "tomwatson",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00731463",
    FEC_ID_cand = "H0TX19068"
  ),
  tibble(
    first_name = "Joe",
    last_name = "Burnes",
    state_cd = "TX-19",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joe Burnes for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0TX19100"
  ),
  # TX-20 (last updated Oct 3, 2020) -------------------------------------------
  tibble(
    first_name = "Joaquin",
    last_name = "Castro",
    state_cd = "TX-20",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://castro.house.gov",
    campaign_name = "Castro for Congress",
    campaign_website = "https://castroforcongress.com/",
    actblue_link = c(
      "jc4",
      "jc_julian20"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00497933",
    FEC_ID_cand = "H2TX35011"
  ),
  tibble(
    first_name = "Mauro",
    last_name = "Garza",
    state_cd = "TX-20",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mauro Garza for US Congress",
    campaign_website = "https://maurogarzaforcongress.com",
    actblue_link = NA,
    # 50-100-250-500-1000
    anedot_link = "mauro-garza/website-splash",
    other_link = NA,
    FEC_ID_cmte = "C00663435",
    FEC_ID_cand = "H8TX21331"
  ),
  tibble(
    first_name = "Dominick",
    last_name = "Dina",
    state_cd = "TX-20",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dominick for Congress",
    campaign_website = "http://dominickforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://secure.piryx.com/donate/I3B7rg7Q/",
      "Dominick-Dina-Election-Committee/TXCD20"
    ),
    FEC_ID_cmte = "C00716522",
    FEC_ID_cand = "H0TX20132"
  ),
  tibble(
    first_name = "Anita",
    last_name = "Kegley",
    state_cd = "TX-20",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kegley for Congress",
    campaign_website = "https://anitakegleyushouse.com/wp/",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = "https://www.gofundme.com/f/kegley-for-congress/donate",
    FEC_ID_cmte = "C00719575",
    FEC_ID_cand = "H0TX20140"
  ),
  tibble(
    first_name = "Joshua",
    last_name = "Carrizales",
    state_cd = "TX-20",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Carrizales for Congress",
    campaign_website = "https://carrizalesforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.piryx.com/donate/MKE7ZFLt/Carrizales-for-Congress/",
    FEC_ID_cmte = "C00720532",
    FEC_ID_cand = "H0TX20157"
  ),
  # # Cannot find Adam Jonasz
  tibble(
    first_name = "Justin",
    last_name = "Lecea",
    state_cd = "TX-20",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lecea for Congress",
    campaign_website = "https://www.justintx20.com",
    actblue_link = "leceaforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00714956",
    FEC_ID_cand = "H0TX20116"
  ),
  # # McQueen website is down at http://www.mcqueenforcongress.com
  tibble(
    first_name = "Bob", # Robert, goes by Bob
    last_name = "Hostetler",
    state_cd = "TX-20",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.hostetler4congress.org",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://politics.raisethemoney.com/rhostetlerL4R0mG3HEJqEob6-cfiKBQ",
    FEC_ID_cmte = "C00668111",
    FEC_ID_cand = "H8TX20143"
  ),
  # TX-21 (last updated Oct 3, 2020) -------------------------------------------
  # Roy vs. Davis vs. Leeder vs. Felts vs. Boville
  tibble(
    first_name = "Chip",
    last_name = "Roy",
    state_cd = "TX-21",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://roy.house.gov/",
    campaign_name = "Chip Roy for Congress",
    campaign_website = "https://chiproy.com/",
    actblue_link = NA,
    anedot_link = "chiproyforcongress/slug",
    other_link = NA,
    FEC_ID_cmte = "C00662767",
    FEC_ID_cand = "H8TX21307"
  ),
  tibble(
    first_name = "Wendy",
    last_name = "Davis",
    state_cd = "TX-21",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wendy Davis for Congress",
    campaign_website = "https://www.wendydavisforcongress.com/",
    actblue_link = "dav_website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00713297",
    FEC_ID_cand = "H0TX21148"
  ),
  tibble(
    first_name = "Arthur",
    last_name = "DiBianca",
    state_cd = "TX-21",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Arthur DiBianca for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0TX21155"
  ),
  tibble(
    first_name = "Thomas",
    last_name = "Wakely",
    state_cd = "TX-21",
    party = "Grn",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wakely 2020",
    campaign_website = "https://www.wakely2020.green/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00748459",
    FEC_ID_cand = "H6TX21228"
  ),
  # TX-22 (last updated Oct 3, 2020) -------------------------------------------
  # Olson is retiring; open seat
  # Wall vs. Kulkarni
  # vs. Bush vs. Hill vs. Harris vs. Nehls vs. Moore vs. Reed vs. Steele
  tibble(
    first_name = "Troy",
    last_name = "Nehls",
    state_cd = "TX-22",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nehls for Congress",
    campaign_website = "https://www.nehlsforcongress.com",
    actblue_link = NA,
    anedot_link = NA, # "nehls-for-congress/donate",
    # Change documented to WinRed Oct 3, 2020
    # no prompts ---> 25-50-100-250-500-1000-2800
    other_link = "https://secure.winred.com/troynehls/support",
    FEC_ID_cmte = "C00730150",
    FEC_ID_cand = "H0TX22302"
  ),
  tibble(
    first_name = "Sri Preston",
    last_name = "Kulkarni",
    state_cd = "TX-22",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sri Preston Kulkarni for Congress",
    campaign_website = "https://sri2020.com",
    actblue_link = "sri-site",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00662874",
    FEC_ID_cand = "H8TX22313"
  ),
  tibble(
    first_name = "Kathaleen",
    last_name = "Wall",
    state_cd = "TX-22",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kathaleen Wall For Congress",
    campaign_website = "https://kathaleenwall.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/KathaleenWall/donate",
    FEC_ID_cmte = "C00716845",
    FEC_ID_cand = "H8TX02141"
  ),
  # Announced retirement on Jul 25, 2019
  tibble(
    first_name = "Pete", # Peter G., goes by Pete
    last_name = "Olson",
    state_cd = "TX-22",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://olson.house.gov",
    campaign_name = "",
    campaign_website = "https://www.olsonforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00437913",
    FEC_ID_cand = "H8TX22107"
  ),
  tibble(
    first_name = "Pierce",
    last_name = "Bush",
    state_cd = "TX-22",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Pierce Bush for Congress",
    campaign_website = "https://www.piercebushforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Wow, $16,800 option for couple, primary, run-off and general election
    other_link = "https://secure.winred.com/piercebush/donate/",
    FEC_ID_cmte = "C00730168",
    FEC_ID_cand = "H0TX22310"
  ),
  tibble(
    first_name = "Greg",
    last_name = "Hill",
    state_cd = "TX-22",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Greg Hill for Congress",
    campaign_website = "https://www.electgreghill.com",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-1000-2800
    other_link = "https://electgreghill.ichooseapex.com/donate/",
    FEC_ID_cmte = "C00715052",
    FEC_ID_cand = "H0TX22187"
  ),
  tibble(
    first_name = "Felicia",
    last_name = "Harris Hoss",
    state_cd = "TX-22",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Felicia Harris Hoss for Congress",
    campaign_website = "https://www.feliciafortx.com",
    actblue_link = NA,
    anedot_link = "felicia-harris-hoss-for-congress/website-donate",
    other_link = NA,
    FEC_ID_cmte = "C00502310",
    FEC_ID_cand = "H2TX14123"
  ),
  tibble(
    first_name = "Nyanza Davis",
    last_name = "Moore",
    state_cd = "TX-22",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nyanza Moore for Congress",
    campaign_website = "https://www.nyanzadavismoore.com",
    actblue_link = "NDM2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00694935",
    FEC_ID_cand = "H0TX22161"
  ),
  tibble(
    first_name = "Derrick",
    last_name = "Reed",
    state_cd = "TX-22",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Derrick Reed for U.S. Congress",
    campaign_website = "https://derrickreed.com",
    actblue_link = "reedwebsite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00712273",
    FEC_ID_cand = "H0TX22179"
  ),
  tibble(
    first_name = "Howard",
    last_name = "Steele",
    state_cd = "TX-22",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Howard Steele For Congress",
    campaign_website = "https://www.howardsteele.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/HowardSteele/keeptexasgreat",
    FEC_ID_cmte = "C00717215",
    FEC_ID_cand = "H0TX22211"
  ),
  # TX-23 (last updated Jul 13, 2021) -------------------------------------------
  # I realize that Hurd announced to retire but his page is live so will scrape
  tibble(
    first_name = "Gina Ortiz",
    last_name = "Jones",
    state_cd = "TX-23",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gina Ortiz Jones for Congress",
    campaign_website = "https://ginaortizjones.com",
    actblue_link = "ginaortizjones",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00652297",
    FEC_ID_cand = "H8TX23121"
  ),
  tibble(
    first_name = "Tony", # Ernest Anthony, goes by Tony
    last_name = "Gonzales",
    state_cd = "TX-23",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tony Gonzales for Congress",
    campaign_website = "https://tonygonzalesforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/tony-gonzales/donate",
    FEC_ID_cmte = c("C00706614", "C00717785"),
    FEC_ID_cand = "H0TX35015"
  ),
  # Cannot find Waterman
  tibble(
    first_name = "Will",
    last_name = "Hurd",
    state_cd = "TX-23",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://hurd.house.gov",
    campaign_name = "Hurd for Congress",
    campaign_website = "https://www.hurdforcongress.com",
    actblue_link = NA,
    anedot_link = "hurdforcongress/web_donate",
    other_link = NA,
    FEC_ID_cmte = "C00545467",
    FEC_ID_cand = "H0TX23086"
  ),
  tibble(
    first_name = "Alma",
    last_name = "Arredondo-Lynch",
    state_cd = "TX-23",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dr. Alma for Congress",
    campaign_website = "https://www.dralmaforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://dralmaforcongress.revv.co/donate-today",
    FEC_ID_cmte = "C00699942",
    FEC_ID_cand = "H8TX23113"
  ),
  tibble(
    first_name = "Raul",
    last_name = "Reyes",
    state_cd = "TX-23",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Raul Reyes for Congress",
    campaign_website = "https://raulreyesforcongress.com",
    actblue_link = NA,
    anedot_link = "raul-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00705079",
    FEC_ID_cand = "H0TX23177"
  ),
  tibble(
    first_name = "Liz",
    last_name = "Wahl",
    state_cd = "TX-23",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Liz Wahl for Congress",
    campaign_website = "https://lizwahl.com",
    actblue_link = "liz-wahl-for-tx-23-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00693135",
    FEC_ID_cand = "H0TX23144"
  ),
  tibble(
    first_name = "Rosey",
    last_name = "Ramos Abuabara", # Goes simply as Abuabara
    state_cd = "TX-23",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rosey for Texas 23",
    campaign_website = "http://roseyfortx23.com",
    actblue_link = "roseyfortx23",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00709071",
    FEC_ID_cand = "H0TX23193"
  ),
  tibble(
    first_name = "Ben",
    last_name = "Van Winkle",
    state_cd = "TX-23",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ben Van Winkle for Congress",
    campaign_website = "https://vanwinkleforcongress.com",
    actblue_link = NA,
    anedot_link = "ben-van-winkle-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00716894",
    FEC_ID_cand = "H0TX23201"
  ),
  tibble(
    first_name = "Jaime",
    last_name = "Escuder",
    state_cd = "TX-23",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Escuder for Congress",
    campaign_website = "https://www.escuderforcongress.com",
    actblue_link = "escuderforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00717165",
    FEC_ID_cand = "H0TX23219"
  ),
  tibble(
    first_name = "Cecil Burt",
    last_name = "Jones",
    state_cd = "TX-23",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cecil B. Burt Jones for Congress",
    campaign_website = paste0(
      "https://www.gofundme.com/f/",
      "cecil-b-quotburtquot-jones-for-the-us-congress"
    ),
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://www.gofundme.com/f/",
      "cecil-b-quotburtquot-jones-for-the-us-congress"
    ),
    FEC_ID_cmte = "C00719344",
    FEC_ID_cand = "H0TX23227"
  ),
  tibble(
    first_name = "Beto",
    last_name = "Villela",
    state_cd = "TX-23",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Beto Villela for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0TX23300"
  ),
  # TX-24 (last updated Oct 3, 2020) -------------------------------------------
  # Open seat; Marchant retires
  tibble(
    first_name = "Candace",
    last_name = "Valenzuela",
    state_cd = "TX-24",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Candace for Congress",
    campaign_website = "https://candacefor24.com",
    actblue_link = "cd24",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00702225",
    FEC_ID_cand = "H0TX24175"
  ),
  tibble(
    first_name = "Beth",
    last_name = "Van Duyne",
    state_cd = "TX-24",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Beth Van Duyne for Congress",
    campaign_website = "https://www.bethfortexas.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/beth-van-duyne-for-congress/support-today/",
    FEC_ID_cmte = "C00714865",
    FEC_ID_cand = "H0TX24209"
  ),
  tibble(
    first_name = "Darren",
    last_name = "Hamilton",
    state_cd = "TX-24",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Darren Hamilton for Congress",
    campaign_website = "https://www.votehamiltonforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 10-15-25-50-100-250-500-1000-1500
    other_link = "https://www.votehamiltonforcongress.com/contribute",
    FEC_ID_cmte = "C00742700",
    FEC_ID_cand = "H0TX24274"
  ),
  tibble(
    first_name = "Steve", # Stephen, goes by Steve
    last_name = "Kuzmich",
    state_cd = "TX-24",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kuzmich for Congress",
    campaign_website = "https://www.kuzmichforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 50-100-225-1000-2800-5600
    other_link = paste0(
      "https://www.kuzmichforcongress.com/donations/",
      "donate-to-kuzmich-for-congress/"
    ),
    FEC_ID_cmte = "C00691758",
    FEC_ID_cand = "H0TX24142"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Bauer",
    state_cd = "TX-24",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Bauer for Congress",
    campaign_website = "https://bauerfortexas.com",
    actblue_link = NA,
    anedot_link = NA,
    # 5-25-50-80-100-custom
    other_link = "https://bauerfortexas.com/donate/",
    FEC_ID_cmte = "C00731570",
    FEC_ID_cand = "H0TX24266"
  ),
  tibble(
    first_name = "Kenny",
    last_name = "Marchant",
    state_cd = "TX-24",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://marchant.house.gov",
    campaign_name = "Kenny Marchant for Congress",
    campaign_website = "http://www.kennymarchant.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://cm.aristotle.com/d/marchantdonation",
    FEC_ID_cmte = "C00393348",
    FEC_ID_cand = "H4TX24094"
  ),
  tibble(
    first_name = "Kim",
    last_name = "Olson",
    state_cd = "TX-24",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kim Olson for Congress",
    campaign_website = "https://kimforcongress.org",
    actblue_link = c(
      "kim4congress",
      "colonelmarveleoq"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00701706",
    FEC_ID_cand = "H0TX24167"
  ),
  tibble(
    first_name = "Sunny", # Suneetha, goes by Sunny
    last_name = "Chaparala",
    state_cd = "TX-24",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sunny Chaparala for Congress",
    campaign_website = "https://sunnyforcongress.com",
    actblue_link = NA,
    anedot_link = "sunny-chaparala-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00717470",
    FEC_ID_cand = "H0TX24217"
  ),
  #  Crystal Fletcher dropped out Jan 14, 2020
  tibble(
    first_name = "Desi",
    last_name = "Maes",
    state_cd = "TX-24",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Desi for Congress",
    campaign_website = "http://www.desiforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/desi-maes-for-congress/donate",
    FEC_ID_cmte = "C00722504",
    FEC_ID_cand = "H0TX24233"
  ),
  tibble(
    first_name = "David",
    last_name = "Fegan",
    state_cd = "TX-24",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.feganforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # What is this source? 5-25-100-1000-2800-5600
    other_link = "https://www.feganforcongress.com/contribute",
    FEC_ID_cmte = "C00714444",
    FEC_ID_cand = "H0TX24191"
  ),
  tibble(
    first_name = "Jan",
    last_name = "McDowell",
    state_cd = "TX-24",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jan McDowell for Congress",
    campaign_website = "https://www.janmcdowell.com",
    actblue_link = "jan-mcdowell-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00588558",
    FEC_ID_cand = "H6TX24149"
  ),
  tibble(
    first_name = "John",
    last_name = "Biggan",
    state_cd = "TX-24",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Biggan for Congress",
    campaign_website = "https://www.bigganforcongress.com",
    actblue_link = "biggan-contribute",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00700344",
    FEC_ID_cand = "H8TX24111"
  ),
  # Deanna Metzger withdrew
  tibble(
    first_name = "Richard",
    last_name = "Fleming",
    state_cd = "TX-24",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Richard Fleming For Congress",
    campaign_website = "https://fleming4congress.com",
    actblue_link = "fleming4congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00708305",
    FEC_ID_cand = "H0TX24183"
  ),
  tibble(
    first_name = "Jeron",
    last_name = "Liverman",
    state_cd = "TX-24",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jeron Liverman for Congress",
    campaign_website = "http://jeronliverman.vote",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/JeronLiverman/donate",
    FEC_ID_cmte = "C00719674",
    FEC_ID_cand = "H0TX24225"
  ),
  tibble(
    first_name = "Sam", # Samuel Jairo, goes by Sam
    last_name = "Vega",
    state_cd = "TX-24",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sam Vega for Congress",
    campaign_website = "https://samvega2020.com",
    actblue_link = "vega2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00723692",
    FEC_ID_cand = "H0TX24258"
  ),
  # TX-25 (last updated Jul 13, 2021) -------------------------------------------
  tibble(
    first_name = "Roger",
    last_name = "Williams",
    state_cd = "TX-25",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://williams.house.gov",
    campaign_name = "Roger Williams for Congress",
    campaign_website = "https://www.rogerforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/roger-williams-for-us-congress/donate",
    FEC_ID_cmte = "C00498121",
    FEC_ID_cand = "H2TX33040"
  ),
  tibble(
    first_name = "Julie",
    last_name = "Oliver",
    state_cd = "TX-25",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Julie Oliver for Congress",
    campaign_website = "https://www.julieoliver.org",
    actblue_link = "julie-homepage",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00700716", "C00661769"),
    FEC_ID_cand = "H8TX25159"
  ),
  tibble(
    first_name = "Heidi",
    last_name = "Sloan",
    state_cd = "TX-25",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Heidi Sloan for Congress",
    campaign_website = "https://heidisloan.com",
    # Lost it, but 10-25-50-100-250
    actblue_link = "heidi-sloan-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00714253",
    FEC_ID_cand = "H0TX25131"
  ),
  tibble(
    first_name = "Bill",
    last_name = "Kelsey",
    state_cd = "TX-25",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bill Kelsey for Congress",
    campaign_website = "http://kelseyforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H4TX10184"
  ),
  # TX-26 (last updated Jul 13, 2021) -------------------------------------------
  tibble(
    first_name = "Michael",
    last_name = "Burgess",
    state_cd = "TX-26",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://burgess.house.gov",
    campaign_name = "Michael Burgess for Congress",
    campaign_website = "https://burgessforcongress.org",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/michaelburgess/donate",
    FEC_ID_cmte = "C00372532",
    FEC_ID_cand = "H2TX26093"
  ),
  tibble(
    first_name = "Carol",
    last_name = "Iannuzzi",
    state_cd = "TX-26",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Carol H Iannuzzi for U.S. House TX-26",
    campaign_website = "https://carol2020.com",
    actblue_link = "carol_alloptions",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00694307",
    FEC_ID_cand = "H0TX26121"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Boler",
    state_cd = "TX-26",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Boler for Congress",
    campaign_website = "https://www.markboler4congress.org/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0TX26105"
  ),
  # Missed Jack Wyman: http://jackwymanforcongress.com
  # Unfortunately, contribution page not archived
  # TX-27 (last updated Oct 3, 2020) -------------------------------------------
  # Michael Cloud was uncontested (no candidates registered as of Sep 26, 2019)
  # Now Cloud vs. De La Fuente
  tibble(
    first_name = "Michael",
    last_name = "Cloud",
    state_cd = "TX-27",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://cloud.house.gov",
    campaign_name = "Cloud for Congress",
    campaign_website = "https://www.cloudforcongress.com/",
    actblue_link = NA,
    anedot_link = "cloudforcongress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00655332",
    FEC_ID_cand = "H8TX27049"
  ),
  tibble(
    first_name = "Ricardo",
    last_name = "De La Fuente",
    state_cd = "TX-27",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ricardo De La Fuente for US Congress",
    # No donation links on website
    campaign_website = "https://ricardoforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00730838",
    FEC_ID_cand = "H0TX27087"
  ),
  # Not on FEC
  tibble(
    first_name = "Phil",
    last_name = "Gray",
    state_cd = "TX-27",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Phil Gray for United States Congress",
    campaign_website = "https://www.philgray.org",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = paste0(
      "https://www.paypal.com/donate?",
      "token=AfgIXvKKxAaC7AmK3hf2OW6P4_3nZIbiHIRwIRdqga6xpNeCXE5dhx-",
      "0zQ54FrPp03o_IG36meD-2hoP"
    ),
    FEC_ID_cmte = "", # Not available
    FEC_ID_cand = "H6TX01261"
  ),
  # TX-28 (last updated Jul 13, 2021) -------------------------------------------
  # Cuellar vs. Cisneros vs. Whitten
  # DCCC-backed Cuellar, progressive-backed Cisneros (e.g. AOC)
  tibble(
    first_name = "Henry",
    last_name = "Cuellar",
    state_cd = "TX-28",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://cuellar.house.gov",
    campaign_name = "Texans for Henry Cuellar",
    campaign_website = "https://www.henrycuellar.com",
    # Change documented from NGP VAN to ActBlue
    actblue_link = "erc-200821-website",
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/NoScript/-7671037540295507968",
    FEC_ID_cmte = "C00371302",
    FEC_ID_cand = "H2TX23082"
  ),
  tibble(
    first_name = "Jessica",
    last_name = "Cisneros",
    state_cd = "TX-28",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.jessicacisnerosforcongress.com",
    actblue_link = c(
      "cisneros-web",
      "ads-cisneros-gs-dtd",
      "aoc-jessica-email-2020213-1",
      "julian-castro-jessica-cisneros"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00709006",
    FEC_ID_cand = "H0TX28077"
  ),
  tibble(
    first_name = "Sandra",
    last_name = "Whitten",
    state_cd = "TX-28",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sandra for Texas",
    campaign_website = "https://sandrafortexas.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/SandraWhitten/restoring_the_heart",
    FEC_ID_cmte = "C00700823",
    FEC_ID_cand = "H0TX28069"
  ),
  tibble(
    first_name = "Bekah",
    last_name = "Congdon",
    state_cd = "TX-28",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bekah Congdon for Congress",
    campaign_website = "https://www.facebook.com/LibertyBeks/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0TX28085"
  ),
  # TX-29 (last updated Jul 13, 2021) -------------------------------------------
  tibble(
    first_name = "Sylvia",
    last_name = "Garcia",
    state_cd = "TX-29",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://sylviagarcia.house.gov",
    campaign_name = "Sylvia Garcia for Congress",
    campaign_website = "http://sylviaforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 20-35-50-100-500-2500
    other_link = "https://secure.ngpvan.com/HRSGvJW1HECFcWPtvXrHfQ2",
    FEC_ID_cmte = "C00660555",
    FEC_ID_cand = "H8TX29052"
  ),
  tibble(
    first_name = "Jaimy",
    last_name = "Blanco",
    state_cd = "TX-29",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jaimy Blanco for Congress",
    campaign_website = "https://jaimyblancoforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/jaimyblanco/donate",
    FEC_ID_cmte = "C00664581",
    FEC_ID_cand = "H8TX29110"
  ),
  tibble(
    first_name = "Phil",
    last_name = "Kurtz",
    state_cd = "TX-29",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Phil Kurtz for Congress",
    campaign_website = "https://www.philkurtzforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H8TX09153"
  ),
  # TX-30 (last updated Jul 13, 2021) -------------------------------------------
  # Johnson vs. Pennie
  tibble(
    first_name = "Eddie Bernice",
    last_name = "Johnson",
    state_cd = "TX-30",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://ebjohnson.house.gov",
    # Uh... so the website seems genuine but...
    campaign_name = "Eddie Bernice Johnson for Congress",
    campaign_website = "http://www.ebjcampaign.com/",
    actblue_link = NA,
    anedot_link = NA,
    # No reference points. Could be a good baseline... actually! No!
    # It's hidden: 25-50-100-200-500-1500
    other_link = paste0(
      "https://www.campaigncontribution.com/payment/contribution/",
      "info/29f7be14-f873-419c-bf56-7f5668bc41ff"
    ),
    FEC_ID_cmte = "C00254573",
    FEC_ID_cand = "H2TX00015"
  ),
  tibble(
    first_name = "Tre",
    last_name = "Pennie",
    state_cd = "TX-30",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tre Pennie for Congress",
    campaign_website = "https://www.pennieforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/embed/tre-pennie-for-congress",
    FEC_ID_cmte = "C00707059",
    FEC_ID_cand = "H0TX32053"
  ),
  tibble(
    first_name = "Eric",
    last_name = "Williams",
    state_cd = "TX-30",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Eric Williams 2020",
    campaign_website = "https://www.ericwilliams2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H8TX30043"
  ),
  # TX-31 (last updated Jul 13, 2021) -------------------------------------------
  tibble(
    first_name = "John",
    last_name = "Carter",
    state_cd = "TX-31",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://carter.house.gov",
    campaign_name = "John Carter for Congress",
    campaign_website = "https://www.johncarterforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/john-carter/donate",
    FEC_ID_cmte = "C00371203",
    FEC_ID_cand = "H2TX31044"
  ),
  tibble(
    first_name = "Donna",
    last_name = "Imam",
    state_cd = "TX-31",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vote Donna US Congress",
    campaign_website = "https://votefordonna.com",
    actblue_link = "donnaimamtx",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710475",
    FEC_ID_cand = "H0TX31097"
  ),
  tibble(
    first_name = "Christine Eady",
    last_name = "Mann",
    state_cd = "TX-31",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Christine Mann for Congress",
    campaign_website = "https://www.christine4congress.com",
    actblue_link = "dr--christine-eady-mann-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00706754", "C00638346"),
    FEC_ID_cand = "H8TX31033"
  ),
  tibble(
    first_name = "Clark",
    last_name = "Patterson",
    state_cd = "TX-31",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Clark Patterson for Congress",
    campaign_website = "http://www.clarkpforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H2TX01070"
  ),
  # TX-32 (last updated Jul 13, 2021) -------------------------------------------
  tibble(
    first_name = "Colin",
    last_name = "Allred",
    state_cd = "TX-32",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://allred.house.gov",
    campaign_name = "Colin Allred for Congress",
    campaign_website = "https://www.colinallred.com/",
    actblue_link = c(
      "websitewx",
      "ms_ecu_fr_2020_allred"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00637868",
    FEC_ID_cand = "H8TX32098"
  ),
  tibble(
    first_name = "Genevieve",
    last_name = "Collins",
    state_cd = "TX-32",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Genevieve Collins for Congress",
    campaign_website = "https://www.gcforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/gcforcongress/donate",
    FEC_ID_cmte = c("C00715235", "C00702894"),
    FEC_ID_cand = "H0TX32087"
  ),
  tibble(
    first_name = "Jason",
    last_name = "Sigmon",
    state_cd = "TX-32",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jason Sigmon for Congress",
    campaign_website = "https://jasonsigmon.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=UKFUT3MQPQYBA&source=url",
    FEC_ID_cmte = "C00733980",
    FEC_ID_cand = "H0TX32111"
  ),
  # TX-33 (last updated Jul 13, 2021) -------------------------------------------
  tibble(
    first_name = "Marc",
    last_name = "Veasey",
    state_cd = "TX-33",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://veasey.house.gov",
    campaign_name = "Marc Veasey for Congress",
    campaign_website = "https://marcveasey.com",
    actblue_link = "marcveasey2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00506832",
    FEC_ID_cand = "H2TX33073"
  ),
  tibble(
    first_name = "Fabian",
    last_name = "Vasquez",
    state_cd = "TX-33",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Fabian Cordova Vasquez for U.S. Congress",
    campaign_website = "https://www.fcv2020.com/blog/",
    actblue_link = NA,
    anedot_link = "vasquez-campaign-committee/donate",
    other_link = NA,
    FEC_ID_cmte = "C00700294",
    FEC_ID_cand = "H0TX33010"
  ),
  tibble(
    first_name = "Carlos",
    last_name = "Quintanilla",
    state_cd = "TX-33",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Carlos Quintanilla for Congress",
    campaign_website = "http://quintanilla2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00663369",
    FEC_ID_cand = "H2TX06186"
  ),
  tibble(
    first_name = "Jason",
    last_name = "Reeves",
    state_cd = "TX-33",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jason Reeves for Congress",
    campaign_website = "https://www.facebook.com/Jason-Reeves-Libertarian-CD-33-TX-110529837455613/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H4TX33020"
  ),
  # TX-34 (last updated Jul 13, 2021) -------------------------------------------
  # Filemon Vela was uncontested (no candidates registered as of Sep 26, 2019)
  tibble(
    first_name = "Filemon",
    last_name = "Vela",
    state_cd = "TX-34",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://vela.house.gov",
    campaign_name = "Filemon Vela for Congress",
    campaign_website = "http://www.filemonvelaforcongress.com/",
    actblue_link = "velacontribution",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00513531",
    FEC_ID_cand = "H2TX27190"
  ),
  tibble(
    first_name = "Rey", # Reynaldo, goes by Rey
    last_name = "Gonzalez",
    state_cd = "TX-34",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rey Gonzalez for Congress",
    campaign_website = "https://reygonzalez.org",
    # 25-50-100-250-500-1000-1500-2000-2700
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://reygonzalez.org/donate-today.html",
    FEC_ID_cmte = "C00607333",
    FEC_ID_cand = "H0TX34042"
  ),
  tibble(
    first_name = "Anthony",
    last_name = "Cristo",
    state_cd = "TX-34",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cristo for Congress",
    campaign_website = "https://www.facebook.com/CristoForCongress2018/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H8TX15069"
  ),
  # TX-35 (last updated Oct 3, 2020) -------------------------------------------
  # Lloyd Doggett was uncontested (no candidates registered as of Sep 26, 2019)
  # Now Doggett vs. Moutos vs. Daly vs. Hayward vs. Mata vs. Sharon
  tibble(
    first_name = "Lloyd",
    last_name = "Doggett",
    state_cd = "TX-35",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://doggett.house.gov",
    campaign_name = "Lloyd Doggett for Congress",
    campaign_website = "https://www.votedoggett.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/-6306745251476207616",
    FEC_ID_cmte = "C00286500",
    FEC_ID_cand = "H4TX10028"
  ),
  tibble(
    first_name = "Jenny Garcia",
    last_name = "Sharon",
    state_cd = "TX-35",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jenny Garcia Sharon for Congress",
    campaign_website = "https://jennygarciasharon.com",
    actblue_link = NA,
    anedot_link = "jenny-garcia-sharon-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00740068",
    FEC_ID_cand = "H0TX35064"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Loewe",
    state_cd = "TX-35",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Loewe for Congress",
    campaign_website = "http://www.markloewe.org/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H4TX21082"
  ),
  # TX-36 (last updated Jul 13, 2021) -------------------------------------------
  tibble(
    first_name = "Brian",
    last_name = "Babin",
    state_cd = "TX-36",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://babin.house.gov",
    campaign_name = "Babin for Congress",
    campaign_website = "https://www.babinforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/dr-brian-babin-for-congress/donate",
    FEC_ID_cmte = "C00553859",
    FEC_ID_cand = "H6TX02079"
  ),
  tibble(
    first_name = "Rashad",
    last_name = "Lewis",
    state_cd = "TX-36",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lewis for Congress",
    campaign_website = "https://www.lewis4congress.com",
    actblue_link = "lewis-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00725358",
    FEC_ID_cand = "H0TX36021"
  ),
  tibble(
    first_name = "RJ",
    last_name = "Boatman",
    state_cd = "TX-36",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Boatman for Congress",
    campaign_website = "https://boatmanforcongress.org",
    actblue_link = NA,
    anedot_link = "boatman-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00711440",
    FEC_ID_cand = "H0TX36013"
  ),
  tibble(
    first_name = "Chad",
    last_name = "Abbey",
    state_cd = "TX-36",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chad Abbey for Congress",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H0TX36039"
  ),
  # Utah (last updated Oct 3, 2020) ============================================
  # UT-01 (last updated Oct 3, 2020) -------------------------------------------
  # Bishop announced retirement on Jul 29, 2019; Open seat
  # Witt vs. Canon
  # vs. Shepherd vs. Cheek vs. Wallack vs. Stevenson vs. Gibson
  # vs. Durbano vs. Liggera vs. Moore
  tibble(
    first_name = "Blake",
    last_name = "Moore",
    state_cd = "UT-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Blake Moore for Congress",
    campaign_website = "https://electmoore.com",
    actblue_link = NA,
    # No prompts
    anedot_link = "blake-moore-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00738872",
    FEC_ID_cand = "H0UT01205"
  ),
  tibble(
    first_name = "Darren",
    last_name = "Parry",
    state_cd = "UT-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Darren Parry for Congress",
    campaign_website = "https://darrenforutah.com",
    actblue_link = "darrenparry",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00740456",
    FEC_ID_cand = "H0UT01213"
  ),
  tibble(
    first_name = "Katie",
    last_name = "Witt",
    state_cd = "UT-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Katie Witt for Congress",
    campaign_website = "https://www.katiewitt.com",
    actblue_link = NA,
    anedot_link = "katie-witt-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00717637",
    FEC_ID_cand = "H0UT01130"
  ),
  tibble(
    first_name = "Tina",
    last_name = "Cannon",
    state_cd = "UT-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tina Cannon for Congress",
    campaign_website = "https://www.votetinacannon.com",
    actblue_link = NA,
    anedot_link = "tina-cannon-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00715086",
    FEC_ID_cand = "H0UT01114"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Shepard",
    state_cd = "UT-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Shepherd for Congress",
    campaign_website = "https://shepherdforutah.com",
    actblue_link = NA,
    anedot_link = NA,
    # On WinRed Twitter starting Jan 24, 2020
    other_link = "https://secure.winred.com/mark-shepherd-for-congress/donate",
    FEC_ID_cmte = "C00722389",
    FEC_ID_cand = "H0UT01155"
  ),
  tibble(
    first_name = "Jamie",
    last_name = "Cheek",
    state_cd = "UT-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jamie Cheek for Utah",
    campaign_website = "https://www.cheekforutah.com",
    actblue_link = "jamiecheekforutah",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00720276",
    FEC_ID_cand = "H0UT01148"
  ),
  tibble(
    first_name = "Howard",
    last_name = "Wallack",
    state_cd = "UT-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wallack for Congress",
    campaign_website = "https://howardwallack.com",
    actblue_link = NA,
    anedot_link = "wallack-for-congress/donate",
    # 25-50-100-250-500-1000-2000-2800
    other_link = "https://howardwallack.com/donate/",
    FEC_ID_cmte = c("C00731729", "C00505735"),
    FEC_ID_cand = "H2UT02316"
  ),
  tibble(
    first_name = "Bob",
    last_name = "Stevenson",
    state_cd = "UT-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Stevenson 2020",
    campaign_website = "https://stevenson2020.com",
    actblue_link = NA,
    anedot_link = "stevenson-2020/donate",
    other_link = NA,
    FEC_ID_cmte = "C00724260",
    FEC_ID_cand = "H0UT01163"
  ),
  tibble(
    first_name = "Kerry",
    last_name = "Gibson",
    state_cd = "UT-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kerry Gibson for Congress",
    campaign_website = "https://www.gibsonforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/kerrygibson/donate",
    FEC_ID_cmte = "C00734640",
    FEC_ID_cand = "H0UT01171"
  ),
  tibble(
    first_name = "Doug", # Douglas, goes by Doug
    last_name = "Durbano",
    state_cd = "UT-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Durbano for Utah",
    campaign_website = "https://www.durbanoforutah.com",
    actblue_link = NA,
    anedot_link = "durbano-for-utah/donate",
    other_link = NA,
    FEC_ID_cmte = "C00735944",
    FEC_ID_cand = "H0UT01189"
  ),
  tibble(
    first_name = "David",
    last_name = "Liggera",
    state_cd = "UT-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Liggera for Congress",
    campaign_website = "https://www.dliggeraforcongress.com",
    actblue_link = "dliggera",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00738492",
    FEC_ID_cand = "H0UT01197"
  ),
  # UT-02 (last updated Oct 3, 2020) -------------------------------------------
  tibble(
    first_name = "Chris",
    last_name = "Stewart",
    state_cd = "UT-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://stewart.house.gov",
    campaign_name = "Chris Stewart for Utah",
    campaign_website = "https://www.stewartforutah.com",
    actblue_link = NA,
    anedot_link = "friends-for-chris-stewart/donate",
    other_link = NA,
    FEC_ID_cmte = "C00506931",
    FEC_ID_cand = "H2UT02324"
  ),
  tibble(
    first_name = "Kael", # John Kael, goes by Kael
    last_name = "Weston",
    state_cd = "UT-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Weston 2020",
    campaign_website = "https://www.westonforcongress.com",
    actblue_link = "weston-2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00730820",
    FEC_ID_cand = "H0UT02302"
  ),
  # Lost Burkett, but archived:
  # https://burkett4utah.revv.co/donate 5-25-50-100-200-250-500-1000-2800
  # UT-03 (last updated Oct 3, 2020) -------------------------------------------
  tibble(
    first_name = "John",
    last_name = "Curtis",
    state_cd = "UT-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://curtis.house.gov",
    campaign_name = "Curtis for Congress",
    campaign_website = "https://johncurtis.org",
    actblue_link = NA,
    anedot_link = NA,
    # Revv: 25-50-100-150-200-250-2800-5600
    other_link = "https://secure.johncurtis.org/contribute",
    FEC_ID_cmte = "C00647339",
    FEC_ID_cand = "H8UT03238"
  ),
  tibble(
    first_name = "Tim",
    last_name = "Aalders", # Name spelled wrong on FEC website, damn
    state_cd = "UT-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tim Aalders for Congress",
    campaign_website = "https://timaalders.com",
    actblue_link = NA,
    anedot_link = "timaaldersforcongress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00742866",
    FEC_ID_cand = "H0UT03227"
  ),
  tibble(
    first_name = "Devin",
    last_name = "Thorpe",
    state_cd = "UT-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Devin Thorpe for Congress",
    campaign_website = "https://www.devinthorpe.com",
    actblue_link = NA,
    anedot_link = NA,
    # Website I have never seen so far. FundHero? 35-75-125-250
    other_link = "https://donate.fundhero.io/devinthorpe",
    FEC_ID_cmte = "C00740464",
    FEC_ID_cand = "H0UT03193"
  ),
  # UT-04 (last updated Oct 3, 2020) -------------------------------------------
  # Contested race picked by WaPo. Mia Love may enter race.
  tibble(
    first_name = "Ben",
    last_name = "McAdams",
    state_cd = "UT-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://mcadams.house.gov",
    campaign_name = "Ben McAdams for Congress",
    campaign_website = "https://www.benmcadams.com",
    actblue_link = "support-mcadams",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00658633",
    FEC_ID_cand = "H8UT04053"
  ),
  tibble(
    first_name = "Burgess",
    last_name = "Owens",
    state_cd = "UT-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Burgess Owens for Congress",
    campaign_website = "https://www.burgess4utah.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/BurgessOwens/donate",
    FEC_ID_cmte = "C00725853",
    FEC_ID_cand = "H0UT04076"
  ),
  tibble(
    first_name = "Kim",
    last_name = "Coleman",
    state_cd = "UT-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kim for Utah",
    campaign_website = "www.kimforutah.com",
    actblue_link = NA,
    anedot_link = NA,
    # NationBuilder
    other_link = "https://www.kimforutah.com/donate",
    FEC_ID_cmte = "C00715425",
    FEC_ID_cand = "H0UT04043"
  ),
  # Dropped out Dec 16, 2019
  tibble(
    first_name = "Dan",
    last_name = "Hemmert",
    state_cd = "UT-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dan Hemmert for Congress",
    campaign_website = "https://danhemmert.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Originally https://hemmertforcongress.revv.co/donate
    # Revv was incorporated into WinRed, really
    other_link = "https://secure.winred.com/hemmertforcongress/donate",
    FEC_ID_cmte = "C00716589",
    FEC_ID_cand = "H0UT04050"
  ),
  tibble(
    first_name = "Kathleen",
    last_name = "Anderson",
    state_cd = "UT-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kathleen for Utah",
    campaign_website = "https://www.kathleenforutah.com/",
    actblue_link = NA,
    # Change documented to WinRed on Feb 12, 2020
    # At least from Feb 11, 2020
    # 10-25-50-100-250-500-1000-2800 to 2800-1000-500-250-100-50-25
    anedot_link = "kathleenforutah/contribute",
    other_link = NA,
    FEC_ID_cmte = "C00710970",
    FEC_ID_cand = "H0UT04019"
  ),
  tibble(
    first_name = "Jay",
    last_name = "Mcfarland",
    state_cd = "UT-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "JayMac Mcfarland for Congress",
    campaign_website = "https://www.jaymacforutah.com/",
    actblue_link = NA,
    anedot_link = NA,
    # NationBuilder
    other_link = "https://www.jaymacforutah.com/donate",
    FEC_ID_cmte = "C00714311",
    FEC_ID_cand = "H0UT04035"
  ),
  tibble(
    first_name = "Daniel",
    last_name = "Beckstrand",
    state_cd = "UT-04",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Daniel Beckstrand for Congress",
    campaign_website = "https://www.danielbeckstrand.com/",
    actblue_link = "daniel-beckstrand-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00730010",
    FEC_ID_cand = "H0UT04084"
  ),
  tibble(
    first_name = "Jonia",
    last_name = "Broderick",
    state_cd = "UT-04",
    # United Utah Party
    party = "Oth",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jonia for Congress",
    campaign_website = "https://www.teamjonia.com/",
    actblue_link = NA,
    anedot_link = NA,
    # NationBuilder
    # other_link = "https://www.teamjonia.com/donate",
    # Was this always Donorbox?
    # 10-25-50-100-500-1000 + $40 donation for a tote bag filled with everything
    other_link = "https://donorbox.org/embed/jonia-broderick-for-congress",
    FEC_ID_cmte = "C00713875",
    FEC_ID_cand = "H0UT04027"
  ),
  # Not on FEC
  tibble(
    first_name = "John",
    last_name = "Molnar",
    state_cd = "UT-04",
    party = "Lib",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Molnar 4 Utah",
    campaign_website = "https://molnar4utah.home.blog",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/molnar-2020-donate",
    FEC_ID_cmte = NA,
    FEC_ID_cand = NA
  ),
  tibble(
    first_name = "Chris",
    last_name = "Biesinger",
    state_cd = "UT-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Chris Biesinger for Congress - Utah",
    campaign_website = "https://chrisbiesinger.com/",
    actblue_link = NA,
    anedot_link = "friends-of-chris-biesinger/donate",
    other_link = NA,
    FEC_ID_cmte = "C00719617",
    FEC_ID_cand = "H0UT04068"
  ),
  # Vermont (at-large district; last updated Sep 19, 2020) =====================
  # Welch vs. Straw
  # but Straw does not seem to be a viable candidate
  # See https://www.facebook.com/andrewudstraw
  # Multiple candidacies in various states/positions
  tibble(
    first_name = "Peter",
    last_name = "Welch",
    state_cd = "VT-0",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://welch.house.gov",
    campaign_name = "Peter Welch for Congress",
    campaign_website = "https://www.welchforcongress.com/",
    actblue_link = "peter-welch-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00413179",
    FEC_ID_cand = "H6VT00160"
  ),
  tibble(
    first_name = "Miriam",
    last_name = "Berry",
    state_cd = "VT-0",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Miriam Berry RN for Congress",
    campaign_website = "https://www.berry2020.net",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?cmd=_donations&",
      "business=exodus15_21@icloud.com&item_name=Miriam%20Berry%20RN&",
      "currency_code=USD&Z3JncnB0="
    ),
    FEC_ID_cmte = "C00754945",
    FEC_ID_cand = "H0VT00130"
  ),
  # Lost the WinRed page before scraping
  tibble(
    first_name = "Jimmy",
    last_name = "Rodriguez",
    state_cd = "VT-0",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://jimmy4congress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/jimmy-rodriguez-for-congress/donate",
    FEC_ID_cmte = "C00732875",
    FEC_ID_cand = "H0VT01062"
  ),
  # Virginia (last updated Oct 3, 2020) ========================================
  # VA-01 (last updated Oct 3, 2020) -------------------------------------------
  # Wittman vs. Williams
  # vs. Cox vs. Washington vs. Easley vs. Rashid
  tibble(
    first_name = "Rob",
    last_name = "Wittman",
    state_cd = "VA-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://wittman.house.gov",
    campaign_name = "Rob Wittman for Congress",
    campaign_website = "https://robwittman.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/rob-wittman/donate",
    FEC_ID_cmte = "C00441014",
    FEC_ID_cand = "H8VA01147"
  ),
  tibble(
    first_name = "Qasim",
    last_name = "Rashid",
    state_cd = "VA-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rashid for Congress",
    campaign_website = "https://rashidforva.com",
    actblue_link = "rashidforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00734335",
    FEC_ID_cand = "H0VA01235"
  ),
  tibble(
    first_name = "Vangie",
    last_name = "Williams",
    state_cd = "VA-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vangie Williams for Congress",
    campaign_website = "https://vangieforcongress.com",
    actblue_link = c(
      # New link, same amount
      "vw2020close",
      "vangiewilliams2020",
      "vangie"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00632059",
    FEC_ID_cand = "H8VA01303"
  ),
  # Cannot find Cox
  # Washington withdrew, supporting Rashid
  # Cannot find Easley, listed as running for Senate
  # VA-02 (last updated Oct 3, 2020) -------------------------------------------
  tibble(
    first_name = "Elaine",
    last_name = "Luria",
    state_cd = "VA-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://luria.house.gov",
    campaign_name = "Elaine for Congress",
    campaign_website = "https://elaineforcongress.com/",
    # Change within ActBlue documented Oct 3, 2020
    actblue_link = c(
      "luriawebsite20",
      "luriawebsite"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00664375",
    FEC_ID_cand = "H8VA02111"
  ),
  tibble(
    first_name = "Scott",
    last_name = "Taylor",
    state_cd = "VA-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Taylor for Congress 2020",
    campaign_website = "https://www.scotttaylor2020.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/scotttaylor/donate",
    FEC_ID_cmte = c("C00733394", "C00702910", "C00608703"),
    FEC_ID_cand = "H0VA02118"
  ),
  tibble(
    first_name = "Jarome",
    last_name = "Bell",
    state_cd = "VA-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jarome Bell for Congress",
    campaign_website = "https://jaromebellforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Looks like revv: 25-50-100-250-500-1000-2800
    other_link = "https://secure.jaromebellforcongress.com/donate",
    FEC_ID_cmte = "C00725267",
    FEC_ID_cand = "H0VA02175"
  ),
  tibble(
    first_name = "Ben", # Benito, goes by Ben
    last_name = "Loyoloa",
    state_cd = "VA-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ben Loyola for Congress",
    campaign_website = "https://benloyola.com",
    actblue_link = NA,
    anedot_link = "ben-loyola-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00727743",
    FEC_ID_cand = "H0VA02092"
  ),
  # VA-03 (last updated Oct 3, 2020) -------------------------------------------
  tibble(
    first_name = "Bobby",
    last_name = "Scott",
    state_cd = "VA-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://bobbyscott.house.gov",
    campaign_name = "",
    campaign_website = "http://bobbyscottforcongress.com/",
    actblue_link = "bobby-scott-3",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00256925",
    FEC_ID_cand = "H6VA01117"
  ),
  tibble(
    first_name = "John",
    last_name = "Collick",
    state_cd = "VA-03",
    party = "Dep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John Collick for Congress",
    campaign_website = "https://www.collickforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = paste0(
    #   "https://donorbox.org/embed/collick2020?hide_donation_meter=true&",
    #   "designation=Area%20of%20greatest%20need"
    # ),
    # Change documented to WinRed Oct 3, 2020
    # At least from Aug 17, 2020
    other_link =
      "https://secure.winred.com/committee-to-elect-john-collick/general",
    FEC_ID_cmte = "C00703835",
    FEC_ID_cand = "H0VA03124"
  ),
  # VA-04 (last updated Oct 3, 2020) -------------------------------------------
  # Donald McEachin is uncontested (no candidates registered as of Sep 26, '19)
  # Now McEachin vs. Benjamin
  tibble(
    first_name = "Donald",
    last_name = "McEachin",
    state_cd = "VA-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://mceachin.house.gov",
    campaign_name = "McEachin for Congress",
    campaign_website = "https://donaldmceachin.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/-2616864771316316416",
    FEC_ID_cmte = "C00610964",
    FEC_ID_cand = "H6VA04061"
  ),
  tibble(
    first_name = "Leon",
    last_name = "Benjamin",
    state_cd = "VA-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Friends of Leon Benjamin",
    campaign_website = "https://benjamin4congress.com",
    actblue_link = NA,
    anedot_link = "friends-of-leon-benjamin/donate",
    other_link = NA,
    FEC_ID_cmte = "C00733485",
    FEC_ID_cand = "H0VA04064"
  ),
  # VA-05 (last updated Oct 3, 2020) -------------------------------------------
  tibble(
    first_name = "Denver",
    last_name = "Riggleman",
    state_cd = "VA-05",
    party = "Rep",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Denver Riggleman U.S. Congress",
    campaign_website = "https://denverforcongress.com",
    actblue_link = NA,
    anedot_link = "denverforcongress/joindenver",
    other_link = NA,
    FEC_ID_cmte = "C00680488",
    FEC_ID_cand = "H8VA05171"
  ),
  tibble(
    first_name = "Cameron",
    last_name = "Webb",
    state_cd = "VA-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dr. Cameron Webb for Congress",
    campaign_website = "https://www.drcameronwebb.com",
    actblue_link = "cameronwebb",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00714964",
    FEC_ID_cand = "H0VA05145"
  ),
  tibble(
    first_name = "Bob",
    last_name = "Good",
    state_cd = "VA-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.bobgoodforcongress.com",
    actblue_link = NA,
    ## https://web.archive.org/web/20200514202434/https://secure.anedot.com/good-for-congress/donate
    anedot_link = "good-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00721308",
    FEC_ID_cand = "H0VA05160"
  ),
  tibble(
    first_name = "",
    last_name = "",
    state_cd = "VA-05",
    party = "",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "",
    last_name = "",
    state_cd = "VA-05",
    party = "",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "",
    last_name = "",
    state_cd = "VA-05",
    party = "",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  tibble(
    first_name = "",
    last_name = "",
    state_cd = "VA-05",
    party = "",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = ""
  ),
  # VA-06 (last updated Oct 3, 2020) -------------------------------------------
  # Ben Cline is uncontested (no candidates registered as of Sep 26, 2019)
  tibble(
    first_name = "Ben",
    last_name = "Cline",
    state_cd = "VA-06",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://cline.house.gov",
    campaign_name = "Ben Cline for Congress",
    campaign_website = "https://bencline.com/about/",
    actblue_link = NA,
    anedot_link = "ben-cline/cfe01ad94f831d21c8505",
    other_link = NA,
    FEC_ID_cmte = "C00661561",
    FEC_ID_cand = "H8VA06104"
  ),
  tibble(
    first_name = "Nicholas",
    last_name = "Betts",
    state_cd = "VA-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nicholas Betts for Congress",
    campaign_website = "https://www.bettsforcongress.com",
    actblue_link = "nicholas-betts-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00746164",
    FEC_ID_cand = "H0VA06044"
  ),
  # VA-07 (last updated Oct 3, 2020) -------------------------------------------
  tibble(
    first_name = "Abigail",
    last_name = "Spanberger",
    state_cd = "VA-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://spanberger.house.gov",
    campaign_name = "Spanberger for Congress",
    campaign_website = "https://abigailspanberger.com/",
    actblue_link = "spanberger_website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00649913",
    FEC_ID_cand = "H8VA07094"
  ),
  tibble(
    first_name = "Nick",
    last_name = "Freitas",
    state_cd = "VA-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Freitas for Congress",
    campaign_website = "https://www.nickforva.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/nick-freitas-for-congress/donate",
    FEC_ID_cmte = "C00729335",
    FEC_ID_cand = "H0VA07158"
  ),
  tibble(
    first_name = "Tina",
    last_name = "Ramirez",
    state_cd = "VA-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tina Ramirez U.S. Congress",
    campaign_website = "https://tinaramirez.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.anedot.com/tramirez/donate",
    FEC_ID_cmte = "C00704643",
    FEC_ID_cand = "H0VA07109"
  ),
  tibble(
    first_name = "John",
    last_name = "McGuire",
    state_cd = "VA-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "McGuire for Congress",
    campaign_website = "https://mcguire4congress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/JohnMcGuire/donate",
    FEC_ID_cmte = "C00727594",
    FEC_ID_cand = "H0VA07133"
  ),
  tibble(
    first_name = "Andrew",
    last_name = "Knaggs",
    state_cd = "VA-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Andrew Knaggs for Congress",
    campaign_website = "https://andrewknaggs.com/",
    actblue_link = NA,
    anedot_link = NA,
    # WinRed Twitter on Feb 12, 2020, but was WinRed all along since Oct 2019
    # See https://web.archive.org/web/20191001204408/https://www.andrewknaggs.com/
    # 50-100-250-500 on front page though
    other_link = "https://secure.winred.com/knaggs-for-congress/donate-today",
    FEC_ID_cmte = "C00721126",
    FEC_ID_cand = "H0VA07125"
  ),
  tibble(
    first_name = "Peter Thomas",
    last_name = "Greenwald",
    state_cd = "VA-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Greenwald for Congress",
    campaign_website = "https://www.greenwaldforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Uh... interesting... 5-7-10-15-20-50-77-100-250-777-1000-2800
    other_link = "https://www.greenwaldforcongress.com/donate",
    FEC_ID_cmte = "C00715482",
    FEC_ID_cand = "H4VA07127"
  ),
  # # David Brat conceded to Spanberger: leftover from 2018 election it seems
  # # Cannot find Bridgette Williams
  tibble(
    first_name = "Jason",
    last_name = "Roberge",
    state_cd = "VA-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Roberge for Congress",
    campaign_website = "https://robergeforcongress.com/",
    actblue_link = NA,
    anedot_link = "roberge/donate",
    other_link = NA,
    FEC_ID_cmte = "C00714204",
    FEC_ID_cand = "H0VA07117"
  ),
  # VA-08 (last updated Oct 3, 2020) -------------------------------------------
  # Beyer vs. Straw
  # Straw is not a viable candidate. De facto uncontested as of Sep 26, 2019
  tibble(
    first_name = "Don",
    last_name = "Beyer",
    state_cd = "VA-08",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://beyer.house.gov",
    campaign_name = "Don Beyer for Congress",
    campaign_website = "http://friendsofdonbeyer.com/",
    actblue_link = "donb",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00555888",
    FEC_ID_cand = "H4VA08224"
  ),
  # I missed Major Mike Webb but most of his donations are self
  tibble(
    first_name = "Jeff",
    last_name = "Jordan",
    state_cd = "VA-08",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jeff Jordan For Congress",
    campaign_website = "https://jeffjordanforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/jeff-jordan-for-congress/donate",
    FEC_ID_cmte = "C00738013",
    FEC_ID_cand = "H0VA08198"
  ),
  # VA-09 (last updated Oct 3, 2020) -------------------------------------------
  # Morgan Griffith is uncontested (no candidates registered as of Sep 26, 2019)
  # Remains uncontested
  tibble(
    first_name = "Morgan",
    last_name = "Griffith",
    state_cd = "VA-09",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://morgangriffith.house.gov",
    campaign_name = "Morgan Griffith for Congress",
    campaign_website = "http://www.morgangriffithforcongress.com/",
    actblue_link = NA,
    anedot_link =
      "morgan-griffith-for-congress/general-fund-cc55814784b004afc52ae",
    other_link = NA,
    FEC_ID_cmte = "C00477240",
    FEC_ID_cand = "H0VA09055"
  ),
  # VA-10 (last updated Oct 3, 2020) -------------------------------------------
  # Wexton vs. Dove
  # vs. Jones vs. Andrews vs. Truong vs. Stone vs. Abdallah
  tibble(
    first_name = "Jennifer",
    last_name = "Wexton",
    state_cd = "VA-10",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Wexton for Congress",
    campaign_website = "https://jenniferwexton.com",
    actblue_link = "wextonforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00638023",
    FEC_ID_cand = "H8VA10106"
  ),
  tibble(
    first_name = "Aliscia",
    last_name = "Andrews",
    state_cd = "VA-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Andrews for Congress",
    campaign_website = "https://andrewsforvirginia.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/andrews-for-congress/donate",
    FEC_ID_cmte = "C00721142",
    FEC_ID_cand = "H0VA10186"
  ),
  # Name typo; anyway, lost primaries
  tibble(
    first_name = "Jeff",
    last_name = "Dove",
    state_cd = "VA-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jeff Dove for Congress",
    campaign_website = "https://doveforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/JeffDove/donate",
    FEC_ID_cmte = "C00630384",
    FEC_ID_cand = "H8VA11070"
  ),
  tibble(
    first_name = "Rob",
    last_name = "Jones",
    state_cd = "VA-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rob Jones For Congress",
    campaign_website = "https://robjonesforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/RobJones/donate",
    FEC_ID_cmte = "C00713230",
    FEC_ID_cand = "H0VA10160"
  ),
  # Website typo; anyway, lost primaries
  tibble(
    first_name = "Matt",
    last_name = "Truong",
    state_cd = "VA-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Matthew Truong for America",
    campaign_website = "https://mvga.us",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://secure.winred.com/",
      "friends-of-matt-truong/support-matt-truong-for-congress"
    ),
    FEC_ID_cmte = "C00728808",
    FEC_ID_cand = "H0VA10194"
  ),
  tibble(
    first_name = "Elizabeth",
    last_name = "Stone",
    state_cd = "VA-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elizabeth Stone for Virginia",
    campaign_website = "https://elizabethstone4va.com",
    actblue_link = NA,
    anedot_link = "elizabethstone4va/website-splash",
    other_link = NA,
    FEC_ID_cmte = "C00721068",
    FEC_ID_cand = "H0VA10178"
  ),
  # Well I ... cannot find ABDALLAH, UNIT89487 except his GitHub repo
  # VA-11 (last updated Oct 3, 2020) -------------------------------------------
  tibble(
    first_name = "Gerry", # Gerald Edwards, goes by Gerry
    last_name = "Connolly",
    state_cd = "VA-11",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Gerry Connolly for Congress",
    campaign_website = "https://gerryconnolly.com",
    actblue_link = "connolly-for-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00445452",
    FEC_ID_cand = "H8VA11062"
  ),
  tibble(
    first_name = "Manga",
    last_name = "Anantatmula",
    state_cd = "VA-11",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Manga for Congress",
    campaign_website = "https://mangaforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/manga-for-congress/donation",
    FEC_ID_cmte = "C00740449",
    FEC_ID_cand = "H0VA11127"
  ),
  tibble(
    first_name = "Zainab",
    last_name = "Mohsini",
    state_cd = "VA-11",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Zainab Mohsini for U.S. Congress",
    campaign_website = "https://www.zainabmohsini.com",
    actblue_link = "friends-of-zainab-mohsini-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00724625",
    FEC_ID_cand = "H0VA11119"
  ),
  # Washington (last updated Oct 3, 2020) ======================================
  # WA-01 (last updated Oct 3, 2020) -------------------------------------------
  # Suzan DelBene was uncontested (no candidates registered as of Sep 23, 2019)
  # Now DelBene vs. Beeler
  tibble(
    first_name = "Suzan",
    last_name = "DelBene",
    state_cd = "WA-01",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://delbene.house.gov",
    campaign_name = "DelBene for Congress",
    campaign_website = "https://www.delbeneforcongress.com/",
    # 15-25-100 prompt on main page
    actblue_link = "suzan-delbene-2018-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00459099",
    FEC_ID_cand = "H0WA08046"
  ),
  tibble(
    first_name = "Jeffrey",
    last_name = "Beeler",
    state_cd = "WA-01",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Beeler for Congress",
    campaign_website = "https://www.beeler4congress.org",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      # 10-25-50-100-200-500-1000-2800-5600-11200
      "https://beeler4congress.nationbuilder.com/donate",
      # 5-25-100
      paste0(
        "https://www.paypal.com/donate/?",
        "cmd=_s-xclick&hosted_button_id=J2TW9DNRU8BLA&source=url"
      )
    ),
    FEC_ID_cmte = "C00680264",
    FEC_ID_cand = "H8WA01077"
  ),
  # WA-02 (last updated Oct 3, 2020) -------------------------------------------
  # Larsen vs. Call vs. Hazelo vs. Miller
  tibble(
    first_name = "Rick",
    last_name = "Larsen",
    state_cd = "WA-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://larsen.house.gov",
    campaign_name = "Rick Larsen for Congress",
    campaign_website = "https://www.ricklarsen.org/",
    actblue_link = "stand-with-rick-larsen",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00345546",
    FEC_ID_cand = "H0WA02080"
  ),
  tibble(
    first_name = "Tim", # Timothy, goes by Tim
    last_name = "Hazelo",
    state_cd = "WA-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tim Hazelo for Congress",
    campaign_website = "https://www.timhazeloforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/timhazeloforcongress/donate",
    FEC_ID_cmte = "C00737387",
    FEC_ID_cand = "H0WA02197"
  ),
  tibble(
    first_name = "Jason",
    last_name = "Call",
    state_cd = "WA-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Call for Congress",
    campaign_website = "https://www.callforcongress.com/",
    actblue_link = "call-for-congress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00716076",
    FEC_ID_cand = "H0WA02189"
  ),
  # Cannot find the right Daniel R. Miller
  # WA-03 (last updated Oct 3, 2020) -------------------------------------------
  # Herrera Beutler vs. Long vs. Khalil vs. Atencio
  tibble(
    first_name = "Jaime",
    last_name = "Herrera Beutler",
    state_cd = "WA-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://herrerabeutler.house.gov",
    campaign_name = "",
    campaign_website = "https://www.votejaime.com",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from Dec 9, 2019
    # from 10-25-50-100-250-1000-2800-5600-11200
    #   to    25-50-100-250-500-1000-2800
    other_link = paste0(
      # "https://mystique.victorypassport.com/pages/herrerabeutler/donate?",
      # "location=https%3A%2F%2Fgive.victorypassport.com%2Fherrerabeutler%2F",
      # "contribute&location_url=aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5jb2",
      # "0vaGVycmVyYWJldXRsZXIvY29udHJpYnV0ZS8j"
      "https://secure.winred.com/jaime-for-congress/website_donation_page"
    ),
    FEC_ID_cmte = "C00472704",
    FEC_ID_cand = "H0WA03187"
  ),
  tibble(
    first_name = "Carolyn",
    last_name = "Long",
    state_cd = "WA-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Carolyn Long for Congress",
    campaign_website = "https://electlong.com",
    actblue_link = "7-19-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00660472",
    FEC_ID_cand = "H8WA03198"
  ),
  # Khalil withdrew on Feb 3, 2020
  tibble(
    first_name = "Peter",
    last_name = "Khalil",
    state_cd = "WA-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Khalil for Congress",
    campaign_website = "https://www.khalilforcongress.com",
    actblue_link = "khalilpeter",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00700930",
    FEC_ID_cand = "H0WA03237"
  ),
  tibble(
    first_name = "Rudy",
    last_name = "Atencio",
    state_cd = "WA-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Woke America and the Peace Campaign",
    campaign_website = "https://www.rudywa.org",
    actblue_link = "rudyatencio",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00709642",
    FEC_ID_cand = "H0WA03245"
  ),
  # WA-04 (last updated Oct 3, 2020) -------------------------------------------
  # Dan Newhouse is uncontested (no candidates registered as of Sep 23, 2019)
  tibble(
    first_name = "Dan",
    last_name = "Newhouse",
    state_cd = "WA-04",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://newhouse.house.gov",
    campaign_name = "Dan Newhouse for Congress",
    campaign_website = "https://www.dannewhouse.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # At least from Dec 26, 2019
    # from 10-25-50-100-250-1000-2800-5600-11200
    #   to 10-25-50-100-250-1000-2800-5600-11200 (same amounts)
    other_link = paste0(
      # "https://mystique.victorypassport.com/pages/dannewhouse/donate-main?",
      # "location=https%3A%2F%2Fgive.victorypassport.com%2Fdannewhouse%2F",
      # "contribute&location_url=aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5j",
      # "b20vZGFubmV3aG91c2UvY29udHJpYnV0ZS8j"
      "https://secure.winred.com/DanNewhouse/website_donations"
    ),
    FEC_ID_cmte = "C00559393",
    FEC_ID_cand = "H4WA04104"
  ),
  tibble(
    first_name = "Douglas",
    last_name = "McKinley",
    state_cd = "WA-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Doug McKinley for Congress",
    campaign_website = "https://www.dougmckinley2020.com",
    actblue_link = "dm20",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00743880",
    FEC_ID_cand = "H6WA04141"
  ),
  tibble(
    first_name = "Sarena",
    last_name = "Sloot",
    state_cd = "WA-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sarena Sloot 2020",
    campaign_website = "https://sarenasloot2020.com",
    actblue_link = NA,
    anedot_link = NA,
    # 50-100-250-500
    other_link = "https://sarenasloot2020.com/donate/",
    FEC_ID_cmte = "C00747006",
    FEC_ID_cand = "H0WA04128"
  ),
  # WA-05 (last updated Oct 3, 2020) -------------------------------------------
  # McMorris Rodgers vs. Wilson
  tibble(
    first_name = "Cathy",
    last_name = "McMorris Rodgers",
    state_cd = "WA-05",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://mcmorris.house.gov",
    campaign_name = "Cathy McMorris Rodgers for Congress",
    campaign_website = "https://www.cathyforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/cathy-mcmorris-rodgers/donate",
    FEC_ID_cmte = "C00390476",
    FEC_ID_cand = "H4WA05077"
  ),
  tibble(
    first_name = "Dave",
    last_name = "Wilson",
    state_cd = "WA-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elect Dave Wilson",
    campaign_website = "https://www.electdavewilson.com",
    actblue_link = "friends-of-dave-wilson-1",
    anedot_link = NA,
    # 5600-2800-1500-1000-500-250-200-150-100-50
    other_link = "https://www.electdavewilson.com/contribute",
    FEC_ID_cmte = "C00747063",
    FEC_ID_cand = "H0WA05133"
  ),
  # Dropped out Jul 20, 2020
  tibble(
    first_name = "Chris", # Christopher, goes by Chris
    last_name = "Armitage",
    state_cd = "WA-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Armitage for Congress",
    campaign_website = "https://armitageforcongress.com",
    actblue_link = "armitage-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00707265",
    FEC_ID_cand = "H0WA05125"
  ),
  # Now running for Legislature 4th District Position 2
  tibble(
    first_name = "Rob", # Robert, goes by Rob
    last_name = "Chase",
    state_cd = "WA-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rob Chase for Congress",
    campaign_website = "https://www.electrobchase.com",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-1000-2800-5600-11200
    other_link = "https://www.electrobchase.com/support",
    FEC_ID_cmte = "C00738427",
    FEC_ID_cand = "H2WA05048"
  ),
  # WA-06 (last updated Oct 3, 2020) -------------------------------------------
  # Kilmer vs. Parson vs. Kreiselmaier vs. Tirman
  tibble(
    first_name = "Derek",
    last_name = "Kilmer",
    state_cd = "WA-06",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://kilmer.house.gov",
    campaign_name = "Derek Kilmer for Congress",
    campaign_website = "https://derekkilmer.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/3198946187019356160",
    FEC_ID_cmte = "C00514893",
    FEC_ID_cand = "H2WA06129"
  ),
  tibble(
    first_name = "Rebecca",
    last_name = "Parson",
    state_cd = "WA-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rebecca Parson 2020",
    campaign_website = "https://rebeccaforwa.com",
    actblue_link = c(
      # 7-27-50-97-147-1997-2800 ... interesting...
      "rebeccaforwa",
      "rebecca-twitter"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710632",
    FEC_ID_cand = "H0WA06099"
  ),
  tibble(
    first_name = "Elizabeth",
    last_name = "Kreiselmaier",
    state_cd = "WA-06",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elizabeth for Congress",
    # Website supposed to be https://cleanupthehouse.com/ but down
    # Can't find donation link
    campaign_website = "https://cleanupthehouse.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://www.efundraisingconnections.com/c/ElizabethforCongress/",
    FEC_ID_cmte = "C00726513",
    FEC_ID_cand = "H0WA06081"
  ),
  # Tirman decided not to run on Aug 1, 2019
  # WA-07 (last updated Oct 3, 2020) -------------------------------------------
  # Pramila Jayapal was uncontested
  # (no candidates registered as of Sep 23, 2019)
  # Now Jayapal vs. Keller
  tibble(
    first_name = "Pramila",
    last_name = "Jayapal",
    state_cd = "WA-07",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://jayapal.house.gov",
    campaign_name = "Pramila for Congress",
    campaign_website = "https://www.pramilaforcongress.com/",
    actblue_link = "pramilaforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00605592",
    FEC_ID_cand = "H6WA07458"
  ),
  tibble(
    first_name = "Craig",
    last_name = "Keller",
    state_cd = "WA-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vote Craig Keller for Congress Republican",
    campaign_website = "https://keller4america.us",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = paste0(
      "https://www.paypal.com/donate?",
      "token=xEa4_D4_BaL32THOhbaEyarZ0hpoe9ItFXgzzJxWGchMs_B",
      "_rLzv4JlSX53AP1KbY5IWM2uu-dPOIbQx"
    ),
    FEC_ID_cmte = "C00569707",
    FEC_ID_cand = "H4WA07073"
  ),
  # WA-08 (last updated Sep 19, 2020) ------------------------------------------
  # Schrier vs. Jensen vs. Swank vs. Gordon vs. Mitchell
  tibble(
    first_name = "Kim",
    last_name = "Schrier",
    state_cd = "WA-08",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://schrier.house.gov",
    campaign_name = "Dr. Kim Schrier for Congress",
    # Splash site prompting donation of 5 USD
    campaign_website = "https://www.drkimschrier.com",
    actblue_link = "kimschrier",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00652628",
    FEC_ID_cand = "H8WA08189"
  ),
  tibble(
    first_name = "Jesse",
    last_name = "Jensen",
    state_cd = "WA-08",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jesse Jensen for Congress",
    campaign_website = "https://www.jessejensen2020.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/jesse-jensen-for-congress/support",
    FEC_ID_cmte = "C00720870",
    FEC_ID_cand = "H0WA08103"
  ),
  tibble(
    first_name = "Keith",
    last_name = "Swank",
    state_cd = "WA-08",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Keith Swank for America",
    campaign_website = "https://keithswankforamerica.com",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-250-500-1000-2800
    other_link = "https://keithswankforamerica.com/donate/",
    FEC_ID_cmte = "C00700161",
    FEC_ID_cand = "H2WA08166"
  ),
  tibble(
    first_name = "Dan E.",
    last_name = "Gordon",
    state_cd = "WA-08",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dan E Gordon for Congress",
    campaign_website = "https://dan-eg.com",
    actblue_link = NA,
    # No prompts
    anedot_link = "dan-e-gordon-for-congress/donate",
    other_link = NA,
    FEC_ID_cmte = "C00738963",
    FEC_ID_cand = "H0WA08111"
  ),
  tibble(
    first_name = "James",
    last_name = "Mitchell",
    state_cd = "WA-08",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "James Mitchell Pro-Life Democrat",
    campaign_website = "https://www.jamesmitchell4life.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.jamesmitchell4life.com/contribute",
    FEC_ID_cmte = "C00737106",
    FEC_ID_cand = "H0WA08129"
  ),
  # WA-09 (last updated Sep 19, 2020) ------------------------------------------
  # Adam Smith is uncontested (no candidates registered as of Sep 23, 2019)
  tibble(
    first_name = "Adam", # H. Adam, goes by Adam
    last_name = "Smith",
    state_cd = "WA-09",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://adamsmith.house.gov",
    campaign_name = "",
    campaign_website = "http://www.electadamsmith.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/7656909908645972736",
    FEC_ID_cmte = "C00304709",
    FEC_ID_cand = "H6WA09025"
  ),
  tibble(
    first_name = "Doug", # Douglas Michael, goes by Doug
    last_name = "Basler",
    state_cd = "WA-09",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Doug Basler for US Congress GOP",
    campaign_website = "http://dougbasler.com/index.html",
    actblue_link = NA,
    anedot_link = "friends-of-doug-basler/basler4congress",
    other_link = NA,
    FEC_ID_cmte = "C00566232",
    FEC_ID_cand = "H4WA09061"
  ),
  # WA-10 (last updated Sep 19, 2020) ------------------------------------------
  # Denny Heck announced retirement on Dec 4, 2019; now open seat
  # Collins vs. Slotnick
  # vs. Tate vs. Strickland vs. Reeves
  # vs. Gardner vs. Hewett vs. Doglio vs. Ingram
  tibble(
    first_name = "Marilyn",
    last_name = "Strickland",
    state_cd = "WA-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marilyn Strickland for Congress",
    campaign_website = "https://www.stricklandforwashington.com",
    actblue_link = "strickandwebsite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00732826",
    FEC_ID_cand = "H0WA10034"
  ),
  tibble(
    first_name = "Beth",
    last_name = "Doglio",
    state_cd = "WA-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Beth Doglio for Congress",
    campaign_website = "https://www.bethdoglio.com",
    actblue_link = "bethdoglioforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00735316", "C00735308"),
    FEC_ID_cand = "H0WA10075"
  ),
  tibble(
    first_name = "Joshua",
    last_name = "Collins",
    state_cd = "WA-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joshua Collins for US Congress",
    campaign_website = "https://www.joshua2020.com/",
    actblue_link = "joshua2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00701508",
    FEC_ID_cand = "H0WA10018"
  ),
  tibble(
    first_name = "Nancy",
    last_name = "Slotnick",
    state_cd = "WA-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nancy Dailey Slotnick For Congress",
    campaign_website = "http://nancyforus.org/",
    actblue_link = NA,
    anedot_link = "committee-to-elect-nancy-dailey-slotnick/website-donation",
    other_link = NA,
    FEC_ID_cmte = "C00678292",
    FEC_ID_cand = "H8WA10037"
  ),
  tibble(
    first_name = "Ryan",
    last_name = "Tate",
    state_cd = "WA-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ryan Tate for Congress WA10",
    campaign_website = "https://www.ryantateforcongress.org",
    actblue_link = NA,
    anedot_link = NA,
    # 5-10-20-50-100
    other_link = "https://www.ryantateforcongress.org/donate",
    FEC_ID_cmte = "C00729715",
    FEC_ID_cand = "H0WA10026"
  ),
  tibble(
    first_name = "Kristine",
    last_name = "Reeves",
    state_cd = "WA-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kristine Reeves for Congress",
    campaign_website = "https://www.kristinereeves.com",
    actblue_link = "reeves-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00733386",
    FEC_ID_cand = "H0WA10042"
  ),
  tibble(
    first_name = "Phil",
    last_name = "Gardner",
    state_cd = "WA-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Phil Gardner for Congress",
    campaign_website = "https://www.philgardner.com",
    actblue_link = "gardnerforcongress-web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00734855",
    FEC_ID_cand = "H0WA10059"
  ),
  tibble(
    first_name = "Don",
    last_name = "Hewett",
    state_cd = "WA-10",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    # Actual website and Twitter gives 404 errors
    campaign_website = "https://www.facebook.com/donald.hewett.7374",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00735506",
    FEC_ID_cand = "H0WA10067"
  ),
  tibble(
    first_name = "Rian",
    last_name = "Ingrim",
    state_cd = "WA-10",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rian Ingrim for Congress",
    campaign_website = "https://rianforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.efundraisingconnections.com/c/RianIngrim",
    FEC_ID_cmte = "C00737510",
    FEC_ID_cand = "H0WA10083"
  ),
  tibble(
    first_name = "Denny",
    last_name = "Heck",
    state_cd = "WA-10",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://dennyheck.house.gov",
    campaign_name = "Denny Heck for Congress",
    campaign_website = "https://dennyheckforcongress.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/9025759679906646016",
    FEC_ID_cmte = "C00472159",
    FEC_ID_cand = "H0WA03161"
  ),
  # West Virginia (last updated Sep 19, 2020) ==================================
  # WV-01 (last updated Sep 19, 2020) ------------------------------------------
  # McKinley vs. Cline
  tibble(
    first_name = "David",
    last_name = "McKinley",
    state_cd = "WV-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://mckinley.house.gov",
    campaign_name = "McKinley for Congress",
    campaign_website = "https://davidforwv.com",
    actblue_link = NA,
    # Change documented to WinRed on Feb 27, 2020
    # from 25-50-100-250-1000-2800 to 25-50-100-250-500-1000-2800
    anedot_link = NA, # "mckinley/donate",
    other_link = "https://secure.winred.com/davidmckinley/donate",
    FEC_ID_cmte = "C00473132",
    FEC_ID_cand = "H0WV01072"
  ),
  tibble(
    first_name = "Natalie",
    last_name = "Cline",
    state_cd = "WV-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Natalie Cline for U.S. Congress",
    campaign_website = "www.natalieclineforcongress.com",
    actblue_link = "cline2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00706705",
    FEC_ID_cand = "H0WV01114"
  ),
  # WV-02 (last updated Sep 19, 2020) ------------------------------------------
  # Mooney vs. Kunkel vs. Hahn
  tibble(
    first_name = "Alex",
    last_name = "Mooney",
    state_cd = "WV-02",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://mooney.house.gov",
    campaign_name = "Alex Mooney for Congress",
    campaign_website = "https://www.mooneyforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # Currently 404 error
    # Change documented to WinRed on Feb 27, 2020
    # At least from Jan 22, 2020
    other_link = paste0(
      # "https://secure.campaignsolutions.com/mooney/list/proc/donation1/?",
      # "initiativekey=YY5PRUILTSML"
      "https://secure.winred.com/AlexMooney/donate"
    ),
    FEC_ID_cmte = "C00629949",
    FEC_ID_cand = "H4WV02080"
  ),
  tibble(
    first_name = "Cathy",
    last_name = "Kunkel",
    state_cd = "WV-02",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kunkel for Congress",
    campaign_website = "https://kunkelforcongress.com",
    actblue_link = "kunkel-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00713792",
    FEC_ID_cand = "H0WV02179"
  ),
  tibble(
    first_name = "Matthew",
    last_name = "Hahn",
    state_cd = "WV-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Matt Hahn for Congress",
    campaign_website = "https://www.matt-hahnforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/embed/friends-of-matthew-hahn",
    FEC_ID_cmte = "C00735365",
    FEC_ID_cand = "H0WV02187"
  ),
  # WV-03 (last updated Sep 19, 2020) ------------------------------------------
  # Miller vs. Turner vs. Watson vs. Lewis vs. Davis
  tibble(
    first_name = "Carol",
    last_name = "Miller",
    state_cd = "WV-03",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://miller.house.gov",
    campaign_name = "Carol for Congress",
    campaign_website = "https://www.electcarolmiller.com",
    # Change documented to WinRed Nov 16, 2019
    # At least from Nov 13, 2019
    # 25-50-100-250-500-1000-2700 to 25-50-100-350-1000-1500-2800
    actblue_link = NA,
    anedot_link = NA,
    # Woops, didn't put it here for ages...
    other_link = "https://secure.winred.com/CarolMiller/donate",
    # "https://transaxt.com/Donate/MJDUUT/CarolforCongressCommittee/",
    FEC_ID_cmte = "C00653220",
    FEC_ID_cand = "H8WV03097"
  ),
  tibble(
    first_name = "Hilary",
    last_name = "Turner",
    state_cd = "WV-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hilary Turner for Congress",
    campaign_website = "https://www.turnerforcongresswv.com",
    actblue_link = c(
      "hilaryturnerwv",
      "turnerwv"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00718858",
    FEC_ID_cand = "H0WV03151"
  ),
  tibble(
    first_name = "Lacy",
    last_name = "Watson",
    state_cd = "WV-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lacy Watson for Congress",
    campaign_website = "https://votelacy.com",
    actblue_link = "lacy-watson-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00704635",
    FEC_ID_cand = "H0WV03144"
  ),
  tibble(
    first_name = "Jeff", # Jeffrey, goes by Jeff
    last_name = "Lewis",
    state_cd = "WV-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jeff Lewis For Congress",
    campaign_website = "https://www.jefflewisforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link =
      "https://23452256-herm.myregisteredstore.com/tap/checkout.aspx?id=4",
    FEC_ID_cmte = "C00737551",
    FEC_ID_cand = "H0WV03169"
  ),
  tibble(
    first_name = "Paul",
    last_name = "Davis",
    state_cd = "WV-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Paul Davis for Congress",
    campaign_website = "http://pauledavisforcongress.com",
    actblue_link = "davis4house",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00739466",
    FEC_ID_cand = "H0WV03177"
  ),
  # Wisconsin (last updated Sep 19, 2020) ======================================
  # WI-01 (last updated Sep 19, 2020) ------------------------------------------
  tibble(
    first_name = "Bryan",
    last_name = "Steil",
    state_cd = "WI-01",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://steil.house.gov",
    campaign_name = "Steil for Wisconsin",
    campaign_website = "https://www.bryansteil.com",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://donate.steilforwisconsin.com/contribute-button",
    # Change documented to WinRed on Sep 19, 2020
    other_link = "https://secure.winred.com/steilforwisconsin/contribute",
    FEC_ID_cmte = "C00677286",
    FEC_ID_cand = "H8WI01156"
  ),
  tibble(
    first_name = "Roger",
    last_name = "Polack",
    state_cd = "WI-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Roger for Wisconsin",
    campaign_website = "https://rogerforwisconsin.com",
    actblue_link = "roger-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00733600",
    FEC_ID_cand = "H0WI01120"
  ),
  tibble(
    first_name = "Josh", # Joshua Jerry, goes by Josh
    last_name = "Pade", # Randazza-Pade, goes by Pade
    state_cd = "WI-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Josh Pade for Congress",
    campaign_website = "https://www.joshpade.com",
    actblue_link = "josh-pade-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00714816",
    FEC_ID_cand = "H0WI01104"
  ),
  tibble(
    first_name = "Angela",
    last_name = "Cunningham",
    state_cd = "WI-01",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Angela Cunningham to Congress",
    campaign_website = "https://www.voteforcunningham.com",
    actblue_link = "cunninghamforcongress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00730291",
    FEC_ID_cand = "H0WI01112"
  ),
  # WI-02 (last updated Sep 19, 2020) ------------------------------------------
  # Mark Pocan was uncontested (no candidates registered as of Sep 24, 2019)
  # Now Pocan vs. Theron vs. Heller
  tibble(
    first_name = "Mark",
    last_name = "Pocan",
    state_cd = "WI-02",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://pocan.house.gov",
    campaign_name = "Mark Pocan for Congress",
    campaign_website = "https://pocanforcongress.com/",
    actblue_link = "pocanwebsite",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00502179",
    FEC_ID_cand = "H2WI02124"
  ),
  tibble(
    first_name = "Peter", # Daniel Peter, goes by Peter
    last_name = "Theron",
    state_cd = "WI-02",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Theron for Congress",
    campaign_website = "https://theronforcongress.com",
    # Only wants checks
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://theronforcongress.com/contact/",
    # Change documented to WinRed
    other_link = "https://secure.winred.com/friends-of-peter-theron/donate",
    FEC_ID_cmte = "C00450353",
    FEC_ID_cand = "H8WI02121"
  ),
  # Cannot find Heller
  # WI-03 (last updated Sep 19, 2020) ------------------------------------------
  # Kind vs. Neumann
  # vs. Bonner vs. Virijevich vs. Cook vs. Peters vs. Garske
  # vs. Ebben vs. Sundblom
  # --- Bonner withdrew on Nov 24, 2019
  # --- Alex Virijevich (very interesting candidate...) campaigns seems to be
  #     rollover from 2018, and not eligible to serve until 2030
  # --- Cannot find Peters nor Sundblom
  #
  tibble(
    first_name = "Ron", # Ronald James, goes by Ron
    last_name = "Kind",
    state_cd = "WI-03",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://kind.house.gov",
    campaign_name = "Kind for Congress",
    campaign_website = "http://www.ronkind.org/",
    actblue_link = "ronkind2016",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00312017",
    FEC_ID_cand = "H6WI03099"
  ),
  tibble(
    first_name = "Derrick",
    last_name = "Van Orden",
    state_cd = "WI-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Van Orden for Congress",
    campaign_website = "https://vanordenforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/van-orden-for-congress/home",
    FEC_ID_cmte = c("C00742007", "C00702951"),
    FEC_ID_cand = "H0WI03175"
  ),
  # Lost Jessi Ebben and Garske
  tibble(
    first_name = "Mark",
    last_name = "Neumann",
    state_cd = "WI-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Neumann for Congress",
    campaign_website = "http://www.markneumannforcongress.com/",
    actblue_link = "neumann-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00716258",
    FEC_ID_cand = "H0WI03118"
  ),
  # Withdrew on Nov 24, 2019
  tibble(
    first_name = "Justin",
    last_name = "Bonner",
    state_cd = "WI-03",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Justin Bonner for Congress",
    campaign_website = "https://www.justinbonner.com/",
    actblue_link = "justinbonner",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00713982",
    FEC_ID_cand = "H0WI03100"
  ),
  tibble(
    first_name = "Brandon",
    last_name = "Cook",
    state_cd = "WI-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cook 4 Congress",
    campaign_website = "https://twitter.com/cook4ushouse",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = "https://www.paypal.com/paypalme2/Cook4Congress",
    FEC_ID_cmte = "C00724641",
    FEC_ID_cand = "H0WI03126"
  ),
  # Cannot find Peters
  tibble(
    first_name = "John",
    last_name = "Garske",
    state_cd = "WI-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Garske for Congress",
    campaign_website = "https://garskeforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/garske-for-congress/donate",
    FEC_ID_cmte = "C00733246",
    FEC_ID_cand = "H0WI03142"
  ),
  tibble(
    first_name = "Jessi",
    last_name = "Ebben",
    state_cd = "WI-03",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jessi Ebben for U.S. Congress for Wisconsin",
    campaign_website = "https://www.ebbenforwisconsin.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/jessiebben/donate",
    FEC_ID_cmte = "C00736108",
    FEC_ID_cand = "H0WI03159"
  ),
  # Cannot find Sundblom
  # WI-04 (last updated Jul 13, 2021) ------------------------------------------
  tibble(
    first_name = "Gwen",
    last_name = "Moore",
    state_cd = "WI-04",
    party = "Dem",
    incumbent = TRUE,
    office_website = "https://gwenmoore.house.gov",
    campaign_name = "Gwen Moore for Congress",
    campaign_website = "https://www.gwenmooreforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://act.myngp.com/Forms/2328907034006718464",
    FEC_ID_cmte = "C00397505",
    FEC_ID_cand = "H4WI04183"
  ),
  # Rogers is not yet on the FEC
  tibble(
    first_name = "Tim",
    last_name = "Rogers",
    state_cd = "WI-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tim Rogers for Congress",
    campaign_website = "https://www.rogersforwisconsin.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = "https://www.rogersforwisconsin.com/donate",
    FEC_ID_cmte = "C00759506",
    FEC_ID_cand = "H8WI04069"
  ),
  tibble(
    first_name = "Cindy",
    last_name = "Werner",
    state_cd = "WI-04",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cindy Werner For Congress",
    campaign_website = "https://www.werner4congress.com",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-150-250-500-1000-1500-2000-2700
    other_link = "https://www.werner4congress.com/contribute",
    FEC_ID_cmte = "C00717413",
    FEC_ID_cand = "H0WI04132"
  ),
  tibble(
    first_name = "Robert",
    last_name = "Raymond",
    state_cd = "WI-04",
    party = "Ind",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Robert Raymond For Congress",
    campaign_website = "https://www.facebook.com/RobertRaymondCampaignForCongress/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "H4WI04266"
  ),
  # WI-05 (last updated Sep 19, 2020) ------------------------------------------
  # Fitzgerald vs. Palzewicz
  tibble(
    first_name = "Scott",
    last_name = "Fitzgerald",
    state_cd = "WI-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Scott Fitzgerald for Congress",
    campaign_website = "https://scottfitzgeraldforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    # At least from Jan 30, 2020
    other_link =
      "https://secure.winred.com/scott-fitzgerald-for-congress/donate",
    FEC_ID_cmte = "C00720011",
    FEC_ID_cand = "H0WI05113"
  ),
  tibble(
    first_name = "Tom",
    last_name = "Palzewicz",
    state_cd = "WI-05",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Palzewicz for Wisconsin",
    campaign_website = "https://www.tomforwi.com/",
    # Both an updated ActBlue and a new NGP VAN
    actblue_link = c(
      "palzewicz-for-wisconsin-eom2",
      "tomforwi2020"
    ),
    anedot_link = NA,
    other_link = "https://actions.ngpvan.com/v1/Forms/LlzZcYV_AEmaBoDNV3FcIw2",
    FEC_ID_cmte = "C00653733",
    FEC_ID_cand = "H8WI05165"
  ),
  # Did lose primaries, so will eventually delete
  tibble(
    first_name = "Clifford",
    last_name = "DeTemple",
    state_cd = "WI-05",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cliff DeTemple for Congress",
    campaign_website = "https://www.detemple.us",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://politics.raisethemoney.com/cdetemple",
    FEC_ID_cmte = "C00751743",
    FEC_ID_cand = "H0WI05121"
  ),
  # WI-06 (last updated Sep 19, 2020) ------------------------------------------
  tibble(
    first_name = "Glenn",
    last_name = "Grothman",
    state_cd = "WI-06",
    party = "",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "Glenn Grothman for Congress",
    campaign_website = "https://www.glenngrothman.com",
    actblue_link = NA,
    anedot_link =
      "glenn-grothman-for-congress/support-glenn-grothman-for-congress",
    other_link = NA,
    FEC_ID_cmte = "C00561597",
    FEC_ID_cand = "H4WI06048"
  ),
  tibble(
    first_name = "Jessica",
    last_name = "King",
    state_cd = "WI-06",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jessica King for Congress",
    campaign_website = "https://jessicakingforwisconsin.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://actions.ngpvan.com/v1/Forms/4TWofcO3I0eXdjs78us-tg2",
    FEC_ID_cmte = "C00648774",
    FEC_ID_cand = "H8PA16078"
  ),
  # WI-07 (last updated Sep 19, 2020) ------------------------------------------
  # Zero receipts and disbursements as of Sep 16, 2019.
  # Replacing Sean Duffy (open seat). Duffy's donation page is empty WinRed
  # with webpage at https://duffyforwisconsin.com/
  # Cannot find Opela
  tibble(
    first_name = "Tom",
    last_name = "Tiffany",
    state_cd = "WI-07",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://tiffany.house.gov",
    campaign_name = "Tom Tiffany for Congress",
    campaign_website = "https://tiffany4wisconsin.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/tiffany-for-wisconsin/donate",
    FEC_ID_cmte = "C00718635",
    FEC_ID_cand = "H0WI07101"
  ),
  tibble(
    first_name = "Tricia",
    last_name = "Zunker",
    state_cd = "WI-07",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tricia for Wisconsin",
    campaign_website = "https://triciaforwisconsin.com",
    actblue_link = "tricia-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00722876",
    FEC_ID_cand = "H0WI07135"
  ),
  tibble(
    first_name = "Jason",
    last_name = "Church",
    state_cd = "WI-07",
    party = "Rep",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Church for Wisconsin",
    campaign_website = "https://www.churchforwisconsin.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/churchforwisconsin/donate",
    FEC_ID_cmte = "C00721035",
    FEC_ID_cand = "H0WI07127"
  ),
  # WI-08 (last updated Sep 19, 2020) ------------------------------------------
  tibble(
    first_name = "Mike", # Michael John, goes by Mike
    last_name = "Gallagher",
    state_cd = "WI-08",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://gallagher.house.gov",
    campaign_name = "Mike Gallagher for Wisconsin",
    campaign_website = "http://www.mikeforwisconsin.com",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Sep 19, 2020
    # other_link = "https://mfwis.nationbuilder.com/donate",
    other_link = "https://secure.winred.com/mikegallagher/donate-2020/",
    FEC_ID_cmte = "C00610212",
    FEC_ID_cand = "H6WI08155"
  ),
  tibble(
    first_name = "Amanda",
    last_name = "Stuck",
    state_cd = "WI-08",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Amanda Stuck for Congress",
    campaign_website = "https://amandastuck.com",
    actblue_link = "stuck-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00713545",
    FEC_ID_cand = "H0WI08125"
  ),
  # Wyoming (at-large district; last updated Sep 19, 2020) =====================
  # Cheney vs. Hafner
  tibble(
    first_name = "Liz",
    last_name = "Cheney",
    state_cd = "WY-0",
    party = "Rep",
    incumbent = TRUE,
    office_website = "https://cheney.house.gov",
    campaign_name = "Cheney for Wyoming",
    campaign_website = "https://cheneyforwyoming.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed
    # At least from
    # from 10-25-50-100-250-1000-2800
    #   to 10-25-100-250-500-1000-2800
    other_link = paste0(
      # "https://mystique.victorypassport.com/pages/cheney/donate-today?",
      # "location=https%3A%2F%2Fgive.victorypassport.com%2Fcheney%2F",
      # "donate-today&location_url=aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5",
      # "jb20vY2hlbmV5L2RvbmF0ZS10b2RheS8j"
      "https://secure.winred.com/lizcheney/donate"
    ),
    FEC_ID_cmte = "C00607556",
    FEC_ID_cand = "H6WY00159"
  ),
  tibble(
    first_name = "Lynnette",
    last_name = "Grey Bull",
    state_cd = "WY-0",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Grey Bull for Congress",
    campaign_website = "https://www.lynnettegreybull.com",
    actblue_link = "grey-bull-for-congress-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00749598",
    FEC_ID_cand = "H0WY01036"
  ),
  # Although lost primaries...
  tibble(
    first_name = "Carl",
    last_name = "Beach",
    state_cd = "WY-0",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Beach for Congress",
    campaign_website = "https://beachforwyoming.org",
    actblue_link = "beachforwyoming",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00740647",
    FEC_ID_cand = "H0WY01028"
  ),
  # Missed Blake Stanley
  tibble(
    first_name = "Carol",
    last_name = "Hafner",
    state_cd = "WY-0",
    party = "Dem",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Carol Hafner for Wyoming",
    campaign_website = "https://carolhafner.com/",
    actblue_link = "carol-hafner-for-wyoming-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00709048",
    FEC_ID_cand = "H0WY01010"
  )
  # Unroll and tidy ===========================================================
) %>%
  bind_rows() %>%
  mutate(year = 2020) %>%
  filter(last_name != "" & state_cd != "") %>%
  rowwise() %>%
  mutate(
    actblue_link = paste0("https://secure.actblue.com/donate/", actblue_link),
    # Must have /donate at each anedot_link, because there are exceptions
    anedot_link = paste0("https://secure.anedot.com/", anedot_link)
  ) %>%
  ungroup() %>%
  mutate(
    actblue_link = ifelse(
      actblue_link == "https://secure.actblue.com/donate/NA", NA, actblue_link
    ),
    anedot_link = ifelse(
      anedot_link == "https://secure.anedot.com/NA", NA, anedot_link
    )
  )

# Test =========================================================================
assert_that(
  nrow(cand_house_2020 %>% filter(party == "Dem" & !is.na(anedot_link))) == 0
)
assert_that(
  nrow(cand_house_2020 %>% filter(party == "Rep" & !is.na(actblue_link))) == 0
)
assert_that(
  # Creates taskscheduler problems
  nrow(cand_house_2020 %>% filter(is.na(state_cd))) == 0
)
