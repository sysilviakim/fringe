source(here::here("R", "utilities.R"))

cand_senate_2020 <- list(
  # AL: Alabama II (last updated Nov 4, 2020) ==================================
  # Jones vs. Byrne
  # vs. Tuberville vs. Merrill vs. Mooney vs. Adair vs. Sessions vs. Moore
  # Parrish vs. Serbin vs. Hatley vs. Sanders vs. Romagnano vs. Nelson
  # Serbin/Romagnano withdrew before primaries, cannot find Hatley
  # Tuberville won; Jones from the Dems uncontested
  tibble(
    first_name = "Doug",
    last_name = "Jones",
    state = "AL",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.jones.senate.gov",
    campaign_name = "Doug Jones for Senate",
    campaign_website = "https://dougjones.com/",
    actblue_link = c(
      "jones-homepage1" # ,
      # "stand-with-doug",
      # "adopt-doug-jones",
      # "dougjones2020",
      # "bjalsen",
      # # https://twitter.com/ChrisMurphyCT/status/1319047968402644992?s=20
      # "csm-bes-jones",
      # "dougjonesal2020",
      # "fight-for-doug-aug-fb",
      # "stand-with-doug",
      # "jones-lead-fb"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00640623",
    FEC_ID_cand = "S0AL00156"
  ),
  tibble(
    first_name = "Tommy",
    last_name = "Tuberville",
    state = "AL",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tuberville for Senate",
    campaign_website = c(
      "https://tommyforsenate.com/" # ,
      # "https://tommyforsenate.com/home/",
      # rep("", 5)
    ),
    actblue_link = NA,
    anedot_link = "tommy-tuberville-for-senate/donate",
    other_link = c(
      paste0(
        "https://secure.winred.com/tommytuberville/",
        c(
          "official-donate" # ,
          # "splash",  "donate",
          # "pg-fb-supportcoach-101820-8f3f338947744ccf",
          # "pg-fbfr-keepthesenate-nd-080620", "pg-fbfr-confirm-092320",
          # "fb-stop-dem-takeover-081420"
        )
      )
    ),
    FEC_ID_cmte = "C00701672",
    FEC_ID_cand = "S0AL00230"
  ),
  tibble(
    first_name = "Bradley Roberts",
    last_name = "Byrne",
    state = "AL",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Byrne for Senate, Inc.",
    # There seems to be fake website that is phishing, not this one, weird.
    campaign_website = "https://www.bradleybyrne.com/",
    actblue_link = NA,
    anedot_link = NA,
    # #other_link = "https://give.victorypassport.com/byrnesenateal/donate-today-website/",
    # #other_link = paste0( "https://mystique.victorypassport.com/pages/byrnesenateal/",
    # #  "website?location=https%3A%2F%2Fgive.victorypassport.com",
    #  # "%2Fbyrnesenateal%2Fdonate-today-website&location_url=",
    #   "aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5jb20vYnlybmVzZW5h",
    #   "dGVhbC9kb25hdGUtdG9kYXktd2Vic2l0ZS8%2FcmVjdXJyaW5nPXRydWUj"
    # ),
    # Change documented to WinRed on Oct 3, 2019
    # Documented to be July on WinRed Twitter
    # from 10-25-50-100-250-1000-2800 to 25-50-100-250-500-1000-2800
    other_link = "https://secure.winred.com/bradley-byrne/website-donate-today",
    FEC_ID_cmte = "C00697417",
    FEC_ID_cand = "S0AL00206"
  ),
  tibble(
    first_name = "Arnold",
    last_name = "Mooney",
    state = "AL",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Arnold for Alabama",
    campaign_website = "https://arnoldmooney.com/",
    actblue_link = NA,
    anedot_link = NA,
    # revv link
    other_link = "https://secure.arnoldmooney.com/donate/",
    FEC_ID_cmte = "C00705269",
    FEC_ID_cand = "S0AL00263"
  ),
  # Withdrew on Dec 1, 2020
  tibble(
    first_name = "John",
    last_name = "Merrill",
    state = "AL",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elect John Merrill",
    campaign_website = "http://johnhmerrill.com/",
    actblue_link = NA,
    anedot_link = "friends-of-john-h-merrill-for-us-senate/donate",
    other_link = NA,
    FEC_ID_cmte = "C00709477",
    FEC_ID_cand = "S0AL00289"
  ),
  tibble(
    first_name = "Stanley",
    last_name = "Adair",
    state = "AL",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.stanleyadairforsenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    #  # No suggested amounts until you enter email:
    #  # $10.00    $25.00    $50.00    $100.00    $250.00    $500.00    $1,000.00
    other_link = paste0(
      "https://secure.squarespace.com/checkout/donate?",
      "donatePageId=5d4c80056d051e0001848af0&",
      "ss_cid=c20ed336-ccb4-49d1-8ec3-50090e2b92e8&",
      "ss_cvisit=1568274674585&",
      "ss_cvr=53158ee1-87c8-4c8d-af51-d5cc2689ed24%7C1568274675515%7",
      "C1568274675515%7C1568274675515%7C1"
    ),
    FEC_ID_cmte = "C00702118",
    FEC_ID_cand = "S0AL00248"
  ),
  tibble(
    first_name = "Roy",
    last_name = "Moore",
    state = "AL",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Judge Roy Moore for U.S. Senate",
    campaign_website = "https://roymoore.org/",
    actblue_link = NA,
    anedot_link = "judgemoore/moore2020",
    other_link = NA,
    FEC_ID_cmte = "C00710293",
    FEC_ID_cand = "S0AL00297"
  ),
  # Candidacy filed on Nov 18, 2019
  # Eliminated in runoff
  tibble(
    first_name = "Jeff",
    last_name = "Sessions",
    state = "AL",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jeff Sessions for U.S. Senate",
    campaign_website = "https://www.jeffsessions.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://secure.winred.com/",
      "friends-of-sessions-senate-committee-inc/donate/"
    ),
    FEC_ID_cmte = "C00306704",
    FEC_ID_cand = "S6AL00195"
  ),
  # Withdrew on November 3, 2020.
  tibble(
    first_name = "Mike",
    last_name = "Parrish",
    state = "AL",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Citizen Mike 2020",
    campaign_website = "https://citizenmike2020.wixsite.com/mysite",
    # No contribution link, although there are records of having received money
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "S0AL00313",
    FEC_ID_cand = "C00713107"
  ),
  tibble(
    first_name = "Ruth",
    last_name = "Nelson",
    state = "AL",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ruth for AL Senate",
    campaign_website = "https://www.facebook.com/ruthforALSenate-108986210575332/",
    # No contribution link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00726257",
    FEC_ID_cand = "S0AL00321"
  ),
  # AL: Alabama III (last updated Nov 4, 2020) =================================
  # Not up for election 2020.
  # Richard Shelby was uncontested (no candidates registered as of Sep 11, '19)
  # Now Shelby vs. Dean
  tibble(
    first_name = "Richard",
    last_name = "Shelby",
    state = "AL",
    party = "Rep",
    class = "III",
    incumbent = TRUE,
    office_website = "www.shelby.senate.gov",
    campaign_name = "Shelby for U.S. Senate",
    campaign_website = "http://www.shelbyforsenate.com/",
    # Has no(!) donation page linked to his website!!
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    # And he has not raised a single dime during 01/01/2017 to present(!!)
    # from itemized/unitemized individual contributions. How can this be?
    # He certainly has 9,831,274 USD cash on hand.
    # See https://www.rollcall.com/news/campaigns/
    # the-gop-senator-who-hasnt-raised-any-campaign-money-so-far
    # So... could be not running for reelection? Not sure.
    # Searching on the web is a no result too.
    # Not even on WinRed.
    FEC_ID_cmte = "C00193623",
    FEC_ID_cand = "S6AL00013"
  ),
  tibble(
    first_name = "Brandaun",
    last_name = "Dean",
    state = "AL",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brandaun Dean",
    campaign_website = "http://brandaundean.com",
    actblue_link = NA, # "usinthesenate",
    # This actblue link no longer works: "no active recipients"
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00705012",
    FEC_ID_cand = "S2AL00111"
  ),
  # AK: Alaska II (last updated Nov 4, 2020) ===================================
  # Dan Sullivan vs. Matheny vs. Gross
  # vs. Blatchford vs. Cumings vs. Howe
  # Matheny withdrew; Blatchford and Cumings lost in the primary
  tibble(
    first_name = "Dan",
    last_name = "Sullivan",
    state = "AK",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.sullivan.senate.gov",
    campaign_name = "Alaskans for Dan Sullivan",
    campaign_website = "https://dansullivanforalaska.com",
    # Change documented to WinRed on Jan 29, 2019
    # 50-100-250-500-1000-2800-5600 to 25-50-100-150-250-500-1000-2800-5600
    # At least from Dec 10, 2019, according to WinRed Twitter
    actblue_link = NA,
    # anedot_link = "alaskans-for-dan-sullivan/alaskans-for-sullivan",
    anedot_link = NA,
    other_link = c(
      # Change within WinRed, Oct 25, 2020
      "https://secure.winred.com/alaskans-for-dan-sullivan/donate",
      "https://secure.winred.com/alaskans-for-dan-sullivan/email",
      "https://secure.winred.com/alaskans-for-dan-sullivan/ad"
    ),
    FEC_ID_cmte = c("C00570994", "C00551093", ""),
    FEC_ID_cand = "S4AK00214"
  ),
  tibble(
    first_name = "Al",
    last_name = "Gross",
    state = "AK",
    party = "Ind",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dr. Al Gross for U.S. Senate",
    campaign_website = "https://dralgrossak.com/",
    # Independent on FEC yet allowed on ActBlue? Interesting...
    actblue_link = c(
      "algross-2020" # ,
      # "algross-2020-q3search",
      # "gross-q3-d2d-fb",
      # "gross-q4-d2d-fb",
      # "algross"
      # "al-gross-examiner-d2d",
      # "algross_futgen",
      # "flipthesenate-gross"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710822",
    FEC_ID_cand = "S0AK00196"
  ),
  tibble(
    first_name = "John Wayne",
    last_name = "Howe",
    state = "AK",
    party = "Oth", # Third Party
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John Wayne Howe for US Senator for Alaska",
    campaign_website = "https://www.alaskansforjohnwaynehowe.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.alaskansforjohnwaynehowe.com/contribute",
    FEC_ID_cmte = "",
    FEC_ID_cand = "S0AK00238"
  ),
  # Withdrew before primaries
  tibble(
    first_name = "Dave",
    last_name = "Matheny",
    state = "AK",
    party = "Ind",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Matheny for Senate",
    campaign_website = "https://twitter.com/Matheny4Senate",
    actblue_link = NA,
    anedot_link = NA,
    # But 404 error really...
    other_link = "https://mathenyforsenate.nationbuilder.com/donate_today",
    FEC_ID_cmte = "C00710822",
    FEC_ID_cand = "S0AK00196"
  ),
  # AK: Alaska III (last updated Nov 7, 2020) ==================================
  # Not up for election 2020.
  # Lisa Murkowski is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Lisa",
    last_name = "Murkowski",
    state = "AK",
    party = "Rep",
    class = "III",
    incumbent = TRUE,
    office_website = "https://www.murkowski.senate.gov",
    campaign_name = "Lisa Murkowski for U.S. Senate",
    campaign_website = "https://join.lisamurkowski.com",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://give.victorypassport.com/murkowski/contribute/",
    # Turns out you did not need the location bit
    other_link = paste0(
      "https://mystique.victorypassport.com/pages/murkowski/contribute",
      "?location=https%3A%2F%2Fgive.victorypassport.com",
      "%2Fmurkowski%2Fcontribute"
    ),
    FEC_ID_cmte = "C00384529",
    FEC_ID_cand = "S4AK00099"
  ),
  # AZ: Arizona III (last updated Nov 4, 2020) =================================
  # McSally vs. Kelly
  # vs. Brittain vs. Larose vs. Vasquez vs. McCarthy
  # Some very... interesting things come up about more minor candidates
  # Can't find Larose nor Vasquez
  # There is also Sheila Bilyeu who has not yet filed with the FEC
  # http://www.sheilaforsenate.com/index.html
  # There is also Mohammad Arif who has not yet filed with the FEC
  # http://www.arif2020.com (not yet nominated I guess?)
  tibble(
    first_name = "Martha",
    last_name = "McSally",
    state = "AZ",
    party = "Rep",
    class = "III",
    incumbent = TRUE,
    office_website = "https://www.mcsally.senate.gov",
    campaign_name = "McSally For Senate, Inc.",
    campaign_website = "https://www.mcsallyforsenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = paste0(
    #   "https://secure.mcsallyforsenate.com/",
    #   "list/proc/fund/default.aspx?initiativekey=XGOTKJXGRWFS"
    # ),
    # Change documented Oct 3, 2019 to WinRed but on Twitter it says Jul 30
    # Some amounts changed. Monthly 15-25 disappeared
    other_link = c(
      # 35-50-100-250-500-1000-2800-5600
      # "https://secure.winred.com/martha-mcsally/donate",
      # Different amounts: 30-50-100-250-500-1000-2800-5600
      # "https://secure.winred.com/martha-mcsally/eoq/",
      # "https://secure.winred.com/martha-mcsally/arizona-red-search/",
      # "https://secure.winred.com/martha-mcsally/arizona-red/",
      "https://secure.winred.com/martha-mcsally/arizona/" # ,
      # "https://secure.winred.com/martha-mcsally/scotus/"
    ),
    FEC_ID_cmte = "C00666040",
    FEC_ID_cand = "S8AZ00221"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Kelly",
    state = "AZ",
    party = "Dem",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Kelly for Senate",
    campaign_website = "https://markkelly.com/",
    actblue_link = c(
      "mek-website" # ,
      # "ads-mek-evergreensearch-dtd",
      # "adopt-mark-kelly",
      # "markkelly2020"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00696526",
    FEC_ID_cand = "S0AZ00350"
  ),
  tibble(
    first_name = "Craig",
    last_name = "Brittain",
    state = "AZ",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brittain for Senate",
    campaign_website = "http://brittainforsenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 5-10-20-50-100-200-500-1000-2700
    other_link = "https://donorbox.org/embed/brittain",
    FEC_ID_cmte = "C00656298",
    FEC_ID_cand = "S8AZ00189"
  ),
  tibble(
    first_name = "Daniel",
    last_name = "McCarthy",
    state = "AZ",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Demand Daniel for US Senate",
    campaign_website = "https://demanddaniel.com",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented on Jan 29, 2020
    # other_link = "https://daniel.mccarthyarizonasenate.com/donate",
    # 25-50-100-250-1000-2800-5600
    other_link = "https://www.mccarthyarizonasenate.com/donate",
    FEC_ID_cmte = "C00717991",
    FEC_ID_cand = "S0AZ00376"
  ),
  # AZ: Arizona I (last updated Nov 7, 2020) ===================================
  # Not up for reelection 2020.
  # Kyrsten Sinema is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Kyrsten",
    last_name = "Sinema",
    state = "AZ",
    party = "Dem",
    class = "I",
    incumbent = TRUE,
    office_website = "https://www.sinema.senate.gov",
    campaign_name = "Sinema for Arizona",
    campaign_website = "https://kyrstensinema.com/",
    actblue_link = "ks-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00508804",
    FEC_ID_cand = "S8AZ00197"
  ),
  # AR: Arkansas II (last updated Nov 4, 2020) =================================
  # Cotton vs. Whitfield
  tibble(
    first_name = "Tom",
    last_name = "Cotton",
    state = "AR",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.cotton.senate.gov",
    campaign_name = "",
    campaign_website = "https://www.tomcotton.com/",
    # Change documented to WinRed on Oct 28, 2019
    # 25-50-75-100-250-500-1000-2800-5600 to 25-50-100-250-500-1000-2800
    actblue_link = NA,
    anedot_link = NA,
    # other_link = paste0(
    #   "https://secure.tomcotton.com/list/proc/splash/",
    #   "default.aspx?InitiativeKey=4PGFPRVX0NET"
    # ),
    other_link = c(
      "https://secure.winred.com/tom-cotton/donate" # ,
      # 2019 link that I did not catch
      # paste0(
      #   "https://secure.tomcotton.com/list/ads/donation1/?",
      #   "initiativekey=TBJKGGFOE0ST"
      # )
    ),
    # Also https://secure.tomcotton.com/bumper-sticker/default.aspx?
    # InitiativeKey=1D6ORAHYUJSZ
    # 1 sticker = 5 USD
    FEC_ID_cmte = "C00499988",
    FEC_ID_cand = "S4AR00103"
  ),
  tibble(
    first_name = "Ricky",
    last_name = "Harrington",
    state = "AR",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dan Whitfield for Arkansans, Senate 2020",
    campaign_website = "https://www.rickyharringtonsenate.com",
    actblue_link = NA,
    anedot_link = "ricky-dale-harrington-jr-for-us-2020/donate",
    other_link = NA,
    FEC_ID_cmte = "C00726422",
    FEC_ID_cand = "S0AR00218"
  ),
  # Mahony withdrew as of Nov 12, 2019
  tibble(
    first_name = "Josh",
    last_name = "Mahony",
    state = "AR",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mahony for U.S. Senate",
    campaign_website = "https://joshuamahony.com/",
    actblue_link = "mahony-for-u-s--senate-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00704098",
    FEC_ID_cand = "S0AR00200"
  ),
  tibble(
    first_name = "Daniel",
    last_name = "Whitfield",
    state = "AR",
    party = "Ind",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dan Whitfield for Arkansans, Senate 2020",
    campaign_website = "https://danwhitcongress.us",
    actblue_link = NA,
    anedot_link = NA,
    # 10-50-100-250-500-1000-2800
    other_link =
      "https://donorbox.org/daniel-whitfield-for-arkansans-senate-2020",
    FEC_ID_cmte = "C00726422",
    FEC_ID_cand = "S0AR00218"
  ),
  # AR: Arkansas III (last updated Nov 4, 2020) ================================
  # Not up for reelection 2020.
  # John Boozman is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "John",
    last_name = "Boozman",
    state = "AR",
    party = "Rep",
    class = "III",
    incumbent = TRUE,
    office_website = "www.boozman.senate.gov",
    campaign_name = "Boozman for Arkansas",
    campaign_website = "http://www.boozmanforarkansas.com/",
    # Change documented from Spark to Anedot
    actblue_link = NA,
    anedot_link = "boozman-for-arkansas/donate",
    # other_link = paste0(
    #   "https://spark.widgetmakr.com/render/",
    #   "3cdf407d-511d-438a-ba2b-b61a3e7668a9?hostUrl=",
    #   "https%3A%2F%2Fwww.boozmanforarkansas.com%2Fdonate"
    # ),
    FEC_ID_cmte = "C00476317",
    FEC_ID_cand = "S0AR00150"
  ),
  # CA: California I (last updated Nov 4, 2020) ================================
  # Not up for reelection 2020.
  # Dianne Feinstein is uncontested
  # (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Dianne",
    last_name = "Feinstein",
    state = "CA",
    party = "Dem",
    class = "I",
    incumbent = TRUE,
    office_website = "https://www.feinstein.senate.gov/public/",
    # Originally Dianne for California
    campaign_name = "Feinstein for Senate 2024",
    campaign_website = "https://feinsteinforca.com/",
    # Still the 2018 link, still 10-25-50-100-500-1000-2700
    actblue_link = "df-2018-launch",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00539890",
    FEC_ID_cand = "S0CA00199"
  ),
  # CA: California III (last updated Nov 7, 2020) ==============================
  # Not up for reelection 2020.
  # Harris vs. Williams vs. Bradley
  # Cannot find Williams (see https://ballotpedia.org/B._John-Michael_Williams)
  # Bradley now running for CA-33, Harris effectively uncontested
  # Now that Harris is VP candidate... but no tenable candidates on FEC.
  # https://www.nytimes.com/2020/08/13/us/california-senate-kamala-harris.html
  tibble(
    first_name = "Kamala",
    last_name = "Harris",
    state = "CA",
    party = "Dem",
    class = "III",
    incumbent = TRUE,
    office_website = "www.harris.senate.gov/contact",
    campaign_name = "Kamala Harris for Senate",
    campaign_website = "https://kamalaharris.org/",
    # Originally, yes, but this has now concluded, moving on to presidential
    # Harris is still retiring presidential race debts as of Jan 29, 2020
    # Now website leads directly to joebiden.com
    actblue_link = c(
      "kdh-website-header",
      "kdh-em-jan20",
      "kdh-em-feb20",
      "kdh-em-mar20",
      "kdh-em-may20",
      "kdh-harrison-march20-split"
    ), # "kdharrisforca"
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00571919",
    FEC_ID_cand = "S6CA00584"
  ),
  # Taking out of database (https://ballotpedia.org/B._John-Michael_Williams)
  tibble(
    first_name = "JM", # Brant John-Michael, goes by J.M.
    last_name = "Williams",
    state = "CA",
    party = "Ind",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "J.M. Williams for California",
    campaign_website = "https://www.facebook.com/JMForCalifornia/",
    # Cannot find a contribution link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00694737",
    FEC_ID_cand = "S2CA00856"
  ),
  # Not sure who WHITNEY, ELLERTON PRATT MARK MR. III is; cannot find
  # James P. Bradley now running for House CA-33
  # Sarah Sun Liew now running for House CA-33
  # Peter Y. Liu is... https://twitter.com/peterliucagov18?lang=en
  # Also ran for 2018 CA Governor, 2014 Oakland mayor...
  # Lucero Wiley ran for state senate in 2019 and campaign is stopped
  # Link is broken; taking off database
  tibble(
    first_name = "Akinyemi",
    last_name = "Agbede",
    state = "CA",
    party = "Dem",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "VOTE DR. AKINYEMI AGBEDE FOR UNITED STATES SENATE 2022",
    campaign_website = "https://drakinyemiagbedeforunitedstatessenate2022.com/",
    # Cannot find a contribution link
    # Same person running in FL and CA House?
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00698282",
    FEC_ID_cand = "S6CA00634"
  ),
  # CO: Colorado II (last updated Jul 5, 2021) =================================
  # Gardner vs. Johnston vs. Hickenlooper
  # vs. Baer vs. Romanoff vs. Walsh vs. Madden vs. Spaulding vs. Burnes
  # vs. Williams vs. Zornio vs. Bray vs. Warren vs. Garcia vs. Leitzel
  # vs. Milton vs. Pottratz vs. Blanton vs. Kombo vs. Bellamy
  # vs. Burgess vs. Rodriguez vs. Goldfischer vs. Camp vs. Underwood vs. Doane
  # --- Johnston dropped out on Sep 3, 2019
  # --- Baer dropped out to support Hickenlooper on Sep 12, 2019.
  # --- Walsh dropped out to support Hickenlooper on Sep 11, 2019.
  #     Originally ActBlue was 1904-announcement, with 20.20, 50, 100, 250, 500
  # --- Burnes dropped out by Sep 11, 2019, not sure when
  # --- Williams dropped out on Nov 27, 2019
  # --- Leitzel dropped out by Sep 11, 2019, not sure when
  # --- Pottratz dropped out by Sep 11, 2019, not sure when
  # --- Blanton dropped out by Sep 11, 2019, not sure when
  # --- Kombo dropped out on Apr 5, 2019
  # --- Bellamy https://www.facebook.com/vmbellamy/ no real website.
  # --- Burgess dropped out on Sep 20, 2019
  # --- Rodriguez's website at https://www.joshuaforcolorado.com/index.html
  #     is completely empty.
  # --- Camp seems to be running for House (felon?!)
  # --- Dupre seems not viable https://twitter.com/margotdupre1
  # --- Cannot find Boyington
  # --- Doane website not available at https://www.doane4colorado.com
  tibble(
    first_name = "Cory",
    last_name = "Gardner",
    state = "CO",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.gardner.senate.gov",
    campaign_name = "Cory Gardner for Senate",
    campaign_website = "https://www.corygardnerforsenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      "https://secure.winred.com/cory-gardner/thank-you-co"
      # "https://secure.winred.com/cory-gardner/website",
      # "https://secure.winred.com/cory-gardner/thank-you-website",
      # "https://secure.winred.com/cory-gardner/donate"
    ),
    FEC_ID_cmte = "C00492454",
    FEC_ID_cand = "S4CO00395"
  ),
  tibble(
    first_name = "John",
    last_name = "Hickenlooper",
    state = "CO",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Hickenlooper for Colorado",
    campaign_website = "https://www.hickenlooper.com/",
    actblue_link = c(
      "jwh" # , # At least from Apr 15, by archive.org
      # "hickenlooperforco-web-donate",
      # "hickenlooper-colorado2020",
      # "hickenlooperforco-em-launch",
      # "hickenlooperforco-em-dec",
      # "hickenlooperforco-em-nov",
      # "hickenlooperforco-em-oct",
      # "hickenlooperforco-em-sep",
      # "hickenlooperforco-em-aug",
      # "jwh-ads-200401-search-dd",
      # "join-team-hick", # Google ad
      # "hickenlooperforco-signup-rd",
      # "hickenlooperforco-soc",
      # "hickenlooperforco-om-dd-g-nat" # ,
      # "hickenlooperforco-em-harrison",
      # "jwh-em-191216-mm",
      # "jwh-em-191217-mm",
      # "jwh-em-191218-mm",
      # "jwh-em-191219-mm",
      # "jwh-em-191220-eoy",
      # "jwh-em-191223-eoy",
      # "jwh-em-191226-eoy",
      # "jwh-em-191227-eoy",
      # "jwh-em-191228-2things",
      # "jwh-em-191228-tk",
      # "jwh-em-191229-typical",
      # "jwh-em-191230-eoy",
      # "jwh-em-191231-percent-lift",
      # "jwh-em-191231-lousy",
      # "jwh-em-191231-this",
      # "jwh-em-200102-ty",
      # "jwh-em-200107-tv",
      # "jwh-em-200214-4",
      # "jwh-em-200215-lme",
      # "jwh-em-200218-hick",
      # "jwh-em-200218",
      # "jwh-em-200219-short-nd",
      # "jwh-em-200219",
      # "jwh-em-200220-pre",
      # "jwh-em-200221",
      # "jwh-em-200224",
      # "jwh-em-200225",
      # "jwh-em-200225-eom2",
      # "jwh-em-200226",
      # "jwh-em-200226-tweet",
      # "jwh-em-200227-me",
      # "jwh-em-200227-lousy",
      # "jwh-em-200228-ugh",
      # "jwh-em-200228-ftn",
      # "jwh-em-200229-last",
      # "jwh-em-200229-dad",
      # "jwh-em-200302-cal",     # Split with Cunningham
      # "jwh-em-200305-trump",
      # "jwh-em-200309-bullock", # Split with Bullock
      # "jwh-em-200309-invite",
      # "jwh-em-200310-trump",
      # "jwh-em-200311",
      # "jwh-em-200311-short",
      # "jwh-em-200312-lme",
      # "jwh-em-200315-ad",
      # "jwh-em-200315-last",
      # "jwh-em-200318",
      # "jwh-em-200323-3things",
      # "jwh-em-200323-mcconnell-pac",
      # "jwh-em-200324-me",
      # "jwh-em-200324-explain",
      # "jwh-em-200325-mcconnell",
      # "jwh-em-200326-truth",
      # "jwh-em-200326",
      # "jwh-em-200327-4",
      # "jwh-em-200328-me",
      # "jwh-em-200328-public",
      # "jwh-em-200329",
      # "jwh-em-200330",
      # "jwh-em-200330-behind",
      # "jwh-em-200331"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00716720",
    FEC_ID_cand = "S0CO00575"
  ),
  # Johnston dropped out
  # Romanoff lost, now website is down
  tibble(
    first_name = "Andrew",
    last_name = "Romanoff",
    state = "CO",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Romanoff for Colorado",
    campaign_website = "https://andrewromanoff.com/",
    actblue_link = c(
      "romanoff2020",
      "home-web-splash",
      "romanoff-2020"
    ),
    anedot_link = NA,
    other_link = NA,
    # Ran in 2010 as well. Empty string to fend off issues...
    FEC_ID_cmte = c("C00696724", "C00467118", ""),
    FEC_ID_cand = "S0CO00286"
  ),
  tibble(
    first_name = "Dan",
    last_name = "Baer",
    state = "CO",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dan Baer for US Senate",
    campaign_website = "https://danforcolorado.com/",
    actblue_link = "baer-web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00703033",
    FEC_ID_cand = "S0CO00500"
  ),
  # Walsh dropped out
  # Madden lost, now ActBlue down
  tibble(
    first_name = "Alice",
    last_name = "Madden",
    state = "CO",
    party = "",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Alice Madden for U.S. Senate",
    campaign_website = "https://www.alicemadden.com/",
    actblue_link = "madden-web-quick",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00706044",
    FEC_ID_cand = "S0CO00534"
  ),
  # Lost, now ActBlue down
  tibble(
    first_name = "Stephany Rose",
    last_name = "Spaulding",
    state = "CO",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Stephany Rose Spaulding for Colorado",
    campaign_website = "https://stephanyforco.com/",
    actblue_link = c(
      "madden-web-quick",
      "srs_website",
      "srs_runwithme"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00700708",
    FEC_ID_cand = "S0CO00484"
  ),
  # Burnes dropped out
  # Withdrew as of Nov 27, 2019
  tibble(
    first_name = "Angela",
    last_name = "Williams",
    state = "CO",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "http://angela4colo.com/",
    # angela-williams-2 was state senator link
    # Now angela-williams-senate (Oct 6, 2019)
    # Now back to angela-williams-2
    actblue_link = "angela-williams-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711408",
    FEC_ID_cand = "S0CO00559"
  ),
  # Contribution page gone (Jun 30, 2020)
  tibble(
    first_name = "Trish",
    last_name = "Zornio",
    state = "CO",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Zornio for U.S. Senate",
    campaign_website = "https://zornio2020.com/",
    actblue_link = c(
      "zorniohome" # ,
      # "zornio-endofjuly"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00694844",
    FEC_ID_cand = "S0CO00450"
  ),
  # Lost primaries
  tibble(
    first_name = "Diana",
    last_name = "Bray",
    state = "CO",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Diana Bray for U.S. Senate",
    campaign_website = "https://dianaforcolorado.com/",
    actblue_link = "dianaforco",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00700948",
    FEC_ID_cand = "S0CO00492"
  ),
  # Lost primaries
  tibble(
    first_name = "Lorena",
    last_name = "Garcia",
    state = "CO",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lorena for US Senate",
    campaign_website = "https://lorenaforsenate.com/",
    actblue_link = c(
      "lorena-for-senate",
      "putpeople1st",
      "sponsorsignatures"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00691089",
    FEC_ID_cand = "S0CO00435"
  ),
  # Contribution page gone (Jun 30, 2020)
  tibble(
    first_name = "Michelle",
    last_name = "Warren",
    state = "CO",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Michelle Ferrigno Warren for US Senate",
    campaign_website = "https://michelleforcolorado.com/",
    actblue_link = "michelle-ferrigno-warren-for-us-senate-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00714832",
    FEC_ID_cand = "S0CO00567"
  ),
  # Turned into House candidate CO-03
  tibble(
    first_name = "Critter", # Christopher, goes by Critter
    last_name = "Milton",
    state = "CO",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Critter Milton for Colorado",
    campaign_website = "https://crittermilton.com",
    actblue_link = "crittermilton",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00719823",
    FEC_ID_cand = "S0CO00591"
  ),
  # Leitzel dropped out
  # Pottratz dropped out
  # Blanton dropped out
  # Kombo dropped out
  # Bellamy cannot be found
  # Burgess dropped out
  # Rodriguez and Camp do not seem to be viable candidates
  # Contribution page gone (Jun 30, 2020)
  tibble(
    first_name = "Dave", # David, goes by Dave
    last_name = "Goldfischer",
    state = "CO",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "DG For Colorado",
    campaign_website = "https://www.goldfischer2020.com",
    # Unusual combination of 50-100-250-1000-20.20-2800-15
    actblue_link = "dg-for-colorado-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00730929",
    FEC_ID_cand = "S0CO00625"
  ),
  tibble(
    first_name = "Erik",
    last_name = "Underwood",
    state = "CO",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Erik Underwood for U.S. Senate",
    campaign_website = "https://www.erikunderwood.com",
    actblue_link = "underwoodforsenate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00735274", "C00613794"),
    FEC_ID_cand = "S6CO00333"
  ),
  tibble(
    first_name = "Raymon",
    last_name = "Doane",
    state = "CO",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Doane 4 Colorado",
    campaign_website = "https://www.doane4colorado.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00743211",
    FEC_ID_cand = "S0CO00674"
  ),
  # CO: Colorado III (last updated Nov 7, 2020) ================================
  # Not up for reelection 2020.
  # Michael Bennet is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Michael",
    last_name = "Bennet",
    state = "CO",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.bennet.senate.gov",
    campaign_name = "Bennet for Colorado",
    campaign_website = "https://michaelbennet.com/",
    # Currently running for president
    # Now dropped out and the Senate link is restored
    actblue_link = "michael-bennet-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00458398",
    FEC_ID_cand = "S0CO00211"
  ),
  # CT: Connecticut I (last updated Nov 7, 2020) ===============================
  # Not up for reelection 2020.
  # Christopher Murphy is uncontested
  # (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Chris",
    last_name = "Murphy",
    state = "CT",
    party = "Dem",
    class = "I",
    incumbent = TRUE,
    office_website = "www.murphy.senate.gov",
    campaign_name = "Chris Murphy for U.S. Senator",
    campaign_website = "https://chrismurphy.com/",
    actblue_link = "re-elect-murphy",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00492645",
    FEC_ID_cand = "S2CT00132"
  ),
  # CT: Connecticut III (last updated Nov 7, 2020) =============================
  # Not up for reelection 2020.
  # Richard Blumenthal vs. Hyde vs. Flynn
  # Can't find Hyde. https://twitter.com/rfhyde1? Flynn is...
  # https://www.facebook.com/John-Flynn-for-US-Senator-2022-1993571740719177/
  tibble(
    first_name = "Richard",
    last_name = "Blumenthal",
    state = "CT",
    party = "Dem",
    class = "III",
    incumbent = TRUE,
    office_website = "https://www.blumenthal.senate.gov",
    campaign_name = "Blumenthal for Connecticut",
    campaign_website = "https://richardblumenthal.com/",
    actblue_link = "blumenthal",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00492991",
    FEC_ID_cand = "S0CT00177"
  ),
  # DE: Delaware II (last updated Nov 4, 2020) =================================
  # Christopher Coons was uncontested
  # (no candidates registered as of Oct 6, 2019)
  # Now Coons vs. Scarane vs. Turley vs. Witzke
  tibble(
    first_name = "Christopher",
    last_name = "Coons",
    state = "DE",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.coons.senate.gov",
    campaign_name = "Chris Coons for Delaware",
    campaign_website = "https://www.chriscoons.com/",
    actblue_link = "chriscoons.com",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00475392",
    FEC_ID_cand = "S0DE00092"
  ),
  tibble(
    first_name = "Lauren",
    last_name = "Witzke",
    state = "DE",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lauren Witzke for US Senate 2020",
    campaign_website = "http://laurenwitzke.com",
    actblue_link = NA,
    # Also PayPal, prompted to give 20 USD
    # Change to Anedot documented Feb 27, 2020
    anedot_link = NA, # "laurenwitzke/donate",
    # Change to revv documented on Sep 9, 2020
    other_link = "https://secure.laurenwitzke.com/contribute",
    FEC_ID_cmte = "C00732982",
    FEC_ID_cand = "S0DE00183"
  ),
  tibble(
    first_name = "Jessica",
    last_name = "Scarane",
    state = "DE",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jess Scarane for Delaware",
    campaign_website = "https://www.jessfordelaware.com",
    actblue_link = c(
      "jess-website",
      "jess-twitter",
      "jess-facebook"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00727263",
    FEC_ID_cand = "S0DE00175"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Turley",
    state = "DE",
    party = "Ind",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Turley for US Senate 2020",
    campaign_website = "https://www.turley2020.com",
    actblue_link = NA,
    anedot_link = NA,
    # No amount prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?token=fMPcJS2BIV5wcVtvRqS5IOq",
      "jaFZoxIS-R2Ss5IEdOkOPjh0HwjowelcnrYJCsGFVsHcsCW&country.x=US&locale.x="
    ),
    FEC_ID_cmte = "C00726638",
    FEC_ID_cand = "S0DE00167"
  ),
  tibble(
    first_name = "Jim",
    last_name = "DeMartino",
    state = "DE",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jim DeMartino for U.S. Senate",
    campaign_website = "https://votedemartino.com/",
    actblue_link = NA,
    anedot_link = "demartino-for-us-senate/website-splash",
    other_link = NA,
    FEC_ID_cmte = "C00740837",
    FEC_ID_cand = "S0DE00191"
  ),
  tibble(
    first_name = "Nadine",
    last_name = "Frost",
    state = "DE",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nadine Frost for US Senate",
    campaign_website = "http://nadinefrostforussenate.com/",
    # Can't find donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00679738",
    FEC_ID_cand = "S8DE00160"
  ),
  # DE: Delaware I (last updated Nov 7, 2020) ==================================
  # Not up for reelection 2020.
  # Thomas Carper is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Thomas",
    last_name = "Carper",
    state = "DE",
    party = "Dem",
    class = "I",
    incumbent = TRUE,
    office_website = "www.carper.senate.gov",
    campaign_name = "Tom Carper for Senate",
    campaign_website = "https://www.carperfordelaware.com",
    actblue_link = "thomas-carper-3",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00349217",
    FEC_ID_cand = "S8DE00079"
  ),
  # FL: Florida I (last updated Nov 7, 2020) ===================================
  # Not up for reelection 2020.
  # Marco Rubio was uncontested (no candidates registered as of Sep 11, 2019)
  # Now Rubio vs. Barberena
  # Also Josue Larose, but he's an election fraudster??
  tibble(
    first_name = "Marco",
    last_name = "Rubio",
    state = "FL",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.rubio.senate.gov",
    campaign_name = "Marco Rubio for Senate",
    campaign_website = "https://marcorubio.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      "https://secure.winred.com/marco-rubio-for-senate/website",
      # Victory passport got added(?!) leading from the front page
      # 10-25-50-100-250-1000-2800, it could have been there for a long time
      # archived page source however is a class gform so difficult to tell link
      paste0(
        "https://mystique.victorypassport.com/pages/rubio/website?",
        "location=https%3A%2F%2Fgive.victorypassport.com%2F",
        "rubio%2Fdonate-website"
      )
    ),
    FEC_ID_cmte = "C00620518",
    FEC_ID_cand = "S0FL00338"
  ),
  # Will revive when showing more viability
  tibble(
    first_name = "Carlos",
    last_name = "Barberena",
    state = "FL",
    party = "Non",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Carlos for Senate",
    campaign_website = "http://carlosforsenate.com",
    # Website is currently empty
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00726125",
    FEC_ID_cand = "S2FL00433"
  ),
  # FL: Florida III (last updated Nov 7, 2020) =================================
  # Not up for reelection 2020.
  # Rick Scott is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Rick",
    last_name = "Scott",
    state = "FL",
    party = "Rep",
    class = "III",
    incumbent = TRUE,
    office_website = "www.rickscott.senate.gov",
    campaign_name = "Rick Scott for Florida",
    campaign_website = "https://rickscottforflorida.com/",
    actblue_link = NA,
    # Change documented to WinRed, Sep 9, 2020
    anedot_link = NA, # "teamrickscott/donate",
    other_link = "https://secure.winred.com/teamrickscott/donate",
    FEC_ID_cmte = "C00676965",
    FEC_ID_cand = "S8FL00273"
  ),
  # GA: Georgia II (last updated Jul 5, 2021) ==================================
  # Perdue vs. Ossoff
  # vs. Hazel vs. Tomlinson vs. Riggs Amico vs. Terry vs. Johnson vs. Shouse
  # vs. Williams vs. Miller vs. Amun vs. Dejesus vs. Kilby vs. Jowers
  # vs. Lieberman vs. Johnson-Shealey vs. Dillard Smith vs. McGuire vs. Winfield
  # Last updated Jul 5, 2021
  tibble(
    first_name = "David",
    last_name = "Perdue",
    state = "GA",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.perdue.senate.gov",
    campaign_name = "Perdue for Senate",
    campaign_website = "https://perduesenate.com/",
    actblue_link = NA,
    # Suddenly WinRed to Anedot?! Sep 9, 2020 ---> no evidence in archives, what
    # Back to WinRed, Nov 7, 2020, at least from Oct 4, 2020
    anedot_link = "perdue-for-senate/c6e7bf7090044b6644958",
    other_link = c(
      "https://secure.winred.com/david-perdue/perdueforsenate" # ,
      # "https://secure.winred.com/david-perdue/donate"
      # Same link
      # "https://secure.winred.com/perdueforsenate"
    ),
    FEC_ID_cmte = "C00547570",
    FEC_ID_cand = "S4GA11285"
  ),
  tibble(
    first_name = "Jon",
    last_name = "Ossoff",
    state = "GA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jon Ossoff for Senate",
    campaign_website = "https://electjon.com/",
    actblue_link = c(
      "ms_hp_ossoff" # ,
      # "ms_fr_ossoff_q32019_dd_foundingdonor_a",
      # "ms_fr_ossoff_q32019_dd_mcconnell_a",
      # "ms_fr_ossoff_q32019_dd_senateblue_a",
      # "ms_social_ossoff_2019",
      # "jon-ossoff-1"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00718866",
    FEC_ID_cand = "S8GA00180"
  ),
  tibble(
    first_name = "Shane",
    last_name = "Hazel",
    state = "GA",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shane Hazel for Senate",
    campaign_website = "https://shanehazel.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA, ## Patreon
    FEC_ID_cmte = "C00718866",
    FEC_ID_cand = "S8GA00180"
  ),
  # Website now effectively down
  tibble(
    first_name = "Teresa",
    last_name = "Tomlinson",
    state = "GA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Teresa Tomlinson for Senate",
    campaign_website = "https://teresatomlinson.com/",
    actblue_link = "teresatomlinson1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00701383",
    FEC_ID_cand = "S0GA00393"
  ),
  # ActBlue now down
  tibble(
    first_name = "Sarah",
    last_name = "Riggs Amico",
    state = "GA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sarah Riggs Amico for Senate",
    campaign_website = "https://sarahforgeorgia.com/",
    actblue_link = "sarahforgeorgia",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00712349",
    FEC_ID_cand = "S0GA00401"
  ),
  # Ted Terry withdrew Jan 26, 2020 (the Queer Eye mayor...)
  # Cannot find anything for Shouse! Yet there are contributions...
  # Elaine Whigham Williams not a viable candidate
  # https://www.facebook.com/elainewhighamwilliamsforpresident/
  # Ervan Katari Miller also not a viable candidate
  # https://ballotpedia.org/Ervan_Katari_Miller
  tibble(
    first_name = "Akhenaten",
    last_name = "Amun",
    state = "GA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Amun 4 US Senate 2020",
    campaign_website = "http://www.amun4senate.com/",
    actblue_link = "campaign-committee-for-akhenaten-amun-us-senate-2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00683870",
    FEC_ID_cand = "S0GA00351"
  ),
  tibble(
    first_name = "Marckeith",
    last_name = "DeJesus",
    state = "GA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vote DeJesus",
    campaign_website = "https://www.votedejesus.com/",
    # Site under construction, no donation link
    actblue_link = "dejesus-for-us-senate-2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00690974",
    FEC_ID_cand = "S0GA00377"
  ),
  # Cannot find Kilby
  # Jowers only has a Facebook, no link
  tibble(
    first_name = "Darrell",
    last_name = "McGuire",
    state = "GA",
    party = "Write-in",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "McGuire For Senator",
    campaign_website = "https://www.mcguire4senate562.com",
    actblue_link = NA,
    anedot_link = NA,
    # 5-10-25-50-100-150-200-250-500-1000
    other_link = "https://donorbox.org/mcguire-for-u-s-senate",
    FEC_ID_cmte = "C00729285",
    FEC_ID_cand = "S0GA00518"
  ),
  tibble(
    first_name = "Maya",
    last_name = "Dillard Smith",
    state = "GA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Maya for Georgia",
    # Website is empty so link to Twitter... announced Jan 20, 2020
    campaign_website = "https://votemaya.com",
    actblue_link = "maya-for-georgia-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00728782",
    FEC_ID_cand = "S0GA00500"
  ),
  # GA: Georgia III (last updated Nov 4, 2020) =================================
  # Johnny Isakson to retire at end of year (literally announced 2 weeks ago)
  # Isakson is replaced by Kelley Loeffler on Jan 6, 2020 (mega donor?)
  # Doug Collins soon incoming...
  # Last updated Jan 31, 2020
  # No candidates registered but that's because this is a special election
  tibble(
    first_name = "Kelly",
    last_name = "Loeffler",
    state = "GA",
    party = "Rep",
    class = "III",
    incumbent = TRUE,
    office_website = "https://www.loeffler.senate.gov/",
    campaign_name = "Georgians for Kelly Loeffler",
    campaign_website = "https://kellyforsenate.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      "https://secure.winred.com/georgians-for-kelly/donate/" # ,
      # "https://secure.winred.com/georgians-for-kelly/homepage-donate"
    ),
    FEC_ID_cmte = "C00729608",
    FEC_ID_cand = "S0GA00526"
  ),
  tibble(
    first_name = "Raphael",
    last_name = "Warnock",
    state = "GA",
    party = "Dem",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Warnock for Georgia",
    # No donation links yet
    campaign_website = "https://warnockforgeorgia.com",
    # "https://secure.warnockforgeorgia.com/page/s/wfg-join-the-team-gs-acq",
    actblue_link = c(
      "warnock-for-georgia_web", # 5-10-25-50-100-250
      "warnock-for-georgia_social"
      # "warnock-for-georgia-1",   # 25-100-250-1000
      # "wfg_ads-a"                # 25-50-100-250-500-1000-2800
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00736876",
    FEC_ID_cand = "S0GA00559"
  ),
  tibble(
    first_name = "Doug",
    last_name = "Collins",
    state = "GA",
    party = "Rep",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Doug Collins for Congress",
    campaign_website = "https://collinsforga.com",
    actblue_link = NA,
    # WinRed to Anedot again. Why?
    anedot_link = "collins-for-senate/website",
    other_link = "https://secure.winred.com/collins-for-senate-inc/donate",
    FEC_ID_cmte = "C00736983",
    FEC_ID_cand = "S0GA00567"
  ),
  tibble(
    first_name = "Matt",
    last_name = "Lieberman",
    state = "GA",
    party = "Dem",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lieberman for Senate",
    campaign_website = "https://liebermanforsenate.com",
    actblue_link = c(
      "lieberman-om",
      "matt-lieberman-for-senate-1"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00721480",
    FEC_ID_cand = "S0GA00468"
  ),
  tibble(
    first_name = "Wayne", # Arthur Wayne, goes by Wayne
    last_name = "Johnson",
    state = "GA",
    party = "Rep",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wayne Johnson for US Senate",
    campaign_website = "https://www.johnsonsenate.com",
    actblue_link = NA,
    anedot_link = NA,
    # 10-15-20-25-50-100-250-500-1000 Needs a separate scraper
    other_link = "https://www.johnsonsenate.com/contribute-now",
    FEC_ID_cmte = "C00726695",
    FEC_ID_cand = "S0GA00492"
  ),
  tibble(
    first_name = "Tamara",
    last_name = "Johnson-Shealey",
    state = "GA",
    party = "Dem",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tamara for Georgia",
    campaign_website = "https://www.tamaraforgeorgia.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "token=4lUx95QrcrUmXvAI98higfw-yE6V_XKulrw0o1nkkkONLKP2ES6sQ3dt",
      "ObssEi7KEvMOqm&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00726075",
    FEC_ID_cand = "S0GA00484"
  ),
  tibble(
    first_name = "Al", # Elbert
    last_name = "Bartell",
    state = "GA",
    party = "Ind",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bartell for US Senate",
    campaign_website = "https://bartellforsenate.com",
    actblue_link = NA,
    anedot_link = "bartell-for-u-s-senate-committee/donate",
    other_link = NA,
    FEC_ID_cmte = "C00694463",
    FEC_ID_cand = "S0GA00443"
  ),
  tibble(
    first_name = "Richard",
    last_name = "Winfield",
    state = "GA",
    party = "Dem",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Winfield for Senate",
    campaign_website = "https://www.winfieldforsenate.com",
    actblue_link = "winfield-for-senate-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00733030",
    FEC_ID_cand = "S0GA00542"
  ),
  tibble(
    first_name = "Ed",
    last_name = "Tarver",
    state = "GA",
    party = "Dem",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = NA,
    campaign_website = "https://www.tarverforsenate.com",
    ## 2800-1000-500-250-100-25-10
    actblue_link = "tarver-for-senate-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00737700",
    FEC_ID_cand = "S0GA00583"
  ),
  tibble(
    first_name = "Kandiss",
    last_name = "Taylor",
    state = "GA",
    party = "Rep",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = NA,
    campaign_website = "http://kandisstaylor.com/",
    actblue_link = NA,
    anedot_link = NA,
    ## 25-50-100-250-500-1000-2800
    other_link = 
      "https://secure.winred.com/kandiss-taylor-for-u-s-senate/donate",
    FEC_ID_cmte = "C00741215",
    FEC_ID_cand = "S0GA00641"
  ),
  tibble(
    first_name = "Annette Davis",
    last_name = "Jackson",
    state = "GA",
    party = "Rep",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = NA,
    campaign_website = "https://annettedjackson.wixsite.com/ussenate",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA, ## no data
    FEC_ID_cand = "S0GA00716"
  ),
  tibble(
    first_name = "Joy Felicia",
    last_name = "Slade",
    state = "GA",
    party = "Dem",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = NA,
    campaign_website = NA, ## no website
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA, ## no data
    FEC_ID_cand = "S0GA00708"
  ),
  tibble(
    first_name = "Derrick",
    last_name = "Grayson",
    state = "GA",
    party = "Rep",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = NA,
    campaign_website = "https://www.graysonforga.com/",
    actblue_link = NA,
    anedot_link = NA,
    ## 1-5-10-25-50-100-250-1000-2500
    other_link = "https://www.graysonforga.com/donate",
    FEC_ID_cmte = c("C00600759", "C00545111"),
    FEC_ID_cand = "S4GA11236"
  ),
  tibble(
    first_name = "Jamesia",
    last_name = "James",
    state = "GA",
    party = "Dem",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = NA,
    campaign_website = "https://www.jamesiajames4ussenate.com/",
    ## 25-100-250-1000
    actblue_link = "jamesia-james-for-us-senate-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00741058",
    FEC_ID_cand = "S0GA00609"
  ),
  tibble(
    first_name = "Deborah",
    last_name = "Jackson",
    state = "GA",
    party = "Dem",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = NA,
    ## main site: 101-250-1000-500-2800-5600 (weird)
    campaign_website = "https://deborahforgeorgia.com/",
    actblue_link = NA,
    ## !! Democrat but Anedot
    ## 101-250-500-1000-2800
    anedot_link = "deborah-jackson-for-us-senate/donate",
    other_link = NA,
    FEC_ID_cmte = "C00738203",
    FEC_ID_cand = "S0GA00591"
  ),
  # HI: Hawaii I (last updated Nov 7, 2020) ====================================
  # Not up for reelection 2020.
  # Mazie Hirono is uncontested (no candidates registered as of Sep 11, 2019)
  # Not even Hirono is on the FEC list. What?
  tibble(
    first_name = "Mazie",
    last_name = "Hirono",
    state = "HI",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.hirono.senate.gov",
    campaign_name = "Mazie for Hawaii",
    campaign_website = "https://mazieforhawaii.com/",
    actblue_link = c(
      "mkh-web-200430-homepage" # , # 5-10-25-50-100-250-500
      # "mkh" # 25-100-250-1000
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "",
    FEC_ID_cand = "S2HI00106"
  ),
  # HI: Hawaii III (last updated Nov 7, 2020) ==================================
  # Not up for reelection 2020.
  # Brian Schatz is uncontested (no candidates registered as of Sep 11, 2019)
  tibble(
    first_name = "Brian",
    last_name = "Schatz",
    state = "HI",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.schatz.senate.gov",
    campaign_name = "Schatz for Senate",
    campaign_website = "https://brianschatz.com/",
    actblue_link = NA,
    anedot_link = NA,
    # anedot_link = "brian-schatz-3", # Google search
    # Rare non-ActBlue donation link.
    # 25-100-1000-5600-50-250-2800
    other_link =
      "https://contribute.brianschatz.com/page/contribute/contribute",
    FEC_ID_cmte = "C00540732",
    FEC_ID_cand = "S4HI00136"
  ),
  # ID: Idaho II (last updated Nov 7, 2020) ====================================
  # Risch vs. Harris vs. Writz vs. Oler vs. Vandermaas vs. Fleming
  tibble(
    first_name = "Jim",
    last_name = "Risch",
    state = "ID",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.risch.senate.gov",
    campaign_name = "Jim Risch for US Senate",
    campaign_website = "https://www.senatorrisch.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-75-100-250-500-1000-2800-5600
    # Still campaign solutions on official website
    other_link = c(
      paste0(
        "https://secure.campaignsolutions.com/jimrisch/donation1/",
        "?initiativekey=N4MUBY3F3R6T"
      ) # ,
      # "https://secure.winred.com/risch/donate"
    ),
    FEC_ID_cmte = "C00440362",
    FEC_ID_cand = "S8ID00092"
  ),
  tibble(
    first_name = "Paulette",
    last_name = "Jordan",
    state = "ID",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Paulette for Senate",
    campaign_website = "https://pauletteforsenate.com/",
    actblue_link = c(
      "pauletteforsenate2020" # ,
      # "paulette_2020_d2d"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00738757",
    FEC_ID_cand = "S0ID00180"
  ),
  # Website now down
  tibble(
    first_name = "Nancy",
    last_name = "Harris",
    state = "ID",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nancy Harris for Senate",
    campaign_website = "https://nancyharrisforsenate.com/",
    actblue_link = "harrisforsenate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00697045",
    FEC_ID_cand = "S0ID00149"
  ),
  # ActBlue now down
  tibble(
    first_name = "Jim",
    last_name = "Vandermaas",
    state = "ID",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jim Vandermaas for US Senate",
    campaign_website = "http://votevandermaas.co",
    actblue_link = "votevandermaas# ",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00723320",
    FEC_ID_cand = "S0ID00164"
  ),
  tibble(
    first_name = "Raymond",
    last_name = "Writz",
    state = "ID",
    party = "Con",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    # Ah... interesting looking website...
    campaign_website = "http://www.rayjwritz.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 10, 25, 50
    other_link =
      "https://secure.piryx.com/donate/zwLppdZu/Raymond-J-Writz-for-US-Senate/",
    FEC_ID_cmte = "C00629048",
    FEC_ID_cand = "S6ID00104"
  ),
  # Now running for State House 31B
  tibble(
    first_name = "Travis",
    last_name = "Oler",
    state = "ID",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Travis Oler for US Senate",
    campaign_website = "https://www.travisoler.com",
    actblue_link = "travis-oler-for-us-senate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00718809",
    FEC_ID_cand = "S0ID00156"
  ),
  # Website now down
  tibble(
    first_name = "Natalie",
    last_name = "Fleming",
    state = "ID",
    party = "Unf",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Natalie Fleming for Congress",
    campaign_website = "http://natalie4congress.com",
    # I don't see a donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00726877",
    FEC_ID_cand = "S0ID00172"
  ),
  # ID: Idaho III (last updated Nov 7, 2020) ===================================
  # Not up for reelection 2020.
  # Mike Crapo is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Mike",
    last_name = "Crapo",
    state = "ID",
    party = "Rep",
    class = "III",
    incumbent = TRUE,
    office_website = "https://www.crapo.senate.gov",
    campaign_name = "Mike Crapo for US Senate",
    campaign_website = "http://crapoforsenate.com/",
    actblue_link = NA,
    # Anedot ---> now PayPal, Anedot, *and* WinRed
    # Anedot: 100-250-500-1000-2000-2800-3000-5600
    anedot_link = "mike-crapo-for-us-senate/general-fund-cc5cc622d88c69aecf1aa",
    other_link = c(
      # WinRed: 25-50-100-250-500-1000-2800
      "https://secure.winred.com/mike-crapo-for-us-senate/donate",
      # PayPal: 10-20-50-100-250-500-other
      "http://crapoforsenate.com/donate/"
    ),
    FEC_ID_cmte = "C00330886",
    FEC_ID_cand = "S8ID00027"
  ),
  # IL: Illinois II (last updated Nov 7, 2020) =================================
  # Durbin vs. Hubbard
  # vs. Tarter vs. Curran vs. Stava-Murray vs. Jordan Lawlor
  # vs. Seppelfrick vs. Agun vs. Chlebek vs. Wilson
  # Stava-Murray, Seppelfrick, Agun, Jordan Lawlor withdrew
  tibble(
    first_name = "Dick",
    last_name = "Durbin",
    state = "IL",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = " www.durbin.senate.gov",
    campaign_name = "Dick Durbin U.S. Senator",
    # Website changed from "https://dickdurbin.com/",
    campaign_website = "https://www.durbinforsenate.com/",
    actblue_link = c(
      "dick-durbin" # ,
      # "dick-durbin-1",
      # "njdcpacdurbin"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00148999",
    FEC_ID_cand = "S6IL00151"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Curran",
    state = "IL",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Curran for U.S. Senate",
    campaign_website = "https://electcurran.com",
    actblue_link = NA,
    # I missed an Anedot link: 25-50-100-250-500-1000-2000-2800-5600
    anedot_link = "curran-for-us-senate/donate",
    # 25-50-100-250-500-1000-2000-2600
    # other_link = "https://secure.winred.com/markcurran/donate",
    # "https://politics.raisethemoney.com/curran-for-us-senate/?iframe=true",
    FEC_ID_cmte = "C00714998",
    FEC_ID_cand = "S0IL00550"
  ),
  tibble(
    first_name = "Peggy",
    last_name = "Hubbard",
    state = "IL",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Peggy Hubbard for U.S. Senate",
    campaign_website = "http://peggyhubbardforsenate.com/",
    # Change documented to Anedot on Jan 31, 2020
    # 25-50-100-250-500-1000-2800 to 5600-2800-1000-500-250-100-50-25
    actblue_link = NA,
    anedot_link = "peggy-hubbard-for-us-senate-inc/cddb8c38540a49bb66aa7",
    other_link = NA,
    # other_link = "https://hubbardforsenate.revv.co/contribute",
    FEC_ID_cmte = "C00696815",
    FEC_ID_cand = "S0IL00501"
  ),
  # Website now down
  tibble(
    first_name = "Thomas",
    last_name = "Tarter",
    state = "IL",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dr. Tom Tarter for U.S. Senate",
    campaign_website = "http://www.tarterforsenate.com/",
    actblue_link = NA,
    anedot_link = "citizens-for-tom-tarter/donate",
    other_link = NA,
    FEC_ID_cmte = "C00709105",
    FEC_ID_cand = "S0IL00527"
  ),
  tibble(
    first_name = "Casey",
    last_name = "Chlebek",
    state = "IL",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Casey for Senate",
    campaign_website = "https://caseyforsenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 5-25-50
    other_link = paste0(
      "https://www.paypal.com/cgi-bin/webscr?",
      "cmd=_s-xclick&hosted_button_id=V77PT2BD3M36Q&source=url"
    ),
    FEC_ID_cmte = "C00714998",
    FEC_ID_cand = "S0IL00550"
  ),
  tibble(
    first_name = "Willie",
    last_name = "Wilson",
    state = "IL",
    party = "Ind",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Willie Wilson for Senate 2020",
    campaign_website = "https://www.williewilsonforsenate2020.com",
    actblue_link = NA,
    anedot_link = NA,
    #        25-50-100-250-1000-2000-2800
    # now 10-25-50-100-250-1000-2000-2800
    other_link = "https://www.williewilsonforsenate2020.com/donate",
    FEC_ID_cmte = "C00717827",
    FEC_ID_cand = "S0IL00568"
  ),
  tibble(
    first_name = "Danny",
    last_name = "Malouf",
    state = "IL",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Danny 2020",
    campaign_website = "https://www.danny2020.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00697235",
    FEC_ID_cand = "S0IL00618"
  ),
  tibble(
    first_name = "David",
    last_name = "Black",
    state = "IL",
    party = "Grn",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Black for Senate",
    campaign_website = "https://www.davidblackforsenate.org",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "S0IL00634"
  ),
  # IL: Illinois III (last updated Nov 7, 2020) ================================
  # Not up for reelection 2020.
  # Tammy Duckworth is uncontested (no candidates as of Jan 29, 2020)
  tibble(
    first_name = "Tammy",
    last_name = "Duckworth",
    state = "IL",
    party = "Dem",
    class = "III",
    incumbent = TRUE,
    office_website = "www.duckworth.senate.gov",
    campaign_name = "Tammy for Illinois",
    campaign_website = "https://www.tammyduckworth.com/",
    actblue_link = "tduckworth",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00574889",
    FEC_ID_cand = "S6IL00292"
  ),
  # IN: Indiana I (last updated Nov 7, 2020) ===================================
  # Not up for reelection 2020.
  # Mike Braun is uncontested (no candidates registered as of Jan 29, 2020)
  # But he is actually no longer on the roster
  # Instead, Aleem Left Young... who has an attorney general race website
  # now down at http://aleemyoungforattroneygeneral.com/
  tibble(
    first_name = "Mike",
    last_name = "Braun",
    state = "IN",
    party = "Rep",
    class = "I",
    incumbent = TRUE,
    office_website = "https://www.braun.senate.gov/",
    campaign_name = "Mike Braun for Indiana",
    campaign_website = "https://www.mikebraunforindiana.com/",
    actblue_link = NA,
    # Change documented to WinRed on Jan 29, 2020, at least rom Jan 28
    # 25-50-100-500-1000-2700-2800 to 25-50-100-250-500-1000-2800
    # Note that 2700 is explicit about general 2018 debt retirement!!!
    anedot_link = NA,
    # anedot_link = "braun/donate",
    # Link is broken? Empty frame... ah, turns out, hidden Anedot: check source.
    # other_link = "https://www.mikebraunforindiana.com/donate/",
    other_link = "https://secure.winred.com/braun/donate",
    FEC_ID_cmte = "C00653147",
    FEC_ID_cand = "S8IN00171"
  ),
  # IN: Indiana III (last updated Nov 7, 2020) =================================
  # Not up for reelection 2020.
  # Todd Young was uncontested (no candidates registered as of Jan 29, 2020)
  # Now Young vs. Khaaliq
  tibble(
    first_name = "Todd",
    last_name = "Young",
    state = "IN",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.young.senate.gov",
    campaign_name = "Todd Young for US Senate",
    campaign_website = "https://www.toddyoung.org/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      "https://secure.winred.com/toddyoung"
      # This is the current link but same link really
      # "https://secure.winred.com/todd-young/toddyoung"
    ),
    FEC_ID_cmte = "C00459255",
    FEC_ID_cand = "S6IN00191"
  ),
  tibble(
    first_name = "Haneefah",
    last_name = "Khaaliq",
    state = "IN",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Haneefah Khaaliq for U.S. senate",
    # campaign_website = "https://twitter.com/HaneefahKhaaliq",
    campaign_website = "https://haneefahkhaaliq.org",
    actblue_link = "hoosiers-for-haneefah-1",
    anedot_link = NA,
    other_link = "https://haneefahkhaaliq.org/contribute",
    FEC_ID_cmte = "C00746842",
    FEC_ID_cand = "S2IN00133"
  ),
  # IA: Iowa II (last updated Nov 4, 2020) =====================================
  # Ernst vs. Greenfield
  # vs. Mauro vs. Graham vs. Herzog vs. Franken vs. Rieck
  tibble(
    first_name = "Joni",
    last_name = "Ernst",
    state = "IA",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.ernst.senate.gov",
    campaign_name = "Joni for Iowa",
    campaign_website = "https://joniernst.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      "https://secure.winred.com/joni-ernst/website" # ,
      # "https://secure.winred.com/joni-ernst/donate"
    ),
    FEC_ID_cmte = "C00546788",
    FEC_ID_cand = "S4IA00129"
  ),
  tibble(
    first_name = "Theresa",
    last_name = "Greenfield",
    state = "IA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Greenfield for Iowa",
    campaign_website = "https://greenfieldforiowa.com/",
    # Change documented Jan 28, 2020, but same amounts
    actblue_link = c(
      "tg-website-2020" # , # "tg-website",
      # "tg-search-q3",
      # "tg-search-q2",
      # "adopt-theresa-greenfield"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00708164",
    FEC_ID_cand = "S0IA00176"
  ),
  # ActBlue now down
  tibble(
    first_name = "Eddie",
    last_name = "Mauro",
    state = "IA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Eddie Mauro U.S. Senate",
    campaign_website = "https://eddiemauro.com/",
    actblue_link = "mauro-home",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00706648",
    FEC_ID_cand = "S0IA00150"
  ),
  # ActBlue now down
  tibble(
    first_name = "Kimberly",
    last_name = "Graham",
    state = "IA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kimberly for Iowa",
    campaign_website = "https://www.kimberlyforiowa.com/",
    actblue_link = c("kimberlyforiowa", "investments"),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00708065",
    FEC_ID_cand = "S0IA00168"
  ),
  tibble(
    first_name = "Suzanne",
    last_name = "Herzog",
    state = "IA",
    party = "Ind",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Suzanne Herzog for US Senate",
    campaign_website = "https://www.herzogforussenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 5-11-29-53-99
    other_link = "https://donorbox.org/suzanne-herzog-for-us-senate",
    FEC_ID_cmte = "C00711085",
    FEC_ID_cand = "S0IA00184"
  ),
  tibble(
    first_name = "Rick",
    last_name = "Stewart",
    state = "IA",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rick Stewart United States Senator for All Iowans",
    campaign_website = "https://www.rickstewart.com/",
    actblue_link = NA,
    anedot_link = c(
      "rick-stewart-us-senate/rick-stewart-for-u-s-senate-c0381d89f020185eea03f"
    ),
    # No prompts on Anedot but prompts on front page
    other_link = NA,
    FEC_ID_cmte = "C00753657",
    FEC_ID_cand = "S4IA00194"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Franken",
    state = "IA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Franken for Iowa",
    campaign_website = "https://frankenforiowa.org/",
    actblue_link = "franken-for-iowa-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00717157",
    FEC_ID_cand = "S0IA00200"
  ),
  # I lost Cal Woods, whose cal-woods-for-iowa-1 had 25-100-250-1000
  # IA: Iowa III (last updated Nov 4, 2020) ====================================
  # Not up for reelection 2020.
  # Chuck Grassley is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Chuck",
    last_name = "Grassley",
    state = "IA",
    party = "Rep",
    class = "III",
    incumbent = TRUE,
    office_website = "http://www.grassley.senate.gov",
    campaign_name = "Grassley Works",
    campaign_website = "https://grassleyworks.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500
    # other_link = "https://grassleyworks.com/contribute/index.php",
    # Change documented to WinRed on Sep 10, 2020
    # Trouble is, this splits contribution between whopping six diff Senate
    # campaigns: Grassley, Ernst, Gardner, Collins, Tillis, and John James
    # Existing in this form since at least Jul 11, 2020
    other_link = "https://secure.winred.com/grassley/2020",
    FEC_ID_cmte = "C00230482",
    FEC_ID_cand = "S0IA00028"
  ),
  # KS: Kansas II (last updated Jul 5, 2021) ===================================
  # Pat Roberts retiring. Open seat candidates.
  # Marshall vs. Wagle
  # vs. Laturner vs. Grissom vs. Lindstrom vs. Kobach vs. Reddi vs. Buckley
  # vs. Boyda vs. Pruitt vs. Adams vs. Tillman vs. Tuten vs. Bollier
  # Adams is not connecting at https://eadamsforussenate.com
  tibble(
    first_name = "Barbara",
    last_name = "Bollier",
    state = "KS",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bollier for Kansas",
    campaign_website = "https://bollierforkansas.com",
    actblue_link = "bb4ks-web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00723122",
    FEC_ID_cand = "S0KS00323"
  ),
  tibble(
    first_name = "Roger",
    last_name = "Marshall",
    state = "KS",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kansas for Marshall",
    campaign_website = "https://kansansformarshall.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      "https://secure.winred.com/marshall/we-did-it",
      "https://secure.winred.com/marshall/donate"
    ),
    FEC_ID_cmte = "C00576173",
    FEC_ID_cand = "S0KS00315"
  ),
  tibble(
    first_name = "Jason",
    last_name = "Buckley",
    state = "KS",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jason Buckley 2020",
    campaign_website = "http://www.jasonbuckley2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00745141",
    FEC_ID_cand = "S0KS00356"
  ),
  tibble(
    first_name = "Susan",
    last_name = "Wagle",
    state = "KS",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wagle for Kansas",
    campaign_website = "http://www.teamwagle.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/wagle-for-kansas/donate",
    FEC_ID_cmte = "C00713503",
    FEC_ID_cand = "S0KS00299"
  ),
  tibble(
    first_name = "Jake",
    last_name = "LaTurner",
    state = "KS",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "LaTurner for Senate",
    campaign_website = "https://www.jakelaturner.com/",
    actblue_link = NA,
    anedot_link = "laturner/donate",
    # Change documented to WinRed on Sep 19, 2020
    # But primaries are already over
    other_link = "https://secure.winred.com/kansans-for-laturner/donate-sd",
    FEC_ID_cmte = "C00693572",
    FEC_ID_cand = "S0KS00208"
  ),
  # Withdrew and supported Barbara Bollier on Oct 21, 2019
  tibble(
    first_name = "Barry",
    last_name = "Grissom",
    state = "KS",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Grissom for U.S. Senate",
    campaign_website = "https://barrygrissom.com/",
    actblue_link = "websignupredir",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710780",
    FEC_ID_cand = "S0KS00232"
  ),
  tibble(
    first_name = "David",
    last_name = "Lindstrom",
    state = "KS",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lindstrom for Senate",
    campaign_website = "https://lindstromsenate.com/",
    actblue_link = NA,
    # Revv to Anedot documented Jan 31, 2020. Revv form is stopped
    # 100-250-500-1000-1500-2800
    anedot_link = "lindstrom-for-senate/donate",
    other_link = NA,
    # other_link = "https://secure.lindstromsenate.com/donate/",
    FEC_ID_cmte = "C00710939",
    FEC_ID_cand = "S0KS00240"
  ),
  tibble(
    first_name = "Kris",
    last_name = "Kobach",
    state = "KS",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kobach for Senate",
    campaign_website = "https://kriskobach.com/",
    actblue_link = NA,
    anedot_link = "kobach-for-senate/donate", # Still Anedot
    other_link = NA,
    FEC_ID_cmte = "C00711325",
    FEC_ID_cand = "S0KS00257"
  ),
  tibble(
    first_name = "Usha",
    last_name = "Reddi",
    state = "KS",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Reddi for Senate",
    campaign_website = "https://ushareddi.com/",
    actblue_link = "reddiforsenate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00717280",
    FEC_ID_cand = "S0KS00307"
  ),
  # Withdrew on Oct 10, 2019
  tibble(
    first_name = "Nancy",
    last_name = "Boyda",
    state = "KS",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Boyda for Senate",
    campaign_website = "https://www.boydaforsenate.com",
    actblue_link = "boyda",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00710384",
    FEC_ID_cand = "S0KS00224"
  ),
  # Now running for State Senate
  tibble(
    first_name = "Bryan",
    last_name = "Pruitt",
    state = "KS",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Pruitt 4 Kansas",
    campaign_website = "https://www.pruitt4kansas.com",
    actblue_link = NA,
    anedot_link = "pruitt-4-senate/donate",
    other_link = NA,
    FEC_ID_cmte = "C00712133",
    FEC_ID_cand = "S0KS00281"
  ),
  # Elliott Adams website https://eadamsforussenate.com/ down
  tibble(
    first_name = "Robert",
    last_name = "Tillman",
    state = "KS",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://roberttillman4ussenate.org/",
    actblue_link = NA,
    anedot_link = NA,
    # Rare Democratic Non-ActBlue page
    # No prompts
    other_link = paste0(
      "https://www.paypal.com/webapps/shoppingcart?",
      "flowlogging_id=86ac326e9d4b9&mfid=1568321968314_86ac326e9d4b9# /",
      "checkout/openButton"
    ),
    FEC_ID_cmte = "C00712455",
    FEC_ID_cand = "S0KS00265"
  ),
  # Cannot find Paul Tuten
  # KS: Kansas III (last updated Nov 4, 2020) ==================================
  # Not up for reelection 2020.
  # Jerry Moran is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Jerry",
    last_name = "Moran",
    state = "KS",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.moran.senate.gov",
    campaign_name = "Jerry Moran for U.S. Senate",
    campaign_website = "https://www.moranforkansas.com/",
    actblue_link = NA,
    # Still Anedot
    anedot_link = "moran-for-kansas-inc/donate-general-fund",
    other_link = NA,
    FEC_ID_cmte = "C00458315",
    FEC_ID_cand = "S0KS00091"
  ),
  # KY: Kentucky II (last updated Nov 4, 2020) =================================
  # McConnell vs. McGrath
  # vs. Cox vs. Morgan vs. Das vs. Ausbrooks vs. Broihier
  # vs. Frangedakis vs. Smith vs. Noble
  # Tobin ran in 1992... new filing on Jan 22, 2020, so let's wait
  # Noble withdrew
  # Last updated Jan 31, 2020
  tibble(
    first_name = "Mitch",
    last_name = "McConnell",
    state = "KY",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.mcconnell.senate.gov",
    campaign_name = "Team Mitch",
    campaign_website = "https://www.teammitch.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      # Different links, different amounts
      "https://secure.winred.com/mitch-mcconnell/mitchmcconnell",
      ## https://secure.winred.com/mitchmcconnell/donate equivalent
      "https://secure.winred.com/mitchmcconnell/winred-default"
    ),
    FEC_ID_cmte = "C00193342",
    FEC_ID_cand = "S2KY00012"
  ),
  tibble(
    first_name = "Amy",
    last_name = "McGrath",
    state = "KY",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Amy McGrath for Senate",
    campaign_website = "https://amymcgrath.com/",
    actblue_link = c(
      "am-website",
      "amymcgrathkentucky",
      "mcgrath2020-em-may",
      "mcgrath2020-em-june",
      "mcgrath2020-dd-om-gsearch"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711549",
    FEC_ID_cand = "S0KY00339"
  ),
  # Narrow window, withdrew ActBlue already
  # Jul 1 scraped: booker-for-kentucky 7-20.20-50-100-250
  # Jul 1 scraped: cbtwitter 5-25-50-75-100-100-250-500
  # Jul 1 scraped: ads-cb-dd-voterprotection 5-27.14-50-100-250-500-1000
  # Holy! That was close... shame I didn't manage to screenshot them
  tibble(
    first_name = "Charles",
    last_name = "Booker",
    state = "KY",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Amy McGrath for Senate",
    campaign_website = "https://bookerforkentucky.com/",
    actblue_link = c(
      "booker-for-kentucky",
      "cbtwitter",
      "ads-cb-dd-voterprotection"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00733261",
    FEC_ID_cand = "S0KY00420"
  ),
  tibble(
    first_name = "Mike",
    last_name = "Broihier",
    state = "KY",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mike Broihier for Senate",
    campaign_website = "https://www.mikeforky.com/",
    actblue_link = c(
      "broihier",
      "mike-broihier-for-senate-1",
      "mikeforky"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711556",
    FEC_ID_cand = "S0KY00321"
  ),
  # The entirety of Mary Ann Tobin's money is loans, so not entering
  # Also no contribution link at website
  # James Naren website is gone
  # Matt Jones already withdrew Nov 2019
  # Maggie Jo Hilliard website leads to anti-McConnell merch but nothing else
  tibble(
    first_name = "Wes",
    last_name = "Morgan",
    state = "KY",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "C. Wesley Morgan For Kentucky",
    campaign_website = "https://www.morganforsenate2020.com/",
    # Change documented to Anedot on Oct 28, 2019
    actblue_link = NA,
    anedot_link = "c-wesley-morgan-for-kentucky/donate-now",
    other_link = NA,
    # other_link = "https://politics.raisethemoney.com/morganforky/w",
    FEC_ID_cmte = "C00706762",
    FEC_ID_cand = "S0KY00297"
  ),
  tibble(
    first_name = "Steven",
    last_name = "Cox",
    state = "KY",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Steven Cox for US Senate",
    campaign_website = "https://coxforus.com/",
    actblue_link = "coxforky",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00662106",
    FEC_ID_cand = "S0KY00271"
  ),
  # Matt Jones withdrew on Nov 15, 2019
  tibble(
    first_name = "Eric",
    last_name = "Rothmuller",
    state = "KY",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Eric Rothmuller for U.S. Senate",
    campaign_website = "https://ericrothmuller.com",
    actblue_link = "eric-rothmuller-for-kentucky-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00719732",
    FEC_ID_cand = "S0KY00388"
  ),
  tibble(
    first_name = "Jimmy",
    last_name = "Ausbrooks",
    state = "KY",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jimmy Ausbrooks for Kentucky 2020",
    campaign_website = "https://jimmyausbrooks.com/",
    # Used to be no-prompt paypal, can no longer find that link
    actblue_link = c(
      "ausbrooks",
      "campaign-fund-of-jimmy-ausbrooks-1"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00708818",
    FEC_ID_cand = "S0KY00313"
  ),
  # Cannot find Karl Das
  tibble(
    first_name = "Paul",
    last_name = "Frangedakis",
    state = "KY",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dr. Paul Frangedakis for United States Senate Kentucky",
    campaign_website = "https://docpaulfrangedakis.weebly.com/",
    # No contribution link found
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00718833",
    FEC_ID_cand = "S0KY00347"
  ),
  tibble(
    first_name = "Bennie",
    last_name = "Smith",
    state = "KY",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bennie J. Smith for U.S. Senator 2020",
    campaign_website = "http://benniejsmith.com/",
    actblue_link = c(
      "truckin-for-change-ky-10",
      "truckin-for-change-ky-20",
      "truckin-for-change-ky-40",
      "truckin-for-change-ky-80",
      "truckin-for-change-ky-120",
      "truckin-for-change-ky-250",
      "truckin-for-change-ky-1000",
      "bennie-j--smith-for-u-s--senator-2020-1"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00713131", "C00713149", rep("", 6)),
    FEC_ID_cand = "S0KY00354"
  ),
  tibble(
    first_name = "Derek",
    last_name = "Petteys",
    state = "KY",
    party = "Ref",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Derek Petteys for U.S. Senate",
    campaign_website = "https://kysenator2020.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?token=kXsFaGCGa6JmuGuqaoo4xsQ0Cd2FL",
      "dlPruVsikDZjl3Y1N2mGwUCzHfyG_1dy5XqRRL7wG&country.x=US&locale.x="
    ),
    FEC_ID_cmte = "C00730473",
    FEC_ID_cand = "S0KY00412"
  ),
  # Edwards withdrew
  tibble(
    first_name = "Brad", # Stephen Bradley, goes by Brad
    last_name = "Barron",
    state = "KY",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brad Barron for Senate",
    campaign_website = "https://www.bradbarron2020.com",
    actblue_link = NA,
    # No prompt
    anedot_link = "brad-barron/donate",
    other_link = NA,
    FEC_ID_cmte = "C00723270",
    FEC_ID_cand = "S0KY00396"
  ),
  # KY: Kentucky III (last updated Nov 4, 2020) ================================
  # Not up for reelection 2020.
  # Paul Rand is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Paul",
    last_name = "Rand",
    state = "KY",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.paul.senate.gov",
    campaign_name = "Rand Paul for US Senate",
    campaign_website = "https://www.randpaul.com/",
    actblue_link = NA,
    # Slightly different picture but essentially same thing
    # No WinRed even yet
    anedot_link = "randpaul/donate",
    other_link = NA,
    FEC_ID_cmte = "C00496075",
    FEC_ID_cand = "S0KY00156"
  ),
  # LA: Louisiana II (last updated Nov 4, 2020) ================================
  # Bill Cassidy was uncontested (no candidates registered as of Jan 29, 2020)
  # Now Cassidy vs. Pierce vs. Wenstrup vs. Murphy vs. ...
  tibble(
    first_name = "Bill",
    last_name = "Cassidy",
    state = "LA",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.cassidy.senate.gov",
    campaign_name = "Bill Cassidy for Senate",
    campaign_website = "https://www.billcassidy.com/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = paste0(
    #   "https://secure.victorypassport.com/pages/cassidy/contribute",
    #   "?location=https%3A%2F%2Fcontribute.billcassidy.com%2Fcontribute"
    # ),
    # Change documented from victorypassport to WinRed on Oct 3, 2019
    # from 10-25-50-100-250-1000-2800 to
    # 10-25-50-100-500-1000-2800 so 250 jumped to 500 but similar otherwise
    # Looks like it was Sep 16, 2019
    other_link = "https://secure.winred.com/bill-cassidy/website",
    FEC_ID_cmte = "C00543983",
    FEC_ID_cand = "S4LA00107"
  ),
  tibble(
    first_name = "Adrian",
    last_name = "Perkins",
    state = "LA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Perkins for Louisiana",
    campaign_website = "https://perkinsforla.com",
    actblue_link = "perkins_web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00752535",
    FEC_ID_cand = "S0LA00386"
  ),
  tibble(
    first_name = "Derrick",
    last_name = "Edwards",
    state = "LA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vote Derrick Edwards",
    campaign_website = "https://www.votederrickedwards.com",
    actblue_link = "the-committee-to-elect-attorney-derrick-edwards-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00750109", "C00609313"),
    FEC_ID_cand = "S6LA00342"
  ),
  tibble(
    first_name = "Antoine",
    last_name = "Pierce",
    state = "LA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Antoine Pierce for U.S. Senate",
    campaign_website = "https://www.antoinepierce.com/",
    actblue_link = "antoine-pierce-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00700054",
    FEC_ID_cand = "S0LA00311"
  ),
  tibble(
    first_name = "Peter",
    last_name = "Wenstrup",
    state = "LA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Peter Wenstrup Teacher for Senate",
    campaign_website = "https://wenstrupforla.com/home/",
    actblue_link = "mrwenstruptothesenate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00747261",
    FEC_ID_cand = "S0LA00345"
  ),
  tibble(
    first_name = "Dustin",
    last_name = "Murphy",
    state = "LA",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dustin Murphy for United States Senate",
    campaign_website = "https://www.dustinbmurphy.com",
    actblue_link = NA,
    anedot_link = NA,
    ## 10-15-25-50-100-250-500-1000-1500
    other_link = "https://www.dustinbmurphy.com/contribute",
    FEC_ID_cmte = "C00747303",
    FEC_ID_cand = "S0LA00352"
  ),
  tibble(
    first_name = "David Drew",
    last_name = "Knight",
    state = "LA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = NA,
    campaign_website = "https://drew2020.com/",
    # 3-10-27-50-100-250-2700
    actblue_link = "drew-2020-l-l-c-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA, ## nothing
    FEC_ID_cand = "S0LA00402"
  ),
  # LA: Louisiana III (last updated Nov 4, 2020) ===============================
  # Not up for reelection 2020.
  # John Kennedy is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "John Neely",
    last_name = "Kennedy",
    state = "LA",
    party = "Rep",
    class = "III",
    incumbent = TRUE,
    office_website = "https://www.kennedy.senate.gov/public/",
    campaign_name = "John Kennedy for Us",
    campaign_website = "http://www.johnkennedy.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Originally anedot "john-kennedy-for-us-inc/john-kennedy-us-senate",
    # other_link = "https://secure.johnkennedy.com/john-kennedy-us-senate",
    # Change documented Oct 3, 2019 to WinRed
    # with same amounts of 25-50-100-500-1000-2800
    # Looks like it changed Sep 16
    other_link = "https://secure.winred.com/kennedy/donate",
    FEC_ID_cmte = "C00608398",
    FEC_ID_cand = "S4LA00065"
  ),
  # ME: Maine II (last updated Jul 5, 2021) ===================================
  # Collins vs. Gideon vs. Sweet vs. London vs. Kidman
  # I can't find Michael Bunker, Danielle VanHelsing, nor Jonathan Treacy
  # Cathleen London ended her campaign
  tibble(
    first_name = "Susan",
    last_name = "Collins",
    state = "ME",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.collins.senate.gov",
    campaign_name = "Collins for Senator",
    campaign_website = "https://www.susancollins.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Jan 21, 2021
    # 25-50-100-250-1000-2800-5600 from
    # 10-25-50-100-250-1000-2800-5600
    # At least from Dec 17, 2019
    # See https://twitter.com/WINRED/status/1207027156209086464
    other_link = c(
      "https://secure.winred.com/susan-collins/donate-contributebutton",
      "https://secure.winred.com/susan-collins/donate"
    ),
    # paste0(
    #   "https://mystique.victorypassport.com/pages/collins/main",
    #   "?location=https%3A%2F%2Fgive.victorypassport.com%2F",
    #   "collins%2Fdonate-today&location_url=aHR0cHM6Ly9naXZlLnZpY3Rvcnl",
    #   "wYXNzcG9ydC5jb20vY29sbGlucy9kb25hdGUtdG9kYXkvIw%3D%3D"
    # ),
    FEC_ID_cmte = "C00314575",
    FEC_ID_cand = "S6ME00159"
  ),
  tibble(
    first_name = "Sara",
    last_name = "Gideon",
    state = "ME",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sara Gideon for U.S. Senate",
    campaign_website = "https://saragideon.com/",
    actblue_link = c(
      # "sg-website",
      "gideon2020" # ,
      # "adopt-me-dem-susan-collins-seat",
      # "ads-sg-feb",
      # "ads-sg-jan",
      # "ads-sg-dec",
      # "ads-sg-nov"
      # "ads-sg-october"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "S0ME00111",
    FEC_ID_cand = "C00709899"
  ),
  tibble(
    first_name = "Lisa",
    last_name = "Savage",
    state = "ME",
    # Although classified as Unknown in the FEC database, clearly Green Party
    party = "Grn",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lisa for Maine",
    campaign_website = "https://www.lisaformaine.org",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-1000-2800
    other_link = "https://www.lisaformaine.org/donate",
    FEC_ID_cmte = "C00723411",
    FEC_ID_cand = "S0ME00137"
  ),
  tibble(
    first_name = "Ross",
    last_name = "LaJuenesse",
    state = "ME",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ross LaJeunesse for Maine",
    campaign_website = "https://www.rossformaine.com",
    # Missed LaJeunesse but 25-50-100-250-500-1000-2800
    actblue_link = "rl_newweb",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00726182",
    FEC_ID_cand = "S0ME00145"
  ),
  tibble(
    first_name = "Betsy",
    last_name = "Sweet",
    state = "ME",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Betsy Sweet for Senate",
    campaign_website = "https://betsysweet.com/",
    actblue_link = c(
      # Missed new link but 25-50-100-250-500-1000
      "sweet2020",
      # Old link is         25-50-100-250-1000-2800
      "betsysweetforsenate"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "S0ME00103",
    FEC_ID_cand = "C00709279"
  ),
  tibble(
    first_name = "Bre",
    last_name = "Kidman",
    state = "ME",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bre Kidman for Maine",
    campaign_website = "https://www.beekay4maine.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Another rare instance of non-ActBlue
    other_link = "https://www.beekay4maine.com/contribute",
    FEC_ID_cmte = "C00703413",
    FEC_ID_cand = "S0ME00087"
  ),
  tibble(
    first_name = "Derek",
    last_name = "Levasseur",
    state = "ME",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Derek for U.S. Senate",
    campaign_website = "https://derekellisforussenate.godaddysites.com",
    actblue_link = NA,
    anedot_link = "derekforsenate/donate",
    # GoFundMe
    other_link = "www.gofundme.com/f/derek-ellis-for-senator",
    FEC_ID_cmte = "C00706739",
    FEC_ID_cand = "S0ME00095"
  ),
  # ME: Maine I (last updated Nov 4, 2020) =====================================
  # Not up for reelection 2020.
  # Angus King is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Angus",
    last_name = "King",
    state = "ME",
    party = "Ind",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.king.senate.gov",
    campaign_name = "Angus King for Maine",
    campaign_website = "https://angusformaine.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 5-25-50-100-250-500-1000-2700
    other_link = "https://anguskingformaine.nationbuilder.com/",
    FEC_ID_cmte = "C00516047",
    FEC_ID_cand = "S2ME00109"
  ),
  # MD: Maryland I (last updated Nov 4, 2020) ==================================
  # Not up for reelection 2020.
  # Benjamin Cardin is uncontested (no candidates registered as of Jan 29, '20)
  # Cardin also not on FEC for 2024 cycle
  tibble(
    first_name = "Ben",
    last_name = "Cardin",
    state = "MD",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.cardin.senate.gov",
    campaign_name = "Ben Cardin for Senate",
    campaign_website = "https://bencardin.com/",
    actblue_link = "ben_cardin",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00411587",
    FEC_ID_cand = "S6MD03177"
  ),
  # MD: Maryland III (last updated Nov 7, 2020) ================================
  # Not up for reelection 2020.
  # Van Hollen was uncontested (no candidates registered as of Sep 11, 2019)
  # Now Van Hollen vs. Smith
  tibble(
    first_name = "Chris",
    last_name = "Van Hollen",
    state = "MD",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.vanhollen.senate.gov",
    campaign_name = "Van Hollen for Senate",
    campaign_website = "https://vanhollen.org/",
    actblue_link = "vanhollen",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00573758",
    FEC_ID_cand = "S6MD03441"
  ),
  tibble(
    first_name = "Michelle",
    last_name = "Smith",
    state = "MD",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Michelle L. Smith for Senate",
    # https://t.co/xFmzVTOlIL?amp=1 gives 404
    campaign_website = "https://twitter.com/michellesenate",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = "https://www.gofundme.com/f/michelle-l-smith-for-senate",
    FEC_ID_cmte = "C00724328",
    FEC_ID_cand = "S2MD00529"
  ),
  # MA: Massachusetts II (last updated Nov 7, 2020) ============================
  # Markey vs. Liss-Riordan
  # vs. Ayyadurai vs. Mayock vs. Pemberton vs. Gray vs. Kennedy
  # Can't find Frederick Mayock nor Andre Gray
  tibble(
    first_name = "Edward",
    last_name = "Markey",
    state = "MA",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.markey.senate.gov",
    campaign_name = "Ed Markey for US Senate",
    campaign_website = "https://www.edmarkey.com/",
    actblue_link = c(
      # Change documented within ActBlue
      # from 25-50-100-500-1000-2800-5600 to 10-25-50-100-250
      "ejm2020",
      "ed-markey-1",
      "ejmdefault"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00196774",
    FEC_ID_cand = "S4MA00028"
  ),
  # Filed Feb 18, 2020, somehow missed: primary winner
  tibble(
    first_name = "Kevin",
    last_name = "O'Connor",
    state = "MA",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kevin O'Connor for US Senate",
    campaign_website = "https://www.kocforsenate.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/kevinoconnor/donate",
    FEC_ID_cmte = "C00739177",
    FEC_ID_cand = "S0MA00232"
  ),
  # Withdrew
  tibble(
    first_name = "Shannon",
    last_name = "Liss-Riordan",
    state = "MA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://shannonforsenate.com/",
    actblue_link = "shannon-liss-riordan-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00703314",
    FEC_ID_cand = "S0MA00166"
  ),
  # Website came live at Sep 21, 2019. Now lost, but website still live.
  tibble(
    first_name = "Joe",
    last_name = "Kennedy",
    state = "MA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Kennedy for Massachusetts",
    campaign_website = "https://www.kennedyforma.com/",
    actblue_link = "stand-with-joe",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00512970", "C00717108"),
    # Well... FEC says "Candidate for Senate" but ID is still House. Weird.
    FEC_ID_cand = "S0MA00208"
  ),
  # Lost primary, but running as independent?
  tibble(
    first_name = "Shiva",
    last_name = "Ayyadurai",
    state = "MA",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shiva 4 Senate",
    campaign_website = "https://shiva4senate.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 5-10-25-50-100-250-500-1000-2700
    other_link = "https://shiva4senate.com/donate/",
    FEC_ID_cmte = "C00638148",
    FEC_ID_cand = "S8MA00268"
  ),
  tibble(
    first_name = "Andre", # goes by Maha Visnu
    last_name = "Gray",
    state = "MA",
    party = "Grn",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://mahaforsenate.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://www.gofundme.com/f/MahaExploratoryCommittee/donate?",
      "member=2534522"
    ),
    FEC_ID_cmte = "C00715581",
    FEC_ID_cand = "S0MA00190"
  ),
  # Withdrew Oct 14, 2019
  tibble(
    first_name = "Steve",
    last_name = "Pemberton",
    state = "MA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Steve Pemberton for U.S. Senate",
    campaign_website = "https://pembertonforma.com/",
    actblue_link = "ourchancema",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00709675",
    FEC_ID_cand = "S0MA00174"
  ),
  # MA: Massachusetts I (last updated Jul 5, 2021) =============================
  # Not up for reelection 2020.
  # Warren is uncontested (no candidates registered as of Jan 29, 2020)
  # Presidential race has concluded, but not yet on 2022 FEC cycle
  # David Armas is on FEC but cannot find him
  tibble(
    first_name = "Elizabeth",
    last_name = "Warren",
    state = "MA",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.warren.senate.gov",
    campaign_name = "",
    campaign_website = "https://elizabethwarren.com",
    # Looks like it'll be split into different destinations but no specifics
    actblue_link = "ew-web-warren-democrats",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00500843",
    FEC_ID_cand = "S2MA00170"
  ),
  # MI: Michigan II (last updated Jul 5, 2021) ================================
  # Peters vs. James vs. Carr vs. Wilus vs. Squier vs. Dern vs. Willis
  # Cannot find Wilus https://ballotpedia.org/Valgrie_Wilus empty page
  tibble(
    first_name = "Gary",
    last_name = "Peters",
    state = "MI",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.peters.senate.gov",
    campaign_name = "Gary Peters for U.S. Senate",
    campaign_website = "https://petersformichigan.com/",
    actblue_link = "gary-peters-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00437889",
    FEC_ID_cand = "S4MI00355"
  ),
  tibble(
    first_name = "John",
    last_name = "James",
    state = "MI",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John James for Senate",
    campaign_website = "https://johnjamesforsenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Originally revv https://secure.johnjamesforsenate.com/donate
    # with 25-50-100-250-500-2800-5600-11200
    # Change documented Oct 3, 2019
    # to https://secure.winred.com/john-james/donate-123
    # or https://secure.winred.com/john-james/donate, same amounts
    # 25-50-100-250-500-1000-2000
    other_link = c(
      "https://secure.winred.com/john-james/donate" # ,
      # "https://secure.winred.com/john-james/donate-321/?amount=5"
    ),
    FEC_ID_cmte = "C00651208",
    FEC_ID_cand = "S8MI00372"
  ),
  tibble(
    first_name = "Bob",
    last_name = "Carr",
    state = "MI",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elect Bob Carr Senator",
    campaign_website = "https://electbobcarr.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-1000-2700. It's a revv link
    other_link =
      "https://itsallaboutmichigan.revv.co/electbobcarr?utm_source=page_form",
    FEC_ID_cmte = "C00655217",
    FEC_ID_cand = "S4MI00298"
  ),
  tibble(
    first_name = "Marcia",
    last_name = "Squier",
    state = "MI",
    party = "Grn",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Noncorporate Pro-Peace People's Advocate for US Senate",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.crowdpac.com/campaigns/395494/marcia-t-squier",
    FEC_ID_cmte = NA,
    FEC_ID_cand = "S8MI00430"
  ),
  tibble(
    first_name = "Doug",
    last_name = "Dern",
    state = "MI",
    party = "Oth",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Doug Dern for Senate",
    campaign_website = "https://dougdern.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "S2MI00144"
  ),
  tibble(
    first_name = "Valerie",
    last_name = "Willis",
    state = "MI",
    party = "Oth",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Willis for U.S. Senate 2020",
    campaign_website = "https://willisforcongress.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00675256", "C00674556"),
    FEC_ID_cand = "S8MI00422"
  ),
  # MI: Michigan I (last updated Nov 7, 2020) ==================================
  # Not up for reelection 2020.
  # Debbie Stabenow is uncontested (no candidates registered as of Jan 29, '20)
  tibble(
    first_name = "Debbie",
    last_name = "Stabenow",
    state = "MI",
    party = "Dem",
    class = "I",
    incumbent = TRUE,
    office_website = "https://www.stabenow.senate.gov",
    campaign_name = "Stabenow for U.S. Senate",
    campaign_website = "https://debbiestabenow.com/",
    actblue_link = "debbiestabenow2019website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00344473",
    FEC_ID_cand = "S8MI00281"
  ),
  # MN: Minnesota II (last updated Nov 4, 2020) ================================
  # Smith vs. Lewis vs. Barrett vs. Washington vs. Steinberg
  # Cannot find Washington
  tibble(
    first_name = "Tina",
    last_name = "Smith",
    state = "MN",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.smith.senate.gov",
    campaign_name = "Tina for Minnesota",
    campaign_website = "https://tinaforminnesota.com/",
    actblue_link = c(
      "tina-smith" # ,
      # "afa-jeanneandtina" # Amy Klobuchar's appeal
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00663781",
    FEC_ID_cand = "S8MN00578"
  ),
  tibble(
    first_name = "Jason",
    last_name = "Lewis",
    state = "MN",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lewis for MN",
    campaign_website = "https://lewisformn.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      # Change documented within WinRed
      "https://secure.winred.com/jason-lewis-for-senate/thank-you-mn",
      "https://secure.winred.com/jason-lewis-for-senate/donate",
      "https://secure.winred.com/jason-lewis-for-senate/contribute"
    ),
    FEC_ID_cmte = "C00716860",
    FEC_ID_cand = "S0MN00328"
  ),
  tibble(
    first_name = "Rob",
    last_name = "Barrett",
    state = "MN",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rob Barrett for Senate",
    campaign_website = "https://www.robbarrettforsenate.com/",
    actblue_link = NA,
    anedot_link = "barrettsenate2020/donate", # Still Anedot
    other_link = NA,
    FEC_ID_cmte = "C00701094",
    FEC_ID_cand = "S0MN00294"
  ),
  tibble(
    first_name = "Oliver",
    last_name = "Steinberg",
    state = "MN",
    party = "Oth",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Oliver Steinberg for U.S. Senate",
    campaign_website = "https://jackherershero.org/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "S0MN00385"
  ),
  # Missed Forest Hyatt
  # MN: Minnesota I (last updated Nov 4, 2020) =================================
  # Not up for reelection 2020.
  # Amy Klobuchar is uncontested (no candidates registered as of Jan 29, 2020)
  # Running for president currently as of Jan 29, 2020.
  tibble(
    first_name = "Amy",
    last_name = "Klobuchar",
    state = "MN",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.klobuchar.senate.gov",
    campaign_name = "Klobuchar for Minnesota",
    campaign_website = "https://amyklobuchar.com",
    actblue_link = "kfm-webdonate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00431353",
    FEC_ID_cand = "S6MN00267"
  ),
  # MS: Mississippi II (last updated Jul 5, 2021) ==============================
  # Hyde-Smith vs. Espy vs. Edwards vs. Bohren
  tibble(
    first_name = "Cindy",
    last_name = "Hyde-Smith",
    state = "MS",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.hydesmith.senate.gov",
    campaign_name = "Cindy Hyde-Smith for United State Senate",
    campaign_website = "https://cindyhydesmith.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Oops, always look for inner frame with victorypassport...
    # Change documented to WinRed on Feb 27, 2020
    # at least from Jan 27, 2020
    # 10-25-50-100-250-1000-2700 to 10-25-50-100-250-1000-2800 (just the limit)
    other_link = c(
      # "https://mystique.victorypassport.com/pages/hydesmith/main-page-site?",
      # "location=https%3A%2F%2Fgive.victorypassport.com%2Fhydesmith%2Fsite&",
      # "location_url=aHR0cHM6Ly9naXZlLnZpY3RvcnlwYXNzcG9ydC5jb20vaHlkZXNtaX",
      # "RoL3NpdGUvIw%3D%3D",
      "https://secure.winred.com/cindy-hyde-smith/donate"
    ),
    FEC_ID_cmte = "C00675348",
    FEC_ID_cand = "S8MS00261"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Espy",
    state = "MS",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Espy for Senate",
    campaign_website = "http://www.espyforsenate.com/",
    # Currently website itself is broken so I can't find donation site
    actblue_link = "espy-web-2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00675884",
    FEC_ID_cand = "S8MS00287"
  ),
  tibble(
    first_name = "Jimmy",
    last_name = "Edwards",
    state = "MS",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jimmy Edwards for Senate",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "S0MS00086"
  ),
  tibble(
    first_name = "Jensen",
    last_name = "Bohren",
    state = "MS",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.vote4jensen.org",
    actblue_link = "donate4jensen",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00648659",
    FEC_ID_cand = "S8MS00220"
  ),
  # MS: Mississippi I (last updated Nov 7, 2020) ===============================
  # Not up for reelection 2020.
  # Roger Wicker is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Roger",
    last_name = "Wicker",
    state = "MS",
    party = "Rep",
    class = "I",
    incumbent = TRUE,
    office_website = "https://www.wicker.senate.gov",
    campaign_name = "Wicker for Senate",
    campaign_website = "http://www.wickerforsenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/wicker-for-senate/donate_home",
    FEC_ID_cmte = "C00443218",
    FEC_ID_cand = "S8MS00196"
  ),
  # MO: Missouri I (last updated Nov 7, 2020) ==================================
  # Not up for reelection 2020.
  # Hawley Josh is uncontested (no candidates registered as of Jan 29, 2020)
  # He himself not registered at FEC either
  tibble(
    first_name = "Josh",
    last_name = "Hawley",
    state = "MO",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.hawley.senate.gov",
    campaign_name = "Josh Hawley for Senate",
    campaign_website = "https://joshhawley.com/",
    actblue_link = NA,
    anedot_link = "josh2018/c7934f43d8d9231ab49a9",
    # Change documented to WinRed on Sep 16, 2020
    other_link = "https://secure.winred.com/josh-hawley-for-senate/donate",
    FEC_ID_cmte = "C00652727",
    FEC_ID_cand = "S8MO00160"
  ),
  # MO: Missouri III (last updated Nov 7, 2020) ================================
  # Not up for reelection 2020.
  # Roy Blunt was uncontested (no candidates registered as of Sep 11, 2019)
  # Now Blunt vs. Shepard, but cannot find Shepard
  tibble(
    first_name = "Roy",
    last_name = "Blunt",
    state = "MO",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.blunt.senate.gov",
    campaign_name = "Roy Blunt for Missouri",
    campaign_website = "https://www.royblunt.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Oct 26, 2019 with different amounts
    # 15-25-50-100-250-1000-2800 to 25-50-100-250-500-1000-2800
    # Same link but website ---> website2020
    other_link = "https://secure.winred.com/roy-blunt/website2020",
    # other_link = paste0(
    # "https://mystique.victorypassport.com/pages/blunt/",
    # "main-contribute?location=https%3A%2F%2Fgive.victorypassport.com",
    # "%2Fblunt%2Fdonate-today&location_url=aHR0cHM6Ly9naXZlLnZpY3Rv",
    # "cnlwYXNzcG9ydC5jb20vYmx1bnQvZG9uYXRlLXRvZGF5Lz9yZWN1cnJpbmc9dHJ1ZSM%3D"
    # ),
    FEC_ID_cmte = "C00304758",
    FEC_ID_cand = "S0MO00183"
  ),
  tibble(
    first_name = "Timothy",
    last_name = "Shepard",
    state = "MO",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Timothy Shepard Democrats",
    campaign_website = "https://tshepard.com",
    actblue_link = "tshepard-democrats",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00751156)",
    FEC_ID_cand = "S2MO00460"
  ),
  # MT: Montana II (last updated Nov 7, 2020) ==================================
  # Daines vs. Collins vs. Mues vs. Ballard vs. Knoles vs. Neumann
  # Ballard withdrew on Sep 13, 2019
  tibble(
    first_name = "Steve",
    last_name = "Daines",
    state = "MT",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.daines.senate.gov",
    campaign_name = "Steve Daines for Montana",
    campaign_website = "https://www.stevedaines.com/",
    # Change documented to WinRed on Oct 27, 2019
    # 25-50-75-100-250-500-1000-2800-5600 to 25-50-75-100-500-1000-2800-5600
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/steve-daines/donate",
    # other_link = paste0(
    #   "https://secure.stevedaines.com/proc/donate/donation1/",
    #   "?initiativekey=9ORMCUHOH2KW"
    # ),
    FEC_ID_cmte = "C00491357",
    FEC_ID_cand = "S2MT00096"
  ),
  # Donation link down
  tibble(
    first_name = "Steve",
    last_name = "Bullock",
    state = "MT",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Steve Bullock for U.S. Senate",
    campaign_website = "https://stevebullock.com",
    actblue_link = c(
      "sb-homepage" # ,
      # "ms-2020-a",
      # "ms_bullock_fr_q12020",
      # "ms_bullock_fr_q22020"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "S0MT00173",
    FEC_ID_cand = "S0MT00173"
  ),
  tibble(
    first_name = "Cora",
    last_name = "Neumann",
    state = "MT",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cora for Montana",
    campaign_website = "https://coraformontana.com",
    actblue_link = "cora-for-mt",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00721019",
    FEC_ID_cand = "S0MT00132"
  ),
  tibble(
    first_name = "Wilmot",
    last_name = "Collins",
    state = "MT",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wilmot for Montana",
    campaign_website = "https://wilmotcollins.com/",
    actblue_link = c(
      "wc-splash",
      "a-whole-lot-of-folks-for-wilmot-collins-1",
      "1908-social-wc",
      "mt4wilmot"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00706317",
    FEC_ID_cand = "S0MT00090"
  ),
  tibble(
    first_name = "John",
    last_name = "Mues",
    state = "MT",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John Mues for Senate",
    campaign_website = "https://muesformontana.com/",
    actblue_link = "jm4s",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00711978",
    FEC_ID_cand = "S0MT00108"
  ),
  # Withdrew Sep 13, 2019
  tibble(
    first_name = "Jack",
    last_name = "Ballard",
    state = "MT",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ballard for Montana",
    campaign_website = "https://ballardformontana.com/",
    actblue_link = "jackballard",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00714295",
    FEC_ID_cand = "S0MT00116"
  ),
  tibble(
    first_name = "Mike", # Michael, goes by Mike
    last_name = "Knoles",
    state = "MT",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mike Knoles for Montana Senate",
    campaign_website = "https://www.mikeknoles2020.com",
    actblue_link = "mikeknoles# ",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00720672",
    FEC_ID_cand = "S0MT00124"
  ),
  # Looks like I lost Daniel Larson
  # MT: Montana I (last updated Nov 7, 2020) ===================================
  # Not up for reelection 2020.
  # Jon Tester is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Jon",
    last_name = "Tester",
    state = "MT",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.tester.senate.gov",
    campaign_name = "John Tester U.S. Senate",
    campaign_website = "https://www.jontester.com/",
    actblue_link = c(
      # Changed back to jtdefault2018
      "jtdefault2018" # ,
      # "testerforsenate"#
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00412304",
    FEC_ID_cand = "S6MT00162"
  ),
  # NE: Nebraska II (last updated Jul 5, 2021) =================================
  # Sasse vs. Janicek
  # vs. Innis vs. Macek vs. Philips vs. Shelton vs. Siadek
  tibble(
    first_name = "Ben",
    last_name = "Sasse",
    state = "NE",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.sasse.senate.gov",
    campaign_name = "Ben Sasse for U.S. Senate",
    campaign_website = "https://teamsasse.com/",
    # Change documented to WinRed at least from Jan 27, 2020
    # 10-20-50-100-250-500-1000-2800-5600 to 25-50-100-250-500-1000-2800
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      "https://secure.winred.com/bensasse/donate-123",
      "https://secure.winred.com/bensasse/donate",
      "https://secure.winred.com/bensasse/donate-2020" # ,
      # paste0(
      #   "https://transaxt.com/Donate/4S2U6Z/BenSasseforNebraska/",
      #   "?utm_source=homepage"
      # )
    ),
    FEC_ID_cmte = "C00547976",
    FEC_ID_cand = "S4NE00090"
  ),
  tibble(
    first_name = "Chris",
    last_name = "Janicek",
    state = "NE",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Chris Janicek for Senate",
    campaign_website = "http://www.janicekforsenate2020.com/",
    # ActBlue is suspended but there's a new no-prompt PayPal
    # That's because he lost support of DNC perhaps?
    actblue_link = NA, # "janicekforsenate",
    # No prompts
    anedot_link = NA,
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "cmd=_s-xclick&hosted_button_id=7GX4UJ4WWF3RJ&source=url"
    ),
    FEC_ID_cmte = "C00675546",
    FEC_ID_cand = "S8NE00216"
  ),
  tibble(
    first_name = "Angie",
    last_name = "Philips",
    state = "NE",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Angie Philips for Senate 2020",
    campaign_website = "https://voteangieforsenate.com/",
    actblue_link = "angieforsenate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00714055",
    FEC_ID_cand = "S0NE00072"
  ),
  tibble(
    first_name = "Matt",
    last_name = "Innis",
    state = "NE",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Matt Innis for U.S. Senate",
    campaign_website = "https://mattinnis4senate.com/",
    actblue_link = NA,
    # No suggested amounts. Could be a good baseline.
    anedot_link = "matt-innis-for-us-senate-inc/donate",
    other_link = NA,
    FEC_ID_cmte = "C00716977",
    FEC_ID_cand = "S0NE00080"
  ),
  tibble(
    first_name = "Dennis Frank",
    last_name = "Macek",
    state = "NE",
    party = "Dem", ## Rep to Dem??
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dennis Macek for U.S. Senate",
    campaign_website = "http://dennisfrankmacekforsenate.com",
    # New ActBlue detected as of Sep 16, 2020, although already suspended
    # 25-100-250-1000 from source code
    actblue_link = "macek-for-senate-1",
    # No suggested amounts
    anedot_link = "g_o_o_d/macek",
    other_link = NA,
    FEC_ID_cmte = c("C00732743", "C00732735"),
    FEC_ID_cand = "S0NE00106"
  ),
  tibble(
    first_name = "Alisha",
    last_name = "Shelton",
    state = "NE",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Alisha Shelton for Senate",
    campaign_website = "https://sheltonforsenate.com",
    actblue_link = "sheltonforsenate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00723007",
    FEC_ID_cand = "S0NE00098"
  ),
  tibble(
    first_name = "Gene",
    last_name = "Siadek",
    state = "NE",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Siadek 2020",
    campaign_website = "https://www.siadek2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "S0NE00155"
  ),
  # NE: Nebraska I (last updated Nov 7, 2020) ==================================
  # Not up for reelection 2020.
  # Debra Fischer is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Deb",
    last_name = "Fischer",
    state = "NE",
    party = "Rep",
    class = "I",
    incumbent = TRUE,
    office_website = "https://www.fischer.senate.gov",
    campaign_name = "Deb Fischer for U.S. Senate",
    campaign_website = "https://www.fischerfornebraska.com/",
    actblue_link = NA,
    # Change detected to Anedot on Jan 29, 2020
    # At least from Jan 21, 2020
    # from 35-50-100-250-500-1000 to 5-10-25-50-100-250-500-1000
    anedot_link = NA,
    # anedot_link = "debfischer/donate",
    other_link = "https://secure.winred.com/debfischer/pg-website-donate",
    # Committee not detected
    FEC_ID_cmte = "C00498907",
    FEC_ID_cand = "S2NE00094"
  ),
  # NV: Nevada I (last updated Nov 7, 2020) ====================================
  # Not up for reelection 2020.
  # Jacky Rosen is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Jacky",
    last_name = "Rosen",
    state = "NV",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.rosen.senate.gov",
    campaign_name = "Jacky Rosen for US Senate",
    campaign_website = "https://www.rosenfornevada.com/",
    actblue_link = "rosen-homepage",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00606939",
    FEC_ID_cand = "S8NV00156"
  ),
  # NV: Nevada III (last updated Nov 7, 2020) ==================================
  # Not up for reelection 2020.
  # Catherine Cortez Masto is uncontested (no candidates as of Jan 29, 2020)
  tibble(
    first_name = "Catherine",
    last_name = "Cortez Masto",
    state = "NV",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.cortezmasto.senate.gov",
    campaign_name = "Catherine Cortez Masto for Senate",
    campaign_website = "https://catherinecortezmasto.com/",
    actblue_link = "ccmbutton",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00575548",
    FEC_ID_cand = "S6NV00200"
  ),
  # (no candidates registered as of Sep 11, 2019)
  # NH: New Hampshire II (last updated Nov 4, 2020) ============================
  # Shaheen vs. Messner vs. Bolduc vs. O'Brien vs. O'Donnell
  tibble(
    first_name = "Jeanne",
    last_name = "Shaheen",
    state = "NH",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.shaheen.senate.gov",
    campaign_name = "Shaheen for Senate",
    campaign_website = "https://jeanneshaheen.org/",
    # Oh wow. Amazing link...
    actblue_link = c(
      "shaheen-website" # ,
      # "homepagedefault" # ,
      # "afa-jeanneandtina" # Amy Klobuchar's appeal
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00457325",
    FEC_ID_cand = "S0NH00219"
  ),
  tibble(
    first_name = "Bryant Corky", # Goes by Corky
    last_name = "Messner",
    state = "NH",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Corky for Senate",
    campaign_website = "https://corkyforsenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    # anedot_link = "messner-for-senate/homepage_donate",
    other_link = paste0(
      "https://secure.winred.com/messner-for-senate/website_donate"
      # "https://secure.winred.com/",
      # "messner-nh-exploratory-committee/homepage_donate"
    ),
    FEC_ID_cmte = "C00713412",
    FEC_ID_cand = "S0NH00334"
  ),
  tibble(
    first_name = "Don",
    last_name = "Bolduc",
    state = "NH",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bolduc 2020",
    campaign_website = "https://www.donbolduc.com/",
    actblue_link = NA,
    anedot_link = NA,
    # This is a revv account
    other_link = c(
      "https://secure.donbolduc.com/donate/",
      "https://secure.donbolduc.com/action-803359"
    ),
    FEC_ID_cmte = "C00711010",
    FEC_ID_cand = "S0NH00326"
  ),
  # Website is still live but should have withdrawn
  tibble(
    first_name = "Bill",
    last_name = "O'Brien",
    state = "NH",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bill O'Brien for New Hampshire",
    campaign_website = "https://billobrien2020.com/",
    actblue_link = NA,
    # Change documented to Anedot on Jan 29, 2020
    # Change documented to WinRed on Feb 27, 2020
    # 35-50-100-250-1000 to 5-10-25-50-100-500-1000
    # anedot_link = "bill-o-brien-for-new-hampshire/primary",
    # Change documented *back* to Anedot on Sep 16, 2020
    anedot_link = "bill-o-brien-for-state-representative/donate",
    other_link = "https://secure.winred.com/billobrien/donate",
    # other_link = "https://billobrienfornh.revv.co/2020",
    FEC_ID_cmte = c("C00712091", "C00712083"),
    FEC_ID_cand = "S0NH00318"
  ),
  tibble(
    first_name = "Justin",
    last_name = "O'Donnell",
    state = "NH",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "O'Donnell for Liberty",
    campaign_website = "https://www.odonnell2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 20-50-100-500-1000-2000-2800
    other_link = "https://www.odonnell2020.com/donate",
    FEC_ID_cmte = "C00647842",
    FEC_ID_cand = "S0NH00300"
  ),
  # NH: New Hampshire III (last updated Nov 4, 2020) ===========================
  # Not up for reelection 2020.
  # Maggie Hassan is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Maggie",
    last_name = "Hassan",
    state = "NH",
    party = "Dem",
    class = "III",
    incumbent = TRUE,
    office_website = "https://www.hassan.senate.gov",
    campaign_name = "Maggie for NH",
    campaign_website = "http://maggiehassan.com/",
    actblue_link = "hassan_website",
    anedot_link = NA,
    other_link = NA,
    # Committee ID not found
    FEC_ID_cmte = "C00588772",
    FEC_ID_cand = "S6NH00091"
  ),
  # NJ: New Jersey II (last updated Jul 5, 2021) ===============================
  # Booker vs. Meissner vs. Singh
  # vs. McCormick vs. Vergara vs. Mehta vs. Rich vs. Hamm vs.
  # Bell vs. Rivera vs. Flanagan vs. Fernandez
  # Vergara is "write-in" <--- ?? Okay, can't find webpage
  # Jeff Bell deceased as of Feb 2018
  # Rivera Twitter locked, Facebook does not exist, can't find webpage
  tibble(
    first_name = "Cory",
    last_name = "Booker",
    state = "NJ",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.booker.senate.gov",
    campaign_name = "Cory Booker for Senate",
    campaign_website = "https://corybooker.com",
    # Dropped out of presidential race but still retiring primary debt
    # Now has Senate link
    actblue_link = "corybooker-2020-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00540500",
    FEC_ID_cand = "S4NJ00185"
  ),
  tibble(
    first_name = "Rik", # Rikin, goes by Rik
    last_name = "Mehta",
    state = "NJ",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Rik for New Jersey",
    campaign_website = "https://rikfornj.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/rik-mehta-for-senate/action-60545",
    FEC_ID_cmte = "C00722470",
    FEC_ID_cand = "S0NJ00217"
  ),
  # Eliminated in primaries but website is still live
  tibble(
    first_name = "Hirsh",
    last_name = "Singh",
    state = "NJ",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Singh for Senate",
    campaign_website = "https://www.hirshsingh.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Jan 29, 2020
    # from    20.20-75-150-500-1000-2800-5600
    # to 5-10-20.20-50-100-250-500-1000-1500-2800-5600
    # Change documented within WinRed on Sep 17, 2020
    # mobile-contribute --> contribute
    #         20.20-50-75-150-500-2800-5600
    # By mail contribution form wants
    #               50-100-250-500-750-1000-2000-2800
    # https://uploads-ssl.webflow.com/5caa72ded636b7acdc5f6bf0/
    # 5cc3e074b2ea2e16642f811f_Fillable%202019%20Contribution%20Form.pdf
    other_link = c(
      "https://secure.winred.com/hirshsingh/contribute",
      "https://secure.winred.com/hirshsingh/mobile-contribute"
    ),
    # other_link = "https://contribute.hirshsingh.com/donate",
    FEC_ID_cmte = "C00670737",
    FEC_ID_cand = "S0NJ00191"
  ),
  tibble(
    first_name = "Stuart",
    last_name = "Meissner",
    state = "NJ",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Stuart Meissner for US Senate",
    campaign_website = "https://www.meissnerforsenate.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/meissner-2020/contribute/",
    FEC_ID_cmte = c("C00716878", "C00547695"),
    FEC_ID_cand = "S4NJ00250"
  ),
  tibble(
    first_name = "Lisa",
    last_name = "McCormick",
    state = "NJ",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Election Fund of Lisa McCormick",
    campaign_website = "http://lisamccormick.org/",
    actblue_link = "lisa2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00611475",
    FEC_ID_cand = "S0NJ00175"
  ),
  tibble(
    first_name = "Gary",
    last_name = "Rich",
    state = "NJ",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gary Rich for US Senate",
    campaign_website = "https://www.garyrich.org",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-1000-2500
    other_link = "https://www.garyrich.org/donate",
    FEC_ID_cmte = "C00732156",
    FEC_ID_cand = "S0NJ00225"
  ),
  tibble(
    first_name = "Lawrence",
    last_name = "Hamm",
    state = "NJ",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://twitter.com/HammForSenate",
    # No donation link found
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00732594",
    FEC_ID_cand = "S0NJ00233"
  ),
  tibble(
    first_name = "Natalie",
    last_name = "Rivera",
    state = "NJ",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Natalie Rivera 4 Senate",
    campaign_website = "https://natalierivera85.wixsite.com/natalie4ussenate",
    actblue_link = NA,
    anedot_link = NA,
    # No amount prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?token=o1lc6MrayKxaZ5zcUpobjKErK",
      "-QEmSWte0wk0bLyF38zzqPp8E7VDL2f86RuwTmpWCMQyG&country.x=US&locale.x="
    ),
    FEC_ID_cmte = "C00677088",
    FEC_ID_cand = "S8NJ00533"
  ),
  tibble(
    first_name = "Tricia",
    last_name = "Flanagan",
    state = "NJ",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tricia Flanagan for U.S. Senate",
    campaign_website = "https://flanaganforsenate.org/",
    actblue_link = NA,
    anedot_link = NA,
    # No suggested amounts
    other_link = paste0(
      "https://www.paypal.com/donate/?token=",
      "uSg9ntcfLC9EVkQhR9akakjXhlydcoKS4Mh-lKWMCh-6kCP4QWopGrUC_Dhf_HjQdG2AiG&",
      "country.x=US&locale.x="
    ),
    FEC_ID_cmte = "C00674564",
    FEC_ID_cand = "S8NJ00541"
  ),
  tibble(
    first_name = "Veronica",
    last_name = "Fernandez",
    state = "NJ",
    party = "Oth",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Veronica 2020",
    campaign_website = "https://www.veronica2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00742569",
    FEC_ID_cand = "S0NJ00258"
  ),
  # NJ: New Jersey I (last updated Nov 4, 2020) ================================
  # Not up for reelection 2020.
  # Bob Menendez is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Bob",
    last_name = "Menendez",
    state = "NJ",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.menendez.senate.gov",
    campaign_name = "Menendez for Senate",
    campaign_website = "https://www.menendezfornj.com/",
    actblue_link = "menendez_web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00264564",
    FEC_ID_cand = "S6NJ00289"
  ),
  # NM: New Mexico II (last updated Jul 5, 2021) ===============================
  # Udall is retiring: open seat candidates.
  # Lujan vs. Toulouse Oliver
  # vs. Clarkson vs. Rich vs. Haqani vs. Martinez
  # vs. Perkins vs. Sanchez vs. Ronchetti vs. Walsh
  # Giovanni Haqani withdrew and is supporting Toulouse Oliver
  # Cannot yet find Sanchez, only entered Jan 6, 2020
  tibble(
    first_name = "Ben Ray",
    last_name = "Lujan",
    state = "NM",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ben Ray Lujan for U.S. Senate",
    campaign_website = "https://www.benrlujan.com/",
    actblue_link = c(
      # 14-26-49-62-100-250-1000
      "web-popup", # wow what an amazing link he procured
      "brlhomepage"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00443689",
    FEC_ID_cand = "S0NM00058"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Ronchetti",
    state = "NM",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Ronchetti for New Mexico",
    campaign_website = "https://www.markronchetti.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/mark-ronchetti-for-new-mexico/donate",
    FEC_ID_cmte = "C00733469",
    FEC_ID_cand = "S0NM00124"
  ),
  tibble(
    first_name = "Tom",
    last_name = "Udall",
    state = "NM",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.tomudall.senate.gov",
    campaign_name = "Udall for Us All",
    campaign_website = "https://www.tomudall.com/",
    actblue_link = "tudefault",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00329896",
    FEC_ID_cand = "S8NM00184"
  ),
  # Dropped out Oct 29, 2019
  tibble(
    first_name = "Maggie",
    last_name = "Toulouse Oliver",
    state = "NM",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Maggie Toulouse Oliver for U.S. Senate",
    campaign_website = "https://www.maggietoulouseoliverforsenate.com/",
    actblue_link = "mto-web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00703256",
    FEC_ID_cand = "S0NM00074"
  ),
  tibble(
    first_name = "Gavin",
    last_name = "Clarkson",
    state = "NM",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gavin for U.S. Senate",
    campaign_website = "https://www.gavinclarkson.com/",
    actblue_link = NA,
    anedot_link = "gavin-clarkson-for-senate/donate",
    other_link = NA,
    FEC_ID_cmte = "C00702985",
    FEC_ID_cand = "S0NM00066"
  ),
  tibble(
    first_name = "Mick",
    last_name = "Rich",
    state = "NM",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mick Rich 2020",
    campaign_website = "https://mickrich2020.com",
    actblue_link = NA,
    anedot_link = "mick-rich-2020/donate",
    other_link = NA,
    FEC_ID_cmte = c("C00714345", "C00633255"),
    FEC_ID_cand = "S8NM00200"
  ),
  tibble(
    first_name = "Elisa",
    last_name = "Martinez",
    state = "NM",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elisa Martinez for US Senate",
    campaign_website = "https://www.elisafornm.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://secure.elisafornm.com/list/proc/donation1/?",
      "initiativekey=5VOQ8LXUC3YM"
    ),
    FEC_ID_cmte = "C00727966",
    FEC_ID_cand = "S0NM00090"
  ),
  tibble(
    first_name = "Andrew John",
    last_name = "Perkins",
    state = "NM",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Andrew John Perkins for U.S. Senate",
    campaign_website = "https://perkinsforsenate.com",
    # No donation link directly in website
    actblue_link = "committee-to-elect-andrew-perkins-us-senate-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00731349",
    FEC_ID_cand = "S0NM00108"
  ),
  tibble(
    first_name = "Bob",
    last_name = "Walsh",
    state = "NM",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bob Walsh for U.S. Senator",
    campaign_website = "https://www.walshforsenate2020.us/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00741405",
    FEC_ID_cand = "S0NM00132"
  ),
  # NM: New Mexico I (last updated Nov 7, 2020) ================================
  # Not up for reelection 2020.
  # Martin Henrich is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "Martin",
    last_name = "Heinrich",
    state = "NM",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.heinrich.senate.gov",
    campaign_name = "Martin Heinrich for Senate",
    campaign_website = "https://martinheinrich.com/",
    actblue_link = "mh4s",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00434563",
    FEC_ID_cand = "S2NM00088"
  ),
  # NY: New York I (last updated Nov 7, 2020) ==================================
  # Not up for reelection 2020.
  # Kirsten Gillibrand is uncontested
  # (no candidates registered as of Sep 11, 2019)
  tibble(
    first_name = "Kirsten",
    last_name = "Gillibrand",
    state = "NY",
    party = "Dem",
    class = "I",
    incumbent = TRUE,
    office_website = "https://www.gillibrand.senate.gov",
    # Currently withdrew from presidential candidate
    # Therefore senate page not yet up
    campaign_name = "Gilibrand for Senate",
    campaign_website = "https://kirstengillibrand.com/",
    actblue_link = "kirsten-gillibrand-4",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00413914",
    FEC_ID_cand = "S0NY00410"
  ),
  # NY: New York III (last updated Nov 7, 2020) ================================
  # Not up for reelection 2020.
  # Chuck Schumer was uncontested (no candidates registered as of Jan 29, 2020)
  # Now Schumer vs. Sare vs. Mantanona
  # Mantanona is... https://ballotpedia.org/Nicholas_Mantanona
  # Also MANTANONA BUILDING INC (C00756023) ---> will not enter into database
  tibble(
    first_name = "Chuck",
    last_name = "Schumer",
    state = "NY",
    party = "Dem",
    class = "III",
    incumbent = TRUE,
    office_website = "https://www.schumer.senate.gov",
    campaign_name = "Schumer U.S. Senate",
    campaign_website = "https://www.chuckschumer.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Rare non-ActBlue donation link
    # First page $5 prompt, then 25-50-100-250-1000-2800-5600
    # Still hasn't changed. Quite different link...
    other_link = "https://secure.chuckschumer.com/",
    FEC_ID_cmte = "C00346312",
    FEC_ID_cand = "S8NY00082"
  ),
  tibble(
    first_name = "Diane",
    last_name = "Sare",
    state = "NY",
    party = "Ind",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sare for Senate",
    campaign_website = "https://www.sareforsenate.com",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-1000-2800
    other_link = "https://www.sareforsenate.com/donate",
    FEC_ID_cmte = "C00751826",
    FEC_ID_cand = "S2NY00325"
  ),
  # NC: North Carolina II (last updated Nov 4, 2020) ===========================
  # Tillis vs. Tucker
  # vs. Cunningham vs. Mansfield vs. Smith vs. Fuller
  # vs. Swenson vs. Lee vs. Williams vs. Bray
  # Mansfield withdrew on July 5, 2019
  tibble(
    first_name = "Thom",
    last_name = "Tillis",
    state = "NC",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.tillis.senate.gov",
    campaign_name = "Thom Tillis for U.S. Senate",
    campaign_website = "https://www.thomtillis.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      "https://secure.winred.com/thom-tillis/website" # ,
      # "https://secure.winred.com/thom-tillis/pitch-in",
      # "https://secure.winred.com/thom-tillis/impeachthis"
    ),
    FEC_ID_cmte = "S4NC00162",
    FEC_ID_cand = "C00545772"
  ),
  tibble(
    first_name = "Cal",
    last_name = "Cunningham",
    state = "NC",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cal for NC",
    campaign_website = "https://www.calfornc.com/",
    actblue_link = c(
      "cal-website" # ,
      # "calfornc-social?refcode=191015-predebate",
      # "jcc-ads-190617-launch-search",
      # "jcc-ads-191017-fourseats-tw-dd", # 4-14-44-104-144
      # "jcc-ads-200608-evergreen-dd-search" # 50-100-250-500-1000-2800
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00709410", "C00471557"),
    FEC_ID_cand = "S0NC00202"
  ),
  # Withdrew on Dec 2, 2019
  tibble(
    first_name = "Garland",
    last_name = "Tucker",
    state = "NC",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Garland Tucker For Senate",
    campaign_website = "https://www.garlandtucker.com/",
    actblue_link = NA,
    anedot_link = "garland-tucker-for-senate/donate",
    other_link = NA,
    FEC_ID_cmte = "C00705228",
    FEC_ID_cand = "S0NC00319"
  ),
  # Now running for NC-01
  tibble(
    first_name = "Sandy",
    last_name = "Smith",
    state = "NC",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sandy Smith for Congress",
    campaign_website = "https://sandysmithnc.com/",
    actblue_link = NA,
    anedot_link = "sandysmithforsenate/website",
    other_link = NA,
    FEC_ID_cmte = "C00697250",
    FEC_ID_cand = "S0NC00293"
  ),
  tibble(
    first_name = "Trevor",
    last_name = "Fuller",
    state = "NC",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Trevor Fuller for U.S. Senate",
    campaign_website = "https://www.fullerfornorthcarolina.com/",
    actblue_link = "fullerfornorthcarolina",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00694729",
    FEC_ID_cand = "S0NC00285"
  ),
  tibble(
    first_name = "Steve",
    last_name = "Swenson",
    state = "TN",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Steve Swenson for U.S. Senate",
    campaign_website = "https://steveswensonforsenate.org",
    # No donation link. Weird...
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00730200",
    FEC_ID_cand = "S0NC00368"
  ),
  # Now running for Commissioner of Labor instead
  tibble(
    first_name = "Eva",
    last_name = "Lee",
    state = "NC",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://www.evalee4nc.com/",
    # Uh... the ActBlue link is broken though.
    # actblue_link = "Eva4Senate",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00651836",
    FEC_ID_cand = "S0NC00277"
  ),
  tibble(
    first_name = "Steven",
    last_name = "Williams",
    state = "NC",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Steven Williams for North Carolina",
    campaign_website = "https://www.stevenforsenate.com/",
    actblue_link = "steven-williams-for-north-carolina-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00703041",
    FEC_ID_cand = "S0NC00301"
  ),
  tibble(
    first_name = "Shannon",
    last_name = "Bray",
    state = "NC",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Shannon Bray for North Carolina",
    campaign_website = "https://www.shannonbray.us/",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-250-1000-2500
    other_link = "https://www.shannonbray.us/donate",
    FEC_ID_cmte = "C00697870",
    FEC_ID_cand = "S0NC00335"
  ),
  tibble(
    first_name = "Atul",
    last_name = "Goel",
    state = "NC",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Atul Goel for US Senate",
    campaign_website = "http://www.atulgoelforsenate.com",
    actblue_link = NA,
    anedot_link = NA,
    # ... we will send you a link to make the contribution via PayPal.
    other_link = "http://www.atulgoelforsenate.com/index.php/donate",
    FEC_ID_cmte = "C00720219",
    FEC_ID_cand = "S0NC00350"
  ),
  # Cannot find Jeremy Thomas
  tibble(
    first_name = "Larry",
    last_name = "Holmquist",
    state = "NC",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Larry for NC",
    campaign_website = "https://www.larryfornc.com",
    actblue_link = NA,
    anedot_link = NA,
    # No amount prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?",
      "token=etfFGKW4HkigSbYi4hMp_plnsidCIWlqYyMtk5n4fB4N7T3GrM0GM9",
      "shfwLt1bZQy_Vwo0&country.x=US&locale.x="
    ),
    FEC_ID_cmte = "C00736207",
    FEC_ID_cand = "S6NC00308"
  ),
  # NC: North Carolina III (last updated Nov 7, 2020) ==========================
  # Not up for reelection 2020.
  # Richard Burr was uncontested (no candidates registered as of Jan 29, 2019)
  # Pledged this will be his last run
  # Now Burr vs. Brian (also Republican) but cannot find Lee Brian
  tibble(
    first_name = "Richard",
    last_name = "Burr",
    state = "NC",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.burr.senate.gov",
    campaign_name = "Burr for Senate",
    campaign_website = "https://www.burrforsenate.com",
    actblue_link = NA,
    anedot_link = NA,
    # Still Victory Passport: 10-25-50-100-250-1000-2700
    other_link = paste0(
      "https://mystique.victorypassport.com/pages/burr/",
      "post-election-donation-page?location=https%3A%2F%2F",
      "give.victorypassport.com%2Fburr%2Fcontribute"
    ),
    FEC_ID_cmte = "C00385526",
    FEC_ID_cand = "S4NC00089"
  ),
  # ND: North Dakota I (last updated Nov 7, 2020) ==============================
  # Not up for reelection 2020.
  # Kevin Cramer is uncontested (no candidates registered as of Jan 30, 2020)
  tibble(
    first_name = "Kevin",
    last_name = "Cramer",
    state = "ND",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.cramer.senate.gov",
    campaign_name = "Cramer for Senate",
    campaign_website = "https://www.kevincramer.org/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = c(
      # paste0(
      #   "https://spark.widgetmakr.com/render/",
      #   "c82a18a8-0f56-42fa-9bb1-143191b3c141"
      # ),
      "https://secure.winred.com/cramer-for-senate/contribute"
    ),
    FEC_ID_cmte = "C00473371",
    FEC_ID_cand = "S8ND00120"
  ),
  # ND: North Dakota III (last updated Nov 7, 2020) ============================
  # Not up for reelection 2020.
  # John Hoeven vs. Steele but no separate website
  # See https://www.facebook.com/steele2022/ (not the RNC Steele!)
  tibble(
    first_name = "John",
    last_name = "Hoeven",
    state = "ND",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.hoeven.senate.gov",
    campaign_name = "Hoeven for Senate",
    campaign_website = "http://www.hoevenforsenate.com/",
    actblue_link = NA,
    # Change documented to Anedot Sep 17, 2020
    anedot_link = "hoeven-for-senate/donate",
    # from 25-50-100-150-1000-2700
    #   to 25-50-100-150-1000-2800-5600
    # other_link = paste0(
    #   "https://spark.widgetmakr.com/render/",
    #   "c82a18a8-0f56-42fa-9bb1-143191b3c141"
    # ),
    other_link = NA,
    FEC_ID_cmte = "C00473371",
    FEC_ID_cand = "S0ND00093"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Steele",
    state = "ND",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    # Still no proper link at https://twitter.com/MichaelSteele
    campaign_website = paste0(
      "https://www.politicalbank.com/find-candidates/",
      "michael-james-steele?fbclid=IwAR2ru9apWRtMC3xBIKNF",
      "ktyRrRFoa0BaaOeCpgipNEw-6nJXju4eKzMGXMM"
    ),
    actblue_link = "steele2022",
    anedot_link = NA,
    other_link = paste0(
      "https://www.politicalbank.com/find-candidates/",
      "michael-james-steele?fbclid=IwAR2ru9apWRtMC3xBIKNF",
      "ktyRrRFoa0BaaOeCpgipNEw-6nJXju4eKzMGXMM"
    ),
    FEC_ID_cmte = "C00690586",
    FEC_ID_cand = "S2ND00107"
  ),
  # OH: Ohio I (last updated Nov 7, 2020) ======================================
  # Not up for reelection 2020.
  # Sherrod Brown is uncontested (no candidates registered as of Jan 30, 2020)
  tibble(
    first_name = "Sherrod",
    last_name = "Brown",
    state = "OH",
    party = "Dem",
    class = "I",
    incumbent = TRUE,
    office_website = "https://www.brown.senate.gov",
    campaign_name = "Friends of Sherrod Brown",
    campaign_website = "https://www.sherrodbrown.com/",
    actblue_link = "sbdefault",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00264697",
    FEC_ID_cand = "S6OH00163"
  ),
  # OH: Ohio III (last updated Nov 7, 2020) ====================================
  # Not up for reelection 2020.
  # Rob Portman was uncontested (no candidates registered as of Sep 11, 2019)
  # Now Portman vs. Pukita
  tibble(
    first_name = "Rob",
    last_name = "Portman",
    state = "OH",
    party = "Rep",
    class = "III",
    incumbent = TRUE,
    office_website = "https://www.portman.senate.gov",
    campaign_name = "",
    campaign_website = "https://www.robportman.com",
    actblue_link = NA,
    anedot_link = NA,
    #     25-50-75-100-250-500-1000-2800-5600
    # Change documented to WinRed on Sep 17, 2020
    # Now 25-50-100-250-500-1000-2800
    # other_link = "https://www.robportman.com/donation_v2",
    other_link = "https://secure.winred.com/portman/donate",
    FEC_ID_cmte = "C00458463",
    FEC_ID_cand = "S0OH00133"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Pukita",
    state = "OH",
    party = "Rep",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Pukita for US Senate",
    campaign_website = "https://www.pukitaforsenate2022.com",
    actblue_link = NA,
    anedot_link = NA,
    # Nationbuilder: 1-5-10-20-25-50-100-250-1000-2800-5600
    other_link = "https://www.pukitaforsenate2022.com/donate-to-our-team",
    FEC_ID_cmte = "C00722207",
    FEC_ID_cand = "S2OH00287"
  ),
  # OK: Oklahoma II (last updated Jul 5, 2021) =================================
  # Jim Inhofe was uncontested (no candidates registered as of Sep 11, 2019)
  # Now Inhofe vs. Stitt
  # vs. Billings vs. Rogers vs. Britt vs. Dougherty vs. Broyles
  # vs. Farr vs. Nesbitt
  # Cannot find Rogers nor Dougherty
  # Britt withdrew in Aug 21, 2019
  tibble(
    first_name = "Jim",
    last_name = "Inhofe",
    state = "OK",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.inhofe.senate.gov",
    campaign_name = "Jim Inhofe for Senate",
    campaign_website = "https://www.jiminhofe.com/",
    actblue_link = NA,
    anedot_link = "jiminhofe/donate",
    other_link = NA,
    FEC_ID_cmte = "C00207993",
    FEC_ID_cand = "S4OK00083"
  ),
  tibble(
    first_name = "Abby",
    last_name = "Broyles",
    state = "OK",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Abby Broyles for US Senate",
    campaign_website = "https://www.abbybroyles.com",
    actblue_link = "ab-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00725762",
    FEC_ID_cand = "S0OK00370"
  ),
  tibble(
    first_name = "JJ",
    last_name = "Sitt",
    state = "OK",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "JJ Stitt for US Senate",
    campaign_website = "https://www.jjstitt.com",
    actblue_link = NA,
    anedot_link = "jj-stitt-for-us-senate/donate",
    other_link = NA,
    FEC_ID_cmte = "C00716407",
    FEC_ID_cand = "S0OK00347"
  ),
  tibble(
    first_name = "Dylan",
    last_name = "Billings",
    state = "OK",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dylan Billings for Senate",
    # Website is actually private right now...
    campaign_website = "https://www.dylanbillings.com",
    actblue_link = "billings-for-u-s--senate-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00714477",
    FEC_ID_cand = "S0OK00339"
  ),
  tibble(
    first_name = "Joan",
    last_name = "Farr",
    state = "OK",
    party = "Ind",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joan Farr for Senate",
    campaign_website = "https://www.joanfarr.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00556522",
    FEC_ID_cand = "S4OK00224"
  ),
  tibble(
    first_name = "April",
    last_name = "Nesbit",
    state = "OK",
    party = "Ind",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nesbit Senate",
    campaign_website = "https://www.nesbitok.org/",
    actblue_link = NA,
    anedot_link = "https://secure.anedot.com/nesbit_ok/donate",
    other_link = NA,
    FEC_ID_cmte = "C00742353",
    FEC_ID_cand = "S0OK00404"
  ),
  # Looks like I missed Neil Mavis: https://neilmavis2020.com
  # OK: Oklahoma III (last updated Nov 7, 2020) ================================
  # Not up for reelection 2020.
  # James Lankford is uncontested (no candidates registered as of Jan 29, 2020)
  tibble(
    first_name = "James",
    last_name = "Lankford",
    state = "OK",
    party = "Rep",
    class = "III",
    incumbent = TRUE,
    office_website = "www.lankford.senate.gov",
    campaign_name = "Oklahomans for James",
    campaign_website = "http://www.jameslankford.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-200-250-500-1000-2700-5400
    # Change documented to WinRed on Sep 17, 2020
    # other_link = "https://www.trailblz.info/LankfordJames/Donations.aspx",
    # Now 25-50-100-250-500-1000-2800
    other_link = "https://secure.winred.com/families-for-james-lankford/donate",
    FEC_ID_cmte = "C00466482",
    FEC_ID_cand = "S4OK00232"
  ),
  # OR: Oregon II (last updated Jul 5, 2021) ===================================
  # Jeff Merkley had no notable opponents
  # (Larry Reese not found, zero receipts as of Sep 11, 2019 ~ Jan 31, 2020)
  # Now Merkley vs. Perkins vs. Reese vs. Taher vs. Romero vs. Dye
  # Last updated Jan 31, 2020
  tibble(
    first_name = "Jeff",
    last_name = "Merkley",
    state = "OR",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.merkley.senate.gov",
    campaign_name = "Stand with Jeff Merkley",
    campaign_website = "https://www.jeffmerkley.com/",
    actblue_link = "stand-with-jeff-merkley",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00437277",
    FEC_ID_cand = "S8OR00207"
  ),
  tibble(
    first_name = "Jo Rae",
    last_name = "Perkins",
    state = "OR",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Perkins for U.S. Senate",
    campaign_website = "http://perkins4oregon.com",
    actblue_link = NA,
    anedot_link = NA,
    # 25-50-100-250-500-1000. New platform...
    # Change documented to WinRed on Sep 17, 2020
    # other_link = "https://karmapayments.org/perkins-for-congress/donate.php",
    other_link = "https://secure.winred.com/JoRaePerkins/donate",
    # 2800-2000-1000-500-250-100-45-17... interesting
    FEC_ID_cmte = c("C00582635", "C00547646"),
    FEC_ID_cand = "S4OR00156"
  ),
  tibble(
    first_name = "Ibrahim",
    last_name = "Taher",
    state = "OR",
    party = "Grn",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Taher for US Senate",
    campaign_website = "https://ibrahimtaher.com",
    actblue_link = NA,
    anedot_link = NA,
    # 3-5-10-15-25
    other_link = "https://ibrahimtaher.com/donate/",
    FEC_ID_cmte = "C00720094",
    FEC_ID_cand = "S0OR00329"
  ),
  tibble(
    first_name = "Paul",
    last_name = "Romero",
    state = "OR",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Romero for Senate",
    campaign_website = "https://www.romero4oregon.com",
    actblue_link = NA,
    anedot_link = NA,
    # No amount prompts
    other_link = paste0(
      "https://www.paypal.com/donate/?token=z-MR4DP-pua6KgGfKEhY-",
      "epZSv2n1hmyPF_XUew9e5wxqkQVl95Spdc3hS8kWvLuJyisPG&country.x=US",
      "&locale.x=US"
    ),
    FEC_ID_cmte = "C00725523",
    FEC_ID_cand = "S0OR00337"
  ),
  tibble(
    first_name = "Gary",
    last_name = "Dye",
    state = "OR",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gary Dye for Senate",
    campaign_website = NA,
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "S0OR00378"
  ),
  # OR: Oregon III (last updated Nov 7, 2020) ==================================
  # Not up for reelection 2020.
  # Ron Widen is uncontested (no candidates registered as of Jan 30, 2020)
  tibble(
    first_name = "Ron",
    last_name = "Wyden",
    state = "OR",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.wyden.senate.gov",
    campaign_name = "Wyden for Senate",
    campaign_website = "https://www.standtallforamerica.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Another rare instance of non-ActBlue link.
    # fundraising_misc is not working properly on this: 20-35-50-100-2800-5600
    # oberon? In practice it's being properly scraped...
    other_link = "https://act.myngp.com/Forms/-5547459973236783104",
    FEC_ID_cmte = "C00308676",
    FEC_ID_cand = "S6OR00110"
  ),
  # PA: Pennsylvania I (last updated Nov 7, 2020) ==============================
  # Not up for reelection 2020.
  # Bob Casey is uncontested (no candidates registered as of Jan 30, 2020)
  tibble(
    first_name = "Bob",
    last_name = "Casey",
    state = "PA",
    party = "Dem",
    class = "I",
    incumbent = TRUE,
    office_website = "https://www.casey.senate.gov",
    campaign_name = "Bob Casey for Senate",
    campaign_website = "https://bobcasey.com/",
    actblue_link = "bob-casey-homepage",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00431056",
    FEC_ID_cand = "S6PA00217"
  ),
  # PA: Pennsylvania III (last updated Nov 7, 2020) ============================
  # Not up for reelection 2020.
  # Patrick Toomey was uncontested (no candidates registered as of Sep 11, '19)
  # Now Toomey vs. McGuigan vs. Dean
  # Toomey retiring; WinRed already down
  tibble(
    first_name = "Pat",
    last_name = "Toomey",
    state = "PA",
    party = "Rep",
    class = "III",
    incumbent = TRUE,
    office_website = "https://www.toomey.senate.gov",
    campaign_name = "Pat Toomey for Senate",
    campaign_website = "https://www.toomeyforsenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/friends-of-pat-toomey/website",
    FEC_ID_cmte = "C00461046",
    FEC_ID_cand = "S4PA00121"
  ),
  tibble(
    first_name = "John",
    last_name = "McGuigan",
    state = "PA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "McGuigan for Pennsylvania",
    campaign_website = "https://www.mcguiganforpa.com",
    actblue_link = "mcguigan-for-pennsylvania-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00740563",
    FEC_ID_cand = "S2PA00307"
  ),
  tibble(
    first_name = "Brandaun",
    last_name = "Dean",
    state = "PA",
    party = "Dem",
    class = "III",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Brandaun for U.S. Senator",
    campaign_website = "http://brandaundean.com",
    actblue_link = "usinthesenate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00733360",
    FEC_ID_cand = "S2PA00299"
  ),
  # RI: Rhode Island II (last updated Nov 4, 2020) =============================
  # Jack Reed is uncontested (no candidates registered as of Sep 11, 2019)
  # Now Reed vs. Waters
  tibble(
    first_name = "Jack",
    last_name = "Reed",
    state = "RI",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.reed.senate.gov",
    campaign_name = "Jack Reed for United States Senate",
    campaign_website = "https://www.jackreed.com/",
    actblue_link = "jackreed",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00238907",
    FEC_ID_cand = "S6RI00163"
  ),
  tibble(
    first_name = "Allen",
    last_name = "Waters",
    state = "RI",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Allen Waters for Senate",
    campaign_website = "https://www.allenrwaters.com",
    actblue_link = NA,
    anedot_link = NA,
    # Nationbuilder: 25-50-100-250-1000-2800
    other_link = "https://www.allenrwaters.com/donate",
    FEC_ID_cmte = "C00731950",
    FEC_ID_cand = "S0RI00067"
  ),
  # RI: Rhode Island I (last updated Nov 7, 2020) ==============================
  # Not up for reelection in 2020.
  # Sheldon Whitehouse is uncontested
  # (no candidates registered as of Jan 30, 2020)
  tibble(
    first_name = "Sheldon",
    last_name = "Whitehouse",
    state = "RI",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.whitehouse.senate.gov",
    campaign_name = "Whitehouse for Senate",
    campaign_website = "https://whitehouseforsenate.com/",
    actblue_link = "whitehouse_website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00410803",
    FEC_ID_cand = "S6RI00221"
  ),
  # SC: South Carolina II (last updated Jul 5, 2021) ===========================
  # Graham vs. Harrison
  # vs. Reynolds vs. Lapierre vs. Tinubu vs. Sloan
  # vs. Kandies vs. Knights vs. Stone
  # Cannot find Knights or Sloan
  # Stone specifies protest candidacy https://williamforsc.wixsite.com/home
  tibble(
    first_name = "Lindsey",
    last_name = "Graham",
    state = "SC",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.lgraham.senate.gov",
    campaign_name = "Team Graham",
    campaign_website = "https://www.lindseygraham.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to WinRed on Jan 31, 2020
    # At leaste from
    # 25-50-75-100-250-500-1000-2800 to 5-25-50-75-100-250-500-1000-2800-5600
    # On WinRed Twitter Feb 12, 2020
    # Donate link gone from website although winning general election??
    other_link = c(
      # New, different WinRed
      "https://secure.winred.com/team-graham/website",
      "https://secure.winred.com/team-graham/donate" # ,
      # "secure.winred.com/team-graham/sept-20-eoq-s",
      # paste0(
      #   "https://secure.lindseygraham.com/list/ads/ld/?",
      #   "InitiativeKey=AIIQLP28DCDW"
      # )
    ),
    # other_link = paste0(
    #   "https://secure.lindseygraham.com/donation1/default.aspx?",
    #   "InitiativeKey=MCVSKRDTSZ2S"
    # ),
    FEC_ID_cmte = "C00458828",
    FEC_ID_cand = "S0SC00149"
  ),
  tibble(
    first_name = "Jaime",
    last_name = "Harrison",
    state = "SC",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Jaime Harrison for U.S. Senate",
    campaign_website = "https://jaimeharrison.com/",
    actblue_link = c(
      "ms_harrison_fr_homepage_2019" # ,
      # "ms_harrison_fr_social-june",
      # "q3-2019-social",
      # "q4-2019-social",
      # "q1-2020-social",
      # "q2-2020-social",
      # "q3-2020-social",
      # "adopt-jaime-harrison",
      # "ms_harrison_fr_q12019_jaime-a",
      # "ms_harrison_fr_fbtw_launch",
      # "sl_southcarolina_senate_fund"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00696153",
    FEC_ID_cand = "S0SC00289"
  ),
  tibble(
    first_name = "Joe",
    last_name = "Reynolds",
    state = "SC",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Joe Reynolds U.S. Senate 2020",
    campaign_website = "https://reynolds2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 20-50-100-250
    other_link = "https://secure.qgiv.com/for/jrfs/embed/53817/",
    FEC_ID_cmte = "C00700989",
    FEC_ID_cand = "S0SC00297"
  ),
  tibble(
    first_name = "Gloria",
    last_name = "Tinubu",
    state = "SC",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gloria for US Senate",
    campaign_website = "https://gloriaforussenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Rare Democratic Non-ActBlue link
    other_link = "https://act.myngp.com/Forms/-7207440166490405120",
    FEC_ID_cmte = "C00702407",
    FEC_ID_cand = "S0SC00305"
  ),
  tibble(
    first_name = "Michael",
    last_name = "Lapierre",
    state = "SC",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Michael J. Lapierre for United States Senate",
    campaign_website = "https://lapierreforsenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Change documented to Paypal(!), $20 prompt for signed book "A Covenant"
    # Was originally 25-50-100
    other_link = paste0(
      "https://www.paypal.com/webapps/hermes?",
      "token=8SJ56993KD568500J&useraction=commit&",
      "rm=0&mfid=1580510594448_427dfeba7c9b9"
    ),
    # other_link =
    #   "https://donorbox.org/embed/lapierre-for-u-s-senate?show_content=true",
    FEC_ID_cmte = "C00707224",
    FEC_ID_cand = "S0SC00313"
  ),
  tibble(
    first_name = "Dwayne",
    last_name = "Buckner",
    state = "SC",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dwayne T. Buckner for United States Senate",
    campaign_website = "https://www.bucknerforsenate.com",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-150-250-500-1000-1500-2000-2500
    other_link = paste0(
      "https://secure.campaigncontributions.net/",
      "76990/Contribute-to-Dwayne-T-Buckner"
    ),
    FEC_ID_cmte = "C00723494",
    FEC_ID_cand = "S0SC00321"
  ),
  # Cannot find anything
  tibble(
    first_name = "Mark",
    last_name = "Sloan",
    state = "SC",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Sloan for US Senate SC",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00664573",
    FEC_ID_cand = "S0SC00230"
  ),
  tibble(
    first_name = "Peggy",
    last_name = "Kandies",
    state = "SC",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Peggy Kandies for US Senate 2020",
    campaign_website = "https://peggy2020.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://donorbox.org/peggy2020",
    FEC_ID_cmte = "C00678805",
    FEC_ID_cand = "S0SC00248"
  ),
  tibble(
    first_name = "Matthew",
    last_name = "Knights",
    state = "SC",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Knights for US Senate 2020 South Carolina",
    campaign_website = "https://www.facebook.com/MatthewBaldwinKnights/",
    # No direct link from website
    actblue_link = "knights-for-us-senate-sc-2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00688184",
    FEC_ID_cand = "S0SC00255"
  ),
  tibble(
    first_name = "William",
    last_name = "Stone",
    state = "SC",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "William for South Carolina",
    campaign_website = "https://williamforsc.wixsite.com/home",
    # No contribution link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00688184",
    FEC_ID_cand = "S0SC00255"
  ),
  # SC: South Carolina III (last updated Nov 7, 2020) ==========================
  # Not up for reelection 2020.
  # Tim Scott is uncontested (no candidates registered as of Jan 30, 2020)
  tibble(
    first_name = "Tim", # Timothy, goes by Tim
    last_name = "Scott",
    state = "SC",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.scott.senate.gov",
    campaign_name = "Tim Scott for Senate",
    campaign_website = "https://votetimscott.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/tim-scott-for-senate/donate",
    FEC_ID_cmte = "C00540302",
    FEC_ID_cand = "S4SC00240"
  ),
  # SD: South Dakota II (last updated Nov 4, 2020) =============================
  # Mike Rounds was uncontested (no candidates registered as of Sep 11, 2019)
  # Now Rounds vs. Borglum vs. Ahlers vs. Hart vs. Walker
  # Walker: felony record of falsifying petitions
  # Last updated Jan 31, 2020
  tibble(
    first_name = "Mike",
    last_name = "Rounds",
    state = "SD",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.rounds.senate.gov",
    campaign_name = "Rounds for Senate",
    campaign_website = "https://roundsforsenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 50-100-250-500-1000-2800-5600-11200
    # Still Aristotle
    other_link = "https://cm.aristotle.com/d/roundsforsenate",
    FEC_ID_cmte = "C00532465",
    FEC_ID_cand = "S4SD00049"
  ),
  tibble(
    first_name = "Dan",
    last_name = "Ahlers",
    state = "SD",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ahlers for Senate",
    campaign_website = "https://www.danahlers.com",
    actblue_link = "4progress",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00720623",
    FEC_ID_cand = "S0SD00070"
  ),
  tibble(
    first_name = "Scyller",
    last_name = "Borglum",
    state = "SD",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Borglum for Senate",
    campaign_website = "https://www.facebook.com/borglum4USSenate/",
    actblue_link = NA,
    anedot_link = NA,
    # No suggested amounts
    other_link = "https://www.paypal.me/borglumforsenate",
    FEC_ID_cmte = "C00711028",
    FEC_ID_cand = "S0SD00088"
  ),
  tibble(
    first_name = "Clara",
    last_name = "Hart",
    state = "SD",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Clara Hart for Senate",
    campaign_website = "https://twitter.com/claraha74184453",
    # Nothing really...
    actblue_link = "hart-for-us-senate-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00728592",
    FEC_ID_cand = "S0SD00096"
  ),
  # SD: South Dakota III (last updated Sep 18, 2020) ===========================
  # Not up for reelection 2020.
  # Thune is uncontested (no candidates registered as of Jan 30, 2020)
  tibble(
    first_name = "John",
    last_name = "Thune",
    state = "SD",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.thune.senate.gov",
    campaign_name = "Friends of John Thune",
    campaign_website = "http://www.johnthune.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/friends-of-john-thune/donate",
    FEC_ID_cmte = "C00409581",
    FEC_ID_cand = "S2SD00068"
  ),
  # TN: Tennessee II (last updated Jul 5, 2021) ===============================
  # Lamar Alexander is retiring
  tibble(
    first_name = "Bill",
    last_name = "Hagerty",
    state = "TN",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bill Hagerty for U.S. Senate",
    campaign_website = "https://teamhagerty.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/teamhagerty/launch",
    FEC_ID_cmte = "C00718627",
    FEC_ID_cand = "S0TN00169"
  ),
  tibble(
    first_name = "Marquita",
    last_name = "Bradshaw",
    state = "TN",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Marquita Bradshaw for U.S. Senate",
    campaign_website = "https://www.marquitabradshaw.com",
    actblue_link = "marquitaussenate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00720805",
    FEC_ID_cand = "S0TN00342"
  ),
  tibble(
    first_name = "Lamar",
    last_name = "Alexander",
    state = "TN",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.alexander.senate.gov",
    campaign_name = "Alexander for Senate",
    campaign_website = "http://www.alexanderforsenate.com/",
    # Not seeking reelection
    # https://ballotpedia.org/Lamar_Alexander
    actblue_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00383745",
    FEC_ID_cand = "S2TN00058"
  ),
  # Open seat candidates
  # Sethi vs. Hagerty
  # vs. Mackler vs. Flinn vs. Gapp vs. Nielson vs. Neal
  # vs. Onyejiaka vs. Bush vs. Presley vs. McLeod vs. Crim vs. Pettigrew
  # Crim's website http://www.larrycrimussenate.com/ is down
  # and record of disqualification in 2018
  # Last updated on Jan 31, 2020
  tibble(
    first_name = "Manny",
    last_name = "Sethi",
    state = "TN",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Dr. Manny Sethi for U.S. Senate",
    campaign_website = "https://www.drmannyforsenate.com",
    actblue_link = NA,
    anedot_link = NA,
    # Switch documented to WinRed on Sep 19, 2020
    # However, it is already gone.
    # The revv page is still live.
    other_link = "https://secure.winred.com/dr-manny-for-us-senate/donate",
    # other_link = "https://manny.revv.co/donate/?sourcecode=homepage",
    FEC_ID_cmte = "C00708628",
    FEC_ID_cand = "S0TN00144"
  ),
  tibble(
    first_name = "James",
    last_name = "Mackler",
    state = "TN",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "James Mackler for U.S. Senate",
    campaign_website = "https://www.jamesmackler.com/",
    actblue_link = "mackler2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00637850",
    FEC_ID_cand = "S8TN00311"
  ),
  tibble(
    first_name = "George",
    last_name = "Flinn",
    state = "TN",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Real Solutions for TN",
    campaign_website = "www.realsolutionsfortn.com/",
    actblue_link = NA,
    anedot_link = "friends-of-george-flinn-2020/donate",
    other_link = NA,
    FEC_ID_cmte = "C00722397",
    FEC_ID_cand = "S0TN00185"
  ),
  tibble(
    first_name = "Josh",
    last_name = "Gapp",
    state = "TN",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Josh Gapp for U.S. Senate",
    campaign_website = "https://www.joshgapp.com",
    actblue_link = NA,
    anedot_link = NA,
    # No prompts
    other_link = "https://www.joshgapp.com/donate.html",
    FEC_ID_cmte = "C00713750",
    FEC_ID_cand = "S0TN00151"
  ),
  tibble(
    first_name = "Stokes",
    last_name = "Nielson",
    state = "TN",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Stokes for Tennessee Senate",
    campaign_website = "http://stokesfortennesseefreedom.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://www.paypal.com/donate/",
      "?token=5FXwMG8HhK8lrj7-krso6LF9Y5LRZuClNR34tMz0m7fWXCW6q6",
      "GVALcpjDsBAouS1leCrG&country.x=US&locale.x=US"
    ),
    FEC_ID_cmte = "C00701524",
    FEC_ID_cand = "S0TN00136"
  ),
  tibble(
    first_name = "Glen",
    last_name = "Neal",
    state = "TN",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Glen Neal for United States Senate",
    campaign_website = "https://glennealforunitedstatessenate.org",
    actblue_link = NA,
    anedot_link = NA,
    # Paypal, but prompted 25-5-10-20-50-100
    other_link = paste0(
      "https://www.paypal.com/webapps/hermes?",
      "token=31W62373E1542961P&useraction=commit&",
      "rm=0&mfid=1580504486134_b77727f341350"
    ),
    FEC_ID_cmte = "C00719757",
    FEC_ID_cand = "S0TN00177"
  ),
  tibble(
    first_name = "Diana",
    last_name = "Onyejiaka",
    state = "TN",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Diana for TN",
    campaign_website = "http://dianafortn.com",
    actblue_link = "dianafortn",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00723882",
    FEC_ID_cand = "S0TN00193"
  ),
  tibble(
    first_name = "Byron",
    last_name = "Bush",
    state = "TN",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bush for Senate",
    campaign_website = "https://www.bushforsenate.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://bushforsenate.revv.co/donate",
    FEC_ID_cmte = "C00725846",
    FEC_ID_cand = "S0TN00201"
  ),
  tibble(
    first_name = "Johnny",
    last_name = "Presley",
    state = "TN",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Presley for U.S. Senate",
    campaign_website = "https://johnnypresley.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://secure.winred.com/johnny-presley-for-us-senate/donate",
    FEC_ID_cmte = "C00730689",
    FEC_ID_cand = "S0TN00219"
  ),
  # Cannot find McLeod
  # http://www.larrycrimussenate.com Crim website is down
  tibble(
    first_name = "Aaron",
    last_name = "Pettigrew",
    state = "TN",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Aaron Pettigrew for Tennessee",
    campaign_website = "http://aaronpettigrewfortennessee.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://www.efundraisingconnections.com/c/AaronLPettigrew",
    FEC_ID_cmte = "C00665414",
    FEC_ID_cand = "S8TN00394"
  ),
  # I missed John Osborne, Marck Pickrell (PayPal still live, no prompts)
  # Jon Henry (no prompts: https://form.jotform.com/200897795740166)
  tibble(
    first_name = "Robin",
    last_name = "Kimbrough",
    state = "TN",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Robin for Senate",
    campaign_website = "https://www.robinforsenate.com",
    # Suspended, but 25-100-250-1000
    actblue_link = "robin-for-tn-united-states-senate-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00743047",
    FEC_ID_cand = "S0TN00268"
  ),
  tibble(
    first_name = "Roy",
    last_name = "Cope",
    state = "TN",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elect Roy Dale Cope TN 2020 US Senator",
    campaign_website = "https://copetn2020.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = paste0(
      "https://secure.winred.com/roy-dale-cope-tn2020-us-senate-elect/",
      "take-america-back-subscribe-today"
    ),
    FEC_ID_cmte = "C00740266",
    FEC_ID_cand = "S0TN00243"
  ),
  # TN: Tennessee I (last updated Nov 7, 2020) =================================
  # Not up for reelection 2020.
  # Marsha Blackburn is uncontested
  # (no candidates registered as of Jan 30, 2020)
  tibble(
    first_name = "Marsha",
    last_name = "Blackburn",
    state = "TN",
    party = "Rep",
    class = "I",
    incumbent = TRUE,
    office_website = " www.blackburn.senate.gov",
    campaign_name = "Marsha Blackburn for Senate",
    campaign_website = "https://www.marshablackburn.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/marsha-blackburn/website/",
    FEC_ID_cmte = "C00376939",
    FEC_ID_cand = "S8TN00337"
  ),
  # TX: Texas II (last updated Nov 4, 2020) ====================================
  # Cornyn vs. Hegar (O'Rourke may come in?)
  # vs. Edwards vs. West vs. Tzintzun vs. Bell vs. Stovall
  # vs. Love vs. Yancey vs. Hernandez vs. Foster vs. McKennon
  # vs. Cooper vs. Bierschwale vs. Ocegueda vs. Smith vs. Harris
  # vs. Castro vs. Srinivasan vs. Jefferson vs. Brumley vs. Hunter
  tibble(
    first_name = "John",
    last_name = "Cornyn",
    state = "TX",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "www.cornyn.senate.gov",
    campaign_name = "Keep Texas Red",
    campaign_website = "https://www.johncornyn.com/",
    actblue_link = NA,
    anedot_link = NA,
    # other_link = "https://secure.johncornyn.com/contribute",
    # Change documented to WinRed Oct 3, 2019
    # Looks like it was Jul 31, 2019. Same amounts. Weird!
    other_link = c(
      "https://secure.winred.com/john-cornyn/contribute",
      "https://secure.winred.com/john-cornyn/donate",
      "https://secure.winred.com/john-cornyn/official-trump-supporters"
    ),
    FEC_ID_cmte = "C00369033",
    FEC_ID_cand = "S2TX00106"
  ),
  tibble(
    first_name = "MJ", # Mary Jennings, goes by MJ
    last_name = "Hegar",
    state = "TX",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "MJ for Texas",
    campaign_website = "https://www.mjfortexas.com/",
    actblue_link = c(
      "mjh_website" # ,
      # "mj-lessthan1yr"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00649350",
    FEC_ID_cand = "S0TX00233"
  ),
  tibble(
    first_name = "Amanda",
    last_name = "Edwards",
    state = "TX",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "https://amandafortexas.com",
    actblue_link = c(
      "ake_newweb"
      # "amandaedwards-website",
      # "e-ads-launch-20190718-dd-search"
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00712737",
    FEC_ID_cand = "S0TX00258"
  ),
  # Now running for State Senate
  tibble(
    first_name = "Royce",
    last_name = "West",
    state = "TX",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Royce West for U.S. Senate",
    campaign_website = "https://roycewest.com/",
    actblue_link = "rw2020web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00712968",
    FEC_ID_cand = "S0TX00266"
  ),
  tibble(
    first_name = "Cristina Tzintzun",
    last_name = "Ramirez",
    state = "TX",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cristina for Texas",
    campaign_website = "https://cristinafortexas.com/",
    actblue_link = "cristina-for-texas-homepage",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00715409",
    FEC_ID_cand = "S8TX00459"
  ),
  tibble(
    first_name = "Chris", # Robert Christopher Bell, goes by Chris
    last_name = "Bell",
    state = "TX",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Elect Chris Bell for Congress",
    campaign_website = "https://www.electchrisbell.com",
    actblue_link = NA,
    anedot_link = "contributetochris",
    other_link = NA,
    FEC_ID_cmte = "C00710871",
    FEC_ID_cand = "S0TX00324"
  ),
  tibble(
    first_name = "Dwayne Bradley",
    last_name = "Stovall",
    state = "TX",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Texas for Stovall",
    campaign_website = "http://www.texansforstovall.com/",
    actblue_link = NA,
    anedot_link = "texans-for-stovall/less-dc-more-tx",
    other_link = NA,
    FEC_ID_cmte = "C00550830",
    FEC_ID_cand = "S4TX00433"
  ),
  # Withdrew
  tibble(
    first_name = "John",
    last_name = "Love",
    state = "TX",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "John Love for Texas",
    campaign_website = "http://www.johnlovefortexas.com",
    actblue_link = NA,
    anedot_link = NA,
    # Not sure what this is... GivingFuel?
    # 25-50-100-250-500-1000-2800
    other_link = "https://lovefortexas.givingfuel.com/donate",
    FEC_ID_cmte = "C00720573",
    FEC_ID_cand = "S0TX00308"
  ),
  tibble(
    first_name = "Mark",
    last_name = "Yancey",
    state = "TX",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mark Yancey for U.S. Senate",
    campaign_website = "https://voteformark.com",
    actblue_link = NA,
    anedot_link = NA,
    # Not sure what this is
    # 25-50-75-100-500-1000-2800
    other_link = "https://voteformark.com/contribute/",
    FEC_ID_cmte = "C00715516",
    FEC_ID_cand = "S0TX00282"
  ),
  tibble(
    first_name = "Annie", # Anne, goes by Annie
    last_name = "Garcia",
    state = "TX",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Annie for Texans",
    campaign_website = "https://runannierun.com",
    actblue_link = "run-annie-run",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00729475",
    FEC_ID_cand = "S0TX00381"
  ),
  tibble(
    first_name = "Sema",
    last_name = "Hernandez",
    state = "TX",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Sema for Texas",
    campaign_website = "https://semafortexas.com",
    actblue_link = NA,
    anedot_link = c("sema4texas", "sema"),
    other_link = NA,
    FEC_ID_cmte = "C00695007",
    FEC_ID_cand = "S0TX00290"
  ),
  tibble(
    first_name = "Jack",
    last_name = "Foster",
    state = "TX",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "A Better Way",
    campaign_website = "https://www.votejackdaniel.com/home",
    actblue_link = NA,
    anedot_link = NA,
    # None-ActBlue account. 10-25
    other_link = "https://www.votejackdaniel.com/contribute",
    FEC_ID_cmte = "C00713206",
    FEC_ID_cand = "S0TX00274"
  ),
  tibble(
    first_name = "Kerry",
    last_name = "McKennon",
    state = "TX",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "McKennon 2020",
    campaign_website = "https://www.mckennon2020.com",
    actblue_link = NA,
    # No amount prompts
    anedot_link = "mckennon-2020/donate",
    other_link = NA,
    FEC_ID_cmte = "C00724856",
    FEC_ID_cand = "S0TX00340"
  ),
  tibble(
    first_name = "Adrian",
    last_name = "Ocegueda",
    state = "TX",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Adrian for Senate",
    campaign_website = "http://www.texasreason.net/",
    actblue_link = "committee-for-adrian-ocegueda",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00693705",
    FEC_ID_cand = "S0TX00225"
  ),
  # I thought Cooper was now a candidate for Lt. Governor... but...
  tibble(
    first_name = "Michael",
    last_name = "Cooper",
    state = "TX",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Cooper For Texas",
    campaign_website = "https://www.cooperforussenate.com",
    actblue_link = "cooper-for-texas-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00698183",
    FEC_ID_cand = "S0TX00332"
  ),
  tibble(
    first_name = "Virgil",
    last_name = "Bierschwale",
    state = "TX",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Bierschwale for U.S. Senate 2020",
    campaign_website = "https://bierschwaleforussenate.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link =
      "https://donorbox.org/embed/virgil-bierschwale-for-u-s-senator-2020",
    FEC_ID_cmte = "C00728477",
    FEC_ID_cand = "S0TX00365"
  ),
  # Rhett Smith withdrew
  tibble(
    first_name = "Victor",
    last_name = "Harris",
    state = "TX",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Victor Harris for U.S. Senate",
    campaign_website = "https://www.victorfortexas.com",
    actblue_link = "friends-of-victor-h-harris-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00726083",
    FEC_ID_cand = "S0TX00316"
  ),
  tibble(
    first_name = "John",
    last_name = "Castro",
    state = "TX",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Castro for U.S. Senate",
    campaign_website = "https://johncastro.com/",
    # No donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00728097",
    FEC_ID_cand = "S0TX00357"
  ),
  # Cannot find Arjun Srinivasan
  # Cannot find Cedric Jefferson
  # Cannot find James Brumley
  # Cannot find Darrel Reece Hunter
  # TX: Texas I (last updated Nov 7, 2020) =====================================
  # Not up for reelection 2020.
  # Ted Cruz is uncontested (no candidates registered as of Sep 11, 2019)
  tibble(
    first_name = "Ted",
    last_name = "Cruz",
    state = "TX",
    party = "Rep",
    class = "I",
    incumbent = TRUE,
    office_website = "www.cruz.senate.gov",
    campaign_name = "Ted Cruz for Senate",
    campaign_website = "https://www.tedcruz.org/",
    actblue_link = NA,
    # Still Anedot
    anedot_link = "tedcruz/donate",
    other_link = NA,
    FEC_ID_cmte = "C00492785",
    FEC_ID_cand = "S2TX00312"
  ),
  # UT: Utah I (last updated Nov 4, 2020) =====================================
  # Not up for reelection 2020.
  # Mike Lee is uncontested (no candidates registered as of Jan 30, 2020)
  tibble(
    first_name = "Mike",
    last_name = "Lee",
    state = "UT",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.lee.senate.gov",
    campaign_name = "Mike Lee for Senate",
    campaign_website = "https://www.leeforsenate.com/",
    actblue_link = NA,
    anedot_link = "leeforsenate/online-donation",
    other_link = NA,
    FEC_ID_cmte = "C00473827",
    FEC_ID_cand = "S0UT00165"
  ),
  # UT: Utah III (last updated Nov 7, 2020) ====================================
  # Not up for reelection 2020.
  # Mitt Romney is uncontested (no candidates registered as of Jan 30, 2020)
  tibble(
    first_name = "Mitt",
    last_name = "Romney",
    state = "UT",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.romney.senate.gov",
    campaign_name = "Romney for Utah",
    campaign_website = "https://www.romneyforutah.com/",
    actblue_link = NA,
    anedot_link = NA,
    # Uh... link gives 404 error. Oh dear.
    # other_link = "https://winred.revv.co/romney-for-utah-inc/website",
    # Link corrected as of Sep 26, 2019
    other_link = "https://secure.winred.com/mitt-romney/website",
    FEC_ID_cmte = "C00670695",
    FEC_ID_cand = "S8UT00176"
  ),
  # VT: Vermont I (last updated Nov 7, 2020) ===================================
  # Not up for reelection 2020.
  # Bernie Sanders is uncontested (no candidates registered as of Jan 30, 2020)
  # Candidate still running for president ---> now dropped out
  tibble(
    first_name = "Bernie",
    last_name = "Sanders",
    state = "VT",
    party = "Ind",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.sanders.senate.gov",
    # Now only Senate campaign
    campaign_name = "Bernie",
    campaign_website = "https://berniesanders.com",
    actblue_link = "not_me_us",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00411330",
    FEC_ID_cand = "S4VT00033"
  ),
  # VT: Vermont III (last updated Nov 7, 2020) =================================
  # Not up for reelection 2020.
  # Patrick Leahy is uncontested (no candidates registered as of Jan 30, 2020)
  tibble(
    first_name = "Patrick",
    last_name = "Leahy",
    state = "VT",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.leahy.senate.gov",
    campaign_name = "Leahy for Vermont",
    campaign_website = "https://www.leahyforvermont.com/",
    actblue_link = "leahyforsenate",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00068353",
    FEC_ID_cand = "S4VT00017"
  ),
  # VA: Virginia II (last updated Nov 4, 2020) =================================
  # Warner vs. Williams
  # vs. Taylor vs. Gade vs. Speciale vs. Adkins vs. Baldwin
  # vs. Mina vs. Franklin vs. Knapp
  # Can't find Roger K Franklin Jr., Aldous Mina, Mary Knapp
  tibble(
    first_name = "Mark",
    last_name = "Warner",
    state = "VA",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "www.warner.senate.gov",
    campaign_name = "Mark Warner for Virginia",
    campaign_website = "https://markwarnerva.com/",
    actblue_link = "mrw-web",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00438713",
    FEC_ID_cand = "S6VA00093"
  ),
  tibble(
    first_name = "Daniel",
    last_name = "Gade",
    state = "VA",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gade for Virginia",
    campaign_website = "https://www.gadeforvirginia.com/",
    actblue_link = NA,
    # Change within Anedot
    # from 10-25-50-100-250-500-1000-2800
    # to 10-25-50-100-250-500-1000-2800-5600
    anedot_link = "gade-for-virginia-inc/c5c00f1637518ed1164cb",
    # "gade-for-virginia-inc/donate",
    other_link = NA,
    FEC_ID_cmte = "C00716571",
    FEC_ID_cand = "S0VA00146"
  ),
  tibble(
    first_name = "Scott",
    last_name = "Taylor",
    state = "VA",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Taylor for Senate",
    campaign_website = "https://www.taylorforsenate.com/",
    actblue_link = NA,
    # Change documented to WinRed on Jan 31, 2020
    # to 25-50-100-250-500-1000-2800-5600 (Anedot has been paused)
    # anedot_link = "scott-taylor/taylorforsenate",
    anedot_link = NA,
    other_link = "https://secure.winred.com/scotttaylor/donate",
    FEC_ID_cmte = "C00711341",
    FEC_ID_cand = "S0VA00104"
  ),
  tibble(
    first_name = "Victor",
    last_name = "Williams",
    state = "VA",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Williams for US Senate",
    campaign_website = "https://www.vw4v.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-150-250-500-1000-1500-2000-2500
    # other_link = paste0(
    #   "https://secure.campaigncontributions.net/",
    #   "80620/Contribute-to-Victor-Williams"
    # ),
    # Changed links
    # Now 10-25-50-100-150-250-500-1000-15000-2000-2700
    other_link = "https://www.vw4v.com/contribute",
    FEC_ID_cmte = "C00710368",
    FEC_ID_cand = "S0VA00120"
  ),
  tibble(
    first_name = "Thomas",
    last_name = "Speciale",
    state = "VA",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Tom Speciale for Virginia",
    campaign_website = "https://thomasspeciale.com",
    actblue_link = NA,
    anedot_link = "tom-speciale-for-virginia/website-splash",
    other_link = NA,
    FEC_ID_cmte = "C00710608",
    FEC_ID_cand = "S0VA00088"
  ),
  # Adkins withdrew on Dec 26, 2019
  tibble(
    first_name = "Gary",
    last_name = "Adkins",
    state = "VA",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Gary Adkins for Senate",
    campaign_website = "https://www.garyadkinsforsenate.com/",
    actblue_link = NA,
    anedot_link = "gary-adkins-for-senate/donate",
    other_link = NA,
    FEC_ID_cmte = "C00711242",
    FEC_ID_cand = "S0VA00096"
  ),
  tibble(
    first_name = "Alissa",
    last_name = "Baldwin",
    state = "VA",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Baldwin for United States Senate",
    campaign_website = "https://www.alissabaldwin.com/",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-150-250-500-1000-1500-2000-2500
    other_link = "https://secure.campaigncontributions.net/80760/",
    FEC_ID_cmte = "C00710392",
    FEC_ID_cand = "S0VA00070"
  ),
  tibble(
    first_name = "Aldous",
    last_name = "Mina",
    state = "VA",
    party = "Ind",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Aldous Mina for Senate 2020",
    campaign_website = "https://www.facebook.com/AlMinaUSSenate2020/",
    # Nothing...
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00712836",
    FEC_ID_cand = "S0VA00112"
  ),
  # Cannot find Franklin
  tibble(
    first_name = "Roger",
    last_name = "Franklin",
    state = "VA",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "",
    campaign_website = "",
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = c("C00714980", "C00717561"),
    FEC_ID_cand = "S0VA00138"
  ),
  tibble(
    first_name = "Mary",
    last_name = "Knapp",
    state = "VA",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Mary Knapp for U.s. Senate",
    campaign_website = "https://maryknapp.com",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/MaryKnapp/donate",
    FEC_ID_cmte = "C00722645",
    FEC_ID_cand = "S0VA00153"
  ),
  # VA: Virginia I (last updated Nov 7, 2020) ==================================
  # Not up for reelection 2020.
  # Tim Kaine is uncontested (no candidates registered as of Jan 31, 2020)
  tibble(
    first_name = "Tim",
    last_name = "Kaine",
    state = "VA",
    party = "Dem",
    class = "I",
    incumbent = TRUE,
    office_website = "www.kaine.senate.gov",
    campaign_name = "Kaine for Virginia",
    campaign_website = "https://timkaine.com/",
    actblue_link = "tk-homepage",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00495358",
    FEC_ID_cand = "S2VA00142"
  ),
  # WA: Washington I (last updated Nov 7, 2020) ================================
  # Not up for reelection 2020.
  # Maria Cantwell is uncontested (no candidates registered as of Jan 31, 2020)
  tibble(
    first_name = "Maria",
    last_name = "Cantwell",
    state = "WA",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.cantwell.senate.gov",
    campaign_name = "Maria Cantwell for Senate",
    campaign_website = "https://www.cantwell.com/",
    actblue_link = "maria-cantwell-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00349506",
    FEC_ID_cand = "S8WA00194"
  ),
  # WA: Washington III (last updated Nov 7, 2020) ==============================
  # Not up for reelection 2020.
  # Patty Murray is uncontested (no candidates registered as of Jan 31, 2020)
  tibble(
    first_name = "Patty",
    last_name = "Murray",
    state = "WA",
    party = "Dem",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.murray.senate.gov",
    campaign_name = "Patty Murray for U.S. Senate",
    campaign_website = "https://pattymurray.com/",
    actblue_link = "pm-website",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00257642",
    FEC_ID_cand = "S2WA00189"
  ),
  tibble(
    first_name = "Nicolaus",
    last_name = "Sleister",
    state = "WA",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Nick 4 WA",
    campaign_website = "https://www.nick4wa.com",
    # Not ActBlue but Anedot(!)
    actblue_link = NA,
    anedot_link = "nick4wa/donate",
    other_link = NA,
    FEC_ID_cmte = "C00750737",
    FEC_ID_cand = "S2WA00338"
  ),
  # WV: West Virginia II (last updated Jul 5, 2021) ============================
  # Shelley Moore Capito had no notable opponents
  # (Bruce Wiley not found, zero receipts, party unknown as of Sep 11, 2019)
  # Now Capito vs. Swearegin, Whitt vs. Riley vs. Ojeda vs. Moran
  # David Moran is not up on the FEC
  tibble(
    first_name = "Shelley Moore",
    last_name = "Capito",
    state = "WV",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.capito.senate.gov",
    campaign_name = "Capito for West Virginia",
    campaign_website = "http://capito2020.com/",
    # Change documented to WinRed on Oct 28, 2019 (not sure when)
    # 50-100-150-250-500-1000-2800-5600-11200 to 50-100-250-500-1000-2800-5600
    actblue_link = NA,
    anedot_link = NA, # "capitoforsenate/donate"
    other_link = "https://secure.winred.com/shelley-moore-capito/donate",
    FEC_ID_cmte = "C00539825",
    FEC_ID_cand = "S4WV00159"
  ),
  tibble(
    first_name = "Paula Jean",
    last_name = "Swearengin",
    state = "WV",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Paula Jean 2020",
    campaign_website = "https://paulajean.com",
    actblue_link = "paula-jean-swearengin-2020",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00708891",
    FEC_ID_cand = "S0WV00215"
  ),
  tibble(
    first_name = "Allen", # Larry Allen, goes by Allen
    last_name = "Whitt",
    state = "WV",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Allen Whitt for U.S. Senate",
    campaign_website = "https://whittforwv.com",
    actblue_link = NA,
    anedot_link = NA,
    # 10-25-50-100-250-500-1000-2800-5000-5600
    other_link = "https://whittforwv.com/products/donate",
    FEC_ID_cmte = "C00723502",
    FEC_ID_cand = "S0WV00231"
  ),
  tibble(
    first_name = "Richard",
    last_name = "Ojeda",
    state = "WV",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Vote Ojeda",
    campaign_website = "https://voteojeda.com",
    # No donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00734384",
    FEC_ID_cand = "S0WV00249"
  ),
  tibble(
    first_name = "David",
    last_name = "Moran",
    state = "WV",
    party = "Lib",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "David Moran for U.S. Senate",
    campaign_website = "https://www.facebook.com/moran4wv/",
    # No donation link
    actblue_link = NA,
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = NA,
    FEC_ID_cand = "S0WV00264"
  ),
  # WV: West Virginia I (last updated Nov 7, 2020) =============================
  # Not up for reelection 2020.
  # Joe Manchin is uncontested (no candidates registered as of Jan 31, 2020)
  tibble(
    first_name = "Joe",
    last_name = "Manchin",
    state = "WV",
    party = "Dem",
    class = "I",
    incumbent = TRUE,
    office_website = "https://www.manchin.senate.gov",
    campaign_name = "Manchin for West Virginia",
    campaign_website = "https://joemanchinwv.com/",
    actblue_link = c(
      "joe-manchin-2",
      "jmquickdonate" # New link added along with the original
    ),
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00486563",
    FEC_ID_cand = "S0WV00090"
  ),
  # WI: Wisconsin I (last updated Nov 4, 2020) =================================
  # Not up for reelection 2020.
  # Tammy Baldwin is uncontested (no candidates registered as of Jan 31, 2020)
  tibble(
    first_name = "Tammy",
    last_name = "Baldwin",
    state = "WI",
    party = "Dem",
    class = "I",
    incumbent = TRUE,
    office_website = "https://www.baldwin.senate.gov/",
    campaign_name = "Tammy Baldwin for Senate",
    campaign_website = "https://www.tammybaldwin.com/",
    actblue_link = "tbdefault-2",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00326801",
    FEC_ID_cand = "S2WI00219"
  ),
  # WI: Wisconsin III (last updated Nov 4, 2020) ===============================
  # Not up for reelection 2020.
  # Ron Johnson is uncontested (no candidates registered as of Sep 11, 2019)
  tibble(
    first_name = "Ron",
    last_name = "Johnson",
    state = "WI",
    party = "Rep",
    class = "III",
    incumbent = TRUE,
    office_website = "https://www.ronjohnson.senate.gov",
    campaign_name = "Ron Johnson for Wisconson",
    campaign_website = "https://www.ronjohnsonforsenate.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://secure.winred.com/ron-johnson/website",
    FEC_ID_cmte = "C00482984",
    FEC_ID_cand = "S0WI00197"
  ),
  # WY: Wyoming II (last updated Nov 4, 2020) ==================================
  # Mike Enzi
  tibble(
    first_name = "Michael B.",
    last_name = "Enzi",
    state = "WY",
    party = "Rep",
    class = "II",
    incumbent = TRUE,
    office_website = "https://www.enzi.senate.gov/public/",
    campaign_name = "Enzi for U.S. Senate",
    campaign_website = "http://enziforwyoming.com/",
    actblue_link = NA,
    # Donation has been paused (!)
    # Because he will retire and not run for reelection
    # See https://ballotpedia.org/Mike_Enzi
    other_link = NA,
    FEC_ID_cmte = "C00317503",
    FEC_ID_cand = "S6WY00126"
  ),
  # Open seat candidate Cynthia Lummis (was at-large House rep)
  # Other candidates than Wheeler were not viable:
  # see https://www.gillettenewsrecord.com/news/wyoming/
  # article_595d8b00-5527-560f-8aba-64eb0a9cc0ac.html
  # Now Lummis vs. Ludwig vs. Wheeler vs. Jagoda vs. Ben David
  tibble(
    first_name = "Cynthia Marie",
    last_name = "Lummis",
    state = "WY",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Lummis for Wyoming",
    campaign_website = "https://www.lummisforwyoming.com/",
    actblue_link = NA,
    # Still Anedot
    # anedot_link = "lummis-for-wyoming-u-s-senate/cdf4eba3ac96df398da78",
    # Change within Anedot, but essentially the same
    anedot_link = "lummis-for-wyoming-u-s-senate/general-donate",
    other_link = NA,
    FEC_ID_cmte = "C00443580",
    FEC_ID_cand = "S0WY00137"
  ),
  tibble(
    first_name = "Merav",
    last_name = "Ben-David",
    state = "WY",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Ben-David 2020",
    campaign_website = "https://www.bendavid2020.com",
    actblue_link = "bendavid2020-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00734152",
    FEC_ID_cand = "S0WY00152"
  ),
  tibble(
    first_name = "Joshua",
    last_name = "Wheeler",
    state = "WY",
    party = "Rep",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Wheeler 4 U.S. Senate",
    campaign_website = "https://fighting4wyoming.com/",
    actblue_link = NA,
    anedot_link = NA,
    other_link = "https://politics.raisethemoney.com/jwheeler?iframe=true",
    FEC_ID_cmte = "C00698811",
    FEC_ID_cand = "S0WY00103"
  ),
  tibble(
    first_name = "Yana",
    last_name = "Ludwig",
    state = "WY",
    party = "Dem",
    class = "II",
    incumbent = FALSE,
    office_website = NA,
    campaign_name = "Yana for Senate",
    campaign_website = "https://www.yana4wyo.com",
    actblue_link = "yana-for-wyo-1",
    anedot_link = NA,
    other_link = NA,
    FEC_ID_cmte = "C00708982",
    FEC_ID_cand = "S0WY00129"
  ),
  # Jagoda is not a viable candidate
  # WY: Wyoming I (last updated Nov 4, 2020) ===================================
  # Not up for reelection 2020.
  # John Barrasso is uncontested (no candidates registered as of Jan 31, 2020)
  tibble(
    first_name = "John",
    last_name = "Barrasso",
    state = "WY",
    party = "Rep",
    class = "I",
    incumbent = TRUE,
    office_website = "https://www.barrasso.senate.gov",
    campaign_name = "Barrasso for Wyoming",
    campaign_website = "https://www.barrassoforwyoming.com/",
    # Change detected to WinRed on Oct 27, 2019 (looks like from Oct 25)
    # 25-50-100-250-500-1000-2700-5400 to 25-50-100-250-500-1000-2800
    actblue_link = NA,
    # anedot_link = "senator-john-barrasso/general-fund-cd5d574e0a127cde1b760",
    anedot_link = NA,
    other_link = "https://secure.winred.com/barrasso/donate",
    FEC_ID_cmte = "C00436386",
    FEC_ID_cand = "S6WY00068"
  )
  # Unroll and tidy ===========================================================
) %>%
  bind_rows() %>%
  mutate(year = 2020) %>%
  filter(last_name != "") %>%
  rowwise() %>%
  mutate(
    actblue_link = paste0("https://secure.actblue.com/donate/", actblue_link),
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
