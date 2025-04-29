source(here::here("R", "utilities.R"))
load(here("data", "tidy", "mit-tidy.Rda"))
load(here("data", "tidy", "portfolio_summ_federal_first_only.Rda"))
load(here("data", "tidy", "fec_cand_summ_2020.Rda"))

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
  )
) %>%
  bind_rows() %>%
  mutate(
    min = as.Date(min),
    max = as.Date(max),
    last_name = tolower(last_name)
  )

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
      select(-year) %>%
      bind_rows(., house_supp)
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
