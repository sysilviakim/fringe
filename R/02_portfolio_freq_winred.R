categories <- "winred"
source(here::here("R", "01_data_import.R"))

# Wrote function: portfolio_summ ===============================================
df_raw <- df_raw %>%
  # To kick out duplicates
  mutate(url = gsub("\\?sc=winred-directory", "", url)) %>%
  mutate(
    name = gsub(
      paste0(
        " for Congress|For Congress|Friends of |Committee to elect |",
        " 6th District| Inc.| for U.S. Senate,"
      ),
      "", name
    ),
    name = trimws(name)
  ) %>%
  mutate(
    race = case_when(
      name == "Alison Hayden" & race == "CA-16" ~ "CA-15", 
      name == "Liz Marty May" & race == "SD-1" ~ "SD-AL",
      name == "Matthew Morris" & race == "DE-NaN" ~ "DE-AL",
      name == "Scott Perry" & race == "PA-4" ~ "PA-10",
      name == "Vern Buchanan" & race == "FL-13" ~ "FL-16",
      TRUE ~ race
    ),
    race = gsub("-0", "-", race)
  ) %>%
  mutate(
    name = case_when(
      name == "August Plugger" & grepl("august-pfluger-for-congress", url) ~
        "August Pfluger",
      name == "Ben Sasse for U.S. Senate, Inc." ~ "Ben Sasse",
      name == "Cawthorn for NC" ~ "Madison Cawthorn",
      name == "Elect Tuman" ~ "Doug Tuman",
      name == "Garske" ~ "John Garske",
      # GOP Strategist; seems a mistake
      name == "Keats Norfleett" & grepl("gohmert", url) ~ "Louie Gohmert",
      name == "MANGA Anantatmula" ~ "Manga Anantatmula",
      name == "Marianette Miller-Meeks" ~ "Mariannette Miller-Meeks",
      name == "Jon Huey" & grepl("mcclintock", url) ~ "Tom McClintock",
      name == "Nick Lalota" ~ "Nick LaLota",
      name == "steven raiser" ~ "Steven Raiser",
      name == "MANGA FOR CONGRESS" ~ "Manga Anantatmula",
      name == "Virdell" ~ "Wesley Virdell",
      name == "Oristian" ~ "Michael Oristian",
      name == "Dasha" ~ "Dasha Pruett",
      name == "Team Scalise" ~ "Steve Scalise",
      name == "Joe Rae Perkins" ~ "Jo Rae Perkins",
      name == "RaylaForCongress" ~ "Rayla Campbell",
      name == "Langworthy" ~ "Charles Langworthy",
      name == "Lancia" ~ "Robert Lancia",
      name == "Neese" ~ "Terry Neese",
      TRUE ~ name
    )
  )

temp <- df_raw %>%
  # Changed jurisdiction but keep the record
  # filter(!(name == "Jimmy Rodriguez" & race == "AZ-8")) %>%
  filter(!is.na(url)) %>%
  group_split(url) %>%
  map_dfr(~ portfolio_summ(.x, order_vars = c("race", "name"))) %>%
  mutate(
    class = case_when(
      grepl("-", race) & !grepl("-SEN", race) ~ "us house",
      grepl("-SEN", race) ~ "us senate",
      grepl("Party", race) ~ "party",
      grepl("President", race) ~ "pres"
    )
  ) %>%
  arrange(race, name)

assert_that(all(temp$min <= temp$max))
temp %>%
  .$class %>%
  table(useNA = "ifany")

head(sort(table(temp$amount), decreasing = TRUE), 10)

# In WinRed's case, there should be no within-URL changes ======================
temp %>%
  group_by(url, seq, amount) %>%
  filter(n() > 1) %>%
  nrow() %>%
  {assert_that(. == 0)}

# Top 5 Most Frequent Distributions ============================================
temp <- portfolio_na_fig_label(temp)
p <- prop(temp, "amount", sort = TRUE, head = 5, print = FALSE) %>%
  unlist() %>%
  set_names(., nm = names(.)) %>%
  imap(~ tibble(label = .y, freq = as.numeric(.x))) %>%
  bind_rows() %>%
  mutate(
    label = gsub("-", "\n", label),
    label = factor(
      label,
      levels = gsub(
        "-", "\n",
        names(prop(temp, "amount", sort = TRUE, head = 5, print = FALSE))
      )
    )
  ) %>%
  ggplot(aes(x = label, y = freq)) +
  geom_bar(stat = "identity") +
  # xlab("\nSolicitation Amounts, Top 5, WinRed Directory") +
  xlab(NULL) + 
  ylab("Percentage (%)") +
  scale_y_continuous(limits = c(0, 50))

pdf(here("fig/portfolio_freq_top_5_winred.pdf"), width = 3, height = 3)
print(pdf_default(p))
dev.off()

# Save Output (Check for No-Prompt Referrals) ==================================
entities <- df_raw %>%
  select(!!c("name", "race")) %>%
  mutate(name = gsub(" for Congress", "", name)) %>%
  mutate(
    race = ifelse(name == "Alison Hayden" & race == "CA-16", "CA-15", race),
    race = ifelse(name == "Liz Marty May" & race == "SD-1", "SD-AL", race),
    race = ifelse(name == "Matthew Morris" & race == "DE-NaN", "DE-AL", race),
    race = ifelse(name == "Scott Perry" & race == "PA-4", "PA-10", race),
    race = ifelse(name == "Vern Buchanan" & race == "FL-13", "FL-16", race)
  ) %>%
  dedup()

View(anti_join(entities, temp))
nrow(full_join(temp, entities))
winred <- full_join(temp, entities)

save(winred, file = here("data/tidy/portfolio_summ_winred.Rda"))
