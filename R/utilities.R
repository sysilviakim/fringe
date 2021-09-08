# Libraries ====================================================================
library(plyr)
library(tidyverse)
library(lubridate)
library(rvest)
library(here)

library(fst)
library(assertthat)
library(jsonlite)
library(janitor)
library(data.table)
library(xtable)

library(Kmisc)

# Functions ====================================================================
portfolio_summ <- function(df,
                           values_from = "portfolio",
                           order_vars = c("name", "url")) {
  # First, take some necessary steps to trim and clean
  df <- df %>%
    mutate(across(everything(), trimws)) %>%
    # If it is 25-100-250-100-NA, for example, NA was for "Other Values"
    # So delete
    group_by(across(-contains(values_from))) %>%
    filter(
      !(n_distinct(!!as.name(values_from)) > 1 &
        is.na(!!as.name(values_from)))
    ) %>%
    group_by(across(-(contains(values_from) | contains("date")))) %>%
    filter(
      # There are other values than NA; NAs are only in the beginning
      # That indicates scraper that was incomplete
      # Delete these
      !(n_distinct(rleid(!!as.name(values_from))) > 2 &
        rleid(!!as.name(values_from)) == 1 &
        is.na(!!as.name(values_from)))
    ) %>%
    ungroup() %>%
    group_by(across(all_of(order_vars))) %>%
    group_split() %>%
    map_dfr(
      ~ .x %>%
        arrange(date) %>%
        filter(
          # Before and after have no NA values; just this one
          !(!is.na(lag(portfolio)) & (lag(date) != date) &
            !is.na(lead(portfolio)) & (lead(date) != date) &
            is.na(portfolio))
        ) %>%
        filter(
          # If previously non-NA values, no last NA value
          !(!is.na(lag(portfolio)) & (lag(date) != date) &
            is.na(portfolio) & date == last(date))
        )
    )

  # Substitute NA values
  df[[values_from]][is.na(df[[values_from]])] <- -999

  # Pivot from long to wide, create date-columns
  df <- df %>%
    pivot_wider(
      names_from = date,
      values_from = !!as.name(values_from),
      names_prefix = "date_",
      values_fn = list
    ) %>%
    clean_names() %>%
    arrange(across(all_of(order_vars))) %>%
    ungroup()

  # Using all combinations of entities (name-URL) and dates, create nested list
  temp <- cross2(
    df %>% select(contains("date_")) %>% names(),
    seq(nrow(df))
  ) %>%
    map(
      ~ {
        out <- list()
        out[[paste0("row_", str_pad(.x[[2]], width = 5, pad = "0"))]] <-
          df[.x[[2]], .x[[1]], drop = TRUE]
        return(out)
      }
    )

  # Derive unique menus for each entity, excluding NULL and NA values
  menu_list <- seq(nrow(df)) %>%
    map(
      ~ unique(
        temp[
          seq(
            1, length(temp),
            by = df %>% select(contains("date_")) %>% ncol()
          )[.x]:
          seq(
            df %>% select(contains("date_")) %>% ncol(), length(temp),
            by = df %>% select(contains("date_")) %>% ncol()
          )[.x]
        ]
      ) %>%
        bind_rows() %>%
        .[, 1, drop = TRUE]
    ) %>%
    map(~ Filter(Negate(is.null), .x)) %>%
    map(~ Filter(function(x) any(!is.na(x)), .x))

  # For each entity, derive first date a given menu appears in data,
  # + last date, and the menu itself. Bind it with identifiers
  out <- list()
  for (i in seq(nrow(df))) {
    indv <- df[i, ] %>%
      select(contains("date_")) %>%
      map(~ paste(na.omit(unlist(.x)), collapse = "-")) %>%
      unlist() %>%
      .[. != ""] %>%
      .[. != "NA"] %>%
      na.omit()

    out[[i]] <- seq(unique(rleid(indv))) %>%
      set_names(., .) %>%
      map(
        ~ tibble(
          min = names(indv)[min(which(.x == rleid(indv)))],
          max = names(indv)[max(which(.x == rleid(indv)))],
          amount = df[
            i, names(indv)[min(which(.x == rleid(indv)))],
            drop = TRUE
          ] %>%
            unlist() %>%
            paste0(collapse = "-")
        )
      ) %>%
      bind_rows() %>%
      arrange(min) %>%
      row_seq(row = "seq_url")

    out[[i]] <- bind_cols(
      df[i, ] %>%
        select(-contains("date_")) %>%
        slice(rep(1:n(), each = nrow(out[[i]]))),
      out[[i]]
    )
  }
  return(
    out %>%
      bind_rows() %>%
      mutate(across(c("min", "max"), ~ ymd(gsub("date|_", "", .x)))) %>%
      group_by(
        across(setdiff(names(.), c("min", "max", "amount", "url", "seq_url")))
      ) %>%
      group_split() %>%
      map_dfr(
        ~ .x %>%
        arrange(min) %>%
        row_seq(row = "seq")
      ) %>%
      ungroup() %>%
      arrange(across(all_of(c(setdiff(order_vars, "url"), "min"))))
  )
}

summ_calc_fxn <- function(df) {
  df %>%
    rename(min_date = min, max_date = max) %>%
    mutate(
      amount = case_when(
        amount == "0-1-2-3" ~ NA_character_,
        amount == "-999" ~ NA_character_,
        amount == "No\nSuggested\nAmounts" ~ NA_character_,
        TRUE ~ gsub("^0-", "", amount)
      )
    ) %>%
    {
      bind_rows(
        filter(., is.na(amount)) %>%
          rowwise() %>%
          mutate(
            min = NA, max = NA, mean = NA, median = NA,
            q1 = NA, q3 = NA, first = NA, last = NA,
            choices = 0
          ),
        filter(., !is.na(amount)) %>%
          rowwise() %>%
          mutate(
            min = amount_split(amount) %>% min(., na.rm = TRUE),
            max = amount_split(amount) %>% max(., na.rm = TRUE),
            mean = amount_split(amount) %>% mean(., na.rm = TRUE),
            median = amount_split(amount) %>% median(., na.rm = TRUE),
            q1 = amount_split(amount) %>% summary() %>% .[["1st Qu."]],
            q3 = amount_split(amount) %>% summary() %>% .[["3rd Qu."]],
            first = amount_split(amount) %>% .[1],
            last = amount_split(amount) %>% .[length(.)],
            choices = amount_split(amount) %>% length(),
            ineff_2700 = case_when(
              2700 %in% amount_split(amount) ~ 1,
              TRUE ~ 0
            ),
            ineff_2800 = case_when(
              2800 %in% amount_split(amount) ~ 1,
              TRUE ~ 0
            ),
            ineff_2900 = case_when(
              2900 %in% amount_split(amount) ~ 1,
              TRUE ~ 0
            ),
            sanders = case_when(
              27 %in% amount_split(amount) ~ 1,
              TRUE ~ 0
            ),
            beyond_max = case_when(
              !is.na(max) & max > 2800 ~ 1,
              TRUE ~ 0
            )
          )
      )
    }
}

fundraising_actblue <- function(x) {
  out <- read_html(x) %>%
    html_nodes(xpath = ".//script[contains(., 'preloadedState')]") %>%
    html_text() %>%
    as.character() %>%
    str_match_all(., '(?<="amounts":\\[)(\\d+,?)+') %>%
    .[[1]] %>%
    .[, 1] %>%
    str_split(",") %>%
    unlist() %>%
    as.numeric() %>%
    (function(x) x / 100)
  return(out)
}

actblue_js <- function(url) {
  x <- read_html(url) %>%
    html_nodes(xpath = '//*[@type="text/javascript"]') %>%
    html_text()

  temp <- x %>%
    map(
      ~ {
        temp <- str_match_all(.x, "window.indigoListResponse = (\\{.*\\})")
        if (!is.null(temp)) {
          temp[[1]][, 2]
        }
      }
    ) %>%
    unlist() %>%
    fromJSON()

  # temp$entities
  # temp three types: vector, dataframe, value
  temp_df <- temp %>%
    map(is.data.frame) %>%
    unlist() %>%
    which() %>%
    names()

  temp_vc <- temp %>%
    map(~ length(.x) > 1 & !is.data.frame(.x)) %>%
    unlist() %>%
    which() %>%
    names()

  json_rest <- names(temp) %>%
    map(
      ~ {
        if (
          !is.null(temp[[.x]]) &
            !(
              .x %in% c(
                # "entities", "post_donation_upsells", "brandings",
                temp_df,
                # "relevant_surrogate_keys", "share_content", "radio_amounts",
                # "eligibility_values", "eligibility_values_es",
                # "list_disclaimer_policy", "acceptable_card_types",
                # "managing_entity", "fundraising_video", "custom_fields"
                temp_vc
              )
            )
        ) {
          tibble(!!as.name(.x) := temp[[.x]])
        } else {
          NULL
        }
      }
    ) %>%
    keep(~ !is.null(.x)) %>%
    keep(~ nrow(.x) > 0) %>%
    bind_cols()

  out <- tibble(url = url, js_rest = json_rest)
  for (i in c(temp_df, temp_vc)) {
    out[[i]] <- list(temp[[i]])
  }
  assert_that(nrow(out) == 1)

  return(out)
}

amount_split <- function(amount) {
  out <- str_split(gsub("--999|^-999", "", amount), pattern = "-") %>%
    unlist() %>%
    as.numeric() %>%
    na.omit() %>%
    as.vector()
  if (length(out) == 0) {
    return(NA)
  } else {
    return(out)
  }
}

platform_names <- function(df, var = "Platform") {
  df %>%
    mutate(
      !!as.name(var) := case_when(
        !!as.name(var) == "winred" ~ "WinRed",
        !!as.name(var) == "rightus" ~ "Right.us",
        !!as.name(var) == "actblue" ~ "ActBlue"
      )
    )
}

fed_exception_vars <- function(x) {
  if (x == "president") {
    ex <- c("last_name", "url", "year")
  } else if (x == "senate") {
    ex <- c("state", "last_name", "url", "year")
  } else {
    ex <- c("state", "state_cd", "last_name", "url", "year")
  }
  return(ex)
}

portfolio_na_fig_label <- function(df) {
  df %>%
    mutate(
      amount = case_when(
        is.na(amount) ~ "No\nSuggested\nAmounts",
        amount == "-999" ~ "No\nSuggested\nAmounts",
        amount == "0-1-2-3" ~ "No\nSuggested\nAmounts",
        TRUE ~ amount
      )
    ) %>%
    mutate(amount = gsub("--999", "", amount))
}

## 2010-06-25
## (c) Felix Andrews <felix@nfrac.org>
## GPL-2

## Gist at https://gist.github.com/floybix/452201
## Modified to take characters into account (formatC outputs)
## And changed textbf to textcolor

highlight_xtable <-
  function(x, which = NULL, each = c("column", "row"), max = TRUE,
           NA.string = "", type = c("latex", "html"),
           sanitize.text.function = force,
           sanitize.rownames.function = NULL,
           sanitize.colnames.function = NULL, ...) {
    stopifnot(inherits(x, "xtable"))
    each <- match.arg(each)
    type <- match.arg(type)
    digits <- rep(digits(x), length = ncol(x) + 1)
    if (!is.null(which)) {
      stopifnot(nrow(which) == nrow(x))
      stopifnot(ncol(which) == ncol(x))
      boldmatrix <- which
    } else {
      boldmatrix <- matrix(FALSE, ncol = ncol(x), nrow = nrow(x))
      ## round values before calculating max/min to avoid trivial diffs
      for (i in 1:ncol(x)) {
        if (!is.numeric(x[, i])) {
          if (!is.na(parse_number(x[, i]))) {
            temp <- parse_number(x[, i])
            if (!is.na(temp) & !is.na(as.numeric(x[, i]))) {
              if (nchar(x[, i]) == nchar(as.numeric(x[, i]))) {
                x[, i] <- temp
              }
            }
          } else {
            next
          }
        } else {
          x[, i] <- round(x[, i], digits = digits[i + 1])
        }
      }
      if (each == "column") {
        max <- rep(max, length = ncol(x))
        for (i in 1:ncol(x)) {
          xi <- x[, i]
          if (!is.numeric(xi)) next
          if (is.na(max[i])) next
          imax <- max(xi, na.rm = TRUE)
          if (!max[i]) {
            imax <- min(xi, na.rm = TRUE)
          }
          boldmatrix[xi == imax, i] <- TRUE
        }
      } else if (each == "row") {
        max <- rep(max, length = nrow(x))
        for (i in 1:nrow(x)) {
          xi <- x[i, ]
          ok <- sapply(xi, is.numeric)
          if (!any(ok)) next
          if (is.na(max[i])) next
          imax <- max(unlist(xi[ok]), na.rm = TRUE)
          if (!max[i]) {
            imax <- min(unlist(xi[ok]), na.rm = TRUE)
          }
          whichmax <- sapply(xi, identical, imax)
          boldmatrix[i, whichmax] <- TRUE
        }
      }
    }
    ## need to convert to character
    ## only support per-column formats, not cell formats
    display <- rep(display(x), length = ncol(x) + 1)
    for (i in 1:ncol(x)) {
      if (!is.numeric(x[, i])) next
      ina <- is.na(x[, i])
      x[, i] <- formatC(x[, i],
        digits = digits[i + 1],
        format = display[i + 1]
      )
      x[ina, i] <- NA.string
      display(x)[i + 1] <- "s"
      ## embolden
      yes <- boldmatrix[, i]
      if (type == "latex") {
        x[yes, i] <- paste("\\textcolor{vermillion}{", x[yes, i], "}", sep = "")
      } else {
        x[yes, i] <- paste("<strong>", x[yes, i], "</strong>", sep = "")
      }
    }
    print(x, ...,
      type = type, NA.string = NA.string,
      sanitize.text.function = sanitize.text.function,
      sanitize.rownames.function = sanitize.rownames.function,
      sanitize.colnames.function = sanitize.colnames.function
    )
  }

broom_custom <- function(x) {
  x %>%
    broom::tidy() %>%
    mutate(across(where(is.numeric), round, 5))
}

scatter_custom <- function(df, xvar, yvar = "mean", xlab = NULL) {
  p <- ggplot(
    df %>% 
      rename(Party = party) %>%
      mutate(Party = simple_cap(tolower(Party))), 
    aes(
      x = !!as.name(xvar), y = !!as.name(yvar), 
      group = Party, fill = Party, colour = Party
    )
  ) +
    geom_point() + 
    scale_color_manual(values = c("#67a9cf", "#000000", "#ef8a62")) + 
    scale_y_continuous(labels = scales::comma) + 
    ylab(simple_cap(yvar))
  if (!is.null(xlab)) {
    p <- p + xlab(xlab)
  }
  pdf_default(p)
}

# Other options ================================================================
options(scipen = 999)
