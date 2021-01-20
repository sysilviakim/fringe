# Libraries ====================================================================
library(plyr)
library(tidyverse)
library(lubridate)
library(rvest)
library(here)

library(fst)
library(assertthat)
library(jsonlite)

library(Kmisc)

# Functions ====================================================================
portfolio_summ <- function(df,
                           names_from = "date",
                           values_from = "portfolio",
                           exclude_cols = c("name", "race", "url"),
                           order_vars = c("name", "url")) {
  
  # Pivot from long to wide, create date-columns
  df <- df %>%
    pivot_wider(
      names_from = !!as.name(names_from),
      values_from = !!as.name(values_from),
      names_prefix = "date_",
      values_fn = list
    ) %>%
    clean_names() %>%
    arrange(!!!order_vars)
  
  # Using all combinations of entities (name-URL) and dates, create nested list
  temp <- cross2(
    df %>% select(-!!exclude_cols) %>% names(),
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
          seq(1, length(temp), by = (ncol(df) - length(exclude_cols)))[.x]:
            seq(
              ncol(df) - length(exclude_cols), length(temp),
              by = (ncol(df) - length(exclude_cols))
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
      select(-!!exclude_cols) %>%
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
      bind_rows(.id = "seq")
    
    out[[i]] <- bind_cols(
      out[[i]],
      df[i, ] %>%
        select(!!exclude_cols) %>% slice(rep(1:n(), each = nrow(out[[i]])))
    )
  }
  return(
    out %>%
      bind_rows() %>%
      select(!!exclude_cols, everything()) %>%
      mutate_at(vars("min", "max"), ~ ymd(gsub("date|_", "", .x)))
  )
}

# Other options ================================================================
options(scipen = 999)