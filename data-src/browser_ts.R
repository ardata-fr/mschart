library(purrr)
library(magrittr)
library(tibble)
library(dplyr)
library(tidyr)
library(wakefield)
dates <- as.Date(c("2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2016-03-31"))
n <- rpois(n = length(dates), lambda = 1010) + as.integer(runif(n = length(dates), min = -1, max = 1) * 30)
browser_ts <- map2_df(n, dates, function(n, date) {
  tibble( date = date, browser = internet_browser(n) )
  } ) %>%
  group_by(date, browser) %>%
  tally() %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(freq = n / sum(n)) %>%
  select(-n)

# df[order(browser, df$date),]

devtools::use_data(browser_ts)
