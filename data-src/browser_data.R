browser_data <- expand.grid(
  browser = c("Chrome", "IE", "Firefox", "Safari", "Opera", "Android"),
  serie = c("serie1", "serie2", "serie3"), stringsAsFactors = FALSE )
browser_data$value <- seq_len(nrow(browser_data))
devtools::use_data(browser_data)
