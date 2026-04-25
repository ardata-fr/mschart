test_that("ms_stockchart accepts an x column literally named 'group'", {
  df <- data.frame(
    group = c("A", "B", "C", "D"),
    high  = c(11, 15, 10, 18),
    low   = c(9,  13,  8, 16),
    close = c(10, 14,  9, 17)
  )

  expect_no_error({
    chart <- ms_stockchart(
      df,
      x = "group", high = "high", low = "low", close = "close"
    )
    format(
      chart,
      sheetname = "sheet1",
      id_x = "64451212", id_y = "64453248"
    )
  })
})
