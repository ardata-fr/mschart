# Stockchart object

Creation of a stock chart object that can be inserted in a 'Microsoft'
document. When `open` is omitted the chart is a High-Low-Close chart.
When `open` is provided it becomes an Open-High-Low-Close chart with
up/down bars (candlestick).

## Usage

``` r
ms_stockchart(data, x, open = NULL, high, low, close)
```

## Arguments

- data:

  a data.frame

- x:

  column name for categories (typically dates)

- open:

  column name for open values (optional, enables OHLC mode)

- high:

  column name for high values

- low:

  column name for low values

- close:

  column name for close values

## Value

An `ms_chart` object.

## See also

[`chart_settings()`](https://ardata-fr.github.io/mschart/reference/chart_settings.md),
[`chart_ax_x()`](https://ardata-fr.github.io/mschart/reference/chart_ax_x.md),
[`chart_ax_y()`](https://ardata-fr.github.io/mschart/reference/chart_ax_y.md),
[`chart_theme()`](https://ardata-fr.github.io/mschart/reference/set_theme.md),
[`chart_labels()`](https://ardata-fr.github.io/mschart/reference/chart_labels.md)

Other 'Office' chart objects:
[`ms_areachart()`](https://ardata-fr.github.io/mschart/reference/ms_areachart.md),
[`ms_barchart()`](https://ardata-fr.github.io/mschart/reference/ms_barchart.md),
[`ms_boxplotchart()`](https://ardata-fr.github.io/mschart/reference/ms_boxplotchart.md),
[`ms_bubblechart()`](https://ardata-fr.github.io/mschart/reference/ms_bubblechart.md),
[`ms_chart_combine()`](https://ardata-fr.github.io/mschart/reference/ms_chart_combine.md),
[`ms_funnelchart()`](https://ardata-fr.github.io/mschart/reference/ms_funnelchart.md),
[`ms_histogramchart()`](https://ardata-fr.github.io/mschart/reference/ms_histogramchart.md),
[`ms_linechart()`](https://ardata-fr.github.io/mschart/reference/ms_linechart.md),
[`ms_paretochart()`](https://ardata-fr.github.io/mschart/reference/ms_paretochart.md),
[`ms_piechart()`](https://ardata-fr.github.io/mschart/reference/ms_piechart.md),
[`ms_radarchart()`](https://ardata-fr.github.io/mschart/reference/ms_radarchart.md),
[`ms_scatterchart()`](https://ardata-fr.github.io/mschart/reference/ms_scatterchart.md),
[`ms_sunburstchart()`](https://ardata-fr.github.io/mschart/reference/ms_sunburstchart.md),
[`ms_treemapchart()`](https://ardata-fr.github.io/mschart/reference/ms_treemapchart.md),
[`ms_waterfallchart()`](https://ardata-fr.github.io/mschart/reference/ms_waterfallchart.md)

## Examples

``` r
library(officer)

dat <- data.frame(
  date = as.Date("2024-01-01") + 0:4,
  open = c(44, 25, 38, 50, 34),
  high = c(55, 57, 57, 58, 58),
  low = c(11, 12, 13, 11, 25),
  close = c(32, 35, 34, 35, 43)
)

# HLC chart
stock_hlc <- ms_stockchart(
  data = dat, x = "date",
  high = "high", low = "low", close = "close"
)
stock_hlc
#> * 'ms_stockchart' object
#> 
#> * original data [15,3] (sample):
#>         date .mschart_y .mschart_group
#> 1 2024-01-01         55           high
#> 2 2024-01-02         57           high
#> 3 2024-01-03         57           high
#> 4 2024-01-04         58           high
#> 5 2024-01-05         58           high
#> 
#> * series data [5,4] (sample):
#>         date high low close
#> 1 2024-01-01   55  11    32
#> 2 2024-01-02   57  12    35
#> 3 2024-01-03   57  13    34
#> 4 2024-01-04   58  11    35
#> 5 2024-01-05   58  25    43

# OHLC chart (candlestick)
stock_ohlc <- ms_stockchart(
  data = dat, x = "date",
  open = "open", high = "high",
  low = "low", close = "close"
)
stock_ohlc
#> * 'ms_stockchart' object
#> 
#> * original data [20,3] (sample):
#>         date .mschart_y .mschart_group
#> 1 2024-01-01         44           open
#> 2 2024-01-02         25           open
#> 3 2024-01-03         38           open
#> 4 2024-01-04         50           open
#> 5 2024-01-05         34           open
#> 
#> * series data [5,5] (sample):
#>         date open high low close
#> 1 2024-01-01   44   55  11    32
#> 2 2024-01-02   25   57  12    35
#> 3 2024-01-03   38   57  13    34
#> 4 2024-01-04   50   58  11    35
#> 5 2024-01-05   34   58  25    43
```
