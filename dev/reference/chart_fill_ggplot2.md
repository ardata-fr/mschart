# Apply ggplot2 color scale

The default hue color scale from ggplot2.

## Usage

``` r
chart_fill_ggplot2(x, stroke = TRUE)
```

## Arguments

- x:

  a mschart object

- stroke:

  a boolean. Apply the color scale to stroke? Defaults to `TRUE`.

## Value

a mschart object

## chart_fill_ggplot2()

![](figures/fig_theme_ggplot2.png)

## Examples

``` r
p <- ms_scatterchart(
  data = iris, x = "Sepal.Length",
  y = "Sepal.Width", group = "Species"
)

p <- theme_ggplot2(p)
p <- chart_fill_ggplot2(p)
```
