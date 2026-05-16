# Print method for ms_chart

An `ms_chart` object cannot be rendered in R. The default printing
method will only display simple information about the object. If
argument `preview` is set to TRUE, a `pptx` file will be produced and
opened with function `browseURL`.

## Usage

``` r
# S3 method for class 'ms_chart'
print(x, preview = FALSE, ...)
```

## Arguments

- x:

  an `ms_chart` object.

- preview:

  preview the chart in a PowerPoint document

- ...:

  unused

## Value

No return value, called for side effects.
