library(officer)
library(processx)
library(callr)
library(magrittr)
library(purrr)
library(stringr)

z <- list.files(path = "R", full.names = TRUE, pattern = "\\.R$") %>%
  map(.f = readLines, encoding = "UTF-8") %>%
  map(.f = function(x){
    x[grepl("figure{", x, fixed = TRUE)]
  }) %>% Filter(f = length) %>%
  map(.f = function(z) str_extract_all(z, "fig_(.*)\\.png")) %>%
  unlist() %>% unique() %>% sort()
z




extract_example <- function(file = "man/autofit.Rd", dir = tempdir(), webshot = "webshot2",
                            base_width = 400) {
  example_script <- tempfile(fileext = ".R")
  root_str <- tools::file_path_sans_ext(basename(file))
  run(command = "R", args = c("CMD", "Rdconv", "--type=example",
                              file, "-o",
                              example_script))

  outfiles <- r(function(file, root_str, dir, webshot) {
    require("mschart")
    require("officer")
    source(file)
    obj_list <- ls(envir = .GlobalEnv)
    out <- character()
    for (i in seq_along(obj_list)) {
      obj <- get(obj_list[i])
      if (!inherits(obj, "ms_chart")) {
        next
      }
      filename <- file.path(dir, paste0("fig_", root_str, "_", i, ".png"))
      doc <- read_pptx()
      doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
      doc <- ph_with(doc, obj, location = ph_location_fullsize())
      file_out <- print(doc, target = tempfile(fileext = ".pptx"))
      doconv::to_miniature(filename = file_out, fileout = filename)
      out <- c(out, filename)
    }
    out
  }, args = list(file = example_script, root_str = root_str, dir = dir, webshot = webshot))
  widths <- rep(500, length(outfiles))
  rdtags <- sprintf("#' \\if{html}{\\figure{%s}{options: width=\"%.0f\"}}", basename(outfiles), widths)
  rdtags <- paste0(rdtags, collapse = "\n#'\n")
  rdtags <- paste("#' @section Illustrations:", "#'", rdtags, sep = "\n")
  message(rdtags)
  outfiles
}


unlink("figures2", recursive = TRUE, force = TRUE)
dir.create("figures2")

extract_example(file = "man/chart_ax_x.Rd", dir = "figures2", webshot = "webshot", base_width = 500)
extract_example(file = "man/chart_ax_y.Rd", dir = "figures2", webshot = "webshot", base_width = 500)
extract_example(file = "man/chart_settings.Rd", dir = "figures2", webshot = "webshot", base_width = 500)
extract_example(file = "man/ms_linechart.Rd", dir = "figures2", webshot = "webshot", base_width = 500)
extract_example(file = "man/ms_barchart.Rd", dir = "figures2", webshot = "webshot", base_width = 500)
extract_example(file = "man/ms_scatterchart.Rd", dir = "figures2", webshot = "webshot", base_width = 500)

fs::dir_info("figures2")
minimage::compress_images(input = "figures2", output = "man/figures", overwrite = TRUE)
unlink("figures2", recursive = TRUE, force = TRUE)
