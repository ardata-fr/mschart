library(mschart)
remove(list = ls())

# funs -----
#' @title topic names from a package
#' @description list documented topic names in a package
#' @param package_name package name
#' @examples
#' package_man_names("mschart")
#' @noRd
package_man_names <- function(package_name) {
  help_dir <- system.file("help", package = package_name)
  db_file <- file.path(help_dir, package_name)
  z <- tools:::fetchRdDB(db_file)
  names(z)
}


#' @noRd
#' @title Get images from mschart examples
#' @description Run examples from a topic in help pages
#' and create corresponding images in a temporary directory.
#' @param name help topic name, its examples will be run
#' @param pkg the package name where the help topic is located
#' @param pattern name pattern too look for, default to objects whose
#' name start with *chart*.
#' @examples
#' library(flextable)
#' process_manual_mschart(name = "ms_scatterchart", pkg = "mschart")
#' @importFrom utils example
#' @importFrom flextable save_as_image
process_manual_mschart <- function(name, pkg, pattern = "^chart[_0-9]*?$", dir = tempfile()){
  obj_start <- ls(envir = .GlobalEnv)

  if(!dir.exists(dir)){
    dir.create(path = dir, showWarnings = FALSE, recursive = TRUE)
  }

  zz=utils::example(topic = name, package = pkg, character.only = TRUE,
                    give.lines = FALSE, echo = FALSE, local = FALSE)

  obj_list <- ls(pattern = pattern, envir = .GlobalEnv)
  out <- character(length(obj_list))
  names(out) <- obj_list
  for (i in seq_along(obj_list)) {
    obj <- get(obj_list[i])
    if (!inherits(obj, "ms_chart")) {
      next
    }

    tmp_pptx <- tempfile(fileext = ".pptx")
    doc <- read_pptx()
    doc <- add_slide(doc)
    doc <- ph_with(doc, obj, location = ph_location_fullsize())
    print(doc, target = tmp_pptx)
    filename <- file.path(dir, paste0("fig_", name, "_", i, ".png"))
    doconv::to_miniature(filename = tmp_pptx, fileout = filename)
    out[obj_list[i]] <- filename
  }
  rm(list = setdiff(ls(envir = .GlobalEnv), obj_start), envir = .GlobalEnv)
  out
}

# get images from examples ----
man_names <- package_man_names(package_name = "mschart")
out <- list()
dir <- file.path(tempfile(), "figures")
for (man_index in seq_along(man_names)) {
  man_name <- man_names[man_index]
  message(man_name, " (", man_index, " on ", length(man_names), ")")
  out[[man_name]] <- process_manual_mschart(name = man_name, pkg = "mschart", dir = dir)
}


rsvg::rsvg_png("inst/mediasrc/logo-src.svg", file.path(dir, "logo.png"))
file.copy("inst/mediasrc/example.png", file.path(dir, "README-example.png"), overwrite = TRUE)

# compress all images ----
minimage::compress_images(input = dir, "man/figures", overwrite = TRUE)

