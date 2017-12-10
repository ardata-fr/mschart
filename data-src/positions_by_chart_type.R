library(xml2)
library(magrittr)
library(purrr)
library(dplyr)
library(officer)

label_doc <- unpack_folder("inst/ressources/model_labels_pos.pptx", tempdir())
all_xml <- list.files( file.path(label_doc, "ppt/charts"), pattern = "\\.xml$", full.names = TRUE )
all_doc <- map(all_xml, read_xml)
pos <- map( all_doc, xml_find_first, "//c:dLblPos") %>% map_chr(xml_attr, "val")
grouping <- map( all_doc, xml_find_first, "//c:grouping") %>% map_chr(xml_attr, "val")
type <- map( all_doc, xml_find_first, "//c:chart/c:plotArea") %>%
  map(xml_child, 2) %>%
  map_chr(xml_name)

tibble::tibble(type= type, grouping = grouping, pos=pos) %>%
  arrange(type, grouping, pos) %>% distinct()
