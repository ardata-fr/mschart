library(AER)
library(mschart)
library(data.table)

data("USProdIndex", package = "AER")
as.data.frame(USProdIndex)
class(USProdIndex)
dat <- cbind(as.data.frame(USProdIndex),
             date = as.Date(time(USProdIndex)))
setDT(dat)
us_indus_prod <- melt(dat, id.vars = "date", measure.vars = c("unadjusted", "adjusted"),
            variable.name = "type", value.name = "value")
setDF(us_indus_prod)
usethis::use_data(us_indus_prod, overwrite = TRUE)
