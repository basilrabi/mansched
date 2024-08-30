library(usethis)

SSS <- read.csv("data-raw/SSS.csv", colClasses = "numeric")
usethis::use_data(SSS, overwrite = TRUE)
