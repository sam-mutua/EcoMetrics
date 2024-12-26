## code to prepare `DATASET` dataset goes here

eduperform <- read.csv("sp.csv")
usethis::use_data(eduperform, overwrite = TRUE)


keconomy <- read.csv("keconomy.csv")
usethis::use_data(keconomy, overwrite = TRUE)
