library(tidyverse)
library(data.table)

df = data.table::fread("0-data/citations/scopus.csv", data.table = FALSE)
sum(df$`Cited by`, na.rm = TRUE)
max(df$`Cited by`, na.rm = TRUE)
