
# The original data that I obtained was either as a .sav file or a CSV. I'm converting it to RData
# here to speed up the process of loading it into memory.

# Loading Packages
library("foreign")
library("readr")
library("tibble")

# Loading 2015 Intercensal Survey from INEGI. 
# Preventing geo2_mx2015 from converting to factor to allow merge. 
ipums <- read.spss("datasets/ipumsi_00004.sav", to.data.frame = TRUE, max.value.labels = 2000)
ipums <- as_tibble(ipums)
save(ipums, file = "datasets/ipums.RData")