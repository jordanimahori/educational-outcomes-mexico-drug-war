
# This code is replicating and improving upon code originally written in STATA for ECO439 - 
# Empirical Methods in Microeconomics at the University of Toronto. Developed by Jordan Imahori. 
# See README or original paper for additional details and context. Any errors are my own.



# Setting up everything... 

# Clearing previous session.
rm(list = ls())

# Setting working directory.
setwd("/Users/jordan/Documents/Violence in Mexico")

# Loading packages.
library("foreign")
library("tidyverse")

# Loading yearly population estimates by municipality from CONAPO.
municipio_population1990_2030 <- read_csv("municipio_population1990_2030.csv")

# Loading dataset on all intentional deaths in Mexico from INEGI.
load("injury.intent.RData")

# Loading 2015 Intercensal Survey from INEGI. Preventing geo2_mx2015 from converting to factor to allow merge. 
ipums <- read.spss("ipumsi_00004.sav", to.data.frame = TRUE, max.value.labels = 2000)

# Converting datasets from data.frame class to tbd_df
injury.intent <- as_tibble(injury.intent)
municipio_population1990_2030 <- as_tibble(municipio_population1990_2030)
ipums <- as_tibble(ipums)


# ----------------------- CALCULATING MURDER RATES BY MUNICIPALITY ---------------------------


# Restricting sample to only years of interst (2004 - 2014). Dropping sex-specific populations.
mxpop <- filter(municipio_population1990_2030, Year > 2003 & Year < 2015, Sex == "Total")
mxpop <- select(mxpop, c(Code, Year, Population))

# Renaming municipality geo-code for consistency with other datasets.
mxpop <- rename(mxpop, geo2_mx2015 = Code, year = Year, population = Population)

# Restricting sample to only years of interest (2004 - 2014).
murders <- filter(injury.intent, year_occur > 2003 & year_occur < 2015)

# Creating geo-code for municipalities that is consistent across datasets.
murders$geo2_mx2015 <- murders$state_occur_death*1000 + murders$mun_occur_death

# Renaming variables to keep then consistent across datasets.
murders$year <- murders$year_occur

# Restricting the sample to only deaths likely to be associated with drug violence (e.g. exclu. suicide).
murders <- filter(murders, intent == "Homicide")

# Dropping unnecessary variables.
murders <- select(murders, year, geo2_mx2015)

# Calculating the number of murders by year. 
annual_murders <- count(murders, year, geo2_mx2015)
annual_murders <- rename(annual_murders, murders = n)

# Merging population estimates with murders by municipality.
md_set <- left_join(mxpop, annual_murders, by = c("geo2_mx2015", "year"))

# Replacing missing values for murders with zeros.
md_set$murders <- replace_na(md_set$murders, 0)

# Calculating murder rates.
md_set$murder_rate <- (md_set$murders)/(md_set$population)*100000



# ----------------- CALCULATING FIREARM MURDER RATE BY MUNICIPALITY -----------------------


# Re-loading dataset on all intentional deaths in Mexico from INEGI.
fmurders <- injury.intent

# Creating geo-code for municipalities that is consistent across datasets.
fmurders$geo2_mx2015 <- fmurders$state_occur_death*1000 + fmurders$mun_occur_death

# Restricting sample to only years of interest (2004 - 2014).
fmurders <- filter(fmurders, year_occur > 2003 & year_occur < 2015)

# Restricting the sample to only deaths likely to be associated with drug violence (e.g. exclu. suicide).
fmurders <- filter(fmurders, intent == "Homicide", mechanism == "Firearm")

# Renaming variables to keep then consistent across datasets and dropping unnecessary variables.
fmurders$year <- fmurders$year_occur
fmurders <- select(fmurders, year, geo2_mx2015)

# Calculating the number of murders by year. 
annual_fmurders <- count(fmurders, year, geo2_mx2015)
annual_fmurders <- rename(annual_fmurders, firearm_murders = n)

# Merging firearm murders with data on population and all murders by municipality.
md_set <- left_join(md_set, annual_fmurders, by = c("geo2_mx2015", "year"))

# Replacing missing values for firearm murders with zeros.
md_set$firearm_murders <- replace_na(md_set$firearm_murders, 0)

# Calculating murder rates.
md_set$firearm_murder_rate <- (md_set$firearm_murders)/(md_set$population)*100000

# Removing data frames that are no longer needed.
rm(fmurders, injury.intent, municipio_population1990_2030, murders, mxpop, annual_murders, annual_fmurders)




# ---------------------------- CREATING MASTER DATASET ------------------------------------


# Creating variables for the murder rate in each year for all municipalities with data.
md_set$murder_rate_2004 <- if_else(md_set$year == 2004, md_set$murder_rate, 0)
md_set$murder_rate_2005 <- if_else(md_set$year == 2005, md_set$murder_rate, 0)
md_set$murder_rate_2006 <- if_else(md_set$year == 2006, md_set$murder_rate, 0)
md_set$murder_rate_2007 <- if_else(md_set$year == 2007, md_set$murder_rate, 0)
md_set$murder_rate_2008 <- if_else(md_set$year == 2008, md_set$murder_rate, 0)
md_set$murder_rate_2009 <- if_else(md_set$year == 2009, md_set$murder_rate, 0)
md_set$murder_rate_2010 <- if_else(md_set$year == 2010, md_set$murder_rate, 0)
md_set$murder_rate_2011 <- if_else(md_set$year == 2011, md_set$murder_rate, 0)
md_set$murder_rate_2012 <- if_else(md_set$year == 2012, md_set$murder_rate, 0)
md_set$murder_rate_2013 <- if_else(md_set$year == 2013, md_set$murder_rate, 0)
md_set$murder_rate_2014 <- if_else(md_set$year == 2014, md_set$murder_rate, 0)

# Creating two-colum data frame with unique entries for municipality and the murder rates in 2004
murder_merge <- enframe(
  tapply(md_set$murder_rate_2004, md_set$geo2_mx2015, max), 
  name = "geo2_mx2015", value = "murder_rate_2004")

# Adding additional columns for the murder rate in each subsequent year up to 2014
murder_merge$murder_rate_2005 <- tapply(md_set$murder_rate_2005, md_set$geo2_mx2015, max)
murder_merge$murder_rate_2006 <- tapply(md_set$murder_rate_2006, md_set$geo2_mx2015, max)
murder_merge$murder_rate_2007 <- tapply(md_set$murder_rate_2007, md_set$geo2_mx2015, max)
murder_merge$murder_rate_2008 <- tapply(md_set$murder_rate_2008, md_set$geo2_mx2015, max)
murder_merge$murder_rate_2009 <- tapply(md_set$murder_rate_2009, md_set$geo2_mx2015, max)
murder_merge$murder_rate_2010 <- tapply(md_set$murder_rate_2010, md_set$geo2_mx2015, max)
murder_merge$murder_rate_2011 <- tapply(md_set$murder_rate_2011, md_set$geo2_mx2015, max)
murder_merge$murder_rate_2012 <- tapply(md_set$murder_rate_2012, md_set$geo2_mx2015, max)
murder_merge$murder_rate_2013 <- tapply(md_set$murder_rate_2013, md_set$geo2_mx2015, max)
murder_merge$murder_rate_2014 <- tapply(md_set$murder_rate_2014, md_set$geo2_mx2015, max)

# Changing all variable names in IPUMS to lower case and simplifying variable names.
colnames(ipums) <- tolower(colnames(ipums))

# Simplifying names of variables. Creating factor variable for municipality.
ipums <- rename(ipums, afrdes_mom = mx2015a_afrdes_mom, afrdes_pop = mx2015a_afrdes_pop, afrdes = mx2015a_afrdes,
                indig_mom = mx2015a_indig_mom, indig_pop = mx2015a_indig_pop, mig5 = mx2015a_migmun5)

ipums$municipality <- as.factor(ipums$geo2_mx2015)

# Dropping unnecessary variables from 2015 Intercensal Survey.
mdta <- select(ipums, sex, age, urban, sizemx, geo1_mx2015, geo2_mx2015, municipality, bplmx, mig5, lit, edattain, school, yrschool, 
              incearn, lit_mom, lit_pop, yrschool_mom, yrschool_pop, incearn_mom, incearn_pop, afrdes, afrdes_mom, afrdes_pop, indig, 
              indig_mom, indig_pop)

# Converting age to numeric value. 
mdta$age <- as.character(mdta$age)
mdta$age[mdta$age == "1 year"] <- 1
mdta$age[mdta$age == "2 years"] <- 2
mdta$age[mdta$age == "Less than 1 year"] <- 0
mdta$age[mdta$age == "100+"] <- 100
mdta$age[mdta$age == "Not reported/missing"] <- NA
mdta$age <- as.numeric(mdta$age)

# Converting yrschool to numeric value
mdta$yrschool <- as.character(mdta$yrschool)
mdta$yrschool[mdta$yrschool == "NIU (not in universe)"] <- NA
mdta$yrschool[mdta$yrschool == "Unknown/missing"] <- NA
mdta$yrschool[mdta$yrschool == "Some primary"] <- NA
mdta$yrschool[mdta$yrschool == "Some secondary"] <- NA
mdta$yrschool[mdta$yrschool == "Some tertiary"] <- NA
mdta$yrschool[mdta$yrschool == "Some technical after primary"] <- NA
mdta$yrschool[mdta$yrschool == "None or pre-school"] <- 0
mdta$yrschool <- parse_number(as.character(mdta$yrschool, na = NA, trim_ws = TRUE))

# Converting yrschool_mom to numeric value
mdta$yrschool_mom <- as.character(mdta$yrschool_mom)
mdta$yrschool_mom[mdta$yrschool_mom == "NIU (not in universe)"] <- NA
mdta$yrschool_mom[mdta$yrschool_mom == "Unknown/missing"] <- NA
mdta$yrschool_mom[mdta$yrschool_mom == "Some primary"] <- NA
mdta$yrschool_mom[mdta$yrschool_mom == "Some secondary"] <- NA
mdta$yrschool_mom[mdta$yrschool_mom == "Some tertiary"] <- NA
mdta$yrschool_mom[mdta$yrschool_mom == "Some technical after primary"] <- NA
mdta$yrschool_mom[mdta$yrschool_mom == "None or pre-school"] <- 0
mdta$yrschool_mom <- parse_number(as.character(mdta$yrschool_mom, na = NA, trim_ws = TRUE))

# Converting yrschool_pop to numeric value
mdta$yrschool_pop <- as.character(mdta$yrschool_pop)
mdta$yrschool_pop[mdta$yrschool_pop == "NIU (not in universe)"] <- NA
mdta$yrschool_pop[mdta$yrschool_pop == "Unknown/missing"] <- NA
mdta$yrschool_pop[mdta$yrschool_pop == "Some primary"] <- NA
mdta$yrschool_pop[mdta$yrschool_pop == "Some secondary"] <- NA
mdta$yrschool_pop[mdta$yrschool_pop == "Some tertiary"] <- NA
mdta$yrschool_pop[mdta$yrschool_pop == "Some technical after primary"] <- NA
mdta$yrschool_pop[mdta$yrschool_pop == "None or pre-school"] <- 0
mdta$yrschool_pop <- parse_number(as.character(mdta$yrschool_pop, na = NA, trim_ws = TRUE))

# Recoding missing values as NA. 
mdta$incearn[mdta$incearn > 2e+07] <- NA
mdta$incearn_mom[mdta$incearn_mom > 2e+07] <- NA
mdta$incearn_pop[mdta$incearn_pop > 2e+07] <- NA
mdta$mig5[mdta$mig5 > 40000] <- NA
mdta$lit[mdta$lit == "NIU (not in universe)"] <- NA
mdta$edattain[mdta$edattain == "NIU (not in universe)"] <- NA
mdta$school[mdta$school == "NIU (not in universe)"] <- NA
mdta$afrdes[mdta$afrdes == "Unknown"] <- NA
mdta$indig[mdta$indig == "Unknown"] <- NA

# Dropping observations that are missing values for essential variables. 
mdta <- filter(mdta, is.na(mdta$age) == FALSE & is.na(mdta$municipality) == FALSE & is.na(mdta$yrschool) == FALSE)

# Formatting data for merge.
mdta$geo2_mx2015 <- as.character(mdta$geo2_mx2015)
murder_merge$geo2_mx2015 <- as.character(murder_merge$geo2_mx2015)

# Merging murder rate variables into 2015 Intercensal Survey.
mdta <- left_join(mdta, murder_merge, by = c("geo2_mx2015"))

# Removing old datasets from memory.
rm(ipums, murder_merge, md_set)



# -----------------GENERATING INDICATOR VARIABLES FOR ANALYSIS -------------------------


# Creating dummy for a shock to the murder rate above 15
mdta$shock_2005 <- if_else(mdta$murder_rate_2005 - mdta$murder_rate_2004 > 15, 1, 0)
mdta$shock_2006 <- if_else(mdta$murder_rate_2006 - mdta$murder_rate_2005 > 15, 1, 0)
mdta$shock_2007 <- if_else(mdta$murder_rate_2007 - mdta$murder_rate_2006 > 15, 1, 0)
mdta$shock_2008 <- if_else(mdta$murder_rate_2008 - mdta$murder_rate_2007 > 15, 1, 0)
mdta$shock_2009 <- if_else(mdta$murder_rate_2009 - mdta$murder_rate_2008 > 15, 1, 0)
mdta$shock_2010 <- if_else(mdta$murder_rate_2010 - mdta$murder_rate_2009 > 15, 1, 0)
mdta$shock_2011 <- if_else(mdta$murder_rate_2011 - mdta$murder_rate_2010 > 15, 1, 0)
mdta$shock_2012 <- if_else(mdta$murder_rate_2012 - mdta$murder_rate_2011 > 15, 1, 0)
mdta$shock_2013 <- if_else(mdta$murder_rate_2013 - mdta$murder_rate_2012 > 15, 1, 0)
mdta$shock_2014 <- if_else(mdta$murder_rate_2014 - mdta$murder_rate_2013 > 15, 1, 0)


# Creating dummy for shock to the murder rate above 30
mdta$lshock_2005 <- if_else(mdta$murder_rate_2005 - mdta$murder_rate_2004 > 30, 1, 0)
mdta$lshock_2006 <- if_else(mdta$murder_rate_2006 - mdta$murder_rate_2005 > 30, 1, 0)
mdta$lshock_2007 <- if_else(mdta$murder_rate_2007 - mdta$murder_rate_2006 > 30, 1, 0)
mdta$lshock_2008 <- if_else(mdta$murder_rate_2008 - mdta$murder_rate_2007 > 30, 1, 0)
mdta$lshock_2009 <- if_else(mdta$murder_rate_2009 - mdta$murder_rate_2008 > 30, 1, 0)
mdta$lshock_2010 <- if_else(mdta$murder_rate_2010 - mdta$murder_rate_2009 > 30, 1, 0)
mdta$lshock_2011 <- if_else(mdta$murder_rate_2011 - mdta$murder_rate_2010 > 30, 1, 0)
mdta$lshock_2012 <- if_else(mdta$murder_rate_2012 - mdta$murder_rate_2011 > 30, 1, 0)
mdta$lshock_2013 <- if_else(mdta$murder_rate_2013 - mdta$murder_rate_2012 > 30, 1, 0)
mdta$lshock_2014 <- if_else(mdta$murder_rate_2014 - mdta$murder_rate_2013 > 30, 1, 0)


# Generating dummy for whether they were likely to have been enrolled in high school
mdta$enroll_2004 <- if_else(25 < mdta$age & mdta$age < 30, 1, 0)
mdta$enroll_2005 <- if_else(24 < mdta$age & mdta$age < 29, 1, 0)
mdta$enroll_2006 <- if_else(23 < mdta$age & mdta$age < 28, 1, 0)
mdta$enroll_2007 <- if_else(22 < mdta$age & mdta$age < 27, 1, 0)
mdta$enroll_2008 <- if_else(21 < mdta$age & mdta$age < 26, 1, 0)
mdta$enroll_2009 <- if_else(20 < mdta$age & mdta$age < 25, 1, 0)
mdta$enroll_2010 <- if_else(19 < mdta$age & mdta$age < 24, 1, 0)
mdta$enroll_2011 <- if_else(18 < mdta$age & mdta$age < 23, 1, 0)

# Generating count of murder rate spikes for each level. 
mdta$num_spikes15 <- mdta$shock_2005 +  mdta$shock_2006 + mdta$shock_2007 + mdta$shock_2008 + mdta$shock_2009 + mdta$shock_2010 + mdta$shock_2011 + mdta$shock_2012 + mdta$shock_2013 + mdta$shock_2014
mdta$num_spikes30 <- mdta$lshock_2005 +  mdta$lshock_2006 + mdta$lshock_2007 + mdta$lshock_2008 + mdta$lshock_2009 + mdta$lshock_2010 + mdta$lshock_2011 + mdta$lshock_2012 + mdta$lshock_2013 + mdta$lshock_2014

# Creating a dummy variable for possible migration. 
mdta$migration <- if_else(as.character(mdta$bplmx) != as.character(mdta$geo1_mx2015) | mdta$mig5 != mdta$geo2_mx2015, "Yes", "No")

# Ordering dataset by state and municipality
mdta <- arrange(mdta, as.numeric(mdta$geo2_mx2015))

# --------------------------- SAVING THE FILE FOR FUTURE USE -------00----------------------

# Creating a random sample (n = 250,000) for faster processing
smdta <- sample_n(mdta, 250000)

# Saving the files.
save(mdta, file = "master_dataset.RData")
save(smdta, file = "sample_dataset.RData")


# -------------------------------------- END -------------------------------------------

