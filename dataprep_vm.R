
# This code is replicating and improving upon code originally written in STATA for ECO439 - 
# Empirical Methods in Microeconomics at the University of Toronto. Developed by Jordan Imahori. 
# See README or original paper for additional details and context. Any errors are my own.


# Clearing previous session.
rm(list = ls())

# Loading packages.
library("tidyverse")

# Loading yearly population estimates by municipality from CONAPO.
municipio_population1990_2030 <- read_csv("datasets/municipio_population1990_2030.csv")

# Loading dataset on all intentional deaths in Mexico from INEGI.
load("datasets/injury.intent.RData")
injury_intent <- as_tibble(injury.intent)
rm(injury.intent)

# Loading results from 2015 Intercensal Survey. 
load("datasets/ipums.RData")


# ----------------------- CALCULATING MURDER RATES BY MUNICIPALITY ---------------------------

# Restricting to years of interest, dropping sex-specific populations, and renaming variables for consistency.
mxpop <- municipio_population1990_2030 %>%
    filter(Year > 2003 & Year < 2015, Sex == "Total") %>%
    select(c(Code, Year, Population)) %>%
    rename(geo2_mx2015 = Code, year = Year, population = Population)

# Restricting sample to homicides in years of interest (2004 - 2014), renaming variables for 
# consistency and counting the number of homicides occuring in each municipality each year. 
homicides <- injury_intent %>%
    filter(year_occur > 2003 & year_occur < 2015) %>%
    filter(intent == "Homicide") %>%
    rename(year = year_occur) %>%
    mutate(geo2_mx2015 = state_occur_death*1000 + mun_occur_death) %>%
    group_by(geo2_mx2015, year) %>%
    summarise(annual_murders = n())

# Merging population estimates with murders by municipality.
# Replacing missing values for murders with zeros. Calculating homicide rates (as homicides per 100,000).

homicide_rate <- mxpop %>%
    left_join(homicides, by = c("geo2_mx2015", "year")) %>%
    mutate(
        annual_murders = replace_na(annual_murders, 0), 
        murder_rate = (annual_murders/population)*100000
    )
    
homicide_rate <- homicide_rate %>%
    mutate(mrate = str_c("mrate_", homicide_rate$year)) %>%
    pivot_wider(id_cols = c(geo2_mx2015), names_from = mrate, values_from = murder_rate)


# ----------------- CALCULATING FIREARM MURDER RATE BY MUNICIPALITY -----------------------


# Restricting sample to homicides using firearms in years of interest (2004 - 2014) since these
# are more likely to result from drug-related violence. Renaming renaming variables for 
# consistency and counting the number of homicides occuring in each municipality each year.
firearm_homicides <- injury_intent %>%
    filter(year_occur > 2003 & year_occur < 2015) %>%
    filter(intent == "Homicide", mechanism == "Firearm") %>%
    rename(year = year_occur) %>%
    mutate(geo2_mx2015 = state_occur_death*1000 + mun_occur_death) %>%
    group_by(geo2_mx2015, year) %>%
    summarise(firearm_murders = n())

# Merging firearm murders with data on population and all murders by municipality.
# Replacing missing values for murders with zeros. Calculating homicide rates (as homicides per 100,000).
firearm_homicide_rate <- mxpop %>%
    left_join(firearm_homicides, by = c("geo2_mx2015", "year")) %>%
    mutate(
        firearm_murders = replace_na(firearm_murders, 0), 
        firearm_murder_rate = (firearm_murders/population)*100000
    )

# Bringing murder rates in a given municipality into a single row. 
firearm_homicide_rate <- firearm_homicide_rate %>%
    mutate(fmrate = str_c("fmrate_", firearm_homicide_rate$year)) %>%
    pivot_wider(id_cols = geo2_mx2015, names_from = fmrate, values_from = firearm_murder_rate)

# Removing intermediate steps.
rm(injury_intent, municipio_population1990_2030, mxpop, homicides, firearm_homicides)


# -------------------- CLEANING UP 2015 INTERCENSAL SURVEY DATA -----------------------------

# Changing all variable names in IPUMS to lower case and simplifying variable names.
colnames(ipums) <- tolower(colnames(ipums))

# Simplifying names of variables and dropping uneccessary variables. 
ipums <- ipums %>%
    rename(afrdes_mom = mx2015a_afrdes_mom, afrdes_pop = mx2015a_afrdes_pop, 
           afrdes = mx2015a_afrdes, indig_mom = mx2015a_indig_mom, indig_pop = mx2015a_indig_pop, 
           mig5 = mx2015a_migmun5) %>%
    select(sex, age, urban, sizemx, geo1_mx2015, geo2_mx2015, bplmx, mig5, 
           lit, edattain, school, yrschool, incearn, lit_mom, lit_pop, yrschool_mom, yrschool_pop,
           incearn_mom, incearn_pop, afrdes, afrdes_mom, afrdes_pop, indig, indig_mom, indig_pop) %>%
    mutate(municipality = as_factor(geo2_mx2015))
    
# Converting age to numeric value.
ipums$age <- as.character(ipums$age)
    ipums$age[ipums$age == "1 year"] <- "1"
    ipums$age[ipums$age == "2 years"] <- "2"
    ipums$age[ipums$age == "Less than 1 year"] <- "0"
    ipums$age[ipums$age == "100+"] <- "100"
    ipums$age[ipums$age == "Not reported/missing"] <- NA
ipums$age <- as.numeric(ipums$age)

# Converting yrschool to numeric value.
ipums$yrschool <- as.character(ipums$yrschool)
    ipums$yrschool[ipums$yrschool == "NIU (not in universe)"] <- NA
    ipums$yrschool[ipums$yrschool == "Unknown/missing"] <- NA
    ipums$yrschool[ipums$yrschool == "Some primary"] <- NA
    ipums$yrschool[ipums$yrschool == "Some secondary"] <- NA
    ipums$yrschool[ipums$yrschool == "Some tertiary"] <- NA
    ipums$yrschool[ipums$yrschool == "Some technical after primary"] <- NA
    ipums$yrschool[ipums$yrschool == "None or pre-school"] <- 0
ipums$yrschool <- parse_number(as.character(ipums$yrschool, na = NA, trim_ws = TRUE))

# Converting yrschool_mom to numeric value.
ipums$yrschool_mom <- as.character(ipums$yrschool_mom)
    ipums$yrschool_mom[ipums$yrschool_mom == "NIU (not in universe)"] <- NA
    ipums$yrschool_mom[ipums$yrschool_mom == "Unknown/missing"] <- NA
    ipums$yrschool_mom[ipums$yrschool_mom == "Some primary"] <- NA
    ipums$yrschool_mom[ipums$yrschool_mom == "Some secondary"] <- NA
    ipums$yrschool_mom[ipums$yrschool_mom == "Some tertiary"] <- NA
    ipums$yrschool_mom[ipums$yrschool_mom == "Some technical after primary"] <- NA
    ipums$yrschool_mom[ipums$yrschool_mom == "None or pre-school"] <- 0
ipums$yrschool_mom <- parse_number(as.character(ipums$yrschool_mom, na = NA, trim_ws = TRUE))

# Converting yrschool_pop to numeric value
ipums$yrschool_pop <- as.character(ipums$yrschool_pop)
    ipums$yrschool_pop[ipums$yrschool_pop == "NIU (not in universe)"] <- NA
    ipums$yrschool_pop[ipums$yrschool_pop == "Unknown/missing"] <- NA
    ipums$yrschool_pop[ipums$yrschool_pop == "Some primary"] <- NA
    ipums$yrschool_pop[ipums$yrschool_pop == "Some secondary"] <- NA
    ipums$yrschool_pop[ipums$yrschool_pop == "Some tertiary"] <- NA
    ipums$yrschool_pop[ipums$yrschool_pop == "Some technical after primary"] <- NA
    ipums$yrschool_pop[ipums$yrschool_pop == "None or pre-school"] <- 0
ipums$yrschool_pop <- parse_number(as.character(ipums$yrschool_pop, na = NA, trim_ws = TRUE))

# Recoding missing values as NA. IPUMS records missing values as large numbers greater than 2e+07.
ipums$incearn[ipums$incearn > 2e+07] <- NA
ipums$incearn_mom[ipums$incearn_mom > 2e+07] <- NA
ipums$incearn_pop[ipums$incearn_pop > 2e+07] <- NA
ipums$mig5[ipums$mig5 > 40000] <- NA
ipums$lit[ipums$lit == "NIU (not in universe)"] <- NA
ipums$edattain[ipums$edattain == "NIU (not in universe)"] <- NA
ipums$school[ipums$school == "NIU (not in universe)"] <- NA
ipums$afrdes[ipums$afrdes == "Unknown"] <- NA
ipums$indig[ipums$indig == "Unknown"] <- NA

# Dropping observations that are missing values for essential variables. 
ipums <- filter(ipums, is.na(ipums$age) == FALSE & is.na(ipums$municipality) == FALSE & is.na(ipums$yrschool) == FALSE)


# ---------------------------- CREATING MASTER DATASET ------------------------------------

# Formatting data for merge.
ipums$geo2_mx2015 <- as.character(ipums$geo2_mx2015)
homicide_rate$geo2_mx2015 <- as.character(homicide_rate$geo2_mx2015)
firearm_homicide_rate$geo2_mx2015 <- as.character(firearm_homicide_rate$geo2_mx2015)


# Merging murder rate variables into 2015 Intercensal Survey.
mdta <- ipums %>%
    left_join(firearm_homicide_rate, by = "geo2_mx2015") %>%
    left_join(homicide_rate, by = "geo2_mx2015")

# Removing old datasets from memory.
rm(ipums, homicide_rate, firearm_homicide_rate)


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



# --------------------------- SAVING THE FILE FOR FUTURE USE ----------------------------

# Creating a random sample (n = 250,000) for faster processing
smdta <- sample_n(mdta, 250000)

# Saving the files.
save(mdta, file = "educ_mexico_data.RData")
save(smdta, file = "educ_mexico_sample_data.RData")


# -------------------------------------- END -------------------------------------------


