

# This code is replicating and improving upon code originally written in STATA for ECO439 - 
# Empirical Methods in Microeconomics at the University of Toronto. Developed by Jordan Imahori. 
# See README or original paper for additional details and context. Any errors are my own.



# To Do
# 1. Fix the regressions.
# 2. Figure out summary statistics
# 3. Plot graphs



# ------------------------------ SETTING UP EVERYTHING --------------------------------- 

# Clearing previous session
rm(list = ls())

# Setting working directory
setwd("/Users/jordanimahori/Documents/Violence in Mexico")

# Loading packages
library("lfe")
library("tidyverse")

# Loading master dataset. See Data_Prep_VM.R for data cleaning process. 
load("sample_dataset.RData")

# Renaming mdta as MDTA for compatibility
mdta <- smdta
rm(smdta)

# ---------------------------- SETTING UP VARIABLES -------------------------------------- 

# Generating wealth quintiles
mdta$wealthq <- mdta$incearn
mdta$wealthq <- as.integer(cut(mdta$wealthq, breaks = quantile(mdta$incearn, probs = 0:5/5, na.rm = TRUE), include.lowest = TRUE))

# Creating dataframe restricted only to those without probably migration. 
mdta_mig <- filter(mdta, migration != 1)

# Generating vectors of control variables
controls <- c("urban", "sex", "afrdes", "indig", "sizemx")
family <- c("lit_mom", "lit_pop", "yrschool_mom", "yrschool_pop", "incearn_mom", "incearn_pop")

# Dropping entries from municipalities with low populations to minimize driver of murder rates
mdta <- filter(mdta, sizemx != "Less than 2,500 inhabitants")

# ------------------------ GENERATING SUMMARY STATISTICS ---------------------------




# -------------------------- Creating Exploratory Plots ----------------------------

# General changes that will be merged into data prep
mdta$num_spikes30 <- as.factor(mdta$num_spikes30)


# Showing general trend of rising education
ggplot (data = mdta) + 
  geom_smooth(mapping = aes(x = age, y = yrschool))

# Showing differential trend in educational outcomes by income.
ggplot (data = mdta) + 
  geom_smooth(mapping = aes(x = age, y = yrschool)) + 
  facet_wrap(~num_spikes30)

# Showing differential trend in educational outcomes by level of violence experienced.
ggplot (data = mdta) + 
  geom_smooth(mapping = aes(x = age, y = yrschool)) +
  geom_smooth(mapping = aes(x = ))

# Showing that murder rates are associated with educational outcomes regardless of when a person is enrolled
mdta_2004 <- filter(mdta, enroll_2004 == 1)
mdta_2010 <- filter(mdta, enroll_2010 == 1)

ggplot (data = mdta) + 
  geom_smooth(mapping = aes(x = murder_rate_2004, y = yrschool))
  facet_wrap(~wealthq)
  
ggplot (data = mdta) + 
  geom_smooth(mapping = aes(x = murder_rate_2008, y = yrschool))
  facet_wrap(~wealthq)

ggplot (data = mdta_2010) + 
  geom_smooth(mapping = aes(x = murder_rate_2010, y = yrschool)) +
  facet_wrap(~wealthq)



# ----------------------- CONDUCTING PRELIMINARY ANALYSIS --------------------------

# Running Linear Regression with Municipality and Year Fixed Effects. 

# felm(yrschool ~ controls murder_rate_2004:age + murder_rate_2005:age + murder_rate_2006:age + murder_rate_2007:age + murder_rate_2008:age + murder_rate_2009:age + murder_rate_2010:age + murder_rate_2011:age + murder_rate_2012:age | age + municipality, data = mdta_mig)

# felm(yrschool ~ murder_rate_2007 + murder_rate_2008 + murder_rate_2009 | age + municipality, data = sample_mdta)


