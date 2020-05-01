# Visualizing the information contained in the project to present a intuitive understanding of the 
# underlying data. 


# ------------------------------ SETTING UP EVERYTHING --------------------------------- 

# Clearing previous session
rm(list = ls())

# Setting working directory
setwd("/Users/jordanimahori/Documents/Violence in Mexico")

# Loading packages
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

# General changes that will be merged into data prep
mdta$num_spikes30 <- as.factor(mdta$num_spikes30)


# -------------------------- Creating Exploratory Plots ----------------------------

# Showing general trend of rising education. 
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


