# Call packages -----------------------------------------------------------

library(tidyverse)

# Import data -------------------------------------------------------------

VABBdata <- read_csv("./Output/FLANDERS_2020-08-03.csv", col_types = cols(.default = "c"))
CRISTINdata <- read_csv("./Output/NORWAY_2020-08-03.csv", col_types = cols(.default = "c"))

# Table 1. Overview of datasets -------------------------------------------

# Generate datasets

A_CRISTIN <- CRISTINdata
A_VABB <- VABBdata

B_CRISTIN <- CRISTINdata %>% filter(!is.na(WC))
B_VABB <- VABBdata %>% filter(!is.na(WC))

C_CRISTIN <- CRISTINdata %>% filter(!is.na(SM_OECD))
C_VABB <- VABBdata %>% filter(!is.na(SM_OECD))

D_CRISTIN <- CRISTINdata %>% filter(!is.na(erih.oecd1))
D_VABB <- VABBdata %>% filter(!is.na(erih.oecd1))

E_CRISTIN <- CRISTINdata %>% filter(!is.na(VABB.FOS1))
E_VABB <- VABBdata %>% filter(!is.na(NSD.OECD))

# 