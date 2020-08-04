# Call packages -----------------------------------------------------------

library(tidyverse)

# Import data -------------------------------------------------------------

VABBdata <- read_csv("./Output/FLANDERS_2020-08-03.csv", col_types = cols(.default = "c"))
CRISTINdata <- read_csv("./Output/NORWAY_2020-08-04.csv", col_types = cols(.default = "c"))

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


# Appendix 1. Datasets A --------------------------------------------------

cog_vars_SSH.FOS <- c(paste0("FOS_5_", 1:9),
                      paste0("FOS_6_", 1:5),
                      paste0("FOS_6_1_", 1:2),
                      paste0("FOS_6_2_", 1:2),
                      paste0("FOS_6_3_", 1:2))

# Flanders

(SSH.total <- sum(as.double(A_VABB$Fract_count)))

A_VABB_SSH <- A_VABB %>% 
  select(Loi, VABB.FOS1:VABB.FOS5, Fract_count) %>% 
  gather(VABB.FOS_nr, VABB.FOS, VABB.FOS1:VABB.FOS5) %>%
  filter(!is.na(VABB.FOS)) %>% 
  group_by(VABB.FOS) %>% 
  summarise(sum = sum(as.double(Fract_count))) %>% 
  mutate(share = sum / SSH.total * 100) %>% 
  filter(VABB.FOS %in% cog_vars_SSH.FOS)


(SS.total <- sum(A_VABB_SSH$sum[1:9]))
(H.total <- sum(A_VABB_SSH$sum[10:14]))

SSH.total / SSH.total * 100
SS.total / SSH.total * 100
H.total / SSH.total * 100

# Norway

(SSH.total <- sum(as.double(A_CRISTIN$Fract_count)))

A_CRISTIN_SSH <- A_CRISTIN %>% 
  select(VARBEIDLOPENR, NSD.OECD, Fract_count) %>% #Fract_count wrong
  group_by(NSD.OECD) %>% 
  summarise(sum = sum(as.double(Fract_count))) %>% 
  mutate(share = sum / SSH.total * 100) %>% 
  filter(NSD.OECD %in% cog_vars_SSH.FOS)

(SS.total <- sum(A_CRISTIN_SSH$sum[1:9]))
(H.total <- sum(A_CRISTIN_SSH$sum[10:14]))

SSH.total / SSH.total * 100
SS.total / SSH.total * 100
H.total / SSH.total * 100
