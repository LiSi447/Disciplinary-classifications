# Call packages -----------------------------------------------------------

library(tidyverse)
library(stringr)

# Import data -------------------------------------------------------------

VABBdata <- read_csv("./Output/FLANDERS_2020-08-03.csv", col_types = cols(.default = "c"))
CRISTINdata <- read_csv("./Output/NORWAY_2020-08-04.csv", col_types = cols(.default = "c"))


# Prep datasets -----------------------------------------------------------

WOS.vars <- c(paste0("WOS_FOSCAT_uncoded_",1:6))

for (var in WOS.vars) {
  for (i in 1:nrow(VABBdata)) {
    (VABBdata[[var]][i] <- str_replace(VABBdata[[var]][i] ,"FOS", "FOS_"))
    (VABBdata[[var]][i] <- str_replace(VABBdata[[var]][i], "PSYCHOLOGY, SOCIAL PSYCHOLOGY", "FOS_5_1"))
  }
}

for (var in WOS.vars) {
  for (i in 1:nrow(CRISTINdata)) {
    (CRISTINdata[[var]][i] <- str_replace(CRISTINdata[[var]][i] ,"FOS", "FOS_"))
    (CRISTINdata[[var]][i] <- str_replace(CRISTINdata[[var]][i], "PSYCHOLOGY, SOCIAL PSYCHOLOGY", "FOS_5_1"))
  }
}

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

A_VABB_SSH <- A_VABB %>% 
  select(Loi, VABB.FOS1:VABB.FOS5, Fract_count) %>% 
  gather(VABB.FOS_nr, VABB.FOS, VABB.FOS1:VABB.FOS5) %>%
  filter(!is.na(VABB.FOS)) %>% 
  group_by(VABB.FOS) %>% 
  summarise(sum = sum(as.double(Fract_count))) %>% 
  filter(VABB.FOS %in% cog_vars_SSH.FOS) 

VABB.FOS <- c("SS.total", "H.total", "SSH.total")

sum <- c(sum(A_VABB_SSH$sum[1:9]),
       sum(A_VABB_SSH$sum[10:14]),
       sum(as.double(A_VABB$Fract_count))
       )

totals.A.VABB <- data.frame(VABB.FOS, sum)

A_VABB_SSH <- A_VABB_SSH %>% 
  rbind(totals.A.VABB) %>% 
  mutate(share = sum / sum(as.double(A_VABB$Fract_count)) * 100)

# Norway

A_CRISTIN_SSH <- A_CRISTIN %>% 
  select(VARBEIDLOPENR, NSD.OECD, Fract_count) %>% #Fract_count wrong
  group_by(NSD.OECD) %>% 
  summarise(sum = sum(as.double(Fract_count))) %>% 
  filter(NSD.OECD %in% cog_vars_SSH.FOS)

NSD.OECD <- c("SS.total", "H.total", "SSH.total")

sum <- c(sum(A_CRISTIN_SSH$sum[1:9]),
         sum(A_CRISTIN_SSH$sum[10:14]),
         sum(as.double(A_CRISTIN$Fract_count))
         )

totals.A.CRISTIN <- data.frame(NSD.OECD, sum)

A_CRISTIN_SSH <- A_CRISTIN_SSH %>% 
  rbind(totals.A.CRISTIN) %>% 
  mutate(share = sum / sum(as.double(A_CRISTIN$Fract_count)) * 100)
  
# Appendix 2. Dataset B WoS -----------------------------------------------

# Flanders

B_VABB_SSH_WOS <- B_VABB %>% 
  select(Loi, Fract_count, WOS_FOSCAT_uncoded_1:WOS_FOSCAT_uncoded_6) %>% 
  gather(WOS.nr, WOS.OECD, WOS_FOSCAT_uncoded_1:WOS_FOSCAT_uncoded_6) %>% 
  filter(!is.na(WOS.OECD)) %>% 
  group_by(WOS.OECD) %>% 
  distinct(Loi, WOS.OECD, .keep_all = TRUE) %>% 
  summarise(sum = sum(as.double(Fract_count))) %>% 
  filter(WOS.OECD %in% cog_vars_SSH.FOS)

sum <- c(sum(B_VABB_SSH_WOS$sum[1:9]),
         sum(B_VABB_SSH_WOS$sum[10:14]),
         sum(as.double(B_VABB$Fract_count))
)

WOS.OECD <- c("SS.total", "H.total", "SSH.total")

totals.B.VABB.WOS <- data.frame(WOS.OECD, sum)


B_VABB_SSH_WOS <- B_VABB_SSH_WOS %>% 
  rbind(totals.B.VABB.WOS) %>% 
  mutate(share = sum / sum(as.double(B_VABB$Fract_count)) * 100)

# Norway

(SSH.total <- sum(as.double(B_CRISTIN$Fract_count)))

B_CRISTIN_SSH_WOS <- B_CRISTIN %>% 
  select(VARBEIDLOPENR, Fract_count, WOS_FOSCAT_uncoded_1:WOS_FOSCAT_uncoded_6) %>% 
  gather(WOS.nr, WOS.OECD, WOS_FOSCAT_uncoded_1:WOS_FOSCAT_uncoded_6) %>% 
  filter(!is.na(WOS.OECD)) %>% 
  group_by(WOS.OECD) %>% 
  distinct(VARBEIDLOPENR, WOS.OECD, .keep_all = TRUE) %>% 
  summarise(sum = sum(as.double(Fract_count))) %>% 
  filter(WOS.OECD %in% cog_vars_SSH.FOS)

sum <- c(sum(B_CRISTIN_SSH_WOS$sum[1:9]),
         sum(B_CRISTIN_SSH_WOS$sum[10:14]),
         sum(as.double(B_CRISTIN$Fract_count))
)

totals.B.CRISTIN.WOS <- data.frame(WOS.OECD, sum)

B_CRISTIN_SSH_WOS <- B_CRISTIN_SSH_WOS %>% 
  rbind(totals.B.CRISTIN.WOS) %>% 
  mutate(share = sum / sum(as.double(B_CRISTIN$Fract_count)) * 100)

# Appendix 2. Dataset B. VABB and NSD -------------------------------------

# Flanders

B_VABB_SSH_VABB <- B_VABB %>% 
  select(Loi, VABB.FOS1:VABB.FOS5, Fract_count, jaccard_vabb_wos) %>% 
  gather(VABB.FOS_nr, VABB.FOS, VABB.FOS1:VABB.FOS5) %>%
  filter(!is.na(VABB.FOS)) %>% 
  group_by(VABB.FOS) %>% 
  summarise(sum = sum(as.double(Fract_count)),
            jaccard = mean(as.double(jaccard_vabb_wos), na.rm = TRUE)) %>% 
  filter(VABB.FOS %in% cog_vars_SSH.FOS) 

VABB.FOS <- c("SS.total", "H.total", "SSH.total")

sum <- c(sum(B_VABB_SSH_VABB$sum[1:9]),
         sum(B_VABB_SSH_VABB$sum[10:14]),
         sum(as.double(B_VABB$Fract_count)))

jaccard <- c(mean(as.double(B_VABB_SSH_VABB$jaccard[1:9]), na.rm = TRUE),
            mean(as.double(B_VABB_SSH_VABB$jaccard[10:14]), na.rm = TRUE),
            mean(as.double(B_VABB$jaccard_vabb_wos), na.rm = TRUE))

totals.B.VABB.VABB <- data.frame(VABB.FOS, sum, jaccard)

B_VABB_SSH_VABB <- B_VABB_SSH_VABB %>% 
  rbind(totals.B.VABB.VABB) %>% 
  mutate(share = sum / sum(as.double(B_VABB$Fract_count)) * 100)

# Norway

B_CRISTIN_SSH_CRISTIN <- B_CRISTIN %>% 
  select(VARBEIDLOPENR, NSD.OECD, Fract_count, jaccard_npu_wos) %>%
  group_by(NSD.OECD) %>% 
  summarise(sum = sum(as.double(Fract_count)),
            jaccard = mean(as.double(jaccard_npu_wos), na.rm = TRUE)) %>% 
  filter(NSD.OECD %in% cog_vars_SSH.FOS)

NSD.OECD <- c("SS.total", "H.total", "SSH.total")

sum <- c(sum(B_CRISTIN_SSH_CRISTIN$sum[1:9]),
         sum(B_CRISTIN_SSH_CRISTIN$sum[10:14]),
         sum(as.double(B_CRISTIN$Fract_count))
)

jaccard <- c(mean(as.double(B_CRISTIN_SSH_CRISTIN$jaccard[1:9]), na.rm = TRUE),
             mean(as.double(B_CRISTIN_SSH_CRISTIN$jaccard[10:14]), na.rm = TRUE),
             mean(as.double(B_CRISTIN$jaccard_npu_wos), na.rm = TRUE))

totals.B.CRISTIN.CRISTIN <- data.frame(NSD.OECD, sum, jaccard)

B_CRISTIN_SSH_CRISTIN <- B_CRISTIN_SSH_CRISTIN %>% 
  rbind(totals.B.CRISTIN.CRISTIN) %>% 
  mutate(share = sum / sum(as.double(B_CRISTIN$Fract_count)) * 100)


# Appendix 2. Dataset B. Combined -----------------------------------------

# Flanders

B_VABB.combined <- cbind(B_VABB_SSH_VABB, B_VABB_SSH_WOS)
names(B_VABB.combined) <- c("Discipline", "n.VABB", "jaccard", "share.VABB", "d", "n.WOS", "share.WOS")

B_VABB.combined <- B_VABB.combined %>% 
  select(Discipline, n.VABB, n.WOS, share.VABB, share.WOS, jaccard) %>% 
  mutate(
    share.difference.VABB = (share.VABB - share.WOS),
    share.difference.WOS = (share.WOS - share.VABB),
    percentage.difference.VABB = ((n.VABB - n.WOS) / ((n.WOS + n.VABB) / 2)) * 100,
    percentage.difference.WOS = ((n.WOS - n.VABB) / ((n.WOS + n.VABB) / 2)) * 100,
    percentage.error.VABB = ((n.VABB - n.WOS) / n.VABB) * 100,
    percentage.error.WOS = ((n.WOS - n.VABB) / n.WOS) * 100
  )


# Norway

B_CRISTIN.combined <- cbind(B_CRISTIN_SSH_CRISTIN, B_CRISTIN_SSH_WOS)
names(B_CRISTIN.combined) <- c("Discipline", "n.CRISTIN", "jaccard", "share.CRISTIN", "d", "n.WOS", "share.WOS")

B_CRISTIN.combined <- B_CRISTIN.combined %>% 
  select(Discipline, n.CRISTIN, n.WOS, share.CRISTIN, share.WOS, jaccard) %>% 
  mutate(
    share.difference.CRISTIN = (share.CRISTIN - share.WOS),
    share.difference.WOS = (share.WOS - share.CRISTIN),
    percentage.difference.CRISTIN = ((n.CRISTIN - n.WOS) / ((n.WOS + n.CRISTIN) / 2)) * 100,
    percentage.difference.WOS = ((n.WOS - n.CRISTIN) / ((n.WOS + n.CRISTIN) / 2)) * 100,
    percentage.error.CRISTIN = ((n.CRISTIN - n.WOS) / n.CRISTIN) * 100,
    percentage.error.WOS = ((n.WOS - n.CRISTIN) / n.WOS) * 100
  )
