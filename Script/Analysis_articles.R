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

VABBdata$SM.OECD <- ifelse(!is.na(VABBdata$SM_OECD), paste0("FOS_",VABBdata$SM_OECD), NA)
CRISTINdata$SM.OECD <- ifelse(!is.na(CRISTINdata$SM_OECD), paste0("FOS_",CRISTINdata$SM_OECD), NA)

# Define SSH cats

cog_vars_SSH.FOS <- c(paste0("FOS_5_", 1:9),
                      paste0("FOS_6_", 1:5),
                      paste0("FOS_6_1_", 1:2),
                      paste0("FOS_6_2_", 1:2),
                      paste0("FOS_6_3_", 1:2),
                      "FOS_5 AND 6")


cog_vars_SS.FOS <- c(paste0("FOS_5_", 1:9))
cog_vars_H.FOS <- c(paste0("FOS_6_", 1:5),
                    paste0("FOS_6_1_", 1:2),
                    paste0("FOS_6_2_", 1:2),
                    paste0("FOS_6_3_", 1:2))

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

# Flanders

A.VABB.SSH.only <- A_VABB %>% 
  select(Loi, VABB.FOS1:VABB.FOS5, Fract_count) %>% 
  gather(VABB.FOS_nr, VABB.FOS, VABB.FOS1:VABB.FOS5) %>%
  filter(VABB.FOS %in% cog_vars_SSH.FOS) %>% 
  distinct(Loi, VABB.FOS, .keep_all = TRUE)

A.VABB.SSH.only.distinct <- A.VABB.SSH.only %>% distinct(Loi, .keep_all = TRUE)
A.VABB.SS <- A.VABB.SSH.only %>% filter(VABB.FOS %in% cog_vars_SS.FOS) %>% distinct(Loi, .keep_all = TRUE)
A.VABB.H <- A.VABB.SSH.only %>% filter(VABB.FOS %in% cog_vars_H.FOS) %>% distinct(Loi, .keep_all = TRUE)
  
A_VABB_SSH <- A.VABB.SSH.only %>% 
  group_by(VABB.FOS) %>% 
  summarise(sum = sum(as.double(Fract_count))) 
  
VABB.FOS <- c("SS.total", "H.total", "SSH.total")

sum <- c(sum(as.double(A.VABB.SS$Fract_count)),
       sum(as.double(A.VABB.H$Fract_count)),
       sum(as.double(A.VABB.SSH.only.distinct$Fract_count))
       )

totals.A.VABB <- data.frame(VABB.FOS, sum)

A_VABB_SSH <- A_VABB_SSH %>% 
  rbind(totals.A.VABB) %>% 
  mutate(share = sum / sum(as.double(A.VABB.SSH.only.distinct$Fract_count)) * 100)

# Norway

A.CRISTIN.SSH.only <- A_CRISTIN %>% 
  select(VARBEIDLOPENR, NSD.OECD, Fract_count) %>% 
  filter(NSD.OECD %in% cog_vars_SSH.FOS) %>% 
  distinct(VARBEIDLOPENR, NSD.OECD, .keep_all = TRUE)

A.CRISTIN.SS.only <- A.CRISTIN.SSH.only %>% filter(NSD.OECD %in% cog_vars_SS.FOS)
A.CRISTIN.H.only <- A.CRISTIN.SSH.only %>% filter(NSD.OECD %in% cog_vars_H.FOS)

A_CRISTIN_SSH <- A.CRISTIN.SSH.only %>% 
  select(VARBEIDLOPENR, NSD.OECD, Fract_count) %>% 
  filter(NSD.OECD %in% cog_vars_SSH.FOS) %>% 
  group_by(NSD.OECD) %>% 
  summarise(sum = sum(as.double(Fract_count)))

NSD.OECD <- c("SS.total", "H.total", "SSH.total")

sum <- c(sum(as.double(A.CRISTIN.SS.only$Fract_count)),
         sum(as.double(A.CRISTIN.H.only$Fract_count)),
         sum(as.double(A.CRISTIN.SSH.only$Fract_count))
         )

totals.A.CRISTIN <- data.frame(NSD.OECD, sum)

A_CRISTIN_SSH <- A_CRISTIN_SSH %>% 
  rbind(totals.A.CRISTIN) %>% 
  mutate(share = sum / sum(as.double(A_CRISTIN$Fract_count)) * 100)
  
# Appendix 2. Dataset B WoS -----------------------------------------------

# Flanders

WOS.onlySSH.VABB <- B_VABB %>% 
  select(Loi, Fract_count, WOS_FOSCAT_uncoded_1:WOS_FOSCAT_uncoded_6) %>% 
  gather(WOS.nr, WOS.OECD, WOS_FOSCAT_uncoded_1:WOS_FOSCAT_uncoded_6) %>% 
  filter(WOS.OECD %in% cog_vars_SSH.FOS) %>% 
  distinct(Loi, WOS.OECD, .keep_all = TRUE)

WOS.onlySSH.VABB.distinct <- WOS.onlySSH.VABB %>% distinct(Loi, .keep_all = TRUE)
WOS.onlySS.VABB <- WOS.onlySSH.VABB %>% filter(WOS.OECD %in% cog_vars_SS.FOS) %>% distinct(Loi, .keep_all = TRUE)
WOS.onlyH.VABB <- WOS.onlySSH.VABB %>% filter(WOS.OECD %in% cog_vars_H.FOS) %>% distinct(Loi, .keep_all = TRUE)

B_VABB_SSH_WOS <- WOS.onlySSH.VABB %>% 
  group_by(WOS.OECD) %>% 
  summarise(sum = sum(as.double(Fract_count)))

sum <- c(sum(as.double(WOS.onlySS.VABB$Fract_count)),
         sum(as.double(WOS.onlyH.VABB$Fract_count)),
         sum(as.double(WOS.onlySSH.VABB.distinct$Fract_count))
)

WOS.OECD <- c("SS.total", "H.total", "SSH.total")

totals.B.VABB.WOS <- data.frame(WOS.OECD, sum)


B_VABB_SSH_WOS <- B_VABB_SSH_WOS %>% 
  rbind(totals.B.VABB.WOS) %>% 
  mutate(share = sum / sum(as.double(WOS.onlySSH.VABB.distinct$Fract_count)) * 100)

# Norway

WOS.onlySSH.CRISTIN <- B_CRISTIN %>% 
  select(VARBEIDLOPENR, Fract_count, WOS_FOSCAT_uncoded_1:WOS_FOSCAT_uncoded_6) %>% 
  gather(WOS.nr, WOS.OECD, WOS_FOSCAT_uncoded_1:WOS_FOSCAT_uncoded_6) %>% 
  filter(WOS.OECD %in% cog_vars_SSH.FOS) %>% 
  distinct(VARBEIDLOPENR, WOS.OECD, .keep_all = TRUE)

WOS.onlySSH.CRISTIN.distinct <- WOS.onlySSH.CRISTIN %>% distinct(VARBEIDLOPENR, .keep_all = TRUE)
WOS.onlySS.CRISTIN <- WOS.onlySSH.CRISTIN %>% filter(WOS.OECD %in% cog_vars_SS.FOS) %>% distinct(VARBEIDLOPENR, .keep_all = TRUE)
WOS.onlyH.CRISTIN <- WOS.onlySSH.CRISTIN %>% filter(WOS.OECD %in% cog_vars_H.FOS) %>% distinct(VARBEIDLOPENR, .keep_all = TRUE)


B_CRISTIN_SSH_WOS <- WOS.onlySSH.CRISTIN %>% 
  group_by(WOS.OECD) %>% 
  summarise(sum = sum(as.double(Fract_count)))

sum <- c(sum(as.double(WOS.onlySS.CRISTIN$Fract_count)),
         sum(as.double(WOS.onlyH.CRISTIN$Fract_count)),
         sum(as.double(WOS.onlySSH.CRISTIN.distinct$Fract_count))
)

totals.B.CRISTIN.WOS <- data.frame(WOS.OECD, sum)

B_CRISTIN_SSH_WOS <- B_CRISTIN_SSH_WOS %>% 
  rbind(totals.B.CRISTIN.WOS) %>% 
  mutate(share = sum / sum(as.double(WOS.onlySSH.CRISTIN.distinct$Fract_count)) * 100)

# Appendix 2. Dataset B. VABB and NSD -------------------------------------

# Flanders

B.onlySSH.VABB <- B_VABB %>% 
  select(Loi, VABB.FOS1:VABB.FOS5, Fract_count, jaccard_vabb_wos) %>% 
  gather(VABB.FOS_nr, VABB.FOS, VABB.FOS1:VABB.FOS5) %>%
  filter(VABB.FOS %in% cog_vars_SSH.FOS) %>% 
  distinct(Loi, VABB.FOS, .keep_all = TRUE)

B.onlySSH.VABB.distinct <- B.onlySSH.VABB %>% distinct(Loi, .keep_all = TRUE)
B.onlySS.VABB <- B.onlySSH.VABB %>% filter(VABB.FOS %in% cog_vars_SS.FOS) %>% distinct(Loi, .keep_all = TRUE)
B.onlyH.VABB <- B.onlySSH.VABB %>% filter(VABB.FOS %in% cog_vars_H.FOS) %>% distinct(Loi, .keep_all = TRUE)

B_VABB_SSH_VABB <- B.onlySSH.VABB %>% 
  group_by(VABB.FOS) %>% 
  summarise(sum = sum(as.double(Fract_count)),
            jaccard = mean(as.double(jaccard_vabb_wos), na.rm = TRUE))

VABB.FOS <- c("SS.total", "H.total", "SSH.total")

sum <- c(sum(as.double(B.onlySS.VABB$Fract_count)),
         sum(as.double(B.onlyH.VABB$Fract_count)),
         sum(as.double(B.onlySSH.VABB.distinct$Fract_count)))


jaccard <- c(mean(as.double(B_VABB_SSH_VABB$jaccard[1:9]), na.rm = TRUE),
            mean(as.double(B_VABB_SSH_VABB$jaccard[10:14]), na.rm = TRUE),
            mean(as.double(B_VABB$jaccard_vabb_wos), na.rm = TRUE))

totals.B.VABB.VABB <- data.frame(VABB.FOS, sum, jaccard)

B_VABB_SSH_VABB <- B_VABB_SSH_VABB %>% 
  rbind(totals.B.VABB.VABB) %>% 
  mutate(share = sum / sum(as.double(B.onlySSH.VABB.distinct$Fract_count)) * 100)

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

# Appendix 3. Dataset C. Science-Metrix -----------------------------------

# Flanders

SM.onlySSH.VABB <- C_VABB %>% 
  select(Loi, Fract_count, SM.OECD) %>% 
  filter(SM.OECD %in% cog_vars_SSH.FOS) %>% 
  distinct(Loi, SM.OECD, .keep_all = TRUE)

SM.onlySSH.VABB.distinct <- SM.onlySSH.VABB %>% distinct(Loi, .keep_all = TRUE)
SM.onlySS.VABB <- SM.onlySSH.VABB %>% filter(SM.OECD %in% cog_vars_SS.FOS) %>% distinct(Loi, .keep_all = TRUE)
SM.onlyH.VABB <- SM.onlySSH.VABB %>% filter(SM.OECD %in% cog_vars_H.FOS) %>% distinct(Loi, .keep_all = TRUE)

C_VABB_SSH_SM <- SM.onlySSH.VABB %>% 
  group_by(SM.OECD) %>% 
  summarise(sum = sum(as.double(Fract_count)))

sum <- c(sum(as.double(SM.onlySS.VABB$Fract_count)),
         sum(as.double(SM.onlyH.VABB$Fract_count)),
         sum(as.double(SM.onlySSH.VABB.distinct$Fract_count))
)

SM.OECD <- c("SS.total", "H.total", "SSH.total")

totals.C.VABB.SM <- data.frame(SM.OECD, sum)

C_VABB_SSH_SM <- C_VABB_SSH_SM %>% 
  rbind(totals.C.VABB.SM) %>% 
  mutate(share = sum / sum(as.double(SM.onlySSH.VABB.distinct$Fract_count)) * 100)

# Norway

C.CRISTIN.SSH.only.SM <- C_CRISTIN %>% 
  select(VARBEIDLOPENR, SM.OECD, Fract_count) %>% 
  filter(SM.OECD %in% cog_vars_SSH.FOS) %>% 
  distinct(VARBEIDLOPENR, SM.OECD, .keep_all = TRUE)

C.CRISTIN.SS.only.SM <- C.CRISTIN.SSH.only.SM %>% filter(SM.OECD %in% cog_vars_SS.FOS)
C.CRISTIN.H.only.SM <- C.CRISTIN.SSH.only.SM %>% filter(SM.OECD %in% cog_vars_H.FOS)

C_CRISTIN_SSH_SM <- C.CRISTIN.SSH.only.SM %>% 
  select(VARBEIDLOPENR, SM.OECD, Fract_count) %>% 
  filter(SM.OECD %in% cog_vars_SSH.FOS) %>% 
  group_by(SM.OECD) %>% 
  summarise(sum = sum(as.double(Fract_count)))

SM.OECD <- c("SS.total", "H.total", "SSH.total")

sum <- c(sum(as.double(C.CRISTIN.SS.only.SM$Fract_count)),
         sum(as.double(C.CRISTIN.H.only.SM$Fract_count)),
         sum(as.double(C.CRISTIN.SSH.only.SM$Fract_count))
)

totals.C.CRISTIN <- data.frame(SM.OECD, sum)

C_CRISTIN_SSH_SM <- C_CRISTIN_SSH_SM %>% 
  rbind(totals.C.CRISTIN) %>% 
  mutate(share = sum / sum(as.double(C.CRISTIN.SSH.only.SM$Fract_count)) * 100)

# Appendix 3. Dataset C. VABB and NSD -----------------------------------

# Flanders

C.onlySSH.VABB <- C_VABB %>% 
  select(Loi, VABB.FOS1:VABB.FOS5, Fract_count, jaccard_vabb_sm) %>% 
  gather(VABB.FOS_nr, VABB.FOS, VABB.FOS1:VABB.FOS5) %>%
  filter(VABB.FOS %in% cog_vars_SSH.FOS) %>% 
  distinct(Loi, VABB.FOS, .keep_all = TRUE)

C.onlySSH.VABB.distinct <- C.onlySSH.VABB %>% distinct(Loi, .keep_all = TRUE)
C.onlySS.VABB <- C.onlySSH.VABB %>% filter(VABB.FOS %in% cog_vars_SS.FOS) %>% distinct(Loi, .keep_all = TRUE)
C.onlyH.VABB <- C.onlySSH.VABB %>% filter(VABB.FOS %in% cog_vars_H.FOS) %>% distinct(Loi, .keep_all = TRUE)

C_VABB_SSH_VABB <- C.onlySSH.VABB %>% 
  group_by(VABB.FOS) %>% 
  summarise(sum = sum(as.double(Fract_count)),
            jaccard = mean(as.double(jaccard_vabb_sm), na.rm = TRUE))

VABB.FOS <- c("SS.total", "H.total", "SSH.total")

sum <- c(sum(as.double(C.onlySS.VABB$Fract_count)),
         sum(as.double(C.onlyH.VABB$Fract_count)),
         sum(as.double(C.onlySSH.VABB.distinct$Fract_count)))


jaccard <- c(mean(as.double(C_VABB_SSH_VABB$jaccard[1:9]), na.rm = TRUE),
             mean(as.double(C_VABB_SSH_VABB$jaccard[10:14]), na.rm = TRUE),
             mean(as.double(C_VABB$jaccard_vabb_wos), na.rm = TRUE))

totals.C.VABB.VABB <- data.frame(VABB.FOS, sum, jaccard)

C_VABB_SSH_VABB <- C_VABB_SSH_VABB %>% 
  rbind(totals.C.VABB.VABB) %>% 
  mutate(share = sum / sum(as.double(C.onlySSH.VABB.distinct$Fract_count)) * 100)

# Norway

C_CRISTIN_SSH_CRISTIN <- C_CRISTIN %>% 
  select(VARBEIDLOPENR, NSD.OECD, Fract_count, jaccard_npu_sm) %>%
  group_by(NSD.OECD) %>% 
  summarise(sum = sum(as.double(Fract_count)),
            jaccard = mean(as.double(jaccard_npu_sm), na.rm = TRUE)) %>% 
  filter(NSD.OECD %in% cog_vars_SSH.FOS)

NSD.OECD <- c("SS.total", "H.total", "SSH.total")

sum <- c(sum(C_CRISTIN_SSH_CRISTIN$sum[1:9]),
         sum(C_CRISTIN_SSH_CRISTIN$sum[10:14]),
         sum(as.double(C_CRISTIN$Fract_count))
)

jaccard <- c(mean(as.double(C_CRISTIN_SSH_CRISTIN$jaccard[1:9]), na.rm = TRUE),
             mean(as.double(C_CRISTIN_SSH_CRISTIN$jaccard[10:14]), na.rm = TRUE),
             mean(as.double(C_CRISTIN$jaccard_npu_sm), na.rm = TRUE))

totals.C.CRISTIN.CRISTIN <- data.frame(NSD.OECD, sum, jaccard)

C_CRISTIN_SSH_CRISTIN <- C_CRISTIN_SSH_CRISTIN %>% 
  rbind(totals.C.CRISTIN.CRISTIN) %>% 
  mutate(share = sum / sum(as.double(C_CRISTIN$Fract_count)) * 100)

# Appendix 3. Dataset C. Combined -----------------------------------------

# Flanders

dat.dum <- data.frame(matrix(nrow = 1, ncol = ncol(C_VABB_SSH_VABB)))
names(dat.dum) <- names(C_VABB_SSH_VABB)

C_VABB_SSH_VABB <- rbind(dat.dum, C_VABB_SSH_VABB)

C_VABB.combined <- cbind(C_VABB_SSH_VABB, C_VABB_SSH_SM)
names(C_VABB.combined) <- c("Discipline", "n.VABB", "jaccard", "share.VABB", "d", "n.SM", "share.SM")

C_VABB.combined <- C_VABB.combined %>% 
  select(Discipline, n.VABB, n.SM, share.VABB, share.SM, jaccard) %>% 
  mutate(
    share.difference.VABB = (share.VABB - share.SM),
    share.difference.SM = (share.SM - share.VABB),
    percentage.difference.VABB = ((n.VABB - n.SM) / ((n.SM + n.VABB) / 2)) * 100,
    percentage.difference.SM = ((n.SM - n.VABB) / ((n.SM + n.VABB) / 2)) * 100,
    percentage.error.VABB = ((n.VABB - n.SM) / n.VABB) * 100,
    percentage.error.SM = ((n.SM - n.VABB) / n.SM) * 100
  )

# Norway

dat.dum <- data.frame(matrix(nrow = 1, ncol = ncol(C_CRISTIN_SSH_CRISTIN)))
names(dat.dum) <- names(C_CRISTIN_SSH_CRISTIN)
C_CRISTIN_SSH_CRISTIN <- rbind(dat.dum, C_CRISTIN_SSH_CRISTIN)

C_CRISTIN.combined <- cbind(C_CRISTIN_SSH_CRISTIN, C_CRISTIN_SSH_SM)
names(C_CRISTIN.combined) <- c("Discipline", "n.CRISTIN", "jaccard", "share.CRISTIN", "d", "n.SM", "share.SM")

C_CRISTIN.combined <- C_CRISTIN.combined %>% 
  select(Discipline, n.CRISTIN, n.SM, share.CRISTIN, share.SM, jaccard) %>% 
  mutate(
    share.difference.CRISTIN = (share.CRISTIN - share.SM),
    share.difference.SM = (share.SM - share.CRISTIN),
    percentage.difference.CRISTIN = ((n.CRISTIN - n.SM) / ((n.SM + n.CRISTIN) / 2)) * 100,
    percentage.difference.SM = ((n.SM - n.CRISTIN) / ((n.SM + n.CRISTIN) / 2)) * 100,
    percentage.error.CRISTIN = ((n.CRISTIN - n.SM) / n.CRISTIN) * 100,
    percentage.error.SM = ((n.SM - n.CRISTIN) / n.SM) * 100
  )

