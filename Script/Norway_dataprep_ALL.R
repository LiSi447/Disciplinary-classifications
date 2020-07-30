# call packages ----------------------------------------------------------

library(tidyverse)
library(stringdist)

# import data -------------------------------------------------------------

CRISTINdata.FULL <- as_tibble(foreign::read.dta("./Raw data/CRISTIN/Cristin_2005_2017.dta")) # CRISTIN dataset

nsd_COMPLETE <- read_csv2("./Raw data/CRISTIN/npu_1_journals.csv",
                          col_types = cols(.default = "c")) # Norwegian classification

SCIENCEMETRIXjournals <- read_csv2("./Raw data/Science-Metrix/sm_journal_classification_106_1_corr.csv",
                                   col_types = cols(.default = "c")) # Science-Metrix classification

SM_crosswalk <- read_csv2("./Raw data/Science-Metrix/SM_OECD_crosswalk.csv",
                          col_types = cols(.default = "c")) # Science-Metrix to OECD FORD cross-walk

# prep CRISTIN data -------------------------------------------------------

# Adjust column names
names(CRISTINdata.FULL) <- toupper(names(CRISTINdata.FULL))

# Delineate year and publication type and omit records with missing ISSN
CRISTINdata.01 <- CRISTINdata.FULL %>% 
  filter(ARSTALL %in% c(2006:2015) & VARBEIDUNDERKATKODE == "ARTIKKEL") %>% 
  filter(ISSN != "")

# Create a CRISTIN ISSN file

CRISTIN_ISSNs <- CRISTINdata.FULL %>% 
  select(VARBEIDLOPENR, ISSN, ISSN_ELEKTRONISK) %>% 
  gather(ISSN_NR, ISSN, ISSN:ISSN_ELEKTRONISK) %>% 
  filter(!is.na(ISSN)) %>% 
  distinct(VARBEIDLOPENR, ISSN)

# Assign fractionalised count ---------------------------------------------

# calculate fractions (authors with two institutions taken into account)

n_duplicate_authorID <- CRISTINdata.01 %>% 
  group_by(VARBEIDLOPENR, GENERERT_PERSONLOPENR) %>% 
  count()

colnames(n_duplicate_authorID)[3] <- "N_AuthorID"

CRISTINdata.02 <- CRISTINdata.01 %>%
  left_join(n_duplicate_authorID, by = c("VARBEIDLOPENR", "GENERERT_PERSONLOPENR")) %>% 
  mutate(
    denominator = ANTALL_FORFATTERE * N_AuthorID,
    fraction = 1 / denominator
  )

#Calculate fractionalised count per publication ID

pub_fract <- aggregate(CRISTINdata.02$fraction, by=list(VARBEIDLOPENR=CRISTINdata.02$VARBEIDLOPENR), FUN=sum)

colnames(pub_fract)[2] <- c("Fract_count")

#Assign to Cristin data

CRISTINdata.03 <- CRISTINdata.02 %>% left_join(pub_fract, by = "VARBEIDLOPENR")

# Delineate institutions
NORW.inst <- c("00000000184",
               "00000000185",
               "00000000186",
               "00000000192",
               "00000000194",
               "00000000201",
               "00000000204",
               "00000000217")

CRISTINdata.04 <- filter(CRISTINdata.03, INSTITUSJONSNR %in% NORW.inst)

#deduplicate
CRISTINdata.05 <- distinct(CRISTINdata.04, VARBEIDLOPENR, .keep_all = TRUE)

# Add info on the first author --------------------------------------------

first.authors <- CRISTINdata.FULL %>% 
  filter(REKKEFOLGENR == 1) %>% 
  distinct(VARBEIDLOPENR, ETTERNAVN)

names(first.authors)[2] <- "first.author"

# REKKEFOLGENR does not seem to indicate always correctly the author's position

CRISTINdata.06 <- CRISTINdata.05 %>% left_join(first.authors, by = "VARBEIDLOPENR") %>% distinct(VARBEIDLOPENR, .keep_all = TRUE)
# The above is not correct because there is more than one author that gets selected when selecting the first position
# I take the first name without checking which one is correct (applies to 15572 records; most retrieved from WoS, Scopus, Forskdok or Norart)

# Add info on levels and NSD codes ---------------------------------------------------

# Generate a variable with the most recent info on level

Level_variables <- names(nsd_COMPLETE)[17:30]

nsd_COMPLETE$level <- NA

system.time(for (i in 1:nrow(nsd_COMPLETE)) {
  
  for (var in Level_variables) {
    if (!is.na(nsd_COMPLETE$level[i])) {
      break
    }
    if (is.na(nsd_COMPLETE[var][i,])) {
      next
    }
    if (!is.na(nsd_COMPLETE[var][i,])) {
      nsd_COMPLETE$level[i] <- nsd_COMPLETE[var][i,]
    } 
  }
})


# Recode NSD values

nsd_COMPLETE$NSD.OECD <- fct_collapse(nsd_COMPLETE$`NPI Fagfelt`,
                                      FOS_5_1 = c("Psykologi"),
                                      FOS_5_2 = c("Økonomisk-administrative fag", "Samfunnsøkonomi", "Industriell økonomi"),
                                      FOS_5_3 = c("Pedagogikk og utdanning"),
                                      FOS_5_4 = c("Kjønnsforskning", "Sosialantropologi", "Sosialforskning", "Sosiologi", "Asiatiske og afrikanske studier"),
                                      FOS_5_5 = c("Rettsvitenskap"),
                                      FOS_5_6 = c("Statsvitenskap"),
                                      FOS_5_7 = c("Geografi"),
                                      FOS_5_8 = c("Biblioteks- og informasjonsvitenskap", "Medier og kommunikasjon"),
                                      FOS_5_9 = c("Tverrfaglig samfunnsforskning", "Utviklingsstudier"),
                                      FOS_6_1 = c("Historie", "Arkeologi og konservering"),
                                      FOS_6_2 = c("Engelsk", "Gresk og latin", "Lingvistikk", "Litteraturvitenskap",
                                                  "Nordisk", "Romansk", "Slavisk-baltisk", "Tysk og nederlandsk"),
                                      FOS_6_3 = c("Filosofi og idéhistorie", "Teologi og religionsvitenskap"),
                                      FOS_6_4 = c("Arkitektur og design", "Dans", "Kunsthistorie", "Kulturvitenskap",
                                                  "Musikkvitenskap", "Teatervitenskap og drama"),
                                      FOS_6_5 = c("Tverrfaglig humanistisk forskning"))

# Create a smaller file

NSD_mini <- nsd_COMPLETE %>% 
  select(ISSN1 = `Print ISSN`, ISSN2 = `Online ISSN`, NSD.OECD, level) %>% 
  gather(ISSN_nr, ISSN, ISSN1:ISSN2) %>% 
  filter(!is.na(ISSN)) %>% 
  select(-ISSN_nr)

# Add NSD classification and levels

CRISTIN_ISSNs_NSD <- left_join(CRISTIN_ISSNs, NSD_mini, by = "ISSN") %>% 
  filter(!is.na(NSD.OECD) & NSD.OECD != "Ikke tildelt") %>% 
  select(VARBEIDLOPENR, NSD.OECD, level) %>% 
  distinct(VARBEIDLOPENR, .keep_all = TRUE)

CRISTINdata.07 <- left_join(CRISTINdata.06, CRISTIN_ISSNs_NSD, by = "VARBEIDLOPENR")

# Select peer-reviewed only and SSH

SSHvars <- c(paste0("FOS_5_", 1:9), paste0("FOS_6_", 1:5))

CRISTINdata.08 <- filter(CRISTINdata.07, level %in% c(1, 2) & NSD.OECD %in% SSHvars)

# Add Science-Metrix classification ---------------------------------------

SM_ISSNs <- SCIENCEMETRIXjournals %>% 
  gather(ISSN_nr, ISSN, issn:essn) %>% 
  distinct(smsid, ISSN, .keep_all = TRUE) %>% 
  left_join(SM_crosswalk, by = c("Domain_English", "Field_English", "SubField_English")) %>% 
  select(ISSN, Domain_English, SubField_English, OECF_FORD_1) %>% 
  filter(!is.na(ISSN))

names(SM_ISSNs) <- c("ISSN", "SM_TOP", "SM", "SM_OECD")

CRISTIN_SM <- CRISTIN_ISSNs %>% 
  left_join(SM_ISSNs, by = "ISSN") %>% 
  filter(!is.na(SM_OECD)) %>% 
  distinct(VARBEIDLOPENR, SM_TOP, SM, SM_OECD)
CRISTINdata.09 <- left_join(CRISTINdata.08, CRISTIN_SM, by = "VARBEIDLOPENR")