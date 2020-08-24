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

NORWAYdataWOS <- read_csv("./Raw data/Web of Science - KUL/Norway_201807231548.csv")

WOS_SCIE <- read_csv2("./Raw data/Web of Science - online/SCIE2018.csv",
                      col_types = cols(.default = "c")) # SCIE journal list extracted from pdf
WOS_SSCI <- read_csv2("./Raw data/Web of Science - online/SSCI2018.csv",
                      col_types = cols(.default = "c")) # SSCI journal list extracted from pdf
WOS_AHCI <- read_csv2("./Raw data/Web of Science - online/AHCI2018.csv",
                      col_types = cols(.default = "c")) # AHCI journal list extracted from pdf

NORWAYpairs <- read_delim("./Raw data/Web of Science - matching/matching-pairs-NO-fullref-topk-256perms.csv",
                            col_types = list("jaccard" = col_double()),
                            delim = ",")

MANUALPAIRS <- read_csv2("./Raw data/Web of Science - matching/NO_WOSCRISTIN_MANUALPAIRS_19112018.CSV")

WOSdata <- read_csv("./Raw data/Web of Science - online/WOSdata_17012019.csv",
                    col_types = list("WOS_UT_FINAL" = col_character()))

All_journals <- read_csv("./Raw data/Alljournals_09072019_CLEANED.csv", # journals' classification ; ERIH not included
                         col_types = cols(.default = "c"))

erih <- read_csv2("./Raw data/ERIH PLUS/2020-04-03 ERIH PLUS approved journals and series.csv")

NO_Jaccard <- read_csv("./Raw data/Jaccard calculation/NO_Jaccard.csv")

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
  filter(!is.na(ISSN)  & ISSN != "") %>% 
  distinct(VARBEIDLOPENR, ISSN)

# Delineate institutions
NORW.inst <- c("00000000184",
               "00000000185",
               "00000000186",
               "00000000192",
               "00000000194",
               "00000000201",
               "00000000204",
               "00000000217")

CRISTINdata.02 <- filter(CRISTINdata.01, INSTITUSJONSNR %in% NORW.inst)

# Assign fractionalised count ---------------------------------------------

# calculate fractions (authors with two institutions taken into account)

n_duplicate_authorID <- CRISTINdata.02 %>% 
  group_by(VARBEIDLOPENR, GENERERT_PERSONLOPENR) %>% 
  count()

colnames(n_duplicate_authorID)[3] <- "N_AuthorID"

CRISTINdata.03 <- CRISTINdata.02 %>%
  left_join(n_duplicate_authorID, by = c("VARBEIDLOPENR", "GENERERT_PERSONLOPENR")) %>% 
  mutate(
    denominator = ANTALL_FORFATTERE * N_AuthorID,
    fraction = 1 / denominator
  )

#Calculate fractionalised count per publication ID

pub_fract <- aggregate(CRISTINdata.03$fraction, by=list(VARBEIDLOPENR=CRISTINdata.02$VARBEIDLOPENR), FUN=sum)

colnames(pub_fract)[2] <- c("Fract_count")

#Assign to Cristin data

CRISTINdata.04 <- CRISTINdata.03 %>% left_join(pub_fract, by = "VARBEIDLOPENR")

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

nsd_COMPLETE$level <- unlist(nsd_COMPLETE$level)

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

CRISTINdata.09$SM.OECD <- ifelse(!is.na(CRISTINdata.09$SM_OECD), paste0("FOS_",CRISTINdata.09$SM_OECD), NA)

# Add WoS classification --------------------------------------------------

NORWAYdata <- CRISTINdata.09

# Prep CRISTIN data

NORWAYdata <- NORWAYdata %>% 
  mutate(
    bp_CLEANED = as.integer(str_replace_all(NORWAYdata$SIDENR_FRA, "[:alpha:]|[:punct:]|[:space:]", "")),
    ep_CLEANED = as.integer(str_replace_all(NORWAYdata$SIDENR_TIL, "[:alpha:]|[:punct:]|[:space:]", "")),
    title_CLEANED = tolower(str_replace_all(NORWAYdata$TITTELTEKST_ORIGINAL, "[:punct:]", ""))
  )

# Prepare WOS data

NORWAYdataWOS <- NORWAYdataWOS %>% 
  mutate(
    bp_CLEANED = as.integer(str_replace_all(NORWAYdataWOS$BEGINPAGINA, "[:alpha:]|[:punct:]|[:space:]", "")),
    ep_CLEANED = as.integer(str_replace_all(NORWAYdataWOS$EINDPAGINA, "[:alpha:]|[:punct:]|[:space:]", "")),
    title_CLEANED = tolower(str_replace_all(NORWAYdataWOS$TITEL, "[:punct:]", "")),
    first.author = str_replace(str_extract(NORWAYdataWOS$AUTEUR, "[:graph:]*"), ",$", "")
  )

# Add additional ISSNs from WoS journal lists in WOS data

NORWAYdataWOS$print.ISSN_1 <- ifelse(NORWAYdataWOS$ISSN %in% WOS_SCIE$ISSN, WOS_SCIE$ISSN[match(NORWAYdataWOS$ISSN, WOS_SCIE$ISSN)], NA)
NORWAYdataWOS$print.ISSN_2 <- ifelse(NORWAYdataWOS$ISSN %in% WOS_SSCI$ISSN, WOS_SSCI$ISSN[match(NORWAYdataWOS$ISSN, WOS_SSCI$ISSN)], NA)
NORWAYdataWOS$print.ISSN_3 <- ifelse(NORWAYdataWOS$ISSN %in% WOS_AHCI$ISSN, WOS_AHCI$ISSN[match(NORWAYdataWOS$ISSN, WOS_AHCI$ISSN)], NA)
NORWAYdataWOS$print.ISSN <- ifelse(!is.na(NORWAYdataWOS$print.ISSN_1), paste(NORWAYdataWOS$print.ISSN_1),
                                   ifelse(!is.na(NORWAYdataWOS$print.ISSN_2), paste(NORWAYdataWOS$print.ISSN_2),
                                          ifelse(!is.na(NORWAYdataWOS$print.ISSN_3), paste(NORWAYdataWOS$print.ISSN_3),
                                                 NA)))
NORWAYdataWOS$online.ISSN_1 <- ifelse(NORWAYdataWOS$ISSN %in% WOS_SCIE$`E-ISSN`, WOS_SCIE$`E-ISSN`[match(NORWAYdataWOS$ISSN, WOS_SCIE$`E-ISSN`)], NA)
NORWAYdataWOS$online.ISSN_2 <- ifelse(NORWAYdataWOS$ISSN %in% WOS_SSCI$`E-ISSN`, WOS_SSCI$`E-ISSN`[match(NORWAYdataWOS$ISSN, WOS_SSCI$`E-ISSN`)], NA)
NORWAYdataWOS$online.ISSN_3 <- ifelse(NORWAYdataWOS$ISSN %in% WOS_AHCI$`E-ISSN`, WOS_AHCI$`E-ISSN`[match(NORWAYdataWOS$ISSN, WOS_AHCI$`E-ISSN`)], NA)
NORWAYdataWOS$online.ISSN <- ifelse(!is.na(NORWAYdataWOS$online.ISSN_1), paste(NORWAYdataWOS$online.ISSN_1),
                                    ifelse(!is.na(NORWAYdataWOS$online.ISSN_2), paste(NORWAYdataWOS$online.ISSN_2),
                                           ifelse(!is.na(NORWAYdataWOS$online.ISSN_3), paste(NORWAYdataWOS$online.ISSN_3),
                                                  NA)))

# Create a smaller file with only necessary vars

NORWAYdataMINI <- NORWAYdata %>% 
  select("VARBEIDLOPENR", "TITTELTEKST_ORIGINAL", "title_CLEANED", "SIDENR_FRA","SIDENR_TIL", "bp_CLEANED", "ep_CLEANED", "ARSTALL","KILDEKODE","KILDEPOSTID",
         "JOURNALNAVN", "ISSN","ISSN_ELEKTRONISK","SPRAKKODE_ORIGINAL", "first.author")

# Add the file with LSH results

names(NORWAYpairs)[1] <- "VARBEIDLOPENR"
NORWAYdataMINI.v2 <- left_join(NORWAYdataMINI, NORWAYpairs, by = "VARBEIDLOPENR")

# Check for similarity
NORWAYdataMINI.v2 <- NORWAYdataMINI.v2 %>% 
  mutate(
    check.year = ifelse(!is.na(NORWAYdataMINI.v2$ARSTALL),
                        NORWAYdataMINI.v2$ARSTALL - NORWAYdataWOS$PUBJAAR[match(NORWAYdataMINI.v2$wos_id, NORWAYdataWOS$PUBID)],
                        NA),
    check.bp = ifelse(!is.na(NORWAYdataMINI.v2$bp_CLEANED),
                      NORWAYdataMINI.v2$bp_CLEANED - NORWAYdataWOS$bp_CLEANED[match(NORWAYdataMINI.v2$wos_id, NORWAYdataWOS$PUBID)],
                      NA),
    check.ep = ifelse(!is.na(NORWAYdataMINI.v2$ep_CLEANED),
                      NORWAYdataMINI.v2$ep_CLEANED - NORWAYdataWOS$ep_CLEANED[match(NORWAYdataMINI.v2$wos_id, NORWAYdataWOS$PUBID)],
                      NA),
    check.authors = ifelse(!is.na(NORWAYdataMINI.v2$first.author),
                           stringdist(NORWAYdataMINI.v2$first.author, NORWAYdataWOS$first.author[match(NORWAYdataMINI.v2$wos_id, NORWAYdataWOS$PUBID)],
                                      method = "lv"),
                           NA),
    check.ISSN_1 = ifelse(!is.na(NORWAYdataMINI.v2$ISSN),
                          stringdist(NORWAYdataMINI.v2$ISSN,
                                     NORWAYdataWOS$ISSN[match(NORWAYdataMINI.v2$wos_id, NORWAYdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_2 = ifelse(!is.na(NORWAYdataMINI.v2$ISSN),
                          stringdist(NORWAYdataMINI.v2$ISSN,
                                     NORWAYdataWOS$print.ISSN[match(NORWAYdataMINI.v2$wos_id, NORWAYdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_3 = ifelse(!is.na(NORWAYdataMINI.v2$ISSN),
                          stringdist(NORWAYdataMINI.v2$ISSN,
                                     NORWAYdataWOS$online.ISSN[match(NORWAYdataMINI.v2$wos_id, NORWAYdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_4 = ifelse(!is.na(NORWAYdataMINI.v2$ISSN_ELEKTRONISK),
                          stringdist(NORWAYdataMINI.v2$ISSN_ELEKTRONISK,
                                     NORWAYdataWOS$ISSN[match(NORWAYdataMINI.v2$wos_id, NORWAYdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_5 = ifelse(!is.na(NORWAYdataMINI.v2$ISSN_ELEKTRONISK),
                          stringdist(NORWAYdataMINI.v2$ISSN_ELEKTRONISK,
                                     NORWAYdataWOS$print.ISSN[match(NORWAYdataMINI.v2$wos_id, NORWAYdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_6 = ifelse(!is.na(NORWAYdataMINI.v2$ISSN_ELEKTRONISK),
                          stringdist(NORWAYdataMINI.v2$ISSN_ELEKTRONISK,
                                     NORWAYdataWOS$online.ISSN[match(NORWAYdataMINI.v2$wos_id, NORWAYdataWOS$PUBID)],
                                     method = "lv"),
                          NA)
  )
#

#exact matches title_matchCHECK == TRUE
NORWAYdataMINI.v2$title_match <- NORWAYdataWOS$PUBID[match(NORWAYdataMINI.v2$title_CLEANED, NORWAYdataWOS$title_CLEANED)]
NORWAYdataMINI.v2$title_matchCHECK <- ifelse(!is.na(NORWAYdataMINI.v2$title_match), 
                                             ifelse(NORWAYdataMINI.v2$ARSTALL == NORWAYdataWOS$PUBJAAR[match(NORWAYdataMINI.v2$title_match, NORWAYdataWOS$PUBID)],
                                                    ifelse(NORWAYdataMINI.v2$bp_CLEANED == NORWAYdataWOS$bp_CLEANED[match(NORWAYdataMINI.v2$title_match, NORWAYdataWOS$PUBID)],
                                                           ifelse(NORWAYdataMINI.v2$ep_CLEANED == NORWAYdataWOS$ep_CLEANED[match(NORWAYdataMINI.v2$title_match, NORWAYdataWOS$PUBID)],
                                                                  TRUE, FALSE),
                                                           FALSE),
                                                    FALSE),
                                             NA)
#rule 1 rule1.ID == "YES"
test.rule1 <- NORWAYdataMINI.v2 %>% 
  filter(jaccard >= 0.75 & (check.year >= -2 & check.year <= 1) & (check.bp == 0|check.ep == 0))
NORWAYdataMINI.v2 <- NORWAYdataMINI.v2 %>% 
  mutate(
    rule1.ID = ifelse(NORWAYdataMINI.v2$VARBEIDLOPENR %in% test.rule1$VARBEIDLOPENR, "YES", "NO") 
  )

#rule 2 rule2.ID == "YES"
test.rule2 <- NORWAYdataMINI.v2 %>% 
  filter(jaccard >= 0.30 & (check.year >= -2 & check.year <= 1) & (check.bp == 0|check.ep == 0) &
           (check.ISSN_1 == 0|check.ISSN_2==0|check.ISSN_3==0|check.ISSN_4==0|check.ISSN_5==0
            |check.ISSN_6==0) &
           check.authors <= 2)
NORWAYdataMINI.v2 <- NORWAYdataMINI.v2 %>% 
  mutate(
    rule2.ID = ifelse(NORWAYdataMINI.v2$VARBEIDLOPENR %in% test.rule2$VARBEIDLOPENR, "YES", "NO") 
  )

#rule 3 rule3.ID == "YES"
test.rule3 <- NORWAYdataMINI.v2 %>% 
  filter(jaccard >= 0.65 & (check.year >= -2 & check.year <= 1) &
           (check.ISSN_1 == 0|check.ISSN_2==0|check.ISSN_3==0|check.ISSN_4==0|check.ISSN_5==0
            |check.ISSN_6==0))
NORWAYdataMINI.v2 <- NORWAYdataMINI.v2 %>% 
  mutate(
    rule3.ID = ifelse(NORWAYdataMINI.v2$VARBEIDLOPENR %in% test.rule3$VARBEIDLOPENR, "YES", "NO") 
  )

#manual matching !is.na(WOSID) 
NORWAYdataMINI.v2 <- left_join(NORWAYdataMINI.v2, MANUALPAIRS, by = "VARBEIDLOPENR")

#combine
NORWAYdataMINI.v2$indexed.WOS1 <- ifelse(NORWAYdataMINI.v2$title_matchCHECK == TRUE, TRUE,
                                         ifelse(NORWAYdataMINI.v2$rule1.ID == "YES", TRUE,
                                                ifelse(NORWAYdataMINI.v2$rule2.ID == "YES", TRUE,
                                                       ifelse(NORWAYdataMINI.v2$rule3.ID == "YES", TRUE,
                                                              ifelse(!is.na(NORWAYdataMINI.v2$WOSID), TRUE,
                                                                     FALSE)))))
NORWAYdataMINI.v2$indexed.WOS2 <- ifelse(is.na(NORWAYdataMINI.v2$title_matchCHECK)&NORWAYdataMINI.v2$rule1.ID == "YES", TRUE,
                                         ifelse(NORWAYdataMINI.v2$rule2.ID == "YES", TRUE,
                                                ifelse(NORWAYdataMINI.v2$rule3.ID == "YES", TRUE,
                                                       ifelse(!is.na(NORWAYdataMINI.v2$WOSID), TRUE,
                                                              FALSE))))
NORWAYdataMINI.v2$indexed.WOS_FIN <- ifelse(NORWAYdataMINI.v2$indexed.WOS1 == TRUE|NORWAYdataMINI.v2$indexed.WOS2 == TRUE, TRUE, FALSE)

# Add WOS IDs
NORWAYdataMINI.v2$WOS_UT_01 <- ifelse(NORWAYdataMINI.v2$title_matchCHECK == TRUE, NORWAYdataMINI.v2$title_match, NA)
NORWAYdataMINI.v2$WOS_UT_02 <- ifelse(NORWAYdataMINI.v2$rule1.ID == "YES"|NORWAYdataMINI.v2$rule2.ID == "YES"|NORWAYdataMINI.v2$rule3.ID == "YES",
                                      NORWAYdataMINI.v2$wos_id, NA)
NORWAYdataMINI.v2$WOS_UT_03 <- ifelse(!is.na(NORWAYdataMINI.v2$WOSID), NORWAYdataMINI.v2$WOSID, NA)
NORWAYdataMINI.v2$WOS_UT_FINAL <- ifelse(!is.na(NORWAYdataMINI.v2$WOS_UT_01), NORWAYdataMINI.v2$WOS_UT_01,
                                         ifelse(!is.na(NORWAYdataMINI.v2$WOS_UT_02), NORWAYdataMINI.v2$WOS_UT_02,
                                                ifelse(!is.na(NORWAYdataMINI.v2$WOS_UT_03), NORWAYdataMINI.v2$WOS_UT_03,
                                                       NA)))
# Prep WOS data

WOSdata_cleaned <- WOSdata %>% 
  select(WOS_UT_FINAL, WOS_FOSCAT_uncoded_1:WOS_FOSCAT_uncoded_6) %>% 
  gather(WOS_nr, WOS, WOS_FOSCAT_uncoded_1:WOS_FOSCAT_uncoded_6) %>% 
  filter(!is.na(WOS)) %>% 
  distinct(WOS_UT_FINAL, WOS) %>% 
  group_by(WOS_UT_FINAL) %>% 
  mutate(WOS_nr = row_number(),
         WOS = ifelse(WOS == "PSYCHOLOGY, SOCIAL PSYCHOLOGY", "FOS5_1", WOS),
         WOS = str_replace(WOS, "FOS", "FOS_")) %>% 
  ungroup() %>% 
  spread(WOS_nr, WOS)

names(WOSdata_cleaned) <- c("WOS_UT_FINAL", paste0("WOS_",1:6))

# Add WOS classification
NORWAYdata_WOS <- left_join(NORWAYdataMINI.v2, WOSdata_cleaned, by = "WOS_UT_FINAL")
NORWAYdata_WOS <- NORWAYdata_WOS %>% distinct(VARBEIDLOPENR, .keep_all = TRUE)

CRISTINdata.10 <- left_join(CRISTINdata.09, NORWAYdata_WOS, by = "VARBEIDLOPENR")

# Add VABB classification -------------------------------------------------

VABBjournals_FOS <- All_journals %>% # copy the journal classification data set
  select(VABB.FOS1:VABB.FOS5, ISSN1:ISSN4) %>%  
  gather(ISSN_NR, ISSN, ISSN1:ISSN4) %>%  
  filter(!is.na(ISSN)) %>% 
  select(-ISSN_NR) 

CRISTINdata_VABB <- left_join(CRISTIN_ISSNs, VABBjournals_FOS, by = "ISSN") %>% # join on ISSN the article and the journal data set
  filter(!is.na(VABB.FOS1)) %>% 
  distinct(VARBEIDLOPENR, .keep_all = TRUE) %>% # The join generates 7 duplicates. I use the first one
  select(-ISSN)

CRISTINdata.11 <- left_join(CRISTINdata.10, CRISTINdata_VABB, by = "VARBEIDLOPENR") # join on Loi the classification data set to the main VABB data set

# Add ERIH PLUS classification --------------------------------------------

# Clean up ERIH PLUS codes

erih.wip <- erih %>% 
  select(`NSD Journal ID`, erih.OECD = `OECD classifications`) %>% 
  separate(erih.OECD, paste0("erih.oecd",1:14), sep = "; ") %>% 
  gather(d.nr, erih.oecd, erih.oecd1:erih.oecd14) %>% 
  filter(!is.na(erih.oecd)) %>% 
  mutate(
    erih.oecd = as.factor(str_replace(erih.oecd, " \\(.+\\)", ""))
  ) %>% 
  select(-d.nr)

erih.disc <- erih.wip %>% count(erih.oecd)

erih.wip$erih.oecd <- fct_collapse(erih.wip$erih.oecd,
                                   FOS_5_1 = c("Psychology"),
                                   FOS_5_2 = c("Economics and Business"),
                                   FOS_5_3 = c("Educational Sciences"),
                                   FOS_5_4 = c("Sociology"),
                                   FOS_5_5 = c("Law"),
                                   FOS_5_6 = c("Political Science"),
                                   FOS_5_7 = c("Social and Economic Geography"),
                                   FOS_5_8 = c("Media and Communications"),
                                   FOS_5_9 = c("Other Social Sciences"),
                                   FOS_6_1 = c("History and Archaeology"),
                                   FOS_6_2 = c("Languages and Literature"),
                                   FOS_6_3 = c("Philosophy, Ethics and Religion"),
                                   FOS_6_4 = c("Arts"),
                                   FOS_6_5 = c("Other Humanities"))

erih <- left_join(erih, erih.wip, by = "NSD Journal ID")

# Prep erih

erih.ISSNs <- erih %>% 
  select(`NSD Journal ID`, `Print ISSN`, `Online ISSN`, erih.oecd) %>% 
  gather(ISSN_nr, ISSN, `Print ISSN`:`Online ISSN`) %>% 
  filter(!is.na(ISSN)) %>% 
  select(-ISSN_nr) %>% 
  distinct(`NSD Journal ID`, ISSN, erih.oecd)

# Add to CRISTIN data

CRISTIN_ISSNs_erih <- left_join(CRISTIN_ISSNs, erih.ISSNs, by = "ISSN") %>% 
  filter(!is.na(erih.oecd)) %>% 
  distinct(VARBEIDLOPENR, erih.oecd, .keep_all = TRUE) %>% 
  select(VARBEIDLOPENR, erih.oecd, `NSD Journal ID`) %>% 
  group_by(VARBEIDLOPENR) %>% 
  mutate(count.ID = row_number()) %>% 
  ungroup() %>% 
  spread(count.ID, erih.oecd)

names(CRISTIN_ISSNs_erih) <- c("VARBEIDLOPENR", "NSD Journal ID", paste0("erih.oecd", 1:11))

CRISTINdata.12 <- left_join(CRISTINdata.11, CRISTIN_ISSNs_erih, by = "VARBEIDLOPENR")


# Prep data for Jaccard calculation ---------------------------------------

CRISTINdata.jaccard <- CRISTINdata.12 %>% 
  select(VARBEIDLOPENR, NSD.OECD, SM.OECD, VABB.FOS1:VABB.FOS5, WOS_1:WOS_6, erih.oecd1:erih.oecd11)

# Add Jaccard calculation -------------------------------------------------

CRISTINdata.13 <- left_join(CRISTINdata.12, NO_Jaccard, by = "VARBEIDLOPENR")

# Export data -------------------------------------------------------------

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/NORWAY_", currentDate, ".csv")
csvFileName_1 <- paste0("./Output/NORWAY_articles_", currentDate, ".csv")

write_csv(CRISTINdata.13, csvFileName_1, na = "")
write_csv(CRISTINdata.jaccard, csvFileName_1, na = "")