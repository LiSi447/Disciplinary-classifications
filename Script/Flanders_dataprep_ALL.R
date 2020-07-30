
# call packages ----------------------------------------------------------

library(tidyverse)
library(stringdist)

# import data -------------------------------------------------------------

VABBdata <- read_csv2(
  "./Raw data/VABB/170523_overzichtsrapport.csv", # complete VABB dataset
  col_types = cols(.default = "c"))

VABBserialnm <- read_csv2(
  "./Raw data/VABB/vabb7nums_edited24062019.csv", # ISSNs and ISBNs for each Loi
  col_types = cols(.default = "c"))

VABBjournals_classification <- read_csv("./Raw data/VABB/VABBjournalCLAS_08072019.csv", # journals' classification
                                        col_types = cols(.default = "c")) 

All_journals <- read_csv("./Raw data/Alljournals_09072019_CLEANED.csv", # journals' classification
                         col_types = cols(.default = "c"))

VABB_authors<- read_csv("./Raw data/VABB/vabb7authors.csv", # info on authors per Loi
                            col_types = cols(.default = "c"))

SCIENCEMETRIXjournals <- read_csv2("./Raw data/Science-Metrix/sm_journal_classification_106_1_corr.csv",
                                   col_types = cols(.default = "c")) # Science-Metrix classification

SM_crosswalk <- read_csv2("./Raw data/Science-Metrix/SM_OECD_crosswalk.csv",
                          col_types = cols(.default = "c")) # Science-Metrix to OECD FORD cross-walk

nsd_COMPLETE <- read_csv2("./Raw data/CRISTIN/npu_1_journals.csv",
                          col_types = cols(.default = "c")) # Norwegian classification

erih <- read_csv2("./Raw data/ERIH PLUS/2020-04-03 ERIH PLUS approved journals and series.csv")

VABBdata_authors<- read_csv("./Raw data/VABB/vabb7authors.csv",
                            col_types = cols(.default = "c")) # data on VABB authors for each Loi

FLANDERSdataWOS <- read_csv("./Raw data/Web of Science - KUL/Belgium_201807232032.csv",
                            col_types = cols(.default = "c")) # WoS data on Belgium retrieved from the KUL database

WOS_SCIE <- read_csv2("./Raw data/Web of Science - online/SCIE2018.csv",
                      col_types = cols(.default = "c")) # SCIE journal list extracted from pdf
WOS_SSCI <- read_csv2("./Raw data/Web of Science - online/SSCI2018.csv",
                      col_types = cols(.default = "c")) # SSCI journal list extracted from pdf
WOS_AHCI <- read_csv2("./Raw data/Web of Science - online/AHCI2018.csv",
                      col_types = cols(.default = "c")) # AHCI journal list extracted from pdf

FLANDERSpairs <- read_delim("./Raw data/Web of Science - matching/matching-pairs-fullref-topk-256perms.csv",
                            col_types = list("jaccard" = col_double()),
                            delim = ",")

FLANDERS_MANUALPAIRS <- read_csv2("./Raw data/Web of Science - matching/FLANDERS_VABBWOS_MANUALMATCH_15112018.csv",
                                  col_types = cols(.default = "c"))

WOSdata <- read_csv("./Raw data/Web of Science - online/WOSdata_17012019.csv",
                    col_types = cols(.default = "c")) # WoS data retrieved online using WoS ID

# Prep VABB data ----------------------------------------------------------

# check missing values

colSums(is.na(VABBdata))

# limit the timespan to 2006-2015 and select only journals

years <- c(2006:2015)

VABBdata <- VABBdata %>% 
  filter(pubyear %in% years & `VABB-publicatietype` %in% "VABB-1") # VABB-1 stands for journal article

#Add ISSN codes by Loi

VABBserialnm$issnsFIN <- str_replace(VABBserialnm$issnsFIN, "^0$", "") # some missing ISSNs were recorded as 0

VABBserialnm2 <- VABBserialnm %>% 
  select(Loi, issnsFIN) %>% 
  mutate(count.ISSNs = str_count(VABBserialnm$issnsFIN, ";"),
         issnsFIN = str_replace(VABBserialnm$issnsFIN, "^0$", "")) %>% 
  separate(issnsFIN, c("ISSN1", "ISSN2", "ISSN3", "ISSN4", "ISSN5"), sep = ";" ) %>% 
  select(-count.ISSNs) %>% 
  filter(!is.na(ISSN1) & ISSN1 != "")

VABBdata <- left_join(VABBdata, VABBserialnm2, by = "Loi")

VABBdata2 <- VABBdata %>% filter(!is.na(ISSN1)) # remove records with missing ISSNs

# select only peer-reviewed publications

PRstatus_variables <- rev(names(VABBdata)[8:15])

VABBdata2$peer.reviewed <- NA

system.time(for (i in 1:nrow(VABBdata2)) {
  
  for (var in PRstatus_variables) {
    if (!is.na(VABBdata2$peer.reviewed[i])) {
      break
    }
    if (is.na(VABBdata2[var][i,])) {
      next
    }
    if (grepl("1,*", VABBdata2[var][i,])) {
      VABBdata2$peer.reviewed[i] <- "YES"
    } else if (grepl("in WOS \\(bof\\)", VABBdata2[var][i,])) {
      VABBdata2$peer.reviewed[i] <- "YES"
    } else {
      VABBdata2$peer.reviewed[i] <- "NO"
    }
  }
})

VABBdata3 <- VABBdata2 %>%  filter(peer.reviewed == "YES")

# Assign FOS fields using the VABB journal classification (at journal level)

VABBdata_FOS <- VABBdata3 %>% # copy the VABB data set
  select(Loi, ISSN1:ISSN5) %>%  
  gather(ISSN_NR, ISSN, ISSN1:ISSN5) %>% 
  filter(!is.na(ISSN)) 

VABBjournals_FOS <- All_journals %>% # copy the journal classification data set
  select(VABB.FOS1:VABB.FOS5, ISSN1:ISSN4) %>%  
  gather(ISSN_NR, ISSN, ISSN1:ISSN4) %>%  
  filter(!is.na(ISSN)) %>% 
  select(-ISSN_NR) 

VABBdata_FOS2 <- left_join(VABBdata_FOS, VABBjournals_FOS, by = "ISSN") %>% # join on ISSN the article and the journal data set
  filter(!is.na(VABB.FOS1)) %>% 
  distinct(Loi, .keep_all = TRUE) %>% # The join generates more than 2000 duplicates. To check.
  select(-ISSN, -ISSN_NR)

VABBdata4 <- left_join(VABBdata3, VABBdata_FOS2, by = "Loi") # join on Loi the classification data set to the main VABB data set

# Delineate universities

VABBdata5 <- filter(VABBdata4,
                    `lm(vabb-ua)` %in% "Ja" |
                    `lm(vabb-kul)` %in% "Ja" |
                    `lm(vabb-ug)` %in% "Ja" |
                    `lm(vabb-uh)` %in% "Ja" |
                    `lm(vabb-vub)` %in% "Ja")

#Calculate fractionalise count

VABB_authors2 <- VABB_authors %>% 
  select(-editors) %>% 
  filter(!is.na(authors)) %>% 
  separate(authors, paste0("A", 1 : max(str_count(VABB_authors$authors, ";") + 1, na.rm = TRUE)), sep = ";")  %>% 
  gather(Author_NR, Author, A1:A895) %>% 
  filter(!is.na(Author)) %>% 
  mutate(
    FL_author_code = str_extract(Author, "\\[a\\:\\:.+\\]"),
    Author_cleaned = ifelse(!is.na(FL_author_code), FL_author_code, Author),
    Author_type = ifelse(!is.na(FL_author_code), "Flemish", "Other")
  ) %>% 
  distinct(Loi, Author_cleaned, .keep_all = TRUE) %>% 
  select(-Author_NR, -Author, -FL_author_code)

Author_count <- VABB_authors2 %>% 
  group_by(Loi) %>% 
  mutate(Total_n_authors = n()) %>% 
  ungroup()

Flemish_authors <- VABB_authors2 %>% 
  group_by(Loi, Author_type) %>%
  filter(Author_type == "Flemish") %>% 
  summarize (Flemish_n_authors = n()) %>% 
  ungroup() %>% 
  select(-Author_type)

Author_count2 <- left_join(Author_count, Flemish_authors, by = "Loi") %>% 
  mutate(Fract_count = 1 / Total_n_authors * Flemish_n_authors) %>% 
  distinct(Loi, .keep_all = TRUE) %>% 
  select(Loi, Fract_count)

VABBdata6 <- left_join(VABBdata5, Author_count2, by = "Loi")

#Omit records with missing fractionalised count

VABBdata7 <- VABBdata6 %>% filter(!is.na(Fract_count))

#Delineate SSH using organisational classification

org_vars_SSH <- names(VABBdata7)[c(16:29, 31:35)] # select org clas vars that refer to SSH; irc(a::irc.85) is ommitted
notSSH.org <- VABBdata7 %>% filter_at(org_vars_SSH, all_vars(is.na(.)))
VABBdata7$ORG_SSH <- !(VABBdata7$Loi %in% notSSH.org$Loi)

VABBdata8 <- VABBdata7 %>% filter(VABBdata7$ORG_SSH == TRUE)

#Delineate SSH using cognitive classification

cog_vars_SSH.VABB <- paste0("VABB.FOS", 1:5)
cog_vars_SSH.FOS <- c(paste0("FOS_5_", 1:9),
                      paste0("FOS_6_", 1:5),
                      paste0("FOS_6_1_", 1:2),
                      paste0("FOS_6_2_", 1:2),
                      paste0("FOS_6_3_", 1:2))

VABBdata8$VABB.cog.SSH <- NA
for (i in 1:nrow(VABBdata8)) {
  for (var in cog_vars_SSH.VABB) {
    if (VABBdata8$VABB.cog.SSH[i] %in% TRUE) {
      break
    } else {
      VABBdata8$VABB.cog.SSH[i] <- VABBdata8[var][i, ] %in% cog_vars_SSH.FOS
    }
  }
}

VABBdata9 <- VABBdata8 %>%
  filter(VABB.cog.SSH == TRUE)

#Recode VABB.FOS cats

for (var in cog_vars_SSH.VABB) {
  str_replace_all(var, c("FOS_6_1_1", "FOS_6_1_2"), "FOS_6_1")
  str_replace_all(var, c("FOS_6_2_1", "FOS_6_2_2"), "FOS_6_2")
  str_replace_all(var, c("FOS_6_3_1", "FOS_6_3_2"), "FOS_6_3")
}


# Delineate ISSNs and Loi for adding classifications ----------------------

VABB_ISSNs <- VABBdata9 %>% 
  select(Loi, ISSN1, ISSN2, ISSN3, ISSN4, ISSN5) %>% 
  gather(ISSN_nr, ISSN, ISSN1:ISSN5) %>% 
  distinct(Loi, ISSN, .keep_all = TRUE)


# Add Science-Metrix classification ---------------------------------------

SM_ISSNs <- SCIENCEMETRIXjournals %>% 
  gather(ISSN_nr, ISSN, issn:essn) %>% 
  distinct(smsid, ISSN, .keep_all = TRUE) %>% 
  left_join(SM_crosswalk, by = c("Domain_English", "Field_English", "SubField_English")) %>% 
  select(ISSN, Domain_English, SubField_English, OECF_FORD_1) %>% 
  filter(!is.na(ISSN))

names(SM_ISSNs) <- c("ISSN", "SM_TOP", "SM", "SM_OECD")

# ISSNs for the following 7 Loi's appear with 2 different journals  in Science-Metrix classification. I remove these records

LoisRemove <- c("c:vabb:278294", "c:vabb:299627", "c:vabb:317652", "c:vabb:326741", "c:vabb:341235", "c:vabb:345312", "c:vabb:351859")

VABB_SM <- VABB_ISSNs %>% 
  left_join(SM_ISSNs, by = "ISSN") %>% 
  filter(!is.na(SM_OECD)) %>% 
  distinct(Loi, SM_TOP, SM, SM_OECD) %>% 
  filter(!Loi %in% LoisRemove)

VABBdata10 <- left_join(VABBdata9, VABB_SM, by = "Loi")


# Add NSD classification --------------------------------------------------

# Recode NSD values

nsd_COMPLETE$NSD.OECD <- fct_collapse(nsd_COMPLETE$`NPI Fagfelt`,
                                      FOS_1_1 = c("Matematikk"),
                                      FOS_1_2 = c("Datateknikk og datavitenskap", "Informatikk", "Nett og nettverksfunksjonalitet"),
                                      FOS_1_3 = c("Fysikk"),
                                      FOS_1_4 = c("Kjemi"),
                                      FOS_1_5 = c("Geofag", "Anvendt geologi og petroleumsfag"),
                                      FOS_1_6 = c("Biologi"),
                                      FOS_2_0 = c("Bioteknologi", "Generell teknologi", "Tverrfaglig teknologi"), #Bioteknologi does not match this category exactly
                                      FOS_2_1 = c("Bygg og konstruksjonsteknikk"),
                                      FOS_2_2 = c("Elkraft og elektrotekniske fag", "Elektronikk og kybernetikk"),
                                      FOS_2_3 = c("Maskinteknikk", "Energi"),
                                      FOS_2_4 = c("Kjemisk teknologi"),
                                      FOS_2_5 = c("Materialteknologi"),
                                      FOS_2_7 = c("Marin og maritim teknologi", "Miljøteknologi og industriell økologi"),
                                      FOS_3_0 = c("Tverrfaglig naturvitenskap og medisin"),
                                      FOS_3_1 = c("Biomedisin", "Farmasi, farmakologi og toksikologi", "Generell medisin"),
                                      FOS_3_2 = c("Geriatri", "Nevrologi", "Onkologi", "Anestesi, intensiv, akutt", "Gastroenterologi og hepatologi", "Øre-nese-hals", "Psykiatri",
                                                  "Hjerte, kar og luftveier", "Nefrologi", "Radiologi og billeddiagnostikk", "Endokrinologi", "Infeksjoner", "Odontologi",
                                                  "Pediatri", "Dermatologi og venerologi", "Hematologi", "Kirurgiske fag" , "Revmatologi" , "Øyesykdommer" ),
                                      FOS_3_3 = c("Samfunnsmedisin", "Idrettsforskning", "Sykepleie"),
                                      FOS_4_3 = c("Veterinærmedisin"),
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

# Above includes only those levels that are identified in VABB data

NSD_mini <- nsd_COMPLETE %>% 
  select(ISSN1 = `Print ISSN`, ISSN2 = `Online ISSN`, NSD.OECD) %>% 
  gather(ISSN_nr, ISSN, ISSN1:ISSN2) %>% 
  filter(!is.na(ISSN)) %>% 
  select(-ISSN_nr)

VABB_ISSNs_NSD <- left_join(VABB_ISSNs, NSD_mini, by = "ISSN") %>% filter(!is.na(NSD.OECD) & NSD.OECD != "Ikke tildelt") %>% 
  select(Loi, NSD.OECD) %>% distinct(Loi, .keep_all = TRUE) # removes 20 records %>%

# A number of ISSNs assigned to a single VABB record appear with multiple journals in NSD (that are classified differently)
# This applies to 20 records;  I use the first classification that is picked up automatically

VABBdata11 <- left_join(VABBdata10, VABB_ISSNs_NSD, by = "Loi") %>% distinct(Loi, .keep_all = TRUE)


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

# Add to VABB data

VABB_ISSNs_erih <- left_join(VABB_ISSNs, erih.ISSNs, by = "ISSN") %>% 
  filter(!is.na(erih.oecd)) %>% 
  distinct(Loi, erih.oecd, .keep_all = TRUE) %>% 
  select(Loi, erih.oecd, `NSD Journal ID`) %>% 
  group_by(Loi) %>% 
  mutate(count.Loi = row_number()) %>% 
  ungroup() %>% 
  spread(count.Loi, erih.oecd)

names(VABB_ISSNs_erih) <- c("Loi", "NSD Journal ID", paste0("erih.oecd", 1:13))

VABBdata12 <- left_join(VABBdata11, VABB_ISSNs_erih, by = "Loi")

# Add WoS classification --------------------------------------------------

FLANDERSdata <- VABBdata12

# Prep VABB data
FLANDERSdata <- FLANDERSdata %>% 
  mutate(
    bp_CLEANED = as.integer(str_replace_all(FLANDERSdata$bp, "[:alpha:]|[:punct:]|[:space:]", "")),
    ep_CLEANED = as.integer(str_replace_all(FLANDERSdata$ep, "[:alpha:]|[:punct:]|[:space:]", "")),
    title_CLEANED = tolower(str_replace_all(FLANDERSdata$ctitle, "[:punct:]", "")),
    author_list = VABBdata_authors$authors[match(FLANDERSdata$Loi, VABBdata_authors$Loi)],
    first.author = str_replace(str_extract(author_list, "[:graph:]*"), ",$" , "")
  )

# Prep WOS data
FLANDERSdataWOS <- FLANDERSdataWOS %>% 
  mutate(
    bp_CLEANED = as.integer(str_replace_all(FLANDERSdataWOS$BEGINPAGINA, "[:alpha:]|[:punct:]|[:space:]", "")),
    ep_CLEANED = as.integer(str_replace_all(FLANDERSdataWOS$EINDPAGINA, "[:alpha:]|[:punct:]|[:space:]", "")),
    title_CLEANED = tolower(str_replace_all(FLANDERSdataWOS$TITEL, "[:punct:]", "")),
    first.author = str_replace(str_extract(FLANDERSdataWOS$AUTEUR, "[:graph:]*"), ",$", "")
  )

# Add additional ISSNs from WoS journal lists in WOS data
FLANDERSdataWOS$print.ISSN_1 <- ifelse(FLANDERSdataWOS$ISSN %in% WOS_SCIE$ISSN, WOS_SCIE$ISSN[match(FLANDERSdataWOS$ISSN, WOS_SCIE$ISSN)], NA)
FLANDERSdataWOS$print.ISSN_2 <- ifelse(FLANDERSdataWOS$ISSN %in% WOS_SSCI$ISSN, WOS_SSCI$ISSN[match(FLANDERSdataWOS$ISSN, WOS_SSCI$ISSN)], NA)
FLANDERSdataWOS$print.ISSN_3 <- ifelse(FLANDERSdataWOS$ISSN %in% WOS_AHCI$ISSN, WOS_AHCI$ISSN[match(FLANDERSdataWOS$ISSN, WOS_AHCI$ISSN)], NA)
FLANDERSdataWOS$print.ISSN <- ifelse(!is.na(FLANDERSdataWOS$print.ISSN_1), paste(FLANDERSdataWOS$print.ISSN_1),
                                     ifelse(!is.na(FLANDERSdataWOS$print.ISSN_2), paste(FLANDERSdataWOS$print.ISSN_2),
                                            ifelse(!is.na(FLANDERSdataWOS$print.ISSN_3), paste(FLANDERSdataWOS$print.ISSN_3),
                                                   NA)))
FLANDERSdataWOS$online.ISSN_1 <- ifelse(FLANDERSdataWOS$ISSN %in% WOS_SCIE$`E-ISSN`, WOS_SCIE$`E-ISSN`[match(FLANDERSdataWOS$ISSN, WOS_SCIE$`E-ISSN`)], NA)
FLANDERSdataWOS$online.ISSN_2 <- ifelse(FLANDERSdataWOS$ISSN %in% WOS_SSCI$`E-ISSN`, WOS_SSCI$`E-ISSN`[match(FLANDERSdataWOS$ISSN, WOS_SSCI$`E-ISSN`)], NA)
FLANDERSdataWOS$online.ISSN_3 <- ifelse(FLANDERSdataWOS$ISSN %in% WOS_AHCI$`E-ISSN`, WOS_AHCI$`E-ISSN`[match(FLANDERSdataWOS$ISSN, WOS_AHCI$`E-ISSN`)], NA)
FLANDERSdataWOS$online.ISSN <- ifelse(!is.na(FLANDERSdataWOS$online.ISSN_1), paste(FLANDERSdataWOS$online.ISSN_1),
                                      ifelse(!is.na(FLANDERSdataWOS$online.ISSN_2), paste(FLANDERSdataWOS$online.ISSN_2),
                                             ifelse(!is.na(FLANDERSdataWOS$online.ISSN_3), paste(FLANDERSdataWOS$online.ISSN_3),
                                                    NA)))

#create a smaller VABB file

FLANDERSdataMINI <- FLANDERSdata %>% 
  select("Loi", "ctitle", "title_CLEANED", "bp","ep", "bp_CLEANED", "ep_CLEANED", "pubyear","peer.reviewed", "jtitle", "ISSN1","ISSN2","ISSN3", "ISSN4","isi","lg", "first.author")
# column 'wosID' was missing in this dataset.

# Add the file with LSH results
names(FLANDERSpairs)[1] <- "Loi"
FLANDERSdataMINI.v2 <- left_join(FLANDERSdataMINI, FLANDERSpairs, by = "Loi")

# Adjust variable types
FLANDERSdataMINI.v2$pubyear <- as.integer(FLANDERSdataMINI.v2$pubyear)
FLANDERSdataWOS$PUBJAAR <- as.integer(FLANDERSdataWOS$PUBJAAR)
FLANDERSdataMINI.v2$bp_CLEANED <- as.integer(FLANDERSdataMINI.v2$bp_CLEANED)
FLANDERSdataWOS$bp_CLEANED <- as.integer(FLANDERSdataWOS$bp_CLEANED)
FLANDERSdataMINI.v2$ep_CLEANED <- as.integer(FLANDERSdataMINI.v2$ep_CLEANED)
FLANDERSdataWOS$ep_CLEANED <- as.integer(FLANDERSdataWOS$ep_CLEANED)

# Check for similarity
FLANDERSdataMINI.v2 <- FLANDERSdataMINI.v2 %>% 
  mutate(
    check.year = ifelse(!is.na(FLANDERSdataMINI.v2$pubyear),
                        FLANDERSdataMINI.v2$pubyear - FLANDERSdataWOS$PUBJAAR[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                        NA),
    check.bp = ifelse(!is.na(FLANDERSdataMINI.v2$bp_CLEANED),
                      FLANDERSdataMINI.v2$bp_CLEANED - FLANDERSdataWOS$bp_CLEANED[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                      NA),
    check.ep = ifelse(!is.na(FLANDERSdataMINI.v2$ep_CLEANED),
                      FLANDERSdataMINI.v2$ep_CLEANED - FLANDERSdataWOS$ep_CLEANED[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                      NA),
    check.authors = ifelse(!is.na(FLANDERSdataMINI.v2$first.author),
                           stringdist(FLANDERSdataMINI.v2$first.author, FLANDERSdataWOS$first.author[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                                      method = "lv"),
                           NA),
    check.ISSN_1 = ifelse(!is.na(FLANDERSdataMINI.v2$ISSN1),
                          stringdist(FLANDERSdataMINI.v2$ISSN1,
                                     FLANDERSdataWOS$ISSN[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_2 = ifelse(!is.na(FLANDERSdataMINI.v2$ISSN1),
                          stringdist(FLANDERSdataMINI.v2$ISSN1,
                                     FLANDERSdataWOS$print.ISSN[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_3 = ifelse(!is.na(FLANDERSdataMINI.v2$ISSN1),
                          stringdist(FLANDERSdataMINI.v2$ISSN1,
                                     FLANDERSdataWOS$online.ISSN[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_4 = ifelse(!is.na(FLANDERSdataMINI.v2$ISSN2),
                          stringdist(FLANDERSdataMINI.v2$ISSN2,
                                     FLANDERSdataWOS$ISSN[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_5 = ifelse(!is.na(FLANDERSdataMINI.v2$ISSN2),
                          stringdist(FLANDERSdataMINI.v2$ISSN2,
                                     FLANDERSdataWOS$print.ISSN[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_6 = ifelse(!is.na(FLANDERSdataMINI.v2$ISSN2),
                          stringdist(FLANDERSdataMINI.v2$ISSN2,
                                     FLANDERSdataWOS$online.ISSN[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_7 = ifelse(!is.na(FLANDERSdataMINI.v2$ISSN3),
                          stringdist(FLANDERSdataMINI.v2$ISSN3,
                                     FLANDERSdataWOS$ISSN[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_8 = ifelse(!is.na(FLANDERSdataMINI.v2$ISSN3),
                          stringdist(FLANDERSdataMINI.v2$ISSN3,
                                     FLANDERSdataWOS$print.ISSN[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_9 = ifelse(!is.na(FLANDERSdataMINI.v2$ISSN3),
                          stringdist(FLANDERSdataMINI.v2$ISSN3,
                                     FLANDERSdataWOS$online.ISSN[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                                     method = "lv"),
                          NA),
    check.ISSN_10 = ifelse(!is.na(FLANDERSdataMINI.v2$ISSN4),
                           stringdist(FLANDERSdataMINI.v2$ISSN4,
                                      FLANDERSdataWOS$ISSN[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                                      method = "lv"),
                           NA),
    check.ISSN_11 = ifelse(!is.na(FLANDERSdataMINI.v2$ISSN4),
                           stringdist(FLANDERSdataMINI.v2$ISSN4,
                                      FLANDERSdataWOS$print.ISSN[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                                      method = "lv"),
                           NA),
    check.ISSN_12 = ifelse(!is.na(FLANDERSdataMINI.v2$ISSN4),
                           stringdist(FLANDERSdataMINI.v2$ISSN4,
                                      FLANDERSdataWOS$online.ISSN[match(FLANDERSdataMINI.v2$wos_id, FLANDERSdataWOS$PUBID)],
                                      method = "lv"),
                           NA)
  )
#
#(1) exact matches ## title_matchCHECK == TRUE
FLANDERSdataMINI.v2$title_match <- FLANDERSdataWOS$PUBID[match(FLANDERSdataMINI.v2$title_CLEANED, FLANDERSdataWOS$title_CLEANED)] # this retrieves WoS ID using exact match of a title

FLANDERSdataMINI.v2$title_matchCHECK <- ifelse(!is.na(FLANDERSdataMINI.v2$title_match), 
                                               ifelse(FLANDERSdataMINI.v2$pubyear == FLANDERSdataWOS$PUBJAAR[match(FLANDERSdataMINI.v2$title_match, FLANDERSdataWOS$PUBID)],
                                                      ifelse(FLANDERSdataMINI.v2$bp_CLEANED == FLANDERSdataWOS$bp_CLEANED[match(FLANDERSdataMINI.v2$title_match, FLANDERSdataWOS$PUBID)],
                                                             ifelse(FLANDERSdataMINI.v2$ep_CLEANED == FLANDERSdataWOS$ep_CLEANED[match(FLANDERSdataMINI.v2$title_match, FLANDERSdataWOS$PUBID)],
                                                                    TRUE, FALSE),
                                                             FALSE),
                                                      FALSE),
                                               NA) # this validates the matched title with year and page numbers

#(2) LSH rule 1 ## rule1.ID == "YES"
test.rule1 <- FLANDERSdataMINI.v2 %>% 
  filter(jaccard >= 0.75 & (check.year >= -2 & check.year <= 1) & (check.bp == 0|check.ep == 0))

FLANDERSdataMINI.v2 <- FLANDERSdataMINI.v2 %>% 
  mutate(
    rule1.ID = ifelse(FLANDERSdataMINI.v2$Loi %in% test.rule1$Loi, "YES", "NO") 
  )

#(3) LSH rule 2 rule2.ID == "YES"
test.rule2 <- FLANDERSdataMINI.v2 %>% 
  filter(jaccard >= 0.30 & (check.year >= -2 & check.year <= 1) & (check.bp == 0|check.ep == 0) &
           (check.ISSN_1 == 0|check.ISSN_2==0|check.ISSN_3==0|check.ISSN_4==0|check.ISSN_5==0
            |check.ISSN_6==0|check.ISSN_7==0|check.ISSN_8==0|check.ISSN_9==0|check.ISSN_10==0|check.ISSN_11==0|
              check.ISSN_12==0) &
           check.authors <= 2)

FLANDERSdataMINI.v2 <- FLANDERSdataMINI.v2 %>% 
  mutate(
    rule2.ID = ifelse(FLANDERSdataMINI.v2$Loi %in% test.rule2$Loi, "YES", "NO") 
  )

#(4) LSH rule 3 rule3.ID == "YES"
test.rule3 <- FLANDERSdataMINI.v2 %>% 
  filter(jaccard >= 0.65 & (check.year >= -2 & check.year <= 1) &
           (check.ISSN_1 == 0|check.ISSN_2==0|check.ISSN_3==0|check.ISSN_4==0|check.ISSN_5==0
            |check.ISSN_6==0|check.ISSN_7==0|check.ISSN_8==0|check.ISSN_9==0|check.ISSN_10==0|check.ISSN_11==0|
              check.ISSN_12==0))

FLANDERSdataMINI.v2 <- FLANDERSdataMINI.v2 %>% 
  mutate(
    rule3.ID = ifelse(FLANDERSdataMINI.v2$Loi %in% test.rule3$Loi, "YES", "NO") 
  )

#(5) manual matching manual.match == "YES"
FLANDERSdataMINI.v2 <- left_join(FLANDERSdataMINI.v2, FLANDERS_MANUALPAIRS, by = "Loi")
FLANDERSdataMINI.v2 <- FLANDERSdataMINI.v2 %>% 
  mutate(
    manual.match = ifelse(!is.na(PUBID), "YES", "NO")
  )

#COMBINE MATCHED RESULTS
FLANDERSdataMINI.v2$indexed.WOS1 <- ifelse(FLANDERSdataMINI.v2$title_matchCHECK == TRUE, TRUE,
                                           ifelse(FLANDERSdataMINI.v2$rule1.ID == "YES", TRUE,
                                                  ifelse(FLANDERSdataMINI.v2$rule2.ID == "YES", TRUE,
                                                         ifelse(FLANDERSdataMINI.v2$rule3.ID == "YES", TRUE,
                                                                ifelse(FLANDERSdataMINI.v2$manual.match == "YES", TRUE,
                                                                       FALSE)))))

FLANDERSdataMINI.v2$indexed.WOS2 <- ifelse(is.na(FLANDERSdataMINI.v2$title_matchCHECK)&FLANDERSdataMINI.v2$rule1.ID == "YES", TRUE,
                                           ifelse(FLANDERSdataMINI.v2$rule2.ID == "YES", TRUE,
                                                  ifelse(FLANDERSdataMINI.v2$rule3.ID == "YES", TRUE,
                                                         ifelse(FLANDERSdataMINI.v2$manual.match == "YES", TRUE,
                                                                FALSE))))

FLANDERSdataMINI.v2$indexed.WOS_FIN <- ifelse(FLANDERSdataMINI.v2$indexed.WOS1 == TRUE|FLANDERSdataMINI.v2$indexed.WOS2 == TRUE, TRUE, FALSE)

#
FLANDERSdataMINI.v2$WOS_UT_01 <- ifelse(FLANDERSdataMINI.v2$title_matchCHECK == TRUE, FLANDERSdataMINI.v2$title_match, NA)
FLANDERSdataMINI.v2$WOS_UT_02 <- ifelse(FLANDERSdataMINI.v2$rule1.ID == "YES"|FLANDERSdataMINI.v2$rule2.ID == "YES"|FLANDERSdataMINI.v2$rule3.ID == "YES",
                                        FLANDERSdataMINI.v2$wos_id, NA)
FLANDERSdataMINI.v2$WOS_UT_03 <- ifelse(FLANDERSdataMINI.v2$manual.match == "YES", FLANDERSdataMINI.v2$PUBID, NA)
FLANDERSdataMINI.v2$WOS_UT_FINAL <- ifelse(!is.na(FLANDERSdataMINI.v2$WOS_UT_01), FLANDERSdataMINI.v2$WOS_UT_01,
                                           ifelse(!is.na(FLANDERSdataMINI.v2$WOS_UT_02), FLANDERSdataMINI.v2$WOS_UT_02,
                                                  ifelse(!is.na(FLANDERSdataMINI.v2$WOS_UT_03), FLANDERSdataMINI.v2$WOS_UT_03,
                                                         NA)))

#                                                         
FLANDERSdata_WOS <- left_join(FLANDERSdataMINI.v2, WOSdata, by = "WOS_UT_FINAL") %>% distinct(Loi, .keep_all = TRUE)

VABBdata13 <- left_join(VABBdata12, FLANDERSdata_WOS, by = "Loi")