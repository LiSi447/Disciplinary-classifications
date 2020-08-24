# Call packages -----------------------------------------------------------

library(tidyverse)

# Import data -------------------------------------------------------------

VABBdata <- read_csv("./Output/FLANDERS_2020-08-03.csv", col_types = cols(.default = "c"))
CRISTINdata <- read_csv("./Output/NORWAY_2020-08-04.csv", col_types = cols(.default = "c"))

VABBjournals_classification <- read_csv("./Raw data/VABB/VABBjournalCLAS_08072019.csv", 
                                        col_types = cols(.default = "c")) #VABB journals
nsd_COMPLETE <- read_csv2("./Raw data/CRISTIN/npu_1_journals.csv",
                          col_types = cols(.default = "c"))#NSD journals
SCIENCEMETRIXjournals <- read_csv2("./Raw data/Science-Metrix/sm_journal_classification_106_1_corr.csv",
                                   col_types = cols(.default = "c")) # Science-Metrix classification
SM_crosswalk <- read_csv2("./Raw data/Science-Metrix/SM_OECD_crosswalk.csv",
                          col_types = cols(.default = "c")) # Science-Metrix to OECD FORD cross-walk
WOS_SCIE <- read_csv2("./Raw data/Web of Science - online/SCIE2018.csv",
                      col_types = cols(.default = "c")) # SCIE journal list extracted from pdf
WOS_SSCI <- read_csv2("./Raw data/Web of Science - online/SSCI2018.csv",
                      col_types = cols(.default = "c")) # SSCI journal list extracted from pdf
WOS_AHCI <- read_csv2("./Raw data/Web of Science - online/AHCI2018.csv",
                      col_types = cols(.default = "c")) # AHCI journal list extracted from pdf
WOSdata <- read_csv("./Raw data/Web of Science - online/WOSdata_18042019.csv",
                    col_types = cols(.default = "c")) # WoS data retrieved online using WoS ID

erih <- read_csv2("./Raw data/ERIH PLUS/2020-04-03 ERIH PLUS approved journals and series.csv", col_types = cols(.default = "c")) #ERIH PLUS

jaccard <- read_csv("./Raw data/Jaccard calculation/journals_jaccard_20200819.csv", col_types = cols(.default = "c")) # Jaccard

journaltitles.cleaned <- readxl::read_excel("./Raw data/Supplememtary material _ journal classifications combined 20191113.xlsx",
           sheet = "Sheet1",
           col_types = "text")

journalIDs.cleaned.VABB <- readxl::read_excel("./Manually cleaned data/JournalIDs.xlsx",
                                                sheet = "Corrections-1",
                                                col_types = "text")

journalIDs.cleaned.duplicates <- readxl::read_excel("./Manually cleaned data/JournalIDs.xlsx",
                                              sheet = "Duplicate journal IDs",
                                              col_types = "text")
# Identify unique VABB journals from VABB data  -----------------------------------------------------

# Create a Loi-ISSN dataset
VABBdata_ISSNs <- VABBdata %>% 
  select(Loi, ISSN1.x:ISSN4.x, ISSN5) %>% 
  gather(ISSN_NR, ISSN, ISSN1.x:ISSN5) %>% 
  filter(!is.na(ISSN)) %>% 
  distinct(Loi, ISSN)

# Create VABB journal id-ISSN dataset

VABBjournals_ISSNs <- VABBjournals_classification %>% 
  select(vabbJID, Nummer, `Alternatief ISSN`, ISSN1:ISSN5) %>% 
  gather(ISSN_NR, ISSN, Nummer:ISSN5) %>% 
  filter(!is.na(ISSN)) %>% 
  distinct(vabbJID, ISSN)

# Add journal ID to Loi

VABBdata_Loi.journalID <- VABBdata_ISSNs %>% 
  left_join(VABBjournals_ISSNs, by = "ISSN") %>% 
  distinct(Loi, vabbJID) %>% 
  group_by(Loi) %>% 
  mutate(count.Loi = row_number()) %>% 
  ungroup()

# One Loi with multiple journalIDs (applies to 206 Loi's)

temp <- VABBdata_Loi.journalID %>% 
  filter(count.Loi > 1)

multiple.VABBjournalIDs <- VABBdata_Loi.journalID %>% 
  filter(Loi %in% temp$Loi)  #412 Lois / 90 journal IDs to check

# Export for cleaning in  MS Excel (JournalIDs.xlsx)
#write_csv(multiple.VABBjournalIDs, here::here("Output/LoiswithMultipleJournalIDs_2020-08-13.csv"), na = "")

# Identify unique VABB journals from VABB data

unique.VABBjournals <- VABBdata_Loi.journalID %>% 
  distinct(vabbJID) %>% 
  left_join(VABBjournals_ISSNs, by = "vabbJID")

# Result: 5632 journals (includes potential duplicates)

# Identify unique NSD journals from CRISTIN data ---------------------------

# Create an publication ID-ISSN dataset

CRISTINdata_ISSNs <- CRISTINdata %>% 
  select(VARBEIDLOPENR, ISSN.x:ISSN_ELEKTRONISK.x) %>% 
  gather(ISSN_NR, ISSN, ISSN.x:ISSN_ELEKTRONISK.x) %>% 
  filter(!is.na(ISSN)) %>% 
  distinct(VARBEIDLOPENR, ISSN)

# Create an journal ID-ISSN dataset

NSDjournals_ISSNs <- nsd_COMPLETE %>% 
  select(tidsskrift_id, `Print ISSN`, `Online ISSN`) %>% 
  gather(ISSN_NR, ISSN, `Print ISSN`:`Online ISSN`) %>% 
  filter(!is.na(ISSN)) %>% 
  distinct(tidsskrift_id, ISSN)

# Add journal IDs to publication IDs and check for multiple journal IDs with a single publication ID

CRISTINdata.ID.journalID <- CRISTINdata_ISSNs %>% 
  left_join(NSDjournals_ISSNs, by = "ISSN") %>% 
  filter(!is.na(tidsskrift_id)) %>% 
  distinct(VARBEIDLOPENR, tidsskrift_id, .keep_all = TRUE) %>% 
  group_by(VARBEIDLOPENR) %>% 
  mutate(count.Loi = row_number()) %>% # just 1 journal for each publication ID
  ungroup() %>% 
  select(-count.Loi)

# Identify unique NSD journals from CRISTIN data

unique.NSDjournals <- CRISTINdata.ID.journalID %>% 
  distinct(tidsskrift_id) %>% 
  left_join(NSDjournals_ISSNs, by = "tidsskrift_id")

# Combine NSD and VABB journals ---------------------------------

all.journals.A <- full_join(unique.VABBjournals, unique.NSDjournals, by = "ISSN") %>% 
  distinct(tidsskrift_id, vabbJID) %>%
  filter(!is.na(tidsskrift_id) & !is.na(vabbJID)) %>% 
  mutate(journal.ID = paste("::j::", sprintf('%0.5d', 1:2301), sep="")) %>% 
  filter(!is.na(journal.ID))

all.journals.B <- full_join(unique.VABBjournals, unique.NSDjournals, by = "ISSN") %>% 
  distinct(tidsskrift_id, vabbJID) %>%
  filter(!tidsskrift_id %in% all.journals.A$tidsskrift_id & !vabbJID %in% all.journals.A$vabbJID) %>% 
  mutate(journal.ID = paste("::j::", sprintf('%0.5d', 2302:8554), sep=""))

all.journals <- rbind(all.journals.A, all.journals.B) %>% 
  distinct(tidsskrift_id, vabbJID, journal.ID) %>% 
  left_join(VABBjournals_ISSNs, by = c("vabbJID")) %>% 
  left_join(NSDjournals_ISSNs, by = c("tidsskrift_id", "ISSN")) %>% 
  mutate(ISSN = ifelse(is.na(ISSN), NSDjournals_ISSNs$ISSN[match(tidsskrift_id, NSDjournals_ISSNs$tidsskrift_id)], ISSN))

# Add local classifications and titles -----------------------------------------------

VABBjournals_codes <- VABBjournals_classification %>% 
  select(vabbJID, Title.VABB = Omschrijving, FOS1:FOS5)

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

NSDjournals_codes <- nsd_COMPLETE %>%
  select(tidsskrift_id, NSD.OECD, Title.NSD = `Original tittel`)

# Add VABB and NSD classification

all.journals2 <- all.journals %>% 
  left_join(VABBjournals_codes, by = "vabbJID") %>% 
  left_join(NSDjournals_codes, by = "tidsskrift_id")

# check for missing values

all.journals.VABB <- all.journals2 %>% 
  filter(!is.na(vabbJID)) %>% 
  filter(is.na(FOS1)) # 48 journals have missing values

all.journals.NSD <- all.journals2 %>% 
  filter(!is.na(tidsskrift_id)) %>% 
  filter(is.na(NSD.OECD)) # none missing

# Add Science-Metrix ---------------------------------------------

SM_issns <- SCIENCEMETRIXjournals %>% 
  left_join(SM_crosswalk, by = c("Domain_English", "Field_English","SubField_English")) %>% 
  select(issn, essn, smsid) %>%
  gather(ISSN_nr, ISSN, issn:essn) %>% 
  select(-ISSN_nr) %>% 
  filter(!is.na(ISSN)) %>% 
  distinct(ISSN, smsid) 

SM_codes <- SCIENCEMETRIXjournals %>% 
  left_join(SM_crosswalk, by = c("Domain_English", "Field_English","SubField_English")) %>% 
  select(smsid, Title.SM = Source_title, sm.OECD = OECF_FORD_1)

all.journals3 <- all.journals2 %>% 
  left_join(SM_issns, by = "ISSN") %>% 
  left_join(SM_codes, by = "smsid")

# Check for missing values

all.journals.SM <- all.journals3 %>% 
  filter(!is.na(smsid)) %>% 
  filter(is.na(sm.OECD)) # none missing
  
# Add Web of Science ---------------------------------------------

# Create an WC and ISSN overview and clean up the WOS categories

WOS.journals <- WOSdata %>% 
  select(SN, EI, Title.WOS = SO, FoS_1A:FoS_6A) %>% 
  gather(ISSN_nr, ISSN, SN:EI) %>% 
  filter(!is.na(ISSN) & !is.na(FoS_1A)) %>% 
  select(-ISSN_nr) %>% 
  distinct(ISSN, FoS_1A, .keep_all = TRUE) %>%
  gather(WOS_cat, WOS, FoS_1A:FoS_6A) %>% 
  filter(!is.na(WOS)) %>% 
  distinct(ISSN, WOS, .keep_all = TRUE) %>% 
  select(-WOS_cat) %>% 
  group_by(ISSN) %>% 
  mutate(
    count.ISSN = row_number()
  ) %>% 
  ungroup() %>% 
  spread(count.ISSN, WOS)

names(WOS.journals)[3:8] <- c(paste0("FoS_",1:6,"A"))

WOS.journals$FoS_2A <- str_replace_all(WOS.journals$FoS_2A, "PSYCHOLOGY, SOCIAL PSYCHOLOGY", "FOS_5_1")

# Join

all.journals4 <- all.journals3 %>% 
  left_join(WOS.journals, by = "ISSN")

# check for  ISSNs with multiple IDs

multiple.ISSNs <- all.journals4 %>% 
  group_by(ISSN) %>% 
  mutate(count.ISSN = row_number()) %>% 
  ungroup() %>% 
  filter(count.ISSN > 1 & !is.na(ISSN))

multiple.ISSNs.ALL <- all.journals4 %>% 
  filter(ISSN %in% multiple.ISSNs$ISSN)

#63 ISSNs identified. To clean at the end.

# Add ERIH PLUS -----------------------------------------------------------

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
  select(`NSD Journal ID`, `Print ISSN`, `Online ISSN`, Title.erih = `Original Title`) %>% 
  gather(ISSN_nr, ISSN, `Print ISSN`:`Online ISSN`) %>% 
  filter(!is.na(ISSN)) %>% 
  select(-ISSN_nr) %>% 
  distinct(`NSD Journal ID`, ISSN, .keep_all = TRUE)

erih.journals <- erih.wip %>% 
  distinct(`NSD Journal ID`, erih.oecd) %>% 
  group_by(`NSD Journal ID`) %>% 
  mutate(count.cats = row_number()) %>% 
  ungroup() %>% 
  spread(count.cats, erih.oecd) %>% 
  left_join(erih.ISSNs, by = "NSD Journal ID")

names(erih.journals)[2:15] <- c(paste0("ERIH_",1:14))

all.journals5 <- all.journals4 %>% 
  left_join(erih.journals, by = "ISSN")

# Transform to wide -------------------------------------------------------

all.journals.wide <- all.journals5 %>% 
  group_by(journal.ID) %>% 
  fill(c(vabbJID:tidsskrift_id, Title.VABB:Title.erih), .direction = "downup") %>% 
  mutate(count.ID = row_number()) %>% 
  ungroup() %>% 
  spread(count.ID, ISSN)

# Clean up the dataset

duplicate.IDs <- all.journals.wide %>% group_by(journal.ID) %>% count() %>% filter(n > 1)

to.clean <- all.journals.wide %>% 
  filter(journal.ID %in% duplicate.IDs$journal.ID) # 52 records

# Change the order of variables
all.journals.wide <- all.journals.wide %>% 
  select(journal.ID, vabb.ID = vabbJID, cristin.ID = tidsskrift_id, sm.ID = smsid, erih.id = `NSD Journal ID`,
         Title.VABB, Title.NSD, Title.WOS, Title.SM, Title.erih,
         VABB_1 = FOS1, VABB_2 = FOS2, VABB_3 = FOS3, VABB_4 = FOS4, VABB_5 = FOS5,
         NPU = NSD.OECD,
         SM = sm.OECD,
         WOS_1 = FoS_1A, WOS_2 = FoS_2A, WOS_3 = FoS_3A, WOS_4 = FoS_4A, WOS_5 = FoS_5A, WOS_6 = FoS_6A,
         ERIH_1:ERIH_14,
         ISSN_1 = `1`, ISSN_2 = `2`, ISSN_3 = `3`, ISSN_4 = `4`) 

# Add Jaccard calculations ------------------------------------------------

jaccard.min <- jaccard %>% 
  select(journal.ID, jaccard_vabb_wos:jaccard_vabb_npu, jaccard_vabb_erih:jaccard_npu_erih)

all.journals.wide2 <- all.journals.wide %>% 
  left_join(jaccard.min, by = "journal.ID")

test <- all.journals.wide2 %>% 
  count(journal.ID) %>% 
  filter(n > 1) # 25 journal IDs that have to be checked

# Clean the journal titles ------------------------------------------------

# Create an ISSN - journal ID overview for all journals
journalIDs.ISSNs <- all.journals.wide2 %>% 
  select(journal.ID, ISSN_1:ISSN_2) %>% 
  gather(ISSN_nr, ISSN, ISSN_1:ISSN_2) %>% 
  filter(!is.na(ISSN)) %>% 
  select(-ISSN_nr)

# Create a cleaned title and ISSN overview

titles.ISSNs <- journaltitles.cleaned %>% 
  select(TITLE, ISSN_1:ISSN_4) %>% 
  gather(ISSN_nr, ISSN, ISSN_1:ISSN_4) %>% 
  filter(!is.na(ISSN)) %>% 
  select(-ISSN_nr)

# Combine titles with journal IDs

titles.journalIDs <- left_join(journalIDs.ISSNs, titles.ISSNs, by = "ISSN") %>% 
  distinct(journal.ID, TITLE) %>% 
  filter(!is.na(TITLE) & !is.na(journal.ID))

# Add the cleaned titles

all.journals.wide3 <- all.journals.wide2 %>% 
  left_join(titles.journalIDs, by = "journal.ID") %>%
  mutate(TITLE = ifelse(is.na(TITLE), Title.NSD, TITLE))

missing.titles <- all.journals.wide3 %>% filter(is.na(TITLE)) # 3 journals have a missing cleaned title
  
# Clean the category names ------------------------------------------------

cats.vabb.sm <- names(all.journals.wide3)[c(11:15, 17)]
cats.other <- names(all.journals.wide3)[c(16, 18:37)]

for (cat in cats.vabb.sm) { # add sm
  for (i in 1:nrow(all.journals.wide3)) {
    if ( (!is.na(all.journals.wide3[cat][i,]))) {
            all.journals.wide3[cat][i,] <- paste0("FORD_", all.journals.wide3[cat][i,])
    }
  }
}

all.journals.wide3 <- all.journals.wide3 %>% 
  mutate_at(vars(all_of(cats.vabb.sm)),
    funs(case_when(
      . %in% c("FORD_6_1_1", "FORD_6_1_2") ~ "FORD_6_1",
      . %in% c("FORD_6_2_1", "FORD_6_2_2") ~ "FORD_6_2",
      . %in% c("FORD_6_3_1", "FORD_6_3_2") ~ "FORD_6_3",
      TRUE ~ .
    ))
  ) %>% 
  mutate_at(vars(all_of(cats.other)),
            funs(
              str_replace(., "FOS_", "FORD_")
            )
  )

# Add the manually identified errors from the VABB file : merge --------------------------------------

merge.IDs <- journalIDs.cleaned.VABB %>% 
  filter(Action %in% c("Merge", "Merge; title \"International Journal for Educational and Vocational Guidance\"")) %>% 
  distinct(Journal.ID.1, Journal.ID.2, Journal.ID.3)

merge.dataset.raw <- merge.IDs %>% 
  mutate(pair = row_number()) %>% 
  gather(Journal.ID_nr, journal.ID, Journal.ID.1:Journal.ID.3) %>% 
  select(-Journal.ID_nr) %>% 
  inner_join(all.journals.wide3, by = "journal.ID") %>% 
  group_by(pair) %>% 
  fill(vabb.ID:ERIH_14, .direction = "downup") %>% 
  ungroup()

merge.ISSNs <- merge.dataset.raw %>% 
  select(pair, journal.ID, ISSN_1:ISSN_4) %>% 
  gather(ISSN_nr, ISSN, ISSN_1:ISSN_4) %>% 
  filter(!is.na(ISSN)) %>% 
  select(-ISSN_nr) %>% 
  distinct(pair, ISSN) %>% 
  group_by(pair) %>% 
  mutate(ISSN_nr = row_number()) %>% 
  ungroup() %>% 
  spread(ISSN_nr, ISSN)

names(merge.ISSNs) <- c("pair", paste0("ISSN_",1:4))

merge.dataset <- merge.dataset.raw %>% 
  filter(journal.ID %in% merge.IDs$Journal.ID.1) %>% 
  left_join(merge.IDs, by = c("journal.ID" = "Journal.ID.1")) %>% 
  distinct(journal.ID, .keep_all = TRUE) %>% 
  select(-ISSN_1:-ISSN_4) %>% 
  left_join(merge.ISSNs, by = "pair") %>% 
  select(-pair, Journal.ID.2)

# Remove from the journal overview all rows with IDs that need to be merge

all.journals.wide4 <- all.journals.wide3 %>% 
  filter(!journal.ID %in% c(merge.IDs$Journal.ID.1, merge.IDs$Journal.ID.2, merge.IDs$Journal.ID.3))

# Combine datasets

all.journals.wide5 <-  bind_rows(all.journals.wide4, merge.dataset)

# Add the manually identified errors from the VABB file : other --------------------------------------

merge.IDs.keep <- journalIDs.cleaned.VABB %>% 
  filter(Action %in% c("keep this", "should be IEEE technology & society magazine; keep this")) %>% 
  distinct(Journal.ID.1, .keep_all = TRUE)

merge.IDs.remove <- journalIDs.cleaned.VABB %>% 
  filter(Action == "remove this journal record") %>% 
  distinct(Journal.ID.1, .keep_all = TRUE)

# Remove the inaccurate records

all.journals.wide6 <- all.journals.wide5 %>% 
  filter(!journal.ID %in% merge.IDs.remove$Journal.ID.1)

# Final clean up ---------------------------------------------------------------------

# add missing cognitive codes for VABB journals

missing.VABB <- all.journals.wide6 %>% 
  filter(!is.na(vabb.ID) & is.na(VABB_1))

all.journals.wide6$VABB_1[which(all.journals.wide6$journal.ID == "::j::02285")] <- "FORD_5_8"
all.journals.wide6$VABB_2[which(all.journals.wide6$journal.ID == "::j::02285")] <- "FORD_6_2"

# add missing titles (TITLE)

missing.titles <- all.journals.wide6 %>% 
  filter(is.na(TITLE))

all.journals.wide6$TITLE[which(all.journals.wide6$journal.ID == "::j::05616")] <- "Review of World Economics"

# check journal IDs that appear multiple times

duplicate.IDs <- all.journals.wide6 %>% count(journal.ID) %>% filter(n > 1)

duplicate.IDs.toclean <- all.journals.wide6 %>% 
  filter(journal.ID %in% duplicate.IDs$journal.ID)

# Export for cleaning in  MS Excel (JournalIDs.xlsx)
#write_csv(duplicate.IDs.toclean, here::here("Output/duplicateJournalIDs_2020-08-14.csv"), na = "")

# Incorportate errors identified manually

journalIDs.cleaned.duplicates$Action <- ifelse(journalIDs.cleaned.duplicates$journal.ID == "::j::01808" & journalIDs.cleaned.duplicates$TITLE == "International Review of Applied Linguistics in Language Teaching",
                                               "keep this", journalIDs.cleaned.duplicates$Action)

remove.ids <- journalIDs.cleaned.duplicates %>% 
  filter(Action == "remove this") %>% 
  select(-Notes:-Adjust)

adjust.ids <- journalIDs.cleaned.duplicates %>% 
  filter(Action == "keep this" & !is.na(Adjust))

all.journals.wide7 <- anti_join(all.journals.wide6, remove.ids, by = names(all.journals.wide6[c(1:10, 49)])) %>% 
  distinct(journal.ID, .keep_all = TRUE) # removes two records that could not be taken out in the previous step becase of wrong character encoding

all.journals.wide7$Title.VABB[which(all.journals.wide7$journal.ID == "::j::01186")] <- "American Economic Journal: Economic Policy"
all.journals.wide7$vabb.ID[which(all.journals.wide7$journal.ID == "::j::01186")] <- "vabb:j:0000661A"

all.journals.wide7$TITLE[which(all.journals.wide7$journal.ID == "::j::04114")] <- "California Law Review"
all.journals.wide7$Title.WOS[which(all.journals.wide7$journal.ID == "::j::04114")] <- "California Law Review"

all.journals.wide7$Title.VABB[which(all.journals.wide7$journal.ID == "::j::01130")] <- NA
all.journals.wide7$vabb.ID[which(all.journals.wide7$journal.ID == "::j::01130")] <- NA
all.journals.wide7$Title.WOS[which(all.journals.wide7$journal.ID == "::j::01130")] <- NA
all.journals.wide7$VABB_1[which(all.journals.wide7$journal.ID == "::j::01130")] <- NA
all.journals.wide7$WOS_1[which(all.journals.wide7$journal.ID == "::j::01130")] <- NA

all.journals.wide7$ISSN_1[which(all.journals.wide7$journal.ID == "::j::02205")] <- "0019-7939"
all.journals.wide7$ISSN_2[which(all.journals.wide7$journal.ID == "::j::02205")] <- NA

all.journals.wide7$ISSN_2[which(all.journals.wide7$journal.ID == "::j::01954")] <- NA
all.journals.wide7$Title.erih[which(all.journals.wide7$journal.ID == "::j::01954")] <- NA
all.journals.wide7$ERIH_1[which(all.journals.wide7$journal.ID == "::j::01954")] <- NA

all.journals.wide7$ISSN_1[which(all.journals.wide7$journal.ID == "::j::01955")] <- NA
all.journals.wide7$Title.VABB[which(all.journals.wide7$journal.ID == "::j::01955")] <- "Nordicom Review" # VABB journal ID is not adjusted

all.journals.wide7$Title.VABB[which(all.journals.wide7$journal.ID == "::j::01937")] <- "Poznan Studies in Contemporary Linguistics"

all.journals.wide7$cristin.ID[which(all.journals.wide7$journal.ID == "::j::00255")] <- NA
all.journals.wide7$Title.NSD[which(all.journals.wide7$journal.ID == "::j::00255")] <- NA
all.journals.wide7$NPU[which(all.journals.wide7$journal.ID == "::j::00255")] <- NA
all.journals.wide7$erih.id[which(all.journals.wide7$journal.ID == "::j::00255")] <- NA
all.journals.wide7$ERIH_1[which(all.journals.wide7$journal.ID == "::j::00255")] <- NA
all.journals.wide7$Title.erih[which(all.journals.wide7$journal.ID == "::j::00255")] <- NA

# check ISSNs that appear with multiple journal IDs

all.ISSNs.with.multipleIDs <- all.journals.wide7 %>% 
  gather(ISSN_nr, ISSN, ISSN_1:ISSN_4) %>% 
  filter(!is.na(ISSN)) %>% 
  group_by(journal.ID, ISSN) %>%
  count() %>% 
  filter(n > 1) # none

# clean up ISSNs

all.journals.ISSNs <- all.journals.wide7 %>% 
  gather(ISSN_nr, ISSN, ISSN_1:ISSN_4) %>% 
  filter(!is.na(ISSN)) %>% 
  distinct(journal.ID, ISSN) %>% 
  group_by(journal.ID) %>% 
  mutate(ISSN_nr = row_number()) %>% 
  spread(ISSN_nr, ISSN) %>% 
  select(journal.ID, ISSN_1 = "1", ISSN_2 = "2", ISSN_3 = "3", ISSN_4 = "4")

all.journals.wide8 <- all.journals.wide7 %>% 
  select(-ISSN_1:-ISSN_4) %>% 
  left_join(all.journals.ISSNs, by = "journal.ID")

# Export data -------------------------------------------------------------

# Prep for export 1 -- to analyse

all.journals.COMPLETE <- all.journals.wide8 %>% 
  select(-Journal.ID.2, -Journal.ID.3) # 5 records less than before

# Prep for export 2 -- to submit

all.journals.COMPLETE_min <- all.journals.wide8 %>% 
  select(TITLE, VABB_1:ERIH_14, ISSN_1:ISSN_4)

# Export

currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/JOURNALS_foranalysis_", currentDate, ".csv")
csvFileName_2 <- paste0("./Output/JOURNALS_complete_", currentDate, ".csv")

write_csv(all.journals.COMPLETE, csvFileName_1, na = "")
write_csv(all.journals.COMPLETE_min, csvFileName_2, na = "")