## call package

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# import data

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
  distinct(Loi, .keep_all = TRUE) %>% # The join generates more than 2000 duplicates. To check.
  filter(!is.na(VABB.FOS1)) %>% 
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