# Call packages -----------------------------------------------------------

library(tidyverse)
library(readxl)

# Import data -------------------------------------------------------------

VABBdata <- read_csv("./Output/FLANDERS_2020-08-24.csv", col_types = cols(.default = "c"))
WOS.info <- read_excel("./Manually cleaned data/BE_WOSVABB_MANUAL_08112018.xlsx" , sheet = "VABB-1")
FLANDERSdataWOS <- read_csv("./Raw data/Web of Science - KUL/Belgium_201807232032.csv",
                            col_types = cols(.default = "c")) # WoS data on Belgium retrieved from the KUL database


# Check accuracy ----------------------------------------------------------

# Identify records that should be  in WOS but are not identified using our method

# identify articles in WOS BOF

PRstatus_variables <- rev(names(VABBdata)[8:15])

VABBdata$in.WOS.bof <- NA

system.time(for (i in 1:nrow(VABBdata)) {
  
  for (var in PRstatus_variables) {
    if (!is.na(VABBdata$in.WOS.bof[i])) {
      break
    }
    if (is.na(VABBdata[var][i,])) {
      next
    }
    if (grepl("in WOS \\(bof\\)", VABBdata[var][i,])) {
      VABBdata$in.WOS.bof[i] <- "YES"
    } else {
      VABBdata$in.WOS.bof[i] <- "NO"
    }
  }
})

# identify articles with  WOS id

VABBdata$is.wosID <- str_detect(VABBdata$isi.x, "[:digit:]{15}")

# summarise

VABBdata.inWOS <- VABBdata  %>% 
  group_by(in.WOS.bof, is.wosID, indexed.WOS_FIN) %>% 
  count()

# Retrieve info on possible reasons why something was not identified

VABBdata$WOS.notes <- WOS.info$NOTES[match(VABBdata$Loi, WOS.info$Loi)]

VABBdata.unidentified <- VABBdata %>% 
  filter(in.WOS.bof == "YES" & is.na(indexed.WOS_FIN)) %>% 
  #filter((in.WOS.bof == "YES" | is.wosID == TRUE) & 
          #(indexed.WOS_FIN == FALSE | is.na(indexed.WOS_FIN))) %>% 
  select(Loi, in.WOS.bof, is.wosID, indexed.WOS_FIN, WOS.notes) %>% 
  group_by(WOS.notes) %>% 
  count()

# Select records that were not identified and that have no notes

VABBdata.nonotes <- VABBdata %>% 
  filter(in.WOS.bof == "YES" & is.na(indexed.WOS_FIN) & (is.na(WOS.notes) | WOS.notes == "TODO"))

WOS.missed <- FLANDERSdataWOS %>% filter(PUBID %in% VABBdata.nonotes$isi.x)

# checked manually  on 25/09/2020, 4 likely due to language and 10 due to metadata errors (? could it be that either WOS or VABB data were not changed to lowercase?).