## This script contains VABB data prep limited to transformation of VABB data only, no external data are added

## call package

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# import data

VABBdata <- read_csv2(
  "./Raw data/VABB/170523_overzichtsrapport.csv",
  col_types = cols(.default = "c"))
VABBserialnm <- read_csv2(
  "./Raw data/VABB/vabb7nums_edited24062019.csv",
  col_types = cols(.default = "c"))


# limit the timespan to 2006-2015 and select only journals

years <- c(2006:2015)

VABBdata <- VABBdata %>% 
  filter(pubyear %in% years & `VABB-publicatietype` %in% "VABB-1")

# select only peer-reviewed publications

PRstatus_variables <- rev(names(VABBdata)[8:15])

VABBdata2$peer.reviewed <- NA

for (i in 1:nrow(VABBdata2)) {
  
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
}

VABBdata3 <- VABBdata2 %>%  filter(peer.reviewed == "YES")

# Add column with info whether the record has ISI code


# Add column with info whether the record is in WoS BOF


# Select records only from universities

VABBdata4 <- VABBdata3 %>% filter(`lm(vabb-ua)` %in% "Ja" |
                                `lm(vabb-kul)` %in% "Ja" |
                                `lm(vabb-ug)` %in% "Ja" |
                                `lm(vabb-uh)` %in% "Ja" |
                                `lm(vabb-vub)` %in% "Ja")

#write data file for next module
currentDate <- Sys.Date() 
csvFileName <- paste("/Output/VABB/Temp/VABB_output_m1_",currentDate,".csv",sep="") 
write_csv(VABBdata3, csvFileName, na="")



