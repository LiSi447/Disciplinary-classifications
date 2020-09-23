# Call packages -----------------------------------------------------------

library(tidyverse)

# Import data -------------------------------------------------------------

journals <- read_csv("./Output/JOURNALS_foranalysis_2020-08-24.csv", col_types = cols(.default = "c"))

# Prep --------------------------------------------------------------------

SSH.codes <- c(paste0("FORD_5_",1:9), paste0("FORD_6_",1:5))
SSH.codes2 <- c(paste0("FOS_5_",1:9), paste0("FOS_6_",1:5))
SS.codes <- c(SSH.codes[1:9], SSH.codes2[1:9])
H.codes <- c(SSH.codes[10:14], SSH.codes2[10:14])

journals <- journals %>% 
  mutate_if(str_detect(names(.), "jaccard"), as.double)

# Journals by dataset -----------------------------------------------------

journals %>% filter(!is.na(VABB_1)) %>% count(journal.ID)
journals %>% filter(!is.na(VABB_1) & !is.na(WOS_1)) %>% count(journal.ID)
journals %>% filter(!is.na(VABB_1) & !is.na(SM)) %>% count(journal.ID)
journals %>% filter(!is.na(VABB_1) & !is.na(ERIH_1)) %>% count(journal.ID)
journals %>% filter(!is.na(VABB_1) & !is.na(NPU)) %>% count(journal.ID)


journals %>% filter(!is.na(NPU)) %>% count(journal.ID)
journals %>% filter(!is.na(NPU) & !is.na(WOS_1)) %>% count(journal.ID)
journals %>% filter(!is.na(NPU) & !is.na(SM)) %>% count(journal.ID)
journals %>% filter(!is.na(NPU) & !is.na(ERIH_1)) %>% count(journal.ID)
journals %>% filter(!is.na(NPU) & !is.na(VABB_1)) %>% count(journal.ID)
# Jaccard : equivalent assignment -----------------------------------------

# totals

Equiv.SSH_journals <- journals %>%
  gather(pair, jaccard, jaccard_vabb_wos:jaccard_npu_erih) %>% 
  select(journal.ID, pair, jaccard) %>% 
  filter(!is.na(jaccard)) %>% 
  mutate(equivalent = jaccard == 1) %>% 
  group_by(equivalent, pair) %>% 
  count() %>% 
  group_by(pair) %>%
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(share = round(n / total, digits = 2)) %>% 
  filter(equivalent == TRUE)

# SS

Equiv.SS_journals <- journals %>%
  gather(VABB.nr, VABB, VABB_1:VABB_5) %>% 
  filter(VABB %in% SS.codes | NPU %in% SS.codes) %>% 
  distinct(journal.ID, .keep_all = TRUE) %>% 
  gather(pair, jaccard, jaccard_vabb_wos:jaccard_npu_erih) %>% 
  select(journal.ID, pair, jaccard) %>% 
  filter(!is.na(jaccard)) %>% 
  mutate(equivalent = jaccard == 1) %>% 
  group_by(equivalent, pair) %>% 
  count() %>% 
  group_by(pair) %>%
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(share = round(n / total, digits = 2)) %>% 
  filter(equivalent == TRUE)

# H

Equiv.H_journals <- journals %>%
  gather(VABB.nr, VABB, VABB_1:VABB_5) %>% 
  filter(VABB %in% H.codes | NPU %in% H.codes) %>% 
  distinct(journal.ID, .keep_all = TRUE) %>% 
  gather(pair, jaccard, jaccard_vabb_wos:jaccard_npu_erih) %>% 
  select(journal.ID, pair, jaccard) %>% 
  filter(!is.na(jaccard)) %>% 
  mutate(equivalent = jaccard == 1) %>% 
  group_by(equivalent, pair) %>% 
  count() %>% 
  group_by(pair) %>%
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(share = round(n / total, digits = 2)) %>% 
  filter(equivalent == TRUE)

# By discipline -- VABB

Equiv.by.disc_VABB.journals <- journals %>%
  gather(VABB.nr, VABB, VABB_1:VABB_5) %>% 
  filter(VABB %in% SSH.codes) %>% 
  select(journal.ID, VABB, jaccard_vabb_wos, jaccard_vabb_sm, jaccard_vabb_erih, jaccard_vabb_npu) %>% 
  gather(pair, jaccard, jaccard_vabb_wos:jaccard_vabb_npu) %>% 
  filter(!is.na(jaccard)) %>% 
  distinct(journal.ID, pair, VABB, jaccard) %>% 
  mutate(equivalent = jaccard == 1) %>% 
  group_by(VABB, pair, equivalent) %>% 
  count() %>% 
  group_by(pair, VABB) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(share = round(n / total, digits = 2)) %>% 
  filter(equivalent == TRUE) %>% 
  select(VABB, pair, share) %>% 
  spread(pair, share)
  
  
# By discipline -- NPU

Equiv.by.disc_NPU.journals <- journals %>%
  filter(NPU %in% SSH.codes) %>% 
  select(journal.ID, NPU, jaccard_npu_wos, jaccard_npu_sm, jaccard_npu_erih, jaccard_vabb_npu) %>% 
  gather(pair, jaccard, jaccard_npu_wos:jaccard_vabb_npu) %>% 
  filter(!is.na(jaccard)) %>% 
  distinct(journal.ID, pair, NPU, jaccard) %>% 
  mutate(equivalent = jaccard == 1) %>% 
  group_by(NPU, pair, equivalent) %>% 
  count() %>% 
  group_by(pair, NPU) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(share = round(n / total, digits = 2)) %>% 
  filter(equivalent == TRUE) %>% 
  select(NPU, pair, share) %>% 
  spread(pair, share)



# Jaccard similarity -- journals ------------------------------------------

# all SSH

summary_jaccard.journals <- journals %>% 
  select(jaccard_vabb_wos, jaccard_npu_wos, jaccard_vabb_sm, jaccard_npu_sm, jaccard_vabb_npu, jaccard_vabb_erih, jaccard_npu_erih) %>% 
  gather(pair, value, jaccard_vabb_wos:jaccard_npu_erih) %>% 
  filter(!is.na(value)) %>% 
  group_by(pair) %>% 
  summarise(mean = round(mean(value, na.rm = TRUE), 2),
            median = median(value),
            sd = round(sd(value, na.rm = TRUE), 2),
            min = round(min(value, na.rm = TRUE), 2),
            max =  round(max(value, na.rm = TRUE), 2)
  )

# SS - VABB

summary_jaccard.journals.SS.VABB <- journals %>% 
  gather(VABB.nr, VABB, VABB_1:VABB_5) %>% 
  filter(VABB %in% SS.codes) %>% 
  select(jaccard_vabb_wos, jaccard_vabb_sm, jaccard_vabb_npu, jaccard_vabb_erih) %>% 
  gather(pair, value, jaccard_vabb_wos:jaccard_vabb_erih) %>% 
  filter(!is.na(value)) %>% 
  group_by(pair) %>% 
  summarise(mean = round(mean(value, na.rm = TRUE), 2),
            median = median(value),
            sd = round(sd(value, na.rm = TRUE), 2),
            min = round(min(value, na.rm = TRUE), 2),
            max =  round(max(value, na.rm = TRUE), 2)
  )

# SS - NPU

summary_jaccard.journals.SS.NPU <- journals %>% 
  filter(NPU %in% SS.codes) %>% 
  select(jaccard_npu_wos, jaccard_npu_sm, jaccard_vabb_npu, jaccard_npu_erih) %>% 
  gather(pair, value, jaccard_npu_wos:jaccard_npu_erih) %>% 
  filter(!is.na(value)) %>% 
  group_by(pair) %>% 
  summarise(mean = round(mean(value, na.rm = TRUE), 2),
            median = median(value),
            sd = round(sd(value, na.rm = TRUE), 2),
            min = round(min(value, na.rm = TRUE), 2),
            max =  round(max(value, na.rm = TRUE), 2)
  )

# H - VABB

summary_jaccard.journals.H.VABB <- journals %>% 
  gather(VABB.nr, VABB, VABB_1:VABB_5) %>% 
  filter(VABB %in% H.codes) %>% 
  select(jaccard_vabb_wos, jaccard_vabb_sm, jaccard_vabb_npu, jaccard_vabb_erih) %>% 
  gather(pair, value, jaccard_vabb_wos:jaccard_vabb_erih) %>% 
  filter(!is.na(value)) %>% 
  group_by(pair) %>% 
  summarise(mean = round(mean(value, na.rm = TRUE), 2),
            median = median(value),
            sd = round(sd(value, na.rm = TRUE), 2),
            min = round(min(value, na.rm = TRUE), 2),
            max =  round(max(value, na.rm = TRUE), 2)
  )

# H - NPU

summary_jaccard.journals.H.NPU <- journals %>% 
  filter(NPU %in% H.codes) %>% 
  select(jaccard_npu_wos, jaccard_npu_sm, jaccard_vabb_npu, jaccard_npu_erih) %>% 
  gather(pair, value, jaccard_npu_wos:jaccard_npu_erih) %>% 
  filter(!is.na(value)) %>% 
  group_by(pair) %>% 
  summarise(mean = round(mean(value, na.rm = TRUE), 2),
            median = median(value),
            sd = round(sd(value, na.rm = TRUE), 2),
            min = round(min(value, na.rm = TRUE), 2),
            max =  round(max(value, na.rm = TRUE), 2)
  )

# overview by discipline 

summary_jaccard.by.discipline_NPU <- journals %>% 
  group_by(NPU) %>% 
  summarise(mean.jaccard.npu_wos = round(mean(jaccard_npu_wos, na.rm = TRUE), 2),
            mean.jaccard.npu_sm = round(mean(jaccard_npu_sm, na.rm = TRUE), 2),
            mean.jaccard.npu_erih = round(mean(jaccard_npu_erih, na.rm = TRUE), 2),
            mean.jaccard.npu_vabb = round(mean(jaccard_vabb_npu, na.rm = TRUE), 2),
            median.jaccard.npu_wos = round(median(jaccard_npu_wos, na.rm = TRUE), 2),
            median.jaccard.npu_sm = round(median(jaccard_npu_sm, na.rm = TRUE), 2),
            median.jaccard.npu_erih = round(median(jaccard_npu_erih, na.rm = TRUE), 2),
            median.jaccard.npu_vabb = round(median(jaccard_vabb_npu, na.rm = TRUE), 2),
            sd.jaccard.npu_wos = round(sd(jaccard_npu_wos, na.rm = TRUE), 2),
            sd.jaccard.npu_sm = round(sd(jaccard_npu_sm, na.rm = TRUE), 2),
            sd.jaccard.npu_erih = round(sd(jaccard_npu_erih, na.rm = TRUE), 2),
            sd.jaccard.npu_vabb = round(sd(jaccard_vabb_npu, na.rm = TRUE), 2)
            ) %>% 
  filter(!is.na(NPU))


summary_jaccard.by.discipline_VABB <- journals %>% 
  gather(VABB_nr, VABB, VABB_1:VABB_5) %>% 
  filter(!is.na(VABB)) %>% 
  group_by(VABB) %>% 
  summarise(mean.jaccard.vabb_wos = round(mean(jaccard_vabb_wos, na.rm = TRUE), 2),
            mean.jaccard.vabb_sm = round(mean(jaccard_vabb_sm, na.rm = TRUE), 2),
            mean.jaccard.vabb_erih = round(mean(jaccard_vabb_erih, na.rm = TRUE), 2),
            mean.jaccard.vabb_npu = round(mean(jaccard_vabb_npu, na.rm = TRUE), 2),
            median.jaccard.vabb_wos = round(median(jaccard_vabb_wos, na.rm = TRUE), 2),
            median.jaccard.vabb_sm = round(median(jaccard_vabb_sm, na.rm = TRUE), 2),
            median.jaccard.vabb_erih = round(median(jaccard_vabb_erih, na.rm = TRUE), 2),
            median.jaccard.vabb_npu = round(median(jaccard_vabb_npu, na.rm = TRUE), 2),
            sd.jaccard.vabb_wos = round(sd(jaccard_vabb_wos, na.rm = TRUE), 2),
            sd.jaccard.vabb_sm = round(sd(jaccard_vabb_sm, na.rm = TRUE), 2),
            sd.jaccard.vabb_erih = round(sd(jaccard_vabb_erih, na.rm = TRUE), 2),
            sd.jaccard.vabb_npu = round(sd(jaccard_vabb_npu, na.rm = TRUE), 2)
            ) %>% 
  filter(VABB %in% SSH.codes)

summary_jaccard.journals.by.discipline <- cbind(summary_jaccard.by.discipline_NPU, summary_jaccard.by.discipline_VABB)

summary_jaccard.journals_long <- summary_jaccard.journals.by.discipline %>% 
  select("FORD" = NPU, -VABB, mean.jaccard.npu_wos, mean.jaccard.npu_sm, mean.jaccard.npu_erih,
         mean.jaccard.npu_vabb, mean.jaccard.vabb_wos, mean.jaccard.vabb_sm, mean.jaccard.vabb_erih,
         mean.jaccard.vabb_npu) %>% 
  gather(pair, mean.jaccard, mean.jaccard.npu_wos:mean.jaccard.vabb_npu) %>% 
  mutate(
    level = "journal"
  ) %>% 
  mutate_if(is.numeric, ~ round(., 1))

# recode vars

summary_jaccard.journals_long$FORD <- recode(summary_jaccard.journals_long$FORD, FORD_5_1 = "PSY", FORD_5_2 = "ECON", FORD_5_3 = "EDU", FORD_5_4 = "SOC", FORD_5_5 = "LAW",
                                             FORD_5_6 = "POL", FORD_5_7 = "GEO", FORD_5_8 = "MEDIA", FORD_5_9 = "OTHS",
                                             FORD_6_1 = "HIST", FORD_6_2 = "LANG", FORD_6_3 = "PHIL", FORD_6_4 = "ART", FORD_6_5 = "OTHH")

summary_jaccard.journals_long$pair <- recode(summary_jaccard.journals_long$pair,
                                             mean.jaccard.npu_sm = "NPU-SM", mean.jaccard.npu_vabb = "NPU-VABB",
                                             mean.jaccard.npu_wos = "NPU-WOS", mean.jaccard.vabb_npu = "VABB-NPU",
                                             mean.jaccard.vabb_sm = "VABB-SM", mean.jaccard.vabb_wos = "VABB-WOS",
                                             mean.jaccard.vabb_erih = "VABB-ERIH", mean.jaccard.npu_erih = "NPU-ERIH"
)

# transform to wide
summary_jaccard.journals_wide <- summary_jaccard.journals_long %>% 
  group_by(FORD) %>% 
  mutate(counter = row_number()) %>% 
  ungroup() %>% 
  spread(FORD, pair)

# Export data -------------------------------------------------------------


currentDate <- Sys.Date()

csvFileName_1 <- paste0("./Output/equivalent_assigment_journals_", currentDate, ".csv")
csvFileName_2 <- paste0("./Output/jaccard_journals_", currentDate, ".csv")

write_csv(Equivalent.assignment_journals, csvFileName_1, na = "")
write_csv(summary_jaccard.journals_long, csvFileName_2, na = "")
