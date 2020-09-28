# Call packages -----------------------------------------------------------

library(tidyverse)
library(extrafont)
library(ggalluvial)
library(RColorBrewer)
library(randomcoloR)
library(gridExtra)

# Import data -------------------------------------------------------------

VABBdata <- read_csv("./Output/FLANDERS_2020-08-24.csv", col_types = cols(.default = "c"))
CRISTINdata <- read_csv("./Output/NORWAY_2020-08-24.csv", col_types = cols(.default = "c"))

# Prep --------------------------------------------------------------------

SSH.codes <- c(paste0("FORD_5_",1:9), paste0("FORD_6_",1:5))
SSH.codes2 <- c(paste0("FOS_5_",1:9), paste0("FOS_6_",1:5))

# Define theme

theme_academic_ls <- function() {
  theme_classic() +
    theme(
      legend.position = "none",
      text = element_text(family = "Times"),
      axis.line.y = element_blank(),
      axis.line.x = element_line(color = "grey70"),
      axis.text = element_text(size = 10),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 6, color = "grey30"),
      axis.ticks = element_blank(),
      #panel.grid.major.y = element_line(),
      plot.margin = margin(0, 5, 0, 5, "mm")
    )
}

# set date
currentDate <- Sys.Date()

# Define colors

col_vector <- c("#0E405B", "#F79889", "#0E5E5E", "#D32D45","#CDEDF6",
                "#F42A56", "#257E93", "#F98296", "#F4E5D7",
                "#AF756E", "#1B174F", "#0E776A", "#4DC7E8", "#4796A8",
                '#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231')

# Prep  data - FLANDERS -------------------------------------------------------

clas.vars<- c(paste0("VABB.FOS", 1:5), "SM.OECD", "NSD.OECD", paste0("erih.oecd", 1:13), paste0("WOS_",1:6))

#recode variables
VABBdata <- VABBdata %>% 
  mutate_at(clas.vars, funs(as.factor(.))) %>% 
  mutate_at(clas.vars, funs(recode(. , FOS_5_1 = "PSY", FOS_5_2 = "ECON", FOS_5_3 = "EDU", FOS_5_4 = "SOC", FOS_5_5 = "LAW",
                                        FOS_5_6 = "POL", FOS_5_7 = "GEO", FOS_5_8 = "MEDIA", FOS_5_9 = "OTHS",
                                        FOS_6_1 = "HIST", FOS_6_2 = "LANG", FOS_6_3 = "PHIL", FOS_6_4 = "ART", FOS_6_5 = "OTHH",
                                        FOS_1_1 = "NAT", FOS_1_2 = "NAT", FOS_1_3 = "NAT", FOS_1_4 = "NAT", FOS_1_5 = "NAT", FOS_1_6 = "NAT", FOS_1_7 = "NAT",
                                        FOS_2_0 = "ENG", FOS_2_1 = "ENG", FOS_2_2 = "ENG", FOS_2_3 = "ENG", FOS_2_4 = "ENG", FOS_2_5 = "ENG", FOS_2_6 = "ENG", FOS_2_7 = "ENG", FOS_2_8 = "ENG", FOS_2_9 = "ENG", FOS_2_10 = "ENG", FOS_2_11 = "ENG",
                                        FOS_3_1 = "MED", FOS_3_2 = "MED", FOS_3_3 = "MED", FOS_3_4 = "MED", FOS_3_5 = "MED",
                                        FOS_4_1 = "AGR", FOS_4_2 = "AGR", FOS_4_3 = "AGR", FOS_4_4 = "AGR", FOS_4_5 = "AGR",
                                        `FOS_5 AND 6` = "IND", `FOS_1 AND 2` = "IND"))
  )

# Prep data : WOS
VABBdata_foralluvial.WOS <- VABBdata %>% 
  select(Loi, VABB.FOS1:VABB.FOS5, WOS_1:WOS_6, Fract_count) %>% 
  gather(VABB.FOS_nr, VABB.FOS, VABB.FOS1:VABB.FOS5) %>% 
  gather(WOS_nr, WOS, WOS_1:WOS_6) %>% 
  filter(!is.na(VABB.FOS) & !is.na(WOS)) %>% 
  group_by(WOS, VABB.FOS) %>% 
  summarise(sum = sum(as.double(Fract_count))) %>% 
  ungroup() %>% 
  filter(!VABB.FOS %in% c("NAT", "ENG", "MED", "AGR", "IND")) %>% 
  mutate_at(c("VABB.FOS", "WOS"), funs(factor(. , levels = c("PSY", "ECON", "EDU", "SOC", "LAW", "POL", "GEO", "MEDIA", "OTHS",
                                                             "HIST", "LANG","PHIL", "ART", "OTHH",
                                                             "NAT", "ENG", "MED", "AGR", "IND"))))

# Prep data: SM
VABBdata_foralluvial.SM <- VABBdata %>% 
  select(Loi, VABB.FOS1:VABB.FOS5,SM.OECD, Fract_count) %>% 
  gather(VABB.FOS_nr, VABB.FOS, VABB.FOS1:VABB.FOS5) %>% 
  filter(!is.na(VABB.FOS) & !is.na(SM.OECD)) %>% 
  group_by(SM.OECD, VABB.FOS) %>% 
  summarise(sum = sum(as.double(Fract_count))) %>% 
  ungroup() %>% 
  filter(!VABB.FOS %in% c("NAT", "ENG", "MED", "AGR", "IND")) %>% 
  mutate_at(c("VABB.FOS", "SM.OECD"), funs(factor(. , levels = c("PSY", "ECON", "EDU", "SOC", "LAW", "POL", "GEO", "MEDIA", "OTHS",
                                                             "HIST", "LANG","PHIL", "ART", "OTHH",
                                                             "NAT", "ENG", "MED", "AGR", "IND"))))

# Prep data: ERIH
VABBdata_foralluvial.ERIH <- VABBdata %>% 
  select(Loi, VABB.FOS1:VABB.FOS5, erih.oecd1:erih.oecd13, Fract_count) %>% 
  gather(VABB.FOS_nr, VABB.FOS, VABB.FOS1:VABB.FOS5) %>% 
  gather(ERIH_nr, ERIH, erih.oecd1:erih.oecd13) %>% 
  filter(!is.na(VABB.FOS) & !is.na(ERIH)) %>% 
  group_by(ERIH, VABB.FOS) %>% 
  summarise(sum = sum(as.double(Fract_count))) %>% 
  ungroup() %>% 
  filter(!VABB.FOS %in% c("NAT", "ENG", "MED", "AGR", "IND")) %>% 
  mutate_at(c("VABB.FOS", "ERIH"), funs(factor(. , levels = c("PSY", "ECON", "EDU", "SOC", "LAW", "POL", "GEO", "MEDIA", "OTHS",
                                                             "HIST", "LANG","PHIL", "ART", "OTHH",
                                                             "NAT", "ENG", "MED", "AGR", "IND"))))

# Prep data: NPU
VABBdata_foralluvial.NPU <- VABBdata %>% 
  select(Loi, VABB.FOS1:VABB.FOS5, NSD.OECD, Fract_count) %>% 
  gather(VABB.FOS_nr, VABB.FOS, VABB.FOS1:VABB.FOS5) %>% 
  filter(!is.na(VABB.FOS) & !is.na(NSD.OECD)) %>% 
  group_by(NSD.OECD, VABB.FOS) %>% 
  summarise(sum = sum(as.double(Fract_count))) %>% 
  ungroup() %>% 
  filter(!VABB.FOS %in% c("NAT", "ENG", "MED", "AGR", "IND")) %>% 
  mutate_at(c("VABB.FOS", "NSD.OECD"), funs(factor(. , levels = c("PSY", "ECON", "EDU", "SOC", "LAW", "POL", "GEO", "MEDIA", "OTHS",
                                                                 "HIST", "LANG","PHIL", "ART", "OTHH",
                                                                 "NAT", "ENG", "MED", "AGR", "IND"))))

# Prep data - Norway ------------------------------------------------------
#recode variables

clas.vars<- c(paste0("VABB.FOS", 1:5), "SM.OECD", "NSD.OECD", paste0("erih.oecd", 1:11), paste0("WOS_",1:6))

CRISTINdata <- CRISTINdata %>% 
  mutate_at(clas.vars, funs(as.factor(.))) %>% 
  mutate_at(clas.vars, funs(recode(. , FOS_5_1 = "PSY", FOS_5_2 = "ECON", FOS_5_3 = "EDU", FOS_5_4 = "SOC", FOS_5_5 = "LAW",
                                   FOS_5_6 = "POL", FOS_5_7 = "GEO", FOS_5_8 = "MEDIA", FOS_5_9 = "OTHS",
                                   FOS_6_1 = "HIST", FOS_6_2 = "LANG", FOS_6_3 = "PHIL", FOS_6_4 = "ART", FOS_6_5 = "OTHH",
                                   FOS_1_1 = "NAT", FOS_1_2 = "NAT", FOS_1_3 = "NAT", FOS_1_4 = "NAT", FOS_1_5 = "NAT", FOS_1_6 = "NAT", FOS_1_7 = "NAT",
                                   FOS_2_0 = "ENG", FOS_2_1 = "ENG", FOS_2_2 = "ENG", FOS_2_3 = "ENG", FOS_2_4 = "ENG", FOS_2_5 = "ENG", FOS_2_6 = "ENG", FOS_2_7 = "ENG", FOS_2_8 = "ENG", FOS_2_9 = "ENG", FOS_2_10 = "ENG", FOS_2_11 = "ENG",
                                   FOS_3_1 = "MED", FOS_3_2 = "MED", FOS_3_3 = "MED", FOS_3_4 = "MED", FOS_3_5 = "MED",
                                   FOS_4_1 = "AGR", FOS_4_2 = "AGR", FOS_4_3 = "AGR", FOS_4_4 = "AGR", FOS_4_5 = "AGR",
                                   `FOS_5 AND 6` = "IND", `FOS_1 AND 2` = "IND"))
  )

#WOS
CRISTINdata_foralluvial.WOS <- CRISTINdata %>% 
  select(VARBEIDLOPENR, NSD.OECD, WOS_1:WOS_6, Fract_count) %>% 
  gather(WOS_nr, WOS, WOS_1:WOS_6) %>% 
  filter(!is.na(WOS)) %>% 
  group_by(NSD.OECD, WOS) %>% 
  summarise(sum  = sum(as.double(Fract_count))) %>% 
  ungroup() %>% 
  mutate_at(c("NSD.OECD", "WOS"), funs(factor(. , levels = c("PSY", "ECON", "EDU", "SOC", "LAW", "POL", "GEO", "MEDIA", "OTHS",
                                                                  "HIST", "LANG","PHIL", "ART", "OTHH",
                                                                  "NAT", "ENG", "MED", "AGR", "IND"))))
#SM
CRISTINdata_foralluvial.SM <- CRISTINdata %>% 
  select(VARBEIDLOPENR, NSD.OECD, SM.OECD, Fract_count) %>% 
  filter(!is.na(SM.OECD)) %>% 
  group_by(NSD.OECD, SM.OECD) %>% 
  summarise(sum  = sum(as.double(Fract_count))) %>% 
  ungroup() %>% 
  mutate_at(c("NSD.OECD", "SM.OECD"), funs(factor(. , levels = c("PSY", "ECON", "EDU", "SOC", "LAW", "POL", "GEO", "MEDIA", "OTHS",
                                                             "HIST", "LANG","PHIL", "ART", "OTHH",
                                                             "NAT", "ENG", "MED", "AGR", "IND"))))
#ERIH
CRISTINdata_foralluvial.ERIH <- CRISTINdata %>% 
  select(VARBEIDLOPENR, NSD.OECD, erih.oecd1:erih.oecd11, Fract_count) %>% 
  gather(erih.nr, ERIH, erih.oecd1:erih.oecd11) %>% 
  filter(!is.na(ERIH)) %>% 
  group_by(NSD.OECD, ERIH) %>% 
  summarise(sum  = sum(as.double(Fract_count))) %>% 
  ungroup() %>% 
  mutate_at(c("NSD.OECD", "ERIH"), funs(factor(. , levels = c("PSY", "ECON", "EDU", "SOC", "LAW", "POL", "GEO", "MEDIA", "OTHS",
                                                                 "HIST", "LANG","PHIL", "ART", "OTHH",
                                                                 "NAT", "ENG", "MED", "AGR", "IND"))))


#VABB
CRISTINdata_foralluvial.VABB <- CRISTINdata %>% 
  select(VARBEIDLOPENR, NSD.OECD, VABB.FOS1:VABB.FOS5, Fract_count) %>% 
  gather(vabb.nr, VABB, VABB.FOS1:VABB.FOS5) %>% 
  filter(!is.na(VABB)) %>% 
  group_by(NSD.OECD, VABB) %>% 
  summarise(sum  = sum(as.double(Fract_count))) %>% 
  ungroup() %>% 
  mutate_at(c("NSD.OECD", "VABB"), funs(factor(. , levels = c("PSY", "ECON", "EDU", "SOC", "LAW", "POL", "GEO", "MEDIA", "OTHS",
                                                              "HIST", "LANG","PHIL", "ART", "OTHH",
                                                              "NAT", "ENG", "MED", "AGR", "IND"))))

# Generate alluvials for VABB ---------------------------------------------

#WOS

VABBdata_foralluvial.WOS %>% filter(sum > 75) %>% count(VABB.FOS) # 9 and 14 missing

VABB_B <- VABBdata_foralluvial.WOS %>% 
  #arrange(desc(sum)) %>% 
  #group_by(VABB.FOS) %>% 
  #slice(1:3) %>% 
  #ungroup() %>% 
  filter(sum > 75) %>% 
  ggplot(aes(axis1 = VABB.FOS, axis2 = WOS, y = sum)) +
  geom_alluvium(aes(fill = VABB.FOS), width = 1/10, alpha = 0.5) +
  geom_stratum(alpha = 0.8, width = 1/10, color = "grey50", size = 0.01) +
  #geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum)),
                           size = 1.5, family = "Times",
                           segment.colour = "black",
                           segment.size = 0.1,
                           direction ="x",
                           box.padding = 0.1,
                           point.padding = NA) +
  #ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum), label.strate = TRUE, direction ="x") # something not right at all
  theme_academic_ls() +
  scale_x_continuous(breaks = 1:2, labels = c("VABB", "Web of Science")) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = col_vector[c(-9, -14)])

#SM

VABBdata_foralluvial.SM %>% filter(sum > 75) %>% count(VABB.FOS) # 9 and 13, 14 missing

VABB_C <- VABBdata_foralluvial.SM %>% 
  filter(sum > 75) %>% 
  ggplot(aes(axis1 = VABB.FOS, axis2 = SM.OECD, y = sum)) +
  geom_alluvium(aes(fill = VABB.FOS), width = 1/10, alpha = 0.5) +
  geom_stratum(alpha = 0.8, width = 1/10, color = "grey50", size = 0.01) +
  #geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum)),
                           size = 1.5, family = "Times",
                           segment.colour = "black",
                           segment.size = 0.1,
                           direction ="x",
                           box.padding = 0.1,
                           point.padding = NA) +
  #ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum), label.strate = TRUE, direction ="x") # something not right at all
  theme_academic_ls() +
  scale_x_continuous(breaks = 1:2, labels = c("VABB", "Science-Metrix")) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = col_vector[c(-9, -13, -14)])

#ERIH 

VABBdata_foralluvial.ERIH %>% filter(sum > 75) %>% count(VABB.FOS) # 9 and 14 missing

VABB_D <- VABBdata_foralluvial.ERIH %>% 
  filter(sum > 75) %>% 
  ggplot(aes(axis1 = VABB.FOS, axis2 = ERIH, y = sum)) +
  geom_alluvium(aes(fill = VABB.FOS), width = 1/10, alpha = 0.5) +
  geom_stratum(alpha = 0.8, width = 1/10, color = "grey50", size = 0.01) +
  #geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum)),
                           size = 1.5, family = "Times",
                           segment.colour = "black",
                           segment.size = 0.1,
                           direction ="x",
                           box.padding = 0.1,
                           point.padding = NA) +
  #ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum), label.strate = TRUE, direction ="x") # something not right at all
  theme_academic_ls() +
  scale_x_continuous(breaks = 1:2, labels = c("VABB", "ERIH PLUS")) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = col_vector[c(-9, -14)])

#NPU 

VABBdata_foralluvial.NPU %>% filter(sum > 75) %>% count(VABB.FOS) # 9 and 14 missing

VABB_E <- VABBdata_foralluvial.NPU %>% 
  filter(sum > 75) %>% 
  ggplot(aes(axis1 = VABB.FOS, axis2 = NSD.OECD, y = sum)) +
  geom_alluvium(aes(fill = VABB.FOS), width = 1/10, alpha = 0.5) +
  geom_stratum(alpha = 0.8, width = 1/10, color = "grey50", size = 0.01) +
  #geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum)),
                           size = 1.5, family = "Times",
                           segment.colour = "black",
                           segment.size = 0.1,
                           direction ="x",
                           box.padding = 0.1,
                           point.padding = NA) +
  #ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum), label.strate = TRUE, direction ="x") # something not right at all
  theme_academic_ls() +
  scale_x_continuous(breaks = 1:2, labels = c("VABB", "NPU")) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = col_vector[c(-9, -14)])


# Generate alluvials for CRISTIN ------------------------------------------

#WOS 

CRISTINdata_foralluvial.WOS %>% filter(sum > 75) %>% count(NSD.OECD) # 14 missing

CRISTIN_B <- CRISTINdata_foralluvial.WOS %>% 
  filter(sum > 75) %>% 
  ggplot(aes(axis1 = NSD.OECD, axis2 = WOS, y = sum)) +
  geom_alluvium(aes(fill = NSD.OECD), width = 1/10, alpha = 0.5) +
  geom_stratum(alpha = 0.8, width = 1/10, color = "grey50", size = 0.01) +
  #geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum)),
                           size = 1.5, family = "Times",
                           segment.colour = "black",
                           segment.size = 0.1,
                           direction ="x",
                           box.padding = 0.1,
                           point.padding = NA) +
  #ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum), label.strate = TRUE, direction ="x") # something not right at all
  theme_academic_ls() +
  scale_x_continuous(breaks = 1:2, labels = c("NPU", "Web of Science")) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = col_vector[-14])

#SM

CRISTINdata_foralluvial.SM %>% filter(sum > 75) %>% count(NSD.OECD) # 13 and 14 missing

CRISTIN_C <- CRISTINdata_foralluvial.SM %>% 
  filter(sum > 75) %>% 
  ggplot(aes(axis1 = NSD.OECD, axis2 = SM.OECD, y = sum)) +
  geom_alluvium(aes(fill = NSD.OECD), width = 1/10, alpha = 0.5) +
  geom_stratum(alpha = 0.8, width = 1/10, color = "grey50", size = 0.01) +
  #geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum)),
                           size = 1.5, family = "Times",
                           segment.colour = "black",
                           segment.size = 0.1,
                           direction ="x",
                           box.padding = 0.1,
                           point.padding = NA) +
  #ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum), label.strate = TRUE, direction ="x") # something not right at all
  theme_academic_ls() +
  scale_x_continuous(breaks = 1:2, labels = c("NPU", "Science-Metrix")) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = col_vector[c(-13, -14)])

#ERIH

CRISTINdata_foralluvial.ERIH %>% filter(sum > 75) %>% count(NSD.OECD) # 7, 9, 14 missing

CRISTIN_D <- CRISTINdata_foralluvial.ERIH %>% 
  filter(sum > 75) %>% 
  ggplot(aes(axis1 = NSD.OECD, axis2 = ERIH, y = sum)) +
  geom_alluvium(aes(fill = NSD.OECD), width = 1/10, alpha = 0.5) +
  geom_stratum(alpha = 0.8, width = 1/10, color = "grey50", size = 0.01) +
  #geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum)),
                           size = 1.5, family = "Times",
                           segment.colour = "black",
                           segment.size = 0.1,
                           direction ="x",
                           box.padding = 0.1,
                           point.padding = NA) +
  #ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum), label.strate = TRUE, direction ="x") # something not right at all
  theme_academic_ls() +
  scale_x_continuous(breaks = 1:2, labels = c("NPU", "ERIH PLUS")) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = col_vector[c(-7,  -9, -14)])


#VABB

CRISTINdata_foralluvial.VABB %>% filter(sum > 75) %>% count(NSD.OECD) # 14 missing

CRISTIN_E <- CRISTINdata_foralluvial.VABB %>% 
  filter(sum > 75 & VABB != "MED") %>% 
  ggplot(aes(axis1 = NSD.OECD, axis2 = VABB, y = sum)) +
  geom_alluvium(aes(fill = NSD.OECD), width = 1/10, alpha = 0.5) +
  geom_stratum(alpha = 0.8, width = 1/10, color = "grey50", size = 0.01) +
  #geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum)),
                           size = 1.5, family = "Times",
                           segment.colour = "black",
                           segment.size = 0.1,
                           direction ="x",
                           box.padding = 0.1,
                           point.padding = NA) +
  #ggrepel::geom_text_repel(stat = "stratum", aes(label = after_stat(stratum), label.strate = TRUE, direction ="x") # something not right at all
  theme_academic_ls() +
  scale_x_continuous(breaks = 1:2, labels = c("NPU", "VABB")) +
  theme(
    axis.text.y = element_blank(),
    axis.line.x.bottom = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = col_vector[-14])


# Combine plots -----------------------------------------------------------

plot1 <- grid.arrange(VABB_B, CRISTIN_B, 
             VABB_C, CRISTIN_C,
             VABB_D, CRISTIN_D,
             nrow = 3)

ggsave(paste0("alluvials_1_", currentDate, ".png"), plot1 , "png", here::here("Plots"),
       width = 17, height = 24, units = "cm", dpi = 600 )

plot2 <- grid.arrange(VABB_D, CRISTIN_D, 
             VABB_E, CRISTIN_E,
             nrow = 2)

ggsave(paste0("alluvials_2_", currentDate, ".png"), plot2 , "png", here::here("Plots"),
       width = 17, height = 16, units = "cm", dpi = 600 )


plot3 <- grid.arrange(VABB_B, CRISTIN_B, 
                      VABB_D, CRISTIN_D,
                      nrow = 2)

ggsave(paste0("alluvials_3_", currentDate, ".png"), plot3 , "png", here::here("Plots"),
       width = 17, height = 16, units = "cm", dpi = 600 )

ggsave(paste0("alluvials_3_", currentDate, ".pdf"), plot3 , "pdf", here::here("Plots"),
       width = 17, height = 16, units = "cm", dpi = 600 )



plot4 <- grid.arrange(VABB_C, CRISTIN_C, 
                      VABB_E, CRISTIN_E,
                      nrow = 2)

ggsave(paste0("alluvials_4_", currentDate, ".png"), plot4 , "png", here::here("Plots"),
       width = 17, height = 16, units = "cm", dpi = 600 )

ggsave(paste0("alluvials_4_", currentDate, ".pdf"), plot4 , "pdf", here::here("Plots"),
       width = 17, height = 16, units = "cm", dpi = 600 )