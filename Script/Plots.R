# Call packages -----------------------------------------------------------

library(tidyverse)
library(extrafont)
library(ggalluvial)
library(RColorBrewer)

# Import data -------------------------------------------------------------

FLANDERS_A <- read_csv("./Output/FLANDERS_A_2020-08-25.csv")
FLANDERS_B <- read_csv("./Output/FLANDERS_B_2020-09-14.csv")
FLANDERS_C <- read_csv("./Output/FLANDERS_C_2020-09-14.csv")
FLANDERS_D <- read_csv("./Output/FLANDERS_D_2020-09-14.csv")
FLANDERS_E <- read_csv("./Output/FLANDERS_E_2020-09-14.csv")
FLANDERS_NOTSSH <- read_csv("./Output/FLANDERS_NOTSSH_2020-08-25.csv")

NORWAY_A <- read_csv("./Output/NORWAY_A_2020-09-14.csv")
NORWAY_B <- read_csv("./Output/NORWAY_B_2020-09-14.csv")
NORWAY_C <- read_csv("./Output/NORWAY_C_2020-09-14.csv")
NORWAY_D <- read_csv("./Output/NORWAY_D_2020-09-14.csv")
NORWAY_E <- read_csv("./Output/NORWAY_E_2020-09-14.csv")
NORWAY_NOTSSH <- read_csv("./Output/NORWAY_NOTSSH_2020-08-25.csv")

ea_journals <- read_csv("./Output/equivalent_assigment_journals_2020-08-27.csv")
ea_articles <- read_csv("./Output/equivalent_assigment_articles_2020-08-27.csv")

jaccard_journals <- read_csv("./Output/jaccard_journals_2020-09-17.csv")
jaccard_articles <- read_csv("./Output/jaccard_articles_2020-09-17.csv")

VABBdata <- read_csv("./Output/FLANDERS_2020-08-24.csv", col_types = cols(.default = "c"))
CRISTINdata <- read_csv("./Output/NORWAY_2020-08-24.csv", col_types = cols(.default = "c"))

# Prep --------------------------------------------------------------------

SSH.codes <- c(paste0("FORD_5_",1:9), paste0("FORD_6_",1:5))
SSH.codes2 <- c(paste0("FOS_5_",1:9), paste0("FOS_6_",1:5))

jaccard <- rbind(jaccard_articles, jaccard_journals)

# Define theme

theme_academic_ls <- function() {
  theme_classic() +
    theme(
      legend.position = "none",
      text = element_text(family = "Times New Roman"),
      axis.line.y = element_blank(),
      axis.line.x = element_line(color = "grey70"),
      axis.text = element_text(size = 10),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 12, color = "grey30"),
      axis.ticks = element_blank(),
      panel.grid.major.y = element_line(),
      plot.margin = margin(5, 5, 5, 5, "mm")
    )
}

# set date
currentDate <- Sys.Date()

# Boxplots on jaccard by discipline ---------------------------------------------------------
# is it plotting means and not medians?? --  YES

pos <- position_jitter(width = 0.05, seed = 2)

jaccard$pair <- as.factor(jaccard$pair)
jaccard <- jaccard %>%
  mutate(
    pair = factor(pair, levels = c("VABB-WOS", "VABB-SM", "VABB-ERIH", "NPU-WOS", "NPU-SM", "NPU-ERIH", "VABB-NPU", "NPU-VABB"))
  )

jaccard.boxplot <- jaccard %>% 
  ggplot(aes(x = level, y = mean.jaccard)) +
  geom_boxplot(aes(middle = mean(mean.jaccard)),alpha = 0.2, colour = "grey70", fill = "white", outlier.alpha = 0, notchwidth = 0.2) +
  geom_jitter(position = pos, size = 1) +
  ggrepel::geom_text_repel(aes(label = FORD), position = pos, size = 2, family = "Times New Roman", 
                           segment.color = "grey20",
                           segment.size = 0.2,
                           box.padding = unit(0.4, 'lines')) +
  theme_academic_ls() +
  theme(
    axis.text.x = element_text(color = "grey60", size = 9, vjust=-0.5),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_line(size = 0.5, linetype = "dotted"),
    strip.background = element_rect(color="grey90", fill="grey90", size=0)
  ) +
  scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.5)) +
  facet_wrap(~ pair) 

#ggsave(paste0("jaccard_boxplot_", currentDate, ".png"), jaccard.boxplot, "png", here::here("Plots"),
       width = 18, height = 20, units = "cm", dpi = 600 )

# Line plots on jaccard by discipline -------------------------------------

# to continue... divide SS and H separately? And annotate directly on the line!? 

jaccard.by.database <- jaccard %>% 
  mutate(database = ifelse(str_detect(pair, "^VABB"), "VABB", "CRISTIN"),
         pair = str_replace(pair, "^VABB-", ""),
         pair = str_replace(pair, "^NPU-", ""),
         pair = str_replace(pair, "^VABB", "VABB-NPU"),
         pair = str_replace(pair, "^NPU", "VABB-NPU"),
         FORD = factor(as.factor(FORD), levels = c("PSY", "ECON", "EDU", "SOC", "LAW", "POL", "GEO", "MEDIA", "OTHS",
                                                  "HIST", "LANG","PHIL", "ART", "OTHH")))


jaccard.by.database <- jaccard.by.database %>% 
  mutate(
    pair = factor(as.factor(pair), levels = c("WOS", "SM", "ERIH", "VABB-NPU"))
  )

# all disciplines. line plot

palette.Dark2 <- brewer.pal(8, "Set1")
Dark2.range <- colorRampPalette(palette.Dark2)

jaccard.disciplines.plot <- jaccard.by.database %>% 
  ggplot(aes(x = pair, y = mean.jaccard, group = FORD, colour = FORD)) +
  geom_line(size = 1.5, alpha = 0.8) +
  ggrepel::geom_text_repel(data = filter(jaccard.by.database, pair == "WOS"), aes(label = FORD),
                           size = 2, family = "Times New Roman",
                           segment.size = 0.2,
                           segment.alpha = 0.5,
                           nudge_x = -0.35,
                           direction = "y",
                           hjust = 1) +
  geom_point(color = "white", size = 0.5) +
  facet_grid(level ~ database) +
  theme_academic_ls() +
  scale_color_manual(values = Dark2.range(14)) +
  scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.5)) +
  theme(
    strip.background = element_rect(color="grey90", fill="grey90", size=0),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.5, linetype = "dotted")
  )

#ggsave(paste0("jaccard_disciplines_", currentDate, ".png"), jaccard.disciplines.plot , "png", here::here("Plots"),
       width = 18, height = 13, units = "cm", dpi = 600 )


# by discipline. long -- lines colored

jaccard.disciplines.plot.A <- jaccard.by.database %>% 
  ggplot(aes(x = pair, y = mean.jaccard, group = FORD, colour = mean.jaccard, label = mean.jaccard)) +
  geom_line(size = 1.5) +
  geom_point(color = "white", size = 0.5) +
  geom_text(nudge_y = 0.25, size = 3, colour = "grey70", family = "Times New Roman") +
  facet_grid(FORD ~ database + level, switch = "y") +
  theme_academic_ls() +
  scale_colour_gradient2(low = "#cc2500", mid = "#CDEDF6" , high = "#042A2B", 
                         midpoint=median(jaccard.by.database$mean.jaccard)) +
  scale_y_continuous(limits = c(-0.2, 1.3)) +
  theme(
    strip.background = element_rect(color="grey90", fill="grey90", size=0),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  )

# by discipline. long -- dots colored

jaccard.disciplines.plot.A <- jaccard.by.database %>% 
  ggplot(aes(x = pair, y = 0.5, group = FORD, color = mean.jaccard, label = mean.jaccard)) +
  geom_line(size = 0.5, colour = "grey80", linetype = "dotted") +
  geom_point(aes(size = mean.jaccard)) + #was size = 4; midpoint should be adjusted to median
  geom_text(nudge_y = 0.05, size = 2, colour = "grey10", family = "Times New Roman") +
  facet_grid(FORD ~ database + level, switch = "y") +
  scale_size_continuous(breaks=c(0,0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.50", "0.75","1"),
                        limits=c(0,1)) +
  scale_color_gradient2(low = "#cc2500", mid = "#CDEDF6", high = "#032324", 
                       midpoint=median(jaccard.by.database$mean.jaccard),
                       breaks=c(0,0.25, 0.5, 0.75 , 1),labels=c("0", "0.25", "0.50", "0.75","1"),
                       limits=c(0,1)) +
  scale_y_continuous(limits = c(-0.5, 1.3)) +
  guides(color= guide_legend("Mean of Jaccard index"), size=guide_legend("Mean of Jaccard index")) +
  theme_academic_ls() +
  #labs(colour = "Mean of Jaccard index") +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 6, color = "grey20"),
    legend.title = element_text(size = 6, color = "grey20"),
    strip.background = element_rect(color="white", fill="white", size=0),
    strip.text = element_text(size = 7, color = "grey20"),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x =  element_text(size = 7, color = "grey20", angle = 45),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(paste0("jaccard_disciplines_dots_", currentDate, ".png"), jaccard.disciplines.plot.A , "png", here::here("Plots"),
       width = 17, height = 20, units = "cm", dpi = 600 )


# Plotting the difference in share ----------------------------------------

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
} # from here: https://stackoverflow.com/questions/33524669/labeling-outliers-of-boxplots-in-r

#prep data

shares.data <- data.frame(
  "Discipline" = FLANDERS_B$Discipline[1:14],
  "FLANDERS.B" = FLANDERS_B$share.difference.VABB[1:14],
  "FLANDERS.C" = FLANDERS_C$share.difference.VABB[2:15],
  "FLANDERS.D" = FLANDERS_D$share.difference.VABB[1:14],
  "FLANDERS.E" = FLANDERS_E$share.difference.VABB[1:14],
  "NORWAY.B" = NORWAY_B$share.difference.CRISTIN[1:14],
  "NORWAY.C" = NORWAY_C$share.difference.CRISTIN[2:15],
  "NORWAY.D" = NORWAY_D$share.difference.CRISTIN[1:14],
  "NORWAY.E" = NORWAY_E$share.difference.CRISTIN[1:14]
) %>% 
  gather(dataset, value, FLANDERS.B:NORWAY.E) %>% 
  mutate(direction = ifelse(value > 0, "Decrease", "Increase")) %>% 
  mutate_if(is.numeric, abs) %>% 
  separate(dataset, c("Database", "Pair")) %>% 
  group_by(Database, Pair) %>% 
  mutate(Outlier = ifelse(is_outlier(value), as.character(Discipline), as.character(NA))) %>% 
  ungroup() %>% 
  mutate(Outlier = recode(as.factor(Outlier), FOS_5_1 = "PSY", FOS_5_4 = "SOC", FOS_5_7 = "GEO", FOS_5_9  = "OTHS"),
         Pair = recode(as.factor(Pair), B = "WOS", C = "SM", D = "ERIH", E = "VABB-NPU"))


shares.boxplots <- shares.data %>%
  #filter(Database == "FLANDERS", Pair == "B") %>% 
  ggplot(aes(x = Pair, y = value)) +
  geom_boxplot(alpha = 0.2, colour = "grey70", fill = "white", outlier.alpha = 0, notchwidth = 0.2) +
  geom_jitter(aes(color = direction), position = pos, size = 1, alpha = 0.7) +
  #geom_text(aes(label = Outlier), na.rm = TRUE, hjust = -0.3) +
  ggrepel::geom_text_repel(aes(label = Outlier), position = pos, size = 2, family = "Times New Roman", 
                           segment.color = "grey20",
                           segment.size = 0.2,
                           box.padding = unit(0.4, 'lines')) +
  scale_color_manual(values = c("#cc2500", "#29a2a6")) +
  facet_wrap(vars(Database)) +
  theme_academic_ls() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(color = "grey60", size = 9, vjust=-0.5),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.5, linetype = "dotted"),
    strip.background = element_rect(color="grey90", fill="grey90", size=0)
  )

ggsave(paste0("shares_boxplots_", currentDate, ".png"), shares.boxplots , "png", here::here("Plots"),
       width = 14, height = 10, units = "cm", dpi = 600 )