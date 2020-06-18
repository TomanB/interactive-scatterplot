### interactive plot with animal longlifety dataset ### from https://genomics.senescence.info/species/biblio.php

## load packages
library(tidyverse)
library(ggiraph)
library(rvg)
library(ggthemes)
library(scales)

## set working directory
setwd("~/your working directory")

## import data - https://genomics.senescence.info/download.html#anage
anage <- read_tsv(file = "~/path to anage_data.txt")
colnames(anage) <- str_replace_all(colnames(anage), "\\s",replacement = "_")
colnames(anage)

# remove data with low quality
anage_f <- anage %>% dplyr::filter(Data_quality == "acceptable")
colnames(anage_f)

anage_f$`Adult_weight_(kg)` = anage_f$`Adult_weight_(g)` / 1000

anage_mammals <- anage_f %>% dplyr::filter(Class == "Mammalia")
anage_birds <- anage_f %>% dplyr::filter(Class == "Aves")

# calculate pearson correlation between adult weight and max. longevity
anage_mammals_R <- cor.test(log10(anage_mammals$`Adult_weight_(kg)`), log10(anage_mammals$`Maximum_longevity_(yrs)`), method = "pearson")
anage_birds_R <- cor.test(log10(anage_birds$`Adult_weight_(kg)`), log10(anage_birds$`Maximum_longevity_(yrs)`), method = "pearson")

## plot
dat_text <- data.frame(label = c(paste0("R=",round(anage_birds_R$estimate[1],6)," "), paste0("R=",round(anage_mammals_R$estimate[1],6)," ")),
  Class   = c("Aves","Mammalia"))

ggplot() +
  geom_point(data = anage_mammals, mapping = aes(x = (`Adult_weight_(kg)`), y = `Maximum_longevity_(yrs)`, color = Class),
            size = 2, alpha = 0.5) +
  geom_point(data = anage_birds, mapping = aes(x = (`Adult_weight_(kg)`), y = `Maximum_longevity_(yrs)`, color = Class),
            size = 2, alpha = 0.5) + 
  #geom_smooth(data = anage_birds, aes(x=(`Adult_weight_(kg)`), y=(`Maximum_longevity_(yrs)`)), method="lm", size=2, fullrange = TRUE) +
  #geom_smooth(data = anage_mammals, aes(x=(`Adult_weight_(kg)`), y=(`Maximum_longevity_(yrs)`)), method="lm", size=2, fullrange = TRUE) +
  scale_x_log10(breaks = c(0.001,0.01,0.1,1,10,100,1000,10000,100000), limits = c(0.001,200000)) +
  xlab("\nadult body weight [kg]") + ylab("maximum longevity [years]\n") + facet_wrap( ~ Class, ncol=1, scales='free') +
  scale_y_log10(breaks = c(1,10,100,200,300), limits = c(1,300)) + theme_classic() + annotation_logticks(base = 10) +
  theme(plot.title = element_text(color="black", size=14, face= "bold", hjust=0.5),
        axis.title.x = element_text(color="black", size=16, hjust=0.5, face="bold"),
        axis.text.x = element_text(color="black", size=14),
        axis.text.y = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        legend.text = element_text(color="black", size=14),
        legend.title = element_text(color="black", size=16, face="bold"),
        legend.position = c(0.8,0.65), legend.background = element_blank(), strip.text.x = element_blank()) +
  scale_colour_manual(values = c("firebrick3","black"), labels = c("Birds (n=1057)","Mammals (n=868)")) +
  geom_text(data    = dat_text, mapping = aes(x = 0.01, y = 200, label = label), size = 5)

## interactive plot
mytheme_main <- theme(panel.background = element_blank(), panel.grid.major = element_line(colour = "#dddddd"), 
                      axis.ticks = element_line(colour = "#dddddd"))

mytheme_map <- theme(panel.background = element_blank(), axis.title.x = element_blank(), axis.text = element_blank(),
                     axis.line.x = element_blank(), axis.line.y = element_blank(), axis.title.y = element_blank(),
                     axis.ticks.x = element_blank(), axis.ticks.y = element_blank() )

interactive <- ggplot() +
  geom_point_interactive(data = anage_mammals, mapping = aes(x = (`Adult_weight_(kg)`), y = `Maximum_longevity_(yrs)`, color = Class, tooltip = anage_mammals$Common_name),
             size = 2, alpha = 0.4) +
  geom_point_interactive(data = anage_birds, mapping = aes(x = (`Adult_weight_(kg)`), y = `Maximum_longevity_(yrs)`, color = Class, tooltip = anage_birds$Common_name),
             size = 2, alpha = 0.4) + 
  scale_x_log10(breaks = c(0.001,0.01,0.1,1,10,100,1000,10000,100000), limits = c(0.001,200000)) +
  xlab("\nadult body weight [kg]") + ylab("maximum longevity [years]\n") + facet_wrap( ~ Class, ncol=1, scales='free') +
  scale_y_log10(breaks = c(1,10,100,200,300), limits = c(1,300)) + theme_classic() + annotation_logticks(base = 10) +
  theme(plot.title = element_text(color="black", size=12, face= "bold", hjust=0.5),
        axis.title.x = element_text(color="black", size=14, hjust=0.5, face="bold"),
        axis.text.x = element_text(color="black", size=12),
        axis.text.y = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        legend.text = element_text(color="black", size=12),
        legend.title = element_text(color="black", size=14, face="bold"),
        legend.position = c(0.8,0.7), legend.background = element_blank(), strip.text.x = element_blank()) +
  scale_colour_manual(values = c("firebrick3","black"), labels = c("Birds (n=1057)","Mammals (n=868)")) +
  geom_text(data    = dat_text, mapping = aes(x = 0.01, y = 200, label = label), size = 4)

ggiraph(code = {print(interactive + mytheme_main)})

