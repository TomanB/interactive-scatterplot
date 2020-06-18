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

anage_df <- rbind(anage_birds, anage_mammals)

## calculate basal metabolic rate
anage_df$b_met_rate <- anage_df$`Metabolic_rate_(W)` / anage_df$`Adult_weight_(g)`
  
## plot
dat_text <- data.frame(label = c(paste0("R=",round(anage_birds_R$estimate[1],6)," "), paste0("R=",round(anage_mammals_R$estimate[1],6)," ")),
  Class   = c("Aves","Mammalia"))

A <- ggplot() + geom_point(data = anage_df, mapping = aes(x = (`Adult_weight_(kg)`), y = `Maximum_longevity_(yrs)`, color = `Class`,
                                            text = paste("common name:", anage_df$Common_name, "<br>")), size = 2, alpha = 0.5) + 
  scale_x_log10(breaks = c(0.001,0.01,0.1,1,10,100,1000,10000,100000), limits = c(0.001,200000)) +
  xlab("\nadult body weight [kg]") + ylab("maximum longevity [years]\n") +
  scale_y_log10(breaks = c(1,10,100,200,300), limits = c(1,300)) + theme_classic() + annotation_logticks(base = 10) +
  theme(plot.title = element_text(color="black", size=14, face= "bold", hjust=0.5),
        axis.title.x = element_text(color="black", size=16, hjust=0.5, face="bold"),
        axis.text.x = element_text(color="black", size=14),
        axis.text.y = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        legend.text = element_text(color="black", size=14),
        legend.title = element_text(color="black", size=16, face="bold"),
        legend.position = c(0.8,0.6), legend.background = element_blank(), strip.text.x = element_blank()) +
  scale_colour_manual(values = c("firebrick3","black"), labels = c("Birds (n=1057)","Mammals (n=868)")) +
  geom_text(data    = dat_text, mapping = aes(x = 0.01, y = 200, label = label), size = 5) + 
  facet_wrap( ~ Class, ncol=1, scales='free')

## interactive plot
ggplotly(A)


# plot metabolic rate vs longevity
B <- ggplot() +
  geom_point(data = anage_df, mapping = aes(x = `b_met_rate`, y = `Maximum_longevity_(yrs)`, color = `Class`,
                                            text = paste("common name:", anage_df$Common_name, "<br>")), size = 2, alpha = 0.5) + 
  scale_x_log10(breaks = c(0.0001,0.001,0.01,0.1), limits = c(0.0001,0.1)) +
  xlab("\nbasal metabolic rate [W/kg]") + ylab("maximum longevity [years]\n") +
  scale_y_log10(breaks = c(1,10,50,100), limits = c(1,100)) + theme_classic() + annotation_logticks(base = 10) +
  theme(plot.title = element_text(color="black", size=14, face= "bold", hjust=0.5),
        axis.title.x = element_text(color="black", size=16, hjust=0.5, face="bold"),
        axis.text.x = element_text(color="black", size=14),
        axis.text.y = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        legend.text = element_text(color="black", size=14),
        legend.title = element_text(color="black", size=16, face="bold"),
        legend.position = c(0.2,0.7), legend.background = element_blank(), strip.text.x = element_blank()) +
  scale_colour_manual(values = c("firebrick3","black"), labels = c("Birds (n=1057)","Mammals (n=868)")) +
  #geom_text(data    = dat_text, mapping = aes(x = 0.01, y = 200, label = label), size = 5) + 
  facet_wrap( ~ Class, ncol=1, scales='free')

## interactive plot
ggplotly(B)
