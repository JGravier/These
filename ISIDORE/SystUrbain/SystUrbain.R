library(tidyverse)

#### Général ####
SystUrbain <- read.csv("Data_ISIDORE_SystUrbain.csv",
                       header = TRUE,
                       sep = ";",
                       dec = ",")

General <- SystUrbain[c(1:27),]
General <- General %>%
  gather(key = Expression, value = "Value", SUFP, SdVFP)

ggplot(General, aes(Value)) +
  geom_histogram(binwidth = 5, color = "white") +
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017. Sources : ISIDORE, 29/11/2016") +
  ggtitle("Distribution générale des expressions") +
  xlab("Fréquence pondérée") + 
  ylab("Nombre de discipline") +
  facet_wrap(~Expression)

#### Sup à 5 et 10% ####
filter(General, Value > 5)
filter(General, Value > 10)

# Visualisation archéo & géo
Disci <- SystUrbain[c(3,13),]
Disci$Discipline <- c("Archéologie", "Géographie")

Disci %>%
  gather(key = Expression, value = "Value", SUFP, SdVFP) %>%
  ggplot(aes(Discipline, Value)) +
  geom_bar(stat = "identity") +
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017. Sources : ISIDORE, 29/11/2016") +
  ggtitle("Expressions en archéologie et en géographie") +
  ylab("Fréquence pondérée") + 
  xlab("") +
  facet_wrap(~Expression)
