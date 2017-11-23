library(tidyverse)
library(ade4)
library(ggthemes)

## Import données (après avoir enregistré les données en .csv sur votre ordinateur)
Noyon <- read.csv2("Noyon_2000_50ansEUDCDE.csv",
                   header = T,
                   sep = ";",
                   stringsAsFactors = F)
head(Noyon)

Noyon <- Noyon %>%
  unite(Dates, Debut, Fin, sep = "-", remove = FALSE)
rownames(Noyon) <- Noyon$Dates

## fonctions
source("fonctions.R")

## test du khi-deux
Noyon %>% select(F1:F10, -F7) %>% chisq.test()

## V de Cramer pour voir l'intensité de la relation entre temps et fonctions urbaines
library(lsr)
Noyon %>% select(F1:F10, -F7) %>% cramersV() # relation globale plutôt faible : logique sachant le nb périodes
# et le fait que l'on a des fonctions urbaines qui existent de manière générale "en ville"

#### Ecarts aux pourcentages moyens ####
EPM_Noyon <- Noyon %>%
  select(F1:F10, -F7) %>%
  EPM()
write.csv(EPM_Noyon, "Noyon_EPM.csv")
EPM_Noyon$Periodes <- Noyon$Dates
EPM_Noyon$Debut <- Noyon$Debut


EPM_Noyon %>%
  gather(key = "Fonction", value = "Data", F1:F10) %>%
  ggplot() +
  geom_bar(aes(Fonction, Data), stat = "identity") +
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017") +
  ggtitle("Noyon : Les fonctions urbaines (1er-21e s.)") +
  labs(subtitle = "Écarts aux pourcentages moyens (équivaut aux écarts à l'indépendance des fréquences des fonctions par période chronologique)") +
  xlab("Fonctions urbaines") +
  ylab("") +
  facet_wrap(~reorder(Periodes, Debut)) +
  coord_flip()


#### CAH sur distance khi 2 ####
# Application sur les données de Noyon
CAH_k2_Noyon <- Noyon %>%
  select(F1:F10, -F7) %>%
  CAH_DistKhi2()
plot(CAH_k2_Noyon, hang = -1, cex = 0.6, 
     main = "Dendrogramme - Noyon (1er-21e s.)",
     xlab = "Périodes chronologiques")

## Clustering type cutree de la CAH (on fait apparaître les clusters)
# inertie
inertieNk <- sort(CAH_k2_Noyon$height, decreasing = TRUE)
plot(sort(CAH_k2_Noyon$height, decreasing = TRUE), type = "h", xlab = "Nombre de classes", ylab = "Inertie")

inertieN2k <- inertieNk/sum(inertieNk)*100
barplot(inertieN2k[1:15], col = "#454847", border = "#454847", names.arg = seq(1, 15, 1),
        xlab = "Nombre de classes",
        ylab = "Part de l'inertie totale (%)")
# donc pour le présent cas, on peut découper en 5

TypochronoNk <- cutree(CAH_k2_Noyon, k = 5)  # k = nombre de classes

# Tableau des écarts à l'indépendance
EcartNoyon <- Noyon %>%
  select(F1:F10, -F7) %>%
  TabEcart() %>%
  mutate(Cluster = factor(TypochronoNk, levels = 1:5))
rownames(EcartNoyon) <- Noyon$Dates
write.csv(EcartNoyon, "Noyon_EcartInde.csv")

EcartNoyon <- EcartNoyon %>%
  group_by(Cluster)
EcartNoyon <- EcartNoyon %>%
  select(F1:F10) %>%
  summarise_each(funs(mean))
EcartNoyon$Cluster <- c("1-250", "251-650", "651-1150", "1151-1800", "1801-2016")
EcartNoyon$Reorder <- c(1:5)
write.csv(EcartNoyon, "Noyon_EcartInde_meanCluster.csv")

EcartNoyon %>%
  gather(key = "Fonction", value = "Data", F1:F10) %>%
  ggplot() +
  geom_bar(aes(Fonction, Data, fill = Cluster), stat = "identity") +
  theme_julie() +
  scale_fill_tableau(palette = "colorblind10",  breaks = NULL) +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017") +
  ggtitle("Noyon : 1er-21e s.") +
  labs(subtitle = "Classe de périodes : CAH (en distance khi-deux)") +
  ylab("Moyennes des écarts à l'indépendance par classe") +
  facet_wrap(~reorder(Cluster, Reorder)) +
  coord_flip()

# Visualisation classes CAH en couleur
library(scales)
devtools::install_github("Larmarange/JLutils")
library(JLutils)

colPalPerso <- c("#006ba4ff","#595959ff", "#5f9ed1ff","#ff800eff", "#abababff", "green")
show_col(colPalPerso)

col_CAH_Plot <- colPalPerso[unique(TypochronoNk[CAH_k2_Noyon$order])]
A2Rplot(CAH_k2_Noyon, k = 5, boxes = FALSE, col.up = "gray50", # fonction dans JLutils (devtools)
        col.down = col_CAH_Plot,
        show.labels = TRUE, main = "Dendrogramme - Noyon (1er-21e s.)")
legend(x="topright", paste("Classe", c(2,1,5,4,3), sep=" "),
       cex=1,
       seg.len=4,
       col=col_CAH_Plot,
       pch=NA,
       lty=1,
       lwd=4,
       title = "Périodes : CAH (en distance khi-deux)")


## En normalisant les écarts à l'indépendance
## Toutes périodes : résidus de Pearson
EcartNoyonNorm <- Noyon %>%
  select(F1:F10, -F7) %>%
  TabEcartPearsonResidus() %>%
  mutate(Cluster = factor(TypochronoNk, levels = 1:5))
rownames(EcartNoyonNorm) <- Noyon$Dates
write.csv(EcartNoyonNorm, "Noyon_EcartIndeNorm.csv")

EcartNoyonNorm <- EcartNoyonNorm %>%
  group_by(Cluster)
EcartNoyonNorm <- EcartNoyonNorm %>%
  select(F1:F10) %>%
  summarise_each(funs(mean))
EcartNoyonNorm$Cluster <- c("1-250", "251-650", "651-1150", "1151-1800", "1801-2016")
EcartNoyonNorm$Reorder <- c(1:5)
write.csv(EcartNoyonNorm, "Noyon_EcartIndeNorm_meanCluster.csv")

EcartNoyonNorm %>%
  gather(key = "Fonction", value = "Data", F1:F10) %>%
  ggplot() +
  geom_bar(aes(Fonction, Data, fill = Cluster), stat = "identity") +
  theme_julie() +
  scale_fill_tableau(palette = "colorblind10",  breaks = NULL) +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017") +
  ggtitle("Noyon : 1er-21e s.") +
  labs(subtitle = "Classe de périodes : CAH (en distance khi-deux)") +
  ylab("Moyennes des écarts standardisés par classe (résidus de Pearson)") +
  facet_wrap(~reorder(Cluster, Reorder)) +
  coord_flip()


