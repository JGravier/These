library(tidyverse)
library(ggthemes)
library(readxl)
library(classInt)

theme_julie <- function(){
  julie <- theme_igray() +
    theme(plot.subtitle = element_text(face = "italic"), 
          plot.caption = element_text(size = 8), 
          axis.title = element_text(size = 12), 
          axis.text = element_text(size = 9.5), 
          plot.title = element_text(size = 15, 
                                    face = "bold", hjust = 0.5), legend.text = element_text(size = 11.5), 
          legend.title = element_text(size = 11.7), 
          plot.background = element_rect(fill = "gray97"), 
          legend.key = element_rect(fill = "gray97"), 
          legend.background = element_rect(fill = "gray97"), 
          legend.position = "bottom", legend.direction = "horizontal")
  julie
}

panel.cor <- function(x, y, digits = 2, cex.cor, ...) # selon http://www.r-bloggers.com/scatter-plot-matrices-in-r/
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # calcul coefficient de corrélation
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.7, txt)
  
  # calcul p-value
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.5, txt2)
  
  # calcul r² : ajout perso, car très utile pour nos données en SHS !
  rdeux <- lm(formula = x ~ y)
  rdeux <- summary(rdeux)$r.squared
  txt <- format(c(rdeux, 0.123456789), digits = digits)[1]
  txt <- paste("r²= ", txt, sep = "")
  text(0.5, 0.3, txt)
}





# Données espace étudié
Data <- read_excel("Data_EtmjPop_V2.xls", sheet = 2)
View(Data)

###### Analyse de la relation linéaire : pop 1836 & surface ########
# Test de significativité : corrélation, coefficient de corrélation de Pearson, Kendall ou Spearman
Cor1836 <- cor.test(Data$POP1836, Data$AREA,
                    method = "pearson",
                    conf.level = 0.95)
Cor1836

# Régression linéaire simple (sachant que x = y, écrit comme Var à expliquer ~ Var explicative)
reg1836 <- lm(formula = Data$AREA ~ Data$POP1836) # Ou intercept = B(eta), et autre = A(lpha), sachant que y = Ax + B
reg1836

# Expressivité plus fine de la régression
S1836 <- summary(reg1836)
S1836

S1836$r.squared #R² facilement identifiable

# Plot relation linéaire entre population et surface
ggplot(data = Data,
       aes(POP1836, AREA)) +
  geom_smooth(method=lm, color = "darkgrey", se = T) + # ajout de la droite de régression avec intervalle de confiance (sans: se = FALSE)
  geom_point(stat = "identity", color = "#313131") +
  geom_rug(sides ="bl") + # Densité marginale
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2018. Sources : IGN (Géoportail) & LDH-INED (Cassini.fr)") +
  xlab("Population en 1836") +
  ylab("Surface entre 1818 et 1836 (en ha)") +
  annotate("text", x = 900000,y = 4350, label = "Paris") +
  annotate("text", x = 250000,y = 3900, label = "y = 0.005x + 38.06") +
  annotate("text", x = 250000,y = 3700, label = "r² = 0.99")


###### Analyse de la relation linéaire sans Paris ########
Data1 <- filter(Data, NOM_COMM != "PARIS")
Data1

Cor1 <- cor.test(Data1$POP1836, Data1$AREA,
                 method = "pearson",
                 conf.level = 0.95)
Cor1

reg1 <- lm(formula = Data1$AREA ~ Data1$POP1836)
reg1

S1 <- summary(reg1)
S1

S1$r.squared

ggplot(data = Data1,
       aes(POP1836, AREA)) +
  geom_smooth(method=lm, color = "darkgrey", se = T) + # ajout de la droite de régression avec intervalle de confiance (sans: se = FALSE)
  geom_point(stat = "identity", color = "#313131") +
  geom_rug(sides ="bl") + # Densité marginale
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2018. Sources : IGN (Géoportail) & LDH-INED (Cassini.fr)") +
  xlab("Population en 1836") +
  ylab("Surface entre 1818 et 1836 (en ha)") +
  annotate("text", x = 50000,y = 250, label = "y = 0.0076x + 25.28") +
  annotate("text", x = 50000,y = 230, label = "r² = 0.76")


ScatterPlot_Lineaire <- as.matrix(Data[, c(3,6,7,8)])
colnames(ScatterPlot_Lineaire) <- c("Surface", "POP 1821", "POP 1836", "POP 1866")

pairs(ScatterPlot_Lineaire, upper.panel = panel.cor,
      main = "Villes et bourgs du nord de la France")


ScatterPlot_Lineaire <- as.matrix(Data1[, c(3,6,7,8)])
colnames(ScatterPlot_Lineaire) <- c("Surface", "POP 1821", "POP 1836", "POP 1866")

pairs(ScatterPlot_Lineaire, upper.panel = panel.cor,
      main = "Villes et bourgs du nord de la France (sans Paris)")

ScatterPlot_Log10 <- log10(ScatterPlot_Lineaire)

pairs(ScatterPlot_Log10, upper.panel = panel.cor,
      main = "Villes et bourgs du nord de la France (log 10, sans Paris)")

###### Transformation bi-logartihmique et ajustement puissance #####
reg2 <- lm(formula = log10(Data$AREA) ~ log10(Data$POP1836)) 
# Ou intercept = b(eta), et l'autre = a(lpha), sachant que sur linéaire y = ax + b
# en général ln(y) = aln(x) + b, soit y = e^b * x^a
# sachant que l'inverse de log10(x) = 10^x [en norme ISO 80000-2 on écrit log10 = lg]
# lg(y) = alg(x) + b, alors y = 10^b * x^a
reg2

Cor2 <- cor.test(log10(Data$POP1836), log10(Data$AREA),
                 method = "pearson",
                 conf.level = 0.95)
Cor2

# Expressivité plus fine de la régression
S2 <- summary(reg2)
S2

S2$r.squared #R² facilement identifiable

####### Plot avec transformation des axes en log10 + ajustement puissance
ggplot(data = Data,
       aes(POP1836, AREA)) +
  scale_x_log10() + # transformation en log10 des xi
  scale_y_log10() + # idem sur yi
  geom_smooth(method=lm, color = "darkgrey", se = T) + # lm sur log10
  geom_point(stat = "identity", color = "#313131") +
  geom_rug(sides ="bl") +
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2018. Sources : IGN (Géoportail) & LDH-INED (Cassini.fr)") +
  xlab("Population en 1836 (log10)") +
  ylab("Surface entre 1818 et 1836 (en ha) (log10)") +
  annotate("text", x = 900000,y = 3900, label = "Paris")


## Récupération des résidus 
ResidusLg <- function(x, y){
  reg <- lm(formula = log10(y) ~ log10(x))
  reg <- summary(reg)
  YPrimei <- 10^reg$coefficients[1,1]*x^reg$coefficients[2,1]  # On fait l'inverse de lg(y), y = 10^b * x^a
  Residus <- y - YPrimei
}

DataEspaceEtudie$Residus <- ResidusLg(DataEspaceEtudie$POP1836, DataEspaceEtudie$Etjm3)

ggplot(data = DataEspaceEtudie, 
       aes(x = reorder(NOM, -Residus), # Tri pour ordre décroissant fonction reorder
           y = Residus)) + 
  geom_bar(stat = "identity") + # Diagramme en baton
  scale_x_discrete(breaks = NULL) +
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017. Sources : IGN (Géoportail) & LDH-INED (Cassini.fr)") +
  xlab("Villes étudiées") +
  ylab("Résidus des surfaces sur ajustement puissance (en ha)")



# Sans Paris
reg2SsParis <- lm(formula = log10(Data1$AREA) ~ log10(Data1$POP1836)) 
reg2SsParis

Cor2ssP <- cor.test(log10(Data1$POP1836), log10(Data1$AREA),
                    method = "pearson",
                    conf.level = 0.95)
Cor2ssP

# Expressivité plus fine de la régression
S2ssP <- summary(reg2SsParis)
S2ssP

S2ssP$r.squared #R² facilement identifiable

####### Plot avec transformation des axes en log10 + ajustement puissance
ggplot(data = Data1,
       aes(POP1836, AREA)) +
  scale_x_log10() + # transformation en log10 des xi
  scale_y_log10() + # idem sur yi
  geom_smooth(method=lm, color = "darkgrey", se = T) + # lm sur log10
  geom_point(stat = "identity", color = "#313131") +
  geom_rug(sides ="bl") +
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2018. Sources : IGN (Géoportail) & LDH-INED (Cassini.fr)") +
  xlab("Population en 1836 (log10)") +
  ylab("Surface entre 1818 et 1836 (log10)")


Data1$Residus <- ResidusLg(Data1$POP1836, Data1$Etjm3)
head(Data1)

ggplot(data = Data1, 
       aes(x = reorder(NOM, -Residus), # Tri pour ordre décroissant fonction reorder
           y = Residus)) + 
  geom_bar(stat = "identity", color = "black") + # Diagramme en baton
  scale_x_discrete(breaks = NULL) +
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017. Sources : IGN (Géoportail) & LDH-INED (Cassini.fr)") +
  xlab("Villes étudiées") +
  ylab("Résidus des surfaces de l'ajustement puissance (en ha)")

ggplot(Data1, aes("",Residus)) + 
  geom_boxplot() +
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017. Sources : IGN (Géoportail) & LDH-INED (Cassini.fr)") +
  xlab("") +
  ylab("Résidus des surfaces de l'ajustement puissance (en ha)")


Data1 %>%
  ggplot(aes(TypeEtmj, Residus)) +
  geom_boxplot() +
  xlab("") +
  ylab("Résidus des surfaces de l'ajustement puissance (ha)") +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017. Sources : IGN (Géoportail) & LDH-INED (Cassini.fr)") +
  theme_julie()

summary(Data1$Residus)


########## IDEM mais sur les "villes" selon Etmj ##########
DataV <- filter(Data1, TYPE_LIEU == "Ville") # 72 entités villes au total
head(DataV)
nrow(DataV)

CorV <- cor.test(log10(DataV$POP1836), log10(DataV$AREA),
                 method = "pearson",
                 conf.level = 0.95)
CorV

regV <- lm(formula = log10(DataV$AREA) ~ log10(DataV$POP1836))
regV

SV <- summary(regV)
SV

SV$r.squared #R² facilement identifiable

ggplot(DataV, aes(POP1836, AREA)) +
  scale_x_log10() + # transformation en log10 des xi
  scale_y_log10() + # idem sur yi
  geom_smooth(method=lm, color = "darkgrey", se = T) + # lm sur log10
  geom_point(stat = "identity", color = "#313131") +
  geom_rug(sides ="bl") +
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017. Sources : IGN (Géoportail) & LDH-INED (Cassini.fr)") +
  xlab("Population en 1836 (log10)") +
  ylab("Surface entre 1818 et 1836 (en ha) (log10)")

ggplot(DataV, 
       aes(x = reorder(NOM, -Residus), # Tri pour ordre décroissant fonction reorder
           y = Residus)) + 
  geom_bar(stat = "identity", color = "black") + # Diagramme en baton
  scale_x_discrete(breaks = NULL) +
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017. Sources : IGN (Géoportail) & LDH-INED (Cassini.fr)") +
  xlab("Villes étudiées") +
  ylab("Résidus des surfaces de l'ajustement puissance (en ha)")

ggplot(DataV, aes("",Residus)) + 
  geom_boxplot() +
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017. Sources : IGN (Géoportail) & LDH-INED (Cassini.fr)") +
  xlab("") +
  ylab("Résidus des surfaces de l'ajustement puissance (en ha)")

ScatterPlot_LineaireVilles <- as.matrix(DataV[, c(4,9,10,11)])
colnames(ScatterPlot_LineaireVilles) <- c("Surface", "POP 1821", "POP 1836", "POP 1866")

pairs(ScatterPlot_LineaireVilles, upper.panel = panel.cor,
      main = "Les villes du nord de la France")

ScatterPlot_VillesLog10 <- log10(ScatterPlot_LineaireVilles)

pairs(ScatterPlot_VillesLog10, upper.panel = panel.cor,
      main = "Les villes du nord de la France (log 10)")


########## IDEM mais sur les "bourgs" selon Etmj ##########
DataB <- filter(Data1, TYPE_LIEU == "Bourg") # 65 entités bourgs au total
DataB

CorB <- cor.test(log10(DataB$POP1836), log10(DataB$AREA),
                 method = "pearson",
                 conf.level = 0.95)
CorB

regB <- lm(formula = log10(DataB$AREA) ~ log10(DataB$POP1836))
regB

SB <- summary(regB)
SB

SB$r.squared #R² facilement identifiable

ggplot(DataB,  aes(POP1836, AREA)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method=lm, color = "darkgrey", se = T) +
  geom_point(stat = "identity", color = "#313131") +
  geom_rug(sides ="bl") +
  theme_julie() +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017. Sources : IGN (Géoportail) & LDH-EHESS (Cassini.fr)") +
  xlab("Population en 1836 (log10)") +
  ylab("Surface entre 1818 et 1836 (en ha) (log10)")

ScatterPlot_LineaireBourgs <- as.matrix(DataB[, c(4,9,10,11)])
colnames(ScatterPlot_LineaireBourgs) <- c("Surface", "POP 1821", "POP 1836", "POP 1866")

pairs(ScatterPlot_LineaireBourgs, upper.panel = panel.cor,
      main = "Les bourgs du nord de la France")

# Sortie de tableau
SortieTableauGeneral <- data.frame(c(nrow(Data), Cor2$conf.int, Cor2$estimate, Cor2$p.value, S2$r.squared, 10^reg2$coefficients[1], reg2$coefficients[2]),
                                   c(nrow(Data1), Cor2ssP$conf.int, Cor2ssP$estimate, Cor2ssP$p.value, S2ssP$r.squared, 10^reg2SsParis$coefficients[1], reg2SsParis$coefficients[2]),
                                   c(nrow(DataV), CorV$conf.int, CorV$estimate, CorV$p.value, SV$r.squared, 10^regV$coefficients[1], regV$coefficients[2]),
                                   c(nrow(DataB), CorB$conf.int, CorB$estimate, CorB$p.value, SB$r.squared, 10^regB$coefficients[1], regB$coefficients[2]),
                                   stringsAsFactors = F)
# pour reg = ax^beta
SortieTableauGeneral <- as.data.frame(t(SortieTableauGeneral))
colnames(SortieTableauGeneral) <- c("n","confidence interval (level = 0.95)","confidence interval (level = 0.95)", "r", "p", "r²", "a", "beta")
rownames(SortieTableauGeneral) <- c("Général", "Général (sans Paris)", "Villes (sans Paris)", "Bourgs (sans Paris)")
SortieTableauGeneral <- round(SortieTableauGeneral, 3) # arrondir à 3 chiffres après la virgule

write.csv(SortieTableauGeneral, "Tableau_Correlations_Pop1836&Etjm.csv")


#### Joujou scatterplot : avec Paris ou sans ####
DataScatterplotVilles <- Data1 %>%
  filter(TypeEtmj == "Ville") %>%
  select(Etjm3, POP1793:POP1866) %>%
  rename(Surface = Etjm3) %>%
  as.matrix()

pairs(DataScatterplotVilles, upper.panel = panel.cor,
      main = "Les villes du nord de la France : Ville")

DataScatterplotVillesLog <- log10(DataScatterplotVilles)

pairs(DataScatterplotVillesLog, upper.panel = panel.cor,
      main = "Les villes du nord de la France : Ville (log 10)")


# Observation (janvier 2016) des villes & bourgs de la distribution
Data1bis <- Data1 %>%
  mutate(OrigEtmj = TYPE_LIEU) %>%
  mutate(TYPE_LIEU = "Complet") %>%
  rbind(Data1 %>% mutate(OrigEtmj = TYPE_LIEU))
Data1bis <- Data1bis %>%
  rename(Type = OrigEtmj)

Data1bis %>%
  ggplot(aes(POP1836, AREA)) +
  geom_smooth(method=lm, color = "darkgrey", fill = "lightgrey") + # lm sur log10
  geom_point(stat = "identity", aes(color = Type)) +
  scale_x_log10("Population en 1836 (log10)") +
  scale_y_log10("Surface entre 1818 et 1835 (ha) (log10)") +
  geom_rug(sides ="bl") +
  theme_julie() +
  scale_colour_tableau("colorblind10") +
  labs(caption = "J. Gravier | UMR Géographie-cités 2017. Sources : IGN (Géoportail) & LDH-EHESS (Cassini.fr)") +
  facet_wrap(~TYPE_LIEU)

# Initialement : visualisation habituelle, moins lisible je trouve
Data1 %>%
  ggplot(aes(POP1836, AREA, group = TYPE_LIEU, shape = TYPE_LIEU)) +
  geom_smooth(method=lm, color = "grey", fill = "lightgrey") + # lm sur log10
  geom_point(aes(colour = TYPE_LIEU)) +
  scale_shape_manual(values = c(24, 25)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_log10("Population en 1836 (log10)") +
  scale_y_log10("Surface entre 1818 et 1836 (ha) (log10)") +
  geom_rug(sides ="bl")
# Thibaut et Julien trouve cette visu plus claire


# Bourgs : scatterplot
BourgScatterplot <- as.matrix(DataB[, c(4,8,9,10,11)])
colnames(BourgScatterplot) <- c("Surface", "POP1793", "POP1821", "POP1836", "POP1866")

pairs(BourgScatterplot, upper.panel = panel.cor,
      main = "Les bourgs du nord de la France (carte d'État-major)")

BourgScatterplotLog <- log10(BourgScatterplot)

pairs(BourgScatterplotLog, upper.panel = panel.cor,
      main = "Les bourgs du nord de la France (log 10) (carte d'État-major)")

###### Linéaire Bourgs ######
#### Ajustement très mauvais à cause de Saint-Amand-les-eaux ####
DataB2 <- DataEspaceEtudie %>%
  filter(NOM != "SAINT-AMAND-LES-EAUX" & TypeEtmj == "Bourg")
regLinB <- lm(formula = DataB2$Etjm3 ~ DataB2$POP1836)
regLinB

SLinB <- summary(regLinB)
SLinB

SLinB$r.squared

ggplot(DataB2,
       aes(POP1836, Etjm3)) +
  geom_point(stat = "identity") +
  geom_smooth(method=lm, color = "darkgrey", se = FALSE) + # ajout de la droite de régression avec intervalle de confiance (sans: se = FALSE)
  geom_rug(sides ="bl") + # Densité marginale
  theme_julie() +
  xlab("Population en 1836") +
  ylab("Surface entre 1818 et 1836 (en ha)")