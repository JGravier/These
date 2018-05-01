#### 0K général ###
Gal <- ISIDORE_Data.interdisci[c(1:27),]
par(mfrow = c(2,2))
barplot(Gal$P1FP, xlab = "Pluridisciplinarité", ylab = "Fréquence pondérée", col = "#454847")
barplot(Gal$T1FP, xlab = "Transdisciplinarité", ylab = "Fréquence pondérée",col = "#454847")
barplot(Gal$I1FP, xlab = "Interdisciplinarité", ylab = "Fréquence pondérée", col = "#454847")
barplot(Gal$F_EfDiscipli, xlab = "Ressources par disciplines", ylab = "Fréquence", col = "#454847")

### selon archéo, géo et histoire ###
Disci <- ISIDORE_Data.interdisci[c(3,13,16),]

par(mfrow = c(2,2))
barplot(Disci$P1FP, 
        xlab = "(a) Pluridisciplinarité", ylab = "Fréquence pondérée", 
        names.arg = Disci$Discipline, col = "#454847")
barplot(Disci$T1FP, 
        xlab = "(b) Transdisciplinarité", ylab = "Fréquence pondérée",
        names.arg = Disci$Discipline, col = "#454847")
barplot(Disci$I1FP, 
        xlab = "(c) Interdisciplinarité", ylab = "Fréquence pondérée", 
        names.arg = Disci$Discipline, col = "#454847")
barplot(Disci$F_EfDiscipli, 
        xlab = "(d) Ressources par disciplines", ylab = "Fréquence", 
        names.arg = Disci$Discipline, col = "#454847")


#### Interdisciplinarité ou interdisciplinaire (+ autres mots) selon archéo, géo et histoire ####
par(mfrow = c(3,2))
barplot(Disci$P1FP, 
        xlab = "Pluridisciplinarité", ylab = "Fréquence pondérée", 
        names.arg = Disci$Discipline, col = "#F22300")
barplot(Disci$P2FP, 
        xlab = "Pluridisciplinaire", ylab = "Fréquence pondérée", 
        names.arg = Disci$Discipline, col = "#F22300")
barplot(Disci$I1FP, 
        xlab = "Interdisciplinarité", ylab = "Fréquence pondérée", 
        names.arg = Disci$Discipline, col = "#EBCC2A")
barplot(Disci$I2FP, 
        xlab = "Interdisciplinaire", ylab = "Fréquence pondérée", 
        names.arg = Disci$Discipline, col = "#EBCC2A")
barplot(Disci$T1FP, 
        xlab = "Transdisciplinarité", ylab = "Fréquence pondérée",
        names.arg = Disci$Discipline, col = "#3C9AB2")
barplot(Disci$T2FP, 
        xlab = "Transdisciplinaire", ylab = "Fréquence pondérée",
        names.arg = Disci$Discipline, col = "#3C9AB2")

