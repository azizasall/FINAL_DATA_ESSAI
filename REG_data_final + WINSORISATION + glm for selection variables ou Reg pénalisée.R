# final folder à remettre au prof


rm(list=ls())


# Importer base de données ------------------------------------------------

variables_2000_1 <- read.csv("base_de_donnees_2000.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")


variables_2007_1 <- read.csv("base_de_donnees_2007.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")


variables_2008_1 <- read.csv("base_de_donnees_2008.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")


variables_2009_1 <- read.csv("base_de_donnees_2009.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")


variables_2016_1 <- read.csv("base_de_donnees_2016.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")


variables_2018_1 <- read.csv("base_de_donnees_2018.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")


variables_2019_1 <- read.csv("base_de_donnees_2019.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")






# ENLEVONS LA PREMIERE COLONNE --------------------------------------------

variables_2000 <- variables_2000_1[-1]
variables_2007 <- variables_2007_1[-1]
variables_2008 <- variables_2008_1[-1]
variables_2009 <- variables_2009_1[-1]
variables_2016 <- variables_2016_1[-1]
variables_2018 <- variables_2018_1[-1]
variables_2019 <- variables_2019_1[-1]




# METTONS EN FACTOR LA VARIABLE COTE DE CREDIT ----------------------------

variables_2000$Cote_credit <- factor(variables_2000$Cote_credit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2000$Cote_credit)

variables_2007$Cote_credit <- factor(variables_2007$Cote_credit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2007$Cote_credit)

variables_2008$Cote_credit <- factor(variables_2008$Cote_credit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2008$Cote_credit)

variables_2009$Cote_credit <- factor(variables_2009$Cote_credit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2009$Cote_credit)

variables_2016$Cote_credit <- factor(variables_2016$Cote_credit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2016$Cote_credit)

variables_2018$Cote_credit <- factor(variables_2018$Cote_credit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2018$Cote_credit)

variables_2019$Cote_credit <- factor(variables_2019$Cote_credit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2019$Cote_credit)



class(variables_2019$Cote_credit)
levels(variables_2000$Cote_credit)





# METTONS EN FACTOR firm_inv et firm_spec ---------------------------------

 

variables_2000$binaire_cote_credit_2000 <- factor(variables_2000$binaire_cote_credit_2000, levels = c("firm_inv", "firm_spec"))
variables_2007$binaire_cote_credit_2007 <- factor(variables_2007$binaire_cote_credit_2007, levels = c("firm_inv", "firm_spec"))
variables_2008$binaire_cote_credit_2008 <- factor(variables_2008$binaire_cote_credit_2008, levels = c("firm_inv", "firm_spec"))
variables_2009$binaire_cote_credit_2009 <- factor(variables_2009$binaire_cote_credit_2009, levels = c("firm_inv", "firm_spec"))
variables_2016$binaire_cote_credit_2016 <- factor(variables_2016$binaire_cote_credit_2016, levels = c("firm_inv", "firm_spec"))
variables_2018$binaire_cote_credit_2018 <- factor(variables_2018$binaire_cote_credit_2018, levels = c("firm_inv", "firm_spec"))
variables_2019$binaire_cote_credit_2019 <- factor(variables_2019$binaire_cote_credit_2019, levels = c("firm_inv", "firm_spec"))









# to delete : ou simplement comme en factor quand on le plot, R le met directement en barre
plot(variables_2019$Cote_credit)




# REPRESNTATION DIAGRAMME EN BARRE COTE DE CREDIT -------------------------


library(ggplot2)

library(dplyr)
count_data_2000 <- variables_2000 %>% 
  count(Cote_credit)

ggplot(count_data_2000, aes(x=Cote_credit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=-0.500, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2000", u="Nombre d'observation",
       title = "")






library(dplyr)
count_data_2007 <- variables_2007 %>% 
  count(Cote_credit)

ggplot(count_data_2007, aes(x=Cote_credit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=-0.500, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2007", u="Nombre d'observation",
       title = "")





library(dplyr)
count_data_2008 <- variables_2008 %>% 
  count(Cote_credit)

ggplot(count_data_2008, aes(x=Cote_credit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=-0.500, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2008", u="Nombre d'observation",
       title = "")





library(dplyr)
count_data_2009 <- variables_2009 %>% 
  count(Cote_credit)

ggplot(count_data_2009, aes(x=Cote_credit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=-0.500, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2009", u="Nombre d'observation",
       title = "")




library(dplyr)
count_data_2016 <- variables_2016 %>% 
  count(Cote_credit)

ggplot(count_data_2016, aes(x=Cote_credit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=-0.500, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2016", u="Nombre d'observation",
       title = "")




library(dplyr)
count_data_2018 <- variables_2018 %>% 
  count(Cote_credit)

ggplot(count_data_2018, aes(x=Cote_credit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=-0.500, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2018", u="Nombre d'observation",
       title = "")




library(dplyr)
count_data_2019 <- variables_2019 %>% 
  count(Cote_credit)

ggplot(count_data_2019, aes(x=Cote_credit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=0.00, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2019", u="Nombre d'observation",
       title = "")





# delete partout line avec fix





#rm(list = ls())


# CONVERTISSONS LA VOLATILITE MENSUELLE EN ANNEE   

volatilite_annuelle <- variables_2000$volalitilite_30_jours * sqrt(12)
dim(variables_2000)
#fix(variables_2000)

variables_2000_x <- cbind(variables_2000[-19], volatilite_annuelle)
dim(variables_2000_x)
#fix(variables_2000_x)





volatilite_annuelle <- variables_2007$volalitilite_30_jours * sqrt(12)
dim(variables_2007)
#fix(variables_2007)

variables_2007_x <- cbind(variables_2007[-19], volatilite_annuelle)
dim(variables_2007_x)
#fix(variables_2007_x)




volatilite_annuelle <- variables_2008$volalitilite_30_jours * sqrt(12)
dim(variables_2008)
#fix(variables_2008)

variables_2008_x <- cbind(variables_2008[-19], volatilite_annuelle)
dim(variables_2008_x)
#fix(variables_2008_x)





volatilite_annuelle <- variables_2009$volalitilite_30_jours * sqrt(12)
dim(variables_2009)
#fix(variables_2009)

variables_2009_x <- cbind(variables_2009[-20], volatilite_annuelle)
dim(variables_2009_x)
#fix(variables_2009_x)





volatilite_annuelle <- variables_2016$volalitilite_30_jours * sqrt(12)
dim(variables_2016)
#fix(variables_2016)

variables_2016_x <- cbind(variables_2016[-20], volatilite_annuelle)
dim(variables_2016_x)
#fix(variables_2016_x)







volatilite_annuelle <- variables_2018$volalitilite_30_jours * sqrt(12)
dim(variables_2018)
#fix(variables_2018)

variables_2018_x <- cbind(variables_2018[-23], volatilite_annuelle)
dim(variables_2018_x)
#fix(variables_2018_x)






volatilite_annuelle <- variables_2019$volalitilite_30_jours * sqrt(12)
dim(variables_2019)
#fix(variables_2019)

variables_2019_x <- cbind(variables_2019[-22], volatilite_annuelle)
dim(variables_2019_x)
#fix(variables_2019_x)






















# to delete
# NUAGE DE POINTS :  --> tendance, not need
#-->not need boxplot fait l'affaire pr données extrèmes
# pour see la corrélation avec nuage de points






# ---> plot(variables_2019_x[, 3:ncol(variables_2019_x)]) # but deja fait dans matrice de corrélation that why not take account variables X corrélées entre elles

# ---> plot(variables_2019_x[, 3:4], xlab = "", ylab = "")
# ---> abline(lm(variables_2019_x[,3]~ variables_2019_x[,4], data = variables_2019_x), col = "red", lwd = 2)





# to delete 
# peut t'on utiliser k-validation croisée avec logit modèle : oui mais only regression simple






















# Traitement des valeurs extrèmes par WINSORISATION -----------------------


variables_2000_wins <- variables_2000_x[,-c(1,2)]
variables_2007_wins <- variables_2007_x[,-c(1,2)]
variables_2008_wins <- variables_2008_x[,-c(1,2)]
variables_2009_wins <- variables_2009_x[,-c(1,2)]
variables_2016_wins <- variables_2016_x[,-c(1,2)]
variables_2018_wins <- variables_2018_x[,-c(1,2)]
variables_2019_wins <- variables_2019_x[,-c(1,2)]



library(DescTools)

variables_2000_wins[]<-lapply(variables_2000_wins, Winsorize, minval = NULL, maxval = NULL, probs = c(0.05, 0.95), na.rm = FALSE)
variables_2007_wins[]<-lapply(variables_2007_wins, Winsorize, minval = NULL, maxval = NULL, probs = c(0.05, 0.95), na.rm = FALSE)
variables_2008_wins[]<-lapply(variables_2008_wins, Winsorize, minval = NULL, maxval = NULL, probs = c(0.05, 0.95), na.rm = FALSE)
variables_2009_wins[]<-lapply(variables_2009_wins, Winsorize, minval = NULL, maxval = NULL, probs = c(0.05, 0.95), na.rm = FALSE)
variables_2016_wins[]<-lapply(variables_2016_wins, Winsorize, minval = NULL, maxval = NULL, probs = c(0.05, 0.95), na.rm = FALSE)
variables_2018_wins[]<-lapply(variables_2018_wins, Winsorize, minval = NULL, maxval = NULL, probs = c(0.05, 0.95), na.rm = FALSE)
variables_2019_wins[]<-lapply(variables_2019_wins, Winsorize, minval = NULL, maxval = NULL, probs = c(0.05, 0.95), na.rm = FALSE)




# Vérification pour 2018 :
# comparaison des Boxplots après winsorisation --> porter attention sur l'échelle entre les 2 plots

par(mfrow = c(1, 2))
boxplot(variables_2018_x$Marge_beneficiaire_nette, xlab = "Marge_beneficiaire_nette")
boxplot(variables_2018_wins$Marge_beneficiaire_nette, xlab = "Marge_beneficiaire_nette_WINS")


# vérification
variables_2018_x$Marge_sur_EBITDA == variables_2018_wins$Marge_sur_EBITDA
cbind(variables_2018_x$Marge_sur_EBITDA, variables_2018_wins$Marge_sur_EBITDA)
#-->





boxplot(variables_2018_x$Marge_sur_EBITDA, xlab = "Marge_sur_EBITDA")
boxplot(variables_2018_wins$Marge_sur_EBITDA, xlab = "Marge_sur_EBITDA_WINS")


boxplot(variables_2018_x$Marge_sur_EBIT, xlab = "Marge_sur_EBIT")
boxplot(variables_2018_wins$Marge_sur_EBIT, xlab = "Marge_sur_EBIT_WINS")


boxplot(variables_2018_x$Rendement_sur_cap_prop, xlab = "Rendement_sur_cap_prop")
boxplot(variables_2018_wins$Rendement_sur_cap_prop, xlab = "Rendement_sur_cap_prop_WINS")

boxplot(variables_2018_x$Rendement_sur_actif, xlab = "Rendement_sur_actif")
boxplot(variables_2018_wins$Rendement_sur_actif, xlab = "Rendement_sur_actif_WINS")

boxplot(variables_2018$Croissance_adj_des_Ben_ann, xlab = "Croissance_adj_des_Ben_ann")
boxplot(variables_2018_wins$Croissance_adj_des_Ben_ann, xlab = "Croissance_adj_des_Ben_ann_WINS")

boxplot(variables_2018_x$Croissance_tot_actif, xlab = "Croissance_tot_actif")
boxplot(variables_2018_wins$Croissance_tot_actif, xlab = "Croissance_tot_actif_WINS")

boxplot(variables_2018_x$ratio_ben_avt_impot_sur_frais_int, xlab = "ratio_ben_avt_impot_sur_frais_int")
boxplot(variables_2018_wins$ratio_ben_avt_impot_sur_frais_int, xlab = "ratio_ben_avt_impot_sur_frais_int_WINS")

boxplot(variables_2018_x$ratio_tot_dette_sur_tot_actif, xlab = "ratio_tot_dette_sur_tot_actif")
boxplot(variables_2018_wins$ratio_tot_dette_sur_tot_actif, xlab = "ratio_tot_dette_sur_tot_actif_WINS")

boxplot(variables_2018_x$ratio_tot_liab_sur_tot_actif, xlab = "ratio_tot_liab_sur_tot_actif")
boxplot(variables_2018_wins$ratio_tot_liab_sur_tot_actif, xlab = "ratio_tot_liab_sur_tot_actif_WINS")

boxplot(variables_2018_x$ratio_B_non_rep_sur_Tot_actif, xlab = "ratio_B_non_rep_sur_Tot_actif")
boxplot(variables_2018_wins$ratio_B_non_rep_sur_Tot_actif, xlab = "ratio_B_non_rep_sur_Tot_actif_WINS")

boxplot(variables_2018_x$ratio_Flux_de_TR_expl_sur_passif_cour, xlab = "ratio_Flux_de_TR_expl_sur_passif_cour")
boxplot(variables_2018_wins$ratio_Flux_de_TR_expl_sur_passif_cour, xlab = "ratio_Flux_de_TR_expl_sur_passif_cour_WINS")

boxplot(variables_2018_x$ratio_actuel, xlab = "ratio_actuel")
boxplot(variables_2018_wins$ratio_actuel, xlab = "ratio_actuel_WINS")

boxplot(variables_2018_x$Ratio_de_liquid_reduite, xlab = "Ratio_de_liquid_reduite")
boxplot(variables_2018_wins$Ratio_de_liquid_reduite, xlab = "Ratio_de_liquid_reduite_WINS")

boxplot(variables_2018_x$Ratio_de_liquidite, xlab = "Ratio_de_liquidite")
boxplot(variables_2018_wins$Ratio_de_liquidite, xlab = "Ratio_de_liquidite_WINS")

boxplot(variables_2018_x$Ratio_Fonds_de_roulmt_sur_ventes, xlab = "Ratio_Fonds_de_roulmt_sur_ventes")
boxplot(variables_2018_wins$Ratio_Fonds_de_roulmt_sur_ventes, xlab = "Ratio_Fonds_de_roulmt_sur_ventes_WINS")

boxplot(variables_2018_x$ratio_Fonds_de_roulement_sur_tot_actif, xlab = "ratio_Fonds_de_roulement_sur_tot_actif")
boxplot(variables_2018_wins$ratio_Fonds_de_roulement_sur_tot_actif, xlab = "ratio_Fonds_de_roulement_sur_tot_actif_WINS")

boxplot(variables_2018_x$Total_actif, xlab = "Total_actif")
boxplot(variables_2018_wins$Total_actif, xlab = "Total_actif_WINS")

boxplot(variables_2018_x$Marge_operationnelle, xlab = "Marge_operationnelle")
boxplot(variables_2018_wins$Marge_operationnelle, xlab = "Marge_operationnelle_WINS")

boxplot(variables_2018_x$Beta_applique, xlab = "Beta_applique")
boxplot(variables_2018_wins$Beta_applique, xlab = "Beta_applique_WINS")

boxplot(variables_2018_x$volatilite_annuelle, xlab = "volatilite_annuelle")
boxplot(variables_2018_wins$volatilite_annuelle, xlab = "volatilite_annuelle_WINS")




# --------------------->
# WINSORISONS NOS VARIABLES (Traitement valeur extrèmes):

# --------------------->











# a ajuster

variables_2000 <- cbind(variables_2000_x[, c(1,2)], variables_2000_wins)
variables_2007 <- cbind(variables_2007_x[, c(1,2)], variables_2007_wins)
variables_2008 <- cbind(variables_2008_x[, c(1,2)], variables_2008_wins)
variables_2009 <- cbind(variables_2009_x[, c(1,2)], variables_2009_wins)
variables_2016 <- cbind(variables_2016_x[, c(1,2)], variables_2016_wins)
variables_2018 <- cbind(variables_2018_x[, c(1,2)], variables_2018_wins)
variables_2019 <- cbind(variables_2019_x[, c(1,2)], variables_2019_wins)







# STATISTIQUES DESCRIPTIVES ---------------------------------------------------


# (1) CALCULONS LA MATRICE DE CORRELATION  ------------------------------------


library(ggplot2)
library(ggcorrplot)


# corrélation 2019
corr_2019 = round(cor(variables_2019[,c(-1,-2)]), 1)

#dev.new()

ggcorrplot(corr_2019, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("tomato2", "thistle", "springgreen3"),
           ggtheme = theme_bw)




# corrélation 2018
corr_2018 = round(cor(variables_2018[,c(-1,-2)]), 1)


#POUR TENIR COMPTE DE : marge bénéfciaie et beta appliqué car pas présent dans 2019
ggcorrplot(corr_2018, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("tomato2", "thistle", "springgreen3"),
           ggtheme = theme_bw)








# (2) STATISTIQUES DESCRIPTIVES -------------------------------------------


names(variables_2019)



# Table T_x2 :

table(variables_2019$Cote_credit) #ça me compte le nombre de chaque élément de chaque facter
#AAA j'en ai que 2 donc si je le #fixe comme reférence only ça ne fera pas de sens
#pour les prédictions donc dans multinomial logit reg #fixer AAA, AA et A comme ref


table(variables_2000$Cote_credit)
table(variables_2007$Cote_credit)
table(variables_2008$Cote_credit)
table(variables_2009$Cote_credit)
table(variables_2016$Cote_credit)
table(variables_2018$Cote_credit)
table(variables_2019$Cote_credit)


length(variables_2000$Cote_credit)
length(variables_2007$Cote_credit)
length(variables_2008$Cote_credit)
length(variables_2009$Cote_credit)
length(variables_2016$Cote_credit)
length(variables_2018$Cote_credit)
length(variables_2019$Cote_credit)




summary(variables_2000)
summary(variables_2007)
summary(variables_2008)
summary(variables_2009)
summary(variables_2016)
summary(variables_2018)
summary(variables_2019)




#Construction Table Stat Descriptives pour une meilleur importation des résultats

# 2019
as.data.frame(variables_2019)
matrixx_2019 <- matrix(0, ncol(variables_2019)-2, 6)

rownames(matrixx_2019) <- colnames(variables_2019[c(-1,-2)])
colnames(matrixx_2019) <- c("Moyenne", "Ecart_Type_en_%", "Min", "Max", "1er_quartile", "3eme_quartile")

for(j in 1:nrow(matrixx_2019)){ #19
  matrixx_2019[j,1] <- round(mean(as.numeric(unlist(variables_2019[,(j+2)]))), digits = 2)
  matrixx_2019[j,2] <- round(sqrt(var(variables_2019[ ,(j+2)])), digits = 1)
  matrixx_2019[j,3] <- round(min(variables_2019[ ,(j+2)]), digits = 2)
  matrixx_2019[j,4] <- round(max(variables_2019[ ,(j+2)]), digits = 2)
  matrixx_2019[j,5] <- round(quantile(variables_2019[ ,(j+2)], 0.25), digits = 2)
  matrixx_2019[j,6] <- round(quantile(variables_2019[ ,(j+2)], 0.75), digits = 2)
}

print(matrixx_2019)

# vérification calcul écart type   -> les autres peuvent être vérifiés avec summary
apply(variables_2019[,c(-1,-2)], 2, sd)

#rm(list = ls())


# 2018
as.data.frame(variables_2018)
matrixx_2018 <- matrix(0, ncol(variables_2018)-2, 6)

rownames(matrixx_2018) <- colnames(variables_2018[c(-1,-2)])
colnames(matrixx_2018) <- c("Moyenne", "Ecart_Type_en_%", "Min", "Max", "1er_quartile", "3eme_quartile")

for(j in 1:nrow(matrixx_2018)){ #19
  matrixx_2018[j,1] <- round(mean(as.numeric(unlist(variables_2018[,(j+2)]))), digits = 2)
  matrixx_2018[j,2] <- round(sqrt(var(variables_2018[ ,(j+2)])), digits = 1)
  matrixx_2018[j,3] <- round(min(variables_2018[ ,(j+2)]), digits = 2)
  matrixx_2018[j,4] <- round(max(variables_2018[ ,(j+2)]), digits = 2)
  matrixx_2018[j,5] <- round(quantile(variables_2018[ ,(j+2)], 0.25), digits = 2)
  matrixx_2018[j,6] <- round(quantile(variables_2018[ ,(j+2)], 0.75), digits = 2)
}

print(matrixx_2018)

# vérification calcul écart type   -> les autres peuvent être vérifiés avec summary
round(apply(variables_2018[,c(-1,-2)], 2, sd), digits = 3)








str(variables_2000)
str(variables_2007)
str(variables_2008)
str(variables_2009)
str(variables_2016)
str(variables_2018)
str(variables_2019)





# (3) ANALYSE UNIVARIEE ---------------------------------------------------


#2019 

# Régression logit simple firme d'investissement en fonction firme spécualtive

library(nnet)

# Définissions firme spéculative comme level
variables_2019$binaire_cote_credit_2019 <- relevel(variables_2019$binaire_cote_credit_2019, ref = "firm_inv")




 
modele_uvni_2019_Marge_sur_EBITDA <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$Marge_sur_EBITDA, data = variables_2019)

library(afex)
set_sum_contrasts()
library(car)
round(anova_1 <- Anova(modele_uvni_2019_Marge_sur_EBITDA,type="III"), digits = 5)






modele_uvni_2019_Marge_sur_EBIT <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$Marge_sur_EBIT, data = variables_2019)
anova_2 <- Anova(modele_uvni_2019_Marge_sur_EBIT,type="III")

modele_uvni_2019_Rendement_sur_cap_prop <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$Rendement_sur_cap_prop, data = variables_2019)
round(anova_3 <- Anova(modele_uvni_2019_Rendement_sur_cap_prop,type="III"), digits = 3)


modele_uvni_2019_Rendement_sur_actif<- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$Rendement_sur_actif, data = variables_2019)
anova_4 <- Anova(modele_uvni_2019_Rendement_sur_actif,type="III")

modele_uvni_2019_Croissance_adj_des_Bén_ann <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$Croissance_adj_des_Ben_ann, data = variables_2019)
anova_5 <- Anova(modele_uvni_2019_Croissance_adj_des_Bén_ann,type="III")

modele_uvni_2019_Croissance_tot_actif <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$Croissance_tot_actif, data = variables_2019)
anova_6 <- Anova(modele_uvni_2019_Croissance_tot_actif,type="III")

modele_uvni_2019_ratio_ben_avt_impot_sur_frais_int <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$ratio_ben_avt_impot_sur_frais_int, data = variables_2019)
anova_7 <- Anova(modele_uvni_2019_ratio_ben_avt_impot_sur_frais_int,type="III")

modele_uvni_2019_ratio_tot_dette_sur_tot_actif <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$ratio_tot_dette_sur_tot_actif, data = variables_2019)
anova_8 <- Anova(modele_uvni_2019_ratio_tot_dette_sur_tot_actif,type="III")

modele_uvni_2019_ratio_tot_liab_sur_tot_actif <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$ratio_tot_liab_sur_tot_actif, data = variables_2019)
anova_9 <- Anova(modele_uvni_2019_ratio_tot_liab_sur_tot_actif,type="III")

modele_uvni_2019_ratio_B_non_rep_sur_Tot_actif <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$ratio_B_non_rep_sur_Tot_actif, data = variables_2019)
anova_10 <- Anova(modele_uvni_2019_ratio_B_non_rep_sur_Tot_actif,type="III")

modele_uvni_2019_ratio_Flux_de_TR_expl_sur_passif_cour <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$ratio_Flux_de_TR_expl_sur_passif_cour, data = variables_2019)
anova_11 <- Anova(modele_uvni_2019_ratio_Flux_de_TR_expl_sur_passif_cour,type="III")

modele_uvni_2019_ratio_actuel <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$ratio_actuel, data = variables_2019)
anova_12 <- Anova(modele_uvni_2019_ratio_actuel,type="III")

modele_uvni_2019_Ratio_de_liquid_réduite <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$Ratio_de_liquid_reduite, data = variables_2019)
anova_13 <- Anova(modele_uvni_2019_Ratio_de_liquid_réduite,type="III")

modele_uvni_2019_Ratio_de_liquidité <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$Ratio_de_liquidite, data = variables_2019)
anova_14 <- Anova(modele_uvni_2019_Ratio_de_liquidité,type="III")

modele_uvni_2019_Ratio_Fonds_de_roulmt_sur_ventes <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$Ratio_Fonds_de_roulmt_sur_ventes, data = variables_2019)
anova_15 <- Anova(modele_uvni_2019_Ratio_Fonds_de_roulmt_sur_ventes,type="III")

modele_uvni_2019_ratio_Fonds_de_roulement_sur_tot_actif <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$ratio_Fonds_de_roulement_sur_tot_actif, data = variables_2019)
anova_16 <- Anova(modele_uvni_2019_ratio_Fonds_de_roulement_sur_tot_actif,type="III")

modele_uvni_2019_Total_actif <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$Total_actif, data = variables_2019)
anova_17 <- Anova(modele_uvni_2019_Total_actif,type="III")

modele_uvni_2019_Marge_opérationnelle <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$Marge_operationnelle, data = variables_2019)
anova_18 <- Anova(modele_uvni_2019_Marge_opérationnelle,type="III")

modele_uvni_2019_Beta_applique <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$Beta_applique, data = variables_2019)
anova_19 <- Anova(modele_uvni_2019_Beta_applique,type="III")





#check it aand  delete it # normalement already done
modele_uvni_2019_Volatilite_annuelle <- multinom(variables_2019$binaire_cote_credit_2019~variables_2019$volatilite_annuelle, data = variables_2019)
anova_20 <- Anova(modele_uvni_2019_Volatilite_annuelle,type="III")


# to delete
#rm(list = ls())








# CALCUL DE LA MOYENNE ET DE L'ECART-TYPE PAR CATEGORIE ET PAR VARIABLE

# 2019
library(dplyr)

variables_2019_firm_inv <- variables_2019 %>%
  filter(binaire_cote_credit_2019=="firm_inv")


variables_2019_firm_spec <- variables_2019 %>%
  filter(binaire_cote_credit_2019=="firm_spec")



# Table : visualisation résultats analyses univariées
matrixx_2 <- matrix(0, ncol(variables_2019)-2, 6)

rownames(matrixx_2) <- colnames(variables_2019[c(-1,-2)])
colnames(matrixx_2) <- c("Moyenne_Firme_d_investissement", "Moyenne_Firme_spéculative", "Ecart_type_Firme_d_investissement", "Ecart_type_Firme_spéculative", "Wald_(_chi_2_)", "Pr_>_chi_2")

for(j in 1:nrow(matrixx_2)){ #19
  matrixx_2[j,1] <- round(mean(as.numeric(unlist(variables_2019_firm_inv[,(j+2)]))), digits = 2)
  matrixx_2[j,2] <- round(mean(as.numeric(unlist(variables_2019_firm_spec[,(j+2)]))), digits = 2)
  
  matrixx_2[j,3] <- round(sqrt(var(variables_2019_firm_inv[ ,(j+2)])), digits = 1)
  matrixx_2[j,4] <- round(sqrt(var(variables_2019_firm_spec[ ,(j+2)])), digits = 1)
  
}

matrixx_2


# VERIFIACTION POUR MOYENNE ET ECART-TYPE

# MEAN
mean_by_firm_inv <- colMeans(variables_2019_firm_inv[,c(-1, -2)])
mean_by_firm_spec <- colMeans(variables_2019_firm_spec[,c(-1, -2)])

# STD_DEV
std_dv_by_firm_inv_1 <- apply(variables_2019_firm_inv[,c(-1, -2)], 2, sd)
std_dv_by_firm_spec_1 <- apply(variables_2019_firm_spec[,c(-1, -2)], 2, sd)






# Remplissons la colonne Chisquare de notre tableau

matrixx_2[1,5] <- anova_1$`LR Chisq`
matrixx_2[2,5] <- anova_2$`LR Chisq`
matrixx_2[3,5] <- anova_3$`LR Chisq`
matrixx_2[4,5] <- anova_4$`LR Chisq`
matrixx_2[5,5] <- anova_5$`LR Chisq`
matrixx_2[6,5] <- anova_6$`LR Chisq`
matrixx_2[7,5] <- anova_7$`LR Chisq`
matrixx_2[8,5] <- anova_8$`LR Chisq`
matrixx_2[9,5] <- anova_9$`LR Chisq`
matrixx_2[10,5] <- anova_10$`LR Chisq`
matrixx_2[11,5] <- anova_11$`LR Chisq`
matrixx_2[12,5] <- anova_12$`LR Chisq`
matrixx_2[13,5] <- anova_13$`LR Chisq`
matrixx_2[14,5] <- anova_14$`LR Chisq`
matrixx_2[15,5] <- anova_15$`LR Chisq`
matrixx_2[16,5] <- anova_16$`LR Chisq`
matrixx_2[17,5] <- anova_17$`LR Chisq`
matrixx_2[18,5] <- anova_18$`LR Chisq`
matrixx_2[19,5] <- anova_19$`LR Chisq`
matrixx_2[20,5] <- anova_20$`LR Chisq`


# Remplissons la colonne Pr > chiSquare
width = 3

matrixx_2[1,6] <- round ( anova_1$`Pr(>Chisq)`, digits = 3)
matrixx_2[2,6] <- anova_2$`Pr(>Chisq)`
matrixx_2[3,6] <- anova_3$`Pr(>Chisq)`
matrixx_2[4,6] <- anova_4$`Pr(>Chisq)`
matrixx_2[5,6] <- anova_5$`Pr(>Chisq)`
matrixx_2[6,6] <- anova_6$`Pr(>Chisq)`
matrixx_2[7,6] <- anova_7$`Pr(>Chisq)`
matrixx_2[8,6] <- anova_8$`Pr(>Chisq)`
matrixx_2[9,6] <- anova_9$`Pr(>Chisq)`
matrixx_2[10,6] <- anova_10$`Pr(>Chisq)`
matrixx_2[11,6] <- anova_11$`Pr(>Chisq)`
matrixx_2[12,6] <- anova_12$`Pr(>Chisq)`
matrixx_2[13,6] <- anova_13$`Pr(>Chisq)`
matrixx_2[14,6] <- anova_14$`Pr(>Chisq)`
matrixx_2[15,6] <- anova_15$`Pr(>Chisq)`
matrixx_2[16,6] <- anova_16$`Pr(>Chisq)`
matrixx_2[17,6] <- anova_17$`Pr(>Chisq)`
matrixx_2[18,6] <- anova_18$`Pr(>Chisq)`
matrixx_2[19,6] <- anova_19$`Pr(>Chisq)`






round(matrixx_2, digits = 3)









# ON SAUTE POUR AVANCER CAR NOT IMPORTANT POUR LA SUITE
# A REVOIR 

# ----------> to delete : pour genre graphe : voir fichier wword AU PROPRE 

# impossible pour moi car mes xlab sont numérique
# apres analyse univarié

# from en bas

predict_cote_credit_2019 <- predict(modele_uvni_2019_Volatilite_annuelle, variables_2019)
# from en bas


plot(variables_2019$volatilite_annuelle,  predict_cote_credit_2019)
xtt <-  variables_2019$volatilite_annuelle
prob <-  modele_uvni_2019_Volatilite_annuelle$fitted.values
lines(xtt[order(xtt)], prob[order(xtt)], col = 'red')

# ----------> to delete










# to delete
# from ---------------> to --------------->
# a continuer

### représentation graphique univariée

library(dplyr)
cc_2019 <- variables_2019 %>%
  group_by(Cote_credit) %>%
  summarise(Marge_sur_EBITDA_test = sum(Marge_sur_EBITDA), Marge_sur_EBIT_test = sum(Marge_sur_EBIT),
            Rendement_sur_cap_prop_test = sum(Rendement_sur_cap_prop), Marge_opérationnelle_test = sum(Marge_operationnelle))



plot(cc_2019$Cote_credit, cc_2019$Marge_sur_EBITDA_test, type = "l")
plot(cc_2019$Cote_credit, cc_2019$Marge_sur_EBIT_test, type = "l")
plot(cc_2019$Cote_credit, cc_2019$Rendement_sur_cap_prop_test, type = "l")
plot(cc_2019$Cote_credit, cc_2019$Marge_opérationnelle_test, type = "l")







library(dplyr)
cc_2018 <- variables_2018 %>%
  group_by(Cote_credit) %>%
  summarise(Marge_sur_EBITDA_test = sum(Marge_sur_EBITDA))

plot(cc_2018$Marge_sur_EBITDA_test, type = "l")

# from ---------------> to --------------->


















# ---> to delete : 
# try courbe ROC 






# ANALYSE MULTIVARIEE -----------------------------------------------------
library(nnet)



# MODELE 1 ----------------------------------------------------------------


# 1)	Modèle 1 avec deux catégories de cote de crédit : firmes d'investissement et firmes spéculative




# a) périodes d'expansion -------------------------------------------------



#---------> périodes d'expansion : 2000 et 2016


variables_2016$binaire_cote_credit_2016 <- relevel(variables_2016$binaire_cote_credit_2016, ref = "firm_inv")
variables_2000$binaire_cote_credit_2000 <- relevel(variables_2000$binaire_cote_credit_2000, ref = "firm_inv")

#on choisit seulement celles qui sont significatives d'après notre analyse univariée

# on choisit celles qui ont une p-value dans analyse univariée de 0.00
# et ceux de la même catégorie s'il en reste plus d'un on vérifie leur corrélation et si c'est forte on en choisit qu'un seul choisit

modele_1_2016 <- multinom(variables_2016$binaire_cote_credit_2016 ~ Marge_sur_EBITDA +
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2016)

confint(modele_1_2016) # permet d'avoir intervalle de confiance

table(variables_2016$binaire_cote_credit_2016)



modele_1_2000 <- multinom(variables_2000$binaire_cote_credit_2000 ~ Marge_sur_EBITDA +
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2000) # mais j'ai ratio_ben_avt_impot_sur_frais_int



### Pour avoir :  coef, std Error, z value, Pr(>|z|)
library(AER) # à utiliser pour les autres
round(coeftest(modele_1_2016), digits = 3) 
round(coeftest(modele_1_2000), digits = 3) 

# ou 
library(broom)
tidy(modele_1_2016)

# ou
library(RVAideMemoire)
test.multinom(modele_1_2016, Marge_sur_EBITDA) # mais il faut réécrire toutes les variables explicatives qu'on avait utilisé dans le modèle une par une

# ou
### Calcul de la p-value
z_2016 <- summary(modele_1_2016)$coefficients/summary(modele_1_2016)$standard.errors # = wald test selon UCLA

p_value_2016 <- (1 - pnorm(abs(z_2016), 0, 1)) * 2 # on multiplie par 2 car c'est un 2 tails test


library(AER) 
round(coeftest(modele_1_2000), digits = 3) 



### OIM = Only Intercept Model
OIM_2016 <- multinom(binaire_cote_credit_2016 ~ 1, data = variables_2016)
OIM_2000 <- multinom(binaire_cote_credit_2000 ~ 1, data = variables_2000)

anova(OIM_2016, modele_1_2016) # ça me donne LR stat & Pr(Chi) # to delete INTERPRETION #from :https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html
anova(OIM_2000, modele_1_2000)



# ----->
# to delete : see cours machine learning test de significativité modele multivariée : 
# notes cours 3  page 5 plit 3

anova(OIM_2016, modele_1_2016, test = "Chisq")# test de khi-deux basé sur la deviance
# INTERPRETATION
# p_value < à 5% on rejette H0 donc modele_1_2016 best dnc meilleur

# evaluation de la performance  : data train et data test

# ----->


# vérification Résultats anova = the same
lrtest(OIM_2016, modele_1_2016)
lrtest(OIM_2000, modele_1_2000)

### Pseudo Rsquare : on prend celui de Nagelkerke
library("DescTools")

PseudoR2(modele_1_2016, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_1_2000, which = c("CoxSnell","Nagelkerke","McFadden"))

# vérifier Pseudo R-square : McFadden
library(pscl)
pR2(modele_1_2016)

# Pseudo R-carré : McFadden
L <-1-logLik(modele_1_2016)/logLik(OIM_2016)

# Pseudo Rsquare McFadden
nnet.mod.loglik <- nnet:::logLik.multinom(modele_1_2016) # from modèle 1 de base
OIM_2016.loglik <- nnet:::logLik.multinom(OIM_2016) # from modèle en haut qu'on v1 de run
(nnet.mod.mfr2 <- as.numeric(1 - nnet.mod.loglik/OIM_2016.loglik))


# Calcul des mesures de la précision de la prédiction
### on va extraire les coefficients du modèle et les mettre en exponentiels

# Pour le risque relatif
exp(coef(modele_1_2016)) # to delete : voir interprétation UCLA & bookdown.org & youtube
exp(coef(modele_1_2000))

###  les probabilités prédites pour chaque firme
head(predict_proba_2016 <- fitted(modele_1_2016), 20) # pour le 1er il y avait 94% de chance que le modèle le prédit spec et donc 6% de chance que ça le prédit inv
# mais ce raisonnement ne tient pas
head(predict_proba_2000 <- fitted(modele_1_2000), 20)

# ou vérfication
predict_proba_2016_test_1 <- modele_1_2016$fitted.values

# ou vérification
predict_proba_2016_test_2 <- predict(modele_1_2016, variables_2016, type = "prob")

# to delete :       juste pour comprendre l'interprétation des prédictions de proba car ici somme des proba n'est pas égale à 1
### vérifions si somme des proba = 1
mat_predict_proba_2016 <- matrix(predict_proba_2016)
sum(mat_predict_proba_2016)

### comparaison
sum(as.numeric(predict_proba_2016==predict_proba_2016_test_1))
sum(as.numeric(predict_proba_2016==predict_proba_2016_test_2))
sum(as.numeric(predict_proba_2016_test_1==predict_proba_2016_test_2))

### prédiction en termes de firme_inv et firme_spec par le modele
predict_cote_credit_2016 <- predict(modele_1_2016, variables_2016)
predict_cote_credit_2000 <- predict(modele_1_2000, variables_2000)

### mettre cote à cote prediction à coté vrai vriables
head(data.frame(observed=variables_2016$binaire_cote_credit_2016, predicted=predict_cote_credit_2016), 20)
head(data.frame(observed=variables_2000$binaire_cote_credit_2000, predicted=predict_cote_credit_2000), 20)

### comparaison predictions qui sont tombées good par rapport au données de départ
sum(as.numeric(predict_cote_credit_2016==variables_2016$binaire_cote_credit_2016)) # 115 éléments qui ont été bien prédit par le modèle conformément aux données de départ sur 151 données au total
sum(as.numeric(predict_cote_credit_2000==variables_2000$binaire_cote_credit_2000))

### matrice de confusion : erreur dans les prédictions
mc_2016 <- table(predict(modele_1_2016), variables_2016$binaire_cote_credit_2016)  # to delete : voir interprétation dans other doc R reg
mc_2000 <- table(predict(modele_1_2000), variables_2000$binaire_cote_credit_2000) 








# ----->
# to delete : only test pr comparer result with cours Marchine Learning
# donc intuition dans MISCLASSIFICATION et BOnne_CLASSIFICATION was good

mc_2016

err_tot <- mean(predict(modele_1_2016) != variables_2016$binaire_cote_credit_2016 )

err_inv <- mean ( (predict(modele_1_2016) != variables_2016$binaire_cote_credit_2016 ) [variables_2016$binaire_cote_credit_2016 =="firm_inv" ]   )

err_spec <- mean ( (predict(modele_1_2016) != variables_2016$binaire_cote_credit_2016 ) [variables_2016$binaire_cote_credit_2016 =="firm_spec" ]   )

err_tot
err_inv
err_spec

# donc il y a plus d'erreur dans la classe spec


# courbe ROC : page 4, plit 2 note cours machine learning

library(ROCR) # seance lab 3

knn_2_prob <- attr(predict(modele_1_2016) , "prob")

knn_2_prob_yes = ifelse((predict(modele_1_2016))=="firm_inv", knn_2_prob, 1 - knn_2_prob)


# ----->







# to delete        try to understand
### en Pourcentage # [2] ---> c'est comme mon R^2 (R square)
mc_pourcentage_2016 <- mc_2016 / colSums(mc_2016) # voir interprétation dans other doc R
mc_pourcentage_2000 <- mc_2000 / colSums(mc_2000)

# to delete way calcul from mc_2016
81 / (81+14) # = 0.8526316
14 / (22+34) # = 0.25

22 / (81+14) # =  0.2315789
34 / (22+34) # =  0.6071429

#--->
# to delete 
#Misclassification et good classification well checked

#--->




#--->
# to delete
# si on se base sur cours apprentissage aut en science de l'ad donc miscalsification_pourcentage_2016 = error_total (see en haut)
# donc reste erreur total = not erreur donc goo prevision =precision

# donc intuition et analyse et interpretation sur MISCLASSIFICATION is good
#--->



### MISCLASSIFICATION pour présentation output : erreur de classification
#### Pourcentage de mauvaise classification (misclassifaction)
miscalsification_pourcentage_2016 <- 1- sum(diag(mc_2016))/sum(mc_2016) # from mc : (14 + 22) / 151
# to delete : voir interpretation dans l'auttre doc R

miscalsification_pourcentage_2000 <- 1- sum(diag(mc_2000))/sum(mc_2000)

### ACCURACY DU MODEL pour présentation output
### Pourcenttage de bonne classification : la somme de la diag qu'on divise par le nombre total d'observation 
bonne_classifcation_pourcentage_2016 <- sum(diag(mc_2016))/sum(mc_2016)  # ou 1 - miscalsification_pourcentage_2016
bonne_classifcation_pourcentage_2000 <- sum(diag(mc_2000))/sum(mc_2000)

# vérification
# MISCLASSIFICATION + ACCURACY DU MODEL
miscalsification_pourcentage_2016+bonne_classifcation_pourcentage_2016
miscalsification_pourcentage_2000+bonne_classifcation_pourcentage_2000

#ou vérification
# ça nous donne matrice de confision et statistique
library(caret)
confusionMatrix(table(predict(modele_1_2016), variables_2016$binaire_cote_credit_2016)) 
confusionMatrix(table(predict(modele_1_2000), variables_2000$binaire_cote_credit_2000)) 

# calcul pour chaque ratings sensitivity,.... 
# calcul aussi Accuracy, CI, p-value

confusionMatrix(table(predict(modele_1_2000), variables_2000$binaire_cote_credit_2000)) 

### to delete : observation de départ en POURCENTAGE
n <- table(variables_2016$binaire_cote_credit_2016) #nombre de firme pour chaque type de rating
n/sum(n) # pourcentage de firme par type de rating # la plupart des firmes ont un rating de 4



### Table précision prédiction : périodes d'expansion : 2000 et 2016

# 2016

prediction_table_2016 <- matrix(0,3, 2)
colnames(prediction_table_2016) <- c("Prediction_correcte_2016", "Prediction_incorrecte")
rownames(prediction_table_2016) <- c("Firmes_investissement", "Firmes_speculative", "Precision_Globale_de_la_Prediction")

prediction_table_2016[1,1] <- mc_pourcentage_2016[1,1]
prediction_table_2016[2,1] <- mc_pourcentage_2016[2,2]
prediction_table_2016[3,1] <- bonne_classifcation_pourcentage_2016

prediction_table_2016[1,2] <- 1 - mc_pourcentage_2016[1,1]
prediction_table_2016[2,2] <- 1 - mc_pourcentage_2016[2,2]
prediction_table_2016[3,2] <- bonne_classifcation_pourcentage_2016

round(prediction_table_2016 * 100, digits = 2)









# 2000

prediction_table_2000 <- matrix(0,3, 2)
colnames(prediction_table_2000) <- c("Prediction_correcte_2000", "Prediction_incorrecte")
rownames(prediction_table_2000) <- c("Firmes_investissement", "Firmes_speculative", "Precision_Globale_de_la_Prediction")

prediction_table_2000[1,1] <- mc_pourcentage_2000[1,1]
prediction_table_2000[2,1] <- mc_pourcentage_2000[2,2]
prediction_table_2000[3,1] <- bonne_classifcation_pourcentage_2000

prediction_table_2000[1,2] <- 1 - mc_pourcentage_2000[1,1]
prediction_table_2000[2,2] <- 1 - mc_pourcentage_2000[2,2]
prediction_table_2000[3,2] <- bonne_classifcation_pourcentage_2000

round(prediction_table_2000 * 100, digits = 2)















# b) périodes de récession ------------------------------------------------



#---------> périodes de récession : 2007, 2008, 2009, 2018 et 2019


variables_2019$binaire_cote_credit_2019 <- relevel(variables_2019$binaire_cote_credit_2019, ref = "firm_inv")
variables_2018$binaire_cote_credit_2018 <- relevel(variables_2018$binaire_cote_credit_2018, ref = "firm_inv")
variables_2009$binaire_cote_credit_2009 <- relevel(variables_2009$binaire_cote_credit_2009, ref = "firm_inv")
variables_2008$binaire_cote_credit_2008 <- relevel(variables_2008$binaire_cote_credit_2008, ref = "firm_inv")
variables_2007$binaire_cote_credit_2007 <- relevel(variables_2007$binaire_cote_credit_2007, ref = "firm_inv")

#on choisit seulement celles qui sont significatives d'après notre analyse univariée

# on choisit celles qui ont une p-value dans analyse univariée de 0.00
# et ceux de la même catégorie s'il en reste plus d'un on check leur corrélation et si c'est forte on en choisit qu'un seul choisit

modele_1_2019 <- multinom(variables_2019$binaire_cote_credit_2019 ~ Marge_sur_EBITDA + 
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2019)




table(variables_2019$binaire_cote_credit_2019)
confint(modele_1_2019) # permet d'avoir intervalle de confiance


modele_1_2018 <- multinom(variables_2018$binaire_cote_credit_2018 ~ Marge_sur_EBITDA +
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2018)


modele_1_2009 <- multinom(variables_2009$binaire_cote_credit_2009 ~ Marge_sur_EBITDA +
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2009)


modele_1_2008 <- multinom(variables_2008$binaire_cote_credit_2008 ~ Marge_sur_EBITDA +
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2008)


modele_1_2007 <- multinom(variables_2007$binaire_cote_credit_2007 ~ Marge_sur_EBITDA +
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2007)



### Pour avoir :  coef, std Error, z value, Pr(>|z|)
library(AER) # à utiliser pour les autres
round(coeftest(modele_1_2019), digits = 3) 

# ou 
library(broom)
tidy(modele_1_2019)

# ou
library(RVAideMemoire)
test.multinom(modele_1_2019, Marge_sur_EBITDA) # mais il faut réécrire toutes les variables explicatives qu'on avait utilisé dans le modèle une par une

# ou
### Calcul de la p-value
z_2019 <- summary(modele_1_2019)$coefficients/summary(modele_1_2019)$standard.errors # = wald test selon UCLA

p_value_2019 <- (1 - pnorm(abs(z_2019), 0, 1)) * 2 # on multiplie par 2 car c'est un 2 tails test



#rm(list = ls())

# to delete

# AIC de chque période
glance(modele_1_2019) # marche pr lm
glance(modele_1_2019)





library(AER) 
round(coeftest(modele_1_2018), digits = 3) 

library(AER)
round(coeftest(modele_1_2009), digits = 3) 

library(AER) 
round(coeftest(modele_1_2008), digits = 3) 

library(AER)
round(coeftest(modele_1_2007), digits = 3) 


### OIM = Only Intercept Model
OIM_2019 <- multinom(binaire_cote_credit_2019 ~ 1, data = variables_2019)
OIM_2018 <- multinom(binaire_cote_credit_2018 ~ 1, data = variables_2018)
OIM_2009 <- multinom(binaire_cote_credit_2009 ~ 1, data = variables_2009)
OIM_2008 <- multinom(binaire_cote_credit_2008 ~ 1, data = variables_2008)
OIM_2007 <- multinom(binaire_cote_credit_2007 ~ 1, data = variables_2007)

anova(OIM_2019, modele_1_2019) # ça me donne LR stat & Pr(Chi) # to delete INTERPRETION #from :https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html
anova(OIM_2018, modele_1_2018)
anova(OIM_2009, modele_1_2009)
anova(OIM_2008, modele_1_2008)
anova(OIM_2007, modele_1_2007)

# vérification Résultats anova = the same
lrtest(OIM_2019, modele_1_2019)
lrtest(OIM_2018, modele_1_2018)
lrtest(OIM_2009, modele_1_2009)
lrtest(OIM_2008, modele_1_2008)
lrtest(OIM_2007, modele_1_2007)

### Pseudo Rsquare : on prend celui de Nagelkerke
library("DescTools")

PseudoR2(modele_1_2019, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_1_2018, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_1_2009, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_1_2008, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_1_2007, which = c("CoxSnell","Nagelkerke","McFadden"))

# vérifier Pseudo R-square : McFadden
library(pscl)
pR2(modele_1_2019)

# Pseudo R-carré : McFadden
L <-1-logLik(modele_1_2019)/logLik(OIM_2019)

# Pseudo Rsquare McFadden
nnet.mod.loglik <- nnet:::logLik.multinom(modele_1_2019) # from modèle 1 de base
OIM_2019.loglik <- nnet:::logLik.multinom(OIM_2019) # from modèle en haut qu'on v1 de run
(nnet.mod.mfr2 <- as.numeric(1 - nnet.mod.loglik/OIM_2019.loglik))





# Calcul des mesures de la précision de la prédiction
### on va extraire les coefficients du modèle et les mettre en exponentiels

# Pour le risque relatif
exp(coef(modele_1_2019)) # to delete : voir interprétation UCLA & bookdown.org & youtube
exp(coef(modele_1_2018))
exp(coef(modele_1_2009))
exp(coef(modele_1_2008))
exp(coef(modele_1_2007))

###  les probabilités prédites pour chaque firme
head(predict_proba_2019 <- fitted(modele_1_2019), 20) # pour le 1er il y avait 94% de chance que le modèle le prédit spec et donc 6% de chance que ça le prédit inv
                                                      # mais ce raisonnement ne tient pas
head(predict_proba_2018 <- fitted(modele_1_2018), 20)
head(predict_proba_2009 <- fitted(modele_1_2009), 20)
head(predict_proba_2008 <- fitted(modele_1_2008), 20)
head(predict_proba_2007 <- fitted(modele_1_2007), 20)

# ou vérfication
predict_proba_2019_test_1 <- modele_1_2019$fitted.values

# ou vérification
predict_proba_2019_test_2 <- predict(modele_1_2019, variables_2019, type = "prob")

# to delete :       juste pour comprendre l'interprétation des prédictions de proba car ici somme des proba n'est pas égale à 1
### vérifions si somme des proba = 1
mat_predict_proba_2019 <- matrix(predict_proba_2019)
sum(mat_predict_proba_2019)

### comparaison
sum(as.numeric(predict_proba_2019==predict_proba_2019_test_1)) # donc on a 151 TRUE donc c'est identique
sum(as.numeric(predict_proba_2019==predict_proba_2019_test_2))
sum(as.numeric(predict_proba_2019_test_1==predict_proba_2019_test_2))

### prédiction en termes de firme_inv et firme_spec par le modele
predict_cote_credit_2019 <- predict(modele_1_2019, variables_2019) # je ne vais pas interpréter ça un par un donc je le mets dans un tableau
predict_cote_credit_2018 <- predict(modele_1_2018, variables_2018)
predict_cote_credit_2009 <- predict(modele_1_2009, variables_2009)
predict_cote_credit_2008 <- predict(modele_1_2008, variables_2008)
predict_cote_credit_2007 <- predict(modele_1_2007, variables_2007)




### mettre cote à cote prediction à coté vrai vriables
head(data.frame(observed=variables_2019$binaire_cote_credit_2019, predicted=predict_cote_credit_2019), 20)
head(data.frame(observed=variables_2018$binaire_cote_credit_2018, predicted=predict_cote_credit_2018), 20)
head(data.frame(observed=variables_2009$binaire_cote_credit_2009, predicted=predict_cote_credit_2009), 20)
head(data.frame(observed=variables_2008$binaire_cote_credit_2008, predicted=predict_cote_credit_2008), 20)
head(data.frame(observed=variables_2007$binaire_cote_credit_2007, predicted=predict_cote_credit_2007), 20)

### comparaison predictions qui sont tombées good par rapport au données de départ
sum(as.numeric(predict_cote_credit_2019==variables_2019$binaire_cote_credit_2019)) # 115 éléments qui ont été bien prédit par le modèle conformément aux données de départ sur 151 données au total
sum(as.numeric(predict_cote_credit_2018==variables_2018$binaire_cote_credit_2018))
sum(as.numeric(predict_cote_credit_2009==variables_2009$binaire_cote_credit_2009))
sum(as.numeric(predict_cote_credit_2008==variables_2008$binaire_cote_credit_2008))
sum(as.numeric(predict_cote_credit_2007==variables_2007$binaire_cote_credit_2007))

### matrice de confusion : erreur dans les prédictions
mc_2019 <- table(predict(modele_1_2019), variables_2019$binaire_cote_credit_2019)  # to delete : voir interprétation dans other doc R reg
mc_2018 <- table(predict(modele_1_2018), variables_2018$binaire_cote_credit_2018) 
mc_2009 <- table(predict(modele_1_2009), variables_2009$binaire_cote_credit_2009) 
mc_2008 <- table(predict(modele_1_2008), variables_2008$binaire_cote_credit_2008) 
mc_2007 <- table(predict(modele_1_2007), variables_2007$binaire_cote_credit_2007) 

# to delete        try to understand
###
mc_pourcentage_2019 <- mc_2019 / colSums(mc_2019) # voir interprétation dans other doc R : 0.831 c'est pour colonne firm inv : = (79 * 100%) / (79+16)
mc_pourcentage_2018 <- mc_2018 / colSums(mc_2018)
mc_pourcentage_2009 <- mc_2009 / colSums(mc_2009)
mc_pourcentage_2008 <- mc_2008 / colSums(mc_2008)
mc_pourcentage_2007 <- mc_2007 / colSums(mc_2007)

# to delete way calcul from mc_2019
81 / (81+14) # = 0.8526316
14 / (22+34) # = 0.25

22 / (81+14) # =  0.2315789
34 / (22+34) # =  0.6071429


### MISCLASSIFICATION pour présentation output
#### Pourcentage de mauvaise classification (misclassifaction)
miscalsification_pourcentage_2019 <- 1- sum(diag(mc_2019))/sum(mc_2019) # from mc : (14 + 22) / 151
# to delete : voir interpretation dans l'auttre doc R

miscalsification_pourcentage_2018 <- 1- sum(diag(mc_2018))/sum(mc_2018)
miscalsification_pourcentage_2009 <- 1- sum(diag(mc_2009))/sum(mc_2009)
miscalsification_pourcentage_2008 <- 1- sum(diag(mc_2008))/sum(mc_2008)
miscalsification_pourcentage_2007 <- 1- sum(diag(mc_2007))/sum(mc_2007)

### ACCURACY DU MODEL pour présentation output
### Pourcenttage de bonne classification : la somme de la diag qu'on divise par le nombre total d'observation 
bonne_classifcation_pourcentage_2019 <- sum(diag(mc_2019))/sum(mc_2019)  # ou 1 - miscalsification_pourcentage_2019
bonne_classifcation_pourcentage_2018 <- sum(diag(mc_2018))/sum(mc_2018)
bonne_classifcation_pourcentage_2009 <- sum(diag(mc_2009))/sum(mc_2009)
bonne_classifcation_pourcentage_2008 <- sum(diag(mc_2008))/sum(mc_2008)
bonne_classifcation_pourcentage_2007 <- sum(diag(mc_2007))/sum(mc_2007)

# vérification
# MISCLASSIFICATION + ACCURACY DU MODEL
miscalsification_pourcentage_2019+bonne_classifcation_pourcentage_2019
miscalsification_pourcentage_2018+bonne_classifcation_pourcentage_2018
miscalsification_pourcentage_2009+bonne_classifcation_pourcentage_2009
miscalsification_pourcentage_2008+bonne_classifcation_pourcentage_2008
miscalsification_pourcentage_2007+bonne_classifcation_pourcentage_2007

#ou vérification
# ça nous donne matrice de confision et statistique
library(caret)
confusionMatrix(table(predict(modele_1_2019), variables_2019$binaire_cote_credit_2019)) 
confusionMatrix(table(predict(modele_1_2018), variables_2018$binaire_cote_credit_2018)) 
confusionMatrix(table(predict(modele_1_2009), variables_2009$binaire_cote_credit_2009)) 
confusionMatrix(table(predict(modele_1_2008), variables_2008$binaire_cote_credit_2008)) 
confusionMatrix(table(predict(modele_1_2007), variables_2007$binaire_cote_credit_2007)) 

# calcul pour chaque ratings sensitivity,.... 
# calcul aussi Accuracy, CI, p-value

confusionMatrix(table(predict(modele_1_2018), variables_2018$binaire_cote_credit_2018)) 
confusionMatrix(table(predict(modele_1_2009), variables_2009$binaire_cote_credit_2009)) 
confusionMatrix(table(predict(modele_1_2008), variables_2008$binaire_cote_credit_2008)) 
confusionMatrix(table(predict(modele_1_2007), variables_2007$binaire_cote_credit_2007)) 

### to delete : observation de départ en POURCENTAGE
n <- table(variables_2019$binaire_cote_credit_2019) #nombre de firme pour chaque type de rating
n/sum(n) # pourcentage de firme par type de rating # la plupart des firmes ont un rating de 4




#to delete
#rm(list = ls())



### Table précision prédiction : périodes de récession : 2007, 2008, 2009, 2018 et 2019

# 2019

prediction_table_2019 <- matrix(0,3, 2)
colnames(prediction_table_2019) <- c("Prediction_correcte_2019", "Prediction_incorrecte")
rownames(prediction_table_2019) <- c("Firmes_investissement", "Firmes_speculative", "Precision_Globale_de_la_Prediction")

prediction_table_2019[1,1] <- mc_pourcentage_2019[1,1]
prediction_table_2019[2,1] <- mc_pourcentage_2019[2,2]
prediction_table_2019[3,1] <- bonne_classifcation_pourcentage_2019

prediction_table_2019[1,2] <- 1 - mc_pourcentage_2019[1,1]
prediction_table_2019[2,2] <- 1 - mc_pourcentage_2019[2,2]
prediction_table_2019[3,2] <- bonne_classifcation_pourcentage_2019

round(prediction_table_2019 * 100, digits = 2)





# 2018
prediction_table_2018 <- matrix(0,3, 2)
colnames(prediction_table_2018) <- c("Prediction_correcte_2018", "Prediction_incorrecte")
rownames(prediction_table_2018) <- c("Firmes_investissement", "Firmes_speculative", "Precision_Globale_de_la_Prediction")

prediction_table_2018[1,1] <- mc_pourcentage_2018[1,1]
prediction_table_2018[2,1] <- mc_pourcentage_2018[2,2]
prediction_table_2018[3,1] <- bonne_classifcation_pourcentage_2018

prediction_table_2018[1,2] <- 1 - mc_pourcentage_2018[1,1]
prediction_table_2018[2,2] <- 1 - mc_pourcentage_2018[2,2]
prediction_table_2018[3,2] <- bonne_classifcation_pourcentage_2018

round(prediction_table_2018 * 100, digits = 2)




# 2009
prediction_table_2009 <- matrix(0,3, 2)
colnames(prediction_table_2009) <- c("Prediction_correcte_2009", "Prediction_incorrecte")
rownames(prediction_table_2009) <- c("Firmes_investissement", "Firmes_speculative", "Precision_Globale_de_la_Prediction")

prediction_table_2009[1,1] <- mc_pourcentage_2009[1,1]
prediction_table_2009[2,1] <- mc_pourcentage_2009[2,2]
prediction_table_2009[3,1] <- bonne_classifcation_pourcentage_2009

prediction_table_2009[1,2] <- 1 - mc_pourcentage_2009[1,1]
prediction_table_2009[2,2] <- 1 - mc_pourcentage_2009[2,2]
prediction_table_2009[3,2] <- bonne_classifcation_pourcentage_2009

round(prediction_table_2009 * 100, digits = 2)







# 2008
prediction_table_2008 <- matrix(0,3, 2)
colnames(prediction_table_2008) <- c("Prediction_correcte_2008", "Prediction_incorrecte")
rownames(prediction_table_2008) <- c("Firmes_investissement", "Firmes_speculative", "Precision_Globale_de_la_Prediction")

prediction_table_2008[1,1] <- mc_pourcentage_2008[1,1]
prediction_table_2008[2,1] <- mc_pourcentage_2008[2,2]
prediction_table_2008[3,1] <- bonne_classifcation_pourcentage_2008

prediction_table_2008[1,2] <- 1 - mc_pourcentage_2008[1,1]
prediction_table_2008[2,2] <- 1 - mc_pourcentage_2008[2,2]
prediction_table_2008[3,2] <- bonne_classifcation_pourcentage_2008

round(prediction_table_2008 * 100, digits = 2)






# 2007
prediction_table_2007 <- matrix(0,3, 2)
colnames(prediction_table_2007) <- c("Prediction_correcte_2007", "Prediction_incorrecte")
rownames(prediction_table_2007) <- c("Firmes_investissement", "Firmes_speculative", "Precision_Globale_de_la_Prediction")

prediction_table_2007[1,1] <- mc_pourcentage_2007[1,1]
prediction_table_2007[2,1] <- mc_pourcentage_2007[2,2]
prediction_table_2007[3,1] <- bonne_classifcation_pourcentage_2007

prediction_table_2007[1,2] <- 1 - mc_pourcentage_2007[1,1]
prediction_table_2007[2,2] <- 1 - mc_pourcentage_2007[2,2]
prediction_table_2007[3,2] <- bonne_classifcation_pourcentage_2007

round(prediction_table_2007 * 100, digits = 2)









# ---->
# ---->
# ---->
# ---->
# ---->
# ---->
# ---->
# ---->
# ---->
# ---->
# ---->
# ---->
# ---->



# to delete
#rm(list = ls())



# to delete phrase en bas
# try to have presentation excel final avant de se questionner why choose only ces 4


# to delete
# Tableau OUTPUT des pourcentages de bonne et mauvaise classifaction










# to delete
### Test the goodness of fit : TEST D'INDEPENDANCE APRES PREDICTION
chisq.test(variables_2019$binaire_cote_credit_2019, predict(modele_1_2019))


# to delete
# try have to ratio de la volatilité via Bloomberg



# to delete may be
#----------->
# TESTDE DURBIN WATSON D'AUTOCORRELATION
require(lmtest)
dwtest(multinom(as.integer(variables_2019$binaire_cote_credit_2019) ~ Marge_sur_EBITDA +
                  ratio_Fonds_de_roulement_sur_tot_actif +
                  ratio_ben_avt_impot_sur_frais_int +
                  Total_actif, data = variables_2019))
#----------->





















# to delete

# to delete may be # va marche pour modele 2
# see : https://stackoverflow.com/questions/33316898/r-tukey-posthoc-tests-for-nnet-multinom-multinomial-fit-to-test-for-overall-dif
library(effects)
plot(effect(modele_1_2019,term="Marge_sur_EBITDA"),ylab="",type="probability",style="stacked",colors=rainbow(7))












# to delete
#rm(list = ls())











#rm(list = ls())


# MODELE 2 ----------------------------------------------------------------


# 1)	Modèle 2 avec trois catégories de cote de crédit : firmes d'investissement contre  BB et B_&_CCC



# construction de mmes variables : donc je sépare la catégorie des firmes spéculatives en deux
# donc on crée seulement le bloc B_&_CCC


colnames(variables_2019)

variables_2019$binaire_cote_credit_2019
variables_2019$Cote_credit
table(variables_2019$Cote_credit)

cote_credt_modele_2 <- c("AAA"="firm_inv", "AA"="firm_inv", "A"="firm_inv", "BBB"="firm_inv", 
                   "BB"="BB", "B"="B_&_CCC", "CCC"="B_&_CCC")


# to delete
#rm(list = ls())



# creons la cote de credit du modele 2


cote_credit_2019_mod_2 <- variables_2019$Cote_credit # extraction cote de credit 2019
cote_credit_2019_mod_2 <- cote_credt_modele_2[cote_credit_2019_mod_2]
table(cote_credit_2019_mod_2)
variables_2019_mod_2 <- cbind(cote_credit_2019_mod_2, variables_2019)




cote_credit_2018_mod_2 <- variables_2018$Cote_credit # extraction cote de credit 2018
cote_credit_2018_mod_2 <- cote_credt_modele_2[cote_credit_2018_mod_2]
table(cote_credit_2018_mod_2)
variables_2018_mod_2 <- cbind(cote_credit_2018_mod_2, variables_2018)



cote_credit_2016_mod_2 <- variables_2016$Cote_credit # extraction cote de credit 2016
cote_credit_2016_mod_2 <- cote_credt_modele_2[cote_credit_2016_mod_2]
table(cote_credit_2016_mod_2)
variables_2016_mod_2 <- cbind(cote_credit_2016_mod_2, variables_2016)






cote_credit_2009_mod_2 <- variables_2009$Cote_credit # extraction cote de credit 2009
cote_credit_2009_mod_2 <- cote_credt_modele_2[cote_credit_2009_mod_2]
table(cote_credit_2009_mod_2)
variables_2009_mod_2 <- cbind(cote_credit_2009_mod_2, variables_2009)



cote_credit_2008_mod_2 <- variables_2008$Cote_credit # extraction cote de credit 2008
cote_credit_2008_mod_2 <- cote_credt_modele_2[cote_credit_2008_mod_2]
table(cote_credit_2008_mod_2)
variables_2008_mod_2 <- cbind(cote_credit_2008_mod_2, variables_2008)



cote_credit_2007_mod_2 <- variables_2007$Cote_credit # extraction cote de credit 2007
cote_credit_2007_mod_2 <- cote_credt_modele_2[cote_credit_2007_mod_2]
table(cote_credit_2007_mod_2)
variables_2007_mod_2 <- cbind(cote_credit_2007_mod_2, variables_2007)



cote_credit_2000_mod_2 <- variables_2000$Cote_credit # extraction cote de credit 2000
cote_credit_2000_mod_2 <- cote_credt_modele_2[cote_credit_2000_mod_2]
table(cote_credit_2000_mod_2)
variables_2000_mod_2 <- cbind(cote_credit_2000_mod_2, variables_2000)







# to deleta
#rm(list = ls())





#----------> méttons nos cotes de crédit en factor

# POUR 2000, on va seulement mettre 2 niveaux étant donnée que BB_&_CCC n'existe pas car sinon ça fausse notre régression
variables_2000_mod_2$cote_credit_2000_mod_2 <- factor(variables_2000_mod_2$cote_credit_2000_mod_2, levels = c("firm_inv", "BB", "B_&_CCC"))
variables_2007_mod_2$cote_credit_2007_mod_2 <- factor(variables_2007_mod_2$cote_credit_2007_mod_2, levels = c("firm_inv", "BB", "B_&_CCC"))
variables_2008_mod_2$cote_credit_2008_mod_2 <- factor(variables_2008_mod_2$cote_credit_2008_mod_2, levels = c("firm_inv", "BB", "B_&_CCC"))
variables_2009_mod_2$cote_credit_2009_mod_2 <- factor(variables_2009_mod_2$cote_credit_2009_mod_2, levels = c("firm_inv", "BB", "B_&_CCC"))
variables_2016_mod_2$cote_credit_2016_mod_2 <- factor(variables_2016_mod_2$cote_credit_2016_mod_2, levels = c("firm_inv", "BB", "B_&_CCC"))
variables_2018_mod_2$cote_credit_2018_mod_2 <- factor(variables_2018_mod_2$cote_credit_2018_mod_2, levels = c("firm_inv", "BB", "B_&_CCC"))
variables_2019_mod_2$cote_credit_2019_mod_2 <- factor(variables_2019_mod_2$cote_credit_2019_mod_2, levels = c("firm_inv", "BB", "B_&_CCC"))





# a) périodes d'expansion -------------------------------------------------



#---------> périodes d'expansion : 2000 et 2016 : dans le modèle 2 on ne change pas la référence


variables_2016_mod_2$cote_credit_2016_mod_2 <- relevel(variables_2016_mod_2$cote_credit_2016_mod_2, ref = "firm_inv")
variables_2000_mod_2$cote_credit_2000_mod_2 <- relevel(variables_2000_mod_2$cote_credit_2000_mod_2, ref = "firm_inv")

#on choisit seulement celles qui sont significatives d'après notre analyse univariée

# on choisit celles qui ont une p-value dans analyse univariée de 0.00
# et ceux de la même catégorie s'il en reste plus d'un on vérifie leur corrélation et si c'est forte on en choisit qu'un seul choisit

modele_2_2016 <- multinom(variables_2016_mod_2$cote_credit_2016_mod_2 ~ Marge_sur_EBITDA +
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2016_mod_2)

confint(modele_2_2016) # permet d'avoir intervalle de confiance

table(variables_2016_mod_2$cote_credit_2016_mod_2)
length(variables_2016_mod_2$cote_credit_2016_mod_2)




#suite

modele_2_2000 <- multinom(variables_2000_mod_2$cote_credit_2000_mod_2 ~ Marge_sur_EBITDA +
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2000_mod_2) # mais j'ai ratio_ben_avt_impot_sur_frais_int



### Pour avoir :  coef, std Error, z value, Pr(>|z|)
library(AER) # à utiliser pour les autres
round(coeftest(modele_2_2016), digits = 3) 
round(coeftest(modele_2_2000), digits = 3) 

# ou 
library(broom)
tidy(modele_2_2016)

# ou
library(RVAideMemoire)
test.multinom(modele_2_2016, Marge_sur_EBITDA) # mais il faut réécrire toutes les variables explicatives qu'on avait utilisé dans le modèle une par une

# ou
### Calcul de la p-value
z_2016_mod_2 <- summary(modele_2_2016)$coefficients/summary(modele_2_2016)$standard.errors # = wald test selon UCLA

p_value_2016_mod_2 <- (1 - pnorm(abs(z_2016_mod_2), 0, 1)) * 2 # on multiplie par 2 car c'est un 2 tails test


library(AER) 
round(coeftest(modele_2_2000), digits = 3) 



### OIM = Only Intercept Model
OIM_2016_mod_2 <- multinom(cote_credit_2016_mod_2 ~ 1, data = variables_2016_mod_2)
OIM_2000_mod_2 <- multinom(cote_credit_2000_mod_2 ~ 1, data = variables_2000_mod_2)

anova(OIM_2016_mod_2, modele_2_2016) # ça me donne LR stat & Pr(Chi) # to delete INTERPRETION #from :https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html
anova(OIM_2000_mod_2, modele_2_2000)

# vérification Résultats anova = the same
lrtest(OIM_2016_mod_2, modele_2_2016)
lrtest(OIM_2000_mod_2, modele_2_2000)

### Pseudo Rsquare : on prend celui de Nagelkerke
library("DescTools")

PseudoR2(modele_2_2016, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_2_2000, which = c("CoxSnell","Nagelkerke","McFadden"))

# vérifier Pseudo R-square : McFadden
library(pscl)
pR2(modele_2_2016)

# Pseudo R-carré : McFadden
L_mod_2 <-1-logLik(modele_2_2016)/logLik(OIM_2016_mod_2)

# Pseudo Rsquare McFadden
nnet.mod.loglik_mod_2 <- nnet:::logLik.multinom(modele_2_2016) # from modèle 1 de base
OIM_2016_mod_2.loglik <- nnet:::logLik.multinom(OIM_2016_mod_2) # from modèle en haut qu'on v1 de run
(nnet.mod.mfr2 <- as.numeric(1 - nnet.mod.loglik_mod_2/OIM_2016_mod_2.loglik))


# Calcul des mesures de la précision de la prédiction
### on va extraire les coefficients du modèle et les mettre en exponentiels

# Pour le risque relatif
exp(coef(modele_2_2016)) # to delete : voir interprétation UCLA & bookdown.org & youtube
exp(coef(modele_2_2000))

###  les probabilités prédites pour chaque firme
head(predict_proba_2016_mod_2 <- fitted(modele_2_2016), 20) # pour le 1er il y avait 94% de chance que le modèle le prédit spec et donc 6% de chance que ça le prédit inv
# mais ce raisonnement ne tient pas
head(predict_proba_2000_mod_2 <- fitted(modele_2_2000), 20)

# ou vérfication
predict_proba_2016_mod_2_test_1 <- modele_2_2016$fitted.values

# ou vérification
predict_proba_2016_mod_2_test_2 <- predict(modele_2_2016, variables_2016_mod_2, type = "prob")

# to delete :       juste pour comprendre l'interprétation des prédictions de proba car ici somme des proba n'est pas égale à 1
### vérifions si somme des proba = 1
mat_predict_proba_2016_mod_2_mod_2 <- matrix(predict_proba_2016_mod_2)
sum(mat_predict_proba_2016_mod_2_mod_2)

### comparaison
sum(as.numeric(predict_proba_2016_mod_2==predict_proba_2016_mod_2_test_1))
sum(as.numeric(predict_proba_2016_mod_2==predict_proba_2016_mod_2_test_2))
sum(as.numeric(predict_proba_2016_mod_2_test_1==predict_proba_2016_mod_2_test_2))

### prédiction en termes de firme_inv et firme_spec par le modele
predict_cote_credit_2016_mod_2 <- predict(modele_2_2016, variables_2016_mod_2)
predict_cote_credit_2000_mod_2 <- predict(modele_2_2000, variables_2000_mod_2)

### mettre cote à cote prediction à coté vrai vriables
head(data.frame(observed=variables_2016_mod_2$cote_credit_2016_mod_2, predicted=predict_cote_credit_2016_mod_2), 20)
head(data.frame(observed=variables_2000_mod_2$cote_credit_2000_mod_2, predicted=predict_cote_credit_2000_mod_2), 20)

### comparaison predictions qui sont tombées good par rapport au données de départ
sum(as.numeric(predict_cote_credit_2016_mod_2==variables_2016_mod_2$cote_credit_2016_mod_2)) # 115 éléments qui ont été bien prédit par le modèle conformément aux données de départ sur 151 données au total
sum(as.numeric(predict_cote_credit_2000_mod_2==variables_2000_mod_2$cote_credit_2000_mod_2))

### matrice de confusion : erreur dans les prédictions
mc_2016_mod_2 <- table(predict(modele_2_2016), variables_2016_mod_2$cote_credit_2016_mod_2)  # to delete : voir interprétation dans other doc R reg
mc_2000_mod_2 <- table(predict(modele_2_2000), variables_2000_mod_2$cote_credit_2000_mod_2) 

# to delete        try to understand
### en Pourcentage # [2] ---> c'est comme mon R^2 (R square)
mc_pourcentage_2016_mod_2 <- mc_2016_mod_2 / colSums(mc_2016_mod_2) # voir interprétation dans other doc R
mc_pourcentage_2000_mod_2 <- mc_2000_mod_2 / colSums(mc_2000_mod_2)

# to delete way calcul from mc_2016_mod_2
81 / (81+14) # = 0.8526316
14 / (22+34) # = 0.25

22 / (81+14) # =  0.2315789
34 / (22+34) # =  0.6071429


### MISCLASSIFICATION pour présentation output
#### Pourcentage de mauvaise classification (misclassifaction)
miscalsification_pourcentage_2016_mod_2 <- 1- sum(diag(mc_2016_mod_2))/sum(mc_2016_mod_2) # from mc : (14 + 22) / 151
# to delete : voir interpretation dans l'auttre doc R

miscalsification_pourcentage_2000_mod_2 <- 1- sum(diag(mc_2000_mod_2))/sum(mc_2000_mod_2)

### ACCURACY DU MODEL pour présentation output
### Pourcenttage de bonne classification : la somme de la diag qu'on divise par le nombre total d'observation 
bonne_classifcation_pourcentage_2016_mod_2 <- sum(diag(mc_2016_mod_2))/sum(mc_2016_mod_2)  # ou 1 - miscalsification_pourcentage_2016_mod_2
bonne_classifcation_pourcentage_2000_mod_2 <- sum(diag(mc_2000_mod_2))/sum(mc_2000_mod_2)

# vérification
# MISCLASSIFICATION + ACCURACY DU MODEL
miscalsification_pourcentage_2016_mod_2+bonne_classifcation_pourcentage_2016_mod_2
miscalsification_pourcentage_2000_mod_2+bonne_classifcation_pourcentage_2000_mod_2

#ou vérification
# ça nous donne matrice de confision et statistique
library(caret)
confusionMatrix(table(predict(modele_2_2016), variables_2016_mod_2$cote_credit_2016_mod_2)) 
confusionMatrix(table(predict(modele_2_2000), variables_2000_mod_2$cote_credit_2000_mod_2)) 

# calcul pour chaque ratings sensitivity,.... 
# calcul aussi Accuracy, CI, p-value

confusionMatrix(table(predict(modele_2_2000), variables_2000_mod_2$cote_credit_2000_mod_2)) 

### to delete : observation de départ en POURCENTAGE
n_mod_2 <- table(variables_2016_mod_2$cote_credit_2016_mod_2) #nombre de firme pour chaque type de rating
n_mod_2/sum(n_mod_2) # pourcentage de firme par type de rating # la plupart des firmes ont un rating de 4



### Table précision prédiction : périodes d'expansion : 2000 et 2016

# 2016


# to delete : check it if conforme

prediction_table_2016_mod_2 <- matrix(0,4, 2)
colnames(prediction_table_2016_mod_2) <- c("Prediction_correcte_2016", "Prediction_incorrecte")
rownames(prediction_table_2016_mod_2) <- c("Firmes_investissement", "BB", "BB_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2016_mod_2[1,1] <- mc_pourcentage_2016_mod_2[1,1]
prediction_table_2016_mod_2[2,1] <- mc_pourcentage_2016_mod_2[2,2]
prediction_table_2016_mod_2[3,1] <- mc_pourcentage_2016_mod_2[3,3]
prediction_table_2016_mod_2[4,1] <- bonne_classifcation_pourcentage_2016_mod_2

prediction_table_2016_mod_2[1,2] <- 1 - mc_pourcentage_2016_mod_2[1,1]
prediction_table_2016_mod_2[2,2] <- 1 - mc_pourcentage_2016_mod_2[2,2]
prediction_table_2016_mod_2[3,2] <- 1 - mc_pourcentage_2016_mod_2[3,3]
prediction_table_2016_mod_2[4,2] <- bonne_classifcation_pourcentage_2016_mod_2

round(prediction_table_2016_mod_2 * 100, digits = 2)









# 2000

prediction_table_2000_mod_2 <- matrix(0,4, 2)
colnames(prediction_table_2000_mod_2) <- c("Prediction_correcte_2000", "Prediction_incorrecte")
rownames(prediction_table_2000_mod_2) <- c("Firmes_investissement", "BB", "BB_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2000_mod_2[1,1] <- mc_pourcentage_2000_mod_2[1,1]
prediction_table_2000_mod_2[2,1] <- mc_pourcentage_2000_mod_2[2,2]
prediction_table_2000_mod_2[3,1] <- mc_pourcentage_2000_mod_2[3,3]
prediction_table_2000_mod_2[4,1] <- bonne_classifcation_pourcentage_2000_mod_2

prediction_table_2000_mod_2[1,2] <- 1 - mc_pourcentage_2000_mod_2[1,1]
prediction_table_2000_mod_2[2,2] <- 1 - mc_pourcentage_2000_mod_2[2,2]
prediction_table_2000_mod_2[3,2] <- 1 - mc_pourcentage_2000_mod_2[3,3]
prediction_table_2000_mod_2[4,2] <- bonne_classifcation_pourcentage_2000_mod_2

round(prediction_table_2000_mod_2 * 100, digits = 2)















# b) périodes de récession ------------------------------------------------



#---------> périodes de récession : 2007, 2008, 2009, 2018 et 2019


variables_2019_mod_2$cote_credit_2019_mod_2 <- relevel(variables_2019_mod_2$cote_credit_2019_mod_2, ref = "firm_inv")
variables_2018_mod_2$cote_credit_2018_mod_2 <- relevel(variables_2018_mod_2$cote_credit_2018_mod_2, ref = "firm_inv")
variables_2009_mod_2$cote_credit_2009_mod_2 <- relevel(variables_2009_mod_2$cote_credit_2009_mod_2, ref = "firm_inv")
variables_2008_mod_2$cote_credit_2008_mod_2 <- relevel(variables_2008_mod_2$cote_credit_2008_mod_2, ref = "firm_inv")
variables_2007_mod_2$cote_credit_2007_mod_2 <- relevel(variables_2007_mod_2$cote_credit_2007_mod_2, ref = "firm_inv")

#on choisit seulement celles qui sont significatives d'après notre analyse univariée

# on choisit celles qui ont une p-value dans analyse univariée de 0.00
# et ceux de la même catégorie s'il en reste plus d'un on check leur corrélation et si c'est forte on en choisit qu'un seul choisit

modele_2_2019 <- multinom(variables_2019_mod_2$cote_credit_2019_mod_2 ~ Marge_sur_EBITDA + 
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2019_mod_2)



table(variables_2019_mod_2$cote_credit_2019_mod_2)
length(variables_2019_mod_2$cote_credit_2019_mod_2)
confint(modele_2_2019) # permet d'avoir intervalle de confiance






# ------> to delete jusqu'à to delete en bas
# to delete

# CROSS VALIDATION AVEC LOGISITK REG MODELE 1 & 2 2019 --------------------

# MODELE 1 : 2019   --> rég Penalisée  --> see commennt l'interpréter


#CV10 <- trainControl(method = "repeatedcv", number = 10)
#lm_1_CV <- train(cote_credit_2019_mod_2 ~ Marge_sur_EBITDA + 
#                   ratio_ben_avt_impot_sur_frais_int + 
#                   ratio_Fonds_de_roulement_sur_tot_actif +
#                   Total_actif + volatilite_annuelle,
#                 data = variables_2019_mod_2, method = "multinom", trControl = CV10)
#print(lm_1_CV)


# MODELE 2 : 2019
#CV10 <- trainControl(method = "cv", number = 10)
#lm_1_CV <- train(mpg ~ horsepower, data = Auto, method = "nprne", trControl = CV10)
#print(lm_1_CV)




# ------> to delete en haut









modele_2_2018 <- multinom(variables_2018_mod_2$cote_credit_2018_mod_2 ~ Marge_sur_EBITDA +
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2018_mod_2)


modele_2_2009 <- multinom(variables_2009_mod_2$cote_credit_2009_mod_2 ~ Marge_sur_EBITDA +
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2009_mod_2)


modele_2_2008 <- multinom(variables_2008_mod_2$cote_credit_2008_mod_2 ~ Marge_sur_EBITDA +
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                          data = variables_2008_mod_2)


modele_2_2007 <- multinom(variables_2007_mod_2$cote_credit_2007_mod_2 ~ Marge_sur_EBITDA +
                            ratio_ben_avt_impot_sur_frais_int + 
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2007_mod_2)

### Pour avoir :  coef, std Error, z value, Pr(>|z|)
library(AER) # à utiliser pour les autres
round(coeftest(modele_2_2019), digits = 3) 

# ou 
library(broom)
tidy(modele_2_2019)

# ou
library(RVAideMemoire)
test.multinom(modele_2_2019, Marge_sur_EBITDA) # mais il faut réécrire toutes les variables explicatives qu'on avait utilisé dans le modèle une par une

# ou
### Calcul de la p-value
z_2019_mod_2 <- summary(modele_2_2019)$coefficients/summary(modele_2_2019)$standard.errors # = wald test selon UCLA

p_value_2019_mod_2 <- (1 - pnorm(abs(z_2019_mod_2), 0, 1)) * 2 # on multiplie par 2 car c'est un 2 tails test



#rm(list = ls())

# to delete

glance(modele_2_2019) # marche pr lm






library(AER) 
round(coeftest(modele_2_2018), digits = 3) 

library(AER)
round(coeftest(modele_2_2009), digits = 3) 

library(AER) 
round(coeftest(modele_2_2008), digits = 3) 

library(AER)
round(coeftest(modele_2_2007), digits = 3) 


### OIM = Only Intercept Model
OIM_2019_mod_2 <- multinom(cote_credit_2019_mod_2 ~ 1, data = variables_2019_mod_2)
OIM_2018_mod_2 <- multinom(cote_credit_2018_mod_2 ~ 1, data = variables_2018_mod_2)
OIM_2009_mod_2 <- multinom(cote_credit_2009_mod_2 ~ 1, data = variables_2009_mod_2)
OIM_2008_mod_2 <- multinom(cote_credit_2008_mod_2 ~ 1, data = variables_2008_mod_2)
OIM_2007_mod_2 <- multinom(cote_credit_2007_mod_2 ~ 1, data = variables_2007_mod_2)

anova(OIM_2019_mod_2, modele_2_2019) # ça me donne LR stat & Pr(Chi) # to delete INTERPRETION #from :https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html
anova(OIM_2018_mod_2, modele_2_2018)
anova(OIM_2009_mod_2, modele_2_2009)
anova(OIM_2008_mod_2, modele_2_2008)
anova(OIM_2007_mod_2, modele_2_2007)

# vérification Résultats anova = the same
lrtest(OIM_2019_mod_2, modele_2_2019)
lrtest(OIM_2018_mod_2, modele_2_2018)
lrtest(OIM_2009_mod_2, modele_2_2009)
lrtest(OIM_2008_mod_2, modele_2_2008)
lrtest(OIM_2007_mod_2, modele_2_2007)

### Pseudo Rsquare : on prend celui de Nagelkerke
library("DescTools")

PseudoR2(modele_2_2019, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_2_2018, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_2_2009, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_2_2008, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_2_2007, which = c("CoxSnell","Nagelkerke","McFadden"))

# vérifier Pseudo R-square : McFadden
library(pscl)
pR2(modele_2_2019)

# Pseudo R-carré : McFadden
L <-1-logLik(modele_2_2019)/logLik(OIM_2019_mod_2)

# Pseudo Rsquare McFadden
nnet.mod.loglik_mod_2 <- nnet:::logLik.multinom(modele_2_2019) # from modèle 1 de base
OIM_2019_mod_2.loglik <- nnet:::logLik.multinom(OIM_2019_mod_2) # from modèle en haut qu'on v1 de run
(nnet.mod.mfr2 <- as.numeric(1 - nnet.mod.loglik_mod_2/OIM_2019_mod_2.loglik))





# Calcul des mesures de la précision de la prédiction
### on va extraire les coefficients du modèle et les mettre en exponentiels

# Pour le risque relatif
exp(coef(modele_2_2019)) # to delete : voir interprétation UCLA & bookdown.org & youtube
exp(coef(modele_2_2018))
exp(coef(modele_2_2009))
exp(coef(modele_2_2008))
exp(coef(modele_2_2007))

###  les probabilités prédites pour chaque firme
head(predict_proba_2019_mod_2 <- fitted(modele_2_2019), 20) # pour le 1er il y avait 94% de chance que le modèle le prédit spec et donc 6% de chance que ça le prédit inv
# mais ce raisonnement ne tient pas
head(predict_proba_2018_mod_2 <- fitted(modele_2_2018), 20)
head(predict_proba_2009_mod_2 <- fitted(modele_2_2009), 20)
head(predict_proba_2008_mod_2 <- fitted(modele_2_2008), 20)
head(predict_proba_2007_mod_2 <- fitted(modele_2_2007), 20)

# ou vérfication
predict_proba_2019_mod_2_test_1 <- modele_2_2019$fitted.values

# ou vérification
predict_proba_2019_mod_2_test_2 <- predict(modele_2_2019, variables_2019_mod_2, type = "prob")

# to delete :       juste pour comprendre l'interprétation des prédictions de proba car ici somme des proba n'est pas égale à 1
### vérifions si somme des proba = 1
mat_predict_proba_2019_mod_2_mod_2 <- matrix(predict_proba_2019_mod_2)
sum(mat_predict_proba_2019_mod_2_mod_2)

### comparaison
sum(as.numeric(predict_proba_2019_mod_2==predict_proba_2019_mod_2_test_1)) # donc on a 151 TRUE donc c'est identique
sum(as.numeric(predict_proba_2019_mod_2==predict_proba_2019_mod_2_test_2))
sum(as.numeric(predict_proba_2019_mod_2_test_1==predict_proba_2019_mod_2_test_2))

### prédiction en termes de firme_inv et firme_spec par le modele
predict_cote_credit_2019_mod_2 <- predict(modele_2_2019, variables_2019_mod_2) # je ne vais pas interpréter ça un par un donc je le mets dans un tableau
predict_cote_credit_2018_mod_2 <- predict(modele_2_2018, variables_2018_mod_2)
predict_cote_credit_2009_mod_2 <- predict(modele_2_2009, variables_2009_mod_2)
predict_cote_credit_2008_mod_2 <- predict(modele_2_2008, variables_2008_mod_2)
predict_cote_credit_2007_mod_2 <- predict(modele_2_2007, variables_2007_mod_2)




### mettre cote à cote prediction à coté vrai vriables
head(data.frame(observed=variables_2019_mod_2$cote_credit_2019_mod_2, predicted=predict_cote_credit_2019_mod_2), 20)
head(data.frame(observed=variables_2018_mod_2$cote_credit_2018_mod_2, predicted=predict_cote_credit_2018_mod_2), 20)
head(data.frame(observed=variables_2009_mod_2$cote_credit_2009_mod_2, predicted=predict_cote_credit_2009_mod_2), 20)
head(data.frame(observed=variables_2008_mod_2$cote_credit_2008_mod_2, predicted=predict_cote_credit_2008_mod_2), 20)
head(data.frame(observed=variables_2007_mod_2$cote_credit_2007_mod_2, predicted=predict_cote_credit_2007_mod_2), 20)

### comparaison predictions qui sont tombées good par rapport au données de départ
sum(as.numeric(predict_cote_credit_2019_mod_2==variables_2019_mod_2$cote_credit_2019_mod_2)) # 115 éléments qui ont été bien prédit par le modèle conformément aux données de départ sur 151 données au total
sum(as.numeric(predict_cote_credit_2018_mod_2==variables_2018_mod_2$cote_credit_2018_mod_2))
sum(as.numeric(predict_cote_credit_2009_mod_2==variables_2009_mod_2$cote_credit_2009_mod_2))
sum(as.numeric(predict_cote_credit_2008_mod_2==variables_2008_mod_2$cote_credit_2008_mod_2))
sum(as.numeric(predict_cote_credit_2007_mod_2==variables_2007_mod_2$cote_credit_2007_mod_2))

### matrice de confusion : erreur dans les prédictions
mc_2019_mod_2 <- table(predict(modele_2_2019), variables_2019_mod_2$cote_credit_2019_mod_2)  # to delete : voir interprétation dans other doc R reg
mc_2018_mod_2 <- table(predict(modele_2_2018), variables_2018_mod_2$cote_credit_2018_mod_2) 
mc_2009_mod_2 <- table(predict(modele_2_2009), variables_2009_mod_2$cote_credit_2009_mod_2) 
mc_2008_mod_2 <- table(predict(modele_2_2008), variables_2008_mod_2$cote_credit_2008_mod_2) 
mc_2007_mod_2 <- table(predict(modele_2_2007), variables_2007_mod_2$cote_credit_2007_mod_2) 

# to delete        try to understand
###
mc_pourcentage_2019_mod_2 <- mc_2019_mod_2 / colSums(mc_2019_mod_2) # voir interprétation dans other doc R : 0.831 c'est pour colonne firm inv : = (79 * 100%) / (79+16)
mc_pourcentage_2018_mod_2 <- mc_2018_mod_2 / colSums(mc_2018_mod_2)
mc_pourcentage_2009_mod_2 <- mc_2009_mod_2 / colSums(mc_2009_mod_2)
mc_pourcentage_2008_mod_2 <- mc_2008_mod_2 / colSums(mc_2008_mod_2)
mc_pourcentage_2007_mod_2 <- mc_2007_mod_2 / colSums(mc_2007_mod_2)

# to delete way calcul from mc_2019_mod_2
81 / (81+14) # = 0.8526316
14 / (22+34) # = 0.25

22 / (81+14) # =  0.2315789
34 / (22+34) # =  0.6071429


### MISCLASSIFICATION pour présentation output
#### Pourcentage de mauvaise classification (misclassifaction)
miscalsification_pourcentage_2019_mod_2 <- 1- sum(diag(mc_2019_mod_2))/sum(mc_2019_mod_2) # from mc : (14 + 22) / 151
# to delete : voir interpretation dans l'auttre doc R

miscalsification_pourcentage_2018_mod_2 <- 1- sum(diag(mc_2018_mod_2))/sum(mc_2018_mod_2)
miscalsification_pourcentage_2009_mod_2 <- 1- sum(diag(mc_2009_mod_2))/sum(mc_2009_mod_2)
miscalsification_pourcentage_2008_mod_2 <- 1- sum(diag(mc_2008_mod_2))/sum(mc_2008_mod_2)
miscalsification_pourcentage_2007_mod_2 <- 1- sum(diag(mc_2007_mod_2))/sum(mc_2007_mod_2)

### ACCURACY DU MODEL pour présentation output
### Pourcenttage de bonne classification : la somme de la diag qu'on divise par le nombre total d'observation 
bonne_classifcation_pourcentage_2019_mod_2 <- sum(diag(mc_2019_mod_2))/sum(mc_2019_mod_2)  # ou 1 - miscalsification_pourcentage_2019_mod_2
bonne_classifcation_pourcentage_2018_mod_2 <- sum(diag(mc_2018_mod_2))/sum(mc_2018_mod_2)
bonne_classifcation_pourcentage_2009_mod_2 <- sum(diag(mc_2009_mod_2))/sum(mc_2009_mod_2)
bonne_classifcation_pourcentage_2008_mod_2 <- sum(diag(mc_2008_mod_2))/sum(mc_2008_mod_2)
bonne_classifcation_pourcentage_2007_mod_2 <- sum(diag(mc_2007_mod_2))/sum(mc_2007_mod_2)

# vérification
# MISCLASSIFICATION + ACCURACY DU MODEL
miscalsification_pourcentage_2019_mod_2+bonne_classifcation_pourcentage_2019_mod_2
miscalsification_pourcentage_2018_mod_2+bonne_classifcation_pourcentage_2018_mod_2
miscalsification_pourcentage_2009_mod_2+bonne_classifcation_pourcentage_2009_mod_2
miscalsification_pourcentage_2008_mod_2+bonne_classifcation_pourcentage_2008_mod_2
miscalsification_pourcentage_2007_mod_2+bonne_classifcation_pourcentage_2007_mod_2

#ou vérification
# ça nous donne matrice de confision et statistique
library(caret)
confusionMatrix(table(predict(modele_2_2019), variables_2019_mod_2$cote_credit_2019_mod_2)) 
confusionMatrix(table(predict(modele_2_2018), variables_2018_mod_2$cote_credit_2018_mod_2)) 
confusionMatrix(table(predict(modele_2_2009), variables_2009_mod_2$cote_credit_2009_mod_2)) 
confusionMatrix(table(predict(modele_2_2008), variables_2008_mod_2$cote_credit_2008_mod_2)) 
confusionMatrix(table(predict(modele_2_2007), variables_2007_mod_2$cote_credit_2007_mod_2)) 

# calcul pour chaque ratings sensitivity,.... 
# calcul aussi Accuracy, CI, p-value

confusionMatrix(table(predict(modele_2_2018), variables_2018_mod_2$cote_credit_2018_mod_2)) 
confusionMatrix(table(predict(modele_2_2009), variables_2009_mod_2$cote_credit_2009_mod_2)) 
confusionMatrix(table(predict(modele_2_2008), variables_2008_mod_2$cote_credit_2008_mod_2)) 
confusionMatrix(table(predict(modele_2_2007), variables_2007_mod_2$cote_credit_2007_mod_2)) 

### to delete : observation de départ en POURCENTAGE
n_2019_mod_2 <- table(variables_2019_mod_2$cote_credit_2019_mod_2) #nombre de firme pour chaque type de rating
n_2019_mod_2/sum(n_2019_mod_2) # pourcentage de firme par type de rating # la plupart des firmes ont un rating de 4




#to delete
#rm(list = ls())



### Table précision prédiction : périodes de récession : 2007, 2008, 2009, 2018 et 2019

# 2019

prediction_table_2019_mod_2 <- matrix(0,4, 2)
colnames(prediction_table_2019_mod_2) <- c("Prediction_correcte_2019", "Prediction_incorrecte")
rownames(prediction_table_2019_mod_2) <- c("Firmes_investissement", "BB", "BB_&_CCC", "Precision_Globale_de_la_Prediction")



prediction_table_2019_mod_2[1,1] <- mc_pourcentage_2019_mod_2[1,1]
prediction_table_2019_mod_2[2,1] <- mc_pourcentage_2019_mod_2[2,2]
prediction_table_2019_mod_2[3,1] <- mc_pourcentage_2019_mod_2[3,3]
prediction_table_2019_mod_2[4,1] <- bonne_classifcation_pourcentage_2019_mod_2


# to delete 
# a ajuster ligne 2 et 3
prediction_table_2019_mod_2[1,2] <- 1 - mc_pourcentage_2019_mod_2[1,1]
prediction_table_2019_mod_2[2,2] <- 1 - mc_pourcentage_2019_mod_2[2,2]
prediction_table_2019_mod_2[3,2] <- 1 - mc_pourcentage_2019_mod_2[3,3]
prediction_table_2019_mod_2[4,2] <- bonne_classifcation_pourcentage_2019_mod_2

round(prediction_table_2019_mod_2 * 100, digits = 2)





# 2018
prediction_table_2018_mod_2 <- matrix(0,4, 2)
colnames(prediction_table_2018_mod_2) <- c("Prediction_correcte_2018", "Prediction_incorrecte")
rownames(prediction_table_2018_mod_2) <- c("Firmes_investissement", "BB", "BB_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2018_mod_2[1,1] <- mc_pourcentage_2018_mod_2[1,1]
prediction_table_2018_mod_2[2,1] <- mc_pourcentage_2018_mod_2[2,2]
prediction_table_2018_mod_2[3,1] <- mc_pourcentage_2018_mod_2[3,3]
prediction_table_2018_mod_2[4,1] <- bonne_classifcation_pourcentage_2018_mod_2


# to delete 
# a ajuster ligne 2 et 3
prediction_table_2018_mod_2[1,2] <- 1 - mc_pourcentage_2018_mod_2[1,1]
prediction_table_2018_mod_2[2,2] <- 1 - mc_pourcentage_2018_mod_2[2,2]
prediction_table_2018_mod_2[3,2] <- 1 - mc_pourcentage_2018_mod_2[3,3]
prediction_table_2018_mod_2[4,2] <- bonne_classifcation_pourcentage_2018_mod_2

round(prediction_table_2018_mod_2 * 100, digits = 2)




# 2009
prediction_table_2009_mod_2 <- matrix(0,4, 2)
colnames(prediction_table_2009_mod_2) <- c("Prediction_correcte_2009", "Prediction_incorrecte")
rownames(prediction_table_2009_mod_2) <- c("Firmes_investissement", "BB", "BB_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2009_mod_2[1,1] <- mc_pourcentage_2009_mod_2[1,1]
prediction_table_2009_mod_2[2,1] <- mc_pourcentage_2009_mod_2[2,2]
prediction_table_2009_mod_2[3,1] <- mc_pourcentage_2009_mod_2[3,3]
prediction_table_2009_mod_2[4,1] <- bonne_classifcation_pourcentage_2009_mod_2


prediction_table_2009_mod_2[1,2] <- 1 - mc_pourcentage_2009_mod_2[1,1]
prediction_table_2009_mod_2[2,2] <- 1 - mc_pourcentage_2009_mod_2[2,2]
prediction_table_2009_mod_2[3,2] <- 1 - mc_pourcentage_2009_mod_2[3,3]
prediction_table_2009_mod_2[4,2] <- bonne_classifcation_pourcentage_2009_mod_2

round(prediction_table_2009_mod_2 * 100, digits = 2)







# 2008
prediction_table_2008_mod_2 <- matrix(0,4, 2)
colnames(prediction_table_2008_mod_2) <- c("Prediction_correcte_2008", "Prediction_incorrecte")
rownames(prediction_table_2008_mod_2) <- c("Firmes_investissement", "BB", "BB_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2008_mod_2[1,1] <- mc_pourcentage_2008_mod_2[1,1]
prediction_table_2008_mod_2[2,1] <- mc_pourcentage_2008_mod_2[2,2]
prediction_table_2008_mod_2[3,1] <- mc_pourcentage_2008_mod_2[3,3]
prediction_table_2008_mod_2[4,1] <- bonne_classifcation_pourcentage_2008_mod_2


prediction_table_2008_mod_2[1,2] <- 1 - mc_pourcentage_2008_mod_2[1,1]
prediction_table_2008_mod_2[2,2] <- 1 - mc_pourcentage_2008_mod_2[2,2]
prediction_table_2008_mod_2[3,2] <- 1 - mc_pourcentage_2008_mod_2[3,3]
prediction_table_2008_mod_2[4,2] <- bonne_classifcation_pourcentage_2008_mod_2

round(prediction_table_2008_mod_2 * 100, digits = 2)






# 2007
prediction_table_2007_mod_2 <- matrix(0,4, 2)
colnames(prediction_table_2007_mod_2) <- c("Prediction_correcte_2007", "Prediction_incorrecte")
rownames(prediction_table_2007_mod_2) <- c("Firmes_investissement", "BB", "BB_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2007_mod_2[1,1] <- mc_pourcentage_2007_mod_2[1,1]
prediction_table_2007_mod_2[2,1] <- mc_pourcentage_2007_mod_2[2,2]
prediction_table_2007_mod_2[3,1] <- mc_pourcentage_2007_mod_2[3,3]
prediction_table_2007_mod_2[4,1] <- bonne_classifcation_pourcentage_2007_mod_2


# to delete 
# a ajuster ligne 2 et 3

prediction_table_2007_mod_2[1,2] <- 1 - mc_pourcentage_2007_mod_2[1,1]
prediction_table_2007_mod_2[2,2] <- 1 - mc_pourcentage_2007_mod_2[2,2]
prediction_table_2007_mod_2[3,2] <- 1 - mc_pourcentage_2007_mod_2[3,3]
prediction_table_2007_mod_2[4,2] <- bonne_classifcation_pourcentage_2007_mod_2

round(prediction_table_2007_mod_2 * 100, digits = 2)




# to delete

#rm(list = ls())




# to delete
# Modele 3 juste copier modele 2 et remplacer all 
# modele_2 par modele_3
# et
# mod_2 par mod_3





















# MODELE 3 ----------------------------------------------------------------




# 1)	Modèle 3 avec 5 catégories de cote de crédit : AAA_&_AA, A, BBB, BB et B_&_CCC



# création des bloc AAA_&_AA et B_CCC

# --->
# to delete
variables_2019$binaire_cote_credit_2019
table(variables_2019$binaire_cote_credit_2019)
variables_2019$Cote_credit
table(variables_2019$Cote_credit)

# --->


cote_credt_modele_3 <- c("AAA"="AAA_&_AA", "AA"="AAA_&_AA", "A"="A", "BBB"="BBB", 
                         "BB"="BB", "B"="B_&_CCC", "CCC"="B_&_CCC")


# to delete
#rm(list = ls())



# creons la cote de credit du modele 2


cote_credit_2019_mod_3 <- variables_2019$Cote_credit # extraction cote de credit 2019
cote_credit_2019_mod_3 <- cote_credt_modele_3[cote_credit_2019_mod_3]
table(cote_credit_2019_mod_3)
variables_2019_mod_3 <- cbind(cote_credit_2019_mod_3, variables_2019)



cote_credit_2018_mod_3 <- variables_2018$Cote_credit # extraction cote de credit 2018
cote_credit_2018_mod_3 <- cote_credt_modele_3[cote_credit_2018_mod_3]
table(cote_credit_2018_mod_3)
variables_2018_mod_3 <- cbind(cote_credit_2018_mod_3, variables_2018)



cote_credit_2016_mod_3 <- variables_2016$Cote_credit # extraction cote de credit 2016
cote_credit_2016_mod_3 <- cote_credt_modele_3[cote_credit_2016_mod_3]
table(cote_credit_2016_mod_3)
variables_2016_mod_3 <- cbind(cote_credit_2016_mod_3, variables_2016)



cote_credit_2009_mod_3 <- variables_2009$Cote_credit # extraction cote de credit 2009
cote_credit_2009_mod_3 <- cote_credt_modele_3[cote_credit_2009_mod_3]
table(cote_credit_2009_mod_3)
variables_2009_mod_3 <- cbind(cote_credit_2009_mod_3, variables_2009)



cote_credit_2008_mod_3 <- variables_2008$Cote_credit # extraction cote de credit 2008
cote_credit_2008_mod_3 <- cote_credt_modele_3[cote_credit_2008_mod_3]
table(cote_credit_2008_mod_3)
variables_2008_mod_3 <- cbind(cote_credit_2008_mod_3, variables_2008)



cote_credit_2007_mod_3 <- variables_2007$Cote_credit # extraction cote de credit 2007
cote_credit_2007_mod_3 <- cote_credt_modele_3[cote_credit_2007_mod_3]
table(cote_credit_2007_mod_3)
variables_2007_mod_3 <- cbind(cote_credit_2007_mod_3, variables_2007)



cote_credit_2000_mod_3 <- variables_2000$Cote_credit # extraction cote de credit 2000
cote_credit_2000_mod_3 <- cote_credt_modele_3[cote_credit_2000_mod_3]
table(cote_credit_2000_mod_3)
variables_2000_mod_3 <- cbind(cote_credit_2000_mod_3, variables_2000)







# to deleta
#rm(list = ls())





#----------> méttons nos cotes de crédit en factor


# POUR 2000, on va seulement mettre 2 niveaux étant donnée que BB_&_CCC n'existe pas car sinon ça fausse notre régression
variables_2000_mod_3$cote_credit_2000_mod_3 <- factor(variables_2000_mod_3$cote_credit_2000_mod_3, levels = c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC"))
variables_2007_mod_3$cote_credit_2007_mod_3 <- factor(variables_2007_mod_3$cote_credit_2007_mod_3, levels = c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC"))
variables_2008_mod_3$cote_credit_2008_mod_3 <- factor(variables_2008_mod_3$cote_credit_2008_mod_3, levels = c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC"))
variables_2009_mod_3$cote_credit_2009_mod_3 <- factor(variables_2009_mod_3$cote_credit_2009_mod_3, levels = c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC"))
variables_2016_mod_3$cote_credit_2016_mod_3 <- factor(variables_2016_mod_3$cote_credit_2016_mod_3, levels = c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC"))
variables_2018_mod_3$cote_credit_2018_mod_3 <- factor(variables_2018_mod_3$cote_credit_2018_mod_3, levels = c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC"))
variables_2019_mod_3$cote_credit_2019_mod_3 <- factor(variables_2019_mod_3$cote_credit_2019_mod_3, levels = c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC"))



# a) périodes d'expansion -------------------------------------------------



#---------> périodes d'expansion : 2000 et 2016 : dans le modèle 2 on ne change pas la référence


variables_2016_mod_3$cote_credit_2016_mod_3 <- relevel(variables_2016_mod_3$cote_credit_2016_mod_3, ref = "AAA_&_AA")
variables_2000_mod_3$cote_credit_2000_mod_3 <- relevel(variables_2000_mod_3$cote_credit_2000_mod_3, ref = "AAA_&_AA")




modele_3_2016 <- multinom(variables_2016_mod_3$cote_credit_2016_mod_3 ~ Marge_sur_EBITDA +
                            Rendement_sur_cap_prop  +
                            ratio_ben_avt_impot_sur_frais_int + ratio_B_non_rep_sur_Tot_actif +
                            ratio_tot_dette_sur_tot_actif + ratio_Flux_de_TR_expl_sur_passif_cour +
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2016_mod_3)     


confint(modele_3_2016) # permet d'avoir intervalle de confiance

table(variables_2016_mod_3$cote_credit_2016_mod_3)
length(variables_2016_mod_3$cote_credit_2016_mod_3)



#suite

modele_3_2000 <- multinom(variables_2000_mod_3$cote_credit_2000_mod_3 ~ Marge_sur_EBITDA +
                            Rendement_sur_cap_prop  +
                            ratio_ben_avt_impot_sur_frais_int + ratio_B_non_rep_sur_Tot_actif_2000 +
                            ratio_tot_dette_sur_tot_actif + ratio_Flux_de_TR_expl_sur_passif_cour +
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2000_mod_3)      
                            
                      

      
#-------------------->                            
# to delete

#Marge_sur_EBITDA +
#Rendement_sur_cap_prop +         --->       Rendement_sur_actif -->delete car fortement corrélée avec rendement sur capitaux propre 0.7
#ratio_ben_avt_impot_sur_frais_int + ratio_B_non_rep_sur_Tot_actif_2000 +
#Ratio_Fonds_de_roulmt_sur_ventes + ----> -->delete car fortement corrélée avec ratio fonds de roulement sur tot actif 0.8
# ---->      ratio_Fonds_de_roulement_sur_tot_actif 
#Total_actif + volatilite_annuelle,
#data = variables_2000_mod_3) 
#------------------>




### Pour avoir :  coef, std Error, z value, Pr(>|z|)
library(AER) # à utiliser pour les autres
round(coeftest(modele_3_2016), digits = 3) 
round(coeftest(modele_3_2000), digits = 3) 

# ou 
library(broom)
tidy(modele_3_2016)

# ou
library(RVAideMemoire)
test.multinom(modele_3_2016, Marge_sur_EBITDA) # mais il faut réécrire toutes les variables explicatives qu'on avait utilisé dans le modèle une par une

# ou
### Calcul de la p-value
z_2016_mod_3 <- summary(modele_3_2016)$coefficients/summary(modele_3_2016)$standard.errors # = wald test selon UCLA

p_value_2016_mod_3 <- (1 - pnorm(abs(z_2016_mod_3), 0, 1)) * 2 # on multiplie par 2 car c'est un 2 tails test





### OIM = Only Intercept Model
OIM_2016_mod_3 <- multinom(cote_credit_2016_mod_3 ~ 1, data = variables_2016_mod_3)
OIM_2000_mod_3 <- multinom(cote_credit_2000_mod_3 ~ 1, data = variables_2000_mod_3)

anova(OIM_2016_mod_3, modele_3_2016) # ça me donne LR stat & Pr(Chi) # to delete INTERPRETION #from :https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html
anova(OIM_2000_mod_3, modele_3_2000)

# vérification Résultats anova = the same
lrtest(OIM_2016_mod_3, modele_3_2016)
lrtest(OIM_2000_mod_3, modele_3_2000)

### Pseudo Rsquare : on prend celui de Nagelkerke
library("DescTools")

PseudoR2(modele_3_2016, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_3_2000, which = c("CoxSnell","Nagelkerke","McFadden"))

# vérifier Pseudo R-square : McFadden
library(pscl)
pR2(modele_3_2016)

# Pseudo R-carré : McFadden
L_mod_3 <-1-logLik(modele_3_2016)/logLik(OIM_2016_mod_3)

# Pseudo Rsquare McFadden
nnet.mod.loglik_mod_3 <- nnet:::logLik.multinom(modele_3_2016) # from modèle 1 de base
OIM_2016_mod_3.loglik <- nnet:::logLik.multinom(OIM_2016_mod_3) # from modèle en haut qu'on v1 de run
(nnet.mod.mfr2 <- as.numeric(1 - nnet.mod.loglik_mod_3/OIM_2016_mod_3.loglik))


# Calcul des mesures de la précision de la prédiction
### on va extraire les coefficients du modèle et les mettre en exponentiels

# Pour le risque relatif
exp(coef(modele_3_2016)) # to delete : voir interprétation UCLA & bookdown.org & youtube
exp(coef(modele_3_2000))

###  les probabilités prédites pour chaque firme
head(predict_proba_2016_mod_3 <- fitted(modele_3_2016), 20) # pour le 1er il y avait 94% de chance que le modèle le prédit spec et donc 6% de chance que ça le prédit inv
# mais ce raisonnement ne tient pas
head(predict_proba_2000_mod_3 <- fitted(modele_3_2000), 20)

# ou vérfication
predict_proba_2016_mod_3_test_1 <- modele_3_2016$fitted.values

# ou vérification
predict_proba_2016_mod_3_test_2 <- predict(modele_3_2016, variables_2016_mod_3, type = "prob")

# to delete :       juste pour comprendre l'interprétation des prédictions de proba car ici somme des proba n'est pas égale à 1
### vérifions si somme des proba = 1
mat_predict_proba_2016_mod_3_mod_3 <- matrix(predict_proba_2016_mod_3)
sum(mat_predict_proba_2016_mod_3_mod_3)

### comparaison
sum(as.numeric(predict_proba_2016_mod_3==predict_proba_2016_mod_3_test_1))
sum(as.numeric(predict_proba_2016_mod_3==predict_proba_2016_mod_3_test_2))
sum(as.numeric(predict_proba_2016_mod_3_test_1==predict_proba_2016_mod_3_test_2))

### prédiction en termes de firme_inv et firme_spec par le modele
predict_cote_credit_2016_mod_3 <- predict(modele_3_2016, variables_2016_mod_3)
predict_cote_credit_2000_mod_3 <- predict(modele_3_2000, variables_2000_mod_3)

### mettre cote à cote prediction à coté vrai vriables
head(data.frame(observed=variables_2016_mod_3$cote_credit_2016_mod_3, predicted=predict_cote_credit_2016_mod_3), 20)
head(data.frame(observed=variables_2000_mod_3$cote_credit_2000_mod_3, predicted=predict_cote_credit_2000_mod_3), 20)

### comparaison predictions qui sont tombées good par rapport au données de départ
sum(as.numeric(predict_cote_credit_2016_mod_3==variables_2016_mod_3$cote_credit_2016_mod_3)) # 115 éléments qui ont été bien prédit par le modèle conformément aux données de départ sur 151 données au total
sum(as.numeric(predict_cote_credit_2000_mod_3==variables_2000_mod_3$cote_credit_2000_mod_3))

### matrice de confusion : erreur dans les prédictions
mc_2016_mod_3 <- table(predict(modele_3_2016), variables_2016_mod_3$cote_credit_2016_mod_3)  # to delete : voir interprétation dans other doc R reg
mc_2000_mod_3 <- table(predict(modele_3_2000), variables_2000_mod_3$cote_credit_2000_mod_3) 

# to delete        try to understand
### en Pourcentage # [2] ---> c'est comme mon R^2 (R square)
mc_pourcentage_2016_mod_3 <- mc_2016_mod_3 / colSums(mc_2016_mod_3) # voir interprétation dans other doc R
mc_pourcentage_2000_mod_3 <- mc_2000_mod_3 / colSums(mc_2000_mod_3)

# to delete way calcul from mc_2016_mod_3
81 / (81+14) # = 0.8526316
14 / (22+34) # = 0.25

22 / (81+14) # =  0.2315789
34 / (22+34) # =  0.6071429


### MISCLASSIFICATION pour présentation output
#### Pourcentage de mauvaise classification (misclassifaction)
miscalsification_pourcentage_2016_mod_3 <- 1- sum(diag(mc_2016_mod_3))/sum(mc_2016_mod_3) # from mc : (14 + 22) / 151
# to delete : voir interpretation dans l'auttre doc R

miscalsification_pourcentage_2000_mod_3 <- 1- sum(diag(mc_2000_mod_3))/sum(mc_2000_mod_3)

### ACCURACY DU MODEL pour présentation output
### Pourcenttage de bonne classification : la somme de la diag qu'on divise par le nombre total d'observation 
bonne_classifcation_pourcentage_2016_mod_3 <- sum(diag(mc_2016_mod_3))/sum(mc_2016_mod_3)  # ou 1 - miscalsification_pourcentage_2016_mod_3
bonne_classifcation_pourcentage_2000_mod_3 <- sum(diag(mc_2000_mod_3))/sum(mc_2000_mod_3)

# vérification
# MISCLASSIFICATION + ACCURACY DU MODEL
miscalsification_pourcentage_2016_mod_3+bonne_classifcation_pourcentage_2016_mod_3
miscalsification_pourcentage_2000_mod_3+bonne_classifcation_pourcentage_2000_mod_3

#ou vérification
# ça nous donne matrice de confision et statistique
library(caret)
confusionMatrix(table(predict(modele_3_2016), variables_2016_mod_3$cote_credit_2016_mod_3)) 
confusionMatrix(table(predict(modele_3_2000), variables_2000_mod_3$cote_credit_2000_mod_3)) 

# calcul pour chaque ratings sensitivity,.... 
# calcul aussi Accuracy, CI, p-value

confusionMatrix(table(predict(modele_3_2000), variables_2000_mod_3$cote_credit_2000_mod_3)) 

### to delete : observation de départ en POURCENTAGE
n_mod_3 <- table(variables_2016_mod_3$cote_credit_2016_mod_3) #nombre de firme pour chaque type de rating
n_mod_3/sum(n_mod_3) # pourcentage de firme par type de rating # la plupart des firmes ont un rating de 4



### Table précision prédiction : périodes d'expansion : 2000 et 2016

# 2016


# to delete : check it if conforme

prediction_table_2016_mod_3 <- matrix(0,6, 2)
colnames(prediction_table_2016_mod_3) <- c("Prediction_correcte_2016", "Prediction_incorrecte")
rownames(prediction_table_2016_mod_3) <- c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2016_mod_3[1,1] <- mc_pourcentage_2016_mod_3[1,1]
prediction_table_2016_mod_3[2,1] <- mc_pourcentage_2016_mod_3[2,2]
prediction_table_2016_mod_3[3,1] <- mc_pourcentage_2016_mod_3[3,3]
prediction_table_2016_mod_3[4,1] <- mc_pourcentage_2016_mod_3[4,4]
prediction_table_2016_mod_3[5,1] <- mc_pourcentage_2016_mod_3[5,5]
prediction_table_2016_mod_3[6,1] <- bonne_classifcation_pourcentage_2016_mod_3

prediction_table_2016_mod_3[1,2] <- 1 - mc_pourcentage_2016_mod_3[1,1]
prediction_table_2016_mod_3[2,2] <- 1 - mc_pourcentage_2016_mod_3[2,2]
prediction_table_2016_mod_3[3,2] <- 1 - mc_pourcentage_2016_mod_3[3,3]
prediction_table_2016_mod_3[4,2] <- 1 - mc_pourcentage_2016_mod_3[4,4]
prediction_table_2016_mod_3[5,2] <- 1 - mc_pourcentage_2016_mod_3[5,5]
prediction_table_2016_mod_3[6,2] <- bonne_classifcation_pourcentage_2016_mod_3

round(prediction_table_2016_mod_3 * 100, digits = 2)









# 2000

prediction_table_2000_mod_3 <- matrix(0,6, 2)
colnames(prediction_table_2000_mod_3) <- c("Prediction_correcte_2000", "Prediction_incorrecte")
rownames(prediction_table_2000_mod_3) <- c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2000_mod_3[1,1] <- mc_pourcentage_2000_mod_3[1,1]
prediction_table_2000_mod_3[2,1] <- mc_pourcentage_2000_mod_3[2,2]
prediction_table_2000_mod_3[3,1] <- mc_pourcentage_2000_mod_3[3,3]
prediction_table_2000_mod_3[4,1] <- mc_pourcentage_2000_mod_3[4,4]
prediction_table_2000_mod_3[5,1] <- mc_pourcentage_2000_mod_3[5,5]
prediction_table_2000_mod_3[6,1] <- bonne_classifcation_pourcentage_2000_mod_3

prediction_table_2000_mod_3[1,2] <- 1 - mc_pourcentage_2000_mod_3[1,1]
prediction_table_2000_mod_3[2,2] <- 1 - mc_pourcentage_2000_mod_3[2,2]
prediction_table_2000_mod_3[3,2] <- 1 - mc_pourcentage_2000_mod_3[3,3]
prediction_table_2000_mod_3[4,2] <- 1 - mc_pourcentage_2000_mod_3[4,4]
prediction_table_2000_mod_3[5,2] <- 1 - mc_pourcentage_2000_mod_3[5,5]
prediction_table_2000_mod_3[6,2] <- bonne_classifcation_pourcentage_2000_mod_3

round(prediction_table_2000_mod_3 * 100, digits = 2)








# b) période récession ----------------------------------------------------




# to delete 
# delete tous les attach car dans chaque modele je fait 'data = ... ' qui précise ou je take les données
# le faire après check si output the same, apres delete all of them



#---------> périodes de récession : 2007, 2008, 2009, 2018 et 2019


variables_2019_mod_3$cote_credit_2019_mod_3 <- relevel(variables_2019_mod_3$cote_credit_2019_mod_3, ref = "AAA_&_AA")
variables_2018_mod_3$cote_credit_2018_mod_3 <- relevel(variables_2018_mod_3$cote_credit_2018_mod_3, ref = "AAA_&_AA")
variables_2009_mod_3$cote_credit_2009_mod_3 <- relevel(variables_2009_mod_3$cote_credit_2009_mod_3, ref = "AAA_&_AA")
variables_2008_mod_3$cote_credit_2008_mod_3 <- relevel(variables_2008_mod_3$cote_credit_2008_mod_3, ref = "AAA_&_AA")
variables_2007_mod_3$cote_credit_2007_mod_3 <- relevel(variables_2007_mod_3$cote_credit_2007_mod_3, ref = "AAA_&_AA")

#on choisit seulement celles qui sont significatives d'après notre analyse univariée

# on choisit celles qui ont une p-value dans analyse univariée de 0.00
# et ceux de la même catégorie s'il en reste plus d'un on check leur corrélation et si c'est forte on en choisit qu'un seul choisit

modele_3_2019 <- multinom(variables_2019_mod_3$cote_credit_2019_mod_3 ~ Marge_sur_EBITDA +
                            Rendement_sur_cap_prop  +
                            ratio_ben_avt_impot_sur_frais_int + ratio_B_non_rep_sur_Tot_actif +
                            ratio_tot_dette_sur_tot_actif + ratio_Flux_de_TR_expl_sur_passif_cour +
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2019_mod_3)      
                            
                            
                          



table(variables_2019_mod_3$cote_credit_2019_mod_3)
length(variables_2019_mod_3$cote_credit_2019_mod_3)
confint(modele_3_2019) # permet d'avoir intervalle de confiance


modele_3_2018 <- multinom(variables_2018_mod_3$cote_credit_2018_mod_3 ~ Marge_sur_EBITDA +
                            Rendement_sur_cap_prop  +
                            ratio_ben_avt_impot_sur_frais_int + ratio_B_non_rep_sur_Tot_actif +
                            ratio_tot_dette_sur_tot_actif + ratio_Flux_de_TR_expl_sur_passif_cour +
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2018_mod_3)      
                            



modele_3_2009 <- multinom(variables_2009_mod_3$cote_credit_2009_mod_3 ~ Marge_sur_EBITDA +
                            Rendement_sur_cap_prop  +
                            ratio_ben_avt_impot_sur_frais_int + ratio_B_non_rep_sur_Tot_actif +
                            ratio_tot_dette_sur_tot_actif + ratio_Flux_de_TR_expl_sur_passif_cour +
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2009_mod_3)      

                            

modele_3_2008 <- multinom(variables_2008_mod_3$cote_credit_2008_mod_3 ~ Marge_sur_EBITDA +
                            Rendement_sur_cap_prop  +
                            ratio_ben_avt_impot_sur_frais_int + ratio_B_non_rep_sur_Tot_actif +
                            ratio_tot_dette_sur_tot_actif + ratio_Flux_de_TR_expl_sur_passif_cour +
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2008_mod_3)      
                            
                            
modele_3_2007 <- multinom(variables_2007_mod_3$cote_credit_2007_mod_3 ~ Marge_sur_EBITDA +
                            Rendement_sur_cap_prop  +
                            ratio_ben_avt_impot_sur_frais_int + ratio_B_non_rep_sur_Tot_actif +
                            ratio_tot_dette_sur_tot_actif + ratio_Flux_de_TR_expl_sur_passif_cour +
                            ratio_Fonds_de_roulement_sur_tot_actif +
                            Total_actif + volatilite_annuelle,
                            data = variables_2007_mod_3)      




### Pour avoir :  coef, std Error, z value, Pr(>|z|)
library(AER) # à utiliser pour les autres
round(coeftest(modele_3_2019), digits = 3) 

# ou 
library(broom)
tidy(modele_3_2019)

# ou
library(RVAideMemoire)
test.multinom(modele_3_2019, Marge_sur_EBITDA) # mais il faut réécrire toutes les variables explicatives qu'on avait utilisé dans le modèle une par une

# ou
### Calcul de la p-value
z_2019_mod_3 <- summary(modele_3_2019)$coefficients/summary(modele_3_2019)$standard.errors # = wald test selon UCLA

p_value_2019_mod_3 <- (1 - pnorm(abs(z_2019_mod_3), 0, 1)) * 2 # on multiplie par 2 car c'est un 2 tails test



#rm(list = ls())

# to delete

glance(modele_3_2019) # marche pr lm






library(AER) 
round(coeftest(modele_3_2018), digits = 3) 

library(AER)
round(coeftest(modele_3_2009), digits = 3) 

library(AER) 
round(coeftest(modele_3_2008), digits = 3) 

library(AER)
round(coeftest(modele_3_2007), digits = 3) 


### OIM = Only Intercept Model
OIM_2019_mod_3 <- multinom(cote_credit_2019_mod_3 ~ 1, data = variables_2019_mod_3)
OIM_2018_mod_3 <- multinom(cote_credit_2018_mod_3 ~ 1, data = variables_2018_mod_3)
OIM_2009_mod_3 <- multinom(cote_credit_2009_mod_3 ~ 1, data = variables_2009_mod_3)
OIM_2008_mod_3 <- multinom(cote_credit_2008_mod_3 ~ 1, data = variables_2008_mod_3)
OIM_2007_mod_3 <- multinom(cote_credit_2007_mod_3 ~ 1, data = variables_2007_mod_3)

anova(OIM_2019_mod_3, modele_3_2019) # ça me donne LR stat & Pr(Chi) # to delete INTERPRETION #from :https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html
anova(OIM_2018_mod_3, modele_3_2018)
anova(OIM_2009_mod_3, modele_3_2009)
anova(OIM_2008_mod_3, modele_3_2008)
anova(OIM_2007_mod_3, modele_3_2007)

# vérification Résultats anova = the same
lrtest(OIM_2019_mod_3, modele_3_2019)
lrtest(OIM_2018_mod_3, modele_3_2018)
lrtest(OIM_2009_mod_3, modele_3_2009)
lrtest(OIM_2008_mod_3, modele_3_2008)
lrtest(OIM_2007_mod_3, modele_3_2007)

### Pseudo Rsquare : on prend celui de Nagelkerke
library("DescTools")

PseudoR2(modele_3_2019, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_3_2018, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_3_2009, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_3_2008, which = c("CoxSnell","Nagelkerke","McFadden"))
PseudoR2(modele_3_2007, which = c("CoxSnell","Nagelkerke","McFadden"))

# vérifier Pseudo R-square : McFadden
library(pscl)
pR2(modele_3_2019)

# Pseudo R-carré : McFadden
L <-1-logLik(modele_3_2019)/logLik(OIM_2019_mod_3)

# Pseudo Rsquare McFadden
nnet.mod.loglik_mod_3 <- nnet:::logLik.multinom(modele_3_2019) # from modèle 1 de base
OIM_2019_mod_3.loglik <- nnet:::logLik.multinom(OIM_2019_mod_3) # from modèle en haut qu'on v1 de run
(nnet.mod.mfr2 <- as.numeric(1 - nnet.mod.loglik_mod_3/OIM_2019_mod_3.loglik))





# Calcul des mesures de la précision de la prédiction
### on va extraire les coefficients du modèle et les mettre en exponentiels

# Pour le risque relatif
exp(coef(modele_3_2019)) # to delete : voir interprétation UCLA & bookdown.org & youtube
exp(coef(modele_3_2018))
exp(coef(modele_3_2009))
exp(coef(modele_3_2008))
exp(coef(modele_3_2007))

###  les probabilités prédites pour chaque firme
head(predict_proba_2019_mod_3 <- fitted(modele_3_2019), 20) # pour le 1er il y avait 94% de chance que le modèle le prédit spec et donc 6% de chance que ça le prédit inv
# mais ce raisonnement ne tient pas
head(predict_proba_2018_mod_3 <- fitted(modele_3_2018), 20)
head(predict_proba_2009_mod_3 <- fitted(modele_3_2009), 20)
head(predict_proba_2008_mod_3 <- fitted(modele_3_2008), 20)
head(predict_proba_2007_mod_3 <- fitted(modele_3_2007), 20)

# ou vérfication
predict_proba_2019_mod_3_test_1 <- modele_3_2019$fitted.values

# ou vérification
predict_proba_2019_mod_3_test_2 <- predict(modele_3_2019, variables_2019_mod_3, type = "prob")

# to delete :       juste pour comprendre l'interprétation des prédictions de proba car ici somme des proba n'est pas égale à 1
### vérifions si somme des proba = 1
mat_predict_proba_2019_mod_3_mod_3 <- matrix(predict_proba_2019_mod_3)
sum(mat_predict_proba_2019_mod_3_mod_3)

### comparaison
sum(as.numeric(predict_proba_2019_mod_3==predict_proba_2019_mod_3_test_1)) # donc on a 151 TRUE donc c'est identique
sum(as.numeric(predict_proba_2019_mod_3==predict_proba_2019_mod_3_test_2))
sum(as.numeric(predict_proba_2019_mod_3_test_1==predict_proba_2019_mod_3_test_2))

### prédiction en termes de firme_inv et firme_spec par le modele
predict_cote_credit_2019_mod_3 <- predict(modele_3_2019, variables_2019_mod_3) # je ne vais pas interpréter ça un par un donc je le mets dans un tableau
predict_cote_credit_2018_mod_3 <- predict(modele_3_2018, variables_2018_mod_3)
predict_cote_credit_2009_mod_3 <- predict(modele_3_2009, variables_2009_mod_3)
predict_cote_credit_2008_mod_3 <- predict(modele_3_2008, variables_2008_mod_3)
predict_cote_credit_2007_mod_3 <- predict(modele_3_2007, variables_2007_mod_3)




### mettre cote à cote prediction à coté vrai vriables
head(data.frame(observed=variables_2019_mod_3$cote_credit_2019_mod_3, predicted=predict_cote_credit_2019_mod_3), 20)
head(data.frame(observed=variables_2018_mod_3$cote_credit_2018_mod_3, predicted=predict_cote_credit_2018_mod_3), 20)
head(data.frame(observed=variables_2009_mod_3$cote_credit_2009_mod_3, predicted=predict_cote_credit_2009_mod_3), 20)
head(data.frame(observed=variables_2008_mod_3$cote_credit_2008_mod_3, predicted=predict_cote_credit_2008_mod_3), 20)
head(data.frame(observed=variables_2007_mod_3$cote_credit_2007_mod_3, predicted=predict_cote_credit_2007_mod_3), 20)

### comparaison predictions qui sont tombées good par rapport au données de départ
sum(as.numeric(predict_cote_credit_2019_mod_3==variables_2019_mod_3$cote_credit_2019_mod_3)) # 115 éléments qui ont été bien prédit par le modèle conformément aux données de départ sur 151 données au total
sum(as.numeric(predict_cote_credit_2018_mod_3==variables_2018_mod_3$cote_credit_2018_mod_3))
sum(as.numeric(predict_cote_credit_2009_mod_3==variables_2009_mod_3$cote_credit_2009_mod_3))
sum(as.numeric(predict_cote_credit_2008_mod_3==variables_2008_mod_3$cote_credit_2008_mod_3))
sum(as.numeric(predict_cote_credit_2007_mod_3==variables_2007_mod_3$cote_credit_2007_mod_3))

### matrice de confusion : erreur dans les prédictions
mc_2019_mod_3 <- table(predict(modele_3_2019), variables_2019_mod_3$cote_credit_2019_mod_3)  # to delete : voir interprétation dans other doc R reg
mc_2018_mod_3 <- table(predict(modele_3_2018), variables_2018_mod_3$cote_credit_2018_mod_3) 
mc_2009_mod_3 <- table(predict(modele_3_2009), variables_2009_mod_3$cote_credit_2009_mod_3) 
mc_2008_mod_3 <- table(predict(modele_3_2008), variables_2008_mod_3$cote_credit_2008_mod_3) 
mc_2007_mod_3 <- table(predict(modele_3_2007), variables_2007_mod_3$cote_credit_2007_mod_3) 

# to delete        try to understand
###
mc_pourcentage_2019_mod_3 <- mc_2019_mod_3 / colSums(mc_2019_mod_3) # voir interprétation dans other doc R : 0.831 c'est pour colonne firm inv : = (79 * 100%) / (79+16)
mc_pourcentage_2018_mod_3 <- mc_2018_mod_3 / colSums(mc_2018_mod_3)
mc_pourcentage_2009_mod_3 <- mc_2009_mod_3 / colSums(mc_2009_mod_3)
mc_pourcentage_2008_mod_3 <- mc_2008_mod_3 / colSums(mc_2008_mod_3)
mc_pourcentage_2007_mod_3 <- mc_2007_mod_3 / colSums(mc_2007_mod_3)



### MISCLASSIFICATION pour présentation output
#### Pourcentage de mauvaise classification (misclassifaction)
miscalsification_pourcentage_2019_mod_3 <- 1- sum(diag(mc_2019_mod_3))/sum(mc_2019_mod_3) # from mc : (14 + 22) / 151
# to delete : voir interpretation dans l'auttre doc R

miscalsification_pourcentage_2018_mod_3 <- 1- sum(diag(mc_2018_mod_3))/sum(mc_2018_mod_3)
miscalsification_pourcentage_2009_mod_3 <- 1- sum(diag(mc_2009_mod_3))/sum(mc_2009_mod_3)
miscalsification_pourcentage_2008_mod_3 <- 1- sum(diag(mc_2008_mod_3))/sum(mc_2008_mod_3)
miscalsification_pourcentage_2007_mod_3 <- 1- sum(diag(mc_2007_mod_3))/sum(mc_2007_mod_3)

### ACCURACY DU MODEL pour présentation output
### Pourcenttage de bonne classification : la somme de la diag qu'on divise par le nombre total d'observation 
bonne_classifcation_pourcentage_2019_mod_3 <- sum(diag(mc_2019_mod_3))/sum(mc_2019_mod_3)  # ou 1 - miscalsification_pourcentage_2019_mod_3
bonne_classifcation_pourcentage_2018_mod_3 <- sum(diag(mc_2018_mod_3))/sum(mc_2018_mod_3)
bonne_classifcation_pourcentage_2009_mod_3 <- sum(diag(mc_2009_mod_3))/sum(mc_2009_mod_3)
bonne_classifcation_pourcentage_2008_mod_3 <- sum(diag(mc_2008_mod_3))/sum(mc_2008_mod_3)
bonne_classifcation_pourcentage_2007_mod_3 <- sum(diag(mc_2007_mod_3))/sum(mc_2007_mod_3)

# vérification
# MISCLASSIFICATION + ACCURACY DU MODEL
miscalsification_pourcentage_2019_mod_3+bonne_classifcation_pourcentage_2019_mod_3
miscalsification_pourcentage_2018_mod_3+bonne_classifcation_pourcentage_2018_mod_3
miscalsification_pourcentage_2009_mod_3+bonne_classifcation_pourcentage_2009_mod_3
miscalsification_pourcentage_2008_mod_3+bonne_classifcation_pourcentage_2008_mod_3
miscalsification_pourcentage_2007_mod_3+bonne_classifcation_pourcentage_2007_mod_3

#ou vérification
# ça nous donne matrice de confision et statistique
library(caret)
confusionMatrix(table(predict(modele_3_2019), variables_2019_mod_3$cote_credit_2019_mod_3)) 
confusionMatrix(table(predict(modele_3_2018), variables_2018_mod_3$cote_credit_2018_mod_3)) 
confusionMatrix(table(predict(modele_3_2009), variables_2009_mod_3$cote_credit_2009_mod_3)) 
confusionMatrix(table(predict(modele_3_2008), variables_2008_mod_3$cote_credit_2008_mod_3)) 
confusionMatrix(table(predict(modele_3_2007), variables_2007_mod_3$cote_credit_2007_mod_3)) 

# calcul pour chaque ratings sensitivity,.... 
# calcul aussi Accuracy, CI, p-value

confusionMatrix(table(predict(modele_3_2018), variables_2018_mod_3$cote_credit_2018_mod_3)) 
confusionMatrix(table(predict(modele_3_2009), variables_2009_mod_3$cote_credit_2009_mod_3)) 
confusionMatrix(table(predict(modele_3_2008), variables_2008_mod_3$cote_credit_2008_mod_3)) 
confusionMatrix(table(predict(modele_3_2007), variables_2007_mod_3$cote_credit_2007_mod_3)) 

### to delete : observation de départ en POURCENTAGE
n_2019_mod_3 <- table(variables_2019_mod_3$cote_credit_2019_mod_3) #nombre de firme pour chaque type de rating
n_2019_mod_3/sum(n_2019_mod_3) # pourcentage de firme par type de rating # la plupart des firmes ont un rating de 4




#to delete
#rm(list = ls())



### Table précision prédiction : périodes de récession : 2007, 2008, 2009, 2018 et 2019

# 2019

prediction_table_2019_mod_3 <- matrix(0,6, 2)
colnames(prediction_table_2019_mod_3) <- c("Prediction_correcte_2019", "Prediction_incorrecte")
rownames(prediction_table_2019_mod_3) <- c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2019_mod_3[1,1] <- mc_pourcentage_2019_mod_3[1,1]
prediction_table_2019_mod_3[2,1] <- mc_pourcentage_2019_mod_3[2,2]
prediction_table_2019_mod_3[3,1] <- mc_pourcentage_2019_mod_3[3,3]
prediction_table_2019_mod_3[4,1] <- mc_pourcentage_2019_mod_3[4,4]
prediction_table_2019_mod_3[5,1] <- mc_pourcentage_2019_mod_3[5,5]
prediction_table_2019_mod_3[6,1] <- bonne_classifcation_pourcentage_2019_mod_3

prediction_table_2019_mod_3[1,2] <- 1 - mc_pourcentage_2019_mod_3[1,1]
prediction_table_2019_mod_3[2,2] <- 1 - mc_pourcentage_2019_mod_3[2,2]
prediction_table_2019_mod_3[3,2] <- 1 - mc_pourcentage_2019_mod_3[3,3]
prediction_table_2019_mod_3[4,2] <- 1 - mc_pourcentage_2019_mod_3[4,4]
prediction_table_2019_mod_3[5,2] <- 1 - mc_pourcentage_2019_mod_3[5,5]
prediction_table_2019_mod_3[6,2] <- bonne_classifcation_pourcentage_2019_mod_3

round(prediction_table_2019_mod_3 * 100, digits = 2)





# 2018
prediction_table_2018_mod_3 <- matrix(0,6, 2)
colnames(prediction_table_2018_mod_3) <- c("Prediction_correcte_2018", "Prediction_incorrecte")
rownames(prediction_table_2018_mod_3) <- c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2018_mod_3[1,1] <- mc_pourcentage_2018_mod_3[1,1]
prediction_table_2018_mod_3[2,1] <- mc_pourcentage_2018_mod_3[2,2]
prediction_table_2018_mod_3[3,1] <- mc_pourcentage_2018_mod_3[3,3]
prediction_table_2018_mod_3[4,1] <- mc_pourcentage_2018_mod_3[4,4]
prediction_table_2018_mod_3[5,1] <- mc_pourcentage_2018_mod_3[5,5]
prediction_table_2018_mod_3[6,1] <- bonne_classifcation_pourcentage_2018_mod_3


prediction_table_2018_mod_3[1,2] <- 1 - mc_pourcentage_2018_mod_3[1,1]
prediction_table_2018_mod_3[2,2] <- 1 - mc_pourcentage_2018_mod_3[2,2]
prediction_table_2018_mod_3[3,2] <- 1 - mc_pourcentage_2018_mod_3[3,3]
prediction_table_2018_mod_3[4,2] <- 1 - mc_pourcentage_2018_mod_3[4,4]
prediction_table_2018_mod_3[5,2] <- 1 - mc_pourcentage_2018_mod_3[5,5]
prediction_table_2018_mod_3[6,2] <- bonne_classifcation_pourcentage_2018_mod_3

round(prediction_table_2018_mod_3 * 100, digits = 2)





# 2009


prediction_table_2009_mod_3 <- matrix(0,6, 2)
colnames(prediction_table_2009_mod_3) <- c("Prediction_correcte_2009", "Prediction_incorrecte")
rownames(prediction_table_2009_mod_3) <- c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2009_mod_3[1,1] <- mc_pourcentage_2009_mod_3[1,1]
prediction_table_2009_mod_3[2,1] <- mc_pourcentage_2009_mod_3[2,2]
prediction_table_2009_mod_3[3,1] <- mc_pourcentage_2009_mod_3[3,3]
prediction_table_2009_mod_3[4,1] <- mc_pourcentage_2009_mod_3[4,4]
prediction_table_2009_mod_3[5,1] <- mc_pourcentage_2009_mod_3[5,5]
prediction_table_2009_mod_3[6,1] <- bonne_classifcation_pourcentage_2009_mod_3

prediction_table_2009_mod_3[1,2] <- 1 - mc_pourcentage_2009_mod_3[1,1]
prediction_table_2009_mod_3[2,2] <- 1 - mc_pourcentage_2009_mod_3[2,2]
prediction_table_2009_mod_3[3,2] <- 1 - mc_pourcentage_2009_mod_3[3,3]
prediction_table_2009_mod_3[4,2] <- 1 - mc_pourcentage_2009_mod_3[4,4]
prediction_table_2009_mod_3[5,2] <- 1 - mc_pourcentage_2009_mod_3[5,5]
prediction_table_2009_mod_3[6,2] <- bonne_classifcation_pourcentage_2009_mod_3

round(prediction_table_2009_mod_3 * 100, digits = 2)




# 2008
prediction_table_2008_mod_3 <- matrix(0,6, 2)
colnames(prediction_table_2008_mod_3) <- c("Prediction_correcte_2008", "Prediction_incorrecte")
rownames(prediction_table_2008_mod_3) <- c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2008_mod_3[1,1] <- mc_pourcentage_2008_mod_3[1,1]
prediction_table_2008_mod_3[2,1] <- mc_pourcentage_2008_mod_3[2,2]
prediction_table_2008_mod_3[3,1] <- mc_pourcentage_2008_mod_3[3,3]
prediction_table_2008_mod_3[4,1] <- mc_pourcentage_2008_mod_3[4,4]
prediction_table_2008_mod_3[5,1] <- mc_pourcentage_2008_mod_3[5,5]
prediction_table_2008_mod_3[6,1] <- bonne_classifcation_pourcentage_2008_mod_3

prediction_table_2008_mod_3[1,2] <- 1 - mc_pourcentage_2008_mod_3[1,1]
prediction_table_2008_mod_3[2,2] <- 1 - mc_pourcentage_2008_mod_3[2,2]
prediction_table_2008_mod_3[3,2] <- 1 - mc_pourcentage_2008_mod_3[3,3]
prediction_table_2008_mod_3[4,2] <- 1 - mc_pourcentage_2008_mod_3[4,4]
prediction_table_2008_mod_3[5,2] <- 1 - mc_pourcentage_2008_mod_3[5,5]
prediction_table_2008_mod_3[6,2] <- bonne_classifcation_pourcentage_2008_mod_3

round(prediction_table_2008_mod_3 * 100, digits = 2)






# 2007

prediction_table_2007_mod_3 <- matrix(0,6, 2)
colnames(prediction_table_2007_mod_3) <- c("Prediction_correcte_2007", "Prediction_incorrecte")
rownames(prediction_table_2007_mod_3) <- c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2007_mod_3[1,1] <- mc_pourcentage_2007_mod_3[1,1]
prediction_table_2007_mod_3[2,1] <- mc_pourcentage_2007_mod_3[2,2]
prediction_table_2007_mod_3[3,1] <- mc_pourcentage_2007_mod_3[3,3]
prediction_table_2007_mod_3[4,1] <- mc_pourcentage_2007_mod_3[4,4]
prediction_table_2007_mod_3[5,1] <- mc_pourcentage_2007_mod_3[5,5]
prediction_table_2007_mod_3[6,1] <- bonne_classifcation_pourcentage_2007_mod_3

prediction_table_2007_mod_3[1,2] <- 1 - mc_pourcentage_2007_mod_3[1,1]
prediction_table_2007_mod_3[2,2] <- 1 - mc_pourcentage_2007_mod_3[2,2]
prediction_table_2007_mod_3[3,2] <- 1 - mc_pourcentage_2007_mod_3[3,3]
prediction_table_2007_mod_3[4,2] <- 1 - mc_pourcentage_2007_mod_3[4,4]
prediction_table_2007_mod_3[5,2] <- 1 - mc_pourcentage_2007_mod_3[5,5]
prediction_table_2007_mod_3[6,2] <- bonne_classifcation_pourcentage_2007_mod_3

round(prediction_table_2007_mod_3 * 100, digits = 2)














# METHODE MACHINE LEARNING ------------------------------------------------


# METHODES DE LA SELECTION DE VARIABLES  ----------------------------------
















# ON SUATE POUR L'INSTANT








# METHODE NOT GOOD POUR SELECTION DE VARIABLE CAR JE L'AI FAIT WAY REGRESSION LINEAIRE
# to delete line --> # ça marche aussi avec multinom


# a) Période d'expansion --------------------------------------------------



# ---> to delete all with leap --> donc subsets_reg car not pour logistique rég mais +tot pr lm reg
# 2016
library(leaps)
ppp_2016 <- variables_2016_mod_3[4:ncol(variables_2016_mod_3)]

variables_2016_mod_3_reg_sub_2 <- cbind(variables_2016_mod_3[1], ppp_2016)

variables_2016_mod_3_reg_sub_2 <- data.frame(variables_2016_mod_3_reg_sub_2)
regsubsets_full_2016 <- regsubsets(cote_credit_2016_mod_3 ~ . , data = variables_2016_mod_3_reg_sub_2, nvmax = 20)

regsubsets_summary_2016 <- summary(regsubsets_full_2016)


# le RSS et le R2 ne peuvent pas être utilisés directement pour effectuer 
# une sélection entre les modèles avec un nombre différent de prédicteurs. 
# Ceci est dû au fait que le RSS va toujours décroître et que le R2
# va toujours augmenter avec l'ajout de prédicteurs

par( mfrow = c(1, 2) )

plot(regsubsets_summary_2016$rss, type = 'b', xlab = 'Number of predictors k', ylab = 'RSS')
plot(regsubsets_summary_2016$rsq, type = 'b', xlab = 'Number of predictors k', ylab = 'R squared')


# Le Rsqr adjusté, le Cp de Mallow et le BIC offrent de meilleur mesures pour choisir le meilleur modèle puisque qu'ils estiment l'erreur de test en tenant compte de la complexité des modèles

par( mfrow = c(1, 3) )

plot(regsubsets_summary_2016$adjr2, type = 'b', xlab = 'Number of predictors d', ylab = 'Adjusted R squared', main = 'Adjusted R squared')
best_adjr2_2016 = which.max(regsubsets_summary_2016$adjr2)
best_adjr2_2016
points(best_adjr2_2016, regsubsets_summary_2016$adjr2[best_adjr2_2016], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2016$cp, type = 'b', xlab = 'Number of predictors d', ylab = 'Mallow\'s Cp', main = 'Mallow\'s Cp')
best_cp_2016 = which.min(regsubsets_summary_2016$cp)
best_cp_2016
points(best_cp_2016, regsubsets_summary_2016$cp[best_cp_2016], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2016$bic, type = 'b', xlab = 'Number of predictors d', ylab = 'BIC', main = 'BIC')
best_bic_2016 = which.min(regsubsets_summary_2016$bic)
best_bic_2016
points(best_bic_2016, regsubsets_summary_2016$bic[best_bic_2016], col = "red", cex = 2, pch=17)


# visualiser les modèles ordonnés selon chacune des mesures
par( mfrow = c(1, 3) )

plot(regsubsets_full_2016, scale = "adjr2", main = 'Adjusted R squared')
plot(regsubsets_full_2016, scale = "Cp", main = 'Mallow\'s Cp')
plot(regsubsets_full_2016, scale = "bic", main = 'BIC')





# 2000
library(leaps)
ppp_2000 <- variables_2000_mod_3[4:ncol(variables_2000_mod_3)]

variables_2000_mod_3_reg_sub_2 <- cbind(variables_2000_mod_3[1], ppp_2000)

variables_2000_mod_3_reg_sub_2 <- data.frame(variables_2000_mod_3_reg_sub_2)
regsubsets_full_2000 <- regsubsets(cote_credit_2000_mod_3 ~ . , data = variables_2000_mod_3_reg_sub_2, nvmax = 20)

regsubsets_summary_2000 <- summary(regsubsets_full_2000)


# le RSS et le R2 ne peuvent pas être utilisés directement pour effectuer 
# une sélection entre les modèles avec un nombre différent de prédicteurs. 
# Ceci est dû au fait que le RSS va toujours décroître et que le R2
# va toujours augmenter avec l'ajout de prédicteurs

par( mfrow = c(1, 2) )

plot(regsubsets_summary_2000$rss, type = 'b', xlab = 'Number of predictors k', ylab = 'RSS')
plot(regsubsets_summary_2000$rsq, type = 'b', xlab = 'Number of predictors k', ylab = 'R squared')


# Le Rsqr adjusté, le Cp de Mallow et le BIC offrent de meilleur mesures pour choisir le meilleur modèle puisque qu'ils estiment l'erreur de test en tenant compte de la complexité des modèles

par( mfrow = c(1, 3) )

plot(regsubsets_summary_2000$adjr2, type = 'b', xlab = 'Number of predictors d', ylab = 'Adjusted R squared', main = 'Adjusted R squared')
best_adjr2_2000 = which.max(regsubsets_summary_2000$adjr2)
best_adjr2_2000
points(best_adjr2_2000, regsubsets_summary_2000$adjr2[best_adjr2_2000], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2000$cp, type = 'b', xlab = 'Number of predictors d', ylab = 'Mallow\'s Cp', main = 'Mallow\'s Cp')
best_cp_2000 = which.min(regsubsets_summary_2000$cp)
best_cp_2000
points(best_cp_2000, regsubsets_summary_2000$cp[best_cp_2000], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2000$bic, type = 'b', xlab = 'Number of predictors d', ylab = 'BIC', main = 'BIC')
best_bic_2000 = which.min(regsubsets_summary_2000$bic)
best_bic_2000
points(best_bic_2000, regsubsets_summary_2000$bic[best_bic_2000], col = "red", cex = 2, pch=17)


# visualiser les modèles ordonnés selon chacune des mesures
par( mfrow = c(1, 3) )

plot(regsubsets_full_2000, scale = "adjr2", main = 'Adjusted R squared')
plot(regsubsets_full_2000, scale = "Cp", main = 'Mallow\'s Cp')
plot(regsubsets_full_2000, scale = "bic", main = 'BIC')














# b) Période de récession -------------------------------------------------


# 2019
library(leaps)
ppp_2019 <- variables_2019_mod_3[4:ncol(variables_2019_mod_3)]

variables_2019_mod_3_reg_sub_2 <- cbind(variables_2019_mod_3[1], ppp_2019)

variables_2019_mod_3_reg_sub_2 <- data.frame(variables_2019_mod_3_reg_sub_2)
regsubsets_full_2019 <- regsubsets(cote_credit_2019_mod_3 ~ . , data = variables_2019_mod_3_reg_sub_2, nvmax = 20)

regsubsets_summary_2019 <- summary(regsubsets_full_2019)


# le RSS et le R2 ne peuvent pas être utilisés directement pour effectuer 
# une sélection entre les modèles avec un nombre différent de prédicteurs. 
# Ceci est dû au fait que le RSS va toujours décroître et que le R2
# va toujours augmenter avec l'ajout de prédicteurs

par( mfrow = c(1, 2) )

plot(regsubsets_summary_2019$rss, type = 'b', xlab = 'Number of predictors k', ylab = 'RSS')
plot(regsubsets_summary_2019$rsq, type = 'b', xlab = 'Number of predictors k', ylab = 'R squared')


# Le Rsqr adjusté, le Cp de Mallow et le BIC offrent de meilleur mesures pour choisir le meilleur modèle puisque qu'ils estiment l'erreur de test en tenant compte de la complexité des modèles

par( mfrow = c(1, 3) )

plot(regsubsets_summary_2019$adjr2, type = 'b', xlab = 'Number of predictors d', ylab = 'Adjusted R squared', main = 'Adjusted R squared')
best_adjr2_2019 = which.max(regsubsets_summary_2019$adjr2)
best_adjr2_2019
points(best_adjr2_2019, regsubsets_summary_2019$adjr2[best_adjr2_2019], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2019$cp, type = 'b', xlab = 'Number of predictors d', ylab = 'Mallow\'s Cp', main = 'Mallow\'s Cp')
best_cp_2019 = which.min(regsubsets_summary_2019$cp)
best_cp_2019
points(best_cp_2019, regsubsets_summary_2019$cp[best_cp_2019], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2019$bic, type = 'b', xlab = 'Number of predictors d', ylab = 'BIC', main = 'BIC')
best_bic_2019 = which.min(regsubsets_summary_2019$bic)
best_bic_2019
points(best_bic_2019, regsubsets_summary_2019$bic[best_bic_2019], col = "red", cex = 2, pch=17)


# visualiser les modèles ordonnés selon chacune des mesures
par( mfrow = c(1, 3) )

plot(regsubsets_full_2019, scale = "adjr2", main = 'Adjusted R squared')
plot(regsubsets_full_2019, scale = "Cp", main = 'Mallow\'s Cp')
plot(regsubsets_full_2019, scale = "bic", main = 'BIC')





# 2018
library(leaps)
ppp_2018 <- variables_2018_mod_3[4:ncol(variables_2018_mod_3)]

variables_2018_mod_3_reg_sub_2 <- cbind(variables_2018_mod_3[1], ppp_2018)

variables_2018_mod_3_reg_sub_2 <- data.frame(variables_2018_mod_3_reg_sub_2)
regsubsets_full_2018 <- regsubsets(cote_credit_2018_mod_3 ~ . , data = variables_2018_mod_3_reg_sub_2, nvmax = 20)

regsubsets_summary_2018 <- summary(regsubsets_full_2018)


# le RSS et le R2 ne peuvent pas être utilisés directement pour effectuer 
# une sélection entre les modèles avec un nombre différent de prédicteurs. 
# Ceci est dû au fait que le RSS va toujours décroître et que le R2
# va toujours augmenter avec l'ajout de prédicteurs

par( mfrow = c(1, 2) )

plot(regsubsets_summary_2018$rss, type = 'b', xlab = 'Number of predictors k', ylab = 'RSS')
plot(regsubsets_summary_2018$rsq, type = 'b', xlab = 'Number of predictors k', ylab = 'R squared')


# Le Rsqr adjusté, le Cp de Mallow et le BIC offrent de meilleur mesures pour choisir le meilleur modèle puisque qu'ils estiment l'erreur de test en tenant compte de la complexité des modèles

par( mfrow = c(1, 3) )

plot(regsubsets_summary_2018$adjr2, type = 'b', xlab = 'Number of predictors d', ylab = 'Adjusted R squared', main = 'Adjusted R squared')
best_adjr2_2018 = which.max(regsubsets_summary_2018$adjr2)
best_adjr2_2018
points(best_adjr2_2018, regsubsets_summary_2018$adjr2[best_adjr2_2018], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2018$cp, type = 'b', xlab = 'Number of predictors d', ylab = 'Mallow\'s Cp', main = 'Mallow\'s Cp')
best_cp_2018 = which.min(regsubsets_summary_2018$cp)
best_cp_2018
points(best_cp_2018, regsubsets_summary_2018$cp[best_cp_2018], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2018$bic, type = 'b', xlab = 'Number of predictors d', ylab = 'BIC', main = 'BIC')
best_bic_2018 = which.min(regsubsets_summary_2018$bic)
best_bic_2018
points(best_bic_2018, regsubsets_summary_2018$bic[best_bic_2018], col = "red", cex = 2, pch=17)


# visualiser les modèles ordonnés selon chacune des mesures
par( mfrow = c(1, 3) )

plot(regsubsets_full_2018, scale = "adjr2", main = 'Adjusted R squared')
plot(regsubsets_full_2018, scale = "Cp", main = 'Mallow\'s Cp')
plot(regsubsets_full_2018, scale = "bic", main = 'BIC')





# 2009
library(leaps)
ppp_2009 <- variables_2009_mod_3[4:ncol(variables_2009_mod_3)]

variables_2009_mod_3_reg_sub_2 <- cbind(variables_2009_mod_3[1], ppp_2009)

variables_2009_mod_3_reg_sub_2 <- data.frame(variables_2009_mod_3_reg_sub_2)
regsubsets_full_2009 <- regsubsets(cote_credit_2009_mod_3 ~ . , data = variables_2009_mod_3_reg_sub_2, nvmax = 20)

regsubsets_summary_2009 <- summary(regsubsets_full_2009)


# le RSS et le R2 ne peuvent pas être utilisés directement pour effectuer 
# une sélection entre les modèles avec un nombre différent de prédicteurs. 
# Ceci est dû au fait que le RSS va toujours décroître et que le R2
# va toujours augmenter avec l'ajout de prédicteurs

par( mfrow = c(1, 2) )

plot(regsubsets_summary_2009$rss, type = 'b', xlab = 'Number of predictors k', ylab = 'RSS')
plot(regsubsets_summary_2009$rsq, type = 'b', xlab = 'Number of predictors k', ylab = 'R squared')


# Le Rsqr adjusté, le Cp de Mallow et le BIC offrent de meilleur mesures pour choisir le meilleur modèle puisque qu'ils estiment l'erreur de test en tenant compte de la complexité des modèles

par( mfrow = c(1, 3) )

plot(regsubsets_summary_2009$adjr2, type = 'b', xlab = 'Number of predictors d', ylab = 'Adjusted R squared', main = 'Adjusted R squared')
best_adjr2_2009 = which.max(regsubsets_summary_2009$adjr2)
best_adjr2_2009
points(best_adjr2_2009, regsubsets_summary_2009$adjr2[best_adjr2_2009], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2009$cp, type = 'b', xlab = 'Number of predictors d', ylab = 'Mallow\'s Cp', main = 'Mallow\'s Cp')
best_cp_2009 = which.min(regsubsets_summary_2009$cp)
best_cp_2009
points(best_cp_2009, regsubsets_summary_2009$cp[best_cp_2009], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2009$bic, type = 'b', xlab = 'Number of predictors d', ylab = 'BIC', main = 'BIC')
best_bic_2009 = which.min(regsubsets_summary_2009$bic)
best_bic_2009
points(best_bic_2009, regsubsets_summary_2009$bic[best_bic_2009], col = "red", cex = 2, pch=17)


# visualiser les modèles ordonnés selon chacune des mesures
par( mfrow = c(1, 3) )

plot(regsubsets_full_2009, scale = "adjr2", main = 'Adjusted R squared')
plot(regsubsets_full_2009, scale = "Cp", main = 'Mallow\'s Cp')
plot(regsubsets_full_2009, scale = "bic", main = 'BIC')





# 2008
library(leaps)
ppp_2008 <- variables_2008_mod_3[4:ncol(variables_2008_mod_3)]

variables_2008_mod_3_reg_sub_2 <- cbind(variables_2008_mod_3[1], ppp_2008)

variables_2008_mod_3_reg_sub_2 <- data.frame(variables_2008_mod_3_reg_sub_2)
regsubsets_full_2008 <- regsubsets(cote_credit_2008_mod_3 ~ . , data = variables_2008_mod_3_reg_sub_2, nvmax = 20)

regsubsets_summary_2008 <- summary(regsubsets_full_2008)


# le RSS et le R2 ne peuvent pas être utilisés directement pour effectuer 
# une sélection entre les modèles avec un nombre différent de prédicteurs. 
# Ceci est dû au fait que le RSS va toujours décroître et que le R2
# va toujours augmenter avec l'ajout de prédicteurs

par( mfrow = c(1, 2) )

plot(regsubsets_summary_2008$rss, type = 'b', xlab = 'Number of predictors k', ylab = 'RSS')
plot(regsubsets_summary_2008$rsq, type = 'b', xlab = 'Number of predictors k', ylab = 'R squared')


# Le Rsqr adjusté, le Cp de Mallow et le BIC offrent de meilleur mesures pour choisir le meilleur modèle puisque qu'ils estiment l'erreur de test en tenant compte de la complexité des modèles

par( mfrow = c(1, 3) )

plot(regsubsets_summary_2008$adjr2, type = 'b', xlab = 'Number of predictors d', ylab = 'Adjusted R squared', main = 'Adjusted R squared')
best_adjr2_2008 = which.max(regsubsets_summary_2008$adjr2)
best_adjr2_2008
points(best_adjr2_2008, regsubsets_summary_2008$adjr2[best_adjr2_2008], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2008$cp, type = 'b', xlab = 'Number of predictors d', ylab = 'Mallow\'s Cp', main = 'Mallow\'s Cp')
best_cp_2008 = which.min(regsubsets_summary_2008$cp)
best_cp_2008
points(best_cp_2008, regsubsets_summary_2008$cp[best_cp_2008], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2008$bic, type = 'b', xlab = 'Number of predictors d', ylab = 'BIC', main = 'BIC')
best_bic_2008 = which.min(regsubsets_summary_2008$bic)
best_bic_2008
points(best_bic_2008, regsubsets_summary_2008$bic[best_bic_2008], col = "red", cex = 2, pch=17)


# visualiser les modèles ordonnés selon chacune des mesures
par( mfrow = c(1, 3) )

plot(regsubsets_full_2008, scale = "adjr2", main = 'Adjusted R squared')
plot(regsubsets_full_2008, scale = "Cp", main = 'Mallow\'s Cp')
plot(regsubsets_full_2008, scale = "bic", main = 'BIC')





# 2007
library(leaps)
ppp_2007 <- variables_2007_mod_3[4:ncol(variables_2007_mod_3)]

variables_2007_mod_3_reg_sub_2 <- cbind(variables_2007_mod_3[1], ppp_2007)

variables_2007_mod_3_reg_sub_2 <- data.frame(variables_2007_mod_3_reg_sub_2)
regsubsets_full_2007 <- regsubsets(cote_credit_2007_mod_3 ~ . , data = variables_2007_mod_3_reg_sub_2, nvmax = 20)

regsubsets_summary_2007 <- summary(regsubsets_full_2007)


# le RSS et le R2 ne peuvent pas être utilisés directement pour effectuer 
# une sélection entre les modèles avec un nombre différent de prédicteurs. 
# Ceci est dû au fait que le RSS va toujours décroître et que le R2
# va toujours augmenter avec l'ajout de prédicteurs

par( mfrow = c(1, 2) )

plot(regsubsets_summary_2007$rss, type = 'b', xlab = 'Number of predictors k', ylab = 'RSS')
plot(regsubsets_summary_2007$rsq, type = 'b', xlab = 'Number of predictors k', ylab = 'R squared')


# Le Rsqr adjusté, le Cp de Mallow et le BIC offrent de meilleur mesures pour choisir le meilleur modèle puisque qu'ils estiment l'erreur de test en tenant compte de la complexité des modèles

par( mfrow = c(1, 3) )

plot(regsubsets_summary_2007$adjr2, type = 'b', xlab = 'Number of predictors d', ylab = 'Adjusted R squared', main = 'Adjusted R squared')
best_adjr2_2007 = which.max(regsubsets_summary_2007$adjr2)
best_adjr2_2007
points(best_adjr2_2007, regsubsets_summary_2007$adjr2[best_adjr2_2007], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2007$cp, type = 'b', xlab = 'Number of predictors d', ylab = 'Mallow\'s Cp', main = 'Mallow\'s Cp')
best_cp_2007 = which.min(regsubsets_summary_2007$cp)
best_cp_2007
points(best_cp_2007, regsubsets_summary_2007$cp[best_cp_2007], col = "red", cex = 2, pch=17)

plot(regsubsets_summary_2007$bic, type = 'b', xlab = 'Number of predictors d', ylab = 'BIC', main = 'BIC')
best_bic_2007 = which.min(regsubsets_summary_2007$bic)
best_bic_2007
points(best_bic_2007, regsubsets_summary_2007$bic[best_bic_2007], col = "red", cex = 2, pch=17)


# visualiser les modèles ordonnés selon chacune des mesures
par( mfrow = c(1, 3) )

plot(regsubsets_full_2007, scale = "adjr2", main = 'Adjusted R squared')
plot(regsubsets_full_2007, scale = "Cp", main = 'Mallow\'s Cp')
plot(regsubsets_full_2007, scale = "bic", main = 'BIC')

# --------------> tout en haut not good car use way REGRESSION LINEAIRE

























# REGRESSION PENALISEE : (ridge, lasso, elastic net), Validation croi --------




#-------------->
# to delete 
# vif : need modele lm(
# see lab REGRESSION LOGISTIQUE : lab 2, lab 3 et lab 4

# et dire les variables qu'une régression ridege ou lasso nous propose
#-------------->




# to delete this line : ---> same variables que mod 3

library(glmnet)

# 2019  

response_2019 <- variables_2019_mod_3$cote_credit_2019_mod_3
other_var_2019 <- variables_2019_mod_3[4:ncol(variables_2019_mod_3)]
variables_2019_reg_pen <- cbind(response_2019, other_var_2019)

x_2019 = model.matrix(response_2019 ~ . , data = variables_2019_reg_pen)[,-1]



# x_2019 = model.matrix(response_2019 ~ Marge_sur_EBITDA +
#                         Rendement_sur_cap_prop  +
#                         ratio_ben_avt_impot_sur_frais_int + ratio_B_non_rep_sur_Tot_actif +
#                         ratio_tot_dette_sur_tot_actif + ratio_Flux_de_TR_expl_sur_passif_cour +
#                         ratio_Fonds_de_roulement_sur_tot_actif +
#                         Total_actif + volatilite_annuelle, data = variables_2019_reg_pen)[,-1]  # --> [,-1] pour enlever l'intercepte


y_2019 = variables_2019_reg_pen$response_2019

# LASSO from lab4

# (1)


lasso_model_2019_ungrouped <-  glmnet(x_2019, y_2019, alpha = 1, family = "multinomial", type.multinomial = "ungrouped")
plot(lasso_model_2019_ungrouped, xvar = 'lambda')



# --> delete grouped car "warning" if take en compte all variable

lasso_model_2019_grouped <-  glmnet(x_2019, y_2019, alpha = 1, family = "multinomial", type.multinomial = "grouped")
plot(lasso_model_2019_grouped, xvar = 'lambda')



# (2) --> meilleur lamda sélectionné à partir de cross validation

# Afin de choisir la valeur pour le paramètre d'ajustement ?? avec une 10-validation croisée, on utilise la méthode cv.glmnet:
set.seed(2021)
cv_lasso_2019_ungrouped <-  cv.glmnet(x_2019, y_2019, alpha = 1, family = "multinomial", type.multinomial = "ungrouped", nfolds = 10)
cv_lasso_2019_ungrouped$lambda.min
cv_lasso_2019_ungrouped$lambda.1se
plot(cv_lasso_2019_ungrouped)

# to delete line : -----> si on take nb var comme dans mod_3, not "warning" dans grouped

# --> delete grouped car "warning" if take en compte all variable
set.seed(2021)
cv_lasso_2019_grouped <-  cv.glmnet(x_2019, y_2019, alpha = 1, family = "multinomial", type.multinomial = "grouped", nfolds = 10)
cv_lasso_2019_grouped$lambda.min
cv_lasso_2019_grouped$lambda.1se
plot(cv_lasso_2019_grouped)


# MODELE QU'ON OBTIENT & POINTS = VARIABLES NON SELECTIONNEES

# delete  lamda.min et conserver only lamda.1se

#Si on utilise l'erreur minimale de validation croisée (correpondante à la ligne verticale à la gauche), on obtient un modèle avec les 10 variables:
coef(cv_lasso_2019_ungrouped, s = "lambda.min")
coef(cv_lasso_2019_grouped, s = "lambda.min")



# --> delete grouped car "warning" if take en compte all variable

# Si on utilise le lambda qui correspond à la plus grande valeur de lambda telle que le MSE est à l'intérieur d'une erreur standard du minimum (la ligne vertical à la droite), on obtient un modèle avec entre [7 et 3] variables ()!:
coef(cv_lasso_2019_ungrouped, s = "lambda.1se")
coef(cv_lasso_2019_grouped, s = "lambda.1se")


# do it comme en haut --------------------> to delete this line
# matrice de confusion: ON USE ONLY 1se not min
response_2019_prob_ungrouped <- unlist(predict(cv_lasso_2019_ungrouped, x_2019, s = "lambda.1se", type="class"))
response_2019_prob_ungrouped <- factor(response_2019_prob_ungrouped, levels = c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC"))
table(response_2019_prob_ungrouped, variables_2019_reg_pen$response_2019)


response_2019_prob_grouped <- predict(cv_lasso_2019_grouped, x_2019, s = "lambda.1se", type="class")
response_2019_prob_grouped <- factor(response_2019_prob_grouped, levels = c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC"))
table(response_2019_prob_grouped, variables_2019_reg_pen$response_2019)




#--> delete ---> all with lamda.min : maybe

#ON SEE PREDICTION SUR LES 5 CLASSE

response_2019_prob_ungrouped <- predict(cv_lasso_2019_ungrouped, x_2019, s = "lambda.min", type="class")
mc_2019_ungrouped <- table(response_2019_prob_ungrouped, variables_2019_reg_pen$response_2019)


response_2019_prob_grouped <- predict(cv_lasso_2019_grouped, x_2019, s = "lambda.min", type="class")
mc_2019_grouped <- table(response_2019_prob_grouped, variables_2019_reg_pen$response_2019)



# TABLE ACCURARY & MISCLASSIFICATION

mc_pourcentage_2019_ungrouped <- mc_2019_ungrouped / colSums(mc_2019_ungrouped)

mc_pourcentage_2019_grouped <- mc_2019_grouped / colSums(mc_2019_grouped)

### MISCLASSIFICATION pour présentation output
#### Pourcentage de mauvaise classification (misclassifaction)
miscalsification_pourcentage_2019_ungrouped <- 1- sum(diag(mc_2019_ungrouped))/sum(mc_2019_ungrouped) # from mc : (14 + 22) / 151

miscalsification_pourcentage_2019_grouped <- 1- sum(diag(mc_2019_grouped))/sum(mc_2019_grouped)

### ACCURACY DU MODEL pour présentation output
### Pourcenttage de bonne classification : la somme de la diag qu'on divise par le nombre total d'observation 
bonne_classifcation_pourcentage_2019_ungrouped <- sum(diag(mc_2019_ungrouped))/sum(mc_2019_ungrouped) 

bonne_classifcation_pourcentage_2019_grouped <- sum(diag(mc_2019_grouped))/sum(mc_2019_grouped) 

# vérification
# MISCLASSIFICATION + ACCURACY DU MODEL
miscalsification_pourcentage_2019_ungrouped+bonne_classifcation_pourcentage_2019_ungrouped

miscalsification_pourcentage_2019_grouped+bonne_classifcation_pourcentage_2019_grouped




# TABLE DE PRECISION

# ungrouped
prediction_table_2019_ungrouped_reg_pen <- matrix(0,6, 2)
colnames(prediction_table_2019_ungrouped_reg_pen) <- c("Prediction_correcte_2016_en_%", "Prediction_incorrecte_en_%")
rownames(prediction_table_2019_ungrouped_reg_pen) <- c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2019_ungrouped_reg_pen[1,1] <- mc_pourcentage_2019_ungrouped[1,1]
prediction_table_2019_ungrouped_reg_pen[2,1] <- mc_pourcentage_2019_ungrouped[2,2]
prediction_table_2019_ungrouped_reg_pen[3,1] <- mc_pourcentage_2019_ungrouped[3,3]
prediction_table_2019_ungrouped_reg_pen[4,1] <- mc_pourcentage_2019_ungrouped[4,4]
prediction_table_2019_ungrouped_reg_pen[5,1] <- mc_pourcentage_2019_ungrouped[5,5]
prediction_table_2019_ungrouped_reg_pen[6,1] <- bonne_classifcation_pourcentage_2019_ungrouped

prediction_table_2019_ungrouped_reg_pen[1,2] <- 1 - mc_pourcentage_2019_ungrouped[1,1]
prediction_table_2019_ungrouped_reg_pen[2,2] <- 1 - mc_pourcentage_2019_ungrouped[2,2]
prediction_table_2019_ungrouped_reg_pen[3,2] <- 1 - mc_pourcentage_2019_ungrouped[3,3]
prediction_table_2019_ungrouped_reg_pen[4,2] <- 1 - mc_pourcentage_2019_ungrouped[4,4]
prediction_table_2019_ungrouped_reg_pen[5,2] <- 1 - mc_pourcentage_2019_ungrouped[5,5]
prediction_table_2019_ungrouped_reg_pen[6,2] <- bonne_classifcation_pourcentage_2019_ungrouped

round(prediction_table_2019_ungrouped_reg_pen * 100, digits = 2)


#vérification
library("lattice")
library("ggplot2")
library("caret")
variables_2019_reg_pen$response_2019 <- factor(variables_2019_reg_pen$response_2019, levels = c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC"))
response_2019_prob_ungrouped <- factor(response_2019_prob_ungrouped, levels = c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC"))
confusionMatrix(data=response_2019_prob_ungrouped, reference=variables_2019_reg_pen$response_2019)





# grouped
prediction_table_2019_grouped_reg_pen <- matrix(0,6, 2)
colnames(prediction_table_2019_grouped_reg_pen) <- c("Prediction_correcte_2016_en_%", "Prediction_incorrecte_en_%")
rownames(prediction_table_2019_grouped_reg_pen) <- c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC", "Precision_Globale_de_la_Prediction")

prediction_table_2019_grouped_reg_pen[1,1] <- mc_pourcentage_2019_grouped[1,1]
prediction_table_2019_grouped_reg_pen[2,1] <- mc_pourcentage_2019_grouped[2,2]
prediction_table_2019_grouped_reg_pen[3,1] <- mc_pourcentage_2019_grouped[3,3]
prediction_table_2019_grouped_reg_pen[4,1] <- mc_pourcentage_2019_grouped[4,4]
prediction_table_2019_grouped_reg_pen[5,1] <- mc_pourcentage_2019_grouped[5,5]
prediction_table_2019_grouped_reg_pen[6,1] <- bonne_classifcation_pourcentage_2019_grouped

prediction_table_2019_grouped_reg_pen[1,2] <- 1 - mc_pourcentage_2019_grouped[1,1]
prediction_table_2019_grouped_reg_pen[2,2] <- 1 - mc_pourcentage_2019_grouped[2,2]
prediction_table_2019_grouped_reg_pen[3,2] <- 1 - mc_pourcentage_2019_grouped[3,3]
prediction_table_2019_grouped_reg_pen[4,2] <- 1 - mc_pourcentage_2019_grouped[4,4]
prediction_table_2019_grouped_reg_pen[5,2] <- 1 - mc_pourcentage_2019_grouped[5,5]
prediction_table_2019_grouped_reg_pen[6,2] <- bonne_classifcation_pourcentage_2019_grouped

round(prediction_table_2019_grouped_reg_pen * 100, digits = 2)


#vérification
library("lattice")
library("ggplot2")
library("caret")
variables_2019_reg_pen$response_2019 <- factor(variables_2019_reg_pen$response_2019, levels = c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC"))
response_2019_prob_grouped <- factor(response_2019_prob_grouped, levels = c("AAA_&_AA", "A", "BBB", "BB", "B_&_CCC"))
confusionMatrix(data=response_2019_prob_grouped, reference=variables_2019_reg_pen$response_2019)














# to delete
# COURBE ROC --> ROCR currently supports only evaluation of binary classification tasks.

library(ROCR)
pred_logit = prediction(response_2019_prob_ungrouped, variables_2019_reg_pen$response_2019)
roc_logit = performance(pred_logit, measure = "tpr", x.measure = "fpr")
plot(roc_logit, col = 'green', main = "ROC curve of logistic")
abline(0, 1, col = 'darkgray', lty = 2)



















# LASSO ------> http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#log

fit_grouped <- glmnet(x, y, family = "multinomial", type.multinomial = "ungrouped")
fit_ungrouped <- glmnet(x, y, family = "multinomial", type.multinomial = "grouped")

plot(fit_grouped, xvar = "lambda", label = TRUE, type.coef = "2norm")

plot(fit_ungrouped, xvar = "lambda", label = TRUE, type.coef = "2norm")



# We can also do cross-validation and plot the returned object.

cvfit_grouped <- cv.glmnet(x, y, family="multinomial", type.multinomial = "grouped", parallel = TRUE)
plot(cvfit_grouped)




# Users may wish to predict at the optimally selected ??:

predict(cvfit_grouped, newx = x[1:10,], s = "lambda.min", type = "class")









# ELASTIC NET



# ---> pour suite voir last feuille notes cours 
# matrice de confusion



# erreurs





# courbe ROC



# auc = air sous la courbe




































# Linear Discriminant Analysis --------------------------------------------



# on peut use après all technique qu'on avait dans reg logistique ici
# matrice confusion, courbe roc, ...

library(MASS)

lda_full <- lda_full(variables_2016_mod_3$cote_credit_2016_mod_3 ~ Marge_sur_EBITDA +
                       Rendement_sur_cap_prop  + ratio_ben_avt_impot_sur_frais_int + 
                       ratio_B_non_rep_sur_Tot_actif + ratio_tot_dette_sur_tot_actif + 
                       ratio_Flux_de_TR_expl_sur_passif_cour +
                       ratio_Fonds_de_roulement_sur_tot_actif +
                       Total_actif + volatilite_annuelle,
                       data = variables_2016_mod_3)     


