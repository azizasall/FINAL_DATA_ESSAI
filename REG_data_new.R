
# REGRESSION AVEC good_variables -------------------------------------------


rm(list=ls())

# glm n'est pas pour multi nomial logit reg
# car le relevel only nnet qui peut le faire avec multinom()



#AZURE : TRY PUT DATABASE THERE POUR SEE (en csv)

# et y'a way de faire summarize data déjà intégrer in faire search



# Les libraries -----------------------------------------------------------

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

# plusieurs colonnes, avec des 0 et 1 ==> one hot encoding ... 



getwd() # my working directory est ou mon fichier doit être pour que read.csv le trouve si c'était pas un projet ça allait me sortir bureau puis document et donc c'est là que j'allais devoir mettre mon dossier excel






# IMPORTER DATABASE -------------------------------------------------------


variables_2000 <- read.csv("ma_base_de_donnees_2000.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")


variables_2007 <- read.csv("ma_base_de_donnees_2007.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")


variables_2008 <- read.csv("ma_base_de_donnees_2008.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")


variables_2009 <- read.csv("ma_base_de_donnees_2009.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")


variables_2016 <- read.csv("ma_base_de_donnees_2016.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")


variables_2018 <- read.csv("ma_base_de_donnees_2018.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")


variables_2019 <- read.csv("ma_base_de_donnees_2019.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")








#attach(variables_2019_1)
########################################################################################

# enlever not need et put good ordre

# CONSTRUCTION GOOD DATABASE

#sans colonne 1, 2 & 4
mm_00 <- matrix(colnames(variables_2000))                                   
m_nb_00 <- matrix(1:length(mm_00))                                   
cbind(m_nb_00, mm_00)                                   
variables_2000_11 <- variables_2000[c(-1, -2, -4, -10, -12, -13, -19)]
colnames(variables_2000_11)

variables_2000_1 <- variables_2000_11[, c(1, 2, 3, 4, 5, 6, 8, 9, 16, 10, 11, 12, 13, 17, 7, 14, 15)]

union(colnames(variables_2000_11), colnames(variables_2000_1))
unique(colnames(variables_2000_11), colnames(variables_2000_1))
intersect(colnames(variables_2000_11), colnames(variables_2000_1))
setdiff(colnames(variables_2000_11), colnames(variables_2000_1)) # must have 0
setequal(colnames(variables_2000_11), colnames(variables_2000_1)) # must have TRU






mm_07 <- matrix(colnames(variables_2007))                                   
m_nb_07 <- matrix(1:length(mm_07))                                   
cbind(m_nb_07, mm_07)                                 
variables_2007_11 <- variables_2007[c(-1,-2,-4, -10, -12, -13, -14, -20)]
colnames(variables_2007_11)

variables_2007_1 <- variables_2007_11[, c(1, 2, 3, 4, 5, 6, 8, 9, 15, 16,10, 11, 12, 13, 17, 7, 14 )]

union(colnames(variables_2007_11), colnames(variables_2007_1))
unique(colnames(variables_2007_11), colnames(variables_2007_1))
intersect(colnames(variables_2007_11), colnames(variables_2007_1))
setdiff(colnames(variables_2007_11), colnames(variables_2007_1)) # must have 0
setequal(colnames(variables_2007_11), colnames(variables_2007_1)) # must have TRU






mm_08 <- matrix(colnames(variables_2008))                                   
m_nb_08 <- matrix(1:length(mm_08))                                   
cbind(m_nb_08, mm_08)                                   
variables_2008_11 <- variables_2008[c(-1,-2,-4, -10, -12, -13, -14, -20)]
colnames(variables_2008_11)

variables_2008_1 <- variables_2008_11[, c(1,         2, 3, 4, 5, 6,       8, 9, 15, 16,          10, 11, 12, 13, 17       , 7, 14)]

union(colnames(variables_2008_11), colnames(variables_2008_1))
unique(colnames(variables_2008_11), colnames(variables_2008_1))
intersect(colnames(variables_2008_11), colnames(variables_2008_1))
setdiff(colnames(variables_2008_11), colnames(variables_2008_1)) # must have 0
setequal(colnames(variables_2008_11), colnames(variables_2008_1)) # must have TRU






mm_09 <- matrix(colnames(variables_2009))                                   
m_nb_09 <- matrix(1:length(mm_09))                                   
cbind(m_nb_09, mm_09)                                   
variables_2009_11 <- variables_2009[c(-1,-2,-4, -10, -11, -13, -14, -15, -21)]
colnames(variables_2009_11)

variables_2009_1 <- variables_2009_11[, c(1,        2, 3, 4, 5, 6,         8, 9, 15, 16, 17,        10, 11, 12, 13, 18,                7, 14 )]

union(colnames(variables_2009_11), colnames(variables_2009_1))
unique(colnames(variables_2009_11), colnames(variables_2009_1))
intersect(colnames(variables_2009_11), colnames(variables_2009_1))
setdiff(colnames(variables_2009_11), colnames(variables_2009_1)) # must have 0
setequal(colnames(variables_2009_11), colnames(variables_2009_1)) # must have TRU






mm_16 <- matrix(colnames(variables_2016))                                   
m_nb_16 <- matrix(1:length(mm_16))                                   
cbind(m_nb_16, mm_16)                                   
variables_2016_11 <- variables_2016[c(-1,-2,-4, -10, -11, -13, -14, -15, -21)]
colnames(variables_2016_11)

variables_2016_1 <- variables_2016_11[, c(1,        2, 3, 4, 5, 6,      8, 9, 15, 16, 17,       10, 11, 12, 13,18,           7, 14)]

union(colnames(variables_2016_11), colnames(variables_2016_1))
unique(colnames(variables_2016_11), colnames(variables_2016_1))
intersect(colnames(variables_2016_11), colnames(variables_2016_1))
setdiff(colnames(variables_2016_11), colnames(variables_2016_1)) # must have 0
setequal(colnames(variables_2016_11), colnames(variables_2016_1)) # must have TRU






mm_18 <- matrix(colnames(variables_2018))                                   
m_nb_18 <- matrix(1:length(mm_18))                                   
cbind(m_nb_18, mm_18)                                   
variables_2018_11 <- variables_2018[c(-1,-2,-4, -10, -11, -13, -14, -15, -21)]
colnames(variables_2018_11)

variables_2018_1 <- variables_2018_11[, c(1,     2, 3, 4, 5, 6, 16, 17,         8, 9, 18, 19, 20,            10, 11, 12, 13, 21,                        7, 14,          15 )]

union(colnames(variables_2018_11), colnames(variables_2018_1))
unique(colnames(variables_2018_11), colnames(variables_2018_1))
intersect(colnames(variables_2018_11), colnames(variables_2018_1))
setdiff(colnames(variables_2018_11), colnames(variables_2018_1)) # must have 0
setequal(colnames(variables_2018_11), colnames(variables_2018_1)) # must have TRU







mm_19 <- matrix(colnames(variables_2019))                                   
m_nb_19 <- matrix(1:length(mm_19))                                   
cbind(m_nb_19, mm_19)                                   
variables_2019_11 <- variables_2019[c(-1,-2,-4, -9, -10, -12, -13, -14, -20)]
colnames(variables_2019_11)

variables_2019_1 <- variables_2019_11[, c(1,     2, 3, 4, 5, 15, 16,         7, 8, 17, 18, 19,            9, 10, 11, 12, 20,                        6, 13,          14 )]

union(colnames(variables_2019_11), colnames(variables_2019_1))
unique(colnames(variables_2019_11), colnames(variables_2019_1))
intersect(colnames(variables_2019_11), colnames(variables_2019_1))
setdiff(colnames(variables_2019_11), colnames(variables_2019_1)) # must have 0
setequal(colnames(variables_2019_11), colnames(variables_2019_1)) # must have TRU






# 









# AJOUTER INVESTMENT GRADE & SPECULATIVE GRADE ----------------------------


# ajout colonnes avec Cote_crédit_binaire


#catégories AAA, AA, A et BBB sont considérées comme les firmes favorables à l'investissement (firmes d'investissement) 
#et celles dans les catégories BB, B, CCC sont les firmes favorables à la spéculation (firmes de spéculation)


binaire_cote_credit <- c("AAA"="firm_inv","AA"="firm_inv", "A"="firm_inv", "BBB"="firm_inv",
                         "BB"="firm_spec", "B"="firm_spec","CCC"="firm_spec")



binaire_cote_credit_2000 <- binaire_cote_credit[variables_2000_1$Cote_crédit]
binaire_cote_credit_2007 <- binaire_cote_credit[variables_2007_1$Cote_crédit]
binaire_cote_credit_2008 <- binaire_cote_credit[variables_2008_1$Cote_crédit]
binaire_cote_credit_2009 <- binaire_cote_credit[variables_2009_1$Cote_crédit]
binaire_cote_credit_2016 <- binaire_cote_credit[variables_2016_1$Cote_crédit]
binaire_cote_credit_2018 <- binaire_cote_credit[variables_2018_1$Cote_crédit]
binaire_cote_credit_2019 <- binaire_cote_credit[variables_2019_1$Cote_crédit]








variables_2000_2 <- cbind(binaire_cote_credit_2000, variables_2000_1)
variables_2007_2 <- cbind(binaire_cote_credit_2007, variables_2007_1)
variables_2008_2 <- cbind(binaire_cote_credit_2008, variables_2008_1)
variables_2009_2 <- cbind(binaire_cote_credit_2009, variables_2009_1)
variables_2016_2 <- cbind(binaire_cote_credit_2016, variables_2016_1)
variables_2018_2 <- cbind(binaire_cote_credit_2018, variables_2018_1)
variables_2019_2 <- cbind(binaire_cote_credit_2019, variables_2019_1)


# après avoir eu good variables qui nous intéressent ==> donc good data base

#write.csv(variables_2000_2, file = "base_de_donnees_2000.csv")
#write.csv(variables_2007_2, file = "base_de_donnees_2007.csv")
#write.csv(variables_2008_2, file = "base_de_donnees_2008.csv")
#write.csv(variables_2009_2, file = "base_de_donnees_2009.csv")
#write.csv(variables_2016_2, file = "base_de_donnees_2016.csv")
#write.csv(variables_2018_2, file = "base_de_donnees_2018.csv")
#write.csv(variables_2019_2, file = "base_de_donnees_2019.csv")










# STATISTIQUES DESCRIPTIVES



# ok dans final
# (1) CORRELATION

# pour la CORRELATION can use aussi PACKAGE DataExplorer et faire plot_correlation

library(DataExplorer)


# not good car pas de chiffres
# plot_correlation(variables_2019_2)


# done! : en faire une matrice de correlation good

# cor(variables_2019_2[,-1]) ça seulement not beau à voir et difficile à comprendre car beaucoup de variables donc library ggcorrplot

library(ggplot2)
library(ggcorrplot)

corr = round(cor(variables_2019_2[,c(-1,-2)]), 1)

#dev.new()
ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("tomato2", "thistle", "springgreen3"),
           ggtheme = theme_bw)


#library(corrplot)
#my_cor_plot <- cor(variables_2019_2[,-1])

#dev.new()
#corrplot(my_cor_plot)

#dev.new()
#corrplot.mixed(my_cor_plot, upper = "number")



# ok dans final

# (2) statistiques Descriptives -----------------------------------------------



names(variables_2019_2)

table(variables_2019_2$Cote_crédit) #ça me compte le nombre de chaque élément de chaque facter
#AAA j'en ai que 2 donc si je le fixe comme reférence only ça ne fera pas de sens
#pour les prédiction donc dans multinomial logit reg fixer AAA, AA et A comme ref


table(variables_2000_2$Cote_crédit)
table(variables_2007_2$Cote_crédit)
table(variables_2008_2$Cote_crédit)
table(variables_2009_2$Cote_crédit)
table(variables_2016_2$Cote_crédit)
table(variables_2018_2$Cote_crédit)
table(variables_2019_2$Cote_crédit)


length(variables_2000_2$Cote_crédit)
length(variables_2007_2$Cote_crédit)
length(variables_2008_2$Cote_crédit)
length(variables_2009_2$Cote_crédit)
length(variables_2016_2$Cote_crédit)
length(variables_2018_2$Cote_crédit)
length(variables_2019_2$Cote_crédit)



summary(variables_2000_2)
summary(variables_2007_2)
summary(variables_2008_2)
summary(variables_2009_2)
summary(variables_2016_2)
summary(variables_2018_2)
summary(variables_2019_2)




str(variables_2000_2)
str(variables_2007_2)
str(variables_2008_2)
str(variables_2009_2)
str(variables_2016_2)
str(variables_2018_2)
str(variables_2019_2)









# Pas trop interessant caar ça me donne pour chaque chiffre 
#différent de Marge_sur_EBIT le nombre de chaque type de ratings que j'ai
# ça aurait pu être intéressant si on avait une autre variable nominale

#----> x <- with(variables_2019_2, table(Cote_credit, Marge_sur_EBIT))



# faire la moyenne par nombre de rating

# ne marche pas et pourtant marche dans UCLA site
# normalement devait permettre de faire le calcul de la moyenne et du SD par type de ratiing
#------> with(variables_2019_2, do.call(rbind, tapply(Cote_credit ,Marge_sur_EBITDA, function(x) c(M = mean(x), SD = sd(x)))))




























# NOT RUN 

# TRANSFORMER CODE DE CREDIT NUMERIC EN CHARACTER -------------------------

#si je voulais ne pas télécharger de new database avec ratings en character
variables_2019$Cote_crédit_num #on sélectionne lle good variable avec 1:7 (donc num)

#puis on le tranforme en factor avec les character par labels
variables_2019$Cote_crédit_num <- factor(variables_2019$Cote_crédit_num,
                                     levels = c(1:7), 
                                     labels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
#ça le transforme direct en character














# ok dans final

# Pour respecter l'ordre des Levels  ---------------------------------------


#indiquons le levels de reférence # on l'enregistre dans out
#pour moi investement grade donc 1, 2, 3, 4 et now AAA, AA, A, BBB, ...

#not need de les transformer en 1, 2, ...,7 
#donc laisser grand group rating

# Cote_creditF le F pour FACTOR => donc on crée new variable et on l'ajoute dans variables_2019_1
variables_2000_2$Cote_crédit <- factor(variables_2000_2$Cote_crédit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2000_2$Cote_crédit)

variables_2007_2$Cote_crédit <- factor(variables_2007_2$Cote_crédit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2007_2$Cote_crédit)

variables_2008_2$Cote_crédit <- factor(variables_2008_2$Cote_crédit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2008_2$Cote_crédit)

variables_2009_2$Cote_crédit <- factor(variables_2009_2$Cote_crédit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2009_2$Cote_crédit)

variables_2016_2$Cote_crédit <- factor(variables_2016_2$Cote_crédit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2016_2$Cote_crédit)

variables_2018_2$Cote_crédit <- factor(variables_2018_2$Cote_crédit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2018_2$Cote_crédit)

variables_2019_2$Cote_crédit <- factor(variables_2019_2$Cote_crédit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
is.factor(variables_2019_2$Cote_crédit)



class(variables_2019_2$Cote_crédit)
levels(variables_2019_2$Cote_crédit)










# ok dans final

# factor pour firm_inv et firm_spec 

variables_2000_2$binaire_cote_credit_2000 <- factor(variables_2000_2$binaire_cote_credit_2000, levels = c("firm_inv", "firm_spec"))
variables_2007_2$binaire_cote_credit_2007 <- factor(variables_2007_2$binaire_cote_credit_2007, levels = c("firm_inv", "firm_spec"))
variables_2008_2$binaire_cote_credit_2008 <- factor(variables_2008_2$binaire_cote_credit_2008, levels = c("firm_inv", "firm_spec"))
variables_2009_2$binaire_cote_credit_2009 <- factor(variables_2009_2$binaire_cote_credit_2009, levels = c("firm_inv", "firm_spec"))
variables_2016_2$binaire_cote_credit_2016 <- factor(variables_2016_2$binaire_cote_credit_2016, levels = c("firm_inv", "firm_spec"))
variables_2018_2$binaire_cote_credit_2018 <- factor(variables_2018_2$binaire_cote_credit_2018, levels = c("firm_inv", "firm_spec"))
variables_2019_2$binaire_cote_credit_2019 <- factor(variables_2019_2$binaire_cote_credit_2019, levels = c("firm_inv", "firm_spec"))



is.factor(variables_2000_2$binaire_cote_credit_2000)






#ok dans final

# ANALYSE UNIVARIEE ???????????????, on use this one :

# (1) on fait moyenne et std dev par firm_inv et par firm_spec
# (2) on run chaque variable individuellement y = alpha + b1*X1
# (3) on run y = alpha (modele nul)
# on cherche wald et p-value


# on le fait sur 2018 qui a toutes variables qui sont présente dans toutes les autres



# (1)


library(dplyr)

variables_2019_2_firm_inv <- variables_2019_2 %>%
  filter(binaire_cote_credit_2019=="firm_inv")


variables_2019_2_firm_spec <- variables_2019_2 %>%
  filter(binaire_cote_credit_2019=="firm_spec")


# MEAN
mean_by_firm_inv <- colMeans(variables_2019_2_firm_inv[,c(-1, -2)])
mean_by_firm_spec <- colMeans(variables_2019_2_firm_spec[,c(-1, -2)])



# way 1

# STD_DEV
std_dv_by_firm_inv_1 <- apply(variables_2019_2_firm_inv[,c(-1, -2)], 2, sd)
std_dv_by_firm_spec_1 <- apply(variables_2019_2_firm_spec[,c(-1, -2)], 2, sd)


# NOT GOOD HERE
# way 2
#library(matrixStats)
#std_dv_by_firm_inv_2 <- colSds(variables_2019_2_firm_inv[c(-1, -2)])
#std_dv_by_firm_spec_2 <- colSds(variables_2019_2_firm_spec[,c(-1, -2)])






#ok dans final

# (2) WALD et P > chi^2

#TEST UNIVAIRE
library(nnet)

variables_2019_2$binaire_cote_credit_2019 <- relevel(variables_2019_2$binaire_cote_credit_2019, ref = "firm_inv")

#modele pas contraint
modele_uvni_2019_1 <- multinom(variables_2019_2$binaire_cote_credit_2019~variables_2019_2$Marge_sur_EBITDA, data = variables_2019_2)
summary(modele_uvni_2019_1)



#ok dans final

# donne Résultat Chisq et Pr(>Chisq) analyse univariée


library(afex)
set_sum_contrasts() # use sum coding, necessary to make type III LR tests valid
library(car)
aannoovv <- Anova(modele_uvni_2019_1,type="III")

aannoovv$`LR Chisq`
aannoovv$Df
aannoovv$`Pr(>Chisq)`



variables_2019_2




# other type anova : https://odr.inra.fr/intranet/carto/cartowiki/index.php/Statistiques_descriptives_avec_R


# moyenne et std dev par firm_inv & firm_spec

library(tidyverse)

#stat descr avec firm_inv --> voir en bas too
stat_desc_by_firm_inv <-  variables_2019_2 %>%
  filter(binaire_cote_credit_2019=="firm_inv") 
  
colMeans(stat_desc_by_firm_inv[,c(-1,-2)]) # moyenne des col par firm_inv
sd(stat_desc_by_firm_inv$Marge_sur_EBITDA)









# Donnne Resultat coef analyse multivariée


# ça c'est plutot pour lorsque je vais rouler le modèle : pour have OUTPUT
library(AER)  #idem que package (Broom) see en haut
coefttest <- coeftest(modele_uvni_2019_1) # me donne 

coefttest$ # nothing










#NOM VARIABLES 2018 qui contient toutes les noms de variables qu'on peut retouver dans les autres aussi

# donc c'est ça qu'on copie après on enlève dont qu'on ton a pas besoin
h_2018 <- (Cote_crédit_num + Cote_crédit + RATINGS + Marge_bénéficiaire nette +
  Marge_sur_EBITDA + Marge_sur_EBIT + Rendement_sur_cap_prop + Rendement_sur_actif +
  Dette_LT + Total_passif + Total_actif + Bénéfices_non_répartis + Flux_de_trésorie_d.expl + Passif_courant +
  ratio_ben_avt_impot_sur_frais_int + ratio_tot_dette_sur_tot_actif + ratio_actuel + Ratio_de_liquid_réduite + 
  Ratio_de_liquidité +Fonds_roulement + Ratio_Fonds_de_roulmt_sur_ventes + Marge_opérationnelle +
  Beta_applique + Croissance_adj_des_Bén_ann + Croissance_tot_actif + ratio_tot_liab_sur_tot_actif + 
  ratio_B_non_rep_sur_Tot_actif + ratio_Flux_de_TR_expl_sur_passif_cour + ratio_Fonds_de_roulement_sur_tot_actif)




colnames(variables_2019_2)

attach(variables_2019_2)


variables_2019_2$binaire_cote_credit_2019 <- relevel(variables_2019_2$binaire_cote_credit_2019, ref = "firm_inv")

my_model_an_univ <- multinom(variables_2019_2$binaire_cote_credit_2019 ~ Marge_sur_EBITDA, data = variables_2019_2)
summary(my_model_an_univ)











# -------------->
# on saute

#sauvegarde from finale

nom_reg <- c("modele_uvni_2019_Marge_sur_EBITDA", "modele_uvni_2019_Marge_sur_EBIT", "modele_uvni_2019_Rendement_sur_cap_prop",
             "modele_uvni_2019_Rendement_sur_actif", "modele_uvni_2019_Croissance_adj_des_Bén_ann", "modele_uvni_2019_Croissance_tot_actif",
             "modele_uvni_2019_ratio_ben_avt_impot_sur_frais_int","modele_uvni_2019_ratio_tot_dette_sur_tot_actif",
             "modele_uvni_2019_ratio_tot_liab_sur_tot_actif", "modele_uvni_2019_ratio_B_non_rep_sur_Tot_actif", 
             "modele_uvni_2019_ratio_Flux_de_TR_expl_sur_passif_cour","modele_uvni_2019_ratio_actuel",
             "modele_uvni_2019_Ratio_de_liquid_réduite", "modele_uvni_2019_Ratio_de_liquidité",
             "modele_uvni_2019_Ratio_Fonds_de_roulmt_sur_ventes", "modele_uvni_2019_ratio_Fonds_de_roulement_sur_tot_actif",
             "modele_uvni_2019_Total_actif", "modele_uvni_2019_Marge_opérationnelle", "modele_uvni_2019_Beta_applique")

length(nom_reg) # 21 - 2 = 19

# -------------->


















# a refaire

# ------->
variables_2019_2$binaire_cote_credit_2019 <- relevel(variables_2019_2$binaire_cote_credit_2019, ref = "firm_inv")

ratingslogit <- multinom(variables_2019_2$binaire_cote_credit_2019 ~ Marge_sur_EBITDA, data = variables_2019_2)


# plot data
plot(x = variables_2019_2$Marge_sur_EBITDA, 
     y = variables_2019_2$binaire_cote_credit_2019,
     main = "Probit and Logit Models Model of the Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.9)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line of Probit and Logit models
x <- seq(0, 3, 0.01)
y_probit <- predict(denyprobit, list(pirat = x), type = "response")
y_logit <- predict(ratingslogit,variables_2019_2, list( variables_2019_2$Marge_sur_EBITDA = x), type = "response")



lines(x, y_probit, lwd = 1.5, col = "steelblue")
lines(x, y_logit, lwd = 1.5, col = "black", lty = 2)

# add a legend
legend("topleft",
       horiz = TRUE,
       legend = c("Probit", "Logit"),
       col = c("steelblue", "black"), 
       lty = c(1, 2))  



#-------->  
































# ok dans final

library(dplyr)
count_data_2000 <- variables_2000_2 %>% 
  count(Cote_crédit)

ggplot(count_data_2000, aes(x=Cote_crédit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=-0.500, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2000", u="Nombre d'observation",
       title = "")






library(dplyr)
count_data_2007 <- variables_2007_2 %>% 
  count(Cote_crédit)

ggplot(count_data_2007, aes(x=Cote_crédit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=-0.500, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2007", u="Nombre d'observation",
       title = "")





library(dplyr)
count_data_2008 <- variables_2008_2 %>% 
  count(Cote_crédit)

ggplot(count_data_2008, aes(x=Cote_crédit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=-0.500, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2008", u="Nombre d'observation",
       title = "")





library(dplyr)
count_data_2009 <- variables_2009_2 %>% 
  count(Cote_crédit)

ggplot(count_data_2009, aes(x=Cote_crédit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=-0.500, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2009", u="Nombre d'observation",
       title = "")




library(dplyr)
count_data_2016 <- variables_2016_2 %>% 
  count(Cote_crédit)

ggplot(count_data_2016, aes(x=Cote_crédit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=-0.500, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2016", u="Nombre d'observation",
       title = "")




library(dplyr)
count_data_2018 <- variables_2018_2 %>% 
  count(Cote_crédit)

ggplot(count_data_2018, aes(x=Cote_crédit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=-0.500, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2018", u="Nombre d'observation",
       title = "")




library(dplyr)
count_data_2019 <- variables_2019_2 %>% 
  count(Cote_crédit)

ggplot(count_data_2019, aes(x=Cote_crédit, y=n))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8, fill="gray16")+
  geom_text(aes(label=n), fontface = "bold",vjust=0.00, position = position_dodge(0.9),size = 6)+
  labs(x="Cote de crédit 2019", u="Nombre d'observation",
       title = "")


























































# POUR ANALYSE MULTIVARIEE

# ------------------------->
# FROM YOUTUBE VIDEO --> link : https://www.youtube.com/watch?v=fDjKa7yWk1U&list=PLiX9HD2vtIDEM4Ttu3bqffXMN1TwsM5Jl&index=1
# ------------------------->
#je fais la régression de investement grade (firme d'investissement)
# en fonction de spéculative grade (firme spéculative)

# ALL INTERPERETATION C'EST POUR MODEL 1
library(nnet)

###--> can delete
#REIMPORT DATA BASE avec AAA, ... rating grand gourpe
#dans ma base de donnée changer A,2,..,7 par rating groupe
###-->







#dans nos données il n'y a pas d'aspect temporel, 
#c'est purement une coupe transversale, donc varianble catégorique pour rating et continue pour variable



# Pour la régression
# indiquons le reférence level en 1, 2, 3, 4, 5, 6, 7 => pour nous c'est 1
#variables_2019_2$Cote_credit <- relevel(variables_2019_2$Cote_credit, ref = "1") #car me dit que ref doit être de longueur 1
variables_2019_2$binaire_cote_credit_2019 <- relevel(variables_2019_2$binaire_cote_credit_2019, ref = "firm_inv") #  on use 1 comme le refrence level et on le put dans out qu'on vient de créer



variables_2019_2$Cote_crédit <- relevel(variables_2019_2$Cote_crédit, ref = "AAA")


#from :https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html
# Run a "only intercept" model
model_intercept_bin <- multinom(variables_2019_2$binaire_cote_credit_2019 ~ 1, data = variables_2019_2)
summary(model_intercept_bin) # ça devient une régression linéaire (quand c'est binaire)


model_intercept <- multinom(variables_2019_2$Cote_crédit ~ 1, data = variables_2019_2)
summary(model_intercept)

# INTERPRETATION COEF FROM this site 
#Ce sont les coefficients logit relatifs à la catégorie de référence. Par exemple, sous «math», le -0,185 suggère que pour une augmentation d'une unité du score «science», le coefficient logit pour «faible» par rapport à «moyen» diminuera de ce montant, -0,185.


#ok dans final

# 11.7.4 Check the model fit information
# the anova function is confilcted with JMV's anova function, so we need to unlibrary the JMV function before we use the anova function.
# detach("package:jmv", unload=TRUE), not need var not use jmv
# Compare the our test model with the "Only intercept" model
# anova(OIM,multi_mo)
anova(model_intercept,my_model_2) # OU USE AER package
# Voir INertpretation : interpretation of the Model Fit information



#chi-square test :
#c'est la différence entre les Residual Deviance dans les 2 modèles




# ON SAUTE 
#-------> parenthèse : tester collinéarité avec VIF  : MULTICOLLINEARITE ET AUTOCORELLATION
# https://stats.stackexchange.com/questions/69959/test-multicollinearity-for-multinomial-logit-regression

library(perturbR)

require(car)
require(perturbR)

colldiag(mod = multinom(variables_2019_2$binaire_cote_credit_2019 ~ variables_2019_2$Marge_sur_EBITDA, model = TRUE), scale = FALSE, 
         center = FALSE, add.intercept = TRUE)




# TESTDE DURBIN WATSON D'AUTOCORRELATION
require(lmtest)
dwtest(multinom(as.integer(variables_2019_2$binaire_cote_credit_2019) ~ variables_2019_2$Marge_sur_EBITDA))


dwtest(my_model_2) # ne maarche pas 
#------->








# MODELE_PROCESSING -------------------------------------------------------

# Développons notre multinomial modéle

#pour quoi nous avons choisi le multinom :
#Below we use the multinom function from the nnet package to estimate a multinomial logistic regression model. There are other functions in other R packages capable of multinomial regression. We chose the multinom function because it does not require the data to be reshaped (as the mlogit package does) and to mirror the example code found in Hilbe's Logistic Regression Models.



#(1) modèle de départ qui nous permet de calculer p-value

my_model_2 <- multinom(variables_2019_2$Cote_crédit~., data = variables_2019_2)

summary(my_model_2)



# ok dans final 
coeftest(my_model_2) # aussi AER package 




# BOOK POUR CALCULER ALL THIS TYPE OF TEST ET LEUR INTERPRETATION : https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html
# et Book UCLA pour interpréter résultat





# BOOK POUR CALCULER ALL THIS TYPE OF TEST ET LEUR INTERPRETATION : https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html
# LIKELIHOOD
# Log Likelihood = final value lorsque qu'on run le model et on met un signe moins (-) devant
# Voir UCLA pour comprendre = texte en bas
# This model-running output includes some iteration history and includes the final negative log-likelihood 179.981726


# Chi-square Test of  voir en haut après nnet après (model_intercept) 
# c'est la différence des Residual Deviance



# ok dans final

# PSEUDO R SQUARE (chech le name ??????)
# Pseudo Rsquare : McFadden's pseudo R-squared 
# For models estimated by multinom the McFadden's pseudo R-squared can be easily calculated as follows:
# see : https://stackoverflow.com/questions/43623076/multinomial-logit-in-r-mlogit-versus-nnet/43697814

nnet.mod.loglik <- nnet:::logLik.multinom(my_model_2) # from modèle 1 de base

nnet.mod0 <- multinom(variables_2019_2$Cote_crédit ~ 1, data = variables_2019_2)# ON RUN REG en fonction de 1 --> modele contraint
nnet.mod0.loglik <- nnet:::logLik.multinom(nnet.mod0) # from modèle en haut qu'on v1 de run
(nnet.mod.mfr2 <- as.numeric(1 - nnet.mod.loglik/nnet.mod0.loglik)) # 1 - modèle 1 / modele en fonction de 1



# ou (mm résultat) 

# Pseudo R-Sqaure : preuve = même chose qu'avec DescTools : McFadden 
# Test de vraisemblance : Likelihood test # MAIS C'EST PLUTOT LE PSEUDO R-SQUARE AUSSI

# on dans final

#Pour estimer le coefficient de corrélation
#modele contraint
nullmodele_2 <- multinom(variables_2019_2$Cote_crédit~1,  data=variables_2019_2) # Y ~ 1 means modèle nulle, ça signifie que (  output is explained by a constant term (the intercept)/ The intercept is implied in all your models, so you are testing for the effect of a after accounting for the intercept. )
summary(nullmodele_2)                                                     # source explication en haut : https://stats.stackexchange.com/questions/6505/likelihood-ratio-test-in-r

L <-1-logLik(my_model_2)/logLik(nullmodele_2)






# lrtest(nullmodele_2, my_model_2) # me donne LogLik des 2 modélé, Chisq(df) et Pr>Chisq

#nb d'observation ---> ok
# ? LR chi^2(5) -----> ok
# ? Prob > chi^2 = p-value Likelihood Test ----> ok
# ? Pseudo R^2 -----> ok ---> avec DescTools
# ? Log Lihelihood  ---> ok (et on met - devant) see code : lrtest(nullmodele_2, my_model_2) aussi sur UCLA












# voir en bas avec "DescTools" from book with multinom


# on saute           ça donne rien

# NE MARCHE PAS AVEC MULTINOM quand on le run on voit avec quoi ça marche
# Pseudo R2 and loglikelyhood ratio test
library(rcompanion)
nagelkerke(my_model_2) 







# Pour chi-square 
# see : https://data.princeton.edu/wws509/r/c6s2




# ok dans final

# WALD TEST
# see https://stats.stackexchange.com/questions/63222/getting-p-values-for-multinom-in-r-nnet-package

library(AER)  #idem que package (Broom) see en haut
coeftest(my_model_2) # me donne 




# ok dans final

library(broom) # IDEM QUE PACKAGE PACKAGE (AER) voir en bas
tidy(my_model_2)



# ok dans final

#donne même chose que AER et broom
library(RVAideMemoire)
test.multinom(modele_uvni_2019_1, variables_2019_2$Marge_sur_EBITDA)





# ok dans final

#WALD ET Pr>Chisq ==> comme dans table 2 Amdoumi
# Univariate analysis Resulte
# see : https://stats.stackexchange.com/questions/63222/getting-p-values-for-multinom-in-r-nnet-package

library(afex)
set_sum_contrasts() # use sum coding, necessary to make type III LR tests valid
library(car)
Anova(my_model_2,type="III")

?Anova # for multinomial logit and proportional-odds logit models, likelihood-ratio tests are calculated



# ok dans final

# see : https://stackoverflow.com/questions/33316898/r-tukey-posthoc-tests-for-nnet-multinom-multinomial-fit-to-test-for-overall-dif
library(effects)
plot(effect(my_model_2,term="Marge_sur_EBITDA"),ylab="",type="probability",style="stacked",colors=rainbow(7))




# ok dans final          # puis enlevé mais c'est pour calculer INTERVALLE DE CONFIANCE

# see : https://stats.stackexchange.com/questions/63222/getting-p-values-for-multinom-in-r-nnet-package

confint(my_model_2, level = 0.95)

summary(my_model_2)

# et

# ok dans final            puis enlevé car je n'en ai pas besoin pour chaque variable    

#Comme déjà dit par OP les tests de wald ne sont pas vraiment bons pour les modèles multinomiaux, nous devrions vraiment utiliser tests de vraisemblance. Ci-dessous, je montre un moyen facile d'obtenir cela via les fonctions du package MASS, en utilisant un exemple de la page d'aide de nnet :: multinom. La fonction de bête de somme utilisée est MASS :: dropterm:
MASS::dropterm(my_model_2, trace=FALSE, test="Chisq") # donne Df, AIC, LRT, Pr(Chi) pour chaque variable





# ok dans finale                         puis enlevé

# P-VALUE LIKELIHOOD RATIO TEST

# source : https://stats.stackexchange.com/questions/6505/likelihood-ratio-test-in-r
# pchisq = the (non-central) Chi-squared Distribution 
p_value_of_the_LR_test <- 1-pchisq(my_model_2$deviance, 5) # df 156 from L : test en haut



# ok dans final

#from mm link
anova(nullmodele_2, my_model_2)   # me donne aussi Pr(Chi) du modèle 
# interprétation : book multinom : apres nnet j'ai put link book there



# ok dans final

# from mm link
lrtest(nullmodele_2, my_model_2) # me donne LogLik des 2 modélé, Chisq et Pr>Chisq




# not need dans final

# pour avoir t-tes
t.test(Marge_sur_EBITDA)





# NOT THE PACKAGE  --> voir en bas avec book with multinom
library(PseudoR2)
PseudoR2(my_model_2, which = NULL)




# ok dans final

# FROM BOOK R multinom github

#11.7.6 Calculate the Pseudo R-Square
# Please takeout the "#" Sign to run the code
# Load the DescTools package for calculate the R square
library("DescTools")
# Calculate the R Square
PseudoR2(my_model_2, which = c("CoxSnell","Nagelkerke","McFadden"))

# Voir interpréation dans Book


#11.7.7 Likelihood Ratio Tests


# et suivant

















###--> ON SAUTE

#it's goo j'ai les même chose : https://irudnyts.github.io/multinomial-regression/
#______________________________________________________________________________________________________
# TRY WITH MLOGIT 
library(mlogit)

attach(variables_2019_2)

long_data0 = mlogit.data(variables_2019_2, choice ="Cote_credit" , shape = "wide")

mlogit_model <- mlogit(Cote_credit~0|Marge_sur_EBIT, data = long_data0)
summary(mlogit_model)

#marche only si je ne mets pas variables_2019 en factor 

#CHECK TRY (MULTINOM)
my_model_check <- multinom(Cote_credit~Marge_sur_EBIT, data = variables_2019_2)

#______________________________________________________________________________________________________
# TRY
library(VGAM)
fit_vgam <- vglm(Cote_credit ~ Marge_sur_EBIT, multinomial(refLevel = "AAA"), 
                 data = variables_2019_2)


#______________________________________________________________________________________________________
# TRY
library(mnlogit)
fit_mnlogit <- mnlogit(Cote_credit ~ 1 | Marge_sur_EBIT | 1, long_data0)



summary(fit_vgam)
summary(my_model_check)
summary(mlogit_model)

#______________________________________________________________________________________________________

## -->























#suite from video 

#(2) après calcul p-value, on see que les 5 variables 
#[a] Marge_sur_EBITDA, [b] Rendement_sur_cap_prop, [c]ratio_ben_avt_impot_sur_frais_int,
#[d] ratio_tot_dette_sur_tot_actif, [e] Total_actif, 
#not significatif pour all 6 equation donc on le drop
#pour faire le modèle sans ces variables qui ne sont pas significatives  
my_model_2 <- multinom(Cote_credit~.- Marge_sur_EBITDA - Rendement_sur_cap_prop - ratio_ben_avt_impot_sur_frais_int - ratio_tot_dette_sur_tot_actif - Total_actif,
                       data = variables_2019_2)


#on refait tout ce qui suit pour my_model_2 pour aller checker les p-values
#de nouveau














#ou
# pour my modèle 1 si on devait taper toutes les varaibles 1 par 1 

#my_model_2 <- multinom(Cote_credit~Marge_sur_EBITDA+Marge_sur_EBIT+Rendement_sur_cap_prop+
#                     Rendement_sur_actif+ratio_tot_liab_sur_tot_actif+
#                       ratio_B_non_rep_sur_Tot_actif+ratio_Flux_de_TR_expl_sur_passif_cour+
#                       ratio_ben_avt_impot_sur_frais_int+ratio_tot_dette_sur_tot_actif+
#                       ratio_actuel+Ratio_de_liquid_réduite+ratio_Fonds_de_roulement+Ratio_de_liquidité+
#                       Ratio_Fonds_de_roulmt_sur_ventes+Total_actif+Marge_d_explt+Beta_applique, data = variables_2019_2)



#can use variable comme ça car appliqué attach() function
summary(my_model_2)
summary(my_model_2)


# la partie predict permet de developper un modele pour dire si une firme
# sera coté AAA, AAA, ...., CCC

# Les ratings sont données par des agences de notation donc l'idée est de
# développer un modèle qui permet pouvoir donner un rating à une firme sans
# avoir une agence de notation


# bien afficher résultats cor() et résultat rég
# Pour cette façon de faire, il faut que j'utilise les coefficients pour
# calculer les probabilités ++> ce sont les mêmes que l'équation numéro 8
# dans P2 corrigé = P3 





# FROM UCLA ---------------------------------------------------------------
# The ratio of the probability of choosing one outcome category over the probability of choosing the baseline category is often referred as relative risk (and it is sometimes referred to as odds, described in the regression parameters above). The relative risk is the right-hand side linear equation exponentiated, leading to the fact that the exponentiated regression coefficients are relative risk ratios for a unit change in the predictor variable. We can exponentiate the coefficients from our model to see these risk ratios.



#  ok dans final

# calculating the odd ratio : risk ratio (traduire phrase en haut)
exp(coef(my_model_2))

exp(coef(my_model_2)) # voir interprétation from UCLA aussi

#INTERPRETATION : from : https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html

#Supposons dans l'exemple précédent où nous prédisions le succès comptable par un prédicteur de 
#compétence en mathématiques que b = coef = 2,69. Ainsi, le rapport de cotes est exp (2,69) ou 14,73. 
#Par conséquent, les chances de réussite sont 14,73 fois plus élevées pour un élève par exemple 
#qui avait une note pré-test de 5 que pour un élève dont la note avant le test était de 4






# ON SAUTE

# NOT ALL
# 11.3 Test d'hypothèse des coefficients
# VIF   ----> interpetation VIF : https://statisticalhorizons.com/multicollinearity

# detecter Multicol using R : http://www.sthda.com/english/articles/39-regression-model-diagnostics/160-multicollinearity-essentials-and-vif-in-r/
# on RUN LINEAR MODEL
# need 2 samples
library(tidyverse)
library(caret)

car::vif(model_lm)







# Lire all UCLA en français pour bien comprendre de quoi il s'agit









# ON SAUTE

### ---> NOT RUN : sauter
# Dans video youtube en bas: ou  link (https://www.youtube.com/watch?v=sMh6g-d6FQk&t=270s)
# loggpdds and probabilities plots
library(visreg)
visreg(my_model_2, "variables_2019_2$Marge_sur_EBIT" )

# Porbabilities 
visreg(my_model_2, "variables_2019_2$Marge_sur_EBIT", scale = "response", rug = 4)

#marginal effect
library(margins)
effects_logit = margins(my_model_2)
print(effects_logit)
summary(effects_logit)
plot(effects_logit)
### --->


















# INTERPRETATION : UCLA et link (https://www.youtube.com/watch?v=sMh6g-d6FQk&t=270s)

# ok dans final

# You can also use predicted probabilities to help you understand the model. You can calculate predicted probabilities for each of our outcome levels using the fitted function. We can start by generating the predicted probabilities for the observations in our dataset and viewing the first few rows
head(prédict_proba_ucla <- fitted(my_model_2)) # mm chose que [predict_proba_2]
# ou dans book : https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html
# 11.7.5 Calculate the Goodness of fit


# ok dans final

# the same
# Check the predicted probability for each program
head(my_model_2$fitted.values, 30)


# ok dans final

# We can get the predicted result by use predict function
head(predict(my_model_2),30)




# ok dans final

# Test the goodness of fit : TEST D'INDEPENDANCE
# chisq.test(hsb$prog2,predict(multi_mo))
chisq.test(variables_2019_2$Cote_crédit, predict(my_model_2)) # donne chi-square et p-value

#interpretation en FR : https://odr.inra.fr/intranet/carto/cartowiki/index.php/Statistiques_descriptives_avec_R















# suite youtube

#Next, if we want to examine the changes in predicted probability associated with one of our two variables, we can create small datasets varying one variable while holding the other constant. We will first do this holding write at its mean and examining the predicted probabilities for each level of ses.
#----> dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
#----> predict(test, newdata = dses, "probs")




#Another way to understand the model using the predicted probabilities is to look at the averaged predicted probabilities for different values of the continuous predictor variable write within each level of ses.
#----> dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), write = rep(c(30:70),3))
## store the predicted probabilities for each value of ses and write
#----> pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of ses
#----> by(pp.write[, 3:5], pp.write$ses, colMeans)




# VOIR SUITE DANS UCLA ++>  NOTAMMENT POUR PLOT GGPLOT2


















# PREDICTION_RATINGS ------------------------------------------------------

#PREDICTION DE MES RATINGS : [°°° NOT VRAI °°°° cette partie ne me concerne mm pas vraiment
#only need my good model avec good variables après divers tests ]

#FINALEMENT CA ME CONCERNE CAR PREDICT_PROBA ME PERMET D'AVOIR, UNE FOIS
#LE GOOD MODEL LE RESULTAT DE CHAQUE LIGNE DU MODEL EN EQUATION

# prédiction de mes ratings 
predict_ratings_2 <- predict(my_model_2, variables_2019_2) # ça only le fait pour mes 156 observations

predict_ratings_2 <- predict(my_model_2, variables_2019_2)


# ok dans final

# mettre prediction à coté vrai vriables

data.frame(observed=variables_2019_2$Cote_crédit, predicted=predict_ratings_2)

#nombre de fois ou on a la même chose dans les prédictions pour le good modèle
#et le modèle partiel (régression alternative) #can use it pour tester 
#LA SIGNIFICATIVITE CONJOINTE : on a modèle contraint et modèle non contraint


# ok dans final

#COMPARAISONS
#entre les 2 modèle
sum(as.numeric(predict_ratings_2==predict_ratings_2))

#entre modèle 1 et rating départ
sum(as.numeric(predict_ratings_2==variables_2019_2$Cote_crédit))

#entre modèle 2 et rating départ
sum(as.numeric(predict_ratings_2==variables_2019_2$Cote_credit))


# ____INTERPRETATION____ de predict_ratings_2 au dessus : le modèle prédit la prmeière firme avec un rating de BB,
# la firme numero 2 avec un rating de BBB, 
# la firme numero3 avec un rating de A,  
# la firme numero 136 avec un rating de A


# donc si on le compare par rapport à ce qu'on avait au départ, 
# on a la même chose pour la firme 1 et la firme 2 prédiction = rating de départ, donc on a un good match
# pour la firme 3, il y a une misclassification car on avait 6 (B) et le modèle prédit 3 (A)


# ok dans final et mm chose que predicts en haut

#si je veux faire la prédiction des probabilités
predict_proba_2 <- predict(my_model_2, variables_2019_2, type = "prob") # OUTPUT j'ai la prédiction de la prob de chaque variable pour chaque firme 
head(predict_proba_2)


predict_proba_2 <- predict(my_model_2, variables_2019_2, type = "prob")





# somme des probabilités par ligne = 1
# pour le checher on put réusltat dans matrice
my_mat_proba_predit_2 <- matrix(predict_proba_2, 156, 7)
sum(my_mat_proba_predit_2[1,])


my_mat_proba_predit_2 <- matrix(predict_proba_2, 156, 7)
sum(my_mat_proba_predit_2[1, ])


# INTERPETATION : 
#pour la ligne 1  1, la probabilité que la firme soit rated 1 (AAA) est très faible (5.512994e_08)


# si on veut la prédiction de la probabilité only pour certaines firmes spécifique
predict_proba_pr_certain_only_2 <- predict(my_model_2, variables_2019_2[c(1,10,40,50,156),], type = "prob")

predict_proba_pr_certain_only_2 <- predict(my_model_2, variables_2019_2[c(1,10,40,50,156),], type = "prob")

#### MISCLASSIFICATION ERROR
# donc on compare la prédiction du modele avec les good data actuelle 
# pour voir combien le match ne tient pas

# cm = confusion matrix
cm_2 <- table(predict(my_model_2), variables_2019_2$Cote_crédit)

cm_2 <- table(predict(my_model_2), variables_2019_2$Cote_credit)





#INTERPRETATION DE LA MATRICE : 
# les axes sont nos ratings de 1 à 7 pour AAA jusqu'à CCC
# le (1,2,3,4,5,6,7) en  haut =HORIZONTALE= (name) représente the actual values
# le (1,2,3,4,5,6,7)  =VERTICALE= (name) représente the predicted values from the model

#on commece par (1,2,3,4,5,6,7)en haut = HORIZONTALE=actual value puis 
# puis par (1,2,3,4,5,6,7) =VERTICALE= prédiction

#H=horizontale et V= verticale
# intersection (1H,1V)=>2 : il y a 2 firmes qui sont classées AAA soit (1) alors que le modèle à prédit qu'elle sera AAA soit (4) à son tour 

# intersection (4H,4H)=>50 : il y a 50 firmes qui sont classées BBB soit (4) alors que le modèle à prédit qu'elle sera BBB soit (4) à son tour 

# intersection (3H,4V)=>9 : il y a 9 firmes qui sont classées A soit (3) alors que le modèle à prédit qu'elle sera BBB soit (4) à son tour 

# intersection (7H,6V)=>5 : il y a 5 firmes qui sont classées CCC soit (7) alors que le modèle à prédit qu'elle sera B soit (6) à son tour 

### données sur la diagonale sont les correctes classifications
### données en dehors de la diagonale sont les classifications incorrectes


#POURCENTAGE DE MMISCLASSIFICATION
# si on fait la somme des éléments hors diagonale qu'on divise par le total 
# d'e variable ()'observation (156) on aura le pourcentage de misclassification
miscalsification_pourcentage_2 <- 1- sum(diag(cm_2))/sum(cm_2) # sans le 1 ça donne accuracy ou précision du modèle et avec le 1 - ça donne le misclasification errors

miscalsification_pourcentage_2 <- 1- sum(diag(cm_2))/sum(cm_2) 

#°°°INTERESTING°°°
#INTERPRETATION_2 : 34.62% du temps le modele misclassifie les ratings
#ça signifie que le modèle dit que pour la firme (voir interprétation cm_2 (3H, 2V))
#le rating doit être  2 (AA) mais en réalité le rating de cette firme est 3 (A)

#SI ON AVAIT MODELE AVEC 100% DE PRECISION, LES ELEMENTS HORS DIAGONALE DONNERONT ZERO


#ACCURACY DU MODEL
# sans le 1 ça donne accuracy ou précision du modèle et avec le 1 - ça donne le misclasification errors

#Résultat idem que quand on use package caret -> confisionMatrix function
accuracy_pourcentage_2 <- sum(diag(cm_2))/sum(cm_2) #le modèle est précis à 65% 

accuracy_pourcentage_2 <- sum(diag(cm_2))/sum(cm_2) #le modèle est précis à 58%

#je les ai calculé tous (accuracy, misclassificaation)
# on peut aussi calculer recall & precision see doc LABIFUL machine learning labiful





#OU from you (https://www.youtube.com/watch?v=sMh6g-d6FQk&t=270s)
# ça nous donne matrice de confision et statistique
library(caret)
confusionMatrix(table(predict(my_model_2), variables_2019_2$Cote_crédit)) 
# calcul pour chaque ratings sensitivity,.... 
# calcul aussi Accuracy, CI, p-value








# ok dans final


# on peut l'avoir on shoot from code en haut from youtube après my_modele_2

# CALCUL_P_VALUE From Youtube video : debut près de nnet library ----------------------------------------------------------

# Note : ce Z test = Test de Wald (selon UCLA : https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/)
# et selon https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html

#2 tail z-test         

# on divise les coef par rapport au std error 
# on le fait pour chaque ratio (rating 1 en fonction rating 2, 3, 4, 5, 6, 7)
z_2 <- summary(my_model_2)$coefficients/summary(my_model_2)$standard.errors # = wald test selon UCLA

z_2 <- summary(my_model_2)$coefficients/summary(my_model_2)$standard.errors


# calcul du p_value # tjrs à 5% = 0.05 
p_value_2 <- (1 - pnorm(abs(z_2), 0, 1)) * 2 # on multiplie par 2 car c'est un 2 tails test

p_value_2 <- (1 - pnorm(abs(z_2), 0, 1)) * 2 # tous significative donc c'est notre FINAL MODELE
#MAIS RAFINER PAR DIFFERENTS TEST AVANT ==>donc modèle_3 même

#pAS SIGNIFICATIF ON DELETE DU MODELE FINAL

dim(p_value_2)
dim(p_value_2)

####### from article from ariane : Multinomial Logistic Regression : Kwak, Chanyeong; Clayton-Matthews, Alan
#INTERPRETATION
# Cependant, comme leurs valeurs P étaient trop élevées, elles ont été jugées insignifiantes et ont été supprimées du modèle final.



#si p-value inférieur à 5% ou 0.05 donc significatif

# si on a une variable pour qui tous les p-value de 1 par rapport à 2, 3, 4, 5, 6, et 7
#---->si tous significatifs : on ne drop pas la varaible
#----> si un p-value de la colonne significatif et les autres non, on ne drop pas aussi

#---->PAR CONTRE si tous les p-value sont supérieures à 5% pour ttes les equations
#---->donc not significatif, on laisse tomber la variable
#---->donc la  variable ne contribue pas de façon significative au modèle
#---->DONC ON PEUT L'ENLEVER : POUR CE FAIRE : 
#---->(exemple c'est le cas pour la variable Total_actif) 
#---->GO SEE EN HAUT EQUATION my_modele (2) ==>

#donc pour résumer, si dans la colonne pour un ratio, une p-value not significative et
# le reste siignifactif --> we can't drop
#INTERPRETATION :
# quand la p-value est petite, IC est élevé car IC = 1 - p-value 
#tous les interceptes ont une p-value de 0 ==> donc IC est près de 100% 
#donc interceptes jouent un role significant 


# p-value ratio_ben_avt_impot_sur_frais_int pour (2) = 0.9593431 donc pas significatif
#car supérieur à 5% #--> d'ailleurs même not significatif pour toutes 
# les equations de 2 à 7 -> donc ce ratio n'est pas significatif pour quand 1 est 
#la reférence et qu'on regarder le level 2, 3, 4, 5, 6 ,7 pour la response


# pour ratio_tot_dette_sur_tot_actif, tous les plus value sont petit et 
#inférieures à 5% donc donc le ratio est significatif quant AAA (1) est la reférence
# et qu'on regarde CCC (7) comme response





# ok dans final

# INTERPRETATION MODEL OUTPUT ---------------------------------------------
#ACCURACY & SENSITIVITY

# [1] [video 4] youtube ---> https://www.youtube.com/watch?v=POyTaeneHJY&list=RDCMUCuWECsa_za4gm7B3TLgeV_A&start_radio=1&t=7
n <- table(variables_2019_2$Cote_crédit) #nombre de firme pour chaque type de rating

n/sum(n) # pourcentage de firme par type de rating # la plupart des firmes ont un rating de 4


# [2] ---> c'est comme mon R^2 (R square)
cm_2 / colSums(cm_2)
#INTERPRETATION
#(1H,1V) --> le modèle fait 100% correcte classification pr AAA (1)
#(2H,2V) --> le modèle fait 61.538462% correcte classification pr AA (1)

#CONCLUSION
#donc le modèle est un peu confuse pour rating AA comparé au rating AAA
#mais dû aussi au nombre de AAA pas beaucoup

#donc le modèle fait un bon job 1, 4, comparé au reste

cm_2 / colSums(cm_2)

















# REGRESSION_LINK_SITE_SOURCES --------------------------------------------

#from cours Gestion Quantitative des risk Théme 10  vidéo 40 min (à revoir pour explication modèle probit)

# https://stats.idre.ucla.edu/other/dae/   (exemples de tous types de régression avec tous les logiciels R, SAS, Mplus, SPSS, STATA ) lors théme 10 cours Gestion Quantitative des RIsques dans le DEMO R pour les logit model
# https://www.econometrics-with-r.org/11-2-palr.html   (book : économétrie avec R avec des exemple de probit et logit)


#les variables x chercher leurs sd() aussi
#la variables y faire table,
#dans ucla, voir tests qu'ils ont fait et interprétation




# PARTIE_ANALYSE_ECONOMETRIQUE_SE_REFERER_A_NOTE_TRAVAIL_SESSION_ECONOMETRIE --------

#nos ratings sont des varibales catégorique 1,2,3,..,7 donc la magnitude ne veut rien dire

####VOIR COURS ECONOMETRIE MODELE STATIQUE : QUELS TEST Y FAIRE ?

# ! dans cours économétrie: voir aussi comment éliminer variable si multicolinéarité



# MON TYPE DE SERIE : statique (l'ordre des valeurs n'a pas de sens), différent de serie temporelle

#___________________________________________________________________________________________
#___________________________________________________________________________________________
#___________________________________________________________________________________________
# STATIONNARITE : TEST DE DUCKEY-FULLER : on ne doit pas le faire car vise serie temporelle
# _-_-_-> tous les tests d'hypothèses seront effectués en tenant compte IC de 95%
# _-_-_-> avant d'estmer le modèle : analyse de la STATIONNARITE de nos variables pour s'assuré que le modèle n'est pas erroné
#  on fait test de DICKEY FULLER 
#pour cela transformons données en ts (donc en données temporelles)

# ? quand faire test de stationnarité ? ==>ici on a pas de ts

# not ce ne sont pas les tests appropriés car on a ici une coupe transversale
# et pas un ts (nos data sont nominale)
library(tseries)

plot(good_variables_2$ratio_B_non_rep_sur_Tot_actif, type = 'l') # avec le plot on voit qu'il n' y a pas de stationnarité

tes_station_ratio_B_non_rep_sur_Tot_actif <-good_variables_2$ratio_B_non_rep_sur_Tot_actif

#transformons le data en ts pour éviter mssage WARNING sur le tes de 
m <- ts(data = tes_station_ratio_B_non_rep_sur_Tot_actif)

dev.new()
plot(m)

class(m) # avec le plot on voit qu'il n' y a pas de stationnarité

adf.test(m, alternative = 'stationary') # quand je transorme mes data en ts il n'y a de message WARNING sur le test de Dickey Fuller

#résultat : p-value = 0.02275 soit 2,275% donc < à 5% et donc est-ce qu'on significatif
#donc il n'y a pas de stationnarité 
# ??? mais la question est : 
# est-ce qu'on doit faie un test de STATIONNARITE avec ce type de donnée qui ne sont pas des ts
# est-ce que la stationnarité est importante dans ce type de données 
# je n'ai pas ici des données historique mais toutes les données même date (à savoir 2019)


# MAIS MES DATA NE SONT PAS DES ts donc je ne devais pas les transformer en ts
# et le message d'alerte de R veut simplement dire que la p-value normale est < à 0.01
# ce qui veut dire qu'au seuil de 5%, il y a stationnarité de ma variable.

#FAIRE UN TABLEAU DE PRESENTATION DES P-VALUE DU TEST DE DICKEY FULLER pour chaque variable
# REPONSE :not need car test de Dickey-Fuller vise serie temporelle
#___________________________________________________________________________________________
#___________________________________________________________________________________________
#___________________________________________________________________________________________



# search INDICATEURS ET TEST ECONOMETRIQUE A FAIRE POUR UNE SERIE STATIQUE








#TESTER MULTI COLLINEARITE et ENLEVER CERTAINES VARIABLES : see cours ECONOMETRIE

#







##dans UCLA voir test pour variable significatif
#par ratio de vraisemblance (modele contraint et non contraint)
#done sur cours théme 10 gestion quant des risk




















#####################################done!############################
# mardi 3 Nov : supprimer all lignes et col avec NA only sans other values
######################################################################

#####################################done!############################
# mercredi 4 Nov : (1)  delete doublons 
######################################################################

#####################################done!############################
# jeudi 5 Nov :  #(1.1)impute NA value avec mice, 
# (2) convertir ratings, 
######################################################################


#####################################done!############################
# Vendredi 6 Nov : # (3) constituer good ratios 
# (2.2) Renommer all names en FR 

######################################################################



#####################################done!############################
#Samedi # faire data 2019 (samedi)

######################################################################



#####################################done!############################
#mardi # matrice de correlation good to display

######################################################################
#, (4) faire 1st régression (ici j'ai rating 2020)


#*************OBJECTIF_AVANT_VENDREDI_BIEN-AVANCER*************************


# [1]
# (a) continuer la vidéo  qui restent :--> https://www.youtube.com/watch?v=oxRy2DMrOF4&list=PL34t5iLfZddvv-L5iFFpd_P1jy_7ElWMG&index=2
# NEXT VIDEO 4 # le 3 déjà fait
#calcul R^2 (Rsquare)

# (b)
# faire travail comme travail de session économétrie : éliminer ratio colinéaire
# éliminer multicolinéarité, 
# faire test hétéroscédasticité , ... et corriger s'il y a lieu 
# permet aussi d'éliminer certaines variables


# [2]
# relancer good modèle avec good ratio et interpréter (donner conclusion)


#[3]
# FINAL MODELE -->c'est le modèle ou tous les ratios sont significatifs
# avec p-value à IC : 95%

# [3]
#une fios qu'on a notre good modèle, chaque ligne des coefficients va permettre
# de write une équation ==> avant même partie predict 
# PAR EXMPLE 
# log [Pr(2)/Pr(1)]=intercept + x1 * var1 + x2 * var2 + ........
#INTERPRETATION : le log de la probabilité que le rating soit AA par rapport 
# à la probabilité qu'il soit AAA sera donnée par l'équation

# le RESULTAT (les probabilités log [Pr(2)/Pr(1)],...) QUE DONNE CES EQUATION 
#sont calculé directement par R par predict_proba_2


#chercher rating 2019 pour pouvoir faire traite avec data 2018
# ratings 2020 data (ratios) fin year 2019    # faire attention société to delete en fonction data dispo sur base de données
# ratings 2018 data (ratios) fin year 2018
##################



# refaire tous ces test pour data_sample_test (donc mes ratings_2019 + data 2018)

# faire back test pour see si modèle tient




# quand il y a _-_-_-> mean a put cette note dans doc essai

# quels sont les X qu'on doit mettre


#explication cour théme 10 vers 1H05

# vérification que les gens font en pratique : estimer le probit
# puis le logit et voir si les résultats (le summary) sont semblables
# s'il y a pas trop de différence entre logit vs probit
# ça nous rassure car ça nous dit que le choix de la 
# distribution (le F(.)) a peu d'importance et que la partie 
# importante c'est ce qu'on a spécifié à savoir les variables
# explicatives ==> donc on est rassuré

# alors que 

# à variables explicatives égales, si le fait de changer la distribution
# F(.) contre une normale ou probit ça change beaucoup les coefs
# c'est pas bon car sensibilité troublante (forte) car ça change la 
# forme du CDF 



























#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SOURCES : NOTES LEARNING A PUT EN NOTES APRS : SOURCES

# SERIE TEMPORELLE / WIKIPEDIA
# dans ARMA useing valeurs retardées (d'où le terme Auto Regressive)==>ici must take valeur temporel en compte
# le modèle ARMA ne permet de traiter que les séries dites stationnaires (des moments du premier ordre qui sont invariants au cours du temps)
# Les modèles ARIMA permettent de traiter les séries non stationnaires après avoir déterminé le niveau d'intégration (le nombre de fois qu'il faut différencier la série avant de la rendre stationnaire).
#LACUNE :  le modèle ARIMA ou ARMA souffre d'une lacune majeure : il est incapable de traiter simultanément plus d'une variable (série).
#SOLUTION :  C'est ce qu'a fait en partie Christopher Sims en proposant en 1980 le modèle Vector Auto Regressive (VAR) qui permet de traiter concomitamment plusieurs variables. Mais, contrairement au modèle structurel à plusieurs variables, dans les modèles VAR, toutes les variables sont endogènes. Cette manière de modéliser en faisant abstraction d'une théorie économique a donné naissance à ce que l'on a appelé l'Économétrie sans théorie.

# Ces modèles (ARIMA et VAR) ne permettent de traiter que des phénomènes qui sont linéaires ou approximativement (par exemple le PIB) mais ne permettent pas de "capturer" les propriétés des phénomènes qui sont non linéaires (les variables financières par exemple, inflation, cours d'action etc.). Pour prendre en compte à la fois non linéarité et la forte variabilité de ces variables, l'économètre américain Robert F. Engle a le premier développé le modèle dit ARCH (Auto Regressive Conditional Heteroscedasticity) en 1982.


# SERIE STATIQUE / WIKIPEDIA
# En mathématique, une série statistique est simplement une liste de valeurs d'un même ensemble, dans laquelle l'ordre des termes n'est pas significatif (a contrario d'une série temporelle.
#Pour une série statistique à une variable quantitative, on définit des INDICATEURS classiques que sont la moyenne, la médiane, le mode, les quartiles, déciles et autres quantiles, ainsi que des indicateurs de dispersion comme l'écart interquartile, la variance, l'écart type. On peut ensuite comparer la distribution empirique avec des lois de probabilité usuelles et définir des intervalles de confiance pour leurs paramètres.
#Pour une série statistique à une variable qualitative, on peut déjà calculer les effectifs de chaque valeur observée et calculer les fréquences associées.
#Pour une série statistique à deux variables (ou plus), des méthodes permettent entre autres d'évaluer la liaison entre les variables.



#dans Dickey Fuller : test de stationnarité : unit root = racine unitaire