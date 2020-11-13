
# REGRESSION AVEC good_variables -------------------------------------------

#*** travailler now avec base de données car à chaque relance des 
#*** codes en haut j'ai une nouvelle base de données à cause du mice

#*** Donc new fichier R nommé REG ou j'importe ma base de données
#*** 

# si c'était rating 2020 et ratio data 2019 j'allait pouvoir faire rég
# ou rating 2019 et ratio data 2019

rm(list=ls())




#importer data (fichier : ma_base_de_donnees_2019.csv) par read.csv

# on le nomme good_variables 


getwd() # my working directory est ou mon fichier doit être pour que read.csv le trouve si c'était pas un projet ça allait me sortir bureau puis document et donc c'est là que j'allais devoir mettre mon dossier excel


good_variables <- read.csv("ma_base_de_donnees_2019.csv", 
                          header = TRUE, sep = ",", quote = "\"",
                          dec = ".", fill = TRUE, comment.char = "")

summary(good_variables)

#sans colonne 1
good_variables_1 <- good_variables[-1]


good_variables_1$Cote_crédit <- as.factor(good_variables_1$Cote_crédit)
class(good_variables_1$Cote_crédit)
levels(good_variables_1$Cote_crédit)

str(good_variables_1)
summary(good_variables_1)


table(Cote_crédit) #ça me compte le nombre de chaque élément

#attach(good_variables_1)

# done! : en faire une matrice de correlation good

# cor(good_variables_1[,-1]) ça seulement not beau à voir et difficile à comprendre car beaucoup de variables donc library ggcorrplot

library(ggplot2)
library(ggcorrplot)

corr = round(cor(good_variables_1[,-1]), 1)

dev.new()
ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("tomato2", "thistle", "springgreen3"),
           ggtheme = theme_bw)


#library(corrplot)
#my_cor_plot <- cor(good_variables_1[,-1])

#dev.new()
#corrplot(my_cor_plot)

#dev.new()
#corrplot.mixed(my_cor_plot, upper = "number")









# ------------------------->
# A DELETE MES DONNEES NOT SAME FORMAT
# FROM EXEMPLE CRAN MLOGIT
# ------------------------->
library("mlogit")
table(Cote_crédit) #ça me compte le nombre de chaque élément


ml_model_1 <- mlogit(Cote_crédit ~ good_variables_1[-1] , data = good_variables_1, reflevel = "1")

H <- dfidx(good_variables_1, choice = "Cote_crédit", varying = c(1:18))

m <- mlogit(good_variables_1, Cote_crédit~ Marge_sur_EBITDA + Rendement_sur_cap_prop)
# ------------------------->
# A DELETE MES DONNEES NOT SAME FORMAT
# FROM EXEMPLE CRAN MLOGIT
# ------------------------->








# ------------------------->
# FROM YOUTUBE VIDEO --> link : https://www.youtube.com/watch?v=fDjKa7yWk1U&list=PLiX9HD2vtIDEM4Ttu3bqffXMN1TwsM5Jl&index=1
# ------------------------->
#je fais la régression de investement grade (firme d'investissement)
# en fonction de spéculative grade (firme spéculative)

# ALL INTERPERETATION C'EST POUR MODEL 1
library(nnet)


#indiquons le levels de reférence # on l'enregistre dans out
#pour moi investement grade donc 1, 2, 3, 4

# Cote_créditF le F pour FACTOR => donc on crée new variable et on l'ajoute dans good_variables_1
good_variables_1$Cote_crédit <- factor(good_variables_1$Cote_crédit)


# indiquons le reférence level en 1, 2, 3, 4, 5, 6, 7 => pour nous c'est 1
#good_variables_1$Cote_crédit <- relevel(good_variables_1$Cote_crédit, ref = "1") #car me dit que ref doit être de longueur 1
good_variables_1$Cote_crédit <- relevel(good_variables_1$Cote_crédit, ref = "1") #  on use 1 comme le refrence level et on le put dans out qu'on vient de créer




# Développons notre multinomial modéle

#(1) modèle de départ qui nous permet de calculer p-value
my_model_1 <- multinom(Cote_crédit~., data = good_variables_1)


#(2) après calcul p-value, on see que les 5 variables 
#[a] Marge_sur_EBITDA, [b] Rendement_sur_cap_prop, [c]ratio_ben_avt_impot_sur_frais_int,
#[d] ratio_tot_dette_sur_tot_actif, [e] Total_actif, 
#not significatif pour all 6 equation donc on le drop
#pour faire le modèle sans ces variables qui ne sont pas significatives  
my_model_2 <- multinom(Cote_crédit~.- Marge_sur_EBITDA - Rendement_sur_cap_prop - ratio_ben_avt_impot_sur_frais_int - ratio_tot_dette_sur_tot_actif - Total_actif,
                     data = good_variables_1)


#on refait tout ce qui suit pour my_model_2 pour aller checker les p-values
#de nouveau




#ou
# pour my modèle 1 si on devait taper toutes les varaibles 1 par 1 

#my_model_1 <- multinom(Cote_crédit~Marge_sur_EBITDA+Marge_sur_EBIT+Rendement_sur_cap_prop+
#                     Rendement_sur_actif+ratio_tot_liab_sur_tot_actif+
#                       ratio_B_non_rep_sur_Tot_actif+ratio_Flux_de_TR_expl_sur_passif_cour+
#                       ratio_ben_avt_impot_sur_frais_int+ratio_tot_dette_sur_tot_actif+
#                       ratio_actuel+Ratio_de_liquid_réduite+ratio_Fonds_de_roulement+Ratio_de_liquidité+
#                       Ratio_Fonds_de_roulmt_sur_ventes+Total_actif+Marge_d_explt+Beta_applique, data = good_variables_1)



#can use variable comme ça car appliqué attach() function
summary(my_model_1)
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

# prédiction de mes ratings 
predict_ratings_1 <- predict(my_model_1, good_variables_1) # ça only le fait pour mes 156 observations

predict_ratings_2 <- predict(my_model_2, good_variables_1)

#nombre de fois ou on a la même chose dans les prédictions pour le good modèle
#et le modèle partiel (régression alternative) #can use it pour tester 
#LA SIGNIFICATIVITE CONJOINTE

sum(as.numeric(predict_ratings_1==predict_ratings_2))

# ____INTERPRETATION____ de l'quation au dessus : le modèle prédit la prmeière firme avec un rating de BB,
# la firme numero 2 avec un rating de BBB, 
# la firme numero3 avec un rating de A,  
# la firme numero 136 avec un rating de A
  

# donc si on le compare par rapport à ce qu'on avait au départ, 
# on a la même chose pour la firme 1 et la firme 2 prédiction = rating de départ, donc on a un good match
# pour la firme 3, il y a une misclassification car on avait 6 (B) et le modèle prédit 3 (A)

#si je veux faire la prédiction des probabilités
predict_proba_1 <- predict(my_model_1, good_variables_1, type = "prob") # OUTPUT j'ai la prédiction de la prob de chaque variable pour chaque firme 

predict_proba_2 <- predict(my_model_2, good_variables_1, type = "prob")

# somme des probabilités par ligne = 1
# pour le checher on put réusltat dans matrice
my_mat_proba_predit_1 <- matrix(predict_proba_1, 156, 7)
sum(my_mat_proba_predit_1[1, ])


my_mat_proba_predit_2 <- matrix(predict_proba_2, 156, 7)
sum(my_mat_proba_predit_2[1, ])


# INTERPETATION : 
#pour la ligne 1  1, la probabilité que la firme soit rated 1 (AAA) est très faible (5.512994e_08)


# si on veut la prédiction de la probabilité only pour certaines firmes spécifique
predict_proba_pr_certain_only_1 <- predict(my_model_1, good_variables_1[c(1,10,40,50,156),], type = "prob")

predict_proba_pr_certain_only_2 <- predict(my_model_2, good_variables_1[c(1,10,40,50,156),], type = "prob")

#### MISCLASSIFICATION ERROR
# donc on compare la prédiction du modele avec les good data actuelle 
# pour voir combien le match ne tient pas

# cm = confusion matrix
cm_1 <- table(predict(my_model_1), good_variables_1$Cote_crédit)

cm_2 <- table(predict(my_model_2), good_variables_1$Cote_crédit)

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
miscalsification_pourcentage_1 <- 1- sum(diag(cm_1))/sum(cm_1) 

miscalsification_pourcentage_2 <- 1- sum(diag(cm_2))/sum(cm_2) 

#INTERPRETATION_1 : 34.62% du temps le modele misclassifie les ratings


#2 tail z-test

# on divise les coef par rapport au std error 
# on le fait pour chaque ratio (rating 1 en fonction rating 2, 3, 4, 5, 6, 7)
z_1 <- summary(my_model_1)$coefficients/summary(my_model_1)$standard.errors

z_2 <- summary(my_model_2)$coefficients/summary(my_model_2)$standard.errors


# calcul du p_value # tjrs à 5% = 0.05 
p_value_1 <- (1 - pnorm(abs(z_1), 0, 1)) * 2 # on multiplie par 2 car c'est un 2 tails test

p_value_2 <- (1 - pnorm(abs(z_2), 0, 1)) * 2

dim(p_value_1)
dim(p_value_2)

#si p-value inférieur à 5% ou 0.05 donc significatif

# si on a un variable pour qui tous les p-value de 1 par rapport à 2, 3, 4, 5, 6, et 7
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





#


















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



# faire travail comme travail de session économétrie : éliminer ratio colinéaire
# éliminer multicolinéarité, 
# faire test hétéroscédasticité , ... et corriger s'il y a lieu 

#chercher rating 2019 pour pouvoir faire traite avec data 2018
# ratings 2020 data (ratios) fin year 2019    # faire attention société to delete en fonction data dispo sur base de données
# ratings 2018 data (ratios) fin year 2018
##################


# faire back test pour see si modèle tient

