
# REGRESSION AVEC good_variables -------------------------------------------

#*** travailler now avec base de données car à chaque relance des 
#*** codes en haut j'ai une nouvelle base de données à cause du mice

#*** Donc new fichier R nommé REG ou j'importe ma base de données
#*** 

# si c'était rating 2020 et ratio data 2019 j'allait pouvoir faire rég
# ou rating 2019 et ratio data 2019

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


# importer data (fichier : ma_base_de_donnees_2019.csv) par read.c --------


# on le nomme good_variables 


getwd() # my working directory est ou mon fichier doit être pour que read.csv le trouve si c'était pas un projet ça allait me sortir bureau puis document et donc c'est là que j'allais devoir mettre mon dossier excel


good_variables <- read.csv("ma_base_de_donnees_2019_rating_char_&_num.csv", 
                          header = TRUE, sep = ",", quote = "\"",
                          dec = ".", fill = TRUE, comment.char = "")



#renommons my_rating_grd_grp en Cote_credit
names(good_variables)[2] <- "Cote_credit"



#sans colonne 1 & 3
good_variables_1 <- good_variables[c(-1,-3)]




#si je voulais ne pas télécharger de new database avec ratings en character
good_variables$Cote_crédit #on sélectionne lle good variable avec 1:7 (donc num)

#puis on le tranforme en factor avec les character par labels
good_variables$Cote_crédit <- factor(good_variables$Cote_crédit,
                                     levels = c(1:7), 
                                     labels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
#ça le transforme direct en character






# Pour respecter l'ordre des Levels ---------------------------------------


#indiquons le levels de reférence # on l'enregistre dans out
#pour moi investement grade donc 1, 2, 3, 4 et now AAA, AA, A, BBB, ...

#not need de les transformer en 1, 2, ...,7 
#donc laisser grand group rating

# Cote_creditF le F pour FACTOR => donc on crée new variable et on l'ajoute dans good_variables_1
good_variables_1$Cote_credit <- factor(good_variables_1$Cote_credit, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))

is.factor(good_variables_1$Cote_credit)




# statistiques Descriptives -----------------------------------------------

attach(good_variables_1)


table(Cote_credit) #ça me compte le nombre de chaque élément de chaque facter
#AAA j'en ai que 2 donc si je le fixe comme reférence only ça ne fera pas de sens
#pour les prédiction donc dans multinomial logit reg fixer AAA, AA et A comme ref



summary(good_variables_1)


# Pas trop interessant caar ça me donne pour chaque chiffre 
#différent de Marge_sur_EBIT le nombre de chaque type de ratings que j'ai
# ça aurait pu être intéressant si on avait une autre variable nominale

#----> x <- with(good_variables_1, table(Cote_credit, Marge_sur_EBIT))



# faire la moyenne par nombre de rating

# ne marche pas et pourtant marche dans UCLA site
# normalement devait permettre de faire le calcul de la moyenne et du SD par type de ratiing
#------> with(good_variables_1, do.call(rbind, tapply(Cote_credit ,Marge_sur_EBITDA, function(x) c(M = mean(x), SD = sd(x)))))







good_variables_1$Cote_credit <- factor(good_variables_1$Cote_credit, levels =c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
class(good_variables_1$Cote_credit)
levels(good_variables_1$Cote_credit)

str(good_variables_1)
summary(good_variables_1)









table(Cote_credit) #compter le nombre d'élément pour chaque facter







# pour la CORRELATION can use aussi PACKAGE DataExplorer et faire plot_correlation

library(DataExplorer)
plot_correlation(x)















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
# FROM YOUTUBE VIDEO --> link : https://www.youtube.com/watch?v=fDjKa7yWk1U&list=PLiX9HD2vtIDEM4Ttu3bqffXMN1TwsM5Jl&index=1
# ------------------------->
#je fais la régression de investement grade (firme d'investissement)
# en fonction de spéculative grade (firme spéculative)

# ALL INTERPERETATION C'EST POUR MODEL 1
library(nnet)

#REIMPORT DATA BASE avec AAA, ... rating grand gourpe
#dans ma base de donnée changer A,2,..,7 par rating groupe


#dans nos données il n'y a pas d'aspect temporel, 
#c'est purement une coupe transversale



# Pour la régression
# indiquons le reférence level en 1, 2, 3, 4, 5, 6, 7 => pour nous c'est 1
#good_variables_1$Cote_credit <- relevel(good_variables_1$Cote_credit, ref = "1") #car me dit que ref doit être de longueur 1
good_variables_1$Cote_credit <- relevel(good_variables_1$Cote_credit, ref = "AAA") #  on use 1 comme le refrence level et on le put dans out qu'on vient de créer




# MODELE_PROCESSING -------------------------------------------------------

# Développons notre multinomial modéle

#pour quoi nous avons choisi le multinom :
#Below we use the multinom function from the nnet package to estimate a multinomial logistic regression model. There are other functions in other R packages capable of multinomial regression. We chose the multinom function because it does not require the data to be reshaped (as the mlogit package does) and to mirror the example code found in Hilbe's Logistic Regression Models.



#(1) modèle de départ qui nous permet de calculer p-value
my_model_1 <- multinom(Cote_credit~., data = good_variables_1)















#it's goo j'ai les même chose : https://irudnyts.github.io/multinomial-regression/
#______________________________________________________________________________________________________
# TRY WITH MLOGIT 
library(mlogit)

attach(good_variables_1)

long_data0 = mlogit.data(good_variables_1, choice ="Cote_credit" , shape = "wide")

mlogit_model <- mlogit(Cote_credit~0|Marge_sur_EBIT, data = long_data0)
summary(mlogit_model)

#marche only si je ne mets pas good_variables en factor 

#CHECK TRY (MULTINOM)
my_model_check <- multinom(Cote_credit~Marge_sur_EBIT, data = good_variables_1)

#______________________________________________________________________________________________________
# TRY
library(VGAM)
fit_vgam <- vglm(Cote_credit ~ Marge_sur_EBIT, multinomial(refLevel = "AAA"), 
                 data = good_variables_1)


#______________________________________________________________________________________________________
# TRY
library(mnlogit)
fit_mnlogit <- mnlogit(Cote_credit ~ 1 | Marge_sur_EBIT | 1, long_data0)



summary(fit_vgam)
summary(my_model_check)
summary(mlogit_model)

#______________________________________________________________________________________________________














#suite

#(2) après calcul p-value, on see que les 5 variables 
#[a] Marge_sur_EBITDA, [b] Rendement_sur_cap_prop, [c]ratio_ben_avt_impot_sur_frais_int,
#[d] ratio_tot_dette_sur_tot_actif, [e] Total_actif, 
#not significatif pour all 6 equation donc on le drop
#pour faire le modèle sans ces variables qui ne sont pas significatives  
my_model_2 <- multinom(Cote_credit~.- Marge_sur_EBITDA - Rendement_sur_cap_prop - ratio_ben_avt_impot_sur_frais_int - ratio_tot_dette_sur_tot_actif - Total_actif,
                     data = good_variables_1)


#on refait tout ce qui suit pour my_model_2 pour aller checker les p-values
#de nouveau





# Test de vraisemblance : Likelihood test

#Pour estimer le coefficient de corrélation
nullmodele_1 <- multinom(Cote_credit~1,  data=good_variables_1)
summary(nullmodele_1)

L <-1-logLik(my_model_1)/logLik(nullmodele_1)

# ? LR chi^2(5) = 108
# ? Prob > chi^2 = 
# ? Pseudo R^2 = 0.4664486
# ? Log Lihelihood = 






#ou
# pour my modèle 1 si on devait taper toutes les varaibles 1 par 1 

#my_model_1 <- multinom(Cote_credit~Marge_sur_EBITDA+Marge_sur_EBIT+Rendement_sur_cap_prop+
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





# FROM UCLA ---------------------------------------------------------------
# The ratio of the probability of choosing one outcome category over the probability of choosing the baseline category is often referred as relative risk (and it is sometimes referred to as odds, described in the regression parameters above). The relative risk is the right-hand side linear equation exponentiated, leading to the fact that the exponentiated regression coefficients are relative risk ratios for a unit change in the predictor variable. We can exponentiate the coefficients from our model to see these risk ratios.
exp(coef(my_model_1))

exp(coef(my_model_1))


# You can also use predicted probabilities to help you understand the model. You can calculate predicted probabilities for each of our outcome levels using the fitted function. We can start by generating the predicted probabilities for the observations in our dataset and viewing the first few rows
head(prédict_proba_ucla <- fitted(my_model_1)) # mm chose que [predict_proba_1]



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
predict_ratings_1 <- predict(my_model_1, good_variables_1) # ça only le fait pour mes 156 observations

predict_ratings_2 <- predict(my_model_2, good_variables_1)

#nombre de fois ou on a la même chose dans les prédictions pour le good modèle
#et le modèle partiel (régression alternative) #can use it pour tester 
#LA SIGNIFICATIVITE CONJOINTE : on a modèle contraint et modèle non contraint


#COMPARAISONS
#entre les 2 modèle
sum(as.numeric(predict_ratings_1==predict_ratings_2))

#entre modèle 1 et rating départ
sum(as.numeric(predict_ratings_1==good_variables_1$Cote_credit))

#entre modèle 2 et rating départ
sum(as.numeric(predict_ratings_2==good_variables_1$Cote_credit))


# ____INTERPRETATION____ de predict_ratings_1 au dessus : le modèle prédit la prmeière firme avec un rating de BB,
# la firme numero 2 avec un rating de BBB, 
# la firme numero3 avec un rating de A,  
# la firme numero 136 avec un rating de A
  

# donc si on le compare par rapport à ce qu'on avait au départ, 
# on a la même chose pour la firme 1 et la firme 2 prédiction = rating de départ, donc on a un good match
# pour la firme 3, il y a une misclassification car on avait 6 (B) et le modèle prédit 3 (A)

#si je veux faire la prédiction des probabilités
predict_proba_1 <- predict(my_model_1, good_variables_1, type = "prob") # OUTPUT j'ai la prédiction de la prob de chaque variable pour chaque firme 
head(predict_proba_1)

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
cm_1 <- table(predict(my_model_1), good_variables_1$Cote_credit)

cm_2 <- table(predict(my_model_2), good_variables_1$Cote_credit)

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
miscalsification_pourcentage_1 <- 1- sum(diag(cm_1))/sum(cm_1) # sans le 1 ça donne accuracy ou précision du modèle et avec le 1 - ça donne le misclasification errors

miscalsification_pourcentage_2 <- 1- sum(diag(cm_2))/sum(cm_2) 

#°°°INTERESTING°°°
#INTERPRETATION_1 : 34.62% du temps le modele misclassifie les ratings
#ça signifie que le modèle dit que pour la firme (voir interprétation cm_1 (3H, 2V))
#le rating doit être  2 (AA) mais en réalité le rating de cette firme est 3 (A)

#SI ON AVAIT MODELE AVEC 100% DE PRECISION, LES ELEMENTS HORS DIAGONALE DONNERONT ZERO


#ACCURACY DU MODEL
# sans le 1 ça donne accuracy ou précision du modèle et avec le 1 - ça donne le misclasification errors
accuracy_pourcentage_1 <- sum(diag(cm_1))/sum(cm_1) #le modèle est précis à 65%

accuracy_pourcentage_2 <- sum(diag(cm_2))/sum(cm_2) #le modèle est précis à 58%

#je les ai calculé tous (accuracy, misclassificaation)
# on peut aussi calculer recall & precision see doc LABIFUL machine learning labiful









# CALCUL_P_VALUE ----------------------------------------------------------

#2 tail z-test

# on divise les coef par rapport au std error 
# on le fait pour chaque ratio (rating 1 en fonction rating 2, 3, 4, 5, 6, 7)
z_1 <- summary(my_model_1)$coefficients/summary(my_model_1)$standard.errors

z_2 <- summary(my_model_2)$coefficients/summary(my_model_2)$standard.errors


# calcul du p_value # tjrs à 5% = 0.05 
p_value_1 <- (1 - pnorm(abs(z_1), 0, 1)) * 2 # on multiplie par 2 car c'est un 2 tails test

p_value_2 <- (1 - pnorm(abs(z_2), 0, 1)) * 2 # tous significative donc c'est notre FINAL MODELE
#MAIS RAFINER PAR DIFFERENTS TEST AVANT ==>donc modèle_3 même

#pAS SIGNIFICATIF ON DELETE DU MODELE FINAL

dim(p_value_1)
dim(p_value_2)

####### from article from ariane : Multinomial Logistic Regression : Kwak, Chanyeong; Clayton-Matthews, Alan
#INTERPRETATION
# Cependant, comme leurs valeurs P étaient trop élevées, elles ont été jugées insignifiantes et ont été supprimées du modèle final.



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





# INTERPRETATION MODEL OUTPUT ---------------------------------------------
#ACCURACY & SENSITIVITY

# [1] [video 4] youtube ---> https://www.youtube.com/watch?v=POyTaeneHJY&list=RDCMUCuWECsa_za4gm7B3TLgeV_A&start_radio=1&t=7
n <- table(Cote_credit) #nombre de firme pour chaque type de rating

n/sum(n) # pourcentage de firme par type de rating # la plupart des firmes ont un rating de 4


# [2] ---> c'est comme mon R^2 (R square)
cm_1 / colSums(cm_1)
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

plot(good_variables_1$ratio_B_non_rep_sur_Tot_actif, type = 'l') # avec le plot on voit qu'il n' y a pas de stationnarité

tes_station_ratio_B_non_rep_sur_Tot_actif <-good_variables_1$ratio_B_non_rep_sur_Tot_actif

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
#sont calculé directement par R par predict_proba_1


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