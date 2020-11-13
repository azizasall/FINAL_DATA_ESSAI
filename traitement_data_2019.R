################################################################################
# CHANGER DATA 2018 PAR 2019

# Deja checked pas identique les data 2018 et 2019



rm(list = ls())

# import_data -------------------------------------------------------------

library(readxl)
X454_data_Copie <- read_excel("C:/Users/aziza/Desktop/Session_3/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/454_data - Copie.xlsx", 
                              col_names = FALSE)
View(X454_data_Copie)

data.1 <- X454_data_Copie[3:23 ,4:ncol(X454_data_Copie)]



# extraction_variables_names ----------------------------------------------------

variables_names <- X454_data_Copie[3,5:39]

# select _2019_data -------------------------------------------------------

data_2019 <- data.1[2,]

data_2019.1 <- data_2019[,2:ncol(data_2019)]




#j'ai 35 ratios

data_2019.2 <- matrix(data_2019.1, 410, 35, byrow = TRUE)

#ajout nom variables

colnames(data_2019.2) <- variables_names



data_2019.2.df <- as.data.frame(data_2019.2)
dim(data_2019.2.df)

colSums(is.na(data_2019.2.df))

colnames(data_2019.2.df)




# importer_ratings_2020 --------------------------------------------------------

library(readxl)
X454_ratings <- read_excel("C:/Users/aziza/Desktop/Session_3/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/454_ratings.xlsx", 
                           col_names = FALSE)
View(X454_ratings)


#extraction ratings
ratings <- X454_ratings[5:nrow(X454_ratings) , 3]



#cbind ratings et data_2019.2.df

data_2019.2.df <- cbind(ratings, data_2019.2.df)
dim(ratings)
dim(data_2019.2.df)

#renommer premiere colonne par RATINGS
names(data_2019.2.df)[1] <- "RATINGS"





# suppression_des_rows_with_not_data ---------------------------------

#je sais que j'ai 35 variables donc je construit une new database sans
#les rows (firmes) qui ont 35 données manquantes donc qui n'ont aucune donnée
#sur aucune variable

#la somme des NA de chaque row dans la database
rowSums(is.na(data_2019.2.df))
colSums(is.na(data_2019.2.df))

#si on a plus de 35 NA on delete le row (sachant qu'on a 35 variables)
data_2019.3.df <- data_2019.2.df[rowSums(is.na(data_2019.2.df))<35,]

dim(data_2019.3.df)



# suppression_des_colonnes_avec_plus_de_228_NA_values ---------------------


#je delete 1 by 1 les colonnes que je veux [le risk c'est que j'en laisse certain et si data base large not possible]
#data_2019.3.df <- data_2019.2.df[,c(-8,-21, -24, -25, -27, -28, -31, -32, -33, -34)]

#SOLUTION

#(1)calculer le nombre de NA par column pour avoir une visibiilité
colSums(is.na(data_2019.3.df))


#^^^^^^^^^^^^Not need pour 2019 ça va sauter comme PROFIT MARGIN^^^^^^^^^^^^
#^^^^^^^^^^^^à delete si je veux^^^^^^^^^^^^
#_____Pour_see_/_à_after_mice_nbr_NA_227_____
p_1 <- matrix(data_2019.3.df$TOT_MKT_VAL)




# est-ce que je ne peux pas faire la même chose "mice" pour un ratio de vol
# avec beaucoup de NA exple pour BEST_EBIT STDDEV
#ou garder le seul que j'ai applied beta ? 

#BEST_EBIT_STDDEV beaucoup de NA donc garder celui que j'ai et travailler avec
check <- matrix(data_2019.3.df$BEST_EBIT_STDDEV)




#(2) constituer une new database qui ne tient compte que des colonnes
#avec moins de 228 NA values (pour savoir le nombre : étape 1)

#calcul la somme des NA sur les colonnes et les colonne dont la somme
#des NA est supérieure à 228 ne les prends pas en compte
#x <- y[, colSums(is.na(y))<220]

data_2019.4.df <- data_2019.3.df[ ,colSums(is.na(data_2019.3.df))<220]
dim(data_2019.4.df) # 2018 il me reste 25 variables
                    # 2019 il me reste 23 variables


x <- colnames(data_2019.4.df)




# Delete_duplicate_ou_doublons_en_FR --------------------------------------

#ça me laisse seulement le 1er des éléments dupliqués
data_2019.5.df <- data_2019.4.df[!duplicated(data_2019.4.df[c("EBITDA_MARGIN", "EBITDA_TO_REVENUE")]),]
dim(data_2019.5.df)


is.na(data_2019.5.df)
any(is.na(data_2019.5.df))
colSums(is.na(data_2019.5.df))
rowSums(is.na(data_2019.5.df))



# impute_NA_value_avec_mice -----------------------------------------------

library(mice)

#voir le type de mes données
summary(data_2019.5.df[,2:ncol(data_2019.5.df)])  #données sont des character donc changer en numérique
str(data_2019.5.df)


#changement type character en numeric

#source :  https://stackoverflow.com/questions/37707060/converting-data-frame-column-from-character-to-numeric/37707117
#pour convertir les colonnes 1 by 1 en numeric
#------------> yyz$b <- as.numeric(as.character(yyz$b))

data_2019.5.df$EBITDA_MARGIN <- as.numeric(as.character(data_2019.5.df$EBITDA_MARGIN)) 

#pour onvertir toutes les colonnes en une seule fois en numeric
#------------> yyz[] <- lapply(yyz, function(x) as.numeric(as.character(x)))

data_2019.5.df[,-1] <- lapply(data_2019.5.df[,-1], function(x) as.numeric(as.character(x)))

summary(data_2019.5.df)
dim(data_2019.5.df)
# convertir colonne RATINGS en factor
is.factor(data_2019.5.df$RATINGS)

data_2019.5.df$RATINGS <- as.factor(data_2019.5.df$RATINGS)

summary(data_2019.5.df)
dim(data_2019.5.df)


# y a t'il des NA sur EBITDA_MARGIN et ou se trouvent t'ils
any(is.na(data_2019.5.df$EBITDA_MARGIN))
which(is.na(data_2019.5.df$EBITDA_MARGIN))

# exple remplacer par la moyenne toutes les rows vide par exple pour EBITDA_MARGIN
#---> data_2019.5.df$EBITDA_MARGIN[which(is.na(data_2019.5.df$EBITDA_MARGIN))] <- mean(data_2019.5.df$EBITDA_MARGIN, na.rm = TRUE)

#---> which(is.na(data_2019.5.df$EBITDA_MARGIN))
#---> any(is.na(data_2019.5.df$EBITDA_MARGIN))



#continuons avec mice pour remplacer NA value : imputation

# pour savoir les différentes méthodes d'imputation qui exixste sur mice 
#---> methods(mice)


#créons un new dataset pour mice
data_2019.6.df <- data_2019.5.df

summary(data_2019.6.df)


#=============================================================================

#lorsque je l'ai fait la premiere fois ça n'a pas marché pour certaine variables

#car colinéaire à un autre : solution : remove.collinear = FALSE

#car leur min -33 000 trés démesuré par rapport au max +90 pour la
#firme numéro 13 donc must delete this firme pour que ça soit cohérent

#dans la ligne 105 aussi on retrouve la même chose

#il en rete toujours car il est collinéaire avec un autre donc solution
# remove.collinear = FALSE
#=============================================================================


#créons un new dataset ou l'on delete firmes à la ligne numero 13 et 105
#--------------not enlever line-----------------------
#data_2019.6.df <- data_2019.6.df[c(-21, -29, -43, -60, -63 ,-117, -136),] #PAS MËME CONTEXT
#je le fait au suivant pour la ligne 13

# sur EBITDA_MARGIN et EBIT_TO_NET_SALES et EBIT_TO_REVENUE je détecte une valeur abérante 
# le min par rapport au max ne fait pas de sens donc enlevons la ligne

#quand on le sort() on voit que c'est une valeur abbérante car on quitte -33292 pour aller à -40 le plus petit qui suit et tous à la ligne 3 
sort(data_2019.6.df$EBIT_TO_NET_SALES)

#identifions le d'abord
which.min(data_2019.6.df$EBITDA_MARGIN) # le donnée se trouve à la ligne 13
which.min(data_2019.6.df$EBIT_TO_NET_SALES) # le donnée se trouve à la ligne 13
which.min(data_2019.6.df$EBITDA_TO_REVENUE)
#supprimons la lignes
data_2019.6.df <- data_2019.6.df[-13,] # pour checher si c'est enlevé, lorsque je refais le which.min pour savoir ou se trouve le min c'est à a ligne 95

#enlevons EBITDA_TO_REVENUE car identique à EBITDA_TO_MARGIN (ici c'est la colonne qu'on enlève)
data_2019.6.df <- data_2019.6.df[-3]
dim(data_2019.6.df)
dim(data_2019.5.df)



n <- colnames(data_2019.6.df)

summary(data_2019.6.df)
dim(data_2019.6.df)
fix(data_2019.6.df)




#pour maxit, plus c'est grand plus la prédiction est good, 
# solution ne pas tenir compte de la colinéarité : remove.collinear = FALSE

#identifier la liste des colonnes sans NA value pour ne pas en tenir compte dans imputation de mice
which(colSums(is.na(data_2019.6.df))==0)


library(mice)
#mice quand ça ne marche pas 3 problèmes en générale que si tu les règles ça résoud le problème
# [1] le premier enlever les lignes qui qui on des valeur abérrrantes par exemple le min est de -33 200 et le suivant -40 dand tu fais le sort()
# [2] enlever les variables qui sont identiques sur les données mais only nom de variable qui diffèrent par exemple (EBITDA_TO_REVENUE & EBITDA_TO_MARGIN) et j'ai enlevé 1
# [3] lorsqu'on fait le mice ne pas tenir compte des colonnes sans NA 
# après ça quand je fais mice(), ça marche sans loggedEvents

#data_imputation <- mice(data_2019.6.df[-1], m=5, method = "pmm", maxit = 5, remove.collinear = TRUE)
data_imputation <- mice(data_2019.6.df[,c(-1, -6, -8, -10, -13, -19)], m=5, method = "pmm", maxit = 5, remove.collinear = TRUE)
#si j'applique le mice du haut, il y aura toujours un loggedEvents car y a beaucoup de colonnes qui n'ont pas de NA donc mieux vaut ne pas en tenir compte dans le mice


#avant de passer faisons l'xtraction de colonnes car après need them lors réintégration
#à l'exception du 1 qui est = à ratings
var_not_ds_mice <- data_2019.6.df[,c(6, 8, 10, 13, 19)]

col_names_var_not_ds_mice <- colnames(var_not_ds_mice)



#je ne tiens pas compte des colonnes sans NA #on les enleve de l'imputation

#s'il y en a
#pour voir le nombre de loggedEven c--à-d les variables qui n'ont pas été traité par mice
data_imputation$loggedEvents #ici c'est EBIT_TO_NET_SALES donc on le tient pas compte de l'imutation 
                             #et comme il n'a que 3 NA on va le remplacer par sa moyenne
                             #mais après avoir relancer depuis data_2019.5.df on voir qu'il n'y en a pas. ça apparaisait toujours car je le lancer à partir de la dernière base de données ou j'avais fait des modifications sans l'actualiser
colSums(is.na(data_2019.6.df))

fix(data_2019.6.df)

#apres le run, data_imputation n'est pas un database mais ce qui a permis de faire imputation

summary(data_2019.6.df$RETURN_COM_EQY)

data_imputation$imp$EBITDA_MARGIN

final_clean_dataset <- complete(data_imputation, 5)



any(is.na(final_clean_dataset))

colSums(final_clean_dataset) #qd y a NA colSums return NA pour colonne avec NA

colSums(is.na(final_clean_dataset))





#^^^^^^^^^^^^à delete si je veux^^^^^^^^^^^^
#_____Pour_see_/_à_after_mice_nbr_NA_avant_227_____
#p_2 <- matrix(final_clean_dataset$TOT_MKT_VAL) #plus applicable car plus de TOT_MARKET_VALUE sur 2019 car delete car beaucoup de NA value
#fix(p_2)

#p_1




#il en restre toujours 2 not traité avec la colinéarité donc je le résoud en
#ls remplaçant par la moyenne

#identifions les
summary(final_clean_dataset) 
data_imputation$loggedEvents



#not need comparé à 2018 car ici le mice(à tout réglé) donc not need de remplacer ces 2 par la moyenne

#final_clean_dataset$EBIT_TO_NET_SALES[which(is.na(final_clean_dataset$EBIT_TO_NET_SALES))] <- mean(final_clean_dataset$EBIT_TO_NET_SALES, na.rm = TRUE)
#final_clean_dataset$EBITDA_TO_REVENUE[which(is.na(final_clean_dataset$EBITDA_TO_REVENUE))] <- mean(final_clean_dataset$EBITDA_TO_REVENUE, na.rm = TRUE)


any(is.na(final_clean_dataset)) 

# Calculer pourcentage de data NA -----------------------------------------
p <- function(x){sum(is.na(x))/length(x)*100}
apply(final_clean_dataset, 2, p)


dim(final_clean_dataset) # 16 variables car on dans le micee(), on avait pas tenue compte des 6 variables qui n'avait aucune NA value. Donc il faut les remettre dans final_clean_dataset
dim(data_2019.5.df)      #23 variables
dim(data_2019.6.df)      #22 variables


#ajoutons les colonnes qui n'avaient pas de NA value dans data_2019.6.df dans final_clean_dataset sans ajouter le 1er qui correspond au ratings car on va l'ajouter plus tard


final_clean_dataset.2 <- cbind(final_clean_dataset, data_2019.6.df[,c(6, 8, 10, 13, 19)])
dim(final_clean_dataset.2)

#vérifions si toutes les variables sont présentes
colnames(final_clean_dataset.2)==colnames(data_2019.6.df)

check_col_if_same <- matrix(c(colnames(final_clean_dataset.2),colnames(data_2019.6.df)), 2, 22)
#--------->https://abcdr.thinkr.fr/comment-comparer-deux-series-de-valeurs-avec-r/
#autres façons de faire
union(colnames(final_clean_dataset.2),colnames(data_2019.6.df)) # et comme c'est 22 donc j'ai mes colonnes de final_clean_dataset.2 égales à celles de data_2019.6.df 
unique(c(colnames(final_clean_dataset.2),colnames(data_2019.6.df))) 
#La fonction intersect(x,y) donne un vecteur composé des éléments qui se trouvent à la fois dans x et dans y (intersection de x et de y).
intersect(colnames(final_clean_dataset.2),colnames(data_2019.6.df))
#La fonction setdiff(x,y) donne tous les éléments de x qui ne sont pas dans y. Attention setdiff(y,x) donnera tous les éléments de y qui ne sont pas dans x, donc le résultat ne sera pas le même.
setdiff(colnames(final_clean_dataset.2),colnames(data_2019.6.df))
#La fonction setequal(x,y) retourne la valeur TRUE si x est le même vecteur que y, FALSE sinon.
setequal(colnames(final_clean_dataset.2),colnames(data_2019.6.df))




#ou 
colSums(is.na(final_clean_dataset.2))

md.pattern(final_clean_dataset)

#pour plot NA value
library(DataExplorer)
plot_missing(final_clean_dataset.2)


summary(final_clean_dataset.2)
colnames(final_clean_dataset.2)



# convertir_les_ratings_en_valeur_numérique -------------------------------


#ratings = Variable catégorielle ordonnée

#j'ai 19 nivveaux donc 19 éléments différents et chacun je l'ai 1 ou plusieurs fois
my_RATINGS <- data_2019.6.df$RATINGS
my_RATINGS <- matrix(my_RATINGS)
dim(my_RATINGS)


#juste pour checher mes level donc create new variable
rate_check <- factor(my_RATINGS)
levels(rate_check)



#avant de le convertir en factor on doit faire les conversion
# en grand groupe et en numerique sinon ça va inverser l'ordre
#lors transformation en grd group et en numeric



# conversion en grand groupe AAA, AA, A, BBB, BB, B, CCC
# way [ http://www.duclert.org/r-vecteurs-operations/vecteurs-R.php  ]
#test
# ----> v <- c("a" = "A", "b" = "B", "c" = "C"); 
# ----> v[c("a", "b", "b", "a", "c")] 


grd_grp_rat <- c("AAA"="AAA", "AA+"="AA", "AA"="AA", "AA-"="AA",
                 "A+"="A", "A"="A", "A-"="A",
                 "BBB+"="BBB", "BBB"="BBB", "BBB-"="BBB",
                 "BB+"="BB", "BB"="BB", "BB-"="BB",
                 "B+"="B","B"="B","B-"="B",
                 "CCC+"="CCC", "CCC"="CCC","CCC-"="CCC")

my_rating_grd_grp <- grd_grp_rat[my_RATINGS]

#pour voir si conversion bien fait / a my_RATINGS
see <- matrix(my_rating_grd_grp)
dim(see)

fix(see)





# (1)convertir les rating en chiffres (en tenant compte investement et speculative grade)
#conversion des grand groupe en valeur numerique 1, 2, 3, 4, 5, 6, 7


grd_grp_num <- c("AAA"=1, "AA"=2,"A"=3,
                 "BBB"=4,"BB"=5,"B"=6, "CCC"=7)

my_rating_grd_num <- grd_grp_num[my_rating_grd_grp]

#pour pouvoir comparer si good après conversion
see_2 <- matrix(my_rating_grd_num)
dim(see_2)



#juste pour faire good plot
#on met factor en spécifiant l'ordre des levels (le niveau de chaque factor)
see <- factor(see, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
a <- cbind(see, final_clean_dataset.2) # car j'ai besoin que ça soit une base de données pour pouvoir le manipuler dans dplyr et dans ggplot



#pour tester quelque chose
#write.csv(a, file = "test_good_var_avec_char_rating.csv") # si on oublie le .csv le dossier ne sera pas dans le wd




library(dplyr)
count_data <- a %>% 
  count(see)


#changer ordre bin

dev.new() # permet de sortir le plot zoom de son cadre et si j'en écrit envore ça m'en sort un autre

library(ggplot2)
ggplot(count_data, aes(x=see, y=n))+
  geom_bar(stat = "identity", fill="gray16")+
  geom_text(aes(label=n), vjust=-0.500)+
  labs(x="Cote de crédit 2020", u="Nombre d'observation",
       title = "")







# cbind()_ratings_numérik_et_final_data ------------------------------------

data_2019_avt_good_ratio <-cbind(my_rating_grd_num, final_clean_dataset.2)
my_rating_grd_num #ça le donne le level de chaque chiffre =rating car je les ai défini plus haut
dim(final_clean_dataset.2)
dim(data_2019_avt_good_ratio)


colnames(data_2019_avt_good_ratio)

is.element("WORKING_CAPITAL" , colnames(data_2019_avt_good_ratio))
is.element("TOT_MKT_VAL" , colnames(data_2019_avt_good_ratio))

# constituer good ratio ---------------------------------------------------

#je peux use tidyverse plutot commode
library(dplyr)  # database : data_2019_avt_good_ratio
data_2019_with_good_ratio <- data_2019_avt_good_ratio%>%
  mutate(ratio_tot_liab_sur_tot_actif=BS_TOTAL_LIABILITIES/BS_TOT_ASSET,
         ratio_B_non_rep_sur_Tot_actif = BS_PURE_RETAINED_EARNINGS/BS_TOT_ASSET,
         ratio_Flux_de_TR_expl_sur_passif_cour = CF_CASH_FROM_OPER/BS_CUR_LIAB,
         ratio_Fonds_de_roulement = WORKING_CAPITAL/BS_TOT_ASSET)
         #ratio_val_marchd_tot_sur_tot_actif =TOT_MKT_VAL/BS_TOT_ASSET) is deleted car TOT_MKT8VAL pas dans new database

colnames(data_2019_with_good_ratio)

dim(data_2019_avt_good_ratio)
dim(data_2019_with_good_ratio)

length(colnames(data_2019_with_good_ratio))



### {quand puis je dire que ces elements sont déterminants des ratings des firmes canadienne?}
### est-ce qand c'est good R_square ??? ==> voir solution Amdoumi




# Renommer all names en FR  -----------------------------------------------

nom_col <- colnames(data_2019_with_good_ratio)
length(nom_col)





#par rapport à 2018 j'enlève les 2 ratios qui ont beaucoup de NA ("Marge_Bénéficiaire" et "Valeur_marchd_tot") et 1 qui est identique à une autre ("EBITDA_sur_Revenu")
#je déplacce aussi ceux que j'avais pas tenue compte lors du mice()

# j'enleve le dernier ratio not pris en compte : ratio_val_marchd_tot_sur_tot_actif

# ceux que j'ai mi en dernier avant de put les ratios de dplyr sont stockés dans col_names_var_not_ds_mice
col_names_var_not_ds_mice #elles sont au nombre de 5 -> ci dessous par ordre
#  "Dette_LT", "Total_actif",  "Trésorie d'exp",  "ratio_tot_dette_sur_tot_actif",  "Marge_d_explt",
# now mettons les avant les 4 ratios constituées avec dplyr donc les 4 last ratios

nom_col_good <- c("Cote_crédit", "Marge_sur_EBITDA",
                  "Marge_sur_EBIT", "Rendement_sur_cap_prop",
                  "Rendement_sur_actif", "Total_passif",
                  "Bénéfice_non_rep",
                  "Passif_courant", "ratio_ben_avt_impot_sur_frais_int", "ratio_actuel",
                  "Ratio_de_liquid_réduite", "Ratio_de_liquidité", "Fonds_roulement",
                  "Ratio_Fonds_de_roulmt_sur_ventes",
                  "Beta_applique", "Croissance_rev_adjuste", "croissance_geom_des_actifs",
                  "Dette_LT", "Total_actif",  "Trésorie d'exp",  "ratio_tot_dette_sur_tot_actif",  "Marge_d_explt",
                  "ratio_tot_liab_sur_tot_actif", "ratio_B_non_rep_sur_Tot_actif", "ratio_Flux_de_TR_expl_sur_passif_cour",
                  "ratio_Fonds_de_roulement")



length(nom_col_good)
length(nom_col_good)

#juste pour des fins de comparaisons

matrice_comparaison_nom_var <- matrix(c(nom_col,nom_col_good), 26, 2)

fix(matrice_comparaison_nom_var)#le 3 est good car EBIT_TO_NET_SALES = Marge_sur_EBIT

#good car comparaison équitable dans leur positionnement


# renommer variables ------------------------------------------------------

#pour ne pas avoir nom colonne les mêmes 
data_2019_with_good_ratio.1 <- data_2019_with_good_ratio


colnames(data_2019_with_good_ratio.1) <- nom_col_good



# extrraire database avec bons ratios only ---------------------------------

#j'enlève aussi ceux que j'avais deleted (comme profit margin) et les mettre en ordre
#donc je run dejà existant et ceux not dispo dans data_2019_with_good_ratio.1 marque error et je les delete ici

library(dplyr)
bon_variables <- data_2019_with_good_ratio.1%>%
  select(Cote_crédit, Marge_sur_EBITDA,
         Marge_sur_EBIT, Rendement_sur_cap_prop, Rendement_sur_actif,
         ratio_tot_liab_sur_tot_actif, ratio_B_non_rep_sur_Tot_actif,
         ratio_Flux_de_TR_expl_sur_passif_cour, ratio_ben_avt_impot_sur_frais_int,
         ratio_tot_dette_sur_tot_actif, ratio_actuel, Ratio_de_liquid_réduite, ratio_Fonds_de_roulement,
         Ratio_de_liquidité, Ratio_Fonds_de_roulmt_sur_ventes, Total_actif,
         Marge_d_explt, Beta_applique)


fix(bon_variables)


#extraire base de donneés sur excel

#write.csv(my_extract_data, file = "data_base_2018") # on fait l'extraction 

#------> write.csv(bon_variables, file = "ma_base_de_donnees_2019.csv") # si on oublie le .csv le dossier ne sera pas dans le wd




# REGRESSION AVEC bon_variables -------------------------------------------

#*** travailler now avec base de données car à chaque relance des 
#*** codes en haut j'ai une nouvelle base de données à cause du mice

#*** Donc new fichier R nommé REG ou j'importe mas base de données
#*** 

# si c'était rating 2020 et ratio data 2019 j'allait pour faire rég
# ou rating 2019 et ratio data 2019

bon_variables$Cote_crédit <- as.factor(bon_variables$Cote_crédit)
class(bon_variables$Cote_crédit)
levels(bon_variables$Cote_crédit)

str(bon_variables)
summary(bon_variables)
cor(bon_variables[,-1])


#je fais la régression de investement grade (firme d'investissement)
# en fonction de spéculative grade (firme spéculative)

library(nnet)

#indiquons le levels de reférence
#pour moi investement grade donc 1, 2, 3, 4
bon_variables$Cote_crédit <- relevel(bon_variables$Cote_crédit, ref = "1") #car me dit que ref doit être de longueur 1

my_model <- multinom(Cote_crédit~., data = bon_variables)
summary(my_model)



#




#Partition data pour sortir un échantillon
#not dans ce cas car mon échantillon test est dans data 2018 et ratings 2019
#mais juste pour know comment le faire
#donc pour cela on partition les données 
set.seed(222)

bon_variables_tets <- bon_variables

sample_test <- sample(2, nrow(bon_variables_tets),
                      replace = TRUE,
                      prob = c(0.7, 0.3)) #la probabilité : &er database 70% et 2nd database 30% data
sample_training_test <- bon_variables_tets[sample_test==1,]
sampe_testing_test <- bon_variables_tets[sample_test==2,]













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


#*************OBJECTIF_AVANT_VENDREDI_BIEN-AVANCER*************************



#, (4) faire 1st régression (ici j'ai rating 2020)

#chercher rating 2019 pour pouvoir faire traite avec data 2018
# ratings 2020 data (ratios) fin year 2019    # faire attention société to delete en fonction data dispo sur base de données
# ratings 2018 data (ratios) fin year 2018
##################


# faire back test pour see si modèle tient

