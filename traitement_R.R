rm(list = ls())

# import_data -------------------------------------------------------------

library(readxl)
X454_data_Copie <- read_excel("C:/Users/aziza/Desktop/Session_3/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/454_data - Copie.xlsx", 
                              col_names = FALSE)
View(X454_data_Copie)

data.1 <- X454_data_Copie[3:23 ,4:ncol(X454_data_Copie)]



# extraction_variables_names ----------------------------------------------------

variables_names <- X454_data_Copie[3,5:39]

# select _2018_data -------------------------------------------------------

data_2018 <- data.1[3,]

data_2018.1 <- data_2018[,2:ncol(data_2018)]




#j'ai 35 ratios

data_2018.2 <- matrix(data_2018.1, 410, 35, byrow = TRUE)

#ajout nom variables

colnames(data_2018.2) <- variables_names



data_2018.2.df <- as.data.frame(data_2018.2)


colSums(is.na(data_2018.2.df))

colnames(data_2018.2.df)




# importer_ratings --------------------------------------------------------

library(readxl)
X454_ratings <- read_excel("C:/Users/aziza/Desktop/Session_3/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/454_ratings.xlsx", 
                           col_names = FALSE)
View(X454_ratings)


#extraction ratings
ratings <- X454_ratings[5:nrow(X454_ratings) , 3]



#cbind ratings et data_2018.2.df

data_2018.2.df <- cbind(ratings, data_2018.2.df)

#renommer premiere colonne par RATINGS
names(data_2018.2.df)[1] <- "RATINGS"





# suppression_des_rows_with_not_data ---------------------------------

#je sais que j'ai 35 variables donc je construit une new database sans
#les rows (firmes) qui ont 35 données manquantes donc qui n'ont aucune donnée
#sur aucune variable

#la somme des NA de chaque row dans la database
rowSums(is.na(data_2018.2.df))

#si on a plus de 35 NA on delete le row (sachant qu'on a 35 variables)
data_2018.3.df <- data_2018.2.df[rowSums(is.na(data_2018.2.df))<35,]




# suppression_des_colonnes_avec_plus_de_228_NA_values ---------------------


#je delete 1 by 1 les colonnes que je veux [le risk c'est que j'en laisse certain et si data base large not possible]
#data_2018.3.df <- data_2018.2.df[,c(-8,-21, -24, -25, -27, -28, -31, -32, -33, -34)]

#SOLUTION

#(1)calculer le nombre de NA par column pour avoir une visibiilité
colSums(is.na(data_2018.3.df))

#(2) constituer une new database qui ne tient compte que des colonnes
#avec moins de 228 NA values (pour savoir le nombre : étape 1)

data_2018.4.df <- data_2018.3.df[ ,colSums(is.na(data_2018.3.df))<228]
dim(data_2018.4.df) # il me reste 24 variables



x <- colnames(data_2018.4.df)




# Delete_duplicate_ou_doublons_en_FR --------------------------------------

data_2018.5.df <- data_2018.4.df[!duplicated(data_2018.4.df[c("PROF_MARGIN", "EBITDA_MARGIN")]),]
dim(data_2018.5.df)





# impute_NA_value_avec_mice -----------------------------------------------

library(mice)

#voir le type de mes données
summary(data_2018.5.df[,2:ncol(data_2018.5.df)])  #données sont des character donc changer en numérique
str(data_2018.5.df)
#changement type character en numeric

#source :  https://stackoverflow.com/questions/37707060/converting-data-frame-column-from-character-to-numeric/37707117
#pour convertir les colonnes 1 by 1 en numeric
#------------> yyz$b <- as.numeric(as.character(yyz$b))

data_2018.5.df$EBITDA_MARGIN <- as.numeric(as.character(data_2018.5.df$EBITDA_MARGIN)) 

#pour onvertir toutes les colonnes en une seule fois en numeric
#------------> yyz[] <- lapply(yyz, function(x) as.numeric(as.character(x)))

data_2018.5.df[,-1] <- lapply(data_2018.5.df[,-1], function(x) as.numeric(as.character(x)))

summary(data_2018.5.df)

# convertir colonne RATINGS en factor
is.factor(data_2018.5.df$RATINGS)

data_2018.5.df$RATINGS <- as.factor(data_2018.5.df$RATINGS)

summary(data_2018.5.df)
dim(data_2018.5.df)


# y a t'il des NA sur EBITDA_MARGIN et ou se trouvent t'ils
any(is.na(data_2018.5.df$EBITDA_MARGIN))
which(is.na(data_2018.5.df$EBITDA_MARGIN))

# exple remplacer par la moyenne toutes les rows vide par exple pour EBITDA_MARGIN
#---> data_2018.5.df$EBITDA_MARGIN[which(is.na(data_2018.5.df$EBITDA_MARGIN))] <- mean(data_2018.5.df$EBITDA_MARGIN, na.rm = TRUE)

#---> which(is.na(data_2018.5.df$EBITDA_MARGIN))
#---> any(is.na(data_2018.5.df$EBITDA_MARGIN))



#continuons avec mice pour remplacer NA value : imputation

# pour savoir les différentes méthodes d'imputation qui exixste sur mice 
#---> methods(mice)


#créons un new dataset pour mice
data_2018.6.df <- data_2018.5.df

summary(data_2018.6.df)


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
data_2018.6.df <- data_2018.6.df[c(-13, -105),]

summary(data_2018.6.df)


#pour maxit, plus c'est grand plus la prédiction est good, 
# solution ne pas tenir compte de la colinéarité : remove.collinear = FALSE
data_imputation <- mice(data_2018.6.df[-1], m=5, method = "pmm", maxit = 20, remove.collinear = TRUE)

#apres le run, data_imputation n'est pas un database mais ce qui a permis de faire imputation

summary(data_2018.6.df$RETURN_COM_EQY)

data_imputation$imp$PROF_MARGIN

final_clean_dataset <- complete(data_imputation, 5)

#pour voir le nombre de loggedEven c--à-d les variables qui n'ont pas été traité par mice
data_imputation$loggedEvents


any(is.na(final_clean_dataset))

colSums(final_clean_dataset)


#il en restre toujours 2 not traité avec la colinéarité donc je le résoud en
#ls remplaçant par la moyenne

#identifions les
summary(final_clean_dataset) 
data_imputation$loggedEvents


final_clean_dataset$EBIT_TO_NET_SALES[which(is.na(final_clean_dataset$EBIT_TO_NET_SALES))] <- mean(final_clean_dataset$EBIT_TO_NET_SALES, na.rm = TRUE)
final_clean_dataset$EBITDA_TO_REVENUE[which(is.na(final_clean_dataset$EBITDA_TO_REVENUE))] <- mean(final_clean_dataset$EBITDA_TO_REVENUE, na.rm = TRUE)


any(is.na(final_clean_dataset)) 

# Calculer pourcentage de data NA -----------------------------------------
p <- function(x){sum(is.na(x))/length(x)*100}
apply(final_clean_dataset, 2, p)

summary(final_clean_dataset)

fix(final_clean_dataset)










#####################################done!############################
# mardi 3 Nov : supprimer all lignes et col avec NA only sans other values
######################################################################


#####################################done!############################
# mercredi 4 Nov : (1)  delete doublons 
######################################################################




#####################################done!############################
# jeudi 5 Nov :  #(1.1)impute NA value avec mice, 
######################################################################



#*************OBJECTIF_AVANT_VENDREDI_BIEN-AVANCER*************************

           
             # (2) convertir ratings, 
             # (3) constituer good ratios, (4) faire 1st régression
##################
