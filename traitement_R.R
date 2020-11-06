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
dim(data_2018.6.df)

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
md.pattern(final_clean_dataset)

#pour plot NA value
library(DataExplorer)
plot_missing(final_clean_dataset)


summary(final_clean_dataset)




# convertir_les_ratings_en_valeur_numérique -------------------------------


#ratings = Variable catégorielle ordonnée

#j'ai 19 nivveaux donc 19 éléments différents et chacun je l'ai 1 ou plusieurs fois
my_RATINGS <- data_2018.6.df$RATINGS
my_RATINGS <- matrix(my_RATINGS)
dim(my_RATINGS)





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




# (1)convertir les rating en chiffres (en tenant compte investement et speculative grade)
#conversion des grand groupe en valeur numerique 1, 2, 3, 4, 5, 6, 7


grd_grp_num <- c("AAA"=1, "AA"=2,"A"=3,
                 "BBB"=4,"BB"=5,"B"=6, "CCC"=7)

my_rating_grd_num <- grd_grp_num[my_rating_grd_grp]

#pour pouvoir comparer si good après conversion
see_2 <- matrix(my_rating_grd_num)
dim(see_2)



#juste pour faire good plot

see <- factor(see, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
a <- cbind(see, final_clean_dataset)

library(dplyr)
count_data <- a %>% 
  count(see)


#changer ordre bin
ggplot(count_data, aes(x=see, y=n))+
  geom_bar(stat = "identity", fill="gray16")+
  geom_text(aes(label=n), vjust=-0.500)+
  labs(x="Cote de crédit", u="Nombre d'observation",
       title = "")
  






# cbind()_ratings_numérik_et_final_data ------------------------------------

data_2018_avt_good_ratio <-cbind(my_rating_grd_num, final_clean_dataset)

#plot see_2 pour voir hist des mes ratings

# hist(data_2018_avt_good_ratio$my_rating_grd_num)



library(ggplot2)

ggplot(data_2018_avt_good_ratio,aes(x=my_rating_grd_num))+
  geom_bar()  


#Renommer all names en FR 








#---------------------- a delete after done------------------
#je peux même le mettre apès lors qu'il ne va rester que faire la régression
#---------------------- a delete after done------------------


#(2) définissons le niveau (levels) des facteurs précisant 
#le level de chacun
#permet de passer aux régressions
# si on le transforme en facteur avant ça va réarranger l'ordre 
#ajoutons un ordre au facteur my_rating_grd_num   # EN LEVER POUR L'INSTANT



#---------------------- a delete after done------------------
#on a déjà renommer col et rating en cote_crédit
#on a déjà fait cbind() entre cte_crédit et data et call final data
#dnc only colonne cote_credit qu'on met en facteur
#---------------------- a delete after done------------------



final_data <- factor(final_data$cote_crédit, ordered = TRUE)
#(levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
#si on le met, ça reconverti tout en lettre

#pour see sous forme matrice
mat <- matrix(my_rating_grd_num_fac)

class(my_rating_grd_num_fac) # classe
typeof(my_rating_grd_num_fac) # sa nature

#extraire le levels (19 différents)
levels_ratings <- levels(my_rating_grd_num_fac)
length(levels_ratings)









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



#*************OBJECTIF_AVANT_VENDREDI_BIEN-AVANCER*************************

           
              # (2.2) Renommer all names en FR 
             # (3) constituer good ratios, (4) faire 1st régression
##################
