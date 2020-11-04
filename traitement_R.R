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

View(data_2018.1)



#j'ai 35 ratios

data_2018.2 <- matrix(data_2018.1, 410, 35, byrow = TRUE)

#ajout nom variables

colnames(data_2018.2) <- variables_names






data_2018.2.df <- as.data.frame(data_2018.2)


View(data_2018.2.df)

fix(data_2018.2.df)


colSums(is.na(data_2018.2.df))

colnames(data_2018.2.df)




# importer_ratings --------------------------------------------------------

library(readxl)
X454_ratings <- read_excel("C:/Users/aziza/Desktop/Session_3/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/454_ratings.xlsx", 
                           col_names = FALSE)
View(X454_ratings)


#extraction ratings
ratings <- X454_ratings[5:nrow(X454_ratings) , 3]

ratings


#cbind ratings et data_2018.2.df

data_2018.2.df <- cbind(ratings, data_2018.2.df)
fix(data_2018.2.df)

# suppression_des_rows_with_not_data ---------------------------------

#je sais que j'ai 35 variables donc je construit une new database sans
#les rows (firmes) qui ont 35 données manquantes donc qui n'ont aucune donnée
#sur aucune variable

#la somme des NA de chaque row dans la database
rowSums(is.na(data_2018.2.df))

#si on a plus de 35 NA on delete le row (sachant qu'on a 35 variables)
data_2018.3.df <- data_2018.2.df[rowSums(is.na(data_2018.2.df))<35,]
fix(data_2018.3.df)



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

fix(data_2018.4.df)

x <- colnames(data_2018.4.df)




# Delete_duplicate_ou_doublons_en_FR --------------------------------------

data_2018.5.df <- data_2018.4.df[!duplicated(data_2018.4.df[c("PROF_MARGIN", "EBITDA_MARGIN")]),]
dim(data_2018.5.df)
fix(data_2018.5.df)



# impute_NA_value_avec_mice -----------------------------------------------

library(mice)

#voir le type de mes données
summary(data_2018.5.df[,2:ncol(data_2018.5.df)])  #données sont des character donc changer en numérique

#changement type character en numeric

library(dplyr)

x <- colnames(data_2018.5.df)

as.vector(x)

#quand je fais select(PROF_MARGIN)%>%  ça marche


  data_2018.5.df %>%
  select(GEO_GROW_TOT_ASSET)%>%
  unlist()%>%
  class()



  
  data_2018.5.df %>%
    select(starts_with("PROF_MARGIN"))%>%
    select(ends_with("GEO_GROW_TOT_ASSET"))%>%
    unlist()%>%
    class()
  

mean(data_2018.5.df$EBITDA_TO_REVENUE) # see message d'erreur

summary(data_2018.5.df)



summary(data_2018.5.df[-1])

str(data_2018.5.df)




# Calculer pourcentage de data NA -----------------------------------------
p <- function(x){sum(is.na(x))/length(x)*100}
apply(data_2018.4.df, 2, p)











#####################################done!############################
# mardi : supprimer all lignes et col avec NA only sans other values
######################################################################


#####################################done!############################
# mercredi : (1)  delete doublons 
######################################################################




#*************OBJECTIF_AVANT_VENDREDI_BIEN-AVANCER*************************



            #(1.1)impute NA value avec mice, 
             # (2) convertir ratings, 
             # (3) constituer good ratios, (4) faire 1st régression
##################
