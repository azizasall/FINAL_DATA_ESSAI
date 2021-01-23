################################################################################
# DATA BASE POUR 
# 2000, 
# pick entre les 2,  
# 2007, 
# 2008, 
# 2009, 
# pick entre les 2 , ==> 2018
#2019





rm(list = ls())



library(DataExplorer)    #plot_missing()  fait le plot par colonne



# import_data -------------------------------------------------------------


library(readxl)
X454_data_Copie <- read_excel("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/454_data - Copie.xlsx", 
                              col_names = FALSE)
##View(X454_data_Copie)

# VOLATILITE_2
library(readxl)
VOLATILITY_2_454 <- read_excel("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/VOLATILITY_#_2_454.xlsx", 
                               col_names = FALSE)
##View(VOLATILITY_2_454)






data.1 <- X454_data_Copie[3:23 ,4:ncol(X454_data_Copie)]
##View(data.1)




vol_data.1 <- VOLATILITY_2_454[6:51 ,5:ncol(VOLATILITY_2_454)]
##View(vol_data.1)


# extraction_variables_names ----------------------------------------------------

variables_names <- X454_data_Copie[3,5:39]
dim(variables_names)



vol_variables_names <- VOLATILITY_2_454[6,7:31]
dim(vol_variables_names)



# pour résoudre problème variables_names, le fait qu'il commence à compter à partir de 5
# et donc lorsqu'on le ajoute rating qu'il en ajoute 4 de plus
variables_names <- matrix(variables_names, 1,35)
vol_variables_names <- matrix(vol_variables_names, 1,25)


# select _2000_2007_2008_2009_2016_2018_2019_data -------------------------------------------------------

###fix(data.1)

data_2000 <- data.1[21,]
data_2007 <- data.1[14,]
data_2008 <- data.1[13,]
data_2009 <- data.1[12,]
data_2016 <- data.1[5,]
data_2018 <- data.1[3,]
data_2019 <- data.1[2,]



vol_2019 <- vol_data.1[3,]

vol_2018 <- vol_data.1[4,]

vol_2016_1 <- vol_data.1[8,]
vol_2016_2 <- vol_data.1[9,]
vol_2016_3 <- vol_data.1[10,]

vol_2009_1 <- vol_data.1[23,]
vol_2009_2 <- vol_data.1[24,]

vol_2008_1 <- vol_data.1[25,]
vol_2008_2 <- vol_data.1[26,]

vol_2007_1 <- vol_data.1[27,]
vol_2007_2 <- vol_data.1[28,]

vol_2000_1 <- vol_data.1[41,]
vol_2000_2 <- vol_data.1[42,]



###fix(data_2000)


#chech qu'il sont bons
##View(data_2000)
##View(data_2007)
##View(data_2008)
##View(data_2009)
##View(data_2016)
##View(data_2018)
##View(data_2019)



data_2000.1 <- data_2000[,2:ncol(data_2000)]
data_2007.1 <- data_2007[,2:ncol(data_2007)]
data_2008.1 <- data_2008[,2:ncol(data_2008)]
data_2009.1 <- data_2009[,2:ncol(data_2009)]
data_2016.1 <- data_2016[,2:ncol(data_2016)]
data_2018.1 <- data_2018[,2:ncol(data_2018)]
data_2019.1 <- data_2019[,2:ncol(data_2019)]

dim(data_2000.1)



vol_2019.1 <-  vol_2019[,3:ncol(vol_2019)]

vol_2018.1 <- vol_2018[,3:ncol(vol_2018)]

vol_2016.1 <- vol_2016_1[,3:ncol(vol_2016_1)]
vol_2016.2 <- vol_2016_1[,3:ncol(vol_2016_2)]
vol_2016.3 <- vol_2016_3[,3:ncol(vol_2016_3)]

vol_2009.1 <- vol_2009_1[,3:ncol(vol_2009_1)]
vol_2009.2 <- vol_2009_2[,3:ncol(vol_2009_2)]

vol_2008.1 <- vol_2008_1[,3:ncol(vol_2008_1)]
vol_2008.2 <- vol_2008_2[,3:ncol(vol_2008_2)]

vol_2007.1 <- vol_2007_1[,3:ncol(vol_2007_1)]
vol_2007.2 <- vol_2007_2[,3:ncol(vol_2007_2)]

vol_2000.1 <- vol_2000_1[,3:ncol(vol_2000_1)]
vol_2000.2 <- vol_2000_2[,3:ncol(vol_2000_2)]





#j'ai 35 ratios
##View(data_2000.1)

data_2000.2 <- matrix(data_2000.1, 410, 35, byrow = TRUE)
data_2007.2 <- matrix(data_2007.1, 410, 35, byrow = TRUE)
data_2008.2 <- matrix(data_2008.1, 410, 35, byrow = TRUE)
data_2009.2 <- matrix(data_2009.1, 410, 35, byrow = TRUE)
data_2016.2 <- matrix(data_2016.1, 410, 35, byrow = TRUE)
data_2018.2 <- matrix(data_2018.1, 410, 35, byrow = TRUE)
data_2019.2 <- matrix(data_2019.1, 410, 35, byrow = TRUE)



vol_2019.1_2 <- matrix(vol_2019.1, 410, 25, byrow = TRUE)

vol_2018.1_2 <- matrix(vol_2018.1, 410, 25, byrow = TRUE)

vol_2016.1_2 <- matrix(vol_2016.1, 410, 25, byrow = TRUE)
vol_2016.2_2 <- matrix(vol_2016.2, 410, 25, byrow = TRUE)
vol_2016.3_2 <- matrix(vol_2016.3, 410, 25, byrow = TRUE)

vol_2009.1_2 <- matrix(vol_2009.1, 410, 25, byrow = TRUE)
vol_2009.2_2 <- matrix(vol_2009.2, 410, 25, byrow = TRUE)

vol_2008.1_2 <- matrix(vol_2008.1, 410, 25, byrow = TRUE)
vol_2008.2_2 <- matrix(vol_2008.2, 410, 25, byrow = TRUE)

vol_2007.1_2 <- matrix(vol_2007.1, 410, 25, byrow = TRUE)
vol_2007.2_2 <- matrix(vol_2007.2, 410, 25, byrow = TRUE)

vol_2000.1_2 <- matrix(vol_2000.1, 410, 25, byrow = TRUE)
vol_2000.2_2 <- matrix(vol_2000.2, 410, 25, byrow = TRUE)













data_2000.2.df <- as.data.frame(data_2000.2)
dim(data_2000.2.df)
##View(data_2000.2.df)
##fix(data_2000.2.df)

data_2007.2.df <- as.data.frame(data_2007.2)
dim(data_2007.2.df)

data_2008.2.df <- as.data.frame(data_2008.2)
dim(data_2008.2.df)

data_2009.2.df <- as.data.frame(data_2009.2)
dim(data_2009.2.df)

data_2016.2.df <- as.data.frame(data_2016.2)
dim(data_2016.2.df)

data_2018.2.df <- as.data.frame(data_2018.2)
dim(data_2018.2.df)

data_2019.2.df <- as.data.frame(data_2019.2)
dim(data_2019.2.df)





vol_2019.1_2_2.df <- as.data.frame(vol_2019.1_2)

vol_2018.1_2_2.df <- as.data.frame(vol_2018.1_2)

vol_2016.1_2_2.df <- as.data.frame(vol_2016.1_2)
vol_2016.2_2_2.df <- as.data.frame(vol_2016.2_2)
vol_2016.3_2_2.df <- as.data.frame(vol_2016.3_2)

vol_2009.1_2_2.df <- as.data.frame(vol_2009.1_2)
vol_2009.2_2_2.df <- as.data.frame(vol_2009.2_2)

vol_2008.1_2_2.df <- as.data.frame(vol_2008.1_2)
vol_2008.2_2_2.df <- as.data.frame(vol_2008.2_2)

vol_2007.1_2_2.df <- as.data.frame(vol_2007.1_2)
vol_2007.2_2_2.df <- as.data.frame(vol_2007.2_2)

vol_2000.1_2_2.df <- as.data.frame(vol_2000.1_2)
vol_2000.2_2_2.df <- as.data.frame(vol_2000.2_2)



















library(DataExplorer)
plot_missing(data_2000.2.df)
plot_missing(data_2007.2.df)
plot_missing(data_2008.2.df)
plot_missing(data_2009.2.df)
plot_missing(data_2016.2.df)
plot_missing(data_2018.2.df)
plot_missing(data_2019.2.df)





colSums(is.na(data_2000.2.df))
colSums(is.na(data_2007.2.df))
colSums(is.na(data_2008.2.df))
colSums(is.na(data_2009.2.df))
colSums(is.na(data_2016.2.df))
colSums(is.na(data_2018.2.df))
colSums(is.na(data_2019.2.df))






colnames(data_2000.2.df)
colnames(data_2007.2.df)
colnames(data_2008.2.df)
colnames(data_2009.2.df)
colnames(data_2016.2.df)
colnames(data_2018.2.df)
colnames(data_2019.2.df)



# Package DataExplorer


#plot_missing(as.numeric(as.character(data_2000.2.df)))



# importer_ratings_2000_2007_2008_2009_2016_2018_2019 --------------------------------------------------------
# From doc SDM


library(readxl)
ratings_SDM <- read_excel("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/SDM_ratings_2020-12-18-Elhadji-Adbou-Aziz-Sall_a_jour.xlsx", 
                           col_names = FALSE)
##View(ratings_SDM)




#extraction ratings
ratings_2000 <-matrix(ratings_SDM[9, 3:ncol(ratings_SDM)],410,1)
ratings_2000 <- data.frame(ratings_2000)


##View(ratings_2000)
dim(ratings_2000)



ratings_2007 <-matrix(ratings_SDM[16, 3:ncol(ratings_SDM)],410,1)
ratings_2007 <- data.frame(ratings_2007)

ratings_2008 <-matrix(ratings_SDM[17, 3:ncol(ratings_SDM)],410,1)
ratings_2008 <- data.frame(ratings_2008)

ratings_2009 <-matrix(ratings_SDM[18, 3:ncol(ratings_SDM)],410,1)
ratings_2009 <- data.frame(ratings_2009)

ratings_2016 <-matrix(ratings_SDM[25, 3:ncol(ratings_SDM)],410,1)
ratings_2016 <- data.frame(ratings_2016)

ratings_2018 <-matrix(ratings_SDM[27, 3:ncol(ratings_SDM)],410,1)
ratings_2018 <- data.frame(ratings_2018)

ratings_2019 <-matrix(ratings_SDM[28, 3:ncol(ratings_SDM)],410,1)
ratings_2019 <- data.frame(ratings_2019)





# TO DELETE
#ratings_2007 <- ratings_SDM[16, 3:ncol(ratings_SDM)]
#ratings_2008 <- ratings_SDM[17, 3:ncol(ratings_SDM)]
#ratings_2009 <- ratings_SDM[18, 3:ncol(ratings_SDM)]
#ratings_2016 <- ratings_SDM[25, 3:ncol(ratings_SDM)]
#ratings_2018 <- ratings_SDM[27, 3:ncol(ratings_SDM)]
#ratings_2019 <- ratings_SDM[28, 3:ncol(ratings_SDM)]




#cbind ratings et data_2019.2.df


## line : https://stackoverflow.com/questions/14872081/add-a-pre#fix-to-column-names

# let change les noms des colonnes pour pouvoir faire plot_missig par la suite
# car actuellement ce qu'on a c'est : V1, V2, V3, ... et puis lorsqu'on le cbind on a de nouveau V1, V2, V3, ... 
# donc une répétition c'est pourquoi ça me dit que je fais un duplicata lorsque je veux lancer un plot_missing




# 2000
#colnames(data_2000.2.df) <- variables_names
#fix(data_2000.2.df)
#colnames(data_2000.2.df) <- paste("2000_data", colnames(data_2000.2.df), sep = "_")
#data_2000.2.df

colnames(vol_2000.1_2_2.df) <- vol_variables_names
colnames(vol_2000.1_2_2.df) <- paste("2000_vol_1", colnames(vol_2000.1_2_2.df), sep = "_")


colnames(vol_2000.2_2_2.df) <- vol_variables_names
colnames( vol_2000.2_2_2.df) <- paste("2000_vol_2", colnames( vol_2000.2_2_2.df), sep = "_")



# 2007
#colnames(data_2007.2.df) <- variables_names
#colnames(data_2007.2.df) <- paste("2007_data", colnames(data_2007.2.df), sep = "_")
#data_2007.2.df

colnames(vol_2007.1_2_2.df) <- vol_variables_names
colnames(vol_2007.1_2_2.df) <- paste("2007_vol_1", colnames(vol_2007.1_2_2.df), sep = "_")
vol_2007.1_2_2.df

colnames(vol_2007.2_2_2.df) <- vol_variables_names
colnames( vol_2007.2_2_2.df) <- paste("2007_vol_2", colnames( vol_2007.2_2_2.df), sep = "_")
vol_2007.2_2_2.df


# 2008
#colnames(data_2008.2.df) <- variables_names
#colnames(data_2008.2.df) <- paste("2008_data", colnames(data_2008.2.df), sep = "_")
#data_2008.2.df

colnames(vol_2008.1_2_2.df) <- vol_variables_names
colnames(vol_2008.1_2_2.df) <- paste("2008_vol_1", colnames(vol_2008.1_2_2.df), sep = "_")
vol_2008.1_2_2.df

colnames(vol_2008.2_2_2.df) <- vol_variables_names
colnames( vol_2008.2_2_2.df) <- paste("2008_vol_2", colnames( vol_2008.2_2_2.df), sep = "_")
vol_2008.2_2_2.df


# 2009
#colnames(data_2009.2.df) <- variables_names
#colnames(data_2009.2.df) <- paste("2009_data", colnames(data_2009.2.df), sep = "_")
#data_2009.2.df

colnames(vol_2008.1_2_2.df) <- vol_variables_names
colnames(vol_2009.1_2_2.df) <- paste("2009_vol_1", colnames(vol_2009.1_2_2.df), sep = "_")
vol_2009.1_2_2.df

colnames(vol_2009.2_2_2.df) <- vol_variables_names
colnames( vol_2009.2_2_2.df) <- paste("2009_vol_2", colnames( vol_2009.2_2_2.df), sep = "_")
vol_2009.2_2_2.df



# 2016
#colnames(data_2016.2.df) <- variables_names
#colnames(data_2016.2.df) <- paste("2016_data", colnames(data_2016.2.df), sep = "_")
#data_2016.2.df

colnames(vol_2016.1_2_2.df) <- vol_variables_names
colnames(vol_2016.1_2_2.df) <- paste("2016_vol_1", colnames(vol_2016.1_2_2.df), sep = "_")
vol_2016.1_2_2.df

colnames(vol_2016.2_2_2.df) <- vol_variables_names
colnames( vol_2016.2_2_2.df) <- paste("2016_vol_2", colnames( vol_2016.2_2_2.df), sep = "_")
vol_2016.2_2_2.df

colnames(vol_2016.3_2_2.df) <- vol_variables_names
colnames( vol_2016.3_2_2.df) <- paste("2016_vol_3", colnames( vol_2016.3_2_2.df), sep = "_")
vol_2016.3_2_2.df


# 2018
#colnames(data_2018.2.df) <- variables_names
#colnames(data_2018.2.df) <- paste("2018_data", colnames(data_2018.2.df), sep = "_")
#data_2018.2.df

colnames(vol_2018.1_2_2.df) <- vol_variables_names
colnames(vol_2018.1_2_2.df) <- paste("2018_vol_1", colnames(vol_2018.1_2_2.df), sep = "_")
vol_2018.1_2_2.df


# 2019
#colnames(data_2019.2.df) <- variables_names
#colnames(data_2019.2.df) <- paste("2019_data", colnames(data_2019.2.df), sep = "_")
#data_2019.2.df

colnames(vol_2019.1_2_2.df) <- vol_variables_names
colnames(vol_2019.1_2_2.df) <- paste("2019_vol_1", colnames(vol_2019.1_2_2.df), sep = "_")
colnames(vol_2019.1_2_2.df)















#cbind

data_2000.2.df_rat <- cbind(ratings_2000, data_2000.2.df,vol_2000.1_2_2.df, vol_2000.2_2_2.df)
##View(data_2000.2.df_rat)
#fix(data_2000.2.df_rat)
dim(data_2000.2.df_rat)



data_2007.2.df_rat <- cbind(ratings_2007, data_2007.2.df, vol_2007.1_2_2.df, vol_2007.2_2_2.df)
data_2008.2.df_rat <- cbind(ratings_2008, data_2008.2.df, vol_2008.1_2_2.df, vol_2008.2_2_2.df)
data_2009.2.df_rat <- cbind(ratings_2009, data_2009.2.df, vol_2009.1_2_2.df, vol_2009.2_2_2.df)
data_2016.2.df_rat <- cbind(ratings_2016, data_2016.2.df, vol_2016.1_2_2.df, vol_2016.2_2_2.df, vol_2016.3_2_2.df)
data_2018.2.df_rat <- cbind(ratings_2018, data_2018.2.df, vol_2018.1_2_2.df)
data_2019.2.df_rat <- cbind(ratings_2019, data_2019.2.df, vol_2019.1_2_2.df)



#fix(data_2016.2.df_rat)


##fix(data_2019.2.df_rat)


plot_missing(data_2000.2.df_rat)

# suppression_des_rows_with_not_data ---------------------------------

#je sais que j'ai 35 variables donc je construis une new database sans
#les rows (firmes) qui ont 35 données manquantes donc qui n'ont aucune donnée
#sur aucune variable

#la somme des NA de chaque row dans la database
rowSums(is.na(data_2000.2.df_rat))
rowSums(is.na(data_2007.2.df_rat))
rowSums(is.na(data_2008.2.df_rat))
rowSums(is.na(data_2009.2.df_rat))
rowSums(is.na(data_2016.2.df_rat))
rowSums(is.na(data_2018.2.df_rat))
rowSums(is.na(data_2019.2.df_rat))




colSums(is.na(data_2019.2.df_rat))

# on ne garder que les lignes dont la somme des NA est inférieur à 34 donc 33 et inf
dim(data_2000.2.df_rat)





# j'élimine que les lignes complètement vides 

# et après avoir éliminer les colonnes complètement vide je vais le refaire cette partie por éliminer les lignes qui ont plus de la moitié des cases vides




# si la somme des NA est inférieure à 36 on garde
# si la somme des NA est = à 36 ou supérieur à 36 on delete line # ce qui veut dire que si j'ai only one élément dans la ligne je ne vais pas delete la ligne car composé de 36 colonnes
data_2000.3.df <- data_2000.2.df_rat[rowSums(is.na(data_2000.2.df_rat))<86,]  
rowSums(is.na(data_2000.3.df))
dim(data_2000.3.df)

colSums(is.na(data_2000.3.df))

##fix(data_2000.3.df)


#dev.new() 
plot_missing(data_2000.2.df_rat) # fait le plot par colonne 
plot_missing(data_2000.3.df)




data_2007.3.df <- data_2007.2.df_rat[rowSums(is.na(data_2007.2.df_rat))<86,]
dim(data_2007.3.df)

data_2008.3.df <- data_2008.2.df_rat[rowSums(is.na(data_2008.2.df_rat))<86,]
dim(data_2008.3.df)

data_2009.3.df <- data_2009.2.df_rat[rowSums(is.na(data_2009.2.df_rat))<86,]
dim(data_2009.3.df)

data_2016.3.df <- data_2016.2.df_rat[rowSums(is.na(data_2016.2.df_rat))<111,]
dim(data_2016.3.df)

data_2018.3.df <- data_2018.2.df_rat[rowSums(is.na(data_2018.2.df_rat))<61,]
dim(data_2018.3.df)

data_2019.3.df <- data_2019.2.df_rat[rowSums(is.na(data_2019.2.df_rat))<61,]
dim(data_2019.3.df)


rowSums(is.na(data_2018.3.df))
rowSums(is.na(data_2000.3.df))



##fix(data_2000.3.df)
dim(data_2000.3.df)




ncol(data_2000.3.df)

dim(variables_names)



# just pour le plot quand on pput name pour que ça marche on plot " data_2019.3.df_2 "

data_2000.3.df_2 <- data_2000.3.df
data_2007.3.df_2 <- data_2007.3.df
data_2008.3.df_2 <- data_2008.3.df
data_2009.3.df_2 <- data_2009.3.df
data_2016.3.df_2 <- data_2016.3.df
data_2018.3.df_2 <- data_2018.3.df
data_2019.3.df_2 <- data_2019.3.df







# ON SUATE

#ajout nom variables
# pour 1 va marcher car not same name

# pour 2 et 3
# 2009


#vol_variables_names_1 <- vol_variables_names
#vol_variables_names_2 <- vol_variables_names

#colnames(vol_variables_names_1) <- paste("name_1", colnames(vol_variables_names_1), sep = "_")
#vol_variables_names_1

#colnames(vol_variables_names_2) <- paste("#_2", colnames( vol_variables_names_2), sep = "_")
#vol_variables_names_2
#(vol_variables_names)









#ajout rating dans la liste

#variables_names_2_pr_1 <- c("RATINGS", variables_names)
#variables_names_2_pr_2 <- c("RATINGS", variables_names)
#variables_names_2_pr_3 <- c("RATINGS", variables_names)



#View(variables_names_2_pr_2)



variables_name_2000 <- c("RATINGS", variables_names, colnames(vol_2000.1_2_2.df), colnames(vol_2000.2_2_2.df))
colnames(data_2000.3.df_2) <- variables_name_2000
colnames(data_2000.3.df_2)

 


variables_name_2007 <- c("RATINGS", variables_names, colnames(vol_2007.1_2_2.df), colnames(vol_2007.2_2_2.df))
colnames(data_2007.3.df_2) <- variables_name_2007
colnames(data_2007.3.df_2)



variables_name_2008 <- c("RATINGS", variables_names, colnames(vol_2008.1_2_2.df), colnames(vol_2008.2_2_2.df))
colnames(data_2008.3.df_2) <- variables_name_2008
colnames(data_2008.3.df_2)


variables_name_2009 <- c("RATINGS", variables_names, colnames(vol_2009.1_2_2.df), colnames(vol_2009.2_2_2.df))
colnames(data_2009.3.df_2) <- variables_name_2009
colnames(data_2009.3.df_2)



variables_name_2016 <- c("RATINGS", variables_names, colnames(vol_2016.1_2_2.df), colnames(vol_2016.2_2_2.df), colnames(vol_2016.3_2_2.df))
colnames(data_2016.3.df_2) <- variables_name_2016
colnames(data_2016.3.df_2)


variables_name_2018 <- c("RATINGS", variables_names, colnames(vol_2018.1_2_2.df))
colnames(data_2018.3.df_2) <- variables_name_2018
colnames(data_2018.3.df_2)


variables_name_2019 <- c("RATINGS", variables_names, colnames(vol_2019.1_2_2.df))
colnames(data_2019.3.df_2) <- variables_name_2019
colnames(data_2019.3.df_2)





















# suppression_des_colonnes_avec_plus_de_228_NA_values ---------------------


#je delete 1 by 1 les colonnes que je veux [le risk c'est que j'en laisse certain et si data base large not possible]
#data_2019.3.df <- data_2019.2.df[,c(-8,-21, -24, -25, -27, -28, -31, -32, -33, -34)]

#SOLUTION

#(1)calculer le nombre de NA par column pour avoir une visibiilité 

# pour see plot_missing le data_2019.3.df_2 donc le _2 car the same 
# data_2000.3.df_2 = data_2000.3.df (avant qu'on ajoute noms col)




# avec this way to do la première mesure de vol que j'ai est la vol20D et 72% NA donc beaucoup de données manquantes
# SOLUTION : take vol 30 jours donc refait sur new doc


#dev.new()
plot_missing(data_2000.3.df_2[c(-52, -77)]) # pour see le nb de cellule vide par colonne
colSums(is.na(data_2000.3.df_2))  # pour voir le nombre
dim(data_2000.3.df) # 256 -> 100 donc 65% (car sinon rating saute) -> 166 : car je vais delete all colonne avec plus de 60% NA
# donc c'est 153 que je vais mettre dans next step ou l'on ne tient pas compte des colonne avec plus de
# 153 celulles vides


plot_missing(data_2007.3.df_2[c(-52, -77)])
colSums(is.na(data_2007.3.df_2))
dim(data_2007.3.df) # 351 -> 100 donc 60% -> 210


plot_missing(data_2008.3.df_2[c(-52, -77)])
colSums(is.na(data_2008.3.df_2))
dim(data_2008.3.df) # 355 -> 100 donc 60% -> 213

plot_missing(data_2009.3.df_2[c(-52, -77)])
colSums(is.na(data_2009.3.df_2))
dim(data_2009.3.df) # 363 -> 100 donc 60% -> 217

plot_missing(data_2016.3.df_2[c(-52, -77, -102)])
colSums(is.na(data_2016.3.df_2))
dim(data_2016.3.df) # 403 -> 100 donc 60% -> 241

plot_missing(data_2018.3.df_2[c(-52)])
colSums(is.na(data_2018.3.df_2))
dim(data_2018.3.df)# 406 -> 100 donc 60% -> 243

plot_missing(data_2019.3.df_2[c(-52)])
colSums(is.na(data_2019.3.df_2))
dim(data_2019.3.df)# 407 -> 100 donc 60% -> 243












# AVEC FICHIER EXCEL VOL_2 STOP HERE CAR NEW DATA SUR VOL CONTIENT BEAUCOUP DE NA













#^^^^^^^^^^^^Not need pour 2019 ça va sauter comme PROFIT MARGIN^^^^^^^^^^^^
#^^^^^^^^^^^^à delete si je veux^^^^^^^^^^^^
#_____Pour_see_/_à_after_mice_nbr_NA_227_____

#p_1 <- matrix(data_2019.3.df$TOT_MKT_VAL)




# est-ce que je ne peux pas faire la même chose "mice" pour un ratio de vol
# avec beaucoup de NA exple pour BEST_EBIT STDDEV
#ou garder le seul que j'ai applied beta ? 

#BEST_EBIT_STDDEV beaucoup de NA donc garder celui que j'ai et travailler avec

#check <- matrix(data_2019.3.df$BEST_EBIT_STDDEV)








# DELETE COLONNE WITH LOT OF NA -------------------------------------------


#(2) constituer une new database qui ne tient compte que des colonnes
#avec moins de 228 NA values (pour savoir le nombre : étape 1)

#calcul la somme des NA sur les colonnes et les colonne dont la somme
#des NA est supérieure à 228 ne les prends pas en compte
#x <- y[, colSums(is.na(y))<220]


#je vais me baser pour chaque data base en fonction du nombre de Ratings


# changé car ajout vol

# se base sur .3.df car .3.df_2 etait juste pour plot car can't afeter ajout name col

# le nombre 153 représente 60% de la base de données 2000. Voir step en haut


# avec vol --> 60% change mais ici maintient  même old 60% et éléimine all new 
# donc si not good, take vol_30days et le multiplier par 12

dim(data_2000.3.df_2)
data_2000.4.df_x <- data_2000.3.df_2[ ,colSums(is.na(data_2000.3.df_2))<166] # Take all colonnes ou la somme du nombre de NA est inférieur à 153. donc si sur une colonne on fait la somme des NA et que c'est 154 ou plus on ne tient pas compte de cette colonne
dim(data_2000.4.df_x)

plot_missing(data_2000.4.df_x)
##fix(data_2000.4.df_x)

test_data_2000.3.df_2 <- data_2000.2.df_rat[rowSums(is.na(data_2000.2.df_rat))<20,]  
##fix(test_data_2000.3.df_2)

# COMPTER nombre de celulle qui ne sont pas des NA dans ratings_2000
nb_not_NA <- sum(as.numeric(!is.na(test_data_2000.3.df_2$ratings_2000))) # j'en ai que 58



plot_missing(data_2000.4.df_x)








# le nombre 210 représente 60% de la base de données 2007. Voir step en haut
data_2007.4.df_x <- data_2007.3.df_2[ ,colSums(is.na(data_2007.3.df_2))<210]
dim(data_2007.4.df_x)
##fix(data_2007.4.df_x)
plot_missing(data_2007.4.df_x)

# le nombre 213 représente 60% de la base de données 2008. Voir step en haut
data_2008.4.df_x <- data_2008.3.df_2[ ,colSums(is.na(data_2008.3.df_2))<213]
dim(data_2008.4.df_x)
##fix(data_2008.4.df_x)
plot_missing(data_2008.4.df_x)

# le nombre 217 représente 60% de la base de données 2009. Voir step en haut
data_2009.4.df_x <- data_2009.3.df_2[ ,colSums(is.na(data_2009.3.df_2))<217]
dim(data_2009.4.df_x)
##fix(data_2009.4.df_x)
plot_missing(data_2009.4.df_x)

# le nombre 241 représente 60% de la base de données 2016. Voir step en haut
data_2016.4.df_x <- data_2016.3.df_2[ ,colSums(is.na(data_2016.3.df_2))<241]
dim(data_2016.4.df_x)
##fix(data_2016.4.df_x)
plot_missing(data_2016.4.df_x)

# le nombre 243 représente 60% de la base de données 2018. Voir step en haut
data_2018.4.df_x <- data_2018.3.df_2[ ,colSums(is.na(data_2018.3.df_2))<243]
dim(data_2018.4.df_x)
##fix(data_2018.4.df_x)
plot_missing(data_2018.4.df_x)

# le nombre 243 représente 60% de la base de données 2019. Voir step en haut
data_2019.4.df_x <- data_2019.3.df_2[ ,colSums(is.na(data_2019.3.df_2))<243]
dim(data_2019.4.df_x) # 2018 il me reste 25 variables
##fix(data_2019.4.df_x)
plot_missing(data_2019.4.df_x)
# 2019 il me reste 23 variables


















# comme on connait nombre de colonne qui restent après avoir delete ceux qui n'ont pas de data



# Refaire le rowSums pour éléiminer les lignes avec plus de la moitié vide par data base
# si la somme des NA est inférieure à 36 on garde
# si la somme des NA est = à 36 ou supérieur à 36 on delete line # ce qui veut dire que si j'ai only one élément dans la ligne je ne vais pas delete la ligne car composé de 36 colonnes

# donc je garde si 50% des cases par lignes est filled in
dim(data_2000.4.df_x) # nrow = 19 donc 19/2 = 10 donc si une ligne à plus de 10 cases vides, je le delete
data_2000.4.df <- data_2000.4.df_x[rowSums(is.na(data_2000.4.df_x))<10,]  
rowSums(is.na(data_2000.4.df))
dim(data_2000.4.df)




dim(data_2007.4.df_x) # nrow = 20 donc 19/2 = 10 donc si une ligne à plus de 10 cases vides, je le delete
data_2007.4.df <- data_2007.4.df_x[rowSums(is.na(data_2007.4.df_x))<10,]
dim(data_2007.4.df)



dim(data_2008.4.df_x) # nrow = 20 donc 19/2 = 10 donc si une ligne à plus de 10 cases vides, je le delete
data_2008.4.df <- data_2008.4.df_x[rowSums(is.na(data_2008.4.df_x))<10,]
dim(data_2008.4.df)


dim(data_2009.4.df_x)# nrow = 21 donc 19/2 = 11 donc si une ligne à plus de 11 cases vides, je le delete
data_2009.4.df <- data_2009.4.df_x[rowSums(is.na(data_2009.4.df_x))<11,]
dim(data_2009.4.df)


dim(data_2016.4.df_x) # nrow = 21 donc 19/2 = 10 donc si une ligne à plus de 10 cases vides, je le delete
data_2016.4.df <- data_2016.4.df_x[rowSums(is.na(data_2016.4.df_x))<10,]
dim(data_2016.4.df)



dim(data_2018.4.df_x) # nrow = 24 donc 19/2 = 12 donc si une ligne à plus de 12 cases vides, je le delete
data_2018.4.df <- data_2018.4.df_x[rowSums(is.na(data_2018.4.df_x))<12,]
dim(data_2018.4.df)



dim(data_2019.4.df_x) # nrow = 23 donc 19/2 = 12 donc si une ligne à plus de 12 cases vides, je le delete
data_2019.4.df <- data_2019.4.df_x[rowSums(is.na(data_2019.4.df_x))<12,]
dim(data_2019.4.df)














x1 <- colnames(data_2000.4.df)
x2 <- colnames(data_2007.4.df)
x3 <- colnames(data_2008.4.df)
x4 <- colnames(data_2009.4.df)
x5 <- colnames(data_2016.4.df)
x6 <- colnames(data_2018.4.df)
x7 <- colnames(data_2019.4.df)


x7
#






# name qu'on retrouve dans les 2
intersect(x1, x2) # éléments qui se trouve à la fois dans les 2
intersect(x2,x1)

unique(c(x1,x2))

union(x1,x2)

setdiff(x1,x2) # 5 éléments qui se trouve dans x1 que l'on ne retrouvera pas dans x1

setdiff(x2,x1) # tous les éléments qu'on trouve dans x2 se trouve aussi dans x1











# Delete_duplicate_ou_doublons_en_FR --------------------------------------

#ça me laisse seulement le 1er des éléments dupliqués
data_2000.5.df <- data_2000.4.df[!duplicated(data_2000.4.df[c("EBITDA_MARGIN", "EBITDA_TO_REVENUE")]),]
dim(data_2000.5.df)


data_2007.5.df <- data_2007.4.df[!duplicated(data_2007.4.df[c("EBITDA_MARGIN", "EBITDA_TO_REVENUE")]),]
dim(data_2007.5.df)

data_2008.5.df <- data_2008.4.df[!duplicated(data_2008.4.df[c("EBITDA_MARGIN", "EBITDA_TO_REVENUE")]),]
dim(data_2008.5.df)

data_2009.5.df <- data_2009.4.df[!duplicated(data_2009.4.df[c("EBITDA_MARGIN", "EBITDA_TO_REVENUE")]),]
dim(data_2009.5.df)

data_2016.5.df <- data_2016.4.df[!duplicated(data_2016.4.df[c("EBITDA_MARGIN", "EBITDA_TO_REVENUE")]),]
dim(data_2016.5.df)

data_2018.5.df <- data_2018.4.df[!duplicated(data_2018.4.df[c("EBITDA_MARGIN", "EBITDA_TO_REVENUE")]),]
dim(data_2018.5.df)

data_2019.5.df <- data_2019.4.df[!duplicated(data_2019.4.df[c("EBITDA_MARGIN", "EBITDA_TO_REVENUE")]),]
dim(data_2019.5.df)







#calculer le nombre de rating non vide dans ".5.df"


# 2000 on a 38 ratings non vides sur 105  ==> donc 67 vides
dim(data_2000.5.df)
sum(as.numeric(!is.na(data_2000.5.df$RATINGS)))
##fix(data_2000.5.df)
plot_missing(data_2000.5.df)


# 2007 on a 91 ratings non vides sur 149 ==> donc 58 vides
dim(data_2007.5.df)
sum(as.numeric(!is.na(data_2007.5.df$RATINGS)))
##fix(data_2007.5.df)
plot_missing(data_2007.5.df)


# 2008 on a 99 rating non vides sur 160 ==> donc 61 vides
dim(data_2008.5.df)
sum(as.numeric(!is.na(data_2008.5.df$RATINGS)))
##fix(data_2008.5.df)
plot_missing(data_2008.5.df)


# 2009 on a 103 ratings non vides sur 164 ==> donc 61 vides
dim(data_2009.5.df)
sum(as.numeric(!is.na(data_2009.5.df$RATINGS)))
##fix(data_2009.5.df)
plot_missing(data_2009.5.df)


# 2016 on a 151 ratings non vides sur 163 ==> donc 12 vides
dim(data_2016.5.df)
sum(as.numeric(!is.na(data_2016.5.df$RATINGS)))
##fix(data_2016.5.df)
plot_missing(data_2016.5.df)


# 2018 on a 156 ratings non vides sur 161 ==> donc 5 vides
dim(data_2018.5.df)
sum(as.numeric(!is.na(data_2018.5.df$RATINGS)))
##fix(data_2018.5.df)
plot_missing(data_2018.5.df)



# 2019 on a 152 ratings non vides sur 156 ==> donc 4 vides
dim(data_2019.5.df)
sum(as.numeric(!is.na(data_2019.5.df$RATINGS)))
##fix(data_2019.5.df)
plot_missing(data_2019.5.df)




any(is.na(data_2019.5.df))
colSums(is.na(data_2019.5.df))
rowSums(is.na(data_2019.5.df))








# enlevons pour 2007, 2008, 2009, 2016, 2018 & 2019 les lignes dont ratings vides


# 2000
dim(data_2000.5.df)

data_2000.5.df_2 <- data_2000.5.df
##fix(data_2000.5.df_2)

# 2007
dim(data_2007.5.df)

data_2007.5.df_2 <- data_2007.5.df[!is.na(data_2007.5.df$RATINGS),]
dim(data_2007.5.df_2)
##fix(data_2007.5.df_2)

plot_missing(data_2007.5.df_2)

 

# 2008
dim(data_2008.5.df)

data_2008.5.df_2 <- data_2008.5.df[!is.na(data_2008.5.df$RATINGS),]
dim(data_2008.5.df_2)
##fix(data_2008.5.df_2)

plot_missing(data_2008.5.df_2)





# 2009
dim(data_2009.5.df)

data_2009.5.df_2 <- data_2009.5.df[!is.na(data_2009.5.df$RATINGS),]
dim(data_2009.5.df_2)
##fix(data_2009.5.df_2)

plot_missing(data_2009.5.df_2)





# 2016
dim(data_2016.5.df)

data_2016.5.df_2 <- data_2016.5.df[!is.na(data_2016.5.df$RATINGS),]
dim(data_2016.5.df_2)
##fix(data_2016.5.df_2)

plot_missing(data_2016.5.df_2)





# 2018
dim(data_2018.5.df)

data_2018.5.df_2 <- data_2018.5.df[!is.na(data_2018.5.df$RATINGS),]
dim(data_2018.5.df_2)
##fix(data_2018.5.df_2)

plot_missing(data_2018.5.df_2)





# 2019
dim(data_2019.5.df)

data_2019.5.df_2 <- data_2019.5.df[!is.na(data_2019.5.df$RATINGS),]
dim(data_2019.5.df_2)
##fix(data_2019.5.df_2)

plot_missing(data_2019.5.df_2)
















# impute_NA_value_avec_mice -----------------------------------------------

library(mice)

summary(data_2000.5.df_2)




# je unlist tout le monde


#voir le type de mes données
# 2000
data_2000.5.df_2 <- as.data.frame(lapply(data_2000.5.df_2, unlist))

summary(data_2000.5.df_2[,2:ncol(data_2000.5.df_2)])  #données sont des character donc changer en numérique

data_2000.5.df_2$RATINGS


# 2007

data_2007.5.df_2 <- as.data.frame(lapply(data_2007.5.df_2, unlist))
summary(data_2007.5.df_2[,2:ncol(data_2007.5.df_2)])

data_2007.5.df_2$RATINGS



# 2008
data_2008.5.df_2 <- as.data.frame(lapply(data_2008.5.df_2, unlist))
summary(data_2008.5.df_2[,2:ncol(data_2008.5.df_2)])

data_2008.5.df_2$RATINGS

# 2009
data_2009.5.df_2 <- as.data.frame(lapply(data_2009.5.df_2, unlist))
summary(data_2009.5.df_2[,2:ncol(data_2009.5.df_2)])

data_2009.5.df_2$RATINGS


# 2016
data_2016.5.df_2 <- as.data.frame(lapply(data_2016.5.df_2, unlist))
summary(data_2016.5.df_2[,2:ncol(data_2016.5.df_2)])


# 2018
data_2018.5.df_2 <- as.data.frame(lapply(data_2018.5.df_2, unlist))
summary(data_2018.5.df_2[,2:ncol(data_2018.5.df_2)])


# 2019
data_2019.5.df_2 <- as.data.frame(lapply(data_2019.5.df_2, unlist))
summary(data_2019.5.df_2[,2:ncol(data_2019.5.df_2)])







#changement type character en numeric

#source :  https://stackoverflow.com/questions/37707060/converting-data-frame-column-from-character-to-numeric/37707117
#pour convertir les colonnes 1 by 1 en numeric
#------------> yyz$b <- as.numeric(as.character(yyz$b))

# data_2019.5.df_2$EBITDA_MARGIN <- as.numeric(as.character(data_2019.5.df_2$EBITDA_MARGIN)) 

#pour onvertir toutes les colonnes en une seule fois en numeric
#------------> yyz[] <- lapply(yyz, function(x) as.numeric(as.character(x)))







data_2000.5.df_2[,-1] <- lapply(data_2000.5.df_2[,-1], function(x) as.numeric(as.character(x)))
summary(data_2000.5.df_2)
dim(data_2000.5.df_2)





data_2007.5.df_2[,-1] <- lapply(data_2007.5.df_2[,-1], function(x) as.numeric(as.character(x)))
summary(data_2007.5.df_2)
dim(data_2007.5.df_2)




data_2008.5.df_2[,-1] <- lapply(data_2008.5.df_2[,-1], function(x) as.numeric(as.character(x)))
summary(data_2008.5.df_2)
dim(data_2008.5.df_2)



data_2009.5.df_2[,-1] <- lapply(data_2009.5.df_2[,-1], function(x) as.numeric(as.character(x)))
summary(data_2009.5.df_2)
dim(data_2009.5.df_2)



data_2016.5.df_2[,-1] <- lapply(data_2016.5.df_2[,-1], function(x) as.numeric(as.character(x)))
summary(data_2016.5.df_2)
dim(data_2016.5.df_2)



data_2018.5.df_2[,-1] <- lapply(data_2018.5.df_2[,-1], function(x) as.numeric(as.character(x)))
summary(data_2018.5.df_2)
dim(data_2018.5.df_2)





data_2019.5.df_2[,-1] <- lapply(data_2019.5.df_2[,-1], function(x) as.numeric(as.character(x)))
summary(data_2019.5.df_2)
dim(data_2019.5.df_2)








# quand y a liste ça va poser problème pour mettre factor
# SOLUTION : 
# (1) mettre unlist toute la base de donnée
# (2) changer en numerique all à l'exception de variable qu'on veut mettre factor (ici rating)
# (3) changer factor la variable qui nous interesse

# convertir colonne RATINGS en factor
is.list(data_2000.5.df_2$RATINGS)



data_2000.5.df_2$RATINGS <- as.factor(data_2000.5.df_2$RATINGS)
data_2007.5.df_2$RATINGS <- as.factor(data_2007.5.df_2$RATINGS)
data_2008.5.df_2$RATINGS <- as.factor(data_2008.5.df_2$RATINGS)
data_2009.5.df_2$RATINGS <- as.factor(data_2009.5.df_2$RATINGS)
data_2016.5.df_2$RATINGS <- as.factor(data_2016.5.df_2$RATINGS)
data_2018.5.df_2$RATINGS <- as.factor(data_2018.5.df_2$RATINGS)
data_2019.5.df_2$RATINGS <- as.factor(data_2019.5.df_2$RATINGS)




levels(data_2000.5.df_2$RATINGS)
levels(data_2007.5.df_2$RATINGS)
levels(data_2008.5.df_2$RATINGS)
levels(data_2009.5.df_2$RATINGS)
levels(data_2016.5.df_2$RATINGS)
levels(data_2018.5.df_2$RATINGS)
levels(data_2019.5.df_2$RATINGS)



summary(data_2019.5.df_2)
dim(data_2019.5.df_2)


# y a t'il des NA sur EBITDA_MARGIN et ou se trouvent t'ils
any(is.na(data_2019.5.df_2$EBITDA_MARGIN))
which(is.na(data_2019.5.df_2$EBITDA_MARGIN))

# exple remplacer par la moyenne toutes les rows vide par exple pour EBITDA_MARGIN
#---> data_2019.5.df_2$EBITDA_MARGIN[which(is.na(data_2019.5.df_2$EBITDA_MARGIN))] <- mean(data_2019.5.df_2$EBITDA_MARGIN, na.rm = TRUE)

#---> which(is.na(data_2019.5.df_2$EBITDA_MARGIN))
#---> any(is.na(data_2019.5.df_2$EBITDA_MARGIN))



#continuons avec mice pour remplacer NA value : imputation

# pour savoir les différentes méthodes d'imputation qui exixste sur mice 
#---> methods(mice)


#créons un new dataset pour mice
data_2000.6.df <- data_2000.5.df_2
data_2007.6.df <- data_2007.5.df_2
data_2008.6.df <- data_2008.5.df_2
data_2009.6.df <- data_2009.5.df_2
data_2016.6.df <- data_2016.5.df_2
data_2018.6.df <- data_2018.5.df_2
data_2019.6.df <- data_2019.5.df_2




summary(data_2000.6.df)
summary(data_2007.6.df) # NO PROBLEM DE VALEUR ABBERANTE
summary(data_2008.6.df) # NO PROBLEM DE VALEUR ABBERANTE
summary(data_2009.6.df) 
summary(data_2016.6.df)
summary(data_2018.6.df)
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
sort(data_2000.6.df$EBIT_TO_NET_SALES)
sort(data_2007.6.df$EBIT_TO_NET_SALES)# NO PROBLEM DE VALEUR ABBERANTE
sort(data_2008.6.df$EBIT_TO_NET_SALES)# NO PROBLEM DE VALEUR ABBERANTE
sort(data_2009.6.df$EBIT_TO_NET_SALES)
sort(data_2016.6.df$EBIT_TO_NET_SALES)
sort(data_2018.6.df$EBIT_TO_NET_SALES)
sort(data_2019.6.df$EBIT_TO_NET_SALES)

# 2000
#identifions le d'abord
which.min(data_2000.6.df$EBITDA_MARGIN) # le donnée se trouve à la ligne 63
which.min(data_2000.6.df$EBIT_TO_NET_SALES) # le donnée se trouve à la ligne 63
which.min(data_2000.6.df$EBITDA_TO_REVENUE)
#supprimons la lignes
data_2000.6.df <- data_2000.6.df[-63,] # pour checher si c'est enlevé, lorsque je refais le which.min pour savoir ou se trouve le min c'est à a ligne 95
summary(data_2000.6.df)


# 2007 NO PROBLEM VALEUR ABBERANTE

# 2008 NO PROBLEM VALEUR ABBERANTE


# 2009
which.min(data_2009.6.df$EBITDA_MARGIN) # le donnée se trouve à la ligne 54
which.min(data_2009.6.df$EBIT_TO_NET_SALES) # le donnée se trouve à la ligne 54
which.min(data_2009.6.df$EBITDA_TO_REVENUE)
#supprimons la lignes
data_2009.6.df <- data_2009.6.df[-54,] # pour checher si c'est enlevé, lorsque je refais le which.min pour savoir ou se trouve le min c'est à a ligne 95



# 2016
which.min(data_2016.6.df$EBITDA_MARGIN) # le donnée se trouve à la ligne 13
which.min(data_2016.6.df$EBIT_TO_NET_SALES) # le donnée se trouve à la ligne 13
which.min(data_2016.6.df$EBITDA_TO_REVENUE)
#supprimons la lignes
data_2016.6.df <- data_2016.6.df[-13,] # pour checher si c'est enlevé, lorsque je refais le which.min pour savoir ou se trouve le min c'est à a ligne 95



# 2018
which.min(data_2018.6.df$EBITDA_MARGIN) # le donnée se trouve à la ligne 13
which.min(data_2018.6.df$EBIT_TO_NET_SALES) # le donnée se trouve à la ligne 13
which.min(data_2018.6.df$EBITDA_TO_REVENUE)
#supprimons la lignes
data_2018.6.df <- data_2018.6.df[-13,] # pour checher si c'est enlevé, lorsque je refais le which.min pour savoir ou se trouve le min c'est à a ligne 95


# 2019
which.min(data_2019.6.df$EBITDA_MARGIN) # le donnée se trouve à la ligne 13
which.min(data_2019.6.df$EBIT_TO_NET_SALES) # le donnée se trouve à la ligne 13
which.min(data_2019.6.df$EBITDA_TO_REVENUE)
#supprimons la lignes
data_2019.6.df <- data_2019.6.df[-13,] # pour checher si c'est enlevé, lorsque je refais le which.min pour savoir ou se trouve le min c'est à a ligne 95





#enlevons EBITDA_TO_MARGIN car identique à EBITDA_TO_REVENUE (ici c'est la colonne qu'on enlève)
data_2000.6.df <- data_2000.6.df[-3]
dim(data_2000.6.df)
dim(data_2000.5.df)
##View(data_2000.6.df)


data_2007.6.df <- data_2007.6.df[-3]
dim(data_2007.6.df)
dim(data_2007.5.df)
##View(data_2007.6.df)


data_2008.6.df <- data_2008.6.df[-3]
dim(data_2008.6.df)
dim(data_2008.5.df)
##View(data_2008.6.df)


data_2009.6.df <- data_2009.6.df[-3]
dim(data_2009.6.df)
dim(data_2009.5.df)
##View(data_2009.6.df)


data_2016.6.df <- data_2016.6.df[-3]
dim(data_2016.6.df)
dim(data_2016.5.df)
##View(data_2016.6.df)



data_2018.6.df <- data_2018.6.df[-3]
dim(data_2018.6.df)
dim(data_2018.5.df)
##View(data_2018.6.df)



data_2019.6.df <- data_2019.6.df[-2] # CAR NOT PTOFIT MARGIN COL DELETED ==> CHECK IT
dim(data_2019.6.df)
dim(data_2019.5.df)
##View(data_2019.6.df)










n1 <- colnames(data_2000.6.df)
n2 <- colnames(data_2007.6.df)
n3 <- colnames(data_2008.6.df)
n4 <- colnames(data_2009.6.df)
n5 <- colnames(data_2016.6.df)
n6 <- colnames(data_2018.6.df)
n7 <- colnames(data_2019.6.df)   # 2019 sur data not yet profit margin








summary(data_2019.6.df)
dim(data_2019.6.df)
##fix(data_2019.6.df)




#pour maxit, plus c'est grand plus la prédiction est good, 
# solution ne pas tenir compte de la colinéarité : remove.collinear = FALSE

#identifier la liste des colonnes sans NA value pour ne pas en tenir compte dans imputation de mice
which(colSums(is.na(data_2000.6.df))==0) # 2, 4, 7, 8, 9, 12, 18
which(colSums(is.na(data_2007.6.df))==0) 
which(colSums(is.na(data_2008.6.df))==0) 
which(colSums(is.na(data_2009.6.df))==0) 
which(colSums(is.na(data_2016.6.df))==0) 
which(colSums(is.na(data_2018.6.df))==0) 
which(colSums(is.na(data_2019.6.df))==0) 



#pour (2019) col 1,6, 8, 10, 13, 19


library(mice)
#mice quand ça ne marche pas 3 problèmes en générale que si tu les règles ça résoud le problème
# [1] le premier enlever les lignes qui qui on des valeur abérrrantes par exemple le min est de -33 200 et le suivant -40 dand tu fais le sort()
# [2] enlever les variables qui sont identiques sur les données mais only nom de variable qui diffèrent par exemple (EBITDA_TO_REVENUE & EBITDA_TO_MARGIN) et j'ai enlevé 1
# [3] lorsqu'on fait le mice ne pas tenir compte des colonnes sans NA 
# après ça quand je fais mice(), ça marche sans loggedEvents



# 2000  --> check log event pour all scenarios

#data_imputation <- mice(data_2000.6.df[-1], m=5, method = "pmm", maxit = 5, remove.collinear = TRUE)

#which(colSums(is.na(data_2000.6.df))==0) 
#colSums(is.na(data_2000.6.df))
#data_imputation_3 <- mice(data_2000.6.df[,c(1,19)], m=5, method = "pmm", maxit = 5, remove.collinear = TRUE)
#dim(data_2000.6.df)

data_imputation_2000 <- mice(data_2000.6.df, m=5, method = "pmm", maxit = 5, remove.collinear = TRUE)
data_imputation_2007 <- mice(data_2007.6.df, m=5, method = "pmm", maxit = 5, remove.collinear = TRUE)
data_imputation_2008 <- mice(data_2008.6.df, m=5, method = "pmm", maxit = 5, remove.collinear = TRUE)
data_imputation_2009 <- mice(data_2009.6.df, m=5, method = "pmm", maxit = 5, remove.collinear = TRUE)
data_imputation_2016 <- mice(data_2016.6.df, m=5, method = "pmm", maxit = 5, remove.collinear = TRUE)
data_imputation_2018 <- mice(data_2018.6.df, m=5, method = "pmm", maxit = 5, remove.collinear = TRUE)
data_imputation_2019 <- mice(data_2019.6.df, m=5, method = "pmm", maxit = 5, remove.collinear = TRUE)


plot_missing(data_2018.6.df)








#pas tenir compte (en bas)

#si j'applique le mice du haut, il y aura toujours un loggedEvents car y a beaucoup de colonnes qui n'ont pas de NA donc mieux vaut ne pas en tenir compte dans le mice


#avant de passer faisons l'xtraction de colonnes car après need them lors réintégration
#à l'exception du 1 qui est = à ratings


#var_not_ds_mice <- data_2000.6.df[,c(2, 4, 7, 8, 9, 12, 18)]
#col_names_var_not_ds_mice <- colnames(var_not_ds_mice)


#je ne tiens pas compte des colonnes sans NA #on les enleve de l'imputation



#s'il y en a
#pour voir le nombre de loggedEven c--à-d les variables qui n'ont pas été traité par mice
data_imputation_2000$loggedEvents #ici c'est EBIT_TO_NET_SALES donc on le tient pas compte de l'imutation 
#et comme il n'a que 3 NA on va le remplacer par sa moyenne
#mais après avoir relancer depuis data_2019.5.df on voir qu'il n'y en a pas. ça apparaisait toujours car je le lancer à partir de la dernière base de données ou j'avais fait des modifications sans l'actualiser
data_imputation_2007$loggedEvents
data_imputation_2008$loggedEvents
data_imputation_2009$loggedEvents
data_imputation_2016$loggedEvents
data_imputation_2018$loggedEvents
data_imputation_2019$loggedEvents






#apres le run, data_imputation n'est pas un database mais ce qui a permis de faire imputation
summary(data_2000.6.df$RETURN_COM_EQY)

data_imputation_2000$imp$EBITDA_MARGIN



final_clean_dataset_2000 <- complete(data_imputation_2000, 5)
final_clean_dataset_2007 <- complete(data_imputation_2007, 5)
final_clean_dataset_2008 <- complete(data_imputation_2008, 5)
final_clean_dataset_2009 <- complete(data_imputation_2009, 5)
final_clean_dataset_2016 <- complete(data_imputation_2016, 5)
final_clean_dataset_2018 <- complete(data_imputation_2018, 5)
final_clean_dataset_2019 <- complete(data_imputation_2019, 5)



##View(final_clean_dataset_2016)
plot_missing(data_2019.6.df)
plot_missing(final_clean_dataset_2019)


any(is.na(final_clean_dataset_2000))
any(is.na(final_clean_dataset_2007))
any(is.na(final_clean_dataset_2008))
any(is.na(final_clean_dataset_2009))
any(is.na(final_clean_dataset_2016))
any(is.na(final_clean_dataset_2018))
any(is.na(final_clean_dataset_2019))







colSums(final_clean_dataset_2000[-1]) #qd y a NA colSums return NA pour colonne avec NA
colSums(is.na(final_clean_dataset_2000))

dim(final_clean_dataset_2000)
plot_missing(final_clean_dataset_2000)







#il en restre toujours 2 not traité avec la colinéarité donc je le résoud en
#ls remplaçant par la moyenne

#identifions les
plot_missing(final_clean_dataset_2016)
plot_missing(final_clean_dataset_2018)
plot_missing(final_clean_dataset_2019)

summary(final_clean_dataset_2016) 
data_imputation_2016$loggedEvents



#not need comparé à 2018 car ici le mice(à tout réglé) donc not need de remplacer ces 2 par la moyenne

final_clean_dataset_2016$EBIT_TO_NET_SALES[which(is.na(final_clean_dataset_2016$EBIT_TO_NET_SALES))] <- mean(final_clean_dataset_2016$EBIT_TO_NET_SALES, na.rm = TRUE)
final_clean_dataset_2018$EBIT_TO_NET_SALES[which(is.na(final_clean_dataset_2018$EBIT_TO_NET_SALES))] <- mean(final_clean_dataset_2018$EBIT_TO_NET_SALES, na.rm = TRUE)
final_clean_dataset_2019$EBIT_TO_NET_SALES[which(is.na(final_clean_dataset_2019$EBIT_TO_NET_SALES))] <- mean(final_clean_dataset_2019$EBIT_TO_NET_SALES, na.rm = TRUE)




any(is.na(final_clean_dataset_2000))
any(is.na(final_clean_dataset_2007))
any(is.na(final_clean_dataset_2008))
any(is.na(final_clean_dataset_2009))
any(is.na(final_clean_dataset_2016))
any(is.na(final_clean_dataset_2018))
any(is.na(final_clean_dataset_2019))





dim(data_2019.5.df)      #23 variables
dim(data_2019.6.df)      #22 variables : on a enleve EBITDA_TO_MARGIN car identique à EBITDA_TO_REVENUE
dim(final_clean_dataset_2019)







#ajoutons les colonnes qui n'avaient pas de NA value dans data_2019.6.df dans final_clean_dataset sans ajouter le 1er qui correspond au ratings car on va l'ajouter plus tard

#final_clean_dataset.2 <- cbind(final_clean_dataset, data_2019.6.df[,c(6, 8, 10, 13, 19)])















# NOT RUN  (UNTIL NOT RUN EN BAS)


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







# NOT RUN (FROM NOT RUN EN HAUT)
































# convertir_les_ratings_en_valeur_numérique -------------------------------


#ratings = Variable catégorielle ordonnée

#j'ai 19 nivveaux donc 19 éléments différents et chacun je l'ai 1 ou plusieurs fois
my_RATINGS_2000 <- final_clean_dataset_2000$RATINGS
my_RATINGS_2000 <- matrix(my_RATINGS_2000)
dim(my_RATINGS_2000)



my_RATINGS_2007 <- final_clean_dataset_2007$RATINGS
my_RATINGS_2007 <- matrix(my_RATINGS_2007)
dim(my_RATINGS_2007)


my_RATINGS_2008 <- final_clean_dataset_2008$RATINGS
my_RATINGS_2008 <- matrix(my_RATINGS_2008)
dim(my_RATINGS_2008)


my_RATINGS_2009 <- final_clean_dataset_2009$RATINGS
my_RATINGS_2009 <- matrix(my_RATINGS_2009)
dim(my_RATINGS_2009)


my_RATINGS_2016 <- final_clean_dataset_2016$RATINGS
my_RATINGS_2016 <- matrix(my_RATINGS_2016)
dim(my_RATINGS_2016)


my_RATINGS_2018 <- final_clean_dataset_2018$RATINGS
my_RATINGS_2018 <- matrix(my_RATINGS_2018)
dim(my_RATINGS_2018)


my_RATINGS_2019 <- final_clean_dataset_2019$RATINGS
my_RATINGS_2019 <- matrix(my_RATINGS_2019)
dim(my_RATINGS_2019)



#juste pour checher mes level donc create new variable
rate_check_2000 <- factor(my_RATINGS_2000)
levels(rate_check_2000)

rate_check_2007 <- factor(my_RATINGS_2007)
levels(rate_check_2007)

rate_check_2008 <- factor(my_RATINGS_2008)
levels(rate_check_2008)

rate_check_2009 <- factor(my_RATINGS_2009)
levels(rate_check_2009)

rate_check_2016 <- factor(my_RATINGS_2016)
levels(rate_check_2016)

rate_check_2018 <- factor(my_RATINGS_2018)
levels(rate_check_2018)

rate_check_2019 <- factor(my_RATINGS_2019)
levels(rate_check_2019)


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



my_rating_grd_grp_2000 <- grd_grp_rat[my_RATINGS_2000]
my_rating_grd_grp_2007 <- grd_grp_rat[my_RATINGS_2007]
my_rating_grd_grp_2008 <- grd_grp_rat[my_RATINGS_2008]
my_rating_grd_grp_2009 <- grd_grp_rat[my_RATINGS_2009]
my_rating_grd_grp_2016 <- grd_grp_rat[my_RATINGS_2016]
my_rating_grd_grp_2018 <- grd_grp_rat[my_RATINGS_2018]
my_rating_grd_grp_2019 <- grd_grp_rat[my_RATINGS_2019]




#pour voir si conversion bien fait / a my_RATINGS
see_2000 <- matrix(my_rating_grd_grp_2000)
dim(see_2000)

##fix(see_2000)

see_2007 <- matrix(my_rating_grd_grp_2007)
see_2008 <- matrix(my_rating_grd_grp_2008)
see_2009 <- matrix(my_rating_grd_grp_2009)
see_2016 <- matrix(my_rating_grd_grp_2016)
see_2018 <- matrix(my_rating_grd_grp_2018)
see_2019 <- matrix(my_rating_grd_grp_2019)





# (1)convertir les rating en chiffres (en tenant compte investement et speculative grade)
#conversion des grand groupe en valeur numerique 1, 2, 3, 4, 5, 6, 7


grd_grp_num <- c("AAA"=1, "AA"=2,"A"=3,
                 "BBB"=4,"BB"=5,"B"=6, "CCC"=7)

my_rating_grd_num_2000 <- grd_grp_num[my_rating_grd_grp_2000]
my_rating_grd_num_2007 <- grd_grp_num[my_rating_grd_grp_2007]
my_rating_grd_num_2008 <- grd_grp_num[my_rating_grd_grp_2008]
my_rating_grd_num_2009 <- grd_grp_num[my_rating_grd_grp_2009]
my_rating_grd_num_2016 <- grd_grp_num[my_rating_grd_grp_2016]
my_rating_grd_num_2018 <- grd_grp_num[my_rating_grd_grp_2018]
my_rating_grd_num_2019 <- grd_grp_num[my_rating_grd_grp_2019]




as.factor(my_rating_grd_grp_2000)

#pour pouvoir comparer si good après conversion
see_2_2000 <- matrix(my_rating_grd_num_2000)
dim(see_2_2000)


see_2_2007 <- matrix(my_rating_grd_num_2007)
see_2_2008 <- matrix(my_rating_grd_num_2008)
see_2_2009 <- matrix(my_rating_grd_num_2009)
see_2_2016 <- matrix(my_rating_grd_num_2016)
see_2_2018 <- matrix(my_rating_grd_num_2018)
see_2_2019 <- matrix(my_rating_grd_num_2019)


#juste pour faire good plot
#on met factor en spécifiant l'ordre des levels (le niveau de chaque factor)
see_2000 <- factor(see_2000, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
a_2000 <- cbind(see_2_2000, see_2000,final_clean_dataset_2000) # car j'ai besoin que ça soit une base de données pour pouvoir le manipuler dans dplyr et dans ggplot

see_2007 <- factor(see_2007, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
a_2007 <- cbind(see_2_2007, see_2007,final_clean_dataset_2007)

see_2008 <- factor(see_2008, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
a_2008 <- cbind(see_2_2008, see_2008,final_clean_dataset_2008)

see_2009 <- factor(see_2009, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
a_2009 <- cbind(see_2_2009, see_2009,final_clean_dataset_2009)

see_2016 <- factor(see_2016, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
a_2016 <- cbind(see_2_2016, see_2016,final_clean_dataset_2016)

see_2018 <- factor(see_2018, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
a_2018 <- cbind(see_2_2018, see_2018,final_clean_dataset_2018)


see_2019 <- factor(see_2019, levels = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"))
a_2019 <- cbind(see_2_2019, see_2019,final_clean_dataset_2019)



# not what I want to do
plot_bar(see_2000)
plot_bar(my_rating_grd_grp_2000)



#pour tester quelque chose
# a not good nom en français
#write.csv(a, file = "ma_base_de_donnees_2019_rating_char.csv") # si on oublie le .csv le dossier ne sera pas dans le wd






# ou DataExplorer



library(dplyr)
count_data_2000 <- a_2000 %>% 
  count(see_2000)



count_data_2007 <- a_2007 %>% 
  count(see_2007)


count_data_2008 <- a_2008 %>% 
  count(see_2008)



count_data_2009 <- a_2009 %>% 
  count(see_2009)


count_data_2016 <- a_2016 %>% 
  count(see_2016)



count_data_2018 <- a_2018 %>% 
  count(see_2018)



count_data_2019 <- a_2019 %>% 
  count(see_2019)



#changer ordre bin
library(ggplot2)


#dev.new() # permet de sortir le plot zoom de son cadre et si j'en écrit envore ça m'en sort un autre
ggplot(count_data_2000, aes(x=see_2000, y=n))+
  geom_bar(stat = "identity", fill="gray16")+
  geom_text(aes(label=n), vjust=-0.500)+
  labs(x="Cote de crédit 2000", u="Nombre d'observation",
       title = "")





ggplot(count_data_2007, aes(x=see_2007, y=n))+
  geom_bar(stat = "identity", fill="gray16")+
  geom_text(aes(label=n), vjust=-0.500)+
  labs(x="Cote de crédit 2007", u="Nombre d'observation",
       title = "")



ggplot(count_data_2008, aes(x=see_2008, y=n))+
  geom_bar(stat = "identity", fill="gray16")+
  geom_text(aes(label=n), vjust=-0.500)+
  labs(x="Cote de crédit 2008", u="Nombre d'observation",
       title = "")


ggplot(count_data_2009, aes(x=see_2009, y=n))+
  geom_bar(stat = "identity", fill="gray16")+
  geom_text(aes(label=n), vjust=-0.500)+
  labs(x="Cote de crédit 2009", u="Nombre d'observation",
       title = "")


ggplot(count_data_2016, aes(x=see_2016, y=n))+
  geom_bar(stat = "identity", fill="gray16")+
  geom_text(aes(label=n), vjust=-0.500)+
  labs(x="Cote de crédit 2016", u="Nombre d'observation",
       title = "")



ggplot(count_data_2018, aes(x=see_2018, y=n))+
  geom_bar(stat = "identity", fill="gray16")+
  geom_text(aes(label=n), vjust=-0.500)+
  labs(x="Cote de crédit 2018", u="Nombre d'observation",
       title = "")




ggplot(count_data_2019, aes(x=see_2019, y=n))+
  geom_bar(stat = "identity", fill="gray16")+
  geom_text(aes(label=n), vjust=-0.500)+
  labs(x="Cote de crédit 2019", u="Nombre d'observation",
       title = "")










# cbind()_ratings_numérik_et_final_data ------------------------------------

data_2000_avt_good_ratio <-cbind(my_rating_grd_num_2000, my_rating_grd_grp_2000,final_clean_dataset_2000)
my_rating_grd_num_2000 #ça le donne le level de chaque chiffre =rating car je les ai défini plus haut
dim(final_clean_dataset_2000)
dim(data_2000_avt_good_ratio)


data_2007_avt_good_ratio <-cbind(my_rating_grd_num_2007, my_rating_grd_grp_2007,final_clean_dataset_2007)
data_2008_avt_good_ratio <-cbind(my_rating_grd_num_2008, my_rating_grd_grp_2008,final_clean_dataset_2008)
data_2009_avt_good_ratio <-cbind(my_rating_grd_num_2009, my_rating_grd_grp_2009,final_clean_dataset_2009)
data_2016_avt_good_ratio <-cbind(my_rating_grd_num_2016, my_rating_grd_grp_2016,final_clean_dataset_2016)
data_2018_avt_good_ratio <-cbind(my_rating_grd_num_2018, my_rating_grd_grp_2018,final_clean_dataset_2018)
data_2019_avt_good_ratio <-cbind(my_rating_grd_num_2019, my_rating_grd_grp_2019,final_clean_dataset_2019)



colnames(data_2000_avt_good_ratio)

is.element("WORKING_CAPITAL" , colnames(data_2000_avt_good_ratio))
is.element("TOT_MKT_VAL" , colnames(data_2000_avt_good_ratio))


is.element("WORKING_CAPITAL" , colnames(data_2007_avt_good_ratio))
is.element("TOT_MKT_VAL" , colnames(data_2007_avt_good_ratio))


is.element("WORKING_CAPITAL" , colnames(data_2008_avt_good_ratio))
is.element("TOT_MKT_VAL" , colnames(data_2008_avt_good_ratio))


is.element("WORKING_CAPITAL" , colnames(data_2009_avt_good_ratio))
is.element("TOT_MKT_VAL" , colnames(data_2009_avt_good_ratio))


is.element("WORKING_CAPITAL" , colnames(data_2016_avt_good_ratio))
is.element("TOT_MKT_VAL" , colnames(data_2016_avt_good_ratio))


is.element("WORKING_CAPITAL" , colnames(data_2018_avt_good_ratio))
is.element("TOT_MKT_VAL" , colnames(data_2018_avt_good_ratio))


is.element("WORKING_CAPITAL" , colnames(data_2019_avt_good_ratio))
is.element("TOT_MKT_VAL" , colnames(data_2019_avt_good_ratio))





# constituer good ratio ---------------------------------------------------

#je peux use tidyverse plutot commode
library(dplyr)  # database : data_2000_avt_good_ratio

# 2000
data_2000_with_good_ratio <- data_2000_avt_good_ratio%>%
  mutate(ratio_Flux_de_TR_expl_sur_passif_cour = CF_CASH_FROM_OPER/BS_CUR_LIAB,
         ratio_Fonds_de_roulement_sur_tot_actif = WORKING_CAPITAL/BS_TOT_ASSET)

#ratio_val_marchd_tot_sur_tot_actif =TOT_MKT_VAL/BS_TOT_ASSET) is deleted car TOT_MKT8VAL pas dans new database
# ratio_tot_liab_sur_tot_actif=BS_TOTAL_LIABILITIES/BS_TOT_ASSET, (idem)
#ratio_B_non_rep_sur_Tot_actif = BS_PURE_RETAINED_EARNINGS/BS_TOT_ASSET,

is.element("TOT_MKT_VAL", colnames(data_2000_avt_good_ratio))
is.element("BS_TOTAL_LIABILITIES", colnames(data_2000_avt_good_ratio))
is.element("BS_PURE_RETAINED_EARNINGS", colnames(data_2000_avt_good_ratio))

colnames(data_2000_with_good_ratio)

dim(data_2000_avt_good_ratio)
dim(data_2000_with_good_ratio)

length(colnames(data_2000_with_good_ratio))



# 2007
data_2007_with_good_ratio <- data_2007_avt_good_ratio%>%
  mutate(ratio_B_non_rep_sur_Tot_actif = BS_PURE_RETAINED_EARNINGS/BS_TOT_ASSET,
         ratio_Flux_de_TR_expl_sur_passif_cour = CF_CASH_FROM_OPER/BS_CUR_LIAB,
         ratio_Fonds_de_roulement_sur_tot_actif = WORKING_CAPITAL/BS_TOT_ASSET)

#ratio_val_marchd_tot_sur_tot_actif =TOT_MKT_VAL/BS_TOT_ASSET) is deleted car TOT_MKT8VAL pas dans new database
# ratio_tot_liab_sur_tot_actif=BS_TOTAL_LIABILITIES/BS_TOT_ASSET, (idem)


colnames(data_2007_avt_good_ratio)

is.element("TOT_MKT_VAL", colnames(data_2007_avt_good_ratio))
is.element("BS_TOTAL_LIABILITIES", colnames(data_2007_avt_good_ratio))
is.element("BS_PURE_RETAINED_EARNINGS", colnames(data_2007_avt_good_ratio))







# 2008
data_2008_with_good_ratio <- data_2008_avt_good_ratio%>%
  mutate(ratio_B_non_rep_sur_Tot_actif = BS_PURE_RETAINED_EARNINGS/BS_TOT_ASSET,
         ratio_Flux_de_TR_expl_sur_passif_cour = CF_CASH_FROM_OPER/BS_CUR_LIAB,
         ratio_Fonds_de_roulement_sur_tot_actif = WORKING_CAPITAL/BS_TOT_ASSET)

#ratio_val_marchd_tot_sur_tot_actif =TOT_MKT_VAL/BS_TOT_ASSET) is deleted car TOT_MKT8VAL pas dans new database
# ratio_tot_liab_sur_tot_actif=BS_TOTAL_LIABILITIES/BS_TOT_ASSET, (idem)


colnames(data_2008_avt_good_ratio)

is.element("TOT_MKT_VAL", colnames(data_2008_avt_good_ratio))
is.element("BS_TOTAL_LIABILITIES", colnames(data_2008_avt_good_ratio))
is.element("BS_PURE_RETAINED_EARNINGS", colnames(data_2008_avt_good_ratio))




# 2009
data_2009_with_good_ratio <- data_2009_avt_good_ratio%>%
  mutate(ratio_tot_liab_sur_tot_actif=BS_TOTAL_LIABILITIES/BS_TOT_ASSET,
         ratio_B_non_rep_sur_Tot_actif = BS_PURE_RETAINED_EARNINGS/BS_TOT_ASSET,
         ratio_Flux_de_TR_expl_sur_passif_cour = CF_CASH_FROM_OPER/BS_CUR_LIAB,
         ratio_Fonds_de_roulement_sur_tot_actif = WORKING_CAPITAL/BS_TOT_ASSET)

#ratio_val_marchd_tot_sur_tot_actif =TOT_MKT_VAL/BS_TOT_ASSET) is deleted car TOT_MKT8VAL pas dans new database

colnames(data_2009_avt_good_ratio)

is.element("TOT_MKT_VAL", colnames(data_2009_avt_good_ratio))
is.element("BS_TOTAL_LIABILITIES", colnames(data_2009_avt_good_ratio))
is.element("BS_PURE_RETAINED_EARNINGS", colnames(data_2009_avt_good_ratio))




# 2016
data_2016_with_good_ratio <- data_2016_avt_good_ratio%>%
  mutate(ratio_tot_liab_sur_tot_actif=BS_TOTAL_LIABILITIES/BS_TOT_ASSET,
         ratio_B_non_rep_sur_Tot_actif = BS_PURE_RETAINED_EARNINGS/BS_TOT_ASSET,
         ratio_Flux_de_TR_expl_sur_passif_cour = CF_CASH_FROM_OPER/BS_CUR_LIAB,
         ratio_Fonds_de_roulement_sur_tot_actif = WORKING_CAPITAL/BS_TOT_ASSET)

#ratio_val_marchd_tot_sur_tot_actif =TOT_MKT_VAL/BS_TOT_ASSET) is deleted car TOT_MKT8VAL pas dans new database

colnames(data_2016_avt_good_ratio)

is.element("TOT_MKT_VAL", colnames(data_2016_avt_good_ratio))
is.element("BS_TOTAL_LIABILITIES", colnames(data_2016_avt_good_ratio))
is.element("BS_PURE_RETAINED_EARNINGS", colnames(data_2016_avt_good_ratio))





# 2018
data_2018_with_good_ratio <- data_2018_avt_good_ratio%>%
  mutate(ratio_tot_liab_sur_tot_actif=BS_TOTAL_LIABILITIES/BS_TOT_ASSET,
         ratio_B_non_rep_sur_Tot_actif = BS_PURE_RETAINED_EARNINGS/BS_TOT_ASSET,
         ratio_Flux_de_TR_expl_sur_passif_cour = CF_CASH_FROM_OPER/BS_CUR_LIAB,
         ratio_Fonds_de_roulement_sur_tot_actif = WORKING_CAPITAL/BS_TOT_ASSET)

#ratio_val_marchd_tot_sur_tot_actif =TOT_MKT_VAL/BS_TOT_ASSET) is deleted car TOT_MKT8VAL pas dans new database
# ratio_tot_liab_sur_tot_actif=BS_TOTAL_LIABILITIES/BS_TOT_ASSET, (idem)
#ratio_B_non_rep_sur_Tot_actif = BS_PURE_RETAINED_EARNINGS/BS_TOT_ASSET,

colnames(data_2018_avt_good_ratio)

is.element("TOT_MKT_VAL", colnames(data_2018_avt_good_ratio))
is.element("BS_TOTAL_LIABILITIES", colnames(data_2018_avt_good_ratio))
is.element("BS_PURE_RETAINED_EARNINGS", colnames(data_2018_avt_good_ratio))





# 2019
data_2019_with_good_ratio <- data_2019_avt_good_ratio%>%
  mutate(ratio_tot_liab_sur_tot_actif=BS_TOTAL_LIABILITIES/BS_TOT_ASSET,
         ratio_B_non_rep_sur_Tot_actif = BS_PURE_RETAINED_EARNINGS/BS_TOT_ASSET,
         ratio_Flux_de_TR_expl_sur_passif_cour = CF_CASH_FROM_OPER/BS_CUR_LIAB,
         ratio_Fonds_de_roulement_sur_tot_actif = WORKING_CAPITAL/BS_TOT_ASSET)

#ratio_val_marchd_tot_sur_tot_actif =TOT_MKT_VAL/BS_TOT_ASSET) is deleted car TOT_MKT8VAL pas dans new database

colnames(data_2019_avt_good_ratio)

is.element("TOT_MKT_VAL", colnames(data_2019_avt_good_ratio))
is.element("BS_TOTAL_LIABILITIES", colnames(data_2019_avt_good_ratio))
is.element("BS_PURE_RETAINED_EARNINGS", colnames(data_2019_avt_good_ratio))









is.element("APPLIED_BETA", colnames(data_2000_avt_good_ratio))
is.element("APPLIED_BETA", colnames(data_2007_avt_good_ratio))
is.element("APPLIED_BETA", colnames(data_2008_avt_good_ratio))
is.element("APPLIED_BETA", colnames(data_2009_avt_good_ratio))
is.element("APPLIED_BETA", colnames(data_2016_avt_good_ratio))
is.element("APPLIED_BETA", colnames(data_2018_avt_good_ratio))
is.element("APPLIED_BETA", colnames(data_2019_avt_good_ratio))


























### {quand puis je dire que ces elements sont déterminants des ratings des firmes canadienne?}
### est-ce qand c'est good R_square ??? ==> voir solution Amdoumi




# Renommer all names en FR  -----------------------------------------------




#par rapport à 2018 j'enlève les 2 ratios qui ont beaucoup de NA ("Marge_Bénéficiaire" et "Valeur_marchd_tot") et 1 qui est identique à une autre ("EBITDA_sur_Revenu")
#je déplacce aussi ceux que j'avais pas tenue compte lors du mice()

# j'enleve le dernier ratio not pris en compte : ratio_val_marchd_tot_sur_tot_actif

# ceux que j'ai mi en dernier avant de put les ratios de dplyr sont stockés dans col_names_var_not_ds_mice

#NOT RUN
col_names_var_not_ds_mice #elles sont au nombre de 5 -> ci dessous par ordre
#  "Dette_LT", "Total_actif",  "Trésorie d'exp",  "ratio_tot_dette_sur_tot_actif",  "Marge_d_explt",
# now mettons les avant les 4 ratios constituées avec dplyr donc les 4 last ratios





# 2000
nom_col_2000 <- colnames(data_2000_with_good_ratio)
nom_col_2000
length(nom_col_2000)

# changer Ordre 2000
nom_col_good_2000 <- c("Cote_crédit_num", "Cote_crédit", "RATINGS", "Marge_bénéficiaire nette",
                  "Marge_sur_EBITDA","Marge_sur_EBIT", "Rendement_sur_cap_prop","Rendement_sur_actif",
                  "Dette_LT",
                  "Total_actif","Flux_de_trésorie_d'expl","Passif_courant",
                  "ratio_ben_avt_impot_sur_frais_int", "ratio_tot_dette_sur_tot_actif",
                  "ratio_actuel","Ratio_de_liquid_réduite","Ratio_de_liquidité","Fonds_roulement",
                  "Ratio_Fonds_de_roulmt_sur_ventes","Marge_opérationnelle","Beta_applique",
                  "ratio_Flux_de_TR_expl_sur_passif_cour","ratio_Fonds_de_roulement_sur_tot_actif")
                  
                  
                  
length(nom_col_2000)
length(nom_col_good_2000)

#juste pour des fins de comparaisons
matrice_comparaison_nom_var_1_2000 <- matrix(nom_col_2000, 23, 1)
matrice_comparaison_nom_var_2_2000 <- matrix(nom_col_good_2000, 23, 1)

m_2000 <-  cbind(matrice_comparaison_nom_var_1_2000, matrice_comparaison_nom_var_2_2000)
##fix(m_2000)

#le 3 est good car EBIT_TO_NET_SALES = Marge_sur_EBIT

#good car comparaison équitable dans leur positionnement













# 2007
nom_col_2007 <- colnames(data_2007_with_good_ratio)
nom_col_2007
length(nom_col_2007)

# changer Ordre 2007
nom_col_good_2007 <-  c("Cote_crédit_num", "Cote_crédit", "RATINGS", "Marge_bénéficiaire nette",
                        "Marge_sur_EBITDA","Marge_sur_EBIT", "Rendement_sur_cap_prop","Rendement_sur_actif",
                        "Dette_LT",
                        "Total_actif","Bénéfices_non_répartis","Flux_de_trésorie_d'expl","Passif_courant",
                        "ratio_ben_avt_impot_sur_frais_int", "ratio_tot_dette_sur_tot_actif",
                        "ratio_actuel","Ratio_de_liquid_réduite","Ratio_de_liquidité","Fonds_roulement",
                        "Ratio_Fonds_de_roulmt_sur_ventes","Marge_opérationnelle","ratio_B_non_rep_sur_Tot_actif",
                        "ratio_Flux_de_TR_expl_sur_passif_cour","ratio_Fonds_de_roulement_sur_tot_actif")

  


length(nom_col_2007)
length(nom_col_good_2007)

#juste pour des fins de comparaisons
matrice_comparaison_nom_var_1_2007 <- matrix(nom_col_2007, 24, 1)
matrice_comparaison_nom_var_2_2007 <- matrix(nom_col_good_2007, 24, 1)

m_2007 <-  cbind(matrice_comparaison_nom_var_1_2007, matrice_comparaison_nom_var_2_2007)
##fix(m_2007)

#le 3 est good car EBIT_TO_NET_SALES = Marge_sur_EBIT

#good car comparaison équitable dans leur positionnement









# 2008
nom_col_2008 <- colnames(data_2008_with_good_ratio)
nom_col_2008
length(nom_col_2008)

# changer Ordre 2008
nom_col_good_2008 <-c("Cote_crédit_num", "Cote_crédit", "RATINGS", "Marge_bénéficiaire nette",
                      "Marge_sur_EBITDA","Marge_sur_EBIT", "Rendement_sur_cap_prop","Rendement_sur_actif",
                      "Dette_LT",
                      "Total_actif","Bénéfices_non_répartis","Flux_de_trésorie_d'expl","Passif_courant",
                      "ratio_ben_avt_impot_sur_frais_int", "ratio_tot_dette_sur_tot_actif",
                      "ratio_actuel","Ratio_de_liquid_réduite","Ratio_de_liquidité","Fonds_roulement",
                      "Ratio_Fonds_de_roulmt_sur_ventes","Marge_opérationnelle","ratio_B_non_rep_sur_Tot_actif",
                      "ratio_Flux_de_TR_expl_sur_passif_cour","ratio_Fonds_de_roulement_sur_tot_actif")

  
  


length(nom_col_2008)
length(nom_col_good_2008)

#juste pour des fins de comparaisons
matrice_comparaison_nom_var_1_2008 <- matrix(nom_col_2008, 24, 1)
matrice_comparaison_nom_var_2_2008 <- matrix(nom_col_good_2008, 24, 1)

m_2008 <-  cbind(matrice_comparaison_nom_var_1_2008, matrice_comparaison_nom_var_2_2008)
##fix(m_2008)

#le 3 est good car EBIT_TO_NET_SALES = Marge_sur_EBIT

#good car comparaison équitable dans leur positionnement










# 2009
nom_col_2009 <- colnames(data_2009_with_good_ratio)
nom_col_2009
length(nom_col_2009)

# changer Ordre 2009
nom_col_good_2009 <- c("Cote_crédit_num", "Cote_crédit", "RATINGS", "Marge_bénéficiaire nette",
                       "Marge_sur_EBITDA","Marge_sur_EBIT", "Rendement_sur_cap_prop","Rendement_sur_actif",
                       "Dette_LT","Total_passif",
                       "Total_actif","Bénéfices_non_répartis","Flux_de_trésorie_d'expl","Passif_courant",
                       "ratio_ben_avt_impot_sur_frais_int", "ratio_tot_dette_sur_tot_actif",
                       "ratio_actuel","Ratio_de_liquid_réduite","Ratio_de_liquidité","Fonds_roulement",
                       "Ratio_Fonds_de_roulmt_sur_ventes", "Marge_opérationnelle",
                       "ratio_tot_liab_sur_tot_actif", 
                       "ratio_B_non_rep_sur_Tot_actif",
                       "ratio_Flux_de_TR_expl_sur_passif_cour","ratio_Fonds_de_roulement_sur_tot_actif")


  
length(nom_col_2009)
length(nom_col_good_2009)

#juste pour des fins de comparaisons
matrice_comparaison_nom_var_1_2009 <- matrix(nom_col_2009, 26, 1)
matrice_comparaison_nom_var_2_2009 <- matrix(nom_col_good_2009, 26, 1)

m_2009 <-  cbind(matrice_comparaison_nom_var_1_2009, matrice_comparaison_nom_var_2_2009)
##fix(m_2009)

#le 3 est good car EBIT_TO_NET_SALES = Marge_sur_EBIT

#good car comparaison équitable dans leur positionnement









# 2016
nom_col_2016 <- colnames(data_2016_with_good_ratio)
nom_col_2016
length(nom_col_2016)

# changer Ordre 2016
nom_col_good_2016 <- c("Cote_crédit_num", "Cote_crédit", "RATINGS", "Marge_bénéficiaire nette",
                  "Marge_sur_EBITDA","Marge_sur_EBIT", "Rendement_sur_cap_prop","Rendement_sur_actif",
                  "Dette_LT","Total_passif",
                  "Total_actif","Bénéfices_non_répartis","Flux_de_trésorie_d'expl","Passif_courant",
                  "ratio_ben_avt_impot_sur_frais_int", "ratio_tot_dette_sur_tot_actif",
                  "ratio_actuel","Ratio_de_liquid_réduite","Ratio_de_liquidité","Fonds_roulement",
                  "Ratio_Fonds_de_roulmt_sur_ventes", "Marge_opérationnelle",
                  "ratio_tot_liab_sur_tot_actif", 
                  "ratio_B_non_rep_sur_Tot_actif",
                  "ratio_Flux_de_TR_expl_sur_passif_cour","ratio_Fonds_de_roulement_sur_tot_actif")


length(nom_col_2016)
length(nom_col_good_2016)

#juste pour des fins de comparaisons
matrice_comparaison_nom_var_1_2016 <- matrix(nom_col_2016, 26, 1)
matrice_comparaison_nom_var_2_2016 <- matrix(nom_col_good_2016, 26, 1)

m_2016 <-  cbind(matrice_comparaison_nom_var_1_2016, matrice_comparaison_nom_var_2_2016)
##fix(m_2016)

#le 3 est good car EBIT_TO_NET_SALES = Marge_sur_EBIT

#good car comparaison équitable dans leur positionnement










# 2018
nom_col_2018 <- colnames(data_2018_with_good_ratio)
nom_col_2018
length(nom_col_2018)

# changer Ordre 2018
nom_col_good_2018 <- c("Cote_crédit_num", "Cote_crédit", "RATINGS", "Marge_bénéficiaire nette",
                       "Marge_sur_EBITDA","Marge_sur_EBIT", "Rendement_sur_cap_prop","Rendement_sur_actif",
                       "Dette_LT","Total_passif",
                       "Total_actif","Bénéfices_non_répartis","Flux_de_trésorie_d'expl","Passif_courant",
                       "ratio_ben_avt_impot_sur_frais_int", "ratio_tot_dette_sur_tot_actif",
                       "ratio_actuel","Ratio_de_liquid_réduite","Ratio_de_liquidité","Fonds_roulement",
                       "Ratio_Fonds_de_roulmt_sur_ventes", "Marge_opérationnelle",
                       "Beta_applique","Croissance_adj_des_Bén_ann","Croissance_tot_actif",
                       "ratio_tot_liab_sur_tot_actif", 
                       "ratio_B_non_rep_sur_Tot_actif",
                       "ratio_Flux_de_TR_expl_sur_passif_cour","ratio_Fonds_de_roulement_sur_tot_actif")

  
  
  
  

length(nom_col_2018)
length(nom_col_good_2018)

#juste pour des fins de comparaisons
matrice_comparaison_nom_var_1_2018 <- matrix(nom_col_2018, 29, 1)
matrice_comparaison_nom_var_2_2018 <- matrix(nom_col_good_2018, 29, 1)

m_2018 <-  cbind(matrice_comparaison_nom_var_1_2018, matrice_comparaison_nom_var_2_2018)
##fix(m_2018)

#le 3 est good car EBIT_TO_NET_SALES = Marge_sur_EBIT

#good car comparaison équitable dans leur positionnement












# 2019
nom_col_2019 <- colnames(data_2019_with_good_ratio)
nom_col_2019
length(nom_col_2019)

# changer Ordre 2019
nom_col_good_2019 <- c("Cote_crédit_num", "Cote_crédit", "RATINGS",
                       "Marge_sur_EBITDA","Marge_sur_EBIT", "Rendement_sur_cap_prop","Rendement_sur_actif",
                       "Dette_LT","Total_passif",
                       "Total_actif","Bénéfices_non_répartis","Flux_de_trésorie_d'expl","Passif_courant",
                       "ratio_ben_avt_impot_sur_frais_int", "ratio_tot_dette_sur_tot_actif",
                       "ratio_actuel","Ratio_de_liquid_réduite","Ratio_de_liquidité","Fonds_roulement",
                       "Ratio_Fonds_de_roulmt_sur_ventes", "Marge_opérationnelle",
                       "Beta_applique","Croissance_adj_des_Bén_ann","Croissance_tot_actif",
                       "ratio_tot_liab_sur_tot_actif", 
                       "ratio_B_non_rep_sur_Tot_actif",
                       "ratio_Flux_de_TR_expl_sur_passif_cour","ratio_Fonds_de_roulement_sur_tot_actif")


length(nom_col_2019)
length(nom_col_good_2019)

#juste pour des fins de comparaisons
matrice_comparaison_nom_var_1_2019 <- matrix(nom_col_2019, 28, 1)
matrice_comparaison_nom_var_2_2019 <- matrix(nom_col_good_2019, 28, 1)

m_2019 <-  cbind(matrice_comparaison_nom_var_1_2019, matrice_comparaison_nom_var_2_2019)
##fix(m_2019)

#le 3 est good car EBIT_TO_NET_SALES = Marge_sur_EBIT

#good car comparaison équitable dans leur positionnement









# 2018 en à 29 donc le plus (donc on le compare par rapport tout les autres)

setdiff(nom_col_good_2018, nom_col_good_2000) # on trouve 6 éléments dans 2018 qu'on ne trouve pas dans 2000
setdiff(nom_col_good_2000, nom_col_good_2018) # 0 = tout ce qu'on trouve dans 2000 on le trouve dans 2018

setdiff(nom_col_good_2018, nom_col_good_2007)
setdiff(nom_col_good_2007, nom_col_good_2018)

setdiff(nom_col_good_2018, nom_col_good_2008)
setdiff(nom_col_good_2008, nom_col_good_2018)

setdiff(nom_col_good_2018, nom_col_good_2009)
setdiff(nom_col_good_2009, nom_col_good_2018)

setdiff(nom_col_good_2018, nom_col_good_2016)
setdiff(nom_col_good_2016, nom_col_good_2018)

setdiff(nom_col_good_2018, nom_col_good_2018)
setdiff(nom_col_good_2018, nom_col_good_2018)

setdiff(nom_col_good_2018, nom_col_good_2019)
setdiff(nom_col_good_2019, nom_col_good_2018)






setdiff(nom_col_good_2019, nom_col_good_2018) 










# renommer variables ------------------------------------------------------

#pour ne pas avoir nom colonne les mêmes 
data_2000_with_good_ratio.1 <- data_2000_with_good_ratio
colnames(data_2000_with_good_ratio.1) <- nom_col_good_2000



data_2007_with_good_ratio.1 <- data_2007_with_good_ratio
colnames(data_2007_with_good_ratio.1) <- nom_col_good_2007


data_2008_with_good_ratio.1 <- data_2008_with_good_ratio
colnames(data_2008_with_good_ratio.1) <- nom_col_good_2008


data_2009_with_good_ratio.1 <- data_2009_with_good_ratio
colnames(data_2009_with_good_ratio.1) <- nom_col_good_2009


data_2016_with_good_ratio.1 <- data_2016_with_good_ratio
colnames(data_2016_with_good_ratio.1) <- nom_col_good_2016


data_2018_with_good_ratio.1 <- data_2018_with_good_ratio
colnames(data_2018_with_good_ratio.1) <- nom_col_good_2018


data_2019_with_good_ratio.1 <- data_2019_with_good_ratio
colnames(data_2019_with_good_ratio.1) <- nom_col_good_2019

##fix(data_2019_with_good_ratio.1)







# JE FAIS EXTRACTION DE ALL VARAIBLES ET APRES LORS REG JE VAIS SELECTIONNER CEUX QUI M'INTERESSENT

#write.csv(data_2000_with_good_ratio.1, file = "ma_base_de_donnees_2000.csv")
#write.csv(data_2007_with_good_ratio.1, file = "ma_base_de_donnees_2007.csv")
#write.csv(data_2008_with_good_ratio.1, file = "ma_base_de_donnees_2008.csv")
#write.csv(data_2009_with_good_ratio.1, file = "ma_base_de_donnees_2009.csv")
#write.csv(data_2016_with_good_ratio.1, file = "ma_base_de_donnees_2016.csv")
#write.csv(data_2018_with_good_ratio.1, file = "ma_base_de_donnees_2018.csv")
#write.csv(data_2019_with_good_ratio.1, file = "ma_base_de_donnees_2019.csv")





























# extrraire database avec bons ratios only ---------------------------------

#j'enlève aussi ceux que j'avais deleted (comme profit margin) et les mettre en ordre
#donc je run dejà existant et ceux not dispo dans data_2019_with_good_ratio.1 marque error et je les delete ici

library(dplyr)
bon_variables <- data_2019_with_good_ratio.1%>%
  select(Cote_crédit, Marge_sur_EBITDA,
         Marge_sur_EBIT, Rendement_sur_cap_prop, Rendement_sur_actif,
         ratio_tot_liab_sur_tot_actif, ratio_B_non_rep_sur_Tot_actif,
         ratio_Flux_de_TR_expl_sur_passif_cour, ratio_ben_avt_impot_sur_frais_int,
         ratio_tot_dette_sur_tot_actif, ratio_actuel, Ratio_de_liquid_réduite, ratio_Fonds_de_roulement_sur_tot_actif,
         Ratio_de_liquidité, Ratio_Fonds_de_roulmt_sur_ventes, Total_actif,
         Marge_d_explt, Beta_applique)


##fix(bon_variables)


#extraire base de donneés sur excel

#write.csv(my_extract_data, file = "data_base_2018") # on fait l'extraction 

#------> write.csv(bon_variables, file = "ma_base_de_donnees_2019.csv") # si on oublie le .csv le dossier ne sera pas dans le wd


































# REG DANS OTHER FICHIER


# Pour faire extraction aussi avec rating en character

bon_variables_2 <- cbind(my_rating_grd_grp, bon_variables)
#------> write.csv(bon_variables_2, file = "ma_base_de_donnees_2019_rating_char_&_num.csv") # si on oublie le .csv le dossier ne sera pas dans le wd


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

