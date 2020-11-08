
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

#bien afficher résultats cor() et résultat rég


?summarise()

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

