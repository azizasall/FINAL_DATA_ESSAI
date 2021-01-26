rm(list = ls())



# construction data base avec vol_ 30D


library(readr)
vol_30D_2000 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/VOL_30D/vol_30D_2000.csv", 
                         col_names = FALSE)
##fix(vol_30D_2000)
colnames(vol_30D_2000) <- c("x1","vol_30D_2000")
vol_30D_2000 <- vol_30D_2000[-1,]
##fix(vol_30D_2000)


library(readr)
vol_30D_2007 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/VOL_30D/vol_30D_2007.csv", 
                         col_names = FALSE)
##fix(vol_30D_2007)
colnames(vol_30D_2007) <- c("x1","vol_30D_2007")
vol_30D_2007 <- vol_30D_2007[-1,]
##fix(vol_30D_2007)




library(readr)
vol_30D_2008 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/VOL_30D/vol_30D_2008.csv", 
                         col_names = FALSE)
##fix(vol_30D_2008)
colnames(vol_30D_2008) <- c("x1","vol_30D_2008")
vol_30D_2008 <- vol_30D_2008[-1,]
##fix(vol_30D_2008)


library(readr)
vol_30D_2009 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/VOL_30D/vol_30D_2009.csv", 
                         col_names = FALSE)
##fix(vol_30D_2009)
colnames(vol_30D_2009) <- c("x1","vol_30D_2009")
vol_30D_2009 <- vol_30D_2009[-1,]
##fix(vol_30D_2009)


library(readr)
vol_30D_2016 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/VOL_30D/vol_30D_2016.csv", 
                         col_names = FALSE)
##fix(vol_30D_2016)
colnames(vol_30D_2016) <- c("x1","vol_30D_2016")
vol_30D_2016 <- vol_30D_2016[-1,]
##fix(vol_30D_2016)




library(readr)
vol_30D_2018 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/VOL_30D/vol_30D_2018.csv", 
                         col_names = FALSE)
##fix(vol_30D_2018)
colnames(vol_30D_2018) <- c("x1","vol_30D_2018")
vol_30D_2018 <- vol_30D_2018[-1,]
##fix(vol_30D_2018)




library(readr)
vol_30D_2019 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/VOL_30D/vol_30D_2019.csv", 
                         col_names = FALSE)

##fix(vol_30D_2019)
colnames(vol_30D_2019) <- c("x1","vol_30D_2019")
vol_30D_2019 <- vol_30D_2019[-1,]
##fix(vol_30D_2019)


vol_30D_2019 <- lapply(vol_30D_2019, function(x) as.numeric(as.character(x)))

mean(vol_30D_2019$vol_30D_2019)



# ajoutons 1 ligne de vol_30D_2009
vol_30D_2019 <- vol_30D_2019$vol_30D_2019

dim(vol_30D_2019)
class(vol_30D_2019)
matrix(vol_30D_2019)
vol_30D_2019
mean(vol_30D_2019)


length(vol_30D_2019)

vol_30D_2019 <- matrix(c(vol_30D_2019, 25.98505), 151, 1)
dim(vol_30D_2019)
vol_30D_2019
colnames(vol_30D_2019) <- "volalitilite_30_jours"

##fix(vol_30D_2019)



#  #  #


# importer data sans vol

library(readr)
base_de_donnees_2000 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/base_de_données_sans_vol/base_de_donnees_2000.csv")
#fix(base_de_donnees_2000)

#ajoutons vol_30D
dim(base_de_donnees_2000)
dim(vol_30D_2000)
base_de_donnees_2000 <- cbind(base_de_donnees_2000, volalitilite_30_jours = vol_30D_2000$vol_30D_2000)
#fix(base_de_donnees_2000)


library(readr)
base_de_donnees_2007 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/base_de_données_sans_vol/base_de_donnees_2007.csv")
dim(base_de_donnees_2007)

#ajoutons vol_30D
dim(base_de_donnees_2007)
dim(vol_30D_2007)
base_de_donnees_2007 <- cbind(base_de_donnees_2007, volalitilite_30_jours = vol_30D_2007$vol_30D_2007)
dim(base_de_donnees_2007)
#fix(base_de_donnees_2007)




library(readr)
base_de_donnees_2008 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/base_de_données_sans_vol/base_de_donnees_2008.csv")
dim(base_de_donnees_2008)
#fix(base_de_donnees_2008)

#ajoutons vol_30D
dim(base_de_donnees_2008)
dim(vol_30D_2008)
base_de_donnees_2008 <- cbind(base_de_donnees_2008, volalitilite_30_jours = vol_30D_2008$vol_30D_2008)
dim(base_de_donnees_2008)
#fix(base_de_donnees_2008)



library(readr)
base_de_donnees_2009 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/base_de_données_sans_vol/base_de_donnees_2009.csv")
dim(base_de_donnees_2009)
#fix(base_de_donnees_2009)

#ajoutons vol_30D
dim(base_de_donnees_2009)
dim(vol_30D_2009)
base_de_donnees_2009 <- cbind(base_de_donnees_2009, volalitilite_30_jours = vol_30D_2009$vol_30D_2009)
dim(base_de_donnees_2009)
#fix(base_de_donnees_2009)




library(readr)
base_de_donnees_2016 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/base_de_données_sans_vol/base_de_donnees_2016.csv")
dim(base_de_donnees_2016)
#fix(base_de_donnees_2016)

#ajoutons vol_30D
dim(base_de_donnees_2016)
dim(vol_30D_2016)
base_de_donnees_2016 <- cbind(base_de_donnees_2016, volalitilite_30_jours = vol_30D_2016$vol_30D_2016)
dim(base_de_donnees_2016)
#fix(base_de_donnees_2009)




library(readr)
base_de_donnees_2018 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/base_de_données_sans_vol/base_de_donnees_2018.csv")
dim(base_de_donnees_2018)
#fix(base_de_donnees_2018)


#ajoutons vol_30D
dim(base_de_donnees_2018)
dim(vol_30D_2018)
base_de_donnees_2018 <- cbind(base_de_donnees_2018, volalitilite_30_jours = vol_30D_2018$vol_30D_2018)
dim(base_de_donnees_2018)
#fix(base_de_donnees_2018)



library(readr)
base_de_donnees_2019 <- read_csv("C:/Users/aziza/Desktop/Projet de fin d'études en gestion financière/data_traiement_dans_R/FINAL_DATA/FINAL_DATA/FINAL_DATA_ESSAI/base_de_données_sans_vol/base_de_donnees_2019.csv")
#fix(base_de_donnees_2019)

#ajoutons vol_30D
dim(base_de_donnees_2019)
dim(vol_30D_2019)
base_de_donnees_2019 <- cbind(base_de_donnees_2019, volalitilite_30_jours = vol_30D_2019)
dim(base_de_donnees_2019)
#fix(base_de_donnees_2019)


####


#rm(list = ls())


# unlist et put numéric avant 
#try summary pour voir ce que ça donne

fix(base_de_donnees_2000)
summary(base_de_donnees_2000)

base_de_donnees_2000 <- base_de_donnees_2000[-1]

base_de_donnees_2000 <- as.data.frame(lapply(base_de_donnees_2000, unlist))
summary(base_de_donnees_2000)




fix(base_de_donnees_2007)
summary(base_de_donnees_2007)

base_de_donnees_2007 <- base_de_donnees_2007[-1]

base_de_donnees_2007 <- as.data.frame(lapply(base_de_donnees_2007, unlist))
summary(base_de_donnees_2007)




fix(base_de_donnees_2008)
summary(base_de_donnees_2008)

base_de_donnees_2008 <- base_de_donnees_2008[-1]

base_de_donnees_2008 <- as.data.frame(lapply(base_de_donnees_2008, unlist))
summary(base_de_donnees_2008)




fix(base_de_donnees_2009)
summary(base_de_donnees_2009)

base_de_donnees_2009 <- base_de_donnees_2009[-1]

base_de_donnees_2009 <- as.data.frame(lapply(base_de_donnees_2009, unlist))
summary(base_de_donnees_2009)




fix(base_de_donnees_2016)
summary(base_de_donnees_2016)

base_de_donnees_2016 <- base_de_donnees_2016[-1]

base_de_donnees_2016 <- as.data.frame(lapply(base_de_donnees_2016, unlist))
summary(base_de_donnees_2016)




fix(base_de_donnees_2018)
summary(base_de_donnees_2018)

base_de_donnees_2018 <- base_de_donnees_2018[-1]

base_de_donnees_2018 <- as.data.frame(lapply(base_de_donnees_2018, unlist))
summary(base_de_donnees_2018)




fix(base_de_donnees_2019)
summary(base_de_donnees_2019)

base_de_donnees_2019 <- base_de_donnees_2019[-1]

base_de_donnees_2019 <- as.data.frame(lapply(base_de_donnees_2019, unlist))
summary(base_de_donnees_2019)



#base_de_donnees_2000[,c(-1,-2)] <- lapply(base_de_donnees_2000[,c(-1,-2)], function(x) as.numeric(as.character(x)))
#summary(base_de_donnees_2000[,c(-1,-2)])
#dim(base_de_donnees_2000[,c(-1,-2)])





# correction col name
colnames(base_de_donnees_2000)
col_2000 <- c( "binaire_cote_credit_2000", "Cote_credit" ,"Marge_beneficiaire_nette"        
         ,"Marge_sur_EBITDA"                      
         ,"Marge_sur_EBIT"                        
         ,"Rendement_sur_cap_prop"                
         ,"Rendement_sur_actif"                   
         ,"ratio_ben_avt_impot_sur_frais_int"     
         ,"ratio_tot_dette_sur_tot_actif"         
         ,"ratio_Flux_de_TR_expl_sur_passif_cour" 
         ,"ratio_actuel"                          
         ,"Ratio_de_liquid_reduite"            
         ,"Ratio_de_liquidite"                 
         ,"Ratio_Fonds_de_roulmt_sur_ventes"      
         ,"ratio_Fonds_de_roulement_sur_tot_actif"
         ,"Total_actif"                           
         ,"Marge_operationnelle"               
         ,"Beta_applique"                         
         ,"volalitilite_30_jours"  )

colnames(base_de_donnees_2000) <- col_2000





colnames(base_de_donnees_2007)
col_2007 <- c("binaire_cote_credit_2007"              
              , "Cote_credit"                        
              ,"Marge_beneficiaire_nette"        
              ,"Marge_sur_EBITDA"                      
              ,"Marge_sur_EBIT"                        
              , "Rendement_sur_cap_prop"                
              , "Rendement_sur_actif"                   
              , "ratio_ben_avt_impot_sur_frais_int"     
              , "ratio_tot_dette_sur_tot_actif"         
              , "ratio_B_non_rep_sur_Tot_actif"         
              ,"ratio_Flux_de_TR_expl_sur_passif_cour" 
              ,"ratio_actuel"                          
              ,"Ratio_de_liquid_reduite"            
              ,"Ratio_de_liquidite"                 
              , "Ratio_Fonds_de_roulmt_sur_ventes"      
              , "ratio_Fonds_de_roulement_sur_tot_actif"
              , "Total_actif"                           
              , "Marge_operationnelle"               
               ,"volalitilite_30_jours")

colnames(base_de_donnees_2007) <- col_2007








colnames(base_de_donnees_2008)

col_2008 <- c("binaire_cote_credit_2008"              
,"Cote_credit"                        
,"Marge_beneficiaire_nette"        
,"Marge_sur_EBITDA"                      
,"Marge_sur_EBIT"                        
,"Rendement_sur_cap_prop"                
, "Rendement_sur_actif"                   
, "ratio_ben_avt_impot_sur_frais_int"     
, "ratio_tot_dette_sur_tot_actif"         
, "ratio_B_non_rep_sur_Tot_actif"         
, "ratio_Flux_de_TR_expl_sur_passif_cour" 
, "ratio_actuel"                          
, "Ratio_de_liquid_reduite"            
, "Ratio_de_liquidite"                 
,"Ratio_Fonds_de_roulmt_sur_ventes"      
, "ratio_Fonds_de_roulement_sur_tot_actif"
, "Total_actif"                           
, "Marge_operationnelle"               
,"volalitilite_30_jours" )


colnames(base_de_donnees_2008) <- col_2008

fix(base_de_donnees_2008)




colnames(base_de_donnees_2009)

col_2009 <- c("binaire_cote_credit_2009"              
              , "Cote_credit"                        
              , "Marge_beneficiaire_nette"        
              , "Marge_sur_EBITDA"                      
              , "Marge_sur_EBIT"                        
              , "Rendement_sur_cap_prop"                
              , "Rendement_sur_actif"                   
              , "ratio_ben_avt_impot_sur_frais_int"     
              , "ratio_tot_dette_sur_tot_actif"         
              , "ratio_tot_liab_sur_tot_actif"          
              , "ratio_B_non_rep_sur_Tot_actif"         
              , "ratio_Flux_de_TR_expl_sur_passif_cour" 
              , "ratio_actuel"                          
              , "Ratio_de_liquid_reduite"            
              , "Ratio_de_liquidite"                 
              , "Ratio_Fonds_de_roulmt_sur_ventes"      
              , "ratio_Fonds_de_roulement_sur_tot_actif"
              , "Total_actif"                           
              , "Marge_operationnelle"               
              , "volalitilite_30_jours"  
               )

colnames(base_de_donnees_2009) <- col_2009
colnames(base_de_donnees_2009)





colnames(base_de_donnees_2016)

col_2016 <- c( "binaire_cote_credit_2016"              
              , "Cote_credit"                        
              , "Marge_beneficiaire_nette"        
              , "Marge_sur_EBITDA"                      
              , "Marge_sur_EBIT"                        
              , "Rendement_sur_cap_prop"                
              , "Rendement_sur_actif"                   
              , "ratio_ben_avt_impot_sur_frais_int"     
              , "ratio_tot_dette_sur_tot_actif"         
              , "ratio_tot_liab_sur_tot_actif"          
              , "ratio_B_non_rep_sur_Tot_actif"         
              , "ratio_Flux_de_TR_expl_sur_passif_cour" 
              , "ratio_actuel"                          
              , "Ratio_de_liquid_reduite"            
              , "Ratio_de_liquidite"                 
              , "Ratio_Fonds_de_roulmt_sur_ventes"      
              , "ratio_Fonds_de_roulement_sur_tot_actif"
              , "Total_actif"                           
              , "Marge_operationnelle"               
              ,"volalitilite_30_jours")

colnames(base_de_donnees_2016) <- col_2016







colnames(base_de_donnees_2018)

col_2018 <- c( "binaire_cote_credit_2018"              
               ,"Cote_credit"                        
               , "Marge_beneficiaire_nette"        
               , "Marge_sur_EBITDA"                      
               , "Marge_sur_EBIT"                        
               , "Rendement_sur_cap_prop"                
               , "Rendement_sur_actif"                   
               , "Croissance_adj_des_Ben_ann"         
               , "Croissance_tot_actif"                  
               , "ratio_ben_avt_impot_sur_frais_int"     
               , "ratio_tot_dette_sur_tot_actif"         
               , "ratio_tot_liab_sur_tot_actif"          
               , "ratio_B_non_rep_sur_Tot_actif"         
               , "ratio_Flux_de_TR_expl_sur_passif_cour" 
               , "ratio_actuel"                          
               , "Ratio_de_liquid_reduite"            
               , "Ratio_de_liquidite"                 
               , "Ratio_Fonds_de_roulmt_sur_ventes"      
               , "ratio_Fonds_de_roulement_sur_tot_actif"
               , "Total_actif"                           
               , "Marge_operationnelle"               
               , "Beta_applique"                         
                ,"volalitilite_30_jours")

colnames(base_de_donnees_2018) <- col_2018









colnames(base_de_donnees_2019)

col_2019 <- c("binaire_cote_credit_2019"              
              , "Cote_credit"                        
              , "Marge_sur_EBITDA"                      
              , "Marge_sur_EBIT"                        
              , "Rendement_sur_cap_prop"                
              , "Rendement_sur_actif"                   
              , "Croissance_adj_des_Ben_ann"         
              , "Croissance_tot_actif"                  
              , "ratio_ben_avt_impot_sur_frais_int"     
              , "ratio_tot_dette_sur_tot_actif"         
              , "ratio_tot_liab_sur_tot_actif"          
              , "ratio_B_non_rep_sur_Tot_actif"         
              , "ratio_Flux_de_TR_expl_sur_passif_cour" 
              , "ratio_actuel"                          
              , "Ratio_de_liquid_reduite"            
              , "Ratio_de_liquidite"                 
              , "Ratio_Fonds_de_roulmt_sur_ventes"      
              , "ratio_Fonds_de_roulement_sur_tot_actif"
              , "Total_actif"                           
              , "Marge_operationnelle"               
              , "Beta_applique"                         
              , "volalitilite_30_jours")

colnames(base_de_donnees_2019) <- col_2019



#rm(list = ls())
summary(base_de_donnees_2019)
fix(base_de_donnees_2019)

# base de données avec vol 

#write.csv(base_de_donnees_2000, file = "base_de_donnees_2000.csv")
#write.csv(base_de_donnees_2007, file = "base_de_donnees_2007.csv")
#write.csv(base_de_donnees_2008, file = "base_de_donnees_2008.csv")
#write.csv(base_de_donnees_2009, file = "base_de_donnees_2009.csv")
#write.csv(base_de_donnees_2016, file = "base_de_donnees_2016.csv")
#write.csv(base_de_donnees_2018, file = "base_de_donnees_2018.csv")
#write.csv(base_de_donnees_2019, file = "base_de_donnees_2019.csv")


