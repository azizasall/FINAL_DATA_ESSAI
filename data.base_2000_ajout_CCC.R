# new base de données pour 2000 ajout de CCC



# good name
fix(variables_2000_1)
variables_2000_1$binaire_cote_credit_2000
variables_2000_1$Cote_credit

#-------->
#to delete

fix(variables_2000_1)
variables_2000_1 <- variables_2000_1[c(-1, -2)]
fix(variables_2000_1)
dim(variables_2000_1)
variables_2000_1$binaire_cote_credit_2000

colnames(variables_2000_1)




cote_credit_new <- read.csv("Cote_crédit_2000.csv", 
                            header = TRUE, sep = ",", quote = "\"",
                            dec = ".", fill = TRUE, comment.char = "")

cote_credit_new <- rbind(cote_credit_new, "B", "B")

colnames(cote_credit_new) <- c("X", "Cote_credit_2000")

Cote_credit_2000_a <- cote_credit_new$Cote_credit_2000
Cote_credit_2000_a

table(cote_credit_2000_a)




# transformons cote de credit new en binaire
binari <- c("AAA"="firm_inv", "AA"="firm_inv", "A"="firm_inv", "BBB"="firm_inv",
            "BB" = "firm_spec", "B"="firm_spec", "CCC"="firm_spec")


credit_credit_bin_2000 <- binari[Cote_credit_2000_a]

credit_credit_bin_2000


table(credit_credit_bin_2000)
table(Cote_credit_2000_a)

Cote_credit_2000_a
credit_credit_bin_2000

fix(variables_2000_1)
variables_2000_1 <- variables_2000_1[c(-1,-2)]
fix(variables_2000_1)


#good names
binaire_cote_credit_2000 <- credit_credit_bin_2000
Cote_credit <- Cote_credit_2000_a



variables_2000_1 <- cbind(binaire_cote_credit_2000, Cote_credit, variables_2000_1)
fix(variables_2000_1)


#write.csv(variables_2000_1, file = "base_de_donnees_2000.csv")




#-------->
