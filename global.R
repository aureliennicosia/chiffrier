


list.of.packages <- c("ggplot2", "devtools", "lubridate", "nlme", 
  "plotly", "shinydashboard", "shiny", "git2r", 'DT')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, 
  "Package"])]
if (length(new.packages)) install.packages(new.packages)
library(devtools)
library(shinyTable)
install_github("trestletech/shinyTable")
library(shiny)
library(shinydashboard)
# chiffrier Guy Beaulieu

if (!require("DT")) install.packages('DT')
library(DT)

source('fonctionUtiles.R')

# 1.data frame sur le produit

print.money <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}
print.pourcentage <- function(x, ...) {
  paste0( formatC(as.numeric(x*100), format="f", digits=2, big.mark=","), '%')
}


update_df.produit <- function( prix.coutant, marge.brute, list.pourcentage){
  df.produit <- data.frame( matrix(NA, nrow = 8, ncol = 2), 
                            row.names = c('Prix coutant du produit', 'Prix de gros',
                                          'Marge brute', 'Profit Compagnie et admin.',
                                          'Royauté', 'Bonification',
                                          'Comission directe', 'Prix de vente'))
  colnames(df.produit) = c('Pourcentage', 'Prix')
  df.produit$Pourcentage <- list.pourcentage
  df.produit['Prix coutant du produit',]$Prix <- prix.coutant
  df.produit['Marge brute',]$Prix <- marge.brute
  df.produit['Prix de gros',]$Prix <- prix.coutant + marge.brute
  df.produit['Profit Compagnie et admin.',]$Prix <- marge.brute*df.produit['Profit Compagnie et admin.',]$Pourcentage
  df.produit['Royauté',]$Prix <- marge.brute*df.produit['Royauté',]$Pourcentage
  df.produit['Bonification',]$Prix <- marge.brute*df.produit['Bonification',]$Pourcentage
  df.produit['Prix de vente',]$Prix <- df.produit['Prix de gros',]$Prix/(1-df.produit['Comission directe',]$Pourcentage)
  df.produit['Comission directe',]$Prix <- df.produit['Prix de vente',]$Prix*df.produit['Comission directe',]$Pourcentage
  return(df.produit)
}

#df.produit <- update_df.produit(df.produit, prix.coutant = 56, marge.brute = 80, list.pourcentage)


pourc.royaute = function(p, nbr.mois, p_royaute) return(sum(p^c(1:nbr.mois))-p_royaute)

update_df.royaute <- function( nbr.mois,prix.coutant, marge.brute, nbr.personne, bon.max, method = c('geo', 'normal'), list.pourcentage){
  
  #if(bon.max > nbr.mois ) stop('le nombre de mois maximum de royauté dois etre inferieur au nombre de mois considéré')
  df.produit <- update_df.produit( prix.coutant = prix.coutant, marge.brute = marge.brute, list.pourcentage = list.pourcentage)
  df.royaute <- data.frame( matrix(NA, nrow = 4 + nbr.mois, ncol = nbr.mois + 1))
  colnames(df.royaute) <- c('Pourcentage royauté', paste("Mois ", 1:nbr.mois, sep=""))
  rownames(df.royaute) <- c('Client père', paste("Génération ", 1:nbr.mois, sep=""), 
                            'Royauté client', 'Total vente', 'Total Marge brute')
  # pourcentage de royraute
  df.royaute['Client père',]$`Pourcentage royauté` <- df.produit['Royauté',]$Pourcentage
  
  
  
  df.royaute[ paste("Génération ", 1:nbr.mois, sep=""),]$`Pourcentage royauté` <- rep(0,nbr.mois)
  if (method == 'geo'){
    solve_pourc.royaute <- uniroot(f = pourc.royaute, interval = c(0,1), nbr.mois = nbr.mois, p_royaute = df.produit['Royauté',]$Pourcentage)
    df.royaute[ paste("Génération ", 1:min(bon.max, nbr.mois), sep=""),]$`Pourcentage royauté` <- solve_pourc.royaute$root^c(1:min(bon.max, nbr.mois));
  } 
  if (method == 'normal') df.royaute[ paste("Génération ", 1:min(bon.max, nbr.mois), sep=""),]$`Pourcentage royauté` <- df.produit['Royauté',]$Pourcentage/min(bon.max, nbr.mois);
  
  # nombre de personnes par mois
  df.royaute[ paste("Génération ", 1:nbr.mois, sep=""),paste("Mois ", 1:nbr.mois, sep="")] <- matrix(0,nbr.mois, nbr.mois)
  
  
  df.royaute['Client père',paste("Mois ", 1:nbr.mois, sep="")] <- 1
  df.royaute['Génération 1',"Mois 1"] <- nbr.personne
  for (i in (2:nbr.mois)){
    df.royaute['Génération 1',paste("Mois ", as.character(i), sep="")] <- df.royaute['Génération 1',paste("Mois ", as.character(i-1), sep="")] +
      nbr.personne
  }
  
  
  
  
  for (j in (2:nbr.mois)){
    
    if (nbr.mois >= 2 ){
      for (i in (j:nbr.mois)){
        df.royaute[paste("Génération ", as.character(j), sep=""),
                   paste("Mois ", as.character(i), sep="")] <-df.royaute[paste("Génération ", as.character(j), sep=""),
                                                                         paste("Mois ", as.character(i-1), sep="")] + 
          nbr.personne*df.royaute[paste("Génération ", as.character(j-1), sep=""),
                                  paste("Mois ", as.character(i-1), sep="")]
      }
    }
  }
  
  
  # calcul royauté +total vente et on regarde pour la bonification
  for (i in (1:nbr.mois)){
    df.royaute['Royauté client',paste("Mois ", as.character(i), sep="")] <-
      df.produit['Marge brute',]$Prix*
      df.royaute[ paste("Génération ", 1:nbr.mois, sep=""),paste("Mois ", as.character(i), sep="")] %*%
      df.royaute[ paste("Génération ", 1:nbr.mois, sep=""),'Pourcentage royauté']
    df.royaute['Total vente',paste("Mois ", as.character(i), sep="")] <- sum( df.royaute[paste("Génération ", 1:nbr.mois, sep=""),paste("Mois ", as.character(i), sep="")])*df.produit['Prix de vente',]$Prix
    df.royaute['Total Marge brute',paste("Mois ", as.character(i), sep="")] <- sum( df.royaute[paste("Génération ", 1:nbr.mois, sep=""),paste("Mois ", as.character(i), sep="")])*df.produit['Marge brute',]$Prix
    
  }
  
  return(df.royaute)
  
}


#update_df.royaute(df.produit,  nbr.mois, nbr.personne, bon.max, method = 'geo')

update_df.royauteBoni <- function(prix.coutant, marge.brute, list.pourcentage, nbr.mois, nbr.personne, bon.max , method = 'geo', bonificiation){
  if (length(bonificiation[,1]) != nbr.mois) stop('l escalier de bonification doit etre composé de', nbr.mois, ' nombres de mois')
  df.royaute_sansBon <- update_df.royaute( prix.coutant = prix.coutant, marge.brute = marge.brute, nbr.mois = nbr.mois,
                                           nbr.personne = nbr.personne, bon.max = bon.max, method = method, list.pourcentage = list.pourcentage)
  
  for ( i in (1:nbr.mois)){
    if (df.royaute_sansBon['Total vente', paste("Mois ", as.character(i), sep="")] > bonificiation[i,1]) {
      bon.max_new = bon.max + 1
      list.pourcentage_new = list.pourcentage
      list.pourcentage_new[5] = list.pourcentage[5] + bonificiation[i,2]
      df.produit_new <-update_df.produit( prix.coutant = prix.coutant, marge.brute = marge.brute, list.pourcentage = list.pourcentage_new )
      df.royaute_sansBon <- update_df.royaute(prix.coutant = prix.coutant, marge.brute = marge.brute, nbr.mois = nbr.mois,
                                              nbr.personne = nbr.personne, bon.max = bon.max_new, method = method, list.pourcentage = list.pourcentage_new)
      
      
      
    }
    
    
  }
  df.royaute_sansBon$`Pourcentage royauté`[c(1:(1+nbr.mois))] = print.pourcentage(df.royaute_sansBon$`Pourcentage royauté`)[c(1:(1+nbr.mois))]
  for( i in (0:2)){ 
    df.royaute_sansBon[dim(df.royaute_sansBon)[1]-i,-1] <- print.money(df.royaute_sansBon[dim(df.royaute_sansBon)[1]-i,-1])
    
  }
  return(df.royaute_sansBon)
}


print.money <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}
print.pourcentage <- function(x, ...) {
  paste0( formatC(as.numeric(x*100), format="f", digits=3, big.mark=","), '%')
}