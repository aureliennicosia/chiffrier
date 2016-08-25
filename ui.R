
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# list.pourcentage<- c(1,1,1,0.4,0.5,0.1,0.2,1)
# prix.coutant = 56
# marge.brute = 80
# nbr.mois <- 7
# nbr.personne <-2
# bon.max <- 5
# list.pourcentage<- c(1,1,1,0.4,0.5,0.1,0.2,1)
# bon.max = 7, method = 'geo'

#bonificiation = cbind(350*seq(1:nbr.mois), seq(from = 0, to = 0.1, length.out = nbr.mois))

sidebar <- dashboardSidebar(
  numericInput("prix.coutant", ("Prix coutant"), value= 20),
  numericInput("marge.brute", ("Marge brute"), value = 80),
  sliderInput("nbr.mois",("Nombre de mois"), value = 5, min = 1, max = 10 ),
  sliderInput("bon.max",("Nombre de generation de royaute maximum sans bonification"), value = 5, min = 1, max = 10 ),
  sliderInput("nbr.personne",("Nombre de personnes ajoutes par mois"), value = 2, min = 1, max = 5 ),
  selectInput("method", 
                     label = ("Methode de la repartition des royautes"), 
                     choices = list("geometrique" = "geo", 
                                    "standard" = "normal"),
                     selected = "geo")
  )



# Simple header -----------------------------------------------------------
dashboardPage(
  dashboardHeader(title = "Chiffrier by Aurelien Nicosia", titleWidth = 450),
  dashboardSidebar(sidebar ), #,titleWidth = 250
  dashboardBody(
    # Boxes need to be put in a row (or column)
    
   
              
      
      fluidRow( 
           box(
             title = "Bonification",width= 6,collapsible= TRUE,status = "warning", solidHeader = TRUE, br(),
             tabPanel("boni",
             numericInput("valeur.1", "Volume d'affaire genere pour avoir 1% de bonification", value = 10000)
             )
           ),
           box(
             title = "Pourcentage",width= 6,collapsible= TRUE,status = "warning", solidHeader = TRUE, br(),
             tabPanel("Produit",
                      numericInput("p.profit", ("Pourcentage profit compagnie et administration"), value = 40),
                      numericInput("p.royaute", ("pourcentage royaute"), value = 50),
                      numericInput("p.bonification", ("Pourcentage bonification"), value = 10),
                      numericInput("p.comission", ("Pourcentage comission directe sur prix de vente"), value = 20)
             )
           )),
  
      fluidRow( box(
        title = "Information sur le produit",width= 4,collapsible= TRUE,status = "primary", solidHeader = TRUE, br(), 
        tableOutput("df.produit")),
             box(title= "Royaute" ,width= 8,statut = "info",collapsible= TRUE, solidHeader = TRUE, br(),
                 DT::dataTableOutput("df.royaute"))
      
           
           
     ))
    )
  
