

# Setup Shiny app back-end components
# -------------------------------------
server <- function(input, output) {
 
  
  pourcentage <-reactive({
   
    list.pourcentage<- c(1,1,1,input$p.profit/100,input$p.royaute/100,input$p.bonification/100,input$p.comission/100,1)
  })
  
  bonification <- reactive({
    
    bonificiation = cbind(input$valeur.1*seq(1:input$nbr.mois), seq(from = 0, to = input$p.bonification/100, length.out = input$nbr.mois))
    
    
    })
  
  output$df.produit <- renderTable({
    
    #list.pourcentage<- c(1,1,1,0.4,0.5,0.1,0.2,1)
   
    df.produit = update_df.produit( prix.coutant = input$prix.coutant,
                                   marge.brute = input$marge.brute, list.pourcentage = pourcentage() )
      df.produit$Prix <-   print.money(df.produit$Prix)  
      df.produit$Pourcentage <- print.pourcentage(df.produit$Pourcentage)
     return(df.produit)
    
  })


 output$df.royaute <- DT::renderDataTable({
   
   #list.pourcentage<- c(1,1,1,0.4,0.5,0.1,0.2,1)
   
   df.produit = update_df.produit( prix.coutant = input$prix.coutant,
                                   marge.brute = input$marge.brute, list.pourcentage = pourcentage() )
  

 #df.produit = update_df.produit( prix.coutant = input$prix.coutant, marge.brute = input$marge.brute, list.pourcentage = list.pourcentage )

  #up = update_df.royaute(df.produit, input$nbr.mois, input$nbr.personne, input$bon.max, input$method)
   up = update_df.royauteBoni( input$prix.coutant , input$marge.brute , list.pourcentage= pourcentage(), 
                       input$nbr.mois, input$nbr.personne, input$bon.max ,method = input$method ,bonificiation= bonification())
    # df.royaute_sansBon[c(1:(1+input$nbr.mois)),1] = print.pourcentage(df.royaute_sansBon[c(1:(1+input$nbr.mois)),1])
    # for( i in ((input$nbr.mois+1):dim(df.royaute_sansBon)[1])){ 
    #   df.royaute_sansBon[i,-1] <- print.money(df.royaute_sansBon[i,-1])
    #   
    # }
    return(datatable(up, rownames = TRUE ))
 })
 
 output$prix.coutant <- renderValueBox({
   valueBox(
   paste0(input$prix.coutant, "$"), "Prix coutant", icon = icon("list"),
   color = "purple"
   )
 })
 
 
  
}

