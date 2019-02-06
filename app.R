#
# QUEFTS Shiny App

library(shiny)
library(shinyjs)
library(shinydashboard)
source("QUEFTS.R")
source("combine_yields.R")
source("nutrientUPT.R")

ui = dashboardPage(
  
  dashboardHeader(title = 'QUEFTS MODEL', titleWidth = 400),
  
  dashboardSidebar(
    
    useShinyjs(),
    width = 400,
    sidebarMenu(
      #fluidRow(
      
      # tags$head(
      #   tags$style(type="text/css", "label{ display: table-cell; text-align: left; vertical-align: middle; } .form-group { display: table-row;}")
      # ),
      
      # The Inputs to the Model
      h2("INPUTS"),
      div( style = "display:inline;blockbackground-color: rgb(120, 120, 120);color:white;height:120px;width:400px",
           h4("1 Farm Information"),
           numericInput(inputId = 'in11', label = "1.1 Farm Size (HA)", 
                                 value = 10, min = 0, max = NA, step = NA, width = '90px')
           #h6("(HA)")
           
           ),
      
      div( style = "background-color: rgb(180, 180, 180);color:black;height:400px;width:400px",
           h4("2 Site Soil Nutrients"),
           numericInput(inputId = 'in21', label = "2.1 soilC (g/kg)", value = 20, min = 0, max = NA, step = NA),
           numericInput(inputId = 'in22', label = "2.2 soilPolsen (p-OLSEN, mg/kg)", value = 5, min = 0, max = NA, step = NA),
           numericInput(inputId = 'in23', label = "2.3 soilK (exchangeable K, mmol/kg)", value = 3, min = 0, max = NA, step = NA),
           numericInput(inputId = 'in24', label = "2.4 soilPH/acidity ", value = 5.8, min = 0, max = NA, step = NA)
            
      ),
      div( style = "width:400px",
           h4("3 Fertilizer Information"),
           h6("Fertilizer Mass Fraction' is currently inactive", 
              style = "background-color:red;color:black;width:230px"),
           selectInput(inputId = 'in31', label = '3.1 Fertilizer Mass Fraction', 
                  choices = c("NA", "Fert 1", "Fert 2", "Fert 3") ),
           numericInput(inputId = 'in32', label = "3.2 Fertilizer 1 Quantity (KG)", value = 130, min = 0, max = NA, step = NA),
           numericInput(inputId = 'in33', label = "3.3 Fertilizer 2 Quantity (KG)", value = 10, min = 0, max = NA, step = NA),
           numericInput(inputId = 'in34', label = "3.4 Fertilizer 3 Quantity (KG)", value = 10, min = 0, max = NA, step = NA),
           
           numericInput(inputId = 'in35', label = "3.5 Cost ($)", value = 100, min = 0, max = NA, step = NA)
      ),
      
      div( style = "background-color: rgb(240, 240, 240);color:black;height:200px;width:400px",
           h4("4 Maize Information"),
           numericInput(inputId = 'in41', label = "4.1 Attainable Yield (KG)", value = 20, min = 0, max = NA, step = NA),
           numericInput(inputId = 'in42', label = "4.2 Price (per HA)", value = 30, min = 0, max = NA, step = NA)
      ),
      
      # Auto hide advanced options unless needed
      a(id = "AdvancedOptions", "5 Advanced Options"),
      shinyjs::hidden(
        div( style = "width:400px",
             id = "advanced",
             numericInput(inputId = "in51", label = "5.1 Advanced Option 1", value = 30, min = 0, max = NA, step = NA),
             numericInput(inputId = "in52", label = "5.2 Advanced Option 2", value = 45, min = 0, max = NA, step = NA)
        )
      ),
      actionButton(inputId = 'run', label = "RUN")
      )
    #)
    ),
    
    # SHOW input value in the meantime
  dashboardBody(
      
      helpText('The input Values'),
      verbatimTextOutput('ex_out'),
      verbatimTextOutput('yield_est')
    )
  )


server = function(input, output) {
  # Show/Hide advanced options
  shinyjs::onclick("AdvancedOptions", toggle(id = "advanced", anim = TRUE))
  
  
  runmodel <- eventReactive(input$run, {
    str(sapply(sprintf('in%d', c(11, 21:24, 31:34, 41:42)), function(id) {
      input[[id]]
    }))
  })
  
  output$yield_est <- renderText({
    # Inputs
    siteSoilNutrient <- matrix(c(input$in21, input$in22, input$in23, input$in24), ncol = 4, byrow = TRUE)
    colnames(siteSoilNutrient) <- c('soilC', 'soilPolsen', 'soilK', 'soilpH')
    fert_massfrac <-  matrix(c(0.14, 0.061, 0.116, 0.46, 0.0, 0.0, .180, .209, 0), ncol = 3, byrow = TRUE)      #Mass fraction for NPK (0.14, 0.061, 0.116) and Urea (0.46, 0.0, 0.0)
    fert_amt <-  matrix(c(input$in32, input$in32, input$in32), ncol= 3)
    nutrients_kg.ha <- fert_amt %*% t(fert_massfrac)
    
    ad <- matrix(c(26, 180, 24, 60, 540, 96)) #from Sattari 2014
    QUEFTS(siteSoilNutrient = siteSoilNutrient, nutrients_kg.ha = nutrients_kg.ha, ad = ad)
    #siteSoilNutrient[,1]
    #colnames(siteSoilNutrient)
    #siteSoilNutrient[,'soilC']

    })
  
  output$ex_out <- renderPrint({runmodel()})
  
}


# Run the application 
shinyApp(ui = ui, server = server)

