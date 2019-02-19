#
# QUEFTS Shiny App

library(shiny)
library(shinyjs)
library(shinydashboard)

library(raster)
library(magrittr)


ui = dashboardPage(
  
  dashboardHeader(title = 'QUEFTS MODEL', titleWidth = 400),
  
  dashboardSidebar(
    
    useShinyjs(),
    width = 400,
    sidebarMenu(
      
      # The Inputs to the Model
      h2("INPUTS"),
      #Farm Information
      div( style = "display:inline;blockbackground-color: rgb(120, 120, 120);color:white;height:120px;width:400px",
           h4("1 Farm Information"),
           numericInput(inputId = 'in11', label = "1.1 Farm Size (HA)", 
                                 value = 10, min = 0, max = NA, step = NA)
           ),
      
      # div( style = "background-color: rgb(180, 180, 180);color:black;height:400px;width:400px",
      #      h4("2 Site Soil Nutrients"),
      #      numericInput(inputId = 'in21', label = "2.1 soilC (g/kg)", value = 20, min = 0, max = NA, step = NA),
      #      numericInput(inputId = 'in22', label = "2.2 soilPolsen (p-OLSEN, mg/kg)", value = 5, min = 0, max = NA, step = NA),
      #      numericInput(inputId = 'in23', label = "2.3 soilK (exchangeable K, mmol/kg)", value = 3, min = 0, max = NA, step = NA),
      #      numericInput(inputId = 'in24', label = "2.4 soilPH/acidity ", value = 5.8, min = 0, max = NA, step = NA)
      #       
      # ),
      div( style = "width:400px",
           h4("3 Fertilizer Information"),
           
           checkboxGroupInput(inputId = 'in31', label = '3.1 Fertilizer Mass Fraction (N,P,K)',
                              choices = list("NPK (0.14, 0.061, 0.116)" = "NPK",
                                             "UREA (0.46, 0.0, 0.0)" = "UREA",
                                             "Fert 3 (.180, .209, 0)" = "Fert_3"
                              ),
                              #fert_list,
                              selected = "NPK (0.14, 0.061, 0.116)"
                              ),
           
           shinyjs::hidden(
             div( style = "background-color: rgb(180, 180, 180);height:80px;width:300px", id = "NPK",
                  numericInput(inputId = 'in39', label = "NPK Quantity (KG)", value = 130, min = 0, max = NA, step = NA)
                  )
           ),
           shinyjs::hidden(
             div( style = "background-color: rgb(180, 180, 180);height:80px;width:300px", id = "UREA",
                  numericInput(inputId = 'in310', label = "UREA Quantity (KG)", value = 10, min = 0, max = NA, step = NA)
                  )
           ),
           shinyjs::hidden(
             div( style = "background-color: rgb(180, 180, 180);height:80px;width:300px", id = "Fert_3",
                  numericInput(inputId = 'in311', label = "Fert_3 Quantity (KG)", value = 10, min = 0, max = NA, step = NA)
                  )
             )
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
    ),
    
    # SHOW input value in the meantime
  dashboardBody(
      
      helpText('The input Values'),
      verbatimTextOutput('ex_out'),
      plotOutput('yield_est')
    )
  )



server = function(input, output, session) {
  source('QUEFTS.R')
  source('nutrientUPT.R')
  source('combine_yields.R')
  
  fert_list <- list("NPK (0.14, 0.061, 0.116)" = "NPK",
                    "UREA (0.46, 0.0, 0.0)" = "UREA",
                    "Fert 3 (.180, .209, 0)" = "Fert_3"
  )
  
  
  # Show/Hide advanced options
  shinyjs::onclick("AdvancedOptions", toggle(id = "advanced", anim = TRUE))
  # Show/Hide add fertilizer options
  shinyjs::onclick("add_fert", toggle(id = "Add_Fertilizer", anim = TRUE))
  
  # Show/Hide Fertilizer Quantity options
  observe({ toggle(id = "NPK", condition = {"NPK" %in% input$in31} )})
  observe({ toggle(id = "UREA", condition = {"UREA" %in% input$in31} )})
  observe({ toggle(id = "Fert_3", condition = {"Fert_3" %in% input$in31} )})
  
  
  observeEvent(input$Add, {
    fert_list[paste0(input$in32, " (", input$in33, ", ", input$in34, ", ", input$in35, ")")] <-  input$in32
    updateCheckboxGroupInput(session, "in31", choices = fert_list )
  })
  
  seeinputs <- eventReactive(input$run, {
    str(sapply(sprintf('in%d', c(11, 21:24, 31:34, 39, 310, 311, 41:42)), function(id) {
      input[[id]]
    }))
  })

  
  runmodel <- eventReactive(input$run,{
      # library(raster)  library(magrittr)
      
      # Model Functions
      source('QUEFTS.R')
      source('nutrientUPT.R')
      source('combine_yields.R')
      
      # Paths to Soil nutrients information (Rasters)
      soilC <- raster('data/TZA_ORCDRC_T__M_sd1_1km.tif')
      soilPolsen <- soilC
      soilPolsen[] <- 15
      soilK <- raster('data/TZA_EXKX_T__M_xd1_1km.tif')
      soilpH <- raster('data/TZA_PHIHOX_T__M_sd1_1km.tif') / 10
      
      # Crop predictions
      WY <- raster('data/TZAgeosurvey_h2o_crp_predictions.tif') %>% resample(soilC) #'data/africa_1km_crop_TZA.tif'
      WY[WY < 0.8] <- NA   #remove low probability areas
      WY[WY >= 0.8] <- 12000   #convert to WY. Should be taken from GYGA
      
      # Load the data
      rasters_input <-
        sapply(list(soilC, soilPolsen, soilK, soilpH, WY), getValues) %>% cbind(index =
                                                                                  1:(nrow(.)))
      rasters_input <-
        rasters_input[complete.cases(rasters_input), ]  #compute only values with all data
      colnames(rasters_input) <-
        c('soilC', 'soilPolsen', 'soilK', 'soilpH', 'WY', 'index')
      
      # Get the fertilizer amounts and the added fertilizers mass fractions
      # fert_massfrac1 <-
      #   c(0.14, 0.061, 0.116)       #Mass fraction for NPK (0.14, 0.061, 0.116) and Urea (0.46, 0.0, 0.0)
      # fert_massfrac2 <-
      #   c(0.46, 0.0, 0.0)       #Mass fraction for NPK (0.14, 0.061, 0.116) and Urea (0.46, 0.0, 0.0)
      # fert_amt1 <-  c(50)
      # fert_amt2 <-  c(10)
      #
      
      fert_amtNPK <- fert_amtUREA <- fert_amtFert_3 <- c(0)
      fert_massfracNPK <- fert_massfracUREA <- fert_massfracFert_3 <- c(0,0,0)
      
      if ("NPK" %in% input$in31){
        fert_amtNPK <- c(input$in39)
        fert_massfracNPK <- c(0.14, 0.061, 0.116)
        
      }
      if ("UREA" %in% input$in31){
        fert_amtUREA <- c(input$in310)
        fert_massfracUREA <- c(0.46, 0.0, 0.0) 
        
      }
      if ("Fert_3" %in% input$in31){
        fert_amtFert_3 <- c(input$in311)
        fert_massfracFert_3 <- c(.180, .209, 0)
        
      }
      
      
      
      
      
      # Calculate the Nutrients Kg/Ha
      # nutrients_kg.ha <-
      #   fert_amt1 * fert_massfrac1  + fert_amt2 * fert_massfrac2
      
      nutrients_kg.ha <- fert_amtNPK * fert_massfracNPK + 
                         fert_amtUREA * fert_massfracUREA  + 
                         fert_amtFert_3 * fert_massfracFert_3
      
      yields <-
        apply(
          rasters_input,
          FUN = QUEFTS,
          MARGIN = 1,
          nutrients_kg.ha = nutrients_kg.ha
        )
      
      # Plot the results - after converting to raster
      results  <- soilC
      results[] <- NA
      #results[yields[, 'index']] <- yields
      plot(yields)
      #plot(results)
      #plot(soilC)
    })
  
  # Only Respond to the RUN button
  output$yield_est <- renderPlot({runmodel()})
  output$ex_out <- renderPrint({seeinputs()})
  
}


# Run the application 
shinyApp(ui = ui, server = server)

