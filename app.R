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
      div( style = "display:inline;background-color: rgb(120, 120, 120);color:White;width:400px", #;height:120px
           h4("1 Farm Information"),
           numericInput(inputId = 'in11', label = "1.1 Farm Size (HA)", 
                                 value = 10, min = 0, max = 1000, step = NA)
           #h5(".  1.2 Farm Location"),
           # numericInput(inputId = 'in12', label = "Longitude", 
           #              value = 10, min = 0, max = NA, step = NA),
           # numericInput(inputId = 'in13', label = "Latitude", 
           #              value = 10, min = 0, max = NA, step = NA)
           ),
      

      div( style = "width:400px",
           h4("2 Fertilizer Information"),
           
           checkboxGroupInput(inputId = 'in21', label = '2.1 Fertilizer Mass Fraction (N,P,K)',
                              choices = list("NPK (0.14, 0.061, 0.116)" = "NPK",
                                             "UREA (0.46, 0.0, 0.0)" = "UREA",
                                             "Fert 3 (.180, .209, 0)" = "Fert_3"
                              ),
                              #fert_list,
                              selected = "NPK (0.14, 0.061, 0.116)"
                              ),
           
           shinyjs::hidden(
             div( style = "background-color: rgb(180, 180, 180);height:80px;width:300px", id = "NPK",
                  numericInput(inputId = 'in27', label = "NPK Quantity (KG)", value = 130, min = 0, max = NA, step = NA)
                  )
           ),
           shinyjs::hidden(
             div( style = "background-color: rgb(180, 180, 180);height:80px;width:300px", id = "UREA",
                  numericInput(inputId = 'in28', label = "UREA Quantity (KG)", value = 10, min = 0, max = NA, step = NA)
                  )
           ),
           shinyjs::hidden(
             div( style = "background-color: rgb(180, 180, 180);height:80px;width:300px", id = "Fert_3",
                  numericInput(inputId = 'in29', label = "Fert_3 Quantity (KG)", value = 10, min = 0, max = NA, step = NA)
                  )
             )
           ),
      
      numericInput(inputId = 'in310', label = "Total Fertilizer Cost", 
                   value = 10, min = 0, max = NA, step = NA),
      
      div( style = "color:White;width:400px", #;height:200px
           h4("3 Maize Information"),
           numericInput(inputId = 'in31', label = "3.1 Expected Income per KG", value = 20, min = 0, max = NA, step = NA)
      ),
      
      # Auto hide advanced options unless needed
      a(id = "AdvancedOptions", "4 Advanced Options"),
      shinyjs::hidden(
        div( style = "width:400px",
             id = "advanced",
             numericInput(inputId = "in51", label = "4.1 Advanced Option 1", value = 30, min = 0, max = NA, step = NA),
             numericInput(inputId = "in52", label = "4.2 Advanced Option 2", value = 45, min = 0, max = NA, step = NA)
        )
      ),
      actionButton(inputId = 'run', label = "RUN")
      
      
      )
    ),
    
    # SHOW input value in the meantime
  dashboardBody(
      # helpText('The input Values'),
      # verbatimTextOutput('ex_out'),
      # SHOW whole country values in the meantime
      # plotOutput('yield_est')
      plotOutput("farm_yield_est")
    )
  )



server = function(input, output, session) {
  source('QUEFTS.R')
  source('nutrientUPT.R')
  source('combine_yields.R')
  source('load_rasters.R')
  
  fert_list <- list("NPK (0.14, 0.061, 0.116)" = "NPK",
                    "UREA (0.46, 0.0, 0.0)" = "UREA",
                    "Fert 3 (.180, .209, 0)" = "Fert_3"
  )
  
  

  
  # Show/Hide advanced options
  shinyjs::onclick("AdvancedOptions", toggle(id = "advanced", anim = TRUE))
  # Show/Hide add fertilizer options
  shinyjs::onclick("add_fert", toggle(id = "Add_Fertilizer", anim = TRUE))
  
  # Show/Hide Fertilizer Quantity options
  observe({ toggle(id = "NPK", condition = {"NPK" %in% input$in21} )})
  observe({ toggle(id = "UREA", condition = {"UREA" %in% input$in21} )})
  observe({ toggle(id = "Fert_3", condition = {"Fert_3" %in% input$in21} )})
  
  
  # observeEvent(input$Add, {
  #   fert_list[paste0(input$in32, " (", input$in33, ", ", input$in34, ", ", input$in35, ")")] <-  input$in32
  #   updateCheckboxGroupInput(session, "in31", choices = fert_list )
  # })
  
  seeinputs <- eventReactive(input$run, {
    str(sapply(sprintf('in%d', c(11, 21:24, 27:29, 210, 31)), function(id) {
      input[[id]]
    }))
  })

  # Only Respond to the RUN button
  farm_runmodel <- eventReactive(input$run,{
    # long <- input$in12
    # lati <- input$in13
    # a <- sqrt(2500 * input$in11) # Distant to edges from centre - Square farm
    # farm.extent <- extent(long - a, long + a, lati - a, lati + a)
    #
    # # Extract Farm Extent
    # farm.soilC <- raster::crop(soilC, farm.extent)
    # farm.soilPolsen <- raster::crop(soilPolsen, farm.extent)
    # farm.soilK <- raster::crop(soilK, farm.extent)
    # farm.soilpH <- raster::crop(soilpH, farm.extent)
    # farm.WY <- raster::crop(WY, farm.extent)

    # Load the data
    rasters_input <-
        sapply(list(soilC, soilPolsen, soilK, soilpH, WY), getValues) %>% cbind(index =
                                                                                   1:(nrow(.)))
    # # rasters_input <-
    # #   sapply(list(farm.soilC, farm.soilPolsen, farm.soilK, farm.soilpH, farm.WY), getValues) %>% cbind(index =
    # #                                                                                                      1:(nrow(.)))

    rasters_input <-
      rasters_input[complete.cases(rasters_input), ]  #compute only values with all data
    colnames(rasters_input) <-
      c('soilC', 'soilPolsen', 'soilK', 'soilpH', 'WY', 'index')

      fert_amtNPK <- fert_amtUREA <- fert_amtFert_3 <- c(0)
      fert_massfracNPK <- fert_massfracUREA <- fert_massfracFert_3 <- c(0,0,0)

      if ("NPK" %in% input$in21){
        fert_amtNPK <- c(input$in27)
        fert_massfracNPK <- c(0.14, 0.061, 0.116)

      }
      if ("UREA" %in% input$in21){
        fert_amtUREA <- c(input$in28)
        fert_massfracUREA <- c(0.46, 0.0, 0.0)

      }
      if ("Fert_3" %in% input$in21){
        fert_amtFert_3 <- c(input$in29)
        fert_massfracFert_3 <- c(.180, .209, 0)

      }


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
      results[rasters_input[, 'index']] <- yields
    #plot(yields)
    plot(results, main = "Predicted Yield")
      #plot(soilC)
    })
  
  
  output$ex_out <- renderPrint({seeinputs()})
  output$farm_yield_est <- renderPlot({farm_runmodel()})
  
  
  # output$yield_est <- renderPlot({
  #   # Load the data
  #   rasters_input <-
  #     sapply(list(soilC, soilPolsen, soilK, soilpH, WY), getValues) %>% cbind(index =
  #                                                                               1:(nrow(.)))
  # 
  #   rasters_input <-
  #     rasters_input[complete.cases(rasters_input), ]  #compute only values with all data
  #   colnames(rasters_input) <-
  #     c('soilC', 'soilPolsen', 'soilK', 'soilpH', 'WY', 'index')
  #   
  #   fert_amtNPK <- fert_amtUREA <- fert_amtFert_3 <- c(15)
  #   
  #   fert_massfracNPK <- c(0.14, 0.061, 0.116)
  #   fert_massfracUREA <- c(0.46, 0.0, 0.0)
  #   fert_massfracFert_3 <- c(.180, .209, 0)
  #   
  #   nutrients_kg.ha <- fert_amtNPK * fert_massfracNPK + 
  #     fert_amtUREA * fert_massfracUREA  + 
  #     fert_amtFert_3 * fert_massfracFert_3
  #   
  #   yields <-
  #     apply(
  #       rasters_input,
  #       FUN = QUEFTS,
  #       MARGIN = 1,
  #       nutrients_kg.ha = nutrients_kg.ha
  #     )
  #   
  #   # Plot the results - after converting to raster
  #   results  <- soilC
  #   results[] <- NA
  #   results[rasters_input[, 'index']] <- yields
  #   #plot(yields)
  #   #plot(soilC)
  #   plot(results)
  #   }
  #   )
}


# Run the application 
shinyApp(ui = ui, server = server)

