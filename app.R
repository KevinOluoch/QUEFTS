#
# QUEFTS Shiny App

library(shiny)
library(shinyjs)
library(shinydashboard)

ui = dashboardPage(
  
  dashboardHeader(title = 'QUEFTS MODEL'),
  
  dashboardSidebar(
    
    useShinyjs(),
    
    sidebarMenu( 
      # The Inputs to the Model
      h2("INPUTS"),
      div( style = "background-color: rgb(120, 120, 120);color:black;height:120px",
           h4("1 Farm Information"),
           numericInput(inputId = 'in11', label = "1.1 Farm Size (HA)", value = 10, min = 0, max = NA, step = NA)
           ),
      
      div( style = "background-color: rgb(180, 180, 180);color:black;height:300px",
           h4("2 Site Soil Nutrients"),
           numericInput(inputId = 'in21', label = "2.1 soilC (g/kg)", value = 300, min = 0, max = NA, step = NA),
           numericInput(inputId = 'in22', label = "2.2 soilPolsen (p-OLSEN, mg/kg)", value = 300, min = 0, max = NA, step = NA),
           numericInput(inputId = 'in23', label = "2.3 soilK (exchangeable K, mmol/kg)", value = 300, min = 0, max = NA, step = NA)
      ),
      div(
           h4("3 Fertilizer Information"),
           selectInput(inputId = 'in31', label = '3.1 Fertilizer Mass Fraction', 
                  choices = c("NA", "Fert 1", "Fert 2", "Fert 3") ),
           numericInput(inputId = 'in32', label = "3.2 Fertilizer 1 Quatity (KG)", value = 300, min = 0, max = NA, step = NA),
           numericInput(inputId = 'in33', label = "3.3 Fertilizer 2 Quatity (KG)", value = 300, min = 0, max = NA, step = NA),
           numericInput(inputId = 'in34', label = "3.4 Fertilizer 3 Quatity (KG)", value = 300, min = 0, max = NA, step = NA),
           
           numericInput(inputId = 'in35', label = "3.5 Cost ($)", value = 100, min = 0, max = NA, step = NA)
      ),
      
      div( style = "background-color: rgb(240, 240, 240);color:black;height:200px",
           h4("4 Maize Information"),
           numericInput(inputId = 'in41', label = "4.1 Attainable Yield (KG)", value = 20, min = 0, max = NA, step = NA),
           numericInput(inputId = 'in42', label = "4.2 Price (per HA)", value = 30, min = 0, max = NA, step = NA)
      ),
      
      # Auto hide advanced options unless needed
      a(id = "AdvancedOptions", "5 Advanced Options"),
      shinyjs::hidden(
        div(id = "advanced",
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
      verbatimTextOutput('ex_out')
    )
  )


server = function(input, output) {
  # Show/Hide advanced options
  shinyjs::onclick("AdvancedOptions", toggle(id = "advanced", anim = TRUE))
  
  
  runmodel <- eventReactive(input$run, {
    str(sapply(sprintf('in%d', 1:6), function(id) {
      input[[id]]
    }))
  })
  
  output$ex_out <- renderPrint({runmodel()})
  
}


# Run the application 
shinyApp(ui = ui, server = server)

