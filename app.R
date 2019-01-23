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
      
      numericInput(inputId = 'in1', label = "1. Farm Size (HA)", value = 0, min = 0, max = NA, step = NA),
      
      selectInput(inputId = 'in2', label = '2. Fertilizer Mass Fraction', 
                  choices = c("NA", "Fert 1", "Fert 2", "Fert 3", "Fert 4") ),
      
      numericInput(inputId = 'in3', label = "3. Fertilizer Quatity (KG)", value = 0, min = 0, max = NA, step = NA),
      
      numericInput(inputId = 'in4', label = "4. Cost ($)", value = 0, min = 0, max = NA, step = NA),
      
      numericInput(inputId = 'in5', label = "5. Attainable Yield (KG)", value = 0, min = 0, max = NA, step = NA),
      
      numericInput(inputId = 'in6', label = "6. Price (per HA)", value = 0, min = 0, max = NA, step = NA),
      
      # Auto hide advanced options unless needed
      a(id = "AdvancedOptions", "Advanced Options"),
      shinyjs::hidden(
        div(id = "advanced",
            numericInput(inputId = "in7", label = "Advanced Option 1", value = 30, min = 0, max = NA, step = NA),
            numericInput(inputId = "in8", label = "Advanced Option 2", value = 45, min = 0, max = NA, step = NA)
        )
      ),
      actionButton(inputId = 'run', label = "RUN")
      
    )),
    
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

