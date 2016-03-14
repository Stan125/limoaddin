#------------------------------------------------------------------#
# R-Projekt: Histogram Add-In
# Authors: Stanislaus Stadlmann
# created for STATWORX
#------------------------------------------------------------------#


# Addin: Making histograms with some Interactive Elements
hist_addin <- function() {
  
  #.............................
  # PRELIMINARIES
  #.............................
  library(rstudioapi)
  library(dplyr)
  library(miniUI)
  library(ggplot2)
  library(shiny)
  
  # Function that scans the working space for dataframes
  search_df <- function() {
    # Container
    c <- c()
    
    # Function to tell which place an object has in the workspace 
    w <- function(x) {
      ls <- ls(envir = .GlobalEnv)
      return(which(ls == x))
    }
    
    # Which object is a dataframe?
    for (data in ls(envir = .GlobalEnv)) {
      if (any(class(eval(parse(text = data))) == "data.frame")) {
        c[w(data)] <- data
      }
    }
    
    # Return all non-NA values
    return(c[!is.na(c)])
    
    # Delete the rest
    rm(w)
    rm(c)
  }
  
  # UI
  ui <- miniPage(
    gadgetTitleBar("Interactive Histogram"),
    fillRow(
      miniContentPanel(
        # Select Dataset
        selectInput(label = "Select your dataset:",
                    inputId = "dataset",
                    choices = c("", search_df())),
        
        # Select Variable
        uiOutput("choices1"),
        
        # Plot Density
        checkboxInput(inputId = "density",
                      label = "Plot Density"),
        
        # Adjust #bins
        sliderInput(inputId = "slider",
                    label = "Adjust number of bins",
                    min = 5,
                    max = 50,
                    value = 10)
      ),
      miniContentPanel(plotOutput("plot1"))
    )
  )
  
  
  server <- function(input, output, session) {
    
    # Was a dataset selected?
    data <- reactive({
      validate(
        need(input$dataset != "", "Please select a data set")
      )
      get(input$dataset)
    })
    
    # Was a numeric variable selected?
    variable <- reactive({
      validate(
        if (is.null(input$variable) == FALSE) {
          need(mode(data()[[input$variable]]) == "numeric", "Please pick a numeric variable")
        }
      )
      input$variable
    })
    
    # Render the UI button with variable selections, if dataset is selected
    output$choices1 <- renderUI({
      col.names <- colnames(data())
      selectInput(inputId = "variable",
                  label = "Select your variable",
                  choices = col.names)
    })
    
    # Render Histogram
    output$plot1 <- renderPlot({
      g <- ggplot(data = data(),
                  aes_string(x = variable())) +
        geom_histogram(aes(y = ..density..),
                       stat = "bin",
                       bins = input$slider) +
        theme_bw()
      if (input$density == TRUE) {
        g <- g + geom_density(fill = "firebrick", alpha = .5)
      }
      g
    })
    
    # Stop App if Done Button is pressed
    observeEvent(input$done, {
      stopApp()
    })

  }
  
  # Where should the App be viewn?
  viewer <- dialogViewer(dialogName = "Histogram Add-In", 
                         height = 600, 
                         width = 900)
  runGadget(ui, server, viewer = viewer)
}