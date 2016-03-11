# Addin: Making histograms with some Interactive Elements
hist_addin <- function() {
  #.............................
  # PRELIMINARIES
  #.............................
  library(rstudioapi)
  library(dplyr)
  library(shiny)
  library(miniUI)
  library(ggplot2)
  
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
        selectInput(label = "Select your dataset:",
                    inputId = "dataset",
                    choices = c("", search_df())),
        uiOutput("choices1"),
        checkboxInput(inputId = "density",
                      label = "Plot Density")
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
    
    variable <- reactive({
      validate(
        need(input$variable != "", "Please select a variable"),
        if (input$variable != "") {
          need(class(get(input$dataset)[, input$variable]) == "numeric", "Please pick a numeric variable")
        }
      )
      input$variable
    })
    
    
    output$choices1 <- renderUI({
      col.names <- c("",colnames(data()))
      selectInput(inputId = "variable",
                  label = "Select your variable",
                  choices = col.names)
    })
    
    output$plot1 <- renderPlot({
      ggplot(data = data(),
             aes_string(x = variable())) +
        geom_histogram() +
        theme_bw()
    })
    
    observeEvent(input$done, {
      stopApp()
    })
    
  }

  viewer <- dialogViewer(dialogName = "Histogram Add-In", 
                         height = 600, 
                         width = 900)
  runGadget(ui, server, viewer = viewer)
}

hist_addin()

# Now all that's left is sharing this addin -- put this function
# in an R package, provide the registration metadata at
# 'inst/rstudio/addins.dcf', and you're ready to go!