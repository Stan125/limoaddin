\name{hist_addin}
\alias{hist_addin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Histogram Add-In}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hist_addin()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
hist_addin <- function() {
  #.............................
  # PRELIMINARIES
  #.............................
  library(rstudioapi)
  library(dplyr)
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
