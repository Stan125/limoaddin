library(rstudioapi)
library(dplyr)
library(shiny)
library(miniUI)

# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.
sushicat <- function() {
  
  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("Cute Cat"),
    miniContentPanel(img(src = "http://costumewall.com/wp-content/uploads/2015/09/cute-cat-costumes-30.jpg",
                         style = "display: block; margin: 0 auto;"))
  )
  
  server <- function(input, output, session) {
    
    # Set some CSS styles for our clock.
    clockStyles <- paste(
      "border: 1px solid #DADADA",
      "background-color: #EFEFEF",
      "border-radius: 5px",
      "font-size: 6em",
      "margin-top: 60px",
      "text-align: center",
      sep = "; "
    )
    
    observeEvent(input$done, {
      stopApp()
    })
    
    }
  
  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space to display the clock.
  viewer <- dialogViewer(dialogName = "Sushi Cat", 
                         height = 600, 
                         width = 900)
  runGadget(ui, server, viewer = viewer)
}

# Now all that's left is sharing this addin -- put this function
# in an R package, provide the registration metadata at
# 'inst/rstudio/addins.dcf', and you're ready to go!