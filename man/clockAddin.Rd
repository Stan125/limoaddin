\name{clockAddin}
\alias{clockAddin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Clock Function}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
clockAddin()
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
function () 
{
    ui <- miniPage(gadgetTitleBar("Clock"), miniContentPanel(uiOutput("time")))
    server <- function(input, output, session) {
        clockStyles <- paste("border: 1px solid #DADADA", "background-color: #EFEFEF", 
            "border-radius: 5px", "font-size: 6em", "margin-top: 60px", 
            "text-align: center", sep = "; ")
        invalidatePeriodically <- reactiveTimer(intervalMs = 1000)
        observe({
            invalidatePeriodically()
            time <- Sys.time()
            output$time <- renderUI({
                p(style = clockStyles, time)
            })
        })
        observeEvent(input$done, {
            timeText <- paste0("\"", as.character(Sys.time()), 
                "\"")
            rstudioapi::insertText(timeText)
            stopApp()
        })
    }
    viewer <- dialogViewer(dialogName = "Clock Add-In", height = 600, 
        width = 1200)
    runGadget(ui, server, viewer = viewer)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
