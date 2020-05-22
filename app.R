#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gridExtra)
library(latex2exp)

# Define UI for application that draws a histogram

ui <- fluidPage(
  # Application title
  titlePanel("Critical Values for Normal and t-Distributions"),
  
  radioButtons(
    "disttype",
    "Distribution type:",
    c(
      "Standard Normal Distribution" = "norm",
      "t Distribution" = "tdist"
    )
  ),
  conditionalPanel(
    condition = "input.disttype == 'tdist'",
    numericInput(
      "degfree",
      "Degrees of freedom:",
      10,
      min = 1,
      max = 100000
    )
  ),
  numericInput("alpha", "alpha", 0.05, min(0.000001, max = 0.99999999)),
  radioButtons(
    "tail",
    "Which tail?",
    c(
      "two-tailed" = "twotail",
      "left tail" = "left",
      "right tail" = "right"
    )
  ),
  plotOutput("distPlot", width = "80%")
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    ddist <-
      switch(
        input$disttype,
        "norm" = dnorm,
        "tdist" = function(x) dt(x = x, df = input$degfree))
    qdist <-
      switch(
        input$disttype,
        "norm" = qnorm,
        "tdist" = function(p) qt(p = p, df = input$degfree))
    
    if (input$tail == "twotail") {
      alpha <- input$alpha / 2
      alphalabel <- paste("Shaded Area = $\\alpha/2$ = ", alpha)
    } else {
      alpha <- input$alpha
      alphalabel <- paste("Shaded Area = $\\alpha$ = ", alpha)
    }
    zneg <- qdist(p = alpha)
    zpos <- qdist(p = 1-alpha)
    lb <- min(-3, 1.3 * zneg)
    ub <- max(3, 1.3 * zpos)
    x <- seq(lb, ub, length.out = 1000)
    df <- data.frame(x = x, y = ddist(x = x))
    
    p <- ggplot(data = df, aes(x = x, y = y)) + geom_line()
    
    if(input$tail == "twotail" | input$tail == "left"){
      loc <- (lb + zneg) / 2
      p <- (p 
            + geom_area(data = subset(df, x < zneg), aes(y = y), fill = "grey", alpha = 0.5)
            + annotate('text', x = loc, y = 0.1,
              label = TeX(alphalabel, output = "character"),
              parse = TRUE)
            + annotate('segment', x = loc, y = 0.09,
                       xend = loc,
                       yend = 0.02,
                       arrow = arrow(length = unit(0.1, "inches")))
            + annotate('text', x = zneg, y = -0.03, label = round(zneg, 2))
            + annotate('segment', x = zneg, y = -0.02, xend=zneg, yend = 0, arrow = arrow(length = unit(0.1, "inches")) )
            
            )
    }
    
    if(input$tail == "twotail" | input$tail == "right"){
      loc <- (ub + zpos) / 2
      p <- (p
            + geom_area(data = subset(df, x >= zpos), aes(y = y), fill = "grey", alpha = 0.5)
            + annotate('text', x = loc, y = 0.1,
              label = TeX(alphalabel, output = "character"),
              parse = TRUE)
            + annotate('segment', x = loc, y = 0.09,
              xend = loc,
              yend = 0.02,
              arrow = arrow(length = unit(0.1, "inches")))
            + annotate('text', x = zpos, y = -0.03, label = round(zpos, 2))
            + annotate('segment', x = zpos, y = -0.02, xend=zpos, yend = 0, arrow = arrow(length = unit(0.1, "inches")) )
            )
    }
    if(input$disttype == "norm"){
      p <- p + xlab("z-value")
    } else if(input$disttype == "tdist"){
      p <- p + xlab("t-value")
    }
    p
  
  })
}


shinyApp(ui, server)
