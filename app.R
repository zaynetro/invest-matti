## app.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)
library(data.table)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      # box(
        plotOutput("balance_line_graph", height = 350)
        # )
      # ,
      # box(
      #   title = "Controls",
      #   sliderInput("slider", "Number of observations:", 1, 100, 50)
      # )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  actual <- rnorm(100) * 50 + 5000
  investment <- rnorm(100) * 50 + 5000 + cumsum(rnorm(100) * 50 + 10)
  timestamps <- seq( as.Date("2017-07-01"), by=1, len=100)
  finances <- data.table(date = rep(x = timestamps, times=2), 
                         balance=c(actual, investment),
                         type=c(rep("Current", times=100), rep("Investment", times=100)))
  
  output$balance_line_graph <- renderPlot({
    print(
      ggplot(data = finances, aes(x=date, y=balance, color=type, linetype=type)) + 
        geom_line() +
        geom_smooth(method = "lm", formula = y ~ exp(x)) +
        # scale_y_continuous(limits=c(0, 75000)) +
        # scale_color_manual(name="", values=c(prediction="blue", actual="red"), labels=c("Investment Plan", "Current")) +
        # scale_linetype_manual(name="", values=c(prediction=3, actual=1), labels=c("Investment Plan", "Current")) +
        # theme_minimal() +
        labs(x="Date", y="Total")
    )
    # data <- histdata[seq_len(input$slider)]
    # plot(x = timestamps, y=balance, col='red') +
    # lines(x = timestamps, y=balance, col = 'steelblue')
  })
}

shinyApp(ui, server)