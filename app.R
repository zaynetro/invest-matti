## app.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)
library(data.table)

header <- dashboardHeader(title = "Invest Matti dashboard",
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                         text = "Make an investement already today",
                                         icon = icon("exclamation-triangle"),
                                         status = "warning"
                                       )
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 90, color = "green",
                                                "Plan progress this month"
                                       )
                          ))

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Suggestions", tabName = "suggestions", icon = icon("dashboard"),
           badgeLabel = "new", badgeColor = "green"),
  menuItem("Plan", icon = icon("calendar"), tabName = "plan"),
  absolutePanel(bottom = 0, height = "180px", right = 0, left = 0,
                tags$div(class = "matti-logo"))
))

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(tabName = "suggestions",
        h2("Suggestions"),
        fluidRow(
          plotOutput("balance_line_graph", height = 350)
        )
    ),
    
    tabItem(tabName = "plan",
        h2("Plan")
    )
  )
)
  
ui <- dashboardPage(
  header,
  sidebar,
  body
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