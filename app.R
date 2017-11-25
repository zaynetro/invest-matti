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
        h3("Alternative 1"),
        fluidRow(
          plotOutput("alt1_line_graph", height = 350)
        ),
        h3("Alternative 2"),
        fluidRow(
          plotOutput("alt2_line_graph", height = 350)
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

get_investment <- function(principle, years, vector_length= 365, interest_rate = 0.04, method='test') {
  return(seq(principle, (principle*(1+(interest_rate/vector_length))^(years*vector_length)), length.out = years*vector_length))
}

get_investment2 <- function(principle, years, monthly, vector_length= 365, interest_rate = 0.03, method='test') {
  fund <- principle
  inves <- numeric()
  for(i in 1:(years*vector_length%/%30)) {
    inves <- c(inves, seq(fund, (fund*(1+(interest_rate/30))^(1/12*30)), length.out = 30))
    fund <- inves[length(inves)] + monthly
  }
  remaining_length <- years*vector_length %% 30
  inves <- c(inves, seq(fund, (fund*(1+(interest_rate/remaining_length))^(1/12*remaining_length)), length.out = remaining_length))
  print("investment summary")
  print(summary(inves))
  print(length(inves))
  return(inves)
}

server <- function(input, output) {
  set.seed(6)
  vector_length <- 365
  years <- 20
  actual <- c(6000)
  for(i in 2:vector_length) {
    actual <- c(actual, (actual[length(actual)] +
                           sample(c(0, rnorm(1, 0, 1)[1] * 50),
                                  1,
                                  runif(1,0.7,0.9)))
                )
  }
  actual <- c(actual, rep(actual[vector_length], times=(years*vector_length)))
  investment <- get_investment(principle = actual[vector_length],
                               years = years,
                               vector_length = vector_length)
  print(length(investment))
  timestamps <- seq( as.Date("2016-07-01"), by=1, len=((years+1)*vector_length))
  finances <- data.table(date = rep(x = c(timestamps, timestamps[(vector_length+1):length(timestamps)])), 
                         balance=c(actual, investment),
                         type=c(rep("Current", times=((years+1)*vector_length)), 
                                rep("Investment", times=(years*vector_length))))
  ########################
  
  actual <- c(4000)
  for(i in 2:vector_length) {
    actual <- c(actual, (actual[length(actual)] +
                           sample(c(0, rnorm(1, 0, 1)[1] * 50),
                                  1,
                                  runif(1,0.7,0.9)))
    )
  }
  monthly <- 300
  in_year <- 12 * monthly
  actual <- c(actual, seq(actual[vector_length], (actual[vector_length]+years*in_year), 
                          length.out=(years*vector_length)))
  print("actual length")
  print(length(actual))
  investment <- get_investment2(principle = actual[vector_length],
                               years = years,
                               monthly = monthly,
                               vector_length = vector_length)
  print("investment length")
  print(length(investment))
  
  finances2 <- data.table(date = rep(x = c(timestamps, timestamps[(vector_length+1):length(timestamps)])), 
                         balance=c(actual, investment),
                         type=c(rep("Current", times=((years+1)*vector_length)), 
                                rep("Investment", times=(years*vector_length))))
  print(finances2)
  output$alt1_line_graph <- renderPlot({
    print(
      ggplot(data = finances, aes(x=date, y=balance, color=type, linetype=type)) + 
        geom_line() +
        labs(x="Date", y="Total")
    )
  })
  
  output$alt2_line_graph <- renderPlot({
    print(
      ggplot(data = finances2, aes(x=date, y=balance, color=type, linetype=type)) + 
        geom_line() +
        labs(x="Date", y="Total")
    )
  })
}

shinyApp(ui, server)