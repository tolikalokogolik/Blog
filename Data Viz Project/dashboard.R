library(tidyverse)
library(shiny)
library(DT)
library(leaflet)
library(plotly)
library(shinydashboard)
library(ggrepel)

ui = dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      box(
        width = 4,
        
      ),
      column(
        width = 8,
        box(width = 12),
        box(width = 12)
      )
    ),
    fluidRow(
      box(
        width = 6
      ),
      column(
        width = 6,
        tabsetPanel(
          tabPanel(
            "Tab 1",
            p("Here is Tab 1 information")
          ),
          tabPanel(
            "Tab 2",
            p("Here is Tab 2 information")
          )
        )
      )
      
    ),
    fluidRow(
      width = 12,
      box(
        column(
          width = 12
        ),
        column(
          width = 12,
          tabsetPanel(
            tabPanel(
              "Tab 1",
              p("Here is Tab 1 information")
            ),
            tabPanel(
              "Tab 2",
              p("Here is Tab 2 information")
            )
          )
        )
      ),
      
    )
  )
)

server = function(input, output){
  
}

shinyApp(ui = ui, server = server)
