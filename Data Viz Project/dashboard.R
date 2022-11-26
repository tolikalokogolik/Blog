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
        checkboxGroupInput(
          "employment_type_", 
          h4("Choose employment type"), 
          choices = list("Full-time" = "FT",
                         "Part-time" = "PT",
                         "Contract" = "CT",
                         "Freelance" = "FL"),
          selected = unique(df_salaries$employment_type)
        ),
        checkboxGroupInput(
          "company_size_", 
          h4("Choose company size"), 
          choices = list("less than 50 employees (small)" = "S",
                         "50 to 250 employees (medium)" = "M",
                         "more than 250 employees (large)" = "L"),
          selected = unique(df_salaries$company_size)
        ),
        selectInput(
          "work_year_", 
          h4("Select year"), 
          choices = list(
            "All" = "all", 
            "2022" = 2022,
            "2021" = 2021,
            "2020" = 2020
          ), 
          selected = "all"
        )
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
        box(
          selectInput(
            "level", 
            h4("Select experience level"), 
            choices = list(
              "Entry-level / Junior" = "EN", 
              " Mid-level / Intermediate" = "MI",
              "Senior-level / Expert" = "SE",
              "Executive-level / Director" = "EX"
            ), 
            selected = "all"
          ),
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
        
      )
      
    ),
    fluidRow(
      width = 12,
      box(
        column(
          width = 12,
          selectInput(
            "job_title", 
            h4("Select job title"), 
            choices = unique(df_salaries$job_title), 
            selected = unique(df_salaries$job_title)[1]
          )
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
