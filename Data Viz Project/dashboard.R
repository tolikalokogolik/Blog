source("functions.R")

df_salaries <- get_salaries_data()


ui = dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      column(
        width = 4,
        box(
          width = 12,
          selectInput(
            "country", 
            h4("Select country"), 
            choices = unique(df_salaries$employee_residence), 
            selected = unique(df_salaries$employee_residence)[1]
          ),
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
            selected = c("S", "M", "L")
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
          ),
          
        ),
        box(
          width = 12,
          selectInput(
            "job_title", 
            h4("Select job title"), 
            choices = unique(df_salaries$job_title), 
            selected = unique(df_salaries$job_title)[1]
          )
        )
      ),
      
      column(
        width = 8,
        box(
          width = 12,
          plotOutput("plot_exp_level_dist")
        ),
        box(width = 12,
            plotOutput("plot_salaries_dist"))
      )
    ),
    fluidRow(
      width = 12,
      box(
        width = 12,
        column(
          width = 12,
          h3("Google search engine data about selected job and country"),
          selectInput(
            "time", 
            h4("Select search history range"), 
            choices = list(
              "Past hour" = "now 1-H", 
              "Past 4 hours" = "now 4-H",
              "Past day" = "now 1-d",
              "Past 7 days" = "now 7-d",
              "Past 30 days" = "today 1-m",
              "Past 90 days" = "today 3-m",
              "Past 12 months" = "today 12-m",
              "Past 5 years" = "today+5-y",
              "2004 - present" = "all"
            ), 
            selected = "today+5-y"
          ),
          tabsetPanel(
            tabPanel(
              "Searches over time",
              plotOutput("plot_search_time")
            ),
            tabPanel(
              "Related topics",
              plotOutput("plot_search_related_topics")
            )
          )
        )
      )
 
    ),
    fluidRow(
      box(
        width = 6,
        h3("Job titles workcloud in this country"),
        plotOutput("plot_wordcloud")
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
      
    )
    
  )
)

server = function(input, output){
  output$plot_exp_level_dist = renderPlot({
    sdf_salaries = get_salaries_subdata(df_salaries,
                                        input$country,
                                        input$employment_type_,
                                        input$company_size_,
                                        input$work_year_,
                                        input$level)
    
    plot_exp_level_dist(sdf_salaries[[2]])
  })
  
  output$plot_salaries_dist = renderPlot({
    sdf_salaries = get_salaries_subdata(df_salaries,
                                        input$country,
                                        input$employment_type_,
                                        input$company_size_,
                                        input$work_year_,
                                        input$level)
    
    plot_salary_dist(sdf_salaries[[1]],
                     sdf_salaries[[2]])
  })
  
  output$plot_wordcloud = renderPlot({
    sdf_salaries = get_salaries_subdata(df_salaries,
                                        input$country,
                                        input$employment_type_,
                                        input$company_size_,
                                        input$work_year_,
                                        input$level)
    
    plot_wordcloud(sdf_salaries[[2]])
  })
  
  output$plot_search_time = renderPlot({
    df_google <- get_google_data(input$job_title,
                                 input$country,
                                 input$time)
    
    plot_search_time(df_google[[1]])
  })
  
  output$plot_search_related_topics = renderPlot({
    df_google <- get_google_data(input$job_title,
                                 input$country,
                                 input$time)
    
    plot_search_related_topics(df_google[[2]],
                               input$job_title)
  })
}

shinyApp(ui = ui, server = server)
