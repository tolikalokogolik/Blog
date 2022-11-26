##### SETUP -----
library(gtrendsR)
library(readr)
#library(kaggler)
library(RSelenium)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lemon)
library(scales)
library(ggrepel)
library(wordcloud)
library(RColorBrewer)
library(tidyverse)
library(tidygraph)
library(ggraph)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

GRAY1 = "#231e20"
GRAY2 = "#414040"
GRAY3 = "#555655"
GRAY4 = "#646369"
GRAY5 = "#76787B"
GRAY6 = "#828282"
GRAY7 = "#929497"
GRAY8 = "#a6a6a5"
GRAY9 = "#bfbebe"
BLUE1 = "#164a7e"
BLUE2 = "#4a81bf"
BLUE3 = "#93b2d7"
BLUE4 = "#94afc5"
BLUE5 = "#21435d"
BLUE6 = "#95b3d7"
RED1 = "#c3514e"
RED2 = "#e6bab7"
RED3 = "#800d00"
GREEN1 = "#0c8040"
GREEN2 = "#9abb59"
GREEN3 = "#31859c"
GREEN4 = "#4bacc5"
GREEN5 = "#93cddd"
ORANGE1 = "#f79747"
ORANGE2 = "#fac090"

theme_swd <- function() {
  theme_minimal(base_size = 11, base_family = "Helvetica") +
    theme(
      panel.grid.major = element_line(size = 0.1, color = GRAY9),
      panel.grid.minor = element_blank(),
      axis.line = element_line(size = .13, color = GRAY8),
      axis.text = element_text(color = GRAY7),
      axis.ticks.x = element_line(size = 0.5, color = GRAY8),
      axis.ticks.y = element_line(size = 0.5, color = GRAY8),
      axis.title = element_text(color = GRAY3),
      axis.title.y = element_text(hjust = 1, margin = margin(0, 6, 0, 15, "pt")),
      axis.title.x = element_text(hjust = 0, margin = margin(6, 0, 15, 0, "pt")),
      plot.subtitle = element_text(color = GRAY4, size= 11),
      plot.title = element_text(color = GRAY4, size= 15),
      plot.title.position = "plot", # This aligns the plot title to the very left edge
      plot.caption = element_text(hjust = 0, color = GRAY6),
      plot.caption.position = "plot",
      plot.margin = margin(.5,.5,.5,.5,"cm"),
      strip.text = element_text(color = GRAY7)) 
}


##### DATA INGESTION -----

df_google <- gtrends(c("data science"),geo=c("EE"))

# kgl_auth(creds_file = 'C:/Users/natali00/Downloads/kaggle.json')
# response <- kgl_datasets_download_all(owner_dataset = "bryanb/aiml-salaries")
# 
# download.file(response[["url"]], "data/temp.zip", mode="wb")
# unzip_result <- unzip("data/temp.zip", exdir = "data/", overwrite = TRUE)
df_salaries <- read_csv("data/salaries.csv")

url <- "https://www.linkedin.com/jobs/search/?keywords=data%20scientist&location=Estonia"
# 
# ((paragraphs <- read_html(url) %>% html_nodes('ul'))[[7]] %>% html_nodes('a')) %>% html_text()
# 
# url2 <- "https://www.linkedin.com/jobs/search/?keywords=data%20scientist&location=Estonia"
# 
# ((paragraphs <- read_html(url2) %>% html_nodes('ul'))[[7]] %>% html_nodes('a')) %>% html_text()
# (paragraphs <- read_html(url2) %>% html_nodes('span')) 
# data[[66]] %>% html_elements("span")
# data <- (paragraphs <- read_html(url2) %>% html_nodes('li')) 
# for (i in 1:length(data)){
#   #print(data[[i]]%>% html_attrs())
#   # if (grepl("40",data[[i]]%>% html_, fixed = TRUE)){
#   #   print(i)
#   # }
#   attrs <- data[[i]]%>% html_attrs()
#   if (any(grepl("aria-current", attrs)) & any(grepl("aria-label", attrs)) & any(grepl("type", attrs))){
#     print(i)
#   }
# }  
# 
# read_html(url2) %>% html_elements(xpath="/html/body/div[6]") %>% html_text()
# 
# url3 = "https://www.linkedin.com/jobs/search/?keywords=data%20scientist&location=Estonia&position=1&pageNum=2"
# length((read_html(url3) %>%  html_nodes('ul'))[[7]] %>% html_nodes('a') %>% html_text)
# (read_html(url3) %>%  html_nodes('h1')) %>% html_text()

###get all possible data before
rD <- rsDriver(port = 4567L,
                          browser = "firefox",
                          extraCapabilities = list(
                            "moz:firefoxOptions" = list(
                              args = list('--headless')
                            )
                          )
)
remDr <- rD$client
remDr$open()

remDr$navigate(url)

bodyEl <- remDr$findElement("css", "body")
for (i in 1:30) {
  bodyEl$sendKeysToElement(list(key = "end"))
  Sys.sleep(5)
}

lol <- remDr$findElements(using = "xpath", "/html/body/div[3]/div/main/section[2]/ul")
#lol$findChildElement("h3")
df_linkedin <- tolower(unlist(lapply(lol, function(x) {x$getElementText()})))

library(stringr)

df_linkedin <- df_linkedin %>% str_split("\n") %>% data.frame()

colnames(df_linkedin) <- "output"

df_linkedin <- df_linkedin %>% 
  filter(grepl("data scien", output))

##### GRAPHS -----

country = "US"
employment_type_ = c("FT", "CT", "PT", "FL")
work_year_ = "all"
company_size_ = c("S", "L", "M")

sdf_salaries <- df_salaries %>% 
  filter(employee_residence == country,
         employment_type %in% employment_type_,
         company_size %in% company_size_)

if (work_year_ != "all"){
  sdf_salaries2 <- sdf_salaries %>% 
    filter(work_year == work_year_)
} else {
  sdf_salaries2 <- sdf_salaries
}

plot_exp_level_dist <- function(sdf_salaries2){
  plot <- sdf_salaries2 %>% 
    mutate(experience_level = factor(experience_level, levels =  c("EN", "MI", "SE", "EX")),
           color = case_when(experience_level == "EN" ~ BLUE3,
                             experience_level == "MI" ~ BLUE2,
                             experience_level == "SE" ~ BLUE1,
                             T ~ BLUE5)) %>% 
    ggplot(aes(x = experience_level, fill = color)) +
    geom_histogram(stat="count") +
    theme_swd() +
    scale_fill_identity() +
    ylab("Count of workers") +
    xlab("Experience level") +
    ggtitle("Distribution of experience levels")+
    
  
  return(plot)
}

plot_exp_level_dist(sdf_salaries2)

plot_salary_dist <- function(sdf_salaries, sdf_salaries2){
  salary_range <- c(min(sdf_salaries$salary_in_usd),
                    max(sdf_salaries$salary_in_usd))
  
  plot <- sdf_salaries2 %>% 
    ggplot(aes(x = salary_in_usd, y = ..density.. , fill = BLUE2)) +
    geom_histogram() + 
    geom_density(linewidth = 1.2,
                 linetype = 2,
                 colour = BLUE5,
                 alpha=0.3,
                 fill = BLUE5) +
    xlim(salary_range) +
    theme_swd() +
    scale_fill_identity() +
    ylab("Density") +
    xlab("Salary") +
    ggtitle("Distribution of salaries") +
    scale_x_continuous(labels = label_dollar())
  
  return(plot)
  
}

plot_salary_dist(sdf_salaries, sdf_salaries2) 





library(rlang)

level = "all"

if (level != "all"){
  sdf_salaries3 <- sdf_salaries %>% 
    filter(experience_level == level)
} else {
  sdf_salaries3 <- sdf_salaries
}

sdf_salaries3 %>% 
  group_by(work_year) %>% 
  summarise(mean_salary = mean(salary_in_usd),
            n = n()) %>% 
  gather(key = key, value = value, -work_year) %>% 
  spread(work_year, value) %>% 
  mutate(change20.21 = case_when(key == "mean_salary" ~ -100 + !!sym('2021')/!!sym('2020')*100,
                                 T ~ !!sym('2021') - !!sym('2020')) ,
         change21.22 = case_when(key == "mean_salary" ~ -100 + !!sym('2022')/!!sym('2021')*100,
                                 T ~ !!sym('2022') - !!sym('2021'))) 






plot_wordcloud <- function(sdf_salaries2){
  words <- sdf_salaries2 %>% 
    group_by(job_title) %>% 
    summarise(n = n())
  
  plot <- wordcloud(words = words$job_title, 
            freq = words$n, 
            colors=brewer.pal(9,"Blues")[5:length(brewer.pal(9,"Blues"))],
            random.order=FALSE,
            rot.per=0.35,
            min.freq = 1)
  
  return(plot)
}


plot_wordcloud(sdf_salaries2)




job_title = unique(sdf_salaries$job_title)[10]
possibleTime = c("now 1-H", "now 4-H", "now 1-d",
                 "now 7-d", "today 1-m", "today 3-m",
                 "today 12-m", "today+5-y", "all")
time_ = possibleTime[9]

df_google <- gtrends(job_title,
                     geo = country,
                     time = time_)

plot_search_time <- function(data){
  plot <- data  %>% 
    ggplot(aes(x=date, y=hits, color=BLUE5)) +
    geom_line() +
    theme_swd() +
    scale_color_identity()+
    ylab("Hits") +
    xlab("Date") +
    ggtitle("Google search engine usage")
  
  return(plot)
}


plot_search_time(df_google[[1]])

plot_search_related_topics <- function(data, job_title){
  node <- data.frame(topics = c(unique((data %>% 
                                          filter(!is.na(as.numeric(subject))))$value), 
                                job_title))
  
  edge <- data %>% 
    filter(!is.na(as.numeric(subject))) %>% 
    mutate(subject = as.numeric(subject)) %>% 
    rename(from = keyword,
           to = value) %>% 
    select(from, to, subject)
  
  g <- tbl_graph(nodes = node,
                 edges = edge,
                 node_key = "topics",
                 directed = F)
  
  plot <- ggraph(g, layout = "kk") +
    geom_node_point() +
    geom_edge_link(aes(width = subject, color = subject))+
    geom_node_label(aes(label = topics)) +
    xlim(-4,5) +
    theme_void() +
    theme(legend.position = "None") +
    scale_edge_color_gradient(low = BLUE3, high = BLUE1)
  
  return(plot)
}


plot_search_related_topics(df_google[["related_topics"]], job_title)