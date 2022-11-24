library(shiny)
library(tidyverse)

# Dataset containing world university rankings from QS in 2022
rankings <- read.csv("WorldUniRankings2022.csv") %>%
  mutate(Rank = as.numeric(str_replace(rank_display, "=", ""))) %>% # several schools don't have exact ranks
  drop_na()

ui <- fluidPage(titlePanel("World University Rankings"),
                sidebarLayout(
                  sidebarPanel(h2("Filter by ranking"),
                               # Feature 1: A slider that filters for schools within a range of ranks
                               # This allows people to obtain a more relevant list of schools
                               sliderInput("rankingInput", "Rank", 
                                           min = 1, max = 494,
                                           value = c(1, 25))),
                  mainPanel(
                    h2("Number of Schools by Region"),
                    plotOutput("regioncountplot"),
                    br(),
                    h2("Details for Selected Schools"),
                    tableOutput("results")
                  )
                ))

server <- function(input, output) {
  output$regioncountplot <- renderPlot({
    # Feature 2: Plot that shows how many universities of interest are in each geographic region
    # This allows people to see which places in the world have their schools of interest
    filtered <- rankings %>%
      filter(Rank >= input$rankingInput[1],
             Rank <= input$rankingInput[2]) %>%
      group_by(region) %>%
      count()
    ggplot(filtered, aes(x = reorder(region, n), y = n)) +
      geom_bar(stat = "identity") +
      labs(x = "Region", y = "Number of Schools") +
      theme_minimal()
  })
  
  # Feature 3: Table that displays details of selected universities
  # This lets people see the rankings, names, and locations of schools of interest
  output$results <- renderTable({
    selected <- rankings %>%
      filter(Rank >= input$rankingInput[1],
             Rank <= input$rankingInput[2]) %>%
      select(Rank, university, country, region) %>%
      `colnames<-`(c("Rank", "University Name", "Country", "Region"))
    selected
  })
}
shinyApp(ui = ui, server = server)
