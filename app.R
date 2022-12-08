library(shiny)
library(tidyverse)
library(maps)
library(ggiraph)
library(mapproj)

# Dataset containing world university rankings from QS in 2022
rankings <- read.csv("WorldUniRankings2022.csv") %>%
  mutate(Rank = as.numeric(str_replace(rank_display, "=", ""))) %>% # several schools don't have exact ranks
  drop_na()

# Dataset containing mapping polygons for countries around the world
map_data <- map_data("world")

# Dataset containing longitude and latitude for major cities
cities_data <- world.cities

# Formatting countries to be the same in all datasets
rankings <- mutate(rankings,
                   country = ifelse(country == "Iran, Islamic Republic of", "Iran",
                                    ifelse(country == "China (Mainland)", "China",
                                           country)))
cities_data <- mutate(cities_data,
                      country.etc = ifelse(country.etc == "USA", "United States",
                                    ifelse(country.etc == "UK", "United Kingdom",
                                           country.etc)))

map_data <- mutate(map_data,
                   region = ifelse(region == "USA", "United States",
                                   ifelse(region == "UK", "United Kingdom",
                                          region)))

# List of countries that can be selected in dropdown menu
valid_countries <- intersect(cities_data$country.etc, rankings$country) %>% 
  unique() %>%
  sort()

# rankings with latitude and longitude data 
rankings_loc <- rankings %>%
  inner_join(cities_data, by = c("country" = "country.etc",
                                 "city" = "name"))

ui <- fluidPage(titlePanel("World University Rankings"),
                sidebarLayout(
                  sidebarPanel(h2("Filter by ranking"),
                               # Feature 1: A slider that filters for schools within a range of ranks
                               # This allows people to obtain a more relevant list of schools
                               sliderInput("rankingInput", "Filter by Ranking", 
                                           min = 1, max = 494,
                                           value = c(1, 25)),
                               # Feature 4: A checkbox that enables additional filtering by country
                               # This gives people an additional way of filtering for more specific schools
                               checkboxInput("countryFilter", label = "Also filter by country"),
                               # Feature 4.5: A dropdown menu that filters for schools from a selected country
                               selectInput("countryInput",
                                 label = "Filter by Country",
                                 choices = valid_countries
                               )),
                  mainPanel(
                    h2("Locations of Selected Schools"),
                    p("Hover over each point to see school details"),
                    ggiraphOutput("schoollocationsplot"),
                    br(),
                    # Feature 5: Only show the plot for number of schools by region if user isn't filtering by country
                    # Otherwise, this plot would be pretty useless
                    conditionalPanel(
                      condition = "input.countryFilter != 1",
                      h2("Number of Schools by Region"),
                      plotOutput("regioncountplot"),
                      br(),
                    ),
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
    if(input$countryFilter == TRUE){
      selected <- rankings %>%
        filter(country == input$countryInput,
               Rank >= input$rankingInput[1],
               Rank <= input$rankingInput[2]) %>%
        select(Rank, university, country, city) %>%
        `colnames<-`(c("Rank", "University Name", "Country", "City"))
      selected
    }
    else {selected <- rankings %>%
      filter(Rank >= input$rankingInput[1],
             Rank <= input$rankingInput[2]) %>%
      select(Rank, university, country, city) %>%
      `colnames<-`(c("Rank", "University Name", "Country", "City"))
    selected}
  })
  
  output$schoollocationsplot <- renderggiraph({
    # Feature 6: Map of world or selected country with selected schools
    # This lets people see the locations of their schools
    if(input$countryFilter == TRUE){
      # extracting polygon for specific country
      country_polygon <- map_data %>% 
        filter(region == input$countryInput)
      filtered_rankings_loc <- rankings_loc %>%
        filter(country == input$countryInput,
               Rank >= input$rankingInput[1],
               Rank <= input$rankingInput[2]) %>%
      # Feature 6.5: School info on hover
      # This lets people know the name, score, and rank of schools on the map
        mutate(tooltip = sprintf("School Name: %s\nScore: %s\nRank: %s",
                                 university, score, Rank))
        
      
      plot <- ggplot() +
        geom_polygon(data = country_polygon, 
                     aes(x = long, y = lat, group = group),
                     fill = "grey", alpha = 0.3) +
        # geom_point(data = filtered_rankings_loc,
        #            aes(x = long, y = lat, size = score / 10),
        #            alpha = 0.25) +
        geom_point_interactive(data = filtered_rankings_loc,
                               aes(x = long, y = lat, size = score / 10, tooltip = tooltip),
                               alpha = 0.25) +
        theme_void() + 
        xlim(floor(min(country_polygon$long)),
             ceiling(max(country_polygon$long))) + 
        ylim(floor(min(country_polygon$lat)),
             ceiling(max(country_polygon$lat))) + 
        coord_map() +
        labs(size = "Score / 10")
      ggiraph(code = print(plot))
    } else{
      filtered_rankings_loc <- rankings_loc %>%
        filter(Rank >= input$rankingInput[1],
               Rank <= input$rankingInput[2]) %>%
        mutate(tooltip = sprintf("School Name: %s\nScore: %s\nRank: %s",
                                 university, score, Rank))
      
      plot <- ggplot() +
        geom_polygon(data = map_data, 
                     aes(x = long, y = lat, group = group),
                     fill = "grey", alpha = 0.3) +
        # geom_point(data = filtered_rankings_loc,
        #            aes(x = long, y = lat, size = score / 10),
        #            alpha = 0.25) +
        geom_point_interactive(data = filtered_rankings_loc,
                               aes(x = long, y = lat, size = score / 10, tooltip = tooltip),
                               alpha = 0.25) +
        theme_void() + 
        coord_map() +
        labs(size = "Score / 10")
      ggiraph(code = print(plot))
    }
  })
}
shinyApp(ui = ui, server = server)
