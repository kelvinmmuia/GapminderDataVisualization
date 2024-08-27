# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(gapminder)
library(bslib)
library(ggpubr)
library(ggthemes)

# Load the gapminder dataset
data("gapminder")

# Define UI
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "shiny"),
  
  title = "Global Demographics and Economics",
  
  # Tab 1: Life Expectancy over Time
  tabPanel("Life Expectancy",
           sidebarLayout(
             sidebarPanel(
               sliderInput("year_lifeExp", "Select Year:", 
                           min = min(gapminder$year), 
                           max = max(gapminder$year), 
                           value = min(gapminder$year), 
                           step = 5, animate = TRUE, sep = ""),
               selectInput("continent_lifeExp", "Select Continent:", 
                           choices = unique(gapminder$continent), 
                           selected = "Europe")
             ),
             mainPanel(
               fluidRow(
                 column(12, 
                        plotOutput("plot_lifeExp", height = "400px")
                 ),
                 column(12, 
                        plotOutput("summary_lifeExp")
                 )
               )
             )
           )),
  
  # Tab 2: Population over Time
  tabPanel("Population",
           sidebarLayout(
             sidebarPanel(
               sliderInput("year_pop", "Select Year:", 
                           min = min(gapminder$year), 
                           max = max(gapminder$year), 
                           value = min(gapminder$year), 
                           step = 5, animate = TRUE, sep = ""),
               selectInput("continent_pop", "Select Continent:", 
                           choices = unique(gapminder$continent), 
                           selected = "Africa")
             ),
             mainPanel(
               fluidRow(
                 column(12, 
                        plotOutput("plot_pop", height = "400px")
                 ),
                 column(12, 
                        plotOutput("summary_pop")
                 )
               )
             )
           )),
  
  # Tab 3: GDP per Capita over Time
  tabPanel("GDP per Capita",
           sidebarLayout(
             sidebarPanel(
               sliderInput("year_gdpPercap", "Select Year:", 
                           min = min(gapminder$year), 
                           max = max(gapminder$year), 
                           value = min(gapminder$year), 
                           step = 5, animate = TRUE, sep = ""),
               selectInput("continent_gdpPercap", "Select Continent:", 
                           choices = unique(gapminder$continent), 
                           selected = "Europe")
             ),
             mainPanel(
               fluidRow(
                 column(12, 
                        plotOutput("plot_gdpPercap", height = "400px")
                 ),
                 column(12, 
                        plotOutput("summary_gdpPercap")
                 )
               )
             )
           ))
)


server <- function(input, output) {
  
  # Helper function to format numbers with comma separator
  format_numbers <- function(x) {
    format(x, big.mark = ",", scientific = FALSE)
  }
  
  # Reactive data subset based on year and continent for Life Expectancy
  filtered_data_lifeExp <- reactive({
    gapminder %>%
      filter(year == input$year_lifeExp & continent == input$continent_lifeExp) %>%
      arrange(desc(lifeExp)) %>%
      head(15)
  })
  
  # Render Life Expectancy plot
  output$plot_lifeExp <- renderPlot({
    data <- filtered_data_lifeExp()
    avg_lifeExp <- mean(data$lifeExp)
    
    ggplot(data, aes(x = reorder(country, lifeExp), y = lifeExp)) +
      geom_col(fill = "#007bff") +
      geom_hline(yintercept = avg_lifeExp, linewidth=2, linetype = "dashed"
                 , color = "red") +
      coord_flip() +
      labs(title = paste("Life Expectancy in", input$year_lifeExp,
                         "for top 15 countries in", input$continent_lifeExp),
           x = "Country", y = "Life Expectancy") +
      theme_wsj() +
      scale_y_continuous(labels = format_numbers)
  })
  
  # Render Summary Plot for Life Expectancy
  output$summary_lifeExp <- renderPlot({
    data <- gapminder %>%
      filter(year == input$year_lifeExp & continent == input$continent_lifeExp)
    
    ggplot(data, aes(x = reorder(country, lifeExp), y = lifeExp)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = paste("Overall Distribution of Life Expectancy in",
                         input$year_lifeExp, "for", input$continent_lifeExp),
           x = "Country", y = "Life Expectancy") +
      theme_wsj() +
      scale_y_continuous(labels = format_numbers)
  })
  
  # Reactive data subset based on year and continent for Population
  filtered_data_pop <- reactive({
    gapminder %>%
      filter(year == input$year_pop & continent == input$continent_pop) %>%
      arrange(desc(pop)) %>%
      head(15)
  })
  
  # Render Population plot
  output$plot_pop <- renderPlot({
    data <- filtered_data_pop()
    avg_pop <- mean(data$pop)
    
    ggplot(data, aes(x = reorder(country, pop), y = pop)) +
      geom_col(fill = "#28a745") +
      geom_hline(yintercept = avg_pop, linetype = "dashed", linewidth = 2, 
                 color = "red") +
      coord_flip() +
      labs(title = paste("Population in", input$year_pop, 
                         "for top 15 countries in", input$continent_pop),
           x = "Country", y = "Population") +
      theme_wsj() +
      scale_y_continuous(labels = format_numbers)
  })
  
  # Render Summary Plot for Population
  output$summary_pop <- renderPlot({
    data <- gapminder %>%
      filter(year == input$year_pop & continent == input$continent_pop)
    
    ggplot(data, aes(x = reorder(country, pop), y = pop)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      coord_flip() +
      labs(title = paste("Overall Distribution of Population in",
                         input$year_pop, "for", input$continent_pop),
           x = "Country", y = "Population") +
      theme_wsj() +
      scale_y_continuous(labels = format_numbers)
  })
  
  # Reactive data subset based on year and continent for GDP per Capita
  filtered_data_gdpPercap <- reactive({
    gapminder %>%
      filter(year == input$year_gdpPercap & continent == input$continent_gdpPercap) %>%
      arrange(desc(gdpPercap)) %>%
      head(15)
  })
  
  # Render GDP per Capita plot
  output$plot_gdpPercap <- renderPlot({
    data <- filtered_data_gdpPercap()
    avg_gdpPercap <- mean(data$gdpPercap)
    
    ggplot(data, aes(x = reorder(country, gdpPercap), y = gdpPercap)) +
      geom_col(fill = "#ffc107") +
      geom_hline(yintercept = avg_gdpPercap, linetype = "dashed",linewidth = 2, 
                 color = "red") +
      coord_flip() +
      labs(title = paste("GDP per Capita in", input$year_gdpPercap,
                         "for top 15 countries in", input$continent_gdpPercap),
           x = "Country", y = "GDP per Capita") +
      theme_wsj() +
      scale_y_continuous(labels = format_numbers)
  })
  
  # Render Summary Plot for GDP per Capita
  output$summary_gdpPercap <- renderPlot({
    data <- gapminder %>%
      filter(year == input$year_gdpPercap & continent == input$continent_gdpPercap)
    
    ggplot(data, aes(x = reorder(country, gdpPercap), y = gdpPercap)) +
      geom_bar(stat = "identity", fill = "gold") +
      coord_flip() +
      labs(title = paste("Overall Distribution of GDP per Capita in", 
                         input$year_gdpPercap, "for", input$continent_gdpPercap),
           x = "Country", y = "GDP per Capita") +
      theme_wsj() +
      scale_y_continuous(labels = format_numbers)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
