library(shiny)
library(dplyr)
library(ggplot2)
library(pokemon)

pokemon <- pokemon

ui <- fluidPage(
  
  titlePanel("Pokémon Base Stats Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Instructions"),
      helpText("Choose one or more Pokémon base stats. 
               If you select one stat, you'll see a histogram.
               If you select two stats, you'll see a scatterplot and a correlation value.
               You can also filter the data by Type."),
      hr(),
      
      checkboxGroupInput(
        "vars", "Select stat(s):",
        choices = list(
          "Attack" = "attack",
          "Defense" = "defense",
          "Special Attack" = "special_attack",
          "Special Defense" = "special_defense"
        ),
        selected = "attack"
      ),
      
      selectInput(
        "color", "Graph color:",
        choices = c("Forest Green" = "forestgreen", "Cornflowerblue" = "cornflowerblue", "Firebrick" = "firebrick")
      ),
      
      checkboxInput("add_line", "Add regression line (for scatterplots)", value = FALSE),
      
      hr(),
      
      selectInput(
        "type_filter", "Filter by Type:",
        choices = c("All", sort(unique(pokemon$type_1))), selected = "All"
      ),
      
      hr(),
      sliderInput("bins", "Number of bins:", min = 5, max = 50, value = 30)
    ),
    
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("stats"),
      
      br(),
      
      tags$img(
        src = "https://upload.wikimedia.org/wikipedia/commons/thumb/9/98/International_Pok%C3%A9mon_logo.svg/1200px-International_Pok%C3%A9mon_logo.svg.png",
        height = "80px"
      ),
      tags$p("The Pokémon Company. (n.d.). International Pokémon logo [Image]. Wikimedia Commons.")
    )
  )
)

server <- function(input, output) {
  
  poke_filtered <- reactive({
    poke_filtered <- pokemon
    
    if (input$type_filter != "All") {
      poke_filtered <- poke_filtered |>
        filter(type_1 == input$type_filter)
    }
    
    poke_filtered
  })
  
  output$plot <- renderPlot({
    req(input$vars)
    poke_filtered <- poke_filtered()
    
    validate(need(nrow(poke_filtered) > 0, "No data available with your filters."))
    
    vars <- input$vars
    
    if (length(vars) == 1) {
      ggplot(poke_filtered, aes(x = .data[[vars[1]]])) +
        geom_histogram(bins = input$bins, fill = input$color, color = "black") +
        labs(
          title = paste("Distribution of", vars[1]),
          x = vars[1], y = "Count"
        ) +
        theme_minimal()
      
    } else if (length(vars) == 2) {
      plot <- ggplot(poke_filtered, aes(x = .data[[vars[1]]], y = .data[[vars[2]]])) +
        geom_point(color = input$color, size = 2) +
        labs(
          title = paste(vars[1], "vs", vars[2]),
          x = vars[1], y = vars[2]
        ) +
        theme_minimal()
      
      if (input$add_line) {
        plot <- plot + geom_smooth(method = "lm", se = FALSE, color = "black")
      }
      
      plot
      
    } else {
      
      pairs(poke_filtered[, vars], main = "Pairs Plot of Selected Stats")
    }
  })
  
  output$stats <- renderPrint({
    req(input$vars)
    poke_filtered <- poke_filtered()
    vars <- input$vars
    
    if (length(vars) == 1) {
      x <- poke_filtered[[vars[1]]]
      stats <- c(
        Mean = mean(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE)
      )
      print(round(stats, 3))
      
    } else if (length(vars) == 2) {
      x <- poke_filtered[[vars[1]]]
      y <- poke_filtered[[vars[2]]]
      corr_value <- cor(x, y, use = "pairwise.complete.obs")
      cat("Correlation between", vars[1], "and", vars[2], ":\n")
      print(round(corr_value, 3))
      
    } else {
      cat("Multiple variables selected — pairs plot displayed above.")
    }
  })
}

shinyApp(ui, server)