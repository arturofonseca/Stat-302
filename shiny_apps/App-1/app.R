# App-1: L10 Ex1 ----------------------------------------------------------

# load packages
library(shiny)
library(bslib)
library(tidyverse)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
    # App title ----
    title = "Hello World!",
    # Sidebar panel for inputs ----
    sidebar = sidebar(
        # Input: Slider for the number of bins ----
        sliderInput(
            inputId = "bins",
            label = "Number of bins:",
            min = 5,
            max = 50,
            value = 30
        )
    ),
    # Output: Histogram ----
    plotOutput(outputId = "distPlot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful |> pull(waiting)
        bin_breaks <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        ggplot(faithful, aes(waiting)) +
            geom_histogram(breaks = bin_breaks,
                           color = "orange",
                           fill = "#007bc2",) +
            theme_minimal() +
            labs(title = "Histogram of waiting times", x = "Waiting time to next eruption (in mins)", y = "Frequency") +
            theme(plot.title = element_text(face = "bold", hjust = 0.5))
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)

# Define UI
ui <- page_sidebar(
    title = "My Shiny App",
    sidebar = sidebar(
        "Shiny is available on CRAN, so you can install it in the usual way from your R console:",
        code('install.packages("shiny")'),
    ),
    card(
        card_header("Introducing Shiny"),
        "Shiny is a package from Posit that makes it incredibly easy to build interactive web applications with R.
    For an introduction and live examples, visit the Shiny homepage (https://shiny.posit.co).",
        card_image("www/shiny.svg", height = "300px"),
        card_footer("Shiny is a product of Posit.")
    )
)

# Define server logic ----
server <- function(input, output) {
    
}

# Run the app ----
shinyApp(ui = ui, server = server)
