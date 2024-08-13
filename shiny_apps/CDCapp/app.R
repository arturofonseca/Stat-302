# CDCapp: L10 Ex2 ----------------------------------------------------------

# load packages ----
library(shiny)
library(bslib)
library(tidyverse)

# load data ----
cdc <- read_delim(file = "data/cdc.txt", delim = "|") |>
    mutate(
        genhlth = factor(
            genhlth,
            levels = c("excellent", "very good", "good", "fair", "poor"),
            labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")
        )
    )

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
    # App title ----
    title = "CDC BRFSS: Histogram of Weight Grouped by Gender",
    # Sidebar panel for inputs ----
    sidebar = sidebar(
        position = "right",
        # Input: Slider for the number of bins ----
        sliderInput(
            inputId = "bins",
            label = "Number of bins:",
            min = 5,
            max = 50,
            value = 30,
            animate = TRUE
        )
    ), 
    # Output: Histogram ----
    card(
        plotOutput(outputId = "distPlot")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        # draw the histogram with the specified number of bins
        ggplot(cdc, aes(weight, fill = gender)) +
            geom_histogram(bins = input$bins, color = "black") +
            theme_minimal() +
            labs(x = "Weight in Pounds", y = "Count", fill = "Gender") +
            scale_fill_discrete(labels = c("Female", "Male")) +
            theme(legend.position = "inside",
                  legend.position.inside = c(0.48, 0.75))
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
