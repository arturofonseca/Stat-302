# CDCapp: L11 Ex2 ----------------------------------------------------------

# load packages ----
library(shiny)
library(bslib)
library(tidyverse)

# load data ----
cdc <- read_delim(file = "data/cdc.txt", delim = "|") |>
    mutate(
        exerany = factor(exerany, levels = c(1, 0), labels = c("Yes", "No")),
        hlthplan = factor(hlthplan, levels = c(1, 0), labels = c("Yes", "No")),
        smoke100 = factor(smoke100, levels = c(1, 0), labels = c("Yes", "No")),
        gender = factor(gender, levels = c("f", "m"), labels = c("Female", "Male")),
        genhlth = factor(
            genhlth,
            levels = c("excellent", "very good", "good", "fair", "poor"),
            labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")
        )
    )

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
    # App title ----
    title = "CDC BRFSS: Histogram of Weight Grouped by Gender", # Sidebar panel for inputs ----
    sidebar = sidebar(
        position = "right",
        # Input: Slider for the number of bins ----
        selectInput(
            "select",
            "Select Variable:",
            list(
                "Actual Weight" = "weight",
                "Desired Weight" = "wtdesire",
                "Height" = "height"
            ),
            selected = "Actual Weight"
        ),
        sliderInput(
            inputId = "bins",
            label = "Number of bins:",
            min = 5,
            max = 50,
            value = 30,
            animate = TRUE
        ),
        radioButtons(
            "radio",
            "Select Fill/Legend Variable:",
            choices = c(
                "General Health" = "genhlth",
                "Health Coverage" = "hlthplan",
                "Exercised in Past Month" = "exerany",
                "Smoked 100 Cigarettes" = "smoke100",
                "Gender" = "gender"
            ),
            selected = "gender"
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
        ggplot(cdc, aes(!! sym(input$select), fill = !! sym(input$radio))) +
            geom_histogram(bins = input$bins, color = "black") +
            ggthemes::theme_fivethirtyeight() +
            labs(x = switch(
                input$select,
                "weight" = "Actual Weight in Pounds",
                "wtdesire" = "Desired Weight in Pounds",
                "height" = "Height in Inches"
            ), y = "Count", fill = switch(
                input$radio,
                "genhlth" = "General Health",
                "hlthplan" = "Health Coverage",
                "exerany" = "Exercised in Past Month",
                "smoke100" = "Smoked 100 Cigarettes",
                "gender" ="Gender"
            )) +
            theme(
                axis.title = element_text(),
                legend.position = "top",
                legend.title.position = "top",
                legend.title = element_text(hjust = 0.5)
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
