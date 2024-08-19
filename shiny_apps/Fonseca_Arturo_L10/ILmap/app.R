# CDCapp: L10 Ex2 ----------------------------------------------------------

# load packages ----
library(shiny)
library(bslib)
library(tidyverse)
library(tigris)

# load data ----
options(tigris_use_cache = TRUE)
il_data <- counties(state = "IL", cb = T)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
    # App title ----
    title = "Where I'm From",
    # Sidebar panel for inputs ----
    sidebar = sidebar(
        "Illinois is random state that you may or may not have heard of",
        code('so dismiss() it if you havent'),
        "Anywaysis, here's a cool app that I made",
        position = "left"
    ), 
    layout_column_wrap(
        width = 1/2,
        height = 300,
        card(full_screen = TRUE, card_header("IL Map"), plotOutput(outputId = "distPlot")),
        card(
            full_screen = TRUE,
            card_header("Random Stuff"),
            card_image("www/shiny.svg", height = "300px"),
            card_footer("Shiny is a product of Posit.")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        ggplot(il_data) +
            geom_sf(color = "black", fill = "lightblue", alpha = 0.5) +
            theme_void()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
