# State Map: L12 Ex1 ----------------------------------------------------------

# load packages ----
library(shiny)
library(bslib)
library(tidyverse)
library(sf)

# load data ----
country_data <- read_sf("data/county_data.shp")

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
    # App title ----
    title = "Country Demographic Map by State", # Sidebar panel for inputs ----
    sidebar = sidebar(
        position = "left",
        width = "3in",
        helpText("Create demographic maps with information from 2020 US Census"),
        # Input: Slider for the number of bins ----
        selectInput(
            "state",
            "Choose a state to display",
            choices = str_to_title(unique(country_data$state)),
            selected = "Illinois"
        ),
        selectInput(
            "var",
            "Choose a variable to display",
            choices = c(
                "Percent White",
                "Percent Black",
                "Percent Hispanic",
                "Percent Asian"
                # "Percent White" = "darkgreen",
                # "Percent Black" = "black",
                # "Percent Hispanic" = "darkorange",
                # "Percent Asian" = "darkviolet"
            ),
            selected = "Percent White"
        ),
    ), 
    # Output: Histogram ----
    card(
        plotOutput(outputId = "map")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$map <- renderPlot({
        fill_var = switch(
            input$var,
            "Percent White" = "white",
            "Percent Black" = "black",
            "Percent Hispanic" = "hispanic",
            "Percent Asian" = "asian"
        )
        fill_color <- switch(
            input$var,
            "Percent White" = "darkgreen",
            "Percent Black" = "black",
            "Percent Hispanic" = "darkorange",
            "Percent Asian" = "darkviolet"
        )
        fill_guide_title <- switch(
            input$var,
            "Percent White" = "% White",
            "Percent Black" = "% Black",
            "Percent Hispanic" = "% Hispanic",
            "Percent Asian" = "% Asian"
        )
        country_data |>
            filter(state == tolower(input$state)) |>
            ggplot() +
            geom_sf(aes(geometry = geometry, fill = !!sym(fill_var))) +
            scale_fill_gradient(
                name = fill_guide_title,
                low = "white",
                high = fill_color,
                limits = c(0, 100)
            ) +
            theme_void(base_size = 14) +
            labs(title = str_to_title(input$state)) +
            theme(plot.title = element_text(size = 24, hjust = 0.5))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
