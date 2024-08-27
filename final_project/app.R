# Final Project ----------------------------------------------------------

# Setup ----
# load packages
library(shiny)
library(bslib)
library(tidyverse)
library(showtext)

# fonts
font_add_google("Inter", "inter")
showtext_auto()

# load data
cdc <- read_csv("data/diabetes_binary_health_indicators_BRFSS2015.csv")

# clean data
cdc <- cdc |>
    janitor::clean_names() |>
    relocate(gen_hlth:phys_hlth, bmi, .after = last_col()) |>
    mutate(
        across(diabetes_binary:diff_walk, ~ factor(
            .x, levels = c(0, 1), labels = c("No", "Yes")
        )),
        sex = factor(
            sex,
            levels = c(0, 1),
            labels = c("Female", "Male")
        ),
        across(age:gen_hlth, as.factor)
    )

# Define UI for app ----
ui <- navbarPage(
    title = "Diabetes Insights App",
    # Tab 1 ----
    tabPanel(
        title = "Overview",
        fluidPage(
            titlePanel("Welcome to Diabetes Insights App!"),
            h3("By Arturo Fonseca"),
            h3("Introduction"),
            p(
                "Healthcare is extremely important. It is crucial to get treated for the conditions that one has.
            Just as important, if not more, is being aware of having that condition. According to the CDC,
            1 in 5 Americans have diabetes and don't know it. However, there are common symptoms that show up
            when one has diabetes. Additionally, one's habits and behaviors can predispose them to diabetes and prediabetes."
            ),
            div(img(src = "1in5.png", height = "200px"), style = "text-align: center;"),
            br(),
            wellPanel(
                h4("Important Note"),
                p(
                    "For the rest of the project, I will be using the term 'diabetes' to refer to both diabetes and prediabetes
                  as both conditions are present in the dataset as a 'Yes' for ",
                    tags$code("diabetes_binary"),
                    "."
                ),
                p(
                    "Also, please do not use this project to diagnose yourself. If you have any doubts, please go speak with your doctor..."
                )
            ),
            p(
                "This app has 4 tabs, starting with the 'Overview' tab which we're currently on. The next 3 tabs have plots pertaining to
              how the distribution of diabetic patients changes with certain variables. Feel free to navigate to each tab in no particular order."
            ),
            h3("Motivation"),
            p(
                "It is important to take care of youself and watch out for what to avoid to protect yourself from diabetes.
              Additionally, it might also be beneficial to see if the reader might align with the symptoms / behaviors / demographic of diabetic patients."
            ),
            h3("Data Overview"),
            p(
                "This dataset is the 2014 BRFSS Survey Data and Documentation from the",
                tags$a(href = "https://www.cdc.gov/brfss/annual_data/annual_2014.html", "CDC"),
                ".
                The dataset has 253,680 survey responses with 22 columns, 1 of which is the target variable:",
                tags$code("diabetes_binary"),
                ". Most are binary variables (e.g. smoked more than 100 cigarettes ?), and some are integer values.",
            ),
            p(
                "We are mostly interested in how certain variables break down by diabetes status. It is important to note that obviously most people don't have this condition,
                 meaning we have a significant class imbalance (only about 13% of respondents had diabetes). Regardless, we can still pick up on the trends of this small population."
            ),
            plotOutput("diabetes_dist"),
            radioButtons(
                "radio",
                "Did you like this presentation?",
                c("Yes", "Yes"),
                selected = "Yes"
            ),
            actionButton("action", "Submit Answer to Author")
        )
    ),
    # Tab 2 ----
    tabPanel(
        title = "Health Conditions",
        fluidPage(
            titlePanel("Health Conditions"),
            p(
                "Starting off with the distribution of the respondants' BMI, we see a clear trend:
          diabetic people tend to have a higher BMI. The dashed line represents the median for
          each respective group. The shaded backgrounds denotes the BMI group: underweight, obese, etc."
            ),
            plotOutput("bmi", width = "90%"),
            tags$hr(style = "border-color: gray; border-width: 2px;"),
            sidebarLayout(sidebarPanel(
                p(
                    "Here, we examine how the percentage of people with these conditions differ
                  depending on whether the group is diabetic or not. For example, it is more likely
                  that a diabetic has high blood pressure than not, when that is not the case for non-diabetics.
                  These plots uncover general trends in who within each group carry these conditions."
                ),
                selectInput(
                    "health_select",
                    "Select variable to view:",
                    choices = c(
                        "High Blood Pressure",
                        "High Cholesterol",
                        "Heart Disease/Attack",
                        "Difficulty Walking"
                    ),
                    selected = "High Blood Pressure"
                ),
            ), mainPanel(plotOutput("pie")))
        )
    ),
    # Tab 3 ----
    tabPanel(title = "Behavior", fluidPage(
        titlePanel("Behavior"), tabsetPanel(
            tabPanel(
                "Pie Chart(s)",
                p(
                    "There is not a huge difference in the proportion of smokers depending on diabetes status,
                       but there is still a difference nonetheless."
                ),
                plotOutput("pie_behavior")
            ),
            tabPanel("Density Plots", sidebarLayout(
                sidebarPanel(
                    p(
                        "We see that a majority of the diabetic group spends more days thinking
                              about their health when compared to non-diabetics."
                    ),
                    selectInput(
                        "behavior_select",
                        "Select variable to view:",
                        choices = c(
                            "Days Spent Thinking About Mental Health",
                            "Days Spent Thinking About Physical Health"
                        ),
                        selected = "Days Spent Thinking About Mental Health"
                    )
                ), mainPanel(plotOutput("behavior"))
            ))
        )
    )),
    # Tab 4 ----
    tabPanel(title = "Socioeconomic", fluidPage(
        titlePanel("Socioeconomic Factors"),
        sidebarLayout(sidebarPanel(
            p(
                "We see the importance of socioeconomic factors on diabetes status. It is
              especially important seeing the trend in diabetes when looking at income and education.
              Low-income and uneducated earners have a higher proportion of diabetics than high-income
              and educated earners. This was surprising to me because I thought diabetes wouldn't
              discrimante based on how much someone makes, but that is sadly not the case..."
            ),
            selectInput(
                "socio_select",
                "Select variable to view:",
                choices = c("Education", "Income", "Age", "Sex"),
                selected = "Education"
            )
        ), mainPanel(plotOutput("socio")))
    ))
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    output$diabetes_dist <- renderPlot({
        ggplot(cdc, aes(x = NA, fill = diabetes_binary)) +
            geom_bar(color = "black") +
            coord_polar("y") +
            geom_text(
                aes(label = scales::label_comma()(after_stat(count))),
                stat = "count",
                position = position_stack(vjust = 0.5),
                family = "inter",
                size = 8
            ) +
            scale_fill_manual(values = c("lightgray", "deepskyblue")) +
            labs(title = "Makeup of Survery Respondents", fill = "Diabetic?") +
            theme_void() +
            theme(text = element_text(family = "inter", size = 18))
    })
    
    underweight_range <- c(0, 18.5)
    healthy_range <- c(18.5, 25)
    overweight_range <- c(25, 30)
    obese_range <- c(30, Inf)
    
    output$bmi <- renderPlot({
        ggplot(cdc, aes(bmi, fill = diabetes_binary)) +
            annotate(
                "rect",
                xmin = underweight_range[1],
                xmax = underweight_range[2],
                ymin = -Inf,
                ymax = Inf,
                color = "black",
                fill = "lightblue",
                alpha = 0.2
            ) +
            annotate(
                "rect",
                xmin = healthy_range[1],
                xmax = healthy_range[2],
                ymin = -Inf,
                ymax = Inf,
                color = "black",
                fill = "lightgreen",
                alpha = 0.2
            ) +
            annotate(
                "rect",
                xmin = overweight_range[1],
                xmax = overweight_range[2],
                ymin = -Inf,
                ymax = Inf,
                color = "black",
                fill = "orange",
                alpha = 0.2
            ) +
            annotate(
                "rect",
                xmin = obese_range[1],
                xmax = obese_range[2],
                ymin = -Inf,
                ymax = Inf,
                color = "black",
                fill = "red",
                alpha = 0.2
            ) +
            geom_density(alpha = 0.8, bw = 1) +
            geom_vline(
                data = cdc |>
                    group_by(diabetes_binary) |>
                    summarize(median = median(bmi)),
                aes(xintercept = median, color = diabetes_binary),
                linetype = "dashed",
                size = 1,
                show.legend = F
            ) +
            scale_color_manual(values = c("black", "blue")) +
            scale_fill_manual(values = c("lightgray", "deepskyblue")) +
            scale_x_continuous(expand = expansion(0), limits = c(0, 50)) +
            scale_y_continuous(expand = expansion(c(0, 0.05))) +
            labs(
                x = "BMI",
                y = "Density",
                title = "Distribution of BMI",
                fill = "Diabetic?"
            ) +
            theme_minimal() +
            theme(
                text = element_text(family = "inter", size = 18),
                axis.text.y = element_blank(),
                legend.background = element_rect(color = "black", fill = "white"),
                legend.position = "inside",
                legend.position.inside = c(0.95, 0.5),
                legend.justification.inside = c(1, 0.5)
            )
    })
    
    output$pie <- renderPlot({
        choice <- switch(
            input$health_select,
            "High Blood Pressure" = "high_bp",
            "High Cholesterol" = "high_chol",
            "Heart Disease/Attack" = "heart_diseaseor_attack",
            "Difficulty Walking" = "diff_walk"
        )
        cdc |>
            group_by(diabetes_binary, !!sym(choice)) |>
            summarize(count = n(), .groups = "drop") |>
            group_by(diabetes_binary) |>
            mutate(percent = count / sum(count)) |>
            ungroup() |>
            ggplot(aes(NA, percent, fill = !!sym(choice))) +
            geom_bar(stat = "identity", color = "black") +
            coord_polar("y") +
            facet_wrap( ~ factor(diabetes_binary, labels = c("Non-diabetic", "Diabetic")), scales = "free") +
            scale_fill_manual(values = c("lightgray", "deepskyblue")) +
            geom_text(
                aes(label = scales::percent(round(percent, 2))),
                position = position_stack(vjust = 0.5),
                size = 8,
                family = "inter"
            ) +
            labs(
                title = paste(
                    "Distribution of Those With",
                    input$health_select,
                    "by Diabetes"
                ),
                fill = paste0(input$health_select, "?")
            ) +
            theme_void() +
            theme(text = element_text(family = "inter", size = 18),
                  legend.position = "bottom")
    })
    
    output$socio <- renderPlot({
        choice <- tolower(input$socio_select)
        labels <- switch(
            input$socio_select,
            "Education" = c(
                "Never attended",
                "Elementary",
                "Some High School" ,
                "High School",
                "Some College",
                "4+ Year College"
            ),
            "Income" = c(
                "<$10K",
                "<15K",
                "<$20K",
                "<$25K",
                "<$35K",
                "<$50K",
                "<$75K",
                "$75K+"
            ),
            "Age" = c("<24", paste("<", seq(30, 80, by = 5)), "80+"),
            "Sex" = levels(cdc$sex)
        )
        ggplot(cdc, aes(!!sym(choice), fill = diabetes_binary)) +
            geom_bar(position = "fill", color = "black") +
            scale_fill_manual(values = c("lightgray", "deepskyblue")) +
            labs(
                x = input$socio_select,
                y = "Proportion",
                title = paste("Proportion of Diabetics by", input$socio_select),
                fill = "Diabetic?"
            ) +
            scale_x_discrete(labels = labels, expand = expansion(0.1)) +
            scale_y_continuous(labels = scales::label_percent(),
                               expand = expansion(0.01)) +
            theme_minimal() +
            theme(
                text = element_text(family = "inter", size = 18),
                legend.position = "top",
                axis.text.x = element_text(angle = -20, hjust = 0.5)
            )
    })
    
    output$pie_behavior <- renderPlot({
        cdc |>
            group_by(diabetes_binary, smoker) |>
            summarize(count = n(), .groups = "drop") |>
            group_by(diabetes_binary) |>
            mutate(percent = count / sum(count)) |>
            ungroup() |>
            ggplot(aes(NA, percent, fill = smoker)) +
            geom_bar(stat = "identity", color = "black") +
            coord_polar("y") +
            facet_wrap( ~ factor(diabetes_binary, labels = c("Non-diabetic", "Diabetic")), scales = "free") +
            scale_fill_manual(values = c("lightgray", "deepskyblue")) +
            geom_text(
                aes(label = scales::percent(round(percent, 2))),
                position = position_stack(vjust = 0.5),
                size = 8,
                family = "inter"
            ) +
            labs(title = "Distribution of Smokers by Diabetes", fill = "Smoker?") +
            theme_void() +
            theme(text = element_text(family = "inter", size = 18),
                  legend.position = "bottom")
    })
    
    output$behavior <- renderPlot({
        choice <- switch(
            input$behavior_select,
            "Days Spent Thinking About Mental Health" = "ment_hlth",
            "Days Spent Thinking About Physical Health" = "phys_hlth"
        )
        ggplot(cdc, aes(!!sym(choice), fill = diabetes_binary)) +
            geom_density(position = "fill",
                         bw = 1,
                         alpha = 0.7) +
            geom_hline(
                yintercept = 0.5,
                linetype = "dashed",
                color = "black"
            ) +
            scale_fill_manual(values = c("lightgray", "deepskyblue")) +
            scale_x_continuous(expand = expansion(0)) +
            scale_y_continuous(labels = scales::label_percent(),
                               expand = expansion(0)) +
            labs(
                x = paste(input$behavior_select, "(In Past 30 Days)"),
                y = "Relative Density",
                title = paste(
                    "Relative Density of",
                    substring(input$behavior_select, 27),
                    "Days by Diabetes Status"
                ),
                fill = "Diabetic?"
            ) +
            theme_minimal() +
            theme(
                text = element_text(family = "inter", size = 18),
                legend.position = "inside",
                legend.position.inside = c(0.5, 0.9),
                legend.justification = c(0.5, 1),
                legend.direction = "horizontal",
                legend.background = element_rect(color = "black", fill = "white")
            )
    })
}

# Run the application ----
shinyApp(ui = ui, server = server)
