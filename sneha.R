library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

# 1. Load data from the correct folder
student_data <- read.csv(
  "C:/R studio/Students_Grading_Dataset.csv",
  stringsAsFactors = FALSE
)

# 2. UI
ui <- dashboardPage(
  dashboardHeader(title = "Student Performance"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Details",  tabName = "details",  icon = icon("table"))
    ),
    selectInput("gender",    "Gender:",
                choices   = c("All", sort(unique(student_data$Gender))),
                selected  = "All", multiple = TRUE, selectize = FALSE),
    selectInput("dept",      "Department:",
                choices   = c("All", sort(unique(student_data$Department))),
                selected  = "All", multiple = TRUE, selectize = FALSE),
    selectInput("parentEdu", "Parental Education:",
                choices   = c("All", sort(unique(student_data$Parent_Education_Level))),
                selected  = "All", multiple = TRUE, selectize = FALSE),
    selectInput("internet",  "Internet Access:",
                choices   = c("All", sort(unique(student_data$Internet_Access_at_Home))),
                selected  = "All", multiple = TRUE, selectize = FALSE),
    selectInput("extra",     "Extracurricular Activities:",
                choices   = c("All", sort(unique(student_data$Extracurricular_Activities))),
                selected  = "All", multiple = TRUE, selectize = FALSE)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("avgMidterm",     width = 4),
                valueBoxOutput("avgFinal",       width = 4),
                valueBoxOutput("avgAssignments", width = 4)
              ),
              fluidRow(
                box(title = "Avg Midterm Score by Gender",   status = "primary",
                    solidHeader = TRUE, width = 4,
                    plotOutput("midtermPlot", height = 240)),
                box(title = "Avg Final Score by Gender",     status = "success",
                    solidHeader = TRUE, width = 4,
                    plotOutput("finalPlot",   height = 240)),
                box(title = "Avg Assignments Avg by Gender", status = "warning",
                    solidHeader = TRUE, width = 4,
                    plotOutput("assignPlot",  height = 240))
              )
      ),
      tabItem(tabName = "details",
              box(title = "Filtered Student Data", status = "info",
                  solidHeader = TRUE, width = 12,
                  DTOutput("dataTable"))
      )
    )
  )
)

# 3. Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    df <- student_data
    if (!("All" %in% input$gender))    df <- df %>% filter(Gender               %in% input$gender)
    if (!("All" %in% input$dept))      df <- df %>% filter(Department           %in% input$dept)
    if (!("All" %in% input$parentEdu)) df <- df %>% filter(Parent_Education_Level %in% input$parentEdu)
    if (!("All" %in% input$internet))  df <- df %>% filter(Internet_Access_at_Home %in% input$internet)
    if (!("All" %in% input$extra))     df <- df %>% filter(Extracurricular_Activities %in% input$extra)
    df
  })
  
  # Value boxes
  output$avgMidterm <- renderValueBox({
    avg <- mean(filtered_data()$Midterm_Score, na.rm = TRUE) %>% round(1)
    valueBox(if (is.nan(avg)) "N/A" else avg, "Avg Midterm Score",
             icon = icon("calculator"), color = "blue")
  })
  output$avgFinal <- renderValueBox({
    avg <- mean(filtered_data()$Final_Score, na.rm = TRUE) %>% round(1)
    valueBox(if (is.nan(avg)) "N/A" else avg, "Avg Final Score",
             icon = icon("book"), color = "green")
  })
  output$avgAssignments <- renderValueBox({
    avg <- mean(filtered_data()$Assignments_Avg, na.rm = TRUE) %>% round(1)
    valueBox(if (is.nan(avg)) "N/A" else avg, "Avg Assignments Avg",
             icon = icon("edit"), color = "yellow")
  })
  
  # Bar charts of mean scores by Gender
  output$midtermPlot <- renderPlot({
    ggplot(filtered_data(), aes(Gender, Midterm_Score, fill = Gender)) +
      stat_summary(fun = mean, geom = "col", width = 0.6) +
      labs(x = "Gender", y = "Mean Midterm Score") + theme_minimal() +
      theme(legend.position = "none")
  })
  output$finalPlot <- renderPlot({
    ggplot(filtered_data(), aes(Gender, Final_Score, fill = Gender)) +
      stat_summary(fun = mean, geom = "col", width = 0.6) +
      labs(x = "Gender", y = "Mean Final Score") + theme_minimal() +
      theme(legend.position = "none")
  })
  output$assignPlot <- renderPlot({
    ggplot(filtered_data(), aes(Gender, Assignments_Avg, fill = Gender)) +
      stat_summary(fun = mean, geom = "col", width = 0.6) +
      labs(x = "Gender", y = "Mean Assignments Avg") + theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Data table
  output$dataTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
}

# 4. Launch
shinyApp(ui, server)
