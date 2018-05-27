shinyUI(
  navbarPage(
    title = "US Airline",
    tabPanel(
      title = "Descriptive",
      pageWithSidebar(
        headerPanel('Descriptive statistics'),
        sidebarPanel(
          selectInput(inputId = "target", label = "Target", choices = c("Target 1", "Target 2"), selected = "Target 1"),
          checkboxInput(inputId = "married",label = "Married", value = T),
          sliderInput(inputId = "day", label = "Day of the week", min = 0, max = 6, value = 0, step = 1,animate = T)
        ),
      mainPanel(
        tabsetPanel(
          tabPanel(title = "geo map", htmlOutput('geoChart')),
          tabPanel(title = "Table", dataTableOutput('datatable'))
          )
        )
      )
    ),
    tabPanel(
      title = "Model",
      pageWithSidebar(
        headerPanel('Predictive Model'),
        sidebarPanel(
          selectInput(inputId = "predicted_target", label = "Target", choices = c("Target 1", "Target 2"), selected = "Target 1"),
          numericInput(inputId = "ntrees", label = "N trees", value = 50, min = 25, max = 150, step = 25),
          numericInput(inputId = "interaction", label = "Interaction Depth", value = 5, min = 1, max = 10, step = 1),
          numericInput(inputId = "learning", label = "Learning Rate", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
          numericInput(inputId = "proportion", label = "Train/Test proportion", value = 0.5, min = 0.1, max = 0.9, step = 0.1),
          selectInput(inputId = "var", label = "Variable in the model", choices = gbmVar, selected = gbmVar, multiple = T),
          actionButton(inputId = "run", label = "Run")
          ),
        mainPanel(
          fluidRow(
            column(5, plotOutput('roc')),
            column(5, dataTableOutput('importance'))
          )
        )
      )
    )
  )
)
