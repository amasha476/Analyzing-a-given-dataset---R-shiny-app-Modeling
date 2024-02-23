library(shiny)
shinyUI(fluidPage(
  tags$b(titlePanel("Let's Analyze Your Dataset")),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file", multiple = TRUE),
      tags$b(helpText("Upload csv files only")),
      checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=','), selected = ','),
      textInput("ResponseVariable","Response Variable",placeholder ="Enter your Response Varaible"),
      tags$b(uiOutput("selectfile")),
    ),
    mainPanel(
      uiOutput("tb")

    )

  )
))
