library(shiny)
require(markdown)
shinyUI(
    navbarPage(
      "Predict Next Word",
        tabPanel(
          "Run App",
            pageWithSidebar(
                headerPanel("Predict Next English Word"),
                sidebarPanel(
                    h4("Please, type the sentence: "),
                    textInput(inputId = 'userWords', label = "", value = ""),
                    sliderInput("numPredicted", "Number of words to show", 
                                min=1, max=10, value=4)
                ),
            mainPanel(
                h3('Results of prediction'),
                textOutput("prediction"),
                hr(),
                div(dataTableOutput("predictionTable"), style='font-size:100%'),
                conditionalPanel("input.userWords == '' && $('html').hasClass('shiny-busy')",
                                 "Loading. This will take a while."))
            )),
            tabPanel("Documentation",
                     mainPanel(
                         includeMarkdown("predictWordDocumentation.md")
                     )
            )
        )
)