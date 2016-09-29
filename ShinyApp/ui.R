library(shiny)
require(markdown)
shinyUI(
    navbarPage("Predict Next Word",
        tabPanel("Run App",
            pageWithSidebar(
                headerPanel("Predict Next English Word"),
                sidebarPanel(
                    h4("Please, type the sentence: "),
                    textInput(inputId = 'userWords', label = "", value = "")
                ),
            mainPanel(
                h3('Results of prediction'),
                verbatimTextOutput("prediction")
                )
            )),
            tabPanel("Documentation",
                     mainPanel(
                         includeMarkdown("predictWordDocumentation.md")
                     )
            )
        )
)