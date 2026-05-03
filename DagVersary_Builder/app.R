#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Adversary Builder"),

    tabsetPanel(
      tabPanel("Setup",
               htmlOutput("warning_count"),
               numericInput("adversary_total",
                            label = "# adversaries", value = 1, step = 1, min = 1, width = "25%"),
               selectInput("tier", "Select adversary tier", choices = c(1:4), 
                           selected = 1, multiple = FALSE, width = "25%"),
               uiOutput("adversary_counts", 
                        label = "Specify the # of adversaries by type"),
               uiOutput("battle_points")
               ),
          tabPanel("Results",
                   plotOutput("distPlot"))
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$warning_count <- renderText({
    if (input$adversary_total > 9) {
      "<p style ='color: red'><b><i>That's a lot of adversaries! Consider using Hordes in place of some.</i></b></p>"
      }
    })
  
  adversary_lister <- function(inputId, label, choices, value = 0) {
    tags$div(selectInput(inputId, label, choices = choices, selected = value),
             style="display:inline-block")
    
    
    # div(style="display:inline-block",
    #     tags$label(label, `for` = inputId), 
    #     tags$select(id = inputId, class = "shiny-input-select",
    #                 class = "form-control",
    #                 size = NULL, 
    #                 selectOptions(choices, 0, inputId, FALSE),
    #                 class="input-small"))
  }
  
  ### DEV NOTE: WANT TO DYNAMICALLY RESTRICT THE DROPDOWN MAX PER CATEGORY TO THE input$adversary_total AND ADD 'VERIFY' MESSAGE WHEN TOTAL > input$adversary_total
  output$adversary_counts <- renderUI({
    bootstrapPage(
      adversary_lister("bruiser_count", "# Bruisers", choices = 0:input$adversary_total),
      adversary_lister("horde_count", "# Hordes", choices = 0:input$adversary_total),
      adversary_lister("leader_count", "# Leaders", choices = 0:input$adversary_total),
      adversary_lister("minion_count", "# Minions", choices = 0:input$adversary_total),
      adversary_lister("ranged_count", "# Ranged", choices = 0:input$adversary_total),
      adversary_lister("skulk_count", "# Skulks", choices = 0:input$adversary_total),
      adversary_lister("solo_count", "# Solos", choices = 0:input$adversary_total),
      adversary_lister("standard_count", "# Standards", choices = 0:input$adversary_total),
      adversary_lister("support_count", "# Supports", choices = 0:input$adversary_total),
      adversary_lister("social_count", "# Socials", choices = 0:input$adversary_total)
    )
  })
  
  output$battle_points <- renderUI({})
  
  ###
  ### WORKING AREA ABOVE HERE
  ###
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$adversary_total + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
