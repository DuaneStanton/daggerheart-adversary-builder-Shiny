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

# Define UI ====================================================================
ui <- fluidPage(

    # Application title
    titlePanel("Adversary Builder"),

    tabsetPanel(
      tabPanel("Setup",
               numericInput("party_total",
                            label = "# party members", value = 4, step = 1, min = 1, width = "25%"),
               selectInput("fight_type",
                           label = "Type of fight", 
                           choices = c("Regular",
                                       "Regular (but adversary tier < party tier)",
                                       "Easier/shorter", 
                                       "Tougher (add +1d4 to adversary damage rolls)",
                                       "Tougher (add +2 to adversary damage rolls)",
                                       "Harder/longer"),
                           selected = "Regular"),
               htmlOutput("warning_count"),
               numericInput("adversary_total",
                            label = "# adversaries", value = 1, step = 1, min = 1, width = "25%"),
               selectInput("tier", "Select adversary tier", choices = c(1:4), 
                           selected = 1, multiple = FALSE, width = "25%"),
               uiOutput("adversary_counts", 
                        label = "Specify the # of adversaries by type"),
               htmlOutput("warning_battle_points"),
               htmlOutput("battle_points")
               ),
          tabPanel("Results",
                   plotOutput("distPlot"))
        )
)

# Define server ================================================================
server <- function(input, output) {

  ###
  ### TODO
  ###
  ### - add tally for adversary count
  ### - work out containerized adversary buildout tab (build via click button on 'setup' tab)
  ### -- needs to allow user to track HP, Stress, and Conditions (likely include extendable 'add condition' text input field along w/ multi-select dropdowns for standard)
  ### - workout Obsidian-friendly copy-able formatted tab (LIKELY FUNCTION)
  ### - add validation checks
  ### - incorporate 'add +1d4 to dmg roll' and 'add static +2 to dmg roll' in rendered damage dice for each 'result' tab
  ### - likely build out an 'internal functions' R script to organize things
  ### - work on aesthetics (hope & fear, baby!)
  
  adversary_lister <- function(inputId, label, choices, value = 0) {
    tags$div(selectInput(inputId, label, choices = choices, selected = value),
             style="display:inline-block")
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
  
  
  # input${adversary type}_count is character - need numeric for battle points
  adv_names <- c("bruiser", "horde", "leader", "minion", "ranged", "skulk", "solo", "standard", "support", "social")
  adv_rctv <- lapply(seq_along(adv_names), \(i) {
    reactive({as.numeric(input[[paste0(adv_names[i], "_count")]])})
  })
  names(adv_rctv) <- adv_names
  
  adv_total <- reactive({
    req(adv_rctv[[1]]())
    sum(vapply(seq_along(adv_names), 
               \(i){ as.numeric(input[[paste0(adv_names[i], "_count")]]) },
               numeric(1L)) )
    })
  
  
  output$warning_count <- renderText({
    if (adv_total() > input$adversary_total) {
      "<p style ='color: red'><b><i>Adversary total > intended # adversaries</i></b></p>"
    }
  })
  
  btl_pts_adv <- reactive({
    sum(
      1 * floor(adv_rctv[["minion"]]() / input$party_total),
      1 * (adv_rctv[["social"]]() + adv_rctv[["standard"]]()),
      2 * (adv_rctv[["horde"]]() + adv_rctv[["ranged"]]() + adv_rctv[["skulk"]]() + adv_rctv[["standard"]]()),
      3 * adv_rctv[["leader"]](),
      4 * adv_rctv[["bruiser"]](),
      5 * adv_rctv[["solo"]]()
    )
  }) 

  btl_pts_bdgt <- reactive({
    sum(
      3 * input$party_total + 2,
      ifelse(input$fight_type == "Easier/shorter", -1, 0),
      ifelse(input$fight_type %in% c("Tougher (add +1d4 to adversary damage rolls)",
                                     "Tougher (add +2 to adversary damage rolls)") |
               adv_rctv[["solo"]]() > 1, 
             -2, 0),
      ifelse(input$fight_type == "Regular (but adversary tier < party tier)", 1, 0),
      ifelse(
        sum(adv_rctv[["bruiser"]](), adv_rctv[["horde"]](), adv_rctv[["leader"]](), adv_rctv[["solo"]]()) == 0L,
        1, 0),
      ifelse(input$fight_type == "Harder/longer", 2, 0)
    )
  })

  output$warning_battle_points <- renderText({
    if (btl_pts_adv() > btl_pts_bdgt()) {
      "<p style ='color: red'><b><i>Current 'battle points' > your 'budget' - may be tougher than design intended</i></b></p>"
    }
  })
  
  output$battle_points <- renderText({
    paste0("<p style = 'color:blue'><i>Current battle points: ", btl_pts_adv(), 
           "; 'budget': ", btl_pts_bdgt(), "</i></p>")
    })
  
  ###
  ### WORKING AREA ABOVE HERE
  ###

    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
