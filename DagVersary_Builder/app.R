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
#library(tidyr)

source("R/Supporting Functions and Code.R")

# Define UI ====================================================================
ui <- fluidPage(

    # Application title
    titlePanel("Adversary Builder"),
    htmlOutput("use_note"),
    tabsetPanel(
      tabPanel("Start",
               tags$div(h4("Specify party size, challenge type, and adversary counts, then move to 'Customize'")),
               numericInput("party_total",
                            label = "# party members", value = 4, step = 1, min = 1, width = "25%"),
               selectInput("fight_type",
                           label = "Challenge", 
                           choices = c("Regular",
                                       "Regular (but adversary tier < party tier)",
                                       "Easier/shorter", 
                                       "Tougher (add +1d4 to adversary damage rolls)",
                                       "Tougher (add +2 to adversary damage rolls)",
                                       "Harder/longer"),
                           selected = "Regular"),
               selectInput("tier", "Select adversary tier", choices = tier_vals, 
                           selected = 1, multiple = FALSE, width = "25%"),
               htmlOutput("adv_tally"),
               uiOutput("adv_counts", 
                        label = "Specify the # of adversaries by type"),
               htmlOutput("warning_battle_points"),
               htmlOutput("battle_points")
               ),
      tabPanel("Customize",
               tags$div(h4("Customize details for your adversaries below, then move to 'Run'")),
               uiOutput("adv_spec")), ### USER SPECIFIES DIFFICULTY/HP/STRESS/THRESHOLDS/DAMAGE DICE/NAMES/MOTIVATIONS/???
      tabPanel("Run",
               tags$div(h4("Use this panel to run adversaries in-app"))), ### USER-INTERACTIVE FOR RUNNING FROM SERVER - INCLUDE DICE ROLLER AND BUTTON PER ADVERSARY???
      tabPanel("Obsidian",
               tags$div(h4("Copy from this tab to Obsidian if runing adversaries there"))), ### OPTIONAL COPYABLE TEXT FOR RUNNING IN OBSIDIAN
      tabPanel("Credits",
               htmlOutput("sources")) ### CREDIT DAGGERHEART SRD AND RIGHTKNIGHTTOFIGHT
      )
)

# Define server ================================================================
server <- function(input, output) {

  output$use_note <- renderText({
    "<p>This application will <b>always</b> be provided for free - enjoy!</p>"
  })
  ###
  ### TODO
  ###
  ### - work out containerized adversary buildout tab (build via click button on 'setup' tab)
  ### -- needs to allow user to track HP, Stress, and Conditions (likely include extendable 'add condition' text input field along w/ multi-select dropdowns for standard)
  ### - workout Obsidian-friendly copy-able formatted tab (LIKELY FUNCTION)
  ### - add validation checks
  ### - incorporate 'add +1d4 to dmg roll' and 'add static +2 to dmg roll' in rendered damage dice for each 'result' tab
  ### - likely build out an 'internal functions' R script to organize things
  ### - work on aesthetics (hope & fear, baby!)
  
  # server Start panel ---------------------------------------------------------
  adversary_lister <- function(inputId, label, choices = 0:input$adv_total, value = 0) {
    tags$div(selectInput(inputId, label, choices = choices, selected = value),
             style="display:inline-block")
  }

    output$adv_counts <- renderUI({
    lapply(seq_along(adv_types), \(i) { build_adv_count(typ = adv_types[i]) })
  })
  
  adv_rctv <- lapply(seq_along(adv_types), \(i) {
    reactive({as.numeric(input[[paste0(adv_types[i], "_count")]])})
  })
  names(adv_rctv) <- adv_types
  
  adv_ct_vec <- reactive({
    req(adv_rctv[[1]]())
    
    v <- vapply(seq_along(adv_types), 
                \(i){ as.numeric(input[[paste0(adv_types[i], "_count")]]) },
                numeric(1L))
    names(v) <- adv_types
    v
  })
  
  active_adv_ct_vec <- reactive({ adv_ct_vec()[adv_ct_vec() > 0] })
  
  adv_total <- reactive({
    req(adv_rctv[[1]]())
    sum(adv_ct_vec())
    })
  
  output$adv_tally <- renderText({
    paste0("<p style ='color: blue'><b><i>Adversary total: ", adv_total(), "</i></b></p>")
  })
  
  btl_pts_adv <- reactive({
    sum(
      1 * floor(adv_rctv[["Minion"]]() / input$party_total),
      1 * (adv_rctv[["Social"]]() + adv_rctv[["Standard"]]()),
      2 * (adv_rctv[["Horde"]]() + adv_rctv[["Ranged"]]() + adv_rctv[["Skulk"]]() + adv_rctv[["Standard"]]()),
      3 * adv_rctv[["Leader"]](),
      4 * adv_rctv[["Bruiser"]](),
      5 * adv_rctv[["Solo"]]()
    )
  }) 

  btl_pts_bdgt <- reactive({
    sum(
      3 * input$party_total + 2,
      ifelse(input$fight_type == "Easier/shorter", -1, 0),
      ifelse(input$fight_type %in% c("Tougher (add +1d4 to adversary damage rolls)",
                                     "Tougher (add +2 to adversary damage rolls)") |
               adv_rctv[["Solo"]]() > 1, 
             -2, 0),
      ifelse(input$fight_type == "Regular (but adversary tier < party tier)", 1, 0),
      ifelse(
        sum(adv_rctv[["Bruiser"]](), adv_rctv[["Horde"]](), adv_rctv[["Leader"]](), adv_rctv[["Solo"]]()) == 0L,
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
    if (btl_pts_adv() == btl_pts_bdgt()) {
      "<p style = 'color:green'><b>Reached 'budget'!</b></p>"
      } else {
      paste0("<p style = 'color:blue'><i>Current battle points: ", btl_pts_adv(), 
             "; 'budget': ", btl_pts_bdgt(), "</i></p>")
      }
    })
  
  ###
  ### WORKING CODE ABOVE HERE, IN-DEVELOPMENT CODE BELOW
  ###

  # server Customize panel -----------------------------------------------------
  output$adv_spec <-
    renderUI({
      req(active_adv_ct_vec())
      output = tagList()

    lapply(seq_along(active_adv_ct_vec()), \(i) {
      lapply(1:active_adv_ct_vec()[i], \(j) {
        
        typ_ <- names(active_adv_ct_vec())[i]
        tier <- input$tier
        
        output[[i]] <- tagList()
        output[[i]][[j]] <- build_adv_ui(typ_, j, tier)
        
        output
      })
    })
            
  ### NEED TO CREATE TEMPLATE FOR TYPE-APPROPRIATE MOTIVE/TACTIC DEFAULTS
  ### CREATE MULTIPLE SETS TO APPLY FOR EACH TYPE AND ALLOW USER TO CHECK BOX FOR RANDOMIZED
    })
  
  # server Run panel -----------------------------------------------------------
  
  # server Obsidian panel ------------------------------------------------------
  
  # server Credits panel -------------------------------------------------------
  output$sources <- 
    renderText({
      tags$div("<p>This website includes materials from the Daggerheart System Reference Document 1.0, © Critical Role, LLC. All rights reserved.</p>")
      tags$div("<p>Suggested adversary stats come from the <a href='https://docs.google.com/document/d/12g-obIkdGJ_iLL19bS0oKPDDvPbPI9pWUiFqGw8ED88/edit?tab=t.0#heading=h.mdjo15f06zjv'>RightKnighttoFight’s Guide to Making Custom Adversaries v1.6</a> Google doc</p>")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
