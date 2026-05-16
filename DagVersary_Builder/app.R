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
library(stringi)

source("R/Supporting Functions and Code.R")

# Define UI ====================================================================
ui <- fluidPage(
    titlePanel("Daggerheart Adversary Builder"),
    tags$head(tags$style("
    .adv-count-sel {
      width: 100px;
    }
    .adv-input {
      border: 4px solid #320e45;
      background: #7d4e7d;
      color: #eddb9d;
      padding-left: 10px;
      padding-bottom: 20px;
    }
    .adv-run {
      display: flex;
      border: 4px solid #320e45;
      background: #d3bee8;
      color: #320e45;
      padding-left: 5px;
      margin-left: 5px;
      padding-right: 5px;
      margin-right: 5px;
    }
    .form-control {
      background: #efefef
    }
    .selectize-input.full.has-items.has-options {
      background: #f2efce
    }
    .form-group {
      margin-bottom: 10px !important;
      width: 100%;
    }
    .form-group.shiny-input-container {
      width: 100%;
      padding-right: 10px;
      outline-width: 20px;
    }
    .bottom-aligned {
      display: flex;
      align-items: flex-end;
      column-gap: 10px;
    }
    .bottom-aligned > div {
      flex-grow: 1;
    }
    .inline label{ 
      display: table-cell; 
      text-align: center; 
      vertical-align: middle;
      padding-right: 15px;
      }
    .inline .form-group { display: table-row; }
    ")),
    htmlOutput("use_note"),
    tabsetPanel(
      tabPanel("Start",
               div(h4("Specify party size, challenge type, adversary tier, and adversary counts, then move to 'Customize'")),
               numericInput("party_total",
                            label = "# party members", value = 4, step = 1, min = 1, width = "140px"),
               selectInput("fight_type",
                           label = "Challenge", 
                           choices = c("Regular",
                                       "Regular (but adversary tier < party tier)",
                                       "Easier/shorter", 
                                       "Tougher (add +1d4 to adversary damage rolls)",
                                       "Tougher (add +2 to adversary damage rolls)",
                                       "Harder/longer"),
                           selected = "Regular", width = "350px"),
               selectInput("tier", "Select adversary tier", choices = tier_vals, 
                           selected = 1, multiple = FALSE, width = "150px"),
               htmlOutput("adv_tally"),
               uiOutput("adv_counts", 
                        label = "Specify the # of adversaries by type"),
               htmlOutput("warning_battle_points"),
               htmlOutput("battle_points")
               ),
      tabPanel("Customize",
               div(h4("Customize details for your adversaries below, then move to 'Run'")),
               htmlOutput("dmg_note"),
               uiOutput("adv_spec")), 
      tabPanel("Run",
               div(h4("Use this panel to run adversaries in-app from the 'Customize' tab")),
               uiOutput("adv_run")), ### USER-INTERACTIVE FOR RUNNING FROM SERVER - INCLUDE DICE ROLLER AND BUTTON PER ADVERSARY???
      
      tabPanel("Obsidian",
               div(h4("Copy from this tab to Obsidian if running adversaries there - details from the 'Customize' tab"))), ### OPTIONAL COPYABLE TEXT FOR RUNNING IN OBSIDIAN
      tabPanel("Credits",
               htmlOutput("sources")), 
      tabPanel("Feature Listing",
               div(h4("Lookup table of features for adversaries")),
               "WORK IN PROGRESS")
      )
)

# Define server ================================================================
server <- function(input, output) {

  output$use_note <- renderText({
    "<p>Custom adversary builder using <i>RightKnighttoFight</i>'s Guide and the Daggerheart SRD (see <b>Credit</b> tab)</p>"
  })
  ###
  ### TODO
  ###
  ### - work out containerized adversary buildout tab (build via click button on 'setup' tab)
  ### -- needs to allow user to track HP, Stress, and Conditions (likely include extendable 'add condition' text input field along w/ multi-select dropdowns for standard)
  ### - workout Obsidian-friendly copy-able formatted tab (LIKELY FUNCTION)
  ### - add validation checks
  ### - incorporate 'add +1d4 to dmg roll' and 'add static +2 to dmg roll' in rendered damage dice for each 'result' tab
  ### - work on aesthetics (hope & fear, baby!)
  
  # server Start panel ---------------------------------------------------------
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
  
  active_adv_ct_vec <- reactive({adv_ct_vec()[adv_ct_vec() > 0]})
  
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
  dmg_add <- reactive({
    if (input$fight_type == "Tougher (add +1d4 to adversary damage rolls)") {"+1d4"
    } else if (input$fight_type == "Tougher (add +2 to adversary damage rolls)") {"+2"
    } else {"none"}
  })
  
  output$dmg_note <- renderText({
    if (dmg_add() %in% c("+1d4", "+2")) {
      paste("<h3>Because you specified adding", dmg_add(), 
            "to damage rolls, the 'Run' and 'Obsidian' tabs will include that for each adversary</h3>")
    } else {""}
  })
  
  output$adv_spec <-
    renderUI({
      req(active_adv_ct_vec())
      output = tagList()

    lapply(seq_along(active_adv_ct_vec()), \(i) {
      lapply(1:active_adv_ct_vec()[i], \(j) {
        output[[i]] <- tagList()
        output[[i]][[j]] <- 
          div(class = "adv-input",
              build_adv_spec_ui(names(active_adv_ct_vec())[i], j, input$tier) )
        output
      })
    })
  ### NEED TO CREATE TEMPLATE FOR TYPE-APPROPRIATE MOTIVE/TACTIC DEFAULTS
  ### CREATE MULTIPLE SETS TO APPLY FOR EACH TYPE AND ALLOW USER TO CHECK BOX FOR RANDOMIZED
    })
  
  # server Run panel -----------------------------------------------------------
  output$adv_run <- 
    renderUI({
      req(active_adv_ct_vec())
      output = tagList()
      
      lapply(seq_along(active_adv_ct_vec()), \(i) {
        lapply(1:active_adv_ct_vec()[i], \(j) {
          output[[i]] <- tagList()
          output[[i]][[j]] <- 
            div(class = "adv-run",
                column(6,
                build_adv_run_ui(input, names(active_adv_ct_vec())[i], j, input$tier) )
            )
          output
        })
      })
    })
  
  
  # server Obsidian panel ------------------------------------------------------
  
  # server Credits panel -------------------------------------------------------
  output$sources <- 
    renderUI({
      HTML(
        paste0(
          "<p>This website includes materials from the Daggerheart System Reference Document 1.0, © Critical Role, LLC. All rights reserved.</p>",
          "<p>Suggested adversary stats come from the <a href='https://docs.google.com/document/d/12g-obIkdGJ_iLL19bS0oKPDDvPbPI9pWUiFqGw8ED88/edit?tab=t.0#heading=h.mdjo15f06zjv'>RightKnighttoFight’s Guide to Making Custom Adversaries v1.6</a> Google doc</p>"
          )
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
