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
library(DT)

source("R/Supporting Functions and Code.R")
source("R/Customize section code.R")
source("R/Run section code.R")
source("R/Obsidian_Daggerforge code.R")
source("R/Obsidian_ITS Theme code.R")
source("R/Environment Builder code.R")
source("R/Environment Export code.R")

# Define UI ====================================================================
ui <- fluidPage(
  titlePanel("Daggerheart Adversary Builder"),
  tags$head(tags$style("
    .container-fluid { background-color: #d3c0d3; }
    .nav { 
      background-color: #53386b;
      border-radius: 10px;
    }
    .adv-count-sel { width: 100px; }
    .adv-input {
      border: 4px solid #320e45;
      border-radius: 5px;
      background: #7d4e7d;
      color: #eddb9d;
      padding-left: 10px;
      padding-bottom: 20px;
    }
    .adv-run {
      border-radius: 5px;
      display: flex;
      padding-left: 5px;
      margin-left: 5px;
      padding-right: 5px;
      margin-right: 5px;
      width: 710px;
    }
    .adv-run-gen {
      border: 4px solid #320e45;
      background: #d3bee8;
      color: #320e45;
    }
    .adv-run-colossus-fw {
      border: 4px solid #320e45;
      background: #afa9e8;
      color: #320e45;
    }
    .adv-run-colossus-sg {
      border: 2px solid #320e45;
      background: #c2bee8;
      color: #320e45;
    }
    .shiny-html-output .shiny-bound-output { margin-right: 5px; }
    .shiny-output-error {
      font-size: 18px;
      color: #d00dad;
    }
    .form-control { background: #efefef }
    .selectize-input.full.has-items.has-options { background: #f2efce }
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
    .bottom-aligned > div { flex-grow: 1; }
    .inline label { 
      display: table-cell; 
      text-align: center; 
      vertical-align: middle;
      padding-right: 15px;
      }
    .inline .form-group { display: table-row; }
    .inline .selectize-input.full.has-items.has-options {
      width: 60px;
    }
    #json_dl {
      background-color: #53386b;
      border-color: #320e45;
      color: #e0c34c;
      font-size: 18px;
    }
    #markdown_dl {
      background-color: #53386b;
      border-color: #320e45;
      color: #e0c34c;
      font-size: 18px;
    }
    #json_dl_env {
      background-color: #53386b;
      border-color: #320e45;
      color: #e0c34c;
      font-size: 18px;
    }
    #markdown_dl_env {
      background-color: #53386b;
      border-color: #320e45;
      color: #e0c34c;
      font-size: 18px;
    }
    table.dataTable.display tbody tr.odd { 
      background-color: #f7e9ab;
    }
    table.dataTable.display tbody tr.even { 
      background-color: #dbd2a7;
    }
    ")),
  htmlOutput("use_note"),
  tabsetPanel(type = "pills",
              tabPanel(div("Start", style = "font-size: 16px; color: #e8d37d;"),
                       div(h4("Specify party size, challenge type, adversary tier, and adversary counts, then move to 'Customize'")),
                       numericInput("party_total", label = "# party members", value = 4, step = 1, min = 1, width = "140px"),
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
                       div(h5("Note: if you're running these via Obsidian - Daggerforge and want multiples of a specific adversary, just create them once here and specify the count in the Daggerforge plugin '- [#] +' interface.")),
                       uiOutput("adv_counts", label = "Specify the # of adversaries by type"),
                       htmlOutput("warning_battle_points"),
                       htmlOutput("battle_points"),
                       htmlOutput("colossus_note")
              ),
              tabPanel(div("Customize", style = "font-size: 16px; color: #e8d37d;"),
                       div(h4("Customize details for your adversaries below, then move to 'Run'")),
                       htmlOutput("reset_caution"),
                       div("See notes below the feature reference table in the 'Feature Table' tab if you'd like to include adversary-specific details in your features."),
                       htmlOutput("dmg_note"),
                       div(class="inline", title = "Minions and Hordes have different (default) behavior", style = "width: 300px;",
                           selectInput("feat_fill_ct", label = "# filled features per adversary", choices = 0:5, selected = 0, width = "100px")),
                       uiOutput("adv_spec")
              ), 
              tabPanel(div("Run", style = "font-size: 16px; color: #e8d37d;"),
                       div(h4("Use this panel to run adversaries in-app from the 'Customize' tab")),
                       div(uiOutput("adv_run"))
              ), 
              tabPanel(div("Adversary Export", style = "font-size: 16px; color: #e8d37d;"),
                       tabsetPanel(
                         tabPanel(div("Obsidian - Daggerforge", style = "font-size: 16px; color: #e8d37d;"),
                                  div(h4("Download a JSON file from this tab to upload to Obsidian via the Daggerforge plugin if running adversaries there. First build adversaries using details from the 'Customize' tab. You -may- need to close and reopen Obsidian after uploading to access newly-added adversaries, which will be available in the 'Custom' category of the 'Source' filter.")),
                                  htmlOutput("dgrfg_colossus_note"),
                                  downloadButton("json_dl", label = "Download JSON file for Daggerforge"),
                                  div(h4("The downloaded .json will look like the below:")),
                                  verbatimTextOutput(outputId = "json_dl_prvw") ),
                         tabPanel(div("Obsidian - ITS Theme", style = "font-size: 16px; color: #e8d37d;"),
                                  div(h4("Download a text file from this tab to select all > copy > paste into Obsidian if running adversaries there and you have the ITS Theme set up. First build adversaries using details from the 'Customize' tab.")),
                                  div(span("Note: you", style = "font-size: 16px;"), span(" can ", style = "color: blue; font-size: 16px;"), span("copy-paste this text into Obsidian without the ITS Theme applied, but the formatting looks much worse.", style = "font-size: 16px")),
                                  downloadButton("markdown_dl", label = "Download text file of Markdown for copy-pasting to Obsidian"),
                                  div(h4("The downloaded .txt file will look like the below:")),
                                  verbatimTextOutput(outputId = "mkdn_txt_dl_prvw") )
                       )
              ), 
              tabPanel(div("Feature Table", style = "font-size: 16px; color: #e8d37d;"),
                       div(h4("Lookup table of features for adversaries - customize as you see fit!")),
                       div(uiOutput("feat_tbl_msg")),
                       div(dataTableOutput("features_df")),
                       div(h4("Notes for dynamic feature details")),
                       htmlOutput("dynam_feat_notes")
              ),
              tabPanel(div("Environment Builder", style = "font-size: 16px; color: #e8d37d;"),
                       div(h4("Template for exportable environments for session use")),
                       div("Note: Updating the environment tier / type count will reset the entry fields; it's better to complete your work for in-progress evironments if you decide to change things.", style = "font-size: 16px; color: darkblue"),
                       selectInput("env_tier", "Select environment tier", choices = tier_vals, 
                                   selected = 1, multiple = FALSE, width = "180px"),
                       uiOutput("env_dmg_note"),
                       tabsetPanel(
                         tabPanel(div("Counts by Type", style = "font-size: 16px; color: #e8d37d;"), 
                                  uiOutput("env_note1"),
                                  uiOutput("env_counts", label = "Specify the # of environments by type")),
                         tabPanel(div("Notes", style = "font-size: 16px; color: #e8d37d;"),
                                  uiOutput("env_notes")),
                         tabPanel(div("Customize", style = "font-size: 16px; color: #e8d37d;"), 
                                  uiOutput("env_spec")) )
                       ),
              tabPanel(div("Environment Export", style = "font-size: 16px; color: #e8d37d;"),
                       div(h4("Export created environment(s) as either a JSON or text file intended for use in Obsidian")),
                       tabsetPanel(
                         tabPanel(div("Obsidian - Daggerforge", style = "font-size: 16px; color: #e8d37d;"),
                                  div(h4("Download a JSON file from this tab to upload to Obsidian via the Daggerforge plugin if running environments there. You -may- need to close and reopen Obsidian after uploading to access newly-added environments, which will be available in the 'Custom' category of the 'Source' filter.")),
                                  downloadButton("json_dl_env", label = "Download JSON file for Daggerforge"),
                                  div(h4("The downloaded .json will look like the below:")),
                                  verbatimTextOutput(outputId = "json_dl_prvw_env")),
                         tabPanel(div("Obsidian - ITS Theme", style = "font-size: 16px; color: #e8d37d;"),
                                  div(h4("Download a text file from this tab to select all > copy > paste into Obsidian if running environments there and you have the ITS Theme set up.")),
                                  div(span("Note: you", style = "font-size: 16px;"), span(" can ", style = "color: blue; font-size: 16px;"), span("copy-paste this text into Obsidian without the ITS Theme applied, but the formatting looks much worse.", style = "font-size: 16px")),
                                  downloadButton("markdown_dl_env", label = "Download text file of Markdown for copy-pasting to Obsidian"),
                                  div(h4("The downloaded .txt file will look like the below:")),
                                  verbatimTextOutput(outputId = "mkdn_txt_dl_prvw_env")) )
                       ),
              tabPanel(div("Credits", style = "font-size: 16px; color: #e8d37d;"),
                       htmlOutput("sources")
              )
  )
)

# Define server ================================================================
server <- function(input, output, session) {
  
  output$use_note <- renderText({
    "<p>Custom adversary builder informed by <i>RightKnighttoFight</i>'s Guide and the Daggerheart SRD (see <b>Credits</b> tab)</p>"
  })
  
  # server Start panel ---------------------------------------------------------
  output$adv_counts <- renderUI({
    lapply(seq_along(adv_types), \(i) { build_adv_count(typ = adv_types[i]) })
  })
  
  adv_ct_vec <- reactive({
    lapply(seq_along(adv_types), \(i) {
      validate(need(
        is.integer(input[[paste0(adv_types[i], "_count")]]) &&
          input[[paste0(adv_types[i], "_count")]] >= 0,
        paste0(adv_types[i], " count input must be a single non-negative whole number on 'Start' tab. Use '0' for none.")
      ))
    })
    v <- vapply(seq_along(adv_types), 
                \(i){ input[[paste0(adv_types[i], "_count")]] },
                numeric(1L))
    names(v) <- adv_types
    v
  })
  
  active_adv_ct_vec <- reactive({adv_ct_vec()[adv_ct_vec() > 0]})
  
  adv_total <- reactive({ sum(active_adv_ct_vec()) })
  
  output$adv_tally <- renderText({
    paste0("<p style ='color: blue'><b><i>Adversary total: ", adv_total(), "</i></b></p>")
  })
  
  btl_pts_adv <- reactive({
    validate(need(is.integer(input$party_total) && input$party_total > 0, 
                  "Party Total in 'Start' tab must be a positive whole number."))
    sum(
      1 * floor(adv_ct_vec()[["Minion"]] / input$party_total),
      1 * (adv_ct_vec()[["Social"]] + adv_ct_vec()[["Standard"]]),
      2 * (adv_ct_vec()[["Horde"]] + adv_ct_vec()[["Ranged"]] + adv_ct_vec()[["Skulk"]] + adv_ct_vec()[["Standard"]]),
      3 * adv_ct_vec()[["Leader"]],
      4 * adv_ct_vec()[["Bruiser"]],
      5 * adv_ct_vec()[["Solo"]]
    )
  }) 
  
  btl_pts_bdgt <- reactive({
    sum(
      3 * input$party_total + 2,
      ifelse(input$fight_type == "Easier/shorter", -1, 0),
      ifelse(input$fight_type %in% c("Tougher (add +1d4 to adversary damage rolls)",
                                     "Tougher (add +2 to adversary damage rolls)") |
               adv_ct_vec()[["Solo"]] > 1, 
             -2, 0),
      ifelse(input$fight_type == "Regular (but adversary tier < party tier)", 1, 0),
      ifelse(
        sum(adv_ct_vec()[["Bruiser"]], adv_ct_vec()[["Horde"]], adv_ct_vec()[["Leader"]], adv_ct_vec()[["Solo"]]) == 0L,
        1, 0),
      ifelse(input$fight_type == "Harder/longer", 2, 0)
    )
  })
  
  output$warning_battle_points <- renderText({
    if (btl_pts_adv() > btl_pts_bdgt()) {
      "<p style ='color: red'><b><i>Current 'battle points' > your 'budget' - may be tougher than design intended</i></b></p>"
    } else if (sum(adv_ct_vec()) > 0 && any(grepl("Colossus", names(active_adv_ct_vec())))) {
      "<p style = 'color: darkred; font-size: 18px;'><b><i>'battle points' budget is meant for non-Colossus-including encounters; a Colossus can be its own encounter. If adding other adversaries think of how they may interact with the Colossus.</i></b></p>"
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
  
  output$colossus_note <- renderText({
    if (sum(adv_ct_vec()) > 0 && any(grepl("Colossus", names(active_adv_ct_vec()))) & adv_ct_vec()["Colossus_framework"] == 0) {
      "<p style = 'color: darkred; font-size: 18px;'><b><i>Any encounter with a Colossus should include a Colossus framework and multiple segments.</i></b>"
    } else if (sum(adv_ct_vec()) > 0 && adv_ct_vec()["Colossus_framework"] > 1 && adv_ct_vec()["Colossus_average_segment"] + adv_ct_vec()["Colossus_strong_segment"] >= adv_ct_vec()["Colossus_framework"]) {
      "<p style = 'color: darkred; font-size: 18px;'><b><i>If using multiple Colossus frameworks, make sure to specify which framework each segment type is tied to.</i></b>"
    } else if (sum(adv_ct_vec()) > 0 && (adv_ct_vec()["Colossus_average_segment"] + adv_ct_vec()["Colossus_strong_segment"] < adv_ct_vec()["Colossus_framework"])) {
      "<p style = 'color: darkred; font-size: 18px;'><b><i>Each Colossus framework should have at least one segment (probably more) per framework.</i></b>"
    }
  })
  
  # server Customize panel -----------------------------------------------------
  # note: this validation check uses inputs defined further down
  cstm_chk <- reactive({
    req(adv_runset())
    valid_customize_chk(input, adv_runset())
  })
  
  
  colossus_groupset <- reactive({id_colossus_components(input, adv_ct_vec(), id = "grp")})
  colossus_fwk <- reactive({id_colossus_components(input, adv_ct_vec(), id = "fwk")})
  # id text for outputting adversaries in order, and for validation checks
  adv_runset <- reactive({
    req(active_adv_ct_vec())
    non_colossi <- active_adv_ct_vec()[!grepl("Colossus", names(active_adv_ct_vec()))]
    
    c(
      # Colossi first - plausibly focus of any scene they're in
      if (length(colossus_groupset()) > 0L) {colossus_groupset()},
      # then non-Colossi
      if (length(non_colossi) > 0L) {
        lapply(1:length(non_colossi), \(z) {
          paste0(names(non_colossi)[z], "_", 1:(non_colossi[z]))
        }) |> unlist(recursive = FALSE)
      }
    )
  })
  
  dmg_add <- reactive({
    if (input$fight_type == "Tougher (add +1d4 to adversary damage rolls)") {"+1d4"
    } else if (input$fight_type == "Tougher (add +2 to adversary damage rolls)") {"+2"
    } else {"none"}
  })
  
  output$reset_caution <- renderText({
    "<p style = 'color: darkred; font-size: 18px;'><i>Please note: If you go back to the 'Start' tab and change any settings, the app will update and you will lose details entered in this tab and the Run/Obsidian/Feature Table tabs.</i></p>"
  })
  
  output$dmg_note <- renderText({
    if (dmg_add() %in% c("+1d4", "+2")) {
      paste("<h4>Because you specified adding", dmg_add(), 
            "to damage rolls, the 'Run' and 'Obsidian' tabs will include that for each adversary</h4>")
    } else {""}
  })
  
  output$adv_spec <-
    renderUI({
      req(active_adv_ct_vec())
      output = tagList()
      
      lapply(seq_along(active_adv_ct_vec()), \(i) {
        lapply(1:active_adv_ct_vec()[i], \(j) {
          output[[i]] <- tagList()
          
          if (grepl("Colossus", names(active_adv_ct_vec())[i])) {
            output[[i]][[j]] <- 
              div(class = "adv-input",
                  build_colossus_spec_ui(names(active_adv_ct_vec())[i], j, input$tier,
                                         multi_frame = adv_ct_vec()["Colossus_framework"] > 1) )
          } else {
            output[[i]][[j]] <- 
              div(class = "adv-input",
                  build_adv_spec_ui(names(active_adv_ct_vec())[i], j, input$tier) )
          }
          
          output
        })
      })
    })
  
  feat_df <- reactive({ # note: already sorted by type, then tier, then passive/action/reaction, then feat name
    req(active_adv_ct_vec()) # note: tier should only be NA for 'general use' features
    filter(feat_ref_df, tier == input$tier, adv_type %in% c("general_use", names(active_adv_ct_vec())))
  })
  
  horde_feat_df <- reactive({ # used for the -non- 'Horde (#)' features set
    req(feat_df())
    if ("Horde" %in% feat_df()$adv_type) {
      filter(feat_df(), adv_type == "Horde" & !grepl("Horde \\(|Contains Multitudes", feat_name))
    }
  })
  
  col_fw_feat_df <- reactive({ # used for the -non- 'Colossal Power' features set
    req(feat_df())
    if ("Colossus_framework" %in% feat_df()$adv_type) {
      filter(feat_df(), adv_type == "Colossus_framework" & !grepl("Colossal Power", feat_name))
    }
  })
  
  feat_obsvr <- reactive({list(active_adv_ct_vec(), input$feat_fill_ct)})
  
  # Minion adversaries always have their feature set filled (standard plus 'synergistic extra one' per each)
  # Horde adversaries and Colossus frameworks are similar (standard starting set) but have some flexibility
  observeEvent(feat_obsvr(), {
    req(active_adv_ct_vec())
    if ("Minion" %in% names(active_adv_ct_vec())) {
      for (i in 1:(active_adv_ct_vec()[["Minion"]])) {
        fill_minion_feat(i, feat_df()[feat_df()$adv_type == "Minion",])
      }
    }
    
    if ("Horde" %in% names(active_adv_ct_vec())) {
      for (i in 1:(active_adv_ct_vec()[["Horde"]])) {
        fill_horde_feat(i, feat_df()[feat_df()$adv_type == "Horde",])
      }
      
      # first two features are 'spoken for'
      if (input$feat_fill_ct > 2 &&
          nrow(horde_feat_df()) >= (as.numeric(input$feat_fill_ct) - 2)) { # populate as available
        feat_idx_mat <- # row is adversary {#}, col is feature index in filtered feat_df()
          feat_sampler_idx(active_adv_ct_vec()[["Horde"]], horde_feat_df(), as.numeric(input$feat_fill_ct) - 2)
        
        for (i in 1:(active_adv_ct_vec()[["Horde"]])) {
          for (j in 1:(ncol(feat_idx_mat))) {
            update_inputs("Horde", i, j + 2, horde_feat_df(), feat_idx_mat[i, j])
          }
        }
      }
    }
    
    if ("Colossus_framework" %in% names(active_adv_ct_vec())) {
      for (i in 1:(active_adv_ct_vec()[["Colossus_framework"]])) {
        fill_col_fw_feat(i, feat_df()[feat_df()$adv_type == "Colossus_framework",])
      }
      
      # first two features are 'spoken for'
      if (input$feat_fill_ct > 1 &&
          nrow(col_fw_feat_df()) >= (as.numeric(input$feat_fill_ct) - 1)) { # populate as available
        feat_idx_mat <- # row is adversary {#}, col is feature index in filtered feat_df()
          feat_sampler_idx(active_adv_ct_vec()[["Colossus_framework"]], col_fw_feat_df(), as.numeric(input$feat_fill_ct) - 1)
        
        for (i in 1:(active_adv_ct_vec()[["Colossus_framework"]])) {
          for (j in 1:(ncol(feat_idx_mat))) {
            update_inputs("Colossus_framework", i, j + 1, col_fw_feat_df(), feat_idx_mat[i, j])
          }
        }
      }
    }
  })
  
  # populate features appropriate to adversary type when user indicates >0 features to fill ----
  observeEvent(feat_obsvr(), {
    req(active_adv_ct_vec(),
        input$feat_fill_ct)
    if (input$feat_fill_ct > 0) {
      for (i in 1:length(active_adv_ct_vec())) { # per adversary type
        adv_nm_ <- names(active_adv_ct_vec())[i]
        adv_ct_ <- unname(active_adv_ct_vec())[i]
        if (!(adv_nm_ %in% c("Minion", "Horde", "Colossus_framework"))) {
          feat_idx_mat <- # row is adversary {#}, col is feature index in filtered feat_df()
            feat_sampler_idx(adv_ct_, filter(feat_df(), adv_type == adv_nm_), as.numeric(input$feat_fill_ct))
          
          for (j in 1:adv_ct_) { # per count within type
            for (k in 1:min(5, as.numeric(input$feat_fill_ct), nrow(filter(feat_df(), adv_type == adv_nm_)))) {
              update_inputs(adv_nm_, j, k, filter(feat_df(), adv_type == adv_nm_), feat_idx_mat[j, k])
            }
            if (as.numeric(input$feat_fill_ct) < 5) {
              for (k in 5:(as.numeric(input$feat_fill_ct) + 1)) {
                update_inputs(adv_nm_, j, k, data.frame(), NULL)
              }
            }
          }
        }
      }
    } else if (input$feat_fill_ct == 0) { # reset to defaults
      for (i in 1:length(active_adv_ct_vec())) { # per adversary type
        adv_nm_ <- names(active_adv_ct_vec())[i]
        adv_ct_ <- unname(active_adv_ct_vec())[i]
        for (j in 1:adv_ct_) {
          for (k in 1:5) {
            update_inputs(adv_nm_, j, k, data.frame(), NULL)
          }
        }
      }
    }
  })
  
  # Colossus-specific case when multiple frameworks active ---------------------
  col_fw_names <- reactive({
    req(active_adv_ct_vec())
    if (adv_ct_vec()["Colossus_framework"] > 1) {
      lapply(1:(adv_ct_vec()["Colossus_framework"]), \(i) {
        input[[namify("Colossus_framework", i, "name")]]
      }) |> unlist()
    } else {"At most 1 Colossus framework"}
  })
  
  observeEvent(col_fw_names(), {
    col_fwk_ct <- adv_ct_vec()["Colossus_framework"]
    
    if (col_fwk_ct > 1 & any(col_fw_names() != "")) {
      str_col_segmt_ct <- adv_ct_vec()["Colossus_strong_segment"]
      if (str_col_segmt_ct > 0) {
        for (i in 1:str_col_segmt_ct) {
          updateSelectInput(
            inputId = namify("Colossus_strong_segment", i, "parent_frame"),
            choices = col_fw_names(),
            selected = NULL)
        }
      }
      
      avg_col_segmt_ct <- adv_ct_vec()["Colossus_average_segment"]
      if (avg_col_segmt_ct > 0) {
        for (i in 1:avg_col_segmt_ct) { 
          updateSelectInput(
            inputId = namify("Colossus_average_segment", i, "parent_frame"),
            choices = col_fw_names(), 
            selected = NULL) 
        }
      }
      
    }
  })
  
  # organize Colossus elements by framework membership -------------------------
  # update colossus 'adjacent segments' names as the names are populated
  col_seg_names_by_fwk <- reactive({
    req(isTruthy(input[["Colossus_strong_segment_1_name"]]) || isTruthy(input[["Colossus_average_segment_1_name"]]))
    id_colossus_components(input, adv_ct_vec(), id = "seg")
  })
  
  observeEvent(col_seg_names_by_fwk(), {
    col_fwk_ct <- adv_ct_vec()["Colossus_framework"]
    col_str_seg_ct <- adv_ct_vec()["Colossus_strong_segment"]
    col_avg_seg_ct <- adv_ct_vec()["Colossus_average_segment"]
    
    # note: choices begin with 'name' pasted at front to prevent 'initialization is empty' issue - sub() below removes this
    if (col_str_seg_ct > 0) {
      for (i in 1:col_str_seg_ct) {
        for (j in 1:4) {# 1:{# calls to build_col_mt_as() in build_colossus_spec_ui() in Customize section code.R}
          name_optns <- if (col_fwk_ct == 1) {col_seg_names_by_fwk()} else {col_seg_names_by_fwk()[[input[[namify("Colossus_strong_segment", i, "parent_frame")]]]]}
          name_optns <- sub("^name", "", name_optns)
          
          updateSelectInput(inputId = namify("Colossus_strong_segment", i, paste0("mottac_adj", j)),
                            choices = 
                              c("None", unique(name_optns)[unique(name_optns) != input[[namify("Colossus_strong_segment", i, "name")]]])
          )
        }
      }
    }
    
    if (col_avg_seg_ct > 0) {
      for (i in 1:col_avg_seg_ct) {
        for (j in 1:4) {# 1:{# calls to build_col_mt_as() in build_colossus_spec_ui() in Customize section code.R}
          name_optns <- if (col_fwk_ct == 1) {col_seg_names_by_fwk()} else {col_seg_names_by_fwk()[[input[[namify("Colossus_average_segment", i, "parent_frame")]]]]}
          name_optns <- sub("^name", "", name_optns)
          
          updateSelectInput(inputId = namify("Colossus_average_segment", i, paste0("mottac_adj", j)),
                            choices = 
                              c("None", unique(name_optns)[unique(name_optns) != input[[namify("Colossus_average_segment", i, "name")]]])
          )
        }
      }
    }
  })
  
  
  # server Run panel -----------------------------------------------------------
  output$adv_run <- 
    renderUI({
      req(cstm_chk() == 0L)
      output <- tagList()
      
      lapply(1:length(adv_runset()), \(i) {
        output[[i]] <- 
          if (grepl("Colossus_framework", adv_runset()[i])) {
            div(class = "adv-run adv-run-colossus-fw",
                column(12, build_colossus_fw_run_ui(input, adv_runset()[i], input$tier))
            )
          } else if (grepl("Colossus_.+_segment", adv_runset()[i])) {
            div(class = "adv-run adv-run-colossus-sg",
                column(12, build_colossus_sg_run_ui(input, adv_runset()[i], input$tier))
            )
          } else {
            div(class = "adv-run adv-run-gen",
                column(12, build_adv_run_ui(input, adv_runset()[i], input$tier))
            )
          }
        output
      })
    })
  
  # server Adversary Export panel ----------------------------------------------
  # Obsidian - Daggerforge subpanel --------------------------------------------
  json_file <- reactive({
    req(cstm_chk() == 0L)
    
    lapply(1:length(adv_runset()), \(i) {
      if (grepl("Colossus", adv_runset()[i])) {
        json_prep_adversary_col(input, a.t(adv_runset()[i]), a.n(adv_runset()[i]), input$tier,
                                fwk_id = colossus_fwk()[i], input$feat_fill_ct)
      } else {
        json_prep_adversary(input, a.t(adv_runset()[i]), a.n(adv_runset()[i]), input$tier, input$feat_fill_ct)
      }
    }) |> 
      jsonify()
  })
  
  output$json_dl <- downloadHandler(
    filename = function() {
      paste0("daggerforge_dagversary-", format(Sys.time(), "%d-%b-%Y %Hh %Mm %Z"), ".json")
    },
    content = function(file) {
      cat(json_file(), file = file)
    }
  )
  
  output$json_dl_prvw <- renderText({ json_file() })
  
  output$dgrfg_colossus_note <- 
    renderText({
      req(adv_runset())
      if (any(grepl("Colossus", names(active_adv_ct_vec())))) {
        "<p style = 'color:blue'><b>Note: as of this app's build, Daggerforge doesn't currently have standalone 'Colossus framework/segment' category for filtering by adversary type; Colossi will be under Source = 'Custom' and '(col fw)' (framework) '(col sg)' (segment) is added to the adversary names.</b></p>"
      }
    })
  
  # Obsidian - ITS Theme subpanel ----------------------------------------------
  markdown_file <- reactive({
    req(cstm_chk() == 0L)
    
    lapply(1:length(adv_runset()), \(i) {
      if (grepl("Colossus", adv_runset()[i])) {
        markdown_prep_adversary_col(input, a.t(adv_runset()[i]), a.n(adv_runset()[i]), input$tier,
                                    fwk_id = colossus_fwk()[i], input$feat_fill_ct)
      } else {
        markdown_prep_adversary(input, a.t(adv_runset()[i]), a.n(adv_runset()[i]), input$tier, input$feat_fill_ct)
      }
    }) |> 
      markdownize()
  })
  
  output$markdown_dl <- downloadHandler(
    filename = function() {
      paste0("obsidian_markdown_dagversary-", format(Sys.time(), "%d-%b-%Y %Hh %Mm %Z"), ".txt")
    },
    content = function(file) {
      cat(markdown_file(), file = file)
    }
  )
  
  output$mkdn_txt_dl_prvw <- renderText({ markdown_file() })
  
  # server Features panel ------------------------------------------------------
  output$feat_tbl_msg <- renderUI({
    if (adv_total() == 0) {HTML("<div style = 'font-size: 18px;'><i>This tab will update with general-use and relevant selected-adversary/-tier features when you make selections in the 'Start' tab.</i></div>")}
  })
  
  feat_tbl <- reactive({
    req(adv_runset())
    tryCatch({feat_df()}, error = \(e) {data.frame()})
  })
  
  output$features_df <- 
    DT::renderDT(feat_tbl(), options = list(striped = TRUE, hover = TRUE), rownames = FALSE)
  
  output$dynam_feat_notes <- renderUI({
    HTML(paste0("<div style = 'font-size: 15px;'>",
                "You can have adversary-specific details in the features you provide that this app will update. This works for the feature name and feature detail text fields, though generally it's meant for the feature detail text. Note that the 'dynamic' text must be within the &lt;&lt; &gt;&gt; 'double less than / double greater than' characters to work.<br>",
                "<br>",
                "Options currently supported are:<br>",
                "<br>",
                "&lt;&lt;tier&gt;&gt; : the tier number specified on the Start tab (can be useful for adversary-summoning details, number of attacks, or damage modifiers), ",
                "e.g. <i>Spend a Fear to make &lt;&lt;tier&gt;&gt; standard attacks against the magical mystical MacGuffin to up the stakes.</i><br>",
                "<br>",
                "&lt;&lt;exp_dmg&gt;&gt; : the expected value/average of the adversary's standard attack damage - the expected value of the 'Dice damage' or the 'Avg damage' number depending on if you select 'Use Avg' for the 'Damage dice' selection<br>",
                "<br>",
                "&lt;&lt;dmg&gt;&gt; : either the 'Dice damage' detail (e.g. '1d6+2') or the 'Avg damage' number (e.g. '5') depending on if you select 'Use Avg' for the 'Damage dice' selection<br>",
                "<br>",
                "&lt;&lt;square_tier&gt;&gt; : the square of the tier number (can be useful for damage modifiers)<br>",
                "<br>",
                "&lt;&lt;tierside_left&gt;&gt; : selects from the following listing of dice sides, in increasing position by tier number: [8, 10, 12, 20] (can be useful for higher-damage attacks), ",
                "e.g. <i>Spend a Fear to rhyme a word with orange - mortals within Close range must succeed on a Knowledge Reaction Roll or take 3d&lt;&lt;tierside_left&gt;&gt; magic damage. And no, 'door hinge' doesn't count.</i><br>",
                "<br>",
                "&lt;&lt;tierside_right&gt;&gt; : selects from the following listing of dice sides, in increasing position by tier number: [4, 6, 8, 10] (can be used for lower-damage attacks, though '0.5x dmg' or similar may make more sense)<br>",
                "<br>",
                "&lt;&lt;minion_pasv&gt;&gt; : plug in the 'Minion passive' value (only works for Minions, and only really used for the <i>Minion (#)</i> Passive feature)<br>",
                "<br>",
                "&lt;&lt;perhp&gt;&gt; : plug in the '{# Horde creatures per HP}' value (only works for Hordes, and only really used for the <i>Contains Multitudes</i> Passive feature)<br>",
                "<br>",
                "<b>Advanced functionality:</b><br>",
                "You can modify details to multiply the dynamic value (use a positive number less than 1 to divide) or add to/subtract from the value (use whole numbers for any addition/subtraction); ",
                "the following example doubles the dice damage value and adds 3 to the value (e.g. 1d6+2 becomes 2d6+5).<br>",
                "<br>",
                "<i>Mark a Stress and make a standard attack against a target within range. On a success, the target takes &lt;&lt;2x dmg +3&gt;&gt; physical damage.</i><br>",
                "<br>",
                "This app's code is meant for dice damage multipliers around 0.5 (half damage) to 3 (triple damage) - going beyond that range may not work, and there's a lower limit of 1d4.<br>",
                "<br>",
                "Modified 'Dice damage' details will try to calculate the combination of dice side and number of dice that has an expected value matching (or closest to) the multiplier/addition change - for example, if the 'Dice damage' detail is 3d10+4 (expected value: 20.5) and your feature includes &lt;&lt;1.5x dmg&gt;&gt;, the app will use 3d20-1 (expected value: 31.5; to keep the app from bogging down only a few addition/subtraction values are considered).<br><br>",
                "</div>"
    )
    )
  })
  
  # server Environment Builder panel -------------------------------------------
  output$env_dmg_note <- renderUI({
    HTML(paste0("Recommended damage range for this tier (for appropriate features): ",
           "<b>", env_ref_df[env_ref_df$tier == input$env_tier, "dmg_rng"], "</b>"))
  })
  
  output$env_notes <- renderUI({
    div(HTML(paste0(
               "<br><h4>Feature elements to consider</h4>",
               "Environment features often involve one or more of the following:",
               "<br><i>h/t RightKnightToFight's guide - see 'Credits' tab</i>",
               "<br>- Something (or things) for the party to interact with (possibly leading to an Action Roll, or simply setting the stage)",
               "<br>- Summoning or directly presenting a threat or challenge, including dealing damage/stress",
               "<br>- Alter the environment/scene (react to the party, signal a threat, take away or change opportunity)",
               "<br>- Provide context/set the tone for the environment",
               "<br><br><h4>Countdowns: wording in feature text</h4>",
               "- Please use only one countdown in a feature OR a paired Progress/Consequence countdown using fixed numbers in a single feature; Daggerforge processing will not automatically recognize and include the second countdown in a feature and this program only accounts for paired Progress/Consequence countdowns of type 'countdown (single #).",
               "<br>- Always use 'Countdown (#*)' for feature description text, where the '#*' element can use one of the following options:",
               "<br>- An actual number (e.g. 'Countdown (4)')",
               "<br>- A number with a modifying word 'loop', 'increasing', or 'decreasing' before the number (e.g. 'Countdown (loop 4)'",
               "<br>- A dice indicator for randomized countdowns (e.g. 'Countdown (1d4)'); a loop modifier can be applied (e.g. 'Countdown (loop 1d4)')",
               "<br><br>- Consider including descriptors like 'Progress' or 'Consequence' before 'Countdown'; this is especially important for a pursuit- or escape-type pair of countdowns to identify which countdown is which.",
               "<br>- When there are -multiple- countdowns in a single feature, the exported environment text for the Daggerforge JSON will intentionally replace the word 'Countdown' with 'Ctdown' as the JSON will create standalone countdown entries for the multiples; default Daggerforge processing may not capture both countdowns using the initial text and the 'intentional typo' will prevent the default processing from duplicating the countdowns.",
               "<br>- Note that because Daggerforge and ITS Theme countdown UI don't currently support automated increases to the counter maximum, an <i>increasing</i> countdown will have have 5 additional counter slots to provide some buffer counters.",
               "<br>- Countdowns will use the feature name as the countdown name; if there are multiple countdowns, 'Progress' and 'Consequence' will be added if those immediately precede 'Countdown'."
               )),
        style = "font-size: 16px;")
  })
  
  output$env_note1 <- renderUI({
    HTML(
      paste0(
        "<h4>Select the number of each type of environment below, then fill in details in the 'Customize' tab before exporting.</h4>",
        "<h4>See the 'Notes' tab for more details about environment features and countdowns.</h4>"
      )
    )
  })
  
  output$env_counts <- renderUI({
    lapply(seq_along(env_types), \(i) { build_env_count(typ = names(env_types)[i]) })
  })
  
  env_ct_vec <- reactive({
    lapply(seq_along(env_types), \(i) {
      validate(need(
        is.integer(input[[paste0(names(env_types)[i], "_count")]]) &&
          input[[paste0(names(env_types)[i], "_count")]] >= 0,
        paste0(names(env_types)[i], " count input must be a single non-negative whole number. Use '0' for none.")
      ))
    })
    v <- vapply(seq_along(env_types), 
                \(i){ input[[paste0(names(env_types)[i], "_count")]] },
                numeric(1L))
    names(v) <- names(env_types)
    v
  })
  
  active_env_ct_vec <- reactive({env_ct_vec()[env_ct_vec() > 0]})
  
  output$env_spec <-
    renderUI({
      req(min(active_adv_ct_vec()) > 0)
      output = tagList()
      
      lapply(seq_along(active_env_ct_vec()), \(i) {
        lapply(1:active_env_ct_vec()[i], \(j) {
          output[[i]] <- tagList()
          
          output[[i]][[j]] <- 
            div(class = "env-input",
                build_env_spec_ui(names(active_env_ct_vec())[i], j, input$env_tier) )
          
          output
        })
      })
    })
  
  env_runset <- reactive({
    req(min(active_env_ct_vec()) > 0)

    lapply(1:length(active_env_ct_vec()), \(z) {
          paste0(names(active_env_ct_vec())[z], "_", 1:(active_env_ct_vec()[z]))
        }) |> unlist(recursive = FALSE)
    })
  
  # tracking feature text to ensure proper updates to export text --------------
  env_feat_tracker <- reactive({
    req(env_runset())
    
    vapply(1:length(env_runset()), \(i) {
      vapply(1:5, \(j) {
        paste0(input[[paste0(env_runset(), "_featname_", j)]],
               input[[paste0(env_runset(), "_feattype_", j)]],
               input[[paste0(env_runset(), "_feattext_", j)]],
               input[[paste0(env_runset(), "_featquestion_", j)]])
      }, character(1L)) |> paste(collapse = "__")
    }, character(1L))
  })
  
  # server Environment Export panel --------------------------------------------
  # Obsidian - Daggerforge subpanel --------------------------------------------
  json_file_env <- reactive({
    req(env_runset(), env_feat_tracker())
    
    lapply(1:length(env_runset()), \(i) {
      jsonify_environment(input, a.t(env_runset()[i]), a.n(env_runset()[i])) # note: a.t and a.n originally built for adveraries, but exact same structure/functionality works for enviroment details
    }) |> 
      jsonify_env()
  })
  
  output$json_dl_env <- downloadHandler(
    filename = function() {
      paste0("daggerforge_dagversary_env-", format(Sys.time(), "%d-%b-%Y %Hh %Mm %Z"), ".json")
    },
    content = function(file) {
      cat(json_file_env(), file = file)
    }
  )
  
  output$json_dl_prvw_env <- renderText({ json_file_env() })
  
  # server Credits panel -------------------------------------------------------
  output$sources <- 
    renderUI({
      HTML(
        paste0(
          "<p style = 'font-size: 17px;'>This application includes materials from the Daggerheart System Reference Document 1.0, © Critical Role, LLC. All rights reserved.</p>",
          "<p style = 'font-size: 17px;'>Suggested adversary stats come from the RightKnight’s Guide to Making Custom Adversaries; available on <a href='https://heartofdaggers.com/products/making-custom-adversaries/' target='_blank'>Heart of Daggers</a> and <a href='https://www.drivethrurpg.com/en/product/526778/making-custom-adversaries-a-guide?__cf_chl_f_tk=ieOvKuTJrRapON6U4kOB1Gn4gYAdWN80uQwjmWvzF_0-1782857279-1.0.1.1-w3Fy.GazDAVKmT4.G5scrIpi9bRSP63GRn0h1VDiE0M' target='_blank'>Drive Thru RPG</a>",
          "<p style = 'font-size: 17px;'>Horde feature 'Contains Multitudes' and Minion feature 'Join or Die' heavily inspired by a post by Reddit user ThatZeroRed</p>"
        )
      )
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
