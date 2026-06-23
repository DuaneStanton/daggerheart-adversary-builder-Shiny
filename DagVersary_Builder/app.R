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
    .inline label{ 
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
               div(h5("Note: if you're running these via Obsidian - Daggerforge and want multiples of a specific adversary, just create them once here and specify the count in the Daggerforge plugin '- [#] +' interface.")),
               uiOutput("adv_counts", 
                        label = "Specify the # of adversaries by type"),
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
                   selectInput("feat_fill_ct", "# filled features per adversary", choices = 0:5, selected = 0, width = "100px")),
               uiOutput("adv_spec")
               ), 
      tabPanel(div("Run", style = "font-size: 16px; color: #e8d37d;"),
               div(h4("Use this panel to run adversaries in-app from the 'Customize' tab")),
               div(uiOutput("adv_run")),
               #textOutput("RUNCHECK")### REMOVE WHEN DONE TESTING
               ), 
      tabPanel(div("Obsidian - Daggerforge", style = "font-size: 16px; color: #e8d37d;"),
               div(h4("Download a JSON file from this tab to upload to Obsidian via the Daggerforge plugin if running adversaries there. First build adversaries using details from the 'Customize' tab. You -may- need to close and reopen Obsidian after uploading to access newly-added adversaries, which will be available in the 'Custom' category of the 'Source' filter.")),
               htmlOutput("dgrfg_colossus_note"),
               downloadButton("json_dl", label = "Download JSON file for Daggerforge"),
               div(h4("The downloaded .json will look like the below:")),
               verbatimTextOutput(outputId = "json_dl_prvw")
               ), 
      tabPanel(div("Obsidian - ITS Theme", style = "font-size: 16px; color: #e8d37d;"),
               div(h4("Download a text file from this tab to select all > copy > paste into Obsidian if running adversaries there and you have the ITS Theme set up. First build adversaries using details from the 'Customize' tab.")),
               div(span("Note: you", style = "font-size: 16px;"), span(" can ", style = "color: blue; font-size: 16px;"), span("copy-paste this text into Obsidian without the ITS Theme applied, but the formatting looks much worse.", style = "font-size: 16px")),
               downloadButton("markdown_dl", label = "Download text file of Markdown for copy-pasting to Obsidian"),
               div(h4("The downloaded .txt file will look like the below:")),
               verbatimTextOutput(outputId = "mkdn_txt_dl_prvw")
               ),
      tabPanel(div("Feature Table", style = "font-size: 16px; color: #e8d37d;"),
               div(h4("Lookup table of features for adversaries")),
               div(uiOutput("feat_tbl_msg")),
               div(dataTableOutput("features_df")),
               div(h4("Notes for dynamic feature details")),
               htmlOutput("dynam_feat_notes")
               ),
    tabPanel(div("Credits", style = "font-size: 16px; color: #e8d37d;"),
             htmlOutput("sources")
             )
      )
)

# Define server ================================================================
server <- function(input, output, session) {
  
  output$use_note <- renderText({
    "<p>Custom adversary builder using <i>RightKnighttoFight</i>'s Guide and the Daggerheart SRD (see <b>Credits</b> tab)</p>"
  })

  ### TODO:
  ### - add validation checks with more user-friendly error messages
  
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
  colossus_groupset <- reactive({id_colossus_components(input, adv_ct_vec(), id = "grp")})
  colossus_fwk <- reactive({id_colossus_components(input, adv_ct_vec(), id = "fwk")})
  
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
  # id text for outputting adversaries in order
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
  
  output$adv_run <- 
    renderUI({
      req(adv_runset())
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
  
  # monitor Stress status and set 'Vulnerable' condition as appropriate --------
  adv_w_stress <- reactive({ adv_runset()[!grepl("Colossus_.+_segment", adv_runset())] })
  
  ###
  ### MAY NEED TO REVISE THIS IF KEEPING - INIT TRIGGERS VAPPLY ERROR
  ###
  # have_stress_0 <- reactive({
  #     req(list(length(adv_w_stress()) > 0L,
  #              input[[namify(a.t(adv_w_stress()[1]), a.n(adv_w_stress()[1]), "stress_run")]] >= 0))
  #     vapply(1:length(adv_w_stress()), \(i) {
  #       input[[namify(a.t(adv_w_stress()[i]), a.n(adv_w_stress()[i]), "stress_run")]] == 0
  #       }, logical(1L))
  #   })

  #stress_0_adv <- reactive({ adv_w_stress()[have_stress_0()] })

  ###
  ### ALL STEPS DOWN TO HERE WORK...
  ###
  # observeEvent(stress_0_adv(), {
  #   if (length(stress_0_adv()) > 0L) {
  #     for (i in 1:length(stress_0_adv())) {
  #       updateCheckboxGroupInput(
  #         inputId = paste0(stress_0_adv()[i], "_conds"),
  #         selected = unique(c(input[[namify(a.t(adv_w_stress()[i]), a.n(adv_w_stress()[i]), "stress_run")]],
  #                             "Vulnerable"))
  #       )
  #     }
  #   }
  # })
  
  #output$RUNCHECK <- renderText(paste(stress_0_adv(), collapse = " / "))
  
  # server Obsidian - Daggerforge panel ----------------------------------------
  json_file <- reactive({
    req(adv_runset())
    
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
  
  output$json_dl_prvw <- renderText({json_file()})
  
  output$dgrfg_colossus_note <- 
    renderText({
      req(adv_runset())
      if (any(grepl("Colossus", names(active_adv_ct_vec())))) {
        "<p style = 'color:blue'><b>Note: as of this app's build, Daggerforge doesn't currently have standalone 'Colossus framework/segment' category for filtering by adversary type; Colossi will be under Source = 'Custom' and '(col fw)' (framework) '(col sg)' (segment) is added to the adversary names.</b></p>"
      }
    })
  
  # server Obsidian - ITS Theme panel ------------------------------------------
  
  markdown_file <- reactive({
    req(adv_runset())
    
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
  
  output$mkdn_txt_dl_prvw <- renderText({
    req(adv_runset())
    markdown_file()
    })
  
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
      "e.g. <i>Spend a Fear to make &lt;&lt;tier&gt;&gt; standard attacks against the basket of cute something-or-others you introduced as a Chekhov's Emotionally Damaging Event plot device. Then take a good hard look in the mirror and think about what you've done.</i><br>",
      "<br>",
      "&lt;&lt;exp_dmg&gt;&gt; : the expected value/average of the adversary's standard attack damage - the expected value of the 'Dice damage' or the 'Avg damage' number depending on if you select 'Use Avg' for the 'Damage dice' selection<br>",
      "<br>",
      "&lt;&lt;dmg&gt;&gt; : either the 'Dice damage' detail (e.g. '1d6+2') or the 'Avg damage' number (e.g. '5') depending on if you select 'Use Avg' for the 'Damage dice' selection<br>",
      "<br>",
      "&lt;&lt;square_tier&gt;&gt; : the square of the tier number (can be useful for damage modifiers)<br>",
      "<br>",
      "&lt;&lt;tierside_left&gt;&gt; : selects from the following listing of dice sides, in increasing position by tier number: [8, 10, 12, 20] (can be useful for higher-damage attacks), ",
      "e.g. <i>Spend a Fear to divide by zero - mortals within Close range must succeed on a Knowledge Reaction Roll or take 3d&lt;&lt;tierside_left&gt;&gt; magic damage.</i><br>",
      "<br>",
      "&lt;&lt;tierside_right&gt;&gt; : selects from the following listing of dice sides, in increasing position by tier number: [4, 6, 8, 10] (can be used for lower-damage attacks, though '0.5x dmg' or similar may make more sense)<br>",
      "<br>",
      "&lt;&lt;minion_pasv&gt;&gt; : plug in the 'Minion passive' value (only works for Minions, and only really used for the <i>Minion (#)</i> Passive feature<br>",
      "<br>",
      "&lt;&lt;perhp&gt;&gt; : plug in the '{# Horde creatures per HP}' value (only works for Hordes, and only really used for the <i>Contains Multitudes</i> Passive feature<br>",
      "<br>",
      "Advanced functionality:<br>",
      "You can modify details to multiply the dynamic value (use a positive number less than 1 to divide) or add to/subtract from the value; ",
      "the following example doubles the dice damage value and adds 3 to the value (e.g. 1d6+2 becomes 2d6+5)<br>",
      "<br>",
      "<i>Mark a Stress and make a standard attack against a target within range. On a success, the target takes &lt;&lt;2x dmg +3&gt;&gt; physical damage.</i><br>",
      "<br>",
      "This app's code is meant for dice damage multipliers around 0.5 (half damage) to 3 (triple damage) - going beyond that range may not work, and there's a lower limit of 1d4.<br>",
      "Modified 'Dice damage' details will try to calculate the combination of dice side and number of dice that has an expected value matching (or closest to) the multiplier/addition change - for example, if the 'Dice damage' detail is 3d10+4 (expected value: 20.5) and your feature includes &lt;&lt;1.5x dmg&gt;&gt;, the app will use 3d20-1 (expected value: 31.5; to keep the app from bogging down only a few addition/subtraction values are considered).",
      "</div>"
      )
    )
  })
  
  # server Credits panel -------------------------------------------------------
  output$sources <- 
    renderUI({
      HTML(
        paste0(
          "<p style = 'font-size: 17px;'>This website includes materials from the Daggerheart System Reference Document 1.0, © Critical Role, LLC. All rights reserved.</p>",
          "<p style = 'font-size: 17px;'>Suggested adversary stats come from the <a href='https://docs.google.com/document/d/12g-obIkdGJ_iLL19bS0oKPDDvPbPI9pWUiFqGw8ED88/edit?tab=t.0#heading=h.mdjo15f06zjv'>RightKnighttoFight’s Guide to Making Custom Adversaries v1.6</a> Google Doc</p>",
          "<p style = 'font-size: 17px;'>Horde feature 'Contains Multitudes' and Minion feature 'Join or Die' heavily inspired by a post by Reddit user ThatZeroRed</p>"
        )
      )
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
