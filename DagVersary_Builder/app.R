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
    .shiny-html-output .shiny-bound-output {
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
    .inline .selectize-input.full.has-items.has-options {
      width: 60px;
    }
    #json_dl {
      background-color: #53386b;
      color: #e0c34c;
      border-color: #320e45;
    }
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
               div(h5("Note: if you're running these via Obsidian - Daggerforge and want multiples of a specific adversary, just create them once here and specify the count in the Daggerforge plugin '- [#] +' interface.")),
               uiOutput("adv_counts", 
                        label = "Specify the # of adversaries by type"),
               htmlOutput("warning_battle_points"),
               htmlOutput("battle_points"),
               htmlOutput("colossus_note")
               ),
      tabPanel("Customize",
               div(h4("Customize details for your adversaries below, then move to 'Run'")),
               htmlOutput("dmg_note"),
               div(class="inline", title = "Minions and Hordes have different (default) behavior", style = "width: 300px;",
                   selectInput("feat_fill_ct", "# filled features per adversary", choices = 0:5, selected = 0, width = "100px")),
               uiOutput("adv_spec")
               ), 
      tabPanel("Run",
               div(h4("Use this panel to run adversaries in-app from the 'Customize' tab")),
               div(uiOutput("adv_run")),
               textOutput("RUNCHECK")### REMOVE WHEN DONE TESTING
               ), 
      ### USER-INTERACTIVE FOR RUNNING FROM SERVER - INCLUDE DICE ROLLER AND BUTTON PER ADVERSARY???
      tabPanel("Obsidian - Daggerforge",
               div(h4("Download JSON file from this tab to upload to Obsidian via the Daggerforge plugin if running adversaries there. First build adversaries using details from the 'Customize' tab. You'll need to close and reopen Obsidian after uploading to access newly-added adversaries.")),
               htmlOutput("dgrfg_colossus_note"),
               downloadButton("json_dl", label = "Download JSON file for Daggerforge"),
               div(h4("The downloaded .json will look like the below:")),
               verbatimTextOutput(outputId = "json_dl_prvw")
               ), 
      ### OPTIONAL COPYABLE TEXT FOR RUNNING IN OBSIDIAN
      tabPanel("Obsidian - ITS Theme",
               div(h4("Copy from this tab to paste into Obsidian if running adversaries there. First build adversaries using details from the 'Customize' tab.")),
               "WORK IN PROGRESS"
               ),
      tabPanel("Credits",
               htmlOutput("sources")
               ), 
      tabPanel("Feature Listing",
               div(h4("Lookup table of features for adversaries")),
               div(dataTableOutput("features_df"))
               )
      )
)

# Define server ================================================================
server <- function(input, output) {

  output$use_note <- renderText({
    "<p>Custom adversary builder using <i>RightKnighttoFight</i>'s Guide and the Daggerheart SRD (see <b>Credit</b> tab)</p>"
  })

  ### TODO:
  ### - work out Obsidian-friendly copy-able formatted tab (LIKELY FUNCTION)
  ### - add validation checks
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
  
  ###
  ### TODO: NEED AT MINIMUM AS MANY SEGMENTS AS FRAMEWORKS - PROVIDE MESSAGE IF THIS CONDITION IS NOT MET
  ### TODO: PROVIDE A WARNING IF # FRAMEWORKS > 1
  ### TODO: PROVIDE A MESSAGE THAT BATTLE POINTS BUDGET ISN'T REALLY APPLICABLE FOR COLOSSI - CREATE SOMETHING THAT FOLLOWS THE FICTION AND WILL BE FUN FOR YOUR CAMPAIGN!
  ###
  
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
      "<p style = 'color: darkred; font-size: 18px;'><b><i>'battle points' budget is meant for non-Colossus-including encounters; a Colossus can be its own encounter. If adding other adversaries think of how they may interact with the Colossus.</i></b>"
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
    req(active_adv_ct_vec())
    filter(feat_ref_df, tier == input$tier, adv_type %in% names(active_adv_ct_vec()))
  })
  
  horde_df <- reactive({ # used for the -non- 'Horde (#)' features set
    req(feat_df())
    if ("Horde" %in% feat_df()$adv_type) {
      filter(feat_df(), adv_type == "Horde" & !grepl("Horde \\(", feat_name))
    }
  })
  
  feat_obsvr <- reactive({list(active_adv_ct_vec(), input$feat_fill_ct)})
  
  # function to streamline the update...Input process
  update_inputs <- function(adv_nm, adv_num, inpt_num, adv_df, val_idx = NULL) {
    updateTextInput(inputId = namify(adv_nm, adv_num, paste0("featname_", inpt_num)),
                    value = ifelse(is.null(val_idx), "", adv_df$feat_name[val_idx]))
    updateSelectInput(inputId = namify(adv_nm, adv_num, paste0("feattype_", inpt_num)),
                      selected = if (is.null(val_idx)) {"Passive"} else {adv_df$feat_type[val_idx]})
    updateTextInput(inputId = namify(adv_nm, adv_num, paste0("feattext_", inpt_num)),
                    value = ifelse(is.null(val_idx), "", adv_df$feat_text[val_idx]))
  }
  
  # function to fill Minion features
  fill_minion_feat <- function(id_ct, feat_df) {
    df_ <- feat_df[feat_df$adv_type == "Minion",] # already sorted so Passive then Action 
    update_inputs("Minion", id_ct, 1, df_, 1)
    update_inputs("Minion", id_ct, 2, df_, 2)
  }
  
  # function to ensure first Horde feat is always...the 'Horde' feat
  fill_horde_feat <- function(id_ct, feat_df) {
    df_ <- as.data.frame(feat_df[feat_df$adv_type == "Horde" & grepl("Horde \\(", feat_df$feat_name),])
    update_inputs("Horde", id_ct, 1, df_, 1)
  }
  
  smpl_1 <- function(x){sample(x, size = 1, replace = FALSE, prob = NULL)}
  
  # generating a feature set per adversary, with no repeats of features per adversary
  feat_sampler_idx <- function(n_sametyp_adv, feat_df, n_feat) {
    feat_n <- min(nrow(feat_df), n_feat)
    idx_psv <- which(feat_df$feat_type == "Passive")
    idx_act <- which(feat_df$feat_type == "Action")
    idx_rct <- which(feat_df$feat_type == "Reaction")
    idx_set <- c(idx_psv, idx_act, idx_rct)
    # first selection must be either passive or action
    # ...ensure no repeats per adversary for feature set
    feat_mat <- matrix(nrow = n_sametyp_adv, ncol = feat_n)
    smpl_ <- sample(c(idx_psv, idx_act), size = n_sametyp_adv, replace = TRUE)
    feat_mat[, 1] <- smpl_
    
    if (feat_n > 1) {
      for (i in 1:n_sametyp_adv) {
        for (j in 2:feat_n) {
          if (length(setdiff(idx_set, feat_mat[i, 1:(j- 1)])) >= 1) {
            feat_mat[i, j] <- smpl_1(setdiff(idx_set, feat_mat[i, 1:(j - 1)]))
          }
        }
      }
      feat_mat
    } else {feat_mat}
  }
  
  # populate features appropriate to adversary type when user indicates >0 features to fill ----
  observeEvent(feat_obsvr(), {
    req(active_adv_ct_vec(),
        input$feat_fill_ct)
    if (input$feat_fill_ct > 0) {
      for (i in 1:length(active_adv_ct_vec())) { # per adversary type
        adv_nm_ <- names(active_adv_ct_vec())[i]
        adv_ct_ <- unname(active_adv_ct_vec())[i]
        if (!(adv_nm_ %in% c("Minion", "Horde"))) {
          feat_idx_mat <- # row is adversary {#}, col is feature index in filtered feat_df()
            feat_sampler_idx(adv_ct_, filter(feat_df(), adv_type == adv_nm_), as.numeric(input$feat_fill_ct))
        }
        
        for (j in 1:adv_ct_) { # per count within type
          if (adv_nm_ == "Minion") {
          fill_minion_feat(j, feat_df()) 
          } else if (adv_nm_ == "Horde") {
            fill_horde_feat(j, feat_df()[feat_df()$adv_type == adv_nm_,]) 
            ###
            ### MAY NEED TROUBLESHOOT - SOMETIMES DUPLICATES ENTRIES WHEN FEW OPTIONS ; MORE FEATURES AT LOWER LEVELS WOULD MINIMIZE RISK
            ###
            if (input$feat_fill_ct > 1) { # populate as available
              if (nrow(horde_df()) >= (as.numeric(input$feat_fill_ct) - 1)) {
                
                feat_idx_mat <- # row is adversary {#}, col is feature index in filtered feat_df()
                  feat_sampler_idx(adv_ct_, 
                                   filter(horde_df(), adv_type == adv_nm_), 
                                   as.numeric(input$feat_fill_ct) - 1)

                for (k in 2:as.numeric(input$feat_fill_ct)) {
                  update_inputs(adv_nm_, j, k, horde_df(), feat_idx_mat[j, k - 1])
                }
              }
            }
            if (as.numeric(input$feat_fill_ct) < 5) {
              for (k in 5:(as.numeric(input$feat_fill_ct) + 1)) {
                update_inputs(adv_nm_, j, k, data.frame(), NULL)
              }
            }
            
          } else {### non-Horde, non-Minion set
            for (k in 1:as.numeric(input$feat_fill_ct)) {
              update_inputs(adv_nm_, j, k, filter(feat_df(), adv_type == adv_nm_), feat_idx_mat[j, k])
            }
            if (as.numeric(input$feat_fill_ct) < 5) {
              for (k in 5:(as.numeric(input$feat_fill_ct) + 1)) {
                update_inputs(adv_nm_, j, k, data.frame(), NULL)
              }
            }
          }
          ### ADD COLOSSUS FRAMEWORK EXCEPTION FOR FEATURE 1 AS FIXED???
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
    
    ###
    ### TODO: LOOK INTO MERGING STRONG/AVERAGE SEGMENT CODE LOGIC...USING col_seg_names_by_fwk() ???
    ###
    
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
                                fwk_id = colossus_fwk()[i])
      } else {
        json_prep_adversary(input, a.t(adv_runset()[i]), a.n(adv_runset()[i]), input$tier)
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
  
  output$json_dl_prvw <- renderText({json_file()})#renderPrint({cat(json_file())})
  
  output$dgrfg_colossus_note <- 
    renderText({
      if (any(grepl("Colossus", names(active_adv_ct_vec())))) {
        "<p style = 'color:blue'><b>Note: as of this app's build, Daggerforge doesn't currently have standalone 'Colossus framework/segment' category for filtering by adversary type; Colossi will be under Source = 'Custom' and '(col fw)' (framework) '(col sg)' (segment) is added to the adversary names.</b></p>"
      }
    })
  
  # server Obsidian - ITS Theme panel ------------------------------------------
  
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
  
  # server Features panel ------------------------------------------------------
  feat_tbl <- reactive({
    tryCatch({select(feat_df(), -feat_detail_note)}, error = \(e) {data.frame()})
    })
  
  output$features_df <- 
    DT::renderDataTable(feat_tbl(), options = list(striped = TRUE, hover = TRUE))
}

# Run the application 
shinyApp(ui = ui, server = server)
