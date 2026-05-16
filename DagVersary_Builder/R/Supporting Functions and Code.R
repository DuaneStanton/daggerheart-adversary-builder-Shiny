# Functions and Code Setup for the DagVersary App

library(dplyr) # needed here for data.frame processing

# values used in multiple places in the app ------------------------------------
adv_types <- 
  c("Bruiser", "Horde", "Leader", "Minion", "Ranged", "Skulk", "Solo", "Standard", "Support", "Social")
tier_vals <- 1:4
distances <- c("Melee", "Very Close", "Close", "Far", "Very Far")

# build adversary type count ---------------------------------------------------
###
### ADD TOOLTIP WITH BRIEF OVERVIEW OF TYPE???
###
build_adv_count <- function(typ) {
  tags$div(class = "adv-count-sel",
  numericInput(inputId = paste0(typ, "_count"), label = paste0("# ", typ, "s"),
               value = 0, min = 0, step = 1),
  style="display:inline-block")
}

# read in CSV of recommended/starter adversary stats by type and tier ----------
# note: stat ranges come from (and full credit owed to)  
#       RightKnighttoFight’s Guide to Making Custom Adversaries v1.6
# note: Colossal (Framework/Average Segment / Strong Segment) not yet implemented

# function to calculate midpoint from vector of "# - #" strings
calc_midpt <- function(x){
  suppressWarnings(
  vapply(seq_along(x), \(i) {
    if (is.na(x[i])) {NA_real_ # will throw 'NAs introduced by coercion' warning
      } else if (grepl("-", x[i])) {
        x1 <- sub("^(-*\\d+).+", "\\1", x[i])
        x1 <- gsub(" ", "", x1) 
        x1 <- as.numeric(x1)
        
        x2 <- sub("^-*\\d+(.+)$", "\\1", x[i]) 
        x2 <- gsub(" ", "", x2) 
        x2 <- sub("-", "", x2) 
        x2 <- as.numeric(x2)
        
        ceiling(median(c(x1, x2)))
      } else {as.numeric(x[i])}
  }, numeric(1L))
  )
}

# note: when working interactively in this R script, use
#       "DagVersary_Builder/dagversary_stats_reference.csv" instead
adv_ref_df <- 
  read.csv("dagversary_stats_reference.csv") |> 
  mutate(
    across(.cols = c(ends_with("_rng"), minion_pasv), .fns = calc_midpt, .names = "{.col}_md"),
  )
md_cols_idx <- grep("_rng_md", colnames(adv_ref_df))
colnames(adv_ref_df)[md_cols_idx] <- sub("_rng", "", colnames(adv_ref_df)[md_cols_idx])

adv_ref_df$dice_pool_lst <- 
  lapply(1:nrow(adv_ref_df), \(i) {if (!is.na(adv_ref_df$dice_pool_optns[i])){
    strsplit(adv_ref_df$dice_pool_optns[i], split = ",")[[1]] }})

# customize tooltip for recommended difficulty range ---------------------------
recommend_range <- function(x, tier, typ, colnm) {
  paste("Recommended range:",
        x[[paste0(colnm, "_rng")]][x$tier == tier & x$adv_type == typ])
}

# build uiOutput as-needed for adversaries -------------------------------------
namify <- function(type, num, detail){paste0(type, "_", num, "_", detail)}

###
# .tooltiptext {
#   visibility: hidden; /* Hidden by default */
#     width: 130px;
#   background-color: black;
#   color: #ffffff;
#     text-align: center;
#   padding: 5px 0;
#   border-radius: 6px;
#   position: absolute;
#   z-index: 1; /* Ensure tooltip is displayed above content */
# }
###

textify <- function(typ, num, detail, placehold, wd = NULL) {
  textInput(inputId = namify(typ, num, detail), label = NULL, placeholder = placehold, width = wd)
}

numerify <- function(df_, tier, type, nbr, detail, lbltxt, valcol, divwd = "80px", inpwd = NULL) {
  df__ <- df_[df_$tier == tier & df_$adv_type == type,]
  div(title = recommend_range(df_, tier, type, detail), style = paste0("width: ", divwd),#"white-space: pre-line",
      numericInput(inputId = namify(type, nbr, detail),
                   label = lbltxt,
                   value = df__[[valcol]],
                   width = inpwd))
}

dmg_dice_pooler <- function(df_, tier, type, nbr, detail) {
  df__ <- df_[df_$tier == tier & df_$adv_type == type,]
  div(title = "Select higher constant for more consistency, or high dice-s-de for more swinginess",
      style = "width: 120px;",
      selectInput(inputId = namify(type, nbr, detail),
                  label = "Damage dice",
                  choices = c(unlist(df__$dice_pool_lst), "Use Avg")))
}

featurize <- function(type, num, detail_nbr) {
  dtl_ftnm <- paste0("featname_", detail_nbr)
  dtl_fttyp <- paste0("feattype_", detail_nbr)
  dtl_ftdsc <- paste0("feattext_", detail_nbr)
  fluidRow(
    column(width = 12,
           div(class = "bottom-aligned",
               div(textify(type, num, dtl_ftnm, "Feature name", "180px")),
               div(selectInput(namify(type, num, dtl_fttyp), label = "Feature type", choices = c("Passive", "Action", "Reaction"), width = "110px")),
               div(textify(type, num, dtl_ftdsc, "Feature description", "420px")) ))
    )
}

# UI for Customize tab per adversary -------------------------------------------
build_adv_spec_ui <- function(typ, num, tr) {
   div(
     h3(renderText({paste0("Tier ", tr, " ", typ, " (#", num, ")")})),
     textify(typ, num, "name", paste0(typ, "_", num, " (name)"), "250px"),
     textify(typ, num, "desc", "A brief description of the adversary", "400px"),
     textify(typ, num, "mottac1", "Motive/tactic 1", "200px"),
     textify(typ, num, "mottac2", "Motive/tactic 2", "200px"),
     textify(typ, num, "mottac3", "Motive/tactic 3", "200px"),
     fluidRow(
       column(width = 12, offset = 0,
              div(class = "bottom-aligned",
                  div(numerify(adv_ref_df, tr, typ, num, "diff", "Difficulty", "diff_md")),
                  div(numerify(adv_ref_df, tr, typ, num, "thresh_maj", "Major Threshold", "thresh_maj_md", "150px")), 
                  div(numerify(adv_ref_df, tr, typ, num, "thresh_sev", "Severe Threshold", "thresh_sev_md", "170px")),
                  div(numerify(adv_ref_df, tr, typ, num, "hp", "HP", "hp_md", "70px", "70px")),
                  div(numerify(adv_ref_df, tr, typ, num, "stress", "Stress", "stress_md", "60px", "60px")) )
       )),
     fluidRow(
       column(width = 12, offset = 0,
              div(class = "bottom-aligned",
                  div(numerify(adv_ref_df, tr, typ, num, "atk", "ATK", "atk_md", "70px", "70px")),
                  div(textify(typ, num, "wpn", "Weapon", "140px")),
                  div(selectInput(namify(typ, num, "rng"), label = "Weapon range", choices = distances, width = "110px")),
                  dmg_dice_pooler(adv_ref_df, tr, typ, num, "dmg_dice"),
                  numerify(adv_ref_df, tr, typ, num, "dmg_avg", "Avg damage", "dmg_avg_md", "100px", "120px"),
                  div(selectInput(namify(typ, num, "dmg_typ"), label = "Damage type", choices = c("phy", "mag", "phy & mag", "direct"), width = "120px")) )
       )),
     textify(typ, num, "exp", "Experience(s)", "350px"),
     ### DEV NOTE: REMOVE THIS ON RUN/OBSIDIAN IF ONLY PLACEHOLDER TEXT
     ### DEVNOTE: In the "Run" and "Obsidian" tabs, bold numbers / dice / "spend a Fear" (or spend {#} fear)
     featurize(typ, num, 1),
     featurize(typ, num, 2),
     featurize(typ, num, 3),
     featurize(typ, num, 4),
     featurize(typ, num, 5)
    )
}

# UI for Run tab per adversary -------------------------------------------------
list_adv_name <- function(inpt, typ, num, tr) {
  nm <- reactive({inpt[[namify(typ, num, "name")]]})
  paste0("<b><Big>", ifelse(nm() == "", "Adversary", nm()), "</Big></b> (T", tr, " ", typ, " \u0023", num, ")</p>")
}

list_adv_desc <- function(inpt, typ, num) {
  dsc <- reactive({inpt[[namify(typ, num, "desc")]]})
  paste0(ifelse(dsc() == "","The most foul, cruel, and bad-tempered rodent you ever set eyes on", dsc()))
}

list_motives_tactics <- function(inpt, typ, num) {
  mt1 <- reactive({inpt[[namify(typ, num, "mottac1")]]})
  mt2 <- reactive({inpt[[namify(typ, num, "mottac2")]]})
  mt3 <- reactive({inpt[[namify(typ, num, "mottac3")]]})
  
  if (any(mt1() != "", mt2() != "", mt3() != "")) {
    fluidRow(paste0("Motives and tactics: ", 
                    paste(ifelse(mt1() != "", mt1(), "_drop_"), 
                          ifelse(mt2() != "", mt2(), "_drop_"),
                          ifelse(mt3() != "", mt3(), "_drop_"), sep = ", ") |> 
                      gsub(pattern = " ,|_drop_,|, _drop_|,$", replacement = "")))
  }
}

list_stats_1 <- function(inpt, typ, num) {
  
  atk_txt <- reactive({ifelse(inpt[[namify(typ, num, "atk")]] > -1, 
                              paste0("+", inpt[[namify(typ, num, "atk")]]),
                              inpt[[namify(typ, num, "atk")]])})
  
  wpn_txt_lbl <- reactive({
    paste0(ifelse(inpt[[namify(typ, num, "wpn")]] == "", "{weapon}", inpt[[namify(typ, num, "wpn")]]), ": ",
           inpt[[namify(typ, num, "rng")]])})
  
  wpn_txt <- reactive({
    paste(if (inpt[[namify(typ, num, "dmg_dice")]] == "Use Avg") {avg_dmg()
      } else {dice_dmg()}, inpt[[namify(typ, num, "dmg_typ")]])
  })
  
  dice_dmg <- reactive({
    if (inpt$fight_type == "Tougher (add +1d4 to adversary damage rolls)") {
      paste(inpt[[namify(typ, num, "dmg_dice")]], "+1d4")
    } else if (inpt$fight_type == "Tougher (add +2 to adversary damage rolls)" &
               inpt[[namify(typ, num, "dmg_dice")]] != "Use Avg") {
      dmg_end <- sub(".+d(.+)", "\\1", inpt[[namify(typ, num, "dmg_dice")]])
      dmg_str <- sub("(.+d).+", "\\1", inpt[[namify(typ, num, "dmg_dice")]])
      paste0(dmg_str, as.numeric(dmg_end) + 2)
    } else {inpt[[namify(typ, num, "dmg_dice")]]}
    })
  
  avg_dmg <- reactive({
    if (inpt$fight_type == "Tougher (add +1d4 to adversary damage rolls)") {
      paste(inpt[[namify(typ, num, "dmg_avg")]], "+1d4")
    } else if (inpt$fight_type == "Tougher (add +2 to adversary damage rolls)") {
      inpt[[namify(typ, num, "dmg_avg")]] + 2
    } else {inpt[[namify(typ, num, "dmg_avg")]]}
    })
  
  df_ <- reactive({
    tibble(
      Diff = inpt[[namify(typ, num, "diff")]],
      Thresholds = paste(inpt[[namify(typ, num, "thresh_maj")]], "/", 
                        inpt[[namify(typ, num, "thresh_sev")]]),
      ATK = atk_txt(),
      !!sym(wpn_txt_lbl()) := wpn_txt()
    )
  })
  
  renderTable({df_()}, align = "c")
}

msg_hp_stress <- function(inpt, typ, num) {
  msg <- reactive({
    if (inpt[[namify(typ, num, "stress_run")]] == 0 & inpt[[namify(typ, num, "hp_run")]] > 0) {
      "<p style ='color: green; text-align: center'><b>Fully stressed! Now <i>vulnerable</i> and any incurred stress reduces HP by 1.</b></p>"
    } else if (inpt[[namify(typ, num, "hp_run")]] == 0) {"<p style ='color: red; text-align: center'><b>DEFEATED!</b></p>"}
  })
  
  renderUI(HTML(msg()))
}

process_feat_txt <- function(txt) {
  txt_ <- txt
  find_bolders <- stri_locate_all(str = txt, regex = ".ark a .tress|[0-9]+d[0-9]+|.pend a .ear|.pend [0-9]+ .ear")[[1]]

  if (!all(is.na(find_bolders))) {
    # account for adding '<b>' and '</b>' to the string as needed
    for (i in 1:nrow(find_bolders)) {
      find_bolders[i, "start"] <-  find_bolders[i, "start"] + 7 * (i - 1)
      find_bolders[i, "end"] <-  find_bolders[i, "end"] + 7 * (i - 1)
    }
    
    for (z in 1:nrow(find_bolders)) {
      txt_ <- 
        paste0(
          substr(txt_, start = 1, stop = find_bolders[z, "start"] - 1),
          "<b>", substr(txt_, start = find_bolders[z, "start"], stop = find_bolders[z, "end"]), "</b>",
          substr(txt_, start = find_bolders[z, "end"] + 1, stop = nchar(txt_))
          )
    }
    txt_
  } else {
    txt
  }
}

has_feattxt <- function(inpt, typ, num, ftrnum) {
  inpt[[namify(typ, num, paste0("feattext_", ftrnum))]] != ""
}

process_feature <- function(inpt, typ, num, ftrnum) {
  featnm <- reactive({
    if (has_feattxt(inpt, typ, num, ftrnum)) {
      ifelse(inpt[[namify(typ, num, paste0("featname_", ftrnum))]] == "", 
             "<b>{Feature}:", paste0("<b>", inpt[[namify(typ, num, paste0("featname_", ftrnum))]]))
    }
  })
  
  feattyp <- reactive({
    if (has_feattxt(inpt, typ, num, ftrnum)) {
      paste0(" - ", inpt[[namify(typ, num, paste0("feattype_", ftrnum))]], ":</b> ")
    }
  })
  
  feattxt <- reactive({
    if (has_feattxt(inpt, typ, num, ftrnum)) {
      process_feat_txt(inpt[[namify(typ, num, paste0("feattext_", ftrnum))]])
    }
  })
  
  if (has_feattxt(inpt, typ, num, ftrnum)) {paste0(featnm(), feattyp(), feattxt())}
}

classify_feattyp <- function(inpt, typ, num, ftrnum) {
  if (inpt[[namify(typ, num, paste0("feattype_", ftrnum))]] == "Passive") {1
  } else if (inpt[[namify(typ, num, paste0("feattype_", ftrnum))]] == "Action"){2
  } else if (inpt[[namify(typ, num, paste0("feattype_", ftrnum))]] == "Reaction"){3}
}

list_features <- function(inpt, typ, num) {
  # list Passives, then Actions, then Reactions...if feature text present
  feattypes_set <- reactive({
    c(if (has_feattxt(inpt, typ, num, 1)) {classify_feattyp(inpt, typ, num, 1)} else {0},
      if (has_feattxt(inpt, typ, num, 2)) {classify_feattyp(inpt, typ, num, 2)} else {0},
      if (has_feattxt(inpt, typ, num, 3)) {classify_feattyp(inpt, typ, num, 3)} else {0},
      if (has_feattxt(inpt, typ, num, 4)) {classify_feattyp(inpt, typ, num, 4)} else {0},
      if (has_feattxt(inpt, typ, num, 5)) {classify_feattyp(inpt, typ, num, 5)} else {0})
  })
  
  feat_list <- reactive({
    if (!all(feattypes_set() == 0)) {
      x <- 
      do.call(
        what = rbind,
        args = lapply(1:length(feattypes_set()), \(z){
          data.frame(idx = z, typval = feattypes_set()[z],
                     txt = process_feature(inpt, typ, num, z)) })
        )
      x <- x[x$typval > 0,]
      x <- x[order(x$typval),]
      #idx <- x$idx
      
      ###
      ### RESUME HERE NEED TROUBLESHOOT HERE WHEN MULTIPLE ENTRIES
      ###
      # paste(
      #   unlist(
      #    lapply(1:length(idx), \(z) {process_feature(inpt, typ, num, idx)})
      #   ),
      #   collapse = "<br>"
      # )
      
      
      
      # vapply(1:length(idx), \(z) {process_feature(inpt, typ, num, idx)}, character(1L)) |> 
      #   paste(collapse = "<br>")
    }
  })
  if (!is.null(feat_list())) {
    renderUI(HTML(feat_list()))
  }
}

build_adv_run_ui <- function(inpt, typ, num, tr, dmg_add) {
  column(width = 5,
    fluidRow(renderUI({HTML(list_adv_name(inpt, typ, num, tr))})),
    fluidRow(list_adv_desc(inpt, typ, num)),
    list_motives_tactics(inpt, typ, num),
    fluidRow(list_stats_1(inpt, typ, num)),
    fluidRow(msg_hp_stress(inpt, typ, num)),
    fluidRow(
      column(width = 2),
      column(width = 4,
             div(selectInput(namify(typ, num, "hp_run"), label = "HP", choices = c(0:inpt[[namify(typ, num, "hp")]]), selected = inpt[[namify(typ, num, "hp")]], width = "70px"))),
      column(width = 6,
             div(selectInput(namify(typ, num, "stress_run"), label = "Stress", choices = c(0:inpt[[namify(typ, num, "stress")]]), selected = inpt[[namify(typ, num, "stress")]], width = "70px")))
    ),
    fluidRow(list_features(inpt, typ, num)) ###
    ### RESUME HERE - ADD STATUS (SELECT MULTIPLE PERMITTED, INCLUDE 'DEFEATED!' WHICH IS AUTO-SELECTED WHEN hp_run == 0)
  )
}
