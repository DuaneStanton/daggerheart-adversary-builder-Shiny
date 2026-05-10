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
  tags$div(
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

# setwidth <- function(type, num, detail, wd_pct=120){
#   tags$head(tags$style(type="text/css", 
#                        paste0("#", namify(type, num, detail), "{width:", wd_pct, "%;}"))
#             )
# }

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

numerify <- function(df_, tier, type, nbr, detail, lbltxt, valcol) {
  df__ <- df_[df_$tier == tier & df_$adv_type == type,]
  tags$div(title = recommend_range(df_, tier, type, detail), style = "width: 80px",#"white-space: pre-line",
            numericInput(inputId = namify(type, nbr, detail),
                         label = lbltxt,
                         value = df__[[valcol]]))
}

dmg_dice_pooler <- function(df_, tier, type, nbr, detail) {
  df__ <- df_[df_$tier == tier & df_$adv_type == type,]
  tags$div(title = "Select higher constant for more consistency, or high dice-s-de for more swinginess",
           style = "width: 130px;",
           selectInput(inputId = namify(type, nbr, detail),
                       label = "Damage dice",
                       choices = c(unlist(df__$dice_pool_lst), "Use Avg instead")))
}


# styl_fr <- "width = 100px; height: 130px; display: inline-block; vertical-align: bottom"
# styl_fr2 <- "width = 150px; height: 130px; display: inline-block; vertical-align: bottom"

build_adv_ui_1 <- function(inpt, typ, num, tr) {
  ###renderUI({
    # Name (Tier / Type)
    # Brief desc
    # Motive(s)
    # Difficulty / Thresholds / ATK / Weapon: Range (_ dice __dmg typ)
    # HP / Stress
    # Experience
    # Features

    tags$div(
      h3(renderText({paste0("Tier ", tr, " ", typ, " (#", num, ")")})),
      textify(typ, num, "name", paste0(typ, "_", num, " (name)")),
      textify(typ, num, "desc", "A brief description of the adversary"),
      textify(typ, num, "mottac1", "Motive/tactic 1"),
      textify(typ, num, "mottac2", "Motive/tactic 2"),
      textify(typ, num, "mottac3", "Motive/tactic 3"),
      ###
      ### ALLOW USER TO CHECK A BOX FOR 'AVG DMG' (STATIC #) OR 'DICE DMG' (DROPDOWN SEL)??
      ### NOTE: +2 OR +1d4 SHOULD BE APPLICABLE FOR -EITHER-
      ###
      ### LET USER CHECK A BOX TO MODIFY DICE # / DICE SIDE # / ATK + ???
      fluidRow(
        column(width = 12, 
               div(class = "bottom-aligned",
                   div(numerify(adv_ref_df, tr, typ, num, "diff", "Difficulty", "diff_md")),
                   div(numerify(adv_ref_df, tr, typ, num, "thresh_maj", "Major Threshold", "thresh_maj_md")), 
                   div(numerify(adv_ref_df, tr, typ, num, "thresh_sev", "Severe Threshold", "thresh_sev_md")),
                   div(numerify(adv_ref_df, tr, typ, num, "hp", "HP", "hp_md")),
                   div(numerify(adv_ref_df, tr, typ, num, "stress", "Stress", "stress_md")) )
        )),
      fluidRow(
        column(width = 12, 
               div(class = "bottom-aligned",
                   div(numerify(adv_ref_df, tr, typ, num, "atk", "ATK", "atk_md")),
                   div(textify(typ, num, "wpn", "Weapon", "140px")),
                   div(selectInput(namify(typ, num, "rng"), label = "Weapon range", choices = distances, width = "110px")),
                   dmg_dice_pooler(adv_ref_df, tr, typ, num, "dmg_dice"),
                   numerify(adv_ref_df, tr, typ, num, "dmg_avg", "Avg damage", "dmg_avg_md"),
                   div(selectInput(namify(typ, num, "dmgtyp"), label = "Damage type", choices = c("phy", "mag"), width = "100px")) )
        )),
      textify(typ, num, "exp", "Experience(s)"), ### DEV NOTE: REMOVE THIS ON RUN/OBSIDIAN IF ONLY PLACEHOLDER TEXT
      ### RESUME HERE: TEXT FOR FEATURE NAME AND ANOTHER FOR FEATURE DETAIL
      ### DEVNOTE: In the "Run" and "Obsidian" tabs, bold numbers / dice / "spend a Fear" (or spend {#} fear)
                   
    )
    # renderText({
    #   paste0("<p><b>")
    # })
  ###})
}
