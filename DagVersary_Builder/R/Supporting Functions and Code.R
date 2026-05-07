# Functions and Code Setup for the DagVersary App

library(dplyr)

# values used in multiple places in the app ------------------------------------
adv_types <- 
  c("Bruiser", "Horde", "Leader", "Minion", "Ranged", "Skulk", "Solo", "Standard", "Support", "Social")
tier_vals <- 1:4

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
###
### RESUME HERE - EXTRACT START VALUES FROM LISTED RANGES
###
adv_ref_df <- read.csv("dagversary_stats_reference.csv")

# customize tooltip for recommended difficulty range ---------------------------
recommend_difficulty <- function(tier, typ) {
  paste("Recommended difficulty range:",
        adv_ref_df$diff_rng[adv_ref_df$tier == tier & adv_ref_df$adv_type == typ])
}

# build uiOutput as-needed for adversaries -------------------------------------
build_adv_ui <- function(typ, num, tier) {
  ###renderUI({
    # Name (Tier / Type)
    # Brief desc
    # Motive(s)
    # Difficulty / Thresholds / ATK / Weapon: Range (_ dice __dmg typ)
    # HP / Stress
    # Experience
    # Features
    
    # possible ref re: looped content of renderUI
    # https://stackoverflow.com/questions/42169380/shiny-renderui-with-multiple-inputs
    
    #"<p style ='color: red'><b><i>Adversary total > intended # adversaries</i></b></p>"
    
    tags$div(
      renderText({paste0("Tier ", tier, " ", typ, " (#", num, ")")}),
      textInput(inputId = paste0(typ, "_", num, "_name"), 
                label = NULL,
                placeholder = paste0(typ, "_", num, " (name)")),
      textInput(inputId = paste0(typ, "_", num, "_desc"), 
                label = NULL,
                placeholder = "A brief description of the adversary"),
      textInput(inputId = paste0(typ, "_", num, "_mottac1"), 
                label = NULL,
                placeholder = "Motive/tactic 1"),
      textInput(inputId = paste0(typ, "_", num, "_mottac2"), 
                label = NULL,
                placeholder = "Motive/tactic 2"),
      textInput(inputId = paste0(typ, "_", num, "_mottac3"), 
                label = NULL,
                placeholder = "Motive/tactic 3"),
      ###
      ### ALLOW USER TO CHECK A BOX FOR 'AVG DMG' (STATIC #) OR 'DICE DMG' (DROPDOWN SEL)
      ### NOTE: +2 OR +1d4 SHOULD BE APPLICABLE FOR -EITHER-
      ###
      ### LET USER CHECK A BOX TO MODIFY DICE # / DICE SIDE # / ATK + ???
      ###
      ### WANT VALUES TO BE MIDPOINT OF RKTF RECOMMENDATION
      ###
      tags$div(title = recommend_difficulty(tier, typ),
               numericInput(inputId = paste0(typ, "_", num, "_diff"),
                            label = "Difficulty",
                            value = 10) ),
      style="display:inline-block"
    )
    # renderText({
    #   paste0("<p><b>")
    # })
  ###})
}
