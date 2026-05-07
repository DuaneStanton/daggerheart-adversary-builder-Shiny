# Functions and Code Setup for the DagVersary App

# build adversary type count ---------------------------------------------------
build_adv_count <- function(typ, chcs, val = 0) {
  tags$div(
  selectInput(inputId = paste0(typ, "_count"), 
                       label = paste0("# ", typ, "s"), 
                       choices = chcs, 
                       selected = val),
  style="display:inline-block")
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
      style="display:inline-block"####
    )
    # renderText({
    #   paste0("<p><b>")
    # })
  ###})
}
