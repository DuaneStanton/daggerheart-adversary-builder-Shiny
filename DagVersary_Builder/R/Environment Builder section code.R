# code for the Environment Builder tab =========================================
# environment-specific reference stats
env_ref_df <- 
  data.frame(
    tier = 1:4,
    dmg_rng = c("1d6+1 to 1d8+3", "2d6+3 to 2d10+2", "3d8+3 to 3d10+1", "4d8+3 to 4d10+10"),
    difficulty = c(11, 14, 17, 20)
  )

env_types <- 
  c("Event" = "Defined more by special activity/occurrence than the physical space", 
    "Exploration" = "Wondrous location with mysteries and marvels to discover", 
    "Traversal" = "Dangerous location where moving around the space itself is a challenge",
    "Social" = "Location primarily presenting interpersonal challenges")

# function for environmental features
# note: taken directly from adversary-orientedf 'featurize'; nm_val / ft_sel / dsc_val
# not currently used, but left in in case of future use
featurize_env <- function(type, num, detail_nbr, nm_val = "", ft_sel = NULL, dsc_val = "") {
  dtl_ftnm <- paste0("featname_", detail_nbr)
  dtl_fttyp <- paste0("feattype_", detail_nbr)
  dtl_ftdsc <- paste0("feattext_", detail_nbr)
  dtl_ftqst <- paste0("featquestion_", detail_nbr)
  
  fluidRow(
    column(width = 12,
           div(class = "bottom-aligned",
               div(textify(type, num, dtl_ftnm, "Feature name", "180px", nm_val)),
               div(selectInput(namify(type, num, dtl_fttyp), label = "Feature type", choices = c("Passive", "Action", "Reaction"), selected = ft_sel, width = "110px")),
               div(textAreafy(type, num, dtl_ftdsc, "Feature description", "420px", dsc_val)),
               div(textAreafy(type, num, dtl_ftqst, "Feature question(s) to flesh out details", "420px")) ))
  )
}

numerify_env <- function(df_, tier, type, nbr, detail, lbltxt, valcol, divwd = "80px", inpwd = NULL) {
  df__ <- df_[df_$tier == tier,]
  div(title = "Baseline value presented; lower/raise by 3 for baseline of next lower/higher tier", 
      style = paste0("width: ", divwd),
      numericInput(inputId = namify(type, nbr, detail),
                   label = lbltxt,
                   value = df__[[valcol]],
                   width = inpwd))
}

# build environment type count -------------------------------------------------
build_env_count <- function(typ) {
  tags$div(class = "env-count-sel", title = env_types[[typ]],
           numericInput(inputId = paste0(typ, "_count"), 
                        label = paste0("# ", gsub("_", " ", typ), "s"),
                        value = 0, min = 0, step = 1),
           style="display:inline-block")
}

# template to fill in environment details
build_env_spec_ui <- function(typ, num, tr) {
  div(
    h3(renderText({paste0("Tier ", tr, " ", typ, " (#", num, ")")})),
    textify(typ, num, "name", paste0(typ, "_", num, " (name)"), "250px"),
    textify(typ, num, "desc", "A brief description of the environment", "400px"),
    textAreafy(typ, num, "impulses", "Impulse(s)", "400px"),
    div(numerify_env(env_ref_df, tr, typ, num, "diff", "Difficulty", "difficulty")),
    div(textAreafy(typ, num, "ptntl_adv", "Potential Adversaries", "400px")),
    h4("Features"),
    featurize_env(typ, num, 1),
    featurize_env(typ, num, 2),
    featurize_env(typ, num, 3),
    featurize_env(typ, num, 4),
    featurize_env(typ, num, 5)
  )
}

example_feature_names <- c("Thing The First", "Second Thing", "Three's The Charm")
example_feature_descriptions <- c("Nothing to find in here", 
                                  "Spend a Fear to initiate a Progress Countdown (4) and Consequence Countdown (5) and stuff",
                                  "Single countdown (loop 1d4) to spice things up")

# function to process countdown details from environment features for Daggerforge/Obsidian use
count_down_countdowns <- function(feat_names, feat_descs) {
  id_countdown <- grepl("countdown \\(", tolower(feat_descs))
  if (any(id_countdown)) {
    ### - count countdowns per feature
    ### - ID names
    ### - ID progress/consequence
    ### - if multiple countdowns, account for prog/conseq and append _{#} by position within feature text to feature name
    ###   WHILE accounting for prog/conseq special case (e.g. just one of each -> no _#; multiple, then {featname} Progress_#)
  } else {NULL}
}


###
### RESUME HERE: FUNCTION TO PROCESS FEATURE COUNTDOWN TEXT
### ANOTHER TO BOLD DAMAGE TEXT (#s) FOR MARKDOWN OUTPUT
### -ALSO ADD COUNTDOWN FUNCTIONALITY FOR ADVERSARIES- <MARKDOWN ONLY>; DaggerForge ALREADY HANDLES THIS FROM FEATURE TEXT CONTEXT e.g. "Featname - Passive: Countdown (1d4)" WILL PROCESS FOR 1d4 ROLL TO GENERATE THE COUNTDOWN

# COUNTDOWN TEXT FUNCTION: ID FEATURE NAME, NUMBER OF FEATURES IN THE DESCRIPTION TEXT,
# AND IF ANY OF THE COUNTDOWNS HAVE NAMES
# IF MULTIPLE COUNTDOWNS AND -NOT- NAMED, USE {feature name}_{countdown #} AS NAME
# SPECIAL CASE: PROGRESS AND COUNTDOWN IN THE FEATURE DESC AND THOSE ARE IT
# ...THEN NO '_#' SUFFIX


# NOTE: IN DAGGERFORGE, 'countdown' JSON FIELD IS ONLY INCLUDED IF THERE IS A SPECIFIED COUNTDOWN FOR THE ENVIRONMENT
# REFERENCE list_features_obsdn.dgrfrg / .obsdn (TOP OF RESPECTIVE CODE FILES)

###
### RESUME HERE; REFERENCE app.R LINE 342 AND RELATED FOR REFERENCE
### NOTES:
### - FOR NON-CHASE COUNTDOWN, USE Countdown (#) WORDING; CREATE CHECKBOX IN MARKDOWN FOR THIS
### ...IN TEXT PROCESSING ACCOUNT FOR LOWERCASE POTENTIAL
### ...SPECIAL CASE FOR Countdown (Loop #) ; NOTE THE LOOP NATURE AND OUTPUT MAX #
### ...OTHER SPECIAL CASE Countdown (Loop 1d{#}) ; NOTE THE DICE NATURE AND OUTPUT MAX #
###   AND OUTPUT CONTDOWNS FOR EACH WITH APPROPRIATE LABELS