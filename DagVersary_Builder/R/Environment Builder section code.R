# code for the Environment Builder tab =========================================
# environment-specific reference stats
env_ref_df <- 
  data.frame(
    tier = 1:4,
    dmg_lo = c("1d6+1", "2d6+3", "3d8+3", "4d8+3"),
    dmg_hi = c("1d8+3", "2d10+2", "3d10+1", "4d10+10"),
    difficulty = c(11, 14, 17, 20)
  )

env_types <- 
  c("Defined more by the activity occurring here than the terrain" = "Event", 
    "Wondrous location with mysteries and marvels to discover" = "Exploration", 
    "Dangerous location where moving around the space itself is a challenge" = "Traversal",
    "Location primarily presenting interpersonal challenges" = "Social")

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
               div(textify(type, num, dtl_ftdsc, "Feature description", "420px", dsc_val)),
               div(textify(type, num, dtl_ftqst, "Feature question(s)", "420px")) ))
  )
}

# template to fill in environment details
build_env_ui <- function(typ, num, tr) {
  div(
    h3(renderText({paste0("Tier ", tr, " ", typ, " (#", num, ")")})),
    textify(typ, num, "name", paste0(typ, "_", num, " (name)"), "250px"),
    textify(typ, num, "desc", "A brief description of the environment", "400px"),
    textify(typ, num, "impulse1", "Impulse 1", "200px"),
    textify(typ, num, "impulse2", "Impulse 2", "200px"),
    textify(typ, num, "impulse3", "Impulse 3", "200px"),
    div(numerify(stat_ref_df, tr, typ, num, "diff", "Difficulty", "diff_md")),
    div(textify(typ, num, "ptntl_adv", "Potential Adversaries", "400px")),
    h4("Features"),
    featurize_env(typ, num, 1),
    featurize_env(typ, num, 2),
    featurize_env(typ, num, 3),
    featurize_env(typ, num, 4),
    featurize_env(typ, num, 5)
  )
}

###
### RESUME HERE; REFERENCE app.R LINE 342 AND RELATED FOR REFERENCE
###