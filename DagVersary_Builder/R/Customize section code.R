# code for the Customize tab ===================================================
# customize tooltip for recommended difficulty range ---------------------------
recommend_range <- function(x, tier, typ, colnm) {
  paste("Recommended range:", x[[colnm]][x$tier == tier & x$adv_type == typ])
}

# build uiOutput as-needed for adversaries -------------------------------------
textify <- function(typ, num, detail, placehold, wd = NULL, val = "") {
  textInput(inputId = namify(typ, num, detail), label = NULL, value = val, placeholder = placehold, width = wd)
}

numerify <- function(df_, tier, type, nbr, detail, lbltxt, valcol, divwd = "80px", inpwd = NULL) {
  df__ <- df_[df_$tier == tier & df_$adv_type == type,]
  div(title = recommend_range(df_, tier, type, paste0(detail, "_rng")), 
      style = paste0("width: ", divwd),
      numericInput(inputId = namify(type, nbr, detail),
                   label = lbltxt,
                   value = df__[[valcol]],
                   width = inpwd))
}

dmg_dice_pooler <- function(df_, tier, type, nbr, detail) {
  df__ <- df_[df_$tier == tier & df_$adv_type == type,]
  div(title = "Select higher constant for more consistency, or high dice-side for more swinginess",
      style = "width: 120px;",
      selectInput(inputId = namify(type, nbr, detail),
                  label = "Damage dice",
                  choices = c(unlist(df__$dice_pool_lst), "Use Avg")))
}

feat_setup <- function(featname, featdtl = "", feattype, feattext) {
  if (featdtl != ""){featname <- paste0(featname, "(", featdtl, ")")}
  feattxt <- gsub("{featdtl}", featdtl, feattxt)
  list("nm" = featname, "typ" = feattype, "txt" = feattext)
}

featurize <- function(type, num, detail_nbr, nm_val = "", ft_sel = NULL, dsc_val = "") {
  dtl_ftnm <- paste0("featname_", detail_nbr)
  dtl_fttyp <- paste0("feattype_", detail_nbr)
  dtl_ftdsc <- paste0("feattext_", detail_nbr)
  fluidRow(
    column(width = 12,
           div(class = "bottom-aligned",
               div(textify(type, num, dtl_ftnm, "Feature name", "180px", nm_val)),
               div(selectInput(namify(type, num, dtl_fttyp), label = "Feature type", choices = c("Passive", "Action", "Reaction"), selected = ft_sel, width = "110px")),
               div(textify(type, num, dtl_ftdsc, "Feature description", "420px", dsc_val)) ))
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
                 div(numerify(stat_ref_df, tr, typ, num, "diff", "Difficulty", "diff_md")),
                 div(numerify(stat_ref_df, tr, typ, num, "thresh_maj", "Major Threshold", "thresh_maj_md", "150px")), 
                 div(numerify(stat_ref_df, tr, typ, num, "thresh_sev", "Severe Threshold", "thresh_sev_md", "170px")),
                 if (typ == "Horde"){div(title = "# creatures per HP for flavor", numericInput(namify(typ, num, "perhp"), label = "#/HP", value = 5, step = 1, width = "70px"))},
                 div(numerify(stat_ref_df, tr, typ, num, "hp", "HP", "hp_md", "70px", "70px")),
                 div(numerify(stat_ref_df, tr, typ, num, "stress", "Stress", "stress_md", "60px", "60px")),
                 if (typ == "Minion"){div(numerify(stat_ref_df, tr, typ, num, "minion_pasv", "Minion Passive", "minion_pasv_md", "120px", "120px"))})
      )),
    fluidRow(
      column(width = 12, offset = 0,
             div(class = "bottom-aligned",
                 div(numerify(stat_ref_df, tr, typ, num, "atk", "ATK", "atk_md", "70px", "70px")),
                 div(textify(typ, num, "wpn", "Weapon", "140px")),
                 div(selectInput(namify(typ, num, "rng"), label = "Weapon range", choices = distances, width = "110px")),
                 dmg_dice_pooler(stat_ref_df, tr, typ, num, "dmg_dice"),
                 numerify(stat_ref_df, tr, typ, num, "dmg_avg", "Avg damage", "dmg_avg_md", "100px", "120px"),
                 div(selectInput(namify(typ, num, "dmg_typ"), label = "Damage type", choices = c("phy", "mag", "phy & mag", "direct"), width = "120px")) )
      )),
    textify(typ, num, "exp", "Experience(s) e.g. Conjurer of cheap tricks +2", "350px"),
    ### DEV NOTE: REMOVE THIS ON RUN/OBSIDIAN IF ONLY PLACEHOLDER TEXT
    ### DEVNOTE: In the "Run" and "Obsidian" tabs, bold numbers / dice / "spend a Fear" (or spend {#} fear)
    featurize(typ, num, 1),
    featurize(typ, num, 2),
    featurize(typ, num, 3),
    featurize(typ, num, 4),
    featurize(typ, num, 5)
  )
}

# UI distinct to Colossal adversaries ------------------------------------------

###
### TODO: TIE SEGMENTS TO TORSO NAME - ALLOW SELECTINPUT WHERE NAME FROM FRAMEWORK IS, INCLUDE PLACEHOLDER IF EMPTY
###

# note: multi_frame is an under-consideration element to allow designing/running multiple colossi in a single instance - may be removed
build_colossus_spec_ui <- function(typ, num, tr, multi_frame = FALSE) {
  div(
    h3(renderText({paste0("Tier ", tr, " ", gsub("_", " ", typ), " (#", num, ")")})),
    div(title = "Framework: colossus name (e.g. 'Mountainbreaker'); Segment: part name (e.g. 'Mountainbreaker Left Arm')",
        textify(typ, num, "name", paste0(sub("segment", "sgmt", typ), "_", num, " (name)"), "250px")),
    if (grepl("framework", typ)) {
      textify(typ, num, "desc", "A brief description of the adversary", "400px") },
    if (grepl("segment", typ) && multi_frame) {
      selectInput(namify(typ, num, "parent_frame"), label = "Associated framework", choices = "give Colossus frameworks names", selected = "give Colossus frameworks names", width = "300px")
    },
    div(title = if (!grepl("framework", typ)) {"Reference other segment name (e.g. 'Arm' if this segment is 'Body')"},
      textify(typ, num, "mottac_adj1", 
      ifelse(grepl("framework", typ), "Motive/tactic 1", "Adjacent segment type 1"), "200px")
      ),
    div(title = if (!grepl("framework", typ)) {"Reference other segment name (e.g. 'Leg' if this segment is 'Body')"},
        textify(typ, num, "mottac_adj2", 
                ifelse(grepl("framework", typ), "Motive/tactic 2", "Adjacent segment type 2"), "200px")
    ),
    div(title = if (!grepl("framework", typ)) {"Reference other segment name (e.g. 'Tail' if this segment is 'Body')"},
        textify(typ, num, "mottac_adj3", 
                ifelse(grepl("framework", typ), "Motive/tactic 3", "Adjacent segment type 3"), "200px")
    ),
    div(title = if (!grepl("framework", typ)) {"Reference other segment name (e.g. 'Body' if this segment is 'Arm')"},
        textify(typ, num, "mottac_adj4", 
                ifelse(grepl("framework", typ), "Motive/tactic 4", "Adjacent segment type 4"), "200px")
    ),
    if (grepl("framework", typ)) { # framework
      fluidRow(
        column(width = 12, offset = 0,
               div(class = "bottom-aligned",
                   div(textify(typ, num, "sz", "Size (height, width)")),
                   div(numerify(stat_ref_df, tr, typ, num, "thresh_maj", "Major Threshold", "thresh_maj_md", "150px")), 
                   div(numerify(stat_ref_df, tr, typ, num, "thresh_sev", "Severe Threshold", "thresh_sev_md", "170px")),
                   div(numerify(stat_ref_df, tr, typ, num, "stress", "Stress", "stress_md", "60px", "60px")))
        ))
    } else { # segment
      fluidRow(
        column(width = 12, offset = 0,
               div(class = "bottom-aligned",
                   div(numerify(stat_ref_df, tr, typ, num, "diff", "Difficulty", "diff_md")),
                   div(title = "HP per each segment of this type", numerify(stat_ref_df, tr, typ, num, "hp", "HP", "hp_md", "70px", "70px")) )
        ))
    },
    if (grepl("framework", typ)) {textify(typ, num, "exp", "Experience(s)", "350px")
    } else {
      fluidRow(
        column(width = 12, offset = 0,
               div(class = "bottom-aligned",
                   div(numerify(stat_ref_df, tr, typ, num, "atk", "ATK", "atk_md", "70px", "70px")),
                   div(textify(typ, num, "wpn", "Weapon", "140px")),
                   div(selectInput(namify(typ, num, "rng"), label = "Weapon range", choices = distances, width = "110px")),
                   dmg_dice_pooler(stat_ref_df, tr, typ, num, "dmg_dice"),
                   numerify(stat_ref_df, tr, typ, num, "dmg_avg", "Avg damage", "dmg_avg_md", "100px", "120px"),
                   div(selectInput(namify(typ, num, "dmg_typ"), label = "Damage type", choices = c("phy", "mag", "phy & mag", "direct"), width = "120px")) ))
      )
      },
    ###
    ### TODO: frameworks -always- have the Colossal Power reaction (make feature 1); everything else is modifiable
    ###       LIKELY NEED TO ENFORCE THIS IN THE 'Run section code'
    featurize(typ, num, 1, 
              nm_val = ifelse(grepl("framework", typ), "Colossal Power", ""),
              ft_sel = if (grepl("framework", typ)) {"Reaction"},
              dsc_val = ifelse(grepl("framework", typ), "When &lt;the colossus&gt; fails an attack, you gain a Fear.", "")),
    featurize(typ, num, 2),
    featurize(typ, num, 3),
    featurize(typ, num, 4),
    featurize(typ, num, 5)
    
  )
  }