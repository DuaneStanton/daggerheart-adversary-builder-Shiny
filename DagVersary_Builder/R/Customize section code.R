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
    textify(typ, num, "exp", "Experience(s) e.g. 'Conjurer of cheap tricks +2'", "350px"),
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

# function to build motives/tactics (Colossus framework) / adjacent segment(s) (Colossus segment) input
build_col_mt_as <- function(typ, num, inpt_num, lbl = NULL) {
  div(
    if (grepl("framework", typ)) {
      textify(typ, num, paste0("mottac_adj", inpt_num), paste("Motive/tactic", inpt_num), "200px")
    } else {
      selectInput(namify(typ, num, paste0("mottac_adj", inpt_num)),
                  label = lbl,
                  choices = "None", # will update as user enters segment names
                  width = "200px")
    }
  )
}

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
    fluidRow(
      div(title = 
          ifelse(grepl("framework", typ), "Motives/Tactics", "Adjacent segments"))
      ),
    div(title = if (!grepl("framework", typ)) {"Repeats of the same segment names here will be simplified to unique values in the next set of tabs"},
        build_col_mt_as(typ, num, 1, ifelse(grepl("framework", typ), "Motives/Tactics", "Adjacent segment(s)"))),
    build_col_mt_as(typ, num, 2),
    build_col_mt_as(typ, num, 3),
    build_col_mt_as(typ, num, 4),
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
    if (grepl("framework", typ)) {textify(typ, num, "exp", "Experience(s) e.g. 'Burninator +2'", "350px")
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

# function to streamline the update...Input process
update_inputs <- function(adv_nm, adv_num, inpt_num, adv_df, val_idx = NULL) {
  updateTextInput(inputId = namify(adv_nm, adv_num, paste0("featname_", inpt_num)),
                  value = ifelse(is.null(val_idx), "", adv_df$feat_name[val_idx]))
  updateSelectInput(inputId = namify(adv_nm, adv_num, paste0("feattype_", inpt_num)),
                    selected = if (is.null(val_idx)) {"Passive"} else {adv_df$feat_type[val_idx]})
  updateTextInput(inputId = namify(adv_nm, adv_num, paste0("feattext_", inpt_num)),
                  value = ifelse(is.null(val_idx), "", adv_df$feat_text[val_idx]))
}

# Minion and Horde features always fill with a standard set whenever input$feat_fill_ct > 0
# function to fill Minion features
fill_minion_feat <- function(id_ct, minion_feat_df) {
  update_inputs("Minion", id_ct, 1, minion_feat_df, grep("Minion", minion_feat_df$feat_name))
  update_inputs("Minion", id_ct, 2, minion_feat_df, grep("Join or Die", minion_feat_df$feat_name))
  update_inputs("Minion", id_ct, 3, minion_feat_df, grep("Group Attack", minion_feat_df$feat_name))
}

fill_horde_feat <- function(id_ct, horde_feat_df) {
  update_inputs("Horde", id_ct, 1, horde_feat_df, grep("Horde \\(", horde_feat_df$feat_name))
  update_inputs("Horde", id_ct, 2, horde_feat_df, grep("Contains Multitudes", horde_feat_df$feat_name))
}

fill_col_fw_feat <- function(id_ct, col_fw_feat_df) {
  update_inputs("Colossus_framework", id_ct, 1, col_fw_feat_df, grep("Colossal Power", col_fw_feat_df$feat_name))
}

# generating a feature set per adversary, with no repeats of features per adversary
feat_sampler_idx <- function(n_sametyp_adv, feat_df, n_feat) {
  feat_n <- min(nrow(feat_df), n_feat)
  idx_psv <- which(feat_df$feat_type == "Passive")
  idx_act <- which(feat_df$feat_type == "Action")
  idx_rct <- which(feat_df$feat_type == "Reaction")
  idx_set <- c(idx_psv, idx_act, idx_rct)
  feat_mat <- matrix(nrow = n_sametyp_adv, ncol = feat_n)
  for (i in 1:n_sametyp_adv) {
    feat_mat[i,] <- sample(idx_set, size = feat_n, replace = FALSE)
  }
  feat_mat
}

