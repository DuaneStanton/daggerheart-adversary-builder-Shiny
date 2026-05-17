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
    across(.cols = ends_with("_rng"), .fns = calc_midpt, .names = "{.col}_md"),
  )
md_cols_idx <- grep("_rng_md", colnames(adv_ref_df))
colnames(adv_ref_df)[md_cols_idx] <- sub("_rng", "", colnames(adv_ref_df)[md_cols_idx])

adv_ref_df$dice_pool_lst <- 
  lapply(1:nrow(adv_ref_df), \(i) {if (!is.na(adv_ref_df$dice_pool_optns[i])){
    strsplit(adv_ref_df$dice_pool_optns[i], split = ",")[[1]] }})

# customize tooltip for recommended difficulty range ---------------------------
recommend_range <- function(x, tier, typ, colnm) {
  paste("Recommended range:", x[[colnm]][x$tier == tier & x$adv_type == typ])
}

# build uiOutput as-needed for adversaries -------------------------------------
namify <- function(type, num, detail){paste0(type, "_", num, "_", detail)}

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

###
### HERE OR IN app.R, NEED TO BUILD IN TO FEATURES FOR KEY TYPES:
### - {for Minion} Minion ({minion pasv val here}) - Passive: The {Minion} is defeated when they take any damage. For every {minion pasv val here} damage a PC deals to the {Minion}, defeat an additional Minion within range the attack would succeed against.
### - {for Minion} Group Attack - Action: Spend a Fear to choose a target and spotlight all {Minions} within Close range of them. Those Minions move into Melee range of the target and make one shared attack roll. On a success, they deal {dmg} physical damage each. Combine this damage.
### - {for Horde} Horde ({half dmg, OR lesser of down 1 dice size / half dice sides AND half round-down +#}) - Passive: When the {Horde} has marked half or more of their HP, their standard attack deals {SAME AS PRIOR {...}} {physical/magic} damage instead.
### - {for Solo} Relentless (2-4) - Passive: Can be spotlighted up to {two/three/four} times per GM turn. Spend Fear as usual to spotlight them.

feat_setup <- function(featname, featdtl = "", feattype, feattext) {
  if (featdtl != ""){featname <- paste0(featname, "(", featdtl, ")")}
  feattxt <- gsub("{featdtl}", featdtl, feattxt)
  list("nm" = featname, "typ" = feattype, "txt" = feattext)
}

###
### WORK IN PROGRESS: POPULATING featurize() WITH 1) REQUISITE and 2) RANDOMLY SELECTED TYPE-APPROPRIATE FEATURES
###

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
                  div(numerify(adv_ref_df, tr, typ, num, "diff", "Difficulty", "diff_md")),
                  div(numerify(adv_ref_df, tr, typ, num, "thresh_maj", "Major Threshold", "thresh_maj_md", "150px")), 
                  div(numerify(adv_ref_df, tr, typ, num, "thresh_sev", "Severe Threshold", "thresh_sev_md", "170px")),
                  if (typ == "Horde"){div(title = "# creatures per HP for flavor", numericInput(namify(typ, num, "perhp"), label = "#/HP", value = 5, step = 1, width = "70px"))},
                  div(numerify(adv_ref_df, tr, typ, num, "hp", "HP", "hp_md", "70px", "70px")),
                  div(numerify(adv_ref_df, tr, typ, num, "stress", "Stress", "stress_md", "60px", "60px")),
                  if (typ == "Minion"){div(numerify(adv_ref_df, tr, typ, num, "minion_pasv", "Minion Passive", "minion_pasv_md", "120px", "120px"))})
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
  paste0("<b><span style=font-size: 2em;>", 
         ifelse(nm() == "", "Adversary", nm()), 
         "</spanp></b><span> (T", tr, " ", typ, " \u0023", num, ")</span>",
         if (typ == "Horde"){paste0(" <span>(", inpt[[namify(typ, num, "perhp")]], "/HP)</span>")})
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
      dmg_end <- sub(".+d\\d+\\+(.+)", "\\1", inpt[[namify(typ, num, "dmg_dice")]]) |> as.numeric()
      dmg_str <- sub("(.+d\\d+\\+).+", "\\1", inpt[[namify(typ, num, "dmg_dice")]])
      paste0(dmg_str, (dmg_end + 2))
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
    } else if (inpt[[namify(typ, num, "hp_run")]] == 0) {"<p style ='color: #ff175b; text-align: center'><BIG><b>DEFEATED!</b></BIG></p>"}
  })
  
  renderUI(HTML(msg()))
}

process_feat_txt <- function(txt) {
  txt_ <- txt
  
  find_bolders <- stri_locate_all(str = txt, regex = ".ark a .tress|[0-9]+d[0-9]+|.pend a .ear|.pend [0-9]+ .ear")[[1]]
  find_bolders <- cbind(find_bolders, 0) # creates 3 column with '0'
  find_italics <- stri_locate_all(str = txt, regex = "Hidden|hidden|Restrained|restrained|Vulnerable|vulnerable")[[1]]
  find_italics <- cbind(find_italics, 1)
  
  process_mat <- rbind(find_bolders, find_italics)
  process_mat <- process_mat[order(process_mat[, "start"]),]
  
  if (!all(is.na(process_mat[, "start"]))) {
    # account for adding '<b>' and '</b>' OR '<i>' and '</i>' to the string as needed
    for (i in 1:nrow(process_mat)) {
      process_mat[i, "start"] <-  process_mat[i, "start"] + 7 * (i - 1)
      process_mat[i, "end"] <-  process_mat[i, "end"] + 7 * (i - 1)
    }
    
    for (z in 1:nrow(process_mat)) {
      txt_ <- 
        paste0(
          substr(txt_, start = 1, stop = process_mat[z, "start"] - 1),
          ifelse(process_mat[z, 3] == 0, "<b>", "<i>"),
          substr(txt_, start = process_mat[z, "start"], stop = process_mat[z, "end"]), 
          ifelse(process_mat[z, 3] == 0, "</b>", "</i>"),
          substr(txt_, start = process_mat[z, "end"] + 1, stop = nchar(txt_))
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
  
  if (has_feattxt(inpt, typ, num, ftrnum)) {paste0(featnm(), feattyp(), feattxt())} else {""}
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
      paste(x$txt, collapse = "<br>---<br>")
    }
  })
  if (!is.null(feat_list())) {
    renderUI(HTML(feat_list()))
  }
}

build_adv_run_ui <- function(inpt, typ, num, tr, dmg_add) {
  div(
    fluidRow(renderUI({HTML(list_adv_name(inpt, typ, num, tr))})),
    fluidRow(list_adv_desc(inpt, typ, num)),
    list_motives_tactics(inpt, typ, num),
    fluidRow(list_stats_1(inpt, typ, num)),
    fluidRow(msg_hp_stress(inpt, typ, num)),
    fluidRow(
      column(width = 6,
             div(selectInput(namify(typ, num, "hp_run"), label = "HP", choices = c(0:inpt[[namify(typ, num, "hp")]]), selected = inpt[[namify(typ, num, "hp")]], width = "70px"))),
      column(width = 6,
             div(selectInput(namify(typ, num, "stress_run"), label = "Stress", choices = c(0:inpt[[namify(typ, num, "stress")]]), selected = inpt[[namify(typ, num, "stress")]], width = "70px"))),
    ),
    fluidRow(list_features(inpt, typ, num)),
    fluidRow(div(class="inline", title = "Hidden: rolls against have disadvantage; Restrained: can't move, can take actions; Vulnerable: all rolls against have advantage", 
                 checkboxGroupInput(namify(typ, num, "conds"), label = "Conditions: ", choices = c("Hidden", "Restrained", "Vulnerable"), inline = TRUE))),
    fluidRow(textify(typ, num, "custom_cond", placehold = "Type custom conditions here", ))
    )
}
