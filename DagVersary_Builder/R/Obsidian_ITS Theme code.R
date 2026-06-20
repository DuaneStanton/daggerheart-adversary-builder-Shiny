# code to take user-specified Customize tab details into Markdown structure for copying to Obsidian with ITS Theme

# function for processing features
# version with -optional- text styling and features separated at end -----------

list_features_obsdn.mkdn <- function(inpt, typ, num, auto_feat_ct) {
  # list Passives, then Actions, then Reactions...if feature text present
  
  # auto_feat_ct just tracks input$feat_fill_ct ; this helps ensure the feature list updates if only the feature text changes
  feat_ct <- auto_feat_ct
  
  feattypes_set <- reactive({
    c(if (has_feattxt(inpt, typ, num, 1)) {classify_feattyp(inpt, typ, num, 1)} else {0},
      if (has_feattxt(inpt, typ, num, 2)) {classify_feattyp(inpt, typ, num, 2)} else {0},
      if (has_feattxt(inpt, typ, num, 3)) {classify_feattyp(inpt, typ, num, 3)} else {0},
      if (has_feattxt(inpt, typ, num, 4)) {classify_feattyp(inpt, typ, num, 4)} else {0},
      if (has_feattxt(inpt, typ, num, 5)) {classify_feattyp(inpt, typ, num, 5)} else {0})
  })
  
  feat_list0 <- reactive({
    if (!all(feattypes_set() == 0)) {
      x <- 
        do.call(
          what = rbind,
          args = lapply(1:length(feattypes_set()), \(z){
            data.frame(idx = z, typval = feattypes_set()[z],
                       txt = 
                         process_feature(inpt, typ, num, z)  |> 
                         process_feat_txt_dtl(inpt = inpt, typ = typ, num = num)) })
        )
      x <- x[x$typval > 0,]
      x <- x[order(x$typval),]
      paste(x$txt, collapse = "<br>---<br>")
    }
  })
  
  feat_list <- reactive({
    if (!is.null(feat_list0())) {
      x1 <- strsplit(feat_list0(), split = "<br>---<br>")[[1]]
      x2 <- 
        lapply(1:length(x1), \(i) {
          # need to trim the <b><i> and </i></b> wrapper around the {Feature Name} - {Feature Type} text
          x <- x1[i]
          x <- gsub("<b>|</b>", "**", x)
          x <- gsub("<i>|</i>", "*", x)
          x <- gsub("\\&lt;", "<", x)
          x <- gsub("\\&gt;", ">", x)
          
          x
        })
      x2
    } else {
      list()
    }
  })
  feat_list()
}

# prep function for non-Colossus adversaries -----------------------------------
# essentially the same as the Daggerforge-prep function
markdown_prep_adversary <- function(inpt, typ, num, tr, auto_feat_ct) {
  adv_name <- 
    paste0(inpt[[namify(typ, num, "name")]], 
           if (typ == "Horde") {paste0(" (", inpt[[namify(typ, num, "perhp")]], "/HP)")})
  
  mot1 <- if (inpt[[namify(typ, num, "mottac1")]] != "") {inpt[[namify(typ, num, "mottac1")]]}
  mot2 <- if (inpt[[namify(typ, num, "mottac2")]] != "") {inpt[[namify(typ, num, "mottac2")]]}
  mot3 <- if (inpt[[namify(typ, num, "mottac3")]] != "") {inpt[[namify(typ, num, "mottac3")]]}
  
  mottac <- 
    if (any(!is.null(mot1), !is.null(mot2), !is.null(mot3))) {
      paste(mot1, mot2, mot3, sep = ", ") |> sub(pattern = ", $", replacement = "")} else {""}
  
  dice_dmg <- 
    if (inpt$fight_type == "Tougher (add +1d4 to adversary damage rolls)") {
      paste(inpt[[namify(typ, num, "dmg_dice")]], "+1d4")
    } else if (inpt$fight_type == "Tougher (add +2 to adversary damage rolls)" &
               inpt[[namify(typ, num, "dmg_dice")]] != "Use Avg") {
      dmg_end <- sub(".+d\\d+\\+(.+)", "\\1", inpt[[namify(typ, num, "dmg_dice")]]) |> as.numeric()
      dmg_str <- sub("(.+d\\d+\\+).+", "\\1", inpt[[namify(typ, num, "dmg_dice")]])
      paste0(dmg_str, (dmg_end + 2))
    } else {inpt[[namify(typ, num, "dmg_dice")]]}
  
  avg_dmg <- 
    if (inpt$fight_type == "Tougher (add +1d4 to adversary damage rolls)") {
      paste(inpt[[namify(typ, num, "dmg_avg")]], "+1d4")
    } else if (inpt$fight_type == "Tougher (add +2 to adversary damage rolls)") {
      inpt[[namify(typ, num, "dmg_avg")]] + 2
    } else {inpt[[namify(typ, num, "dmg_avg")]]}
  
  list( 
    id = "",
    name = adv_name,
    tier = tr,
    type = typ,
    desc = inpt[[namify(typ, num, "desc")]],
    motives = mottac,
    difficulty = inpt[[namify(typ, num, "diff")]],
    thresholdMajor = inpt[[namify(typ, num, "thresh_maj")]],
    thresholdSevere = inpt[[namify(typ, num, "thresh_sev")]],
    hp = inpt[[namify(typ, num, "hp")]],
    stress = inpt[[namify(typ, num, "stress")]],
    atk = ifelse(inpt[[namify(typ, num, "atk")]] > -1,
                 paste0("+", inpt[[namify(typ, num, "atk")]]),
                 inpt[[namify(typ, num, "atk")]]),
    weaponName = ifelse(inpt[[namify(typ, num, "wpn")]] == "",
                        "{weapon}", inpt[[namify(typ, num, "wpn")]]),
    weaponRange = inpt[[namify(typ, num, "rng")]],
    weaponDamage =
      paste(if (inpt[[namify(typ, num, "dmg_dice")]] == "Use Avg") {avg_dmg
      } else {dice_dmg}, inpt[[namify(typ, num, "dmg_typ")]]),
    xp = inpt[[namify(typ, num, "exp")]],
    source = "custom",
    features = list_features_obsdn.mkdn(inpt, typ, num, auto_feat_ct)
  )
}

# prep function for Colossus adversaries - note the category names are same as non-Colossus adversaries to fit Daggerforge structure ----
# essentially the same as the Daggerforge-prep function
markdown_prep_adversary_col <- function(inpt, typ, num, tr, fwk_id, auto_feat_ct) {
  fwk <- grepl("framework", typ)
  
  adv_name <- 
    if (fwk) {paste0(inpt[[namify(typ, num, "name")]], " (col fw)")
    } else {paste0(inpt[[namify(typ, num, "name")]], " (col sg)")}
  
  mt1 <- if (inpt[[namify(typ, num, "mottac_adj1")]] != "") {inpt[[namify(typ, num, "mottac_adj1")]]}
  mt2 <- if (inpt[[namify(typ, num, "mottac_adj1")]] != "") {inpt[[namify(typ, num, "mottac_adj2")]]}
  mt3 <- if (inpt[[namify(typ, num, "mottac_adj1")]] != "") {inpt[[namify(typ, num, "mottac_adj3")]]}
  mt4 <- if (inpt[[namify(typ, num, "mottac_adj1")]] != "") {inpt[[namify(typ, num, "mottac_adj4")]]}
  
  mt_set <- c(mt1, mt2, mt3, mt4) |> unique()
  if (length(mt_set) > 1L) {mt_set <- mt_set[mt_set != "None"]}
  
  if (fwk) {
    desc. <- paste0("(", inpt[[namify(typ, num, "sz")]], " ", inpt[[namify(typ, num, "desc")]], ")")
    mottac <- sub(", $", "", paste(mt_set, collapse = ", "))
  } else { # segments will list the 'parent' framework name here
    desc. <- paste0("Adjacent segment(s): ", paste(mt_set, collapse = ", "))
    mottac <- inpt[[namify(a.t(fwk_id), a.n(fwk_id), "name")]]
  }
  
  dice_dmg <- 
    if (fwk) {"-"} else {
      if (inpt$fight_type == "Tougher (add +1d4 to adversary damage rolls)") {
        paste(inpt[[namify(typ, num, "dmg_dice")]], "+1d4")
      } else if (inpt$fight_type == "Tougher (add +2 to adversary damage rolls)" &
                 inpt[[namify(typ, num, "dmg_dice")]] != "Use Avg") {
        dmg_end <- sub(".+d\\d+\\+(.+)", "\\1", inpt[[namify(typ, num, "dmg_dice")]]) |> as.numeric()
        dmg_str <- sub("(.+d\\d+\\+).+", "\\1", inpt[[namify(typ, num, "dmg_dice")]])
        paste0(dmg_str, (dmg_end + 2))
      } else {inpt[[namify(typ, num, "dmg_dice")]]}
    }
  
  avg_dmg <- 
    if (fwk) {"-"} else {
      if (inpt$fight_type == "Tougher (add +1d4 to adversary damage rolls)") {
        paste(inpt[[namify(typ, num, "dmg_avg")]], "+1d4")
      } else if (inpt$fight_type == "Tougher (add +2 to adversary damage rolls)") {
        inpt[[namify(typ, num, "dmg_avg")]] + 2
      } else {inpt[[namify(typ, num, "dmg_avg")]]}
    }
  
  list(
    id = "",
    name = adv_name,
    tier = tr,
    type = gsub("_", " ", typ),
    desc = desc.,
    motives = mottac,
    difficulty = if (fwk) {"-"} else {inpt[[namify(typ, num, "diff")]]},
    # applying given framework's damage thresholds to segments for easier tracking
    thresholdMajor = inpt[[namify(a.t(fwk_id), a.n(fwk_id), "thresh_maj")]],
    thresholdSevere = inpt[[namify(a.t(fwk_id), a.n(fwk_id), "thresh_sev")]],
    hp = if (fwk) {"-"} else {inpt[[namify(typ, num, "hp")]]},
    stress = if (fwk) {inpt[[namify(typ, num, "stress")]]} else {"-"},
    atk = if (fwk) {"-"} else {
      ifelse(inpt[[namify(typ, num, "atk")]] > -1,
             paste0("+", inpt[[namify(typ, num, "atk")]]),
             inpt[[namify(typ, num, "atk")]])
    },
    weaponName = if (fwk) {"-"} else {
      ifelse(inpt[[namify(typ, num, "wpn")]] == "",
             "{weapon}", inpt[[namify(typ, num, "wpn")]])
    },
    weaponRange = if (fwk) {"-"} else {inpt[[namify(typ, num, "rng")]]},
    weaponDamage = if (fwk) {"-"} else {
      paste(if (inpt[[namify(typ, num, "dmg_dice")]] == "Use Avg") {avg_dmg
      } else {dice_dmg}, inpt[[namify(typ, num, "dmg_typ")]])
    },
    xp = if (fwk) {inpt[[namify(typ, num, "exp")]]} else {"-"},
    source = "custom",
    features = list_features_obsdn.mkdn(inpt, typ, num, auto_feat_ct)
  )
}

markdownize_features <- function(feat_list) {
  if (length(feat_list > 0L)) {
    lapply(1:length(feat_list), \(i) {paste0(">>- ", feat_list[[i]]) }) |> 
      paste(collapse = "\n")
  } else {">>{no features specified}\n"}
}

# function to generate markdown structure for HP and Stress checkboxes
# note: a full row can have up to 6 checkboxes per HP / Stress column
mkdnize_hp.stress <- function(hp, stress) {
  hp_full_row_need <- hp %/% 6
  hp_part_row_ct <- ifelse(6 * hp_full_row_need == hp, 0, hp - 6 * hp_full_row_need)
  
  stress_full_row_need <- stress %/% 6
  stress_part_row_ct <- ifelse(6 * stress_full_row_need == stress, 0, stress - 6 * stress_full_row_need)
  
  row_need <- max(hp_full_row_need + as.numeric(hp_part_row_ct > 0),
                  stress_full_row_need + as.numeric(stress_part_row_ct > 0))
  
  hp_row_set <- lapply(1:(hp_full_row_need + as.numeric(hp_part_row_ct > 0)), \(i) {
    if (i < hp_full_row_need + 1) {
      paste(rep("<input type='checkbox' unchecked/>", 6), collapse = " ")
    } else {
      paste(rep("<input type='checkbox' unchecked/>", hp_part_row_ct), collapse = " ")
    }
  })
  
  stress_row_set <- lapply(1:(stress_full_row_need + as.numeric(stress_part_row_ct > 0)), \(i) {
    if (i < stress_full_row_need + 1) {
      paste(rep("<input type='checkbox' unchecked/>", 6), collapse = " ")
    } else {
      paste(rep("<input type='checkbox' unchecked/>", stress_part_row_ct), collapse = " ")
    }
  })
  
  hp_stress_row_set <- 
    vapply(1:row_need, \(i) {
      hp_entry <- 
        if (i <= hp_full_row_need + as.numeric(hp_part_row_ct > 0)) { 
          hp_row_set[[i]]
        } else {" "}
      
      stress_entry <- 
        if (i <= stress_full_row_need + as.numeric(stress_part_row_ct > 0)) { 
          stress_row_set[[i]]
        } else {" "}
      
      paste0("| ", hp_entry, " | ", stress_entry, " |")
    }, character(1L))
  
  paste0(">> ", paste0(hp_stress_row_set, "\n")) |> paste(collapse = "")
}


# function to create markdown-ready adversary listing
# note: \u0022 is unicode for ", \u005c is unicode for \, \u0009 is unicode for horizontal tab
markdownize <- function(adv_list) {
  a_l <- 
    lapply(1:length(adv_list), \(i) {
      paste0(
        ">> [!infobox|background-purple wfull]+\n",
        ">> ### <font size='5'>", adv_list[[i]]$name, "</font> <font size='3'>(T", 
        adv_list[[i]]$tier, " ", adv_list[[i]]$type, ")</font>\n",
        ">> *", adv_list[[i]]$desc, "*\n",
        ">>\n",
        ">> | |<input type='checkbox' unchecked/> Hidden <input type='checkbox' unchecked/> Restrained <input type='checkbox' unchecked/> Vulnerable|\n",
        ">> |:-:|:-:|\n",
        ">> ||**Motives and Tactics:**<br>", adv_list[[i]]$motives, "|\n",
        ">>\n",
        ">> | Difficulty | Thresholds | ATK | ",  adv_list[[i]]$weaponName, ": ",  adv_list[[i]]$weaponRange, "|\n",
        ">> |:-:|:-:|:-:|:-:|\n",
        ">> |", adv_list[[i]]$difficulty, " | ", adv_list[[i]]$thresholdMajor, " / ", adv_list[[i]]$thresholdSevere, " | ",
        adv_list[[i]]$atk, " | ", adv_list[[i]]$weaponDamage, "|\n", 
        ">> ##### Resources\n",
        ">> | HP (", adv_list[[i]]$hp, ") | Stress (", adv_list[[i]]$stress, ")|\n",
        ">> |:---:|:---:|\n",
        mkdnize_hp.stress(adv_list[[i]]$hp, adv_list[[i]]$stress),
        ">> **Experience:** ", adv_list[[i]]$xp, "\n",
        ">> ### Features\n", markdownize_features(adv_list[[i]]$features), "\n"
      )
    })
  
  paste0(
    ">[!multi-column]\n",
    ">>[!infobox|background-purple wfull]+\n",
    paste(a_l, collapse = ">\n")
  ) 
}

