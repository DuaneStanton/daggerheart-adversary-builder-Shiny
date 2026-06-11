# code to take user-specified Customize tab details into JSON structure for Daggerforge upload

### REMEMBER COLOSSUS-SPECIFIC ELEMENTS - USE THE STRUCTURE BELOW AND POPULATE AS APPROPRIATE

# function for processing features
# version with -optional- text styling and features separated at end -----------

list_features_obsdn <- function(inpt, typ, num, json = TRUE) {
  # list Passives, then Actions, then Reactions...if feature text present
  feattypes_set <- reactive({
    c(if (has_feattxt(inpt, typ, num, 1)) {classify_feattyp(inpt, typ, num, 1)} else {0},
      if (has_feattxt(inpt, typ, num, 2)) {classify_feattyp(inpt, typ, num, 2)} else {0},
      if (has_feattxt(inpt, typ, num, 3)) {classify_feattyp(inpt, typ, num, 3)} else {0},
      if (has_feattxt(inpt, typ, num, 4)) {classify_feattyp(inpt, typ, num, 4)} else {0},
      if (has_feattxt(inpt, typ, num, 5)) {classify_feattyp(inpt, typ, num, 5)} else {0})
  })
  
  ### TODO: convert &lt; to < and &gt; to > in feature text
  feat_list0 <- reactive({
    if (!all(feattypes_set() == 0)) {
      x <- 
        do.call(
          what = rbind,
          args = lapply(1:length(feattypes_set()), \(z){
            data.frame(idx = z, typval = feattypes_set()[z],
                       txt = process_feature(inpt, typ, num, z)  |> 
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
          x1[i] <- gsub("<b>|<i>|</b>|</i>", "", x1[i])
          nm_ <- sub("^(.+)\\s*-.+$", "\\1", x1[i]) |> gsub(pattern = "^\\s*|\\s*$", replacement = "")
          typ_ <- sub("^.+\\s*-\\s*(.+):.+$", "\\1", x1[i])
          cst_ <- if (grepl("Mark a|[0-9]+ .tress | Spend a|[0-9]+ .ear", x1[i])) {
            sub("^.+:\\s*(.ark a .tress|.ark [0-9]+ .tress|Spend a .ear|Spend [0-9] .ear).+$", "\\1", x1[i])
          }
          rct_ <- 
            if (is.null(cst_)) {sub("^.+:\\s*", "", x1[i])
            } else {sub(paste0("^.+", cst_), "", x1[i])} 
          
          list("name" = nm_, "type" = typ_, "cost" = cst_, "richContent" = rct_)
        })
    x2
    } else {
      list(list("name" = "", "type" = "", "cost" = "", "richContent" = ""))
    }
  })
  feat_list()
}

# NOTE: SETUP FOR COLOSSI NEEDS TO BE SLIGHTLY DIFFERENT
json_prep_adversary <- function(inpt, typ, num, tr) {
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
    id = paste0("Dagversary", sample(1:1e6, 1)),
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
    features = list_features_obsdn(inpt, typ, num, json = TRUE)
  )
}

# function to process feature lists for jsonify() below
jsonify_features <- function(feat_list) {
  f_l <- 
  lapply(1:length(feat_list), \(i) {
    paste0(
      "\u0009\u0009{\n",
      "\u0009\u0009\u0022name\u0022: \u0022", feat_list[[i]]$name, "\u0022,\n",
      "\u0009\u0009\u0022type\u0022: \u0022", feat_list[[i]]$type, "\u0022,\n",
      "\u0009\u0009\u0022cost\u0022: \u0022", feat_list[[i]]$cost, "\u0022,\n",
      "\u0009\u0009\u0022richContent\u0022: \u0022", 
      "<div-class=\u005c\u0022df-p\u005c\u0022>", 
      feat_list[[i]]$richContent, "</div>\u0022\n",
      "\u0009\u0009}"
    )
  })

  paste(f_l, collapse = ",\n")
}

# function to create pseudo JSON ('pretty' JSON with " instead of standard \")
# note: \u0022 is unicode for ", \u0009 is unicode for horizontal tab, \u005c is unicode for \
jsonify <- function(adv_list) {
  a_l <- 
  lapply(1:length(adv_list), \(i) {
    paste0(
      "\u0009{\n",
      "  \u0009\u0022id\u0022: \u0022", adv_list[[i]]$id, "\u0022,\n",
      "  \u0009\u0022name\u0022: \u0022", adv_list[[i]]$name, "\u0022,\n",
      "  \u0009\u0022tier\u0022: \u0022", adv_list[[i]]$tier, "\u0022,\n",
      "  \u0009\u0022type\u0022: \u0022", adv_list[[i]]$type, "\u0022,\n",
      "  \u0009\u0022desc\u0022: \u0022", adv_list[[i]]$desc, "\u0022,\n",
      "  \u0009\u0022motives\u0022: \u0022", adv_list[[i]]$motives, "\u0022,\n",
      "  \u0009\u0022difficulty\u0022: \u0022", adv_list[[i]]$difficulty, "\u0022,\n",
      "  \u0009\u0022thresholdMajor\u0022: \u0022", adv_list[[i]]$thresholdMajor, "\u0022,\n",
      "  \u0009\u0022thresholdSevere\u0022: \u0022", adv_list[[i]]$thresholdSevere, "\u0022,\n",
      "  \u0009\u0022hp\u0022: \u0022", adv_list[[i]]$hp, "\u0022,\n",
      "  \u0009\u0022stress\u0022: \u0022", adv_list[[i]]$stress, "\u0022,\n",
      "  \u0009\u0022atk\u0022: \u0022", adv_list[[i]]$atk, "\u0022,\n",
      "  \u0009\u0022weaponName\u0022: \u0022", adv_list[[i]]$weaponName, "\u0022,\n",
      "  \u0009\u0022weaponRange\u0022: \u0022", adv_list[[i]]$weaponRange, "\u0022,\n",
      "  \u0009\u0022weaponDamage\u0022: \u0022", adv_list[[i]]$weaponDamage, "\u0022,\n",
      "  \u0009\u0022xp\u0022: \u0022", adv_list[[i]]$xp, "\u0022,\n",
      "  \u0009\u0022source\u0022: \u0022", adv_list[[i]]$source, "\u0022,\n",
      "  \u0009\u0022features\u0022: [\n", jsonify_features(adv_list[[i]]$features),
      "  \u0009]\n",
      "\u0009}"
    )
  })
  
  paste0(
    "{\n",
    "\u0022adversaries\u0022: [\n",
    paste(a_l, collapse = ",\n"),
    "\n  ]\n",
    "}"
  ) 
}
