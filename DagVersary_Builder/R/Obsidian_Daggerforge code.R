# code to take user-specified Customize tab detils into JSON structure for Daggerforge upload

# ALL FIELD NAMES LISTED IN ORDER

### REMEMBER COLOSSUS-SPECIFIC ELEMENTS - USE THE STRUCTURE BELOW AND POPULATE AS APPROPRIATE

# function for processing features
###
### RESUME HERE - CUE FROM list_features in Run code, and related/needed elements
###

# NOTE: SETUP FOR
jsonify_adversary <- function(inpt, typ, num, tr) {
  adv_name <- 
    paste0(inpt[[namify(typ, num, "name")]], 
           if (typ == "Horde") {paste0(adv_name, " (", inpt[[namify(typ, num, "perhp")]], "/HP)")})
    
  mot1 <- if (inpt[[namify(typ, num, "mottac1")]] != "") {inpt[[namify(typ, num, "mottac1")]]}
  mot2 <- if (inpt[[namify(typ, num, "mottac2")]] != "") {inpt[[namify(typ, num, "mottac2")]]}
  mot3 <- if (inpt[[namify(typ, num, "mottac3")]] != "") {inpt[[namify(typ, num, "mottac3")]]}
  
  mottac <- 
    if (any(!is.null(mot1), !is.null(mot2), !is.null(mot3))) {
      paste(mot1, mot2, mot3, collapse = ", ")} else {""}
    
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
    #id = paste0("Dagversary"),
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
    xp = inpt[[namify(a.t(adv_elemname), a.n(adv_elemname), "exp")]],
    source = custom,
    features =
      lapply()
  )
  
  lapply(1:1, \(i) {
    
    
  })
}

list_adv_name <- function(inpt, typ, num, tr) {
  nm <- reactive({inpt[[namify(typ, num, "name")]]})
  paste0("<b><span style=font-size: 2em;>", 
         ifelse(nm() == "", "Adversary", nm()), 
         "</spanp></b><span> (T", tr, " ", gsub("_", " ", typ), " \u0023", num, ")</span>",
         if (typ == "Horde"){paste0(" <span>(", inpt[[namify(typ, num, "perhp")]], "/HP)</span>")})
}




# single string entry per:
# id, name, tier, type (HORDE NOTE: ({#}/HP) GOES HERE AFTER 'Horde' ), desc, motives (comma separates), difficulty, thresholdMajor, thresholdSevere, hp, stress, atk,  
# weaponName, weaponRange, weaponDamage, xp, source (= "custom" for each)

# features list attributes in order by name (type):
# name (string), type (string), 
# cost (string; Mark a Stress or Spend a Fear as the leading text of the description),
# richContent (string, main/body of the feature; NOTE: do NOT format the dice/damage in bold - the Daggerforge plugin has some sort of utility to enable click-to-roll for unformatted {#}d{#} or d{#} text here)


adv_df <- data.frame(name = "Bob", tier = "1", type = "Thingy")
feat_1 <- data.frame(name = "Feature 1", type = "Passive", cost = "", richContent = "Feature text here")
feat_2 <- data.frame(name = "Feature 2", type = "Action", cost = "Mark a Stress", richContent = "Feature text here")

adv_df[1, "features"][[1]] <- list(feat_1)
adv_df[2, "features"][[1]] <- list(feat_2)

adv_list <- 
list(
  name = "Bob",
  tier = "1",
  type = "Thingy",
  features = list(
    list(name = "Feature 1", 
         type = "Passive", 
         cost = "", 
         richContent = "Feature text here"),
    list(name = "Feature 2", 
         type = "Action", 
         cost = "Mark a Stress", 
         richContent = "Feature text here")
  )
)


jsonlite::toJSON(adv_list) |> jsonlite::prettify() # MIGHT be OK...content isn't in continer boxes in the Daggerforge example, though...


jsonlite::toJSON(
  data.frame(
    adv_name = "Bob",
    adv_type = "Thingy",
    feats = list(list(x = 0, y = 1), list(x = 2, y = 3))
  )
)


