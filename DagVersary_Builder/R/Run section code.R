# code for the Run tab =========================================================
# UI for Run tab per adversary -------------------------------------------------
list_adv_name <- function(inpt, typ, num, tr) {
  nm <- inpt[[namify(typ, num, "name")]]
  paste0("<b><span style=font-size: 2em;>", 
         ifelse(nm == "", "Adversary", nm), 
         "</spanp></b><span> (T", tr, " ", gsub("_", " ", typ), " \u0023", num, ")</span>",
         if (typ == "Horde"){paste0(" <span>(", inpt[[namify(typ, num, "perhp")]], "/HP)</span>")})
}

list_adv_desc <- function(inpt, typ, num) {
  dsc <- inpt[[namify(typ, num, "desc")]]#})
  paste0(ifelse(dsc == "","The most foul, cruel, and bad-tempered rodent you ever set eyes on", dsc))
}

list_motives_tactics <- function(inpt, typ, num) {
  mt1 <- inpt[[namify(typ, num, "mottac1")]]
  mt2 <- inpt[[namify(typ, num, "mottac2")]]
  mt3 <- inpt[[namify(typ, num, "mottac3")]]
  
  if (any(mt1 != "", mt2 != "", mt3 != "")) {
    fluidRow(paste0("Motives and tactics: ", 
                    paste(ifelse(mt1 != "", mt1, "_drop_"),
                          ifelse(mt2 != "", mt2, "_drop_"),
                          ifelse(mt3 != "", mt3, "_drop_"), sep = ", ") |> 
                      gsub(pattern = " ,|_drop_,|, _drop_|,$", replacement = "")))
  }
}

list_stats_1 <- function(inpt, typ, num) {
  
  atk_txt <- 
    ifelse(inpt[[namify(typ, num, "atk")]] > -1,
           paste0("+", inpt[[namify(typ, num, "atk")]]),
           inpt[[namify(typ, num, "atk")]])

  dmg_dice <-
    if (inpt[[namify(typ, num, "dmg_dice")]] == "Custom") {inpt[[namify(typ, num, "cstm_dc")]]
    } else {inpt[[namify(typ, num, "dmg_dice")]]}
  
  dice_dmg <- 
    if (inpt$fight_type == "Tougher (add +1d4 to adversary damage rolls)") {
      paste(dmg_dice, "+1d4")
    } else if (inpt$fight_type == "Tougher (add +2 to adversary damage rolls)" &
               inpt[[namify(typ, num, "dmg_dice")]] != "Use Average") {
      dmg_end <- sub(".+d\\d+\\+(.+)", "\\1", dmg_dice) |> as.numeric()
      dmg_str <- sub("(.+d\\d+\\+).+", "\\1", dmg_dice)
      paste0(dmg_str, (dmg_end + 2))
    } else {dmg_dice}
  
  avg_dmg <- 
    if (inpt$fight_type == "Tougher (add +1d4 to adversary damage rolls)") {
      paste(inpt[[namify(typ, num, "dmg_avg")]], "+1d4")
    } else if (inpt$fight_type == "Tougher (add +2 to adversary damage rolls)") {
      inpt[[namify(typ, num, "dmg_avg")]] + 2
    } else {inpt[[namify(typ, num, "dmg_avg")]]}
  
  wpn_txt_lbl <- 
    paste0(ifelse(inpt[[namify(typ, num, "wpn")]] == "", "{weapon}", inpt[[namify(typ, num, "wpn")]]), ": ",
           inpt[[namify(typ, num, "rng")]])
  
  wpn_txt <- 
    paste(if (inpt[[namify(typ, num, "dmg_dice")]] == "Use Average") {avg_dmg
    } else {dice_dmg}, inpt[[namify(typ, num, "dmg_typ")]])
  
  df_ <- reactive({
    tibble(
      Diff = inpt[[namify(typ, num, "diff")]],
      Thresholds = paste(inpt[[namify(typ, num, "thresh_maj")]], "/", 
                         inpt[[namify(typ, num, "thresh_sev")]]),
      ATK = atk_txt,
      !!sym(wpn_txt_lbl) := wpn_txt
    )
  })
  
  renderTable({df_()}, align = "c")
}

msg_hp_stress <- function(inpt, typ, num) {
  msg <- reactive({
    if (inpt[[namify(typ, num, "stress_run")]] == 0 & inpt[[namify(typ, num, "hp_run")]] > 0) {
      "<p style ='color: green; text-align: center'><b>Fully stressed! <i>Vulnerable</i> and any incurred stress reduces HP by 1.</b></p>"
    } else if (inpt[[namify(typ, num, "hp_run")]] == 0) {"<p style ='color: #9300CF; font-size: 25px; text-align: center;'><b>DEFEATED!</b></p>"}
  })
  
  renderUI(HTML(msg()))
}

msg_status <- function(inpt, typ, num) {
  msg_d <- reactive({
    if ("Defeated" %in% inpt[[namify(typ, num, "conds")]]) { # currently only for Colossi
      "<p style ='color: #9300CF; font-size: 25px; text-align: center;'><b>DEFEATED!</b></p>"
    }
  })
  msg_h <- reactive({
    if ("Hidden" %in% inpt[[namify(typ, num, "conds")]]) {
      "<p style = 'color: #544659; text-align: center; font-size: 18px;'><b><i>Hidden:</i></b> rolls against have disadvantage"
    }
  })
  msg_r <- reactive({
    if ("Restrained" %in% inpt[[namify(typ, num, "conds")]]) {
      "<p style = 'color: black; text-align: center; font-size: 18px;'><b><i>Restrained:</i></b> cannot move, but can take actions"
    }
  })
  msg_v <- reactive({
    if ("Vulnerable" %in% inpt[[namify(typ, num, "conds")]]) {
      "<p style = 'color: darkred; text-align: center; font-size: 18px;'><b><i>Vulnerable:</i></b> rolls against have advantage"
    }
  })
  msg <- reactive({ paste(msg_d(), msg_h(), msg_r(), msg_v(), collapse = "<br>") })
  
  renderUI(HTML(msg()))
}


# function to bold /italicize key phrases in adversary featues -----------------
process_feat_txt <- function(txt) {
  txt_ <- txt
  
  find_bolders <- stri_locate_all(str = txt, regex = ".ark a .tress|.ark [0-9]+ .tress|.ark .tress|[0-9]+d[0-9]+|.pend a .ear|.pend [0-9]+ .ear|.pend .ear")[[1]]
  find_bolders <- cbind(find_bolders, 0) # creates 3 column with '0'
  find_italics <- stri_locate_all(str = txt, regex = "Hidden|hidden|Restrained|restrained|Vulnerable|vulnerable")[[1]]
  find_italics <- cbind(find_italics, 1)
  
  process_mat <- rbind(find_bolders, find_italics)
  process_mat <- process_mat[order(process_mat[, "start"]),]
  process_mat <- process_mat[!is.na(process_mat[, 1]),] # if only one non-NA row left, is coerced to named numeric
  if (!is.matrix(process_mat)) {process_mat <- process_mat |> t() |> as.matrix()}
  
  if (nrow(process_mat) > 0L) {
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

process_feature <- function(inpt, typ, num, ftrnum, mark_nobold = FALSE) {
  featnm <- reactive({
    if (has_feattxt(inpt, typ, num, ftrnum)) {
      ifelse(inpt[[namify(typ, num, paste0("featname_", ftrnum))]] == "", 
             paste0("<b><i>{Feature}", if (mark_nobold) {"xNBSx"}), 
             paste0("<b><i>", if (mark_nobold) {"xNBSx"}, inpt[[namify(typ, num, paste0("featname_", ftrnum))]]))
    }
  })
  
  feattyp <- reactive({
    if (has_feattxt(inpt, typ, num, ftrnum)) {
      paste0(" - ", inpt[[namify(typ, num, paste0("feattype_", ftrnum))]], ":", if (mark_nobold) {"xNBEx"})
    }
  })
  
  feattxt <- reactive({
    if (has_feattxt(inpt, typ, num, ftrnum)) {
      process_feat_txt(inpt[[namify(typ, num, paste0("feattext_", ftrnum))]])
    }
  })
  
  if (has_feattxt(inpt, typ, num, ftrnum)) {paste0(featnm(), feattyp(), "</i></b> ", feattxt())} else {""}
}

classify_feattyp <- function(inpt, typ, num, ftrnum) {
  if (inpt[[namify(typ, num, paste0("feattype_", ftrnum))]] == "Passive") {1
  } else if (inpt[[namify(typ, num, paste0("feattype_", ftrnum))]] == "Action"){2
  } else if (inpt[[namify(typ, num, paste0("feattype_", ftrnum))]] == "Reaction"){3}
}

# function to input designated adversary-specific feature text details ---------
# note: designated-input structure MUST be of the form '<<{DETAIL TEXT TO PROCESS}>>'

# first: useful helper function for processing feature detail text
# function to process the detail text and return the 'populate' value with bold HTML formatting ----
prcs_nbr <- function(feat_det_txt, dice_dmg_txt, avg_dmg, custm_dice, tier, minion_pasv = NULL, horde_perhp = NULL) {
  f_d_t <- gsub("<<|>>", "", feat_det_txt)
  # note: {DETAIL TEXT} involving multiplication / addition / subtraction -always- has the below form
  # {#}x {#d#  for dice // # for average dmg // 'tier' for tier} -/+{#} ; may or may not have multiplier AND add/sub
  mult <- grepl("x", f_d_t)
  mult_ <- if (mult) {sub("^([0-9]+\\.{0,1}[0-9]*)x.*$", "\\1", f_d_t) |> as.numeric()}
  pls <- grepl("\\+", f_d_t)
  add_ <- if (pls) {sub("^.+\\+\\s*([0-9]+)$", "\\1", f_d_t) |> as.numeric()}
  mns <- grepl("\\-", f_d_t)
  sub_ <- if (mns) {sub("^.+\\-\\s*([0-9]+)$", "\\1", f_d_t) |> as.numeric()}
  
  res <- 
    if (grepl("tier", f_d_t)) {
      if (grepl("square_tier", f_d_t)) {"square_tier"
      } else if (grepl("tierside_left", f_d_t)){"tierside_left"
      } else if (grepl("tierside_right", f_d_t)){"tierside_right"
      } else {"tier"}
    } else if (grepl("exp_dmg", f_d_t) & dice_dmg_txt == "Use Average") {"avg_dmg"
    } else if (grepl("dmg", f_d_t) & dice_dmg_txt == "Use Average") {"avg_dmg"
    } else if (grepl("exp_dmg", f_d_t) & dice_dmg_txt != "Use Average") {"exp_dmg"
    } else if (grepl("dmg", f_d_t) & dice_dmg_txt != "Use Average"){"dice"
    } else if (grepl("minion_pasv", f_d_t)){"minion_pasv"
    } else if (grepl("perhp", f_d_t)){"horde_perhp"
    } else {stop(paste0("Error: check feat details and compare against prcs_nbr() function routing - input text was ", f_d_t))}
  
  val_mod <- if (pls) {add_} else if (mns) {-sub_} else {0}
  
  # processing for tier-based detail
  if (res == "tier") {
    tier_detail <- ceiling(as.numeric(tier) * ifelse(mult, mult_, 1) + val_mod)
  }
  
  if (res == "square_tier") {
    tier_detail <- ceiling(as.numeric(tier)^2 * ifelse(mult, mult_, 1) + val_mod)
  }
  
  if (res == "tierside_left") { 
    tier_dice_side <- c(8, 10, 12, 20)[as.numeric(tier)]
    tier_detail <- tier_dice_side
  }
  
  if (res == "tierside_right") {  
    tier_dice_side <- c(4, 6, 8, 10)[as.numeric(tier)]
    tier_detail <- tier_dice_side
  }
  
  # processing for average damage-based detail
  if (res == "avg_dmg") {
    avg_dmg_detail <- ceiling(avg_dmg * ifelse(mult, mult_, 1)) + val_mod
  }
  
  # processing for dice-based detail
  if (res %in% c("exp_dmg", "dice")) {
    dice_dmg_txt <- ifelse(dice_dmg_txt == "Custom", custm_dice, dice_dmg_txt)
    
    dice_ct <- sub("^.*([0-9]+)d[0-9]{1,2}.*$", "\\1", dice_dmg_txt) |> as.numeric()
    dice_sd <- sub("^.*[0-9]+d([0-9]{1,2}).*$", "\\1", dice_dmg_txt) |> as.numeric()
    dice_sign <- sub("^.*[0-9]+d[0-9]{1,2}\\s*(\\D+)[0-9]*$", "\\1", dice_dmg_txt)
    dice_sign <- ifelse(grepl("\\+", dice_sign), 1, -1)
    dice_md <- sub("^.*[0-9]+d[0-9]{1,2}\\s*\\D+([0-9]*)$", "\\1", dice_dmg_txt)
    dice_md <- ifelse(grepl("d", dice_md), 0, as.numeric(dice_md) * dice_sign) # 0 modifier if nothing after {#}d{#}
    dice_sides <- c(4, 6, 8, 10, 12, 20)
    dice_exp_dmg <- dice_ct * (1 + dice_sd) / 2 + dice_md
    if (res == "exp_dmg") {dice_exp_dmg <- ceiling(if (mult) {mult_ * dice_exp_dmg} else {dice_exp_dmg}) + val_mod}
    
    # halved-damage mod: even # dice: keep same sides, halve # dice, halve add-on
    #                    odd # dice: go down 1 side, go down 1 die, halve add-on
    
    # dice change for damage multipliers
    if (mult && mult_ %% 1 == 0) { # integer multiplier: multiply dice count accordingly
      dice_side <- dice_sd
      dice_count <- mult_ * dice_ct
      dice_mod <- ceiling(mult_ * dice_md) + val_mod
    } else if (mult && mult_ == 0.5 && dice_ct %% 2 == 0) { # 1/2 multiplier and starting w/ even # dice: halve dice count
      dice_side <- dice_sd
      dice_count <- mult_ * dice_ct
      dice_mod <- dice_md / 2 + val_mod
      # other non-integer multiplier: add (mult_ > 1) or remove (mult_ < 1) dice count and/or tweak dice '+' modifier
      # keep option with expected value closest to {mult_ * original dice expected value}
    } else if (mult && mult_ > 1) { 
      dice_eval_chk <- expand.grid(side = dice_sides[dice_sides >= dice_sd],
                                   count = dice_ct + c(0, 1, 2),
                                   dice_mod = unique(c(-dice_md, -1, 0, 1, dice_md)))
      
      dice_eval_chk$val <-
        dice_eval_chk$count * (1 + dice_eval_chk$side) / 2 + dice_eval_chk$dice_mod
    
      best_dice <- which.min(abs(dice_exp_dmg * mult_ - dice_eval_chk$val))
      
      dice_side <- dice_eval_chk$side[best_dice]
      dice_count <- dice_eval_chk$count[best_dice]
      dice_mod <- dice_eval_chk$dice_mod[best_dice] + val_mod
    } else if (mult && mult_ < 1) {
      dice_eval_chk <- expand.grid(side = dice_sides[dice_sides <= dice_sd],
                                   count = (dice_ct - c(0, 1, 2))[(dice_ct - c(0, 1, 2)) >= 1],
                                   dice_mod = unique(c(-dice_md, -1, 0, 1, dice_md)))
      
      dice_eval_chk$val <-
        dice_eval_chk$count * (1 + dice_eval_chk$side) / 2 + dice_eval_chk$dice_mod
      
      best_dice <- which.min(abs(dice_exp_dmg * mult_ - dice_eval_chk$val))
      
      dice_side <- dice_eval_chk$side[best_dice]
      dice_count <- dice_eval_chk$count[best_dice]
      dice_mod <- dice_eval_chk$dice_mod[best_dice] + val_mod
    } else { # dice details but NOT multiplication modified
      dice_side <- dice_sd
      dice_count <- dice_ct
      dice_mod <- dice_md + val_mod
    }
    
    dice_mod_txt <- if (dice_mod != 0){ifelse(dice_mod > 0, paste0("+", dice_mod), as.character(dice_mod))}
    
    dice_detail <- paste0(dice_count, "d", dice_side, dice_mod_txt)
    dice_exp_dmg <- dice_exp_dmg
  }
  
  if (res == "minion_pasv") {
    minion_passive_detail <- ifelse(mult, mult_, 1) * minion_pasv + val_mod
  }
  
  if (res == "horde_perhp") {
    horde_perhp_detail <- ifelse(mult, mult_, 1) * horde_perhp + val_mod
  }
  
  # payoff: processed output to plug in to the feature text
  if (res %in% c("tier", "square_tier", "tierside_left", "tierside_right")) {tier_detail
  } else if (res == "avg_dmg") {avg_dmg_detail
  } else if (res == "exp_dmg") {dice_exp_dmg
  } else if (res == "dice") {dice_detail
  } else if (res == "minion_pasv") {minion_passive_detail
  } else if (res == "horde_perhp") {horde_perhp_detail}
}

# function to locate number-driven feature detail text to wrap in bold
embolden <- function(txt) {
  txt_ <- txt
  nbr_idx.0 <- stri_locate_all(str = txt, regex = "[0-9]+d{0,1}[0-9]*\\+{0,1}[0-9]*|d[0-9]+")[[1]]
  
  # special case: don't add bold formatting within {feature name} detail - already bolded along with {eature type}
  feat_start_idx <- stri_locate_all(str = txt, regex = "- Passive:|- Action:|- Reaction:")[[1]]
  
  if (!all(is.na(feat_start_idx))) {
    if (!all(is.na(nbr_idx.0))) {
      nbr_idx <- nbr_idx.0[nbr_idx.0[, 1] > max(feat_start_idx[, 2]), ] |> matrix(ncol = 2)
      colnames(nbr_idx) <- c("start", "end")
    } else {nbr_idx <- NA}
  }
  
  if (!all(is.na(nbr_idx))) {
    ptrns <- vapply(1:nrow(nbr_idx), \(z) {
      substring(txt, first = nbr_idx[z, "start"], last = nbr_idx[z, "end"])
    }, character(1L))
    
    for (z in length(ptrns):1) { # replacing back-to-front keeps position indices
      stringi::stri_sub(txt_, from = nbr_idx[z, "start"], to = nbr_idx[z, "end"]) <- 
        paste0("<b>", ptrns[z], "</b>")
    }
    
    # need to ensure no cases of double-bolding from here + process_feat_txt - causes formatting issue in markdown
    txt_ <- gsub("<b><b>", "<b>", txt_)
    txt_ <- gsub("</b></b>", "</b>", txt_)
    
    txt_
  } else {txt}
  
}

# 'main' feat processing function to replace <<DETAIL>> with desired adversary detail ----
process_feat_txt_dtl <- function(inpt, typ, num, txt) {
  txt_ <- txt
  caret_strt_idx <- stri_locate_all(str = txt, regex = "<<")[[1]]
  caret_end_idx <- stri_locate_all(str = txt, regex = ">>")[[1]]
  
  if (!all(is.na(caret_strt_idx))) {
    caret_idx <- matrix(nrow = nrow(caret_strt_idx), ncol = 2, dimnames = list(NULL, c("start", "end")))
    for (z in 1:nrow(caret_idx)) {
      caret_idx[z,] <- c(caret_strt_idx[z, "start"], caret_end_idx[z, "end"])
    }

    ptrns <- vapply(1:nrow(caret_idx), \(z) {
      substring(txt, first = caret_idx[z, "start"], last = caret_idx[z, "end"])
    }, character(1L))
    
    minion_pasv <- if (typ == "Minion") {inpt[[namify(typ, num, "minion_pasv")]]}
    horde_perhp <- if (typ == "Horde") {inpt[[namify(typ, num, "perhp")]]}
    
    tier_ <- inpt[["tier"]]
    dice_dmg <- inpt[[namify(typ, num, "dmg_dice")]] # note: -MAY- == "Use Average" OR "Custom"
    avg_dmg <- inpt[[namify(typ, num, "dmg_avg")]]
    cstm_dice <- inpt[[namify(typ, num, "cstm_dc")]]

    for (z in length(ptrns):1) { # replacing back-to-front keeps position indices
      stringi::stri_sub(txt_, from = caret_idx[z, "start"], to = caret_idx[z, "end"]) <- 
        prcs_nbr(ptrns[z], dice_dmg, avg_dmg, cstm_dice, tier_, minion_pasv, horde_perhp)
    }
    
    embolden(txt_)
  } else {
    embolden(txt)
  }
}

# function to only list 'active' adversary features ----------------------------
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
            txt_ <- process_feature(inpt, typ, num, z)
            txt_ <- process_feat_txt_dtl(inpt, typ, num, txt_)
            data.frame(idx = z, typval = feattypes_set()[z], txt = txt_) })
        )
      x <- x[x$typval > 0,]
      x <- x[order(x$typval),]
      paste(x$txt, collapse = "<br>---<br>")
    }
  })
  if (!is.null(feat_list())) {
    renderUI(HTML(feat_list()))
  } else {renderUI(HTML("No Features listed - add in 'Customize' tab if desired"))}
}

# function for final adversary 'Run' tab UI ------------------------------------
# note: pre-inclusion of Colossus adversaries, all code (incl. the Run UI output)
# was built on the basis of type and number (within type) as separate inputs and
# loop-driving elements...with Colossi making most sense to output framework and
# then segments, the workflow shifted to a combination 'type_number' input
# (to allow keeping Colossus framework + segments together if multiple Colossi
# are used together) ; thus code from here down is revised to work with the
# 'type_number' setup rather than 'typ, num' as two input elements
# ...code above here is NOT changed to avoid risk of breaking code

# helper functions to get adversary element type / number as-needed
a.t <- function(adv_elemname) {sub("(_[0-9])$", "", adv_elemname)}
a.n <- function(adv_elemname) {sub(".+_([0-9])$", "\\1", adv_elemname)}

build_adv_run_ui <- function(inpt, adv_elemname, tr) {
  div(
    fluidRow(column(width = 12,
          fluidRow(renderUI({HTML(list_adv_name(inpt, a.t(adv_elemname), a.n(adv_elemname), tr))})),
          fluidRow(list_adv_desc(inpt, a.t(adv_elemname), a.n(adv_elemname))),
          list_motives_tactics(inpt, a.t(adv_elemname), a.n(adv_elemname)),
          fluidRow(list_stats_1(inpt, a.t(adv_elemname), a.n(adv_elemname)))
          )),
    fluidRow(column(width = 12, msg_hp_stress(inpt, a.t(adv_elemname), a.n(adv_elemname)))),
    fluidRow(
      column(width = 3, 
             div(class = "inline",
                 selectInput(namify(a.t(adv_elemname), a.n(adv_elemname), "hp_run"), 
                             label = "HP", 
                             choices = c(0:inpt[[namify(a.t(adv_elemname), a.n(adv_elemname), "hp")]]), 
                             selected = inpt[[namify(a.t(adv_elemname), a.n(adv_elemname), "hp")]], 
                             width = "70px"))
             ),
      ### TODO: AUTO-CHECK VULNERABLE BOX WHEN STRESS  = 0 ; MODIFY STRESSED MESSAGE TO REMOVE VULNERABLE TEXT
      column(width = 3,
             div(class = "inline",
                 selectInput(namify(a.t(adv_elemname), a.n(adv_elemname), "stress_run"), 
                             label = "Stress", 
                             choices = c(0:inpt[[namify(a.t(adv_elemname), a.n(adv_elemname), "stress")]]), 
                             selected = inpt[[namify(a.t(adv_elemname), a.n(adv_elemname), "stress")]], 
                             width = "70px"))
      ) ),
    if (inpt[[namify(a.t(adv_elemname), a.n(adv_elemname), "exp")]] != "") {
      fluidRow(column(width = 12, 
                      renderUI({HTML(
                        paste0("<b>Experience(s):</b> ", inpt[[namify(a.t(adv_elemname), a.n(adv_elemname), "exp")]]))}))
               )
    },
    fluidRow(column(width = 12, list_features(inpt, a.t(adv_elemname), a.n(adv_elemname)))),
    fluidRow(column(width = 12, msg_status(inpt, a.t(adv_elemname), a.n(adv_elemname)))),
    fluidRow(div(class="inline",
                 checkboxGroupInput(namify(a.t(adv_elemname), a.n(adv_elemname), "conds"), 
                                    label = "Conditions: ", 
                                    choices = c("Hidden", "Restrained", "Vulnerable"), 
                                    inline = TRUE))),
    fluidRow(textify(a.t(adv_elemname), a.n(adv_elemname), "custom_cond", placehold = "Type custom conditions here"))
  )
}

# function to organize and output Colossus framework/segment -------------------
# function to organize framework/segments when multiple frameworks -------------
# id 'grp' to ID colossus grouping (e.g. Colossus 1 framework, then Colossus 1 segments, then Colossus 2 framework...)
# id 'fwk' to ID colossus framework - redundant for the framework itself; intended for carrying framework damage thresholds into associated segment stat blocks  
# id 'seg' to ID names of other segments with the same parent framework
id_colossus_components <- function(inpt, adv_ct_vec, id = "grp") {
  # framework and segment counts
  fw_ct <- adv_ct_vec["Colossus_framework"]
  sg_str_ct <- adv_ct_vec["Colossus_strong_segment"]
  sg_avg_ct <- adv_ct_vec["Colossus_average_segment"]
  
  # group colossus components appropriately
  if (fw_ct == 1) {
    if (id == "grp") {
      paste0("Colossus_", 
             c("framework_1",
               if (sg_str_ct > 0) {paste0("strong_segment_", 1:sg_str_ct)},
               if (sg_avg_ct > 0) {paste0("average_segment_", 1:sg_avg_ct)})
             )
    } else if (id == "fwk") {
      rep("Colossus_framework_1", 1 + sg_str_ct + sg_avg_ct)
    } else if (id == "seg") { # 'name' paste prevents issues with initially-empty segment names - this is processed out in app.R
      c(if (sg_str_ct > 0) {vapply(1:sg_str_ct, \(z) {paste0("name", inpt[[namify("Colossus_strong_segment", z, "name")]])}, character(1L))},
        if (sg_avg_ct > 0) {vapply(1:sg_avg_ct, \(z) {paste0("name", inpt[[namify("Colossus_average_segment", z, "name")]])}, character(1L))}) 
    }
  } else if (fw_ct > 1) { # grouping by 'containing' framework

    fw_names <- vapply(1:fw_ct, \(z) { 
      inpt[[paste0("Colossus_framework_", z, "_name")]]
    }, character(1L))
    sg_str_fw <- if (sg_str_ct > 0) {
      vapply(1:sg_str_ct, \(z) {
        inpt[[paste0("Colossus_strong_segment_", z, "_parent_frame")]]
      }, character(1L))
    }
    sg_avg_fw <- if (sg_avg_ct > 0) {
      vapply(1:sg_avg_ct, \(z) {
        inpt[[paste0("Colossus_average_segment_", z, "_parent_frame")]]
      }, character(1L))
    }
    
    cls_grps <- 
      lapply(1:length(fw_names), \(z) {
        vec <- 
        c(paste0("Colossus_framework_", z),
          if (!is.null(sg_str_fw)) {
            paste0("Colossus_strong_segment_", which(sg_str_fw == fw_names[z]))
          },
          if (!is.null(sg_avg_fw)) {
            paste0("Colossus_average_segment_", which(sg_avg_fw == fw_names[z]))
          })
    
        vec[!(vec %in% c("Colossus_average_segment_", "Colossus_strong_segment_"))]
      })
    
    fwk_grps <- 
      lapply(1:length(fw_names), \(z) {
        vec <- 
          c(paste0("Colossus_framework_", z),
            if (!is.null(sg_str_fw)) {
              rep(paste0("Colossus_framework_", z), sum(sg_str_fw == fw_names[z]))
            },
            if (!is.null(sg_avg_fw)) {
              rep(paste0("Colossus_framework_", z), sum(sg_avg_fw == fw_names[z]))
            })
        
        vec[!grepl("segment", vec)]
      })
    
    seg_nms <- 
      lapply(1:length(fw_names), \(z) {
        seg_ids <- cls_grps[[z]][grepl("segment", cls_grps[[z]])]
        vapply(1:length(seg_ids), \(a) {paste0("name", inpt[[namify(a.t(seg_ids[a]), a.n(seg_ids[a]), "name")]])}, character(1L))
      })
    # name the list entries (vectors of segment names) with the frameworks - can then select segment name sets using 'id == fwk'-generated run of this function
    names(seg_nms) <- fw_names
    
    if (id == "grp") {unlist(cls_grps, recursive = FALSE)
    } else if (id == "fwk") {unlist(fwk_grps, recursive = FALSE)
    } else if (id == "seg") {seg_nms}
    
    }
  }

list_motives_tactics_adjacent_sgmt <- function(inpt, typ, num) {
  mt1 <- inpt[[namify(typ, num, "mottac_adj1")]]
  mt2 <- inpt[[namify(typ, num, "mottac_adj2")]]
  mt3 <- inpt[[namify(typ, num, "mottac_adj3")]]
  mt4 <- inpt[[namify(typ, num, "mottac_adj4")]]
  
  mt_set <- c(mt1, mt2, mt3, mt4) |> unique()
  if (length(mt_set) > 1L) {mt_set <- mt_set[mt_set != "None"]}
  
  if (!is.null(mt_set)) {
    fluidRow(paste0(ifelse(grepl("framework", typ), "Motives and tactics: ", "Adjacent segment(s): "),
                    sub(", $", "", paste(mt_set, collapse = ", "))
    ))
  }
}

list_stats_fw <- function(inpt, typ, num) {
  df_ <- reactive({
    tibble(
      Size = inpt[[namify(typ, num, "sz")]],
      Diff = inpt[[namify(typ, num, "diff")]],
      Thresholds = paste(inpt[[namify(typ, num, "thresh_maj")]], "/", 
                         inpt[[namify(typ, num, "thresh_sev")]])
    )
  })
  renderTable({df_()}, align = "c")
}

list_stats_sg <- function(inpt, typ, num) {
  atk_txt <- reactive({ifelse(inpt[[namify(typ, num, "atk")]] > -1, 
                              paste0("+", inpt[[namify(typ, num, "atk")]]),
                              inpt[[namify(typ, num, "atk")]])})
  
  dmg_dice <-
    if (inpt[[namify(typ, num, "dmg_dice")]] == "Custom") {inpt[[namify(typ, num, "cstm_dc")]]
    } else {inpt[[namify(typ, num, "dmg_dice")]]}
  
  dice_dmg <- 
    if (inpt$fight_type == "Tougher (add +1d4 to adversary damage rolls)") {
      paste(dmg_dice, "+1d4")
    } else if (inpt$fight_type == "Tougher (add +2 to adversary damage rolls)" &
               inpt[[namify(typ, num, "dmg_dice")]] != "Use Average") {
      dmg_end <- sub(".+d\\d+\\+(.+)", "\\1", dmg_dice) |> as.numeric()
      dmg_str <- sub("(.+d\\d+\\+).+", "\\1", dmg_dice)
      paste0(dmg_str, (dmg_end + 2))
    } else {dmg_dice}
  
  avg_dmg <- 
    if (inpt$fight_type == "Tougher (add +1d4 to adversary damage rolls)") {
      paste(inpt[[namify(typ, num, "dmg_avg")]], "+1d4")
    } else if (inpt$fight_type == "Tougher (add +2 to adversary damage rolls)") {
      inpt[[namify(typ, num, "dmg_avg")]] + 2
    } else {inpt[[namify(typ, num, "dmg_avg")]]}
  
  wpn_txt_lbl <- reactive({
    paste0(ifelse(inpt[[namify(typ, num, "wpn")]] == "", "{weapon}", inpt[[namify(typ, num, "wpn")]]), ": ",
           inpt[[namify(typ, num, "rng")]])})
  
  wpn_txt <- reactive({
    paste(if (inpt[[namify(typ, num, "dmg_dice")]] == "Use Average") {avg_dmg
    } else {dice_dmg}, inpt[[namify(typ, num, "dmg_typ")]])
  })
  
  df_ <- reactive({
    tbl_ <- 
    tibble(
      Diff = inpt[[namify(typ, num, "diff")]], ATK = atk_txt(), wpn = wpn_txt()
      )
    colnames(tbl_)[3] <- wpn_txt_lbl()
    tbl_
  })
  
  renderTable({df_()}, align = "c")
}

### TODO: ADD 'DEFEATED!' TO ALL FRAMEWORK/SEGMENT UI WHEN NECESSARY CONDITION MET (MAKE CHECKBOX FOR FRAMEWORK)
### https://stackoverflow.com/questions/42222469/shiny-change-background-color-of-htmloutput-conditionally
### ...MAYBE ALSO FOR REGULAR ADVERSARIES
msg_stress <- function(inpt, typ, num, dftd = FALSE) {
  msg <- reactive({
    if (!dftd && inpt[[namify(typ, num, "stress_run")]] == 0) {
      "<p style ='color: green; text-align: center'><b>Fully stressed! <i>Vulnerable</i> and any incurred stress reduces HP by 1.</b></p>"
    } else if (dftd) {"<p style ='color: #9300CF; font-size: 25px; text-align: center;'><b>DEFEATED!</b></p>"}
  })
  
  renderUI(HTML(msg()))
}

msg_hp <- function(inpt, typ, num, dftd = FALSE) {
  msg <- reactive({
    if (dftd | inpt[[namify(typ, num, "hp_run")]] == 0) {
      "<p style ='color: #9300CF; font-size: 25px; text-align: center;'><b>DEFEATED!</b></p>"}
  })
  
  renderUI(HTML(msg()))
}

# functions for final Colossus 'Run' tab UI ------------------------------------
build_colossus_fw_run_ui <- function(inpt, col_elemname, tr) {
  div(
    fluidRow(column(width = 12,
                    fluidRow(renderUI({HTML(list_adv_name(inpt, a.t(col_elemname), a.n(col_elemname), tr))})),
                    fluidRow(list_adv_desc(inpt, a.t(col_elemname), a.n(col_elemname))),
                    list_motives_tactics_adjacent_sgmt(inpt, a.t(col_elemname), a.n(col_elemname)),
                    fluidRow(list_stats_fw(inpt, a.t(col_elemname), a.n(col_elemname)))
    )),
    fluidRow(
      column(width = 3, 
             div(class = "inline",
                 selectInput(namify(a.t(col_elemname), a.n(col_elemname), "stress_run"), 
                             label = "Stress", 
                             choices = c(0:inpt[[namify(a.t(col_elemname), a.n(col_elemname), "stress")]]), 
                             selected = inpt[[namify(a.t(col_elemname), a.n(col_elemname), "stress")]], 
                             width = "70px"))
      ),
      ### TODO: AUTO-CHECK VULNERABLE BOX WHEN STRESS  = 0 ; MODIFY STRESSED MESSAGE TO REMOVE VULNERABLE TEXT
      column(width = 9,
             div(class = "inline",
                 msg_stress(inpt, a.t(col_elemname), a.n(col_elemname), dftd = FALSE) ) ### IF ACTIVATED, 'dftd' NEEDS INPUT FROM THE  SERVER --> FROM THE build_colossus_fw_run_ui LEVEL WITH A SERVER-INTERNAL REACTIVE/OBSERVER
      ) ),
    fluidRow(column(width = 12, list_features(inpt, a.t(col_elemname), a.n(col_elemname)))),
    fluidRow(column(width = 12, msg_status(inpt, a.t(col_elemname), a.n(col_elemname)))),
    fluidRow(div(class="inline",
                 ### TODO: ADD 'Defeated' TO THE CONDITION SET???
                 checkboxGroupInput(namify(a.t(col_elemname), a.n(col_elemname), "conds"), 
                                    label = "Conditions: ", choices = c("Hidden", "Restrained", "Vulnerable", "Defeated"), 
                                    inline = TRUE))),
    fluidRow(textify(a.t(col_elemname), a.n(col_elemname), "custom_cond", placehold = "Type custom conditions here"))
  )
}

build_colossus_sg_run_ui <- function(inpt, col_elemname, tr) {
  div(
    fluidRow(column(width = 12,
                    fluidRow(renderUI({HTML(list_adv_name(inpt, a.t(col_elemname), a.n(col_elemname), tr))})),
                    list_motives_tactics_adjacent_sgmt(inpt, a.t(col_elemname), a.n(col_elemname))
    )),
    fluidRow(
      column(width = 3,
             div(class = "inline", fluidRow(list_stats_sg(inpt, a.t(col_elemname), a.n(col_elemname))))
      ),
      column(width = 2, offset = 1, 
             div(class = "inline",
                 selectInput(namify(a.t(col_elemname), a.n(col_elemname), "hp_run"), 
                             label = "HP", 
                             choices = c(0:inpt[[namify(a.t(col_elemname), a.n(col_elemname), "hp")]]), 
                             selected = inpt[[namify(a.t(col_elemname), a.n(col_elemname), "hp")]], 
                             width = "70px"))
      ),
      column(width = 6,
             div(class = "inline",
                 msg_hp(inpt, a.t(col_elemname), a.n(col_elemname), dftd = FALSE) ) ### IF ACTIVATED, 'dftd' NEEDS INPUT FROM THE  SERVER --> FROM THE build_colossus_fw_run_ui LEVEL WITH A SERVER-INTERNAL REACTIVE/OBSERVER
      )
      
    ),
    fluidRow(column(width = 12, list_features(inpt, a.t(col_elemname), a.n(col_elemname)))),
    fluidRow(column(width = 12, msg_status(inpt, a.t(col_elemname), a.n(col_elemname)))),
    fluidRow(div(class="inline",
                 checkboxGroupInput(namify(a.t(col_elemname), a.n(col_elemname), "conds"), 
                                    label = "Conditions: ", choices = c("Hidden", "Restrained", "Vulnerable"), 
                                    inline = TRUE))),
    fluidRow(textify(a.t(col_elemname), a.n(col_elemname), "custom_cond", placehold = "Type custom conditions here"))
  )
}
