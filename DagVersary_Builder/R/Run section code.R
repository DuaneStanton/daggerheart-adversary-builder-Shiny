# code for the Run tab =========================================================
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

###
### TODO: FOR COLOSSAL ADVERSARIES, FILL NULL WITH DETAILS FOR Trogdor, the Burninator
###

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
      "<p style ='color: green; text-align: center'><b>Fully stressed! <i>Vulnerable</i> and any incurred stress reduces HP by 1.</b></p>"
    } else if (inpt[[namify(typ, num, "hp_run")]] == 0) {"<p style ='color: #9300CF; font-size: 25px; text-align: center;'><b>DEFEATED!</b></p>"}
  })
  
  renderUI(HTML(msg()))
}

msg_status <- function(inpt, typ, num) {
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
  msg <- reactive({ paste(msg_h(), msg_r(), msg_v(), collapse = "<br>") })
  
  renderUI(HTML(msg()))
}

# function bold /italicize key phrases in adversary featues --------------------
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

process_feature <- function(inpt, typ, num, ftrnum) {
  featnm <- reactive({
    if (has_feattxt(inpt, typ, num, ftrnum)) {
      ifelse(inpt[[namify(typ, num, paste0("featname_", ftrnum))]] == "", 
             "<b><i>{Feature}", 
             paste0("<b><i>", inpt[[namify(typ, num, paste0("featname_", ftrnum))]]))
    }
  })
  
  feattyp <- reactive({
    if (has_feattxt(inpt, typ, num, ftrnum)) {
      paste0(" - ", inpt[[namify(typ, num, paste0("feattype_", ftrnum))]], ": ")
    }
  })
  
  feattxt <- reactive({
    if (has_feattxt(inpt, typ, num, ftrnum)) {
      process_feat_txt(inpt[[namify(typ, num, paste0("feattext_", ftrnum))]])
    }
  })
  
  if (has_feattxt(inpt, typ, num, ftrnum)) {paste0(featnm(), feattyp(), "</i></b>", feattxt())} else {""}
}

classify_feattyp <- function(inpt, typ, num, ftrnum) {
  if (inpt[[namify(typ, num, paste0("feattype_", ftrnum))]] == "Passive") {1
  } else if (inpt[[namify(typ, num, paste0("feattype_", ftrnum))]] == "Action"){2
  } else if (inpt[[namify(typ, num, paste0("feattype_", ftrnum))]] == "Reaction"){3}
}

# function to input designated adversary-specific feature text details ---------
# note: designated-input structure MUST be of the form '<<{DETAIL TEXT}>>'

# first: useful helper function for processing the details
# function to process the detail text and return the 'populate' value with bold HTML formatting ----
prcs_nbr <- function(feat_det_txt, dice_dmg_txt, avg_dmg, tier) {
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
    if (grepl("tier", f_d_t)) {"tier"
    } else if (grepl("exp_dmg", f_d_t) & dice_dmg_txt == "Use Avg") {"avg_dmg"
    } else if (grepl("dmg", f_d_t) & dice_dmg_txt == "Use Avg") {"avg_dmg"
    } else if (grepl("exp_dmg", f_d_t) & dice_dmg_txt != "Use Avg") {"exp_dmg"
    } else if (grepl("dmg", f_d_t) & dice_dmg_txt != "Use Average"){"dice"
    } else {stop("Error: check feat details and compare against prcs_nbr() function routing")}
  
  val_mod <- if (pls) {add_} else if (mns) {-sub_} else {0}
  
  # processing for tier-based detail
  if (res == "tier") {
    tier_ <- as.numeric(tier) * ifelse(mult, mult_, 1) + val_mod
    tier_detail <- paste0("<b>", ceiling(tier_), "</b>")
  }
  
  # processing for average damage-based detail
  if (res == "avg_dmg") {
    avg_dmg_ <- ceiling(avg_dmg * ifelse(mult, mult_, 1)) + val_mod
    avg_dmg_detail <- paste0("<b>", avg_dmg_, "</b>")
  }
  
  # processing for dice-based detail
  if (res %in% c("exp_dmg", "dice")) {
    dice_ct <- sub("^.*([0-9]+)d[0-9]{1,2}.*$", "\\1", dice_dmg_txt) |> as.numeric()
    dice_sd <- sub("^.*[0-9]+d([0-9]{1,2}).*$", "\\1", dice_dmg_txt) |> as.numeric()
    dice_sign <- sub("^.*[0-9]+d[0-9]{1,2}\\s*(\\D+)[0-9]*$", "\\1", dice_dmg_txt)
    dice_sign <- ifelse(grepl("\\+", dice_sign), 1, -1)
    dice_md <- sub("^.*[0-9]+d[0-9]{1,2}\\s*\\D+([0-9]*)$", "\\1", dice_dmg_txt)
    dice_md <- ifelse(grepl("d", dice_md), 0, as.numeric(dice_md) * dice_sign)
    dice_sides <- c(4, 6, 8, 10, 12, 20)
    dice_exp_dmg <- if (res == "exp_dmg") {dice_ct * (1 + dice_sd) / 2 + dice_md}
    dice_exp_dmg <- ceiling(if (mult) {mult_ * dice_exp_dmg} else {dice_exp_dmg}) + val_mod
    
    dice_side <- 
      if (mult && mult_ == 0.5) {# go down a side (and keep count the same); offer not valid if using d4s
        if (dice_sd == 4) {4} else {dice_sides[max(which(dice_sides < dice_sd))]}
      } else if (mult && mult_ == 1.5) {# go up a side (and keep count the same); offers not valid if using d20s
        if (dice_sd == 20) {20} else {dice_sides[min(which(dice_sides > dice_sd))]}
      } else {dice_sd}
    
    dice_count <-
      if (mult && ((mult_ == 0.5 & dice_sd > 4) | (mult_ == 1.5 & dice_sd < 20))) {dice_ct
      } else if (mult) {ceiling(dice_ct * mult_)
      } else {dice_ct}
    
    dice_mod <- 
      if (mult && (mult_ == 0.5 & dice_sd == 4)){floor(mult_ * dice_md)
      } else if (mult && (mult_ == 1.5 & dice_sd == 20)) {ceiling(mult_ * dice_md)
      } else  if (mult) {ceiling(mult_ * dice_md)
      } else (dice_md)
    
    dice_mod_txt <- if (dice_mod != 0){ifelse(dice_mod > 0, paste0("+", dice_mod), as.character(dice_mod))}
    
    dice_detail <- paste0("<b>", dice_count, "d", dice_side, dice_mod_txt, "</b>")
    dice_exp_dmg <- paste0("<b>", dice_exp_dmg, "</b>")
  }
  
  # payoff: processed output to plug in to the feature text
  if (res == "tier"){tier_detail
  } else if (res == "avg_dmg") {avg_dmg_detail
  } else if (res == "exp_dmg") {dice_exp_dmg
  } else if (res == "dice") {dice_detail}
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
    
    if (typ == "Minion") {
      p_rep <- "min"
      min_pasv <- paste0("<b>", inpt[[namify(typ, num, "minion_pasv")]], "</b>")
    } else {
      tier_ <- inpt[["tier"]]
      dice_dmg <- inpt[[namify(typ, num, "dmg_dice")]] # note: -MAY- == "Use Average"
      avg_dmg <- inpt[[namify(typ, num, "dmg_avg")]]
    }
    ###
    ### CURRENTLY NOT ABLE TO DIRECTLY PROCESS/WORK WITH THE REPLACEMENT PATTERNS AS DESIRED
    ###
    for (z in 1:length(ptrns)) {
      txt_ <- 
        sub(pattern = ptrns[z], 
            replacement = 
              if (ptrns[z] == "<<minion_pasv>>") { min_pasv 
              } else {prcs_nbr(ptrns[z], dice_dmg, avg_dmg, tier_)},
            x = txt_)
    }
    
    txt_
  } else {
    txt
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
            data.frame(idx = z, typval = feattypes_set()[z],
                       # MOSTLY WORKING! LAST THING NEEDED - NEED TO RETAIN THE '<adversary>' text...
                       txt = process_feature(inpt, typ, num, z)  |> 
                             process_feat_txt_dtl(inpt = inpt, typ = typ, num = num)) })
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

# function for final adversary 'Run' tab UI ------------------------------------
build_adv_run_ui <- function(inpt, typ, num, tr, dmg_add) {
  div(
    fluidRow(column(width = 12,
          fluidRow(renderUI({HTML(list_adv_name(inpt, typ, num, tr))})),
          fluidRow(list_adv_desc(inpt, typ, num)),
          list_motives_tactics(inpt, typ, num),
          fluidRow(list_stats_1(inpt, typ, num))
          )),
    fluidRow(column(width = 12, msg_hp_stress(inpt, typ, num))),
    fluidRow(
      column(width = 3, 
             div(class = "inline",
                 selectInput(namify(typ, num, "hp_run"), 
                             label = "HP", 
                             choices = c(0:inpt[[namify(typ, num, "hp")]]), 
                             selected = inpt[[namify(typ, num, "hp")]], 
                             width = "70px"))
             ),
      ### TODO: AUTO-CHECK VULNERABLE BOX WHEN STRESS  = 0 ; MODIFY STRESSED MESSAGE TO REMOVE VULNERABLE TEXT
      column(width = 3,
             div(class = "inline",
                 selectInput(namify(typ, num, "stress_run"), 
                             label = "Stress", 
                             choices = c(0:inpt[[namify(typ, num, "stress")]]), 
                             selected = inpt[[namify(typ, num, "stress")]], 
                             width = "70px"))
      ) ),
    fluidRow(column(width = 12, list_features(inpt, typ, num))),
    fluidRow(column(width = 12, msg_status(inpt, typ, num))),
    fluidRow(div(class="inline",
                 checkboxGroupInput(namify(typ, num, "conds"), label = "Conditions: ", choices = c("Hidden", "Restrained", "Vulnerable"), inline = TRUE))),
    fluidRow(textify(typ, num, "custom_cond", placehold = "Type custom conditions here"))
  )
}