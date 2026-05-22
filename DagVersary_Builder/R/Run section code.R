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