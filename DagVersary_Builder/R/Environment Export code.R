# code for Obsidian - Daggerforge and Obsidian - ITS Theme environments export

example_feature_names <- c("Thing The First", "Second Thing", "Three's The Charm", "Dark side of the fourth", "Five on it")
example_feature_descriptions <- c("Nothing to find in here", 
                                  "Spend a Fear to initiate a Progress Countdown (4) and Consequence Countdown (5) and stuff",
                                  "Single countdown (loop 1d4) to spice things up",
                                  "A countdown (3) then a countdown(2) then a countdown (1)",
                                  "A test consequence countdown (increasing 2)")

# prelim countdown-processing functions ----------------------------------------
count_countdown <- function(txt) {
  find_ctdns <- stri_locate_all(str = tolower(txt), regex = "countdown\\s*\\(.+?\\)")[[1]]
  ctdn_ct <- ifelse(all(is.na(find_ctdns)), 0, nrow(find_ctdns))
  list(
    "txt" = txt,
    "ctdn_idx" = if (ctdn_ct > 0L) {find_ctdns},
    "ctdn_ct" = ctdn_ct
  )
}

typify_countdown <- function(count_countdown_output) {
  cco <- count_countdown_output
  
  if (cco$ctdn_ct == 0L) {
    ctdn_txt <- ""
    ctdn_typs <- "none"
  } else {
    ctdn_txt <- vapply(cco$ctdn_ct:1, \(i) {
      strt <- if (i == 1) {1} else {cco$ctdn_idx[i - 1, "end"] + 1}
      fnsh <- cco$ctdn_idx[i, "end"]
      substring(cco$txt, first = strt, last = fnsh)
    }, character(1L))
    
    ctdn_typs <- 
      # need to process back-to-front, then reverse at the end so classifiers line up
      vapply(1:length(ctdn_txt), \(i) {
        prog <- grepl("progress countdown", tolower(ctdn_txt[i]))
        cnsq <- grepl("consequence countdown", tolower(ctdn_txt[i]))
        if (!prog & !cnsq) {"norm"} else if (prog) {"Progress"} else {"Consequence"}
      }, character(1L))
  }
  
  ctdn_txt <- if (cco$ctdn_ct > 0L) {rev(ctdn_txt)}
  ctdn_typs <- rev(ctdn_typs)
  
  # identify and manage if there are multiples of the same countdown type in the feature
  ctdn_typs_tbl <- table(ctdn_typs)
  
  mult_typ <- which(ctdn_typs_tbl > 1)
  
  if (length(mult_typ) > 0L) {
    m_t_typs <- names(mult_typ)
    typ_idx <- lapply(1:length(m_t_typs), \(i) {
      which(ctdn_typs == m_t_typs[i])
    })
    
    for (i in 1:length(typ_idx)) {
      for (j in 1:length(typ_idx[[i]])) {
        ctdn_typs[typ_idx[[i]][j]] <- 
          paste0(sub("norm", "", ctdn_typs[typ_idx[[i]][j]]), " ", j)
      }
    }
  }
  
  list("txt" = ctdn_txt, "typ" = ctdn_typs)
}

extract_ctdn_nbrs <- function(txt) {
  val <-  sub(".+countdown\\s*\\((.+)\\)", "\\1", tolower(txt))
  inc <- grepl("increasing", tolower(txt))
  
  if (!grepl("[:alpha:]", val)) {
    nbr <- as.numeric(val)
  } else if (grepl("[0-9]*d[0-9]+", val)) {
    dice <- stri_extract(val, regex = "[0-9]*d[0-9]+")
    d_ct <- if (!grepl("^[0-9]+.*$", dice)) {1} else {as.numeric(sub("^([0-9]+)d.*$", "\\1", dice))}
    d_sd <- as.numeric(sub("^.+d", "", dice))
    nbr <- d_ct * d_sd
  } else {
    nbr <- as.numeric(gsub("[^0-9]", "", val))
  }
  
  nbr + 5 * inc # add 5 to the number for buffer if an increasing countdown
}

prep_markdown_countdowns <- function(feature_name, typify_countdown_types, extract_ctdn_nbrs_output) {
  fn <- feature_name
  tct <- typify_countdown_types
  ecno <- extract_ctdn_nbrs_output
  
  if (!all(tct == "none")) {
    countdown_val_vec <- ecno
    names(countdown_val_vec) <- 
      vapply(1:length(tct), \(i) {paste(fn, tct[i])}, character(1L))
    
    countdown_val_vec
  }
} 

# function to process countdown details from environment features for Daggerforge/Obsidian use
process_countdowns <- function(feat_names, feat_descs) {
  ctdn_ct <- lapply(feat_descs, count_countdown) 
  ctdn_typ <- lapply(ctdn_ct, typify_countdown)
  ctdn_nbr <- 
    lapply(1:length(ctdn_typ), \(i) {
      if (!all(ctdn_typ[[i]]$typ == "none")) {
        vapply(1:length(ctdn_typ[[i]]$txt), \(j) {
          extract_ctdn_nbrs(ctdn_typ[[i]]$txt[j])
        }, numeric(1L))
      }
    })
  
  ctdn_mdp <- lapply(1:length(feat_names), \(i) {
    prep_markdown_countdowns(feat_names[i], ctdn_typ[[i]]$typ, ctdn_nbr[[i]])
  })
  
  
  # Daggerforge uses custom countdowns for increasing countdowns OR
  # there are more than 2 countdowns present for the feature
  daggerforge_featdesc <- feat_descs
  daggerforge_ctdns <- vector("list", length(feat_descs))
  for (i in 1:length(ctdn_ct)) {
    if (ctdn_ct[[i]]$ctdn_ct > 1) {
      # abbreviate 'Countdown' or 'countdown' to 'Ctdown' or 'ctdown' so Daggerforge doesn't
      # auto-populate (only first) countdown for multi-countdown features
      # ..these will have processing-introduced countdowns
      daggerforge_featdesc[i] <- gsub("(.)(ountdown)", "\\1tdown", daggerforge_featdesc[i])
      daggerforge_ctdns[[i]] <- ctdn_mdp[[i]]
    }
  }
  
  list("md_cd" = ctdn_mdp, "df_fd" = daggerforge_featdesc, "df_cd" = daggerforge_ctdns)
}

### ISSUE WITH EXAMPLE 4 COUNTDOWN 1

x <- 
  process_countdowns(example_feature_names, example_feature_descriptions)



###
### RESUME HERE: FUNCTION TO PROCESS FEATURE COUNTDOWN TEXT
### ANOTHER TO BOLD DAMAGE TEXT (#s) FOR MARKDOWN OUTPUT
### -ALSO ADD COUNTDOWN FUNCTIONALITY FOR ADVERSARIES- <MARKDOWN ONLY>; DaggerForge ALREADY HANDLES THIS FROM FEATURE TEXT CONTEXT e.g. "Featname - Passive: Countdown (1d4)" WILL PROCESS FOR 1d4 ROLL TO GENERATE THE COUNTDOWN

# COUNTDOWN TEXT FUNCTION: ID FEATURE NAME, NUMBER OF FEATURES IN THE DESCRIPTION TEXT,
# AND IF ANY OF THE COUNTDOWNS HAVE NAMES
# IF MULTIPLE COUNTDOWNS AND -NOT- NAMED, USE {feature name}_{countdown #} AS NAME
# SPECIAL CASE: PROGRESS AND COUNTDOWN IN THE FEATURE DESC AND THOSE ARE IT
# ...THEN NO '_#' SUFFIX


# NOTE: IN DAGGERFORGE, 'countdown' JSON FIELD IS ONLY INCLUDED IF THERE IS A SPECIFIED COUNTDOWN FOR THE ENVIRONMENT
# REFERENCE list_features_obsdn.dgrfrg / .obsdn (TOP OF RESPECTIVE CODE FILES)

###
### RESUME HERE; REFERENCE app.R LINE 342 AND RELATED FOR REFERENCE
### NOTES:
### - FOR NON-CHASE COUNTDOWN, USE Countdown (#) WORDING; CREATE CHECKBOX IN MARKDOWN FOR THIS
### ...IN TEXT PROCESSING ACCOUNT FOR LOWERCASE POTENTIAL
### ...SPECIAL CASE FOR Countdown (Loop #) ; NOTE THE LOOP NATURE AND OUTPUT MAX #
### ...OTHER SPECIAL CASE Countdown (Loop 1d{#}) ; NOTE THE DICE NATURE AND OUTPUT MAX #
###   AND OUTPUT CONTDOWNS FOR EACH WITH APPROPRIATE LABELS

# Daggerforge export processing ================================================

### TODO: MAKE SURE EXPORT UPDATES IF THE BUILDER CONTENT CHANGES

list_env_features_obsdn.dgrfrg <- function(inpt, typ, num) {
  # list Passives, then Actions, then Reactions...if feature text present
  
  feattypes_set <- reactive({
    c(if (has_feattxt(inpt, typ, num, 1)) {classify_feattyp(inpt, typ, num, 1)} else {0},
      if (has_feattxt(inpt, typ, num, 2)) {classify_feattyp(inpt, typ, num, 2)} else {0},
      if (has_feattxt(inpt, typ, num, 3)) {classify_feattyp(inpt, typ, num, 3)} else {0},
      if (has_feattxt(inpt, typ, num, 4)) {classify_feattyp(inpt, typ, num, 4)} else {0},
      if (has_feattxt(inpt, typ, num, 5)) {classify_feattyp(inpt, typ, num, 5)} else {0})
  })
  
  feat_names <- reactive({
    vapply(1:5, \(i) {
      if (feattypes_set()[i] > 0) {inpt[[namify(typ, num, paste0("featname_", num))]]}
    }, character(1L))
  })
  
  feat_descs <- reactive({
    vapply(1:5, \(i) {
      if (feattypes_set()[i] > 0) {inpt[[namify(typ, num, paste0("feattext_", num))]]}
    }, character(1L))
  })
  
  countdown_processed_features <- process_countdowns(feat_names(), feat_descs())
  
  ###
  ### RESUME HERE ; countdown_processed_features IS A REACTIVE
  ###
  
  feat_list0 <- reactive({
    if (!all(feattypes_set() == 0)) {
      x <- 
        do.call(
          what = rbind,
          args = lapply(1:length(feattypes_set()), \(z){
            data.frame(idx = z, typval = feattypes_set()[z],
                       txt =)) })
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
          x1 <- sub("<b><i>", "", x1)
          x1 <- sub("</i></b>", "", x1)
          nm_ <- sub("^(.+)\\s*-.+$", "\\1", x1[i]) |> gsub(pattern = "^\\s*|\\s*$", replacement = "")
          typ_ <- sub("^.+\\s*-\\s*(.+):.+$", "\\1", x1[i])
          cst_ <- if (grepl("Mark a|[0-9]+ .tress | Spend a|[0-9]+ .ear", x1[i])) {
            sub("^.+:\\s*<b>(.ark a .tress|.ark [0-9]+ .tress|Spend a .ear|Spend [0-9] .ear)</b>.+$", "\\1", x1[i])
          }
          rct_ <- 
            if (is.null(cst_)) {sub("^.+:\\s*", "", x1[i])
            } else {sub(paste0("^.+", cst_, "<*/*b*>*"), "", x1[i])} 
          rct_ <- gsub("\\&lt;", "<", rct_)
          rct_ <- gsub("\\&gt;", ">", rct_)
          
          list("name" = nm_, "type" = typ_, "cost" = cst_, "richContent" = rct_)
        })
      x2
    } else {
      list(list("name" = "", "type" = "", "cost" = "", "richContent" = ""))
    }
  })
  feat_list()
}

# function to create environment details list for export handling --------------
build_env_export_list <- function(inpt, typ, num, tr, for_md = TRUE) {
  feat_names <- x
  feat_descs <- y
  
  
  countdown_processed_features <- process_countdowns(feat_names, feat_descs) 
  
  
  
  ### BELOW HERE ONLY FOR REFERENCE
  list(
    "id" = .,
    "name" = .,
    "tier" = .,
    "type" = .,
    "desc" = .,
    "impulse" = .,
    "difficulty" = .,
    "potentialAdversaries" = .,
    "source" = "custom",
    "features" = 
      list(
        "name" = .,
        "type" = .,
        "cost" = ., # ONLY PRESENT IF FEATURE STARTS WITH "Spend a Fear" ...NEED TO PROCESS TO ACCOUNT FOR THIS
        "richContent" = .,
        "questions" = c()
      ),
    "countdowns" = # ONLY PRESENT WHEN PROCESSING-INTRODUCED COUNTDOWNS OUTSIDE THE FEATURE richContent
      list(
        "name" = .,
        "max" = .
      )
  )
}




# ITS Theme (markdown) export processing =======================================

