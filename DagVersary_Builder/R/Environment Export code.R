# code for Obsidian - Daggerforge and Obsidian - ITS Theme environments export

example_feature_names <- c("Thing The First", "Second Thing", "Three's The Charm", "Dark side of the fourth", "Five on it")
example_feature_descriptions <- c("Nothing to find in here", 
                                  "Spend a Fear to initiate a Progress Countdown (4) and Consequence Countdown (5) and stuff",
                                  "Single countdown (loop 1d4) to spice things up",
                                  "A countdown (3) then a countdown(2) then a countdown (1)",
                                  "A test consequence countdown (increasing 2) to become Hidden and Vulnerable, then Mark 2 Stress")

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
  daggerforge_featdesc <- vapply(1:length(feat_descs), \(i) {
    process_feat_txt(feat_descs[i]) |> embolden()
  }, character(1L))
    
  markdown_featdesc <- vapply(1:length(daggerforge_featdesc), \(i) {
    txt <- daggerforge_featdesc[i]
    txt <- gsub("<b>|</b>", "**", txt)
    txt <- gsub("<i>|</i>", "*", txt)
    txt
  }, character(1L))
  
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
  
  list("md_cd" = ctdn_mdp,
       "md_fd" = markdown_featdesc,
       "df_fd" = daggerforge_featdesc, 
       "df_cd" = daggerforge_ctdns)
}

x <- 
  process_countdowns(example_feature_names, example_feature_descriptions)

# function to process feature text for Daggerforge/Markdown
process_env_feattxt_md <- function(feat_names, feat_types, feat_descs, feat_typidx, countdown_nbrs) {
  df_ <- 
  data.frame(
    txt = 
      vapply(1:length(feat_names), \(z) {
        paste0("***",feat_names[i], " - ", feat_types[i], ":",  "*** ", feat_descs[i])
        }, character(1L)),
    idx = feat_typidx
  )
  
  ###
  ### DON'T FORGET QUESTIONS! SEE ABANDONED GROVE IN OBSIDIAN FOR STYLING REFERENCE
  ###
  
  ### MAKE SURE INDICES STAY WITH THE EXACT FEATURE, NOT JUST THE FEATURE TYPE
  list("features" = df_$txt[order(df_$idx)],
       "countdowns" = countdown_nbrs[order(df_$idx)])
}


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

process_env_features <- function(inpt, typ, num) {
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
  
  feat_types <- reactive({
    vapply(1:5, \(i) {
      if (feattypes_set()[i] > 0) {inpt[[namify(typ, num, paste0("feattype_", num))]]}
    }, character(1L))
  })
  
  feat_descs <- reactive({
    vapply(1:5, \(i) {
      if (feattypes_set()[i] > 0) {inpt[[namify(typ, num, paste0("feattext_", num))]]}
    }, character(1L))
  })
  
  feat_qs <- reactive({
    vapply(1:5, \(i) {
      if (feattypes_set()[i] > 0) {inpt[[namify(typ, num, paste0("featquestion_", num))]]}
    }, character(1L))
  })
  
  countdown_processed_features <- process_countdowns(feat_names(), feat_descs())
  
  list(
    "feat_typeidx" = feattypes_set(),
    "feat_names" = feat_names(),
    "feat_types" = feat_types(),
    "feat_processed_txt" = countdown_processed_features(),
    "feat_qs" = feat_qs()
  )
}

process_env_feats_df <- function(feat_list) {#feat_names, feat_types, feat_descs, feat_qs, feat_typidx, countdown_nbrs) {
  # ADD PRELIMINARY 'CHECK IF NOT FULLY EMPTY/NULL' HERE?
  
  df_ <- 
    data.frame(
      nm = feat_list$feat_names,#feat_names,
      typ = feat_list$feat_types,#feat_types,
      desc = feat_list$feat_processed_txt$df_fd,#feat_descs,
      idx = feat_list$feat_typeidx,#feat_typidx
      qs = feat_list$feat_qs
    )
  
  df_ <- df_[order(df_$idx),]
  df_ <- df_[df_$idx > 0,]
  
  df_$cost <- vapply(1:nrow(df_), \(z) {
    if (grepl("Spend a|[0-9]+ .ear", df_$desc[z])) {
      paste0("\u0022",
             sub("^.+:\\s*<b>(Spend a .ear|Spend [0-9] .ear)</b>.+$", "\\1", df_$desc[z]),
             "\u0022,\n")
    } else {NA_character_}
  }, character(1L))
  
  df_$richContent <- vapply(1:nrow(df_), \(z) {
    paste0("<div-class=\u005c\u0022df-p\u005c\u0022>",
           if (is.na(df_$cost[i])) {df_$desc[z]
           } else {sub(paste0("^.+", df_$cost[z], "<*/*b*>*"), "", df_$desc[z])},
           "</div>\u0022,\n")
  }, character(1L))
  
  df_$countdown_nbrs <- lapply(1:length(countdown_nbrs), \(z){countdown_nbrs[[z]]})
  
  countdowns <- lapply(1:nrow(df_), \(z) {
    ctdn <- df_$countdown_nbrs[[z]]
    
    list_set <- 
      if (!is.null(ctdn)) {
        lapply(1:length(ctdn), \(y) {
          c("name" = names(ctdn)[y], "max" = unname(ctdn)[y])
        })
      }
  }) |> unlist(recursive = FALSE)
  
  df_$qstns <- lapply(1:nrow(df_), \(z) {
    if (nchar(df_$qs[z]) > 0) {
      strsplit(df_$qs[z], split = "\\?")[[1]] |> 
        as.character() |> 
        paste0("?") |> 
        sub(pattern = "^ ", replacement = "")
    }
  })
  
  daggerforge_features <- 
    lapply(1:nrow(df_), \(z) {
      paste0("\u0009\u0009{\n", 
             "\u0009\u0009\u0009\u0022name\u0022: \u0022", df_$nm[z], "\u0022,\n",
             "\u0009\u0009\u0009\u0022type\u0022: \u0022", df_$typ[z], "\u0022,\n",
             if (!is.na(df_$cost[z])){paste0("\u0009\u0009\u0009", df_$cost[z])},
             "\u0009\u0009\u0009\u0022richContent\u0022: \u0022", df_$richContent[z], "\u0022,\n",
             "\u0009\u0009\u0009\u0022questions\u0022: [",
             if (!is.null(df_$qstns[[z]])) {
               paste0(
                 "\n",
                 vapply(1:length(df_$qstns[[z]]), \(y) {
                   paste0("\u0009\u0009\u0009\u0009\u0022", df_$qstns[[z]][y], "\u0022")
                   }, character(1L)) |> paste(collapse = ",\n"),
                 "\u0009\u0009\u0009]"
                 )
             } else {"]\n"},
             "\u0009\u0009}"
             )
    }) |> paste(collapse = ",\n")
  
  daggerforge_features <- 
    paste0("\u0009\u0022features\u0022: [\n", daggerforge_features, "\n\u0009]")
    
  # note: these are only 'manually processed' countdowns for cases where 1+
  # features has 2+ countdowns
  daggerforge_countdowns <- 
    if (!is.null(countdowns)) {
      paste0(
        "\u0009\u0022countdowns\u0022: [\n",
        vapply(1:length(countdowns), \(z) {
          paste0(
            "\u0009\u0009{\n",
            "\u0009\u0009\u0009\u0022", "name: \u0022", countdowns[[z]][["name"]], "\u0022,\n",
            "\u0009\u0009\u0009\u0022", "max: ", countdowns[[z]][["max"]], "\n",
            "\u0009\u0009}"
          )
        }) |> 
          paste(collapse = ",\n"),
        "\u0009]\n"
      )
    }
    
    list("features" = daggerforge_features, "countdowns" = daggerforge_countdowns)
}

  ###
  ### RESUME HERE
  ###

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

