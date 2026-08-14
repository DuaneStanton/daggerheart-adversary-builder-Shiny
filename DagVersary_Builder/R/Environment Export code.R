# code for Obsidian - Daggerforge and Obsidian - ITS Theme environments export

# prelim countdown-processing functions ----------------------------------------
count_countdown <- function(txt) {
  poss_regex <- "countdown\\s*\\(.+?\\)|progress countdown\\s*\\(.+?\\)|countdown countdown\\s*\\(.+?\\)"
  
  find_ctdns <- stri_locate_all(str = tolower(txt), regex = poss_regex)[[1]]
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
        loop <- grepl("loop\\s*[0-9]*d{0,1}[0-9]*", tolower(ctdn_txt[i]))
        inc <- grepl("increasing\\s*[0-9]*d{0,1}[0-9]*", tolower(ctdn_txt[i]))
        dec <- grepl("decreasing\\s*[0-9]*d{0,1}[0-9]*", tolower(ctdn_txt[i]))

        typs <- c("Progress", "Consequence", "Loop", "Increasing", "Decreasing", "norm")
        idxs <- c(prog, cnsq, loop, inc, dec, !any(prog, cnsq, loop, inc, dec))
        
        paste(typs[idxs], collapse = ".")

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
  val <-  sub("countdown\\s*\\((.+)\\)", "\\1", tolower(txt))
  
  if (!grepl("[[:alpha:]]", val)) {
    nbr <- as.numeric(val)
  } else if (grepl("[0-9]*d[0-9]+", val)) {
    plus_minus <- grepl("\\+|\\-", val)
    if (plus_minus) {
      plus <- sub("^.*(\\+\\s*[0-9]+).*$", "\\1", val) |> sub(pattern = "\\+", replacement = "") 
      plus <- if (plus == val) {0} else {as.numeric(plus)}
      minus <- sub("^.*(\\-\\s*[0-9]+).*$", "\\1", val) |> sub(pattern = "\\-", replacement = "") 
      minus <- if (minus == val) {0} else {as.numeric(minus)}
      add_mod <- plus - minus 
    } else {add_mod <- 0}
    
    dice <- stri_extract(val, regex = "[0-9]*d[0-9]+")
    d_ct <- if (!grepl("^[0-9]+.*$", dice)) {1} else {as.numeric(sub("^([0-9]+)d.*$", "\\1", dice))}
    d_sd <- as.numeric(sub("^.+d", "", dice))
    nbr <- d_ct * d_sd + add_mod
  } else {
    nbr <- as.numeric(gsub("[^0-9]", "", val))
  }
  
  max(nbr, 1) # just insurance against an entry issue
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
          # add 5 to the number for buffer if an increasing countdown
          extract_ctdn_nbrs(ctdn_typ[[i]]$txt[j]) +
            ifelse(grepl("Increasing", ctdn_typ[[i]]$typ[j]), 5, 0)
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
    if (any(ctdn_ct[[i]]$ctdn_ct > 1 | grepl("Increasing", unlist(ctdn_typ[[i]]$typ)))) {

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


# Daggerforge export processing ================================================
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
      if (feattypes_set()[i] > 0) {inpt[[namify(typ, num, paste0("featname_", i))]]} else {NA_character_}
    }, character(1L))
  })
  
  feat_types <- reactive({
    vapply(1:5, \(i) {
      if (feattypes_set()[i] > 0) {inpt[[namify(typ, num, paste0("feattype_", i))]]} else {NA_character_}
    }, character(1L))
  })
  
  feat_descs <- reactive({
    vapply(1:5, \(i) {
      if (feattypes_set()[i] > 0) {inpt[[namify(typ, num, paste0("feattext_", i))]]} else {NA_character_}
    }, character(1L))
  })
  
  feat_qs <- reactive({
    vapply(1:5, \(i) {
      if (feattypes_set()[i] > 0) {inpt[[namify(typ, num, paste0("featquestion_", i))]]} else {NA_character_}
    }, character(1L))
  })
  
  countdown_processed_features <- process_countdowns(feat_names(), feat_descs())
  
  list(
    "feat_typeidx" = feattypes_set(),
    "feat_names" = feat_names(),
    "feat_types" = feat_types(),
    "feat_processed_txt" = countdown_processed_features,
    "feat_qs" = feat_qs()
  )
}

process_env_feats_df <- function(feat_list) {
  # ADD PRELIMINARY 'CHECK IF NOT FULLY EMPTY/NULL' HERE?
  
  df_ <- 
    data.frame(
      nm = feat_list$feat_names,
      typ = feat_list$feat_types,
      desc = feat_list$feat_processed_txt$df_fd,
      idx = feat_list$feat_typeidx,
      qs = feat_list$feat_qs
    )
  
  countdowns <- feat_list$feat_processed_txt$df_cd[order(df_$idx)] |> unlist()
  
  df_ <- df_[order(df_$idx),]
  df_ <- df_[df_$idx > 0,]
  
  df_$desc <- gsub("<b>|</b>", "", df_$desc)
  
  df_$cost <- vapply(1:nrow(df_), \(z) {
    if (grepl("^.pend a|[0-9]+ .ear", df_$desc[z])) {
      paste0("\u0022",
             sub("^(.pend a .ear|.pend [0-9] .ear).+$", "\\1", df_$desc[z]))
    } else {NA_character_}
  }, character(1L))
  
  df_$desc <- 
    gsub("^(.pend a .ear|.pend [0-9] .ear)", "", df_$desc) |> 
    gsub(pattern = "^ ", replacement = "")
  
  df_$richContent <- vapply(1:nrow(df_), \(z) {
    paste0("<div-class=\u005c\u0022df-p\u005c\u0022>",
           if (is.na(df_$cost[i])) {df_$desc[z]
           } else {sub(paste0("^.+", df_$cost[z], "<*/*b*>*"), "", df_$desc[z])},
           "</div>")
  }, character(1L))
  
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
      paste0("\u0009{\n", 
             "\u0009\u0022name\u0022: \u0022", df_$nm[z], "\u0022,\n",
             "\u0009\u0022type\u0022: \u0022", df_$typ[z], "\u0022,\n",
             if (!is.na(df_$cost[z])){paste0("\u0009\u0022cost\u022: ", df_$cost[z], "\u0022,\n")},
             "\u0009\u0022richContent\u0022: \u0022", df_$richContent[z], "\u0022,\n",
             "\u0009\u0022questions\u0022: [",
             if (!is.null(df_$qstns[[z]])) {
               paste0(
                 "\n",
                 vapply(1:length(df_$qstns[[z]]), \(y) {
                   paste0("\u0009\u0009\u0022", df_$qstns[[z]][y], "\u0022")
                   }, character(1L)) |> paste(collapse = ",\n"),
                 "\n\u0009\u0009]\n"
                 )
             } else {"]\n"},
             "\u0009}"
             )
    }) |> paste(collapse = ",\n")
  
  daggerforge_features <- 
    paste0("\u0022features\u0022: [\n", daggerforge_features, "\n\u0009]")
    
  # note: these are only 'manually processed' countdowns for cases where 1+
  # features has 2+ countdowns
  daggerforge_countdowns <- 
    if (!is.null(countdowns)) {
      paste0(
        "\u0022countdowns\u0022: [\n",
        vapply(1:length(countdowns), \(z) {
          paste0(
            "\u0009{\n",
            "\u0009\u0022", "name\u0022: \u0022", names(countdowns)[z], "\u0022,\n",
            "\u0009\u0022", "max\u0022: ", unname(countdowns[z]), "\n",
            "\u0009}"
          )
        }, character(1L)) |> 
          paste(collapse = ",\n"),
        "\n\u0009]\n"
      )
    }
  
    list("features" = daggerforge_features, "countdowns" = daggerforge_countdowns)
}

jsonify_environment <- function(inpt, typ, num) {
  features_countdowns <- 
    process_env_features(inpt, typ, num) |> 
    process_env_feats_df()
  
  paste0("{\n",
         "\u0022id\u0022: \u0022", 
         paste0("Dagversary_e", paste(sample(c(letters, LETTERS, 0:9), size = 5, replace = TRUE), collapse = "")),
         "\u0022,\n",
         "\u0022name\u0022: \u0022", inpt[[namify(typ, num, "name")]], "\u0022,\n",
         "\u0022tier\u0022: \u0022", inpt[["env_tier"]], "\u0022,\n",
         "\u0022type\u0022: \u0022", typ, "\u0022,\n",
         "\u0022desc\u0022: \u0022", inpt[[namify(typ, num, "desc")]], "\u0022,\n",
         "\u0022impulse\u0022: \u0022", inpt[[namify(typ, num, "impulses")]], "\u0022,\n",
         "\u0022difficulty\u0022: \u0022", inpt[[namify(typ, num, "diff")]], "\u0022,\n",
         "\u0022potentialAdversaries\u0022: \u0022", inpt[[namify(typ, num, "ptntl_adv")]], "\u0022,\n",
         "\u0022source\u0022: \u0022custom\u0022,\n",
         features_countdowns$features,
         if (!is.null(features_countdowns$countdowns)) {
           paste0(",\n", features_countdowns$countdowns)
         } else {"\n"}
         )
}

jsonify_env <- function(e_l) {
  paste0(
    "{\n",
    "\u0022environments\u0022: [\n",
    paste(e_l, collapse = "},\n"),
    "}\n]\n",
    "}"
  )
}


# ITS Theme (markdown) export processing =======================================
markdownize_countdown <- function(countdown_name_max_vec) {
  cnmvl <- countdown_name_max_vec
  
  if (length(cnmvl) > 0L) {
    cntdns <- 
      vapply(1:length(cnmvl), \(z){
        paste0("> | ", sub(" norm$", "", names(cnmvl)[z]), " | ", 
               paste(rep("<input type = 'checkbox' unchecked/>", unname(cnmvl)[z]), collapse = " "), " |")
      }, character(1L)) |> paste(collapse = "\n") |> paste0("\n>\n")
    
    paste0("> ### Countdowns\n",
           "> | Name | Count |\n",
           "> |:-|:-|\n",
           cntdns)
  }
}


markdownize_features_env <- function(feature_df) {
  paste0("> ### Features\n",
         vapply(1:nrow(feature_df), \(z) {
           paste0("> - ", feature_df$feat[z], "\n",
                  if (!is.null(feature_df$qstns[[z]])) {
                    vapply(1:length(feature_df$qstns[[z]]), \(y) {
                      paste0("> *", feature_df$qstns[[z]][y], "*\n")
                    }, character(1L)) |> paste(collapse = "")
                  })
         }, character(1L)) |> paste(collapse = ">\n")
         )
}


process_env_feats_md <- function(feat_list) {
  # ADD PRELIMINARY 'CHECK IF NOT FULLY EMPTY/NULL' HERE?
  
  df_ <- 
    data.frame(
      nm = feat_list$feat_names,
      typ = feat_list$feat_types,
      desc = feat_list$feat_processed_txt$md_fd,
      idx = feat_list$feat_typeidx,
      qs = feat_list$feat_qs
    )
  
  countdowns <- feat_list$feat_processed_txt$md_cd[order(df_$idx)] |> unlist()
  
  df_ <- df_[order(df_$idx),]
  df_ <- df_[df_$idx > 0,]
  
  df_$feat <- paste0("***", df_$nm, " - ", df_$typ, ":*** ", df_$desc)
  
  df_$qstns <- lapply(1:nrow(df_), \(z) {
    if (nchar(df_$qs[z]) > 0) {   #!is.na(df_$qs[z]) ) { #|| nchar(df_$qs[z]) > 0) {
      strsplit(df_$qs[z], split = "\\?")[[1]] |> 
        as.character() |> 
        paste0("?") |> 
        sub(pattern = "^ ", replacement = "")
    }
  })
  
  markdown_countdowns <- markdownize_countdown(countdowns)
  markdown_features <- markdownize_features_env(df_)
  
  list("countdowns" = markdown_countdowns, "features" = markdown_features)
}
 
markdownize_environment <- function(inpt, typ, num) {
  features_countdowns <- 
    process_env_features(inpt, typ, num) |> 
    process_env_feats_md()
  
  paste0("> [!infobox| background-purple wfull]+\n",
         "> # ", inpt[[namify(typ, num, "name")]], " (tier ", inpt[["env_tier"]], " ", typ, ")\n",
         "> *", inpt[[namify(typ, num, "desc")]], "*\n",
         "> **Impulses:** ", inpt[[namify(typ, num, "impulses")]], "\n",
         "> Difficulty: ", inpt[[namify(typ, num, "diff")]], "\n",
         "> Potential Adversaries: ", inpt[[namify(typ, num, "ptntl_adv")]], "\n",
         features_countdowns$countdowns,
         features_countdowns$features,
         "\n")
}