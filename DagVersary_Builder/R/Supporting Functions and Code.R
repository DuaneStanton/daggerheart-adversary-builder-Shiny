# Functions and Code Setup for the DagVersary App

library(dplyr) # needed here for data.frame processing

# values used in multiple places in the app ------------------------------------
adv_types <- 
  c("Bruiser", "Horde", "Leader", "Minion", "Ranged", "Skulk", "Solo", "Standard", "Support", "Social",
    "Colossus_framework", "Colossus_strong_segment", "Colossus_average_segment")
tier_vals <- 1:4
distances <- c("Melee", "Very Close", "Close", "Far", "Very Far")
adv_type_detail <- 
  c("Bruiser" = "Tough and deliver powerful attacks",
    "Horde" = "Group of identical creatures acting together",
    "Leader" = "Command and summon other adversaries",
    "Minion" = "Easily dispatched but dangerous in numbers",
    "Ranged" = "Fragile in close combat, but can attack from a distance for high damage",
    "Skulk" = "Maneuver and exploit opportunities to ambush",
    "Solo" = "A focal challenge to the party (may need a boost if no support, e.g. phases)",
    "Standard" = "'Rank-and-file' adversary typical of their fictional group",
    "Support" = "Enhance their allies and disrupt opponents",
    "Social" = "Present unique challenges to overcome through conversation, not combat",
    "Colossus_framework" = "'Core' of a huge adversary with multiple active parts",
    "Colossus_strong_segment" = "Active part of a huge adversary - more HP and damage",
    "Colossus_average_segment" = "Active part of a huge adversary - less HP and damage")

# build adversary type count ---------------------------------------------------
build_adv_count <- function(typ) {
  tags$div(class = "adv-count-sel", title = adv_type_detail[[typ]],
  numericInput(inputId = paste0(typ, "_count"), 
               label = paste0("# ", gsub("_", " ", typ), "s"),
               value = 0, min = 0, step = 1),
  style="display:inline-block")
}

# read in CSV of recommended/starter adversary stats by type and tier ----------
# note: stat ranges come from (and full credit owed to)  
#       RightKnighttoFight’s Guide to Making Custom Adversaries v1.6

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
stat_ref_df <- 
  read.csv("dagversary_stats_reference.csv") |> 
  mutate(
    across(.cols = ends_with("_rng"), .fns = calc_midpt, .names = "{.col}_md"),
    adv_type = gsub(" ", "_", adv_type)
  )
md_cols_idx <- grep("_rng_md", colnames(stat_ref_df))
colnames(stat_ref_df)[md_cols_idx] <- sub("_rng", "", colnames(stat_ref_df)[md_cols_idx])

stat_ref_df$dice_pool_lst <- 
  lapply(1:nrow(stat_ref_df), \(i) {if (!is.na(stat_ref_df$dice_pool_optns[i])){
    strsplit(stat_ref_df$dice_pool_optns[i], split = ",")[[1]] }})

# load adversary feature reference ---------------------------------------------
# note: when working interactively in this R script, use
#       "DagVersary_Builder/Daggerheart adversary feature lookup.csv" instead
feat_ref_df <- 
  read.csv("Daggerheart adversary feature lookup.csv") |> 
  # expand so 1 row per type/tier/feat
  mutate(
    adv_type = gsub(" ", "_", adv_type),
    incl_t1 = grepl("1", adv_tiers),
    incl_t2 = grepl("2", adv_tiers),
    incl_t3 = grepl("3", adv_tiers),
    incl_t4 = grepl("4", adv_tiers),
    tier_ct = incl_t1 + incl_t2 + incl_t3 + incl_t4,
    feat_text = gsub("<adversary>", "&lt;adversary&gt;", feat_text)
  )

for (i in 1:nrow(feat_ref_df)) {
  feat_ref_df$feat_text[i] <- 
    gsub(paste0("<", feat_ref_df$adv_type[i], ">"), paste0("&lt;", feat_ref_df$adv_type[i], "&gt;"), feat_ref_df$feat_text[i])
}

feat_ref_df <- 
  lapply(1:nrow(feat_ref_df), \(i) {
    lapply(1:(feat_ref_df$tier_ct[i]), \(j) {
      unique_tier <- 
        strsplit(feat_ref_df$adv_tiers[i], split = ",")[[1]] |> 
        gsub(pattern = " ", replacement = "") |> 
        as.numeric()
    
      feat_ref_df[i,] |> 
        select(-c(starts_with("incl_"), contains("tier"))) |> 
        mutate(tier = unique_tier[j])
  }) |> 
    do.call(what = rbind)
}) |> 
  do.call(what = rbind) |> 
  mutate(feat_type = factor(feat_type, levels = c("Passive", "Action", "Reaction")),
         adv_type = factor(adv_type, levels = c("general_use", adv_types))) |> 
  arrange(adv_type, tier, feat_type, feat_name)

# useful for setting up/working with specific adversary's input vals
namify <- function(type, num, detail){paste0(type, "_", num, "_", detail)}
