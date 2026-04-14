# R/03_catalog.R
sets_tbl <- data.frame(
  set_id = c("base", "exp1", "exp2", "exp3", "exp4", "exp5", "exp6"),
  set_en = c(
    "The Worst Takes It All",
    "Wicked to the Core",
    "Evil Comes Prepared",
    "Perfectly Wretched",
    "Despicable Plots",
    "Bigger and Badder",
    "Filled With Fright"
  ),
  set_fr = c(
    "Quel Méchant sommeille en vous ?",
    "Mauvais jusqu'à l'os",
    "La fin est proche",
    "Cruellement infects",
    "Monstrueusement malsains",
    "Plus grands, Plus méchants !",
    "Rempli d'effroi"
  ),
  stringsAsFactors = FALSE
)

villains_tbl <- data.frame(
  villain_id = c(
    "maleficent","jafar","captain_hook","ursula","prince_john","queen_of_hearts",
    "evil_queen","hades","dr_facilier",
    "scar","yzma","ratigan",
    "cruella","mother_gothel","pete",
    "gaston","lady_tremaine","horned_king",
    "syndrome","lotso","madam_mim",
    "oogie_boogie"
  ),
  villain_en = c(
    "Maleficent","Jafar","Captain Hook","Ursula","Prince John","Queen of Hearts",
    "Evil Queen","Hades","Dr. Facilier",
    "Scar","Yzma","Ratigan",
    "Cruella De Vil","Mother Gothel","Pete",
    "Gaston","Lady Tremaine","The Horned King",
    "Syndrome","Lotso","Madam Mim",
    "Oogie Boogie"
  ),
  villain_fr = c(
    "Maléfique","Jafar","Le Capitaine Crochet","Ursula","Le Prince Jean","La Reine de Cœur",
    "La Méchante Reine","Hadès","Dr. Facilier",
    "Scar","Izma","Ratigan",
    "Cruella d'Enfer","Mère Gothel","Pat Hibulaire",
    "Gaston","Madame de Trémaine","Le Seigneur des Ténèbres",
    "Syndrome","Lotso","Madame Mim",
    "Oogie Boogie"
  ),
  set_id = c(
    rep("base", 6),
    rep("exp1", 3),
    rep("exp2", 3),
    rep("exp3", 3),
    rep("exp4", 3),
    rep("exp5", 3),
    "exp6"
  ),
  stringsAsFactors = FALSE
)

label_set <- function(set_id, lang) {
  row <- sets_tbl[sets_tbl$set_id == set_id, , drop = FALSE]
  if (nrow(row) == 0) return(set_id)
  if (lang == "fr") row$set_fr[1] else row$set_en[1]
}

label_villain <- function(villain_id, lang) {
  row <- villains_tbl[villains_tbl$villain_id == villain_id, , drop = FALSE]
  if (nrow(row) == 0) return(villain_id)
  if (lang == "fr") row$villain_fr[1] else row$villain_en[1]
}

# map old stored strings (FR or EN) -> villain_id
name_to_id <- local({
  m <- list()
  for (i in seq_len(nrow(villains_tbl))) {
    m[[villains_tbl$villain_en[i]]] <- villains_tbl$villain_id[i]
    m[[villains_tbl$villain_fr[i]]] <- villains_tbl$villain_id[i]
  }
  unlist(m)
})

villain_to_id <- function(x) {
  x <- trimws(as.character(x))
  out <- name_to_id[x]
  out2 <- ifelse(is.na(out) | out == "", x, out)
  out2
}
