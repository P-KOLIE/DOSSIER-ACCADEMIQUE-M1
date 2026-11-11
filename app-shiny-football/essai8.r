# app.R ---------------------------------------------------------------

library(shiny)
library(tidyverse)
library(bslib)
library(DT)
library(DBI)
library(RSQLite)
library(readr)
library(stringi)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# --------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

norm_names <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

pick_col <- function(nms, patterns) {
  for (p in patterns) {
    hit <- nms[str_detect(nms, p)]
    if (length(hit) > 0) return(hit[[1]])
  }
  NA_character_
}

clean_heure <- function(x) substr(as.character(x), 1, 5)

split_match_teams <- function(match_str) {
  s <- as.character(match_str)
  if (str_detect(s, " - ")) return(str_split(s, " - ", simplify = TRUE)[1, ])
  if (str_detect(s, " vs ")) return(str_split(s, " vs ", simplify = TRUE)[1, ])
  if (str_detect(s, " VS ")) return(str_split(s, " VS ", simplify = TRUE)[1, ])
  c(NA_character_, NA_character_)
}
split_match_teams_vec <- function(x) {
  s <- as.character(x)
  s[is.na(s)] <- ""
  m <- stringr::str_split_fixed(s, "\\s+-\\s+|\\s+vs\\s+|\\s+VS\\s+", 2)
  tibble::tibble(eq1 = stringr::str_trim(m[,1]), eq2 = stringr::str_trim(m[,2]))
}

now_txt <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# Normalisation phases CAN (depuis "Phase ou Groupes")
norm_can_phase <- function(x) {
  x <- str_to_lower(str_trim(x))
  case_when(
    str_detect(x, "huit") ~ "Huitièmes",
    str_detect(x, "quart") ~ "Quarts",
    str_detect(x, "demi") ~ "Demi-finales",
    str_detect(x, "3") | str_detect(x, "troisi") ~ "3e place",
    str_detect(x, "final") ~ "Finale",
    TRUE ~ str_trim(x)
  )
}

# Parse "Résultat (ex: 1-1 (3-2 TAB))" -> score_1/score_2 + tab_1/tab_2
parse_result <- function(x) {
  s <- str_trim(as.character(x))
  if (is.na(s) || s == "") {
    return(list(score_1 = NA_integer_, score_2 = NA_integer_, tab_1 = NA_integer_, tab_2 = NA_integer_))
  }
  score_1 <- score_2 <- tab_1 <- tab_2 <- NA_integer_
  
  m <- str_match(s, "^(\\d+)\\s*-\\s*(\\d+)")
  if (!is.na(m[1, 2])) {
    score_1 <- suppressWarnings(as.integer(m[1, 2]))
    score_2 <- suppressWarnings(as.integer(m[1, 3]))
  }
  
  mt <- str_match(s, "\\((\\d+)\\s*-\\s*(\\d+)\\s*TAB\\)")
  if (!is.na(mt[1, 2])) {
    tab_1 <- suppressWarnings(as.integer(mt[1, 2]))
    tab_2 <- suppressWarnings(as.integer(mt[1, 3]))
  }
  
  list(score_1 = score_1, score_2 = score_2, tab_1 = tab_1, tab_2 = tab_2)
}

score_with_tab <- function(score_1, score_2, tab_1, tab_2) {
  score_txt <- ifelse(!is.na(score_1) & !is.na(score_2),
                      paste0(score_1, "-", score_2), "")
  tab_txt   <- ifelse(!is.na(tab_1) & !is.na(tab_2),
                      paste0(" (", tab_1, "-", tab_2, " TAB)"), "")
  ifelse(score_txt == "", "", paste0(score_txt, tab_txt))
}

read_ldc_csv <- function(path) {
  raw <- readr::read_delim(
    file = path,
    delim = ";",
    show_col_types = FALSE,
    name_repair = "unique_quiet"
  )
  
  # ✅ re-réparation (tibble) au cas où
  raw <- tibble::as_tibble(raw, .name_repair = "unique")
  
  # Nettoyage + normalisation
  nm <- stringr::str_trim(names(raw))
  nm[nm == "" | is.na(nm)] <- paste0("col_", which(nm == "" | is.na(nm)))
  
  nm2 <- norm_names(nm)
  nm2[nm2 == "" | is.na(nm2)] <- paste0("col_", which(nm2 == "" | is.na(nm2)))
  
  # ✅ rendre unique après norm_names (hyper important)
  nm2 <- make.unique(nm2, sep = "_")
  names(raw) <- nm2
  
  # ✅ garde-fou : stoppe si malgré tout il y a des doublons
  if (anyDuplicated(names(raw)) > 0) {
    dup <- unique(names(raw)[duplicated(names(raw))])
    stop("Duplicate names after repair: ", paste(dup, collapse = ", "))
  }
  
  raw
}

is_played_match <- function(mm) {
  !is.null(mm$score_1) && !is.na(mm$score_1) &&
    !is.null(mm$score_2) && !is.na(mm$score_2)
}

# --------------------------------------------------------------------
# Files
# --------------------------------------------------------------------
# Amis
csv_users   <- "amis.csv"
# CDM
csv_groupes <- "Phase_de_groupes-Tableau 1.csv"
csv_finales <- "Phases_finales-Tableau 1.csv"
# CAN
csv_can_groupes <- "Poule-Tableau 1.csv"
csv_can_finales <- "Finales-Tableau 1.csv"
# LDC
csv_ldc_barrages_qualif <- "Barrages_qualif-Tableau 1.csv"
csv_ldc_j1 <- "J1-Tableau 1.csv"
csv_ldc_j2 <- "J2-Tableau 1.csv"
csv_ldc_j3 <- "J3-Tableau 1.csv"
csv_ldc_j4 <- "J4-Tableau 1.csv"
csv_ldc_j5 <- "J5-Tableau 1.csv"
csv_ldc_j6 <- "J6-Tableau 1.csv"
csv_ldc_j7 <- "J7-Tableau 1.csv"
csv_ldc_j8 <- "J8-Tableau 1.csv"
csv_ldc_barrages <- "Barrages-Tableau 1.csv"
csv_ldc_huitiemes <- "Huitiemes-Tableau 1.csv"
csv_ldc_quarts <- "Quarts-Tableau 1.csv"
csv_ldc_demi <- "Demi-Tableau 1.csv"
csv_ldc_finale <- "Finale-Tableau 1.csv"
# --------------------------------------------------------------------
# DB
# --------------------------------------------------------------------
db_path <- "app.sqlite"
con <- dbConnect(RSQLite::SQLite(), db_path)

# Users
dbExecute(con, "
CREATE TABLE IF NOT EXISTS users (
  id TEXT PRIMARY KEY,
  nom TEXT NOT NULL,
  prenom TEXT NOT NULL,
  pseudo TEXT,
  email TEXT,
  phone TEXT,
  mdp TEXT,
  emoji TEXT
);
")
dbExecute(con, "CREATE UNIQUE INDEX IF NOT EXISTS idx_users_pseudo_unique ON users(pseudo)")

# Wallet (par user)
dbExecute(con, "
CREATE TABLE IF NOT EXISTS wallets (
  user_id TEXT PRIMARY KEY,
  solde REAL NOT NULL DEFAULT 0,
  gains_total REAL NOT NULL DEFAULT 0
);
")

# Friend requests + friendships
dbExecute(con, "
CREATE TABLE IF NOT EXISTS friend_requests (
  fr_id INTEGER PRIMARY KEY AUTOINCREMENT,
  from_id TEXT NOT NULL,
  to_id TEXT NOT NULL,
  status TEXT NOT NULL,      -- pending / accepted / refused
  created_at TEXT NOT NULL
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS friendships (
  user1 TEXT NOT NULL,
  user2 TEXT NOT NULL,
  created_at TEXT NOT NULL,
  PRIMARY KEY (user1, user2)
);
")

# Matches
dbExecute(con, "
CREATE TABLE IF NOT EXISTS matches (
  match_id INTEGER PRIMARY KEY,
  competition TEXT NOT NULL,
  phase TEXT,
  groupe TEXT,
  journee INTEGER,
  date_txt TEXT,
  heure_txt TEXT,
  equipe_1 TEXT NOT NULL,
  equipe_2 TEXT NOT NULL,
  remarque TEXT
);
")

# Bet requests
dbExecute(con, "
CREATE TABLE IF NOT EXISTS requests (
  req_id INTEGER PRIMARY KEY AUTOINCREMENT,
  from_id TEXT NOT NULL,
  to_id TEXT NOT NULL,
  competition TEXT NOT NULL,
  match TEXT NOT NULL,
  choix_from TEXT NOT NULL,
  amount REAL NOT NULL,
  status TEXT NOT NULL,      -- pending / accepted / refused / countered
  created_at TEXT NOT NULL,
  ref_req_id INTEGER
);
")

# CAN (colonnes en plus dans matches)
try(dbExecute(con, "ALTER TABLE matches ADD COLUMN score_1 INTEGER"), silent = TRUE)
try(dbExecute(con, "ALTER TABLE matches ADD COLUMN score_2 INTEGER"), silent = TRUE)
try(dbExecute(con, "ALTER TABLE matches ADD COLUMN tab_1 INTEGER"), silent = TRUE)
try(dbExecute(con, "ALTER TABLE matches ADD COLUMN tab_2 INTEGER"), silent = TRUE)
try(dbExecute(con, "ALTER TABLE matches ADD COLUMN winner TEXT"), silent = TRUE)
try(dbExecute(con, "ALTER TABLE matches ADD COLUMN qualif_txt TEXT"), silent = TRUE)

# LDC (colonnes en plus dans matches) - IMPORTANT : avant import et avant affichage
try(dbExecute(con, "ALTER TABLE matches ADD COLUMN pays_1 TEXT"), silent = TRUE)
try(dbExecute(con, "ALTER TABLE matches ADD COLUMN pays_2 TEXT"), silent = TRUE)
try(dbExecute(con, "ALTER TABLE matches ADD COLUMN leg_txt TEXT"), silent = TRUE)          # Aller/Retour
try(dbExecute(con, "ALTER TABLE matches ADD COLUMN pts_cum_1 INTEGER"), silent = TRUE)
try(dbExecute(con, "ALTER TABLE matches ADD COLUMN pts_cum_2 INTEGER"), silent = TRUE)

# ✅ J8 : 2 colonnes qualifié
try(dbExecute(con, "ALTER TABLE matches ADD COLUMN qualif_eq1 TEXT"), silent = TRUE)
try(dbExecute(con, "ALTER TABLE matches ADD COLUMN qualif_eq2 TEXT"), silent = TRUE)


# --------------------------------------------------------------------
# Import users once (IDs padding 001)
# --------------------------------------------------------------------
import_users_once <- function() {
  if (!file.exists(csv_users)) return(invisible(NULL))
  n <- dbGetQuery(con, "SELECT COUNT(*) n FROM users")$n
  if (n > 0) return(invisible(NULL))
  
  raw <- readr::read_delim(csv_users, delim = ";", show_col_types = FALSE, trim_ws = TRUE)
  names(raw) <- str_trim(names(raw))
  
  raw <- raw |>
    mutate(
      Id = str_pad(as.character(Id), 3, pad = "0"),
      Nom = str_trim(Nom),
      `Prénom` = str_trim(`Prénom`),
      Pseudo = if_else(is.na(Pseudo) | str_trim(Pseudo) == "", NA_character_, str_trim(Pseudo))
    )
  
  users <- raw |>
    transmute(
      id = Id,
      nom = Nom,
      prenom = `Prénom`,
      pseudo = Pseudo,
      email = NA_character_,
      phone = NA_character_,
      mdp = NA_character_,
      emoji = NA_character_
    )
  
  dbWriteTable(con, "users", users, append = TRUE)
}
import_users_once()

# init wallets (si manquants)
dbExecute(con, "
INSERT INTO wallets (user_id, solde, gains_total)
SELECT u.id, 75, 120
FROM users u
LEFT JOIN wallets w ON w.user_id = u.id
WHERE w.user_id IS NULL
")

# --------------------------------------------------------------------
# Import matches
# --------------------------------------------------------------------
ID_BASE_CDM <- 100000
ID_BASE_CAN <- 200000

DEV_MODE_MATCHES <- TRUE

# CDM
import_matches_cdm <- function() {
  if (DEV_MODE_MATCHES) {
    dbExecute(con, "DELETE FROM matches WHERE competition='CDM'")
  } else {
    n <- dbGetQuery(con, "SELECT COUNT(*) n FROM matches WHERE competition='CDM'")$n
    if (n > 0) return(invisible(NULL))
  }
  
  # Groupes
  if (file.exists(csv_groupes)) {
    raw <- readr::read_delim(csv_groupes, delim = ";", show_col_types = FALSE)
    nms <- norm_names(names(raw)); names(raw) <- nms
    
    c_match_id <- pick_col(nms, c("^match_id$", "match", "id_match"))
    c_groupe   <- pick_col(nms, c("^groupe$", "group"))
    c_journee  <- pick_col(nms, c("journee", "day"))
    c_date     <- pick_col(nms, c("^date$", "date_"))
    c_heure    <- pick_col(nms, c("^heure", "time"))
    c_eq1      <- pick_col(nms, c("equipe_1", "team_1", "equipe1", "pays_1", "home"))
    c_eq2      <- pick_col(nms, c("equipe_2", "team_2", "equipe2", "pays_2", "away"))
    
    df_g <- raw |>
      transmute(
        match_id = if (!is.na(c_match_id)) ID_BASE_CDM + as.integer(.data[[c_match_id]]) else ID_BASE_CDM + row_number(),
        competition = "CDM",
        phase       = "Phase de groupes",
        groupe      = if (!is.na(c_groupe)) as.character(.data[[c_groupe]]) else NA_character_,
        journee     = if (!is.na(c_journee)) suppressWarnings(as.integer(.data[[c_journee]])) else NA_integer_,
        date_txt    = if (!is.na(c_date)) as.character(.data[[c_date]]) else NA_character_,
        heure_txt   = if (!is.na(c_heure)) clean_heure(.data[[c_heure]]) else NA_character_,
        equipe_1    = if (!is.na(c_eq1)) as.character(.data[[c_eq1]]) else NA_character_,
        equipe_2    = if (!is.na(c_eq2)) as.character(.data[[c_eq2]]) else NA_character_,
        remarque    = NA_character_
      ) |>
      mutate(across(where(is.character), str_trim), groupe = toupper(groupe))
    
    dbWriteTable(con, "matches", df_g, append = TRUE)
  }
  
  # Finales
  if (file.exists(csv_finales)) {
    raw <- readr::read_delim(csv_finales, delim = ";", show_col_types = FALSE)
    nms <- norm_names(names(raw)); names(raw) <- nms
    
    c_match_id <- pick_col(nms, c("^match_id$", "match", "id_match"))
    c_phase    <- pick_col(nms, c("^phase$", "tour", "round"))
    c_date     <- pick_col(nms, c("^date$", "date_"))
    c_heure    <- pick_col(nms, c("^heure", "time"))
    c_eq1      <- pick_col(nms, c("equipe_1", "team_1", "equipe1", "pays_1", "home"))
    c_eq2      <- pick_col(nms, c("equipe_2", "team_2", "equipe2", "pays_2", "away"))
    c_rem      <- pick_col(nms, c("remarque", "note", "infos"))
    
    df_f <- raw |>
      transmute(
        match_id = if (!is.na(c_match_id)) ID_BASE_CDM + as.integer(.data[[c_match_id]]) else ID_BASE_CDM + 10000 + row_number(),
        competition = "CDM",
        phase       = if (!is.na(c_phase)) as.character(.data[[c_phase]]) else "Phases finales",
        groupe      = NA_character_,
        journee     = NA_integer_,
        date_txt    = if (!is.na(c_date)) as.character(.data[[c_date]]) else NA_character_,
        heure_txt   = if (!is.na(c_heure)) clean_heure(.data[[c_heure]]) else NA_character_,
        equipe_1    = if (!is.na(c_eq1)) as.character(.data[[c_eq1]]) else NA_character_,
        equipe_2    = if (!is.na(c_eq2)) as.character(.data[[c_eq2]]) else NA_character_,
        remarque    = if (!is.na(c_rem)) as.character(.data[[c_rem]]) else NA_character_
      ) |>
      mutate(across(where(is.character), str_trim))
    
    dbWriteTable(con, "matches", df_f, append = TRUE)
  }
}
import_matches_cdm()

# CAN
import_matches_can <- function() {
  dbExecute(con, "DELETE FROM matches WHERE competition='CAN'")
  
  # ------------------ POULES ------------------
  if (file.exists(csv_can_groupes)) {
    raw <- readr::read_delim(csv_can_groupes, delim = ";", show_col_types = FALSE)
    names(raw) <- str_trim(names(raw))
    nms <- norm_names(names(raw)); names(raw) <- nms
    c_match_id <- pick_col(nms, c("^match_id$", "^match$", "^id_match$"))
    
    c_pg     <- pick_col(nms, c("^phase_ou_groupes$", "^phase$", "^groupe$", "^groupes$"))
    c_date   <- pick_col(nms, c("^date$"))
    c_heure  <- pick_col(nms, c("^heure", "heure_heure_francaise"))
    c_eq1    <- pick_col(nms, c("^equipe_1$", "^equipe1$", "^pays_1$"))
    c_eq2    <- pick_col(nms, c("^equipe_2$", "^equipe2$", "^pays_2$"))
    c_res    <- pick_col(nms, c("^resultat", "^resultat_ex"))
    c_win    <- pick_col(nms, c("^gagnant$"))
    c_qualif <- pick_col(nms, c("^qualifie$"))
    
    df <- raw |>
      transmute(
        match_id = if (!is.na(c_match_id)) 
          ID_BASE_CAN + suppressWarnings(as.integer(.data[[c_match_id]])) 
        else 
          ID_BASE_CAN + row_number(),
        competition = "CAN",
        phase       = "Phase de groupes",
        groupe      = if (!is.na(c_pg)) as.character(.data[[c_pg]]) else NA_character_,
        journee     = NA_integer_,
        date_txt    = if (!is.na(c_date)) as.character(.data[[c_date]]) else NA_character_,
        heure_txt   = if (!is.na(c_heure)) clean_heure(.data[[c_heure]]) else NA_character_,
        equipe_1    = if (!is.na(c_eq1)) as.character(.data[[c_eq1]]) else NA_character_,
        equipe_2    = if (!is.na(c_eq2)) as.character(.data[[c_eq2]]) else NA_character_,
        remarque    = NA_character_,
        resultat_raw = if (!is.na(c_res)) as.character(.data[[c_res]]) else NA_character_,
        winner      = if (!is.na(c_win)) as.character(.data[[c_win]]) else NA_character_,
        qualif_txt  = if (!is.na(c_qualif)) as.character(.data[[c_qualif]]) else NA_character_
      ) |>
      mutate(across(where(is.character), str_trim))
    
    # groupe: "Groupe A" / "GROUPE A" -> "GROUPE A"
    df <- df |>
      mutate(
        groupe = toupper(groupe),
        groupe = str_replace(groupe, "\\s+", " "),
        groupe = str_replace(groupe, "^GROUPE\\s+", "GROUPE ")
      )
    
    # parse score/tab
    df <- df |>
      mutate(.pr = purrr::map(resultat_raw, parse_result),
             score_1 = purrr::map_int(.pr, "score_1"),
             score_2 = purrr::map_int(.pr, "score_2"),
             tab_1   = purrr::map_int(.pr, "tab_1"),
             tab_2   = purrr::map_int(.pr, "tab_2")) |>
      select(-.pr, -resultat_raw)
    
    # winner fallback si manquant
    df <- df |>
      mutate(
        winner = ifelse(is.na(winner) | winner == "",
                        case_when(
                          !is.na(score_1) & !is.na(score_2) & score_1 > score_2 ~ equipe_1,
                          !is.na(score_1) & !is.na(score_2) & score_2 > score_1 ~ equipe_2,
                          !is.na(tab_1)   & !is.na(tab_2)   & tab_1 > tab_2     ~ equipe_1,
                          !is.na(tab_1)   & !is.na(tab_2)   & tab_2 > tab_1     ~ equipe_2,
                          !is.na(score_1) & !is.na(score_2) & score_1 == score_2 ~ "Match nul",
                          TRUE ~ NA_character_
                        ),
                        winner)
      )
    
    dbWriteTable(con, "matches", df, append = TRUE)
  }
  
  # ------------------ FINALES ------------------
  if (file.exists(csv_can_finales)) {
    raw <- readr::read_delim(csv_can_finales, delim = ";", show_col_types = FALSE)
    names(raw) <- str_trim(names(raw))
    nms <- norm_names(names(raw)); names(raw) <- nms
    c_match_id <- pick_col(nms, c("^match_id$", "^match$", "^id_match$"))
    
    c_pg    <- pick_col(nms, c("^phase_ou_groupes$", "^phase$", "^tour$"))
    c_date  <- pick_col(nms, c("^date$"))
    c_heure <- pick_col(nms, c("^heure", "heure_heure_francaise"))
    c_eq1   <- pick_col(nms, c("^equipe_1$", "^equipe1$", "^pays_1$"))
    c_eq2   <- pick_col(nms, c("^equipe_2$", "^equipe2$", "^pays_2$"))
    c_res   <- pick_col(nms, c("^resultat", "^resultat_ex"))
    c_win   <- pick_col(nms, c("^gagnant$"))
    
    df <- raw |>
      transmute(
        match_id = if (!is.na(c_match_id)) 
          ID_BASE_CAN + suppressWarnings(as.integer(.data[[c_match_id]])) 
        else 
          ID_BASE_CAN + 10000 + row_number(),
        competition = "CAN",
        phase_raw   = if (!is.na(c_pg)) as.character(.data[[c_pg]]) else NA_character_,
        groupe      = NA_character_,
        journee     = NA_integer_,
        date_txt    = if (!is.na(c_date)) as.character(.data[[c_date]]) else NA_character_,
        heure_txt   = if (!is.na(c_heure)) clean_heure(.data[[c_heure]]) else NA_character_,
        equipe_1    = if (!is.na(c_eq1)) as.character(.data[[c_eq1]]) else NA_character_,
        equipe_2    = if (!is.na(c_eq2)) as.character(.data[[c_eq2]]) else NA_character_,
        remarque    = NA_character_,
        resultat_raw = if (!is.na(c_res)) as.character(.data[[c_res]]) else NA_character_,
        winner      = if (!is.na(c_win)) as.character(.data[[c_win]]) else NA_character_
      ) |>
      mutate(across(where(is.character), str_trim))
    
    df <- df |>
      mutate(
        phase = norm_can_phase(phase_raw)
      )
    
    # parse score/tab
    df <- df |>
      mutate(.pr = purrr::map(resultat_raw, parse_result),
             score_1 = purrr::map_int(.pr, "score_1"),
             score_2 = purrr::map_int(.pr, "score_2"),
             tab_1   = purrr::map_int(.pr, "tab_1"),
             tab_2   = purrr::map_int(.pr, "tab_2")) |>
      select(-.pr, -resultat_raw, -phase_raw)
    
    # winner fallback si manquant
    df <- df |>
      mutate(
        winner = ifelse(is.na(winner) | winner == "",
                        case_when(
                          !is.na(score_1) & !is.na(score_2) & score_1 > score_2 ~ equipe_1,
                          !is.na(score_1) & !is.na(score_2) & score_2 > score_1 ~ equipe_2,
                          !is.na(tab_1)   & !is.na(tab_2)   & tab_1 > tab_2     ~ equipe_1,
                          !is.na(tab_1)   & !is.na(tab_2)   & tab_2 > tab_1     ~ equipe_2,
                          TRUE ~ NA_character_
                        ),
                        winner),
        # en finales, le gagnant = qualifié (au moins “passe le tour”)
        qualif_txt = ifelse(is.na(winner) | winner == "", NA_character_, winner)
      )
    
    dbWriteTable(con, "matches", df, append = TRUE)
  }
}
import_matches_can()

#LDC
ID_BASE_LDC <- 300000

norm_ldc_phase <- function(x) {
  x <- str_to_lower(str_trim(as.character(x)))
  case_when(
    str_detect(x, "barrages de qualification") ~ "Barrages de qualification",
    str_detect(x, "^barrage") ~ "Barrages",
    str_detect(x, "huit") ~ "Huitièmes",
    str_detect(x, "quart") ~ "Quarts",
    str_detect(x, "demi") ~ "Demi-finales",
    str_detect(x, "finale") ~ "Finale",
    str_detect(x, "journee") ~ str_to_title(str_replace_all(x, "\\s+", " ")), # "Journée 1"
    TRUE ~ str_to_title(str_replace_all(x, "\\s+", " "))
  )
}

import_one_ldc_file <- function(path,
                                phase_override = NA_character_,
                                id_offset = 0,
                                force_journee = NA_integer_) {
  
  # --- guard ---
  if (is.null(path) || is.na(path) || path == "" || !file.exists(path)) {
    return(invisible(NULL))
  }
  
  # --- J8 flag (défini tout de suite -> dispo partout) ---
  is_j8 <- !is.na(force_journee) && force_journee == 8
  
  # --- read + names ---
  raw <- read_ldc_csv(path)
  nms <- names(raw)
  
  # --- pick_col ---
  c_phase  <- pick_col(nms, c("^phase_ou_groupes$", "^phase$"))
  c_leg    <- pick_col(nms, c("aller_/_retour", "aller_retour", "^aller", "retour"))
  c_date   <- pick_col(nms, c("^date$"))
  c_heure  <- pick_col(nms, c("^heure", "heure_heure_francaise"))
  
  c_eq1    <- pick_col(nms, c("^equipe_1$", "^equipe1$", "^equipe 1$", "equipe_1"))
  c_pays1  <- pick_col(nms, c("^pays_equipe_1$", "pays_equipe_1", "pays_eq1"))
  c_eq2    <- pick_col(nms, c("^equipe_2$", "^equipe2$", "^equipe 2$", "equipe_2"))
  c_pays2  <- pick_col(nms, c("^pays_equipe_2$", "pays_equipe_2", "pays_eq2"))
  
  c_res    <- pick_col(nms, c("^resultat", "resultat_ex"))
  c_win    <- pick_col(nms, c("^gagnant$"))
  c_qualif <- pick_col(nms, c("^qualifie$", "^qualifiee$", "^qualifie_eq", "^statut_final$", "statut_final"))
  
  c_pts1   <- pick_col(nms, c("pts_cumules_equipe_1", "pts_cumules_eq1", "pts_cumules_equipe1"))
  c_pts2   <- pick_col(nms, c("pts_cumules_equipe_2", "pts_cumules_eq2", "pts_cumules_equipe2"))
  
  # ✅ colonnes J8 : on prend en priorité statut_final_eq1/eq2 si présentes
  c_stat1  <- pick_col(nms, c("^statut_final_eq1$", "statut_final_eq1", "^statut_final_eq_?1$"))
  c_stat2  <- pick_col(nms, c("^statut_final_eq2$", "statut_final_eq2", "^statut_final_eq_?2$"))
  
  # si ton read_ldc_csv normalise déjà, normalement elles sont bien "statut_final_eq1/eq2"
  if (is_j8) {
    if ("statut_final_eq1" %in% nms) c_stat1 <- "statut_final_eq1"
    if ("statut_final_eq2" %in% nms) c_stat2 <- "statut_final_eq2"
  }
  
  # --- split match si besoin (si tu as split_match_teams_vec) ---
  c_match <- pick_col(nms, c("^match$", "^rencontre$", "^affiche$", "opposition"))
  teams <- NULL
  if ((is.na(c_eq1) || is.na(c_eq2)) && !is.na(c_match)) {
    # split_match_teams_vec doit renvoyer un df avec $eq1 $eq2 (vecteurs)
    teams <- split_match_teams_vec(raw[[c_match]])
  }
  
  # --- build df ---
  df <- raw |>
    dplyr::transmute(
      match_id     = ID_BASE_LDC + id_offset + dplyr::row_number(),
      competition  = "LDC",
      
      phase_raw    = if (!is.na(c_phase)) as.character(.data[[c_phase]]) else NA_character_,
      leg_txt      = if (!is.na(c_leg))   as.character(.data[[c_leg]])   else NA_character_,
      
      groupe       = NA_character_,
      journee      = if (!is.na(force_journee)) force_journee else NA_integer_,
      
      date_txt     = if (!is.na(c_date))  as.character(.data[[c_date]])  else NA_character_,
      heure_txt    = if (!is.na(c_heure)) clean_heure(.data[[c_heure]])  else NA_character_,
      
      pays_1       = if (!is.na(c_pays1)) as.character(.data[[c_pays1]]) else NA_character_,
      equipe_1     = if (!is.na(c_eq1))   as.character(.data[[c_eq1]])   else if (!is.null(teams)) teams$eq1 else NA_character_,
      
      pays_2       = if (!is.na(c_pays2)) as.character(.data[[c_pays2]]) else NA_character_,
      equipe_2     = if (!is.na(c_eq2))   as.character(.data[[c_eq2]])   else if (!is.null(teams)) teams$eq2 else NA_character_,
      
      resultat_raw = if (!is.na(c_res))   as.character(.data[[c_res]])   else NA_character_,
      winner       = if (!is.na(c_win))   as.character(.data[[c_win]])   else NA_character_,
      qualif_txt   = if (!is.na(c_qualif))as.character(.data[[c_qualif]])else NA_character_,
      
      pts_cum_1    = if (!is.na(c_pts1)) suppressWarnings(as.integer(.data[[c_pts1]])) else NA_integer_,
      pts_cum_2    = if (!is.na(c_pts2)) suppressWarnings(as.integer(.data[[c_pts2]])) else NA_integer_,
      
      # ✅ J8: on lit bien les colonnes statut_final_eq1/eq2, sinon NA
      qualif_eq1   = if (!is.na(c_stat1)) as.character(.data[[c_stat1]]) else NA_character_,
      qualif_eq2   = if (!is.na(c_stat2)) as.character(.data[[c_stat2]]) else NA_character_,
      
      remarque     = NA_character_
    ) |>
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim))
  
  # --- phase ---
  df <- df |>
    dplyr::mutate(
      phase = dplyr::case_when(
        !is.na(phase_override) & phase_override != "" ~ phase_override,
        TRUE ~ norm_ldc_phase(phase_raw)
      )
    )
  
  # --- parse score/tab ---
  df <- df |>
    dplyr::mutate(
      .pr    = purrr::map(resultat_raw, parse_result),
      score_1 = purrr::map_int(.pr, "score_1"),
      score_2 = purrr::map_int(.pr, "score_2"),
      tab_1   = purrr::map_int(.pr, "tab_1"),
      tab_2   = purrr::map_int(.pr, "tab_2")
    ) |>
    dplyr::select(-.pr, -resultat_raw, -phase_raw)
  
  # --- nettoyer equipes vides ---
  df <- df |>
    dplyr::mutate(
      equipe_1 = stringr::str_trim(as.character(equipe_1)),
      equipe_2 = stringr::str_trim(as.character(equipe_2))
    ) |>
    dplyr::filter(
      !is.na(equipe_1) & equipe_1 != "",
      !is.na(equipe_2) & equipe_2 != ""
    )
  
  # --- J8 uniquement : qualif_eq1 / qualif_eq2 ---
  if (is_j8) {
    df <- df |>
      dplyr::mutate(
        qualif_eq1 = stringr::str_trim(as.character(qualif_eq1)),
        qualif_eq2 = stringr::str_trim(as.character(qualif_eq2))
      )
  } else {
    df <- df |>
      dplyr::mutate(
        qualif_eq1 = NA_character_,
        qualif_eq2 = NA_character_
      )
  }
  
  return(df)
}

import_matches_ldc <- function() {
  # En DEV : on écrase
  if (DEV_MODE_MATCHES) {
    dbExecute(con, "DELETE FROM matches WHERE competition='LDC'")
  } else {
    n <- dbGetQuery(con, "SELECT COUNT(*) n FROM matches WHERE competition='LDC'")$n
    if (n > 0) return(invisible(NULL))
  }
  
  # Barrages de qualification
  df <- import_one_ldc_file(csv_ldc_barrages_qualif, phase_override = "Barrages de qualification", id_offset = 0)
  if (!is.null(df)) dbWriteTable(con, "matches", df, append = TRUE)
  
  # Journées J1..J8
  j_paths <- list(csv_ldc_j1, csv_ldc_j2, csv_ldc_j3, csv_ldc_j4, csv_ldc_j5, csv_ldc_j6, csv_ldc_j7, csv_ldc_j8)
  for (j in 1:8) {
    dfj <- import_one_ldc_file(j_paths[[j]], phase_override = paste0("Journée ", j), id_offset = 1000 + j*2000, force_journee = j)
    if (!is.null(dfj)) dbWriteTable(con, "matches", dfj, append = TRUE)
  }
  
  # Barrages (playoffs)
  dfb <- import_one_ldc_file(csv_ldc_barrages, phase_override = "Barrages", id_offset = 25000)
  if (!is.null(dfb)) dbWriteTable(con, "matches", dfb, append = TRUE)
  
  # Phases finales
  df8 <- import_one_ldc_file(csv_ldc_huitiemes, phase_override = "Huitièmes", id_offset = 40000)
  if (!is.null(df8)) dbWriteTable(con, "matches", df8, append = TRUE)
  
  dfq <- import_one_ldc_file(csv_ldc_quarts, phase_override = "Quarts", id_offset = 50000)
  if (!is.null(dfq)) dbWriteTable(con, "matches", dfq, append = TRUE)
  
  dfd <- import_one_ldc_file(csv_ldc_demi, phase_override = "Demi-finales", id_offset = 60000)
  if (!is.null(dfd)) dbWriteTable(con, "matches", dfd, append = TRUE)
  
  dff <- import_one_ldc_file(csv_ldc_finale, phase_override = "Finale", id_offset = 70000)
  if (!is.null(dff)) dbWriteTable(con, "matches", dff, append = TRUE)
}

import_matches_ldc()
# --------------------------------------------------------------------
# UI
# --------------------------------------------------------------------
app_theme <- bs_theme(bootswatch = "vapor")
ui <- page_fillable(
  theme = app_theme,
  
  tags$style(HTML("
    .selectize-dropdown-content {
      max-height: 200px !important;
      overflow-y: auto;
    }

    /* Remplace visuellement 'Dismiss' par 'Fermer' */
    .modal-footer .btn.btn-default {
      font-size: 0 !important;
    }
    .modal-footer .btn.btn-default:after {
      content: 'Fermer' !important;
      font-size: 16px !important;
    }
  ")),
  
  tags$script(HTML("
    // Quand un modal se ferme (bootstrap), on envoie un signal à Shiny
    $(document).on('hidden.bs.modal', '.modal', function () {
      Shiny.setInputValue('modal_closed', Date.now(), {priority: 'event'});
    });

    // Handler : nettoyage des overlays qui peuvent bloquer les clics sur Leaflet
    Shiny.addCustomMessageHandler('forceRemoveModalBackdrop', function(x) {
      // Backdrop bootstrap + wrapper shiny
      $('.modal-backdrop').remove();
      $('#shiny-modal-wrapper').remove();

      // Reset body bootstrap
      $('body').removeClass('modal-open');
      $('body').css('padding-right', '');

      // Sécurité : overlays DataTables (rare mais ça arrive)
      $('.dt-button-background').remove();
      $('.dt-button-collection').remove();
  ")),
  
  uiOutput("app_ui")
)

# --------------------------------------------------------------------
# SERVER
# --------------------------------------------------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    map_tick = 0,
    me_id = NULL,
    selected_match = NULL,
    wallet_tick = 0,
    refresh_tick = 0,
    
    last_compet_tab = "CDM",   # CDM / CAN
    last_cdm_tab    = "groups",# groups / finals
    last_can_tab    = "groups",
    
    last_cdm_group  = NULL,    # ex: "groupe_a" (norm_names)
    last_cdm_phase  = NULL,    # ex: "huitiemes" (norm_names)
    last_can_group  = NULL,
    last_can_phase  = NULL,
    
    last_ldc_tab   = "bq",
    last_ldc_j     = "j1",
    last_ldc_pf    = "huitiemes"
  )
  
  bump_refresh <- function() {
    rv$refresh_tick <- rv$refresh_tick + 1
  }
  
  # ------------------ DATA HELPERS ------------------
  users_tbl <- reactive({
    dbGetQuery(con, "
      SELECT id, nom, prenom,
             COALESCE(pseudo,'') pseudo,
             COALESCE(emoji,'') emoji
      FROM users
      ORDER BY nom, prenom
    ")
  })
  
  user_label <- function(df) {
    paste0(df$id, " - ", df$nom, " ", df$prenom,
           ifelse(df$pseudo != "", paste0(" (@", df$pseudo, ")"), ""))
  }
  
  me <- reactive({
    req(rv$me_id)
    dbGetQuery(con, "SELECT * FROM users WHERE id = ?", params = list(rv$me_id))
  })
  
  wallet <- reactive({
    req(rv$me_id)
    rv$wallet_tick
    dbGetQuery(con, "SELECT solde, gains_total FROM wallets WHERE user_id = ?", params = list(rv$me_id))
  })
  
  friends_tbl <- reactive({
    req(rv$me_id)
    rv$refresh_tick
    df <- dbGetQuery(con, "
      SELECT CASE WHEN user1 = ? THEN user2 ELSE user1 END AS friend_id
      FROM friendships
      WHERE user1 = ? OR user2 = ?
    ", params = list(rv$me_id, rv$me_id, rv$me_id))
    
    if (nrow(df) == 0) return(tibble(id=character(), nom=character(), prenom=character(), pseudo=character()))
    ids <- df$friend_id
    q <- paste(rep("?", length(ids)), collapse = ",")
    dbGetQuery(con, sprintf("
      SELECT id, nom, prenom, COALESCE(pseudo,'') pseudo
      FROM users
      WHERE id IN (%s)
      ORDER BY nom, prenom
    ", q), params = as.list(ids))
  })
  
  # ------------------ IMPORTANT: requests reactives ------------------
  received_requests <- reactive({
    req(rv$me_id)
    rv$refresh_tick
    dbGetQuery(con, "
      SELECT req_id, competition, match, from_id, choix_from, amount
      FROM requests
      WHERE to_id=? AND status='pending'
      ORDER BY req_id DESC
    ", params=list(rv$me_id))
  })
  
  received_count <- reactive({
    req(rv$me_id)
    rv$refresh_tick
    
    dbGetQuery(con, "
    SELECT COUNT(*) n
    FROM requests
    WHERE to_id = ?
      AND status = 'pending'
  ", params = list(rv$me_id))$n
  })
  
  # ------------------ FRIEND INBOX ------------------
  friend_inbox <- reactive({
    req(rv$me_id)
    rv$refresh_tick
    dbGetQuery(con, "
      SELECT fr_id, from_id, created_at
      FROM friend_requests
      WHERE to_id=? AND status='pending'
      ORDER BY fr_id DESC
    ", params=list(rv$me_id))
  })
  
  inbox_count <- reactive({
    req(rv$me_id)
    rv$refresh_tick
    dbGetQuery(con, "
      SELECT COUNT(*) n
      FROM friend_requests
      WHERE to_id=? AND status='pending'
    ", params = list(rv$me_id))$n
  })
  bets_inbox_count <- reactive({
    req(rv$me_id)
    rv$refresh_tick
    dbGetQuery(con, "
    SELECT COUNT(*) n
    FROM requests
    WHERE to_id=? AND status='pending'
  ", params = list(rv$me_id))$n
  })
  
  # ------------------ APP UI (login puis app) ------------------
  output$app_ui <- renderUI({
    if (is.null(rv$me_id)) {
      df <- users_tbl()
      choices <- setNames(df$id, user_label(df))
      
      div(style="max-width:760px; margin:40px auto;",
          card(
            card_header("Connexion"),
            p("Choisis ton profil"),
            selectizeInput(
              "login_user",
              "Profil",
              choices = choices,
              selected = NULL,
              options = list(
                placeholder = "Tape un nom, un pseudo ou un ID",
                maxOptions = 300,
                maxItems = 1,
                dropdownParent = "body"
              )
            ),
            actionButton("btn_login", "Entrer", class="btn btn-primary")
          )
      )
    } else {
      page_sidebar(
        title = NULL,
        sidebar = sidebar(
          width = 320,
          navset_pill_list(
            id = "tab",
            nav_panel(
              tagList("Paris",
                      if (bets_inbox_count() > 0)
                        tags$span(bets_inbox_count(),
                                  class="badge bg-danger",
                                  style="margin-left:8px;")
              ),
              value = "paris"
            ),
            
            nav_panel("Compétitions", value = "compet"),
            nav_panel("Carte", value = "carte"),
            
            nav_panel(
              tagList("Amis",
                      if (inbox_count() > 0)
                        tags$span(inbox_count(),
                                  class="badge bg-danger",
                                  style="margin-left:8px;")
              ),
              value = "amis"
            ),
            
            nav_panel("Portefeuille", value = "wallet"),
            nav_panel("Règles", value = "regles"),
            nav_panel("Infos", value = "infos")
          )
        ),
        div(
          style="display:flex; align-items:center; justify-content:space-between; margin-bottom:12px;",
          h2("PAKAP ⚽", style="margin:0;"),
          actionButton("btn_profile", label="👩", class="btn btn-outline-light")
        ),
        uiOutput("main_ui")
      )
    }
  })
  
  observeEvent(input$btn_login, {
    if (is.null(input$login_user) || input$login_user == "") return()
    rv$me_id <- input$login_user
    
    m <- me()
    if (nrow(m) == 1 && (is.na(m$emoji[[1]]) || m$emoji[[1]] == "")) {
      dbExecute(con, "UPDATE users SET emoji = ? WHERE id = ?", params = list("👩", rv$me_id))
    }
    
    m <- me()
    if (nrow(m) == 1 && (is.na(m$pseudo[[1]]) || str_trim(m$pseudo[[1]]) == "")) {
      showModal(modalDialog(
        title = "Choisis ton pseudo (obligatoire)",
        p("Il doit être unique (personne d’autre ne peut l’utiliser)."),
        textInput("first_pseudo", "Pseudo", value = ""),
        actionButton("btn_save_first_pseudo", "Enregistrer", class="btn btn-primary"),
        easyClose = FALSE
      ))
    }
  })
  
  observeEvent(input$btn_save_first_pseudo, {
    p <- str_trim(input$first_pseudo)
    if (is.null(p) || p == "") return()
    
    exists <- dbGetQuery(
      con,
      "SELECT COUNT(*) n FROM users WHERE lower(pseudo)=lower(?) AND id<>?",
      params = list(p, rv$me_id)
    )$n > 0
    
    if (exists) {
      showModal(modalDialog(title="Pseudo déjà pris", "Choisis un autre pseudo.", easyClose=TRUE))
      return()
    }
    
    dbExecute(con, "UPDATE users SET pseudo=? WHERE id=?", params = list(p, rv$me_id))
    removeModal()
  })
  
  # ------------------ CARTE (persistante + modal + navigation) ------------------
  
  # 1) Monde
  world_sf_simple <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      iso3 = dplyr::if_else(is.na(iso_a3) | iso_a3 == "-99", adm0_a3, iso_a3),
      name_long2 = dplyr::coalesce(name_long, admin, name)
    )
  
  # Labels des plus grands pays
  N_LABELS <- 25
  
  label_sf <- world_sf_simple |>
    dplyr::mutate(.area = as.numeric(sf::st_area(geometry))) |>
    dplyr::arrange(dplyr::desc(.area)) |>
    dplyr::slice(1:N_LABELS) |>
    dplyr::mutate(geom_pt = sf::st_point_on_surface(geometry)) |>
    sf::st_as_sf(sf_column_name = "geom_pt") |>
    sf::st_set_geometry("geom_pt")
  
  france_label <- world_sf_simple |>
    dplyr::filter(iso3 == "FRA" | name_long2 == "France") |>
    dplyr::slice(1) |>
    dplyr::mutate(geom_pt = sf::st_point_on_surface(geometry)) |>
    sf::st_as_sf(sf_column_name = "geom_pt") |>
    sf::st_set_geometry("geom_pt")
  
  label_sf <- dplyr::bind_rows(label_sf, france_label) |>
    dplyr::distinct(iso3, .keep_all = TRUE)
  
  DEFAULT_LNG <- 0
  DEFAULT_LAT <- 20
  DEFAULT_ZOOM <- 2
  
  # Labels pour zoom (tous les pays)
  label_zoom_sf <- world_sf_simple |>
    dplyr::mutate(geom_pt = sf::st_point_on_surface(geometry)) |>
    sf::st_as_sf(sf_column_name = "geom_pt") |>
    sf::st_set_geometry("geom_pt")
  
  # 2) Normalisation clé
  norm_key <- function(x) {
    x |>
      as.character() |>
      stringi::stri_trans_general("Latin-ASCII") |>
      tolower() |>
      stringr::str_replace_all("[’']", " ") |>
      stringr::str_replace_all("[^a-z0-9]+", " ") |>
      stringr::str_squish()
  }
  
  # 3) Dictionnaire + alias (garde ton bloc manual_alias tel quel)
  country_keys_long <- (world_sf_simple |>
                          sf::st_drop_geometry() |>
                          dplyr::transmute(
                            iso3 = iso3,
                            k1 = norm_key(name_long),
                            k2 = norm_key(admin),
                            k3 = norm_key(name)
                          ) |>
                          tidyr::pivot_longer(cols = c(k1, k2, k3), values_to = "key") |>
                          dplyr::filter(!is.na(key), key != "") |>
                          dplyr::distinct(iso3, key)
  )
  
  manual_alias <- tibble::tibble(
    key = c(
      # --- ignorer barrages (géré dans team_to_iso3) ---
      
      # Afrique
      "cote d ivoire", "cote d’ivoire", "ivory coast",
      "rd congo", "republique democratique du congo", "dr congo",
      "guinee equatoriale",
      "cap vert",
      "soudan",
      "botswana",
      "zimbabwe",
      "zambie",
      "comores",
      "tanzanie",
      "ouganda",
      "benin",
      "ghana",
      "senegal",
      "cameroun",
      "mozambique",
      "gabon",
      "angola",
      "maroc",
      "mali",
      "egypte",
      "tunisie",
      "algerie",
      "burkina faso",
      "nigeria",
      "afrique du sud",
      
      # Europe
      "angleterre", "england", 
      "ecosse",
      "norvege",
      "pays bas", "netherlands",
      "croatie", "croatia",
      "belgique",
      "espagne",
      "portugal",
      "allemagne", "germany",
      "autriche",
      "suisse",
      "france",
      
      # Amérique
      "bresil", "brasil", "brazil",
      "etats unis", "etat unis", "usa", "united states", "united states of america",
      "canada",
      "mexique",
      "argentine",
      "paraguay",
      "colombie",
      "panama",
      "curacao", "curaçao",
      "equateur",
      "uruguay",
      "haiti",
      
      # Asie / Océanie
      "coree du sud", "south korea", "korea republic of", "republic of korea",
      "iran",
      "jordanie",
      "ouzbekistan",
      "qatar",
      "arabie saoudite",
      "japon",
      "australie",
      "nouvelle zelande"
    ),
    iso3 = c(
      # Afrique
      "CIV","CIV","CIV",
      "COD","COD","COD",
      "GNQ",
      "CPV",
      "SDN",
      "BWA",
      "ZWE",
      "ZMB",
      "COM",
      "TZA",
      "UGA",
      "BEN",
      "GHA",
      "SEN",
      "CMR",
      "MOZ",
      "GAB",
      "AGO",
      "MAR",
      "MLI",
      "EGY",
      "TUN",
      "DZA",
      "BFA",
      "NGA",
      "ZAF",
      
      # Europe
      "GBR", "GBR", # Angleterre -> UK
      "GBR", # Ecosse -> UK
      "NOR",
      "NLD","NLD",
      "HRV","HRV",
      "BEL",
      "ESP",
      "PRT",
      "DEU","DEU",
      "AUT",
      "CHE",
      "FRA",
      
      # Amérique
      "BRA","BRA","BRA",
      "USA","USA","USA","USA","USA",
      "CAN",
      "MEX",
      "ARG",
      "PRY",
      "COL",
      "PAN",
      "CUW","CUW",
      "ECU",
      "URY",
      "HTI",
      
      # Asie / Océanie
      "KOR","KOR","KOR","KOR",
      "IRN",
      "JOR",
      "UZB",
      "QAT",
      "SAU",
      "JPN",
      "AUS",
      "NZL"
    )
  )
  
  country_alias <- dplyr::bind_rows(country_keys_long, manual_alias) |>
    dplyr::mutate(key = norm_key(key)) |>
    dplyr::distinct(iso3, key)
  
  team_to_iso3 <- function(team_name) {
    if (is.null(team_name) || is.na(team_name)) return(NA_character_)
    if (grepl("^Bar\\.", team_name)) return(NA_character_)
    k <- norm_key(team_name)
    if (k == "") return(NA_character_)
    hit <- country_alias$iso3[match(k, country_alias$key)]
    hit <- hit[!is.na(hit)]
    if (length(hit) == 0) NA_character_ else hit[[1]]
  }
  
  # 4) Rendu leaflet 
  output$world_map <- renderLeaflet({
    rv$map_tick   # ✅ force un re-render complet
    
    leaflet(world_sf_simple, options = leafletOptions(preferCanvas = TRUE)) |>
      addMapPane("background", zIndex = 200) |>
      addMapPane("countries",  zIndex = 410) |>
      addMapPane("labels",     zIndex = 420) |>
      
      addRectangles(
        lng1 = -180, lat1 = -90,
        lng2 =  180, lat2 =  90,
        fillColor = "#A7D8F0", fillOpacity = 1, stroke = FALSE,
        options = pathOptions(pane = "background")
      ) |>
      
      addPolygons(
        data = world_sf_simple,
        layerId = ~iso3,
        label = ~name_long2,
        weight = 1.5,
        color = "#1B5E20",
        fillColor = "#81C784",
        fillOpacity = 1,
        smoothFactor = 0.2,
        options = pathOptions(pane = "countries")
      ) |>
      
      # ✅ Labels "monde" (25 + France)
      addLabelOnlyMarkers(
        data = label_sf,
        lng = ~sf::st_coordinates(geom_pt)[,1],
        lat = ~sf::st_coordinates(geom_pt)[,2],
        label = ~name_long2,
        labelOptions = labelOptions(
          noHide = TRUE, direction = "center", textsize = "12px", textOnly = TRUE,
          style = list(
            "color" = "#0B3D0B",
            "font-weight" = "700",
            "text-shadow" = "0 0 3px rgba(255,255,255,0.9)"
          )
        ),
        options = markerOptions(interactive = FALSE),
        group = "labels_big"
      ) |>
      # Labels zoom (tous les pays)
      addLabelOnlyMarkers(
        data = label_zoom_sf,
        lng = ~sf::st_coordinates(geom_pt)[,1],
        lat = ~sf::st_coordinates(geom_pt)[,2],
        label = ~name_long2,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textsize = "9px",
          textOnly = TRUE,
          style = list(
            "color" = "#0B3D0B",
            "font-weight" = "600",
            "text-shadow" = "0 0 3px rgba(255,255,255,0.9)"
          )
        ),
        options = markerOptions(interactive = FALSE),
        group = "labels_zoom"
      ) |>
      hideGroup("labels_zoom") |>
      
      setView(lng = DEFAULT_LNG, lat = DEFAULT_LAT, zoom = DEFAULT_ZOOM)
  })
  
  # ✅ important : ne pas suspendre quand caché
  outputOptions(output, "world_map", suspendWhenHidden = FALSE)
  
  # 5) stockage
  rv_country_matches <- reactiveVal(NULL)
  rv_country_label   <- reactiveVal(NULL)
  rv_country_iso3    <- reactiveVal(NULL)
  
  output$country_matches_dt <- DT::renderDT({
    df2 <- rv_country_matches()
    req(df2)
    
    DT::datatable(
      df2,
      rownames = FALSE,
      selection = "single",
      class = "compact nowrap",
      options = list(
        dom = "tip",
        pageLength = 12,
        scrollX = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))
      )
    )
  })
  
  # 6) clic pays => modal
  observeEvent(input$world_map_shape_click, {
    click <- input$world_map_shape_click
    if (is.null(click$id) || click$id == "") return()
    
    iso3_clicked <- click$id
    rv_country_iso3(iso3_clicked)
    
    # zoom
    one <- world_sf_simple[world_sf_simple$iso3 == iso3_clicked, ]
    if (nrow(one) == 1) {
      center <- sf::st_coordinates(sf::st_point_on_surface(one))
      
      leafletProxy("world_map") |>
        setView(
          lng = center[1],
          lat = center[2],
          zoom = 4
        )
    } else {
      leafletProxy("world_map") |>
        setView(lng = click$lng, lat = click$lat, zoom = 4)
    }
    
    country_label <- world_sf_simple$name_long2[match(iso3_clicked, world_sf_simple$iso3)]
    if (length(country_label) == 0 || is.na(country_label)) country_label <- iso3_clicked
    rv_country_label(country_label)
    
    df <- dbGetQuery(con, "
  SELECT match_id, competition, phase, groupe, date_txt, heure_txt,
       equipe_1, pays_1, equipe_2, pays_2,
       score_1, score_2, tab_1, tab_2
  FROM matches
")
    
    df2 <- df |>
      dplyr::mutate(
        iso3_1 = dplyr::if_else(
          competition == "LDC" & !is.na(pays_1) & pays_1 != "",
          vapply(pays_1, team_to_iso3, character(1)),
          vapply(equipe_1, team_to_iso3, character(1))
        ),
        iso3_2 = dplyr::if_else(
          competition == "LDC" & !is.na(pays_2) & pays_2 != "",
          vapply(pays_2, team_to_iso3, character(1)),
          vapply(equipe_2, team_to_iso3, character(1))
        ),
        Match  = paste0(equipe_1, " - ", equipe_2),
        Score  = score_with_tab(score_1, score_2, tab_1, tab_2)
      ) |>
      dplyr::filter(iso3_1 == iso3_clicked | iso3_2 == iso3_clicked) |>
      dplyr::transmute(
        match_id,
        Competition = competition,
        Phase = phase,
        Groupe = dplyr::if_else(is.na(groupe) | groupe == "", "", groupe),
        Date = date_txt,
        Heure = heure_txt,
        Match,
        Score
      ) |>
      dplyr::arrange(Date, Heure)
    
    rv_country_matches(df2)
    
    removeModal()
    
    if (nrow(df2) == 0) {
      showModal(modalDialog(
        title = paste0("📍 ", country_label),
        "Aucun match trouvé pour ce pays.",
        footer = tagList(
          actionButton("btn_close_country_modal", "Fermer", class="btn btn-outline-light")
        ),
        easyClose = FALSE
      ))
    } else {
      showModal(modalDialog(
        title = paste0("📍 ", country_label, " — ", nrow(df2), " match(s)"),
        DTOutput("country_matches_dt"),
        size = "l",
        footer = tagList(
          actionButton("btn_close_country_modal", "Fermer", class="btn btn-outline-light")
        ),
        easyClose = FALSE
      ))
    }
  }, ignoreInit = TRUE)
  
  # 7) fermer modal => reset zoom
  observeEvent(input$btn_close_country_modal, {
    removeModal()
    rv_country_iso3(NULL)
    
    leafletProxy("world_map") |>
      setView(lng = DEFAULT_LNG, lat = DEFAULT_LAT, zoom = DEFAULT_ZOOM)
  }, ignoreInit = TRUE)
  
  # 8) clic match => fiche match + switch onglet compet
  observeEvent(input$country_matches_dt_rows_selected, {
    sel <- input$country_matches_dt_rows_selected
    if (!length(sel)) return()
    
    df2 <- rv_country_matches()
    req(df2)
    
    match_id_clicked <- df2$match_id[sel]
    
    mm <- dbGetQuery(con, "
    SELECT *
    FROM matches
    WHERE match_id = ?
    LIMIT 1
  ", params = list(match_id_clicked))
    
    if (nrow(mm) == 0) {
      showNotification('Match introuvable dans la base.', type = 'error')
      return()
    }
    
    rv$selected_match <- as.list(mm[1, ])
    
    removeModal()
    updateTabsetPanel(session, "tab", selected = "compet")
  }, ignoreInit = TRUE)
  
  # 9) retour sur carte => resize Leaflet (utile)
  observeEvent(input$tab, {
    if (identical(input$tab, "carte")) {
      session$onFlushed(function() {
        leafletProxy("world_map") %>% leaflet::invokeMethod(NULL, "invalidateSize")
      }, once = TRUE)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$tab, {
    if (identical(input$tab, "carte")) {
      
      # ✅ rebuild complet de Leaflet
      rv$map_tick <- rv$map_tick + 1
      
      # ✅ après que l’UI soit bien affichée, on force Leaflet à recalculer sa taille
      session$onFlushed(function() {
        leafletProxy("world_map") %>% leaflet::invokeMethod(NULL, "invalidateSize")
      }, once = TRUE)
    }
  }, ignoreInit = TRUE)
  
  # ---------------- FIN CARTE ------------------
  
  #LDC
  is_played <- function(mm) {
    # joué si on a un score (ou winner non vide)
    has_score <- !is.null(mm$score_1) && !is.na(mm$score_1) &&
      !is.null(mm$score_2) && !is.na(mm$score_2)
    has_winner <- !is.null(mm$winner) && !is.na(mm$winner) && mm$winner != ""
    has_score || has_winner
  }
  
  is_from_barrages <- function(mm) {
    ph <- mm$phase %||% ""
    comp <- mm$competition %||% ""
    
    # CDM : tout est pariable si pas joué
    if (comp == "CDM") return(TRUE)
    
    # LDC : à partir des barrages
    if (ph == "Barrages") return(TRUE)
    if (ph %in% c("Huitièmes", "Quarts", "Demi-finales", "Finale")) return(TRUE)
    
    FALSE
  } 
  
  # ------------------ MAIN UI ------------------
  output$main_ui <- renderUI({
    req(rv$me_id)
    m <- me()
    updateActionButton(session, "btn_profile", label = m$emoji[[1]])
    
    tagList(
      # ------------------ CARTE (persistante) ------------------
      # La carte est toujours dans le DOM, juste affichée/masquée en JS
      conditionalPanel(
        condition = "input.tab == 'carte'",
        card(
          card_header("🌍 Carte des pays"),
          leafletOutput("world_map", height = "600px")
        )
      ),
      
      # ------------------ RESTE DE L'APP ------------------
      conditionalPanel(
        condition = "input.tab != 'carte'",
        tagList(
          # ---- PARIS ----
          conditionalPanel(
            condition = "input.tab == 'paris'",
            card(
              card_header("📌 Paris"),
              navset_tab(
                id = "paris_subtab",
                nav_panel("Paris acceptés", value = "acc", DTOutput("bets_table")),
                nav_panel("Paris envoyés",  value = "sent", DTOutput("sent_paris_table")),
                nav_panel(
                  tagList(
                    "Demandes reçues",
                    if (isTRUE(received_count() > 0))
                      tags$span(
                        received_count(),
                        class = "badge bg-danger",
                        style = "margin-left:8px;"
                      )
                  ),
                  value = "recv",
                  DTOutput("received_requests_table"),
                  uiOutput("bet_actions")
                )
              )
            )
          ),
          
          # ---- COMPET ----
          conditionalPanel(
            condition = "input.tab == 'compet'",
            uiOutput("compet_ui")  # 👈 on délègue à un renderUI pour gérer rv$selected_match
          ),
          
          # ---- AMIS ----
          conditionalPanel(
            condition = "input.tab == 'amis'",
            card(
              card_header("👥 Amis"),
              uiOutput("amis_tabs_ui")
            )
          ),
          
          # ---- WALLET ----
          conditionalPanel(
            condition = "input.tab == 'wallet'",
            uiOutput("wallet_ui")
          ),
          
          # ---- REGLES ----
          conditionalPanel(
            condition = "input.tab == 'regles'",
            card(
              card_header("📚 Règles & prévention"),
              h4("Règles"),
              tags$ul(
                tags$li("Pariez entre amis sur vos équipes préférées !"),
                tags$li("Vous pouvez envoyer et recevoir une demande de pari uniquement à un ami accepté."),
                tags$li("Une demande reçue peut être accepter / refuser / modifier (renvoyer)."),
                tags$li("Le montant est prélevé de votre cagnotte au moment de l’acceptation.")
              ),
              br(),
              h4("⚠️ Dangers des paris / addiction"),
              tags$ul(
                tags$li("Fixez-vous un budget maximum et respectez-le."),
                tags$li("Ne misez pas pour vous refaire."),
                tags$li("Si ça prend trop de place : parlez-en et faites une pause."),
                tags$li(tagList(
                  "Les jeux d'argent et de hasard peuvent être dangereux : pertes d'argent, conflits familiaux, addiction… Retrouvez nos conseils sur ",
                  tags$a(href = "https://www.joueurs-info-service.fr", target = "_blank", "joueurs-info-service.fr"),
                  " (", tags$a(href = "tel:0974751313", "09 74 75 13 13"), " - appel non surtaxé)"
                )),
                tags$li("Jouer comporte des risques !")
              )
            )
          ),
          
          # ---- INFOS ----
          conditionalPanel(
            condition = "input.tab == 'infos'",
            {
              m <- me()
              card(
                card_header("ℹ️ Infos"),
                h4("Ton profil"),
                p(paste0("ID : ", m$id[[1]])),
                p(paste0("Nom : ", m$nom[[1]], " ", m$prenom[[1]])),
                p(paste0("Pseudo : @", m$pseudo[[1]])),
                br(),
                p("Prototype Shiny – paris amicaux + gestion amis + DB SQLite."),
                br(),
                p("Pour plus d'informations :"),
                tags$p(
                  "Contactez-nous : ",
                  tags$a(href = "mailto:amelie.pires18@gmail.com", "Amélie Pires"),
                  " | ",
                  tags$a(href = "mailto:pakop2199@gmail.com", "Patrice Kolié")
                ),
                p(tags$em("⚠ Attention :")),
                p(tags$em(tagList(
                  "Les jeux d'argent et de hasard peuvent être dangereux : pertes d'argent, conflits familiaux, addiction… Retrouvez nos conseils sur ",
                  tags$a(href = "https://www.joueurs-info-service.fr", target = "_blank", "joueurs-info-service.fr"),
                  " (", tags$a(href = "tel:0974751313", "09 74 75 13 13"), " - appel non surtaxé)"
                ))),
                p(tags$em("Jouer comporte des risques !"))
              )
            }
          )
        )
      )
    )
  })
  
  output$compet_ui <- renderUI({
    if (is.null(rv$selected_match)) {
      
      card(
        card_header("🏆 Compétitions"),
        textInput(
          "match_search",
          "Rechercher (équipe, pays, compétition ...)",
          value = ""
        ),
        navset_tab(
          id = "compet_main_tab",
          
          nav_panel(
            "CDM", value = "CDM",
            navset_tab(
              id = "cdm_sub_tab",
              nav_panel("CDM - Poules", value = "groups", uiOutput("cdm_groups_ui")),
              nav_panel("CDM - Phases finales", value = "finals", uiOutput("cdm_finals_ui"))
            )
          ),
          
          nav_panel(
            "CAN", value = "CAN",
            navset_tab(
              id = "can_sub_tab",
              nav_panel("CAN - Poules", value = "groups", uiOutput("can_groups_ui")),
              nav_panel("CAN - Phases finales", value = "finals", uiOutput("can_finals_ui"))
            )
          ),
          
          nav_panel(
            "LDC", value = "LDC",
            navset_tab(
              id = "ldc_sub_tab",
              nav_panel("Barrages de qualification", value = "bq", uiOutput("ldc_bq_ui")),
              nav_panel("Journées", value = "jours", uiOutput("ldc_jours_ui")),
              nav_panel("Barrages", value = "barr", uiOutput("ldc_barr_ui")),
              nav_panel("Phases finales", value = "pf", uiOutput("ldc_pf_ui"))
            )
          )
        ),
        p("Clique sur un match pour envoyer une demande à un ami.",
          style="opacity:.85; margin-top:10px;")
      )
      
    } else {
      
      mm <- rv$selected_match
      played <- is_played(mm)
      bettable_phase <- is_from_barrages(mm)
      
      # si joué OU pas une phase "à partir des barrages" => pas de pari
      if (played || !bettable_phase) {
        
        score_txt <- score_with_tab(mm$score_1, mm$score_2, mm$tab_1, mm$tab_2)
        
        card(
          card_header(if (played) "Match terminé"
                      else "Match à venir (pari indisponible)"),
          
          h3(paste0(mm$equipe_1, " vs ", mm$equipe_2)),
          
          p(paste0(
            mm$competition, " • ",
            ifelse(
              identical(mm$competition, "LDC") && !is.na(mm$journee),
              "Journée",
              (mm$phase %||% "")
            ),
            ifelse(!is.na(mm$groupe) && (mm$groupe %||% "") != "",
                   paste0(" • Groupe ", mm$groupe), ""),
            ifelse(!is.na(mm$journee),
                   paste0(" • Journée ", mm$journee), ""),
            ifelse(!is.null(mm$date_txt) && (mm$date_txt %||% "") != "",
                   paste0(" • ", mm$date_txt), ""),
            ifelse(!is.null(mm$heure_txt) && (mm$heure_txt %||% "") != "",
                   paste0(" à ", mm$heure_txt), "")
          ), style="opacity:.85;"),
          
          if (played && score_txt != "")
            p(paste0("Score : ", score_txt)),
          
          if (!is.null(mm$winner) && !is.na(mm$winner) && mm$winner != "")
            p(paste0("Gagnant : ", mm$winner)),
          
          # ----- CAS CAN : Finale et 3e place -----
          if (identical(mm$competition, "CAN") && played) {
            
            if (identical(mm$phase, "Finale")) {
              
              # Déterminer gagnant / perdant
              gagnant <- mm$winner
              perdant <- if (!is.null(gagnant) && gagnant == mm$equipe_1) mm$equipe_2 else mm$equipe_1
              
              tagList(
                if (!is.null(gagnant) && gagnant != "")
                  p(paste0("🏆 1ère place de la CAN : ", gagnant)),
                
                if (!is.null(perdant) && perdant != "")
                  p(paste0("🥈 2ème place de la CAN : ", perdant))
              )
              
            } else if (identical(mm$phase, "3e place")) {
              
              if (!is.null(mm$winner) && mm$winner != "")
                p(paste0("🥉 3e place de la CAN : ", mm$winner))
              
            } else {
              
              # Autres phases CAN : comportement normal
              if (!is.null(mm$qualif_txt) &&
                  !is.na(mm$qualif_txt) && mm$qualif_txt != "")
                p(paste0("Qualifié : ", mm$qualif_txt))
            }
            
          } else {
            
            # Cas général (CDM + LDC sauf J8)
            if (!(identical(mm$competition, "LDC") &&
                  !is.na(mm$journee) && mm$journee == 8)) {
              
              if (!is.null(mm$qualif_txt) &&
                  !is.na(mm$qualif_txt) && mm$qualif_txt != "")
                p(paste0("Qualifié : ", mm$qualif_txt))
            }
          },
          
          if (identical(mm$competition, "LDC") &&
              !is.na(mm$journee) && mm$journee == 8) {
            
            tagList(
              if (!is.null(mm$qualif_eq1) &&
                  !is.na(mm$qualif_eq1) && mm$qualif_eq1 != "")
                p(paste0("Statut ", mm$equipe_1, " : ",
                         mm$qualif_eq1)),
              
              if (!is.null(mm$qualif_eq2) &&
                  !is.na(mm$qualif_eq2) && mm$qualif_eq2 != "")
                p(paste0("Statut ", mm$equipe_2, " : ",
                         mm$qualif_eq2))
            )
          },
          
          br(),
          actionButton("btn_back_matches",
                       "⬅ Retour",
                       class="btn btn-outline-light")
        )
        
      } else {
        
        # UI de pari
        amis <- friends_tbl()
        
        card(
          card_header("🧾 Envoyer une demande de pari"),
          h3(paste0(mm$equipe_1, " vs ", mm$equipe_2)),
          
          p(
            paste0(
              mm$competition, " • ", mm$phase,
              ifelse(!is.na(mm$groupe) && mm$groupe != "",
                     paste0(" • Groupe ", mm$groupe), ""),
              ifelse(!is.na(mm$journee),
                     paste0(" • Journée ", mm$journee), ""),
              " • ", mm$date_txt, " à ", mm$heure_txt
            ),
            style="opacity:.85;"
          ),
          
          radioButtons("pick_team",
                       "Ton choix :",
                       choices = c(mm$equipe_1, mm$equipe_2),
                       inline = TRUE),
          
          numericInput("bet_amount",
                       "Montant (€)",
                       value = 10,
                       min = 1,
                       step = 1),
          
          if (nrow(amis) == 0) {
            div(class="alert alert-warning",
                "Tu n'as aucun ami accepté. Va dans l’onglet Amis pour inviter.")
          } else {
            selectInput(
              "friend_target",
              "Envoyer à :",
              choices = setNames(
                amis$id,
                paste0(
                  amis$id, " - ",
                  amis$nom, " ", amis$prenom,
                  ifelse(amis$pseudo != "",
                         paste0(" (@", amis$pseudo, ")"), "")
                )
              )
            )
          },
          
          div(
            style="display:flex; gap:10px; margin-top:12px;",
            actionButton("btn_send_bet",
                         "Envoyer",
                         class="btn btn-primary"),
            actionButton("btn_back_matches",
                         "⬅ Retour",
                         class="btn btn-outline-light")
          )
        )
        
      }
    }
  })
  
  # ✅ UI WALLET : ton bloc wallet inchangé (mis à part le fait qu'il est séparé)
  output$wallet_ui <- renderUI({
    w <- wallet()
    
    card(
      card_header("💰 Portefeuille"),
      h3(paste0("Solde actuel : ", w$solde[[1]], " €")),
      p(paste0("Gains cumulés : ", w$gains_total[[1]], " €")),
      br(),
      fluidRow(
        column(
          6,
          card(
            card_header("Ajouter"),
            numericInput("add_money", "Montant (€)", value = 10, min = 1, step = 1),
            actionButton("btn_add_money", "Ajouter", class="btn btn-success")
          )
        ),
        column(
          6,
          card(
            card_header("Retirer"),
            numericInput("withdraw_money", "Montant (€)", value = 10, min = 1, step = 1),
            actionButton("btn_withdraw", "Retirer", class = "btn btn-warning"),
            br(), br(),
            uiOutput("wallet_msg")
          )
        )
      )
    )
  })
  
  # --- mémoriser onglet/sous-onglet/sous-sous-onglet ---
  observeEvent(input$compet_main_tab, {
    rv$last_compet_tab <- input$compet_main_tab
  }, ignoreInit = TRUE)
  
  observeEvent(input$cdm_sub_tab, {
    rv$last_cdm_tab <- input$cdm_sub_tab
  }, ignoreInit = TRUE)
  
  observeEvent(input$can_sub_tab, {
    rv$last_can_tab <- input$can_sub_tab
  }, ignoreInit = TRUE)
  
  observeEvent(input$cdm_groups_tab, {
    rv$last_cdm_group <- input$cdm_groups_tab
  }, ignoreInit = TRUE)
  
  observeEvent(input$cdm_finals_tab, {
    rv$last_cdm_phase <- input$cdm_finals_tab
  }, ignoreInit = TRUE)
  
  observeEvent(input$can_groups_tab, {
    rv$last_can_group <- input$can_groups_tab
  }, ignoreInit = TRUE)
  
  observeEvent(input$can_finals_tab, {
    rv$last_can_phase <- input$can_finals_tab
  }, ignoreInit = TRUE)
  
  observeEvent(input$ldc_sub_tab, { 
    rv$last_ldc_tab <- input$ldc_sub_tab 
  }, ignoreInit = TRUE)
  observeEvent(input$ldc_j_tab,   {
    rv$last_ldc_j   <- input$ldc_j_tab 
  },   ignoreInit = TRUE)
  observeEvent(input$ldc_pf_tab,  {
    rv$last_ldc_pf  <- input$ldc_pf_tab 
  },  ignoreInit = TRUE)
  
  # ------------------ PORTFEUILLE ------------------
  observeEvent(input$btn_add_money, {
    req(rv$me_id)
    
    x <- input$add_money %||% 0
    if (is.na(x) || x <= 0) return()
    
    dbExecute(
      con,
      "UPDATE wallets SET solde = solde + ? WHERE user_id = ?",
      params = list(x, rv$me_id)
    )
    rv$wallet_tick <- rv$wallet_tick + 1
    bump_refresh()
    showNotification(paste0("+", x, "€ ajoutés."), type = "message", duration = 1.5)
  })
  
  observeEvent(input$btn_withdraw, {
    req(rv$me_id)
    
    x <- input$withdraw_money %||% 0
    if (is.na(x) || x <= 0) return()
    
    w <- wallet()
    if (nrow(w) == 0) return()
    
    if (w$solde[[1]] < x) {
      showNotification("Solde insuffisant.", type = "error", duration = 2)
      return()
    }
    
    dbExecute(
      con,
      "UPDATE wallets SET solde = solde - ? WHERE user_id = ?",
      params = list(x, rv$me_id)
    )
    rv$wallet_tick <- rv$wallet_tick + 1
    bump_refresh()
    showNotification(paste0("-", x, "€ retirés."), type = "warning", duration = 1.5)
  })
  
  # ------------------ AMIS : recherche + inviter ------------------
  users_search_df <- reactive({
    df <- users_tbl()
    q <- str_trim(input$search_friend %||% "")
    if (q != "") {
      df <- df |>
        filter(
          str_detect(id, fixed(q)) |
            str_detect(tolower(pseudo), tolower(q)) |
            str_detect(tolower(nom), tolower(q)) |
            str_detect(tolower(prenom), tolower(q))
        )
    }
    df
  })
  
  output$users_table <- renderDT({
    df <- users_search_df()
    q <- str_trim(input$search_friend %||% "")
    
    if (q != "") {
      df <- df |>
        filter(
          str_detect(id, fixed(q)) |
            str_detect(tolower(pseudo), tolower(q)) |
            str_detect(tolower(nom), tolower(q)) |
            str_detect(tolower(prenom), tolower(q))
        )
    }
    
    datatable(
      df |> transmute(ID=id, Nom=nom, Prenom=prenom, Pseudo=ifelse(pseudo=="","",paste0("@",pseudo))),
      rownames = FALSE,
      selection = "single",
      options = list(dom="t", paging=FALSE, scrollY="420px", scrollCollapse=TRUE,scrollX = TRUE)
    )
  })
  
  output$invite_actions_top <- renderUI({
    sel <- input$users_table_rows_selected
    if (!length(sel)) {
      tags$button("Inviter", class="btn btn-secondary", disabled=NA)
    } else {
      actionButton("btn_invite_friend", "Inviter", class="btn btn-primary")
    }
  })
  
  observeEvent(input$btn_invite_friend, {
    sel <- input$users_table_rows_selected
    if (!length(sel)) return()
    
    df <- users_search_df()
    target_id <- df$id[sel]
    if (target_id == rv$me_id) return()
    
    a <- min(rv$me_id, target_id)
    b <- max(rv$me_id, target_id)
    
    already <- dbGetQuery(con, "SELECT COUNT(*) n FROM friendships WHERE user1=? AND user2=?", params=list(a,b))$n > 0
    if (already) {
      showModal(modalDialog(title="Déjà amis", "Vous êtes déjà amis.", easyClose=TRUE))
      return()
    }
    
    pending <- dbGetQuery(con, "
      SELECT COUNT(*) n FROM friend_requests
      WHERE ((from_id=? AND to_id=?) OR (from_id=? AND to_id=?)) AND status='pending'
    ", params=list(rv$me_id, target_id, target_id, rv$me_id))$n > 0
    
    if (pending) {
      showModal(modalDialog(title="Invitation déjà en cours", "Une invitation est déjà en attente.", easyClose=TRUE))
      return()
    }
    
    dbExecute(con,
              "INSERT INTO friend_requests (from_id, to_id, status, created_at) VALUES (?,?, 'pending', ?)",
              params = list(rv$me_id, target_id, now_txt())
    )
    
    bump_refresh()
    showModal(modalDialog(title="Invitation envoyée", "OK ✅", easyClose=TRUE))
  })
  
  output$friend_inbox_table <- renderDT({
    rv$refresh_tick
    df <- friend_inbox()
    if (nrow(df)==0) {
      return(datatable(data.frame(Message="Aucune invitation reçue"), rownames=FALSE, options=list(dom="t", paging=FALSE,scrollX = TRUE)))
    }
    u <- users_tbl() |> select(id, nom, prenom, pseudo)
    df <- df |>
      left_join(u, by=c("from_id"="id")) |>
      transmute(fr_id, from_id, De = paste0(from_id, " - ", nom, " ", prenom, ifelse(pseudo!="", paste0(" (@",pseudo,")"), "")))
    datatable(
      df,
      rownames = FALSE,
      selection = "single",
      options = list(
        dom="t",
        paging=FALSE,
        scrollY="420px",
        scrollCollapse=TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))  # ✅ cache fr_id
      )
    )})
  
  output$friend_inbox_actions <- renderUI({
    sel <- input$friend_inbox_table_rows_selected
    if (!length(sel)) {
      div(style="display:flex; gap:10px; margin-top:10px;",
          tags$button("Accepter", class="btn btn-secondary", disabled=NA),
          tags$button("Refuser", class="btn btn-secondary", disabled=NA))
    } else {
      div(style="display:flex; gap:10px; margin-top:10px;",
          actionButton("btn_accept_friend", "Accepter", class="btn btn-success"),
          actionButton("btn_refuse_friend", "Refuser", class="btn btn-danger"))
    }
  })
  
  observeEvent(input$btn_accept_friend, {
    sel <- input$friend_inbox_table_rows_selected
    if (!length(sel)) return()
    
    df <- friend_inbox()
    fr_id <- df$fr_id[sel]
    from_id <- df$from_id[sel]
    
    dbExecute(con, "UPDATE friend_requests SET status='accepted' WHERE fr_id=?", params=list(fr_id))
    
    a <- min(rv$me_id, from_id)
    b <- max(rv$me_id, from_id)
    dbExecute(con, "INSERT OR IGNORE INTO friendships (user1, user2, created_at) VALUES (?,?,?)",
              params=list(a,b, now_txt()))
    
    bump_refresh()
  })
  
  observeEvent(input$btn_refuse_friend, {
    sel <- input$friend_inbox_table_rows_selected
    if (!length(sel)) return()
    
    df <- friend_inbox()
    fr_id <- df$fr_id[sel]
    dbExecute(con, "UPDATE friend_requests SET status='refused' WHERE fr_id=?", params=list(fr_id))
    
    bump_refresh()
  })
  
  output$friends_table <- renderDT({
    rv$refresh_tick
    df <- friends_tbl()
    if (nrow(df)==0) {
      return(datatable(data.frame(Message="Aucun ami accepté"), rownames=FALSE, options=list(dom="t", paging=FALSE,scrollX = TRUE)))
    }
    datatable(
      df |> transmute(ID=id, Nom=nom, Prenom=prenom, Pseudo=ifelse(pseudo=="","",paste0("@",pseudo))),
      rownames=FALSE,
      options=list(dom="t", paging=FALSE, scrollY="420px", scrollCollapse=TRUE)
    )
  })
  
  output$amis_tabs_ui <- renderUI({
    n <- inbox_count()
    badge <- if (isTRUE(n > 0)) tags$span(as.character(n), class="badge bg-danger", style="margin-left:8px;") else NULL
    
    navset_tab(
      nav_panel(
        "Rechercher",
        div(
          style="display:flex; gap:10px; align-items:flex-end;",
          div(style="flex:1;", textInput("search_friend", "Rechercher (ID / pseudo / nom)", value="")),
          uiOutput("invite_actions_top")
        ),
        DTOutput("users_table")
      ),
      nav_panel(tagList("Invitations reçues", badge), DTOutput("friend_inbox_table"), uiOutput("friend_inbox_actions")),
      nav_panel("Mes amis", DTOutput("friends_table"))
    )
  })
  
  # ------------------ PARIS : envoyés ------------------
  output$sent_paris_table <- renderDT({
    req(rv$me_id)
    rv$refresh_tick
    
    df <- dbGetQuery(con, "
      SELECT req_id, competition, match, to_id, choix_from, amount, status, created_at
      FROM requests
      WHERE from_id = ?
        AND status IN ('pending','refused','countered')
      ORDER BY req_id DESC
    ", params = list(rv$me_id))
    
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message="Aucune demande envoyée"), rownames=FALSE, options=list(dom="t", paging=FALSE,scrollX = TRUE)))
    }
    
    u <- users_tbl() |> select(id, nom, prenom, pseudo)
    
    df <- df |>
      left_join(u, by = c("to_id" = "id")) |>
      mutate(
        Adversaire = paste0(to_id, " - ", nom, " ", prenom, ifelse(pseudo != "", paste0(" (@", pseudo, ")"), "")),
        Statut = case_when(
          status == "pending"   ~ "En attente",
          status == "refused"   ~ "Refusé",
          status == "countered" ~ "Refusé - modifié",
          TRUE ~ status
        )
      ) |>
      transmute(
        Competition = competition,
        Match = match,
        Adversaire,
        `Mon choix` = choix_from,
        Montant = amount,
        Statut
      )
    
    datatable(df, rownames=FALSE, options=list(dom="t", paging=FALSE, scrollY="420px", scrollCollapse=TRUE,scrollX = TRUE))
  })
  
  # ------------------ PARIS : demandes reçues ------------------
  output$received_requests_table <- renderDT({
    df <- received_requests()
    if (nrow(df)==0) {
      return(datatable(data.frame(Message="Aucune demande reçue"), rownames=FALSE, options=list(dom="t", paging=FALSE,scrollX = TRUE)))
    }
    
    u <- users_tbl() |> select(id, nom, prenom, pseudo)
    
    df <- df |>
      left_join(u, by=c("from_id"="id")) |>
      transmute(
        req_id, # caché
        Competition = competition,
        Match = match,
        De = paste0(from_id, " - ", nom, " ", prenom, ifelse(pseudo!="", paste0(" (@",pseudo,")"), "")),
        `Choix de l'adversaire` = choix_from,
        Montant = amount
      )
    
    datatable(
      df,
      rownames=FALSE,
      selection="single",
      options=list(
        dom="t",
        paging=FALSE,
        scrollY="420px",
        scrollX = TRUE,
        scrollCollapse=TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))
      )
    )
  })
  
  output$bet_actions <- renderUI({
    sel <- input$received_requests_table_rows_selected
    if (!length(sel)) {
      div(style="display:flex; gap:10px; margin-top:10px;",
          tags$button("Accepter", class="btn btn-secondary", disabled=NA),
          tags$button("Modifier", class="btn btn-secondary", disabled=NA),
          tags$button("Refuser", class="btn btn-secondary", disabled=NA))
    } else {
      div(style="display:flex; gap:10px; margin-top:10px;",
          actionButton("btn_accept_bet", "Accepter", class="btn btn-success"),
          actionButton("btn_modify_bet", "Modifier", class="btn btn-primary"),
          actionButton("btn_refuse_bet", "Refuser", class="btn btn-danger"))
    }
  })
  
  # Actions sur demandes reçues
  observeEvent(input$btn_refuse_bet, {
    sel <- input$received_requests_table_rows_selected
    if (!length(sel)) return()
    
    df <- received_requests()
    req_id <- df$req_id[sel]
    dbExecute(con, "UPDATE requests SET status='refused' WHERE req_id=?", params=list(req_id))
    
    bump_refresh()
  })
  
  observeEvent(input$btn_accept_bet, {
    sel <- input$received_requests_table_rows_selected
    if (!length(sel)) return()
    
    df <- received_requests()
    one <- df[sel, , drop=FALSE]
    
    amount  <- one$amount[[1]]
    req_id  <- one$req_id[[1]]
    sender  <- one$from_id[[1]]   # celui qui a envoyé la demande
    acceptor <- rv$me_id          # moi (qui accepte)
    
    # Soldes des 2 joueurs
    w_acceptor <- dbGetQuery(con, "SELECT solde FROM wallets WHERE user_id = ?", params=list(acceptor))
    w_sender   <- dbGetQuery(con, "SELECT solde FROM wallets WHERE user_id = ?", params=list(sender))
    
    if (nrow(w_acceptor)==0 || nrow(w_sender)==0) return()
    
    # Vérif solvabilité des 2 côtés
    if (w_acceptor$solde[[1]] < amount) {
      showModal(modalDialog(title="Solde insuffisant", "Tu n'as pas assez d'argent pour accepter.", easyClose=TRUE))
      return()
    }
    if (w_sender$solde[[1]] < amount) {
      showModal(modalDialog(
        title="Pari impossible",
        "Ton adversaire n'a pas assez d'argent. Demande invalide.",
        easyClose=TRUE
      ))
      # Optionnel : on refuse automatiquement la demande
      dbExecute(con, "UPDATE requests SET status='refused' WHERE req_id=?", params=list(req_id))
      bump_refresh()
      return()
    }
    
    # Transaction = tout ou rien
    DBI::dbWithTransaction(con, {
      # Débit des deux joueurs
      dbExecute(con, "UPDATE wallets SET solde = solde - ? WHERE user_id = ?", params=list(amount, acceptor))
      dbExecute(con, "UPDATE wallets SET solde = solde - ? WHERE user_id = ?", params=list(amount, sender))
      
      # Acceptation
      dbExecute(con, "UPDATE requests SET status='accepted' WHERE req_id=?", params=list(req_id))
    })
    
    rv$wallet_tick <- rv$wallet_tick + 1
    bump_refresh()
  })
  
  observeEvent(input$btn_modify_bet, {
    sel <- input$received_requests_table_rows_selected
    if (!length(sel)) return()
    
    df <- received_requests()
    one <- df[sel, , drop = FALSE]
    req_id <- one$req_id[[1]]
    
    teams <- split_match_teams(one$match[[1]])
    teamA <- str_trim(teams[[1]])
    teamB <- str_trim(teams[[2]])
    
    showModal(modalDialog(
      title = "✏️ Modifier (contre-proposition)",
      tags$p(tags$b("Match : "), one$match[[1]]),
      tags$p(tags$b("Choix de l’ami : "), one$choix_from[[1]]),
      br(),
      if (!is.na(teamA) && !is.na(teamB)) {
        radioButtons(
          "counter_choice",
          "Ton choix :",
          choices = c(teamA, teamB),
          selected = ifelse(one$choix_from[[1]] == teamA, teamB, teamA),
          inline = TRUE
        )
      } else {
        textInput("counter_choice_txt", "Ton choix :", value = "")
      },
      numericInput("counter_amount", "Nouveau montant (€)", value = one$amount[[1]], min = 1, step = 1),
      div(style="display:flex; gap:10px; margin-top:10px;",
          actionButton("btn_send_counter", "Renvoyer", class="btn btn-primary"),
          actionButton("btn_cancel_counter", "Annuler", class="btn btn-outline-light")
      ),
      easyClose = TRUE
    ))
    
    observeEvent(input$btn_cancel_counter, {
      removeModal()
    }, ignoreInit = TRUE, once = TRUE)
    
    observeEvent(input$btn_send_counter, {
      
      new_amount <- input$counter_amount %||% one$amount[[1]]
      if (is.na(new_amount) || new_amount <= 0) return()
      
      my_choice <- if (!is.na(teamA) && !is.na(teamB)) input$counter_choice else input$counter_choice_txt
      my_choice <- str_trim(as.character(my_choice))
      if (is.na(my_choice) || my_choice == "") return()
      
      # ✅ contrôle cagnotte + débit AU MOMENT DU RENVOI
      w <- wallet()
      if (nrow(w) == 0 || w$solde[[1]] < new_amount) {
        showModal(modalDialog(
          title = "Solde insuffisant",
          paste0("Tu as ", w$solde[[1]], " € mais tu veux miser ", new_amount, " €."),
          easyClose = TRUE
        ))
        return()
      }
      
      dbExecute(
        con,
        "UPDATE wallets SET solde = solde - ? WHERE user_id = ?",
        params = list(new_amount, rv$me_id)
      )
      rv$wallet_tick <- rv$wallet_tick + 1
      
      # 1) on marque l’ancienne demande comme renvoyée
      dbExecute(con, "UPDATE requests SET status='countered' WHERE req_id=?", params = list(req_id))
      
      # 2) on crée UNE SEULE nouvelle demande dans l’autre sens
      dbExecute(con, "
        INSERT INTO requests (from_id, to_id, competition, match, choix_from, amount, status, created_at, ref_req_id)
        VALUES (?,?,?,?,?,?,'pending',?,?)
      ", params = list(
        rv$me_id, one$from_id[[1]], one$competition[[1]], one$match[[1]],
        my_choice, new_amount, now_txt(), req_id
      ))
      
      bump_refresh()
      removeModal()
      DT::selectRows(DT::dataTableProxy("received_requests_table"), NULL)
      updateTabsetPanel(session, "paris_subtab", selected = "sent")
      
    }, ignoreInit = TRUE, once = TRUE)
    
  })
  
  # ------------------ PARIS : acceptés ------------------
  output$bets_table <- renderDT({
    req(rv$me_id)
    rv$refresh_tick
    
    df <- dbGetQuery(con, "
      SELECT req_id, competition, match, from_id, to_id, choix_from, amount, status
      FROM requests
      WHERE (from_id=? OR to_id=?) AND status='accepted'
      ORDER BY req_id DESC
    ", params=list(rv$me_id, rv$me_id))
    
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message="Aucun pari accepté"), rownames=FALSE, options=list(dom="t", paging=FALSE,scrollX = TRUE)))
    }
    
    u <- users_tbl() |> select(id, nom, prenom, pseudo)
    
    df <- df |>
      rowwise() |>
      mutate(
        equipe_1 = split_match_teams(match)[1],
        equipe_2 = split_match_teams(match)[2],
        adversaire_id = ifelse(from_id == rv$me_id, to_id, from_id),
        mon_choix = ifelse(
          from_id == rv$me_id,
          choix_from,
          ifelse(choix_from == equipe_1, equipe_2, equipe_1)
        ),
        statut_fr = "Accepté"
      ) |>
      ungroup() |>
      left_join(u, by = c("adversaire_id" = "id")) |>
      mutate(
        Adversaire = paste0(
          adversaire_id, " - ", nom, " ", prenom,
          ifelse(pseudo != "", paste0(" (@", pseudo, ")"), "")
        )
      ) |>
      transmute(
        Competition = competition,
        Match = match,
        Adversaire,
        `Mon choix` = mon_choix,
        Montant = amount,
        Statut = statut_fr
      )
    
    datatable(df, rownames=FALSE, options=list(dom="t", paging=FALSE, scrollY="420px",scrollX = TRUE, scrollCollapse=TRUE))
  })
  
  # ------------------ COMPETITIONS ------------------
  
  observeEvent(input$compet_main_tab, {
    rv$last_compet_tab <- input$compet_main_tab
  }, ignoreInit = TRUE)
  
  observeEvent(input$cdm_sub_tab, {
    rv$last_cdm_tab <- input$cdm_sub_tab
  }, ignoreInit = TRUE)
  
  observeEvent(input$can_sub_tab, {
    rv$last_can_tab <- input$can_sub_tab
  }, ignoreInit = TRUE)
  
  # -------- CDM --------
  matches_cdm_groupes <- reactive({
    df <- dbGetQuery(con, "
      SELECT match_id, competition, phase, groupe, journee, date_txt, heure_txt, equipe_1, equipe_2, remarque
      FROM matches
      WHERE competition='CDM' AND phase='Phase de groupes'
      ORDER BY groupe, journee, date_txt, heure_txt
    ")
    
    q <- str_trim(input$match_search %||% "")
    if (q != "") {
      qq <- tolower(q)
      df <- df |>
        mutate(.hay = tolower(paste(competition, phase, groupe, equipe_1, equipe_2, date_txt, heure_txt, remarque))) |>
        filter(str_detect(.hay, fixed(qq))) |>
        select(-.hay)
    }
    df
  })
  
  matches_cdm_finales <- reactive({
    df <- dbGetQuery(con, "
      SELECT match_id, competition, phase, date_txt, heure_txt, equipe_1, equipe_2, remarque
      FROM matches
      WHERE competition='CDM' AND phase <> 'Phase de groupes'
      ORDER BY match_id
    ")
    
    q <- str_trim(input$match_search %||% "")
    if (q != "") {
      qq <- tolower(q)
      df <- df |>
        mutate(.hay = tolower(paste(competition, phase, equipe_1, equipe_2, date_txt, heure_txt, remarque))) |>
        filter(str_detect(.hay, fixed(qq))) |>
        select(-.hay)
    }
    df
  })
  
  groups_list <- reactive({
    g <- unique(matches_cdm_groupes()$groupe)
    g <- g[!is.na(g) & g != ""]
    sort(g)
  })
  
  finals_phases <- reactive({
    ph <- unique(matches_cdm_finales()$phase)
    ph <- ph[!is.na(ph) & ph != ""]
    ph
  })
  
  output$cdm_groups_ui <- renderUI({
    gs <- groups_list()
    if (length(gs) == 0) return(p("Aucun match CDM groupes trouvé."))
    navset_tab(
      id = "cdm_groups_tab",
      !!!lapply(gs, function(g) {
        nav_panel(
          paste0("Groupe ", g),
          value = norm_names(g),
          DTOutput(paste0("cdm_g_", norm_names(g)))
        )
      })
    )
  })
  
  observeEvent(groups_list(), {
    gs <- groups_list()
    for (g in gs) {
      local({
        gg <- g
        out_id <- paste0("cdm_g_", norm_names(gg))
        
        output[[out_id]] <- renderDT({
          df <- matches_cdm_groupes() |>
            filter(groupe == gg) |>
            transmute(match_id, Date=date_txt, Heure=heure_txt, Equipe_1=equipe_1, Equipe_2=equipe_2)
          
          datatable(
            df, rownames=FALSE, selection="single",
            options=list(
              dom="t", paging=FALSE, scrollY="420px", scrollCollapse=TRUE,scrollX = TRUE,
              columnDefs = list(list(targets = 0, visible = FALSE))
            )
          )
        })
        
        observeEvent(input[[paste0(out_id, "_rows_selected")]], {
          sel <- input[[paste0(out_id, "_rows_selected")]]
          if (!length(sel)) return()
          
          row <- matches_cdm_groupes() |>
            filter(groupe == gg) |>
            slice(sel)
          
          rv$selected_match <- as.list(row[1, ])
        }, ignoreInit=TRUE)
      })
    }
  }, ignoreInit=FALSE)
  
  output$cdm_finals_ui <- renderUI({
    ph <- finals_phases()
    if (length(ph) == 0) return(p("Aucun match CDM finales trouvé."))
    navset_tab(
      id = "cdm_finals_tab",
      !!!lapply(ph, function(p) {
        nav_panel(
          p,
          value = norm_names(p),
          DTOutput(paste0("cdm_p_", norm_names(p)))
        )
      })
    )
  })
  
  observeEvent(finals_phases(), {
    ph <- finals_phases()
    for (p in ph) {
      local({
        pp <- p
        out_id <- paste0("cdm_p_", norm_names(pp))
        
        output[[out_id]] <- renderDT({
          df <- matches_cdm_finales() |>
            filter(phase == pp) |>
            transmute(match_id, Date=date_txt, Heure=heure_txt, Equipe_1=equipe_1, Equipe_2=equipe_2, Remarque=remarque)
          
          datatable(
            df, rownames=FALSE, selection="single",
            options=list(
              dom="t", paging=FALSE, scrollY="420px", scrollCollapse=TRUE,scrollX = TRUE,
              columnDefs = list(list(targets = 0, visible = FALSE))
            )
          )
        })
        
        observeEvent(input[[paste0(out_id, "_rows_selected")]], {
          sel <- input[[paste0(out_id, "_rows_selected")]]
          if (!length(sel)) return()
          
          row <- matches_cdm_finales() |>
            filter(phase == pp) |>
            slice(sel)
          
          rv$selected_match <- as.list(row[1, ])
        }, ignoreInit=TRUE)
      })
    }
  }, ignoreInit=FALSE)
  
  # -------- CAN --------
  matches_can_groupes <- reactive({
    df <- dbGetQuery(con, "
      SELECT match_id, competition, phase, groupe, journee, date_txt, heure_txt, equipe_1, equipe_2,
             score_1, score_2, tab_1, tab_2, winner, qualif_txt
      FROM matches
      WHERE competition='CAN' AND phase='Phase de groupes'
      ORDER BY groupe, journee, date_txt, heure_txt
    ")
    
    q <- str_trim(input$match_search %||% "")
    if (q != "") {
      qq <- tolower(q)
      df <- df |>
        mutate(.hay = tolower(paste(competition, phase, groupe, equipe_1, equipe_2, date_txt, heure_txt, winner, qualif_txt))) |>
        filter(str_detect(.hay, fixed(qq))) |>
        select(-.hay)
    }
    df
  })
  
  matches_can_finales <- reactive({
    df <- dbGetQuery(con, "
      SELECT match_id, competition, phase, date_txt, heure_txt, equipe_1, equipe_2,
             score_1, score_2, tab_1, tab_2, winner, qualif_txt
      FROM matches
      WHERE competition='CAN' AND phase <> 'Phase de groupes'
      ORDER BY match_id
    ")
    df$phase <- norm_can_phase(df$phase)
    
    q <- str_trim(input$match_search %||% "")
    if (q != "") {
      qq <- tolower(q)
      df <- df |>
        mutate(.hay = tolower(paste(competition, phase, equipe_1, equipe_2, date_txt, heure_txt, winner, qualif_txt))) |>
        filter(str_detect(.hay, fixed(qq))) |>
        select(-.hay)
    }
    df
  })
  
  can_groups_list <- reactive({
    g <- unique(matches_can_groupes()$groupe)
    g <- g[!is.na(g) & g != ""]
    sort(g)
  })
  
  output$can_groups_ui <- renderUI({
    gs <- can_groups_list()
    if (length(gs) == 0) return(p("Aucun match CAN poules trouvé."))
    
    navset_tab(
      id = "can_groups_tab",
      !!!lapply(gs, function(g) {
        label_g <- str_trim(g) |> toupper()
        label_g <- str_replace(label_g, "^GROUPE\\s*", "")
        nav_panel(paste0("Groupe ", label_g), value = norm_names(g),
                  DTOutput(paste0("can_g_", norm_names(g))))
      })
    )
  })
  
  observeEvent(can_groups_list(), {
    gs <- can_groups_list()
    for (g in gs) {
      local({
        gg <- g
        out_id <- paste0("can_g_", norm_names(gg))
        
        output[[out_id]] <- renderDT({
          df <- matches_can_groupes() |>
            filter(groupe == gg) |>
            transmute(
              match_id,
              Date = date_txt,
              Heure = heure_txt,
              Equipe_1 = equipe_1,
              Equipe_2 = equipe_2,
              Score = score_with_tab(score_1, score_2, tab_1, tab_2)
            )
          
          datatable(
            df, rownames=FALSE, selection="single",
            options=list(
              dom="t", paging=FALSE, scrollY="420px", scrollCollapse=TRUE,scrollX = TRUE,
              columnDefs = list(list(targets = 0, visible = FALSE))
            )
          )
        })
        
        observeEvent(input[[paste0(out_id, "_rows_selected")]], {
          sel <- input[[paste0(out_id, "_rows_selected")]]
          if (!length(sel)) return()
          
          row <- matches_can_groupes() |>
            filter(groupe == gg) |>
            slice(sel)
          
          rv$last_compet_tab <- "CAN"
          rv$last_can_tab <- "groups"
          rv$last_can_group <- isolate(input$can_groups_tab)  # optionnel mais utile
          
          rv$selected_match <- as.list(row[1, ])
        }, ignoreInit=TRUE)
      })
    }
  }, ignoreInit=FALSE)
  
  output$can_finals_ui <- renderUI({
    phases_order <- c("Huitièmes", "Quarts", "Demi-finales", "3e place", "Finale")
    
    navset_tab(
      id = "can_finals_tab",
      !!!lapply(phases_order, function(pp) {
        nav_panel(
          pp,
          value = norm_names(pp),
          DTOutput(paste0("can_p_", norm_names(pp)))
        )
      })
    )
  })
  
  observeEvent(TRUE, {
    phases_order <- c("Huitièmes", "Quarts", "Demi-finales", "3e place", "Finale")
    
    for (pp in phases_order) {
      local({
        ppp <- pp
        out_id <- paste0("can_p_", norm_names(ppp))
        
        output[[out_id]] <- renderDT({
          df <- matches_can_finales() |>
            filter(phase == ppp) |>
            transmute(
              match_id,
              Date = date_txt,
              Heure = heure_txt,
              Equipe_1 = equipe_1,
              Equipe_2 = equipe_2,
              Score = score_with_tab(score_1, score_2, tab_1, tab_2)
            )
          
          datatable(
            df, rownames=FALSE, selection="single",
            options=list(
              dom="t", paging=FALSE, scrollY="420px", scrollCollapse=TRUE,scrollX = TRUE,
              columnDefs = list(list(targets = 0, visible = FALSE))
            )
          )
        })
        
        observeEvent(input[[paste0(out_id, "_rows_selected")]], {
          sel <- input[[paste0(out_id, "_rows_selected")]]
          if (!length(sel)) return()
          
          row <- matches_can_finales() |>
            filter(phase == ppp) |>
            slice(sel)
          
          rv$last_compet_tab <- "CAN"
          rv$last_can_tab <- "finals"
          rv$last_can_phase <- isolate(input$can_finals_tab)  # optionnel mais utile
          
          rv$selected_match <- as.list(row[1, ])
        }, ignoreInit=TRUE)
      })
    }
  }, ignoreInit=FALSE)
  
  # -------- LDC --------
  matches_ldc_all <- reactive({
    df <- dbGetQuery(con, "
    SELECT match_id, competition, phase, groupe, journee, date_txt, heure_txt,
           leg_txt, equipe_1, pays_1, equipe_2, pays_2,
           score_1, score_2, tab_1, tab_2, winner, qualif_txt,
           pts_cum_1, pts_cum_2, qualif_eq1, qualif_eq2
    FROM matches
    WHERE competition='LDC'
    ORDER BY match_id
  ")
    
    q <- str_trim(input$match_search %||% "")
    if (q != "") {
      qq <- tolower(q)
      df <- df |>
        mutate(.hay = tolower(paste(competition, phase, journee, leg_txt, equipe_1, pays_1, equipe_2, pays_2, date_txt, heure_txt, winner, qualif_txt))) |>
        filter(str_detect(.hay, fixed(qq))) |>
        select(-.hay)
    }
    df
  })
  
  matches_ldc_bq <- reactive({
    matches_ldc_all() |> filter(phase == "Barrages de qualification")
  })
  
  matches_ldc_barr <- reactive({
    matches_ldc_all() |> filter(phase == "Barrages")
  })
  
  matches_ldc_journee <- function(j) {
    matches_ldc_all() |> filter(journee == j)
  }
  
  matches_ldc_pf <- function(phase_name) {
    matches_ldc_all() |> filter(phase == phase_name)
  }
  
  # UI Barrages qualif
  output$ldc_bq_ui <- renderUI({
    DTOutput("ldc_bq_dt")
  })
  
  output$ldc_bq_dt <- renderDT({
    df <- matches_ldc_bq() |>
      transmute(
        match_id,
        Date = date_txt,
        Heure = heure_txt,
        `Aller/Retour` = leg_txt,
        `Equipe 1` = equipe_1, `Pays 1` = pays_1,
        `Equipe 2` = equipe_2, `Pays 2` = pays_2,
        Résultat = score_with_tab(score_1, score_2, tab_1, tab_2),
        Gagnant = winner,
        Qualifié = qualif_txt
      )
    
    datatable(
      df, rownames = FALSE, selection = "single",
      options = list(dom="t", paging=FALSE, scrollY="420px", scrollCollapse=TRUE,scrollX = TRUE,
                     columnDefs = list(list(targets = 0, visible = FALSE)))
    )
  })
  
  observeEvent(input$ldc_bq_dt_rows_selected, {
    sel <- input$ldc_bq_dt_rows_selected
    if (!length(sel)) return()
    row <- matches_ldc_bq() |> slice(sel)
    rv$last_compet_tab <- "LDC"
    rv$last_ldc_tab <- "bq"
    rv$selected_match <- as.list(row[1, ])
  }, ignoreInit = TRUE)
  
  # UI Journées (J1..J8)
  output$ldc_jours_ui <- renderUI({
    navset_tab(
      id = "ldc_j_tab",
      nav_panel("J1", value="j1", DTOutput("ldc_j1_dt")),
      nav_panel("J2", value="j2", DTOutput("ldc_j2_dt")),
      nav_panel("J3", value="j3", DTOutput("ldc_j3_dt")),
      nav_panel("J4", value="j4", DTOutput("ldc_j4_dt")),
      nav_panel("J5", value="j5", DTOutput("ldc_j5_dt")),
      nav_panel("J6", value="j6", DTOutput("ldc_j6_dt")),
      nav_panel("J7", value="j7", DTOutput("ldc_j7_dt")),
      nav_panel("J8", value="j8", DTOutput("ldc_j8_dt"))
    )
  })
  
  render_ldc_j_dt <- function(j) {
    force(j)
    out_id <- paste0("ldc_j", j, "_dt")
    
    output[[out_id]] <- DT::renderDT({
      base <- matches_ldc_journee(j)
      
      if (j < 8) {
        df <- base |>
          dplyr::transmute(
            match_id,
            Date = date_txt,
            Heure = heure_txt,
            `Equipe 1` = equipe_1, `Pays 1` = pays_1,
            `Equipe 2` = equipe_2, `Pays 2` = pays_2,
            Résultat = score_with_tab(score_1, score_2, tab_1, tab_2),
            Gagnant = winner,
            `Pts cumulés Eq1` = pts_cum_1,
            `Pts cumulés Eq2` = pts_cum_2
          )
      } else {
        df <- base |>
          dplyr::transmute(
            match_id,
            Date = date_txt,
            Heure = heure_txt,
            `Equipe 1` = equipe_1, `Pays 1` = pays_1,
            `Equipe 2` = equipe_2, `Pays 2` = pays_2,
            Résultat = score_with_tab(score_1, score_2, tab_1, tab_2),
            Gagnant = winner,
            `Pts cumulés Eq1` = pts_cum_1,
            `Pts cumulés Eq2` = pts_cum_2,
            `Qualifié Eq1` = as.character(qualif_eq1),
            `Qualifié Eq2` = as.character(qualif_eq2)
          ) |>
          dplyr::mutate(
            `Qualifié Eq1` = dplyr::if_else(is.na(`Qualifié Eq1`), "", `Qualifié Eq1`),
            `Qualifié Eq2` = dplyr::if_else(is.na(`Qualifié Eq2`), "", `Qualifié Eq2`)
          )
      }
      
      DT::datatable(
        df,
        rownames = FALSE,
        selection = "single",
        class = "compact nowrap",
        options = list(
          dom = "t",
          paging = FALSE,
          scrollY = "420px",
          scrollCollapse = TRUE,
          scrollX = TRUE,
          columnDefs = list(list(targets = 0, visible = FALSE))
        )
      )
    })
    
    observeEvent(input[[paste0(out_id, "_rows_selected")]], {
      sel <- input[[paste0(out_id, "_rows_selected")]]
      if (!length(sel)) return()
      
      row <- matches_ldc_journee(j) |> dplyr::slice(sel)
      
      rv$last_compet_tab <- "LDC"
      rv$last_ldc_tab <- "jours"
      rv$last_ldc_j <- paste0("j", j)
      
      rv$selected_match <- as.list(row[1, ])
    }, ignoreInit = TRUE)
  }
  
  for (j in 1:8) render_ldc_j_dt(j)
  
  # UI Barrages
  output$ldc_barr_ui <- renderUI({
    DTOutput("ldc_barr_dt")
  })
  
  output$ldc_barr_dt <- renderDT({
    df <- matches_ldc_barr() |>
      transmute(
        match_id,
        Date = date_txt,
        Heure = heure_txt,
        `Aller/Retour` = leg_txt,
        `Equipe 1` = equipe_1, `Pays 1` = pays_1,
        `Equipe 2` = equipe_2, `Pays 2` = pays_2,
        Résultat = score_with_tab(score_1, score_2, tab_1, tab_2),
        Gagnant = winner,
        Qualifié = qualif_txt
      )
    
    datatable(
      df, rownames = FALSE, selection = "single",
      options = list(dom="t", paging=FALSE, scrollY="420px", scrollCollapse=TRUE,scrollX = TRUE,
                     columnDefs = list(list(targets = 0, visible = FALSE)))
    )
  })
  
  observeEvent(input$ldc_barr_dt_rows_selected, {
    sel <- input$ldc_barr_dt_rows_selected
    if (!length(sel)) return()
    row <- matches_ldc_barr() |> slice(sel)
    rv$last_compet_tab <- "LDC"
    rv$last_ldc_tab <- "barr"
    rv$selected_match <- as.list(row[1, ])
  }, ignoreInit = TRUE)
  
  # UI Phases finales
  output$ldc_pf_ui <- renderUI({
    navset_tab(
      id = "ldc_pf_tab",
      nav_panel("Huitièmes", value="huitiemes", DTOutput("ldc_pf_8_dt")),
      nav_panel("Quarts", value="quarts", DTOutput("ldc_pf_q_dt")),
      nav_panel("Demi", value="demi", DTOutput("ldc_pf_d_dt")),
      nav_panel("Finale", value="finale", DTOutput("ldc_pf_f_dt"))
    )
  })
  
  render_ldc_pf_dt <- function(phase_name, out_id) {
    output[[out_id]] <- renderDT({
      df <- matches_ldc_pf(phase_name) |>
        transmute(
          match_id,
          Date = date_txt,
          Heure = heure_txt,
          `Aller/Retour` = leg_txt,
          `Equipe 1` = equipe_1, `Pays 1` = pays_1,
          `Equipe 2` = equipe_2, `Pays 2` = pays_2,
          Résultat = score_with_tab(score_1, score_2, tab_1, tab_2),
          Gagnant = winner,
          Qualifié = qualif_txt
        )
      
      datatable(
        df, rownames=FALSE, selection="single",
        options=list(dom="t", paging=FALSE, scrollY="420px", scrollCollapse=TRUE,scrollX = TRUE,
                     columnDefs = list(list(targets = 0, visible = FALSE)))
      )
    })
    
    observeEvent(input[[paste0(out_id, "_rows_selected")]], {
      sel <- input[[paste0(out_id, "_rows_selected")]]
      if (!length(sel)) return()
      row <- matches_ldc_pf(phase_name) |> slice(sel)
      
      rv$last_compet_tab <- "LDC"
      rv$last_ldc_tab <- "pf"
      rv$last_ldc_pf <- isolate(input$ldc_pf_tab)
      
      rv$selected_match <- as.list(row[1, ])
    }, ignoreInit = TRUE)
  }
  
  render_ldc_pf_dt("Huitièmes", "ldc_pf_8_dt")
  render_ldc_pf_dt("Quarts", "ldc_pf_q_dt")
  render_ldc_pf_dt("Demi-finales", "ldc_pf_d_dt")
  render_ldc_pf_dt("Finale", "ldc_pf_f_dt")
  
  # -------- Retour depuis une fiche match --------
  observeEvent(input$btn_back_matches, {
    rv$selected_match <- NULL
    
    # ✅ on capture les valeurs réactives ICI (dans l'observer)
    last_compet_tab <- isolate(rv$last_compet_tab)
    last_cdm_tab    <- isolate(rv$last_cdm_tab)
    last_can_tab    <- isolate(rv$last_can_tab)
    
    last_cdm_group  <- isolate(rv$last_cdm_group)
    last_cdm_phase  <- isolate(rv$last_cdm_phase)
    
    last_can_group  <- isolate(rv$last_can_group)
    last_can_phase  <- isolate(rv$last_can_phase)
    
    last_ldc_tab <- isolate(rv$last_ldc_tab)
    last_ldc_j   <- isolate(rv$last_ldc_j)
    last_ldc_pf  <- isolate(rv$last_ldc_pf)
    
    session$onFlushed(function() {
      
      # 1) CDM / CAN / LDC
      if (!is.null(last_compet_tab)) {
        updateTabsetPanel(session, "compet_main_tab", selected = last_compet_tab)
      }
      
      # 2) Poules / Finales + 3) Sous-sous onglet
      if (identical(last_compet_tab, "CDM")) {
        
        if (!is.null(last_cdm_tab)) {
          updateTabsetPanel(session, "cdm_sub_tab", selected = last_cdm_tab)
        }
        
        if (identical(last_cdm_tab, "groups") && !is.null(last_cdm_group)) {
          updateTabsetPanel(session, "cdm_groups_tab", selected = last_cdm_group)
        }
        
        if (identical(last_cdm_tab, "finals") && !is.null(last_cdm_phase)) {
          updateTabsetPanel(session, "cdm_finals_tab", selected = last_cdm_phase)
        }
        
      } else if (identical(last_compet_tab, "CAN")) {
        
        if (!is.null(last_can_tab)) {
          updateTabsetPanel(session, "can_sub_tab", selected = last_can_tab)
        }
        
        if (identical(last_can_tab, "groups") && !is.null(last_can_group)) {
          updateTabsetPanel(session, "can_groups_tab", selected = last_can_group)
        }
        
        if (identical(last_can_tab, "finals") && !is.null(last_can_phase)) {
          updateTabsetPanel(session, "can_finals_tab", selected = last_can_phase)
        }
      } else if (identical(last_compet_tab, "LDC")) {
        
        if (!is.null(last_ldc_tab)) {
          updateTabsetPanel(session, "ldc_sub_tab", selected = last_ldc_tab)
        }
        
        if (identical(last_ldc_tab, "jours") && !is.null(last_ldc_j)) {
          updateTabsetPanel(session, "ldc_j_tab", selected = last_ldc_j)
        }
        
        if (identical(last_ldc_tab, "pf") && !is.null(last_ldc_pf)) {
          updateTabsetPanel(session, "ldc_pf_tab", selected = last_ldc_pf)
        }
      }
    }, once = TRUE)
  })
  
  # -------- Envoi demande pari (CDM uniquement ; CAN est bloquée dans output$main_ui) --------
  observeEvent(input$btn_send_bet, {
    if (is.null(rv$selected_match)) return()
    if (is.null(input$friend_target) || input$friend_target=="") return()
    
    amount <- input$bet_amount %||% 0
    if (is.na(amount) || amount <= 0) return()
    
    w <- wallet()
    if (nrow(w) == 0 || w$solde[[1]] < amount) {
      showModal(modalDialog(
        title = "Solde insuffisant",
        paste0("Tu as ", w$solde[[1]], " € mais tu veux miser ", amount, " €."),
        easyClose = TRUE
      ))
      return()
    }
    
    mm <- rv$selected_match
    match_txt <- paste0(mm$equipe_1, " - ", mm$equipe_2)
    choix <- input$pick_team
    
    dbExecute(con, "
      INSERT INTO requests (from_id, to_id, competition, match, choix_from, amount, status, created_at, ref_req_id)
      VALUES (?,?,?,?,?,?,'pending',?,NULL)
    ", params=list(rv$me_id, input$friend_target, mm$competition, match_txt, choix, amount, now_txt()))
    
    bump_refresh()
    showModal(modalDialog(title="Demande envoyée", "OK ✅", easyClose=TRUE))
    rv$selected_match <- NULL
  })
  
  # ------------------ PROFIL ------------------
  observeEvent(input$btn_profile, {
    req(rv$me_id)
    
    m <- me()
    current_emoji <- m$emoji[[1]]
    if (is.na(current_emoji) || current_emoji == "") current_emoji <- "👩"
    
    showModal(modalDialog(
      title = "Mon profil",
      
      radioButtons(
        "profile_emoji",
        "Genre",
        choices = c("👩"="👩", "👨"="👨"),
        selected = current_emoji,
        inline = TRUE
      ),
      
      textInput("profile_pseudo", "Pseudo", value = m$pseudo[[1]]),
      textInput("profile_email", "Email", value = m$email[[1]] %||% ""),
      textInput("profile_phone", "Numéro", value = m$phone[[1]] %||% ""),
      passwordInput("profile_mdp", "Mot de passe", value = m$mdp[[1]] %||% ""),
      
      div(style="display:flex; gap:10px; margin-top:10px;",
          actionButton("btn_save_profile", "Enregistrer", class="btn btn-primary"),
          actionButton("btn_logout", "Se déconnecter", class="btn btn-outline-light")
      ),
      
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$btn_save_profile, {
    p <- str_trim(input$profile_pseudo)
    if (is.null(p) || p == "") return()
    
    dbExecute(
      con,
      "UPDATE users SET pseudo=?, email=?, phone=?, mdp=?, emoji=? WHERE id=?",
      params = list(
        p,
        input$profile_email,
        input$profile_phone,
        input$profile_mdp,
        input$profile_emoji,
        rv$me_id
      )
    )
    
    removeModal()
  })
  
  observeEvent(input$btn_logout, {
    removeModal()
    rv$me_id <- NULL
    rv$selected_match <- NULL
  })
  
  observe({
    input$world_map_zoom
    
    if (is.null(input$world_map_zoom)) return()
    
    proxy <- leafletProxy("world_map")
    
    if (input$world_map_zoom >= 4) {
      proxy |>
        hideGroup("labels_big") |>
        showGroup("labels_zoom")
    } else {
      proxy |>
        showGroup("labels_big") |>
        hideGroup("labels_zoom")
    }
  })
  
  # ------------------ retour/close + fin server ------------------
  session$onSessionEnded(function() {
    try(dbDisconnect(con), silent = TRUE)
  })
}

shinyApp(ui, server)