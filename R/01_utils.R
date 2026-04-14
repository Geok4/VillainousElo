# R/01_utils.R
`%||%` <- function(a, b) if (is.null(a)) b else a
clamp01 <- function(x) max(0, min(1, x))

# Vectorized (fixes 'length = ... in coercion to logical(1)')
fmt_h_min <- function(minutes) {
  vapply(minutes, function(m) {
    if (length(m) == 0 || is.na(m)) return("—")
    m <- as.integer(round(m))
    h <- m %/% 60
    mm <- m %% 60
    paste0(h, " h ", sprintf("%02d", mm), " min")
  }, character(1))
}

expected_score <- function(ra, rb, scale = 400) {
  1 / (1 + 10 ^ ((rb - ra) / scale))
}

save_rds_safe <- function(obj, path) try(saveRDS(obj, path), silent = TRUE)

load_rds_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  out <- try(readRDS(path), silent = TRUE)
  if (inherits(out, "try-error")) return(NULL)
  out
}

normalize_games <- function(games) {
  lapply(games, function(g) {
    if (is.null(g$durationMin)) g$durationMin <- NA_integer_
    
    seats <- g$seats
    if (!is.data.frame(seats)) {
      if (is.list(seats)) {
        players <- sapply(seats, function(s) s$player %||% "")
        vraw <- sapply(seats, function(s) s$villainId %||% (s$villain %||% ""))
        seats <- data.frame(player = players, villainId = vraw, stringsAsFactors = FALSE)
      } else {
        seats <- data.frame(player = character(), villainId = character(), stringsAsFactors = FALSE)
      }
    }
    
    if (!("villainId" %in% names(seats))) {
      if ("villain" %in% names(seats)) seats$villainId <- seats$villain
      else seats$villainId <- ""
    }
    
    seats$player <- trimws(as.character(seats$player))
    seats$villainId <- villain_to_id(seats$villainId)
    
    seats <- seats[, c("player", "villainId"), drop = FALSE]
    g$seats <- seats
    g
  })
}
