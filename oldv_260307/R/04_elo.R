# R/04_elo.R
compute_elo <- function(games, base_rating = 0, k = 32, alpha = 0.7, scale = 400, return_log = TRUE) {
  alpha <- clamp01(alpha)
  
  playerR <- numeric(); names(playerR) <- character()
  villainR <- numeric(); names(villainR) <- character()
  playerN <- integer(); names(playerN) <- character()
  villainN <- integer(); names(villainN) <- character()
  
  log_rows <- list()
  
  ensure_player <- function(p) {
    if (!p %in% names(playerR)) playerR[p] <<- base_rating
    if (!p %in% names(playerN)) playerN[p] <<- 0L
  }
  ensure_villain <- function(v) {
    if (!v %in% names(villainR)) villainR[v] <<- base_rating
    if (!v %in% names(villainN)) villainN[v] <<- 0L
  }
  
  if (length(games) == 0) {
    return(list(playerRatings = playerR, villainRatings = villainR, playerGames = playerN, villainGames = villainN, log = data.frame()))
  }
  
  games_sorted <- games[order(sapply(games, `[[`, "playedAt"), sapply(games, `[[`, "id"))]
  
  for (g in games_sorted) {
    seats <- g$seats
    if (is.null(seats) || !is.data.frame(seats) || nrow(seats) < 2 || nrow(seats) > 6) next
    
    seats$player <- trimws(seats$player)
    seats$villainId <- trimws(seats$villainId)
    seats <- seats[seats$player != "" & seats$villainId != "", , drop = FALSE]
    n <- nrow(seats)
    if (n < 2 || n > 6) next
    
    winner <- trimws(g$winnerPlayer %||% "")
    if (!winner %in% seats$player) next
    
    dur_min <- suppressWarnings(as.integer(g$durationMin %||% NA_integer_))
    
    for (i in seq_len(n)) {
      ensure_player(seats$player[i])
      ensure_villain(seats$villainId[i])
    }
    
    combined <- setNames(numeric(n), seats$player)
    for (i in seq_len(n)) {
      p <- seats$player[i]
      v <- seats$villainId[i]
      combined[p] <- playerR[p] + villainR[v]
    }
    
    cW <- combined[winner]
    k_eff <- k / (n - 1)
    
    deltaCombined <- setNames(rep(0, n), seats$player)
    for (i in seq_len(n)) {
      pJ <- seats$player[i]
      if (pJ == winner) next
      eW <- expected_score(cW, combined[pJ], scale = scale)
      term <- 1 - eW
      deltaCombined[winner] <- deltaCombined[winner] + term
      deltaCombined[pJ] <- deltaCombined[pJ] - term
    }
    
    for (i in seq_len(n)) {
      p <- seats$player[i]
      v <- seats$villainId[i]
      d_combined <- k_eff * deltaCombined[p]
      d_player <- alpha * d_combined
      d_villain <- (1 - alpha) * d_combined
      
      playerR[p] <- playerR[p] + d_player
      villainR[v] <- villainR[v] + d_villain
      
      playerN[p] <- playerN[p] + 1L
      villainN[v] <- villainN[v] + 1L
      
      if (return_log) {
        log_rows[[length(log_rows) + 1]] <- data.frame(
          gameId = g$id,
          playedAt = g$playedAt,
          durationMin = dur_min,
          nPlayers = n,
          player = p,
          villainId = v,
          isWinner = (p == winner),
          d_combined = d_combined,
          d_player = d_player,
          d_villain = d_villain,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  log_df <- if (return_log && length(log_rows) > 0) do.call(rbind, log_rows) else data.frame()
  
  list(
    playerRatings = playerR,
    villainRatings = villainR,
    playerGames = playerN,
    villainGames = villainN,
    log = log_df
  )
}

make_leaderboard <- function(ratings, games_count) {
  if (length(ratings) == 0) return(data.frame(key = character(), Elo = integer(), Games = integer()))
  df <- data.frame(
    key = names(ratings),
    Elo = as.integer(round(as.numeric(ratings))),
    Games = as.integer(games_count[names(ratings)]),
    stringsAsFactors = FALSE
  )
  df <- df[order(-df$Elo, -df$Games, df$key), , drop = FALSE]
  df
}
