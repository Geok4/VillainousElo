# R/04_elo.R

compute_elo <- function(
    games,
    base_rating = 1500,
    k = 32,
    alpha = 0.5,
    scale = 400,
    normalize_k_by_n = TRUE,  # ✅ équité 2–6 joueurs
    return_log = TRUE
) {
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
    return(list(
      playerRatings = playerR, villainRatings = villainR,
      playerGames = playerN, villainGames = villainN,
      log = data.frame()
    ))
  }
  
  games_sorted <- games[order(sapply(games, `[[`, "playedAt"), sapply(games, `[[`, "id"))]
  
  for (g in games_sorted) {
    seats <- g$seats
    if (is.null(seats) || !is.data.frame(seats)) next
    
    seats$player <- trimws(as.character(seats$player))
    seats$villainId <- trimws(as.character(seats$villainId))
    seats <- seats[seats$player != "" & seats$villainId != "", , drop = FALSE]
    
    n <- nrow(seats)
    if (n < 2 || n > 6) next
    
    winner <- trimws(g$winnerPlayer %||% "")
    if (!winner %in% seats$player) next
    
    dur_min <- suppressWarnings(as.integer(g$durationMin %||% NA_integer_))
    
    # init entities
    for (i in seq_len(n)) {
      ensure_player(seats$player[i])
      ensure_villain(seats$villainId[i])
    }
    
    # combined rating per seat
    combined <- numeric(n)
    p_before <- numeric(n)
    v_before <- numeric(n)
    
    for (i in seq_len(n)) {
      p <- seats$player[i]
      v <- seats$villainId[i]
      p_before[i] <- playerR[p]
      v_before[i] <- villainR[v]
      combined[i] <- p_before[i] + v_before[i]
    }
    
    # softmax probabilities
    q <- 10^(combined / scale)
    probs <- q / sum(q)
    
    # observed scores (1 winner else 0)
    S <- ifelse(seats$player == winner, 1, 0)
    
    # ✅ K normalized by N to keep “expected winner gain” ~ K/2 for any N
    K_eff <- if (normalize_k_by_n) k * (n / (2 * (n - 1))) else k
    
    # combined deltas (zero-sum)
    d_combined <- K_eff * (S - probs)
    
    # apply split (player vs villain)
    for (i in seq_len(n)) {
      p <- seats$player[i]
      v <- seats$villainId[i]
      
      d_p <- alpha * d_combined[i]
      d_v <- (1 - alpha) * d_combined[i]
      
      playerR[p] <- playerR[p] + d_p
      villainR[v] <- villainR[v] + d_v
      
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
          combined_before = combined[i],
          p_win = probs[i],
          K_eff = K_eff,
          d_combined = d_combined[i],
          d_player = d_p,
          d_villain = d_v,
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
