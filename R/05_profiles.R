# R/05_profiles.R
# R/05_profiles.R
build_profiles <- function(games, normalize_by_players = TRUE) {
  if (length(games) == 0) {
    return(list(players = data.frame(), villains = data.frame()))
  }
  
  rows_p <- list()
  rows_v <- list()
  
  for (g in games) {
    seats <- g$seats
    if (is.null(seats) || !is.data.frame(seats)) next
    
    seats$player <- trimws(as.character(seats$player))
    seats$villainId <- trimws(as.character(seats$villainId))
    seats <- seats[seats$player != "" & seats$villainId != "", , drop = FALSE]
    
    n <- nrow(seats)
    if (n < 2 || n > 6) next
    
    winnerP <- trimws(g$winnerPlayer %||% "")
    if (!winnerP %in% seats$player) next
    
    dur <- suppressWarnings(as.integer(g$durationMin %||% NA_integer_))
    dur_metric <- if (!is.na(dur) && dur > 0) {
      if (normalize_by_players) dur / n else dur
    } else NA_real_
    
    chance <- 1 / n
    winnerV <- seats$villainId[match(winnerP, seats$player)]
    
    # players
    for (i in seq_len(n)) {
      p <- seats$player[i]
      win <- (p == winnerP)
      rows_p[[length(rows_p) + 1]] <- data.frame(
        key = p,
        Win = as.integer(win),
        Chance = chance,
        duration = dur_metric,
        stringsAsFactors = FALSE
      )
    }
    
    # villains (each villain in seats is a participant; winner villain = winnerV)
    for (i in seq_len(n)) {
      v <- seats$villainId[i]
      win <- (v == winnerV)
      rows_v[[length(rows_v) + 1]] <- data.frame(
        key = v,
        Win = as.integer(win),
        Chance = chance,
        duration = dur_metric,
        stringsAsFactors = FALSE
      )
    }
  }
  
  dfp <- if (length(rows_p) > 0) do.call(rbind, rows_p) else data.frame()
  dfv <- if (length(rows_v) > 0) do.call(rbind, rows_v) else data.frame()
  
  agg_profile <- function(df) {
    if (nrow(df) == 0) return(data.frame())
    
    Games <- aggregate(Win ~ key, df, length); names(Games)[2] <- "Games"
    Wins  <- aggregate(Win ~ key, df, sum);   names(Wins)[2]  <- "Wins"
    ExpW  <- aggregate(Chance ~ key, df, sum); names(ExpW)[2] <- "ExpWins"
    
    out <- Reduce(function(a,b) merge(a,b, by="key", all=TRUE), list(Games, Wins, ExpW))
    out$Winrate <- ifelse(out$Games > 0, out$Wins / out$Games, NA_real_)
    
    # ✅ equitable across N: baseline = 1/N per game
    out$AboveChance <- out$Wins - out$ExpWins
    out$AdjWinrate  <- ifelse(out$Games > 0, out$AboveChance / out$Games, NA_real_)
    out$SkillIndex  <- ifelse(out$ExpWins > 0, out$Wins / out$ExpWins, NA_real_)  # >1 = better than chance
    
    win_d <- df[df$Win == 1, , drop=FALSE]
    los_d <- df[df$Win == 0, , drop=FALSE]
    
    mean_win <- aggregate(duration ~ key, win_d, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm=TRUE))
    mean_los <- aggregate(duration ~ key, los_d, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm=TRUE))
    names(mean_win)[2] <- "AvgWin"
    names(mean_los)[2] <- "AvgLoss"
    
    out <- merge(out, mean_win, by="key", all.x=TRUE)
    out <- merge(out, mean_los, by="key", all.x=TRUE)
    
    out
  }
  
  list(players = agg_profile(dfp), villains = agg_profile(dfv))
}

build_villain_detail <- function(games, villain_id, player_count = NULL) {
  if (length(games) == 0 || is.null(villain_id) || villain_id == "") {
    return(list(
      summary = data.frame(
        villainId = character(),
        Games = integer(),
        Wins = integer(),
        Winrate = numeric(),
        stringsAsFactors = FALSE
      ),
      by_player = data.frame(
        Rank = integer(),
        Player = character(),
        Games = integer(),
        Wins = integer(),
        Winrate = numeric(),
        stringsAsFactors = FALSE
      )
    ))
  }
  
  rows <- list()
  
  for (g in games) {
    seats <- g$seats
    if (is.null(seats) || !is.data.frame(seats)) next
    
    # Compatibilité ancien format / nouveau format
    if (!("villainId" %in% names(seats))) {
      if ("villain" %in% names(seats)) {
        seats$villainId <- villain_to_id(seats$villain)
      } else {
        seats$villainId <- ""
      }
    }
    
    seats$player <- trimws(as.character(seats$player))
    seats$villainId <- villain_to_id(seats$villainId)
    seats <- seats[seats$player != "" & seats$villainId != "", c("player", "villainId"), drop = FALSE]
    
    n <- nrow(seats)
    if (n < 2 || n > 6) next
    if (!is.null(player_count) && !is.na(player_count) && n != as.integer(player_count)) next
    
    winnerP <- trimws(g$winnerPlayer %||% "")
    if (!winnerP %in% seats$player) next
    
    idx <- which(seats$villainId == villain_id)
    if (length(idx) == 0) next
    
    winnerV <- seats$villainId[match(winnerP, seats$player)]
    
    for (i in idx) {
      rows[[length(rows) + 1]] <- data.frame(
        player = seats$player[i],
        Games = 1L,
        Wins = as.integer(winnerV == villain_id),
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(rows) == 0) {
    return(list(
      summary = data.frame(
        villainId = villain_id,
        Games = 0L,
        Wins = 0L,
        Winrate = NA_real_,
        stringsAsFactors = FALSE
      ),
      by_player = data.frame(
        Rank = integer(),
        Player = character(),
        Games = integer(),
        Wins = integer(),
        Winrate = numeric(),
        stringsAsFactors = FALSE
      )
    ))
  }
  
  df <- do.call(rbind, rows)
  
  by_player <- aggregate(cbind(Games, Wins) ~ player, data = df, FUN = sum)
  by_player$Winrate <- ifelse(by_player$Games > 0, by_player$Wins / by_player$Games, NA_real_)
  by_player <- by_player[order(-by_player$Winrate, -by_player$Games, by_player$player), , drop = FALSE]
  by_player$Rank <- seq_len(nrow(by_player))
  by_player <- by_player[, c("Rank", "player", "Games", "Wins", "Winrate")]
  names(by_player) <- c("Rank", "Player", "Games", "Wins", "Winrate")
  
  summary <- data.frame(
    villainId = villain_id,
    Games = sum(df$Games),
    Wins = sum(df$Wins),
    Winrate = sum(df$Wins) / sum(df$Games),
    stringsAsFactors = FALSE
  )
  
  list(
    summary = summary,
    by_player = by_player
  )
}
