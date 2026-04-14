# R/05_profiles.R
build_profiles <- function(games, normalize_by_players = TRUE) {
  if (length(games) == 0) {
    return(list(players = data.frame(), villains = data.frame()))
  }
  
  rows_p <- list()
  rows_v <- list()
  
  for (g in games) {
    seats <- g$seats
    if (is.null(seats) || !is.data.frame(seats) || nrow(seats) < 2 || nrow(seats) > 6) next
    
    n <- nrow(seats)
    winnerP <- trimws(g$winnerPlayer %||% "")
    if (!winnerP %in% seats$player) next
    
    dur <- suppressWarnings(as.integer(g$durationMin %||% NA_integer_))
    dur_metric <- if (!is.na(dur) && dur > 0) {
      if (normalize_by_players) dur / n else dur
    } else NA_real_
    
    winnerV <- seats$villainId[match(winnerP, seats$player)]
    
    for (i in seq_len(n)) {
      p <- seats$player[i]
      rows_p[[length(rows_p) + 1]] <- data.frame(
        key = p, isWinner = (p == winnerP), duration = dur_metric, stringsAsFactors = FALSE
      )
    }
    
    for (i in seq_len(n)) {
      v <- seats$villainId[i]
      rows_v[[length(rows_v) + 1]] <- data.frame(
        key = v, isWinner = (v == winnerV), duration = dur_metric, stringsAsFactors = FALSE
      )
    }
  }
  
  dfp <- if (length(rows_p) > 0) do.call(rbind, rows_p) else data.frame()
  dfv <- if (length(rows_v) > 0) do.call(rbind, rows_v) else data.frame()
  
  agg_profile <- function(df) {
    if (nrow(df) == 0) return(data.frame())
    games <- aggregate(isWinner ~ key, df, length); names(games)[2] <- "Games"
    wins  <- aggregate(isWinner ~ key, df, sum);   names(wins)[2]  <- "Wins"
    out <- merge(games, wins, by = "key", all = TRUE)
    out$Winrate <- ifelse(out$Games > 0, out$Wins / out$Games, NA_real_)
    
    win_d <- df[df$isWinner == TRUE, , drop = FALSE]
    los_d <- df[df$isWinner == FALSE, , drop = FALSE]
    
    mean_win <- aggregate(duration ~ key, win_d, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
    mean_los <- aggregate(duration ~ key, los_d, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
    
    names(mean_win)[2] <- "AvgWin"
    names(mean_los)[2] <- "AvgLoss"
    
    out <- merge(out, mean_win, by = "key", all.x = TRUE)
    out <- merge(out, mean_los, by = "key", all.x = TRUE)
    
    out
  }
  
  list(players = agg_profile(dfp), villains = agg_profile(dfv))
}
