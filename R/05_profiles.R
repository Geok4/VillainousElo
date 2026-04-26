# R/05_profiles.R

profile_rows_from_log <- function(elo_log, normalize_by_players = TRUE) {
  if (is.null(elo_log) || !is.data.frame(elo_log) || nrow(elo_log) == 0) {
    return(data.frame())
  }

  df <- elo_log
  df$player <- trimws(as.character(df$player %||% ""))
  df$villainId <- villain_to_id(df$villainId %||% "")
  df$nPlayers <- suppressWarnings(as.integer(df$nPlayers %||% NA_integer_))
  df$Win <- as.integer(as.logical(df$isWinner %||% FALSE))
  df$ExpectedWin <- suppressWarnings(as.numeric(df$p_win_villain %||% NA_real_))
  df$ChanceWin <- ifelse(!is.na(df$nPlayers) & df$nPlayers > 0, 1 / df$nPlayers, NA_real_)
  df$ExpectedWin[is.na(df$ExpectedWin)] <- df$ChanceWin[is.na(df$ExpectedWin)]

  dur <- suppressWarnings(as.numeric(df$durationMin %||% NA_real_))
  if (normalize_by_players) {
    df$duration <- ifelse(
      !is.na(dur) & dur > 0 & !is.na(df$nPlayers) & df$nPlayers > 0,
      dur / df$nPlayers,
      NA_real_
    )
  } else {
    df$duration <- ifelse(!is.na(dur) & dur > 0, dur, NA_real_)
  }

  keep <- !is.na(df$nPlayers) &
    df$nPlayers >= 2L & df$nPlayers <= 6L &
    df$player != "" &
    df$villainId != ""

  df <- df[keep, c("gameId", "nPlayers", "player", "villainId", "Win", "ExpectedWin", "ChanceWin", "duration"), drop = FALSE]
  rownames(df) <- NULL
  df
}

aggregate_duration_mean <- function(df, column_name) {
  if (nrow(df) == 0) {
    out <- data.frame(key = character(), value = numeric(), stringsAsFactors = FALSE)
  } else {
    out <- aggregate(
      duration ~ key,
      df,
      function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
    )
  }
  names(out)[2] <- column_name
  out
}

aggregate_profile_metrics <- function(df, key_col) {
  if (nrow(df) == 0) return(data.frame())

  rows <- data.frame(
    key = as.character(df[[key_col]]),
    Win = as.integer(df$Win),
    ExpectedWin = as.numeric(df$ExpectedWin),
    ChanceWin = as.numeric(df$ChanceWin),
    duration = as.numeric(df$duration),
    stringsAsFactors = FALSE
  )

  Games <- aggregate(Win ~ key, rows, length)
  names(Games)[2] <- "Games"

  Wins <- aggregate(Win ~ key, rows, sum)
  names(Wins)[2] <- "Wins"

  ExpectedWins <- aggregate(ExpectedWin ~ key, rows, sum)
  names(ExpectedWins)[2] <- "ExpectedWins"

  ChanceWins <- aggregate(ChanceWin ~ key, rows, sum)
  names(ChanceWins)[2] <- "ChanceWins"

  out <- Reduce(
    function(a, b) merge(a, b, by = "key", all = TRUE),
    list(Games, Wins, ExpectedWins, ChanceWins)
  )

  out$Winrate <- ifelse(out$Games > 0, out$Wins / out$Games, NA_real_)
  out$BaselineWinrate <- ifelse(out$Games > 0, out$ChanceWins / out$Games, NA_real_)
  out$ExpectedWinrate <- ifelse(out$Games > 0, out$ExpectedWins / out$Games, NA_real_)
  out$DeltaWinrate <- ifelse(out$Games > 0, out$Winrate - out$ExpectedWinrate, NA_real_)
  out$AdjustedWinrate <- ifelse(
    out$Games > 0,
    out$Winrate - out$ExpectedWinrate + out$BaselineWinrate,
    NA_real_
  )

  out$AboveChance <- out$Wins - out$ChanceWins
  out$ChanceAdjWinrate <- ifelse(out$Games > 0, out$AboveChance / out$Games, NA_real_)
  out$AdjWinrate <- out$AdjustedWinrate
  out$SkillIndex <- ifelse(out$ExpectedWins > 0, out$Wins / out$ExpectedWins, NA_real_)

  win_d <- rows[rows$Win == 1L, c("key", "duration"), drop = FALSE]
  los_d <- rows[rows$Win == 0L, c("key", "duration"), drop = FALSE]

  out <- merge(out, aggregate_duration_mean(win_d, "AvgWin"), by = "key", all.x = TRUE)
  out <- merge(out, aggregate_duration_mean(los_d, "AvgLoss"), by = "key", all.x = TRUE)

  out
}

build_profiles <- function(elo_log, normalize_by_players = TRUE) {
  rows <- profile_rows_from_log(elo_log, normalize_by_players = normalize_by_players)
  if (nrow(rows) == 0) {
    return(list(players = data.frame(), villains = data.frame()))
  }

  list(
    players = aggregate_profile_metrics(rows, "player"),
    villains = aggregate_profile_metrics(rows, "villainId")
  )
}

build_villain_detail <- function(elo_log, villain_id, player_count = NULL, normalize_by_players = TRUE) {
  empty <- list(
    summary = data.frame(
      villainId = character(),
      Games = integer(),
      Wins = integer(),
      Winrate = numeric(),
      AdjustedWinrate = numeric(),
      ExpectedWinrate = numeric(),
      DeltaWinrate = numeric(),
      stringsAsFactors = FALSE
    ),
    by_player = data.frame(
      Rank = integer(),
      Player = character(),
      Games = integer(),
      Wins = integer(),
      Winrate = numeric(),
      AdjustedWinrate = numeric(),
      ExpectedWinrate = numeric(),
      DeltaWinrate = numeric(),
      stringsAsFactors = FALSE
    )
  )

  if (is.null(villain_id) || villain_id == "") return(empty)

  rows <- profile_rows_from_log(elo_log, normalize_by_players = normalize_by_players)
  if (nrow(rows) == 0) return(empty)

  if (!is.null(player_count) && !is.na(player_count)) {
    rows <- rows[rows$nPlayers == as.integer(player_count), , drop = FALSE]
  }
  rows <- rows[rows$villainId == villain_id, , drop = FALSE]

  if (nrow(rows) == 0) {
    empty$summary <- data.frame(
      villainId = villain_id,
      Games = 0L,
      Wins = 0L,
      Winrate = NA_real_,
      AdjustedWinrate = NA_real_,
      ExpectedWinrate = NA_real_,
      DeltaWinrate = NA_real_,
      stringsAsFactors = FALSE
    )
    return(empty)
  }

  by_player <- aggregate_profile_metrics(rows, "player")
  by_player <- by_player[order(-by_player$AdjustedWinrate, -by_player$Winrate, -by_player$Games, by_player$key), , drop = FALSE]
  by_player$Rank <- seq_len(nrow(by_player))
  by_player <- by_player[, c("Rank", "key", "Games", "Wins", "Winrate", "AdjustedWinrate", "ExpectedWinrate", "DeltaWinrate")]
  names(by_player) <- c("Rank", "Player", "Games", "Wins", "Winrate", "AdjustedWinrate", "ExpectedWinrate", "DeltaWinrate")

  baseline_winrate <- mean(rows$ChanceWin)
  expected_winrate <- mean(rows$ExpectedWin)
  winrate <- mean(rows$Win)

  summary <- data.frame(
    villainId = villain_id,
    Games = nrow(rows),
    Wins = sum(rows$Win),
    Winrate = winrate,
    AdjustedWinrate = winrate - expected_winrate + baseline_winrate,
    ExpectedWinrate = expected_winrate,
    DeltaWinrate = winrate - expected_winrate,
    stringsAsFactors = FALSE
  )

  list(summary = summary, by_player = by_player)
}

build_player_detail <- function(elo_log, player_id, player_count = NULL, normalize_by_players = TRUE) {
  empty <- list(
    summary = data.frame(
      playerId = character(),
      Games = integer(),
      Wins = integer(),
      Winrate = numeric(),
      AdjustedWinrate = numeric(),
      ExpectedWinrate = numeric(),
      DeltaWinrate = numeric(),
      stringsAsFactors = FALSE
    ),
    by_villain = data.frame(
      Rank = integer(),
      VillainId = character(),
      Games = integer(),
      Wins = integer(),
      Winrate = numeric(),
      AdjustedWinrate = numeric(),
      ExpectedWinrate = numeric(),
      DeltaWinrate = numeric(),
      stringsAsFactors = FALSE
    )
  )

  if (is.null(player_id) || player_id == "") return(empty)

  rows <- profile_rows_from_log(elo_log, normalize_by_players = normalize_by_players)
  if (nrow(rows) == 0) return(empty)

  if (!is.null(player_count) && !is.na(player_count)) {
    rows <- rows[rows$nPlayers == as.integer(player_count), , drop = FALSE]
  }
  rows <- rows[rows$player == player_id, , drop = FALSE]

  if (nrow(rows) == 0) {
    empty$summary <- data.frame(
      playerId = player_id,
      Games = 0L,
      Wins = 0L,
      Winrate = NA_real_,
      AdjustedWinrate = NA_real_,
      ExpectedWinrate = NA_real_,
      DeltaWinrate = NA_real_,
      stringsAsFactors = FALSE
    )
    return(empty)
  }

  by_villain <- aggregate_profile_metrics(rows, "villainId")
  by_villain <- by_villain[order(-by_villain$AdjustedWinrate, -by_villain$Winrate, -by_villain$Games, by_villain$key), , drop = FALSE]
  by_villain$Rank <- seq_len(nrow(by_villain))
  by_villain <- by_villain[, c("Rank", "key", "Games", "Wins", "Winrate", "AdjustedWinrate", "ExpectedWinrate", "DeltaWinrate")]
  names(by_villain) <- c("Rank", "VillainId", "Games", "Wins", "Winrate", "AdjustedWinrate", "ExpectedWinrate", "DeltaWinrate")

  baseline_winrate <- mean(rows$ChanceWin)
  expected_winrate <- mean(rows$ExpectedWin)
  winrate <- mean(rows$Win)

  summary <- data.frame(
    playerId = player_id,
    Games = nrow(rows),
    Wins = sum(rows$Win),
    Winrate = winrate,
    AdjustedWinrate = winrate - expected_winrate + baseline_winrate,
    ExpectedWinrate = expected_winrate,
    DeltaWinrate = winrate - expected_winrate,
    stringsAsFactors = FALSE
  )

  list(summary = summary, by_villain = by_villain)
}
