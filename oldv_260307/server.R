# server.R
server <- function(input, output, session) {
  
  currentLang <- reactiveVal("en")
  tr <- function(key) tr_lang(currentLang(), key)
  
  # keep current tab when switching language
  selectedTab <- reactiveVal("input")
  observeEvent(input$main_nav, {
    if (!is.null(input$main_nav) && nzchar(input$main_nav)) selectedTab(input$main_nav)
  })
  
  output$app_ui <- renderUI({
    lang <- currentLang()
    lang_button_label <- if (lang == "en") "FR" else "EN"
    build_ui(lang, lang_button_label, selected_tab = isolate(selectedTab()))
  })
  
  observeEvent(input$toggleLang, {
    currentLang(if (currentLang() == "en") "fr" else "en")
    showNotification(
      if (currentLang() == "fr") "🌐 Langue : Français" else "🌐 Language: English",
      type = "message", duration = 2
    )
  })
  
  app_dir <- getwd()
  rds_path <- file.path(app_dir, "games.rds")
  
  games <- reactiveVal({
    loaded <- load_rds_safe(rds_path)
    if (is.null(loaded)) list() else normalize_games(loaded)
  })
  
  errorMsg <- reactiveVal("")
  dataMsg <- reactiveVal("")
  predMsg <- reactiveVal("")
  
  elo_cfg <- reactiveValues(baseRating = 0, k = 32, alpha = 0.7, scale = 400)
  
  observeEvent(input$openElo, {
    showModal(modalDialog(
      title = tr("elo_modal_title"),
      easyClose = TRUE,
      footer = tagList(
        modalButton(tr("close")),
        actionButton("saveElo", tr("apply"), class = "btn-primary")
      ),
      layout_columns(
        fill = FALSE,
        col_widths = c(6, 6),
        numericInput("m_baseRating", tr("base_rating"), value = elo_cfg$baseRating),
        numericInput("m_k", tr("k_base"), value = elo_cfg$k),
        sliderInput("m_alpha", tr("alpha"), min = 0, max = 1, value = elo_cfg$alpha, step = 0.05),
        numericInput("m_scale", tr("scale"), value = elo_cfg$scale)
      )
    ))
  })
  
  observeEvent(input$saveElo, {
    elo_cfg$baseRating <- as.integer(input$m_baseRating %||% 0)
    elo_cfg$k <- as.numeric(input$m_k %||% 32)
    elo_cfg$alpha <- as.numeric(input$m_alpha %||% 0.7)
    elo_cfg$scale <- as.numeric(input$m_scale %||% 400)
    removeModal()
    showNotification(tr("notif_elo_applied"), type = "message", duration = 2.5)
  })
  
  available_villain_ids <- reactive({
    sets <- input$setsOwned %||% sets_tbl$set_id
    v <- villains_tbl[villains_tbl$set_id %in% sets, "villain_id", drop = TRUE]
    sort(unique(v))
  })
  
  villain_choices_named <- reactive({
    lang <- currentLang()
    ids <- available_villain_ids()
    labels <- vapply(ids, label_villain, character(1), lang = lang)
    setNames(ids, labels)
  })
  
  set_choices_named <- reactive({
    lang <- currentLang()
    setNames(sets_tbl$set_id, if (lang == "fr") sets_tbl$set_fr else sets_tbl$set_en)
  })
  
  known_players <- reactive({
    g <- games()
    if (length(g) == 0) return(character())
    sort(unique(unlist(lapply(g, function(x) x$seats$player))))
  })
  
  observe({
    updateCheckboxGroupInput(
      session, "setsOwned",
      label = tr("boxes_label"),
      choices = set_choices_named(),
      selected = input$setsOwned %||% sets_tbl$set_id
    )
  })
  
  elo_res <- reactive({
    compute_elo(
      games = games(),
      base_rating = elo_cfg$baseRating,
      k = elo_cfg$k,
      alpha = elo_cfg$alpha,
      scale = elo_cfg$scale,
      return_log = TRUE
    )
  })
  
  # -------- Seats UI (Input tab) --------
  output$seats_ui <- renderUI({
    n <- input$numPlayers
    req(n)
    
    v_choices <- villain_choices_named()
    p_choices <- known_players()
    
    tagList(lapply(seq_len(n), function(i) {
      p_id <- paste0("player_", i)
      v_id <- paste0("villain_", i)
      
      current_p <- isolate(input[[p_id]])
      if (is.null(current_p) || current_p == "") current_p <- if (i == 1) "Geoff" else ""
      
      current_v <- isolate(input[[v_id]])
      if (is.null(current_v)) current_v <- ""
      
      v_choices2 <- v_choices
      if (!is.null(current_v) && current_v != "" && !(current_v %in% v_choices2)) {
        v_choices2 <- c(v_choices2, setNames(current_v, label_villain(current_v, currentLang())))
      }
      
      div(
        style = "margin-bottom: 12px;",
        layout_columns(
          fill = FALSE,
          col_widths = c(6, 6),
          selectizeInput(
            p_id, paste0(tr("name"), " #", i),
            choices = unique(c(p_choices, current_p)), selected = current_p,
            options = list(
              create = TRUE, persist = TRUE, createOnBlur = TRUE,
              dropdownParent = "body",
              placeholder = if (currentLang() == "fr") "Choisir ou taper un nouveau nom" else "Select or type a new name"
            )
          ),
          selectizeInput(
            v_id, paste0("Villain #", i),
            choices = v_choices2, selected = current_v,
            options = list(
              create = FALSE,
              dropdownParent = "body",
              placeholder = if (currentLang() == "fr") "Choisir un Vilain" else "Select a Villain"
            )
          )
        )
      )
    }))
  })
  
  observe({
    n <- input$numPlayers
    req(n)
    players <- sapply(seq_len(n), function(i) trimws(input[[paste0("player_", i)]] %||% ""))
    players <- players[players != ""]
    updateSelectInput(
      session, "winnerPlayer",
      label = tr("winner"),
      choices = players,
      selected = if (length(players) > 0) players[1] else NULL
    )
  })
  
  observeEvent(input$addGame, {
    errorMsg("")
    dataMsg("")
    
    n <- input$numPlayers
    req(n)
    
    players <- sapply(seq_len(n), function(i) trimws(input[[paste0("player_", i)]] %||% ""))
    villains <- sapply(seq_len(n), function(i) trimws(input[[paste0("villain_", i)]] %||% ""))
    
    keep <- players != "" & villains != ""
    players <- players[keep]
    villains <- villains[keep]
    
    if (length(players) < 2 || length(players) > 6) { errorMsg(tr("err_need_seats")); return() }
    if (length(unique(players)) != length(players)) { errorMsg(tr("err_unique_players")); return() }
    if (any(!villains %in% available_villain_ids())) { errorMsg(tr("err_villain_not_allowed")); return() }
    
    winner <- trimws(input$winnerPlayer %||% "")
    if (winner == "" || !winner %in% players) { errorMsg(tr("err_winner_invalid")); return() }
    
    dur <- suppressWarnings(as.integer(input$durationMin %||% NA_integer_))
    if (!is.na(dur) && dur < 5) { errorMsg(tr("err_duration_invalid")); return() }
    
    g <- list(
      id = paste0(as.integer(Sys.time()), "_", sample(10000:99999, 1)),
      playedAt = as.character(as.Date(input$playedAt)),
      durationMin = dur,
      winnerPlayer = winner,
      seats = data.frame(player = players, villainId = villains, stringsAsFactors = FALSE)
    )
    
    new_games <- c(games(), list(g))
    games(new_games)
    save_rds_safe(new_games, rds_path)
    showNotification(tr("notif_game_saved"), type = "message", duration = 2.5)
  })
  
  # -------- KPIs --------
  output$kpi_games <- renderText(length(games()))
  
  output$kpi_avg_dur <- renderText({
    g <- games()
    d <- suppressWarnings(as.integer(sapply(g, function(x) x$durationMin %||% NA_integer_)))
    d <- d[!is.na(d)]
    if (length(d) == 0) return("—")
    fmt_h_min(mean(d))
  })
  
  output$kpi_hours <- renderText({
    g <- games()
    d <- suppressWarnings(as.integer(sapply(g, function(x) x$durationMin %||% NA_integer_)))
    d <- d[!is.na(d)]
    if (length(d) == 0) return("—")
    paste0(round(sum(d) / 60, 1), " h")
  })
  
  # -------- Top 3 badges --------
  output$topPlayersBadges <- renderUI({
    df <- make_leaderboard(elo_res()$playerRatings, elo_res()$playerGames)
    top <- head(df, 3)
    if (nrow(top) == 0) return(tags$div(class = "small-muted", tr("not_enough_data")))
    classes <- c("trophy-1", "trophy-2", "trophy-3")
    tagList(lapply(seq_len(nrow(top)), function(i) {
      tags$span(class = "badge-soft",
                tags$span(class = classes[i], icon("trophy")),
                paste0("#", i, " ", top$key[i], " — ", top$Elo[i]))
    }))
  })
  
  output$topVillainsBadges <- renderUI({
    lang <- currentLang()
    df <- make_leaderboard(elo_res()$villainRatings, elo_res()$villainGames)
    top <- head(df, 3)
    if (nrow(top) == 0) return(tags$div(class = "small-muted", tr("not_enough_data")))
    classes <- c("trophy-1", "trophy-2", "trophy-3")
    tagList(lapply(seq_len(nrow(top)), function(i) {
      tags$span(class = "badge-soft",
                tags$span(class = classes[i], icon("trophy")),
                paste0("#", i, " ", label_villain(top$key[i], lang), " — ", top$Elo[i]))
    }))
  })
  
  dt_opts <- list(pageLength = 10, autoWidth = TRUE)
  
  output$playersTable <- renderDT({
    df <- make_leaderboard(elo_res()$playerRatings, elo_res()$playerGames)
    if (nrow(df) == 0) df <- data.frame(key = character(), Elo = integer(), Games = integer())
    shown <- data.frame(Name = df$key, Elo = df$Elo, Games = df$Games, stringsAsFactors = FALSE)
    datatable(shown, rownames = FALSE, class = "stripe hover compact cell-border", options = dt_opts)
  })
  
  output$villainsTable <- renderDT({
    lang <- currentLang()
    df <- make_leaderboard(elo_res()$villainRatings, elo_res()$villainGames)
    if (nrow(df) == 0) df <- data.frame(key = character(), Elo = integer(), Games = integer())
    shown <- data.frame(
      Name = vapply(df$key, label_villain, character(1), lang = lang),
      Elo = df$Elo,
      Games = df$Games,
      stringsAsFactors = FALSE
    )
    datatable(shown, rownames = FALSE, class = "stripe hover compact cell-border", options = dt_opts)
  })
  
  # -------- Profiles --------
  profiles <- reactive({
    build_profiles(games(), normalize_by_players = isTRUE(input$dur_norm %||% TRUE))
  })
  
  output$profilesPlayersTable <- renderDT({
    p <- profiles()$players
    if (nrow(p) == 0) {
      return(datatable(
        data.frame(Name = character(), Elo = integer(), Games = integer(), Winrate = numeric(), AvgWin = character(), AvgLoss = character()),
        rownames = FALSE, class = "stripe hover compact cell-border", options = dt_opts
      ))
    }
    pr <- elo_res()$playerRatings
    elo <- pr[p$key]; elo[is.na(elo)] <- elo_cfg$baseRating
    
    shown <- data.frame(
      Name = p$key,
      Elo = as.integer(round(elo)),
      Games = p$Games,
      Winrate = round(p$Winrate * 100, 1),
      AvgWin = fmt_h_min(p$AvgWin),
      AvgLoss = fmt_h_min(p$AvgLoss),
      stringsAsFactors = FALSE
    )
    shown <- shown[order(-shown$Elo, -shown$Games, shown$Name), , drop = FALSE]
    datatable(shown, rownames = FALSE, class = "stripe hover compact cell-border",
              options = list(pageLength = 12, autoWidth = TRUE))
  })
  
  output$profilesVillainsTable <- renderDT({
    lang <- currentLang()
    v <- profiles()$villains
    if (nrow(v) == 0) {
      return(datatable(
        data.frame(Villain = character(), Elo = integer(), Games = integer(), Winrate = numeric(), AvgWin = character(), AvgLoss = character()),
        rownames = FALSE, class = "stripe hover compact cell-border", options = dt_opts
      ))
    }
    vr <- elo_res()$villainRatings
    elo <- vr[v$key]; elo[is.na(elo)] <- elo_cfg$baseRating
    
    shown <- data.frame(
      Villain = vapply(v$key, label_villain, character(1), lang = lang),
      Elo = as.integer(round(elo)),
      Games = v$Games,
      Winrate = round(v$Winrate * 100, 1),
      AvgWin = fmt_h_min(v$AvgWin),
      AvgLoss = fmt_h_min(v$AvgLoss),
      stringsAsFactors = FALSE
    )
    shown <- shown[order(-shown$Elo, -shown$Games, shown$Villain), , drop = FALSE]
    datatable(shown, rownames = FALSE, class = "stripe hover compact cell-border",
              options = list(pageLength = 12, autoWidth = TRUE))
  })
  
  # -------- Timeline plot helpers --------
  timeline_entity <- function(games, cfg, type = c("player", "villain"), name) {
    type <- match.arg(type)
    if (length(games) == 0 || is.null(name) || name == "") return(data.frame())
    
    base_rating <- cfg$baseRating
    k <- cfg$k
    alpha <- cfg$alpha
    scale <- cfg$scale
    
    playerR <- numeric(); names(playerR) <- character()
    villainR <- numeric(); names(villainR) <- character()
    
    ensure_player <- function(p) if (!p %in% names(playerR)) playerR[p] <<- base_rating
    ensure_villain <- function(v) if (!v %in% names(villainR)) villainR[v] <<- base_rating
    
    games_sorted <- games[order(sapply(games, `[[`, "playedAt"), sapply(games, `[[`, "id"))]
    out <- data.frame(date = character(), gameId = character(), rating = numeric(), stringsAsFactors = FALSE)
    
    for (g in games_sorted) {
      seats <- g$seats
      if (is.null(seats) || !is.data.frame(seats) || nrow(seats) < 2) next
      
      seats$player <- trimws(seats$player)
      seats$villainId <- trimws(seats$villainId)
      seats <- seats[seats$player != "" & seats$villainId != "", , drop = FALSE]
      n <- nrow(seats)
      if (n < 2 || n > 6) next
      
      winner <- trimws(g$winnerPlayer %||% "")
      if (!winner %in% seats$player) next
      
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
        playerR[p] <- playerR[p] + alpha * d_combined
        villainR[v] <- villainR[v] + (1 - alpha) * d_combined
      }
      
      if (type == "player") {
        if (!name %in% names(playerR)) next
        out <- rbind(out, data.frame(date = g$playedAt, gameId = g$id, rating = playerR[name], stringsAsFactors = FALSE))
      } else {
        if (!name %in% names(villainR)) next
        out <- rbind(out, data.frame(date = g$playedAt, gameId = g$id, rating = villainR[name], stringsAsFactors = FALSE))
      }
    }
    out
  }
  
  observe({
    type <- input$timelineType %||% "player"
    lang <- currentLang()
    if (type == "player") {
      choices <- sort(unique(names(elo_res()$playerRatings)))
      output$timelineNameUI <- renderUI(
        selectInput("timelineName", tr("name"), choices = choices, selected = if (length(choices) > 0) choices[1] else NULL)
      )
    } else {
      ids <- sort(unique(names(elo_res()$villainRatings)))
      ch <- setNames(ids, vapply(ids, label_villain, character(1), lang = lang))
      output$timelineNameUI <- renderUI(
        selectInput("timelineName", tr("name"), choices = ch, selected = if (length(ids) > 0) ids[1] else NULL)
      )
    }
  })
  
  output$eloTimelinePlot <- renderPlot({
    req(input$timelineType, input$timelineName)
    cfg <- list(baseRating = elo_cfg$baseRating, k = elo_cfg$k, alpha = elo_cfg$alpha, scale = elo_cfg$scale)
    tl <- timeline_entity(games(), cfg, type = input$timelineType, name = input$timelineName)
    
    if (nrow(tl) == 0) {
      par(bg = "#07150d", fg = "#e9eef6")
      plot.new(); text(0.5, 0.5, tr("not_enough_data"), col = "#cfe0ff", cex = 1.1)
      return()
    }
    
    x <- seq_len(nrow(tl)); y <- tl$rating
    titleName <- if (input$timelineType == "villain") label_villain(input$timelineName, currentLang()) else input$timelineName
    
    par(bg = "#07150d", fg = "#e9eef6", col.axis = "#cfe0ff", col.lab = "#cfe0ff", col.main = "#e9eef6")
    plot(x, y, type = "n",
         xlab = if (currentLang() == "fr") "Parties (chronologique)" else "Games (chronological)",
         ylab = "Elo", main = titleName, xaxt = "n")
    axis(1, at = x, labels = x)
    grid(col = "#1f2a3a", lty = 3)
    
    # ✅ Villainous palette
    glow <- adjustcolor("#2f7d4b", alpha.f = 0.25)
    lines(x, y, lwd = 7, col = glow)
    lines(x, y, lwd = 2.5, col = "#d8c58a")
    points(x, y, pch = 16, cex = 1.1, col = "#f3e7c3")
    points(x, y, pch = 16, cex = 2.0, col = adjustcolor("#2f7d4b", alpha.f = 0.20))
  })
  
  # -------- Duration win/loss --------
  observe({
    type <- input$durType %||% "player"
    lang <- currentLang()
    log_df <- elo_res()$log
    
    if (type == "player") {
      choices <- sort(unique(log_df$player))
      output$durNameUI <- renderUI(
        selectInput("durName", tr("name"), choices = choices, selected = if (length(choices) > 0) choices[1] else NULL)
      )
    } else {
      ids <- sort(unique(log_df$villainId))
      ch <- setNames(ids, vapply(ids, label_villain, character(1), lang = lang))
      output$durNameUI <- renderUI(
        selectInput("durName", tr("name"), choices = ch, selected = if (length(ids) > 0) ids[1] else NULL)
      )
    }
  })
  
  output$durationWinLossPlot <- renderPlot({
    req(input$durType, input$durName)
    log_df <- elo_res()$log
    if (nrow(log_df) == 0) {
      par(bg = "#07150d", fg = "#e9eef6")
      plot.new(); text(0.5, 0.5, tr("not_enough_data"), col = "#cfe0ff", cex = 1.1)
      return()
    }
    df <- log_df[!is.na(log_df$durationMin) & log_df$durationMin > 0, , drop = FALSE]
    if (nrow(df) == 0) {
      par(bg = "#07150d", fg = "#e9eef6")
      plot.new(); text(0.5, 0.5, if (currentLang() == "fr") "Aucune durée renseignée." else "No duration entered.",
                       col = "#cfe0ff", cex = 1.1)
      return()
    }
    if (input$durType == "player") df <- df[df$player == input$durName, , drop = FALSE]
    else df <- df[df$villainId == input$durName, , drop = FALSE]
    
    if (nrow(df) == 0) {
      par(bg = "#07150d", fg = "#e9eef6")
      plot.new(); text(0.5, 0.5, if (currentLang() == "fr") "Aucune ligne correspondant." else "No matching rows.",
                       col = "#cfe0ff", cex = 1.1)
      return()
    }
    
    win <- df$durationMin[df$isWinner]
    loss <- df$durationMin[!df$isWinner]
    titleName <- if (input$durType == "villain") label_villain(input$durName, currentLang()) else input$durName
    
    par(bg = "#07150d", fg = "#e9eef6", col.axis = "#cfe0ff", col.lab = "#cfe0ff", col.main = "#e9eef6")
    boxplot(list(Win = win, Loss = loss),
            main = paste0(tr("duration_win_loss"), " — ", titleName),
            ylab = tr("minutes"),
            border = "#d8c58a",
            col = adjustcolor("#2f7d4b", alpha.f = 0.22))
    grid(col = "#1f2a3a", lty = 3)
  })
  
  # -------- Matchups --------
  observe({
    g <- games()
    present <- character()
    if (length(g) > 0) present <- sort(unique(unlist(lapply(g, function(x) x$seats$villainId))))
    lang <- currentLang()
    
    choices <- c(
      setNames("all", tr("mu_all")),
      setNames(present, vapply(present, label_villain, character(1), lang = lang))
    )
    updateSelectInput(session, "mu_focus", choices = choices, selected = "all")
  })
  
  matchup_df <- reactive({
    g <- games()
    if (length(g) == 0) return(data.frame())
    
    nf <- input$mu_n %||% "all"
    if (nf != "all") {
      nf_i <- as.integer(nf)
      g <- Filter(function(x) nrow(x$seats) == nf_i, g)
    }
    if (length(g) == 0) return(data.frame())
    
    wins <- new.env(parent = emptyenv())
    games_ab <- new.env(parent = emptyenv())
    add_key <- function(env, k, v) {
      if (!exists(k, envir = env, inherits = FALSE)) assign(k, v, envir = env)
      else assign(k, get(k, env) + v, envir = env)
    }
    
    for (game in g) {
      seats <- game$seats
      vlist <- unique(trimws(seats$villainId))
      vlist <- vlist[vlist != ""]
      if (length(vlist) < 2) next
      
      winner_player <- trimws(game$winnerPlayer %||% "")
      if (winner_player == "" || !winner_player %in% seats$player) next
      winner_vill <- seats$villainId[match(winner_player, seats$player)]
      winner_vill <- trimws(winner_vill)
      
      for (A in vlist) {
        for (B in vlist) {
          if (A == B) next
          key <- paste0(A, "|||", B)
          add_key(games_ab, key, 1)
          if (winner_vill == A) add_key(wins, key, 1)
        }
      }
    }
    
    keys <- ls(games_ab)
    if (length(keys) == 0) return(data.frame())
    
    out <- do.call(rbind, lapply(keys, function(k) {
      parts <- strsplit(k, "\\|\\|\\|")[[1]]
      A <- parts[1]; B <- parts[2]
      gcount <- get(k, games_ab)
      wcount <- if (exists(k, wins, inherits = FALSE)) get(k, wins) else 0
      data.frame(
        VillainId = A,
        OpponentId = B,
        GamesTogether = gcount,
        Winrate = wcount / gcount,
        stringsAsFactors = FALSE
      )
    }))
    
    min_g <- input$mu_min_games %||% 2
    out <- out[out$GamesTogether >= min_g, , drop = FALSE]
    
    focus <- input$mu_focus %||% "all"
    if (focus != "all") out <- out[out$VillainId == focus, , drop = FALSE]
    
    if (nrow(out) == 0) return(out)
    out <- out[order(-out$GamesTogether, -out$Winrate, out$VillainId, out$OpponentId), , drop = FALSE]
    out
  })
  
  output$matchupTable <- renderDT({
    df <- matchup_df()
    lang <- currentLang()
    
    opts <- list(
      pageLength = 12,
      lengthMenu = list(c(12, 25, 50), c("12", "25", "50")),
      autoWidth = TRUE
    )
    
    if (nrow(df) == 0) {
      return(datatable(
        data.frame(Villain = character(), Opponent = character(), GamesTogether = integer(), WinratePct = numeric()),
        rownames = FALSE, class = "stripe hover compact cell-border", options = opts
      ))
    }
    
    shown <- data.frame(
      Villain = vapply(df$VillainId, label_villain, character(1), lang = lang),
      Opponent = vapply(df$OpponentId, label_villain, character(1), lang = lang),
      GamesTogether = df$GamesTogether,
      WinratePct = round(df$Winrate * 100, 1),
      stringsAsFactors = FALSE
    )
    datatable(shown, rownames = FALSE, class = "stripe hover compact cell-border", options = opts)
  })
  
  # ✅ Heatmap ggplot2 + bg transparent (colors corrected)
  output$matchupHeatmap <- renderPlot({
    df <- matchup_df()
    lang <- currentLang()
    
    if (is.null(df) || nrow(df) == 0) {
      par(bg = "#07150d", fg = "#e9eef6")
      plot.new(); text(0.5, 0.5, tr("not_enough_data"), col = "#cfe0ff", cex = 1.1)
      return()
    }
    
    villains <- sort(unique(c(df$VillainId, df$OpponentId)))
    if (length(villains) < 2) {
      par(bg = "#07150d", fg = "#e9eef6")
      plot.new()
      msg <- if (lang == "fr") "Pas assez de vilains (min 2)." else "Not enough villains (need ≥2)."
      text(0.5, 0.5, msg, col = "#cfe0ff", cex = 1.1)
      return()
    }
    
    wrap1 <- function(s) {
      s <- as.character(s)
      if (nchar(s) > 14 && grepl(" ", s)) sub(" ", "\n", s) else s
    }
    lab <- vapply(villains, label_villain, character(1), lang = lang)
    lab <- vapply(lab, wrap1, character(1))
    
    grid_df <- expand.grid(
      VillainId = villains,
      OpponentId = villains,
      stringsAsFactors = FALSE
    )
    grid_df$Winrate <- NA_real_
    
    key <- paste(df$VillainId, df$OpponentId, sep = "|||")
    idx <- match(paste(grid_df$VillainId, grid_df$OpponentId, sep = "|||"), key)
    grid_df$Winrate[!is.na(idx)] <- df$Winrate[idx[!is.na(idx)]]
    
    grid_df$Winrate[grid_df$VillainId == grid_df$OpponentId] <- NA_real_
    
    grid_df$OpponentLab <- factor(vapply(grid_df$OpponentId, function(x) lab[match(x, villains)], character(1)), levels = lab)
    grid_df$VillainLab  <- factor(vapply(grid_df$VillainId,  function(x) lab[match(x, villains)], character(1)), levels = rev(lab))
    
    n <- length(villains)
    txt <- if (n <= 8) 10 else if (n <= 12) 9 else 8
    
    pal <- c("#06150d", "#2f7d4b", "#f3e7c3")
    
    p <- ggplot(grid_df, aes(x = OpponentLab, y = VillainLab, fill = Winrate)) +
      geom_tile(color = "#1f2a3a", linewidth = 0.4, na.rm = FALSE) +
      coord_fixed() +
      scale_fill_gradientn(
        colours = pal,
        limits = c(0, 1),
        breaks = c(0, .25, .5, .75, 1),
        labels = c("0%", "25%", "50%", "75%", "100%"),
        na.value = "#07150d",
        name = "Winrate"
      ) +
      labs(
        x = if (lang == "fr") "Adversaire" else "Opponent",
        y = if (lang == "fr") "Vilain" else "Villain",
        title = if (lang == "fr") "Winrate (Vilain vs Adversaire)" else "Winrate (Villain vs Opponent)"
      ) +
      theme_minimal(base_size = txt) +
      ggplot2::theme(
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "#07150d", color = NA),
        
        panel.grid = element_blank(),
        axis.title = element_text(color = "#cfe0ff"),
        axis.text  = element_text(color = "#cfe0ff"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(color = "#e9eef6", face = "bold", size = 12, margin = margin(b = 10)),
        
        legend.position = "right",
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key        = element_rect(fill = "transparent", color = NA),
        
        legend.title = element_text(color = "#f3e7c3", face = "bold"),
        legend.text  = element_text(color = "#cfe0ff"),
        legend.margin = margin(l = 8),
        
        legend.box.background = element_rect(
          fill = "transparent",
          color = adjustcolor("#2f7d4b", alpha.f = 0.45),
          linewidth = 1,
          linetype = 1
        ),
        legend.box.margin = margin(6, 6, 6, 6),
        plot.margin = margin(12, 12, 12, 12)
      ) +
      guides(fill = guide_colorbar(
        barheight = grid::unit(220, "pt"),
        barwidth  = grid::unit(14, "pt"),
        ticks.colour = "#cfe0ff",
        frame.colour = "#1f2a3a"
      ))
    
    print(p)
  }, res = 96, bg = "transparent")
  
  # -------- Prediction --------
  output$pred_seats_ui <- renderUI({
    n <- input$pred_numPlayers
    req(n)
    
    v_choices <- villain_choices_named()
    p_choices <- known_players()
    
    tagList(lapply(seq_len(n), function(i) {
      pid <- paste0("pred_player_", i)
      vid <- paste0("pred_villain_", i)
      
      cp <- isolate(input[[pid]]); if (is.null(cp)) cp <- ""
      cv <- isolate(input[[vid]]); if (is.null(cv)) cv <- ""
      
      v_choices2 <- v_choices
      if (!is.null(cv) && cv != "" && !(cv %in% v_choices2)) {
        v_choices2 <- c(v_choices2, setNames(cv, label_villain(cv, currentLang())))
      }
      
      div(
        style = "margin-bottom: 12px;",
        layout_columns(
          fill = FALSE,
          col_widths = c(6, 6),
          selectizeInput(pid, paste0(tr("name"), " #", i),
                         choices = unique(c(p_choices, cp)), selected = cp,
                         options = list(create = TRUE, persist = TRUE, createOnBlur = TRUE, dropdownParent = "body")),
          selectizeInput(vid, paste0("Villain #", i),
                         choices = v_choices2, selected = cv,
                         options = list(create = FALSE, dropdownParent = "body"))
        )
      )
    }))
  })
  
  pred_data <- eventReactive(input$pred_calc, {
    predMsg("")
    n <- input$pred_numPlayers %||% 4
    n <- max(2, min(6, as.integer(n)))
    
    players <- sapply(seq_len(n), function(i) trimws(input[[paste0("pred_player_", i)]] %||% ""))
    villains <- sapply(seq_len(n), function(i) trimws(input[[paste0("pred_villain_", i)]] %||% ""))
    
    if (any(players == "") || any(villains == "")) { predMsg(tr("pred_fill_all")); return(NULL) }
    if (length(unique(players)) != length(players)) { predMsg(tr("pred_unique_players")); return(NULL) }
    if (any(!villains %in% available_villain_ids())) { predMsg(tr("pred_villain_invalid")); return(NULL) }
    
    pr <- elo_res()$playerRatings
    vr <- elo_res()$villainRatings
    base <- elo_cfg$baseRating
    
    eloP <- sapply(players, function(p) if (p %in% names(pr)) pr[p] else base)
    eloV <- sapply(villains, function(v) if (v %in% names(vr)) vr[v] else base)
    eloT <- as.numeric(eloP) + as.numeric(eloV)
    
    scale <- elo_cfg$scale
    s <- 10^(eloT / scale)
    prob <- s / sum(s)
    
    data.frame(
      Seat = paste0("#", seq_len(n)),
      Player = players,
      Villain = vapply(villains, label_villain, character(1), lang = currentLang()),
      Elo_Player = as.integer(round(eloP)),
      Elo_Villain = as.integer(round(eloV)),
      Elo_Total = as.integer(round(eloT)),
      Prob_pct = round(prob * 100, 1),
      stringsAsFactors = FALSE
    )
  }, ignoreInit = FALSE)
  
  output$predTable <- renderDT({
    df <- pred_data()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        data.frame(Seat = character(), Player = character(), Villain = character(), Elo_Total = integer(), Prob_pct = numeric()),
        rownames = FALSE, class = "stripe hover compact cell-border",
        options = list(pageLength = 10, autoWidth = TRUE)
      ))
    }
    datatable(df, rownames = FALSE, class = "stripe hover compact cell-border", options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$predPlot <- renderPlot({
    df <- pred_data()
    if (is.null(df) || nrow(df) == 0) {
      par(bg = "#07150d", fg = "#e9eef6")
      plot.new()
      text(0.5, 0.5,
           if (currentLang() == "fr") "Renseigne une composition puis clique sur Calculer."
           else "Fill a composition then click Calculate.",
           col = "#cfe0ff", cex = 1.1)
      return()
    }
    
    labels <- paste0(df$Player, "\n(", df$Villain, ")")
    vals <- df$Prob_pct
    
    par(bg = "#07150d", fg = "#e9eef6", col.axis = "#cfe0ff", col.lab = "#cfe0ff",
        col.main = "#e9eef6", mar = c(8, 4, 3, 2))
    
    bp <- barplot(vals,
                  names.arg = labels,
                  las = 2,
                  ylim = c(0, max(100, max(vals) + 10)),
                  border = NA,
                  col = adjustcolor("#2f7d4b", alpha.f = 0.35),
                  main = if (currentLang() == "fr") "Probabilité de victoire (%)" else "Win probability (%)",
                  ylab = "%")
    barplot(vals, add = TRUE, names.arg = rep("", length(vals)), border = NA,
            col = adjustcolor("#d8c58a", alpha.f = 0.18))
    grid(col = "#1f2a3a", lty = 3)
    text(bp, vals + 2, labels = paste0(vals, "%"), col = "#f3e7c3", cex = 0.9)
  })
  
  output$predMsg <- renderText(predMsg())
  
  # -------- Data / history / edit / import / export --------
  output$historyTable <- renderDT({
    g <- games()
    lang <- currentLang()
    if (length(g) == 0) {
      return(datatable(
        data.frame(Date = character(), DurationMin = integer(), Winner = character(), Seats = character()),
        rownames = FALSE, class = "stripe hover compact cell-border", options = dt_opts
      ))
    }
    df <- data.frame(
      Date = sapply(g, `[[`, "playedAt"),
      DurationMin = suppressWarnings(as.integer(sapply(g, function(x) x$durationMin %||% NA_integer_))),
      Winner = sapply(g, `[[`, "winnerPlayer"),
      Seats = sapply(g, function(x) paste0(x$seats$player, " (", vapply(x$seats$villainId, label_villain, character(1), lang = lang), ")", collapse = ", ")),
      stringsAsFactors = FALSE
    )
    df <- df[order(df$Date, decreasing = TRUE), , drop = FALSE]
    datatable(df, rownames = FALSE, class = "stripe hover compact cell-border", options = dt_opts)
  })
  
  observeEvent(input$resetAll, {
    games(list())
    save_rds_safe(list(), rds_path)
    errorMsg(""); dataMsg("")
    showNotification(tr("data_cleared"), type = "message", duration = 2)
  })
  
  output$exportJSON <- downloadHandler(
    filename = function() paste0("villainous-elo-games-", Sys.Date(), ".json"),
    content = function(file) {
      g <- games()
      exportable <- lapply(g, function(x) {
        list(
          id = x$id,
          playedAt = x$playedAt,
          durationMin = x$durationMin,
          winnerPlayer = x$winnerPlayer,
          seats = lapply(seq_len(nrow(x$seats)), function(i) {
            list(player = x$seats$player[i], villainId = x$seats$villainId[i])
          })
        )
      })
      writeLines(toJSON(exportable, auto_unbox = TRUE, pretty = TRUE), file)
    }
  )
  
  observeEvent(input$importJSON, {
    dataMsg(""); errorMsg("")
    req(input$importJSON$datapath)
    
    txt <- try(readLines(input$importJSON$datapath, warn = FALSE), silent = TRUE)
    if (inherits(txt, "try-error")) { dataMsg(tr("import_unreadable")); return() }
    
    parsed <- try(fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE), silent = TRUE)
    if (inherits(parsed, "try-error") || !is.list(parsed)) { dataMsg(tr("import_invalid")); return() }
    
    norm <- list()
    for (g in parsed) {
      if (is.null(g$id) || is.null(g$playedAt) || is.null(g$winnerPlayer) || is.null(g$seats)) next
      dur <- g$durationMin %||% g$durationMinutes %||% NA_integer_
      dur <- suppressWarnings(as.integer(dur))
      
      seats <- g$seats
      if (!is.data.frame(seats)) {
        if (is.list(seats)) {
          players <- sapply(seats, function(s) s$player %||% "")
          vids <- sapply(seats, function(s) s$villainId %||% (s$villain %||% ""))
          seats <- data.frame(player = players, villainId = vids, stringsAsFactors = FALSE)
        } else next
      }
      
      seats$villainId <- villain_to_id(seats$villainId)
      
      norm <- c(norm, list(list(
        id = as.character(g$id),
        playedAt = as.character(g$playedAt),
        durationMin = dur,
        winnerPlayer = as.character(g$winnerPlayer),
        seats = data.frame(
          player = as.character(seats$player),
          villainId = as.character(seats$villainId),
          stringsAsFactors = FALSE
        )
      )))
    }
    
    norm <- normalize_games(norm)
    games(norm)
    save_rds_safe(norm, rds_path)
    showNotification(tr("import_done"), type = "message", duration = 2.5)
  })
  
  observe({
    g <- games()
    if (length(g) == 0) {
      updateSelectInput(session, "editGameId", choices = character())
      return()
    }
    labels <- sapply(g, function(x) paste0(x$playedAt, " — winner: ", x$winnerPlayer, " — ", x$id))
    choices <- setNames(sapply(g, `[[`, "id"), labels)
    updateSelectInput(session, "editGameId", choices = choices, selected = g[[length(g)]]$id)
  })
  
  selected_game <- reactive({
    gid <- input$editGameId %||% ""
    g <- games()
    if (gid == "" || length(g) == 0) return(NULL)
    idx <- which(sapply(g, `[[`, "id") == gid)
    if (length(idx) != 1) return(NULL)
    g[[idx]]
  })
  
  observeEvent(selected_game(), {
    sg <- selected_game()
    if (is.null(sg)) return()
    updateDateInput(session, "editPlayedAt", value = as.Date(sg$playedAt))
    updateNumericInput(session, "editDurationMin", value = as.integer(sg$durationMin %||% 60))
    updateNumericInput(session, "editNumPlayers", value = nrow(sg$seats))
  })
  
  output$editSeatsUI <- renderUI({
    sg <- selected_game()
    if (is.null(sg)) return(tags$div(class = "small-muted", tr("no_game_selected")))
    
    n <- input$editNumPlayers %||% nrow(sg$seats)
    n <- max(2, min(6, as.integer(n)))
    
    p_choices <- unique(c(known_players(), sg$seats$player))
    v_choices <- villain_choices_named()
    lang <- currentLang()
    
    tagList(lapply(seq_len(n), function(i) {
      pid <- paste0("edit_player_", i)
      vid <- paste0("edit_villain_", i)
      
      cur_p <- isolate(input[[pid]])
      cur_v <- isolate(input[[vid]])
      
      if (is.null(cur_p) || cur_p == "") cur_p <- if (i <= nrow(sg$seats)) sg$seats$player[i] else ""
      if (is.null(cur_v) || cur_v == "") cur_v <- if (i <= nrow(sg$seats)) sg$seats$villainId[i] else ""
      
      v_choices2 <- v_choices
      if (!is.null(cur_v) && cur_v != "" && !(cur_v %in% v_choices2)) {
        v_choices2 <- c(v_choices2, setNames(cur_v, label_villain(cur_v, lang)))
      }
      
      div(
        style = "margin-bottom: 12px;",
        layout_columns(
          fill = FALSE,
          col_widths = c(6, 6),
          selectizeInput(pid, paste0(tr("name"), " #", i),
                         choices = unique(c(p_choices, cur_p)), selected = cur_p,
                         options = list(create = TRUE, persist = TRUE, createOnBlur = TRUE, dropdownParent = "body")),
          selectizeInput(vid, paste0("Villain #", i),
                         choices = v_choices2, selected = cur_v,
                         options = list(create = FALSE, dropdownParent = "body"))
        )
      )
    }))
  })
  
  observe({
    sg <- selected_game()
    if (is.null(sg)) return()
    n <- input$editNumPlayers %||% nrow(sg$seats)
    n <- max(2, min(6, as.integer(n)))
    
    players <- sapply(seq_len(n), function(i) trimws(input[[paste0("edit_player_", i)]] %||% ""))
    players <- players[players != ""]
    if (length(players) == 0) {
      updateSelectInput(session, "editWinner", label = tr("winner"), choices = character(), selected = NULL)
    } else {
      sel <- sg$winnerPlayer
      if (!sel %in% players) sel <- players[1]
      updateSelectInput(session, "editWinner", label = tr("winner"), choices = players, selected = sel)
    }
  })
  
  observeEvent(input$applyEdit, {
    dataMsg("")
    sg <- selected_game()
    if (is.null(sg)) { dataMsg(tr("no_game_selected")); return() }
    
    n <- input$editNumPlayers %||% nrow(sg$seats)
    n <- max(2, min(6, as.integer(n)))
    
    players <- sapply(seq_len(n), function(i) trimws(input[[paste0("edit_player_", i)]] %||% ""))
    villains <- sapply(seq_len(n), function(i) trimws(input[[paste0("edit_villain_", i)]] %||% ""))
    
    keep <- players != "" & villains != ""
    players <- players[keep]
    villains <- villains[keep]
    
    if (length(players) < 2 || length(players) > 6) { dataMsg(tr("err_need_seats")); return() }
    if (length(unique(players)) != length(players)) { dataMsg(tr("duplicates_in_game")); return() }
    if (any(!villains %in% villains_tbl$villain_id)) { dataMsg(tr("invalid_villain_catalog")); return() }
    
    winner <- trimws(input$editWinner %||% "")
    if (winner == "" || !winner %in% players) { dataMsg(tr("invalid_winner")); return() }
    
    dur <- suppressWarnings(as.integer(input$editDurationMin %||% NA_integer_))
    if (!is.na(dur) && dur < 5) { dataMsg(tr("err_duration_invalid")); return() }
    
    g <- games()
    idx <- which(sapply(g, `[[`, "id") == sg$id)
    if (length(idx) != 1) { dataMsg(tr("game_not_found")); return() }
    
    g[[idx]]$playedAt <- as.character(as.Date(input$editPlayedAt))
    g[[idx]]$durationMin <- dur
    g[[idx]]$winnerPlayer <- winner
    g[[idx]]$seats <- data.frame(player = players, villainId = villains, stringsAsFactors = FALSE)
    
    games(g)
    save_rds_safe(g, rds_path)
    showNotification(tr("changes_saved"), type = "message", duration = 2.5)
  })
  
  observeEvent(input$deleteGame, {
    dataMsg("")
    sg <- selected_game()
    if (is.null(sg)) { dataMsg(tr("no_game_selected")); return() }
    g <- games()
    g <- g[sapply(g, `[[`, "id") != sg$id]
    games(g)
    save_rds_safe(g, rds_path)
    showNotification(tr("game_deleted"), type = "message", duration = 2.5)
  })
  
  output$errorMsg <- renderText(errorMsg())
  output$dataMsg  <- renderText(dataMsg())
}
