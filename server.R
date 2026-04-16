# server.R
server <- function(input, output, session) {
  
  currentLang <- reactiveVal("en")
  tr <- function(key) tr_lang(currentLang(), key)
  clamp_player_count <- function(x) {
    x <- suppressWarnings(as.integer(x))
    if (is.na(x)) return(2L)
    max(2L, min(6L, x))
  }
  
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
  
  format_pct_label <- function(x, digits = 1) {
    ifelse(is.na(x), "\u2014", paste0(round(100 * x, digits), " %"))
  }
  
  app_reactable_theme <- function(font_size = "12px", compact = TRUE) {
    reactableTheme(
      backgroundColor = "transparent",
      borderColor = "rgba(24, 70, 61, 0.32)",
      stripedColor = "rgba(216, 197, 138, 0.10)",
      highlightColor = "rgba(18, 37, 58, 0.88)",
      cellPadding = if (compact) "7px 10px" else "9px 12px",
      style = list(
        backgroundColor = "transparent",
        color = "#f4f7fb",
        fontSize = font_size
      ),
      headerStyle = list(
        backgroundColor = "#07150d",
        color = "#f4f7fb",
        borderColor = "rgba(216,197,138,0.18)",
        fontSize = "11px",
        letterSpacing = "0.2px",
        fontWeight = "800"
      ),
      inputStyle = list(
        backgroundColor = "#07150d",
        color = "#e9eef6",
        border = "1px solid #1a2f24",
        borderRadius = "8px",
        fontSize = "11px",
        padding = "4px 8px"
      ),
      pageButtonHoverStyle = list(
        backgroundColor = "rgba(255,255,255,.06)"
      ),
      pageButtonActiveStyle = list(
        backgroundColor = "rgba(47,125,75,.18)",
        color = "#f6fbff"
      )
    )
  }
  
  build_reactable <- function(df, page_size = 10, searchable = TRUE, columns = NULL, default_sorted = NULL, class = "app-reactable") {
    rownames(df) <- NULL
    reactable(
      df,
      rownames = FALSE,
      searchable = searchable,
      pagination = TRUE,
      defaultPageSize = page_size,
      compact = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = FALSE,
      fullWidth = TRUE,
      columns = columns,
      defaultSorted = default_sorted,
      defaultColDef = colDef(
        align = "center",
        headerStyle = list(fontWeight = "800", textTransform = "none")
      ),
      theme = app_reactable_theme(),
      class = class
    )
  }
  
  output$playersTable <- renderReactable({
    df <- make_leaderboard(elo_res()$playerRatings, elo_res()$playerGames)
    p <- profiles()$players
    
    if (nrow(df) == 0) {
      shown <- data.frame(
        Name = character(),
        Elo = integer(),
        Games = integer(),
        Winrate = character(),
        AvgWin = character(),
        AvgLoss = character(),
        stringsAsFactors = FALSE
      )
    } else {
      shown <- data.frame(
        key = df$key,
        Name = df$key,
        Elo = df$Elo,
        Games = df$Games,
        stringsAsFactors = FALSE
      )
      
      if (nrow(p) > 0) {
        shown <- merge(
          shown,
          p[, c("key", "Winrate", "AvgWin", "AvgLoss"), drop = FALSE],
          by = "key",
          all.x = TRUE,
          sort = FALSE
        )
      } else {
        shown$Winrate <- NA_real_
        shown$AvgWin <- NA_real_
        shown$AvgLoss <- NA_real_
      }
      
      shown <- shown[order(-shown$Elo, -shown$Games, shown$Name), , drop = FALSE]
      shown$Winrate <- format_pct_label(shown$Winrate)
      shown$AvgWin <- fmt_h_min(shown$AvgWin)
      shown$AvgLoss <- fmt_h_min(shown$AvgLoss)
      shown$key <- NULL
    }
    
    build_reactable(
      shown,
      page_size = 12,
      columns = list(
        Name = colDef(align = "left"),
        Elo = colDef(align = "right"),
        Games = colDef(align = "right"),
        Winrate = colDef(align = "right"),
        AvgWin = colDef(align = "right"),
        AvgLoss = colDef(align = "right")
      ),
      default_sorted = "Elo"
    )
  })
  
  output$villainsTable <- renderReactable({
    lang <- currentLang()
    df <- make_leaderboard(elo_res()$villainRatings, elo_res()$villainGames)
    v <- profiles()$villains
    
    if (nrow(df) == 0) {
      shown <- data.frame(
        Name = character(),
        Elo = integer(),
        Games = integer(),
        Winrate = character(),
        AvgWin = character(),
        AvgLoss = character(),
        stringsAsFactors = FALSE
      )
    } else {
      shown <- data.frame(
        key = df$key,
        Name = vapply(df$key, label_villain, character(1), lang = lang),
        Elo = df$Elo,
        Games = df$Games,
        stringsAsFactors = FALSE
      )
      
      if (nrow(v) > 0) {
        shown <- merge(
          shown,
          v[, c("key", "Winrate", "AvgWin", "AvgLoss"), drop = FALSE],
          by = "key",
          all.x = TRUE,
          sort = FALSE
        )
      } else {
        shown$Winrate <- NA_real_
        shown$AvgWin <- NA_real_
        shown$AvgLoss <- NA_real_
      }
      
      shown <- shown[order(-shown$Elo, -shown$Games, shown$Name), , drop = FALSE]
      shown$Winrate <- format_pct_label(shown$Winrate)
      shown$AvgWin <- fmt_h_min(shown$AvgWin)
      shown$AvgLoss <- fmt_h_min(shown$AvgLoss)
      shown$key <- NULL
    }
    
    build_reactable(
      shown,
      page_size = 12,
      columns = list(
        Name = colDef(align = "left"),
        Elo = colDef(align = "right"),
        Games = colDef(align = "right"),
        Winrate = colDef(align = "right"),
        AvgWin = colDef(align = "right"),
        AvgLoss = colDef(align = "right")
      ),
      default_sorted = "Elo"
    )
  })
  
  # -------- Profiles --------
  profiles <- reactive({
    build_profiles(games(), normalize_by_players = isTRUE(input$dur_norm %||% TRUE))
  })
  
  output$profilesPlayersTable <- renderReactable({
    p <- profiles()$players
    if (nrow(p) == 0) {
      return(build_reactable(
        data.frame(Name = character(), Elo = integer(), Games = integer(), Winrate = character(), AvgWin = character(), AvgLoss = character()),
        page_size = 12,
        columns = list(Name = colDef(align = "left"))
      ))
    }
    pr <- elo_res()$playerRatings
    elo <- pr[p$key]; elo[is.na(elo)] <- elo_cfg$baseRating
    
    shown <- data.frame(
      Name = p$key,
      Elo = as.integer(round(elo)),
      Games = p$Games,
      Winrate = format_pct_label(p$Winrate),
      AvgWin = fmt_h_min(p$AvgWin),
      AvgLoss = fmt_h_min(p$AvgLoss),
      stringsAsFactors = FALSE
    )
    shown <- shown[order(-shown$Elo, -shown$Games, shown$Name), , drop = FALSE]
    build_reactable(
      shown,
      page_size = 12,
      columns = list(
        Name = colDef(align = "left"),
        Elo = colDef(align = "right"),
        Games = colDef(align = "right"),
        Winrate = colDef(align = "right")
      ),
      default_sorted = "Elo"
    )
  })
  
  output$profilesVillainsTable <- renderReactable({
    lang <- currentLang()
    v <- profiles()$villains
    if (nrow(v) == 0) {
      return(build_reactable(
        data.frame(Villain = character(), Elo = integer(), Games = integer(), Winrate = character(), AvgWin = character(), AvgLoss = character()),
        page_size = 12,
        columns = list(Villain = colDef(align = "left"))
      ))
    }
    vr <- elo_res()$villainRatings
    elo <- vr[v$key]; elo[is.na(elo)] <- elo_cfg$baseRating
    
    shown <- data.frame(
      Villain = vapply(v$key, label_villain, character(1), lang = lang),
      Elo = as.integer(round(elo)),
      Games = v$Games,
      Winrate = format_pct_label(v$Winrate),
      AvgWin = fmt_h_min(v$AvgWin),
      AvgLoss = fmt_h_min(v$AvgLoss),
      stringsAsFactors = FALSE
    )
    shown <- shown[order(-shown$Elo, -shown$Games, shown$Villain), , drop = FALSE]
    build_reactable(
      shown,
      page_size = 12,
      columns = list(
        Villain = colDef(align = "left"),
        Elo = colDef(align = "right"),
        Games = colDef(align = "right"),
        Winrate = colDef(align = "right")
      ),
      default_sorted = "Elo"
    )
  })
  
  # -------- Villain focus --------
  observe({
    updateNumericInput(
      session,
      "villainDetailPlayerCount",
      value = clamp_player_count(input$villainDetailPlayerCount %||% 2L),
      min = 2,
      max = 6,
      step = 1
    )
  })

  observe({
    lang <- currentLang()
    ids <- available_villain_ids()
    if (length(ids) == 0) ids <- villains_tbl$villain_id
    
    current_sel <- isolate(input$villainDetailId %||% "")
    if (!(current_sel %in% ids)) {
      current_sel <- if (length(ids) > 0) ids[1] else NULL
    }
    
    updateSelectizeInput(
      session, "villainDetailId",
      label = tr("villain_pick"),
      choices = setNames(ids, vapply(ids, label_villain, character(1), lang = lang)),
      selected = current_sel,
      server = FALSE
    )
  })

  villain_focus_games <- reactive({
    target_n <- clamp_player_count(input$villainDetailPlayerCount %||% 2L)
    g <- games()
    if (length(g) == 0) return(g)
    
    Filter(function(game) {
      seats <- game$seats
      if (is.null(seats) || !is.data.frame(seats)) return(FALSE)
      if (!("villainId" %in% names(seats))) {
        if ("villain" %in% names(seats)) {
          seats$villainId <- villain_to_id(seats$villain)
        } else {
          seats$villainId <- ""
        }
      }
      seats$player <- trimws(as.character(seats$player))
      seats$villainId <- trimws(as.character(seats$villainId))
      seats <- seats[seats$player != "" & seats$villainId != "", , drop = FALSE]
      nrow(seats) == target_n
    }, g)
  })
  
  villain_detail <- reactive({
    vid <- input$villainDetailId %||% ""
    target_n <- clamp_player_count(input$villainDetailPlayerCount %||% 2L)
    if (!nzchar(vid)) {
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
    
    build_villain_detail(villain_focus_games(), vid, player_count = target_n)
  })

  villain_focus_profiles <- reactive({
    build_profiles(villain_focus_games(), normalize_by_players = isTRUE(input$dur_norm %||% TRUE))$villains
  })

  villain_detail_ranks <- reactive({
    vid <- input$villainDetailId %||% ""
    prof <- villain_focus_profiles()
    
    empty <- list(winrate = "\u2014", games = "\u2014")
    if (!nzchar(vid) || nrow(prof) == 0 || !vid %in% prof$key) return(empty)
    
    total <- nrow(prof)
    
    wr_tbl <- prof[order(-prof$Winrate, -prof$Games, prof$key), c("key", "Winrate", "Games"), drop = FALSE]
    wr_tbl$Rank <- seq_len(nrow(wr_tbl))
    wr_rank <- wr_tbl$Rank[match(vid, wr_tbl$key)]
    
    games_tbl <- prof[order(-prof$Games, -prof$Winrate, prof$key), c("key", "Games", "Winrate"), drop = FALSE]
    games_tbl$Rank <- seq_len(nrow(games_tbl))
    games_rank <- games_tbl$Rank[match(vid, games_tbl$key)]
    
    list(
      winrate = paste0("#", wr_rank, " / ", total),
      games = paste0("#", games_rank, " / ", total)
    )
  })

  villain_focus_matchups <- reactive({
    vid <- input$villainDetailId %||% ""
    g <- villain_focus_games()
    if (!nzchar(vid) || length(g) == 0) return(data.frame())
    
    wins_env <- new.env(parent = emptyenv())
    h2h_env <- new.env(parent = emptyenv())
    together_env <- new.env(parent = emptyenv())
    
    add_key <- function(env, k, v = 1L) {
      if (!exists(k, envir = env, inherits = FALSE)) assign(k, v, envir = env)
      else assign(k, get(k, envir = env, inherits = FALSE) + v, envir = env)
    }
    
    for (game in g) {
      seats <- game$seats
      if (is.null(seats) || !is.data.frame(seats)) next
      if (!("villainId" %in% names(seats))) {
        if ("villain" %in% names(seats)) {
          seats$villainId <- villain_to_id(seats$villain)
        } else {
          seats$villainId <- ""
        }
      }
      
      seats$player <- trimws(as.character(seats$player))
      seats$villainId <- trimws(as.character(seats$villainId))
      seats <- seats[seats$player != "" & seats$villainId != "", c("player", "villainId"), drop = FALSE]
      if (!vid %in% seats$villainId) next
      
      opponents <- sort(unique(seats$villainId[seats$villainId != vid]))
      if (length(opponents) == 0) next
      
      winner_player <- trimws(game$winnerPlayer %||% "")
      if (winner_player == "" || !winner_player %in% seats$player) next
      winner_vill <- trimws(seats$villainId[match(winner_player, seats$player)])
      if (winner_vill == "") next
      
      for (opp in opponents) {
        add_key(together_env, opp, 1L)
        if (winner_vill %in% c(vid, opp)) {
          add_key(h2h_env, opp, 1L)
          if (winner_vill == vid) add_key(wins_env, opp, 1L)
        }
      }
    }
    
    opps <- sort(unique(c(ls(together_env), ls(h2h_env), ls(wins_env))))
    if (length(opps) == 0) return(data.frame())
    
    out <- data.frame(
      OpponentId = opps,
      GamesTogether = vapply(opps, function(k) get0(k, envir = together_env, ifnotfound = 0L), integer(1)),
      H2HGames = vapply(opps, function(k) get0(k, envir = h2h_env, ifnotfound = 0L), integer(1)),
      Wins = vapply(opps, function(k) get0(k, envir = wins_env, ifnotfound = 0L), integer(1)),
      stringsAsFactors = FALSE
    )
    out <- out[out$GamesTogether > 0, , drop = FALSE]
    if (nrow(out) == 0) return(out)
    
    out$Winrate <- ifelse(out$H2HGames > 0, out$Wins / out$H2HGames, NA_real_)
    out <- out[order(-out$Winrate, -out$H2HGames, -out$GamesTogether, out$OpponentId), , drop = FALSE]
    rownames(out) <- NULL
    out
  })

  observe({
    updateNumericInput(
      session,
      "playerDetailPlayerCount",
      value = clamp_player_count(input$playerDetailPlayerCount %||% 2L),
      min = 2,
      max = 6,
      step = 1
    )
  })

  observe({
    players <- known_players()
    current_sel <- isolate(input$playerDetailId %||% "")
    if (!(current_sel %in% players)) {
      current_sel <- if (length(players) > 0) players[1] else NULL
    }

    updateSelectizeInput(
      session, "playerDetailId",
      label = tr("player_pick"),
      choices = players,
      selected = current_sel,
      server = FALSE
    )
  })

  player_focus_games <- reactive({
    target_n <- clamp_player_count(input$playerDetailPlayerCount %||% 2L)
    g <- games()
    if (length(g) == 0) return(g)

    Filter(function(game) {
      seats <- game$seats
      if (is.null(seats) || !is.data.frame(seats)) return(FALSE)
      if (!("villainId" %in% names(seats))) {
        if ("villain" %in% names(seats)) {
          seats$villainId <- villain_to_id(seats$villain)
        } else {
          seats$villainId <- ""
        }
      }
      seats$player <- trimws(as.character(seats$player))
      seats$villainId <- trimws(as.character(seats$villainId))
      seats <- seats[seats$player != "" & seats$villainId != "", , drop = FALSE]
      nrow(seats) == target_n
    }, g)
  })

  player_detail <- reactive({
    pid <- input$playerDetailId %||% ""
    target_n <- clamp_player_count(input$playerDetailPlayerCount %||% 2L)
    if (!nzchar(pid)) {
      return(list(
        summary = data.frame(
          playerId = character(),
          Games = integer(),
          Wins = integer(),
          Winrate = numeric(),
          stringsAsFactors = FALSE
        ),
        by_villain = data.frame(
          Rank = integer(),
          VillainId = character(),
          Games = integer(),
          Wins = integer(),
          Winrate = numeric(),
          stringsAsFactors = FALSE
        )
      ))
    }

    build_player_detail(player_focus_games(), pid, player_count = target_n)
  })

  player_focus_profiles <- reactive({
    build_profiles(player_focus_games(), normalize_by_players = isTRUE(input$dur_norm %||% TRUE))$players
  })

  player_detail_ranks <- reactive({
    pid <- input$playerDetailId %||% ""
    prof <- player_focus_profiles()

    empty <- list(winrate = "\u2014", games = "\u2014")
    if (!nzchar(pid) || nrow(prof) == 0 || !pid %in% prof$key) return(empty)

    total <- nrow(prof)

    wr_tbl <- prof[order(-prof$Winrate, -prof$Games, prof$key), c("key", "Winrate", "Games"), drop = FALSE]
    wr_tbl$Rank <- seq_len(nrow(wr_tbl))
    wr_rank <- wr_tbl$Rank[match(pid, wr_tbl$key)]

    games_tbl <- prof[order(-prof$Games, -prof$Winrate, prof$key), c("key", "Games", "Winrate"), drop = FALSE]
    games_tbl$Rank <- seq_len(nrow(games_tbl))
    games_rank <- games_tbl$Rank[match(pid, games_tbl$key)]

    list(
      winrate = paste0("#", wr_rank, " / ", total),
      games = paste0("#", games_rank, " / ", total)
    )
  })

  player_focus_matchups <- reactive({
    pid <- input$playerDetailId %||% ""
    g <- player_focus_games()
    if (!nzchar(pid) || length(g) == 0) return(data.frame())

    wins_env <- new.env(parent = emptyenv())
    h2h_env <- new.env(parent = emptyenv())
    together_env <- new.env(parent = emptyenv())

    add_key <- function(env, k, v = 1L) {
      if (!exists(k, envir = env, inherits = FALSE)) assign(k, v, envir = env)
      else assign(k, get(k, envir = env, inherits = FALSE) + v, envir = env)
    }

    for (game in g) {
      seats <- game$seats
      if (is.null(seats) || !is.data.frame(seats)) next
      if (!("villainId" %in% names(seats))) {
        if ("villain" %in% names(seats)) {
          seats$villainId <- villain_to_id(seats$villain)
        } else {
          seats$villainId <- ""
        }
      }

      seats$player <- trimws(as.character(seats$player))
      seats$villainId <- trimws(as.character(seats$villainId))
      seats <- seats[seats$player != "" & seats$villainId != "", c("player", "villainId"), drop = FALSE]
      if (!pid %in% seats$player) next

      player_villain <- trimws(seats$villainId[match(pid, seats$player)])
      if (!nzchar(player_villain)) next

      opponents <- sort(unique(seats$villainId[seats$player != pid]))
      if (length(opponents) == 0) next

      winner_player <- trimws(game$winnerPlayer %||% "")
      if (winner_player == "" || !winner_player %in% seats$player) next
      winner_vill <- trimws(seats$villainId[match(winner_player, seats$player)])
      if (winner_vill == "") next

      for (opp in opponents) {
        add_key(together_env, opp, 1L)
        if (winner_vill %in% c(player_villain, opp)) {
          add_key(h2h_env, opp, 1L)
          if (winner_player == pid) add_key(wins_env, opp, 1L)
        }
      }
    }

    opps <- sort(unique(c(ls(together_env), ls(h2h_env), ls(wins_env))))
    if (length(opps) == 0) return(data.frame())

    out <- data.frame(
      OpponentId = opps,
      GamesTogether = vapply(opps, function(k) get0(k, envir = together_env, ifnotfound = 0L), integer(1)),
      H2HGames = vapply(opps, function(k) get0(k, envir = h2h_env, ifnotfound = 0L), integer(1)),
      Wins = vapply(opps, function(k) get0(k, envir = wins_env, ifnotfound = 0L), integer(1)),
      stringsAsFactors = FALSE
    )
    out <- out[out$GamesTogether > 0, , drop = FALSE]
    if (nrow(out) == 0) return(out)

    out$Winrate <- ifelse(out$H2HGames > 0, out$Wins / out$H2HGames, NA_real_)
    out <- out[order(-out$Winrate, -out$H2HGames, -out$GamesTogether, out$OpponentId), , drop = FALSE]
    rownames(out) <- NULL
    out
  })
  
  output$villainDetailWinrate <- renderText({
    s <- villain_detail()$summary
    if (nrow(s) == 0 || is.na(s$Winrate[1])) return("—")
    paste0(round(100 * s$Winrate[1], 1), " %")
  })
  
  output$villainDetailGames <- renderText({
    s <- villain_detail()$summary
    if (nrow(s) == 0) return("0")
    as.character(s$Games[1])
  })

  output$villainDetailWinrateRank <- renderText({
    villain_detail_ranks()$winrate
  })

  output$villainDetailGamesRank <- renderText({
    villain_detail_ranks()$games
  })
  
  output$villainDetailPlayersTable <- renderReactable({
    df <- villain_detail()$by_player
    lang <- currentLang()
    
    if (nrow(df) == 0) {
      empty_df <- if (lang == "fr") {
        data.frame(
          Rang = integer(),
          Joueur = character(),
          Parties = integer(),
          Victoires = integer(),
          Winrate = numeric(),
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          Rank = integer(),
          Player = character(),
          Games = integer(),
          Wins = integer(),
          Winrate = numeric(),
          stringsAsFactors = FALSE
        )
      }
      
      rownames(empty_df) <- NULL
      return(reactable(
        empty_df,
        rownames = FALSE,
        searchable = TRUE,
        pagination = TRUE,
        defaultPageSize = 12,
        compact = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = FALSE,
        fullWidth = TRUE,
        defaultColDef = colDef(align = "center", headerStyle = list(fontWeight = "800")),
        class = "villain-reactable"
      ))
    }
    
    shown <- df
    rownames(shown) <- NULL
    
    if (lang == "fr") {
      names(shown) <- c("Rang", "Joueur", "Parties", "Victoires", "Winrate")
      shown$Winrate <- paste0(round(shown$Winrate * 100, 1), " %")
      rank_col <- "Rang"
      player_col <- "Joueur"
      games_col <- "Parties"
      wins_col <- "Victoires"
    } else {
      shown$Winrate <- paste0(round(shown$Winrate * 100, 1), " %")
      rank_col <- "Rank"
      player_col <- "Player"
      games_col <- "Games"
      wins_col <- "Wins"
    }
    
    reactable(
      shown,
      rownames = FALSE,
      searchable = TRUE,
      pagination = TRUE,
      defaultPageSize = 12,
      compact = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = FALSE,
      fullWidth = TRUE,
      columns = stats::setNames(
        list(
          colDef(align = "center"),
          colDef(align = "left"),
          colDef(align = "right"),
          colDef(align = "right"),
          colDef(align = "right")
        ),
        c(rank_col, player_col, games_col, wins_col, "Winrate")
      ),
      defaultColDef = colDef(
        align = "center",
        headerStyle = list(
          fontWeight = "800",
          textTransform = "none"
        )
      ),
      theme = app_reactable_theme(),
      class = "villain-reactable"
    )
  })

  output$playerDetailWinrate <- renderText({
    s <- player_detail()$summary
    if (nrow(s) == 0 || is.na(s$Winrate[1])) return("—")
    paste0(round(100 * s$Winrate[1], 1), " %")
  })

  output$playerDetailGames <- renderText({
    s <- player_detail()$summary
    if (nrow(s) == 0) return("0")
    as.character(s$Games[1])
  })

  output$playerDetailWinrateRank <- renderText({
    player_detail_ranks()$winrate
  })

  output$playerDetailGamesRank <- renderText({
    player_detail_ranks()$games
  })

  output$playerDetailVillainsTable <- renderReactable({
    df <- player_detail()$by_villain
    lang <- currentLang()

    if (nrow(df) == 0) {
      empty_df <- if (lang == "fr") {
        data.frame(
          Rang = integer(),
          Vilain = character(),
          Parties = integer(),
          Victoires = integer(),
          Winrate = numeric(),
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          Rank = integer(),
          Villain = character(),
          Games = integer(),
          Wins = integer(),
          Winrate = numeric(),
          stringsAsFactors = FALSE
        )
      }

      rownames(empty_df) <- NULL
      return(reactable(
        empty_df,
        rownames = FALSE,
        searchable = TRUE,
        pagination = TRUE,
        defaultPageSize = 12,
        compact = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = FALSE,
        fullWidth = TRUE,
        defaultColDef = colDef(align = "center", headerStyle = list(fontWeight = "800")),
        class = "villain-reactable"
      ))
    }

    shown <- data.frame(
      Rank = df$Rank,
      Villain = vapply(df$VillainId, label_villain, character(1), lang = lang),
      Games = df$Games,
      Wins = df$Wins,
      Winrate = paste0(round(df$Winrate * 100, 1), " %"),
      stringsAsFactors = FALSE
    )
    rownames(shown) <- NULL

    if (lang == "fr") {
      names(shown) <- c("Rang", "Vilain", "Parties", "Victoires", "Winrate")
      rank_col <- "Rang"
      villain_col <- "Vilain"
      games_col <- "Parties"
      wins_col <- "Victoires"
    } else {
      rank_col <- "Rank"
      villain_col <- "Villain"
      games_col <- "Games"
      wins_col <- "Wins"
    }

    reactable(
      shown,
      rownames = FALSE,
      searchable = TRUE,
      pagination = TRUE,
      defaultPageSize = 12,
      compact = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = FALSE,
      fullWidth = TRUE,
      columns = stats::setNames(
        list(
          colDef(align = "center"),
          colDef(align = "left"),
          colDef(align = "right"),
          colDef(align = "right"),
          colDef(align = "right")
        ),
        c(rank_col, villain_col, games_col, wins_col, "Winrate")
      ),
      defaultColDef = colDef(
        align = "center",
        headerStyle = list(
          fontWeight = "800",
          textTransform = "none"
        )
      ),
      theme = app_reactable_theme(),
      class = "villain-reactable"
    )
  })

  player_focus_villains_cards <- reactive({
    df <- player_detail()$by_villain
    if (nrow(df) == 0) return(data.frame())
    out <- data.frame(
      OpponentId = df$VillainId,
      GamesTogether = df$Games,
      H2HGames = df$Games,
      Wins = df$Wins,
      Winrate = df$Winrate,
      stringsAsFactors = FALSE
    )
    rownames(out) <- NULL
    out
  })

  render_focus_matchup_cards <- function(df, mode = c("best", "worst"), empty_text, label_fn) {
    mode <- match.arg(mode)
    lang <- currentLang()
    eligible <- df[!is.na(df$Winrate) & df$H2HGames > 0, , drop = FALSE]
    
    if (nrow(eligible) == 0) {
      return(tags$div(class = "villain-matchup-empty", empty_text))
    }
    
    ord <- if (mode == "best") {
      order(-eligible$Winrate, -eligible$H2HGames, eligible$OpponentId)
    } else {
      order(eligible$Winrate, -eligible$H2HGames, eligible$OpponentId)
    }
    shown <- head(eligible[ord, , drop = FALSE], 3)
    
    labels <- if (mode == "best") {
      c("Top 1", "Top 2", "Top 3")
    } else {
      c("Bottom 1", "Bottom 2", "Bottom 3")
    }
    if (lang == "fr") {
      labels <- if (mode == "best") c("Top 1", "Top 2", "Top 3") else c("Flop 1", "Flop 2", "Flop 3")
    }
    
    tags$div(
      class = "villain-matchup-stack",
      lapply(seq_len(nrow(shown)), function(i) {
        row <- shown[i, , drop = FALSE]
        tags$div(
          class = paste(
            "villain-matchup-card",
            if (mode == "best") "best" else "worst",
            paste0("is-rank-", i)
          ),
          tags$div(
            class = "villain-matchup-layout",
            tags$div(
              class = "villain-matchup-main",
              tags$div(class = "villain-matchup-rank", labels[i]),
              tags$div(class = "villain-matchup-name", label_fn(row$OpponentId, lang))
            ),
            tags$div(
              class = "villain-matchup-stats",
              tags$div(
                class = "villain-matchup-stat",
                tags$div(
                  class = "villain-matchup-stat-label",
                  if (lang == "fr") "Winrate" else "Winrate"
                ),
                tags$div(class = "villain-matchup-winrate", paste0(round(100 * row$Winrate, 1), " %"))
              ),
              tags$div(
                class = "villain-matchup-stat",
                tags$div(
                  class = "villain-matchup-stat-label",
                  if (lang == "fr") "H2H" else "H2H"
                ),
                tags$div(
                  class = "villain-matchup-meta",
                  paste0(
                    row$Wins, "/", row$H2HGames, " ",
                    if (lang == "fr") "wins" else "wins"
                  )
                )
              ),
              tags$div(
                class = "villain-matchup-stat",
                tags$div(
                  class = "villain-matchup-stat-label",
                  if (lang == "fr") "Ensemble" else "Together"
                ),
                tags$div(
                  class = "villain-matchup-caption",
                  paste0(row$GamesTogether, " ", if (lang == "fr") "parties" else "games")
                )
              )
            )
          )
        )
      })
    )
  }

  output$villainDetailBestMatchups <- renderUI({
    render_focus_matchup_cards(
      villain_focus_matchups(),
      mode = "best",
      empty_text = tr("villain_matchups_none"),
      label_fn = label_villain
    )
  })

  output$villainDetailWorstMatchups <- renderUI({
    render_focus_matchup_cards(
      villain_focus_matchups(),
      mode = "worst",
      empty_text = tr("villain_matchups_none"),
      label_fn = label_villain
    )
  })

  output$villainDetailMatchupsTable <- renderReactable({
    df <- villain_focus_matchups()
    lang <- currentLang()
    
    if (nrow(df) == 0) {
      empty_df <- data.frame(
        Opponent = character(),
        H2HGames = integer(),
        GamesTogether = integer(),
        Wins = integer(),
        Winrate = numeric(),
        stringsAsFactors = FALSE
      )
      
      if (lang == "fr") names(empty_df) <- c("Adversaire", "Parties H2H", "Parties ensemble", "Victoires", "Winrate")
      else names(empty_df) <- c("Opponent", "H2H games", "Games together", "Wins", "Winrate")
      return(build_reactable(
        empty_df,
        page_size = 12,
        columns = list(
          Adversaire = colDef(align = "left"),
          Opponent = colDef(align = "left")
        )
      ))
    }
    
    shown <- data.frame(
      Opponent = vapply(df$OpponentId, label_villain, character(1), lang = lang),
      H2HGames = df$H2HGames,
      GamesTogether = df$GamesTogether,
      Wins = df$Wins,
      Winrate = format_pct_label(df$Winrate),
      stringsAsFactors = FALSE
    )
    if (lang == "fr") names(shown) <- c("Adversaire", "Parties H2H", "Parties ensemble", "Victoires", "Winrate")
    build_reactable(
      shown,
      page_size = 12,
      columns = list(
        Adversaire = colDef(align = "left"),
        Opponent = colDef(align = "left"),
        `Parties H2H` = colDef(align = "right"),
        `Parties ensemble` = colDef(align = "right"),
        Victoires = colDef(align = "right"),
        `H2H games` = colDef(align = "right"),
        `Games together` = colDef(align = "right"),
        Wins = colDef(align = "right"),
        Winrate = colDef(align = "right")
      )
    )
  })

  output$playerDetailBestMatchups <- renderUI({
    render_focus_matchup_cards(
      player_focus_matchups(),
      mode = "best",
      empty_text = tr("player_matchups_none"),
      label_fn = label_villain
    )
  })

  output$playerDetailBestVillains <- renderUI({
    render_focus_matchup_cards(
      player_focus_villains_cards(),
      mode = "best",
      empty_text = tr("player_matchups_none"),
      label_fn = label_villain
    )
  })

  output$playerDetailWorstVillains <- renderUI({
    render_focus_matchup_cards(
      player_focus_villains_cards(),
      mode = "worst",
      empty_text = tr("player_matchups_none"),
      label_fn = label_villain
    )
  })

  output$playerDetailWorstMatchups <- renderUI({
    render_focus_matchup_cards(
      player_focus_matchups(),
      mode = "worst",
      empty_text = tr("player_matchups_none"),
      label_fn = label_villain
    )
  })

  output$playerDetailMatchupsTable <- renderReactable({
    df <- player_focus_matchups()
    lang <- currentLang()

    if (nrow(df) == 0) {
      empty_df <- data.frame(
        Opponent = character(),
        H2HGames = integer(),
        GamesTogether = integer(),
        Wins = integer(),
        Winrate = numeric(),
        stringsAsFactors = FALSE
      )

      if (lang == "fr") names(empty_df) <- c("Adversaire", "Parties H2H", "Parties ensemble", "Victoires", "Winrate")
      else names(empty_df) <- c("Opponent", "H2H games", "Games together", "Wins", "Winrate")
      return(build_reactable(
        empty_df,
        page_size = 12,
        columns = list(
          Adversaire = colDef(align = "left"),
          Opponent = colDef(align = "left")
        )
      ))
    }

    shown <- data.frame(
      Opponent = vapply(df$OpponentId, label_villain, character(1), lang = lang),
      H2HGames = df$H2HGames,
      GamesTogether = df$GamesTogether,
      Wins = df$Wins,
      Winrate = format_pct_label(df$Winrate),
      stringsAsFactors = FALSE
    )
    if (lang == "fr") names(shown) <- c("Adversaire", "Parties H2H", "Parties ensemble", "Victoires", "Winrate")
    build_reactable(
      shown,
      page_size = 12,
      columns = list(
        Adversaire = colDef(align = "left"),
        Opponent = colDef(align = "left"),
        `Parties H2H` = colDef(align = "right"),
        `Parties ensemble` = colDef(align = "right"),
        Victoires = colDef(align = "right"),
        `H2H games` = colDef(align = "right"),
        `Games together` = colDef(align = "right"),
        Wins = colDef(align = "right"),
        Winrate = colDef(align = "right")
      )
    )
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
      if (is.null(seats) || !is.data.frame(seats)) next
      
      seats$player <- trimws(as.character(seats$player))
      seats$villainId <- trimws(as.character(seats$villainId))
      seats <- seats[seats$player != "" & seats$villainId != "", , drop = FALSE]
      
      n <- nrow(seats)
      if (n < 2 || n > 6) next
      
      winner <- trimws(g$winnerPlayer %||% "")
      if (!winner %in% seats$player) next
      
      for (i in seq_len(n)) {
        ensure_player(seats$player[i])
        ensure_villain(seats$villainId[i])
      }
      
      combined <- numeric(n)
      for (i in seq_len(n)) {
        p <- seats$player[i]
        v <- seats$villainId[i]
        combined[i] <- playerR[p] + villainR[v]
      }
      
      q <- 10^(combined / scale)
      probs <- q / sum(q)
      S <- ifelse(seats$player == winner, 1, 0)
      
      # same normalization as compute_elo()
      K_eff <- k * (n / (2 * (n - 1)))
      d_combined <- K_eff * (S - probs)
      
      for (i in seq_len(n)) {
        p <- seats$player[i]
        v <- seats$villainId[i]
        playerR[p] <- playerR[p] + alpha * d_combined[i]
        villainR[v] <- villainR[v] + (1 - alpha) * d_combined[i]
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
    
    # Filter by player count if requested
    nf <- input$mu_n %||% "all"
    if (nf != "all") {
      nf_i <- as.integer(nf)
      g <- Filter(function(x) nrow(x$seats) == nf_i, g)
    }
    if (length(g) == 0) return(data.frame())
    
    wins_env   <- new.env(parent = emptyenv())  # wins for ordered A|||B
    h2h_env    <- new.env(parent = emptyenv())  # head-to-head games where winner ∈ {A,B}
    together_env <- new.env(parent = emptyenv())# games where both A and B are present
    
    add_key <- function(env, k, v) {
      if (!exists(k, envir = env, inherits = FALSE)) assign(k, v, envir = env)
      else assign(k, get(k, env) + v, envir = env)
    }
    
    for (game in g) {
      seats <- game$seats
      if (is.null(seats) || !is.data.frame(seats)) next
      
      vlist <- unique(trimws(seats$villainId))
      vlist <- vlist[vlist != ""]
      if (length(vlist) < 2) next
      
      winner_player <- trimws(game$winnerPlayer %||% "")
      if (winner_player == "" || !winner_player %in% seats$player) next
      
      winner_vill <- trimws(seats$villainId[match(winner_player, seats$player)])
      if (winner_vill == "") next
      
      # iterate unordered pairs (i < j)
      for (i in 1:(length(vlist) - 1)) {
        for (j in (i + 1):length(vlist)) {
          A <- vlist[i]
          B <- vlist[j]
          
          keyAB <- paste0(A, "|||", B)
          keyBA <- paste0(B, "|||", A)
          
          # Always count "present together"
          add_key(together_env, keyAB, 1)
          add_key(together_env, keyBA, 1)
          
          # Only count H2H when winner is A or B
          if (winner_vill == A || winner_vill == B) {
            add_key(h2h_env, keyAB, 1)
            add_key(h2h_env, keyBA, 1)
            
            # Winner gets the win against the other
            if (winner_vill == A) add_key(wins_env, keyAB, 1)
            if (winner_vill == B) add_key(wins_env, keyBA, 1)
          }
        }
      }
    }
    
    keys <- ls(together_env)
    if (length(keys) == 0) return(data.frame())
    
    out <- do.call(rbind, lapply(keys, function(k) {
      parts <- strsplit(k, "\\|\\|\\|")[[1]]
      A <- parts[1]; B <- parts[2]
      
      games_together <- get(k, together_env)
      h2h_games <- if (exists(k, h2h_env, inherits = FALSE)) get(k, h2h_env) else 0
      wins <- if (exists(k, wins_env, inherits = FALSE)) get(k, wins_env) else 0
      
      winrate <- if (h2h_games > 0) wins / h2h_games else NA_real_
      
      data.frame(
        VillainId = A,
        OpponentId = B,
        GamesTogether = games_together, # présence commune (info)
        H2HGames = h2h_games,           # ✅ base du winrate
        Winrate = winrate,
        stringsAsFactors = FALSE
      )
    }))
    
    # Filter on min H2H games (not "together")
    min_g <- input$mu_min_games %||% 2
    out <- out[out$H2HGames >= min_g, , drop = FALSE]
    
    # Optional focus filter
    focus <- input$mu_focus %||% "all"
    if (focus != "all") out <- out[out$VillainId == focus, , drop = FALSE]
    
    if (nrow(out) == 0) return(out)
    
    out <- out[order(-out$H2HGames, -out$Winrate, -out$GamesTogether, out$VillainId, out$OpponentId), , drop = FALSE]
    out
  })
  
  output$matchupTable <- renderReactable({
    df <- matchup_df()
    lang <- currentLang()
    if (nrow(df) == 0) {
      empty_df <- data.frame(Villain = character(), Opponent = character(), GamesTogether = integer(), H2HGames = integer(), Winrate = character())
      if (lang == "fr") names(empty_df) <- c("Vilain", "Adversaire", "Parties ensemble", "Parties H2H", "Winrate")
      return(build_reactable(
        empty_df,
        page_size = 12,
        columns = list(
          Vilain = colDef(align = "left"),
          Adversaire = colDef(align = "left"),
          Villain = colDef(align = "left"),
          Opponent = colDef(align = "left")
        )
      ))
    }
    
    shown <- data.frame(
      Villain = vapply(df$VillainId, label_villain, character(1), lang = lang),
      Opponent = vapply(df$OpponentId, label_villain, character(1), lang = lang),
      GamesTogether = df$GamesTogether,
      H2HGames = df$H2HGames,
      Winrate = format_pct_label(df$Winrate),
      stringsAsFactors = FALSE
    )
    if (lang == "fr") names(shown) <- c("Vilain", "Adversaire", "Parties ensemble", "Parties H2H", "Winrate")
    build_reactable(
      shown,
      page_size = 12,
      columns = list(
        Vilain = colDef(align = "left"),
        Adversaire = colDef(align = "left"),
        Villain = colDef(align = "left"),
        Opponent = colDef(align = "left"),
        `Parties ensemble` = colDef(align = "right"),
        `Parties H2H` = colDef(align = "right"),
        GamesTogether = colDef(align = "right"),
        H2HGames = colDef(align = "right"),
        Winrate = colDef(align = "right")
      )
    )
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
    
    pal <- c(
      "#6b2230",  # 0%  -> bordeaux sombre (faible)
      "#CB7A8A",  
      "#c3c3c3",   # 50% -> gris neutre (milieu)
      "#5DC382",
      "#2f7d4b"    # 100%-> vert Villainous (fort)
    )
    
    na_col <- "#3a3a3a"  # ✅ “No data” (distinct de 0%)
    
    p <- ggplot(grid_df, aes(x = OpponentLab, y = VillainLab, fill = Winrate)) +
      geom_tile(color = "#1f2a3a", linewidth = 0.4, na.rm = FALSE) +
      coord_fixed() +
      scale_fill_gradientn(
        colours = pal,
        values  = c(0, 0.25, 0.5, 0.75, 1),      # ✅ 5 points: low / mid / high
        limits  = c(0, 1),
        breaks  = c(0, .25, .5, .75, 1),
        labels  = c("0%", "25%", "50%", "75%", "100%"),
        na.value = na_col,           # ✅ cases non évaluées en gris
        name    = "Winrate"
      ) +
      labs(
        x = if (lang == "fr") "Adversaire" else "Opponent",
        y = if (lang == "fr") "Vilain" else "Villain",
        title = if (lang == "fr") "Winrate (Vilain vs Adversaire)" else "Winrate (Villain vs Opponent)",
        caption = if (lang == "fr") "Non évalué = gris" else "No data = grey"
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
  
  output$predTable <- renderReactable({
    df <- pred_data()
    if (is.null(df) || nrow(df) == 0) {
      return(build_reactable(
        data.frame(Seat = character(), Player = character(), Villain = character(), Elo_Total = integer(), Prob_pct = character()),
        page_size = 10,
        columns = list(
          Player = colDef(align = "left"),
          Villain = colDef(align = "left")
        )
      ))
    }
    shown <- df
    shown$Prob_pct <- paste0(shown$Prob_pct, " %")
    build_reactable(
      shown,
      page_size = 10,
      columns = list(
        Player = colDef(align = "left"),
        Villain = colDef(align = "left"),
        Elo_Player = colDef(align = "right"),
        Elo_Villain = colDef(align = "right"),
        Elo_Total = colDef(align = "right"),
        Prob_pct = colDef(align = "right")
      )
    )
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
  output$historyTable <- renderReactable({
    g <- games()
    lang <- currentLang()
    if (length(g) == 0) {
      return(build_reactable(
        data.frame(Date = character(), DurationMin = integer(), Winner = character(), Seats = character()),
        page_size = 10,
        columns = list(
          Winner = colDef(align = "left"),
          Seats = colDef(align = "left")
        )
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
    build_reactable(
      df,
      page_size = 10,
      columns = list(
        Winner = colDef(align = "left"),
        Seats = colDef(align = "left"),
        DurationMin = colDef(align = "right")
      )
    )
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
