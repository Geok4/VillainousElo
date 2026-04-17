# R/07_ui_builder.R
build_ui <- function(lang, lang_button_label, selected_tab = "input") {
  t <- function(key) tr_lang(lang, key)
  
  page_navbar(
    title = tags$div(
      class = "app-brand",
      tags$span(class = "app-title", t("app_title"))
    ),
    
    theme = bs_theme_app,
    fillable = FALSE,
    id = "main_nav",
    selected = selected_tab,
    header = tagList(tags$style(HTML(app_css))),
    
    nav_item(
      div(style = "padding: 6px 10px; display:flex; gap:8px; align-items:center;",
          actionButton("openElo", t("elo_params"), class = "btn btn-sm btn-outline-light"),
          actionButton("toggleLang", lang_button_label, class = "btn btn-sm btn-primary")
      )
    ),
    
    nav_panel(
      t("tab_input"), value = "input",
      layout_sidebar(
        sidebar = sidebar(
          card(
            card_header(t("boxes_header")),
            checkboxGroupInput(
              "setsOwned",
              t("boxes_label"),
              choices = setNames(sets_tbl$set_id, if (lang == "fr") sets_tbl$set_fr else sets_tbl$set_en),
              selected = sets_tbl$set_id
            ),
            tags$div(class = "small-muted", t("boxes_hint"))
          ),
          width = 340
        ),
        layout_columns(
          fill = FALSE,
          col_widths = 12,
          card(
            card_header(t("new_game")),
            layout_columns(
              fill = FALSE,
              col_widths = c(12, 6, 6),
              dateInput("playedAt", t("date"), value = Sys.Date()),
              numericInput("durationMin", t("duration_min"), value = 60, min = 5, step = 5),
              numericInput("numPlayers", t("num_players"), value = 2, min = 2, max = 6, step = 1)
            ),
            tags$div(class = "small-muted", t("seats_hint")),
            tags$div(style = "margin-top: 10px;", uiOutput("seats_ui")),
            selectInput("winnerPlayer", t("winner"), choices = character(), selected = NULL),
            actionButton("addGame", t("add_game"), class = "btn-primary"),
            tags$div(style = "margin-top:10px; color:#ef4444;", textOutput("errorMsg"))
          )
        )
      )
    ),
    
    nav_panel(
      t("tab_dashboard"), value = "dashboard",
      div(class = "tab-scroll",
          layout_columns(
            fill = FALSE,
            col_widths = c(4, 4, 4),
            card(card_header(t("kpi_games")), div(class = "kpi", textOutput("kpi_games")), div(class = "kpi-label", t("total"))),
            card(card_header(t("kpi_avg_dur")), div(class = "kpi", textOutput("kpi_avg_dur")), div(class = "kpi-label", t("average"))),
            card(card_header(t("kpi_hours")), div(class = "kpi", textOutput("kpi_hours")), div(class = "kpi-label", t("total")))
          ),
          
          layout_columns(
            fill = FALSE,
            col_widths = c(6, 6),
            card(card_header(t("top3_players")), uiOutput("topPlayersBadges")),
            card(card_header(t("top3_villains")), uiOutput("topVillainsBadges"))
          ),
          
          layout_columns(
            fill = FALSE,
            col_widths = c(6, 6),
            card(card_header(t("leaderboard_players")), reactableOutput("playersTable")),
            card(card_header(t("leaderboard_villains")), reactableOutput("villainsTable"))
          ),
          
          layout_columns(
            fill = FALSE,
            col_widths = c(6, 6),
            card(
              card_header(t("elo_timeline")),
              layout_columns(
                fill = FALSE,
                col_widths = c(6, 6),
                selectInput("timelineType", t("type"), choices = c("Player" = "player", "Villain" = "villain")),
                uiOutput("timelineNameUI")
              ),
              plotOutput("eloTimelinePlot", height = 260)
            ),
            card(
              card_header(t("duration_win_loss")),
              layout_columns(
                fill = FALSE,
                col_widths = c(6, 6),
                selectInput("durType", t("type"), choices = c("Player" = "player", "Villain" = "villain")),
                uiOutput("durNameUI")
              ),
              plotOutput("durationWinLossPlot", height = 260)
            )
          )
      )
    ),
    
    nav_panel(
      t("tab_villain_focus"), value = "villain_focus",
      div(
        class = "tab-scroll",
        
        div(
          class = "villain-focus-top-row villain-focus-top-row--villain",
          card(
            class = "villain-focus-filter villain-focus-top-card",
            card_header(t("villain_pick")),
            div(
              class = "villain-focus-filter-layout",
              div(
                class = "villain-focus-hero",
                uiOutput("villainDetailHero")
              ),
              div(
                class = "villain-focus-filter-row",
                div(
                  class = "villain-focus-filter-main",
                  selectizeInput(
                    "villainDetailId",
                    t("villain_pick"),
                    choices = villain_choices_html(villains_tbl$villain_id, lang = lang, size = 26),
                    selected = villains_tbl$villain_id[1],
                    options = list(
                      placeholder = t("villain_pick"),
                      dropdownParent = "body",
                      render = villain_selectize_render
                    )
                  )
                ),
                div(
                  class = "villain-focus-filter-side",
                  numericInput(
                    "villainDetailPlayerCount",
                    t("villain_player_count"),
                    value = 2,
                    min = 2,
                    max = 6,
                    step = 1,
                    width = "100%"
                  )
                )
              )
            )
          ),
          card(
            class = "villain-focus-kpi-card villain-focus-top-card",
            card_header(t("villain_global_winrate")),
            div(
              class = "villain-focus-kpi-body",
              div(
                class = "villain-focus-kpi-main",
                div(class = "kpi", textOutput("villainDetailWinrate")),
                div(class = "kpi-label", t("average"))
              ),
              div(
                class = "villain-focus-kpi-side",
                div(class = "kpi-rank", textOutput("villainDetailWinrateRank"))
              )
            )
          ),
          card(
            class = "villain-focus-kpi-card villain-focus-top-card",
            card_header(t("villain_times_played")),
            div(
              class = "villain-focus-kpi-body",
              div(
                class = "villain-focus-kpi-main",
                div(class = "kpi", textOutput("villainDetailGames")),
                div(class = "kpi-label", t("total"))
              ),
              div(
                class = "villain-focus-kpi-side",
                div(class = "kpi-rank", textOutput("villainDetailGamesRank"))
              )
            )
          ),
          card(
            class = "villain-focus-players-card villain-focus-top-card",
            card_header(t("villain_players_ranking")),
            div(class = "villain-focus-players-wrap", reactableOutput("villainDetailPlayersTable"))
          )
        ),

        layout_columns(
          fill = FALSE,
          col_widths = c(3, 3, 6),
          card(
            card_header(t("villain_matchups_best")),
            uiOutput("villainDetailBestMatchups")
          ),
          card(
            card_header(t("villain_matchups_worst")),
            uiOutput("villainDetailWorstMatchups")
          ),
          card(
            card_header(t("villain_matchups_table")),
            div(class = "villain-focus-table-wrap", reactableOutput("villainDetailMatchupsTable"))
          )
        )
      )
    ),
    
    nav_panel(
      t("tab_player_focus"), value = "player_focus",
      div(
        class = "tab-scroll",
        
        div(
          class = "villain-focus-top-row",
          card(
            class = "villain-focus-filter villain-focus-top-card",
            card_header(t("player_pick")),
            div(
              class = "villain-focus-filter-row",
              div(
                class = "villain-focus-filter-main",
                selectizeInput(
                  "playerDetailId",
                  t("player_pick"),
                  choices = character(),
                  selected = NULL,
                  options = list(
                    placeholder = t("player_pick"),
                    dropdownParent = "body"
                  )
                )
              ),
              div(
                class = "villain-focus-filter-side",
                numericInput(
                  "playerDetailPlayerCount",
                  t("player_focus_player_count"),
                  value = 2,
                  min = 2,
                  max = 6,
                  step = 1,
                  width = "100%"
                )
              )
            )
          ),
          card(
            class = "villain-focus-kpi-card villain-focus-top-card",
            card_header(t("player_global_winrate")),
            div(
              class = "villain-focus-kpi-body",
              div(
                class = "villain-focus-kpi-main",
                div(class = "kpi", textOutput("playerDetailWinrate")),
                div(class = "kpi-label", t("average"))
              ),
              div(
                class = "villain-focus-kpi-side",
                div(class = "kpi-rank", textOutput("playerDetailWinrateRank"))
              )
            )
          ),
          card(
            class = "villain-focus-kpi-card villain-focus-top-card",
            card_header(t("player_times_played")),
            div(
              class = "villain-focus-kpi-body",
              div(
                class = "villain-focus-kpi-main",
                div(class = "kpi", textOutput("playerDetailGames")),
                div(class = "kpi-label", t("total"))
              ),
              div(
                class = "villain-focus-kpi-side",
                div(class = "kpi-rank", textOutput("playerDetailGamesRank"))
              )
            )
          )
        ),
        
        layout_columns(
          fill = FALSE,
          col_widths = c(3, 3, 6),
          card(
            card_header(t("player_villains_best")),
            uiOutput("playerDetailBestVillains")
          ),
          card(
            card_header(t("player_villains_worst")),
            uiOutput("playerDetailWorstVillains")
          ),
          card(
            class = "villain-focus-players-card",
            card_header(t("player_villains_ranking")),
            div(class = "villain-focus-players-wrap", reactableOutput("playerDetailVillainsTable"))
          )
        ),
        
        layout_columns(
          fill = FALSE,
          col_widths = c(3, 3, 6),
          card(
            card_header(t("player_matchups_best")),
            uiOutput("playerDetailBestMatchups")
          ),
          card(
            card_header(t("player_matchups_worst")),
            uiOutput("playerDetailWorstMatchups")
          ),
          card(
            class = "villain-focus-players-card",
            card_header(t("player_matchups_table")),
            div(class = "villain-focus-table-wrap", reactableOutput("playerDetailMatchupsTable"))
          )
        )
      )
    ),
    
    nav_panel(
      t("tab_matchups"), value = "matchups",
      div(class = "tab-scroll",
          layout_columns(
            fill = FALSE,
            col_widths = c(4, 8),
            
            card(
              card_header(t("matchups_filters")),
              selectInput(
                "mu_n", t("mu_players"),
                choices = c(setNames("all", t("mu_all")), "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6"),
                selected = "2"
              ),
              sliderInput("mu_min_games", t("mu_min_together"), min = 1, max = 100, value = 1, step = 1),
              selectInput(
                "mu_focus", t("mu_focus"),
                choices = setNames("all", t("mu_all")),
                selected = "all"
              ),
              tags$div(class = "small-muted", t("mu_hint"))
            ),
            
            div(
              style = "display:flex; flex-direction:column; gap:12px;",
              card(card_header(t("mu_table")), reactableOutput("matchupTable")),
              card(card_header(t("mu_heatmap")), plotOutput("matchupHeatmap", height = 560))
            )
          )
      )
    ),
    
    nav_panel(
      t("tab_predict"), value = "predict",
      div(class = "tab-scroll",
          layout_columns(
            fill = FALSE,
            col_widths = c(5, 7),
            
            card(
              card_header(t("pred_compose")),
              numericInput("pred_numPlayers", t("num_players"), value = 2, min = 2, max = 6, step = 1),
              tags$div(class = "small-muted", t("pred_hint")),
              tags$div(style = "margin-top: 10px;", uiOutput("pred_seats_ui")),
              actionButton("pred_calc", t("calculate"), class = "btn-primary"),
              tags$div(style = "margin-top:10px; color:#ef4444;", textOutput("predMsg"))
            ),
            
            card(
              card_header(t("pred_results")),
              reactableOutput("predTable"),
              hr(),
              card_header(t("pred_plot")),
              plotOutput("predPlot", height = 420)
            )
          )
      )
    ),
    
    nav_panel(
      t("tab_data"), value = "data",
      div(class = "tab-scroll",
          layout_columns(
            fill = FALSE,
            col_widths = c(6, 6),
            card(card_header(t("data_history")), reactableOutput("historyTable")),
            card(
              card_header(t("edit_game")),
              tags$p(class = "small-muted", "Select a game, edit, then apply. You can also delete."),
              selectInput("editGameId", t("select_game"), choices = character(), width = "100%"),
              
              layout_columns(
                fill = FALSE,
                col_widths = c(12, 6, 6),
                dateInput("editPlayedAt", t("date"), value = Sys.Date()),
                numericInput("editDurationMin", t("duration_min"), value = 60, min = 5, step = 5),
                numericInput("editNumPlayers", t("num_players"), value = 2, min = 2, max = 6, step = 1)
              ),
              
              tags$div(style = "margin-top: 10px;", uiOutput("editSeatsUI")),
              selectInput("editWinner", t("winner"), choices = character()),
              
              layout_columns(
                fill = FALSE,
                col_widths = c(6, 6),
                actionButton("applyEdit", t("apply_changes"), class = "btn-warning"),
                actionButton("deleteGame", t("delete_game"), class = "btn-danger")
              ),
              
              tags$div(style = "margin-top:10px; color:#ef4444;", textOutput("dataMsg")),
              hr(),
              h5(t("import_export")),
              downloadButton("exportJSON", t("export_json"), class = "btn-outline-light"),
              fileInput("importJSON", t("import_json"), accept = c(".json")),
              hr(),
              actionButton("resetAll", t("reset_all"), class = "btn-outline-danger")
            )
          )
      )
    )
  )
}
