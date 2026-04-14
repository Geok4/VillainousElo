# app.R — entrypoint
source("R/00_packages.R", local = FALSE)
source("R/01_utils.R", local = FALSE)
source("R/02_i18n.R", local = FALSE)
source("R/03_catalog.R", local = FALSE)
source("R/04_elo.R", local = FALSE)
source("R/05_profiles.R", local = FALSE)
source("R/06_theme_css.R", local = FALSE)
source("R/07_ui_builder.R", local = FALSE)

source("ui.R", local = FALSE)
source("server.R", local = FALSE)

shinyApp(ui = ui, server = server)
