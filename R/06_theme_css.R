# R/06_theme_css.R
bs_theme_app <- bs_theme(version = 5, bootswatch = "darkly")

# Palette Villainous (vert sombre + doré)
bs_theme_app <- bs_add_variables(
  bs_theme_app,
  `--bs-body-bg` = "#070a10",
  `--bs-body-color` = "#e9eef6",
  `--bs-card-bg` = "#0e1625",
  `--bs-border-color` = "#1a2f24",
  
  # ✅ Primary = vert Villainous
  `--bs-primary` = "#2f7d4b",
  
  # (optionnel) évite les bleus Bootstrap par défaut dans les pills
  `--bs-nav-pills-link-active-bg` = "#2f7d4b",
  `--bs-nav-pills-link-active-color` = "#ffffff",
  
  # Accents
  `--bs-success` = "#22c55e",
  `--bs-warning` = "#f59e0b",
  `--bs-danger`  = "#ef4444"
)

app_css <- "
@font-face {
  font-family: 'DisneyTitle';
  src: url('fonts/DisneyTitle.ttf') format('truetype');
  font-display: swap;
}

:root{
  --app-gold: #d8c58a;
  --app-gold-2: #f3e7c3;

  /* ✅ verts */
  --app-green: #2f7d4b;     /* vert principal */
  --app-green-2: #1f4d2e;   /* vert foncé (checkbox / hover) */
  --app-green-soft: rgba(47,125,75,0.18);

  /* fonds */
  --app-ink: #07150d;
  --app-panel: #0e1625;
}

body { font-size: 13.5px; }
.small-muted { opacity: .78; font-size: .92rem; }

.card { border-radius: 14px; box-shadow: 0 10px 28px rgba(0,0,0,0.35); }

/* ✅ ENTÊTES de sections en vert foncé (plus de violet) */
.card-header,
.bslib-card .card-header,
.card > .card-header {
  font-weight: 900;
  background: linear-gradient(180deg, rgba(47,125,75,0.22) 0%, var(--app-ink) 55%);
  border-bottom: 1px solid rgba(216,197,138,0.25);
  color: #e9eef6;
}

.btn { border-radius: 10px; }

/* =========================================================
   ✅ BOUTONS : VERT (pas bleu)
   ========================================================= */
.btn-primary, .btn.btn-primary,
.action-button.btn-primary {
  background: linear-gradient(180deg, rgba(47,125,75,0.95) 0%, rgba(31,77,46,0.92) 100%) !important;
  border: 1px solid rgba(216,197,138,0.28) !important;
  color: #ffffff !important;
  box-shadow: 0 10px 22px rgba(0,0,0,0.35);
}
.btn-primary:hover, .btn.btn-primary:hover,
.action-button.btn-primary:hover {
  background: linear-gradient(180deg, rgba(69,160,98,0.98) 0%, rgba(47,125,75,0.95) 100%) !important;
  border-color: rgba(216,197,138,0.38) !important;
}
.btn-primary:focus, .btn.btn-primary:focus,
.action-button.btn-primary:focus {
  box-shadow: 0 0 0 0.18rem rgba(47,125,75,0.28) !important;
}

/* Au cas où certains boutons sortent en secondary/dark -> on les “ramène” au vert */
.btn-secondary, .btn.btn-secondary,
.action-button.btn-secondary,
.btn-dark, .btn.btn-dark {
  background: rgba(47,125,75,0.18) !important;
  border: 1px solid rgba(47,125,75,0.30) !important;
  color: #e9eef6 !important;
}
.btn-secondary:hover, .btn.btn-secondary:hover,
.action-button.btn-secondary:hover,
.btn-dark:hover, .btn.btn-dark:hover {
  background: rgba(47,125,75,0.28) !important;
  border-color: rgba(216,197,138,0.25) !important;
  color: #ffffff !important;
}

/* Outline primary -> vert */
.btn-outline-primary, .btn.btn-outline-primary {
  background: transparent !important;
  border: 1px solid rgba(47,125,75,0.70) !important;
  color: #cfe0ff !important;
}
.btn-outline-primary:hover, .btn.btn-outline-primary:hover {
  background: var(--app-green-soft) !important;
  color: #ffffff !important;
}

/* =========================================================
   ✅ CHECKBOXES (Boxes / expansions) : coche vert foncé
   ========================================================= */

/* Chromium/modern : teinte native */
input[type='checkbox']{
  accent-color: var(--app-green-2);
}

/* Bootstrap form-check */
.form-check-input {
  background-color: var(--app-ink) !important;
  border: 1px solid rgba(47,125,75,0.35) !important;
}
.form-check-input:checked {
  background-color: var(--app-green-2) !important;
  border-color: var(--app-green-2) !important;
}
.form-check-input:focus {
  border-color: rgba(47,125,75,0.65) !important;
  box-shadow: 0 0 0 0.18rem rgba(47,125,75,0.22) !important;
}

/* =========================================================
   Branding / Title
   ========================================================= */
.app-brand{
  display:flex;
  align-items:center;
  gap:12px;
}
.app-brand img{
  height: 34px;
  width: auto;
  filter: drop-shadow(0 8px 18px rgba(0,0,0,.55));
}
.app-title{
  font-family: 'DisneyTitle', ui-serif, Georgia, serif;
  font-weight: 900;
  letter-spacing: .2px;
  line-height: 1;              /* ✅ évite que ça “monte” */
  display:flex;
  align-items:center;
}

/* KPIs */
.kpi { font-size: 22px; font-weight: 900; line-height: 1.05; }
.kpi-label { opacity: .75; margin-top: 6px; font-size: 12px; }
.kpi-rank { margin-top: 8px; color: var(--app-gold-2); font-size: 12.5px; font-weight: 700; }

.villain-focus-filter .form-group { margin-bottom: 12px; }
.villain-focus-filter .selectize-control { margin-bottom: 8px; }
.villain-focus-table-wrap {
  max-width: 980px;
  margin: 0 auto;
}
.villain-focus-table-wrap table.dataTable { width: 100% !important; }

/* Badges */
.badge-soft {
  display:inline-flex; align-items:center; gap:8px;
  padding: 6px 10px; border-radius: 999px;
  background: rgba(47,125,75,0.14);
  border: 1px solid rgba(216,197,138,0.30);
  margin-right: 6px; margin-bottom: 6px;
}
.trophy-1 { color: #d4af37; text-shadow: 0 0 10px rgba(212,175,55,.35); }
.trophy-2 { color: #c0c0c0; text-shadow: 0 0 10px rgba(192,192,192,.25); }
.trophy-3 { color: #cd7f32; text-shadow: 0 0 10px rgba(205,127,50,.25); }

.tab-scroll {
  max-height: calc(100vh - 78px);
  overflow-y: auto;
  overflow-x: hidden;
  padding-right: 10px;
}

/* ✅ Plots transparent */
.shiny-plot-output img { background: transparent !important; }

/* --- DT dark */
table.dataTable { background: transparent !important; border-color: #1a2f24 !important; }
table.dataTable thead th {
  background-color: var(--app-ink) !important;
  color: #e9eef6 !important;
  border-bottom: 1px solid rgba(216,197,138,0.25) !important;
}
table.dataTable tbody td { border-top: 1px solid #162033 !important; }
table.dataTable tbody tr { background-color: #0e1625 !important; }
table.dataTable tbody tr:nth-child(odd) { background-color: #0c1422 !important; }
table.dataTable tbody tr:hover { background-color: #12253a !important; }

.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select {
  background: var(--app-ink);
  color: #e9eef6;
  border: 1px solid #1a2f24;
  border-radius: 10px;
  padding: 6px 10px;
  outline: none;
}

.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate { color: #cfe0ff !important; }

/* Pagination: vert + doré */
.dataTables_wrapper .dataTables_paginate .paginate_button {
  border-radius: 10px !important;
  border: 1px solid rgba(216,197,138,.35) !important;
  background: rgba(47,125,75,.14) !important;
  color: #e9eef6 !important;
  margin: 0 2px !important;
}
.dataTables_wrapper .dataTables_paginate .paginate_button:hover {
  background: rgba(216,197,138,.18) !important;
  color: #ffffff !important;
}
.dataTables_wrapper .dataTables_paginate .paginate_button.current {
  background: rgba(216,197,138,.28) !important;
  border: 1px solid rgba(216,197,138,.70) !important;
  color: #ffffff !important;
}

/* Notifications */
.shiny-notification {
  background: var(--app-ink) !important;
  color: #e9eef6 !important;
  border: 1px solid rgba(216,197,138,.55) !important;
  box-shadow: 0 10px 24px rgba(0,0,0,.45) !important;
  border-radius: 12px !important;
}

#editGameId { font-size: 14.5px; padding: 10px; }

/* =========================================================
   ✅ Navbar trop haute + titre pas centré -> on compresse + centre tout
   ========================================================= */
.navbar, .bslib-navbar {
  padding-top: 0.10rem !important;
  padding-bottom: 0.10rem !important;
  min-height: 40px !important;
  display: flex !important;
  align-items: center !important;
}

/* Centre verticalement le contenu interne */
.navbar .container, .navbar .container-fluid,
.bslib-navbar .container, .bslib-navbar .container-fluid {
  padding-top: 0 !important;
  padding-bottom: 0 !important;
  display: flex !important;
  align-items: center !important;
}

/* Centre le “brand/title” */
.navbar-brand {
  padding-top: 0 !important;
  padding-bottom: 0 !important;
  display: flex !important;
  align-items: center !important;
  line-height: 1 !important;
}

/* Ajuste les liens d’onglets */
.navbar-nav .nav-link,
.navbar .nav-link,
.navbar .nav-item .nav-link {
  padding-top: 0.25rem !important;
  padding-bottom: 0.25rem !important;
}
.nav-pills .nav-link {
  padding-top: 0.25rem !important;
  padding-bottom: 0.25rem !important;
}
/* =======================================================================
   FINAL OVERRIDES (KEEP THIS AT THE VERY END) — force green everywhere
   ======================================================================= */

/* 1) Force ALL “normal” buttons to green (including actionButton defaults) */
body .btn:not(.btn-danger):not(.btn-warning):not(.btn-outline-danger):not(.btn-outline-warning),
body button:not(.btn-danger):not(.btn-warning):not(.btn-outline-danger):not(.btn-outline-warning).btn,
body .action-button:not(.btn-danger):not(.btn-warning),
body .btn.action-button:not(.btn-danger):not(.btn-warning),
body .btn.btn-default.action-button:not(.btn-danger):not(.btn-warning),
body .bslib-input-task-button:not(.btn-danger):not(.btn-warning),
body .bslib-btn:not(.btn-danger):not(.btn-warning) {
  background: linear-gradient(180deg, rgba(47,125,75,0.95) 0%, rgba(31,77,46,0.92) 100%) !important;
  border-color: rgba(216,197,138,0.28) !important;
  color: #ffffff !important;
  box-shadow: 0 10px 22px rgba(0,0,0,0.35) !important;
}

body .btn:not(.btn-danger):not(.btn-warning):not(.btn-outline-danger):not(.btn-outline-warning):hover,
body .action-button:not(.btn-danger):not(.btn-warning):hover,
body .bslib-input-task-button:not(.btn-danger):not(.btn-warning):hover,
body .bslib-btn:not(.btn-danger):not(.btn-warning):hover {
  background: linear-gradient(180deg, rgba(69,160,98,0.98) 0%, rgba(47,125,75,0.95) 100%) !important;
  border-color: rgba(216,197,138,0.38) !important;
}

/* Keep danger & warning readable */
body .btn-danger, body .btn.btn-danger { box-shadow: 0 10px 22px rgba(0,0,0,0.30) !important; }
body .btn-warning, body .btn.btn-warning { box-shadow: 0 10px 22px rgba(0,0,0,0.30) !important; }

/* 2) Force checkbox ticks (Boxes / expansions) to dark green */
body .shiny-input-checkboxgroup input[type='checkbox'],
body input[type='checkbox']{
  accent-color: #1f4d2e !important; /* dark green */
}

/* If bootstrap form-check is used */
body .form-check-input {
  border: 1px solid rgba(47,125,75,0.45) !important;
}
body .form-check-input:checked {
  background-color: #1f4d2e !important;
  border-color: #1f4d2e !important;
}

/* 3) Navbar: keep it compact + vertically center title (remove heading margins) */
body .navbar, body .bslib-navbar {
  min-height: 40px !important;
  padding-top: 0.10rem !important;
  padding-bottom: 0.10rem !important;
  display: flex !important;
  align-items: center !important;
}
body .navbar .container, body .navbar .container-fluid,
body .bslib-navbar .container, body .bslib-navbar .container-fluid {
  display: flex !important;
  align-items: center !important;
  padding-top: 0 !important;
  padding-bottom: 0 !important;
}
body .navbar-brand {
  display: flex !important;
  align-items: center !important;
  padding: 0 !important;
  line-height: 1 !important;
}
body .navbar-brand h1,
body .navbar-brand h2,
body .navbar-brand h3,
body .navbar-brand h4,
body .navbar-brand p,
body .navbar-brand span {
  margin: 0 !important;          /* ✅ THIS fixes the “title too high” */
  line-height: 1 !important;
}

/* 4) Nav pills active should be green (kills any remaining blue in tabs) */
body .nav-pills .nav-link.active,
body .nav-pills .show > .nav-link {
  background-color: #2f7d4b !important;
  color: #ffffff !important;
}


"


