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

/* Inputs / selects */
.form-control,
.form-select,
input[type='text'],
input[type='number'],
input[type='date'],
select {
  background: linear-gradient(180deg, rgba(21,37,28,0.96) 0%, rgba(14,24,18,0.96) 100%) !important;
  color: #eef5ec !important;
  border: 1px solid rgba(216,197,138,0.18) !important;
  box-shadow: inset 0 1px 0 rgba(255,255,255,0.03) !important;
}
.form-control::placeholder,
input::placeholder {
  color: rgba(233,238,246,0.55) !important;
}
.form-control:focus,
.form-select:focus,
input[type='text']:focus,
input[type='number']:focus,
input[type='date']:focus,
select:focus {
  background: linear-gradient(180deg, rgba(24,44,32,0.98) 0%, rgba(16,30,22,0.98) 100%) !important;
  color: #ffffff !important;
  border-color: rgba(216,197,138,0.34) !important;
  box-shadow: 0 0 0 0.18rem rgba(47,125,75,0.18), inset 0 1px 0 rgba(255,255,255,0.04) !important;
}
.selectize-control.single .selectize-input,
.selectize-control.multi .selectize-input {
  background: linear-gradient(180deg, rgba(21,37,28,0.96) 0%, rgba(14,24,18,0.96) 100%) !important;
  border: 1px solid rgba(216,197,138,0.18) !important;
  box-shadow: inset 0 1px 0 rgba(255,255,255,0.03) !important;
  color: #eef5ec !important;
}
.selectize-control.single .selectize-input.focus,
.selectize-control.multi .selectize-input.focus {
  background: linear-gradient(180deg, rgba(24,44,32,0.98) 0%, rgba(16,30,22,0.98) 100%) !important;
  border-color: rgba(216,197,138,0.34) !important;
  box-shadow: 0 0 0 0.18rem rgba(47,125,75,0.18), inset 0 1px 0 rgba(255,255,255,0.04) !important;
}
.selectize-control.single .selectize-input > input,
.selectize-control.multi .selectize-input > input,
.selectize-control.single .selectize-input .item,
.selectize-control.multi .selectize-input .item {
  color: #eef5ec !important;
}
.selectize-control.single .selectize-input > input,
.selectize-control.multi .selectize-input > input {
  background: transparent !important;
  border: 0 !important;
  box-shadow: none !important;
  padding: 0 !important;
  margin: 0 !important;
}
.selectize-control.single .selectize-input::after,
.selectize-control.multi .selectize-input::after {
  border-top-color: rgba(216,197,138,0.72) !important;
}
.selectize-control.single .selectize-input.dropdown-active::after,
.selectize-control.multi .selectize-input.dropdown-active::after {
  border-top-color: rgba(243,231,195,0.92) !important;
}
.selectize-dropdown,
.selectize-dropdown.form-control {
  background: rgba(11,20,15,0.98) !important;
  border: 1px solid rgba(216,197,138,0.18) !important;
  color: #eef5ec !important;
}
.selectize-dropdown .option,
.selectize-dropdown .active {
  color: #eef5ec !important;
}
.selectize-dropdown .option.active,
.selectize-dropdown .active {
  background: rgba(47,125,75,0.22) !important;
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

.villain-inline {
  display: inline-flex;
  align-items: center;
  gap: 10px;
  min-width: 0;
  vertical-align: middle;
}
.villain-inline-compact {
  gap: 8px;
}
.villain-inline-label {
  min-width: 0;
  display: inline-flex;
  align-items: center;
  line-height: 1.1;
}
.villain-avatar-shell {
  flex: 0 0 auto;
  overflow: hidden;
  border-radius: 50%;
  border: 1px solid rgba(216,197,138,0.38);
  box-shadow: 0 4px 12px rgba(0,0,0,0.28);
  background: rgba(255,255,255,0.04);
}
.villain-avatar {
  width: 100%;
  height: 100%;
  display: block;
  object-fit: cover;
  object-position: center;
  transform: scale(1.40);
  transform-origin: center;
}
.selectize-control.single .selectize-input {
  display: flex;
  align-items: center;
  min-height: 44px;
}
.selectize-control.single .selectize-input > .item,
.selectize-control.single .selectize-input > input {
  display: inline-flex !important;
  align-items: center;
  min-height: 26px;
}
.selectize-control.single .selectize-input > input {
  width: 1px !important;
}
.villain-selectize-entry {
  display: flex;
  align-items: center;
  min-height: 100%;
}
.villain-selectize-entry .villain-inline {
  display: flex;
  align-items: center;
  width: 100%;
}
.selectize-input .villain-selectize-entry,
.selectize-dropdown-content .villain-selectize-entry {
  min-height: 30px;
}
.selectize-input .villain-inline-label,
.selectize-dropdown-content .villain-inline-label {
  display: inline-flex;
  align-items: center;
}
.selectize-input .villain-avatar-shell,
.selectize-dropdown-content .villain-avatar-shell {
  margin-top: 0;
  margin-bottom: 0;
}
input[type='number']::-webkit-outer-spin-button,
input[type='number']::-webkit-inner-spin-button {
  opacity: 1;
  filter: invert(84%) sepia(14%) saturate(470%) hue-rotate(355deg) brightness(91%) contrast(91%);
}
input[type='number'] {
  color-scheme: dark;
}

.villain-focus-filter { min-height: 0 !important; }
.villain-focus-top-row {
  max-width: 1500px;
  margin: 0 auto 18px auto;
  display: grid;
  grid-template-columns: minmax(360px, 1.45fr) minmax(360px, 1.25fr) minmax(170px, 0.6fr) minmax(170px, 0.6fr);
  gap: 18px;
  align-items: stretch;
}
.villain-focus-top-row--villain {
  max-width: 1660px;
  grid-template-columns: minmax(340px, 1.2fr) minmax(125px, 0.4fr) minmax(125px, 0.4fr) minmax(420px, 1.45fr);
}
.villain-focus-top-card {
  width: 100%;
  margin: 0;
}
.villain-focus-filter .card-body,
.villain-focus-kpi-card .card-body {
  padding-top: 12px;
  padding-bottom: 12px;
}
.villain-focus-filter-layout {
  display: grid;
  grid-template-columns: 112px minmax(0, 1fr);
  gap: 16px;
  align-items: center;
}
.villain-focus-hero {
  display: flex;
  align-items: center;
  justify-content: center;
  min-height: 116px;
}
.villain-focus-hero-card {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 8px;
}
.villain-focus-hero-avatar-shell {
  width: 92px;
  height: 92px;
  border-radius: 50%;
  overflow: hidden;
  border: 2px solid rgba(216,197,138,0.48);
  box-shadow: 0 10px 24px rgba(0,0,0,0.34);
  background: rgba(255,255,255,0.04);
}
.villain-focus-hero-avatar {
  width: 100%;
  height: 100%;
  display: block;
  object-fit: cover;
  object-position: center;
  transform: scale(1.40);
  transform-origin: center;
}
.villain-focus-hero-name {
  font-size: 12px;
  line-height: 1.15;
  text-align: center;
  color: var(--app-gold-2);
  font-weight: 700;
}
.villain-focus-filter-row {
  display: grid;
  grid-template-columns: minmax(0, 1fr) 132px;
  gap: 12px;
  align-items: end;
}
.villain-focus-filter .form-group { margin-bottom: 0; }
.villain-focus-filter .selectize-control { margin-bottom: 0; }
.villain-focus-filter-side .shiny-input-container,
.villain-focus-filter-main .shiny-input-container {
  margin-bottom: 0;
}
.villain-focus-kpi-card { min-height: 0 !important; }
.villain-focus-kpi-card .card-header {
  padding-bottom: 10px;
}
.villain-focus-kpi-body {
  display: grid;
  grid-template-columns: 1fr;
  gap: 8px;
  align-items: center;
  min-height: 116px;
}
.villain-focus-kpi-main,
.villain-focus-kpi-side {
  min-width: 0;
}
.villain-focus-kpi-side {
  text-align: left;
}
.villain-focus-kpi-card .kpi {
  font-size: 22px;
  margin: 0;
}
.villain-focus-kpi-card .kpi-label {
  margin-top: 2px;
}
.villain-focus-kpi-substats {
  display: grid;
  gap: 4px;
  margin-top: 8px;
}
.villain-focus-kpi-substat {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: 10px;
  font-size: 11px;
}
.villain-focus-kpi-substat-label {
  color: rgba(216,197,138,0.82);
  font-size: 10px;
  letter-spacing: 0.35px;
  text-transform: uppercase;
}
.villain-focus-kpi-substat .shiny-text-output,
.villain-focus-kpi-substat span[id] {
  display: inline;
  margin: 0;
  font-weight: 700;
  color: #f4f7fb;
}
.villain-focus-kpi-card .kpi-rank {
  margin-top: 0;
  white-space: nowrap;
  font-size: 11px;
}
.villain-focus-top-row--villain .villain-focus-kpi-body {
  min-height: 108px;
  gap: 6px;
}
.villain-focus-top-row--villain .villain-focus-kpi-card .card-body {
  padding-left: 10px;
  padding-right: 10px;
}
.villain-focus-top-row--villain .villain-focus-kpi-card .kpi {
  font-size: 28px;
}
.villain-focus-top-row--villain .villain-focus-kpi-card .kpi-label,
.villain-focus-top-row--villain .villain-focus-kpi-card .kpi-rank {
  font-size: 10px;
}
.villain-focus-table-wrap {
  max-width: 980px;
  margin: 0 auto;
}
.villain-focus-table-wrap--wide {
  max-width: none;
}
.villain-focus-table-wrap table.dataTable { width: 100% !important; }
.villain-focus-players-card {
  max-width: none;
  margin: 0;
}
.villain-focus-players-wrap {
  max-width: none;
  margin: 0;
}
.villain-focus-players-wrap table.dataTable { width: 100% !important; }
.villain-focus-players-wrap .dataTables_wrapper {
  font-size: 12px;
}
.villain-focus-players-wrap table.dataTable thead th {
  font-size: 11px;
  letter-spacing: 0.4px;
  padding: 8px 10px !important;
}
.villain-focus-players-wrap table.dataTable tbody td {
  padding: 7px 10px !important;
  border-top: 1px solid rgba(24, 70, 61, 0.55) !important;
}
.villain-focus-players-wrap table.dataTable tbody tr {
  background-color: rgba(10, 16, 24, 0.82) !important;
}
.villain-focus-players-wrap table.dataTable tbody tr:nth-child(odd) {
  background-color: rgba(12, 20, 30, 0.92) !important;
}
.villain-focus-players-wrap .dataTables_length,
.villain-focus-players-wrap .dataTables_filter {
  margin-bottom: 8px;
}
.villain-focus-players-wrap .dataTables_filter input,
.villain-focus-players-wrap .dataTables_length select {
  border-radius: 8px;
  padding: 4px 8px;
  font-size: 11px;
}
.villain-focus-players-wrap .dataTables_info {
  font-size: 11px;
  padding-top: 6px !important;
}
.villain-focus-players-wrap .dataTables_wrapper .dataTables_paginate .paginate_button {
  min-width: 22px !important;
  padding: 2px 6px !important;
  font-size: 10px !important;
  line-height: 1.2 !important;
  border-radius: 6px !important;
  margin: 0 1px !important;
  background: rgba(255,255,255,.03) !important;
  border: 1px solid rgba(255,255,255,.08) !important;
  box-shadow: none !important;
  color: #d7e6ef !important;
}
.villain-focus-players-wrap .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
  background: rgba(255,255,255,.06) !important;
  border: 1px solid rgba(255,255,255,.12) !important;
  color: #ffffff !important;
}
.villain-focus-players-wrap .dataTables_wrapper .dataTables_paginate .paginate_button.current {
  background: rgba(47,125,75,.18) !important;
  border: 1px solid rgba(47,125,75,.22) !important;
  color: #f6fbff !important;
}
.villain-focus-players-wrap .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
  background: rgba(47,125,75,.22) !important;
  border: 1px solid rgba(47,125,75,.28) !important;
}
.villain-focus-players-wrap .dataTables_wrapper .dataTables_paginate .paginate_button.disabled,
.villain-focus-players-wrap .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover {
  background: rgba(255,255,255,.02) !important;
  border: 1px solid rgba(255,255,255,.05) !important;
  color: rgba(215,230,239,.45) !important;
  cursor: default !important;
}
.villain-focus-players-wrap .dataTables_wrapper .dataTables_paginate {
  padding-top: 2px !important;
}
.villain-matchup-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
  gap: 16px;
}
.villain-matchup-stack {
  display: flex;
  flex-direction: column;
  gap: 8px;
}
.villain-matchup-empty {
  padding: 18px;
  border-radius: 14px;
  background: rgba(255,255,255,0.03);
  border: 1px dashed rgba(216,197,138,0.28);
  color: #cfe0ff;
}
.villain-matchup-card {
  position: relative;
  overflow: hidden;
  padding: 10px 12px;
  border-radius: 14px;
  border: 1px solid rgba(216,197,138,0.24);
  box-shadow: inset 0 1px 0 rgba(255,255,255,0.04), 0 12px 28px rgba(0,0,0,0.22);
  width: 100%;
  display: flex;
  align-items: center;
}
.villain-matchup-card.is-rank-1 {
  width: 100%;
  min-height: 96px;
}
.villain-matchup-card.is-rank-2 {
  padding: 9px 11px;
  border-radius: 13px;
  width: 100%;
  min-height: 78px;
}
.villain-matchup-card.is-rank-3 {
  padding: 8px 10px;
  border-radius: 12px;
  opacity: 0.95;
  width: 100%;
  min-height: 64px;
}
.villain-matchup-card::before {
  content: '';
  position: absolute;
  inset: 0;
  background: radial-gradient(circle at top right, rgba(255,255,255,0.12), transparent 45%);
  pointer-events: none;
}
.villain-matchup-card.best {
  background: linear-gradient(135deg, rgba(47,125,75,0.34), rgba(8,22,14,0.92));
}
.villain-matchup-card.worst {
  background: linear-gradient(135deg, rgba(120,36,24,0.42), rgba(18,8,8,0.94));
}
.villain-matchup-rank {
  font-size: 11px;
  letter-spacing: 1.2px;
  text-transform: uppercase;
  color: var(--app-gold-2);
  opacity: 0.92;
}
.villain-matchup-layout {
  display: grid;
  grid-template-columns: minmax(150px, 1fr) 92px 72px 88px;
  gap: 4px 12px;
  align-items: center;
  width: 100%;
}
.villain-matchup-main {
  min-width: 0;
  align-self: center;
  display: flex;
  flex-direction: column;
  justify-content: center;
}
.villain-matchup-stats {
  display: grid;
  grid-template-columns: 92px 72px 88px;
  gap: 4px 12px;
  align-items: end;
  grid-column: 2 / 5;
}
.villain-matchup-stat {
  min-width: 0;
  text-align: left;
}
.villain-matchup-stat-label {
  font-size: 9px;
  letter-spacing: 0.6px;
  text-transform: uppercase;
  color: var(--app-gold-2);
  opacity: 0.88;
  white-space: nowrap;
}
.villain-matchup-stat-value {
  margin-top: 1px;
  font-size: 13px;
  font-weight: 800;
  color: #f4f7fb;
  white-space: nowrap;
}
.villain-matchup-name {
  margin-top: 2px;
  font-size: 18px;
  font-weight: 900;
  line-height: 1.1;
}
.villain-matchup-name .villain-inline {
  display: grid;
  grid-template-columns: 44px minmax(0, 1fr);
  gap: 10px;
  align-items: center;
}
.villain-matchup-name .villain-avatar-shell {
  justify-self: center;
  width: 34px;
  height: 34px;
}
.villain-matchup-name .villain-inline-label {
  min-width: 0;
  display: block;
}
.villain-matchup-card.is-rank-1 .villain-matchup-name .villain-avatar-shell {
  width: 42px;
  height: 42px;
}
.villain-matchup-card.is-rank-2 .villain-matchup-name .villain-avatar-shell {
  width: 38px;
  height: 38px;
}
.villain-matchup-card.is-rank-3 .villain-matchup-name .villain-avatar-shell {
  width: 34px;
  height: 34px;
}
.villain-matchup-card.is-rank-2 .villain-matchup-name { font-size: 16px; }
.villain-matchup-card.is-rank-3 .villain-matchup-name { font-size: 15px; }
.villain-matchup-winrate {
  margin-top: 0;
  font-size: 16px;
  font-weight: 900;
  line-height: 1;
  white-space: nowrap;
}
.villain-matchup-card.is-rank-2 .villain-matchup-winrate { font-size: 15px; }
.villain-matchup-card.is-rank-3 .villain-matchup-winrate { font-size: 14px; }
.villain-matchup-meta {
  margin-top: 0;
  color: #d9e5f8;
  opacity: 0.88;
  font-size: 11px;
  white-space: nowrap;
}
.villain-matchup-card.is-rank-2 .villain-matchup-meta { font-size: 10px; }
.villain-matchup-card.is-rank-3 .villain-matchup-meta { font-size: 9px; }
.villain-matchup-caption {
  margin-top: 0;
  color: var(--app-gold-2);
  font-size: 11px;
  font-weight: 700;
  white-space: nowrap;
}
.villain-matchup-card.is-rank-2 .villain-matchup-caption { font-size: 10px; }
.villain-matchup-card.is-rank-3 .villain-matchup-caption { font-size: 9px; }
.villain-matchup-card.is-rank-2 .villain-matchup-stat-value { font-size: 12px; }
.villain-matchup-card.is-rank-3 .villain-matchup-stat-value { font-size: 11px; }
.villain-matchup-card.is-rank-2 .villain-matchup-stat-label { font-size: 9px; }
.villain-matchup-card.is-rank-3 .villain-matchup-stat-label { font-size: 8px; }

/* Badges */
.badge-soft {
  display:inline-flex; align-items:center; gap:8px;
  padding: 6px 10px; border-radius: 999px;
  background: rgba(47,125,75,0.14);
  border: 1px solid rgba(216,197,138,0.30);
  margin-right: 6px; margin-bottom: 6px;
  vertical-align: middle;
}
.badge-soft-villain {
  gap: 7px;
  padding: 5px 8px;
  font-size: 11px;
  white-space: nowrap;
}
.badge-soft-villain .villain-inline {
  display: inline-flex;
  align-items: center;
  gap: 7px;
}
.badge-soft-villain .villain-inline-label {
  font-size: 14px;
  font-weight: 700;
}
.badge-soft-rank,
.badge-soft-score {
  display: inline-flex;
  align-items: center;
  line-height: 1;
  white-space: nowrap;
}
.badge-soft-score {
  color: var(--app-gold-2);
  font-weight: 700;
  font-size: 11px;
  letter-spacing: 0.2px;
}
.badge-soft-villain .villain-avatar-shell {
  width: 20px !important;
  height: 20px !important;
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


