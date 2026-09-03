
# =====================================================================
# app.R — DASHBOARD R SHINY DES AIRES PROTÉGÉES DU SÉNÉGAL
# =====================================================================
# OBJECTIF :
# Reproduire une application proche de la maquette générée :
# - carte Leaflet avec polygones des AP
# - couleurs par type, gap, financement, besoin et capacité
# - filtres par type, région, année, écosystème et source
# - 5 KPI
# - évolution annuelle des financements
# - coût total vs financement vs gap
# - répartition des besoins
# - fiche du site sélectionné
# - classement des AP sous-financées
# - taux de couverture par région
#
# FICHIERS À PLACER DANS /data :
# 1) AIREPRO.xlsx (ou .csv/.rds)
# 2) aires_protegees.gpkg (ou .geojson/.shp)
#
# La clé commune obligatoire est AP_ID.
#
# COUCHES OPTIONNELLES :
# regions_senegal.gpkg
# ramsar.gpkg
# kba.gpkg
# zones_non_protegees.gpkg
# =====================================================================


# =====================================================================
# ÉTAPE 1 — PACKAGES
# =====================================================================
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(janitor)
library(stringr)
library(plotly)
library(reactable)
library(scales)
library(htmltools)
library(rmapshaper)
library(RColorBrewer)


# =====================================================================
# ÉTAPE 2 — PARAMÈTRES ET COULEURS
# =====================================================================
OPTIONS <- list(
  app_title = "Dashboard Shiny – Aires protégées du Sénégal",
  app_subtitle = "Suivi du financement et des besoins",
  default_year = 2025,

  dark_teal = "#005B55",
  green = "#2E7D32",
  blue = "#2355A6",
  orange = "#F57C00",
  red = "#E84A3A",

  type_colors = c(
    "Parc national" = "#4C956C",
    "Parcs nationaux" = "#4C956C",
    "AMP" = "#4A90E2",
    "Aire marine protégée" = "#4A90E2",
    "Aires marines protégées" = "#4A90E2",
    "Réserve" = "#9B6AC1",
    "Réserve naturelle" = "#9B6AC1",
    "Forêt classée" = "#E8AE3C",
    "Forêts classées" = "#E8AE3C",
    "RNC" = "#4FB3A5",
    "ZIC" = "#D77A61",
    "Réserve sylvopastorale" = "#8A9A5B"
  )
)


# =====================================================================
# ÉTAPE 3 — FONCTIONS UTILITAIRES
# =====================================================================
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

first_non_na <- function(x, default = NA) {
  y <- x[!is.na(x)]
  if (length(y) == 0) default else y[[1]]
}

max_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

sum_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) 0 else sum(x, na.rm = TRUE)
}

numeric_clean <- function(x) {
  if (is.numeric(x)) return(x)
  readr::parse_number(
    as.character(x),
    locale = locale(decimal_mark = ",", grouping_mark = " ")
  )
}

fmt_fcfa <- function(x) {
  if (is.na(x) || !is.finite(x)) return("ND")
  if (abs(x) >= 1e9) return(paste0(round(x / 1e9, 1), " Mds FCFA"))
  if (abs(x) >= 1e6) return(paste0(round(x / 1e6, 1), " M FCFA"))
  paste0(format(round(x), big.mark = " ", scientific = FALSE), " FCFA")
}

fmt_num <- function(x, digits = 0) {
  if (is.na(x) || !is.finite(x)) return("ND")
  format(round(x, digits), big.mark = " ", scientific = FALSE)
}

money_axis_scale <- function(x) {
  m <- suppressWarnings(max(abs(x), na.rm = TRUE))
  if (!is.finite(m)) m <- 0
  if (m >= 1e9) list(divisor = 1e9, label = "Mds FCFA")
  else list(divisor = 1e6, label = "M FCFA")
}

rename_first_match <- function(df, target, candidates) {
  if (target %in% names(df)) return(df)
  hit <- intersect(candidates, names(df))
  if (length(hit) > 0) names(df)[names(df) == hit[[1]]] <- target
  df
}

canonical_source <- function(x) {
  z <- str_to_lower(str_trim(as.character(x)))

  case_when(
    str_detect(z, "etat|état|budget") ~ "Budget État",
    str_detect(z, "ptf|bailleur|afd|kfw|gef|fem|usaid|union européenne|ue|wwf|pnu|pnue") ~ "PTF/Bailleur",
    str_detect(z, "recette|billetterie|entrée|entree|écotour|ecotour|redevance") ~ "Recettes propres",
    str_detect(z, "privé|prive|entreprise|mécénat|mecenat") ~ "Secteur privé",
    TRUE ~ ifelse(is.na(x) | x == "", "Autre", as.character(x))
  )
}

color_for_type <- function(x) {
  out <- rep("#95A5A6", length(x))

  for (i in seq_along(x)) {
    xi <- as.character(x[[i]])

    if (!is.na(xi) && xi %in% names(OPTIONS$type_colors)) {
      out[[i]] <- OPTIONS$type_colors[[xi]]
    } else if (!is.na(xi)) {
      low <- str_to_lower(xi)

      out[[i]] <- case_when(
        str_detect(low, "marine|amp") ~ "#4A90E2",
        str_detect(low, "parc national") ~ "#4C956C",
        str_detect(low, "forêt|foret") ~ "#E8AE3C",
        str_detect(low, "rnc|communaut") ~ "#4FB3A5",
        str_detect(low, "réserve|reserve") ~ "#9B6AC1",
        str_detect(low, "zic|cynég") ~ "#D77A61",
        TRUE ~ "#95A5A6"
      )
    }
  }
  out
}


# =====================================================================
# ÉTAPE 4 — IMPORT DE AIREPRO
# =====================================================================
# Le script accepte :
# data/AIREPRO.xlsx
# data/AIREPRO.csv
# data/AIREPRO.rds
# ou un objet R déjà chargé nommé AIREPRO.
# =====================================================================
load_airepro <- function() {

  if (exists("AIREPRO", envir = .GlobalEnv)) {
    message("Objet AIREPRO trouvé dans l'environnement R.")
    return(get("AIREPRO", envir = .GlobalEnv))
  }

  candidates <- c(
    "data/AIREPRO.xlsx",
    "data/AIREPRO.csv",
    "data/AIREPRO.rds"
  )

  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 0) {
    stop(
      "AIREPRO introuvable. Placez AIREPRO.xlsx, .csv ou .rds dans le dossier data/."
    )
  }

  path <- existing[[1]]
  ext <- tools::file_ext(path)

  if (ext == "xlsx") {
    read_excel(path, sheet = 1)
  } else if (ext == "csv") {
    read_csv(path, show_col_types = FALSE)
  } else if (ext == "rds") {
    readRDS(path)
  } else {
    stop("Format de AIREPRO non pris en charge.")
  }
}


# =====================================================================
# ÉTAPE 5 — HARMONISATION DE AIREPRO
# =====================================================================
# COLONNES MINIMALES :
# AP_ID, Nom_AP, Type_AP, Region, Superficie_ha, Annee,
# Source_financement ou Categorie_source, Financement_FCFA,
# Besoin_total_FCFA
#
# COLONNES RECOMMANDÉES :
# Ecosysteme, Gestionnaire, Ramsar, UNESCO, PAG_a_jour,
# Pression_ecologique, Score_capacite,
# Besoin_personnel_FCFA, Besoin_fonctionnement_FCFA,
# Besoin_investissement_FCFA, Besoin_conservation_FCFA,
# Besoin_gouvernance_FCFA
# =====================================================================
prepare_airepro <- function(df) {

  df <- clean_names(df)

  synonym_map <- list(
    ap_id = c("id_ap", "code_ap", "identifiant_ap", "wdpa_id"),
    nom_ap = c("nom", "nom_aire_protegee", "aire_protegee", "name"),
    type_ap = c("type", "categorie_ap", "statut_ap", "designation"),
    region = c("region_administrative", "regions"),
    ecosysteme = c("ecosysteme_principal", "ecosystem", "milieu"),
    superficie_ha = c("superficie", "surface_ha", "area_ha", "superficie_totale_ha"),
    gestionnaire = c("gestionnaire_principal", "manager", "structure_gestionnaire"),
    ramsar = c("ramsar_o_n", "site_ramsar"),
    unesco = c("unesco_o_n", "biosphere_unesco_o_n", "patrimoine_unesco_o_n"),
    pag_a_jour = c("pag_a_jour_o_n", "pag", "plan_gestion_a_jour"),
    pression_ecologique = c("pression", "niveau_pression", "pression_environnementale"),
    score_capacite = c("score_capacite_0_100", "capacite_gestion", "score_gestion"),
    annee = c("year", "annee_financement", "exercice"),
    categorie_source = c("source_financement", "categorie_financement", "source", "type_source"),
    financement_fcfa = c("montant_execute_fcfa", "financement_total_fcfa", "financement", "montant_financement_fcfa"),
    besoin_total_fcfa = c("total_besoins_fcfa", "besoin_total", "cout_total_fcfa", "cout_financier_fcfa"),
    besoin_personnel_fcfa = c("personnel_fcfa", "cout_personnel_fcfa"),
    besoin_fonctionnement_fcfa = c("fonctionnement_fcfa", "cout_fonctionnement_fcfa"),
    besoin_investissement_fcfa = c("investissement_fcfa", "cout_investissement_fcfa"),
    besoin_conservation_fcfa = c("programmes_conservation_fcfa", "conservation_fcfa"),
    besoin_gouvernance_fcfa = c("gouvernance_admin_fcfa", "gouvernance_fcfa"),
    date_maj = c("date_mise_a_jour", "maj")
  )

  for (target in names(synonym_map)) {
    df <- rename_first_match(df, target, synonym_map[[target]])
  }

  # Si les sources financières sont en colonnes séparées,
  # convertir automatiquement en format long.
  wide_sources <- intersect(
    c(
      "financement_etat_fcfa",
      "financement_ptf_fcfa",
      "recettes_propres_fcfa",
      "financement_prive_fcfa",
      "financement_autres_fcfa"
    ),
    names(df)
  )

  if (!("categorie_source" %in% names(df)) && length(wide_sources) > 0) {

    df <- df |>
      pivot_longer(
        cols = all_of(wide_sources),
        names_to = "source_variable",
        values_to = "financement_fcfa"
      ) |>
      mutate(
        categorie_source = case_when(
          source_variable == "financement_etat_fcfa" ~ "Budget État",
          source_variable == "financement_ptf_fcfa" ~ "PTF/Bailleur",
          source_variable == "recettes_propres_fcfa" ~ "Recettes propres",
          source_variable == "financement_prive_fcfa" ~ "Secteur privé",
          TRUE ~ "Autre"
        )
      )
  }

  if (!("categorie_source" %in% names(df))) df$categorie_source <- "Total"

  if (!("financement_fcfa" %in% names(df))) {
    stop(
      "Aucune variable de financement reconnue dans AIREPRO. ",
      "Ajoutez Financement_FCFA ou adaptez synonym_map."
    )
  }

  needs_cols <- intersect(
    c(
      "besoin_personnel_fcfa",
      "besoin_fonctionnement_fcfa",
      "besoin_investissement_fcfa",
      "besoin_conservation_fcfa",
      "besoin_gouvernance_fcfa"
    ),
    names(df)
  )

  if (!("besoin_total_fcfa" %in% names(df)) && length(needs_cols) > 0) {
    df$besoin_total_fcfa <- rowSums(df[needs_cols], na.rm = TRUE)
  }

  required <- c(
    "ap_id","nom_ap","type_ap","region",
    "superficie_ha","annee","financement_fcfa","besoin_total_fcfa"
  )

  missing <- setdiff(required, names(df))

  if (length(missing) > 0) {
    stop(
      "Colonnes indispensables manquantes : ",
      paste(missing, collapse = ", "),
      ". Renommez les colonnes ou adaptez synonym_map."
    )
  }

  defaults <- list(
    ecosysteme = "Non renseigné",
    gestionnaire = "Non renseigné",
    ramsar = "ND",
    unesco = "ND",
    pag_a_jour = "ND",
    pression_ecologique = "ND",
    score_capacite = NA_real_,
    besoin_personnel_fcfa = NA_real_,
    besoin_fonctionnement_fcfa = NA_real_,
    besoin_investissement_fcfa = NA_real_,
    besoin_conservation_fcfa = NA_real_,
    besoin_gouvernance_fcfa = NA_real_,
    date_maj = NA_character_
  )

  for (nm in names(defaults)) {
    if (!(nm %in% names(df))) df[[nm]] <- defaults[[nm]]
  }

  numeric_cols <- intersect(
    c(
      "superficie_ha","financement_fcfa","besoin_total_fcfa",
      "score_capacite","besoin_personnel_fcfa","besoin_fonctionnement_fcfa",
      "besoin_investissement_fcfa","besoin_conservation_fcfa",
      "besoin_gouvernance_fcfa"
    ),
    names(df)
  )

  for (nm in numeric_cols) df[[nm]] <- numeric_clean(df[[nm]])

  df |>
    mutate(
      ap_id = as.character(ap_id),
      nom_ap = as.character(nom_ap),
      type_ap = as.character(type_ap),
      region = as.character(region),
      ecosysteme = as.character(ecosysteme),
      annee = as.integer(annee),
      categorie_source = canonical_source(categorie_source)
    )
}

AIREPRO_DATA <- load_airepro() |> prepare_airepro()


# =====================================================================
# ÉTAPE 6 — IMPORT DES POLYGONES
# =====================================================================
# >>> ICI VOUS DEVEZ AJOUTER VOTRE FICHIER POLYGONAL <<<
#
# Recommandé : data/aires_protegees.gpkg
# Alternatives :
# data/aires_protegees.geojson
# data/aires_protegees.shp (+ .dbf/.shx/.prj...)
#
# La couche DOIT contenir AP_ID ou un synonyme défini ci-dessous.
# =====================================================================
read_spatial_first <- function(paths, require_ap_id = FALSE) {

  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) return(NULL)

  x <- st_read(existing[[1]], quiet = TRUE) |> clean_names()

  if (require_ap_id) {
    x <- rename_first_match(
      x,
      "ap_id",
      c("id_ap", "code_ap", "identifiant_ap", "wdpa_id", "site_id")
    )

    if (!("ap_id" %in% names(x))) {
      stop("La couche des AP ne contient pas AP_ID.")
    }

    x$ap_id <- as.character(x$ap_id)
  }

  x <- x |> st_make_valid() |> st_transform(4326)

  tryCatch(
    ms_simplify(x, keep = 0.15, keep_shapes = TRUE),
    error = function(e) x
  )
}

AP_SF <- read_spatial_first(
  c(
    "data/aires_protegees.gpkg",
    "data/aires_protegees.geojson",
    "data/aires_protegees.shp"
  ),
  require_ap_id = TRUE
)

if (is.null(AP_SF)) {
  stop(
    "Polygones des aires protégées introuvables. ",
    "Ajoutez data/aires_protegees.gpkg, .geojson ou .shp."
  )
}

AP_GEOM <- AP_SF |> select(ap_id, geometry)


# =====================================================================
# ÉTAPE 7 — COUCHES OPTIONNELLES
# =====================================================================
# Ajoutez ces fichiers seulement si vous voulez les superposer à la carte.
# =====================================================================
REGIONS_SF <- read_spatial_first(
  c("data/regions_senegal.gpkg","data/regions_senegal.geojson")
)

RAMSAR_SF <- read_spatial_first(
  c("data/ramsar.gpkg","data/ramsar.geojson")
)

KBA_SF <- read_spatial_first(
  c("data/kba.gpkg","data/kba.geojson")
)

NON_PROTECTED_SF <- read_spatial_first(
  c("data/zones_non_protegees.gpkg","data/zones_non_protegees.geojson")
)


# =====================================================================
# ÉTAPE 8 — FILTRES DISPONIBLES
# =====================================================================
YEARS_AVAILABLE <- sort(unique(na.omit(AIREPRO_DATA$annee)))

DEFAULT_YEAR <- if (OPTIONS$default_year %in% YEARS_AVAILABLE) {
  OPTIONS$default_year
} else {
  max(YEARS_AVAILABLE, na.rm = TRUE)
}

TYPE_CHOICES <- sort(unique(na.omit(AIREPRO_DATA$type_ap)))
REGION_CHOICES <- sort(unique(na.omit(AIREPRO_DATA$region)))
ECOSYSTEM_CHOICES <- sort(unique(na.omit(AIREPRO_DATA$ecosysteme)))
SOURCE_CHOICES <- sort(unique(na.omit(AIREPRO_DATA$categorie_source)))


# =====================================================================
# ÉTAPE 9 — INTERFACE UTILISATEUR
# =====================================================================
logo_ui <- if (file.exists("www/logo.png")) {
  tags$img(src = "logo.png", class = "brand-logo")
} else {
  div(
    class = "brand-fallback",
    div(class = "brand-mark", "AP"),
    div(
      class = "brand-title",
      "AIRES PROTÉGÉES", tags$br(), "DU SÉNÉGAL"
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$title(OPTIONS$app_title)
  ),

  div(
    class = "app-shell",

    # ------------------------ SIDEBAR ------------------------
    aside(
      class = "left-sidebar",

      div(class = "brand-area", logo_ui),
      div(class = "sidebar-divider"),

      div(class = "sidebar-section-title", "FILTRES"),

      div(
        class = "filter-block",
        selectInput(
          "type_ap",
          "Type d’aire protégée",
          choices = c("Toutes" = "__ALL__", TYPE_CHOICES),
          selected = "__ALL__"
        )
      ),

      div(
        class = "filter-block",
        selectInput(
          "region",
          "Région",
          choices = c("Toutes" = "__ALL__", REGION_CHOICES),
          selected = "__ALL__"
        )
      ),

      div(
        class = "filter-block",
        selectInput(
          "annee",
          "Année",
          choices = YEARS_AVAILABLE,
          selected = DEFAULT_YEAR
        )
      ),

      div(
        class = "filter-block",
        selectInput(
          "ecosysteme",
          "Écosystème",
          choices = c("Tous" = "__ALL__", ECOSYSTEM_CHOICES),
          selected = "__ALL__"
        )
      ),

      div(
        class = "filter-block",
        selectInput(
          "source_financement",
          "Source de financement",
          choices = c("Toutes" = "__ALL__", SOURCE_CHOICES),
          selected = "__ALL__"
        )
      ),

      div(class = "sidebar-section-title small-title", "AFFICHER"),

      div(
        class = "map-mode",
        radioButtons(
          "map_indicator",
          NULL,
          choices = c(
            "Type d’aire protégée" = "type",
            "Gap (%)" = "gap_pct",
            "Gap financier" = "gap_fcfa",
            "Financement" = "financement",
            "Besoin" = "besoin",
            "Capacité" = "capacite"
          ),
          selected = "gap_pct"
        )
      ),

      actionButton(
        "reset_filters",
        "Réinitialiser les filtres",
        class = "reset-btn"
      ),

      div(
        class = "sidebar-footer",
        "Plateforme de suivi du financement",
        tags$br(),
        "des aires protégées du Sénégal"
      )
    ),

    # ------------------------ MAIN ------------------------
    main(
      class = "main-content",

      div(
        class = "top-header",
        div(
          div(class = "main-title", OPTIONS$app_title),
          div(class = "main-subtitle", OPTIONS$app_subtitle)
        ),
        div(
          class = "header-actions",
          downloadButton("export_data", "Exporter", class = "header-btn")
        )
      ),

      # KPI
      div(
        class = "kpi-grid",
        uiOutput("kpi_nb_ap"),
        uiOutput("kpi_surface"),
        uiOutput("kpi_financement"),
        uiOutput("kpi_gap"),
        uiOutput("kpi_couverture")
      ),

      # Grille principale
      div(
        class = "visual-grid",

        div(
          class = "card map-card",
          div(class = "card-title", "Carte des aires protégées du Sénégal"),
          leafletOutput("map", height = "100%")
        ),

        div(
          class = "card evolution-card",
          div(class = "card-title", "Évolution des financements annuels"),
          plotlyOutput("financing_evolution", height = "330px")
        ),

        div(
          class = "card cost-card",
          div(class = "card-title", "Coût financier vs financement"),
          plotlyOutput("cost_vs_finance", height = "270px")
        ),

        div(
          class = "card needs-card",
          div(class = "card-title", "Répartition des besoins"),
          plotlyOutput("needs_breakdown", height = "270px")
        ),

        div(
          class = "card other-card",
          div(class = "card-title", "Autres variables"),
          uiOutput("other_variables")
        ),

        div(
          class = "card ranking-card",
          div(class = "card-title", "AP les plus sous-financées (selon le Gap %)"),
          reactableOutput("ranking_table")
        ),

        div(
          class = "card region-card",
          div(class = "card-title", "Taux de couverture par région"),
          plotlyOutput("regional_coverage", height = "290px")
        )
      ),

      div(class = "app-footer", uiOutput("last_update"))
    )
  )
)


# =====================================================================
# ÉTAPE 10 — LOGIQUE SERVEUR
# =====================================================================
server <- function(input, output, session) {

  selected_ap <- reactiveVal(NULL)

  # 10.1 Réinitialiser
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "type_ap", selected = "__ALL__")
    updateSelectInput(session, "region", selected = "__ALL__")
    updateSelectInput(session, "annee", selected = DEFAULT_YEAR)
    updateSelectInput(session, "ecosysteme", selected = "__ALL__")
    updateSelectInput(session, "source_financement", selected = "__ALL__")
    updateRadioButtons(session, "map_indicator", selected = "gap_pct")
  })

  # 10.2 Filtres structurels
  profile_filtered <- reactive({
    d <- AIREPRO_DATA

    if (!is.null(input$type_ap) && input$type_ap != "__ALL__") {
      d <- d |> filter(type_ap == input$type_ap)
    }

    if (!is.null(input$region) && input$region != "__ALL__") {
      d <- d |> filter(region == input$region)
    }

    if (!is.null(input$ecosysteme) && input$ecosysteme != "__ALL__") {
      d <- d |> filter(ecosysteme == input$ecosysteme)
    }

    d
  })

  # 10.3 Année + source financière
  current_year_data <- reactive({
    req(input$annee)

    d <- profile_filtered() |>
      filter(annee == as.integer(input$annee))

    if (!is.null(input$source_financement) &&
        input$source_financement != "__ALL__") {
      d <- d |> filter(categorie_source == input$source_financement)
    }

    d
  })

  # 10.4 Agrégation par site
  ap_summary <- reactive({

    d <- current_year_data()

    validate(
      need(nrow(d) > 0, "Aucune donnée pour les filtres sélectionnés.")
    )

    d |>
      group_by(ap_id) |>
      summarise(
        nom_ap = first_non_na(nom_ap, "Sans nom"),
        type_ap = first_non_na(type_ap, "Autre"),
        region = first_non_na(region, "ND"),
        ecosysteme = first_non_na(ecosysteme, "ND"),
        superficie_ha = max_na(superficie_ha),
        gestionnaire = first_non_na(gestionnaire, "ND"),
        ramsar = first_non_na(ramsar, "ND"),
        unesco = first_non_na(unesco, "ND"),
        pag_a_jour = first_non_na(pag_a_jour, "ND"),
        pression_ecologique = first_non_na(pression_ecologique, "ND"),
        score_capacite = max_na(score_capacite),

        financement_total = sum_na(financement_fcfa),
        besoin_total = max_na(besoin_total_fcfa),

        besoin_personnel = max_na(besoin_personnel_fcfa),
        besoin_fonctionnement = max_na(besoin_fonctionnement_fcfa),
        besoin_investissement = max_na(besoin_investissement_fcfa),
        besoin_conservation = max_na(besoin_conservation_fcfa),
        besoin_gouvernance = max_na(besoin_gouvernance_fcfa),

        recettes_propres = sum_na(
          ifelse(categorie_source == "Recettes propres", financement_fcfa, 0)
        ),

        .groups = "drop"
      ) |>
      mutate(
        gap_fcfa = pmax(besoin_total - financement_total, 0),

        gap_pct = ifelse(
          !is.na(besoin_total) & besoin_total > 0,
          100 * gap_fcfa / besoin_total,
          NA_real_
        ),

        taux_couverture = ifelse(
          !is.na(besoin_total) & besoin_total > 0,
          100 * financement_total / besoin_total,
          NA_real_
        ),

        financement_ha = ifelse(
          !is.na(superficie_ha) & superficie_ha > 0,
          financement_total / superficie_ha,
          NA_real_
        )
      )
  })

  map_data <- reactive({
    AP_GEOM |> inner_join(ap_summary(), by = "ap_id")
  })

  # 10.5 Clic sur la carte
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id)) selected_ap(as.character(click$id))
  })

  observeEvent(ap_summary(), {
    ids <- ap_summary()$ap_id

    if (length(ids) > 0 &&
        (is.null(selected_ap()) || !(selected_ap() %in% ids))) {
      selected_ap(ids[[1]])
    }
  })

  selected_summary <- reactive({
    req(selected_ap())

    ap_summary() |>
      filter(ap_id == selected_ap()) |>
      slice(1)
  })

  # ===================================================================
  # ÉTAPE 11 — KPI
  # ===================================================================
  kpi_box <- function(icon_text, label, value, subtitle, accent) {
    div(
      class = "kpi-card",
      div(
        class = "kpi-icon",
        style = paste0("background:", accent, ";"),
        icon_text
      ),
      div(
        class = "kpi-body",
        div(class = "kpi-label", label),
        div(class = "kpi-value", style = paste0("color:", accent, ";"), value),
        div(class = "kpi-subtitle", subtitle)
      )
    )
  }

  output$kpi_nb_ap <- renderUI({
    x <- ap_summary()
    kpi_box(
      "AP",
      "Nombre d’AP",
      format(n_distinct(x$ap_id), big.mark = " "),
      "aires protégées",
      "#2E7D32"
    )
  })

  output$kpi_surface <- renderUI({
    x <- ap_summary()
    surf <- sum(x$superficie_ha, na.rm = TRUE)

    kpi_box(
      "ha",
      "Superficie couverte",
      paste0(round(surf / 1e6, 2), " M ha"),
      paste0(fmt_num(surf), " ha"),
      "#6AAE35"
    )
  })

  output$kpi_financement <- renderUI({
    x <- ap_summary()
    total <- sum(x$financement_total, na.rm = TRUE)

    kpi_box(
      "F",
      paste0("Financement total (", input$annee, ")"),
      fmt_fcfa(total),
      "toutes sources filtrées",
      "#2355A6"
    )
  })

  output$kpi_gap <- renderUI({
    x <- ap_summary()
    total <- sum(x$gap_fcfa, na.rm = TRUE)

    kpi_box(
      "G",
      paste0("Gap total (", input$annee, ")"),
      fmt_fcfa(total),
      "besoins non couverts",
      "#F57C00"
    )
  })

  output$kpi_couverture <- renderUI({
    x <- ap_summary()
    fin <- sum(x$financement_total, na.rm = TRUE)
    besoin <- sum(x$besoin_total, na.rm = TRUE)
    taux <- ifelse(besoin > 0, 100 * fin / besoin, NA_real_)

    kpi_box(
      "%",
      "Taux de couverture",
      ifelse(is.na(taux), "ND", paste0(round(taux), "%")),
      "financement / besoin",
      "#00897B"
    )
  })


  # ===================================================================
  # ÉTAPE 12 — CARTE LEAFLET
  # ===================================================================
  output$map <- renderLeaflet({

    d <- map_data()
    req(nrow(d) > 0)

    indicator <- input$map_indicator %||% "gap_pct"

    if (indicator == "type") {

      fill_cols <- color_for_type(d$type_ap)
      legend_labels <- sort(unique(d$type_ap))
      legend_colors <- color_for_type(legend_labels)
      legend_title <- "Type d’aire protégée"
      legend_mode <- "categorical"

    } else {

      variable <- switch(
        indicator,
        gap_pct = d$gap_pct,
        gap_fcfa = d$gap_fcfa,
        financement = d$financement_total,
        besoin = d$besoin_total,
        capacite = d$score_capacite
      )

      palette_name <- switch(
        indicator,
        gap_pct = "YlOrRd",
        gap_fcfa = "YlOrRd",
        financement = "Blues",
        besoin = "PuRd",
        capacite = "YlGn"
      )

      pal <- colorNumeric(
        palette = palette_name,
        domain = variable,
        na.color = "#D9D9D9"
      )

      fill_cols <- pal(variable)

      legend_title <- switch(
        indicator,
        gap_pct = "Gap (%)",
        gap_fcfa = "Gap (FCFA)",
        financement = "Financement (FCFA)",
        besoin = "Besoin (FCFA)",
        capacite = "Score capacité"
      )

      legend_mode <- "numeric"
    }

    popup_html <- paste0(
      "<div class='map-popup'>",
      "<div class='popup-title'>", htmlEscape(d$nom_ap), "</div>",
      "<b>Type :</b> ", htmlEscape(d$type_ap), "<br>",
      "<b>Superficie :</b> ",
      vapply(d$superficie_ha, fmt_num, character(1)), " ha<br>",
      "<b>Financement ", input$annee, " :</b> ",
      vapply(d$financement_total, fmt_fcfa, character(1)), "<br>",
      "<b>Besoin :</b> ",
      vapply(d$besoin_total, fmt_fcfa, character(1)), "<br>",
      "<b>Gap :</b> <span class='popup-gap'>",
      vapply(d$gap_fcfa, fmt_fcfa, character(1)), "</span><br>",
      "<b>Gap % :</b> ",
      ifelse(is.na(d$gap_pct), "ND", paste0(round(d$gap_pct, 1), "%")), "<br>",
      "<b>Score capacité :</b> ",
      ifelse(
        is.na(d$score_capacite),
        "ND",
        paste0(round(d$score_capacite), "/100")
      ),
      "</div>"
    )

    map <- leaflet(d, options = leafletOptions(minZoom = 5)) |>
      addProviderTiles(
        providers$CartoDB.Positron,
        group = "Fond clair"
      ) |>
      addPolygons(
        layerId = ~ap_id,
        group = "Aires protégées",
        color = "#364747",
        weight = 1.1,
        opacity = 0.9,
        fillColor = fill_cols,
        fillOpacity = 0.78,
        popup = popup_html,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#FFD400",
          fillOpacity = 0.93,
          bringToFront = TRUE
        )
      )

    # Couches optionnelles
    if (!is.null(RAMSAR_SF)) {
      map <- map |>
        addPolygons(
          data = RAMSAR_SF,
          group = "Sites Ramsar",
          fill = FALSE,
          color = "#00A6A6",
          weight = 2,
          dashArray = "5,5"
        )
    }

    if (!is.null(KBA_SF)) {
      map <- map |>
        addPolygons(
          data = KBA_SF,
          group = "KBA",
          fill = FALSE,
          color = "#8E5EA2",
          weight = 1.5,
          dashArray = "7,4"
        )
    }

    if (!is.null(NON_PROTECTED_SF)) {
      map <- map |>
        addPolygons(
          data = NON_PROTECTED_SF,
          group = "Zones non protégées prioritaires",
          fillColor = "#F4A261",
          fillOpacity = 0.25,
          color = "#E76F51",
          weight = 1.5
        )
    }

    if (legend_mode == "categorical") {
      map <- map |>
        addLegend(
          position = "topright",
          colors = legend_colors,
          labels = legend_labels,
          title = legend_title,
          opacity = 0.9
        )
    } else {
      map <- map |>
        addLegend(
          position = "topright",
          pal = pal,
          values = variable,
          title = legend_title,
          opacity = 0.9
        )
    }

    overlay_groups <- c(
      "Aires protégées",
      if (!is.null(RAMSAR_SF)) "Sites Ramsar",
      if (!is.null(KBA_SF)) "KBA",
      if (!is.null(NON_PROTECTED_SF)) "Zones non protégées prioritaires"
    )

    map |>
      addLayersControl(
        baseGroups = "Fond clair",
        overlayGroups = overlay_groups,
        options = layersControlOptions(collapsed = TRUE)
      )
  })


  # ===================================================================
  # ÉTAPE 13 — ÉVOLUTION ANNUELLE DU FINANCEMENT
  # ===================================================================
  financing_history <- reactive({

    req(selected_ap())

    profile_filtered() |>
      filter(ap_id == selected_ap()) |>
      group_by(annee, categorie_source) |>
      summarise(montant = sum_na(financement_fcfa), .groups = "drop") |>
      mutate(
        serie = case_when(
          categorie_source == "Budget État" ~ "Budget État",
          categorie_source == "PTF/Bailleur" ~ "Bailleurs",
          categorie_source == "Recettes propres" ~ "Recettes propres",
          TRUE ~ "Autres"
        )
      ) |>
      group_by(annee, serie) |>
      summarise(montant = sum(montant, na.rm = TRUE), .groups = "drop")
  })

  output$financing_evolution <- renderPlotly({

    d <- financing_history()
    req(nrow(d) > 0)

    total <- d |>
      group_by(annee) |>
      summarise(montant = sum(montant, na.rm = TRUE), .groups = "drop") |>
      mutate(serie = "Total")

    plot_df <- bind_rows(d, total)

    scale_info <- money_axis_scale(plot_df$montant)

    plot_df <- plot_df |>
      mutate(valeur = montant / scale_info$divisor)

    color_map <- c(
      "Total" = "#1F6D42",
      "Budget État" = "#2D8CFF",
      "Bailleurs" = "#7E57C2",
      "Recettes propres" = "#F57C00",
      "Autres" = "#7F8C8D"
    )

    plot_ly(
      plot_df,
      x = ~annee,
      y = ~valeur,
      color = ~serie,
      colors = color_map,
      type = "scatter",
      mode = "lines+markers"
    ) |>
      layout(
        margin = list(l = 55, r = 20, t = 10, b = 55),
        xaxis = list(title = "", dtick = 1),
        yaxis = list(title = scale_info$label, rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = -0.18),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) |>
      config(displayModeBar = FALSE)
  })


  # ===================================================================
  # ÉTAPE 14 — COÛT TOTAL / FINANCEMENT / GAP
  # ===================================================================
  output$cost_vs_finance <- renderPlotly({

    s <- selected_summary()

    vals <- tibble(
      categorie = factor(
        c("Coût total", "Financement reçu", "Gap"),
        levels = c("Coût total", "Financement reçu", "Gap")
      ),
      montant = c(
        s$besoin_total,
        s$financement_total,
        s$gap_fcfa
      ),
      couleur = c("#2355A6", "#2E7D32", "#E84A3A")
    )

    scale_info <- money_axis_scale(vals$montant)
    vals <- vals |> mutate(valeur = montant / scale_info$divisor)

    plot_ly(
      vals,
      x = ~categorie,
      y = ~valeur,
      type = "bar",
      marker = list(color = vals$couleur),
      text = ~round(valeur, 1),
      textposition = "outside"
    ) |>
      layout(
        margin = list(l = 55, r = 15, t = 10, b = 55),
        xaxis = list(title = ""),
        yaxis = list(title = scale_info$label, rangemode = "tozero"),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) |>
      config(displayModeBar = FALSE)
  })


  # ===================================================================
  # ÉTAPE 15 — RÉPARTITION DES BESOINS
  # ===================================================================
  output$needs_breakdown <- renderPlotly({

    s <- selected_summary()

    d <- tibble(
      poste = factor(
        c(
          "Personnel",
          "Fonctionnement",
          "Investissement",
          "Conservation",
          "Gouvernance"
        ),
        levels = c(
          "Personnel",
          "Fonctionnement",
          "Investissement",
          "Conservation",
          "Gouvernance"
        )
      ),
      montant = c(
        s$besoin_personnel,
        s$besoin_fonctionnement,
        s$besoin_investissement,
        s$besoin_conservation,
        s$besoin_gouvernance
      ),
      couleur = c("#2E7D32","#2878C8","#7E57C2","#F57C00","#009688")
    ) |>
      filter(!is.na(montant))

    validate(
      need(nrow(d) > 0, "Composantes des besoins non renseignées.")
    )

    scale_info <- money_axis_scale(d$montant)

    d <- d |> mutate(valeur = montant / scale_info$divisor)

    plot_ly(
      d,
      x = ~poste,
      y = ~valeur,
      type = "bar",
      marker = list(color = d$couleur),
      text = ~round(valeur, 1),
      textposition = "outside"
    ) |>
      layout(
        margin = list(l = 55, r = 15, t = 10, b = 65),
        xaxis = list(title = "", tickangle = -20),
        yaxis = list(title = scale_info$label, rangemode = "tozero"),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) |>
      config(displayModeBar = FALSE)
  })


  # ===================================================================
  # ÉTAPE 16 — AUTRES VARIABLES DU SITE
  # ===================================================================
  output$other_variables <- renderUI({

    s <- selected_summary()

    items <- list(
      c("Ramsar", as.character(s$ramsar)),
      c("UNESCO", as.character(s$unesco)),
      c("Gestionnaire", as.character(s$gestionnaire)),
      c("PAG à jour", as.character(s$pag_a_jour)),
      c("Pression écologique", as.character(s$pression_ecologique)),
      c("Recettes propres", fmt_fcfa(s$recettes_propres)),
      c(
        "Financement / ha",
        ifelse(
          is.na(s$financement_ha),
          "ND",
          paste0(fmt_num(s$financement_ha), " FCFA/ha")
        )
      ),
      c(
        "Capacité de gestion",
        ifelse(
          is.na(s$score_capacite),
          "ND",
          paste0(round(s$score_capacite), "/100")
        )
      )
    )

    tagList(
      div(class = "selected-site-name", s$nom_ap),

      lapply(
        items,
        function(it) {
          div(
            class = "variable-row",
            span(class = "variable-label", it[[1]]),
            span(class = "variable-value", it[[2]])
          )
        }
      )
    )
  })


  # ===================================================================
  # ÉTAPE 17 — CLASSEMENT DES AP SOUS-FINANCÉES
  # ===================================================================
  output$ranking_table <- renderReactable({

    d <- ap_summary() |>
      filter(!is.na(gap_pct)) |>
      arrange(desc(gap_pct)) |>
      transmute(
        Rang = row_number(),
        `Aire protégée` = nom_ap,
        `Gap %` = round(gap_pct, 1),
        `Gap (M FCFA)` = round(gap_fcfa / 1e6, 1)
      ) |>
      slice_head(n = 10)

    reactable(
      d,
      compact = TRUE,
      highlight = TRUE,
      bordered = FALSE,
      pagination = FALSE,

      columns = list(
        Rang = colDef(width = 55, align = "center"),

        `Aire protégée` = colDef(minWidth = 220),

        `Gap %` = colDef(
          width = 145,
          cell = function(value) {

            pct <- max(0, min(100, value))

            color <- if (pct >= 75) {
              "#E84A3A"
            } else if (pct >= 50) {
              "#F57C00"
            } else {
              "#E9B949"
            }

            div(
              style = list(
                position = "relative",
                background = "#F1F1F1",
                borderRadius = "4px",
                overflow = "hidden",
                height = "18px"
              ),

              div(
                style = list(
                  width = paste0(pct, "%"),
                  background = color,
                  height = "100%"
                )
              ),

              div(
                style = list(
                  position = "absolute",
                  left = "0",
                  top = "0",
                  width = "100%",
                  textAlign = "center",
                  fontSize = "11px",
                  lineHeight = "18px",
                  fontWeight = 700
                ),
                paste0(value, "%")
              )
            )
          }
        ),

        `Gap (M FCFA)` = colDef(width = 110, align = "right")
      )
    )
  })


  # ===================================================================
  # ÉTAPE 18 — TAUX DE COUVERTURE PAR RÉGION
  # ===================================================================
  output$regional_coverage <- renderPlotly({

    d <- ap_summary() |>
      group_by(region) |>
      summarise(
        financement = sum(financement_total, na.rm = TRUE),
        besoin = sum(besoin_total, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        taux = ifelse(
          besoin > 0,
          100 * financement / besoin,
          NA_real_
        )
      ) |>
      filter(!is.na(taux)) |>
      arrange(taux) |>
      mutate(region = factor(region, levels = region))

    plot_ly(
      d,
      x = ~taux,
      y = ~region,
      type = "bar",
      orientation = "h",
      marker = list(color = "#23A58B"),
      text = ~paste0(round(taux), "%"),
      textposition = "outside"
    ) |>
      layout(
        margin = list(l = 95, r = 35, t = 10, b = 45),
        xaxis = list(
          title = "%",
          range = c(0, max(100, max(d$taux, na.rm = TRUE) * 1.12))
        ),
        yaxis = list(title = ""),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) |>
      config(displayModeBar = FALSE)
  })


  # ===================================================================
  # ÉTAPE 19 — DATE DE MISE À JOUR
  # ===================================================================
  output$last_update <- renderUI({

    dates <- suppressWarnings(as.Date(AIREPRO_DATA$date_maj))

    max_date <- if (all(is.na(dates))) {
      Sys.Date()
    } else {
      max(dates, na.rm = TRUE)
    }

    HTML(
      paste0(
        "Dernière mise à jour : ",
        format(max_date, "%d/%m/%Y")
      )
    )
  })


  # ===================================================================
  # ÉTAPE 20 — EXPORT DES DONNÉES FILTRÉES
  # ===================================================================
  output$export_data <- downloadHandler(

    filename = function() {
      paste0(
        "AIREPRO_export_",
        input$annee,
        "_",
        Sys.Date(),
        ".csv"
      )
    },

    content = function(file) {
      write_csv(current_year_data(), file)
    }
  )
}


# =====================================================================
# ÉTAPE 21 — LANCEMENT
# =====================================================================
shinyApp(ui, server)
