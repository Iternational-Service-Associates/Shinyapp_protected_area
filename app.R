# ============================================================
# APPLICATION SHINY - AIRES PROTEGEES DU SENEGAL
# ============================================================


# ============================================================
# PACKAGES
# ============================================================

library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(rnaturalearth)


# ============================================================
# 1. CHEMINS DES 3 BASES
# ============================================================

dossier <- "C:/Users/H P/Desktop/SHYNI-APP/data"


chemin_base1 <- file.path(
  dossier,
  "aires_protegees.geojson"
)


chemin_base3 <- file.path(
  dossier,
  "Base 2.geojson"
)


chemins_merge <- c(
  file.path(dossier, "merge.xlsx"),
  file.path(dossier, "merge.xls")
)


chemin_merge <- chemins_merge[
  file.exists(chemins_merge)
][1]


if (!file.exists(chemin_base1)) {
  stop("aires_protegees.geojson est introuvable.")
}


if (!file.exists(chemin_base3)) {
  stop("Base 2.geojson est introuvable.")
}


if (is.na(chemin_merge)) {
  stop("Le fichier Excel merge est introuvable.")
}


# ============================================================
# 2. CHARGEMENT DES 3 BASES
# ============================================================

base1 <- st_read(
  chemin_base1,
  quiet = TRUE
)


base_series <- read_excel(
  chemin_merge
)


base3 <- st_read(
  chemin_base3,
  quiet = TRUE
)


# ============================================================
# 3. VERIFICATIONS
# ============================================================

if (!"site_id" %in% names(base1)) {
  stop("site_id absent de aires_protegees.geojson")
}


if (!"site_id" %in% names(base_series)) {
  stop("site_id absent du fichier merge")
}


if (!"site_id" %in% names(base3)) {
  stop("site_id absent de Base 2.geojson")
}


if (!"Annee" %in% names(base_series)) {
  stop("Annee absent du fichier merge")
}


if (!"PAG_a_jour" %in% names(base_series)) {
  stop("PAG_a_jour absent du fichier merge")
}


if (!"Financement_FCFA" %in% names(base_series)) {
  stop("Financement_FCFA absent du fichier merge")
}


if (!"Besoin_total_FCFA" %in% names(base_series)) {
  stop("Besoin_total_FCFA absent du fichier merge")
}


# ============================================================
# 4. IDENTIFIANTS NUMERIQUES
# ============================================================

base1$site_id <- as.numeric(
  base1$site_id
)


base_series$site_id <- as.numeric(
  base_series$site_id
)


base3$site_id <- as.numeric(
  base3$site_id
)


base_series$Annee <- as.numeric(
  base_series$Annee
)


if (any(is.na(base1$site_id))) {
  stop("Certains site_id de la base 1 ne sont pas numériques.")
}


if (any(is.na(base_series$site_id))) {
  stop("Certains site_id de merge ne sont pas numériques.")
}


if (any(is.na(base3$site_id))) {
  stop("Certains site_id de la base 3 ne sont pas numériques.")
}


# ============================================================
# 5. GEOMETRIES
# ============================================================

base1 <- st_make_valid(base1)

base3 <- st_make_valid(base3)


base1 <- base1[
  !st_is_empty(base1),
]


base3 <- base3[
  !st_is_empty(base3),
]


base1 <- st_transform(
  base1,
  4326
)


base3 <- st_transform(
  base3,
  4326
)


# ============================================================
# 6. BASE 3 : site_id + geometry UNIQUEMENT
# ============================================================

base3 <- base3 %>%
  
  select(
    site_id,
    geometry
  )


# ============================================================
# 7. TRADUCTION DE realm
# ============================================================

base1 <- base1 %>%
  
  mutate(
    
    realm_fr = case_when(
      
      tolower(
        trimws(
          as.character(realm)
        )
      ) == "terrestrial" ~ "Terrestre",
      
      tolower(
        trimws(
          as.character(realm)
        )
      ) == "coastal" ~ "Côtier",
      
      tolower(
        trimws(
          as.character(realm)
        )
      ) == "marine" ~ "Marin",
      
      TRUE ~ as.character(realm)
      
    )
    
  )


# ============================================================
# 8. BASE DE REFERENCE DES AIRES
# ============================================================

reference_sites <- base1 %>%
  
  st_drop_geometry() %>%
  
  select(
    any_of(
      c(
        "site_id",
        "name_eng",
        "desig_eng",
        "iucn_cat",
        "realm_fr",
        "gis_area",
        "gov_type",
        "mang_auth"
      )
    )
  ) %>%
  
  distinct(
    site_id,
    .keep_all = TRUE
  )


# ============================================================
# 9. VARIABLES FINANCIERES
# ============================================================

variables_financieres <- c(
  
  "Financement" =
    "Financement_FCFA",
  
  "Besoin total" =
    "Besoin_total_FCFA",
  
  "Besoin en personnel" =
    "Besoin_personnel_FCFA",
  
  "Besoin de fonctionnement" =
    "Besoin_fonctionnement_FCFA",
  
  "Besoin d'investissement" =
    "Besoin_investissement_FCFA",
  
  "Besoin de conservation" =
    "Besoin_conservation_FCFA",
  
  "Besoin de gouvernance" =
    "Besoin_gouvernance_FCFA"
  
)


variables_financieres <-
  variables_financieres[
    unname(
      variables_financieres
    ) %in%
      names(base_series)
  ]


if (length(variables_financieres) == 0) {
  
  stop(
    "Aucune variable financière attendue trouvée dans merge."
  )
  
}


# ============================================================
# 10. VARIABLES DESCRIPTIVES
# ============================================================

variables_descriptives <- c(
  
  "Milieu" =
    "realm_fr",
  
  "Désignation" =
    "desig_eng",
  
  "Catégorie UICN" =
    "iucn_cat",
  
  "Type de gouvernance" =
    "gov_type",
  
  "Superficie SIG" =
    "gis_area"
  
)


# ============================================================
# 11. CONVERSION DES VARIABLES FINANCIERES
# ============================================================

vers_numerique <- function(x) {
  
  if (is.numeric(x)) {
    return(x)
  }
  
  
  x <- as.character(x)
  
  
  x <- gsub(
    "\u00A0",
    "",
    x,
    fixed = TRUE
  )
  
  
  x <- gsub(
    " ",
    "",
    x,
    fixed = TRUE
  )
  
  
  x <- gsub(
    ",",
    "",
    x,
    fixed = TRUE
  )
  
  
  suppressWarnings(
    as.numeric(x)
  )
}


base_series <- base_series %>%
  
  mutate(
    
    across(
      
      any_of(
        unname(
          variables_financieres
        )
      ),
      
      vers_numerique
      
    )
    
  )


# ============================================================
# 11 BIS. AJOUT SUPERFICIE + BESOIN PAR HECTARE
# ============================================================

base_series <- base_series %>%
  
  select(
    -any_of(
      c(
        "gis_area",
        "realm_fr",
        "desig_eng"
      )
    )
  ) %>%
  
  left_join(
    
    reference_sites %>%
      
      select(
        site_id,
        gis_area,
        realm_fr,
        desig_eng
      ),
    
    by = "site_id"
    
  ) %>%
  
  mutate(
    
    gis_area =
      vers_numerique(
        gis_area
      ),
    
    Besoin_unitaire_FCFA_ha =
      case_when(
        
        !is.na(Besoin_total_FCFA) &
          !is.na(gis_area) &
          gis_area > 0 ~
          
          Besoin_total_FCFA /
          gis_area,
        
        TRUE ~ NA_real_
        
      )
    
  )


# ============================================================
# 12. FONCTIONS UTILES
# ============================================================

moyenne_sure <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  
  mean(
    x,
    na.rm = TRUE
  )
}


somme_sure <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  
  sum(
    x,
    na.rm = TRUE
  )
}


premiere_non_na <- function(x) {
  
  x <- as.character(x)
  
  
  x <- x[
    !is.na(x) &
      trimws(x) != ""
  ]
  
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  
  x[1]
}


# ============================================================
# 13. CHOIX DISPONIBLES
# ============================================================

annees <- sort(
  unique(
    na.omit(
      base_series$Annee
    )
  )
)


choix_aires <- setNames(
  
  as.character(
    reference_sites$site_id
  ),
  
  ifelse(
    
    is.na(
      reference_sites$name_eng
    ),
    
    as.character(
      reference_sites$site_id
    ),
    
    reference_sites$name_eng
    
  )
  
)


choix_colonne <- function(
    data,
    variable
) {
  
  if (!variable %in% names(data)) {
    return(character(0))
  }
  
  
  sort(
    unique(
      na.omit(
        as.character(
          data[[variable]]
        )
      )
    )
  )
  
}


choix_region <-
  choix_colonne(
    base_series,
    "Region"
  )


choix_ecosysteme <-
  choix_colonne(
    base_series,
    "Ecosysteme"
  )


choix_gestionnaire <-
  choix_colonne(
    base_series,
    "Gestionnaire"
  )


choix_realm <-
  choix_colonne(
    base1,
    "realm_fr"
  )


choix_designation <-
  choix_colonne(
    base1,
    "desig_eng"
  )


# ============================================================
# 14. CONTOUR DU SENEGAL
# ============================================================

senegal_contour <- rnaturalearth::ne_countries(
  
  scale = "medium",
  
  country = "Senegal",
  
  returnclass = "sf"
  
)


senegal_contour <- st_transform(
  senegal_contour,
  4326
)


# ============================================================
# 15. FONCTION DE FILTRAGE
# ============================================================

filtrer_selection <- function(
    data,
    variable,
    selection
) {
  
  if (
    
    is.null(selection) ||
    
    length(selection) == 0 ||
    
    "ALL" %in% selection ||
    
    !variable %in% names(data)
    
  ) {
    
    return(data)
    
  }
  
  
  data %>%
    
    filter(
      
      as.character(
        .data[[variable]]
      ) %in%
        selection
      
    )
  
}


# ============================================================
# 16. RAYON DES CERCLES
# ============================================================

rayon_proportionnel <- function(x) {
  
  x <- as.numeric(x)
  
  
  if (all(is.na(x))) {
    
    return(
      rep(
        8,
        length(x)
      )
    )
    
  }
  
  
  x[is.na(x)] <- 0
  
  
  x <- pmax(
    x,
    0
  )
  
  
  r <- sqrt(x)
  
  
  if (max(r) == min(r)) {
    
    return(
      rep(
        12,
        length(x)
      )
    )
    
  }
  
  
  6 +
    20 *
    (
      r - min(r)
    ) /
    (
      max(r) - min(r)
    )
  
}


# ============================================================
# 17. INTERFACE UI
# ============================================================

ui <- fluidPage(
  
  
  tags$head(
    
    tags$style(
      
      HTML(
        "

        html, body {
          height: 100%;
          margin: 0;
          padding: 0;
        }


        .container-fluid {
          padding-left: 8px;
          padding-right: 8px;
        }


        h2 {
          margin-top: 7px;
          margin-bottom: 7px;
          font-size: 22px;
        }


        /* ===================================================
           CARTE : CONTOUR VERT FORET
           =================================================== */

        #carte {
          width: 100%;

          border: 3px solid #1B5E20;

          /* TRAIT VERT FORET PLUS MARQUE A DROITE */
          border-right: 7px solid #1B5E20;

          box-sizing: border-box;
        }


        /* ===================================================
           PANNEAUX DES GRAPHIQUES
           =================================================== */

        .mini-panel {
          height: 235px;
          border: 1px solid #e0e0e0;
          border-radius: 5px;
          padding: 4px;
          overflow: hidden;
          background: white;
        }


        /* ===================================================
           PANNEAU DU TABLEAU
           =================================================== */

        .table-panel {
          height: 235px;
          overflow: auto;
          font-size: 10px;
          border: 1px solid #e0e0e0;
          border-radius: 5px;
          padding: 5px;
          background: white;
        }


        .table-panel table {
          font-size: 10px;
        }


        /* ===================================================
           ZONE ENTRE LES GRAPHIQUES EN VERT FORET
           =================================================== */

        .bottom-row {
          background-color: #1B5E20;
          padding-top: 7px;
          padding-bottom: 7px;
          margin-left: 0px;
          margin-right: 0px;
        }


        .bottom-row > div {
          padding-left: 6px;
          padding-right: 6px;
        }


        /* ===================================================
           SIDEBAR EN VERT FORET
           =================================================== */

        .well {
          background-color: #1B5E20 !important;
          border-color: #1B5E20 !important;
        }


        /* ===================================================
           TITRES DU SIDEBAR EN BLANC ET EN GRAS
           =================================================== */

        .well .control-label {
          color: white !important;
          font-weight: 800 !important;
        }


        .well label {
          color: white !important;
        }


        .well .shiny-options-group > label,
        .well .checkbox > label {
          color: white !important;
        }


        /* ===================================================
           TEXTE A L'INTERIEUR DES CHAMPS
           =================================================== */

        .well .form-control,
        .well .selectize-input,
        .well .selectize-input input {
          color: #222222 !important;
        }


        /* ===================================================
           FOND DES CHAMPS EN BLANC
           =================================================== */

        .well .form-control,
        .well .selectize-input {
          background-color: white !important;
        }


        "
      )
      
    )
    
  ),
  
  
  # ==========================================================
  # TITRE PRINCIPAL
  # ==========================================================
  
  titlePanel(
    
    div(
      
      style = "
        text-align:center;
        font-weight:800;
        color:white;
        background-color:#1B5E20;
        padding:8px;
        border-radius:4px;
      ",
      
      "ANALYSE SPATIALE DES AIRES PROTÉGÉES DU SÉNÉGAL"
      
    )
    
  ),
  
  
  sidebarLayout(
    
    
    # ========================================================
    # SIDEBAR
    # ========================================================
    
    sidebarPanel(
      
      width = 2,
      
      
      selectInput(
        
        "periode",
        
        "Année",
        
        choices = c(
          "Nothing",
          "ALL",
          annees
        ),
        
        selected = "Nothing"
        
      ),
      
      
      selectizeInput(
        
        "aire",
        
        "Aire protégée",
        
        choices = choix_aires,
        
        multiple = FALSE,
        
        selected = NULL,
        
        options = list(
          
          placeholder =
            "Choisir une aire ou cliquer sur la carte"
          
        )
        
      ),
      
      
      selectizeInput(
        
        "realm_filtre",
        
        "Milieu",
        
        choices = c(
          "ALL",
          choix_realm
        ),
        
        selected = "ALL",
        
        multiple = TRUE
        
      ),
      
      
      selectizeInput(
        
        "designation_filtre",
        
        "Type d'aire protégée",
        
        choices = c(
          "ALL",
          choix_designation
        ),
        
        selected = "ALL",
        
        multiple = TRUE
        
      ),
      
      
      conditionalPanel(
        
        condition =
          "input.periode != 'Nothing'",
        
        
        selectizeInput(
          
          "region_filtre",
          
          "Région",
          
          choices = c(
            "ALL",
            choix_region
          ),
          
          selected = "ALL",
          
          multiple = TRUE
          
        ),
        
        
        selectizeInput(
          
          "ecosysteme_filtre",
          
          "Écosystème",
          
          choices = c(
            "ALL",
            choix_ecosysteme
          ),
          
          selected = "ALL",
          
          multiple = TRUE
          
        ),
        
        
        selectizeInput(
          
          "gestionnaire_filtre",
          
          "Gestionnaire",
          
          choices = c(
            "ALL",
            choix_gestionnaire
          ),
          
          selected = "ALL",
          
          multiple = TRUE
          
        )
        
      ),
      
      
      uiOutput(
        "variable_carte_ui"
      ),
      
      
      conditionalPanel(
        
        condition =
          "input.periode != 'Nothing'",
        
        
        checkboxGroupInput(
          
          "variables_graph",
          
          "Variables du graphique",
          
          choices =
            variables_financieres,
          
          selected =
            head(
              unname(
                variables_financieres
              ),
              2
            )
          
        )
        
      ),
      
      
      selectInput(
        
        "map_type",
        
        "Type de représentation",
        
        choices = c(
          
          "Polygones" =
            "polygons",
          
          "Cercles proportionnels" =
            "circles"
          
        ),
        
        selected =
          "polygons"
        
      )
      
    ),
    
    
    # ========================================================
    # ZONE PRINCIPALE
    # ========================================================
    
    mainPanel(
      
      width = 10,
      
      
      # ======================================================
      # CARTE
      # ======================================================
      
      leafletOutput(
        
        "carte",
        
        height =
          "410px"
        
      ),
      
      
      # ======================================================
      # GRAPHIQUES + TABLEAU SUR UNE MEME LIGNE
      # ======================================================
      
      fluidRow(
        
        class = "bottom-row",
        
        
        column(
          
          width = 3,
          
          div(
            
            class =
              "mini-panel",
            
            plotOutput(
              
              "graphique",
              
              height =
                "225px"
              
            )
            
          )
          
        ),
        
        
        column(
          
          width = 3,
          
          div(
            
            class =
              "mini-panel",
            
            plotOutput(
              
              "financement_milieu",
              
              height =
                "225px"
              
            )
            
          )
          
        ),
        
        
        column(
          
          width = 3,
          
          div(
            
            class =
              "mini-panel",
            
            plotOutput(
              
              "superficie_gestionnaire",
              
              height =
                "225px"
              
            )
            
          )
          
        ),
        
        
        column(
          
          width = 3,
          
          div(
            
            class =
              "table-panel",
            
            tableOutput(
              "details"
            )
            
          )
          
        )
        
      )
      
    )
    
  )
  
)


# ============================================================
# 18. SERVEUR
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # VARIABLE DE VISUALISATION
  # ==========================================================
  
  output$variable_carte_ui <- renderUI({
    
    
    if (
      input$periode == "Nothing"
    ) {
      
      
      selectInput(
        
        "variable_carte",
        
        "Choix de visualisation",
        
        choices =
          variables_descriptives,
        
        selected =
          "realm_fr"
        
      )
      
      
    } else {
      
      
      selectInput(
        
        "variable_carte",
        
        "Indicateur financier",
        
        choices =
          variables_financieres,
        
        selected =
          unname(
            variables_financieres[1]
          )
        
      )
      
    }
    
  })
  
  
  # ==========================================================
  # GESTION DU ALL
  # ==========================================================
  
  observeEvent(
    input$realm_filtre,
    {
      
      if (
        "ALL" %in%
        input$realm_filtre
      ) {
        
        updateSelectizeInput(
          
          session,
          
          "realm_filtre",
          
          selected =
            choix_realm
          
        )
        
      }
      
    },
    ignoreInit = TRUE
  )
  
  
  observeEvent(
    input$designation_filtre,
    {
      
      if (
        "ALL" %in%
        input$designation_filtre
      ) {
        
        updateSelectizeInput(
          
          session,
          
          "designation_filtre",
          
          selected =
            choix_designation
          
        )
        
      }
      
    },
    ignoreInit = TRUE
  )
  
  
  observeEvent(
    input$region_filtre,
    {
      
      if (
        "ALL" %in%
        input$region_filtre
      ) {
        
        updateSelectizeInput(
          
          session,
          
          "region_filtre",
          
          selected =
            choix_region
          
        )
        
      }
      
    },
    ignoreInit = TRUE
  )
  
  
  observeEvent(
    input$ecosysteme_filtre,
    {
      
      if (
        "ALL" %in%
        input$ecosysteme_filtre
      ) {
        
        updateSelectizeInput(
          
          session,
          
          "ecosysteme_filtre",
          
          selected =
            choix_ecosysteme
          
        )
        
      }
      
    },
    ignoreInit = TRUE
  )
  
  
  observeEvent(
    input$gestionnaire_filtre,
    {
      
      if (
        "ALL" %in%
        input$gestionnaire_filtre
      ) {
        
        updateSelectizeInput(
          
          session,
          
          "gestionnaire_filtre",
          
          selected =
            choix_gestionnaire
          
        )
        
      }
      
    },
    ignoreInit = TRUE
  )
  
  
  # ==========================================================
  # CLIC CARTE -> SIDEBAR
  # ==========================================================
  
  observeEvent(
    input$carte_shape_click,
    {
      
      id_click <-
        input$carte_shape_click$id
      
      
      if (
        !is.null(id_click)
      ) {
        
        
        id_click <-
          as.character(
            id_click
          )
        
        
        updateSelectizeInput(
          
          session,
          
          "aire",
          
          selected =
            id_click
          
        )
        
      }
      
    }
    
  )
  
  
  # ==========================================================
  # DONNEES ANNUELLES FILTREES
  # ==========================================================
  
  series_filtrees <- reactive({
    
    
    data <-
      base_series
    
    
    data <- filtrer_selection(
      data,
      "Region",
      input$region_filtre
    )
    
    
    data <- filtrer_selection(
      data,
      "Ecosysteme",
      input$ecosysteme_filtre
    )
    
    
    data <- filtrer_selection(
      data,
      "Gestionnaire",
      input$gestionnaire_filtre
    )
    
    
    data <- filtrer_selection(
      data,
      "realm_fr",
      input$realm_filtre
    )
    
    
    data <- filtrer_selection(
      data,
      "desig_eng",
      input$designation_filtre
    )
    
    
    data
    
  })
  
  
  # ==========================================================
  # DONNEES DE LA PERIODE
  # ==========================================================
  
  donnees_periode <- reactive({
    
    
    data <-
      series_filtrees()
    
    
    if (
      input$periode == "Nothing"
    ) {
      
      return(data)
      
    }
    
    
    if (
      input$periode != "ALL"
    ) {
      
      
      data <- data %>%
        
        filter(
          
          Annee ==
            as.numeric(
              input$periode
            )
          
        )
      
    }
    
    
    data
    
  })
  
  
  # ==========================================================
  # DONNEES POUR LA CARTE
  # ==========================================================
  
  donnees_carte <- reactive({
    
    
    req(
      input$variable_carte
    )
    
    
    if (
      input$periode == "Nothing"
    ) {
      
      
      infos_recentes <-
        base_series %>%
        
        filter(
          !is.na(Annee)
        ) %>%
        
        arrange(
          site_id,
          desc(Annee)
        ) %>%
        
        group_by(
          site_id
        ) %>%
        
        summarise(
          
          PAG_a_jour =
            premiere_non_na(
              PAG_a_jour
            ),
          
          Besoin_unitaire_FCFA_ha =
            moyenne_sure(
              Besoin_unitaire_FCFA_ha
            ),
          
          .groups =
            "drop"
          
        )
      
      
      data <- base1 %>%
        
        left_join(
          infos_recentes,
          by = "site_id"
        )
      
      
      data <- filtrer_selection(
        data,
        "realm_fr",
        input$realm_filtre
      )
      
      
      data <- filtrer_selection(
        data,
        "desig_eng",
        input$designation_filtre
      )
      
      
      return(data)
      
    }
    
    
    serie <-
      donnees_periode()
    
    
    variable <-
      input$variable_carte
    
    
    valeurs <- serie %>%
      
      group_by(
        site_id
      ) %>%
      
      summarise(
        
        valeur_carte =
          moyenne_sure(
            .data[[variable]]
          ),
        
        .groups =
          "drop"
        
      )
    
    
    infos_popup <- serie %>%
      
      arrange(
        site_id,
        desc(Annee)
      ) %>%
      
      group_by(
        site_id
      ) %>%
      
      summarise(
        
        PAG_a_jour =
          premiere_non_na(
            PAG_a_jour
          ),
        
        Besoin_unitaire_FCFA_ha =
          moyenne_sure(
            Besoin_unitaire_FCFA_ha
          ),
        
        .groups =
          "drop"
        
      )
    
    
    data <- base3 %>%
      
      left_join(
        reference_sites,
        by = "site_id"
      ) %>%
      
      left_join(
        valeurs,
        by = "site_id"
      ) %>%
      
      left_join(
        infos_popup,
        by = "site_id"
      )
    
    
    ids_valides <-
      unique(
        serie$site_id
      )
    
    
    data <- data %>%
      
      filter(
        site_id %in%
          ids_valides
      )
    
    
    data <- filtrer_selection(
      data,
      "realm_fr",
      input$realm_filtre
    )
    
    
    data <- filtrer_selection(
      data,
      "desig_eng",
      input$designation_filtre
    )
    
    
    data
    
  })
  
  
  # ==========================================================
  # CARTE LEAFLET
  # ==========================================================
  
  output$carte <- renderLeaflet({
    
    
    data <-
      donnees_carte()
    
    
    shiny::validate(
      
      shiny::need(
        nrow(data) > 0,
        "Aucune donnée pour cette sélection."
      )
      
    )
    
    
    variable <-
      input$variable_carte
    
    
    if (
      !"name_eng" %in%
      names(data)
    ) {
      
      data$name_eng <-
        as.character(
          data$site_id
        )
      
    }
    
    
    data$nom_affichage <-
      ifelse(
        
        is.na(data$name_eng) |
          data$name_eng == "",
        
        as.character(
          data$site_id
        ),
        
        data$name_eng
        
      )
    
    
    data$pag_affichage <-
      ifelse(
        
        is.na(
          data$PAG_a_jour
        ) |
          data$PAG_a_jour == "",
        
        "Non renseigné",
        
        as.character(
          data$PAG_a_jour
        )
        
      )
    
    
    data$superficie_affichage <-
      ifelse(
        
        is.na(
          data$gis_area
        ),
        
        "Non disponible",
        
        paste0(
          
          format(
            
            round(
              data$gis_area,
              0
            ),
            
            big.mark = " ",
            
            scientific = FALSE,
            
            trim = TRUE
            
          ),
          
          " ha"
          
        )
        
      )
    
    
    data$besoin_unitaire_affichage <-
      ifelse(
        
        is.na(
          data$Besoin_unitaire_FCFA_ha
        ),
        
        "Non disponible",
        
        paste0(
          
          format(
            
            round(
              data$Besoin_unitaire_FCFA_ha,
              0
            ),
            
            big.mark = " ",
            
            scientific = FALSE,
            
            trim = TRUE
            
          ),
          
          " FCFA/ha"
          
        )
        
      )
    
    
    # ========================================================
    # MODE DESCRIPTIF
    # ========================================================
    
    if (
      input$periode == "Nothing"
    ) {
      
      
      valeurs <-
        data[[variable]]
      
      
      if (
        variable == "realm_fr"
      ) {
        
        
        # ====================================================
        # COULEURS DES MILIEUX
        # TERRESTRE = VERT FORET
        # ====================================================
        
        couleurs_realm <- c(
          
          "Terrestre" =
            "#1B5E20",
          
          "Côtier" =
            "#FFD54F",
          
          "Marin" =
            "#0D47A1"
          
        )
        
        
        data$couleur <-
          couleurs_realm[
            data$realm_fr
          ]
        
        
        data$couleur[
          is.na(
            data$couleur
          )
        ] <- "#BDBDBD"
        
        
        type_legende <-
          "realm"
        
        
      } else if (
        is.numeric(valeurs)
      ) {
        
        
        shiny::validate(
          
          shiny::need(
            any(!is.na(valeurs)),
            "Aucune valeur disponible pour cette variable."
          )
          
        )
        
        
        pal <- colorNumeric(
          
          palette =
            "YlOrRd",
          
          domain =
            valeurs,
          
          na.color =
            "#BDBDBD"
          
        )
        
        
        data$couleur <-
          pal(
            valeurs
          )
        
        
        type_legende <-
          "numeric"
        
        
      } else {
        
        
        pal <- colorFactor(
          
          palette =
            "Set2",
          
          domain =
            valeurs,
          
          na.color =
            "#BDBDBD"
          
        )
        
        
        data$couleur <-
          pal(
            valeurs
          )
        
        
        type_legende <-
          "factor"
        
      }
      
      
      data$popup_txt <-
        paste0(
          
          "<b>",
          data$nom_affichage,
          "</b>",
          
          "<br><b>PAG à jour :</b> ",
          data$pag_affichage,
          
          "<br><b>Milieu :</b> ",
          data$realm_fr,
          
          "<br><b>Désignation :</b> ",
          data$desig_eng,
          
          "<br><b>Catégorie UICN :</b> ",
          data$iucn_cat,
          
          "<br><b>Superficie :</b> ",
          data$superficie_affichage,
          
          "<br><b>Besoin par unité de surface :</b> ",
          data$besoin_unitaire_affichage
          
        )
      
      
    } else {
      
      
      valeurs <-
        data$valeur_carte
      
      
      shiny::validate(
        
        shiny::need(
          any(!is.na(valeurs)),
          "Aucune donnée financière disponible pour cette sélection."
        )
        
      )
      
      
      pal <- colorNumeric(
        
        palette =
          "YlOrRd",
        
        domain =
          valeurs,
        
        na.color =
          "#BDBDBD"
        
      )
      
      
      data$couleur <-
        pal(
          valeurs
        )
      
      
      type_legende <-
        "numeric"
      
      
      periode_txt <-
        ifelse(
          
          input$periode == "ALL",
          
          "Toutes les années",
          
          paste(
            "Année",
            input$periode
          )
          
        )
      
      
      data$valeur_affichage <-
        ifelse(
          
          is.na(
            data$valeur_carte
          ),
          
          "Non disponible",
          
          paste0(
            
            format(
              
              round(
                data$valeur_carte,
                0
              ),
              
              big.mark = " ",
              
              scientific = FALSE,
              
              trim = TRUE
              
            ),
            
            " FCFA"
            
          )
          
        )
      
      
      data$popup_txt <-
        paste0(
          
          "<b>",
          data$nom_affichage,
          "</b>",
          
          "<br><b>PAG à jour :</b> ",
          data$pag_affichage,
          
          "<br><b>",
          periode_txt,
          "</b>",
          
          "<br><b>Valeur :</b> ",
          data$valeur_affichage,
          
          "<br><b>Besoin par unité de surface :</b> ",
          data$besoin_unitaire_affichage
          
        )
      
    }
    
    
    # ========================================================
    # CREATION CARTE
    # ========================================================
    
    carte <- leaflet(
      
      options =
        leafletOptions(
          
          preferCanvas =
            TRUE,
          
          minZoom =
            5,
          
          maxZoom =
            12
          
        )
      
    ) %>%
      
      addTiles(
        group =
          "OpenStreetMap"
      )
    
    
    # ========================================================
    # POLYGONES
    # ========================================================
    
    if (
      input$map_type == "polygons"
    ) {
      
      
      carte <- carte %>%
        
        addPolygons(
          
          data =
            data,
          
          layerId =
            ~site_id,
          
          color =
            "#444444",
          
          weight =
            1.5,
          
          fillColor =
            ~couleur,
          
          fillOpacity =
            0.65,
          
          popup =
            ~popup_txt,
          
          label =
            ~nom_affichage,
          
          highlightOptions =
            highlightOptions(
              
              weight =
                4,
              
              color =
                "#FF8C00",
              
              bringToFront =
                TRUE
              
            )
          
        )
      
    }
    
    
    # ========================================================
    # CERCLES PROPORTIONNELS
    # ========================================================
    
    if (
      input$map_type == "circles"
    ) {
      
      
      points <-
        suppressWarnings(
          
          st_point_on_surface(
            data
          )
          
        )
      
      
      if (
        input$periode == "Nothing"
      ) {
        
        
        points$rayon <-
          rayon_proportionnel(
            points$gis_area
          )
        
        
      } else {
        
        
        points$rayon <-
          rayon_proportionnel(
            points$valeur_carte
          )
        
      }
      
      
      carte <- carte %>%
        
        addCircleMarkers(
          
          data =
            points,
          
          layerId =
            ~site_id,
          
          radius =
            ~rayon,
          
          color =
            "#333333",
          
          weight =
            1,
          
          fillColor =
            ~couleur,
          
          fillOpacity =
            0.70,
          
          popup =
            ~popup_txt,
          
          label =
            ~nom_affichage
          
        )
      
    }
    
    
    # ========================================================
    # CONTOUR NATIONAL EN VERT FORET
    # ========================================================
    
    carte <- carte %>%
      
      addPolygons(
        
        data =
          senegal_contour,
        
        color =
          "#1B5E20",
        
        weight =
          3,
        
        opacity =
          1,
        
        fill =
          FALSE,
        
        options =
          pathOptions(
            interactive =
              FALSE
          )
        
      )
    
    
    # ========================================================
    # LEGENDE EN HAUT A DROITE
    # ========================================================
    
    if (
      type_legende == "realm"
    ) {
      
      
      presente <-
        intersect(
          
          names(
            couleurs_realm
          ),
          
          unique(
            data$realm_fr
          )
          
        )
      
      
      carte <- carte %>%
        
        addLegend(
          
          position =
            "topright",
          
          colors =
            unname(
              couleurs_realm[
                presente
              ]
            ),
          
          labels =
            presente,
          
          title =
            "Milieu",
          
          opacity =
            1
          
        )
      
      
    } else {
      
      
      carte <- carte %>%
        
        addLegend(
          
          position =
            "topright",
          
          pal =
            pal,
          
          values =
            valeurs,
          
          title =
            ifelse(
              
              input$periode ==
                "Nothing",
              
              variable,
              
              paste0(
                variable,
                " (FCFA)"
              )
              
            ),
          
          opacity =
            1
          
        )
      
    }
    
    
    bbox <-
      st_bbox(
        senegal_contour
      )
    
    
    carte %>%
      
      fitBounds(
        
        lng1 =
          unname(
            bbox["xmin"]
          ),
        
        lat1 =
          unname(
            bbox["ymin"]
          ),
        
        lng2 =
          unname(
            bbox["xmax"]
          ),
        
        lat2 =
          unname(
            bbox["ymax"]
          )
        
      )
    
  })
  
  
  # ==========================================================
  # SIDEBAR -> SELECTION SUR LA CARTE
  # ==========================================================
  
  observe({
    
    
    req(
      input$aire
    )
    
    
    id_selectionne <-
      as.character(
        input$aire
      )
    
    
    data_actuelle <-
      donnees_carte()
    
    
    aire_selectionnee <-
      data_actuelle %>%
      
      filter(
        
        as.character(
          site_id
        ) ==
          id_selectionne
        
      )
    
    
    if (
      nrow(
        aire_selectionnee
      ) == 0
    ) {
      
      
      aire_selectionnee <-
        base1 %>%
        
        filter(
          
          as.character(
            site_id
          ) ==
            id_selectionne
          
        )
      
    }
    
    
    req(
      nrow(
        aire_selectionnee
      ) > 0
    )
    
    
    bbox_selection <-
      st_bbox(
        aire_selectionnee
      )
    
    
    leafletProxy(
      "carte"
    ) %>%
      
      clearGroup(
        "selection"
      ) %>%
      
      addPolygons(
        
        data =
          aire_selectionnee,
        
        group =
          "selection",
        
        color =
          "#FF8C00",
        
        weight =
          5,
        
        opacity =
          1,
        
        fill =
          FALSE,
        
        options =
          pathOptions(
            interactive =
              FALSE
          )
        
      ) %>%
      
      fitBounds(
        
        lng1 =
          unname(
            bbox_selection["xmin"]
          ),
        
        lat1 =
          unname(
            bbox_selection["ymin"]
          ),
        
        lng2 =
          unname(
            bbox_selection["xmax"]
          ),
        
        lat2 =
          unname(
            bbox_selection["ymax"]
          )
        
      )
    
  })
  
  
  # ==========================================================
  # GRAPHIQUE HISTORIQUE / SERIE TEMPORELLE
  # ==========================================================
  
  output$graphique <- renderPlot({
    
    
    if (
      input$periode == "Nothing"
    ) {
      
      return(NULL)
      
    }
    
    
    shiny::validate(
      
      shiny::need(
        
        !is.null(
          input$aire
        ) &&
          input$aire != "",
        
        "Choisissez une aire."
        
      ),
      
      shiny::need(
        
        length(
          input$variables_graph
        ) > 0,
        
        "Choisissez une variable."
        
      )
      
    )
    
    
    id_selectionne <-
      as.numeric(
        input$aire
      )
    
    
    data <- base_series %>%
      
      filter(
        site_id ==
          id_selectionne
      )
    
    
    if (
      input$periode != "ALL"
    ) {
      
      
      data <- data %>%
        
        filter(
          
          Annee ==
            as.numeric(
              input$periode
            )
          
        )
      
    }
    
    
    shiny::validate(
      
      shiny::need(
        nrow(data) > 0,
        "Aucune donnée."
      )
      
    )
    
    
    data_long <- data %>%
      
      select(
        
        Annee,
        
        all_of(
          input$variables_graph
        )
        
      ) %>%
      
      pivot_longer(
        
        cols =
          -Annee,
        
        names_to =
          "Indicateur",
        
        values_to =
          "Valeur"
        
      ) %>%
      
      group_by(
        Annee,
        Indicateur
      ) %>%
      
      summarise(
        
        Valeur =
          moyenne_sure(
            Valeur
          ),
        
        .groups =
          "drop"
        
      )
    
    
    nom_site <-
      reference_sites$name_eng[
        
        match(
          id_selectionne,
          reference_sites$site_id
        )
        
      ]
    
    
    if (
      length(nom_site) == 0 ||
      is.na(nom_site)
    ) {
      
      nom_site <-
        as.character(
          id_selectionne
        )
      
    }
    
    
    if (
      input$periode == "ALL"
    ) {
      
      
      ggplot(
        
        data_long,
        
        aes(
          
          x =
            Annee,
          
          y =
            Valeur,
          
          color =
            Indicateur,
          
          group =
            Indicateur
          
        )
        
      ) +
        
        geom_line(
          linewidth =
            0.9
        ) +
        
        geom_point(
          size =
            2
        ) +
        
        labs(
          
          title =
            paste(
              "Évolution -",
              nom_site
            ),
          
          x =
            "Année",
          
          y =
            "FCFA",
          
          color =
            "Indicateur"
          
        ) +
        
        scale_x_continuous(
          
          breaks =
            sort(
              unique(
                data_long$Annee
              )
            )
          
        ) +
        
        scale_y_continuous(
          
          labels =
            function(x) {
              
              format(
                
                x,
                
                big.mark = " ",
                
                scientific = FALSE,
                
                trim = TRUE
                
              )
              
            }
          
        ) +
        
        theme_minimal(
          base_size =
            8
        ) +
        
        theme(
          
          plot.title =
            element_text(
              size = 10,
              face = "bold",
              hjust = 0.5
            ),
          
          legend.position =
            "top",
          
          legend.justification =
            "right",
          
          legend.title =
            element_text(
              size = 7
            ),
          
          legend.text =
            element_text(
              size = 6
            )
          
        )
      
      
    } else {
      
      
      ggplot(
        
        data_long,
        
        aes(
          
          x =
            Indicateur,
          
          y =
            Valeur,
          
          fill =
            Indicateur
          
        )
        
      ) +
        
        geom_col(
          width =
            0.65
        ) +
        
        labs(
          
          title =
            paste(
              nom_site,
              "-",
              input$periode
            ),
          
          x =
            NULL,
          
          y =
            "FCFA"
          
        ) +
        
        scale_y_continuous(
          
          labels =
            function(x) {
              
              format(
                
                x,
                
                big.mark = " ",
                
                scientific = FALSE,
                
                trim = TRUE
                
              )
              
            }
          
        ) +
        
        theme_minimal(
          base_size =
            8
        ) +
        
        theme(
          
          plot.title =
            element_text(
              size = 10,
              face = "bold",
              hjust = 0.5
            ),
          
          axis.text.x =
            element_text(
              angle = 35,
              hjust = 1,
              size = 6
            ),
          
          legend.position =
            "none"
          
        )
      
    }
    
  })
  
  
  # ==========================================================
  # DIAGRAMME CIRCULAIRE :
  # FINANCEMENT PAR MILIEU
  # ==========================================================
  
  output$financement_milieu <- renderPlot({
    
    
    if (
      input$periode == "Nothing"
    ) {
      
      return(NULL)
      
    }
    
    
    data <-
      donnees_periode()
    
    
    financement_realm <- data %>%
      
      filter(
        
        !is.na(
          realm_fr
        ),
        
        !is.na(
          Financement_FCFA
        )
        
      ) %>%
      
      group_by(
        realm_fr
      ) %>%
      
      summarise(
        
        Financement =
          somme_sure(
            Financement_FCFA
          ),
        
        .groups =
          "drop"
        
      ) %>%
      
      filter(
        
        !is.na(
          Financement
        ),
        
        Financement > 0
        
      )
    
    
    shiny::validate(
      
      shiny::need(
        
        nrow(
          financement_realm
        ) > 0,
        
        "Aucun financement disponible."
        
      )
      
    )
    
    
    financement_realm <-
      financement_realm %>%
      
      mutate(
        
        Pourcentage =
          100 *
          Financement /
          sum(
            Financement
          )
        
      )
    
    
    ggplot(
      
      financement_realm,
      
      aes(
        
        x = "",
        
        y =
          Financement,
        
        fill =
          realm_fr
        
      )
      
    ) +
      
      geom_col(
        width = 1
      ) +
      
      coord_polar(
        theta = "y"
      ) +
      
      geom_text(
        
        aes(
          
          label =
            paste0(
              round(
                Pourcentage,
                1
              ),
              "%"
            )
          
        ),
        
        position =
          position_stack(
            vjust = 0.5
          ),
        
        size =
          3
        
      ) +
      
      scale_fill_manual(
        
        values = c(
          
          "Terrestre" =
            "#1B5E20",
          
          "Côtier" =
            "#FFD54F",
          
          "Marin" =
            "#0D47A1"
          
        ),
        
        na.value =
          "#BDBDBD"
        
      ) +
      
      labs(
        
        title =
          "Financement annuel par milieu",
        
        fill =
          "Milieu"
        
      ) +
      
      theme_void(
        base_size =
          8
      ) +
      
      theme(
        
        plot.title =
          element_text(
            size = 10,
            face = "bold",
            hjust = 0.5
          ),
        
        legend.position =
          "bottom",
        
        legend.title =
          element_text(
            size = 7
          ),
        
        legend.text =
          element_text(
            size = 7
          )
        
      )
    
  })
  
  
  # ==========================================================
  # PART FINANCIERE PAR GESTIONNAIRE EN POURCENTAGE
  # POUR L'AIRE PROTEGEE SELECTIONNEE
  # ==========================================================
  
  output$superficie_gestionnaire <- renderPlot({
    
    
    if (
      input$periode == "Nothing"
    ) {
      
      return(NULL)
      
    }
    
    
    # ========================================================
    # VERIFIER QU'UNE AIRE PROTEGEE EST SELECTIONNEE
    # ========================================================
    
    shiny::validate(
      
      shiny::need(
        
        !is.null(
          input$aire
        ) &&
          input$aire != "",
        
        "Choisissez une aire protégée."
        
      )
      
    )
    
    
    # ========================================================
    # DONNEES DE LA PERIODE
    # ========================================================
    
    data <-
      donnees_periode()
    
    
    # ========================================================
    # FILTRAGE SUR L'AIRE PROTEGEE SELECTIONNEE
    # ========================================================
    
    id_selectionne <-
      as.numeric(
        input$aire
      )
    
    
    data <- data %>%
      
      filter(
        
        site_id ==
          id_selectionne
        
      )
    
    
    # ========================================================
    # VERIFICATION DES DONNEES DE L'AIRE
    # ========================================================
    
    shiny::validate(
      
      shiny::need(
        
        nrow(data) > 0,
        
        "Aucune donnée disponible pour cette aire protégée."
        
      )
      
    )
    
    
    # ========================================================
    # CALCUL DU FINANCEMENT PAR GESTIONNAIRE
    # ========================================================
    
    financement_gestion <- data %>%
      
      filter(
        
        !is.na(
          Gestionnaire
        ),
        
        trimws(
          as.character(
            Gestionnaire
          )
        ) != "",
        
        !is.na(
          Financement_FCFA
        ),
        
        Financement_FCFA > 0
        
      ) %>%
      
      group_by(
        Gestionnaire
      ) %>%
      
      summarise(
        
        Financement_total =
          sum(
            Financement_FCFA,
            na.rm = TRUE
          ),
        
        .groups =
          "drop"
        
      )
    
    
    # ========================================================
    # VERIFICATION DU FINANCEMENT
    # ========================================================
    
    shiny::validate(
      
      shiny::need(
        
        nrow(
          financement_gestion
        ) > 0,
        
        "Aucun financement disponible par gestionnaire."
        
      ),
      
      shiny::need(
        
        sum(
          financement_gestion$Financement_total,
          na.rm = TRUE
        ) > 0,
        
        "Le financement total est nul."
        
      )
      
    )
    
    
    # ========================================================
    # CALCUL DU POURCENTAGE
    # ========================================================
    
    financement_gestion <-
      financement_gestion %>%
      
      mutate(
        
        Pourcentage =
          100 *
          Financement_total /
          sum(
            Financement_total,
            na.rm = TRUE
          )
        
      )
    
    
    # ========================================================
    # NOM DE L'AIRE PROTEGEE
    # ========================================================
    
    nom_aire <-
      reference_sites$name_eng[
        
        match(
          id_selectionne,
          reference_sites$site_id
        )
        
      ]
    
    
    if (
      
      length(
        nom_aire
      ) == 0 ||
      
      is.na(
        nom_aire
      ) ||
      
      nom_aire == ""
      
    ) {
      
      nom_aire <-
        as.character(
          id_selectionne
        )
      
    }
    
    
    # ========================================================
    # GRAPHIQUE
    # ========================================================
    
    ggplot(
      
      financement_gestion,
      
      aes(
        
        x =
          reorder(
            Gestionnaire,
            Pourcentage
          ),
        
        y =
          Pourcentage
        
      )
      
    ) +
      
      geom_col(
        
        width =
          0.65,
        
        fill =
          "#1B5E20"
        
      ) +
      
      # ======================================================
    # POURCENTAGE A COTE DE CHAQUE BARRE
    # ======================================================
    
    geom_text(
      
      aes(
        
        label =
          paste0(
            round(
              Pourcentage,
              1
            ),
            " %"
          )
        
      ),
      
      hjust =
        -0.15,
      
      size =
        3
      
    ) +
      
      coord_flip(
        clip = "off"
      ) +
      
      labs(
        
        title =
          paste0(
            "Part financière par gestionnaire - ",
            nom_aire
          ),
        
        x =
          NULL,
        
        y =
          "Part dans le financement total (%)"
        
      ) +
      
      # ======================================================
    # AXE GRADUE EN POURCENTAGE
    # ======================================================
    
    scale_y_continuous(
      
      limits =
        c(
          0,
          110
        ),
      
      breaks =
        seq(
          0,
          100,
          20
        ),
      
      labels =
        function(x) {
          
          paste0(
            round(
              x,
              0
            ),
            " %"
          )
          
        },
      
      expand =
        expansion(
          mult =
            c(
              0,
              0.02
            )
        )
      
    ) +
      
      theme_minimal(
        base_size =
          8
      ) +
      
      theme(
        
        plot.title =
          element_text(
            size = 9,
            face = "bold",
            hjust = 0.5
          ),
        
        axis.text.y =
          element_text(
            size = 6
          ),
        
        axis.text.x =
          element_text(
            size = 6
          ),
        
        plot.margin =
          margin(
            5,
            20,
            5,
            5
          )
        
      )
    
  })
  
  
  # ==========================================================
  # TABLEAU D'INFORMATIONS
  # ==========================================================
  
  output$details <- renderTable({
    
    
    req(
      input$aire
    )
    
    
    id_selectionne <-
      as.numeric(
        input$aire
      )
    
    
    if (
      input$periode == "Nothing"
    ) {
      
      
      info_financiere <-
        base_series %>%
        
        filter(
          site_id ==
            id_selectionne
        ) %>%
        
        arrange(
          desc(Annee)
        ) %>%
        
        summarise(
          
          PAG_a_jour =
            premiere_non_na(
              PAG_a_jour
            ),
          
          Besoin_unitaire_FCFA_ha =
            moyenne_sure(
              Besoin_unitaire_FCFA_ha
            )
          
        )
      
      
      resultat <-
        reference_sites %>%
        
        filter(
          site_id ==
            id_selectionne
        ) %>%
        
        select(
          -site_id
        ) %>%
        
        bind_cols(
          info_financiere
        )
      
      
      return(
        resultat
      )
      
    }
    
    
    if (
      input$periode == "ALL"
    ) {
      
      
      resultat <-
        base_series %>%
        
        filter(
          site_id ==
            id_selectionne
        ) %>%
        
        arrange(
          desc(Annee)
        ) %>%
        
        slice_head(
          n = 1
        ) %>%
        
        select(
          
          any_of(
            c(
              
              "name_eng",
              "Annee",
              "PAG_a_jour",
              "Region",
              "Ecosysteme",
              "Gestionnaire",
              "Ramsar",
              "UNESCO",
              "Pression_ecologique",
              "Score_capacite",
              "Besoin_unitaire_FCFA_ha",
              "Categorie_source",
              "Date_MAJ"
              
            )
          )
          
        )
      
      
      return(
        resultat
      )
      
    }
    
    
    annee_selectionnee <-
      as.numeric(
        input$periode
      )
    
    
    base_series %>%
      
      filter(
        
        site_id ==
          id_selectionne,
        
        Annee ==
          annee_selectionnee
        
      ) %>%
      
      select(
        
        any_of(
          c(
            
            "name_eng",
            "Annee",
            "PAG_a_jour",
            "Region",
            "Ecosysteme",
            "Gestionnaire",
            "Ramsar",
            "UNESCO",
            "Pression_ecologique",
            "Score_capacite",
            "Besoin_unitaire_FCFA_ha",
            
            unname(
              variables_financieres
            ),
            
            "Categorie_source",
            "Date_MAJ"
            
          )
        )
        
      )
    
  })
  
}


# ============================================================
# 19. LANCEMENT
# ============================================================

shinyApp(
  ui = ui,
  server = server
)