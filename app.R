
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

dossier <- "data/"

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


# Vérification
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

base1 <- st_transform(
  base1,
  4326
)

base3 <- st_transform(
  base3,
  4326
)


# ============================================================
# 6. BASE 3 :
# site_id + geometry UNIQUEMENT
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
      
      tolower(trimws(as.character(realm))) ==
        "terrestrial" ~ "Terrestre",
      
      tolower(trimws(as.character(realm))) ==
        "coastal" ~ "Côtier",
      
      tolower(trimws(as.character(realm))) ==
        "marine" ~ "Marin",
      
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
    unname(variables_financieres) %in%
      names(base_series)
  ]


if (length(variables_financieres) == 0) {
  stop("Aucune variable financière attendue trouvée dans merge.")
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
# 12. FONCTION MOYENNE SURE
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


# IMPORTANT :
# site_id reste numérique dans la base.
# Le caractère ici sert uniquement au composant HTML Shiny.

choix_aires <- setNames(
  
  as.character(
    reference_sites$site_id
  ),
  
  ifelse(
    is.na(reference_sites$name_eng),
    as.character(reference_sites$site_id),
    reference_sites$name_eng
  )
  
)


choix_colonne <- function(data, variable) {
  
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
# 15. EMPRISE
# ============================================================

bb <- st_bbox(base1)

xmin <- as.numeric(bb["xmin"])
ymin <- as.numeric(bb["ymin"])
xmax <- as.numeric(bb["xmax"])
ymax <- as.numeric(bb["ymax"])


# ============================================================
# 16. FONCTION DE FILTRAGE
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
      as.character(.data[[variable]]) %in%
        selection
    )
}


# ============================================================
# 17. RAYON DES CERCLES
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
# 18. INTERFACE UI
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
          padding-left: 10px;
          padding-right: 10px;
        }

        #carte {
          width: 100%;
        }
        "
      )
      
    )
    
  ),
  
  
  titlePanel(
    
    div(
      style = "
        text-align:center;
        font-weight:bold;
      ",
      "Aires protégées du Sénégal"
    )
    
  ),
  
  
  sidebarLayout(
    
    
    # ========================================================
    # SIDEBAR
    # ========================================================
    
    sidebarPanel(
      
      
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
        
        selected = "polygons"
        
      )
      
    ),
    
    
    # ========================================================
    # ZONE PRINCIPALE
    # ========================================================
    
    mainPanel(
      
      leafletOutput(
        "carte",
        height = "650px"
      ),
      
      br(),
      
      plotOutput(
        "graphique",
        height = "380px"
      ),
      
      br(),
      
      tableOutput(
        "details"
      )
      
    )
    
  )
  
)


# ============================================================
# 19. SERVEUR
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
        
        "Variable à visualiser",
        
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
  # CLIC SUR LA CARTE
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
          as.numeric(
            id_click
          )
        
        
        updateSelectizeInput(
          
          session,
          
          "aire",
          
          selected =
            as.character(
              id_click
            )
          
        )
        
      }
      
    }
  )
  
  
  # ==========================================================
  # DONNEES ANNUELLES FILTREES
  # ==========================================================
  
  series_filtrees <- reactive({
    
    
    data <- base_series
    
    
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
    
    
    data
    
  })
  
  
  # ==========================================================
  # DONNEES POUR LA CARTE
  # ==========================================================
  
  donnees_carte <- reactive({
    
    
    req(
      input$variable_carte
    )
    
    
    # ========================================================
    # NOTHING = BASE 1
    # ========================================================
    
    if (
      input$periode ==
      "Nothing"
    ) {
      
      
      data <- base1
      
      
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
    
    
    # ========================================================
    # ALL OU ANNEE PRECISE
    # ========================================================
    
    serie <- series_filtrees()
    
    
    if (
      input$periode !=
      "ALL"
    ) {
      
      annee_selectionnee <-
        as.numeric(
          input$periode
        )
      
      
      serie <- serie %>%
        
        filter(
          Annee ==
            annee_selectionnee
        )
      
    }
    
    
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
    
    
    data <- base3 %>%
      
      left_join(
        reference_sites,
        by = "site_id"
      ) %>%
      
      left_join(
        valeurs,
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
        
        is.na(data$name_eng),
        
        as.character(
          data$site_id
        ),
        
        data$name_eng
        
      )
    
    
    # ========================================================
    # COULEURS - MODE NOTHING
    # ========================================================
    
    if (
      input$periode ==
      "Nothing"
    ) {
      
      
      valeurs <-
        data[[variable]]
      
      
      if (
        variable ==
        "realm_fr"
      ) {
        
        
        couleurs_realm <- c(
          
          "Terrestre" =
            "#2E7D32",
          
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
          is.na(data$couleur)
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
          pal(valeurs)
        
        
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
          pal(valeurs)
        
        
        type_legende <-
          "factor"
        
      }
      
      
      data$popup_txt <-
        paste0(
          
          "<b>",
          data$nom_affichage,
          "</b>",
          
          "<br><b>ID :</b> ",
          data$site_id,
          
          "<br><b>Milieu :</b> ",
          data$realm_fr,
          
          "<br><b>Désignation :</b> ",
          data$desig_eng,
          
          "<br><b>Catégorie UICN :</b> ",
          data$iucn_cat,
          
          "<br><b>Superficie :</b> ",
          data$gis_area
          
        )
      
      
      # ========================================================
      # COULEURS - MODE ALL / ANNEE
      # ========================================================
      
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
        pal(valeurs)
      
      
      type_legende <-
        "numeric"
      
      
      periode_txt <-
        ifelse(
          
          input$periode ==
            "ALL",
          
          "Moyenne de toutes les années",
          
          paste(
            "Année",
            input$periode
          )
          
        )
      
      
      data$popup_txt <-
        paste0(
          
          "<b>",
          data$nom_affichage,
          "</b>",
          
          "<br><b>ID :</b> ",
          data$site_id,
          
          "<br><b>",
          periode_txt,
          "</b>",
          
          "<br><b>Valeur :</b> ",
          format(
            round(
              data$valeur_carte
            ),
            big.mark = " ",
            scientific = FALSE
          ),
          " FCFA"
          
        )
      
    }
    
    
    # ========================================================
    # CREATION DE LA CARTE
    # ========================================================
    
    carte <- leaflet(
      
      options =
        leafletOptions(
          
          preferCanvas =
            TRUE,
          
          minZoom =
            5,
          
          maxZoom =
            18
          
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
      input$map_type ==
      "polygons"
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
      input$map_type ==
      "circles"
    ) {
      
      
      points <-
        suppressWarnings(
          st_point_on_surface(
            data
          )
        )
      
      
      if (
        input$periode ==
        "Nothing"
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
    # CONTOUR NATIONAL
    # ========================================================
    
    carte <- carte %>%
      
      addPolygons(
        
        data =
          senegal_contour,
        
        color =
          "#111111",
        
        weight =
          4,
        
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
    # LEGENDE
    # ========================================================
    
    if (
      type_legende ==
      "realm"
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
            "bottomright",
          
          colors =
            unname(
              couleurs_realm[
                presente
              ]
            ),
          
          labels =
            presente,
          
          title =
            "Aires protégées",
          
          opacity =
            1
          
        )
      
      
    } else {
      
      
      carte <- carte %>%
        
        addLegend(
          
          position =
            "bottomright",
          
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
    
    
    # ========================================================
    # CADRAGE
    # ========================================================
    
    carte %>%
      
      fitBounds(
        
        lng1 =
          xmin,
        
        lat1 =
          ymin,
        
        lng2 =
          xmax,
        
        lat2 =
          ymax
        
      )
    
  })
  
  
  # ==========================================================
  # GRAPHIQUE
  # ==========================================================
  
  output$graphique <- renderPlot({
    
    
    shiny::validate(
      
      shiny::need(
        input$periode !=
          "Nothing",
        "Choisissez ALL ou une année."
      ),
      
      shiny::need(
        !is.null(input$aire) &&
          input$aire != "",
        "Choisissez une aire ou cliquez sur la carte."
      ),
      
      shiny::need(
        length(
          input$variables_graph
        ) > 0,
        "Choisissez au moins une variable."
      )
      
    )
    
    
    # Conversion de l'ID choisi vers numérique
    
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
      input$periode !=
      "ALL"
    ) {
      
      
      annee_selectionnee <-
        as.numeric(
          input$periode
        )
      
      
      data <- data %>%
        
        filter(
          Annee ==
            annee_selectionnee
        )
      
    }
    
    
    shiny::validate(
      
      shiny::need(
        nrow(data) > 0,
        "Aucune donnée pour cette aire."
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
    
    
    # ========================================================
    # ALL = EVOLUTION DANS LE TEMPS
    # ========================================================
    
    if (
      input$periode ==
      "ALL"
    ) {
      
      
      ggplot(
        
        data_long,
        
        aes(
          x = Annee,
          y = Valeur,
          color = Indicateur,
          group = Indicateur
        )
        
      ) +
        
        geom_line(
          linewidth =
            1.1
        ) +
        
        geom_point(
          size =
            3
        ) +
        
        labs(
          
          title =
            paste(
              "Évolution des indicateurs -",
              nom_site
            ),
          
          x =
            "Année",
          
          y =
            "Montant (FCFA)",
          
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
            13
        )
      
      
      # ========================================================
      # ANNEE PRECISE
      # ========================================================
      
    } else {
      
      
      ggplot(
        
        data_long,
        
        aes(
          x = Indicateur,
          y = Valeur,
          fill = Indicateur
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
            "Montant (FCFA)"
          
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
            13
        ) +
        
        theme(
          
          axis.text.x =
            element_text(
              angle =
                35,
              hjust =
                1
            ),
          
          legend.position =
            "none"
          
        )
      
    }
    
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
    
    
    # ========================================================
    # NOTHING
    # ========================================================
    
    if (
      input$periode ==
      "Nothing"
    ) {
      
      
      return(
        
        reference_sites %>%
          
          filter(
            site_id ==
              id_selectionne
          )
        
      )
      
    }
    
    
    # ========================================================
    # ALL
    # ========================================================
    
    if (
      input$periode ==
      "ALL"
    ) {
      
      return(
        NULL
      )
      
    }
    
    
    # ========================================================
    # ANNEE PRECISE
    # ========================================================
    
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
            
            "site_id",
            "name_eng",
            "Annee",
            "Region",
            "Ecosysteme",
            "Gestionnaire",
            "Ramsar",
            "UNESCO",
            "PAG_a_jour",
            "Pression_ecologique",
            "Score_capacite",
            
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
# 20. LANCEMENT
# ============================================================

shinyApp(
  ui = ui,
  server = server
)


