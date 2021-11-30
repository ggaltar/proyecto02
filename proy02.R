# PAQUETES

library(dplyr)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(DT)
library(plotly)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(shiny)
library(shinydashboard)


# DATOS

# Capa de polígonos: zonas de conservación vial
zonas <-
  st_read(
    "https://raw.githubusercontent.com/ggaltar/danos_red_vial/main/capas/zonas_conservacion_wgs84.geojson",
    quiet = TRUE
  )

# Capa de líneas: red vial nacional
rutas <-
  st_read(
    "https://raw.githubusercontent.com/ggaltar/danos_red_vial/main/capas/red_vial_nacional_wgs84.geojson",
    quiet = TRUE
  ) 

#rutas <- st_transform(rutas_wgs84, crs = 5367)

# Capa de puntos: daños en la red vial nacional
danos <-
  st_read(
    "https://raw.githubusercontent.com/ggaltar/danos_red_vial/main/capas/danos_wgs84.geojson",
    quiet = TRUE
  )

# LISTAS PARA FILTROS

# Lista ordenada de estructuras + "Todas"
lista_estructuras <- unique(danos$estructura)
lista_estructuras <- sort(lista_estructuras)
lista_estructuras <- c("Todas", lista_estructuras)


# Lista ordenada de tipos de daño + "Todos"
lista_tipo <- unique(danos$tipo)
lista_tipo <- sort(lista_tipo)
lista_tipo <- c("Todos", lista_tipo)

# Lista ordenada de severidad + "Todas"
lista_severidad <- c("Todas", "Alta", "Media","Baja")

# Lista ordenada de zonas + "Todas"
lista_zonas <- unique(zonas$Zona2)
lista_zonas <- sort(lista_zonas)
lista_zonas <- c("Todas", lista_zonas)

# COMPONENTES DE LA APLICACIÓN SHINY

# Definición del objeto ui

ui <- dashboardPage(
  dashboardHeader(title = "Daños en la RVN"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      text = "Filtros",
      selectInput(
        inputId = "estructura",
        label = "Estructura",
        choices = lista_estructuras,
        selected = "Todas"
      ),
      selectInput(
        inputId = "tipo",
        label = "Tipo de daño",
        choices = lista_tipo,
        selected = "Todos"
      ),
      selectInput(
        inputId = "severidad",
        label = "Severidad",
        choices = lista_severidad,
        selected = "Todas"
      ),
      selectInput(
        inputId = "zona",
        label = "Zona",
        choices = lista_zonas,
        selected = "Todas"
      ),
      dateRangeInput(
        inputId = "fecha",
        label = "Fecha",
        start = "2015-01-01",
        end   = Sys.Date(),
        separator = " a ",
        language = "es"
      ),
      startExpanded = TRUE
    )
  )),
  dashboardBody(fluidRow(
      box(
        title = "Mapa de ubicación de daños",
        leafletOutput(outputId = "mapa"),
        width = 7
      ),
      box(
        title = "Registros de daños",
        DTOutput(outputId = "tabla"),
        width = 5
      ),      
    ),    
    fluidRow(
    box(
      title = "Elementos dañados",
      plotOutput(outputId = "grafico"),
      width = 12,
      collapsible = TRUE,
      collapsed = F,
    )
  ))
)

# Definición del objeto server

server <- function(input, output, session) {
  filtrarDanos <- reactive({
    # Remoción de geometrías y selección de columnas
    danos_filtrado <-
      danos %>%
      dplyr::select(estructura,elemento, tipo, severidad, descripcio, fecha_dano, cod_zona)
    
    # Filtrado de daños por estructura
    if (input$estructura != "Todas") {
      danos_filtrado <-
        danos_filtrado %>%
        filter(estructura == input$estructura)
    }   
    # Filtrado de daños por tipo
    if (input$tipo != "Todos") {
      danos_filtrado <-
        danos_filtrado %>%
        filter(tipo == input$tipo)
    }
    # Filtrado de daños por severidad
    if (input$severidad != "Todas") {
      danos_filtrado <-
        danos_filtrado %>%
        filter(severidad == input$severidad)
    }
    # Filtrado de daños por zona
    if (input$zona != "Todas") {
      zona <- zonas %>%
        filter(Zona2 == input$zona)
      danos_filtrado <-
        st_intersection(danos_filtrado,zona)
    }
    # Filtrado de daños por fecha
    danos_filtrado <-
      danos_filtrado %>%
      filter(
        fecha_dano >= as.Date(input$fecha[1], origin = "1970-01-01") &
          fecha_dano <= as.Date(input$fecha[2], origin = "1970-01-01")
      )
    return(danos_filtrado)
  })

    
  # Mapa ubicación de los daños
  
  output$mapa <- renderLeaflet({
    registros <- filtrarDanos()
    
    lista_colores <- c("#94d2bd","#94d2bd","#ee9b00","#ca6702","#bb3e03","#ae2012","#9b2226","#ef476f","#ffd166","#06d6a0","#118ab2","#073b4c","#495057")
    
    # Registro de daños, zonas de conservación y ráster de zonas de conservación por cantidad
    leaflet() %>%
      setView(-84.08, 9.83, 8) %>%
      addProviderTiles(providers$CartoDB.Voyager , group = "Voyager") %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
      addPolygons(
        data = zonas,
        color = "#38302e",
        fillColor = "transparent",
        stroke = TRUE,
        weight = 3,
        opacity = 1,
        group = "Zonas de conservación"
      ) %>%    
      addPolylines(
        data = rutas,
        color = "#788585",
        fill = FALSE,
        stroke = TRUE,
        weight = 1,
        opacity = 1,
        group = "Red vial nacional",
        label = paste0(
          "Ruta: ", rutas$ruta
        )
      ) %>%
      
      addCircleMarkers(
        data = registros,
        stroke = F,
        radius = 3,
        fillColor = '#6d597a',
        fillOpacity = 1,
        group = "Daños",
        label = paste0(
          "Estructura: ", registros$estructura,
          ", ",
          "Elemento: ", registros$elemento,
          ", ",
          "Tipo: ", registros$tipo
        ),
        popup = paste0(
          "<strong>Estructura: </strong>", registros$estructura,
          "<br>",
          "<strong>Elemento: </strong>", registros$elemento,
          "<br>",
          "<strong>Tipo de daño: </strong>", registros$tipo,
          "<br>",
          "<strong>Descripción: </strong>", registros$descripcio,
          "<br>",
          "<strong>Fecha del daño (año-mes-día): </strong>", registros$fecha_dano,
          "<br>",
          "<strong>Severidad: </strong>", registros$severidad
        )
      )  %>%
      
      addLayersControl(
        baseGroups = c("Voyager", "OSM", "Positron"),
        overlayGroups = c("Daños", "Red vial nacional", "Zonas de conservación"),
        options = layersControlOptions(collapsed = T)
      ) %>%
      
      hideGroup("Zonas de conservación 2") %>%
      addSearchOSM() %>%
      addResetMapButton() %>%
      addMouseCoordinates()
  })
  
  # Tabla de registro de daños
  
  output$tabla <- renderDT({
    registros <- filtrarDanos()
    registros %>%
      dplyr::select(estructura,elemento, tipo, severidad, fecha_dano) %>%
      st_drop_geometry() %>%
      
      datatable(rownames = FALSE,
                colnames = c('Estructura','Elemento', 'Tipo', 'Severidad','Fecha'),
                options = list(
                  pageLength = 7,
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                )
      )
  })
  
  # Gráfico de principales elementos dañados
  
  
  output$grafico <- renderPlot({
    # Preparación de los datos
    registros <- filtrarDanos()
    elementos <-
      registros %>%
      st_drop_geometry() %>%
      select(elemento) %>%
      rename(Elemento = elemento) %>%
      group_by(Elemento) %>%
      summarise(suma = n())
    
    
    ggplot(elementos, aes(x = reorder(Elemento, -suma),y = suma)) +
      geom_col(colour = "#6d597a", fill = "#6d597a",width = 0.5) +
      geom_text(aes(label = suma), vjust = 1.2, colour = "#93a8ac") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 25,hjust = 1, vjust = 1, size = 14)
      ) +
      xlab("") +
      ylab("Cantidad")
    
  })  
  
}

shinyApp(ui, server)




