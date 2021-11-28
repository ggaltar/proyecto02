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
      width = 12
    ),

    fluidRow(
      box(
        title = "Registros de daños",
        DTOutput(outputId = "tabla"),
        width = 7
      ),
      box(
        title = "Elementos dañados",
        plotOutput(outputId = "grafico"),
        width = 5
      ),      
    )    
  ))
)

# Definición del objeto server

server <- function(input, output, session) {
  filtrarDanos <- reactive({
    # Remoción de geometrías y selección de columnas
    danos_filtrado <-
      danos %>%
      dplyr::select(estructura,elemento, tipo, severidad, fecha_dano)
    
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

    # Registro de daños, zonas de conservación y ráster de zonas de conservación por cantidad
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager , group = "Voyager") %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
      setView(-84.10, 9.5, 7) %>%
      addPolygons(
        data = zonas,
        color = "#370617",
        fillColor = "transparent",
        stroke = TRUE,
        weight = 1.0,
        group = "Zonas de conservación"
      ) %>%
      addPolygons(
        data = rutas,
        color = "#370617",
        fill = FALSE,
        stroke = TRUE,
        weight = 1.0,
        group = "Red vial nacional"
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
          "Tipo: ", registros$tipo,
          ", ",
          "Severidad: ", registros$severidad
        ),
        popup = paste0(
          "<strong>Estructura: </strong>", registros$estructura,
          "<br>",
          "<strong>Elemento: </strong>", registros$elemento,
          "<br>",
          "<strong>Tipo de daño: </strong>", registros$tipo,
          "<br>",
          "<strong>Severidad: </strong>", registros$severidad
        )
      )  %>%
      addLayersControl(
        baseGroups = c("Voyager", "OSM", "Positron"),
        overlayGroups = c("Daños", "Zonas de conservación"),
        options = layersControlOptions(collapsed = T)
      ) %>%
      #hideGroup("Zonas por cantidad") %>%
      addSearchOSM() %>%
      addResetMapButton() %>%
      addMouseCoordinates()
  })
  
  # Tabla de registro de daños
  
  output$tabla <- renderDT({
    registros <- filtrarDanos()
    registros %>%
      st_drop_geometry() %>%
      
      datatable(rownames = FALSE,
                colnames = c('Estructura','Elemento', 'Tipo', 'Severidad','Fecha'),
                options = list(
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                )
      )
  })
  
  # Gráfico de principales elementos dañados
  
  lista_colores <- c("#001219","#005f73","#0a9396","#94d2bd","#94d2bd","#ee9b00","#ca6702","#bb3e03","#ae2012","#9b2226","#ef476f","#ffd166","#06d6a0","#118ab2","#073b4c","#495057")
  
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
      geom_text(aes(label = suma), vjust = 1.2, colour = "White") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 30,hjust = 1, vjust = 1)
      ) +
      xlab("Elementos") +
      ylab("Cantidad")
    
  })  
  
}

shinyApp(ui, server)




