source("packages.R")
source("CREACIONBASE.R")
source("tablas.R")




# Funciones de gráficos ---------------------------------------------------

#realicé estas funciones, de manera de estandarizar los gráficos, y llamarlas
#cuando la persona seleccione la región.

sh <- function(region, n){
  BASE %>%
    filter(RegionAB == region) %>% 
    filter(`Eje Ministerial` == "Seguridad hídrica" & Categoría == "Nuevo") %>%
    group_by(AGUA_DIPRES) %>%
    summarise(monto2025 = sum(`Monto 2025`, na.rm = TRUE)) %>%
    mutate(
      porcentaje = round((monto2025 / sum(monto2025)) * 100,n)
    )
}


tab_eje_ministerial <- function(region){
  BASE %>%
    filter(RegionAB == region) %>% 
    group_by(`Eje Ministerial`, Categoría) %>%
    summarise(Monto2025 = sum(`Monto 2025`), .groups = "drop") %>%
    ungroup() %>%  
    mutate(
      total_global = sum(Monto2025),  
      porcentaje = round(Monto2025 / total_global * 100,1) 
    )
}


CC <- function(region){
  BASE %>%
    filter(RegionAB == region) %>% 
    mutate(CambioClimatico = ifelse(is.na(CambioClimaticoA), "No Aplica", `Cambio Climatico`)) %>%
    group_by(CambioClimatico) %>%
    summarise(Monto2025 = sum(`Monto 2025`, na.rm = TRUE)) %>%
    mutate(
      porcentaje = Monto2025 / sum(Monto2025) * 100
    )
}



tabla_servicios <- function(region, servicios_agrupados,n) {
  
  total_monto_region <- BASE %>%
    filter(RegionAB == region) %>%
    summarise(TotalMonto2025 = sum(`Monto 2025`, na.rm = TRUE)) %>%
    pull(TotalMonto2025)
  
  SERVICIOS <- BASE %>%
    filter(RegionAB == region) %>%
    mutate(
      ServicioAgrupado = case_when(
        Servicio %in% servicios_agrupados ~ "OTROS SERVICIOS",
        TRUE ~ Servicio
      )
    ) %>%
    group_by(ServicioAgrupado) %>%
    summarise(Monto2025 = sum(`Monto 2025`, na.rm = TRUE)) %>%
    mutate(
      PorcentajeAgrupado = round(Monto2025 / total_monto_region * 100, n)  
    )
  
  
  otros_data <- BASE %>%
    filter(RegionAB == region) %>% 
    group_by(Servicio) %>%
    summarise(Monto = sum(`Monto 2025`, na.rm = TRUE)) %>%
    mutate(Porcentaje = round(Monto / sum(Monto) * 100,3)) %>% 
    filter(Servicio %in% servicios_agrupados) |> 
    mutate(Monto = scales::comma(Monto, decimal.mark = ",", big.mark = "."))
  
  
  return(list(
    servicios_agrupados = SERVICIOS,
    detalle_otros = otros_data
  ))
}


# CODIGO SHINY ------------------------------------------------------------

mapa_comunas <- chilemapas::mapa_comunas
mapa_regiones <- mapa_comunas |> 
  group_by(codigo_region) |> 
  summarize(geometry = st_union(geometry))




chile_regiones <- mapa_regiones %>% mutate(RegionAB = case_when(codigo_region == "01" ~ "TARAPACÁ",
                                                                codigo_region == "02" ~ "ANTOFAGASTA",
                                                                codigo_region == "03" ~ "ATACAMA",
                                                                codigo_region == "04" ~ "COQUIMBO",
                                                                codigo_region == "05" ~ "VALPARAÍSO",
                                                                codigo_region == "06" ~ "O'HIGGINS",
                                                                codigo_region == "07" ~ "MAULE",
                                                                codigo_region == "08" ~ "BIOBÍO",
                                                                codigo_region == "09" ~ "LA ARAUCANÍA",
                                                                codigo_region == "10" ~ "LOS LAGOS",
                                                                codigo_region == "11" ~ "AYSÉN",
                                                                codigo_region == "12" ~ "MAGALLANES",
                                                                codigo_region == "13" ~ "METROPOLITANA",
                                                                codigo_region == "14" ~ "LOS RÍOS",
                                                                codigo_region == "15" ~ "ARICA Y PARINACOTA",
                                                                codigo_region == "16" ~ "ÑUBLE"))


chile_regiones <- st_as_sf(chile_regiones)

chile_regiones <- left_join(chile_regiones, tab1, by = "RegionAB")

chile_regiones <- chile_regiones %>% mutate(color = ifelse(IncDec > 1,"darkblue","grey"))


chile_regiones <- st_transform(chile_regiones, crs = "+proj=longlat +datum=WGS84")

#addResourcePath("static", "www")



# ui ----------------------------------------------------------------------



ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: center; gap: 10px; color: #FFFFFF",
    img(src="mop1.jpeg", style="height: 70px; width: auto;"),
    h2("Ley de Presupuestos MOP 2025", style = "margin: 0; color: #FFFFFF"),
  ),
  
  theme = bs_theme(
    bootswatch = "flatly",
    version = 5,
    bg = "#FFFFFF",
    fg = "#003366",
    primary = "#003366",
    secondary = "#A0C4F0",
    success = "#FFFFFF",
    base_font = font_google("Roboto")
  ),
  
  tags$head(
    tags$style(HTML("
      /* Contenedor principal del navbar */
      header.navbar {
        flex-direction: column !important;
        align-items: flex-start !important;
        padding: 15px !important;
      }
      
      .navbar-collapse {
        justify-content: flex-end !important;
      }
      
      /* Opcional: Ajusta el espaciado entre pestañas */
      .nav-tabs .nav-link {
        margin-left: 15px !important;  /* Espacio entre pestañas */
      }
     
      
      /* Estilos para las pestañas (tabs) */
      .nav-tabs .nav-link {
        background-color: #003366 !important;
        color: white !important;
        border-radius: 5px 5px 0 0 !important;
        margin-right: 10px !important;
        border: 1px solid #A0C4F0 !important;
      }
      
      .nav-tabs .nav-link.active {
        background-color: #A0C4F0 !important;
        color: #003366 !important;
        font-weight: bold !important;
      }
    "))
  ),
  
  
  tabPanel(title = tags$span("Información Nacional", style = "font-size: 18px;"),
           fluidPage(
             actionButton("mi_boton", label = "Información importante"),  
             br(), br(),
             
             tabsetPanel(
               tabPanel("Figuras análisis nacional",
                        fluidPage(
                          theme = bs_theme(
                            version = 5,
                            bg = "#FFFFFF",
                            fg = "#003366",
                            primary = "#003366",
                            secondary = "#A0C4F0",
                            success = "#28A745",
                            base_font = font_google("Roboto")
                          ),
                          
                          tags$head(
                            tags$style(HTML("
    
    
#mi_boton {
  background-color: white !important;
  color: #003366 !important;
  border: 2px solid #A0C4F0 !important;
  border-radius: 5px !important;
  padding: 10px 20px !important;
  font-size: 16px !important;
  font-weight: bold !important;
  margin: 5px !important;
  float: right !important;  /* Alinea el botón a la derecha */
}
    
    
      
      /* Estilo para los enlaces de las pestañas */
      .nav-tabs .nav-link {
        background-color: #A0C4F0 !important;
        color: white !important; /* Texto en blanco */
        border-radius: 5px 5px 0 0 !important; /* Bordes redondeados solo arriba */
        margin-right: 2px !important; /* Espaciado entre pestañas */
        border: none !important;
      }
      
      /* pestaña activa */
      .nav-tabs .nav-link.active {
        background-color: #003366 !important; /* Fondo de la pestaña activa */
        color: white !important; /* Texto en blanco */
      }
      
      .card-footer {
        font-size: 10px !important; /* Tamaño de letra más pequeño */
        background-color: #f8f9fa !important;
         text-align: justify !important;      /* Texto justificado */
      text-justify: inter-word !important; /* Espaciado entre palabras */
      hyphens: manual !important;          /* Evita división automática de palabras */
      word-wrap: break-word !important;
      }
      
      .card-header {
      background-color: #FFFFFF !important; /* Fondo blanco */
      color: #003366 !important; /* Letras azules */
      font-size: 16px !important; /* Tamaño de letra más pequeño */
      font-weight: bold !important; /* Texto en negrita */
      padding: 8px 12px !important; /* Espaciado interno */
    }
                            
    "))
                          )
                          
                          ,br(), br(),
                          
                          layout_columns(
                            col_widths = c(4, 4, 4), 
                            
                            value_box(
                              title = "Total inversión 2025",
                              value = p("3.829.909.407.000"),
                              tags$small("Fuente: Departamento de Gestión Presupuestaria", style="font-size: 0.7rem;"),
                              theme = "primary",
                              width = "100%",
                              height = "100px"
                            ),
                            value_box(
                              title = "Proyección Población 2025",
                              "20.206.953 habitantes",
                              tags$small("Fuente: Proyección de población, INE", style="font-size: 0.7rem;"),
                              theme = "primary",
                              width = "100%",
                              height = "100px"
                            ),
                            value_box(
                              title = "Pobreza Multidimensional (2022)",
                              "3.368.245 personas",
                              tags$small("Fuente: Estimación de pobreza, casen 2022", style="font-size: 0.7rem;"),
                              theme = "primary",
                              width = "100%",
                              height = "100px"
                            ),
                            
                            
                            
                            card(
                              card_header("Seleccione una región en el mapa y luego presione la pestaña 'Región seleccionada' para mayor información"),
                              leafletOutput("mapa", height = "330px")
                            ),
                            
                            card(
                              card_header("Inversión a nivel nacional"),
                              plotlyOutput("grafico_nacional"),
                              style = "width: 100%; height: 450px;",
                              card_footer("Fuente: Departamento de Gestión Presupuestaria. Nota: La flecha (arriba/abajo) indica el cambio respecto al presupuesto 2024, con el factor de crecimiento o decrecimiento entre paréntesis.")
                            ),
                            
                            card(
                              card_header("Inversión por categoría del proyecto (arrastre o nuevo)"),
                              plotlyOutput("GrafRegCat"),
                              style = "width: 100%; height: 450px;",
                              card_footer("Fuente: Departamento de Gestión Presupuestaria. Nota: Los montos de inversión se encuentran en miles de pesos.")
                            ),
                            
                            card(
                              card_header("Inversión por Servicios MOP"),
                              plotlyOutput("graf_servicios"),
                              style = "width: 100%; height: 450px;",
                              card_footer("Fuente: Departamento de Gestión Presupuestaria. Nota 1: Los servicios incluídos en la categoría 'OTROS SERVICIOS' son DA, DGA, DP, INH y SISS. Nota 2: Los montos de inversión se encuentran en miles de pesos.")
                            ),
                            
                            card(
                              card_header("Inversión por categoría de cambio climático"),
                              plotlyOutput("CC_NAC"),
                              style = "width: 100%; height: 450px;",
                              card_footer("Fuente: Departamento de Gestión Presupuestaria. Nota: Los montos de inversión se encuentran en miles de pesos.")
                            ),
                            
                            card(
                              card_header("Inversión en nuevos proyectos pertenecientes al eje de Seguridad Hídrica"),
                              plotlyOutput("SHNAC"),
                              style = "width: 100%; height: 450px;",
                              card_footer("Fuente: Departamento de Gestión Presupuestaria. Nota 1: La categorización fue realizada en función de la variable 'Programa Dipres'. 'Consumo Humano' está compuesta por: agua potable rural semi-concentrada y agua potable rural concentrada.'Riego' agrupa: grandes obras de riego, conservación de obras de riego, explotación de obras de riego y obras de riego. La categoría 'Estudios y otros' reune: estudio básico, \nestudio y otros. Finalmente, 'Gestión' abarca: construcción de redes de medición, planes estratégicos de recursos hídricos y ampliación de redes de medición. Nota 2: Los montos de inversión se encuentran en miles de pesos.")
                            ),
                            
                            card(
                              card_header("Inversión por eje ministerial y por categoría del proyecto"),
                              plotlyOutput("grafico_eje_nac", height = "100%", width = "100%"),
                              style = "width: 100%; height: 500px; overflow: hidden;",
                              card_footer("Fuente: Departamento de Gestión Presupuestaria. Nota: Los montos de inversión se encuentran en miles de pesos.")
                            )
                          )
                        )
               ),
               
               tabPanel("Tabla análisis nacional",
                        fluidPage(br(), br(),
                                  layout_columns(
                                    col_widths = 12,  
                                    card(
                                      DTOutput("tabla_nacional"),
                                      card_footer("Fuente: Elaboración propia basada en datos del INE, del Departamento de Gestión Presupuestaria y CASEN 2022. (*) Población proyectada al 2025 según el INE. (**) Indica la cantidad de personas en situación de pobreza multidimensional para el año 2022. Nota: Los montos de inversión presentados no incluyen la participación de las regiones en proyectos de carácter interregional")
                                    )
                                  )
                        )
               )
             )
           )
  ),
  
  tabPanel(title = tags$span("📌 Región Seleccionada", style = "font-size: 18px;"),
           fluidPage(
             
             titlePanel(uiOutput("titulo_region")),
             br(), br(), 
             
             tabsetPanel(
               tabPanel("Figuras análisis regional", br(), br(),
                        layout_columns(
                          col_widths = c(4, 4, 4),  
                          
                          value_box(
                            title = "Total inversión regional 2025",
                            span(textOutput("inversion_text")),
                            tags$small("Fuente: Departamento de Gestión Presupuestaria", style="font-size: 0.7rem;"),
                            theme = "primary",
                            width = "100%",
                            height = "130px"
                          ),
                          
                          value_box(
                            title = "Proyección Población 2025",
                            span(textOutput("poblacion_text")),
                            tags$small("Fuente: Proyección de población, INE", style="font-size: 0.7rem;"),
                            theme = "primary",
                            width = "100%",
                            height = "130px"
                          ),
                          
                          value_box(
                            title = "Pobreza Multidimensional (2022)",
                            span(textOutput("pobreza_text")),
                            tags$small("Fuente: Estimación de pobreza, CASEN 2022", style="font-size: 0.7rem;"),
                            theme = "primary",
                            width = "100%",
                            height = "130px"
                          ),
                          
                          card(
                            card_header("Inversión 2025 por Servicios MOP"),
                            plotlyOutput("grafico_servicios"),
                            style = "width: 100%; height: 460px;",
                            card_footer("Fuente: Departamento de Gestión Presupuestaria. Nota: La categoría 'OTROS SERVICIOS' incluye distintos servicios del MOP, los cuales se detallan en la etiqueta")
                          ),
                          
                          card(
                            card_header("Inversión 2025 por categoría de cambio climático"),
                            plotlyOutput("grafico_cc"),
                            style = "width: 100%; height: 460px;",
                            card_footer("Fuente: Departamento de Gestión Presupuestaria. Nota: Los montos de inversión se encuentran en miles de pesos.")
                          ),
                          
                          card(
                            card_header("Inversión 2025 por eje ministerial y por categoría de proyecto"),
                            plotlyOutput("grafico_eje_ministerial"),
                            style = "width: 100%; height: 460px;",
                            card_footer("Fuente: Departamento de Gestión Presupuestaria. Nota: Los montos de inversión se encuentran en miles de pesos.")
                          ),
                          
                          card(
                            card_header("Inversión 2025 en nuevos proyectos pertenecientes al eje de Seguridad Hídrica"),
                            plotlyOutput("grafico_aguas"),
                            style = "width: 100%; height: 460px;",
                            card_footer("Fuente: Departamento de Gestión Presupuestaria. Nota 1: La categorización fue realizada en función de la variable 'Programa Dipres'. 'Consumo Humano' está compuesta por: agua potable rural semi-concentrada y agua potable rural concentrada.'Riego' agrupa: grandes obras de riego, conservación de obras de riego, explotación de obras de riego y obras de riego. La categoría 'Estudios y otros' reune: estudio básico, estudio y otros. Finalmente, 'Gestión' abarca: construcción de redes de medición, planes estratégicos de recursos hídricos y ampliación de redes de medición. Nota 2: Los montos de inversión se encuentran en miles de pesos.")
                          )
                        )
               ),
               
               tabPanel("Tabla análisis región",
                        fluidPage( br(), br(),
                                   layout_columns(
                                     col_widths = 12,  
                                     card(
                                       DTOutput("tabla_region"),
                                       card_footer("Fuente: Elaboración propia basada en datos del INE, del Departamento de Gestión Presupuestaria y CASEN 2022. (*) Población proyectada al 2025 según el INE. (**) Indica la cantidad de personas en situación de pobreza multidimensional para el año 2022. Nota: Los valores 'Por definir' en la columna Inversión corresponden a comunas que forman parte de proyectos intercomunales pero que no tienen un monto asociado.")
                                     )
                                   )
                        )
               )
             )
           )
  )
)




server <- function(input, output, session) {
  
  region_seleccionada <- reactiveVal(NULL)
  
  
  output$mapa <- renderLeaflet({
    leaflet(chile_regiones) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(
        fillColor = ~ color,
        color = "white",
        weight = 2,
        opacity = 0.4,
        fillOpacity = 0.6,
        highlight = highlightOptions(
          weight = 3,
          color = "white",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~RegionAB,
        layerId = ~RegionAB 
      ) %>%
      addLegend(
        position = "topright",
        colors = c("darkblue","grey"),
        labels = c("Incremento","Decremento"),
        title = "Región con Incremento/Decremento de la Inversión\ncon respecto al año 2024"
      )
    
  })
  
  
  
  
  
  
  observeEvent(input$mi_boton, {
    showModal(modalDialog(
      title = "Metodología",
      tags$ul(
        tags$li("La base de datos consolidada, se obtuvo a partir del cruce de diferentes bases de datos (CUT,PROYECTO DE LEY 2025, PROYECCIÓN DE POBLACIONES, POBLACIÓN CENSAL 2017  y POBREZA MULTIDIMENSIONAL)
            con el fin de normalizarla, se realizó un proceso de transformación de la variable correspondiente a cada servicio MOP, por lo que a cada uno de estos se le asignó la abreviación oficial (ej: Dirección de Planeamiento - DP),
             existen ervicios que tienen un porcentaje muy pequeño por lo que se recurrió a una agrupación en la categoría 'Otros', para mayor detalle de porcentajes y montos, se debe posar el cursor por sobre la categoría."),
        br(),
        tags$li("En la base de datos se encuentra la variable 'Programa dipres', que indica a cuál programa pertenecen los proyectos. Dada la relevancia del eje ministerial de 'Seguridad Hídrica', se exploraron los proyectos nuevos dentro de este eje. Por consiguiente, se filtra la base por proyectos nuevos que pertenezcan al eje de 'Seguridad Hídrica'.
             Se categorizaron las instancias en cuatro categorías: Riego, Gestión, Consumo humano y Estudios y otros.  La primera es 'Consumo Humano' que está \ncompuesta por los programas pertenecientes a: agua potable rural semi-concentrada y agua potable rural concentrada. \nLa segunda es 'Riego' que agrupa: grandes obras de riego, conservación de obras de riego, explotación de obras de riego \ny obras de riego. La categoría 'Estudios y otros' reune los siguientes programas asociados a la Dipres: estudio básico, \nestudio y otros. Finalmente, la categoría 'Gestión' abarca: construcción de redes de medición, planes estratégicos de \nrecursos hídricos y ampliación de redes de medición.")),
      
      tags$h5("Bibliografía"),
      tags$ul(
        tags$li("1. Número de regiones, provincias y comunas fueron recuperados de: Planilla Códigos Únicos Territoriales (CUT), 2018. Subsecretaría de Desarrollo Regional y Administrativo (SUBDERE). Recuperado a partir de: https://www.subdere.gov.cl/documentacion/c%C3%B3digos-%C3%BAnicos-territoriales-actualizados-al-06-de-septiembre-2018"),
        br(),
        tags$li("2. Proyección de población, INE. Recuperado a partir de: https://www.ine.gob.cl//estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion"),
        br(),
        tags$li("3. Estimación de pobreza, casen 2022. Recuperado a partir de: https://observatorio.ministeriodesarrollosocial.gob.cl/pobreza-comunal-2022"),
        br(),
        tags$li("4. Censo 2017, INE. Recuperado a partir de: https://www.ine.gob.cl/estadisticas/sociales/censos-de-poblacion-y-vivienda/censo-de-poblacion-y-vivienda"),
        br(),
        tags$li("5. Departamento de Gestión Presupuestaria")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$mapa_shape_click, {
    req(input$mapa_shape_click)  
    region <- input$mapa_shape_click$id
    region_seleccionada(region)
    
    updateNavbarPage(session, "Proyecto de Ley 2025", selected = "📌 Región seleccionada")
  })
  
  
  
  output$titulo_region <- renderUI({
    if (!is.null(region_seleccionada())) {
      tags$span(paste(region_seleccionada()), style = "font-size: 20px;") 
    } else {
      tags$span("Seleccione una región en el mapa")
    }
  })
  
  
  
  output$titulo_region <- renderUI({
    if (!is.null(region_seleccionada())) {
      ifelse(region_seleccionada() == "ARICA Y PARINACOTA", paste("Estadísticas de la región de Arica y Parinacota"),
             ifelse(region_seleccionada() == "TARAPACÁ", paste("Estadísticas de la región de Tarapacá"),
                    ifelse(region_seleccionada() == "ATACAMA", paste("Estadísticas de la región de Atacama"),
                           ifelse(region_seleccionada() == "ANTOFAGASTA", paste("Estadísticas de la región de Antofagasta"),
                                  ifelse(region_seleccionada() == "COQUIMBO", paste("Estadísticas de la región de Coquimbo"),
                                         ifelse(region_seleccionada() == "VALPARAÍSO", paste("Estadísticas de la región de Valparaíso"),
                                                ifelse(region_seleccionada() == "METROPOLITANA", paste("Estadísticas de la región Metropolitana de Santiago"),
                                                       ifelse(region_seleccionada() == "O'HIGGINS", paste("Estadísticas de la región del Libertador Bernardo O'Higgins"),
                                                              ifelse(region_seleccionada() == "MAULE", paste("Estadísticas de la región del Maule"),
                                                                     ifelse(region_seleccionada() == "ÑUBLE", paste("Estadísticas de la región del Ñuble"),
                                                                            ifelse(region_seleccionada() == "BIOBÍO", paste("Estadísticas de la región del Biobío"),
                                                                                   ifelse(region_seleccionada() == "ARAUCANÍA", paste("Estadísticas de la región de La Araucanía"),
                                                                                          ifelse(region_seleccionada() == "LOS RÍOS", paste("Estadísticas de la región de Los Ríos"),
                                                                                                 ifelse(region_seleccionada() == "LOS LAGOS", paste("Estadísticas de la región de Los Lagos"),
                                                                                                        ifelse(region_seleccionada() == "AYSÉN", paste("Estadísticas de la región de Aysén"),
                                                                                                               ifelse(region_seleccionada() == "MAGALLANES", paste("Estadísticas de la región de Magallanes"), ""))))))))))))))))
    } else {
      tags$span("Seleccione una región en el mapa")
    }
  })
  
  
  output$inversion_text <- renderText({
    req(region_seleccionada())
    a <- tab1 |> filter(RegionAB == region_seleccionada()) |> pull(`Monto 2025`)
    paste(format(a, big.mark = ".", decimal.mark = ","), "(miles de pesos)")
  })
  
  output$poblacion_text <- renderText({
    req(region_seleccionada())
    a <- tab_resumen_nac |> filter(RegionAB == region_seleccionada()) |> pull(`Población 2025`)
    paste(format(a, big.mark = ".", decimal.mark = ","), "habitantes")
  })
  
  output$pobreza_text <- renderText({
    req(region_seleccionada())
    a <- tab_resumen_nac |> filter(RegionAB == region_seleccionada()) |> pull(`Pobreza Multidimensional`)
    paste(format(a, big.mark = ".", decimal.mark = ","), "personas")
  })
  
  
  
  
  
  # Información Regional ----------------------------------------------------
  
  
  
  
  
  servicios_filtrados <- reactive({
    req(region_seleccionada())  
    servicios_por_region |> 
      filter(Region == region_seleccionada())  |> 
      pull(Servicios) |> 
      unlist()
  })
  
  
  
  n_filtrado <- reactive({
    req(region_seleccionada())  
    servicios_por_region |> 
      filter(Region == region_seleccionada())  |> 
      pull(n) |> 
      unlist()
  })  
  
  
  
  output$grafico_servicios <- renderPlotly({
    req(region_seleccionada())  
    
    servicios <- servicios_filtrados()
    n <- n_filtrado()
    
    resultados <- tabla_servicios(region_seleccionada(), servicios,n)
    
    p <- ggplot(resultados$servicios_agrupados, aes(x = reorder(ServicioAgrupado, -Monto2025), y = Monto2025, fill = ServicioAgrupado, text = ifelse(ServicioAgrupado == "OTROS SERVICIOS",
                                                                                                                                                     paste0(
                                                                                                                                                       "<b>Inversión 2025:</b>", scales::comma(Monto2025,  big.mark = ".", decimal.mark = ","), "<br>",
                                                                                                                                                       "<b>Porcentaje:</b>", format(PorcentajeAgrupado, nsmall = 1, decimal.mark = ","), "%", "<br>",
                                                                                                                                                       "<b>Incluye:</b><br>", 
                                                                                                                                                       paste0(resultados$detalle_otros$Servicio,": ", 
                                                                                                                                                              format(resultados$detalle_otros$Porcentaje, nsmall = 1, decimal.mark = ","),
                                                                                                                                                              "%"," - " ,resultados$detalle_otros$Monto ,sep = " ", collapse = "<br>")
                                                                                                                                                     ),
                                                                                                                                                     paste0("Inversión 2025: ", scales::comma(Monto2025,  big.mark = ".", decimal.mark = ","), "<br>",
                                                                                                                                                            "Porcentaje: ", format(PorcentajeAgrupado, nsmall = 1, decimal.mark = ","), "%")))) +
      geom_bar(stat = "identity", width = 0.6)  +
      labs(title = "",
           x = "", y = "") +
      scale_fill_manual(values = colores_servicios) +
      scale_y_continuous(limits = c(0, max(resultados$servicios_agrupados$Monto2025) * 1.1), 
                         labels = function(x) {
                           paste0(format(round(x/1e6), big.mark = ".", decimal.mark = ",", scientific = FALSE), "M")
                         }) +
      theme_minimal() +
      theme(legend.position = "none",panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(), 
            plot.title = element_text(size = 10, margin = margin(b = 20)),
            axis.text.y = element_text(size=8),
            axis.text.x = element_text(size=8)) + 
      coord_flip()
    
    ggplotly(p, tooltip = "text") %>% 
      layout(
        hoverlabel = list(
          bgcolor = "rgba(255,255,255,0.5)" , 
          font = list(color = "black", size = 12)
        )
      ) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$grafico_cc <- renderPlotly({
    datos_cc <- CC(region_seleccionada())
    
    if (is.null(datos_cc) || nrow(datos_cc) == 0) {
      return(NULL) 
    }
    
    plot_ly(
      data = datos_cc, 
      labels = ~CambioClimatico, 
      values = ~Monto2025,  
      type = "pie",
      hole = 0.5,
      textinfo = "percent",  
      hoverinfo = "label+percent+value",  
      marker = list(colors =c("#155E95" , "#98D8EF", "#4C7B8B", "#F4EDD3"), 
                    line = list(color = "white", width = 1))
    ) %>%
      layout(
        font = list(size = 10),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5, y = -0.2,
          xanchor = "center",
          yanchor = "top",
          traceorder = "grouped",  
          itemwidth = 100,  
          font = list(size = 10),
          bgcolor = "rgba(255,255,255,0.7)"
        ),
        yaxis = list(tickformat = ".2f"), 
        xaxis = list(tickformat = ".2f")) %>% 
      config(displayModeBar = FALSE)
  })
  
  
  n_aguas <- reactive({
    req(region_seleccionada())
    print(paste("Región seleccionada:", region_seleccionada()))  # Depuración
    aguas |> 
      filter(Region == region_seleccionada()) |> 
      pull(n) |> 
      as.numeric()
  })
  
  
  output$grafico_aguas <- renderPlotly({
    
    req(region_seleccionada())  
    
    n <- n_aguas()
    
    sh_reg <- sh(region_seleccionada(), n)  
    
    
    if(nrow(sh_reg) == 0) return(NULL)
    
    texto_hover <- paste0(sh_reg$AGUA_DIPRES, "<br>",
                          "Porcentaje: ", format(sh_reg$porcentaje, nsmall = 1, decimal.mark = ","), "%", "<br>",
                          "Inversión 2025: ", scales::comma(sh_reg$monto2025, big.mark = ".", decimal.mark = ","))
    
    
    
    p <- ggplot(sh_reg, aes(
      x = reorder(AGUA_DIPRES, porcentaje), 
      y = porcentaje,
      text = texto_hover
    )) +
      geom_segment(aes(xend = AGUA_DIPRES, y = 0, yend = porcentaje), color = "gray") + 
      geom_point(aes(size = porcentaje), color = "skyblue", fill = "white", shape = 21, stroke = 3) +
      coord_flip() + 
      labs(
        title = "",
        x = NULL,
        y = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 9, hjust = 1, margin = margin(b = 20))
      ) +
      expand_limits(y = c(0, max(sh_reg$porcentaje, na.rm = TRUE) + 5)) +
      scale_y_continuous(labels = scales::percent_format(scale = 1, decimal.mark = ","))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "rgba(255,255,255,0.8)", 
          font = list(color = "black", size = 12)  
        )
      ) %>% 
      config(displayModeBar = FALSE)
  })
  
  
  
  output$grafico_eje_ministerial <- renderPlotly({
    
    p <- ggplot(tab_eje_ministerial(region_seleccionada()), aes(x = `Eje Ministerial`, y = Monto2025, fill = `Eje Ministerial`)) +
      facet_wrap(~ Categoría, labeller = as_labeller(c("Arrastre" = "Proyectos de Arrastre", "Nuevo" = "Proyectos Nuevos"))) +
      labs(
        title = "", 
        x = "", 
        y = "", 
        fill = "",
        caption = ""
      ) + 
      geom_bar(stat = "identity", width = 0.6) + 
      scale_fill_manual(values = colores) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        panel.spacing = unit(0.8, "lines"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.0005),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray90"),
        legend.text = element_text(size = 7.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = c(0.9, 0),
        legend.spacing.x = unit(0.05, "cm"),  
        legend.spacing.y = unit(0.02, "cm"),
        legend.box = "horizontal", 
        legend.key.size = unit(0.4, "cm"),
        legend.box.spacing = unit(0.5, "cm"), 
        plot.margin = margin(t = 10, r = 0, b = 50, l = 0),
        plot.title = element_text(size = 9, hjust = 0.5)) +
      guides(fill = guide_legend(nrow = 3, byrow = TRUE)) + 
      scale_y_continuous(
        labels = label_number(scale = 1e-6, suffix = "M"),
        expand = expansion(mult = c(0, 0.3)) 
      ) 
    
    
    p <- p + aes(text = paste(
      "Categoría:", Categoría, "<br>",
      "Inversión 2025:", scales::comma(Monto2025, big.mark = ".", decimal.mark = ","), "<br>",
      "Porcentaje:", scales::comma(porcentaje, big.mark = ".", decimal.mark = ","), "%"
    ))
    
    
    ggplotly(p, tooltip = "text") %>% 
      layout(
        hoverlabel = list(
          bgcolor = "rgba(255,255,255,0.5)", 
          font = list(color = "black", size = 12)  
        ),
        legend = list(
          orientation = "h",   
          x = 0.5,             
          y = -0.2,            
          xanchor = "center",  
          yanchor = "top")
      ) %>% 
      config(displayModeBar = FALSE)
    
    
  })
  
  
  
  
  
  
  
  
  output$tabla_region <- renderDT({
    req(region_seleccionada())  
    
    tab_resumen <- left_join(tab_poblacion(region_seleccionada()), tab_monto(region_seleccionada()), by = "NombreComuna") 
    
    if (region_seleccionada() == "METROPOLITANA") {
      tab_resumen <- tab_resumen_RM %>%
        mutate(
          `Población 2017` = scales::comma(`Población 2017`, big.mark = ".", decimal.mark = ","),
          `Población 2025` = scales::comma(`Población 2025`, big.mark = ".", decimal.mark = ","),
          Monto2025 = ifelse(is.na(Monto2025), "Por definir", scales::comma(Monto2025, big.mark = ".", decimal.mark = ","))
        ) |> 
        mutate(NombreComuna = case_when(
          NombreComuna == "ALHUE" ~ "ALHUÉ",
          NombreComuna == "CONCHALI" ~ "CONCHALÍ",
          NombreComuna == "CURACAVI" ~ "CURACAVÍ",
          NombreComuna == "ESTACION CENTRAL" ~ "ESTACIÓN CENTRAL",
          NombreComuna == "MACUL" ~ "MACÚL",
          NombreComuna == "MAIPU" ~ "MAIPÚ",
          NombreComuna == "MARIA PINTO" ~ "MARÍA PINTO",
          NombreComuna == "PEÑALOLEN" ~ "PEÑALOLÉN",
          NombreComuna == "SAN JOAQUIN" ~ "SAN JOAQUÍN",
          NombreComuna == "SAN JOSE DE MAIPO" ~ "SAN JOSÉ DE MAIPO",
          NombreComuna == "SAN RAMON" ~ "SAN RAMÓN",
          TRUE ~ NombreComuna
        )) |> 
        rename(
          `Inversión` = `Monto2025`,
          `Población 2025 (*)` = `Población 2025`,
          `Comuna` = `NombreComuna`,
          `Pobreza multidimensional (**)` = `Pobreza Multidimensional`,
        )
    } else if (region_seleccionada() == "BIOBÍO") {
      tab_resumen <- tab_resumen_BIOBIO  |> 
        mutate(
          Monto2025 = if_else(NombreComuna == "PICA", NA_real_, Monto2025),
          `Población 2017` = scales::comma(`Población 2017`, big.mark = ".", decimal.mark = ","),
          `Población 2025` = scales::comma(`Población 2025`, big.mark = ".", decimal.mark = ","),
          Monto2025 = ifelse(is.na(Monto2025), "Por definir", scales::comma(Monto2025, big.mark = ".", decimal.mark = ","))
        ) |> 
        mutate(NombreComuna = case_when(
          NombreComuna == "ALTO BIOBIO" ~ "ALTO BIOBÍO",
          NombreComuna == "CONCEPCION" ~ "CONCEPCIÓN",
          NombreComuna == "HUALPEN" ~ "HUALPÉN",
          NombreComuna == "LOS ALAMOS" ~ "LOS ÁLAMOS",
          NombreComuna == "LOS ANGELES" ~ "LOS ÁNGELES",
          NombreComuna == "MULCHEN" ~ "MULCHÉN",
          NombreComuna == "SANTA BARBARA" ~ "SANTA BÁRBARA",
          NombreComuna == "TIRUA" ~ "TIRÚA",
          NombreComuna == "TOME" ~ "TOMÉ",
          TRUE ~ NombreComuna
        )) |> 
        rename(
          `Inversión` = `Monto2025`,
          `Población 2025 (*)` = `Población 2025`,
          `Comuna` = `NombreComuna`,
          `Pobreza multidimensional (**)` = `Pobreza Multidimensional`,
        )
    } else {
      tab_resumen <- tab_resumen  |> 
        mutate(
          Monto2025 = if_else(NombreComuna == "PICA", NA_real_, Monto2025),
          `Población 2017` = scales::comma(`Población 2017`, big.mark = ".", decimal.mark = ","),
          `Población 2025` = scales::comma(`Población 2025`, big.mark = ".", decimal.mark = ","),
          Monto2025 = ifelse(is.na(Monto2025), "Por definir", scales::comma(Monto2025, big.mark = ".", decimal.mark = ","))
        )  |> 
        mutate(NombreComuna = case_when(NombreComuna == "MARIA ELENA" ~ "MARÍA ELENA", NombreComuna == "OLLAGUE" ~ "OLLAGÜE",
                                        NombreComuna == "COPIAPO" ~ "COPIAPÓ", NombreComuna == "COMBARBALA" ~ "COMBARBALÁ", NombreComuna == "PAIGUANO" ~ "PAIHUANO", NombreComuna == "RIO HURTADO" ~ "RÍO HURTADO", 
                                        NombreComuna == "CONCON" ~ "CONCÓN", NombreComuna == "JUAN FERNANDEZ" ~ "JUAN FERNÁNDEZ", NombreComuna == " OLMUE" ~ "OLMUÉ", NombreComuna == "PUCHUNCAVI" ~ "PUCHUNCAVÍ",
                                        NombreComuna == "QUILPUE" ~ "QUILPUÉ", NombreComuna == "SANTA MARIA" ~ "SANTA MARÍA",
                                        NombreComuna == "VALPARAISO" ~ "VALPARAÍSO", NombreComuna == "MACHALI" ~ "MACHALÍ",
                                        NombreComuna == "CHEPICA" ~ "CHÉPICA",
                                        NombreComuna == "REQUINOA" ~ "REQUÍNOA",NombreComuna == "COLBUN" ~ "COLBÚN",
                                        NombreComuna == "CONSTITUCION" ~ "CONSTITUCIÓN",
                                        NombreComuna == "CURICO" ~ "CURICÓ",
                                        NombreComuna == "HUALAÑE" ~ "HUALAÑÉ",
                                        NombreComuna == "LICANTEN" ~ "LICANTÉN",
                                        NombreComuna == "LONGAVI" ~ "LONGAVÍ",
                                        NombreComuna == "RIO CLARO" ~ "RÍO CLARO",
                                        NombreComuna == "VICHUQUEN" ~ "VICHUQUÉN",NombreComuna == "CHILLAN" ~ "CHILLÁN",
                                        NombreComuna == "CHILLAN VIEJO" ~ "CHILLÁN VIEJO",
                                        NombreComuna == "QUILLON" ~ "QUILLÓN",
                                        NombreComuna == "RANQUIL" ~ "RÁNQUIL",
                                        NombreComuna == "SAN FABIAN" ~ "SAN FABIÁN",
                                        NombreComuna == "SAN NICOLAS" ~ "SAN NICOLÁS",
                                        NombreComuna == "ÑIQUEN" ~ "ÑIQUÉN",NombreComuna == "CURACAUTIN" ~ "CURACAUTÍN",
                                        NombreComuna == "PITRUFQUEN" ~ "PITRUFQUÉN",
                                        NombreComuna == "PUCON" ~ "PUCÓN",
                                        NombreComuna == "PUREN" ~ "PURÉN",
                                        NombreComuna == "TOLTEN" ~ "TOLTÉN",
                                        NombreComuna == "TRAIGUEN" ~ "TRAIGUÉN",
                                        NombreComuna == "VILCUN" ~ "VILCÚN",
                                        NombreComuna == "LA UNION" ~ "LA UNIÓN",
                                        NombreComuna == "MAFIL" ~ "MÁFIL",
                                        NombreComuna == "RIO BUENO" ~ "RÍO BUENO",
                                        NombreComuna == "ANCUD" ~ "ANCÚD",
                                        NombreComuna == "CHAITEN" ~ "CHAITÉN",
                                        NombreComuna == "CURACO DE VELEZ" ~ "CURACO DE VÉLEZ",
                                        NombreComuna == "FUTALEUFU" ~ "FUTALEUFÚ",
                                        NombreComuna == "HUALAIHUE" ~ "HUALAIHUÉ",
                                        NombreComuna == "MAULLIN" ~ "MAULLÍN",
                                        NombreComuna == "PUQUELDON" ~ "PUQUELDÓN",
                                        NombreComuna == "QUEILEN" ~ "QUEILÉN",
                                        NombreComuna == "QUELLON" ~ "QUELLÓN",
                                        NombreComuna == "RIO NEGRO" ~ "RÍO NEGRO",NombreComuna == "AYSEN" ~ "AYSÉN",
                                        NombreComuna == "RIO IBAÑEZ" ~ "RÍO IBAÑEZ",
                                        NombreComuna == "ANTARTICA" ~ "ANTÁRTICA",
                                        NombreComuna == "RIO VERDE" ~ "RÍO VERDE", TRUE ~ NombreComuna
        )) |> 
        
        rename(
          `Inversión` = `Monto2025`,
          `Población 2025 (*)` = `Población 2025`,
          `Comuna` = `NombreComuna`,
          `Pobreza multidimensional (**)` = `Pobreza Multidimensional`,
        )
    }
    
    tab_resumen  |>   
      datatable(
        options = list(
          autoWidth = FALSE,
          scrollX = TRUE,
          columnDefs = list(
            list(width = '100px', targets = 0),
            list(width = '150px', targets = 1),
            list(width = '200px', targets = 2)
          )
        )
      )
  })
  
  
  
  
  # Nacional ----------------------------------------------------------------
  
  
  
  output$grafico_nacional <- renderPlotly({
    
    p <- ggplot(tab1, aes(y = RegionAB, x = `Monto 2025`, text = Label)) +  
      geom_bar(stat = "identity", fill = "darkblue", width = 0.6) +
      theme_minimal() +
      scale_x_continuous(
        labels = scales::label_number(scale = 1e-6, suffix = "M"),
        limits = c(0, 536729160),
        breaks = seq(0, 506729160, by = 100000000)
      ) +
      labs(x = "", y = "", title = "") +
      theme(panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank())
    
    ggplotly(p, tooltip = "text") %>% 
      layout(
        hoverlabel = list(
          bgcolor = "rgba(255,255,255,0.5)",
          font = list(color = "black", size = 11, family = "Arial Black", weight = "bold")
        )
      ) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  
  
  
  
  
  
  
  
  output$graf_servicios <- renderPlotly({
    
    p <- ggplot(SERVICIOS_NAC , aes(x = reorder(SERVICIOS_A, -Monto2025), y = Monto2025, fill = SERVICIOS_A, text = ifelse(SERVICIOS_A == "OTROS SERVICIOS",
                                                                                                                           paste(
                                                                                                                             "<b>Inversión 2025:</b>", scales::comma(Monto2025, big.mark = ".", decimal.mark = ","), "<br>",
                                                                                                                             "<b>Porcentaje:</b>", format(round(Porcentaje, 1), nsmall = 1, decimal.mark = ","), "%", "<br>",
                                                                                                                             "<b>Incluye:</b><br>", 
                                                                                                                             paste(otros_data$Servicio,":", 
                                                                                                                                   format(otros_data$Porcentaje, nsmall = 1, decimal.mark = ","),
                                                                                                                                   "%", sep = " ", collapse = "<br>")
                                                                                                                           ),
                                                                                                                           paste("Inversión 2025:", scales::comma(Monto2025, big.mark = ".", decimal.mark = ","), "<br>",
                                                                                                                                 "Porcentaje:", format(round(Porcentaje, 1), nsmall = 1, decimal.mark = ","), "%"))
    ))+
      geom_bar(stat = "identity", width = 0.6) +
      labs(title = "",
           x = "",  y = "") +
      scale_fill_manual(values = colores_servicios) +
      scale_y_continuous(limits = c(0, max(SERVICIOS_NAC$Monto2025) * 1.1), labels = function(x) {
        paste0(format(round(x/1e6), big.mark = ".", decimal.mark = ",", scientific = FALSE), "M")
      }) +
      theme_minimal()+
      theme(legend.position = "none",
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size=8),
            axis.text.x = element_text(size=8))+
      coord_flip()
    
    
    ggplotly(p, tooltip = "text") %>% 
      layout(
        hoverlabel = list(
          bgcolor = "rgba(255,255,255,0.5)", 
          font = list(color = "black", size = 12)  
        )
      )%>% 
      config(displayModeBar = FALSE)})
  
  
  
  output$CC_NAC <- renderPlotly({
    plot_ly(
      data = CC_NA_OTROS, 
      labels = ~CambioClimatico, 
      values = ~Monto2025,  
      type = "pie",
      hole = 0.5,
      
      textinfo = "percent",  
      hoverinfo = "label+percent+value",  
      
      marker = list(colors = c("#155E95" , "#98D8EF", "#4C7B8B", "#F4EDD3"), 
                    line = list(color = "white", width = 1))
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5, y = -0.2,
          xanchor = "center",
          yanchor = "top",
          traceorder = "grouped",  
          itemwidth = 100,  
          font = list(size = 9),
          bgcolor = "rgba(255,255,255,0.7)"
        ),
        
        
        yaxis = list(tickformat = ".2f"), 
        xaxis = list(tickformat = ".2f")
      ) %>% 
      config(displayModeBar = FALSE)
  })
  
  
  output$grafico_eje_nac <- renderPlotly({
    
    p <- ggplot(tab_eje_ministerial_cat_nac, aes(x = `Eje Ministerial`, y = Monto2025, fill = `Eje Ministerial`)) +
      facet_wrap(~ Categoría, labeller = as_labeller(c("Arrastre" = "Proyectos de Arrastre", "Nuevo" = "Proyectos Nuevos"))) +
      labs(
        title = "", 
        x = "", 
        y = "", 
        fill = "",
        caption = ""
      ) + 
      geom_bar(stat = "identity", width = 0.6) + 
      scale_fill_manual(values = colores) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        panel.spacing = unit(0.8, "lines"), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.0005),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray90"),
        legend.text = element_text(size = 7.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = c(0.9, 0),
        legend.spacing.x = unit(0.05, "cm"),  
        legend.spacing.y = unit(0.02, "cm"),
        legend.box = "horizontal", 
        legend.key.size = unit(0.4, "cm"),
        legend.box.spacing = unit(0.5, "cm"), 
        plot.margin = margin(t = 10, r = 0, b = 50, l = 0),
        plot.title = element_text(size = 9, hjust = 0.5)) +
      guides(fill = guide_legend(nrow = 3, byrow = TRUE)) + 
      scale_y_continuous(
        labels = function(x) {
          paste0(format(round(x/1e6), big.mark = ".", decimal.mark = ",", scientific = FALSE), "M")
        },
        expand = expansion(mult = c(0, 0.3)) 
      ) 
    
    
    p <- p + aes(text = paste(
      "Categoría:", Categoría, "<br>",
      "Inversión 2025:", scales::comma(Monto2025, big.mark = ".", decimal.mark = ","), "<br>",
      "Porcentaje:", scales::comma(porcentaje, big.mark = ".", decimal.mark = ","), "%"
    ))
    
    
    ggplotly(p, tooltip = "text") %>% 
      layout(autosize = TRUE,
             hoverlabel = list(
               bgcolor = "rgba(255,255,255,0.5)", 
               font = list(color = "black", size = 12)  
             ),
             legend = list(
               orientation = "h",   
               x = 0.5,             
               y = -0.2,            
               xanchor = "center",  
               yanchor = "top")
      ) %>% 
      config(displayModeBar = FALSE)
    
    
  })
  
  
  output$SHNAC <- renderPlotly({
    
    texto_hover <- paste0(SH_NAC$AGUA_DIPRES, "<br>",
                          "Porcentaje: ", format(SH_NAC$porcentaje, nsmall = 1, decimal.mark = ","), "%", "<br>",
                          "Inversión 2025: ", scales::comma(SH_NAC$monto2025, big.mark = ".", decimal.mark = ","))
    
    p <- ggplot(SH_NAC, aes(
      x = reorder(AGUA_DIPRES, porcentaje), 
      y = porcentaje,
      text = texto_hover
    )) +
      geom_segment(aes(xend = AGUA_DIPRES, y = 0, yend = porcentaje), color = "gray") + 
      geom_point(aes(size = porcentaje), color = "skyblue", fill = "white", shape = 21, stroke = 3) +
      coord_flip() + 
      labs(
        title = "",
        x = NULL,
        y = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 9, hjust = 1, margin = margin(b = 20))
      ) +
      expand_limits(y = c(0, max(SH_NAC$porcentaje) + 5)) +
      scale_y_continuous(labels = scales::percent_format(scale = 1, decimal.mark = ","))
    
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "rgba(255,255,255,0.8)", 
          font = list(color = "black", size = 12)  
        )
      ) %>% 
      config(displayModeBar = FALSE)
  })
  
  
  output$tabla_nacional <- renderDT({
    tab_resumen_nac %>%
      mutate(
        `Población 2017` = scales::comma(as.numeric(`Población 2017`), big.mark = ".", decimal.mark = ","),
        `Población 2025` = scales::comma(as.numeric(`Población 2025`), big.mark = ".", decimal.mark = ","),
        Monto2025 = scales::comma(as.numeric(Monto2025), big.mark = ".", decimal.mark = ","),
        `Pobreza Multidimensional` = replace_na(`Pobreza Multidimensional`, "12.472")
      ) %>%
      rename(`Región` = `RegionAB`,
             `Pobreza Multidimensional (**)` = `Pobreza Multidimensional`,
             `Población 2025 (*)` = `Población 2025`,
             `Inversión` = `Monto2025`
      ) %>%  
      datatable(
        options = list(
          autoWidth = FALSE,
          scrollX = TRUE,
          columnDefs = list(
            list(width = '100px', targets = 0),  
            list(width = '150px', targets = 1),  
            list(width = '200px', targets = 2)   
          )
        )
      )
  })
  
  
  
  output$GrafRegCat <- renderPlotly({
    p <- ggplot(tabREG_CAT, aes(x = RegionAB, y = TotalCat, fill = Categoría, 
                                text = paste(RegionAB, "<br>",
                                             "Categoría:", Categoría, "<br>",
                                             "Inversión:", scales::comma(TotalCat, big.mark = ".", decimal.mark = ",")))) +
      geom_bar(stat = "identity", position = "stack", width = 0.6) + 
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-6, suffix = "M"),
        limits = c(0, 436729160),
        breaks = seq(0, 506729160, by = 100000000)
      ) + 
      theme_minimal() +
      scale_fill_manual(values = alpha(c( "#F4EDD3", "darkblue"), 1)) +
      theme(
        legend.title = element_blank(),  legend.title.position = "none", panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.margin = margin(t = 0)
      ) +  
      labs(y = "", x="")+
      coord_flip()
    
    
    
    ggplotly(p, tooltip = "text") %>% 
      layout(
        title = list(
          text = "",
          x = 0.2, 
          xanchor = "left",  
          yanchor = "top",
          font = list(size=13)
        ),
        hoverlabel = list(
          bgcolor = "rgba(255,255,255,0.5)", 
          font = list(color = "black", size = 12) 
        ),
        legend = list(
          orientation = "h",   
          x = 0.2,             
          y = -0.3,            
          xanchor = "center",  
          yanchor = "bottom",
          title = list(text = "") 
        ),
        margin = list(l = 100, t = 50)
      ) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  
  output$montoBoxInversion <- renderUI({
    div(
      style = "font-weight: bold; color: #003366;",
      paste("Inversión 2025: $", format(3, big.mark = ".", decimal.mark = ","))
    )
  })
  
  output$montoBoxPoblacion <- renderUI({
    div(
      style = "font-weight: bold; color: #003366;",
      paste("Población 2025: ", "20.206.953")
    )
  })
  
}


shinyApp(ui = ui, server = server)
