library(shiny)
library(leaflet)
library(sf)
library(rnaturalearth)
library(dplyr)
library(readxl)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(bslib)
library(scales)



# CREACIÓN BASE -----------------------------------------------------------


orden_regiones <- c("INTERREGIONAL","MAGALLANES","AYSÉN","LOS LAGOS","LOS RÍOS","LA ARAUCANÍA","BIOBÍO","ÑUBLE","MAULE",
                    "O'HIGGINS", "METROPOLITANA","VALPARAÍSO","COQUIMBO","ATACAMA","ANTOFAGASTA","TARAPACÁ","ARICA Y PARINACOTA")




#PROYECCIONES_POBmulti <- read_excel("PROYECCIONES_POBmulti.xlsx")



BASE <- read_excel("BASE.xlsx")


#############################################
# Añadiendo la Abreviación de los servicios #
#############################################

BASE <- BASE %>% mutate(Servicio = case_when(Servicio == "Dirección de Arquitectura" ~ "DA",
                                             Servicio == "Dirección de Obras Hidráulicas" ~ "DOH",
                                             Servicio == "Dirección de Vialidad" ~ "DV",
                                             Servicio == "Dirección de Obras Portuarias" ~ "DOP",
                                             Servicio == "Dirección de Aeropuertos" ~ "DAP",
                                             Servicio == "Dirección de Planeamiento" ~ "DP",
                                             Servicio == "Subdirección de Servicios Sanitarios Rurales" ~ "SSR",
                                             Servicio == "Conservaciones por Administración Directa - Dirección de Vialidad"  ~ "DV",
                                             Servicio == "Conservaciones por Administración Directa - Dirección de Aeropuertos" ~ "DAP",
                                             Servicio == "Dirección General de Concesiones de Obras Públicas" ~ "DGC",
                                             Servicio == "Dirección General de Aguas" ~ "DGA",
                                             Servicio == "Dirección General de Aguas - Gestión Hídrica y Organizaciones" ~ "DGA",
                                             Servicio == "Instituto Nacional de Hidráulica"  ~ "INH",
                                             Servicio == "Superintendencia de Servicios Sanitarios" ~ "SISS"))



### Juntamos en la misma categoría Adaptación y ADAPTACIÓN
### ya que se asume que se refieren a lo mismo.


BASE <- BASE %>%
  mutate(`Cambio Climatico` = case_when(
    `Cambio Climatico` == "Adaptación" ~ "ADAPTACIÓN",
    TRUE ~ `Cambio Climatico` 
  )) 



### Creamos una categoría nueva que agrupa el NO y el NA de Cambio ### Climático

BASE <- BASE %>%
  mutate(CambioClimaticoA = case_when(
    `Cambio Climatico` == "NO" ~ NA,
    TRUE ~ `Cambio Climatico` 
  )) 



### Creamos una categoría otros para aquellos servicios que 
### tengan poca presencia a nivel presupuestario, con el fin de ### que se pueda ver mejor en un gráfico.



BASE <- BASE %>%
  mutate(SERVICIOS_A = case_when(
    Servicio %in% c("INH", "SISS", "DP", "DGA", "DA") ~ "OTROS",
    TRUE ~ Servicio
  ))



## Aquí, agrupamos por categoría de programa dipres para ver el ## desglose



BASE <- BASE %>%  mutate(AGUA_DIPRES = case_when(`Programa dipres` == "GRANDES OBRAS DE RIEGO" ~ "RIEGO",
                                                 `Programa dipres` == "CONSERVACION DE OBRAS DE RIEGO" ~ "RIEGO",
                                                 `Programa dipres` ==  "OBRAS MEDIANAS DE RIEGO" ~ "RIEGO",
                                                 `Programa dipres` ==  "EXPLOTACION DE OBRAS DE RIEGO" ~ "RIEGO",
                                                 `Programa dipres` ==  "OBRAS DE RIEGO" ~ "RIEGO",
                                                 `Programa dipres` == "AGUA POTABLE RURAL SEMI CONCENTRADO" ~ "CONSUMO HUMANO",
                                                 `Programa dipres` == "AGUA POTABLE RURAL CONCENTRADO" ~ "CONSUMO HUMANO",
                                                 `Programa dipres` == "ESTUDIO BÁSICO" ~ "ESTUDIOS Y OTROS",
                                                 `Programa dipres` == "ESTUDIOS" ~ "ESTUDIOS Y OTROS",
                                                 
                                                 `Programa dipres` == "OTROS" ~ "ESTUDIOS Y OTROS",
                                                 
                                                 TRUE ~ "GESTIÓN"))





#estas son las inversiones para cada región de acuerdo al 
#año 2024


Inv2024 <- c(129823086,83973170,174052596,165583748,155667167,215482639,424443541,197270782,249806178,168593498,298594294,255255930,221003257,442262402,128900221,156212197,208612150)

PL2024 <- as.data.frame(cbind(rev(orden_regiones),Inv2024))
PL2024$Inv2024 <- as.numeric(PL2024$Inv2024)
colnames(PL2024)[1] <- "RegionAB"



# NACIONAL ----------------------------------------------------------------


orden_regiones_tab <- rev(orden_regiones)


tab1 <- BASE %>% group_by(RegionAB) %>% summarise("Monto 2025" = sum(`Monto 2025`)) %>% mutate(
  RegionAB = factor(RegionAB, levels = orden_regiones)
) %>% arrange(RegionAB)




tab1 <- tab1 %>%
  mutate(
    IncDec = round(`Monto 2025` / Inv2024, 2),  
    Flecha = case_when(
      IncDec > 1 ~ "↑",  
      IncDec < 1 ~ "↓",  
      TRUE ~ "="         
    ),
    
    Label = paste0(
      scales::comma(`Monto 2025`, big.mark = ".", decimal.mark = ","),  
      "   ",  
      "<span style='color:red;'>", Flecha, "</span>",
      "   (", scales::comma(IncDec, big.mark = ".", decimal.mark = ","), ")"  
    )
  )



tabREG_CAT <- BASE %>%
  group_by(RegionAB, Categoría) %>%
  summarise(TotalCat = sum(`Monto 2025`), .groups = "keep") %>%
  mutate(RegionAB = factor(RegionAB, levels = orden_regiones)) %>%
  arrange(RegionAB, Categoría)





tab_eje_ministerial_cat_nac <- BASE %>%
  group_by(`Eje Ministerial`, Categoría) %>%
  summarise(Monto2025 = sum(`Monto 2025`), .groups = "drop") %>%
  ungroup() %>%  
  mutate(
    total_global = sum(Monto2025),  
    porcentaje = Monto2025 / total_global * 100 
  )



colores <- c(
  "Integración territorial, conectividad y movilidad" = "#155E95",
  "Desarrollo Productivo, Social, Cultural y Científico" = "#98D8EF",
  "Seguridad Ciudadana y ante desastres naturales y emergencias" = "#4C7B8B",
  "Seguridad hídrica" = "#F4EDD3"
)


tab_servicios <- BASE %>%
  group_by(Servicio) %>%
  summarise(P2025 = sum(`Monto 2025`)) %>%
  mutate(
    total = sum(P2025),  
    porcentaje = P2025 / total * 100  
  )


tab_servicios$Servicio <- reorder(tab_servicios$Servicio, tab_servicios$P2025)


colores_servicios <- c("DV" = "#000957", "DGC" = "#155E95", "SSR" = "#4C585B","DOH" = "#7E99A3", 
                       "DAP" = "#344CB7", "DOP" = "#98D8EF", "DA" = "#C4D9FF", "DGA" ="#1A3A6E", 
                       "DP" = "#537895","ING" = "#A1BBCF", "OTROS" ="#D9ECFF", "SISS" ="#7B8FA6")


SERVICIOS_NAC <- BASE %>%
  group_by(SERVICIOS_A) %>%
  summarise(Monto2025 = sum(`Monto 2025`, na.rm = TRUE)) %>%
  mutate(Porcentaje = round(Monto2025 / sum(Monto2025) * 100,2))

otros_data <- BASE %>%
  group_by(Servicio) %>%
  summarise(Monto = sum(`Monto 2025`, na.rm = TRUE)) %>%
  mutate(Porcentaje = round(Monto / sum(Monto) * 100,3)) %>% 
  filter(Servicio %in% c("INH", "SISS", "DP", "DGA", "DA")) %>%
  mutate(
    Monto = scales::comma(Monto, big.mark = ".", decimal.mark = ",")
  )  


CC_NA_OTROS <- BASE %>%
  mutate(CambioClimatico = ifelse(is.na(CambioClimaticoA), "No aplica", `Cambio Climatico`)) %>%
  group_by(CambioClimatico) %>%
  summarise(Monto2025 = sum(`Monto 2025`, na.rm = TRUE)) %>%
  mutate(
    CambioClimatico = as.factor(CambioClimatico),
    porcentaje = round(Monto2025 / sum(Monto2025) * 100, 2),  # Mantener como numérico
    etiqueta = paste0(CambioClimatico, "\n", 
                      format(porcentaje, nsmall = 2, decimal.mark = ","), "%"),  # Solo formatear aquí
    leyenda_texto = paste0(CambioClimatico, " - ", 
                           scales::comma(Monto2025, big.mark = ".", decimal.mark = ","))
  )



# CODIGO SHINY ------------------------------------------------------------




chile_regiones <- ne_states(country = "Chile", returnclass = "sf") %>% 
                  mutate(RegionAB = case_when(name == "Arica y Parinacota" ~ "ARICA Y PARINACOTA",
                                            name == "Tarapacá" ~ "TARAPACÁ",
                                            name == "Antofagasta" ~ "ANTOFAGASTA",
                                            name == "Atacama" ~ "ATACAMA",
                                            name == "Coquimbo" ~ "COQUIMBO",
                                            name == "Región Metropolitana de Santiago" ~ "METROPOLITANA",
                                            name == "Valparaíso" ~ "VALPARAÍSO",
                                            name == "Maule" ~ "MAULE",
                                            name == "Libertador General Bernardo O'Higgins" ~ "O'HIGGINS",
                                            name == "Ñuble" ~ "ÑUBLE",
                                            name == "La Araucanía" ~ "LA ARAUCANÍA",
                                            name == "Bío-Bío" ~ "BIOBÍO",
                                            name == "Los Ríos" ~ "LOS RÍOS",
                                            name == "Los Lagos" ~ "LOS LAGOS",
                                            name == "Aisén del General Carlos Ibáñez del Campo" ~ "AYSÉN",
                                            name == "Magallanes y Antártica Chilena" ~ "MAGALLANES"
                                      ))




chile_regiones <- left_join(chile_regiones, tab1, by = "RegionAB")

chile_regiones <- chile_regiones %>% mutate(color = ifelse(IncDec > 1,"darkblue","grey"))



ui <- navbarPage(
  title = div(icon("globe-americas"), "Proyecto de Ley 2025"),
  theme = bs_theme(bootswatch = "flatly"),  
  tabPanel("🗺️ Mapa",
           fluidPage(
              fluidRow(titlePanel("Mapa Interactivo de Chile"),
                       
             sidebarPanel(wellPanel(h3("Información Importante"),
                       p("Las gráficas y estadísticas presentadas en este Dashboard fueron obtenidas a partir de los datos
                                   del Departamento de Gestión Presupuestaria, y del Instituto Nacional de Estadísticas (INE)"),
                       tags$ul(
                         tags$li("Todos los montos presentes en todo el dashboard, se encuentran en Miles de Pesos Chilenos."),
                         tags$li("Segundo punto"),
                         tags$li("Tercer punto"))), style = "background: slategray, color :white"),
             mainPanel(leafletOutput("mapa", width = "100%", height = "700px")))
           )
  ),
  tabPanel("📌 Región Seleccionada",
           fluidPage(
             titlePanel(textOutput("titulo_region")),
             h3("Información, gráficos y Estadísticas de la Región"),
             p()
           )
  ),
  
  tabPanel("🇨🇱Nacional",
           fluidPage(
             titlePanel("Información a Nivel Nacional"),
             fluidRow(
               column(4, 
                      plotlyOutput("grafico_nacional", height = "400px")),
               
               column(4, plotlyOutput("GrafRegCat", height = "400px")),
               column(4, plotlyOutput("graf_servicios", height = "400px"))
               ),
             
             fluidRow(
               column(5, plotlyOutput("graf_Eje_Cat", height = "400px")),
               column(5, plotlyOutput("CC_NAC", height = "400px"))
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
        label = ~name,
        layerId = ~name 
      ) %>% 
      addLegend(
        position = "topright",
        colors = c("darkblue","grey"),
        labels = c("Incremento","Decremento"),
        title = "Región con Incremento/Decremento de la Inversión\ncon respecto al año 2024"
      )
    
  })
  
  
  observeEvent(input$mapa_shape_click, {
    region <- input$mapa_shape_click$id
    region_seleccionada(region)
    
    updateNavbarPage(session, "Proyecto de Ley 2025", selected = "📌 Región Seleccionada")
  })
  
  
  output$titulo_region <- renderText({
    if (!is.null(region_seleccionada())) {
      paste("Información de la Región:", region_seleccionada())
    } else {
      "Seleccione una región en el mapa"
    }
  })
  
  
  output$grafico_nacional <- renderPlotly({
    
    p <- ggplot(tab1, aes(y = RegionAB, x = `Monto 2025`, text = Label)) +  
      geom_bar(stat = "identity", fill = "darkblue", width = 0.6) +
      theme_minimal() +
      scale_x_continuous(
        labels = scales::label_number(scale = 1e-6, suffix = "M"),
        limits = c(0, 636729160),
        breaks = seq(0, 506729160, by = 100000000)
      ) +
      labs(x = "", y = "", title = "") +
      theme(plot.title = element_text(size = 10))
    
    ggplotly(p, tooltip = "text") %>% 
      layout(
        annotations = list(
          list(
            text = "Inversión 2025 (miles de pesos) a Nivel Nacional",
            x = 1, 
            y = 1.1, 
            xref = "paper", 
            yref = "paper", 
            showarrow = FALSE,
            font = list(size = 13, color = "black")
          )
        ),
        margin = list(t = 50, b = 0, l = 0, r = 0),  
        hoverlabel = list(
          bgcolor = "#f5f5f5", 
          font = list(color = "black", size = 10)  
        )
      ) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  
  output$GrafRegCat <- renderPlotly({
    p <- ggplot(tabREG_CAT, aes(x = RegionAB, y = TotalCat, fill = Categoría, text = paste("Región:", RegionAB, "<br>",
                                                                                                 "Categoría:", Categoría, "<br>",
                                                                                                 "Inversión:", scales::comma(TotalCat, big.mark = ".", decimal.mark = ",")))) +
      geom_bar(stat = "identity", position = "stack", width = 0.6) + 

      scale_y_continuous(
        labels = scales::label_number(scale = 1e-6, suffix = "M"),
        limits = function(x) {
          if ("Arrastre" %in% tabREG_CAT$Categoría) {
            c(0, 599565751)  
          } else {
            NULL  
          }
        }
      ) + 
      theme_minimal() +
      scale_fill_manual(values = alpha(c("cornsilk4", "darkblue"), .7)) +
      theme(plot.title = element_text(size=10, margin = margin(l=-2)), legend.title = element_text("Tipo de Proyecto", size=10)) +  
      coord_flip()
    
    
    ggplotly(p, tooltip = "text") %>% 
      layout(
        hoverlabel = list(
          bgcolor = "#f5f5f5", 
          font = list(color = "black", size = 12)  
        ),
        legend = list(
          orientation = "h",   
          x = 0.5,             
          y = -0.2,            
          xanchor = "center",  
          yanchor = "top"
        )
      ) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  
  output$graf_Eje_Cat <- renderPlotly({
    
            p <- ggplot(tab_eje_ministerial_cat_nac, aes(x = `Eje Ministerial`, y = Monto2025, fill = `Eje Ministerial`)) +
              facet_wrap(~ Categoría, labeller = as_labeller(c("Arrastre" = "Proyecto de Arrastre", "Nuevo" = "Proyecto Nuevo"))) +
              labs(
                title = "Inversión 2025 por clasificación de proyecto (arrastre y nuevo) y Eje Ministerial", 
                fill = "",
                caption = "Figura 3: Inversión 2025 por categoría de proyecto (nuevo y arrastre) y por los cuatro ejes ministeriales. \n Fuente: Elaboración propia a partir de los datos del Departamento de Gestión Presupuestaria."
              ) + 
              geom_bar(stat = "identity", width = 0.6) + 
              geom_text(
                aes(
                  label = paste0(
                    scales::comma(Monto2025, big.mark = ".", decimal.mark = ","), 
                    "\n", 
                    scales::comma(round(porcentaje, 2), big.mark = ".", decimal.mark = ","), "%"
                  )), 
                vjust = -0.5, 
                size = 3, 
                color = "black", 
                fontface = "bold",
                show.legend = FALSE
              ) +
              scale_fill_manual(values = colores) +
              theme(
                panel.background = element_rect(fill = "white", color = NA),
                panel.spacing = unit(0.8, "lines"), 
                panel.border = element_rect(color = "black", fill = NA, size = 0.0005),
                axis.text.x = element_blank(), 
                axis.text.y = element_text(size = 9),
                strip.text = element_text(size = 9),
                strip.background = element_rect(fill = "white", color = "black"),
                panel.grid.major = element_line(color = "gray90"),
                panel.grid.minor = element_line(color = "gray90"),
                legend.text = element_text(size = 8.5),
                legend.title = element_blank(),
                legend.position = "bottom",
                legend.justification = c(0.9, 0),
                legend.spacing.x = unit(0.05, "cm"),  
                legend.spacing.y = unit(0.02, "cm"),
                legend.box = "horizontal", 
                legend.key.size = unit(0.4, "cm"),
                legend.box.spacing = unit(0.5, "cm"), 
                plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
                plot.title = element_text(size = 10, hjust = 0.5, margin = margin(b = 20)), 
                plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10, r = 100, b = 5, l=-30))) +
              guides(fill = guide_legend(nrow = 3, byrow = TRUE)) + 
              scale_y_continuous(
                labels = scales::label_number(scale = 1e-6, suffix = "M"),
                expand = expansion(mult = c(0, 0.3)) 
              )  + 
              guides(fill = guide_legend(ncol = 2, nrow = 2, byrow = TRUE))
            
            
            
            ggplotly(p, tooltip = "text") %>% 
              layout(
                hoverlabel = list(
                  bgcolor = "#f5f5f5", 
                  font = list(color = "black", size = 12)  
                ),
                legend = list(
                  orientation = "h",  
                  x = 0.5,             
                  y = -0.2,            
                  xanchor = "center",  
                  yanchor = "top"
                )
              ) %>% 
              config(displayModeBar = FALSE)
            
  })
  
  output$graf_servicios <- renderPlotly({
    
    p <- ggplot(SERVICIOS_NAC , aes(x = reorder(SERVICIOS_A, -Monto2025), y = Monto2025, fill = SERVICIOS_A, text = ifelse(SERVICIOS_A == "OTROS",
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
    labs(title = "Distribución de Inversión 2025 por servicios MOP \na nivel nacional",
         x = "Servicios",  y = "") +
    scale_fill_manual(values = colores_servicios) +
    scale_y_continuous(limits = c(0, max(SERVICIOS_NAC$Monto2025) * 1.1), labels = label_number(scale = 1e-6, suffix = "M")) +
    theme_minimal()+
    theme(legend.position = "none", plot.title = element_text(size=10))+
      coord_flip()
  
  
  ggplotly(p, tooltip = "text") %>% 
    layout(
      hoverlabel = list(
        bgcolor = "#f5f5f5", 
        font = list(color = "black", size = 12)  
      ))%>% 
        config(displayModeBar = FALSE)})
  
  
  
  output$CC_NAC <- renderPlotly({
    plot_ly(
      data = CC_NA_OTROS, 
      labels = ~CambioClimatico, 
      values = ~Monto2025,  
      alpha=0.7,
      height = 400,  
      width = 400,
      text = ~etiqueta,
      texinfo = "percent+label",
      hoverinfo = "text",  
      marker = list(colors = c("#578E7E", "cornsilk3"), line = list(color = "white", width = 1))
    ) %>%
      add_pie(hole=0.5) %>% 
      layout(
        font = list(size = 12), 
        title = list(
          text = "Distribución porcentual de Inversiones 2025\npor categoría de cambio climático"
        ),
        showlegend = TRUE,
        legend = list(x = 3,  
                      y = 1.15,
                      xanchor = "center",
                      yanchor = "top",
                      font = list(size = 12, color = "black"),  
                      bgcolor = "rgba(255,255,255,0.7)")) %>%
      config(displayModeBar = FALSE)
  })
}



shinyApp(ui = ui, server = server)

rsconnect::writeManifest()


rsconnect::setAccountInfo(name='isidora2001',
                          token='C87FB1229D2AC6A9614F734ADED23CA2',
                          secret='Vjut94oc5Mel+zbjp/3hRugVzw/wmL4+k42fHxCJ')


rsconnect::deployApp()


usethis::use_git()

