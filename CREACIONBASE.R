
# CREACIÓN BASE SHINY DASHBOARD -------------------------------------------

servicios_por_region <- tibble::tibble(
  Region = c("ARICA Y PARINACOTA", "TARAPACÁ", "ANTOFAGASTA", "ATACAMA", "COQUIMBO", 
             "VALPARAÍSO", "METROPOLITANA", "O'HIGGINS", "MAULE", "ÑUBLE", 
             "BIOBÍO", "LA ARAUCANÍA", "LOS RÍOS", "LOS LAGOS", "AYSÉN", "MAGALLANES"),
  Servicios = list(
    c("DAP", "DGA","DGC"),   # Arica y Parinacota
    c("DAP", "DGA","DGC","DOP"),   # Tarapacá
    c("DA", "DGA","DOP"),   # Antofagasta
    c("DAP", "DOP", "SSR"),   # Atacama
    c("DP", "DOP","DGA","DAP","DA"),   # Coquimbo
    c("DP", "DA"),  # Valparaíso
    c("DP", "DGA","INH","SISS","DAP"),  # Metropolitana
    c("DAP", "DA","DGA","DOP","DP"),   # O'Higgins
    c("DA", "DGA","DGC"),   # Maule
    c("DA", "DAP","DOP","DGC"),   # Ñuble
    c("DA", "DGA","DAP"),  # Biobío
    c("DOP", "DAP"),   # La Araucanía
    c("DOH", "DGA","DGC","DA"),   # Los Ríos
    c("DA", "DGA","DOH"),   # Los Lagos
    c("DA", "DGA","DGC","DP","DOP"),   # Aysén
    c("DA", "DGA","DGC","DOH")    # Magallanes
  ),
  
  n = list(2, # Arica y Parinacota
           3,# Tarapacá
           2, # Antofagasta
           2,# Atacama
           1, # Coquimbo
           2, # Valparaíso
           2, # Metropolitana
           3, # O'Higgins
           1, # Maule
           2, # Ñuble
           2,# Biobío
           1, # La Araucanía
           3, # Los Ríos 
           3, # Los Lagos 
           3, # Aysén
           2)
)







aguas <- tibble::tibble(
  Region = c("ARICA Y PARINACOTA", "TARAPACÁ", "ANTOFAGASTA", "ATACAMA", "COQUIMBO", 
             "VALPARAÍSO", "METROPOLITANA", "O'HIGGINS", "MAULE", "ÑUBLE", 
             "BIOBÍO", "LA ARAUCANÍA", "LOS RÍOS", "LOS LAGOS", "AYSÉN", "MAGALLANES"),
  
  n = list(1, # Arica y Parinacota
           1,# Tarapacá
           2, # Antofagasta
           1,# Atacama
           1, # Coquimbo
           2, # Valparaíso
           1, # Metropolitana
           2, # O'Higgins
           2, # Maule
           2, # Ñuble
           2,# Biobío
           2, # La Araucanía
           2, # Los Ríos 
           2, # Los Lagos 
           2, # Aysén
           2)
)





# CREACIÓN BASE -----------------------------------------------------------


orden_regiones <- c("INTERREGIONAL","MAGALLANES","AYSÉN","LOS LAGOS","LOS RÍOS","LA ARAUCANÍA","BIOBÍO","ÑUBLE","MAULE",
                    "O'HIGGINS", "METROPOLITANA","VALPARAÍSO","COQUIMBO","ATACAMA","ANTOFAGASTA","TARAPACÁ","ARICA Y PARINACOTA")





PROYECCIONES_POBmulti <- read_excel("PROYECCIONES_POBmulti.xlsx")
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
    Servicio %in% c("INH", "SISS", "DP", "DGA", "DA") ~ "OTROS SERVICIOS",
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



tab_poblacion_nac <- PROYECCIONES_POBmulti %>%
  group_by(RegionAB) %>%
  summarise(
    "Población 2017" = sum(TOTAL2017), 
    "Población 2025" = sum(`Suma de Poblacion 2025`), 
    "Crec/Decrec relativo de población 2017 al 2025" = format(
      round((first(`Suma de Poblacion 2025`) - first(TOTAL2017)) / first(TOTAL2017) * 100, 2), 
      big.mark = ".", 
      decimal.mark = ",", 
      nsmall = 2
    ),
    "Pobreza Multidimensional" = scales::comma(
      round(sum(`Número de personas en situación de pobreza multidimensional (**)`)), 
      big.mark = ".", 
      decimal.mark = ","
    )
  )



tab_monto_nac <- BASE %>%
  select(RegionAB, `Monto 2025`) %>%
  group_by(RegionAB) %>%
  summarise("Monto2025" = sum(`Monto 2025`))


tab_resumen_nac <- left_join(tab_poblacion_nac,tab_monto_nac, by = "RegionAB")  %>% 
  mutate(RegionAB = factor(RegionAB, levels = rev(orden_regiones))) %>% 
  arrange(RegionAB) %>% 
  filter(RegionAB != "INTERREGIONAL")




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
                       "DP" = "#537895","ING" = "#A1BBCF", "OTROS SERVICIOS" ="#D9ECFF", "SISS" ="#7B8FA6")


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


SH_NAC <- BASE %>%
  filter(`Eje Ministerial` == "Seguridad hídrica" & Categoría == "Nuevo") %>%
  group_by(AGUA_DIPRES) %>%
  summarise(monto2025 = sum(`Monto 2025`, na.rm = TRUE)) %>%
  mutate(
    porcentaje = round((monto2025 / sum(monto2025)) * 100,2)
  )



CC_NA_OTROS <- BASE %>%
  mutate(CambioClimatico = ifelse(is.na(CambioClimaticoA), "NO APLICA", `Cambio Climatico`)) %>%
  group_by(CambioClimatico) %>%
  summarise(Monto2025 = sum(`Monto 2025`, na.rm = TRUE)) %>%
  mutate(
    porcentaje = Monto2025 / sum(Monto2025) * 100
  )



tabREG_CAT <- BASE %>%
  group_by(RegionAB, Categoría) %>%
  summarise(TotalCat = sum(`Monto 2025`), .groups = "keep") %>%
  mutate(RegionAB = factor(RegionAB, levels = orden_regiones)) %>%
  arrange(RegionAB, Categoría)





