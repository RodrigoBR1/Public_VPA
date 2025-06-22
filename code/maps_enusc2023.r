# 1. Cargar librerías
library(pacman)
p_load(tidyverse, shiny, ggplot2, sf, chilemapas, leaflet, htmlwidgets, leaflet.extras)

# 2.1. Variables ENUSC2023 en una lista de dataframes
# Cargar de dataframes procesados desde ENUSC 2023
enusc_2023 <- setNames(
    lapply(list.files("data_processed/enusc_2023", pattern = "*.csv", full.names = TRUE), read.csv),
    tools::file_path_sans_ext(basename(list.files("data_processed/enusc_2023", pattern = "*.csv", full.names = TRUE)))
)

# 2.2. Print dataframes disponibles en 'enusc_2023'
print(paste0("Dataframes disponibles: ", paste(names(enusc_2023), collapse = " ----- ")))

# 3.1 Cargar mapa de comunas
# Load map data
mapa_chile_comunas <- chilemapas::mapa_comunas %>%
    rowwise() %>%
    filter(st_bbox(geometry)[["xmax"]] >= -80) %>%
    ungroup() %>%
    select(codigo_comuna, geometry) %>%
    left_join(
      chilemapas::codigos_territoriales %>% 
        select(codigo_comuna, nombre_comuna, region = nombre_region),
      by = "codigo_comuna"
    ) %>%
    select(nombre_comuna, region, geometry)

# 3.2. En cada dataframe de 'enusc_2023', unir 'mapa_chile_comunas$geometry' mediante 'nombre_comuna' == 'comuna'
enusc_2023 <- lapply(enusc_2023, function(df) {
    df %>%
        left_join(mapa_chile_comunas, by = c("comuna" = "nombre_comuna")) %>%
        mutate(geometry = ifelse(is.na(geometry), NA, geometry))

})

# 4. Crear mapas con leaflet para cada variable de interés
# Presencia balaceras
plot_presencia_balaceras <- enusc_2023$resultados_si_presencia_balaceras %>%
  filter(region == "Metropolitana de Santiago") %>%
  st_as_sf() %>%  # Convert to sf object
  leaflet() %>%
  addTiles() %>%
  # Create a color palette based on the balaceras presence values
  addPolygons(
    # Color polygons based on presence of balaceras
    fillColor = ~colorNumeric(
      "YlOrRd", 
      domain = si_presencia_balacerasTRUE
    )(si_presencia_balacerasTRUE),
    fillOpacity = 0.7,
    color = "#BDBDC3",
    weight = 1,
    # Create informative popups
    popup = ~paste0(
      "<strong>Comuna: </strong> ", comuna, "<br>",
      "<strong>Estimado (%) que que declara presencia de balaceras en su barrio:</strong> ", 
      round(si_presencia_balacerasTRUE * 100, 1), "% ± ", 
      round(se.si_presencia_balacerasTRUE * 100, 1), "%"
    ),
    # Highlight polygons on hover
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  # Add legend to the map
  addLegend(
    position = "bottomright",
    pal = colorNumeric("YlOrRd", domain = .$si_presencia_balacerasTRUE),
    values = (~si_presencia_balacerasTRUE * 100),
    title = "Presencia de balaceras (%)",
    opacity = 0.7
  ) %>%
  # Add source caption
  addControl(
    html = '<div style="background-color: white; padding: 5px; border-radius: 5px;">
           <small>Fuente: Encuesta Nacional Urbana de Seguridad Ciudadana (ENUSC) 2023</small>
           </div>',
    position = "bottomleft"
  )


# Uso de whatsapp
plot_whatsapp <-  enusc_2023$resultados_si_medidas_seguridad_whatsapp %>%
  filter(region == "Metropolitana de Santiago") %>%
  st_as_sf() %>%  # Convert to sf object
  leaflet() %>%
  addTiles() %>%
  # Create a color palette based on the whatsapp usage values
  addPolygons(
    # Color polygons based on whatsapp usage
    fillColor = ~colorNumeric(
      "YlGn",
      domain = si_medidas_seguridad_whatsappTRUE
    )(si_medidas_seguridad_whatsappTRUE),
    fillOpacity = 0.7,
    color = "#BDBDC3",
    weight = 1,
    # Create informative popups
    popup = ~paste0(
      "<strong>Comuna: </strong> ", comuna, "<br>",
      "<strong>Estimado (%) que declara uso de whatsapp como medida de seguridad:</strong> ",
      round(si_medidas_seguridad_whatsappTRUE * 100, 1), "% ±",
      round(se.si_medidas_seguridad_whatsappTRUE * 100, 1), "%"
    ),
    # Highlight polygons on hover
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  # Add legend to the map
  addLegend(
    position = "bottomright",
    pal = colorNumeric("YlGn", domain = .$si_medidas_seguridad_whatsappTRUE),
    values = (~si_medidas_seguridad_whatsappTRUE * 100),
    title = "Uso de WhatsApp (%)",
    opacity = 0.7
  ) %>%
  # Add source caption
  addControl(
    html = '<div style="background-color: white; padding: 5px; border-radius: 5px;">
           <small>Fuente: Encuesta Nacional Urbana de Seguridad Ciudadana (ENUSC) 2023</small>
           </div>',
    position = "bottomleft"
  )


# Presencia de pandillas
plot_presencia_pandillas <- enusc_2023$resultados_si_presencia_pandillas %>%
  filter(region == "Metropolitana de Santiago") %>%
  st_as_sf() %>%  # Convert to sf object
  leaflet() %>%
  addTiles() %>%
  # Create a color palette based on the pandillas presence values
  addPolygons(
    # Color polygons based on presence of pandillas
    fillColor = ~colorNumeric(
      "YlOrRd",
      domain = si_presencia_pandillasTRUE
    )(si_presencia_pandillasTRUE),
    fillOpacity = 0.7,
    color = "#BDBDC3",
    weight = 1,
    # Create informative popups
    popup = ~paste0(
      "<strong>Comuna: </strong> ", comuna, "<br>",
      "<strong>Estimado (%) que declara presencia de pandillas en su barrio:</strong> ",
      round(si_presencia_pandillasTRUE * 100, 1), "% ±",
      round(se.si_presencia_pandillasTRUE * 100, 1), "%"
    ),
    # Highlight polygons on hover
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  # Add legend to the map
  addLegend(
    position = "bottomright",
    pal = colorNumeric("YlOrRd", domain = .$si_presencia_pandillasTRUE),
    values = (~si_presencia_pandillasTRUE * 100),
    title = "Presencia de pandillas (%)",
    opacity = 0.7
  ) %>%
  # Add source caption
  addControl(
    html = '<div style="background-color: white; padding: 5px; border-radius: 5px;">
           <small>Fuente: Encuesta Nacional Urbana de Seguridad Ciudadana (ENUSC) 2023</small>
           </div>',
    position = "bottomleft"
  )


# Presencia de peleas con armas
plot_peleas_armas <- enusc_2023$resultados_si_presencia_peleas_con_armas_en_barrio %>%
  filter(region == "Metropolitana de Santiago") %>%
  st_as_sf() %>%  # Convert to sf object
  leaflet() %>%
  addTiles() %>%
  # Create a color palette based on the peleas con armas presence values
  addPolygons(
    # Color polygons based on presence of peleas con armas
    fillColor = ~colorNumeric(
      "YlOrRd",
      domain = si_presencia_peleas_con_armas_en_barrioTRUE
    )(si_presencia_peleas_con_armas_en_barrioTRUE),
    fillOpacity = 0.7,
    color = "#BDBDC3",
    weight = 1,
    # Create informative popups
    popup = ~paste0(
      "<strong>Comuna: </strong> ", comuna, "<br>",
      "<strong>Estimado (%) que declara presencia de peleas con armas en su barrio:</strong> ",
      round(si_presencia_peleas_con_armas_en_barrioTRUE * 100, 1), "% ±",
      round(se.si_presencia_peleas_con_armas_en_barrioTRUE * 100, 1), "%"
    ),
    # Highlight polygons on hover
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  # Add legend to the map
  addLegend(
    position = "bottomright",
    pal = colorNumeric("YlOrRd", domain = .$si_presencia_peleas_con_armas_en_barrioTRUE),
    values = (~si_presencia_peleas_con_armas_en_barrioTRUE * 100),
    title = "Presencia de peleas con armas (%)",
    opacity = 0.7
  ) %>%
  # Add source caption
  addControl(
    html = '<div style="background-color: white; padding: 5px; border-radius: 5px;">
           <small>Fuente: Encuesta Nacional Urbana de Seguridad Ciudadana (ENUSC) 2023</small>
           </div>',
    position = "bottomleft"
  )


# Violencia entre vecinos
plot_violencia_vecinos <- enusc_2023$resultados_si_presencia_violencia_entre_vecinos %>%
  filter(region == "Metropolitana de Santiago") %>%
  st_as_sf() %>%  # Convert to sf object
  leaflet() %>%
  addTiles() %>%
  # Create a color palette based on the violencia entre vecinos values
  addPolygons(
    # Color polygons based on violencia entre vecinos
    fillColor = ~colorNumeric(
      "YlOrRd",
      domain = si_presencia_violencia_entre_vecinosTRUE
    )(si_presencia_violencia_entre_vecinosTRUE),
    fillOpacity = 0.7,
    color = "#BDBDC3",
    weight = 1,
    # Create informative popups
    popup = ~paste0(
      "<strong>Comuna: </strong> ", comuna, "<br>",
      "<strong>Estimado (%) que declara violencia entre vecinos:</strong> ",
      round(si_presencia_violencia_entre_vecinosTRUE * 100, 1), "% ±",
      round(se.si_presencia_violencia_entre_vecinosTRUE * 100, 1), "%"
    ),
    # Highlight polygons on hover
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  # Add legend to the map
  addLegend(
    position = "bottomright",
    pal = colorNumeric("YlOrRd", domain = .$si_presencia_violencia_entre_vecinosTRUE),
    values = (~si_presencia_violencia_entre_vecinosTRUE * 100),
    title = "Violencia entre vecinos (%)",
    opacity = 0.7
  ) %>%
  # Add source caption
  addControl(
    html = '<div style="background-color: white; padding: 5px; border-radius: 5px;">
           <small>Fuente: Encuesta Nacional Urbana de Seguridad Ciudadana (ENUSC) 2023</small>
           </div>',
    position = "bottomleft"
  )


# Medida de seguridad en hogares: Rejas
plot_hogares_rejas <- enusc_2023$`hog-com_si_medida_seguridad_rejas` %>%
  filter(region == "Metropolitana de Santiago") %>%
  st_as_sf() %>%  # Convert to sf object
  leaflet() %>%
  addTiles() %>%
  # Create a color palette based on the rejas presence values
  addPolygons(
    # Color polygons based on presence of rejas
    fillColor = ~colorNumeric(
      "YlGn",
      domain = si_medida_seguridad_rejasTRUE
    )(si_medida_seguridad_rejasTRUE),
    fillOpacity = 0.7,
    color = "#BDBDC3",
    weight = 1,
    # Create informative popups
    popup = ~paste0(
      "<strong>Comuna: </strong> ", comuna, "<br>",
      "<strong>Estimado (%) hogares que declaran tener rejas como medida de seguridad:</strong> ",
      round(si_medida_seguridad_rejasTRUE * 100, 1), "% ± ",
      round(se.si_medida_seguridad_rejasTRUE * 100, 1), "%"
    ),
    # Highlight polygons on hover
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )) %>%
  # Add legend to the map
  addLegend(
    position = "bottomright",
    pal = colorNumeric("YlGn", domain = .$si_medida_seguridad_rejasTRUE),
    values = (~si_medida_seguridad_rejasTRUE * 100),
    title = "Hogares con rejas (%)",
    opacity = 0.7
  ) %>%
  # Add source caption
  addControl(
    html = '<div style="background-color: white; padding: 5px; border-radius: 5px;">
           <small>Fuente: Encuesta Nacional Urbana de Seguridad Ciudadana (ENUSC) 2023</small>
           </div>',
    position = "bottomleft"
  )

# Medida de seguridad en hogares: Cerco eléctrico
plot_hogares_cerco_electrico <- enusc_2023$`hog-com_si_medida_seguridad_cerco` %>%
  filter(region == "Metropolitana de Santiago") %>%
  st_as_sf() %>%  # Convert to sf object
  leaflet() %>%
  addTiles() %>%
  # Create a color palette based on the cerco eléctrico presence values
  addPolygons(
    # Color polygons based on presence of cerco eléctrico
    fillColor = ~colorNumeric(
      "YlGn",
      domain = si_medida_seguridad_cercoTRUE
    )(si_medida_seguridad_cercoTRUE),
    fillOpacity = 0.7,
    color = "#BDBDC3",
    weight = 1,
    # Create informative popups
    popup = ~paste0(
      "<strong>Comuna: </strong> ", comuna, "<br>",
      "<strong>Estimado (%) hogares que declaran tener cerco eléctrico como medida de seguridad:</strong> ",
      round(si_medida_seguridad_cercoTRUE * 100, 1), "% ± ",
      round(se.si_medida_seguridad_cercoTRUE * 100, 1), "%"
    ),
    # Highlight polygons on hover
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  # Add legend to the map
  addLegend(
    position = "bottomright",
    pal = colorNumeric("YlGn", domain = .$si_medida_seguridad_cercoTRUE),
    values = (~si_medida_seguridad_cercoTRUE * 100),
    title = "Hogares con cerco eléctrico (%)",
    opacity = 0.7
  ) %>%
  # Add source caption
  addControl(
    html = '<div style="background-color: white; padding: 5px; border-radius: 5px;">
           <small>Fuente: Encuesta Nacional Urbana de Seguridad Ciudadana (ENUSC) 2023</small>
           </div>',
    position = "bottomleft"
  )

# Medida de seguridad en hogares: Protecciones (ventanas, puertas, etc.)
plot_hogares_protecciones <- enusc_2023$`hog-com_si_medidas_seguridad_proteccion` %>%
  filter(region == "Metropolitana de Santiago") %>%
  st_as_sf() %>%  # Convert to sf object
  leaflet() %>%
  addTiles() %>%
  # Create a color palette based on the protecciones presence values
  addPolygons(
    # Color polygons based on presence of protecciones
    fillColor = ~colorNumeric(
      "YlGn",
      domain = si_medidas_seguridad_proteccionTRUE
    )(si_medidas_seguridad_proteccionTRUE),
    fillOpacity = 0.7,
    color = "#BDBDC3",
    weight = 1,
    # Create informative popups
    popup = ~paste0(
      "<strong>Comuna: </strong> ", comuna, "<br>",
      "<strong>Estimado (%) hogares que declaran tener protecciones como medida de seguridad:</strong> ",
      round(si_medidas_seguridad_proteccionTRUE * 100, 1), "% ± ",
      round(se.si_medidas_seguridad_proteccionTRUE * 100, 1), "%"
    ),
    # Highlight polygons on hover
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  # Add legend to the map
  addLegend(
    position = "bottomright",
    pal = colorNumeric("YlGn", domain = .$si_medidas_seguridad_proteccionTRUE),
    values = (~si_medidas_seguridad_proteccionTRUE * 100),
    title = "Hogares con protecciones (%)",
    opacity = 0.7
  ) %>%
  # Add source caption
  addControl(
    html = '<div style="background-color: white; padding: 5px; border-radius: 5px;">
           <small>Fuente: Encuesta Nacional Urbana de Seguridad Ciudadana (ENUSC) 2023</small>
           </div>',
    position = "bottomleft"
  )

# Medida de seguridad en hogares: Whatsapp vecinal
plot_hogares_whatsapp <- enusc_2023$`hog-com_si_medidas_seguridad_whatsapp` %>%
  filter(region == "Metropolitana de Santiago") %>%
  st_as_sf() %>%  # Convert to sf object
  leaflet() %>%
  addTiles() %>%
  # Create a color palette based on the whatsapp presence values
  addPolygons(
    # Color polygons based on presence of whatsapp
    fillColor = ~colorNumeric(
      "YlGn",
      domain = si_medidas_seguridad_whatsappTRUE
    )(si_medidas_seguridad_whatsappTRUE),
    fillOpacity = 0.7,
    color = "#BDBDC3",
    weight = 1,
    # Create informative popups
    popup = ~paste0(
      "<strong>Comuna: </strong> ", comuna, "<br>",
      "<strong>Estimado (%) hogares que declaran tener whatsapp como medida de seguridad:</strong> ",
      round(si_medidas_seguridad_whatsappTRUE * 100, 1), "% ± ",
      round(se.si_medidas_seguridad_whatsappTRUE * 100, 1), "%"
    ),
    # Highlight polygons on hover
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  # Add legend to the map
  addLegend(
    position = "bottomright",
    pal = colorNumeric("YlGn", domain = .$si_medidas_seguridad_whatsappTRUE),
    values = (~si_medidas_seguridad_whatsappTRUE * 100),
    title = "Hogares con WhatsApp (%)",
    opacity = 0.7
  ) %>%
  # Add source caption
  addControl(
    html = '<div style="background-color: white; padding: 5px; border-radius: 5px;">
           <small>Fuente: Encuesta Nacional Urbana de Seguridad Ciudadana (ENUSC) 2023</small>
           </div>',
    position = "bottomleft"
  )

# Robo con violencia a mano armada
plot_robo_violento_con_arma_de_fuego <- enusc_2023$`resultados_pers-com_si_robo_con_violencia_mano_armada` %>%
  filter(region == "Metropolitana de Santiago") %>%
  st_as_sf() %>%  # Convert to sf object
  leaflet() %>%
  addTiles() %>%
  # Create a color palette based on the robo violento presence values
  addPolygons(
    # Color polygons based on presence of robo violento
    fillColor = ~colorNumeric(
      "YlOrRd",
      domain = si_robo_con_violencia_mano_armadaTRUE
    )(si_robo_con_violencia_mano_armadaTRUE),
    fillOpacity = 0.7,
    color = "#BDBDC3",
    weight = 1,
    # Create informative popups
    popup = ~paste0(
      "<strong>Comuna: </strong> ", comuna, "<br>",
      "<strong>Estimado (%) que declara robo con violencia a mano armada:</strong> ",
      round(si_robo_con_violencia_mano_armadaTRUE * 100, 1), "% ± ",
      round(se.si_robo_con_violencia_mano_armadaTRUE * 100, 1), "%"
    ),
    # Highlight polygons on hover
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>%
  # Add legend to the map
  addLegend(
    position = "bottomright",
    pal = colorNumeric("YlOrRd", domain = .$si_robo_con_violencia_mano_armadaTRUE),
    values = (~si_robo_con_violencia_mano_armadaTRUE * 100),
    title = "Robo con violencia a mano armada (%)",
    opacity = 0.7
  ) %>%
  # Add source caption
  addControl(
    html = '<div style="background-color: white; padding: 5px; border-radius: 5px;">
           <small>Fuente: Encuesta Nacional Urbana de Seguridad Ciudadana (ENUSC) 2023</small>
           </div>',
    position = "bottomleft"
  )

# 5. Guardar los mapas como archivos HTML
htmlwidgets::saveWidget(plot_presencia_balaceras, "figures/maps/enusc_2023_presencia_balaceras.html", selfcontained = TRUE)
htmlwidgets::saveWidget(plot_whatsapp, "figures/maps/enusc_2023_whatsapp.html", selfcontained = TRUE)
htmlwidgets::saveWidget(plot_presencia_pandillas, "figures/maps/enusc_2023_presencia_pandillas.html", selfcontained = TRUE)
htmlwidgets::saveWidget(plot_peleas_armas, "figures/maps/enusc_2023_peleas_armas.html", selfcontained = TRUE)
htmlwidgets::saveWidget(plot_violencia_vecinos, "figures/maps/enusc_2023_violencia_vecinos.html", selfcontained = TRUE)
htmlwidgets::saveWidget(plot_hogares_rejas, "figures/maps/enusc_2023_hogares_rejas.html", selfcontained = TRUE)
htmlwidgets::saveWidget(plot_hogares_cerco_electrico, "figures/maps/enusc_2023_hogares_cerco_electrico.html", selfcontained = TRUE)
htmlwidgets::saveWidget(plot_hogares_protecciones, "figures/maps/enusc_2023_hogares_protecciones.html", selfcontained = TRUE)
htmlwidgets::saveWidget(plot_hogares_whatsapp, "figures/maps/enusc_2023_hogares_whatsapp.html", selfcontained = TRUE)
