# Libraries
library(pacman)
p_load(tidyverse, arrow, openxlsx, stringi, ggplot2, ggrepel)

# Load delitos conocidos por la policía o 'Caso policiales' (denuncias + flagrancias) RM 2023
delitos <- read_csv("data_processed/delitos/datos_23_tasa_delitos_cada-100000_limpios.csv") %>% # 'Casos policiales' (denuncias + flagrancias) cada 100 000 habitantes. Scrapeados desde https://cead.spd.gov.cl/estadisticas-delictuales/
                    select(-anio) %>%
                    pivot_wider(
                      names_from = delitos, 
                      values_from = cifra, 
                      names_prefix = "tasa_100k_"
                    ) %>%
                    mutate(across(starts_with("tasa_100k_"), ~replace_na(., 0))) %>%
                    mutate(across(where(is.character), ~stri_trans_general(., "Latin-ASCII"))) # Remove accents and Ñs from character columns


# Load #datovecino data RM 2023
datovecino <- list.files("DATO_VECINO_RM", # Recuperado de https://experience.arcgis.com/experience/6969dea966a14d37bd38435f8bbdfe24 plataforma abierta destinada a que la gente pueda conocer los datos de sus unidades vecinales
             pattern = "*.xlsx", 
             full.names = TRUE) %>%
  lapply(function(file) {
  read.xlsx(file) %>%
    select(
    comuna = `NOMBRE.COMUNA`,
    areas_verdes_m2 = `Sumatoria.de.las.superficies.de.áreas.verdes.contenidas.en.la.unidad.vecinal.(m2)`,
    establecimientos_educacion = `Total.de.establecimientos.de.educación`,
    supermercados = `Cantidad.de.supermercados`,
    instalaciones_cch = `Cantidad.de.Cuarteles.de.Carabineros.de.Chile`,
    instalaciones_pdi = `Cantidad.de.instalaciones.de.Unidades.Policiales.de.Investigación.de.Chile.(PDI)`,
    paraderos = `Cantidad.de.paraderos.de.transporte.público`,
    establecimientos_saluds = `Total.de.establecimientos.de.salud`
    )
  }) %>%
  bind_rows() %>%
  group_by(comuna) %>%
  summarize(
    across(where(is.numeric), ~sum(.x, na.rm = TRUE))
  ) %>%
  mutate(across(where(is.character), ~stri_trans_general(., "Latin-ASCII")), # Remove accents and Ñs from character columns
                           comuna = str_to_title(comuna)) # Convert comuna names to title case


# Load ENUSC files RM 2023
pattern <- "(pers-com|hog-com)_(si_presencia_balaceras|si_arma_en_casa|si_presencia_pandillas|si_presencia_violencia_entre_vecinos|si_presencia_peleas_con_armas_en_barrio|si_medidas_seguridad_whatsapp|si_medida_seguridad_rejas|si_medidas_seguridad_proteccion|si_medida_seguridad_cerco|victimizacion_armada_de_algun_tipo|robo_armas_de_algun_tipo|acceso_casas_controlado|acceso_controlado|acceso_directo|acceso_indeterminado|confia_muchobastante_carabineros|confia_muchobastante_pdi|confia_muchobastante_policias)"
selected <- grep(pattern, list.files("data_processed/enusc_2023", pattern = "*.csv", full.names = TRUE), value = TRUE)
enusc_2023 <- setNames(lapply(selected, read.csv), tools::file_path_sans_ext(basename(selected)))
# Process ENUSC data into a single dataframe
enusc_2023_processed <- names(enusc_2023) %>% 
  reduce(function(acc, name) {
    # Determine suffix based on file name
    suffix <- ifelse(grepl("pers-com", name), "pers-com", "hog-com")
    
    # Find columns containing "TRUE"
    true_cols <- grep("TRUE", names(enusc_2023[[name]]), value = TRUE)
    
    if (length(true_cols) > 0) {
      # Process current dataset
      current_data <- enusc_2023[[name]] %>%
        select(comuna, all_of(true_cols)) %>%
        rename_with(~ paste0(., "_", suffix), -comuna)
      
      # Join with accumulated data
      if (is.null(acc)) {
        return(current_data)
      } else {
        return(left_join(acc, current_data, by = "comuna"))
      }
    } else {
      return(acc)
    }
  }, .init = NULL) %>%
  # Remove accents and Ñs from character columns
  mutate(across(where(is.character), 
                ~stri_trans_general(., "Latin-ASCII")))


# Load DGMN files RM 2024
armas_comunal_rm <- read.csv("DGMN/tablas/data_armas_comuna.csv") %>% filter(año == 2024 & cod_region == 13) %>% #DGMN - Obtenidas mediante transparencia - TABLA BASE
              mutate(across(where(is.character), ~stri_trans_general(., "Latin-ASCII"))) %>% # Remove accents and Ñs from character columns
                group_by(comuna) %>%
                mutate(across(where(is.numeric) & 
                              !any_of(c("año", "cod_comuna", "cod_region", "region", "poblacion")),
                              ~ if(!is.na(poblacion) && poblacion > 0) {
                                  (.x / poblacion) * 100000
                                } else {
                                  .x
                                },
                              .names = "tasa_cada100k_{.col}")) %>%
                ungroup()

# Load CASEN files RM 2022
casen_stats_rm <- read_parquet("data_processed/casen/casen2022_stats_rm.parquet") %>% mutate(comuna = stri_trans_general(comuna, "Latin-ASCII"))

# Load Indice de Prioridad Social (IPS) RM 2022
ips_rm <- read.xlsx("data_processed/ips/IPS_2022.xlsx") %>%
                rename(ranking_ips = Rk,
                       indice_prioridad_social = `IPS.2022`,
                       categoria_ips = `Categoría`) %>%
                mutate(ranking_ips = str_remove(ranking_ips, "°"),
                       categoria_ips = as.factor(categoria_ips)) %>%
                mutate(across(where(is.character), ~stri_trans_general(., "Latin-ASCII"))) # Remove accents and Ñs from character columns


# Join all data into rm_armas
rm_armas <- armas_comunal_rm %>%
  left_join(datovecino, by = "comuna") %>%
  left_join(delitos, by = c("comuna" = "nombre_comuna")) %>%
  left_join(casen_stats_rm, by = "comuna") %>%
  left_join(ips_rm, by = c("comuna" = "Comuna")) %>%
  left_join(enusc_2023_processed, by = "comuna") %>%
  mutate(across(where(is.character),
                ~stri_trans_general(., "Latin-ASCII"))) # Remove accents and Ñs from character columns


# Write csv with colnames(rm_armas)
write.csv(rm_armas, "data_processed/rm_armas.csv", row.names = FALSE)

# Load metadata
metadata <- read.csv("data_processed/metadata_rm_armas.csv") %>% mutate(across(where(is.character), ~stri_trans_general(., "Latin-ASCII")))

# Create a workbook with two sheets
wb <- createWorkbook()
addWorksheet(wb, "data")
addWorksheet(wb, "metadata")

# Write data to sheets
writeData(wb, "data", rm_armas)
writeData(wb, "metadata", metadata)

# Save the workbook
saveWorkbook(wb, "data_processed/rm_armas.xlsx", overwrite = TRUE)


#
##
### Algunos plots de exploración de datos
##
#

# Etiquetas IPS categoria y colores
ips_colors <- c(
  "ALTA PRIORIDAD" = "#e41a1c",         # Red
  "MEDIA ALTA PRIORIDAD" = "#ff7f00",   # Orange
  "MEDIA BAJA PRIORIDAD" = "#a88300",   # Light brown
  "BAJA PRIORIDAD" = "#00bc29",         # Light green
  "SIN PRIORIDAD" = "#024100"           # Dark green
)


# rm_armas$porcentaje_carente_estado en X y rm_armas$`tasa_100k_Otros homicidios` en Y
vivienda_carente_homicidios <- rm_armas %>%
     filter(!is.na(porcentaje_carente_estado) & !is.na(`tasa_100k_Otros homicidios`)) %>%
      ggplot( 
      aes(
        x = porcentaje_carente_estado, 
        y = `tasa_100k_Otros homicidios`,
        color = categoria_ips,
        label = comuna
      )
      ) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE, color = "darkgrey", alpha = 0.3) +
      geom_text_repel(
        size = 3,
        max.overlaps = 10,
        box.padding = 0.5
      ) +
      scale_color_manual(values = ips_colors) +
      labs(
        title = "Relación entre porcentaje de hogares con viviendas 'carentes' y tasa de 'otros homicidios' (no asociados a robo) cada 100,000 hab.",
        subtitle = "Comunas de RM 2023",
        x = "Porcentaje de hogares con viviendas 'carentes'",
        y = "Tasa de 'otros homicidios' (por 100,000 hab.)",
        color = "Categoría IPS",
        caption = "Nota: línea de tendencia se calcula con regresión lineal (geom_smooth con method = 'lm' en R)"
      ) +
      scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme_minimal() +
      annotate(# Añadir el coeficiente de correlación como anotación
        "text",
        x = max(rm_armas$porcentaje_carente_estado, na.rm = TRUE) * 0.9,
        y = max(rm_armas$`tasa_100k_Otros homicidios`, na.rm = TRUE) * 0.9,
        label = paste("correlación (r pearson) =", round(
          cor(
            rm_armas$porcentaje_carente_estado,
            rm_armas$`tasa_100k_Otros homicidios`,
            use = "complete.obs"
          ),
          2
        )),
        hjust = 1,
        size = 5,
        fontface = "bold"
      )

vivienda_carente_homicidios

# rm_armas$`acceso_controladoTRUE_hog-com` en X y rm_armas$`si_medida_seguridad_rejasTRUE_hog-com` en Y, color = categoria_ips
acceso_rejas <- rm_armas %>%
  filter(!is.na(`acceso_controladoTRUE_hog-com`) & !is.na(`si_medida_seguridad_rejasTRUE_hog-com`)) %>%
  ggplot(
    aes(
      x = `acceso_controladoTRUE_hog-com`,
      y = `si_medida_seguridad_rejasTRUE_hog-com`,
      color = categoria_ips,
      label = comuna
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkgrey", alpha = 0.3) +
  geom_text_repel(
    size = 3,
    max.overlaps = 10,
    box.padding = 0.5
  ) +
  scale_color_manual(values = ips_colors) +
  labs(
    title = "Relación entre acceso controlado a viviendas y uso de rejas como medida de seguridad",
    subtitle = "Comunas de RM 2023",
    x = "Porcentaje de Hogares con Acceso Controlado (%)",
    y = "Porcentaje de Hogares con Rejas como Medida de Seguridad (%)",
    color = "Categoría IPS",
    caption = "Nota: línea de tendencia se calcula con regresión lineal (geom_smooth con method = 'lm' en R)"
  ) +
  scale_x_continuous(labels = scales::label_percent(scale = 100)) +
  scale_y_continuous(labels = scales::label_percent(scale = 100)) +
  theme_minimal() +
  annotate(# Añadir el coeficiente de correlación como anotación
    "text",
    x = max(rm_armas$`acceso_controladoTRUE_hog-com`, na.rm = TRUE) * 0.9,
    y = max(rm_armas$`si_medida_seguridad_rejasTRUE_hog-com`, na.rm = TRUE) * 0.9,
    label = paste("correlación (r pearson) =", round(
      cor(
        rm_armas$`acceso_controladoTRUE_hog-com`,
        rm_armas$`si_medida_seguridad_rejasTRUE_hog-com`,
        use = "complete.obs"
      ),
      2
    )),
    hjust = 1,
    size = 5,
    fontface = "bold"
  )

acceso_rejas


# rm_armas$promedio_ytotcorh en X y rm_armas$tasa_cada100k_total_inscritas en Y, color = categoria_ips
ingreso_armas <- rm_armas %>%
  filter(!is.na(tasa_cada100k_total_inscritas) & !is.na(promedio_ytotcorh)) %>%
   ggplot( 
   aes(
     x = promedio_ytotcorh,
     y = tasa_cada100k_total_inscritas,
     color = categoria_ips,
     label = comuna
   )
   ) +
   geom_point() +
   geom_smooth(method = "lm", se = TRUE, color = "darkgrey", alpha = 0.3) +
   geom_text_repel(
     size = 3,
     max.overlaps = 10,
     box.padding = 0.5
   ) +
   scale_color_manual(values = ips_colors) +
   labs(
     title = "Relación entre ingreso promedio del hogar y la tasa cada 100.000 habitantes de armas inscritas",
     subtitle = "Comunas de RM",
     x = "Ingreso Promedio del Hogar (CLP $)",
     y = "Tasa cada 100.000 habitantes de Armas Inscritas (2024)",
     color = "Categoría IPS",
     caption = "Nota: línea de tendencia se calcula con regresión lineal (geom_smooth con method = 'lm' en R)"
   ) +
   scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
   theme_minimal() +
   annotate(# Añadir el coeficiente de correlación como anotación
     "text",
     x = max(rm_armas$promedio_ytotcorh, na.rm = TRUE) * 0.9,
     y = max(rm_armas$tasa_cada100k_total_inscritas, na.rm = TRUE) * 0.9,
     label = paste("correlación (r pearson) =", round(
       cor(
         rm_armas$promedio_ytotcorh,
         rm_armas$tasa_cada100k_total_inscritas,
         use = "complete.obs"
       ),
       2
     )),
     hjust = 1,
     size = 5,
     fontface = "bold"
   )

ingreso_armas


# rm_armas$`si_presencia_balacerasTRUE_hog-com` en X y rm_armas$`acceso_controladoTRUE_hog-com` en Y, color = categoria_ips
acceso_balaceras <- rm_armas %>%
  filter(!is.na(`si_presencia_balacerasTRUE_hog-com`) & !is.na(`acceso_controladoTRUE_hog-com`)) %>%
   ggplot( 
   aes(
     x = `si_presencia_balacerasTRUE_hog-com`,
     y = `acceso_controladoTRUE_hog-com`,
     color = categoria_ips,
     label = comuna
   )
   ) +
   geom_point() +
   geom_smooth(method = "lm", se = TRUE, color = "darkgrey", alpha = 0.3) +
   geom_text_repel(
     size = 3,
     max.overlaps = 10,
     box.padding = 0.5
   ) +
   scale_color_manual(values = ips_colors) +
   labs(
     title = "Relación entre presencia de balaceras y acceso controlado a la vivienda",
     subtitle = "Comunas de RM 2023",
     x = "Porcentaje de Hogares que declara Presencia de Balaceras en barrio(%)",
     y = "Porcentaje de Hogares con Acceso Controlado (%)",
     color = "Categoría IPS",
     caption = "Nota: línea de tendencia se calcula con regresión lineal (geom_smooth con method = 'lm' en R)"
   ) +
   scale_x_continuous(labels = scales::label_percent(scale = 100)) +
   scale_y_continuous(labels = scales::label_percent(scale = 100)) +
   theme_minimal() +
  annotate(# Añadir el coeficiente de correlación como anotación
    "text",
    x = max(rm_armas$`si_presencia_balacerasTRUE_hog-com`, na.rm = TRUE) * 0.9,
    y = max(rm_armas$`acceso_controladoTRUE_hog-com`, na.rm = TRUE) * 0.9,
    label = paste("correlación (r pearson) =", round(
      cor(
        rm_armas$`si_presencia_balacerasTRUE_hog-com`,
        rm_armas$`acceso_controladoTRUE_hog-com`,
        use = "complete.obs"
      ),
      2
    )),
    hjust = 1,
    size = 5,
    fontface = "bold"
  )

acceso_balaceras


# rm_armas$`confia_muchobastante_policiasTRUE_hog-com` en X y rm_armas$`acceso_controladoTRUE_hog-com` en Y, color = categoria_ips
confianza_acceso <- rm_armas %>%
  filter(!is.na(`confia_muchobastante_policiasTRUE_hog-com`) & !is.na(`acceso_controladoTRUE_hog-com`)) %>%
  ggplot(
    aes(
      x = `confia_muchobastante_policiasTRUE_hog-com`,
      y = `acceso_controladoTRUE_hog-com`,
      color = categoria_ips,
      label = comuna
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkgrey", alpha = 0.3) +
  geom_text_repel(
    size = 3,
    max.overlaps = 10,
    box.padding = 0.5
  ) +
  scale_color_manual(values = ips_colors) +
  labs(
    title = "Relación entre confianza en policías y acceso controlado a la vivienda",
    subtitle = "Comunas de RM 2023",
    x = "Porcentaje de Hogares que confía mucho/bastante en Policías (%)",
    y = "Porcentaje de Hogares con Acceso Controlado (%)",
    color = "Categoría IPS",
    caption = "Nota: línea de tendencia se calcula con regresión lineal (geom_smooth con method = 'lm' en R)"
  ) +
  scale_x_continuous(labels = scales::label_percent(scale = 100)) +
  scale_y_continuous(labels = scales::label_percent(scale = 100)) +
  theme_minimal() +
  annotate(# Añadir el coeficiente de correlación como anotación
    "text",
    x = max(rm_armas$`confia_muchobastante_policiasTRUE_hog-com`, na.rm = TRUE) * 0.9,
    y = max(rm_armas$`acceso_controladoTRUE_hog-com`, na.rm = TRUE) * 0.9,
    label = paste("correlación (r pearson) =", round(
      cor(
        rm_armas$`confia_muchobastante_policiasTRUE_hog-com`,
        rm_armas$`acceso_controladoTRUE_hog-com`,
        use = "complete.obs"
      ),
      2
    )),
    hjust = 1,
    size = 5,
    fontface = "bold"
  )

confianza_acceso


# rm_armas$`si_medidas_seguridad_whatsappTRUE_hog-com` en X y rm_armas$`presencia_peleas_con_armas_en_barrioTRUE_hog-com` en Y, color = categoria_ips
violencia_whatsapp <- rm_armas %>%
  filter(!is.na(`si_medidas_seguridad_whatsappTRUE_hog-com`) & !is.na(`si_presencia_peleas_con_armas_en_barrioTRUE_hog-com`)) %>%
  ggplot(
    aes(
      x = `si_medidas_seguridad_whatsappTRUE_hog-com`,
      y = `si_presencia_peleas_con_armas_en_barrioTRUE_hog-com`,
      color = categoria_ips,
      label = comuna
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkgrey", alpha = 0.3) +
  geom_text_repel(
    size = 3,
    max.overlaps = 10,
    box.padding = 0.5
  ) +
  scale_color_manual(values = ips_colors) +
  labs(
    title = "Relación entre uso de WhatsApp para seguridad y presencia de peleas con armas en el barrio",
    subtitle = "Comunas de RM 2023",
    x = "Porcentaje de Hogares que usa WhatsApp para Seguridad (%)",
    y = "Porcentaje de Hogares que declara Presencia de Peleas con Armas en el Barrio (%)",
    color = "Categoría IPS",
    caption = "Nota: línea de tendencia se calcula con regresión lineal (geom_smooth con method = 'lm' en R)"
  ) +
  scale_x_continuous(labels = scales::label_percent(scale = 100)) +
  scale_y_continuous(labels = scales::label_percent(scale = 100)) +
  theme_minimal() +
  annotate(# Añadir el coeficiente de correlación como anotación
    "text",
    x = max(rm_armas$`si_medidas_seguridad_whatsappTRUE_hog-com`, na.rm = TRUE) * 0.9,
    y = max(rm_armas$`si_presencia_peleas_con_armas_en_barrioTRUE_hog-com`, na.rm = TRUE) * 0.9,
    label = paste("correlación (r pearson) =", round(
      cor(
        rm_armas$`si_medidas_seguridad_whatsappTRUE_hog-com`,
        rm_armas$`si_presencia_peleas_con_armas_en_barrioTRUE_hog-com`,
        use = "complete.obs"
      ),
      2
    )),
    hjust = 1,
    size = 5,
    fontface = "bold"
  )

violencia_whatsapp


# rm_armas$`confia_muchobastante_policiasTRUE_hog-com` en X y rm_armas$`si_presencia_balacerasTRUE_hog-com` en Y, color = categoria_ips
confianza_balaceras <- rm_armas %>%
  filter(!is.na(`confia_muchobastante_policiasTRUE_hog-com`) & !is.na(`si_presencia_balacerasTRUE_hog-com`)) %>%
  ggplot(
    aes(
      x = `confia_muchobastante_policiasTRUE_hog-com`,
      y = `si_presencia_balacerasTRUE_hog-com`,
      color = categoria_ips,
      label = comuna
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkgrey", alpha = 0.3) +
  geom_text_repel(
    size = 3,
    max.overlaps = 10,
    box.padding = 0.5
  ) +
  scale_color_manual(values = ips_colors) +
  labs(
    title = "Relación entre confianza en policias (Carabineros o PDI) y presencia de balaceras",
    subtitle = "Comunas de RM 2023",
    x = "Porcentaje de Hogares que confía mucho/bastante en Policías (%)",
    y = "Porcentaje de Hogares que declara Presencia de Balaceras en barrio (%)",
    color = "Categoría IPS",
    caption = "Nota: línea de tendencia se calcula con regresión lineal (geom_smooth con method = 'lm' en R)"
  ) +
  scale_x_continuous(labels = scales::label_percent(scale = 100)) +
  scale_y_continuous(labels = scales::label_percent(scale = 100)) +
  theme_minimal() +
  annotate(# Añadir el coeficiente de correlación como anotación
    "text",
    x = max(rm_armas$`confia_muchobastante_policiasTRUE_hog-com`, na.rm = TRUE) * 0.9,
    y = max(rm_armas$`si_presencia_balacerasTRUE_hog-com`, na.rm = TRUE) * 0.9,
    label = paste("correlación (r pearson) =", round(
      cor(
        rm_armas$`confia_muchobastante_policiasTRUE_hog-com`,
        rm_armas$`si_presencia_balacerasTRUE_hog-com`,
        use = "complete.obs"
      ),
      2
    )),
    hjust = 1,
    size = 5,
    fontface = "bold"
  )

confianza_balaceras


# rm_armas$tasa_cada100k_total_inscritas en X y rm_armas$`si_medidas_seguridad_whatsappTRUE_hog-com` en Y, color = categoria_ips
armas_whatsapp <- rm_armas %>%
  filter(!is.na(tasa_cada100k_total_inscritas) & !is.na(`si_medidas_seguridad_whatsappTRUE_hog-com`)) %>%
  ggplot(
    aes(
      x = tasa_cada100k_total_inscritas,
      y = `si_medidas_seguridad_whatsappTRUE_hog-com`,
      color = categoria_ips,
      label = comuna
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkgrey", alpha = 0.3) +
  geom_text_repel(
    size = 3,
    max.overlaps = 10,
    box.padding = 0.5
  ) +
  scale_color_manual(values = ips_colors) +
  labs(
    title = "Relación entre tasa cada 100.000 habitantes de armas inscritas y uso de WhatsApp para seguridad",
    subtitle = "Comunas de RM 2023",
    x = "Tasa cada 100.000 habitantes de Armas Inscritas (2024)",
    y = "Porcentaje de Hogares que usa WhatsApp para Seguridad (%)",
    color = "Categoría IPS",
    caption = "Nota: línea de tendencia se calcula con regresión lineal (geom_smooth con method = 'lm' en R)"
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_percent(scale = 100)) +
  theme_minimal() +
  annotate(# Añadir el coeficiente de correlación como anotación
    "text",
    x = max(rm_armas$tasa_cada100k_total_inscritas, na.rm = TRUE) * 0.9,
    y = max(rm_armas$`si_medidas_seguridad_whatsappTRUE_hog-com`, na.rm = TRUE) * 0.9,
    label = paste("correlación (r pearson) =", round(
      cor(
        rm_armas$tasa_cada100k_total_inscritas,
        rm_armas$`si_medidas_seguridad_whatsappTRUE_hog-com`,
        use = "complete.obs"
      ),
      2
    )),
    hjust = 1,
    size = 5,
    fontface = "bold"
  )

armas_whatsapp


# rm_armas$`si_presencia_pandillasTRUE_hog-com` en X y rm_armas$tasa_cada100k_total_inscritas en Y, color = categoria_ips
armas_pandillas <- rm_armas %>%
  filter(!is.na(tasa_cada100k_total_inscritas) & !is.na(`si_presencia_pandillasTRUE_hog-com`)) %>%
  ggplot(
    aes(
      x = `si_presencia_pandillasTRUE_hog-com`,
      y = tasa_cada100k_total_inscritas,
      color = categoria_ips,
      label = comuna
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkgrey", alpha = 0.3) +
  geom_text_repel(
    size = 3,
    max.overlaps = 10,
    box.padding = 0.5
  ) +
  scale_color_manual(values = ips_colors) +
  labs(
    title = "Relación entre tasa cada 100.000 habitantes de armas inscritas y presencia de pandillas",
    subtitle = "Comunas de RM 2023",
    x = "Porcentaje de Hogares que declara Presencia de Pandillas (%)",
    y = "Tasa cada 100.000 habitantes de Armas Inscritas (2024)",
    color = "Categoría IPS",
    caption = "Nota: línea de tendencia se calcula con regresión lineal (geom_smooth con method = 'lm' en R)"
  ) +
  scale_x_continuous(labels = scales::label_percent(scale = 100)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  annotate(# Añadir el coeficiente de correlación como anotación
    "text",
    x = max(rm_armas$`si_presencia_pandillasTRUE_hog-com`, na.rm = TRUE) * 0.9,
    y = max(rm_armas$tasa_cada100k_total_inscritas, na.rm = TRUE) * 0.9,
    label = paste("correlación (r pearson) =", round(
      cor(
        rm_armas$`si_presencia_pandillasTRUE_hog-com`,
        rm_armas$tasa_cada100k_total_inscritas,
        use = "complete.obs"
      ),
      2
    )),
    hjust = 1,
    size = 5,
    fontface = "bold"
  )

armas_pandillas


# rm_armas$acceso_controladoTRUE_hog-com en X y rm_armas$tasa_cada100k_total_inscritas en Y, color = categoria_ips
acceso_armas <- rm_armas %>%
  filter(!is.na(tasa_cada100k_total_inscritas) & !is.na(`acceso_controladoTRUE_hog-com`)) %>%
  ggplot(
    aes(
      x = `acceso_controladoTRUE_hog-com`,
      y = tasa_cada100k_total_inscritas,
      color = categoria_ips,
      label = comuna
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkgrey", alpha = 0.3) +
  geom_text_repel(
    size = 3,
    max.overlaps = 10,
    box.padding = 0.5
  ) +
  scale_color_manual(values = ips_colors) +
  labs(
    title = "Relación entre acceso controlado a la vivienda y tasa cada 100.000 habitantes de armas inscritas",
    subtitle = "Comunas de RM 2023",
    x = "Porcentaje de Hogares con Acceso Controlado (%)",
    y = "Tasa cada 100.000 habitantes de Armas Inscritas (2024)",
    color = "Categoría IPS",
    caption = "Nota: línea de tendencia se calcula con regresión lineal (geom_smooth con method = 'lm' en R)"
  ) +
  scale_x_continuous(labels = scales::label_percent(scale = 100)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  annotate(# Añadir el coeficiente de correlación como anotación
    "text",
    x = max(rm_armas$`acceso_controladoTRUE_hog-com`, na.rm = TRUE) * 0.9,
    y = max(rm_armas$tasa_cada100k_total_inscritas, na.rm = TRUE) * 0.9,
    label = paste("correlación (r pearson) =", round(
      cor(
        rm_armas$`acceso_controladoTRUE_hog-com`,
        rm_armas$tasa_cada100k_total_inscritas,
        use = "complete.obs"
      ),
      2
    )),
    hjust = 1,
    size = 5,
    fontface = "bold"
  )

acceso_armas


# rm_armas$`si_presencia_balacerasTRUE_hog-com` en X y rm_armas$tasa_cada100k_total_inscritas en Y, color = categoria_ips
balaceras_armas <- rm_armas %>%
  filter(!is.na(tasa_cada100k_total_inscritas) & !is.na(`si_presencia_balacerasTRUE_hog-com`)) %>%
  ggplot(
    aes(
      x = `si_presencia_balacerasTRUE_hog-com`,
      y = tasa_cada100k_total_inscritas,
      color = categoria_ips,
      label = comuna
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkgrey", alpha = 0.3) +
  geom_text_repel(
    size = 3,
    max.overlaps = 10,
    box.padding = 0.5
  ) +
  scale_color_manual(values = ips_colors) +
  labs(
    title = "Relación entre presencia de balaceras y tasa cada 100.000 habitantes de armas inscritas",
    subtitle = "Comunas de RM 2023",
    x = "Porcentaje de Hogares que declara Presencia de Balaceras en barrio (%)",
    y = "Tasa cada 100.000 habitantes de Armas Inscritas (2024)",
    color = "Categoría IPS",
    caption = "Nota: línea de tendencia se calcula con regresión lineal (geom_smooth con method = 'lm' en R)"
  ) +
  scale_x_continuous(labels = scales::label_percent(scale = 100)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  annotate(# Añadir el coeficiente de correlación como anotación
    "text",
    x = max(rm_armas$`si_presencia_balacerasTRUE_hog-com`, na.rm = TRUE) * 0.9,
    y = max(rm_armas$tasa_cada100k_total_inscritas, na.rm = TRUE) * 0.9,
    label = paste("correlación (r pearson) =", round(
      cor(
        rm_armas$`si_presencia_balacerasTRUE_hog-com`,
        rm_armas$tasa_cada100k_total_inscritas,
        use = "complete.obs"
      ),
      2
    )),
    hjust = 1,
    size = 5,
    fontface = "bold"
  )

balaceras_armas

# rm_armas$`confia_muchobastante_policiasTRUE_hog-com` en X y rm_armas$`si_medidas_seguridad_whatsappTRUE_hog-com` en Y, color = categoria_ips
policias_whatsapp <- rm_armas %>%
  filter(!is.na(`confia_muchobastante_policiasTRUE_hog-com`) & !is.na(`si_medidas_seguridad_whatsappTRUE_hog-com`)) %>%
  ggplot(
    aes(
      x = `confia_muchobastante_policiasTRUE_hog-com`,
      y = `si_medidas_seguridad_whatsappTRUE_hog-com`,
      color = categoria_ips,
      label = comuna
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkgrey", alpha = 0.3) +
  geom_text_repel(
    size = 3,
    max.overlaps = 10,
    box.padding = 0.5
  ) +
  scale_color_manual(values = ips_colors) +
  labs(
    title = "Relación entre confianza en policías y medidas de seguridad por WhatsApp",
    subtitle = "Comunas de RM 2023",
    x = "Confianza en Policías (%)",
    y = "Medidas de Seguridad por WhatsApp (%)",
    color = "Categoría IPS",
    caption = "Nota: línea de tendencia se calcula con regresión lineal (geom_smooth con method = 'lm' en R)"
  ) +
  scale_x_continuous(labels = scales::label_percent(scale = 100)) +
  scale_y_continuous(labels = scales::label_percent(scale = 100)) +
  theme_minimal() +
  annotate(# Añadir el coeficiente de correlación como anotación
    "text",
    x = max(rm_armas$`confia_muchobastante_policiasTRUE_hog-com`, na.rm = TRUE) * 0.9,
    y = max(rm_armas$`si_medidas_seguridad_whatsappTRUE_hog-com`, na.rm = TRUE) * 0.9,
    label = paste("correlación (r pearson) =", round(
      cor(
        rm_armas$`confia_muchobastante_policiasTRUE_hog-com`,
        rm_armas$`si_medidas_seguridad_whatsappTRUE_hog-com`,
        use = "complete.obs"
      ),
      2
    )),
    hjust = 1,
    size = 5,
    fontface = "bold"
  )

policias_whatsapp

# Save plots high resolution
ggsave("figures/armas_rm/vivienda_carente_homicidios.png", vivienda_carente_homicidios, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("figures/armas_rm/acceso_rejas.png", acceso_rejas, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("figures/armas_rm/ingreso_armas.png", ingreso_armas, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("figures/armas_rm/acceso_balaceras.png", acceso_balaceras, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("figures/armas_rm/confianza_acceso.png", confianza_acceso, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("figures/armas_rm/violencia_whatsapp.png", violencia_whatsapp, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("figures/armas_rm/confianza_balaceras.png", confianza_balaceras, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("figures/armas_rm/armas_whatsapp.png", armas_whatsapp, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("figures/armas_rm/armas_pandillas.png", armas_pandillas, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("figures/armas_rm/acceso_armas.png", acceso_armas, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("figures/armas_rm/balaceras_armas.png", balaceras_armas, width = 12, height = 8, dpi = 300, bg = "white")

