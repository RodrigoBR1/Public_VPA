# 1. Cargar librerías
library(pacman)
p_load(tidyverse, dplyr, haven, janitor, readxl, survey, srvyr)

# 2. Leer codebook y mapear nombres largos
vars_23  <- read_excel("CEAD/codes_enusc.xlsx") %>%
  select(long_rec = 1, short = "2023") %>%
  filter(long_rec != "ACCESO_VIV") %>% # Excluir variable de acceso a la vivienda
  filter(!is.na(short))
name_map <- setNames(vars_23$long_rec, vars_23$short)

# 3. Leer datos .sav, renombrar y limpiar
df <- read_sav("CEAD/data_ENUSC/sav/2023.sav", col_select = all_of(names(name_map))) %>%
  rename_with(~ name_map[.x], .cols = everything()) %>%
  clean_names() %>%
  filter(kish == 1) %>%
  # Convertir variables etiquetadas a factores
  mutate(across(
    where(is.labelled),
    ~ haven::as_factor(., levels = "labels")
  )) %>%
  # Recodificar fuente_info_comuna en 6 categorías según instrucciones
  mutate(
      fuente_info_comuna = case_when(
          fuente_info_comuna %in% c("2", "5", "8") ~ "experiencia_o_palabra_cercana",
          fuente_info_comuna %in% c("7", "10") ~ "television",
          fuente_info_comuna %in% c("1", "3", "6") ~ "radio y periodico",
          fuente_info_comuna == "4" ~ "RRSS digitales",
          fuente_info_comuna == "9" ~ "otros",
          fuente_info_comuna %in% c("11", "12") ~ "NS/NR",
          TRUE ~ as.character(fuente_info_comuna)
      ),
      comuna = str_replace_all(comuna, c(
        "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U",
        "á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u",
        "Ü" = "U", "ü" = "u", "Ñ" = "N", "ñ" = "n"
      ))
  )

# Extra. Revisar variables en df
names(df)

# 4a. Definir diseño muestral personas / comunas
to_options <- options(survey.lonely.psu = "certainty")
design <- svydesign(
  ids      = ~ conglomerado,
  strata   = ~ strata,
  weights  = ~ weight_fact_pers_com,
  data     = df,
  nest     = TRUE
)

#4b. Definir diseño muestral hogares / comunas
design_hogares <- svydesign(
  ids      = ~ conglomerado,
  strata   = ~ strata,
  weights  = ~ weight_fact_hog_com,
  data     = df,
  nest     = TRUE
)

# 5a. Crear indicadores binarios personas / comunas
design <- update(design,
  si_presencia_pandillas = I(presencia_pandillas %in% c("Ocasionalmente", "Casi siempre", "Siempre")),
  si_presencia_violencia_entre_vecinos = I(presencia_amenazas_peleas_entre_vecinos %in% c("Ocasionalmente", "Casi siempre", "Siempre")),
  si_presencia_peleas_con_armas_en_barrio = I(presencia_peleas_con_armas_en_barrio %in% c("Ocasionalmente", "Casi siempre", "Siempre")),
  si_presencia_balaceras = I(presencia_balaceras_en_barrio %in% c("Ocasionalmente", "Casi siempre", "Siempre")),
  si_medida_seguridad_rejas = I(medidas_seguridad_rejas == "Sí"),
  si_medida_seguridad_cerco = I(medidas_seguridad_cerco == "Sí"),
  si_medidas_seguridad_proteccion = I(medidas_seguridad_proteccion == "Sí"),
  si_medidas_seguridad_whatsapp = I(medidas_seguridad_whatsapp_intercambia_telefono == "Sí"),
  si_arma_en_casa = I(arma_en_casa == "Sí"),

  # — indicadores de victimización armada (personas) —
  si_robo_vehiculo_mano_armada                  = I(robo_vehiculo_mano_armada == "Si"),
  si_robo_vivienda_sustraen_arma_de_fuego        = I(robo_vivienda_sustraen_arma_de_fuego == "Si"),
  si_robo_con_violencia_mano_armada             = I(robo_con_violencia_mano_armada == "Si"),
  si_robo_por_sorpresa_sustraen_arma_de_fuego   = I(robo_por_sorpresa_sustraen_arma_de_fuego == "Si"),
  si_hurto_sustraen_arma_de_fuego               = I(hurto_sustraen_arma_de_fuego == "Si"),
  si_agresion_mano_armada                       = I(agresion_mano_armada == "Si"),
  si_amenaza_mano_armada                        = I(amenaza_mano_armada == "Si"),
  si_extorsion_mano_armada                      = I(extorsion_mano_armada == "Si"),

  # — confianza en las policias
  confia_muchobastante_carabineros = I(confianza_cch %in% c("Mucho confianza", "Bastante confianza")),
  confia_muchobastante_pdi = I(confianza_pdi %in% c("Mucho confianza", "Bastante confianza")),
  confia_muchobastante_policias = rowSums(cbind(
    confia_muchobastante_carabineros,
    confia_muchobastante_pdi
  ), na.rm = TRUE) > 0,

  # — índice normalizado 0–1 para personas, objetivo es identificar casos donde alguno de los indicadores de victimización armada sea "Si" (cada caso cuenta como 1) —
  victimizacion_armada_de_algun_tipo = rowSums(cbind(
    si_robo_vehiculo_mano_armada,
    si_robo_con_violencia_mano_armada,
    si_hurto_sustraen_arma_de_fuego,
    si_agresion_mano_armada,
    si_amenaza_mano_armada,
    si_extorsion_mano_armada
  ), na.rm = TRUE) > 0,

  # — índice normalizado 0–1 de robo de armas, objetivo es identificar casos donde alguno de los indicadores de robo de armas sea "Si" (cada caso cuenta como 1) —
  robo_armas_de_algun_tipo = rowSums(cbind(
    si_robo_vivienda_sustraen_arma_de_fuego,
    si_robo_por_sorpresa_sustraen_arma_de_fuego,
    si_hurto_sustraen_arma_de_fuego
  ), na.rm = TRUE) > 0
)

# 5b. Crear indicadores binarios hogares / comunas
design_hogares <- update(design_hogares,
  # — indicadores que ya tenías —
  si_presencia_pandillas = I(presencia_pandillas %in% c("Ocasionalmente", "Casi siempre", "Siempre")),
  si_presencia_violencia_entre_vecinos = I(presencia_amenazas_peleas_entre_vecinos %in% c("Ocasionalmente", "Casi siempre", "Siempre")),
  si_presencia_peleas_con_armas_en_barrio = I(presencia_peleas_con_armas_en_barrio %in% c("Ocasionalmente", "Casi siempre", "Siempre")),
  si_presencia_balaceras = I(presencia_balaceras_en_barrio %in% c("Ocasionalmente", "Casi siempre", "Siempre")),
  si_medida_seguridad_rejas = I(medidas_seguridad_rejas == "Sí"),
  si_medida_seguridad_cerco = I(medidas_seguridad_cerco == "Sí"),
  si_medidas_seguridad_proteccion = I(medidas_seguridad_proteccion == "Sí"),
  si_medidas_seguridad_whatsapp = I(medidas_seguridad_whatsapp_intercambia_telefono == "Sí"),
  si_arma_en_casa = I(arma_en_casa == "Sí"),

  # — indicadores de victimización armada (hogares) —
  si_robo_vehiculo_mano_armada                  = I(robo_vehiculo_mano_armada == "Si"),
  si_robo_vivienda_sustraen_arma_de_fuego        = I(robo_vivienda_sustraen_arma_de_fuego == "Si"),
  si_robo_con_violencia_mano_armada             = I(robo_con_violencia_mano_armada == "Si"),
  si_robo_por_sorpresa_sustraen_arma_de_fuego   = I(robo_por_sorpresa_sustraen_arma_de_fuego == "Si"),
  si_hurto_sustraen_arma_de_fuego               = I(hurto_sustraen_arma_de_fuego == "Si"),
  si_agresion_mano_armada                       = I(agresion_mano_armada == "Si"),
  si_amenaza_mano_armada                        = I(amenaza_mano_armada == "Si"),
  si_extorsion_mano_armada                      = I(extorsion_mano_armada == "Si"),

  # — confianza en las policias
  confia_muchobastante_carabineros = I(confianza_cch %in% c("Mucho confianza", "Bastante confianza")),
  confia_muchobastante_pdi = I(confianza_pdi %in% c("Mucho confianza", "Bastante confianza")), 
  confia_muchobastante_policias = rowSums(cbind(
    confia_muchobastante_carabineros,
    confia_muchobastante_pdi
  ), na.rm = TRUE) > 0,

  # — índice normalizado 0–1 para personas, objetivo es identificar casos donde alguno de los indicadores de victimización armada sea "Si" (cada caso cuenta como 1) —
   victimizacion_armada_de_algun_tipo = rowSums(cbind(
    si_robo_vehiculo_mano_armada,
    si_robo_con_violencia_mano_armada,
    si_hurto_sustraen_arma_de_fuego,
    si_agresion_mano_armada,
    si_amenaza_mano_armada,
    si_extorsion_mano_armada
  ), na.rm = TRUE) > 0,

  # — índice normalizado 0–1 de robo de armas, objetivo es identificar casos donde alguno de los indicadores de robo de armas sea "Si" (cada caso cuenta como 1) —
  robo_armas_de_algun_tipo = rowSums(cbind(
    si_robo_vivienda_sustraen_arma_de_fuego,
    si_robo_por_sorpresa_sustraen_arma_de_fuego,
    si_hurto_sustraen_arma_de_fuego
  ), na.rm = TRUE) > 0
)

# Definir las variables binarias para el análisis
vars_binarias <- c(
  "si_presencia_pandillas",
  "si_presencia_violencia_entre_vecinos",
  "si_presencia_peleas_con_armas_en_barrio",
  "si_presencia_balaceras",
  "si_medida_seguridad_rejas",
  "si_medida_seguridad_cerco",
  "si_medidas_seguridad_proteccion",
  "si_medidas_seguridad_whatsapp",
  "si_arma_en_casa",
  "si_robo_vehiculo_mano_armada",
  "si_robo_vivienda_sustraen_arma_de_fuego",
  "si_robo_con_violencia_mano_armada",
  "si_robo_por_sorpresa_sustraen_arma_de_fuego",
  "si_hurto_sustraen_arma_de_fuego",
  "si_agresion_mano_armada",
  "si_amenaza_mano_armada",
  "si_extorsion_mano_armada",
  "victimizacion_armada_de_algun_tipo",
  "robo_armas_de_algun_tipo",
  "confia_muchobastante_carabineros",
  "confia_muchobastante_pdi",
  "confia_muchobastante_policias"
)

# Calculo de gl por comuna
gl_por_comuna <- df %>%
  select(comuna, conglomerado, strata) %>%
  distinct() %>%
  group_by(comuna) %>%
  summarise(
    clusters = n_distinct(conglomerado),
    estratos = n_distinct(strata),
    gl = clusters - estratos,
    .groups = "drop"
  )


# 6a. Calcular proporciones por persona / comuna con error estándar

# Lista vacía para almacenar dataframes de resultados
resultados_comuna <- list()

# Función para calcular proporciones por persona / comuna con intervalos de confianza, para todas las variables binarias

for(var in vars_binarias) {
  formula <- as.formula(paste0("~", var))
  
  # Calcular proporciones con error estándar directamente
  prop_comuna <- survey::svyby(
    formula, 
    ~comuna, 
    design, 
    survey::svymean, 
    vartype = "se", 
    keep.names = FALSE
  )
  
  # Guardar el resultado como dataframe en la lista 'resultados_comuna'
  resultados_comuna[[var]] <- as.data.frame(prop_comuna)
  
  # Opcional: mostrar progreso
  cat("Procesada variable:", var, "\n")
}

# 6b. Calcular proporciones por hogar / comuna con error estándar
# Lista vacía para almacenar dataframes de resultados
resultados_comuna_hogares <- list()
# Función para calcular proporciones por hogar / comuna con intervalos de confianza, para todas las variables binarias
for(var in vars_binarias) {
  formula <- as.formula(paste0("~", var))
  
  # Calcular proporciones con error estándar directamente
  prop_comuna_hogares <- survey::svyby(
    formula, 
    ~comuna, 
    design_hogares, 
    survey::svymean, 
    vartype = "se", 
    keep.names = FALSE
  )
  
  # Guardar el resultado como dataframe en la lista 'resultados_comuna_hogares'
  resultados_comuna_hogares[[var]] <- as.data.frame(prop_comuna_hogares)
  
  # Opcional: mostrar progreso
  cat("Procesada variable (hogares):", var, "\n")
}

# 7a. Guardar cada dataframe en la lista como un archivo CSV para personas / comunas
for (var in names(resultados_comuna)) {
  # Crear el nombre del archivo CSV
  file_name <- paste0("data_processed/enusc_2023/pers-com_", var, ".csv")

  # Guardar el dataframe como CSV
  write.csv(resultados_comuna[[var]], file = file_name, row.names = FALSE)
  
  cat("Guardado:", file_name, "\n")
}

# 7b. Guardar cada dataframe en la lista como un archivo CSV para hogares / comunas
for (var in names(resultados_comuna_hogares)) {
  # Crear el nombre del archivo CSV
  file_name <- paste0("data_processed/enusc_2023/hog-com_", var, ".csv")

  # Guardar el dataframe como CSV
  write.csv(resultados_comuna_hogares[[var]], file = file_name, row.names = FALSE)
  
  cat("Guardado (hogares):", file_name, "\n")
}
