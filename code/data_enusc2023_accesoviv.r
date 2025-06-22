# 1. Cargar librerías
library(pacman)
p_load(tidyverse, dplyr, haven, janitor, readxl, survey, srvyr)

# 2. Leer codebook y mapear nombres largos
vars_23  <- read_excel("CEAD/codes_enusc.xlsx") %>%
  select(long_rec = 1, short = "2023") %>%
  filter(!is.na(short))
name_map <- setNames(vars_23$long_rec, vars_23$short)

# 3. Leer datos .sav, renombrar y limpiar
df <- read_sav("data_processed/enusc_2023/enusc2023_accesoviv.sav", col_select = all_of(names(name_map))) %>% # BDD especial, contiene pregunta sobre acceso vivienda
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
  acceso_casas_controlado = I(acceso_viv == "Casa en condominio con acceso controlado"),
  acceso_directo = I(acceso_viv %in% c("Casa con acceso directo", "Departamento con acceso directo")),
  acceso_controlado = I(acceso_viv %in% c("Departamento en condominio con acceso controlado", 
                                          "Casa en condominio con acceso controlado", 
                                          "Casa sin acceso directo (no necesariamente en condominio)")),
  acceso_indeterminado = I(acceso_viv %in% c("No se puede determinar", "Sin dato"))
)

# 5b. Crear indicadores binarios hogares / comunas
design_hogares <- update(design_hogares,
  acceso_casas_controlado = I(acceso_viv == "Casa en condominio con acceso controlado"),
  acceso_directo = I(acceso_viv %in% c("Casa con acceso directo", "Departamento con acceso directo")),
  acceso_controlado = I(acceso_viv %in% c("Departamento en condominio con acceso controlado", 
                                          "Casa en condominio con acceso controlado", 
                                          "Casa sin acceso directo (no necesariamente en condominio)")),
  acceso_indeterminado = I(acceso_viv %in% c("No se puede determinar", "Sin dato"))
)

# Definir las variables binarias para el análisis
vars_binarias <- c(
  "acceso_casas_controlado",
  "acceso_directo",
  "acceso_controlado",
  "acceso_indeterminado"
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