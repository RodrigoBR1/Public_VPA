# Libraries
library(pacman)
p_load(tidyverse, readxl, readstata13, purrr, stringr, arrow)

# A continuación, código procesamiento CASEN 2022, cortesía de Bastían Olea H (Mg. Sociología UC)
# DESCARGA DE DATA ---------------------------------------------
options(timeout = max(300, getOption("timeout")))

# descargar base
download.file(url = "https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2022/Base%20de%20datos%20Casen%202022%20STATA.dta.zip",
              destfile = "CASEN/data/Casen2022.zip",
              method = "libcurl")

download.file(url = "https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2022/Base%20de%20datos%20provincia%20y%20comuna%20Casen%202022%20STATA.dta.zip",
              destfile = "CASEN/data/Casen2022comunal.zip",
              method = "libcurl")

# descomprimir
unzip(zipfile = "CASEN/data/Casen2022.zip", exdir = "CASEN/data/")
unzip(zipfile = "CASEN/data/Casen2022comunal.zip", exdir = "CASEN/data/")

# eliminar archivos comprimidos
file.remove("CASEN/data/Casen2022.zip")
file.remove("CASEN/data/Casen2022comunal.zip")

# descargar manual
download.file("https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2022/Libro_de_Códigos_Base_Casen_2022.xlsx",
              destfile = "CASEN/manual/Libro_de_Códigos_Base_Casen_2022.xlsx")
# download.file("https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2022/Libro%20de%20codigos%20Base%20de%20datos%20provincia%20y%20comuna%20Casen%202022.xlsx",
#              destfile = "CASEN/manual/Libro_de_codigos_Base_de_datos_provincia_y_comuna_Casen_2022.xlsx")
# ------> venia corrupto, se descarga manualmente desde el sitio web del observatorio

# PROCESAMIENTO DE DATA ---------------------------------------------
#preprocesar ----
message("cargando casen 2022...")

#obtener codigos unicos territoriales desde libro de codigos
cut_comunas <- readxl::read_excel("CASEN/manual/Libro_de_codigos_Base_de_datos_provincia_y_comuna_Casen_2022.xlsx") |> 
  janitor::row_to_names(2) |> 
  janitor::clean_names() |> 
  tidyr::fill(nombre_variable) |> 
  filter(nombre_variable == "comuna") |> 
  select(cut_comuna = valores, comuna = etiquetas_de_valores) |> 
  filter(!is.na(cut_comuna), !is.na(comuna))

#cargar columans de comunas
casen2022comunas <- readstata13::read.dta13("CASEN/data/Base de datos provincia y comuna Casen 2022 STATA.dta", generate.factors = T) |> 
  as_tibble() |> 
  left_join(cut_comunas, join_by(comuna))

#cargar base
casen2022 <- readstata13::read.dta13("CASEN/data/Base de datos Casen 2022 STATA.dta" , generate.factors = T) |> 
  as_tibble()

#unir base con comunas
casen2022_2 <- casen2022 |> 
  left_join(casen2022comunas, join_by(folio, id_persona)) |> 
  select(names(casen2022comunas), everything()) |> 
  select(-id_vivienda, -folio, -id_persona, -cod_upm, -estrato, -varstrat, -varunit, -fecha_entrev)

#guardar
arrow::write_parquet(casen2022_2, "CASEN/data/casen2022.parquet")

# eliminar base de datos
file.remove("CASEN/data/Base de datos Casen 2022 STATA.dta")

# Remover cut_comunas, casen2022comunas, casen2022 y casen2022_2
rm(cut_comunas, casen2022comunas, casen2022, casen2022_2)

# Carga y procesamiento de la base de datos CASEN 2022
#cargar datos
casen <- arrow::read_parquet("CASEN/data/casen2022.parquet")
poblacion <- arrow::read_parquet("CASEN/data/censo_proyecciones_año.parquet")

variables_casen <- c(
  "comuna",
  "region",
  "pco1",                    # jefe de hogar
  "expc",                    # factor de expansión comunal
  "sexo",                    # género
  "ytotcorh",                # Ingreso total del hogar corregido
  "ytotcor",                 # Ingreso total corregido
  "yoprcor",                 # Ingreso ocupación principal
  "ypc",                     # Ingreso total per cápita del hogar corregido
  "ytrabajocor",             # ingreso del trabajo
  "ytrabajocorh",            # ingreso del trabajo del hogar
  "dau",                     # Decil autónomo nacional
  "pobreza",                 # pobreza
  "pobreza_multi_5d",        # pobreza multidimensional
  "activ",                   # actividad
  "contrato",                # Tiene contrato de trabajo
  "hh_d_estado",             # Hogar carente en estado de la vivienda
  "hh_d_servbas",            # Hogar carente en servicios básicos
  "o15"                       # o15. En su trabajo o negocio principal, ¿trabaja como?
)

#filtrar datos
casen2022_comunas <- casen |> 
  select(any_of(variables_casen)) |> 
  mutate(across(where(is.factor), as.character))

  # Procesamiento de estadísticas por comuna
  casen_stats_comuna <- casen2022_comunas %>%
    # Group by comuna
    group_by(comuna, region) %>%
    # Calculate statistics
    summarize(
      # 1. Promedio de variables de ingreso
      promedio_ytotcorh = weighted.mean(as.numeric(ytotcorh), expc, na.rm = TRUE),
      promedio_ytotcor = weighted.mean(as.numeric(ytotcor), expc, na.rm = TRUE),
      promedio_yoprcor = weighted.mean(as.numeric(yoprcor), expc, na.rm = TRUE),
      promedio_ypc = weighted.mean(as.numeric(ypc), expc, na.rm = TRUE),
      promedio_ytrabajocor = weighted.mean(as.numeric(ytrabajocor), expc, na.rm = TRUE),
      promedio_ytrabajocorh = weighted.mean(as.numeric(ytrabajocorh), expc, na.rm = TRUE),
      
      # 2. Porcentaje de población con respuesta distinta a "No pobreza"
      porcentaje_pobreza = weighted.mean(pobreza != "No pobreza", expc, na.rm = TRUE) * 100,
      porcentaje_pobreza_multi = weighted.mean(pobreza_multi_5d != "No pobreza", expc, na.rm = TRUE) * 100,
      
      # 3. Porcentaje de "Ocupados" en activ
      porcentaje_ocupados = weighted.mean(activ == "Ocupados", expc, na.rm = TRUE) * 100,
      
      # 4. Porcentaje de "Sí" en contrato
      porcentaje_contrato = weighted.mean(contrato == "Sí", expc, na.rm = TRUE) * 100,
      
      # 5. Porcentaje de "Carente" en variables hh_d_
      porcentaje_carente_estado = weighted.mean(hh_d_estado == "Carente", expc, na.rm = TRUE) * 100,
      porcentaje_carente_servbas = weighted.mean(hh_d_servbas == "Carente", expc, na.rm = TRUE) * 100
    )

  # 6. Procesamiento especial para o15 (categorías de ocupación)
  # Primero limpiar la variable o15 para eliminar "000. " y reemplazar espacios por "_"
  casen2022_comunas <- casen2022_comunas %>%
    mutate(o15_clean = str_replace(as.character(o15), "^000\\. ", "") %>% 
                       str_replace_all(" ", "_"))

  # Calcular porcentajes por categoría de o15 y convertir a formato ancho
  o15_stats <- casen2022_comunas %>%
    filter(!is.na(o15_clean)) %>%
    group_by(comuna, o15_clean) %>%
    summarize(count = sum(expc, na.rm = TRUE), .groups = "drop") %>%
    group_by(comuna) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    select(-count) %>%
    pivot_wider(names_from = o15_clean, 
                values_from = percentage, 
                names_prefix = "porcentaje_o15_",
                values_fill = 0)

  # Combinar ambos resultados
  casen_stats_final <- casen_stats_comuna %>%
    left_join(o15_stats, by = "comuna")

  # Mismo dataset pero filtrado RM
  casen_stats_rm <- casen_stats_final %>%
    filter(region == "Región Metropolitana de Santiago") %>%
    select(-region)


# Remover casen2022_comunas, casen, o15_stats y casen_stats_comuna
rm(casen2022_comunas, casen, o15_stats, casen_stats_comuna)

# Guardar los resultados
write_parquet(casen_stats_final, "data_processed/casen/casen2022_stats_comuna.parquet")
write_parquet(casen_stats_rm, "data_processed/casen/casen2022_stats_rm.parquet")

