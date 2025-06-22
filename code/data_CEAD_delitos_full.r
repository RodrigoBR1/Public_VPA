# Librerías
library(pacman)
p_load(tidyverse, readxl, RCurl, rvest, purrr, janitor, chilemapas)

# Metadatos importantes
familia_delitos <- c("Delitos violentos", "Delitos asociados a armas", "Delitos asociados a drogas", "Delitos contra la propiedad no violentos")

grupo_delitos <- c("Homicidios y femicidios", "Robos con violencia o intimidación", "Robo por sorpresa", "Lesiones graves o gravísimas", 
                   "Lesiones menos graves", "Amenazas con armas", "Amenazas o riña", "Crímenes y simples delitos ley de armas",
                   "Porte de arma cortante o punzante", "Robos en lugares habitados y no habitados", "Otros robos con fuerza en las cosas")

subgrupo_delitos <- c("Robo con homicidio", "Otros homicidios", "Robos con violencia o intimidación", "Robo violento de vehículo motorizado",
                       "Robo por sorpresa", "Lesiones graves o gravísimas", "Lesiones menos graves", "Amenazas con armas", 
                       "Amenazas o riña", "Disparo injustificado", "Porte / posesión de armas o explosivos", 
                       "Otras infracciones a la ley de armas", "Porte de arma cortante o punzante",
                       "Robo en lugar habitado", "Otros robos con fuerza en las cosas")

# Funciones para lectura y procesamiento de datos desde CEAD, 
# cortesía de Bastián Olea H (Mg. Sociología UC) https://bastianolea.rbind.io/blog/tutorial_delitos_cead/
cead_generar_request <- function(anio_elegido, comuna_numero) {

  # delitos a solicitar
  request_delitos <- "&familia%5B%5D=1&familia%5B%5D=3&familia%5B%5D=4&familia_nombres%5B%5D=Delitos+violentos&familia_nombres%5B%5D=Delitos+asociados+a+armas&familia_nombres%5B%5D=Delitos+contra+la+propiedad+no+violentos&grupo%5B%5D=101&grupo%5B%5D=103&grupo%5B%5D=104&grupo%5B%5D=105&grupo%5B%5D=106&grupo%5B%5D=109&grupo%5B%5D=110&grupo%5B%5D=301&grupo%5B%5D=302&grupo%5B%5D=401&grupo%5B%5D=403&grupo_nombres%5B%5D=Homicidios+y+femicidios&grupo_nombres%5B%5D=Robos+con+violencia+o+intimidaci%C3%B3n&grupo_nombres%5B%5D=Robo+por+sorpresa&grupo_nombres%5B%5D=Lesiones+graves+o+grav%C3%ADsimas&grupo_nombres%5B%5D=Lesiones+menos+graves&grupo_nombres%5B%5D=Amenazas+con+armas&grupo_nombres%5B%5D=Amenazas+o+ri%C3%B1a&grupo_nombres%5B%5D=Cr%C3%ADmenes+y+simples+delitos+ley+de+armas&grupo_nombres%5B%5D=Porte+de+arma+cortante+o+punzante&grupo_nombres%5B%5D=Robos+en+lugares+habitados+y+no+habitados&grupo_nombres%5B%5D=Otros+robos+con+fuerza+en+las+cosas&subgrupo%5B%5D=10105&subgrupo%5B%5D=10107&subgrupo%5B%5D=10301&subgrupo%5B%5D=10302&subgrupo%5B%5D=10401&subgrupo%5B%5D=10501&subgrupo%5B%5D=10601&subgrupo%5B%5D=10901&subgrupo%5B%5D=11001&subgrupo%5B%5D=30101&subgrupo%5B%5D=30102&subgrupo%5B%5D=30103&subgrupo%5B%5D=30201&subgrupo%5B%5D=40101&subgrupo%5B%5D=40301&subgrupo_nombres%5B%5D=Robo+con+homicidio&subgrupo_nombres%5B%5D=Otros+homicidios&subgrupo_nombres%5B%5D=Robos+con+violencia+o+intimidaci%C3%B3n&subgrupo_nombres%5B%5D=Robo+violento+de+veh%C3%ADculo+motorizado&subgrupo_nombres%5B%5D=Robo+por+sorpresa&subgrupo_nombres%5B%5D=Lesiones+graves+o+grav%C3%ADsimas&subgrupo_nombres%5B%5D=Lesiones+menos+graves&subgrupo_nombres%5B%5D=Amenazas+con+armas&subgrupo_nombres%5B%5D=Amenazas+o+ri%C3%B1a&subgrupo_nombres%5B%5D=Disparo+injustificado&subgrupo_nombres%5B%5D=Porte+%2F+posesi%C3%B3n+de+armas+o+explosivos&subgrupo_nombres%5B%5D=Otras+infracciones+a+la+ley+de+armas&subgrupo_nombres%5B%5D=Porte+de+arma+cortante+o+punzante&subgrupo_nombres%5B%5D=Robo+en+lugar+habitado&subgrupo_nombres%5B%5D=Otros+robos+con+fuerza+en+las+cosas"


  # todos los meses del año
  # request_fechas = "&trimestre%5B%5D=1&trimestre%5B%5D=2&trimestre%5B%5D=3&trimestre%5B%5D=4&mes%5B%5D=1&mes%5B%5D=2&mes%5B%5D=3&mes%5B%5D=4&mes%5B%5D=5&mes%5B%5D=6&mes%5B%5D=7&mes%5B%5D=8&mes%5B%5D=9&mes%5B%5D=10&mes%5B%5D=11&mes%5B%5D=12&mes_nombres%5B%5D=Enero&mes_nombres%5B%5D=Febrero&mes_nombres%5B%5D=Marzo&mes_nombres%5B%5D=Abril&mes_nombres%5B%5D=Mayo&mes_nombres%5B%5D=Junio&mes_nombres%5B%5D=Julio&mes_nombres%5B%5D=Agosto&mes_nombres%5B%5D=Septiembre&mes_nombres%5B%5D=Octubre&mes_nombres%5B%5D=Noviembre&mes_nombres%5B%5D=Diciembre"

  # unir fragmentos en una sola request
  paste0("medida=2&tipoVal=2", # medida = 2 equivale a tasa por cada 100 000 habitantes, tipoVal = 2 son 'Casos policiales' (delitos conocidos por el Estado)
    "&anio%5B%5D=", anio_elegido, # Año a solicitar
    request_delitos, # Delitos a solicitar
    "&comuna%5B%5D=", comuna_numero, # Comuna a solicitar (según código)
    "&seleccion=2&descarga=false" # Se descargan datos en formato tasa de Y cada 100 000 habitantes
  )
}

cead_realizar_request <- function(request) {
  RCurl::getURL(url = "https://cead.spd.gov.cl/wp-content/themes/gobcl-wp-master/data/get_estadisticas_delictuales.php",
         postfields = request,
         httpheader = c(Connection = "close", 
                        'Content-Type' = "application/x-www-form-urlencoded; charset=UTF-8", 
                        'Content-length' = nchar(request)
         )
  )
}



# Obtención de datos de prueba
request_prueba <- cead_generar_request(anio_elegido = 2023, comuna_numero = 13103) # Año 2023, Cerro Navia (13103)

datos_prueba <- cead_realizar_request(request = request_prueba)

datos_prueba_limpios <- datos_prueba %>%
  rvest::read_html() %>%
  rvest::html_table() %>%
  purrr::pluck(1) %>%
  janitor::row_to_names(1) %>%
  dplyr::rename(delitos = 1,
                anio_elegido = 2)

# Presentar datos de prueba
head(datos_prueba_limpios, 20)
summary(datos_prueba_limpios)

#
##
### Loop para obtener datos de todas las comunas y años lentamente
##
#

# Limpiar memoria
rm(request_prueba, datos_prueba, datos_prueba_limpios)

# Definir años y comunas
anios_elegidos <- 2005:2024 %>% set_names() # 2005:2024 son todos los disponibles
comunas_por_calcular <- chilemapas::codigos_territoriales %>% # Contiene los codigos oficiales de SUBDERE. 
  pull(codigo_comuna) %>% 
  unique() %>% 
  set_names()
nombres_comunas_calculadas <- chilemapas::codigos_territoriales %>% 
  select(codigo_comuna, nombre_comuna, codigo_region, nombre_region)


# Función extra para scrapping largos, añadida por Rodrigo Brito a código original de Bastian Olea

# Estimación de tiempo mínimo
n_comunas <- length(comunas_por_calcular)
n_años <- length(anios_elegidos)
tiempo_estimado_min <- n_comunas * n_años * 2 / 60  # Asumiendo 2 segundos por consulta, convertido a minutos
message("Tiempo estimado mínimo: ", round(tiempo_estimado_min, 1), " minutos (", round(tiempo_estimado_min/60, 2), " horas)")

# Loop
datos_cead <- map(comunas_por_calcular, \(comuna_numero) {
  message("inciando comuna ", comuna_numero)
  
  #loop dentro del loop: para la comuna, por cada año especificado
  data_comuna <- map(anios_elegidos, \(anio) {
    message("año ", anio)
    
    #generar request
    xml.request = cead_generar_request(anio_elegido = anio, 
      comuna_numero)
    
    #obtener datos usando la función de request
    inicio = Sys.time()
    data_año = cead_realizar_request(xml.request)
    final = Sys.time()
    #dar tiempo de espera en base al tiempo que tomó descargar los datos, para no saturar el servidor
    Sys.sleep((final-inicio)*10)
    
    return(data_año)
  })
  return(data_comuna)
})

# Process the nested list to a tidy dataframe
cead_limpiada <- map_df(names(datos_cead), \(comuna_code) {
  # Obtener información de la comuna desde nombres_comunas_calculadas
  nombre_comuna <- nombres_comunas_calculadas %>%
    filter(codigo_comuna == comuna_code) %>%
    pull(nombre_comuna)
  
  message("Procesando comuna ", nombre_comuna, " (", comuna_code, ")")
  
  # Process each year's data for this comuna
  map_df(seq_along(datos_cead[[comuna_code]]), \(year_idx) {
    year_name <- names(datos_cead[[comuna_code]])[year_idx]
    if(is.null(year_name)) year_name <- as.character(anios_elegidos[year_idx])
    
    message("Procesando año ", year_name)
    
    # Extract HTML data for this comuna and year
    html_data <- datos_cead[[comuna_code]][[year_idx]]
    
    tryCatch({
      # Process the HTML data
      tabla <- html_data %>%
        rvest::read_html() %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        janitor::row_to_names(1) %>%
        dplyr::rename(delitos = 1,
                      !!year_name := 2)
      
      # Append 'tabla' to cead_data with comuna and year information
      tabla_limpia <- tabla %>%
        mutate(nombre_comuna = nombre_comuna,
               codigo_comuna = comuna_code,
               anio = year_name) %>%
        select(nombre_comuna, codigo_comuna, anio, delitos,
               cifra = !!as.symbol(year_name))
      
      message("Comuna ", comuna_code, " año ", year_name,
              " procesada correctamente.")
      return(tabla_limpia)
    }, error = function(e) {
      message("Error procesando comuna ", comuna_code,
              " año ", year_name, ": ", e$message)
      tibble() # Return empty tibble on error
    })
  })
})

# Quedarse sólo con subgrupos de delitos
cead_limpiada <- cead_limpiada %>%
  filter(delitos %in% subgrupo_delitos) %>%
  mutate(delitos = factor(delitos, levels = subgrupo_delitos, labels = subgrupo_delitos))

# Guardar datos limpios
write_csv(cead_limpiada, "CEAD/data_delitos/datos_full_tasa_delitos_cada-100k_limpios.csv")
