# Libraries
library(pacman)
p_load(tidyverse, arrow, openxlsx)

# ------------------- PENDIENTE ADAPTAR A BDD FULL CHILE ALL YEARS -------------------

# Load delitos conocidos por la policía o 'Caso policiales' (denuncias + flagrancias) RM 2023
delitos <- read_csv("data_processed/delitos/datos_23_tasa_delitos_cada-100000_limpios.csv") %>% # 'Casos policiales' (denuncias + flagrancias) cada 100 000 habitantes
                    select(-anio) %>%
                    pivot_wider(
                      names_from = delitos, 
                      values_from = cifra, 
                      names_prefix = "tasa_100k_"
                    )

# Load ENUSC files RM 2023
pattern <- "(pers-com|hog-com)_(si_presencia_balaceras|si_arma_en_casa)"
selected <- grep(pattern, list.files("data_processed/enusc_2023", pattern = "*.csv", full.names = TRUE), value = TRUE)
enusc_2023 <- setNames(lapply(selected, read.csv), tools::file_path_sans_ext(basename(selected)))

# Load DGMN files RM 2023
armas_comunal_rm <- read.csv("DGMN/tablas/data_armas_comuna.csv") %>% filter(año == 2023 & cod_region == 13) #DGMN - Obtenidas mediante transparencia - TABLA BASE

# Load CASEN files RM 2022
casen_stats_rm <- read_parquet("data_processed/casen/casen2022_stats_rm.parquet")

# Load Indice de Prioridad Social (IPS) RM 2022
ips_rm <- read.xlsx("data_processed/ips/IPS_2022.xlsx")

# Combine into a single dataframe, join by "comuna" in 'armas_comunal_rm'
rm_armas <- armas_comunal_rm %>%
    left_join(delitos, by = c("comuna" = "nombre_comuna")) %>%
    left_join(casen_stats_rm, by = "comuna") %>%
    left_join(ips_rm, by = c("comuna" = "Comuna")) %>%
    left_join(enusc_2023$`pers-com_si_presencia_balaceras`, by = "comuna") %>%
    left_join(enusc_2023$`pers-com_si_arma_en_casa`, by = "comuna") %>%
    left_join(enusc_2023$`hog-com_si_presencia_balaceras`, by = "comuna") %>%
    left_join(enusc_2023$`hog-com_si_arma_en_casa`, by = "comuna") %>%
    rename(poblacion_2024 = poblacion)

# Save the combined dataframe as xlsx
write.xlsx(rm_armas, "data_processed/rm_armas.xlsx", rowNames = FALSE)