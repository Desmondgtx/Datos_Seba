
## Prosocial Effort Task ##
# FONDECYT David Huepe #
# Diego Garrido - José Borquez #
# Viña del Mar - 2025 #


# Import Libraries

library(readxl)
library(readr)
library(tidyverse)


# ===========================
# Cleaning data bases Prosocial Effort Task FONDECYT David Huepe

# Import dataset
effort_task = read_excel("Prosocial_effort_task_Seba.xlsx")

# Delete  all columns with "prac" on their name
effort_task = effort_task[, !grepl("prac", names(effort_task), ignore.case = TRUE)]

# Delete  all columns with "boxes" on their name
effort_task = effort_task[, !grepl("boxes", names(effort_task), ignore.case = TRUE)] 

# Save datafile
write.csv(effort_task, "datos_limpios.csv")


# Read dataset without headers
effort_task <- read_csv("datos_limpios.csv", col_names = FALSE)


# Extract two first rows as variables
header1 <- effort_task[1, ] |> unlist(use.names = FALSE)
header2 <- effort_task[2, ] |> unlist(use.names = FALSE)


# Crear nuevos nombres de columnas basados en condiciones específicas
new_colnames <- mapply(function(col_name, second_row_val) {
  # Identificar si el nombre es del tipo exacto "X_choice_screen"
  if (grepl("^\\d+_choice_screen$", col_name)) {
    return(second_row_val)
  } else {
    return(col_name)
  }
}, header1, header2, USE.NAMES = FALSE)


# Leer los datos reales, omitiendo las dos primeras filas
effort_task <- read_csv("datos_limpios.csv", skip = 2, col_names = new_colnames)

# Save datafile
write.csv(effort_task, "datos_limpios.csv")






# ===========================
# Trabajar con base de datos Participantes.xlsx

# Importar datos
effort_task <- read_csv("datos_limpios.csv")
participantes_ID <- read_csv("Participantes_Final_ID.csv")



## Chequeos ## 

# Duplicados en effort_task
duplicados_effort <- effort_task$ID_check[duplicated(effort_task$ID_check)]
cat("\nDuplicados en effort_task (ID_check):\n")
if(length(duplicados_effort) > 0) {
  cat("- Cantidad de IDs duplicados:", length(unique(duplicados_effort)), "\n")
  cat("- IDs duplicados:", unique(duplicados_effort), "\n")
  cat("- Total de registros duplicados:", length(duplicados_effort), "\n")
  
  # Ver detalle de duplicados
  for(id in unique(duplicados_effort)) {
    cat("\n  ID", id, "aparece", sum(effort_task$ID_check == id), "veces\n")
  }
} else {
  cat("- No hay duplicados\n")
}

# Duplicados
# 0000, 2411, 8053, 9477, 7997, 1304, 4625, 0155, 5495


## Verificar IDs existentes en participantes ID que se encuentren duplicados en effort_task
verificar_duplicados_cruzados <- function(effort_task, participantes_ID) {
  
  # IDs únicos de participantes_ID
  ids_participantes <- unique(participantes_ID$id)
  
  # Contar frecuencia de cada ID en effort_task
  frecuencia_ids <- table(effort_task$ID_check)
  
  # Filtrar IDs que están en participantes_ID Y aparecen más de 1 vez en effort_task
  ids_duplicados <- names(frecuencia_ids[frecuencia_ids > 1 & names(frecuencia_ids) %in% ids_participantes])
  
  # Mostrar resultados
  if(length(ids_duplicados) > 0) {
    cat("IDs duplicados encontrados:\n")
    for(id in ids_duplicados) {
      cat("ID", id, "→ aparece", frecuencia_ids[id], "veces en effort_task\n")
    }
  } else {
    cat("No hay IDs de participantes_ID duplicados en effort_task\n")
  }
  
  return(ids_duplicados)
}

# Ejecutar
ids_con_duplicados <- verificar_duplicados_cruzados(effort_task, participantes_ID)

# IDs existentes en "participantes_ID" que en "effort_task" se encuentran duplicados:
# ID 0155 → aparece 2 veces en effort_task 85 y 88 mala
# ID 2411 → aparece 2 veces en effort_task 16 y 27 mala
# ID 4625 → aparece 2 veces en effort_task 71 y 76 mala
# ID 5495 → aparece 2 veces en effort_task 111 y 126 (74%)
# ID 9477 → aparece 2 veces en effort_task 35 y 44 mala



# IDs únicos en cada dataset
ids_effort <- unique(effort_task$ID_check)
ids_participantes <- unique(participantes_ID$id)

cat("- Total IDs únicos en effort_task:", length(ids_effort), "\n")
# - Total IDs únicos en effort_task: 118 
cat("- Total IDs únicos en participantes_ID:", length(ids_participantes), "\n")
# - Total IDs únicos en participantes_ID: 102 




# IDs que SÍ van a calzar
ids_coinciden <- intersect(ids_effort, ids_participantes)
cat("\n- IDs que SÍ coinciden:", length(ids_coinciden), "\n")
if(length(ids_coinciden) <= 20) {
  cat("  IDs:", ids_coinciden, "\n")
}
# - IDs que SÍ coinciden: 101 



# IDs que NO van a calzar (están en effort_task pero no en participantes_ID)
ids_sin_match_effort <- setdiff(ids_effort, ids_participantes)
cat("\n- IDs en effort_task que NO tienen match:", length(ids_sin_match_effort), "\n")
if(length(ids_sin_match_effort) > 0 & length(ids_sin_match_effort) <= 20) {
  cat("  IDs sin match:", ids_sin_match_effort, "\n")
}
# - IDs en effort_task que NO tienen match: 17 
# - IDs sin match: P123 8053 7565 2676 0336 1559 1857 1693 3790 0000 1304 test1 2251 7997 4046 0066 3965 



# IDs que están en participantes_ID pero no en effort_task
ids_sin_usar <- setdiff(ids_participantes, ids_effort)
cat("\n- IDs en participantes_ID que NO se usarán:", length(ids_sin_usar), "\n")
if(length(ids_sin_usar) > 0 & length(ids_sin_usar) <= 20) {
  cat("  IDs no usados:", ids_sin_usar, "\n")
}
# - IDs en participantes_ID que NO se usarán: 1 
# - IDs no usados: 8831 


# ===========================
## Limpieza ##

# Función para formatear ceros a la izquierda
formatear_id <- function(id) {
  sprintf("%04d", as.integer(id))
}

# Aplicar función 
participantes_ID$id = sapply(participantes_ID$id, formatear_id)

# Unir por ID_check
effort_task <- left_join(effort_task, participantes_ID[, c("id", "grupo")], by = c("ID_check" = "id"))

# Reubicar columna "GRUPO" justo después de "ID_check"
effort_task <- effort_task %>% relocate(grupo, .after = ID_check)

# Remover valores duplicados de la columna ID_check
effort_task <- effort_task[!duplicated(effort_task$ID_check), ]

# Reemplazar valores de Experimental por Vulnerable
effort_task$grupo[grep("V", effort_task$grupo)] <- "Vulnerable"
effort_task$grupo[grep("C", effort_task$grupo)] <- "Control"

# Quitar valores NA de la columna grupos
effort_task <- effort_task %>% filter(!is.na(grupo))

# Convertir la columna Progress a numérica (elimina símbolos como "%", si es necesario)
effort_task$Progress <- as.numeric(gsub("%", "", effort_task$Progress))

# Reemplazar NA por 0 en columnas SELF y OTHER
effort_task <- effort_task %>%
  mutate(across(contains("SELF"), ~replace_na(., 0))) %>%
  mutate(across(contains("OTHER"), ~replace_na(., 0)))

# Guardar Archivo Limpio
write.csv(effort_task, "datos_limpios.csv")


# ===========================
# Últimos chequeos

# IDs que faltan en effort Task 
ids_faltantes <- function(effort_task, participantes_ID) {
  faltantes <- setdiff(participantes_ID$id, effort_task$ID_check)
  
  if(length(faltantes) > 0) {
    cat("IDs faltantes en effort_task:", faltantes, "\n")
  } else {
    cat("No faltan IDs\n")
  }
  
  return(faltantes)
}

# Ejecutar
ids_faltantes(effort_task, participantes_ID)
# IDs faltantes en effort_task: 8831 




#####
# Extraer
# Comp2 Q1 y Q2
# attention_check - estímulo 49


# if choise_scren NA && fail_feedback_page_submit = 1
#   eliminarlo
# else fail_feedback_page_submit = 1
#   cambiarlo a 1











# Parte 2


# Cargar datos
datos <- read.csv("datos_limpios.csv")



# Crear un nuevo dataset con solo ID_check, GRUPO y columnas que empiezan con SELF o OTHER
datos_clean <- datos %>% select(ID_check, GRUPO, starts_with("SELF"), starts_with("OTHER"), X49_attention_check)


# Contar cuántos respondieron con "4"
respuestas_4 <- sum(datos_clean$X49_attention_check == 4, na.rm = TRUE) # Respondieron 63 participantes
respuestas_na <- sum(is.na(datos_clean$X49_attention_check)) # NO respondieron 40 participantes



# Divide condition columns with reward, effort and difficulty 
{
  # Identificar las 48 columnas de estímulo (SELF.* / OTHER.*) ──
  stim_cols <- names(datos) %>% str_subset("^(SELF|OTHER)\\.")
  
  # Base mínima con ID_check y GRUPO ────────────────────────────
  datos_base <- datos %>% select(ID_check, GRUPO, X49_attention_check)
  
  # Construir las 192 columnas nuevas ───────────────────────────
  new_cols <- list()
  
  for (i in seq_along(stim_cols)) {
    
    col_name  <- stim_cols[i]
    respuestas <- datos[[col_name]]          # valores 0 / 1 / 2
    
    # Extraer la meta‑información del nombre de columna
    # Capturas: 1‑condición, 2‑reward, 3‑dificultad, 4‑esfuerzo
    meta <- str_match(col_name,
                      "^(SELF|OTHER)\\.(\\d+)(easy|hard)(\\d+)")[ , 2:5]
    
    condicion  <- meta[1]      # SELF / OTHER
    reward     <- meta[2]      # 2 / 6 / 10
    dificultad <- meta[3]      # easy / hard
    esfuerzo   <- meta[4]      # 50 / 65 / 80 / 95
    
    idx <- sprintf("%02d", i)  # 01 … 48  (para garantizar unicidad)
    
    # Crear cuatro columnas con nombres que conservan la info clave
    new_cols[[ paste0("condicion_",  condicion,  "_", idx) ]] <- respuestas
    new_cols[[ paste0("reward_",     reward,     "_", idx) ]] <- respuestas
    new_cols[[ paste0("dificultad_", dificultad, "_", idx) ]] <- respuestas
    new_cols[[ paste0("esfuerzo_",   esfuerzo,   "_", idx) ]] <- respuestas
  }
  
  # Unir todo y obtener el dataset final ────────────────────────
  datos_clean <- bind_cols(datos_base, as_tibble(new_cols))
}


#  Guardar el nuevo dataframe modificado en un archivo CSV
write.csv(datos_clean, "datos_clean.csv")















# Porcentaje de trabajo y omisiones #

datos_clean <- read_csv("datos_clean.csv")


#
# ── 3. Identificar columnas de condición SELF y OTHER ───────────────
self_cols  <- grep("^condicion_SELF_",  names(datos_clean), value = TRUE)
other_cols <- grep("^condicion_OTHER_", names(datos_clean), value = TRUE)

# Verificación rápida (opcional): deberían ser 24 y 24
stopifnot(length(self_cols)  == 24,
          length(other_cols) == 24)

# ── 4. Calcular todos los porcentajes fila a fila ───────────────────
datos_clean <- datos_clean %>% 
  rowwise() %>% 
  mutate(
    ## Trabajo (solo valor 1) ----------------------------------------
    trabajo_self  = sum(c_across(all_of(self_cols))  == 1, na.rm = TRUE) /
      length(self_cols)  * 100,
    trabajo_other = sum(c_across(all_of(other_cols)) == 1, na.rm = TRUE) /
      length(other_cols) * 100,
    
    ## Omisiones / ceros (valor 0) -----------------------------------
    zeros_SELF    = sum(c_across(all_of(self_cols))  == 0, na.rm = TRUE) /
      length(self_cols)  * 100,
    zeros_OTHER   = sum(c_across(all_of(other_cols)) == 0, na.rm = TRUE) /
      length(other_cols) * 100,
    
    ## Omisiones Total
    zeros_TOTAL = (zeros_SELF + zeros_OTHER) / 2
  ) %>% 
  ungroup()



# Redondear los valores de las columnas
datos_clean <- datos_clean %>%
  mutate(
    trabajo_self = round(trabajo_self, 2),
    trabajo_other = round(trabajo_other, 2),
    zeros_SELF = round(zeros_SELF, 2),
    zeros_OTHER = round(zeros_OTHER, 2),
    zeros_TOTAL = round(zeros_TOTAL, 2)
  )





# Paso 1: Detectar las columnas que pertenecen a cada condición
self_cols <- select(datos_clean, starts_with("condicion_SELF"))
other_cols <- select(datos_clean, starts_with("condicion_OTHER"))

# Paso 2: Crear las nuevas columnas con el conteo de omisiones (valores 0)
datos_clean <- datos_clean %>%
  mutate(
    prop_omitidas_SELF = rowSums(self_cols == 0, na.rm = TRUE),
    prop_omitidas_OTHER = rowSums(other_cols == 0, na.rm = TRUE),
    prop_omitidas_TOTAL = prop_omitidas_SELF + prop_omitidas_OTHER
  )


# Guardar set de datos 
write.csv(datos_clean, "datos_clean.csv")











datos_clean_v2 = datos_clean

# Identificar las columnas a modificar (todas excepto las 3 primeras y las 8 últimas)
cols_a_modificar <- names(datos_clean_v2)[3:(ncol(datos_clean_v2) - 8)]

# Reemplazar 0 por NA en esas columnas
datos_clean_v2[cols_a_modificar] <- datos_clean_v2[cols_a_modificar] %>%
  mutate(across(everything(), ~ ifelse(. == 0, NA, .)))

# Reemplazar los valores 2 por 0 en esas columnas
datos_clean_v2[cols_a_modificar] <- datos_clean_v2[cols_a_modificar] %>%
  mutate(across(everything(), ~ ifelse(. == 2, 0, .)))



# Filtrar participantes que NO tienen 7 o más omisiones en SELF u OTHER
datos_clean_v2 <- datos_clean_v2 %>% filter(prop_omitidas_SELF < 7 & prop_omitidas_OTHER < 7)


# Identificar las columnas por condición
self_cols <- select(datos_clean_v2, starts_with("condicion_SELF"))
other_cols <- select(datos_clean_v2, starts_with("condicion_OTHER"))

# Calcular proporciones de trabajo como porcentajes (con 2 decimales)
datos_clean_v2 <- datos_clean_v2 %>%
  mutate(
    proporcion_SELF = round((rowSums(self_cols == 1, na.rm = TRUE) / rowSums(!is.na(self_cols))) * 100, 2),
    proporcion_OTHER = round((rowSums(other_cols == 1, na.rm = TRUE) / rowSums(!is.na(other_cols))) * 100, 2)
  )



# Guardar set de datos 
write.csv(datos_clean_v2, "datos_clean_v2.csv")



















# Paso 1: Identificar las columnas relevantes
primeras_columnas <- 1:3
ultimas_columnas <- (ncol(datos_clean)-9):ncol(datos_clean)

# Extraer nombres de columnas intermedias
columnas_intermedias <- datos_clean[, -(c(primeras_columnas, ultimas_columnas))]
nombres_columnas_intermedias <- colnames(columnas_intermedias)

# Extraer los sufijos (número de trial) para agrupar columnas por trial
sufijos <- str_extract(nombres_columnas_intermedias, "\\d+$") %>% unique()

# Inicializar lista para almacenar valores por combinación
resultados <- data.frame(
  datos_clean[, primeras_columnas],
  datos_clean[, ultimas_columnas]
)

# Inicializar columnas
for (cond in c("SELF", "OTHER")) {
  for (reward in c("2", "6", "10")) {
    resultados[[paste0(cond, "_0", reward)]] <- NA
  }
}

# Paso 2: Calcular promedios por participante para cada combinación
for (i in 1:nrow(datos_clean)) {
  valores <- list(
    SELF_02 = c(), SELF_06 = c(), SELF_10 = c(),
    OTHER_02 = c(), OTHER_06 = c(), OTHER_10 = c()
  )
  
  for (suf in sufijos) {
    cond_col_self <- paste0("condicion_SELF_", suf)
    cond_col_other <- paste0("condicion_OTHER_", suf)
    
    reward_cols <- c(paste0("reward_2_", suf), paste0("reward_6_", suf), paste0("reward_10_", suf))
    reward_col <- reward_cols[reward_cols %in% colnames(datos_clean)]
    reward_val <- if (length(reward_col) > 0) str_extract(reward_col, "\\d+") else NA
    
    if (!is.na(reward_val)) {
      if (cond_col_self %in% colnames(datos_clean)) {
        val <- datos_clean[[cond_col_self]][i]
        if (!is.na(val)) {
          valores[[paste0("SELF_0", reward_val)]] <- c(valores[[paste0("SELF_0", reward_val)]], val)
        }
      } else if (cond_col_other %in% colnames(datos_clean)) {
        val <- datos_clean[[cond_col_other]][i]
        if (!is.na(val)) {
          valores[[paste0("OTHER_0", reward_val)]] <- c(valores[[paste0("OTHER_0", reward_val)]], val)
        }
      }
    }
  }
  
  # Asignar promedios redondeados
  for (nombre in names(valores)) {
    if (length(valores[[nombre]]) > 0) {
      resultados[[nombre]][i] <- round(mean(valores[[nombre]]), 2)
    }
  }
}

# Guardar en nueva variable
datos_anova_reward <- resultados



write.csv(datos_anova_reward, "datos_anova_reward.csv")



















# Paso 1: Identificar columnas que se conservarán
primeras_columnas <- 1:3
ultimas_columnas <- (ncol(datos_clean)-9):ncol(datos_clean)


# Extraer las columnas intermedias
columnas_intermedias <- datos_clean[, -(c(primeras_columnas, ultimas_columnas))]
nombres_columnas_intermedias <- colnames(columnas_intermedias)


# Extraer sufijos de trial
sufijos <- str_extract(nombres_columnas_intermedias, "\\d+$") %>% unique()

# Crear estructura del nuevo dataset
resultados <- data.frame(
  datos_clean[, primeras_columnas],
  datos_clean[, ultimas_columnas]
)

# Crear columnas para esfuerzo
esfuerzos <- c("50", "65", "80", "95")
condiciones <- c("SELF", "OTHER")

for (cond in condiciones) {
  for (eff in esfuerzos) {
    resultados[[paste0(cond, "_", eff)]] <- NA
  }
}

# Paso 2: Recorrer fila por fila y calcular promedios según esfuerzo
for (i in 1:nrow(datos_clean)) {
  valores <- list(
    SELF_50 = c(), SELF_65 = c(), SELF_80 = c(), SELF_95 = c(),
    OTHER_50 = c(), OTHER_65 = c(), OTHER_80 = c(), OTHER_95 = c()
  )
  
  for (suf in sufijos) {
    # Obtener columnas disponibles
    effort_cols <- paste0("esfuerzo_", esfuerzos, "_", suf)
    effort_col <- effort_cols[effort_cols %in% colnames(datos_clean)]
    
    # Extraer valor de esfuerzo de la columna presente
    effort_val <- if (length(effort_col) > 0) str_extract(effort_col, "\\d+") else NA
    
    # Extraer valor de condición
    if (!is.na(effort_val)) {
      cond_col_self <- paste0("condicion_SELF_", suf)
      cond_col_other <- paste0("condicion_OTHER_", suf)
      
      if (cond_col_self %in% colnames(datos_clean)) {
        val <- datos_clean[[cond_col_self]][i]
        if (!is.na(val)) {
          valores[[paste0("SELF_", effort_val)]] <- c(valores[[paste0("SELF_", effort_val)]], val)
        }
      } else if (cond_col_other %in% colnames(datos_clean)) {
        val <- datos_clean[[cond_col_other]][i]
        if (!is.na(val)) {
          valores[[paste0("OTHER_", effort_val)]] <- c(valores[[paste0("OTHER_", effort_val)]], val)
        }
      }
    }
  }
  
  # Guardar promedios en el nuevo dataset
  for (nombre in names(valores)) {
    if (length(valores[[nombre]]) > 0) {
      resultados[[nombre]][i] <- round(mean(valores[[nombre]]), 2)
    }
  }
}

# Guardar en nuevo dataset
datos_anova_effort <- resultados




write.csv(datos_anova_effort, "datos_anova_effort.csv")







## Steps for the long format


# Import Libraries

library(readr)
library(tidyverse)


# Cargar datos
datos_clean <- read_csv("datos_clean.csv")


# Change values of 0 to 3


# Change values of 2 to 0


# Change values of 3 to 2


# Change values of effort (50, 65, 80 & 95) to levels (1, 2, 3 & 4)


# Change values of reward (2, 6 & 10) to levels (1, 2 & 3)


# Change values of condition (Self & Other) to levels (0 & 1) respectively


# Change values of group (Control & Experimental) to levels (0 & 1) respectively


# Long format (be careful with the last 8 columns and the first 3, you have to repeat each value 48 times for each participant)








