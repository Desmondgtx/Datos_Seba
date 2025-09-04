
## Prosocial Effort Task ##
# FONDECYT Sebastián Contreras #
# Diego Garrido - José Borquez #
# Viña del Mar - 2025 #


# Import Libraries

library(readxl)
library(readr)
library(tidyverse)


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















