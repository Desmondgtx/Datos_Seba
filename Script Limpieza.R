
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
participantes_control <- read_excel("Participantes.xlsx", sheet = "Control")
participantes_vulnerable <- read_excel("Participantes.xlsx", sheet = "Vulnerable") 


## Limpieza ##

# Quitar última fila vacia de la hoja participanteS_contrpl
participantes_control = participantes_control[-52,]

# Función para formatear ceros a la izquierda
formatear_id <- function(id) {
  sprintf("%04d", as.integer(id))
}

# Aplicar unión a cada columna ID de los dataframe
participantes_control$`Código de participante` = sapply(participantes_control$`Código de participante`, formatear_id)
participantes_vulnerable$CÓDIGO = sapply(participantes_vulnerable$CÓDIGO, formatear_id)


# Renombrar columna de participantes
names(participantes_control)[names(participantes_control) == "Código de participante"] = "CÓDIGO"

# Renombrar y unir ambos grupos
participantes = bind_rows(participantes_vulnerable, participantes_control) %>% select(CÓDIGO, GRUPO)

# Unir por ID_check
effort_task <- left_join(effort_task, participantes[, c("CÓDIGO", "GRUPO")], by = c("ID_check" = "CÓDIGO"))

# Reubicar columna "GRUPO" justo después de "ID_check"
effort_task <- effort_task %>% relocate(GRUPO, .after = ID_check)

# Quitar valores NA de la columna grupos
effort_task <- effort_task %>% filter(!is.na(GRUPO))

# Remover valores duplicados de la columna ID_check
effort_task <- effort_task[!duplicated(effort_task$ID_check), ]

# Convertir la columna Progress a numérica (elimina símbolos como "%", si es necesario)
effort_task$Progress <- as.numeric(gsub("%", "", effort_task$Progress))

# Limpiar por progreso
# Se descuenta al participante 2939 (Walter Espinoza) con un progreso del 1%
effort_task <- effort_task[effort_task$Progress >= 70, ]

# Quitar datos 0000
effort_task = effort_task[-1,]

# Reemplazar valores de Experimental por Vulnerable
effort_task$GRUPO[grep("Experimental", effort_task$GRUPO)] <- "Vulnerable"

# Reemplazar el error de alguien
effort_task$GRUPO[grep("control", effort_task$GRUPO)] <- "Control"


# Reemplazar NA por 0 en columnas SELF y OTHER
effort_task <- effort_task %>%
  mutate(across(contains("SELF"), ~replace_na(., 0))) %>%
  mutate(across(contains("OTHER"), ~replace_na(., 0)))


# connvertir valores NA a 0 en columnas SELF y OTHER
# datos_self <- datos %>% select(contains("SELF")) %>% replace(is.na(.), 0)
# datos_other <- datos %>% select(contains("OTHER")) %>% replace(is.na(.), 0)


# Guardar Archivo Limpio
write.csv(effort_task, "datos_limpios.csv")


