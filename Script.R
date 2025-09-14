## Prosocial Effort Task ##
# FONDECYT David Huepe #
# Diego Garrido - José Borquez #
# Viña del Mar - 2025 #


# Import Libraries

library(readxl)
library(readr)
library(tidyverse)


# ===========================
# Cleaning data bases Prosocial Effort Task FONwDECYT David Huepe

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



## Limpieza ##


# Función para formatear ceros a la izquierda
formatear_id <- function(id) {
  sprintf("%04d", as.integer(id))
}

# Aplicar unión a cada columna ID de los dataframe
participantes_ID$id = sapply(participantes_ID$id, formatear_id)

# Renombrar columna 
names(participantes_ID)[names(participantes_ID) == "id"] = "CÓDIGO"
names(participantes_ID)[names(participantes_ID) == "grupo"] = "GRUPO"


# Unir por ID_check
effort_task <- left_join(effort_task, participantes_ID[, c("CÓDIGO", "GRUPO")], by = c("ID_check" = "CÓDIGO"))

# Reubicar columna "GRUPO" justo después de "ID_check"
effort_task <- effort_task %>% relocate(GRUPO, .after = ID_check)

# Quitar valores NA de la columna grupos
effort_task <- effort_task %>% filter(!is.na(GRUPO))

# Remover valores duplicados de la columna ID_check
effort_task <- effort_task[!duplicated(effort_task$ID_check), ]

# Reemplazar valores de Experimental por Vulnerable
effort_task$GRUPO[grep("V", effort_task$GRUPO)] <- "Vulnerable"
effort_task$GRUPO[grep("C", effort_task$GRUPO)] <- "Control"

