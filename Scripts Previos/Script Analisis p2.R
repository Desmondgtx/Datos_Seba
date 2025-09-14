
## Prosocial Effort Task ##
# FONDECYT Sebastián Contreras #
# Diego Garrido - José Borquez #
# Viña del Mar - 2025 #


# Import Libraries

library(readxl)
library(readr)
library(tidyverse)


# Cargar datos
datos_clean <- read.csv("datos_clean.csv")



# Proporción de trabajo dividido por Reward con porcentajes
{
# 2. Determinar el número de participantes
N_participants <- nrow(datos)
if (N_participants == 0) {
  stop("El dataframe está vacío. Verifica la carga de datos.")
}
print(paste("Número de participantes detectados:", N_participants))

# 3. Definir las columnas relevantes para cada categoría

# Columnas para la condición SELF
cols_self_reward2_suffixes <- sprintf("%02d", seq(1, 24, by = 3)) # e.g., 01, 04, ..., 22
cols_self_reward6_suffixes <- sprintf("%02d", seq(2, 24, by = 3)) # e.g., 02, 05, ..., 23
cols_self_reward10_suffixes <- sprintf("%02d", seq(3, 24, by = 3)) # e.g., 03, 06, ..., 24

# Columnas para la condición OTHER
# Los ensayos OTHER van del 25 al 48
cols_other_reward2_suffixes <- sprintf("%02d", seq(25, 48, by = 3)) # e.g., 25, 28, ..., 46
cols_other_reward6_suffixes <- sprintf("%02d", seq(26, 48, by = 3)) # e.g., 26, 29, ..., 47
cols_other_reward10_suffixes <- sprintf("%02d", seq(27, 48, by = 3)) # e.g., 27, 30, ..., 48

# Nombres completos de las columnas de respuesta
# (asumiendo que las columnas de respuesta se llaman 'condicion_SELF_XX' y 'condicion_OTHER_XX')
# Si las columnas de respuesta tienen otro patrón (ej. 'reward_2_XX'), ajusta esto.
# Por la inspección previa, parece que 'condicion_SELF_XX' (o _OTHER_XX) contiene la respuesta.

# SELF
actual_cols_self_reward2 <- paste0("condicion_SELF_", cols_self_reward2_suffixes)
actual_cols_self_reward6 <- paste0("condicion_SELF_", cols_self_reward6_suffixes)
actual_cols_self_reward10 <- paste0("condicion_SELF_", cols_self_reward10_suffixes)

# OTHER
actual_cols_other_reward2 <- paste0("condicion_OTHER_", cols_other_reward2_suffixes)
actual_cols_other_reward6 <- paste0("condicion_OTHER_", cols_other_reward6_suffixes)
actual_cols_other_reward10 <- paste0("condicion_OTHER_", cols_other_reward10_suffixes)

# Verificar que las columnas existan (opcional pero bueno para depurar)
check_cols_exist <- function(df, cols_to_check) {
  missing_cols <- cols_to_check[!cols_to_check %in% names(df)]
  if (length(missing_cols) > 0) {
    warning(paste("Las siguientes columnas no se encontraron:", paste(missing_cols, collapse=", ")))
    return(FALSE)
  }
  return(TRUE)
}
all_cols_to_check <- c(actual_cols_self_reward2, actual_cols_self_reward6, actual_cols_self_reward10,
                       actual_cols_other_reward2, actual_cols_other_reward6, actual_cols_other_reward10)
if (!check_cols_exist(datos, all_cols_to_check)) {
  stop("Algunas columnas necesarias no existen en el dataframe. Revisa los nombres de las columnas.")
}


# 4. Calcular la suma de '1's (trabajo) para cada categoría
# Función para sumar '1's en un conjunto de columnas
sum_work_in_columns <- function(df, column_list) {
  # Seleccionar solo las columnas que existen en el df para evitar errores
  existing_columns <- column_list[column_list %in% names(df)]
  if (length(existing_columns) == 0) return(0)
  
  # Sumar las veces que aparece '1' en todas estas columnas
  total_sum <- sum(sapply(existing_columns, function(col) sum(df[[col]] == 1, na.rm = TRUE)), na.rm = TRUE)
  return(total_sum)
}

work_self_r2 <- sum_work_in_columns(datos, actual_cols_self_reward2)
work_self_r6 <- sum_work_in_columns(datos, actual_cols_self_reward6)
work_self_r10 <- sum_work_in_columns(datos, actual_cols_self_reward10)

work_other_r2 <- sum_work_in_columns(datos, actual_cols_other_reward2)
work_other_r6 <- sum_work_in_columns(datos, actual_cols_other_reward6)
work_other_r10 <- sum_work_in_columns(datos, actual_cols_other_reward10)

# 5. Calcular las proporciones
# Hay 8 ensayos por cada combinación de condición/recompensa por participante
num_trials_per_category = 8
denominator <- N_participants * num_trials_per_category

if (denominator == 0) {
  stop("El denominador para la proporción es cero. Revisa N_participants o num_trials_per_category.")
}

prop_self_r2 <- work_self_r2 / denominator
prop_self_r6 <- work_self_r6 / denominator
prop_self_r10 <- work_self_r10 / denominator

prop_other_r2 <- work_other_r2 / denominator
prop_other_r6 <- work_other_r6 / denominator
prop_other_r10 <- work_other_r10 / denominator

# 6. Crear un data frame para ggplot2
plot_data <- data.frame(
  Condition = factor(rep(c("SELF", "OTHER"), each = 3), levels = c("SELF", "OTHER")),
  Reward_Magnitude = factor(
    rep(c("Recompensa 2", "Recompensa 6", "Recompensa 10"), times = 2),
    levels = c("Recompensa 2", "Recompensa 6", "Recompensa 10") # Define el orden en el gráfico
  ),
  Proportion = c(prop_self_r2, prop_self_r6, prop_self_r10,
                 prop_other_r2, prop_other_r6, prop_other_r10)
)

print("Datos para el gráfico:")
print(plot_data)

# 7. Generar el gráfico con ggplot2
ggplot(plot_data, aes(x = Reward_Magnitude, y = Proportion, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Proporción de Trabajo por Condición y Recompensa",
       x = "Magnitud de la Recompensa",
       y = "Proporción de Trabajo (Respuestas '1' / Oportunidades Totales)",
       fill = "Condición") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) + # Formato de porcentaje en eje Y
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top"
  )



# print("Datos para el gráfico:") # Comentado para no llenar la consola
# print(plot_data)

# 7. Generar el gráfico con ggplot2 (CON PORCENTAJES ENCIMA DE LAS BARRAS)
ggplot(plot_data, aes(x = Reward_Magnitude, y = Proportion, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = scales::percent(Proportion, accuracy = 0.1)), # Formato de porcentaje
    position = position_dodge(width = 0.9),    # Asegura que el texto se alinee con las barras esquivadas
    vjust = -0.5,                               # Ajusta la posición vertical del texto (encima de la barra)
    size = 3.5                                  # Tamaño del texto (ajusta según sea necesario)
  ) +
  labs(title = "Proporción de Trabajo por Condición y Recompensa",
       x = "Magnitud de la Recompensa",
       y = "Proporción de Trabajo",
       fill = "Condición") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1), 
    limits = c(0, max(plot_data$Proportion) * 1.15) # Ajustar límite para dar espacio al texto
  ) + 
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top"
  )


}



# Proporcion de trabajo dividido por Reward con porcentajes más Error Estandar
{
# 2. Determinar el número de participantes y de ensayos por categoría
N_participants <- nrow(datos)
num_trials_per_category <- 8

if (N_participants == 0) {
  stop("El dataframe está vacío. Verifica la carga de datos.")
}
if (num_trials_per_category == 0) {
  stop("El número de ensayos por categoría es cero.")
}

if (!"Participant_ID" %in% names(datos)) {
  if ("ID_check" %in% names(datos) && length(unique(datos$ID_check)) == N_participants) {
    datos$Participant_ID <- datos$ID_check
  } else {
    datos$Participant_ID <- 1:N_participants
  }
}

# 3. Definir las columnas relevantes para cada categoría
cols_self_reward2_suffixes <- sprintf("%02d", seq(1, 24, by = 3))
cols_self_reward6_suffixes <- sprintf("%02d", seq(2, 24, by = 3))
cols_self_reward10_suffixes <- sprintf("%02d", seq(3, 24, by = 3))
cols_other_reward2_suffixes <- sprintf("%02d", seq(25, 48, by = 3))
cols_other_reward6_suffixes <- sprintf("%02d", seq(26, 48, by = 3))
cols_other_reward10_suffixes <- sprintf("%02d", seq(27, 48, by = 3))

actual_cols_self_reward2 <- paste0("condicion_SELF_", cols_self_reward2_suffixes)
actual_cols_self_reward6 <- paste0("condicion_SELF_", cols_self_reward6_suffixes)
actual_cols_self_reward10 <- paste0("condicion_SELF_", cols_self_reward10_suffixes)
actual_cols_other_reward2 <- paste0("condicion_OTHER_", cols_other_reward2_suffixes)
actual_cols_other_reward6 <- paste0("condicion_OTHER_", cols_other_reward6_suffixes)
actual_cols_other_reward10 <- paste0("condicion_OTHER_", cols_other_reward10_suffixes)

all_cols_to_check <- c(actual_cols_self_reward2, actual_cols_self_reward6, actual_cols_self_reward10,
                       actual_cols_other_reward2, actual_cols_other_reward6, actual_cols_other_reward10)
check_cols_exist <- function(df, cols_to_check) {
  missing_cols <- cols_to_check[!cols_to_check %in% names(df)]
  if (length(missing_cols) > 0) {
    warning(paste("Las siguientes columnas no se encontraron:", paste(missing_cols, collapse=", ")))
    cols_to_check <- cols_to_check[cols_to_check %in% names(df)]
    if(length(cols_to_check) == 0) return(FALSE)
  }
  return(TRUE)
}
if (!check_cols_exist(datos, all_cols_to_check)) {
  stop("Algunas columnas necesarias no existen en el dataframe. Revisa los nombres de las columnas.")
}

# 4. Calcular proporciones de trabajo POR PARTICIPANTE para cada categoría
calculate_prop_per_participant <- function(df, column_list, trials_per_cat) {
  valid_columns <- column_list[column_list %in% names(df)]
  if (length(valid_columns) == 0) {
    return(rep(NA_real_, nrow(df))) 
  }
  work_counts <- rowSums(df[, valid_columns, drop = FALSE] == 1, na.rm = TRUE)
  return(work_counts / trials_per_cat)
}

participant_data <- data.frame(Participant_ID = datos$Participant_ID)
participant_data$SELF_R02 <- calculate_prop_per_participant(datos, actual_cols_self_reward2, num_trials_per_category)
participant_data$SELF_R06 <- calculate_prop_per_participant(datos, actual_cols_self_reward6, num_trials_per_category)
participant_data$SELF_R10 <- calculate_prop_per_participant(datos, actual_cols_self_reward10, num_trials_per_category)
participant_data$OTHER_R02 <- calculate_prop_per_participant(datos, actual_cols_other_reward2, num_trials_per_category)
participant_data$OTHER_R06 <- calculate_prop_per_participant(datos, actual_cols_other_reward6, num_trials_per_category)
participant_data$OTHER_R10 <- calculate_prop_per_participant(datos, actual_cols_other_reward10, num_trials_per_category)

# 5. Reestructurar datos a formato largo (long format) y AJUSTAR NOMBRES DE RECOMPENSA
data_long <- participant_data %>%
  pivot_longer(cols = -Participant_ID,
               names_to = "Group",
               values_to = "Proportion") %>%
  separate(Group, into = c("Condition", "Reward_Code"), sep = "_R", remove = FALSE) %>%
  mutate(
    Condition = factor(Condition, levels = c("SELF", "OTHER")),
    Reward_Value_Num = as.numeric(Reward_Code), 
    Reward_Magnitude = factor(
      paste0("Recompensa ", Reward_Value_Num), 
      levels = c("Recompensa 2", "Recompensa 6", "Recompensa 10") 
    )
  )

# 6. Calcular Media, SD, N, y SE para cada grupo
plot_data <- data_long %>%
  filter(!is.na(Reward_Magnitude)) %>% 
  group_by(Condition, Reward_Magnitude) %>%
  summarise(
    Mean_Proportion = mean(Proportion, na.rm = TRUE),
    SD_Proportion = sd(Proportion, na.rm = TRUE),
    N = sum(!is.na(Proportion)),
    SE_Proportion = SD_Proportion / sqrt(N),
    .groups = 'drop'
  ) %>%
  mutate(
    ymin = pmax(0, Mean_Proportion - SE_Proportion),
    # ymax no debe superar 1 (100%) si queremos que la barra de error no se corte con el límite fijo
    ymax = pmin(1, Mean_Proportion + SE_Proportion) 
  )

# 7. Generar el gráfico con ggplot2 (EJE Y DE 0% A 100%)
ggplot(plot_data, aes(x = Reward_Magnitude, y = Mean_Proportion, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    position = position_dodge(width = 0.9),
    width = 0.25, 
    color = "gray30",
    linewidth = 0.5 
  ) +
  geom_text( 
    aes(label = scales::percent(Mean_Proportion, accuracy = 0.1), 
        # Asegurar que el texto no intente ir más allá del 100% si ymax es 1
        y = pmin(1, ymax + 0.02) # Posiciona el texto ligeramente sobre ymax, pero no más de 100%
    ), 
    position = position_dodge(width = 0.9),
    vjust = -0.4, # Ajuste vertical para que el texto quede sobre ymax
    size = 3.0    
  ) +
  labs(title = "Proporción de Trabajo por Condición y Recompensa",
       x = "Magnitud de la Recompensa", 
       y = "Proporción Media de Trabajo (+/- SE)",
       fill = "Condición") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), # Establece los límites del eje Y de 0 a 1 (0% a 100%)
    breaks = seq(0, 1, by = 0.2), # Define los quiebres (0%, 20%, ..., 100%)
    expand = expansion(mult = c(0, 0.01)) # Pequeña expansión en el extremo superior para que 100% no toque el borde
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top"
  )

}





# 2. Determinar el número de participantes y de ensayos por categoría
N_participants <- nrow(datos)
num_trials_per_category <- 8

if (N_participants == 0) {
  stop("El dataframe está vacío. Verifica la carga de datos.")
}
if (num_trials_per_category == 0) {
  stop("El número de ensayos por categoría es cero.")
}

# Asegurar que la columna GRUPO existe
if (!"GRUPO" %in% names(datos)) {
  stop("La columna 'GRUPO' no se encuentra en el dataframe. Verifica el nombre de la columna.")
}
# Convertir GRUPO a factor para asegurar el orden en los facets si es necesario
datos$GRUPO <- factor(datos$GRUPO)


if (!"Participant_ID" %in% names(datos)) {
  if ("ID_check" %in% names(datos) && length(unique(datos$ID_check)) == N_participants) {
    datos$Participant_ID <- datos$ID_check
  } else {
    datos$Participant_ID <- 1:N_participants
  }
}

# 3. Definir las columnas relevantes para cada categoría (sin cambios)
cols_self_reward2_suffixes <- sprintf("%02d", seq(1, 24, by = 3))
cols_self_reward6_suffixes <- sprintf("%02d", seq(2, 24, by = 3))
cols_self_reward10_suffixes <- sprintf("%02d", seq(3, 24, by = 3))
cols_other_reward2_suffixes <- sprintf("%02d", seq(25, 48, by = 3))
cols_other_reward6_suffixes <- sprintf("%02d", seq(26, 48, by = 3))
cols_other_reward10_suffixes <- sprintf("%02d", seq(27, 48, by = 3))

actual_cols_self_reward2 <- paste0("condicion_SELF_", cols_self_reward2_suffixes)
actual_cols_self_reward6 <- paste0("condicion_SELF_", cols_self_reward6_suffixes)
actual_cols_self_reward10 <- paste0("condicion_SELF_", cols_self_reward10_suffixes)
actual_cols_other_reward2 <- paste0("condicion_OTHER_", cols_other_reward2_suffixes)
actual_cols_other_reward6 <- paste0("condicion_OTHER_", cols_other_reward6_suffixes)
actual_cols_other_reward10 <- paste0("condicion_OTHER_", cols_other_reward10_suffixes)

all_cols_to_check <- c(actual_cols_self_reward2, actual_cols_self_reward6, actual_cols_self_reward10,
                       actual_cols_other_reward2, actual_cols_other_reward6, actual_cols_other_reward10)
check_cols_exist <- function(df, cols_to_check) {
  missing_cols <- cols_to_check[!cols_to_check %in% names(df)]
  if (length(missing_cols) > 0) {
    warning(paste("Las siguientes columnas no se encontraron:", paste(missing_cols, collapse=", ")))
    cols_to_check <- cols_to_check[cols_to_check %in% names(df)]
    if(length(cols_to_check) == 0) return(FALSE)
  }
  return(TRUE)
}
if (!check_cols_exist(datos, all_cols_to_check)) {
  stop("Algunas columnas necesarias no existen en el dataframe. Revisa los nombres de las columnas.")
}

# 4. Calcular proporciones de trabajo POR PARTICIPANTE para cada categoría
calculate_prop_per_participant <- function(df, column_list, trials_per_cat) {
  valid_columns <- column_list[column_list %in% names(df)]
  if (length(valid_columns) == 0) {
    return(rep(NA_real_, nrow(df))) 
  }
  work_counts <- rowSums(df[, valid_columns, drop = FALSE] == 1, na.rm = TRUE)
  return(work_counts / trials_per_cat)
}

# Incluir GRUPO en participant_data
participant_data <- data.frame(
  Participant_ID = datos$Participant_ID,
  GRUPO = datos$GRUPO # <--- AÑADIDO GRUPO AQUÍ
)
participant_data$SELF_R02 <- calculate_prop_per_participant(datos, actual_cols_self_reward2, num_trials_per_category)
participant_data$SELF_R06 <- calculate_prop_per_participant(datos, actual_cols_self_reward6, num_trials_per_category)
participant_data$SELF_R10 <- calculate_prop_per_participant(datos, actual_cols_self_reward10, num_trials_per_category)
participant_data$OTHER_R02 <- calculate_prop_per_participant(datos, actual_cols_other_reward2, num_trials_per_category)
participant_data$OTHER_R06 <- calculate_prop_per_participant(datos, actual_cols_other_reward6, num_trials_per_category)
participant_data$OTHER_R10 <- calculate_prop_per_participant(datos, actual_cols_other_reward10, num_trials_per_category)

# 5. Reestructurar datos a formato largo (long format)
data_long <- participant_data %>%
  # GRUPO se mantiene como columna identificadora junto con Participant_ID
  pivot_longer(cols = -c(Participant_ID, GRUPO), 
               names_to = "Group",
               values_to = "Proportion") %>%
  separate(Group, into = c("Condition", "Reward_Code"), sep = "_R", remove = FALSE) %>%
  mutate(
    Condition = factor(Condition, levels = c("SELF", "OTHER")),
    Reward_Value_Num = as.numeric(Reward_Code), 
    Reward_Magnitude = factor(
      paste0("Recompensa ", Reward_Value_Num), 
      levels = c("Recompensa 2", "Recompensa 6", "Recompensa 10") 
    )
  )

# 6. Calcular Media, SD, N, y SE para cada grupo (AHORA TAMBIÉN AGRUPANDO POR GRUPO)
plot_data <- data_long %>%
  filter(!is.na(Reward_Magnitude) & !is.na(GRUPO)) %>% 
  # AÑADIR GRUPO A LA AGRUPACIÓN
  group_by(Condition, Reward_Magnitude, GRUPO) %>% 
  summarise(
    Mean_Proportion = mean(Proportion, na.rm = TRUE),
    SD_Proportion = sd(Proportion, na.rm = TRUE),
    N = sum(!is.na(Proportion)),
    SE_Proportion = SD_Proportion / sqrt(N),
    .groups = 'drop'
  ) %>%
  mutate(
    ymin = pmax(0, Mean_Proportion - SE_Proportion),
    ymax = pmin(1, Mean_Proportion + SE_Proportion) 
  )

# 7. Generar el gráfico con ggplot2 (CON PANELES PARA CADA GRUPO)
ggplot(plot_data, aes(x = Reward_Magnitude, y = Mean_Proportion, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    position = position_dodge(width = 0.9),
    width = 0.25, 
    color = "gray30",
    linewidth = 0.5 
  ) +
  geom_text( 
    aes(label = scales::percent(Mean_Proportion, accuracy = 0.1), 
        y = pmin(1, ymax + 0.02) 
    ), 
    position = position_dodge(width = 0.9),
    vjust = -0.4, 
    size = 3.0    
  ) +
  labs(title = "Proporción de Trabajo", # Título actualizado
       x = "Magnitud de la Recompensa", 
       y = "Proporción Media de Trabajo (+/- SE)",
       fill = "Condición") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.2), 
    expand = expansion(mult = c(0, 0.01)) 
  ) +
  # AÑADIR FACET_WRAP PARA DIVIDIR POR GRUPO
  facet_wrap(~ GRUPO, ncol = 2) + # ncol=2 para ponerlos lado a lado si hay 2 grupos
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    strip.text = element_text(face="bold", size=12) # Estilo para los títulos de los paneles (facets)
  )







# 2. Determinar el número de participantes y de ensayos por categoría de esfuerzo
N_participants <- nrow(datos)
# Para cada nivel de esfuerzo dentro de cada condición (SELF/OTHER), hay 6 ensayos.
num_trials_per_effort_category <- 6 

if (N_participants == 0) {
  stop("El dataframe está vacío. Verifica la carga de datos.")
}
if (num_trials_per_effort_category == 0) {
  stop("El número de ensayos por categoría de esfuerzo es cero.")
}

# Añadir Participant_ID si no existe uno fiable
if (!"Participant_ID" %in% names(datos)) {
  if ("ID_check" %in% names(datos) && length(unique(datos$ID_check)) == N_participants) {
    datos$Participant_ID <- datos$ID_check
  } else {
    datos$Participant_ID <- 1:N_participants
  }
}

# 3. Definir las columnas de RESPUESTA ('condicion_SELF_XX', 'condicion_OTHER_XX') 
#    que corresponden a cada nivel de esfuerzo y condición

# SELF Condition
cols_self_effort50_suffixes <- sprintf("%02d", c(1:3, 13:15))
cols_self_effort65_suffixes <- sprintf("%02d", c(4:6, 16:18))
cols_self_effort80_suffixes <- sprintf("%02d", c(7:9, 19:21))
cols_self_effort95_suffixes <- sprintf("%02d", c(10:12, 22:24))

actual_cols_self_e50 <- paste0("condicion_SELF_", cols_self_effort50_suffixes)
actual_cols_self_e65 <- paste0("condicion_SELF_", cols_self_effort65_suffixes)
actual_cols_self_e80 <- paste0("condicion_SELF_", cols_self_effort80_suffixes)
actual_cols_self_e95 <- paste0("condicion_SELF_", cols_self_effort95_suffixes)

# OTHER Condition
cols_other_effort50_suffixes <- sprintf("%02d", c(25:27, 37:39))
cols_other_effort65_suffixes <- sprintf("%02d", c(28:30, 40:42))
cols_other_effort80_suffixes <- sprintf("%02d", c(31:33, 43:45))
cols_other_effort95_suffixes <- sprintf("%02d", c(34:36, 46:48))

actual_cols_other_e50 <- paste0("condicion_OTHER_", cols_other_effort50_suffixes)
actual_cols_other_e65 <- paste0("condicion_OTHER_", cols_other_effort65_suffixes)
actual_cols_other_e80 <- paste0("condicion_OTHER_", cols_other_effort80_suffixes)
actual_cols_other_e95 <- paste0("condicion_OTHER_", cols_other_effort95_suffixes)

# (Opcional) Verificar que las columnas existan
all_cols_to_check_effort <- c(actual_cols_self_e50, actual_cols_self_e65, actual_cols_self_e80, actual_cols_self_e95,
                              actual_cols_other_e50, actual_cols_other_e65, actual_cols_other_e80, actual_cols_other_e95)
check_cols_exist <- function(df, cols_to_check) {
  missing_cols <- cols_to_check[!cols_to_check %in% names(df)]
  if (length(missing_cols) > 0) {
    warning(paste("Las siguientes columnas no se encontraron:", paste(missing_cols, collapse=", ")))
    cols_to_check <- cols_to_check[cols_to_check %in% names(df)]
    if(length(cols_to_check) == 0) return(FALSE)
  }
  return(TRUE)
}
if (!check_cols_exist(datos, all_cols_to_check_effort)) {
  stop("Algunas columnas necesarias para el análisis de esfuerzo no existen. Revisa los nombres.")
}


# 4. Calcular proporciones de trabajo POR PARTICIPANTE para cada categoría de esfuerzo
calculate_prop_per_participant <- function(df, column_list, trials_per_cat) {
  valid_columns <- column_list[column_list %in% names(df)]
  if (length(valid_columns) == 0) {
    return(rep(NA_real_, nrow(df))) 
  }
  work_counts <- rowSums(df[, valid_columns, drop = FALSE] == 1, na.rm = TRUE)
  return(work_counts / trials_per_cat)
}

participant_data_effort <- data.frame(Participant_ID = datos$Participant_ID)
participant_data_effort$SELF_E50 <- calculate_prop_per_participant(datos, actual_cols_self_e50, num_trials_per_effort_category)
participant_data_effort$SELF_E65 <- calculate_prop_per_participant(datos, actual_cols_self_e65, num_trials_per_effort_category)
participant_data_effort$SELF_E80 <- calculate_prop_per_participant(datos, actual_cols_self_e80, num_trials_per_effort_category)
participant_data_effort$SELF_E95 <- calculate_prop_per_participant(datos, actual_cols_self_e95, num_trials_per_effort_category)

participant_data_effort$OTHER_E50 <- calculate_prop_per_participant(datos, actual_cols_other_e50, num_trials_per_effort_category)
participant_data_effort$OTHER_E65 <- calculate_prop_per_participant(datos, actual_cols_other_e65, num_trials_per_effort_category)
participant_data_effort$OTHER_E80 <- calculate_prop_per_participant(datos, actual_cols_other_e80, num_trials_per_effort_category)
participant_data_effort$OTHER_E95 <- calculate_prop_per_participant(datos, actual_cols_other_e95, num_trials_per_effort_category)


# 5. Reestructurar datos a formato largo (long format)
data_long_effort <- participant_data_effort %>%
  pivot_longer(cols = -Participant_ID,
               names_to = "Group",
               values_to = "Proportion") %>%
  separate(Group, into = c("Condition", "Effort_Code"), sep = "_E", remove = FALSE) %>%
  mutate(
    Condition = factor(Condition, levels = c("SELF", "OTHER")),
    Effort_Level_Num = as.numeric(Effort_Code), # Effort_Code será "50", "65", "80", "95"
    Effort_Level = factor(
      paste0("Esfuerzo ", Effort_Level_Num, "%"), 
      levels = c("Esfuerzo 50%", "Esfuerzo 65%", "Esfuerzo 80%", "Esfuerzo 95%") 
    )
  )

# 6. Calcular Media, SD, N, y SE para cada grupo de esfuerzo
plot_data_effort <- data_long_effort %>%
  filter(!is.na(Effort_Level)) %>% 
  group_by(Condition, Effort_Level) %>% 
  summarise(
    Mean_Proportion = mean(Proportion, na.rm = TRUE),
    SD_Proportion = sd(Proportion, na.rm = TRUE),
    N = sum(!is.na(Proportion)),
    SE_Proportion = SD_Proportion / sqrt(N),
    .groups = 'drop'
  ) %>%
  mutate(
    ymin = pmax(0, Mean_Proportion - SE_Proportion),
    ymax = pmin(1, Mean_Proportion + SE_Proportion) 
  )

# 7. Generar el gráfico con ggplot2
ggplot(plot_data_effort, aes(x = Effort_Level, y = Mean_Proportion, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    position = position_dodge(width = 0.9),
    width = 0.25, 
    color = "gray30",
    linewidth = 0.5 
  ) +
  geom_text( 
    aes(label = scales::percent(Mean_Proportion, accuracy = 0.1), 
        y = pmin(1, ymax + 0.02) 
    ), 
    position = position_dodge(width = 0.9),
    vjust = -0.4, 
    size = 3.0    
  ) +
  labs(title = "Proporción de Trabajo por Nivel de Esfuerzo y Condición", 
       x = "Nivel de Esfuerzo Requerido", 
       y = "Proporción Media de Trabajo (+/- SE)",
       fill = "Condición") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.2), 
    expand = expansion(mult = c(0, 0.01)) 
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top"
  )

















# 2. Determinar el número de participantes y de ensayos por categoría de esfuerzo
N_participants <- nrow(datos)
num_trials_per_effort_category <- 6 

if (N_participants == 0) {
  stop("El dataframe está vacío. Verifica la carga de datos.")
}
if (num_trials_per_effort_category == 0) {
  stop("El número de ensayos por categoría de esfuerzo es cero.")
}

# Asegurar que la columna GRUPO existe y convertirla a factor
if (!"GRUPO" %in% names(datos)) {
  stop("La columna 'GRUPO' no se encuentra en el dataframe. Verifica el nombre de la columna.")
}
datos$GRUPO <- factor(datos$GRUPO)


if (!"Participant_ID" %in% names(datos)) {
  if ("ID_check" %in% names(datos) && length(unique(datos$ID_check)) == N_participants) {
    datos$Participant_ID <- datos$ID_check
  } else {
    datos$Participant_ID <- 1:N_participants
  }
}

# 3. Definir las columnas de RESPUESTA ('condicion_SELF_XX', 'condicion_OTHER_XX') 
#    que corresponden a cada nivel de esfuerzo y condición

# SELF Condition
cols_self_effort50_suffixes <- sprintf("%02d", c(1:3, 13:15))
cols_self_effort65_suffixes <- sprintf("%02d", c(4:6, 16:18))
cols_self_effort80_suffixes <- sprintf("%02d", c(7:9, 19:21))
cols_self_effort95_suffixes <- sprintf("%02d", c(10:12, 22:24))

actual_cols_self_e50 <- paste0("condicion_SELF_", cols_self_effort50_suffixes)
actual_cols_self_e65 <- paste0("condicion_SELF_", cols_self_effort65_suffixes)
actual_cols_self_e80 <- paste0("condicion_SELF_", cols_self_effort80_suffixes)
actual_cols_self_e95 <- paste0("condicion_SELF_", cols_self_effort95_suffixes)

# OTHER Condition
cols_other_effort50_suffixes <- sprintf("%02d", c(25:27, 37:39))
cols_other_effort65_suffixes <- sprintf("%02d", c(28:30, 40:42))
cols_other_effort80_suffixes <- sprintf("%02d", c(31:33, 43:45))
cols_other_effort95_suffixes <- sprintf("%02d", c(34:36, 46:48))

actual_cols_other_e50 <- paste0("condicion_OTHER_", cols_other_effort50_suffixes)
actual_cols_other_e65 <- paste0("condicion_OTHER_", cols_other_effort65_suffixes)
actual_cols_other_e80 <- paste0("condicion_OTHER_", cols_other_effort80_suffixes)
actual_cols_other_e95 <- paste0("condicion_OTHER_", cols_other_effort95_suffixes)

all_cols_to_check_effort <- c(actual_cols_self_e50, actual_cols_self_e65, actual_cols_self_e80, actual_cols_self_e95,
                              actual_cols_other_e50, actual_cols_other_e65, actual_cols_other_e80, actual_cols_other_e95)
check_cols_exist <- function(df, cols_to_check) {
  missing_cols <- cols_to_check[!cols_to_check %in% names(df)]
  if (length(missing_cols) > 0) {
    warning(paste("Las siguientes columnas no se encontraron:", paste(missing_cols, collapse=", ")))
    cols_to_check <- cols_to_check[cols_to_check %in% names(df)]
    if(length(cols_to_check) == 0) return(FALSE)
  }
  return(TRUE)
}
if (!check_cols_exist(datos, all_cols_to_check_effort)) {
  stop("Algunas columnas necesarias para el análisis de esfuerzo no existen. Revisa los nombres.")
}

# 4. Calcular proporciones de trabajo POR PARTICIPANTE para cada categoría de esfuerzo
calculate_prop_per_participant <- function(df, column_list, trials_per_cat) {
  valid_columns <- column_list[column_list %in% names(df)]
  if (length(valid_columns) == 0) {
    return(rep(NA_real_, nrow(df))) 
  }
  work_counts <- rowSums(df[, valid_columns, drop = FALSE] == 1, na.rm = TRUE)
  return(work_counts / trials_per_cat)
}

# Incluir GRUPO en participant_data_effort
participant_data_effort <- data.frame(
  Participant_ID = datos$Participant_ID,
  GRUPO = datos$GRUPO # <--- AÑADIDO GRUPO AQUÍ
)
participant_data_effort$SELF_E50 <- calculate_prop_per_participant(datos, actual_cols_self_e50, num_trials_per_effort_category)
participant_data_effort$SELF_E65 <- calculate_prop_per_participant(datos, actual_cols_self_e65, num_trials_per_effort_category)
participant_data_effort$SELF_E80 <- calculate_prop_per_participant(datos, actual_cols_self_e80, num_trials_per_effort_category)
participant_data_effort$SELF_E95 <- calculate_prop_per_participant(datos, actual_cols_self_e95, num_trials_per_effort_category)

participant_data_effort$OTHER_E50 <- calculate_prop_per_participant(datos, actual_cols_other_e50, num_trials_per_effort_category)
participant_data_effort$OTHER_E65 <- calculate_prop_per_participant(datos, actual_cols_other_e65, num_trials_per_effort_category)
participant_data_effort$OTHER_E80 <- calculate_prop_per_participant(datos, actual_cols_other_e80, num_trials_per_effort_category)
participant_data_effort$OTHER_E95 <- calculate_prop_per_participant(datos, actual_cols_other_e95, num_trials_per_effort_category)


# 5. Reestructurar datos a formato largo (long format)
data_long_effort <- participant_data_effort %>%
  # GRUPO se mantiene como columna identificadora junto con Participant_ID
  pivot_longer(cols = -c(Participant_ID, GRUPO), 
               names_to = "Group",
               values_to = "Proportion") %>%
  separate(Group, into = c("Condition", "Effort_Code"), sep = "_E", remove = FALSE) %>%
  mutate(
    Condition = factor(Condition, levels = c("SELF", "OTHER")),
    Effort_Level_Num = as.numeric(Effort_Code), 
    Effort_Level = factor(
      paste0("Esfuerzo ", Effort_Level_Num, "%"), 
      levels = c("Esfuerzo 50%", "Esfuerzo 65%", "Esfuerzo 80%", "Esfuerzo 95%") 
    )
  )

# 6. Calcular Media, SD, N, y SE para cada grupo de esfuerzo (AHORA TAMBIÉN AGRUPANDO POR GRUPO)
plot_data_effort <- data_long_effort %>%
  filter(!is.na(Effort_Level) & !is.na(GRUPO)) %>% 
  # AÑADIR GRUPO A LA AGRUPACIÓN
  group_by(Condition, Effort_Level, GRUPO) %>% 
  summarise(
    Mean_Proportion = mean(Proportion, na.rm = TRUE),
    SD_Proportion = sd(Proportion, na.rm = TRUE),
    N = sum(!is.na(Proportion)),
    SE_Proportion = SD_Proportion / sqrt(N),
    .groups = 'drop'
  ) %>%
  mutate(
    ymin = pmax(0, Mean_Proportion - SE_Proportion),
    ymax = pmin(1, Mean_Proportion + SE_Proportion) 
  )

# 7. Generar el gráfico con ggplot2 (CON PANELES PARA CADA GRUPO)
ggplot(plot_data_effort, aes(x = Effort_Level, y = Mean_Proportion, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    position = position_dodge(width = 0.9),
    width = 0.25, 
    color = "gray30",
    linewidth = 0.5 
  ) +
  geom_text( 
    aes(label = scales::percent(Mean_Proportion, accuracy = 0.1), 
        y = pmin(1, ymax + 0.02) 
    ), 
    position = position_dodge(width = 0.9),
    vjust = -0.4, 
    size = 3.0    
  ) +
  labs(title = "Proporción de Trabajo por Nivel de Esfuerzo, Condición y Grupo", # Título actualizado
       x = "Nivel de Esfuerzo Requerido", 
       y = "Proporción Media de Trabajo (+/- SE)",
       fill = "Condición") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.2), 
    expand = expansion(mult = c(0, 0.01)) 
  ) +
  # AÑADIR FACET_WRAP PARA DIVIDIR POR GRUPO
  facet_wrap(~ GRUPO, ncol = 2) + # ncol=2 para ponerlos lado a lado si hay 2 grupos
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    strip.text = element_text(face="bold", size=12) # Estilo para los títulos de los paneles (facets)
  )







library(scales) # Para el formato de porcentaje en el eje Y

# 1. Cargar el conjunto de datos original o usar el modificado del paso anterior
# Asumiré que 'datos_modificados' ya existe y contiene 'trabajo_self' y 'trabajo_other'
# Si no, primero ejecuta el script anterior para crear esas columnas.
# Por ejemplo:
# datos_modificados <- read.csv("datos_clean.csv")
# # ... (código para crear trabajo_self y trabajo_other) ...
# Este es el código del paso anterior para asegurar que las columnas existen:

columnas_self <- grep("^condicion_SELF_", names(datos_modificados), value = TRUE)
if (length(columnas_self) > 0) {
  datos_modificados$trabajo_self <- (rowSums(datos_modificados[, columnas_self, drop = FALSE] == 1, na.rm = TRUE) / length(columnas_self)) * 100
} else {
  warning("Columnas 'condicion_SELF_' no encontradas, 'trabajo_self' podría no ser calculada correctamente.")
  datos_modificados$trabajo_self <- NA # Evitar error si no existen
}

columnas_other <- grep("^condicion_OTHER_", names(datos_modificados), value = TRUE)
if (length(columnas_other) > 0) {
  datos_modificados$trabajo_other <- (rowSums(datos_modificados[, columnas_other, drop = FALSE] == 1, na.rm = TRUE) / length(columnas_other)) * 100
} else {
  warning("Columnas 'condicion_OTHER_' no encontradas, 'trabajo_other' podría no ser calculada correctamente.")
  datos_modificados$trabajo_other <- NA # Evitar error si no existen
}


# 2. Asegurar una columna de identificación de participante (Participant_ID)
# Usaremos ID_check si existe, o crearemos un ID secuencial.
if ("ID_check" %in% names(datos_modificados)) {
  datos_modificados$Participant_ID_Factor <- factor(datos_modificados$ID_check)
} else if ("Unnamed..0" %in% names(datos_modificados)) {
  # Usar Unnamed..0 como fallback si ID_check no está pero Unnamed..0 sí
  datos_modificados$Participant_ID_Factor <- factor(datos_modificados$Unnamed..0)
  warning("Usando 'Unnamed..0' como ID de participante. Asegúrate de que sea un identificador único.")
} else {
  datos_modificados$Participant_ID_Factor <- factor(1:nrow(datos_modificados))
  warning("No se encontró 'ID_check'. Usando un ID secuencial para los participantes.")
}


# 3. Reestructurar los datos de formato ancho a largo para ggplot2
# Necesitamos columnas: Participant_ID_Factor, Tipo_Trabajo (self/other), Porcentaje
datos_largos_individuales <- datos_modificados %>%
  select(Participant_ID_Factor, trabajo_self, trabajo_other) %>%
  pivot_longer(cols = c(trabajo_self, trabajo_other),
               names_to = "Tipo_Trabajo",
               values_to = "Porcentaje_Trabajo") %>%
  mutate(Tipo_Trabajo = factor(Tipo_Trabajo, labels = c("Trabajo OTHER", "Trabajo SELF"))) # Ajusta etiquetas si es necesario

# Verificar que no haya NAs que causen problemas en el gráfico, o decidir cómo manejarlos
# datos_largos_individuales <- na.omit(datos_largos_individuales) # Opción: omitir filas con NA

# 4. Generar el gráfico de barras agrupadas
ggplot(datos_largos_individuales, aes(x = Participant_ID_Factor, y = Porcentaje_Trabajo, fill = Tipo_Trabajo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_y_continuous(
    name = "Porcentaje de Trabajo (%)",
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = scales::percent_format(scale = 1) # La escala ya está en 0-100
  ) +
  labs(
    title = "Porcentaje de Trabajo SELF y OTHER por Participante",
    x = "Participante",
    fill = "Tipo de Trabajo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    # Ajustar el texto del eje X debido a la gran cantidad de participantes
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6), # Rotar y reducir tamaño
    # Opcionalmente, para muchos participantes, podrías quitar las etiquetas del eje X:
    # axis.text.x = element_blank(),
    # axis.ticks.x = element_blank(),
    legend.position = "top"
  )



