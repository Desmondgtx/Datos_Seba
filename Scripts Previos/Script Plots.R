
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



## Gráfica respuestas datos completos ##
{
# Identificar columnas SELF y OTHER
cols_self <- grep("SELF", names(datos), value = TRUE)
cols_other <- grep("OTHER", names(datos), value = TRUE)

# Reunir datos en formato largo
self_data <- datos %>%
  select(all_of(cols_self)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "respuesta") %>%
  mutate(tipo = "SELF")

other_data <- datos %>%
  select(all_of(cols_other)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "respuesta") %>%
  mutate(tipo = "OTHER")

# Unir ambos datasets
todo <- bind_rows(self_data, other_data)

# Calcular porcentaje por tipo y respuesta
porcentajes <- todo %>%
  group_by(tipo, respuesta) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(tipo) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Ordenar respuestas: 1 (Realizar), 2 (Descansar), 0 (Omisión)
porcentajes$respuesta <- factor(porcentajes$respuesta, 
                                levels = c(1, 2, 0),
                                labels = c("Realizar tarea (1)", 
                                           "Descansar (2)", 
                                           "Omisión (0)"))

# Gráfico de barras
ggplot(porcentajes, aes(x = respuesta, y = porcentaje, fill = tipo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = sprintf("%.1f%%", porcentaje)), 
            vjust = -0.5, position = position_dodge(width = 0.8)) +
  labs(title = "Porcentaje de respuestas en condiciones SELF y OTHER",
       x = "Tipo de respuesta", y = "Porcentaje") +
  scale_fill_manual(values = c("SELF" = "#1f77b4", "OTHER" = "#ff7f0e")) +
  theme_minimal()

}



## Datos Porcentajes y Error Estandar
{
# Identificar columnas SELF y OTHER
cols_self <- grep("SELF", names(datos), value = TRUE)
cols_other <- grep("OTHER", names(datos), value = TRUE)

# Agregar columna GRUPO a cada transformación
self_data <- datos %>%
  select(GRUPO, all_of(cols_self)) %>%
  pivot_longer(cols = all_of(cols_self), names_to = "variable", values_to = "respuesta") %>%
  mutate(tipo = "SELF")

other_data <- datos %>%
  select(GRUPO, all_of(cols_other)) %>%
  pivot_longer(cols = all_of(cols_other), names_to = "variable", values_to = "respuesta") %>%
  mutate(tipo = "OTHER")

# Unir ambos datasets
todo <- bind_rows(self_data, other_data)

# Calcular porcentaje por grupo, tipo y respuesta
porcentajes <- todo %>%
  group_by(GRUPO, tipo, respuesta) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(GRUPO, tipo) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Ordenar respuestas: 1 (Realizar), 2 (Descansar), 0 (Omisión)
porcentajes$respuesta <- factor(porcentajes$respuesta, 
                                levels = c(1, 2, 0),
                                labels = c("Realizar tarea", 
                                           "Descansar", 
                                           "Omisión"))

# Gráfico con facetas por grupo
ggplot(porcentajes, aes(x = respuesta, y = porcentaje, fill = tipo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = sprintf("%.1f%%", porcentaje)), 
            vjust = -0.5, position = position_dodge(width = 0.8)) +
  labs(title = "Porcentaje de respuestas por grupo (SELF y OTHER)",
       x = "Tipo de respuesta", y = "Porcentaje") +
  scale_fill_manual(values = c("SELF" = "#1f77b4", "OTHER" = "#ff7f0e")) +
  facet_wrap(~GRUPO) +
  theme_minimal()
}




# Datos Porcentajes y Error Estandar
{
# Identificar columnas SELF y OTHER
cols_self <- grep("SELF", names(datos), value = TRUE)
cols_other <- grep("OTHER", names(datos), value = TRUE)

# Agregar columna PARTICIPANTE si no existe
# Si tienes un ID único por participante, cambia "row_number()" por ese ID real
datos$PARTICIPANTE <- datos$ID_check

# Transformar a formato largo, incluyendo ID y grupo
self_data <- datos %>%
  select(PARTICIPANTE, GRUPO, all_of(cols_self)) %>%
  pivot_longer(cols = all_of(cols_self), names_to = "variable", values_to = "respuesta") %>%
  mutate(tipo = "SELF")

other_data <- datos %>%
  select(PARTICIPANTE, GRUPO, all_of(cols_other)) %>%
  pivot_longer(cols = all_of(cols_other), names_to = "variable", values_to = "respuesta") %>%
  mutate(tipo = "OTHER")

# Unir ambos datasets
todo <- bind_rows(self_data, other_data)

# Calcular porcentaje por participante
por_participante <- todo %>%
  group_by(PARTICIPANTE, GRUPO, tipo, respuesta) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(PARTICIPANTE, GRUPO, tipo) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Estadísticas agregadas: media y error estándar
stats <- por_participante %>%
  group_by(GRUPO, tipo, respuesta) %>%
  summarise(
    media = mean(porcentaje),
    se = sd(porcentaje) / sqrt(n()),
    .groups = "drop"
  )

# Ordenar niveles de respuesta
stats$respuesta <- factor(stats$respuesta,
                          levels = c(1, 2, 0),
                          labels = c("Realizar tarea (1)", "Descansar (2)", "Omisión (0)"))

# Gráfico con barras de error
ggplot(stats, aes(x = respuesta, y = media, fill = tipo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.2,
                position = position_dodge(width = 0.8)) +
  geom_text(aes(label = sprintf("%.1f%%", media)),
            vjust = -0.5, position = position_dodge(width = 0.8)) +
  labs(title = "Porcentaje de respuestas por grupo con error estándar",
       x = "Tipo de respuesta", y = "Porcentaje promedio") +
  scale_fill_manual(values = c("SELF" = "#1f77b4", "OTHER" = "#ff7f0e")) +
  facet_wrap(~GRUPO) +
  theme_minimal()



# Gráfico con barras y error estándar (sin porcentaje en texto)
ggplot(stats, aes(x = respuesta, y = media, fill = tipo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.2,
                position = position_dodge(width = 0.8)) +
  labs(title = "Porcentaje de respuestas por grupo con error estándar",
       x = "Tipo de respuesta", y = "Porcentaje promedio") +
  scale_fill_manual(values = c("SELF" = "#1f77b4", "OTHER" = "#ff7f0e")) +
  facet_wrap(~GRUPO) +
  theme_minimal()

}




# Separar los datos de SELF y OTHER
self_vals <- stats %>% filter(tipo == "SELF") %>%
  rename(media_self = media, se_self = se) %>%
  select(GRUPO, respuesta, media_self)

other_vals <- stats %>% filter(tipo == "OTHER") %>%
  rename(media_other = media, se_other = se) %>%
  select(GRUPO, respuesta, media_other)

# Unir ambos para poder hacer la resta
diferencias <- left_join(other_vals, self_vals, by = c("GRUPO", "respuesta")) %>%
  mutate(diferencia = media_other - media_self)

# Gráfico de las diferencias OTHER - SELF por tipo de respuesta
ggplot(diferencias, aes(x = respuesta, y = diferencia, fill = GRUPO)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = sprintf("%.1f%%", diferencia)),
            vjust = -0.5, position = position_dodge(width = 0.8)) +
  labs(title = "Diferencia de porcentaje (OTHER - SELF) por tipo de respuesta",
       x = "Tipo de respuesta", y = "Diferencia de porcentaje") +
  scale_fill_manual(values = c("Vulnerable" = "#d62728", "Control" = "#2ca02c")) +
  theme_minimal()













# Separar los datos de SELF y OTHER
self_vals <- stats %>% filter(tipo == "SELF") %>%
  rename(media_self = media, se_self = se) %>%
  select(GRUPO, respuesta, media_self)

other_vals <- stats %>% filter(tipo == "OTHER") %>%
  rename(media_other = media, se_other = se) %>%
  select(GRUPO, respuesta, media_other)

# Unir ambos para poder hacer la resta
diferencias <- left_join(other_vals, self_vals, by = c("GRUPO", "respuesta")) %>%
  mutate(diferencia = media_self - media_other)

# Gráfico de las diferencias OTHER - SELF por tipo de respuesta
ggplot(diferencias, aes(x = respuesta, y = diferencia, fill = GRUPO)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = sprintf("%.1f%%", diferencia)),
            vjust = -0.5, position = position_dodge(width = 0.8)) +
  labs(title = "Diferencia de porcentaje (OTHER - SELF) por tipo de respuesta",
       x = "Tipo de respuesta", y = "Diferencia de porcentaje") +
  scale_fill_manual(values = c("Vulnerable" = "#d62728", "Control" = "#2ca02c")) +
  theme_minimal()











# Partimos del dataframe por participante (por_participante)

# Agrupamos sin separar por GRUPO
global_stats <- por_participante %>%
  group_by(tipo, respuesta) %>%
  summarise(
    media = mean(porcentaje),
    se = sd(porcentaje) / sqrt(n()),
    .groups = "drop"
  )

# Separar SELF y OTHER
self_vals_global <- global_stats %>% filter(tipo == "SELF") %>%
  rename(media_self = media, se_self = se) %>%
  select(respuesta, media_self)

other_vals_global <- global_stats %>% filter(tipo == "OTHER") %>%
  rename(media_other = media, se_other = se) %>%
  select(respuesta, media_other)

# Unir y calcular la diferencia
diferencias_global <- left_join(other_vals_global, self_vals_global, by = "respuesta") %>%
  mutate(diferencia = media_other - media_self)

# Convertir los valores de respuesta en etiquetas legibles
diferencias_global$respuesta <- factor(diferencias_global$respuesta,
                                       levels = c(1, 2, 0),
                                       labels = c("Realizar tarea (1)", "Descansar (2)", "Omisión (0)"))

# Graficar
ggplot(diferencias_global, aes(x = respuesta, y = diferencia, fill = respuesta)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", diferencia)),
            vjust = -0.5) +
  labs(title = "Diferencia global de porcentaje (OTHER - SELF)",
       x = "Tipo de respuesta", y = "Diferencia de porcentaje") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  theme_minimal()





# Usamos nuevamente self_vals_global y other_vals_global (ya definidos)

# Unir y calcular la diferencia como SELF - OTHER
diferencias_global <- left_join(self_vals_global, other_vals_global, by = "respuesta") %>%
  mutate(diferencia = media_self - media_other)

# Convertir los valores de respuesta en etiquetas legibles
diferencias_global$respuesta <- factor(diferencias_global$respuesta,
                                       levels = c("1", "2", "0"),
                                       labels = c("Realizar tarea (1)", "Descansar (2)", "Omisión (0)"))

# Graficar SELF - OTHER
ggplot(diferencias_global, aes(x = respuesta, y = diferencia, fill = respuesta)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", diferencia)),
            vjust = -0.5) +
  labs(title = "Diferencia global de porcentaje (SELF - OTHER)",
       x = "Tipo de respuesta", y = "Diferencia de porcentaje") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  theme_minimal()







