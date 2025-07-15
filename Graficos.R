## Prosocial Effort Task ##
# FONDECYT Sebastián Contreras #
# Diego Garrido - José Borquez #
# Viña del Mar - 2025 #

# Import Libraries
library(readxl)
library(readr)
library(tidyverse)

# Cargar datos
datos <- read.csv("datos_clean_v2.csv")

# ---- 1. Convertir SELF a long (MODIFICADO: incluir GRUPO) ----
self_long <- datos %>%
  select(id = 1, GRUPO, starts_with("condicion_SELF")) %>%   # Agregar GRUPO
  pivot_longer(
    cols = starts_with("condicion_SELF"),
    names_to = "trial",
    values_to = "respuesta"
  ) %>%
  mutate(
    condicion = "SELF",
    stim = str_extract(trial, "\\d+$")
  )

# ---- 2. Convertir OTHER a long (MODIFICADO: incluir GRUPO) ----
other_long <- datos %>%
  select(id = 1, GRUPO, starts_with("condicion_OTHER")) %>%  # Agregar GRUPO
  pivot_longer(
    cols = starts_with("condicion_OTHER"),
    names_to = "trial",
    values_to = "respuesta"
  ) %>%
  mutate(
    condicion = "OTHER",
    stim = str_extract(trial, "\\d+$")
  )

# ---- 3. Unir SELF y OTHER ----
condiciones_long <- bind_rows(self_long, other_long)

# ---- 4. Obtener reward por estímulo ----
# Combinar todas las reward columnas
reward_long <- datos %>%
  select(id = 1, starts_with("reward_")) %>%
  pivot_longer(
    cols = starts_with("reward_"),
    names_to = "reward_col",
    values_to = "valor_reward"
  ) %>%
  mutate(
    stim = str_extract(reward_col, "\\d+$"),
    reward = str_extract(reward_col, "(?<=reward_)\\d+")
  ) %>%
  select(id, stim, reward, valor_reward)

# ---- 5. Unir reward con condiciones ----
datos_completo <- condiciones_long %>%
  left_join(reward_long, by = c("id", "stim")) %>%
  mutate(
    reward = as.factor(reward),
    respuesta = as.numeric(respuesta)
  ) %>%
  filter(!is.na(respuesta))  # eliminar NA

# ---- 6. Calcular proporciones por participante (MODIFICADO: incluir GRUPO) ----
resumen_participante <- datos_completo %>%
  group_by(id, GRUPO, condicion, reward) %>%  # Agregar GRUPO
  summarise(proporcion_trabajo = mean(respuesta), .groups = "drop")

# ---- 7. Calcular promedios entre participantes (MODIFICADO: incluir GRUPO) ----
resumen_total <- resumen_participante %>%
  group_by(GRUPO, condicion, reward) %>%  # Agregar GRUPO
  summarise(
    media = mean(proporcion_trabajo),
    sd = sd(proporcion_trabajo),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )

# Convertir reward en factor ordenado
resumen_total <- resumen_total %>%
  mutate(reward = factor(reward, levels = c("2", "6", "10")))

# ---- 8. GRÁFICO CON PANELES (FACETS) ----
# Opción 1: Paneles lado a lado (horizontal)
grafico_horizontal <- ggplot(resumen_total, aes(x = reward, y = media, fill = condicion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  
  # Etiquetas numéricas encima de las barras (en porcentaje)
  geom_text(aes(label = sprintf("%.1f%%", media * 100),
                y = media + se + 0.03),  # Posicionar encima de la barra de error
            position = position_dodge(width = 0.8),
            vjust = 0, size = 3.5) +
  
  # Barras de error
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  
  # Crear paneles por grupo
  facet_wrap(~ GRUPO, ncol = 2) +
  
  # Títulos y ajustes
  labs(
    title = "Proportion of work by condition, reward and",
    x = "Reward",
    y = "Proportion Of work",
    fill = "Condition"
  ) +
  scale_fill_manual(values = c("SELF" = "#3498db", "OTHER" = "#e74c3c")) +
  scale_y_continuous(limits = c(0, 1.1),  # Aumentar límite superior para dar espacio
                     breaks = seq(0, 1, 0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "top"
  )

# Mostrar gráfico horizontal
print(grafico_horizontal)

# Opción 2: Paneles uno encima del otro (vertical)
grafico_vertical <- ggplot(resumen_total, aes(x = reward, y = media, fill = condicion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  
  # Etiquetas numéricas encima de las barras (en porcentaje)
  geom_text(aes(label = sprintf("%.1f%%", media * 100),
                y = media + se + 0.03),  # Posicionar encima de la barra de error
            position = position_dodge(width = 0.8),
            vjust = 0, size = 3.5) +
  
  # Barras de error
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  
  # Crear paneles por grupo (vertical)
  facet_wrap(~ GRUPO, ncol = 1) +
  
  # Títulos y ajustes
  labs(
    title = "Proporción de trabajo por condición, reward y grupo",
    x = "Reward",
    y = "Proporción de trabajo",
    fill = "Condición"
  ) +
  scale_fill_manual(values = c("SELF" = "#3498db", "OTHER" = "#e74c3c")) +
  scale_y_continuous(limits = c(0, 1.1),  # Aumentar límite superior para dar espacio
                     breaks = seq(0, 1, 0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "top"
  )

print(grafico_vertical)

# Opcional: Crear tabla comparativa
tabla_comparativa <- resumen_total %>%
  mutate(media_porcentaje = sprintf("%.1f%%", media * 100)) %>%
  select(GRUPO, condicion, reward, media_porcentaje, n) %>%
  pivot_wider(names_from = c(condicion, reward), 
              values_from = media_porcentaje,
              names_glue = "{condicion}_{reward}")

print("\nTabla comparativa de proporciones (%):")
print(tabla_comparativa)










## Gráfico de Diferencia SELF - OTHER ##
# Análisis de sesgo prosocial #

# Asumiendo que ya tienes los datos procesados del script anterior
# Si no, ejecuta primero los pasos 1-6 del script anterior

# ---- Calcular diferencias SELF - OTHER ----

# Verificar que tenemos los datos necesarios
if(!exists("resumen_participante")) {
  stop("Primero debes ejecutar los pasos 1-6 del script anterior para generar 'resumen_participante'")
}

# Primero, reorganizar los datos para tener SELF y OTHER en columnas separadas
datos_diferencia <- resumen_participante %>%
  pivot_wider(
    names_from = condicion,
    values_from = proporcion_trabajo,
    names_prefix = "prop_"
  ) %>%
  mutate(
    diferencia = prop_SELF - prop_OTHER  # Calcular diferencia
  ) %>%
  filter(!is.na(diferencia))  # Eliminar casos con NA

# Calcular estadísticas de la diferencia por grupo y reward
resumen_diferencia <- datos_diferencia %>%
  group_by(GRUPO, reward) %>%
  summarise(
    media_diferencia = mean(diferencia),
    sd_diferencia = sd(diferencia),
    n = n(),
    se_diferencia = sd_diferencia / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    reward = factor(reward, levels = c("2", "6", "10")),
    # Convertir a porcentaje
    media_diferencia_pct = media_diferencia * 100,
    se_diferencia_pct = se_diferencia * 100
  )

# Verificar los valores calculados
print("Resumen de diferencias calculadas:")
print(resumen_diferencia)



# ---- Crear el gráfico de diferencias ----
grafico_diferencia <- ggplot(resumen_diferencia, 
                             aes(x = reward, y = media_diferencia_pct, fill = GRUPO)) +
  # Barras agrupadas
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  
  # Línea horizontal en y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  
  # Barras de error
  geom_errorbar(aes(ymin = media_diferencia_pct - se_diferencia_pct, 
                    ymax = media_diferencia_pct + se_diferencia_pct),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  
  # Etiquetas con valores
  geom_text(aes(label = sprintf("%.1f%%", media_diferencia_pct),
                y = ifelse(media_diferencia_pct >= 0,
                           media_diferencia_pct + se_diferencia_pct + 3,  # 3% arriba del error para valores positivos
                           media_diferencia_pct - se_diferencia_pct - 3)), # 3% abajo del error para valores negativos
            position = position_dodge(width = 0.8),
            vjust = 0.5,
            size = 3.5,
            fontface = "bold") +
  
  # Personalización
  labs(
    title = "Difference on proportion of work: Self - Other",
    subtitle = "Positive values show more work for self than other",
    x = "Reward",
    y = "Difference on proportion of work (%)",
    fill = "Group"
  ) +
  
  # Colores distintivos para los grupos
  scale_fill_manual(values = c("Control" = "#3498db", "Vulnerable" = "#e74c3c")) +
  
  # Ajustar escala Y
  scale_y_continuous(
    limits = c(0, 30),  # Ampliar límites para acomodar las etiquetas
    breaks = seq(-40, 40, 10)
  ) +
  
  # Tema
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  
  # Etiquetas personalizadas para el eje X
  scale_x_discrete(labels = c("2" = "Reward 2", "6" = "Reward 6", "10" = "Reward 10"))


# Mostrar el gráfico
print(grafico_diferencia)

# ---- Mostrar tabla resumen ----
tabla_resumen_diferencia <- resumen_diferencia %>%
  select(GRUPO, reward, media_diferencia_pct, se_diferencia_pct, n) %>%
  mutate(
    diferencia = sprintf("%.1f%% ± %.1f%%", media_diferencia_pct, se_diferencia_pct)
  ) %>%
  select(GRUPO, reward, diferencia, n) %>%
  pivot_wider(names_from = reward, values_from = diferencia)

print("\nTabla resumen de diferencias SELF - OTHER (%):")
print(tabla_resumen_diferencia)

# ---- Análisis adicional: Test estadístico ----
# Test t para verificar si las diferencias son significativas (diferentes de 0)
print("\n\nTests estadísticos - Diferencia significativa de 0:")
for(grupo in unique(datos_diferencia$GRUPO)) {
  for(rew in c("2", "6", "10")) {
    datos_subset <- datos_diferencia %>%
      filter(GRUPO == grupo, reward == rew)
    
    if(nrow(datos_subset) > 1) {
      t_test <- t.test(datos_subset$diferencia, mu = 0)
      print(sprintf("%s - Reward %s: t = %.2f, p = %.4f", 
                    grupo, rew, t_test$statistic, t_test$p.value))
    }
  }
}

















# ---- 1. Convertir SELF a long ----
self_long <- datos %>%
  select(id = 1, starts_with("condicion_SELF")) %>%
  pivot_longer(
    cols = starts_with("condicion_SELF"),
    names_to = "trial",
    values_to = "respuesta"
  ) %>%
  mutate(
    condicion = "SELF",
    stim = str_extract(trial, "\\d+$")
  )

# ---- 2. Convertir OTHER a long ----
other_long <- datos %>%
  select(id = 1, starts_with("condicion_OTHER")) %>%
  pivot_longer(
    cols = starts_with("condicion_OTHER"),
    names_to = "trial",
    values_to = "respuesta"
  ) %>%
  mutate(
    condicion = "OTHER",
    stim = str_extract(trial, "\\d+$")
  )

# ---- 3. Unir SELF y OTHER ----
condiciones_long <- bind_rows(self_long, other_long)

# ---- 4. Obtener ESFUERZO por estímulo ----
# Combinar todas las columnas de esfuerzo
esfuerzo_long <- datos %>%
  select(id = 1, starts_with("esfuerzo_")) %>%
  pivot_longer(
    cols = starts_with("esfuerzo_"),
    names_to = "esfuerzo_col",
    values_to = "valor_esfuerzo"
  ) %>%
  mutate(
    stim = str_extract(esfuerzo_col, "\\d+$"),
    nivel_esfuerzo = str_extract(esfuerzo_col, "(?<=esfuerzo_)\\d+")
  ) %>%
  filter(!is.na(valor_esfuerzo)) %>%  # Solo mantener los que tienen valor
  select(id, stim, nivel_esfuerzo)

# ---- 5. Unir esfuerzo con condiciones ----
datos_completo <- condiciones_long %>%
  left_join(esfuerzo_long, by = c("id", "stim")) %>%
  mutate(
    nivel_esfuerzo = factor(nivel_esfuerzo, levels = c("50", "65", "80", "95")),
    respuesta = as.numeric(respuesta)
  ) %>%
  filter(!is.na(respuesta), !is.na(nivel_esfuerzo))  # eliminar NA

# ---- 6. Calcular proporciones por participante ----
resumen_participante <- datos_completo %>%
  group_by(id, condicion, nivel_esfuerzo) %>%
  summarise(proporcion_trabajo = mean(respuesta), .groups = "drop")

# ---- 7. Calcular promedios entre participantes ----
resumen_total <- resumen_participante %>%
  group_by(condicion, nivel_esfuerzo) %>%
  summarise(
    media = mean(proporcion_trabajo),
    sd = sd(proporcion_trabajo),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )

# ---- 8. Graficar con mejoras ----
grafico_esfuerzo <- ggplot(resumen_total, aes(x = nivel_esfuerzo, y = media, fill = condicion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  
  # Etiquetas numéricas encima de las barras (en porcentaje)
  geom_text(aes(label = sprintf("%.1f%%", media * 100),
                y = media + se + 0.03),
            position = position_dodge(width = 0.8),
            vjust = 0, size = 4,
            fontface = "bold") +
  
  # Barras de error
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  
  # Títulos y ajustes
  labs(
    title = "Proporción de trabajo por nivel de esfuerzo y condición",
    subtitle = "Promedio de decisiones de trabajo según el esfuerzo requerido",
    x = "Nivel de Esfuerzo (%)",
    y = "Proporción de trabajo",
    fill = "Condición"
  ) +
  
  # Colores personalizados
  scale_fill_manual(values = c("SELF" = "#3498db", "OTHER" = "#e74c3c")) +
  
  # Escala Y en porcentaje
  scale_y_continuous(limits = c(0, 1.1), 
                     breaks = seq(0, 1, 0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  
  # Personalizar etiquetas del eje X
  scale_x_discrete(labels = c("50" = "50%", "65" = "65%", "80" = "80%", "95" = "95%")) +
  
  # Tema
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Mostrar el gráfico
print(grafico_esfuerzo)

# Guardar el gráfico
ggsave("proporcion_trabajo_por_esfuerzo.png", 
       plot = grafico_esfuerzo, 
       width = 10, 
       height = 7, 
       dpi = 300)

# ---- Mostrar tabla resumen ----
tabla_resumen <- resumen_total %>%
  mutate(
    proporcion_pct = sprintf("%.1f%% ± %.1f%%", media * 100, se * 100)
  ) %>%
  select(condicion, nivel_esfuerzo, proporcion_pct, n) %>%
  pivot_wider(names_from = nivel_esfuerzo, 
              values_from = proporcion_pct)

print("\nTabla resumen de proporciones por nivel de esfuerzo:")
print(tabla_resumen)

# ---- Análisis adicional: tendencia ----
print("\n\nAnálisis de tendencia:")
# Verificar si hay una tendencia lineal con el aumento del esfuerzo
for(cond in c("SELF", "OTHER")) {
  datos_cond <- resumen_total %>% 
    filter(condicion == cond) %>%
    mutate(esfuerzo_num = as.numeric(as.character(nivel_esfuerzo)))
  
  correlacion <- cor.test(datos_cond$esfuerzo_num, datos_cond$media)
  print(sprintf("%s: r = %.3f, p = %.4f", 
                cond, correlacion$estimate, correlacion$p.value))
}

























# ---- 1. Convertir SELF a long (MODIFICADO: incluir GRUPO) ----
self_long <- datos %>%
  select(id = 1, GRUPO, starts_with("condicion_SELF")) %>%  # Agregar GRUPO
  pivot_longer(
    cols = starts_with("condicion_SELF"),
    names_to = "trial",
    values_to = "respuesta"
  ) %>%
  mutate(
    condicion = "SELF",
    stim = str_extract(trial, "\\d+$")
  )

# ---- 2. Convertir OTHER a long (MODIFICADO: incluir GRUPO) ----
other_long <- datos %>%
  select(id = 1, GRUPO, starts_with("condicion_OTHER")) %>%  # Agregar GRUPO
  pivot_longer(
    cols = starts_with("condicion_OTHER"),
    names_to = "trial",
    values_to = "respuesta"
  ) %>%
  mutate(
    condicion = "OTHER",
    stim = str_extract(trial, "\\d+$")
  )

# ---- 3. Unir SELF y OTHER ----
condiciones_long <- bind_rows(self_long, other_long)

# ---- 4. Obtener ESFUERZO por estímulo ----
# Combinar todas las columnas de esfuerzo
esfuerzo_long <- datos %>%
  select(id = 1, starts_with("esfuerzo_")) %>%
  pivot_longer(
    cols = starts_with("esfuerzo_"),
    names_to = "esfuerzo_col",
    values_to = "valor_esfuerzo"
  ) %>%
  mutate(
    stim = str_extract(esfuerzo_col, "\\d+$"),
    nivel_esfuerzo = str_extract(esfuerzo_col, "(?<=esfuerzo_)\\d+")
  ) %>%
  filter(!is.na(valor_esfuerzo)) %>%  # Solo mantener los que tienen valor
  select(id, stim, nivel_esfuerzo)

# ---- 5. Unir esfuerzo con condiciones ----
datos_completo <- condiciones_long %>%
  left_join(esfuerzo_long, by = c("id", "stim")) %>%
  mutate(
    nivel_esfuerzo = factor(nivel_esfuerzo, levels = c("50", "65", "80", "95")),
    respuesta = as.numeric(respuesta)
  ) %>%
  filter(!is.na(respuesta), !is.na(nivel_esfuerzo))  # eliminar NA

# ---- 6. Calcular proporciones por participante (MODIFICADO: incluir GRUPO) ----
resumen_participante <- datos_completo %>%
  group_by(id, GRUPO, condicion, nivel_esfuerzo) %>%  # Agregar GRUPO
  summarise(proporcion_trabajo = mean(respuesta), .groups = "drop")

# ---- 7. Calcular promedios entre participantes (MODIFICADO: incluir GRUPO) ----
resumen_total <- resumen_participante %>%
  group_by(GRUPO, condicion, nivel_esfuerzo) %>%  # Agregar GRUPO
  summarise(
    media = mean(proporcion_trabajo),
    sd = sd(proporcion_trabajo),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )

# ---- 8. Graficar con paneles por grupo ----
grafico_esfuerzo <- ggplot(resumen_total, aes(x = nivel_esfuerzo, y = media, fill = condicion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  
  # Etiquetas numéricas encima de las barras (en porcentaje)
  geom_text(aes(label = sprintf("%.1f%%", media * 100),
                y = media + se + 0.03),
            position = position_dodge(width = 0.8),
            vjust = 0, size = 3.5,
            fontface = "bold") +
  
  # Barras de error
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  
  # Crear paneles por grupo (horizontal)
  facet_wrap(~ GRUPO, ncol = 2) +
  
  # Títulos y ajustes
  labs(
    title = "Proportion of work by effort level, condition and group",
    x = "Effort level",
    y = "Proportion of work",
    fill = "Condition"
  ) +
  
  # Colores personalizados
  scale_fill_manual(values = c("SELF" = "#3498db", "OTHER" = "#e74c3c")) +
  
  # Escala Y en porcentaje
  scale_y_continuous(limits = c(0, 1.15), 
                     breaks = seq(0, 1, 0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  
  # Personalizar etiquetas del eje X
  scale_x_discrete(labels = c("50" = "50%", "65" = "65%", "80" = "80%", "95" = "95%")) +
  
  # Tema
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    panel.spacing = unit(1, "lines")
  )

# Mostrar el gráfico
print(grafico_esfuerzo)

# Guardar el gráfico
ggsave("proporcion_trabajo_por_esfuerzo_grupos.png", 
       plot = grafico_esfuerzo, 
       width = 12, 
       height = 6, 
       dpi = 300)

# ---- Mostrar tabla resumen por grupo ----
tabla_resumen <- resumen_total %>%
  mutate(
    proporcion_pct = sprintf("%.1f%%", media * 100)
  ) %>%
  select(GRUPO, condicion, nivel_esfuerzo, proporcion_pct) %>%
  pivot_wider(names_from = c(condicion, nivel_esfuerzo), 
              values_from = proporcion_pct,
              names_glue = "{condicion}_{nivel_esfuerzo}")

print("\nTabla resumen de proporciones por nivel de esfuerzo y grupo:")
print(tabla_resumen)

# ---- Análisis adicional: tendencia por grupo ----
print("\n\nAnálisis de tendencia por grupo:")
# Verificar si hay una tendencia lineal con el aumento del esfuerzo
for(grupo in unique(resumen_total$GRUPO)) {
  print(paste("\nGrupo:", grupo))
  for(cond in c("SELF", "OTHER")) {
    datos_cond <- resumen_total %>% 
      filter(GRUPO == grupo, condicion == cond) %>%
      mutate(esfuerzo_num = as.numeric(as.character(nivel_esfuerzo)))
    
    if(nrow(datos_cond) > 2) {
      correlacion <- cor.test(datos_cond$esfuerzo_num, datos_cond$media)
      print(sprintf("  %s: r = %.3f, p = %.4f", 
                    cond, correlacion$estimate, correlacion$p.value))
    }
  }
}



























# Asumiendo que ya tienes los datos procesados del script anterior
# Si no, ejecuta primero los pasos 1-6 del script anterior

# ---- Verificar datos necesarios ----
if(!exists("resumen_participante")) {
  stop("Primero debes ejecutar los pasos 1-6 del script anterior para generar 'resumen_participante'")
}

# ---- Calcular diferencias SELF - OTHER ----

# Reorganizar datos para tener SELF y OTHER en columnas separadas
datos_diferencia <- resumen_participante %>%
  pivot_wider(
    names_from = condicion,
    values_from = proporcion_trabajo,
    names_prefix = "prop_"
  ) %>%
  mutate(
    diferencia = prop_SELF - prop_OTHER  # Calcular diferencia
  ) %>%
  filter(!is.na(diferencia))  # Eliminar casos con NA

# Calcular estadísticas de la diferencia por grupo y nivel de esfuerzo
resumen_diferencia <- datos_diferencia %>%
  group_by(GRUPO, nivel_esfuerzo) %>%
  summarise(
    media_diferencia = mean(diferencia),
    sd_diferencia = sd(diferencia),
    n = n(),
    se_diferencia = sd_diferencia / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    nivel_esfuerzo = factor(nivel_esfuerzo, levels = c("50", "65", "80", "95")),
    # Convertir a porcentaje
    media_diferencia_pct = media_diferencia * 100,
    se_diferencia_pct = se_diferencia * 100
  )

# Verificar los valores calculados
print("Resumen de diferencias calculadas:")
print(resumen_diferencia)

# ---- Crear el gráfico de diferencias ----
grafico_diferencia_esfuerzo <- ggplot(resumen_diferencia, 
                                      aes(x = nivel_esfuerzo, y = media_diferencia_pct, fill = GRUPO)) +
  # Barras agrupadas
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  
  # Línea horizontal en y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  
  # Barras de error
  geom_errorbar(aes(ymin = media_diferencia_pct - se_diferencia_pct, 
                    ymax = media_diferencia_pct + se_diferencia_pct),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  
  # Etiquetas con valores
  geom_text(aes(label = sprintf("%.1f%%", media_diferencia_pct),
                y = ifelse(media_diferencia_pct >= 0,
                           media_diferencia_pct + se_diferencia_pct + 3,
                           media_diferencia_pct - se_diferencia_pct - 3)),
            position = position_dodge(width = 0.8),
            vjust = 0.5,
            size = 3.5,
            fontface = "bold") +
  
  # Personalización
  labs(
    title = "Difference on proportion of work: Self - Other",
    x = "Effort level",
    y = "Diffference on proportion of work (%)",
    fill = "Group"
  ) +
  
  # Colores distintivos para los grupos
  scale_fill_manual(values = c("Control" = "#3498db", "Vulnerable" = "#e74c3c")) +
  
  # Ajustar escala Y
  scale_y_continuous(
    limits = c(0, 30),
    breaks = seq(-40, 40, 10)
  ) +
  
  # Personalizar etiquetas del eje X
  scale_x_discrete(labels = c("50" = "50%", "65" = "65%", "80" = "80%", "95" = "95%")) +
  
  # Tema
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Mostrar el gráfico
print(grafico_diferencia_esfuerzo)

# Guardar el gráfico
ggsave("diferencia_self_other_por_esfuerzo.png", 
       plot = grafico_diferencia_esfuerzo, 
       width = 10, 
       height = 7, 
       dpi = 300)

# ---- Tabla resumen ----
tabla_resumen_diferencia <- resumen_diferencia %>%
  select(GRUPO, nivel_esfuerzo, media_diferencia_pct, se_diferencia_pct, n) %>%
  mutate(
    diferencia = sprintf("%.1f%% ± %.1f%%", media_diferencia_pct, se_diferencia_pct)
  ) %>%
  select(GRUPO, nivel_esfuerzo, diferencia, n) %>%
  pivot_wider(names_from = nivel_esfuerzo, 
              values_from = diferencia,
              names_prefix = "Esfuerzo_")

print("\nTabla resumen de diferencias SELF - OTHER por nivel de esfuerzo (%):")
print(tabla_resumen_diferencia)

# ---- Análisis adicional: Tendencia de la diferencia ----
print("\n\nAnálisis de tendencia de la diferencia SELF-OTHER:")
for(grupo in unique(datos_diferencia$GRUPO)) {
  datos_grupo <- resumen_diferencia %>%
    filter(GRUPO == grupo) %>%
    mutate(esfuerzo_num = as.numeric(as.character(nivel_esfuerzo)))
  
  if(nrow(datos_grupo) > 2) {
    correlacion <- cor.test(datos_grupo$esfuerzo_num, datos_grupo$media_diferencia_pct)
    print(sprintf("%s: r = %.3f, p = %.4f", 
                  grupo, correlacion$estimate, correlacion$p.value))
  }
}

# ---- Tests estadísticos ----
print("\n\nTests estadísticos - Diferencia significativa de 0:")
for(grupo in unique(datos_diferencia$GRUPO)) {
  print(paste("\nGrupo:", grupo))
  for(esf in c("50", "65", "80", "95")) {
    datos_subset <- datos_diferencia %>%
      filter(GRUPO == grupo, nivel_esfuerzo == esf)
    
    if(nrow(datos_subset) > 1) {
      t_test <- t.test(datos_subset$diferencia, mu = 0)
      print(sprintf("  Esfuerzo %s%%: t = %.2f, p = %.4f", 
                    esf, t_test$statistic, t_test$p.value))
    }
  }
}

# Interpretación
print("\n\nInterpretación:")
print("- Valores positivos: Mayor proporción de trabajo para SELF que para OTHER (sesgo egoísta)")
print("- Valores negativos: Mayor proporción de trabajo para OTHER que para SELF (sesgo prosocial)")
print("- Valor 0: No hay diferencia entre SELF y OTHER")
print("- La tendencia muestra cómo el sesgo cambia con el nivel de esfuerzo requerido")

