
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
# Here is necessary identify relevant columns (such as "X_choice_screen")
# In this dataset exists two rows as column names
# The need here is to put some second row names as first rows
# In order to work just with ONE row as column names (as usual)

# Import dataset
effort_task = read_excel("Prosocial_effort_task_Seba.xlsx")

# Delete  all columns with "prac" on their name
effort_task = effort_task[, !grepl("prac", names(effort_task), ignore.case = TRUE)]

# Delete  all columns with "boxes" on their name
effort_task = effort_task[, !grepl("boxes", names(effort_task), ignore.case = TRUE)] 

# Save datafile
write.csv(effort_task, "datos_limpios.csv")

# Read dataset without headers
effort_task = read_csv("datos_limpios.csv", col_names = FALSE)

# Extract two first rows as variables (column names)
header1 = effort_task[1, ] |> unlist(use.names = FALSE)
header2 = effort_task[2, ] |> unlist(use.names = FALSE)

# Create JUST ONE new column names based on specific conditions
new_colnames = mapply(function(col_name, second_row_val) {
  # Identify if the name is exactly "X_choice_screen"
  if (grepl("^\\d+_choice_screen$", col_name)) {
    return(second_row_val)
  } else {
    return(col_name)
  }
}, header1, header2, USE.NAMES = FALSE)

# Read data, omiting two first rows
effort_task = read_csv("datos_limpios.csv", skip = 2, col_names = new_colnames)

# Save datafile
write.csv(effort_task, "datos_limpios.csv")



# ===========================
# Work with Participantes.xlsx dataset

# Import data
effort_task = read_csv("datos_limpios.csv")
participantes_ID = read_csv("Participantes_Final_ID.csv")


# ===========================
## ID Checking ## 

# Checking for "effort_task" dataset ID duplicates
duplicados_effort = effort_task$ID_check[duplicated(effort_task$ID_check)]
cat("\nDuplicados en effort_task (ID_check):\n")
if(length(duplicados_effort) > 0) {
  cat("- Cantidad de IDs duplicados:", length(unique(duplicados_effort)), "\n")
  cat("- IDs duplicados:", unique(duplicados_effort), "\n")
  cat("- Total de registros duplicados:", length(duplicados_effort), "\n")
  
  # See duplicates details
  for(id in unique(duplicados_effort)) {
    cat("\n  ID", id, "aparece", sum(effort_task$ID_check == id), "veces\n")
  }
} else {
  cat("- No hay duplicados\n")
}
# Duplicates
# 0000, 2411, 8053, 9477, 7997, 1304, 4625, 0155, 5495


# Verify existing IDs on "participantes_ID" that are duplicated on "effort_task"
verificar_duplicados_cruzados = function(effort_task, participantes_ID) {
  
  # Unique IDs on participantes_ID dataset
  ids_participantes = unique(participantes_ID$id)
  
  # Count frequency of each ID on effort_task
  frecuencia_ids = table(effort_task$ID_check)
  
  # Filter IDs that are on "participantes_ID" and appear duplicated on "effort_task" dataset
  ids_duplicados = names(frecuencia_ids[frecuencia_ids > 1 & names(frecuencia_ids) %in% ids_participantes])
  
  # Show results on console
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

# Execute Function
ids_con_duplicados = verificar_duplicados_cruzados(effort_task, participantes_ID)
# IDs existing on "participantes_ID" dataset that are duplicated on "effort_Task" dataset:
# ID 0155 → appears 2 times in effort_task 85 & 88 (null values)
# ID 2411 → appears 2 times in effort_task 16 & 27 (null values)
# ID 4625 → appears 2 times in effort_task 71 & 76 (null values)
# ID 5495 → appears 2 times in effort_task 111 & 126 (74% of progress on the 2nd one)
# ID 9477 → appears 2 times in effort_task 35 & 44 (null values)


# Unique IDs on each dataset
ids_effort = unique(effort_task$ID_check)
ids_participantes = unique(participantes_ID$id)

cat("- Total IDs únicos en effort_task:", length(ids_effort), "\n")
# - Total uniqes ID in effort_task: 118 
cat("- Total IDs únicos en participantes_ID:", length(ids_participantes), "\n")
# - Total uniqes ID in participantes_ID: 102 


# Match IDs
ids_coinciden = intersect(ids_effort, ids_participantes)
cat("\n- IDs que SÍ coinciden:", length(ids_coinciden), "\n")
if(length(ids_coinciden) <= 20) {
  cat("  IDs:", ids_coinciden, "\n")
}
# - IDs that match: 101 


# IDs that does not match (existing on effort_task but not in participantes_ID)
ids_sin_match_effort = setdiff(ids_effort, ids_participantes)
cat("\n- IDs en effort_task que NO tienen match:", length(ids_sin_match_effort), "\n")
if(length(ids_sin_match_effort) > 0 & length(ids_sin_match_effort) <= 20) {
  cat("  IDs sin match:", ids_sin_match_effort, "\n")
}
# - IDs in effort_task that does not match: 17 
# - IDs swithout a match: P123 8053 7565 2676 0336 1559 1857 1693 3790 0000 1304 test1 2251 7997 4046 0066 3965 


# IDs that are contained on participantes_ID but not in effort_task
ids_sin_usar = setdiff(ids_participantes, ids_effort)
cat("\n- IDs en participantes_ID que NO se usarán:", length(ids_sin_usar), "\n")
if(length(ids_sin_usar) > 0 & length(ids_sin_usar) <= 20) {
  cat("  IDs no usados:", ids_sin_usar, "\n")
}
# - IDs not used: 8831 


# ===========================
## Data cleaning ##

# Function that formats 0 on the left
# The real need here is that some ID like 0738 is left as 738 because of excel
formatear_id = function(id) {
  sprintf("%04d", as.integer(id))
}

# Apply function on column "id"
participantes_ID$id = sapply(participantes_ID$id, formatear_id)

# Join dataset by id column
effort_task = left_join(effort_task, participantes_ID[, c("id", "grupo")], by = c("ID_check" = "id"))

# Put column "grupo" just after "ID_check"
effort_task = effort_task %>% relocate(grupo, .after = ID_check)

# Remove duplicates of the ID_check column
effort_task = effort_task[!duplicated(effort_task$ID_check), ]

# Replace values
effort_task$grupo[grep("V", effort_task$grupo)] <- "Vulnerable"
effort_task$grupo[grep("C", effort_task$grupo)] <- "Control"

# Remove NA of "grupo" column
effort_task = effort_task %>% filter(!is.na(grupo))

# Convert Progress column as numeric
effort_task$Progress = as.numeric(gsub("%", "", effort_task$Progress))

# Replace NA for 0 on SELF & OTHER columns
effort_task = effort_task %>%
  mutate(across(contains("SELF"), ~replace_na(., 0))) %>%
  mutate(across(contains("OTHER"), ~replace_na(., 0)))


# ===========================
# Last checkings

# IDs left on effort_task 
ids_faltantes = function(effort_task, participantes_ID) {
  faltantes = setdiff(participantes_ID$id, effort_task$ID_check)
  
  if(length(faltantes) > 0) {
    cat("IDs faltantes en effort_task:", faltantes, "\n")
  } else {
    cat("No faltan IDs\n")
  }
  
  return(faltantes)
}

# Execute function
ids_faltantes(effort_task, participantes_ID)
# IDs left in effort_task: 8831 


# Save file
write.csv(effort_task, "datos_limpios.csv")



# ===========================
# Part 2

# Load Data
datos = read.csv("datos_limpios.csv")

# Delete first column
datos = datos[, -1]

# Function to format 0 on the left on certain IDs
formatear_id = function(ID_check) {
  sprintf("%04d", as.integer(ID_check))
}

# Apply Function
datos$ID_check = sapply(datos$ID_check, formatear_id)


# Extract only the columns of interest
datos = datos %>% select(ID_check, grupo,
                          starts_with("SELF"), starts_with("OTHER"),
                          matches("^X\\d+_fail_feedback_timing_Page\\.Submit"),
                          Comp2_Q1, Comp2_Q2,
                          X49_attention_check)


# Attention check (stimulus N°49)
respuestas_4 = sum(datos$X49_attention_check == 4, na.rm = TRUE) # Respondieron 60 participantes
respuestas_na = sum(is.na(datos$X49_attention_check)) # NO respondieron 41 participantes


# Divide condition columns with reward, effort and difficulty 
{
  # Identify the 48 columns of stimulus (SELF.* / OTHER.*) ──
  stim_cols = names(datos) %>% str_subset("^(SELF|OTHER)\\.")
  
  # Dataset base columns
  datos_base = datos %>% select(ID_check, grupo,
                                 matches("^X\\d+_fail_feedback_timing_Page\\.Submit"),
                                 Comp2_Q1, Comp2_Q2,
                                 X49_attention_check)
  
  # Build new 192 columns (48 x 4)
  new_cols = list()
  
  for (i in seq_along(stim_cols)) {
    
    col_name  = stim_cols[i]
    respuestas = datos[[col_name]]
    
    # Extract information from the columns
    # Captures: 1‑condicion, 2‑reward, 3‑dificultad, 4‑esfuerzo
    meta = str_match(col_name,
                      "^(SELF|OTHER)\\.(\\d+)(easy|hard)(\\d+)")[ , 2:5]
    
    condicion  = meta[1]      # SELF / OTHER
    reward     = meta[2]      # 2 / 6 / 10
    dificultad = meta[3]      # easy / hard
    esfuerzo   = meta[4]      # 50 / 65 / 80 / 95
    
    idx = sprintf("%02d", i)  
    
    # Create new 4 columns with the info
    new_cols[[ paste0("condicion_",  condicion,  "_", idx) ]] <- respuestas
    new_cols[[ paste0("reward_",     reward,     "_", idx) ]] <- respuestas
    new_cols[[ paste0("dificultad_", dificultad, "_", idx) ]] <- respuestas
    new_cols[[ paste0("esfuerzo_",   esfuerzo,   "_", idx) ]] <- respuestas
  }
  
  # Unify everything
  datos = bind_cols(datos_base, as_tibble(new_cols))
}


# Reorder columns on specific order
datos = datos %>%
  select(
    # First columns
    ID_check, grupo, Comp2_Q1, Comp2_Q2, X49_attention_check,
    
    # Alternate between stimulus columns and fail_feedback columns
    # Trial 01
    contains("_01"), X1_fail_feedback_timing_Page.Submit,
    # Trial 02  
    contains("_02"), X2_fail_feedback_timing_Page.Submit,
    # Trial 03
    contains("_03"), X3_fail_feedback_timing_Page.Submit,
    # Trial 04
    contains("_04"), X4_fail_feedback_timing_Page.Submit,
    # Trial 05
    contains("_05"), X5_fail_feedback_timing_Page.Submit,
    # Trial 06
    contains("_06"), X6_fail_feedback_timing_Page.Submit,
    # Trial 07
    contains("_07"), X7_fail_feedback_timing_Page.Submit,
    # Trial 08
    contains("_08"), X8_fail_feedback_timing_Page.Submit,
    # Trial 09
    contains("_09"), X9_fail_feedback_timing_Page.Submit,
    # Trial 10
    contains("_10"), X10_fail_feedback_timing_Page.Submit,
    # Trial 11
    contains("_11"), X11_fail_feedback_timing_Page.Submit,
    # Trial 12
    contains("_12"), X12_fail_feedback_timing_Page.Submit,
    # Trial 13
    contains("_13"), X13_fail_feedback_timing_Page.Submit,
    # Trial 14
    contains("_14"), X14_fail_feedback_timing_Page.Submit,
    # Trial 15
    contains("_15"), X15_fail_feedback_timing_Page.Submit,
    # Trial 16
    contains("_16"), X16_fail_feedback_timing_Page.Submit,
    # Trial 17
    contains("_17"), X17_fail_feedback_timing_Page.Submit,
    # Trial 18
    contains("_18"), X18_fail_feedback_timing_Page.Submit,
    # Trial 19
    contains("_19"), X19_fail_feedback_timing_Page.Submit,
    # Trial 20
    contains("_20"), X20_fail_feedback_timing_Page.Submit,
    # Trial 21
    contains("_21"), X21_fail_feedback_timing_Page.Submit,
    # Trial 22
    contains("_22"), X22_fail_feedback_timing_Page.Submit,
    # Trial 23
    contains("_23"), X23_fail_feedback_timing_Page.Submit,
    # Trial 24
    contains("_24"), X24_fail_feedback_timing_Page.Submit,
    # Trial 25
    contains("_25"), X25_fail_feedback_timing_Page.Submit,
    # Trial 26
    contains("_26"), X26_fail_feedback_timing_Page.Submit,
    # Trial 27
    contains("_27"), X27_fail_feedback_timing_Page.Submit,
    # Trial 28
    contains("_28"), X28_fail_feedback_timing_Page.Submit,
    # Trial 29
    contains("_29"), X29_fail_feedback_timing_Page.Submit,
    # Trial 30
    contains("_30"), X30_fail_feedback_timing_Page.Submit,
    # Trial 31
    contains("_31"), X31_fail_feedback_timing_Page.Submit,
    # Trial 32
    contains("_32"), X32_fail_feedback_timing_Page.Submit,
    # Trial 33
    contains("_33"), X33_fail_feedback_timing_Page.Submit,
    # Trial 34
    contains("_34"), X34_fail_feedback_timing_Page.Submit,
    # Trial 35
    contains("_35"), X35_fail_feedback_timing_Page.Submit,
    # Trial 36
    contains("_36"), X36_fail_feedback_timing_Page.Submit,
    # Trial 37
    contains("_37"), X37_fail_feedback_timing_Page.Submit,
    # Trial 38
    contains("_38"), X38_fail_feedback_timing_Page.Submit,
    # Trial 39
    contains("_39"), X39_fail_feedback_timing_Page.Submit,
    # Trial 40
    contains("_40"), X40_fail_feedback_timing_Page.Submit,
    # Trial 41
    contains("_41"), X41_fail_feedback_timing_Page.Submit,
    # Trial 42
    contains("_42"), X42_fail_feedback_timing_Page.Submit,
    # Trial 43
    contains("_43"), X43_fail_feedback_timing_Page.Submit,
    # Trial 44
    contains("_44"), X44_fail_feedback_timing_Page.Submit,
    # Trial 45
    contains("_45"), X45_fail_feedback_timing_Page.Submit,
    # Trial 46
    contains("_46"), X46_fail_feedback_timing_Page.Submit,
    # Trial 47
    contains("_47"), X47_fail_feedback_timing_Page.Submit,
    # Trial 48
    contains("_48"), X48_fail_feedback_timing_Page.Submit,
    # Delete later
    X49_fail_feedback_timing_Page.Submit
  )




# Create new columns with Comp Q1 & Q2 results
datos = datos %>%
  mutate(resultados_Comp = case_when(
    Comp2_Q1 == 2 & Comp2_Q2 == 1 ~ 0,  # If Q1=2 y Q2=1, then 0
    is.na(Comp2_Q1) | is.na(Comp2_Q2) ~ 0,  # If any NA, then 0
    TRUE ~ 1  # Anything else is 1
  )) %>%
  # Put this new columns just after Comp_Q2
  relocate(resultados_Comp, .after = Comp2_Q2)



# Delete last column
datos = datos[, -247]


# Apply transformation to every "fail_feedback column" 
for(i in 1:48) {
  # Formatting trial number
  trial_num = sprintf("%02d", i)
  
  # Identify columns names
  col_condicion = grep(paste0("condicion.*_", trial_num, "$"), names(datos), value = TRUE)
  col_feedback = paste0("X", i, "_fail_feedback_timing_Page.Submit")
  
  # Apply transformation
  datos = datos %>%
    mutate(!!col_feedback := case_when(
      .data[[col_condicion]] == 0 ~ NA_real_,  # If condition = 0, convert to NA
      .data[[col_condicion]] == 1 & !is.na(.data[[col_feedback]]) ~ 1,  # If condition = 1 and some value on fail_feedback, convert to 1
      TRUE ~ .data[[col_feedback]]  # If is NA, mantaining as NA
    ))
}



# Renombrar todas las columnas de fail_feedback_timing_Page.Submit
for(i in 1:49) {
  # Nombre original
  old_name <- paste0("X", i, "_fail_feedback_timing_Page.Submit")
  # Nombre nuevo con formato de dos dígitos
  new_name <- sprintf("fallo_%02d", i)
  
  # Renombrar si la columna existe
  if(old_name %in% names(datos)) {
    names(datos)[names(datos) == old_name] <- new_name
  }
}



write.csv(datos, "datos_clean.csv")



# ===========================
# Part 3
# Calculate proportions

# Proportions o work & omitions
datos_clean = read_csv("datos_clean.csv")

# Delete first columns
datos_clean = datos_clean[, -1]

# Identify columns of interest
self_cols  = grep("^condicion_SELF_",  names(datos_clean), value = TRUE)
other_cols = grep("^condicion_OTHER_", names(datos_clean), value = TRUE)

# Calculate proportions 
datos_clean = datos_clean %>% 
  rowwise() %>% 
  mutate(
    ## Work (solo valor 1)
    trabajo_self  = sum(c_across(all_of(self_cols))  == 1, na.rm = TRUE) /
      length(self_cols)  * 100,
    trabajo_other = sum(c_across(all_of(other_cols)) == 1, na.rm = TRUE) /
      length(other_cols) * 100,
    
    ## Omition (valor 0) 
    zeros_SELF    = sum(c_across(all_of(self_cols))  == 0, na.rm = TRUE) /
      length(self_cols)  * 100,
    zeros_OTHER   = sum(c_across(all_of(other_cols)) == 0, na.rm = TRUE) /
      length(other_cols) * 100,
    
    ## Total Omitions (SELF & OTHER)
    zeros_TOTAL = (zeros_SELF + zeros_OTHER) / 2
  ) %>% 
  ungroup()

# Round numbers to the 2nd decimal
datos_clean <- datos_clean %>%
  mutate(
    trabajo_self = round(trabajo_self, 2),
    trabajo_other = round(trabajo_other, 2),
    zeros_SELF = round(zeros_SELF, 2),
    zeros_OTHER = round(zeros_OTHER, 2),
    zeros_TOTAL = round(zeros_TOTAL, 2)
  )

# Calculate adjusted proportions (excluding zeros from denominator)
datos_clean = datos_clean %>% 
  rowwise() %>% 
  mutate(
    # Count zeros to adjust denominator
    zeros_count_self = sum(c_across(all_of(self_cols)) == 0, na.rm = TRUE),
    zeros_count_other = sum(c_across(all_of(other_cols)) == 0, na.rm = TRUE),
    
    # Calculate adjusted proportions (1s / (total - zeros))
    trabajo_self_ajustado = sum(c_across(all_of(self_cols)) == 1, na.rm = TRUE) / 
      (length(self_cols) - zeros_count_self) * 100,
    
    trabajo_other_ajustado = sum(c_across(all_of(other_cols)) == 1, na.rm = TRUE) / 
      (length(other_cols) - zeros_count_other) * 100
  ) %>% 
  ungroup() %>%
  # Round to 2 decimals
  mutate(
    trabajo_self_ajustado = round(trabajo_self_ajustado, 2),
    trabajo_other_ajustado = round(trabajo_other_ajustado, 2)
  ) %>%
  # Remove auxiliary columns
  select(-zeros_count_self, -zeros_count_other)

# Extract columns of interest
self_cols <- select(datos_clean, starts_with("condicion_SELF"))
other_cols <- select(datos_clean, starts_with("condicion_OTHER"))

# Count omitions (0)
datos_clean <- datos_clean %>%
  mutate(
    count_omitidas_SELF = rowSums(self_cols == 0, na.rm = TRUE),
    count_omitidas_OTHER = rowSums(other_cols == 0, na.rm = TRUE),
    count_omitidas_TOTAL = count_omitidas_SELF + count_omitidas_OTHER
  )


# Guardar set de datos 
write.csv(datos_clean, "datos_final.csv")





# ===========================
## Steps for the long format

# Change of work (0, 1 & 2) to  (0 - rest, 1 - work & NA - Omitions)

# Change values of effort (50, 65, 80 & 95) to levels (1, 2, 3 & 4)

# Change values of reward (2, 6 & 10) to levels (1, 2 & 3)

# Change values of condition (Self & Other) to levels (0 & 1) respectively

# Change values of group (Control & Experimental) to levels (0 & 1) respectively

# Long format (repeat each value of the last columns 48 times for each participant)




datos = read_csv("datos_clean.csv")

# Convertir de formato wide a long
datos_long <- datos %>%
  # Seleccionar solo las columnas relevantes para la transformación
  select(ID_check, grupo, 
         matches("^condicion_(SELF|OTHER)_\\d+$"),
         matches("^reward_\\d+_\\d+$"),
         matches("^esfuerzo_\\d+_\\d+$"),
         matches("^fallo_\\d+$")) %>%
  
  # Crear una fila por participante para procesar
  rowwise() %>%
  
  # Para cada participante, crear los 48 trials
  summarise(
    ID_check = ID_check,
    grupo = grupo,
    trials = list(1:48),
    .groups = 'drop'
  ) %>%
  unnest(trials) %>%
  
  # Extraer valores para cada trial
  mutate(
    trial_str = sprintf("%02d", trials),
    
    # Obtener condición (SELF o OTHER)
    condicion = map2_dbl(ID_check, trial_str, function(id, t) {
      col_name <- if(as.numeric(t) <= 24) {
        paste0("condicion_SELF_", t)
      } else {
        paste0("condicion_OTHER_", t)
      }
      datos[datos$ID_check == id, col_name][[1]]
    }),
    
    # Obtener reward
    reward_val = map2_dbl(ID_check, trial_str, function(id, t) {
      # Buscar columna que contenga reward y termine con el número de trial
      col_pattern <- paste0("reward_\\d+_", t, "$")
      col_name <- grep(col_pattern, names(datos), value = TRUE)[1]
      if(!is.na(col_name)) {
        val <- datos[datos$ID_check == id, col_name][[1]]
        # Extraer el valor de reward del nombre de la columna
        as.numeric(str_extract(col_name, "\\d+(?=_\\d+$)"))
      } else {
        NA_real_
      }
    }),
    
    # Obtener esfuerzo
    esfuerzo_val = map2_dbl(ID_check, trial_str, function(id, t) {
      col_pattern <- paste0("esfuerzo_\\d+_", t, "$")
      col_name <- grep(col_pattern, names(datos), value = TRUE)[1]
      if(!is.na(col_name)) {
        val <- datos[datos$ID_check == id, col_name][[1]]
        # Extraer el valor de esfuerzo del nombre de la columna
        as.numeric(str_extract(col_name, "\\d+(?=_\\d+$)"))
      } else {
        NA_real_
      }
    }),
    
    # Obtener fallo
    fallo_val = map2_dbl(ID_check, trial_str, function(id, t) {
      col_name <- paste0("fallo_", t)
      datos[datos$ID_check == id, col_name][[1]]
    })
  ) %>%
  
  # Aplicar transformaciones
  mutate(
    sub = ID_check,
    
    # Transformar decision
    decision = case_when(
      condicion == 1 ~ 1,  # Trabajar
      condicion == 2 ~ 0,  # Descansar
      condicion == 0 ~ 2,  # Omisión
      TRUE ~ NA_integer_
    ),
    
    # Transformar reward
    reward = case_when(
      reward_val == 2 ~ 1,
      reward_val == 6 ~ 2,
      reward_val == 10 ~ 3,
      TRUE ~ NA_integer_
    ),
    
    # Transformar effort
    effort = case_when(
      esfuerzo_val == 50 ~ 1,
      esfuerzo_val == 65 ~ 2,
      esfuerzo_val == 80 ~ 3,
      esfuerzo_val == 95 ~ 4,
      TRUE ~ NA_integer_
    ),
    
    # Agent
    agent = ifelse(trials <= 24, 0, 1),
    
    # Success
    success = case_when(
      is.na(fallo_val) ~ 0,
      fallo_val == 1 ~ 1,
      TRUE ~ 0
    ),
    
    # Grupo
    grupo_num = case_when(
      grupo == "Control" ~ 0,
      grupo == "Vulnerable" ~ 1,
      TRUE ~ NA_integer_
    )
  ) %>%
  
  # Seleccionar columnas finales
  select(sub, decision, reward, effort, agent, success, grupo = grupo_num) %>%
  
  # Ordenar
  arrange(sub)




write.csv(datos_long, "datos_long.csv")



# ===========================
# Datos ANOVA Reward (Optional)

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

# Save Dataset
write.csv(datos_anova_reward, "datos_anova_reward.csv")



# ===========================
# Datos ANOVA Effort (Optional)


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

# Save Dataset
write.csv(datos_anova_effort, "datos_anova_effort.csv")



