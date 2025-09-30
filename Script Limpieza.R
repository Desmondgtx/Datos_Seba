
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
} # 246 columns


# Create new columns with Comp Q1 & Q2 results
datos = datos %>%
  mutate(resultados_Comp = case_when(
    Comp2_Q1 == 2 & Comp2_Q2 == 1 ~ 0,  # If Q1=2 y Q2=1, then 0
    is.na(Comp2_Q1) | is.na(Comp2_Q2) ~ 0,  # If any NA, then 0
    TRUE ~ 1  # Anything else is 1
  )) %>%
  # Put this new columns just after Comp_Q2
  relocate(resultados_Comp, .after = Comp2_Q2)

# Rename every fail_feedback_timing_Page.Submit column
for(i in 1:49) {
  # Original Name
  old_name <- paste0("X", i, "_fail_feedback_timing_Page.Submit")
  # New name
  new_name <- sprintf("fallo_%02d", i)
  
  # Rename
  if(old_name %in% names(datos)) {
    names(datos)[names(datos) == old_name] <- new_name
  }
}


# Reorder columns on specific order
datos = datos %>%
  select(
    # First columns
    ID_check, grupo, Comp2_Q1, Comp2_Q2, resultados_Comp, X49_attention_check,
    
    # Alternate between stimulus columns and fail_feedback columns
    # Para cada trial, seleccionar específicamente las 4 columnas correspondientes
    
    # Trial 01
    matches("^condicion_.*_01$"), matches("^reward_.*_01$"), 
    matches("^dificultad_.*_01$"), matches("^esfuerzo_.*_01$"), 
    fallo_01,
    
    # Trial 02
    matches("^condicion_.*_02$"), matches("^reward_.*_02$"), 
    matches("^dificultad_.*_02$"), matches("^esfuerzo_.*_02$"), 
    fallo_02,
    
    # Trial 03
    matches("^condicion_.*_03$"), matches("^reward_.*_03$"), 
    matches("^dificultad_.*_03$"), matches("^esfuerzo_.*_03$"), 
    fallo_03,
    
    # Trial 04
    matches("^condicion_.*_04$"), matches("^reward_.*_04$"), 
    matches("^dificultad_.*_04$"), matches("^esfuerzo_.*_04$"), 
    fallo_04,
    
    # Trial 05
    matches("^condicion_.*_05$"), matches("^reward_.*_05$"), 
    matches("^dificultad_.*_05$"), matches("^esfuerzo_.*_05$"), 
    fallo_05,
    
    # Trial 06
    matches("^condicion_.*_06$"), matches("^reward_.*_06$"), 
    matches("^dificultad_.*_06$"), matches("^esfuerzo_.*_06$"), 
    fallo_06,
    
    # Trial 07
    matches("^condicion_.*_07$"), matches("^reward_.*_07$"), 
    matches("^dificultad_.*_07$"), matches("^esfuerzo_.*_07$"), 
    fallo_07,
    
    # Trial 08
    matches("^condicion_.*_08$"), matches("^reward_.*_08$"), 
    matches("^dificultad_.*_08$"), matches("^esfuerzo_.*_08$"), 
    fallo_08,
    
    # Trial 09
    matches("^condicion_.*_09$"), matches("^reward_.*_09$"), 
    matches("^dificultad_.*_09$"), matches("^esfuerzo_.*_09$"), 
    fallo_09,
    
    # Trial 10
    matches("^condicion_.*_10$"), matches("^reward_.*_10$"), 
    matches("^dificultad_.*_10$"), matches("^esfuerzo_.*_10$"), 
    fallo_10,
    
    # Trial 11
    matches("^condicion_.*_11$"), matches("^reward_.*_11$"), 
    matches("^dificultad_.*_11$"), matches("^esfuerzo_.*_11$"), 
    fallo_11,
    
    # Trial 12
    matches("^condicion_.*_12$"), matches("^reward_.*_12$"), 
    matches("^dificultad_.*_12$"), matches("^esfuerzo_.*_12$"), 
    fallo_12,
    
    # Trial 13
    matches("^condicion_.*_13$"), matches("^reward_.*_13$"), 
    matches("^dificultad_.*_13$"), matches("^esfuerzo_.*_13$"), 
    fallo_13,
    
    # Trial 14
    matches("^condicion_.*_14$"), matches("^reward_.*_14$"), 
    matches("^dificultad_.*_14$"), matches("^esfuerzo_.*_14$"), 
    fallo_14,
    
    # Trial 15
    matches("^condicion_.*_15$"), matches("^reward_.*_15$"), 
    matches("^dificultad_.*_15$"), matches("^esfuerzo_.*_15$"), 
    fallo_15,
    
    # Trial 16
    matches("^condicion_.*_16$"), matches("^reward_.*_16$"), 
    matches("^dificultad_.*_16$"), matches("^esfuerzo_.*_16$"), 
    fallo_16,
    
    # Trial 17
    matches("^condicion_.*_17$"), matches("^reward_.*_17$"), 
    matches("^dificultad_.*_17$"), matches("^esfuerzo_.*_17$"), 
    fallo_17,
    
    # Trial 18
    matches("^condicion_.*_18$"), matches("^reward_.*_18$"), 
    matches("^dificultad_.*_18$"), matches("^esfuerzo_.*_18$"), 
    fallo_18,
    
    # Trial 19
    matches("^condicion_.*_19$"), matches("^reward_.*_19$"), 
    matches("^dificultad_.*_19$"), matches("^esfuerzo_.*_19$"), 
    fallo_19,
    
    # Trial 20
    matches("^condicion_.*_20$"), matches("^reward_.*_20$"), 
    matches("^dificultad_.*_20$"), matches("^esfuerzo_.*_20$"), 
    fallo_20,
    
    # Trial 21
    matches("^condicion_.*_21$"), matches("^reward_.*_21$"), 
    matches("^dificultad_.*_21$"), matches("^esfuerzo_.*_21$"), 
    fallo_21,
    
    # Trial 22
    matches("^condicion_.*_22$"), matches("^reward_.*_22$"), 
    matches("^dificultad_.*_22$"), matches("^esfuerzo_.*_22$"), 
    fallo_22,
    
    # Trial 23
    matches("^condicion_.*_23$"), matches("^reward_.*_23$"), 
    matches("^dificultad_.*_23$"), matches("^esfuerzo_.*_23$"), 
    fallo_23,
    
    # Trial 24
    matches("^condicion_.*_24$"), matches("^reward_.*_24$"), 
    matches("^dificultad_.*_24$"), matches("^esfuerzo_.*_24$"), 
    fallo_24,
    
    # Trial 25
    matches("^condicion_.*_25$"), matches("^reward_.*_25$"), 
    matches("^dificultad_.*_25$"), matches("^esfuerzo_.*_25$"), 
    fallo_25,
    
    # Trial 26
    matches("^condicion_.*_26$"), matches("^reward_.*_26$"), 
    matches("^dificultad_.*_26$"), matches("^esfuerzo_.*_26$"), 
    fallo_26,
    
    # Trial 27
    matches("^condicion_.*_27$"), matches("^reward_.*_27$"), 
    matches("^dificultad_.*_27$"), matches("^esfuerzo_.*_27$"), 
    fallo_27,
    
    # Trial 28
    matches("^condicion_.*_28$"), matches("^reward_.*_28$"), 
    matches("^dificultad_.*_28$"), matches("^esfuerzo_.*_28$"), 
    fallo_28,
    
    # Trial 29
    matches("^condicion_.*_29$"), matches("^reward_.*_29$"), 
    matches("^dificultad_.*_29$"), matches("^esfuerzo_.*_29$"), 
    fallo_29,
    
    # Trial 30
    matches("^condicion_.*_30$"), matches("^reward_.*_30$"), 
    matches("^dificultad_.*_30$"), matches("^esfuerzo_.*_30$"), 
    fallo_30,
    
    # Trial 31
    matches("^condicion_.*_31$"), matches("^reward_.*_31$"), 
    matches("^dificultad_.*_31$"), matches("^esfuerzo_.*_31$"), 
    fallo_31,
    
    # Trial 32
    matches("^condicion_.*_32$"), matches("^reward_.*_32$"), 
    matches("^dificultad_.*_32$"), matches("^esfuerzo_.*_32$"), 
    fallo_32,
    
    # Trial 33
    matches("^condicion_.*_33$"), matches("^reward_.*_33$"), 
    matches("^dificultad_.*_33$"), matches("^esfuerzo_.*_33$"), 
    fallo_33,
    
    # Trial 34
    matches("^condicion_.*_34$"), matches("^reward_.*_34$"), 
    matches("^dificultad_.*_34$"), matches("^esfuerzo_.*_34$"), 
    fallo_34,
    
    # Trial 35
    matches("^condicion_.*_35$"), matches("^reward_.*_35$"), 
    matches("^dificultad_.*_35$"), matches("^esfuerzo_.*_35$"), 
    fallo_35,
    
    # Trial 36
    matches("^condicion_.*_36$"), matches("^reward_.*_36$"), 
    matches("^dificultad_.*_36$"), matches("^esfuerzo_.*_36$"), 
    fallo_36,
    
    # Trial 37
    matches("^condicion_.*_37$"), matches("^reward_.*_37$"), 
    matches("^dificultad_.*_37$"), matches("^esfuerzo_.*_37$"), 
    fallo_37,
    
    # Trial 38
    matches("^condicion_.*_38$"), matches("^reward_.*_38$"), 
    matches("^dificultad_.*_38$"), matches("^esfuerzo_.*_38$"), 
    fallo_38,
    
    # Trial 39
    matches("^condicion_.*_39$"), matches("^reward_.*_39$"), 
    matches("^dificultad_.*_39$"), matches("^esfuerzo_.*_39$"), 
    fallo_39,
    
    # Trial 40
    matches("^condicion_.*_40$"), matches("^reward_.*_40$"), 
    matches("^dificultad_.*_40$"), matches("^esfuerzo_.*_40$"), 
    fallo_40,
    
    # Trial 41
    matches("^condicion_.*_41$"), matches("^reward_.*_41$"), 
    matches("^dificultad_.*_41$"), matches("^esfuerzo_.*_41$"), 
    fallo_41,
    
    # Trial 42
    matches("^condicion_.*_42$"), matches("^reward_.*_42$"), 
    matches("^dificultad_.*_42$"), matches("^esfuerzo_.*_42$"), 
    fallo_42,
    
    # Trial 43
    matches("^condicion_.*_43$"), matches("^reward_.*_43$"), 
    matches("^dificultad_.*_43$"), matches("^esfuerzo_.*_43$"), 
    fallo_43,
    
    # Trial 44
    matches("^condicion_.*_44$"), matches("^reward_.*_44$"), 
    matches("^dificultad_.*_44$"), matches("^esfuerzo_.*_44$"), 
    fallo_44,
    
    # Trial 45
    matches("^condicion_.*_45$"), matches("^reward_.*_45$"), 
    matches("^dificultad_.*_45$"), matches("^esfuerzo_.*_45$"), 
    fallo_45,
    
    # Trial 46
    matches("^condicion_.*_46$"), matches("^reward_.*_46$"), 
    matches("^dificultad_.*_46$"), matches("^esfuerzo_.*_46$"), 
    fallo_46,
    
    # Trial 47
    matches("^condicion_.*_47$"), matches("^reward_.*_47$"), 
    matches("^dificultad_.*_47$"), matches("^esfuerzo_.*_47$"), 
    fallo_47,
    
    # Trial 48
    matches("^condicion_.*_48$"), matches("^reward_.*_48$"), 
    matches("^dificultad_.*_48$"), matches("^esfuerzo_.*_48$"), 
    fallo_48
  )


# Apply transformation to every "fallo" column 
for(i in 1:48) {
  # Formatting trial number
  trial_num = sprintf("%02d", i)
  
  # Identify columns names
  col_condicion = grep(paste0("condicion.*_", trial_num, "$"), names(datos), value = TRUE)
  col_fallo = paste0("fallo_", trial_num)
  
  # Verificar que ambas columnas existen
  if(length(col_condicion) > 0 && col_fallo %in% names(datos)) {
    
    # Apply transformation
    datos = datos %>%
      mutate(!!col_fallo := case_when(
        # Si condicion = 1 y existe cualquier valor (no NA) en fallo -> poner 1
        .data[[col_condicion]] == 1 & !is.na(.data[[col_fallo]]) ~ 1,
        
        # Si condicion es 0 o 2, y existe valor en fallo -> poner NA
        (.data[[col_condicion]] == 0 | .data[[col_condicion]] == 2) & !is.na(.data[[col_fallo]]) ~ NA_real_,
        
        # Si fallo ya es NA, mantenerlo como NA (independiente del valor de condicion)
        is.na(.data[[col_fallo]]) ~ NA_real_,
        
        # Cualquier otro caso (por seguridad)
        TRUE ~ .data[[col_fallo]]
      ))
  }
}


# Save Dataset
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
    
    ## Total Work (SELF & OTHER)
    trabajo_total = (trabajo_self + trabajo_other) / 2,
    
    ## Omition (valor 0) 
    zeros_SELF    = sum(c_across(all_of(self_cols))  == 0, na.rm = TRUE) /
      length(self_cols)  * 100,
    zeros_OTHER   = sum(c_across(all_of(other_cols)) == 0, na.rm = TRUE) /
      length(other_cols) * 100,
    
    ## Total Omitions (SELF & OTHER)
    zeros_TOTAL = (zeros_SELF + zeros_OTHER) / 2
  ) %>% 
  ungroup() %>%
  # Round to 2 decimals
  mutate(
    trabajo_self = round(trabajo_self, 2),
    trabajo_other = round(trabajo_other, 2),
    trabajo_total = round(trabajo_total, 2),
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
    
    # Count ones (numerators)
    ones_count_self = sum(c_across(all_of(self_cols)) == 1, na.rm = TRUE),
    ones_count_other = sum(c_across(all_of(other_cols)) == 1, na.rm = TRUE),
    
    # Calculate denominators (trials without omissions)
    denominator_self = length(self_cols) - zeros_count_self,
    denominator_other = length(other_cols) - zeros_count_other,
    
    # Calculate adjusted proportions for SELF and OTHER
    trabajo_self_ajustado = ifelse(
      denominator_self > 0,
      (ones_count_self / denominator_self) * 100,
      NA_real_
    ),
    
    trabajo_other_ajustado = ifelse(
      denominator_other > 0,
      (ones_count_other / denominator_other) * 100,
      NA_real_
    ),
    
    # Calculate TOTAL adjusted proportion (ponderado)
    trabajo_total_ajustado = ifelse(
      (denominator_self + denominator_other) > 0,
      ((ones_count_self + ones_count_other) / (denominator_self + denominator_other)) * 100,
      NA_real_
    )
    
  ) %>% 
  ungroup() %>%
  # Round to 2 decimals
  mutate(
    trabajo_self_ajustado = round(trabajo_self_ajustado, 2),
    trabajo_other_ajustado = round(trabajo_other_ajustado, 2),
    trabajo_total_ajustado = round(trabajo_total_ajustado, 2)
  ) %>%
  # Remove auxiliary columns
  select(-zeros_count_self, -zeros_count_other, 
         -ones_count_self, -ones_count_other,
         -denominator_self, -denominator_other)



# Fail Proportion
# Identify "fallo" columns
fallo_cols = grep("^fallo_", names(datos_clean), value = TRUE)[1:48]

# Calculate fail rate
datos_clean = datos_clean %>% 
  rowwise() %>% 
  mutate(
    # Count work (value = 1) on condition_self
    trabajo_self_count = sum(c_across(all_of(self_cols)) == 1, na.rm = TRUE),
    
    # Count fails on SELF conditions
    fallos_self_count = sum(
      mapply(function(cond_col, fallo_col) {
        cond_val = get(cond_col)
        fallo_val = get(fallo_col)
        # Count fails ONLY when works is presented
        return(cond_val == 1 & !is.na(fallo_val) & fallo_val == 1)
      }, 
      self_cols, 
      fallo_cols[1:24])
    ),
    
    # Calculate fail proportion
    tasa_fallo_self = ifelse(
      trabajo_self_count > 0,
      (fallos_self_count / trabajo_self_count) * 100,
      NA_real_
    ),
    
    # Count work on condition OTHER
    trabajo_other_count = sum(c_across(all_of(other_cols)) == 1, na.rm = TRUE),
    
    # Count fails
    fallos_other_count = sum(
      mapply(function(cond_col, fallo_col) {
        cond_val = get(cond_col)
        fallo_val = get(fallo_col)
        # Count fails ONLY when works is presented
        return(cond_val == 1 & !is.na(fallo_val) & fallo_val == 1)
      }, 
      other_cols, 
      fallo_cols[25:48])
    ),
    
    # Calculate fail proportion
    tasa_fallo_other = ifelse(
      trabajo_other_count > 0,
      (fallos_other_count / trabajo_other_count) * 100,
      NA_real_
    ),
    
    # Total times of working
    trabajo_total_count = trabajo_self_count + trabajo_other_count,
    
    # Total fails
    fallos_total_count = fallos_self_count + fallos_other_count,
    
    # Calculate fail proportion
    tasa_fallo_total = ifelse(
      trabajo_total_count > 0,
      (fallos_total_count / trabajo_total_count) * 100,
      NA_real_
    )
    
  ) %>% 
  ungroup() %>%
  # Round to 2 decimals
  mutate(
    tasa_fallo_self = round(tasa_fallo_self, 2),
    tasa_fallo_other = round(tasa_fallo_other, 2),
    tasa_fallo_total = round(tasa_fallo_total, 2)
  ) %>%
  # Remove used columns
  select(-trabajo_self_count, -trabajo_other_count, -trabajo_total_count,
         -fallos_self_count, -fallos_other_count, -fallos_total_count)


# Save dataset
write.csv(datos_clean, "datos_final.csv")


# ===========================
# Part 4
# Long Format

# Load Data Set
datos = read_csv("datos_clean.csv")

# Transform from wide to long format
datos_long <- datos %>%
  # Seleccionar solo las columnas relevantes para la transformación
  select(ID_check, grupo, 
         matches("^condicion_(SELF|OTHER)_\\d+$"),
         matches("^reward_\\d+_\\d+$"),
         matches("^esfuerzo_\\d+_\\d+$"),
         matches("^fallo_\\d+$")) %>%
  
  # Create a row by participant
  rowwise() %>%
  
  # For every participant create 48 trials
  summarise(
    ID_check = ID_check,
    grupo = grupo,
    trials = list(1:48),
    .groups = 'drop'
  ) %>%
  unnest(trials) %>%
  
  # Extract values for every trial
  mutate(
    trial_str = sprintf("%02d", trials),
    
    # SELF o OTHER condition
    condicion = map2_dbl(ID_check, trial_str, function(id, t) {
      # Determine condition (self or other( based on trial number)
      if(as.numeric(t) <= 24) {
        col_name <- paste0("condicion_SELF_", t)
      } else {
        # Trials 25-48 = Other condition
        col_name <- paste0("condicion_OTHER_", t)
      }
      
      # Obtain value of the columns
      if(col_name %in% names(datos)) {
        datos[datos$ID_check == id, col_name][[1]]
      } else {
        NA_real_
      }
    }),
    
    # Obtain real value for reward
    decision_value = condicion, 
    
    # Obtain reward value
    reward_val = map2_dbl(ID_check, trial_str, function(id, t) {
      col_pattern <- paste0("_", t, "$")
      reward_cols <- grep(paste0("^reward_.*", col_pattern), names(datos), value = TRUE)
      
      if(length(reward_cols) > 0) {
        col_name <- reward_cols[1]
        # Extract number of reward by column name
        reward_num <- as.numeric(str_extract(col_name, "(?<=reward_)\\d+(?=_)"))
        return(reward_num)
      } else {
        NA_real_
      }
    }),
    
    # Obtain effort by column name
    esfuerzo_val = map2_dbl(ID_check, trial_str, function(id, t) {
      # Seach columns that ends with trial number
      col_pattern <- paste0("_", t, "$")
      esfuerzo_cols <- grep(paste0("^esfuerzo_.*", col_pattern), names(datos), value = TRUE)
      
      if(length(esfuerzo_cols) > 0) {
        col_name <- esfuerzo_cols[1]
        # Extract effort by column name
        esfuerzo_num <- as.numeric(str_extract(col_name, "(?<=esfuerzo_)\\d+(?=_)"))
        return(esfuerzo_num)
      } else {
        NA_real_
      }
    }),
    
    # Obtein Fail
    fallo_val = map2_dbl(ID_check, trial_str, function(id, t) {
      col_name <- paste0("fallo_", t)
      if(col_name %in% names(datos)) {
        datos[datos$ID_check == id, col_name][[1]]
      } else {
        NA_real_
      }
    })
  ) %>%
  
  # Apply changes
  mutate(
    sub = ID_check,
    
    # ADD TRIAL Column
    trial = trials,
    
    # Transform desition
    decision = case_when(
      decision_value == 1 ~ 1,  # Trabajar
      decision_value == 2 ~ 0,  # Descansar  
      decision_value == 0 ~ 2,  # Omisión
      TRUE ~ NA_integer_
    ),
    
    # Transform reward
    reward = case_when(
      reward_val == 2 ~ 1,
      reward_val == 6 ~ 2,
      reward_val == 10 ~ 3,
      TRUE ~ NA_integer_
    ),
    
    # Transform effort
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
    
    # Group
    grupo_num = case_when(
      grupo == "Control" ~ 0,
      grupo == "Vulnerable" ~ 1,
      TRUE ~ NA_integer_
    )
  ) %>%
  
  # Select columns finals
  select(sub, trial, decision, reward, effort, agent, success, grupo = grupo_num) %>%
  
  # Order
  arrange(sub, trial)

# Save Data long
write.csv(datos_long, "datos_long.csv", row.names = FALSE)



# ===========================
# Part 5
# ANOVA Data

# Read data
datos_long <- read.csv("datos_long.csv")
datos_clean <- read.csv("datos_final.csv")


# Verify participants answers and omitions
participantes_con_datos <- datos_long %>%
  filter(decision != 2) %>%
  pull(sub) %>%
  unique()

participantes_totales <- unique(datos_long$sub)
participantes_faltantes <- setdiff(participantes_totales, participantes_con_datos)

if(length(participantes_faltantes) > 0) {
  cat("Participant with ONLY omitions:", participantes_faltantes, "\n")
}
# Participant with ONLY omitions: 2939 


# Select proportions and columns of interests
columnas_adicionales <- datos_clean %>%
  select(ID_check,
         trabajo_self, trabajo_other, trabajo_total,
         zeros_SELF, zeros_OTHER, zeros_TOTAL,
         trabajo_self_ajustado, trabajo_other_ajustado, trabajo_total_ajustado,
         tasa_fallo_self, tasa_fallo_other, tasa_fallo_total)

# Calculate averaging
model_free_proportions <- datos_long %>%
  # Filter to exclude omitions (2)
  filter(decision != 2) %>%
  group_by(sub, grupo) %>%
  summarise(
    # Proportion for reward (SELF)
    SelfRew1 = mean(decision[agent == 0 & reward == 1] == 1, na.rm = TRUE),
    SelfRew2 = mean(decision[agent == 0 & reward == 2] == 1, na.rm = TRUE),
    SelfRew3 = mean(decision[agent == 0 & reward == 3] == 1, na.rm = TRUE),
    
    # Proportion for reward (OTHER)
    OtherRew1 = mean(decision[agent == 1 & reward == 1] == 1, na.rm = TRUE),
    OtherRew2 = mean(decision[agent == 1 & reward == 2] == 1, na.rm = TRUE),
    OtherRew3 = mean(decision[agent == 1 & reward == 3] == 1, na.rm = TRUE),
    
    # Proportion for effort (SELF)
    SelfEff1 = mean(decision[agent == 0 & effort == 1] == 1, na.rm = TRUE),
    SelfEff2 = mean(decision[agent == 0 & effort == 2] == 1, na.rm = TRUE),
    SelfEff3 = mean(decision[agent == 0 & effort == 3] == 1, na.rm = TRUE),
    SelfEff4 = mean(decision[agent == 0 & effort == 4] == 1, na.rm = TRUE),
    
    # Proportion for effort (OTHER)
    OtherEff1 = mean(decision[agent == 1 & effort == 1] == 1, na.rm = TRUE),
    OtherEff2 = mean(decision[agent == 1 & effort == 2] == 1, na.rm = TRUE),
    OtherEff3 = mean(decision[agent == 1 & effort == 3] == 1, na.rm = TRUE),
    OtherEff4 = mean(decision[agent == 1 & effort == 4] == 1, na.rm = TRUE),
    
    # Total proportion
    WorkSelf = mean(decision[agent == 0] == 1, na.rm = TRUE),
    WorkOther = mean(decision[agent == 1] == 1, na.rm = TRUE),
    
    .groups = 'drop'
  )

# Join datasets with ID matching
columnas_adicionales$ID_check <- sprintf("%04d", as.integer(columnas_adicionales$ID_check))
model_free_proportions$sub <- sprintf("%04d", as.integer(model_free_proportions$sub))

# Join between average and proportion columns
model_free_proportions_v2 <- model_free_proportions %>%
  left_join(columnas_adicionales, by = c("sub" = "ID_check"))


# Round to 4 decimals
model_free_proportions_v2 <- model_free_proportions_v2 %>%
  mutate(across(where(is.numeric) & !sub, ~round(., 4)))

# Save dataset
write.csv(model_free_proportions, "datos_analisis.csv", row.names = FALSE)
write.csv(model_free_proportions_v2, "datos_analisis_v2.csv", row.names = FALSE)



