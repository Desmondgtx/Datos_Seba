## Prosocial Effort Task ##
# FONDECYT Sebastián Contreras #
# Diego Garrido - José Borquez #
# Viña del Mar - 2025 #


#Call library#

library(tidyverse)
library(readr)
library(dplyr)


#Call data frame# 
datos_clean_v2 <- read_csv("datos_clean_v2.csv")
#eliminate the first column only if is necessary
datos_clean_v2 <- datos_clean_v2[,-1]


#Organize data in long format

lon_dato <- datos_clean_v2 %>%  #data effort
  pivot_longer(
    cols = starts_with("esfuerzo_"),
    names_to = "effort",
    names_pattern = "esfuerzo_(\\d+)_\\d+",
    values_to = "nivel"
  )


lon_reward <- datos_clean_v2 %>%  #data reward
  pivot_longer(
    cols = starts_with("reward_"),
    names_to = "reward",
    names_pattern = "reward_(\\d+)_\\d+",
    values_to = "nivel"
  )


lon_condition <- datos_clean_v2 %>%   #data condicion
  pivot_longer(
    cols = starts_with("condicion_"),
    names_to = "condicion",
    names_pattern = "condicion_(SELF|OTHER)_\\d+",
    values_to = "grupo"
  )

# delete columns by variable
#is very important to save the columns ID_check in all 
#data frame,but in at least one of this data frame, is necessary 
#save the column "GRUPO" and "nivel", for to after add the rest of columns in that data frame, 
#in this case we save the columns nivel and "GRUPO" in reward
lon_dato <- lon_dato[, -c(2:157)] #effort
lon_dato <- lon_dato[,-3] # eliminate column "nivel"

lon_reward <- lon_reward[, -c(3:157)] #reward
#read me
{#To avoid deleting the last 8 columns and the first 3 for analysis, use these parameters 4:147
}

lon_condition <- lon_condition[, -c(2:157)]  #condcion
lon_condition <- lon_condition[,-3] #eliminate "nivel"


# combine data sets into one
datos_long_v2 <- cbind(lon_reward, lon_dato[, "effort"], lon_condition[, "condicion"])


# add trail number
datos_long_v2 <- datos_long_v2 %>% 
  group_by(ID_check) %>%
  mutate(trail = row_number()) %>% 
  ungroup()

# change name of the column "nivel" to "choise"
colnames(datos_long_v2)[colnames(datos_long_v2) == "nivel"] <- "Choise"
# change name of the column "condicion" to "condition"
colnames(datos_long_v2)[colnames(datos_long_v2) == "condicion"] <- "condition"

# organize columns
datos_long_v2 <- datos_long_v2[c("ID_check", "trail", "Choise", "effort", "reward", "condition", "GRUPO")]
#read me
{#To organize the database with the other parameters (4:147), add the following command:
  #,"X49_attention_check", "trabajo_self", "trabajo_other", #  "zeros_SELF",
  #"zeros_OTHER", "zeros_TOTAL", "prop_omitidas_SELF", "prop_omitidas_OTHER", "prop_omitidas_TOTAL"
}


# Change values 

#In this data frame, the values of choise is all redy change, 
#but the value of omited is diferent, so we can change to 2 or not, in this case, the value will be changed
{datos_long_v2 <- datos_long_v2 %>%
  mutate(Choise = ifelse(is.na(Choise), 2, Choise))
#in this case, the new value of NA in column Choise is 2
  }

#Change value effort 
datos_long_v2 <- datos_long_v2 %>%
  mutate(effort = case_when(
    effort == 50 ~ 1,
    effort == 65 ~ 2,
    effort == 80 ~ 3,
    effort == 95 ~ 4,
    TRUE ~ effort))

#Change value condition
datos_long_v2 <- datos_long_v2 %>%
  mutate(condition = ifelse(condition == "SELF", 0, condition))

datos_long_v2 <- datos_long_v2 %>%
  mutate(condition = ifelse(condition == "OTHER", 1, condition))

#Change value reward
datos_long_v2 <- datos_long_v2 %>%
  mutate(reward = case_when(
    reward == 2 ~ 1,
    reward == 6 ~ 2,
    reward == 10 ~ 3,
    TRUE ~ reward))


#Change value GRUPO, value Control is O and Vulnerable is 1 
datos_long_v2 <- datos_long_v2 %>%
  mutate(GRUPO = ifelse(GRUPO == "Control", 0, GRUPO))

datos_long_v2 <- datos_long_v2 %>% 
  mutate(GRUPO = ifelse(GRUPO == "Vulnerable", 1, GRUPO))


#Save data frame
write.csv(datos_long_v2, "datos_long_v2.csv")
