# --- Clear environment

rm(list = ls())


# --- Data import
# Source: 
# Nikitin, A. G., et al. (2017). 
# Association of polymorphic markers of genes FTO, KCNJ11, CDKAL1, SLC30A8, and 
# CDKN2B with type 2 diabetes mellitus in the Russian population. PeerJ, 5, e3414. 
# https://doi.org/10.7717/peerj.3414

library(readr)


data_case <- read_delim("Data/raw/data-case.csv", 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                     trim_ws = TRUE) # manually removed single error in HOMA-IR row 281
spec(data_case)


data_control <- read_delim("Data/raw/data-control.csv", 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                     trim_ws = TRUE)
spec(data_control)

# --- Remove rows with NA

data_case_clean <- na.omit(data_case)
data_control_claen <- na.omit(data_control)

# --- Summary statistics

# Age (DM2+) 
mean(data_case$Age, na.rm = TRUE)
sd(data_case$Age, na.rm = TRUE)

# Age (DM2−) 
mean(data_control$Age, na.rm = TRUE)
sd(data_control$Age, na.rm = TRUE)

# BMI (DM2+) 
mean(data_case$BMI, na.rm = TRUE)
sd(data_case$BMI, na.rm = TRUE)

# BMI (DM2−) 
mean(data_control$BMI, na.rm = TRUE)
sd(data_control$BMI, na.rm = TRUE)

# Basal glucose level (DM2+) 
mean(data_case$`Basal glucose level`, na.rm = TRUE)
sd(data_case$`Basal glucose level`, na.rm = TRUE)

# Basal glucose level (DM2−) 
mean(data_control$`Basal glucose level`, na.rm = TRUE)
sd(data_control$`Basal glucose level`, na.rm = TRUE)

# Glucose level 2 h after PGTT (DM2+) 
mean(data_case$`Glucose level 2 h after PGTT`, na.rm = TRUE)
sd(data_case$`Glucose level 2 h after PGTT`, na.rm = TRUE)

# Glucose level 2 h after PGTT (DM2−) 
mean(data_control$`Glucose level 2 h after PGTT`, na.rm = TRUE)
sd(data_control$`Glucose level 2 h after PGTT`, na.rm = TRUE)

# Basal insulin level (DM2+)
mean(data_case$`Basal insulin level`, na.rm = TRUE)
sd(data_case$`Basal insulin level`, na.rm = TRUE)

# Basal insulin level (DM2−) 
mean(data_control$`Basal insulin level`, na.rm = TRUE)
sd(data_control$`Basal insulin level`, na.rm = TRUE)

# Insulin level 2 h after PGTT (DM2+)
mean(data_case$`Insulin level 2 h after PGTT`, na.rm = TRUE)
sd(data_case$`Insulin level 2 h after PGTT`, na.rm = TRUE)

# Insulin level 2 h after PGTT (DM2−)
mean(data_control$`Insulin level 2 h after PGTT`, na.rm = TRUE)
sd(data_control$`Insulin level 2 h after PGTT`, na.rm = TRUE)

# Glycated hemoglobin НbА1с (DM2+)
mean(data_case$`Glycated hemoglobin НbА1с`, na.rm = TRUE)
sd(data_case$`Glycated hemoglobin НbА1с`, na.rm = TRUE)

# HOMA-b (DM2+)
mean(data_case$`HOMA-b`, na.rm = TRUE)
sd(data_case$`HOMA-b`, na.rm = TRUE)

# HOMA-b (DM2−)
mean(data_control$`HOMA-b`, na.rm = TRUE)
sd(data_control$`HOMA-b`, na.rm = TRUE)

# HOMA-IR (DM2+)
mean(data_case$`HOMA-IR`, na.rm = TRUE)
sd(data_case$`HOMA-IR`, na.rm = TRUE)

# HOMA-IR (DM2−)
mean(data_control$`HOMA-IR`, na.rm = TRUE)
sd(data_control$`HOMA-IR`, na.rm = TRUE)


