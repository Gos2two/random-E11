## Clear environment
 
rm(list = ls())
setwd("~/Desktop/random-E11")


## Data import
# Source: 
# Nikitin, A. G., et al. (2017). 
# Association of polymorphic markers of genes FTO, KCNJ11, CDKAL1, SLC30A8, and 
# CDKN2B with type 2 diabetes mellitus in the Russian population. PeerJ, 5, e3414. 
# https://doi.org/10.7717/peerj.3414

library(readr)
library(dplyr)
library(caret)
library(pROC)


d_raw_case <- read_delim("Data/raw/data-case.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                         trim_ws = TRUE) # manually removed single error in HOMA-IR row 281
spec(d_raw_case)


d_raw_control <- read_delim("Data/raw/data-control.csv", 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                            trim_ws = TRUE)
spec(d_raw_control)

#Add case/control indicator
d_case <- d_raw_case %>% mutate(T2D = 1) # Add column with 1 for cases
d_control <- d_raw_control %>% mutate(T2D = 0) # Add column with 0 for controls
d_case <- d_case %>% select( colnames(d_control))

##Create one large data set
d <- rbind(d_case, d_control)

## Remove rows with NA
d_clean <- na.omit(d)

## Remove sample col
d_clean <- select(d_clean,-Sample)

# Data preparation
d_clean$Age <- scale(d_clean$Age)
d_clean$BMI <- scale(d_clean$BMI)
d_clean$`Basal glucose level` <- scale(d_clean$`Basal glucose level`)
d_clean$`Glucose level 2 h after PGTT` <- scale(d_clean$`Glucose level 2 h after PGTT`)
d_clean$`Basal insulin level` <- scale(d_clean$`Basal insulin level`)
d_clean$`Insulin level 2 h after PGTT` <- scale(d_clean$`Insulin level 2 h after PGTT`)
d_clean$`HOMA-b` <- scale(d_clean$`HOMA-b`)
d_clean$`HOMA-IR` <- scale(d_clean$`HOMA-IR`)

# Do one hot encoding for categoricals
dummy_model <- dummyVars(~ `FTO [rs8050136]` +  `FTO [rs7202116]` + `FTO [rs9930506]` +  `KCNJ11 [rs5219]` + `SLC30A8 [rs13266634]` +  `CDKN2B [rs10811661]` + `CDKAL1 [rs7756992]` +  `CDKAL1 [rs9465871]` + `CDKAL1 [rs7754840]` +  `CDKAL1 [rs10946398]` , data = d_clean)
one_hot <- predict(dummy_model, newdata = d_clean)

d_clean <- cbind(d_clean, one_hot)
d_clean <- d_clean[, !sapply(d_clean, is.character)]

# Rename problematic columns
# Clean column names in d_clean
names(d_clean) <- gsub("`", "",                      # Remove backticks
                  gsub("\\[|\\]", "",                # Remove square brackets
                  gsub("\\s", "_",                  # Replace spaces with underscores
                  gsub("/", "", names(d_clean))))) # Remove double quotes

# Export d_clean as a CSV
write.csv(d_clean, file = "data/processed/d_clean.csv", row.names = FALSE)

# --- Summary statistics

# Age (DM2+) 
mean(d_raw_case$Age, na.rm = TRUE)
sd(d_raw_case$Age, na.rm = TRUE)
hist(d_clean$Age)
qqnorm(d_clean$Age)
qqline(d_clean$Age, col = "red", lwd = 2)

# Age (DM2−) 
mean(d_raw_control$Age, na.rm = TRUE)
sd(d_raw_control$Age, na.rm = TRUE)

# BMI (DM2+) 
mean(d_raw_case$BMI, na.rm = TRUE)
sd(d_raw_case$BMI, na.rm = TRUE)
hist(d_clean$BMI)
qqnorm(d_clean$BMI)
qqline(d_clean$BMI, col = "red", lwd = 2)

# BMI (DM2−) 
mean(d_raw_control$BMI, na.rm = TRUE)
sd(d_raw_control$BMI, na.rm = TRUE)

# Basal glucose level (DM2+) 
mean(d_raw_case$`Basal glucose level`, na.rm = TRUE)
sd(d_raw_case$`Basal glucose level`, na.rm = TRUE)
hist(d_clean$`Basal glucose level`)
qqnorm(d_clean$`Basal glucose level`)
qqline(d_clean$`Basal glucose level`, col = "red", lwd = 2)

# Basal glucose level (DM2−) 
mean(d_raw_control$`Basal glucose level`, na.rm = TRUE)
sd(d_raw_control$`Basal glucose level`, na.rm = TRUE)

# Glucose level 2 h after PGTT (DM2+) 
mean(d_raw_case$`Glucose level 2 h after PGTT`, na.rm = TRUE)
sd(d_raw_case$`Glucose level 2 h after PGTT`, na.rm = TRUE)

# Glucose level 2 h after PGTT (DM2−) 
mean(d_raw_control$`Glucose level 2 h after PGTT`, na.rm = TRUE)
sd(d_raw_control$`Glucose level 2 h after PGTT`, na.rm = TRUE)

# Basal insulin level (DM2+)
mean(d_raw_case$`Basal insulin level`, na.rm = TRUE)
sd(d_raw_case$`Basal insulin level`, na.rm = TRUE)

# Basal insulin level (DM2−) 
mean(d_raw_control$`Basal insulin level`, na.rm = TRUE)
sd(d_raw_control$`Basal insulin level`, na.rm = TRUE)

# Insulin level 2 h after PGTT (DM2+)
mean(d_raw_case$`Insulin level 2 h after PGTT`, na.rm = TRUE)
sd(d_raw_case$`Insulin level 2 h after PGTT`, na.rm = TRUE)

# Insulin level 2 h after PGTT (DM2−)
mean(d_raw_control$`Insulin level 2 h after PGTT`, na.rm = TRUE)
sd(d_raw_control$`Insulin level 2 h after PGTT`, na.rm = TRUE)

# Glycated hemoglobin НbА1с (DM2+)
mean(d_raw_case$`Glycated hemoglobin НbА1с`, na.rm = TRUE)
sd(d_raw_case$`Glycated hemoglobin НbА1с`, na.rm = TRUE)

# HOMA-b (DM2+)
mean(d_raw_case$`HOMA-b`, na.rm = TRUE)
sd(d_raw_case$`HOMA-b`, na.rm = TRUE)

# HOMA-b (DM2−)
mean(d_raw_control$`HOMA-b`, na.rm = TRUE)
sd(d_raw_control$`HOMA-b`, na.rm = TRUE)

# HOMA-IR (DM2+)
mean(d_raw_case$`HOMA-IR`, na.rm = TRUE)
sd(d_raw_case$`HOMA-IR`, na.rm = TRUE)

# HOMA-IR (DM2−)
mean(d_raw_control$`HOMA-IR`, na.rm = TRUE)
sd(d_raw_control$`HOMA-IR`, na.rm = TRUE)


