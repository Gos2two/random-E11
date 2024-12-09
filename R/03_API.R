# 03_API.R
library(plumber)
library(caret)
library(pROC)
library(PRROC)
library(ggplot2)

## !!! CHANGE PATH ACCORDINGLY:
# Load the model
logistic_model_paper <- readRDS("/Users/sergigosalvez/desktop/random-E11/models/logistic_model_paper.rds")
logistic_model_stepwise <- readRDS("/Users/sergigosalvez/desktop/random-E11/models/logistic_model_stepwise.rds")
# Load ROC 
roc_paper <- readRDS("/Users/sergigosalvez/desktop/random-E11/models/roc_paper.rds")
roc_stepwise <- readRDS("/Users/sergigosalvez/desktop/random-E11/models/roc_stepwise.rds")
# Load confusion matrix
conf_matrix_paper <- readRDS("/Users/sergigosalvez/desktop/random-E11/models/conf_matrix_paper.rds")
conf_matrix_stepwise <- readRDS("/Users/sergigosalvez/desktop/random-E11/models/conf_matrix_stepwise.rds")
# Load scaling factors for BMI and Age
scale_values <- readRDS("/Users/sergigosalvez/desktop/random-E11/data/processed/scale_values.rds")

#* @apiTitle T2D Prediction API
#* @apiDescription This API allows users to interact with two logistic regression models predicting T2D with single nucleotide polymorphisms (SNPs), BMI and Age. Data source: Nikitin, A. G., et al. (2017). Association of polymorphic markers of genes FTO, KCNJ11, CDKAL1, SLC30A8, and CDKN2B with type 2 diabetes mellitus in the Russian population. PeerJ, 5, e3414. https://doi.org/10.7717/peerj.3414


#* Logistic regression model 1: paper       
#* 
#* @param Age Numeric: Age 
#* @param BMI Numeric: BMI 
#* @param KCNJ11_rs5219 Character: Mutation status (GluGlu, GluLys, LysLys)
#* @param CDKN2B_rs10811661 Character: Mutation status (CC, CT, TT)
#* @param CDKAL1_rs7756992 Character: Mutation status (AA, AG, GG)
#* @param CDKAL1_rs9465871 Character: Mutation status (CC, CT, TT)
#* @param CDKAL1_rs10946398 Character: Mutation status (AA, AC, CC)
#* @param SLC30A8_rs13266634 Character: Mutation status (CC, CT, TT)
#* @post /predict_T2D_1
function(Age, BMI, KCNJ11_rs5219, CDKN2B_rs10811661, CDKAL1_rs7756992, CDKAL1_rs9465871, CDKAL1_rs10946398, SLC30A8_rs13266634) {

    Age <- as.double(Age)
    BMI <- as.double(BMI)
    # Convert SNP inputs to binary variables (0 or 1)
    KCNJ11_rs5219GluGlu <- ifelse(KCNJ11_rs5219 == "GluGlu", 1, 0)
    KCNJ11_rs5219GluLys <- ifelse(KCNJ11_rs5219 == "GluLys", 1, 0)
    
    CDKN2B_rs10811661CC <- ifelse(CDKN2B_rs10811661 == "CC", 1, 0)
    CDKN2B_rs10811661CT <- ifelse(CDKN2B_rs10811661 == "CT", 1, 0)
    
    CDKAL1_rs7756992AA  <- ifelse(CDKAL1_rs7756992 == "AA", 1, 0) 
    CDKAL1_rs7756992AG  <- ifelse(CDKAL1_rs7756992 == "AG", 1, 0)
    
    CDKAL1_rs9465871CC  <- ifelse(CDKAL1_rs9465871 == "CC", 1, 0)
    CDKAL1_rs9465871CT  <- ifelse(CDKAL1_rs9465871 == "CT", 1, 0)
    
    CDKAL1_rs10946398AA  <- ifelse(CDKAL1_rs10946398 == "AA", 1, 0) 
    CDKAL1_rs10946398AC  <- ifelse(CDKAL1_rs10946398 == "AC", 1, 0)
    
    SLC30A8_rs13266634CC  <- ifelse(SLC30A8_rs13266634 == "CC", 1, 0)
    SLC30A8_rs13266634CT  <- ifelse(SLC30A8_rs13266634 == "CT", 1, 0)

    # Prepare the input data
    input_data <- data.frame(
      Age = Age,
      BMI = BMI,
      KCNJ11_rs5219GluGlu = KCNJ11_rs5219GluGlu,
      KCNJ11_rs5219GluLys = KCNJ11_rs5219GluLys,
      CDKN2B_rs10811661CC = CDKN2B_rs10811661CC,
      CDKN2B_rs10811661CT = CDKN2B_rs10811661CT,
      CDKAL1_rs7756992AA = CDKAL1_rs7756992AA,
      CDKAL1_rs7756992AG = CDKAL1_rs7756992AG,
      CDKAL1_rs9465871CC = CDKAL1_rs9465871CC,
      CDKAL1_rs9465871CT = CDKAL1_rs9465871CT,
      CDKAL1_rs10946398AA = CDKAL1_rs10946398AA,
      CDKAL1_rs10946398AC = CDKAL1_rs10946398AC,
      SLC30A8_rs13266634CC = SLC30A8_rs13266634CC,
      SLC30A8_rs13266634CT = SLC30A8_rs13266634CT
    )
    
    # Normalize Age and BMI using the same scaling applied to training data
    input_data_scaled <- predict(scale_values, input_data[, c("Age", "BMI")])
    input_data[, c("Age", "BMI")] <- input_data_scaled
    print("Scaled Age and BMI:")
    print(input_data_scaled)

    # Predict probability
    prediction <- predict(logistic_model_paper, newdata = input_data, type = "response")
    prediction <- ifelse(prediction > 0.5, 1, 0)
    
    # Model metrics
    accuracy <- conf_matrix_paper$overall["Accuracy"]
    kappa <- conf_matrix_paper$overall["Kappa"]
    sensitivity <- conf_matrix_paper$byClass["Sensitivity"]
    specificity <- conf_matrix_paper$byClass["Specificity"]
    
    list(
      prediction = prediction,
      accuracy = accuracy,
      kappa = kappa,
      sensitivity = sensitivity,
      specificity = specificity
    )
}


#* Logistic regression model 2: stepwise       
#* 
#* @param Age Numeric: Age 
#* @param BMI Numeric: BMI
#* @param KCNJ11_rs5219 Character: Mutation status (GluGlu, GluLys, LysLys)
#* @param CDKN2B_rs10811661 Character: Mutation status (CC, CT, TT)
#* @param CDKAL1_rs7756992 Character: Mutation status (AA, AG, GG)
#* @post /predict_T2D_2
function(Age, BMI, KCNJ11_rs5219, CDKN2B_rs10811661, CDKAL1_rs7756992) {
  
  Age <- as.double(Age)
  BMI <- as.double(BMI)
  # Convert SNP inputs to binary variables (0 or 1)
  KCNJ11_rs5219GluGlu <- ifelse(KCNJ11_rs5219 == "GluGlu", 1, 0)
  KCNJ11_rs5219GluLys <- ifelse(KCNJ11_rs5219 == "GluLys", 1, 0)
  
  CDKN2B_rs10811661CC <- ifelse(CDKN2B_rs10811661 == "CC", 1, 0)
  CDKN2B_rs10811661CT <- ifelse(CDKN2B_rs10811661 == "CT", 1, 0)
  
  CDKAL1_rs7756992AA  <- ifelse(CDKAL1_rs7756992 == "AA", 1, 0) 
  CDKAL1_rs7756992AG  <- ifelse(CDKAL1_rs7756992 == "AG", 1, 0)
  
  # Prepare the input data
  input_data <- data.frame(
    Age = Age,
    BMI = BMI,
    KCNJ11_rs5219GluGlu = KCNJ11_rs5219GluGlu,
    KCNJ11_rs5219GluLys = KCNJ11_rs5219GluLys,
    CDKN2B_rs10811661CC = CDKN2B_rs10811661CC,
    CDKN2B_rs10811661CT = CDKN2B_rs10811661CT,
    CDKAL1_rs7756992AA = CDKAL1_rs7756992AA,
    CDKAL1_rs7756992AG = CDKAL1_rs7756992AG
  )
  
  # Normalize Age and BMI using the same scaling applied to training data
  input_data_scaled <- predict(scale_values, input_data[, c("Age", "BMI")])
  input_data[, c("Age", "BMI")] <- input_data_scaled
  print("Scaled Age and BMI:")
  print(input_data_scaled)
  
  # Predict probability
  prediction <- predict(logistic_model_stepwise, newdata = input_data, type = "response")
  prediction <- ifelse(prediction > 0.5, 1, 0)
  
  # Model metrics
  accuracy <- conf_matrix_stepwise$overall["Accuracy"]
  kappa <- conf_matrix_stepwise$overall["Kappa"]
  sensitivity <- conf_matrix_stepwise$byClass["Sensitivity"]
  specificity <- conf_matrix_stepwise$byClass["Specificity"]
  
  list(
    prediction = prediction,
    accuracy = accuracy,
    kappa = kappa,
    sensitivity = sensitivity,
    specificity = specificity
  )
}

#* ROC Curve and AUC 
#* @serializer png
#* @get /models_roc_plot
function() {
  auc_paper <- auc(roc_paper)
  auc_stepwise <- auc(roc_stepwise)
  
  print(ggplot() +
          geom_line(aes(x = 1 - roc_paper$specificities, y = roc_paper$sensitivities), color = "green", size = 1.2) +
          geom_line(aes(x = 1 - roc_stepwise$specificities, y = roc_stepwise$sensitivities), color = "purple", size = 1.2) +
          labs(title = "ROC Curves for Logistic Regression Models",
               x = "Specificity (False Positive Rate)",
               y = "Sensitivity (True Positive Rate)") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme(legend.title = element_blank()) +
          coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
          annotate("text", x = 0.9, y = 0.2, label = paste("AUC (Paper):", round(auc_paper, 2)), color = "green", size = 4) +
          annotate("text", x = 0.9, y = 0.15, label = paste("AUC (Stepwise):", round(auc_stepwise, 2)), color = "purple", size = 4)
  )
}

