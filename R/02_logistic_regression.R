## 2. Logistic regression

# Data source: 
# Nikitin, A. G., et al. (2017). 
# Association of polymorphic markers of genes FTO, KCNJ11, CDKAL1, SLC30A8, and 
# CDKN2B with type 2 diabetes mellitus in the Russian population. PeerJ, 5, e3414. 
# https://doi.org/10.7717/peerj.3414

library(readr)
library(dplyr)
library(caret)
library(pROC)
library(PRROC)

# Train/test split
set.seed(122)
train_index <- createDataPartition(d_clean$T2D, p = 0.8, list = FALSE)
train_data <- d_clean[train_index, ]
test_data <- d_clean[-train_index, ]

# Logistic regression FTO_rs8050136AA + FTO_rs8050136CA + FTO_rs8050136CC + FTO_rs7202116GG + FTO_rs7202116AG + FTO_rs7202116GG + FTO_rs9930506AA + FTO_rs9930506AG + FTO_rs9930506GG + SLC30A8_rs13266634CC + SLC30A8_rs13266634CT + SLC30A8_rs13266634TT
logistic_model <- glm(T2D ~ FTO_rs8050136AA + FTO_rs8050136CA, data = train_data, family = binomial)
summary(logistic_model)

# Predictions
predictions <- predict(logistic_model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Evaluation
confusionMatrix(as.factor(predicted_classes), as.factor(test_data$T2D))

# ROC Curve
roc_curve <- roc(test_data$T2D, predictions)
plot(roc_curve)

# AUC value

auc_value <- auc(roc_curve)
print(auc_value)

# Odds Ratios
odds_ratios <- exp(coef(logistic_model))
print(odds_ratios)

# Confidence intervals for Odds Ratios
conf_int <- exp(confint(logistic_model))
print(conf_int)

# Precision-Recall curve
pr_curve <- pr.curve(scores.class0 = predictions[test_data$T2D == 1],
                     scores.class1 = predictions[test_data$T2D == 0],
                     curve = TRUE)
plot(pr_curve)




