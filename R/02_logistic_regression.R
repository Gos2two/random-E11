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
library(corrplot)

# Correlation matrix
cor_matrix <- cor(d_clean)

# Plot the correlation matrix as a heatmap
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.cex = 0.5)
# Train/test split
set.seed(21)
train_index <- createDataPartition(d_clean$T2D, p = 0.8, list = FALSE)
train_data <- d_clean[train_index, ]
test_data <- d_clean[-train_index, ]

# Logistic regression (1)
# logistic_model <- glm(T2D ~ FTO_rs8050136AA + FTO_rs8050136CA +
#                             FTO_rs7202116GG + FTO_rs7202116AG +
#                             FTO_rs9930506AA + FTO_rs9930506AG +
#                             SLC30A8_rs13266634CC + SLC30A8_rs13266634CT +
#                             KCNJ11_rs5219GluGlu + KCNJ11_rs5219GluLys +
#                             CDKN2B_rs10811661CC + CDKN2B_rs10811661CT +
#                             CDKAL1_rs7756992AA + CDKAL1_rs7756992AG +
#                             CDKAL1_rs9465871CC + CDKAL1_rs9465871CT +
#                             CDKAL1_rs7754840CC + CDKAL1_rs7754840CG +
#                             CDKAL1_rs10946398AA + CDKAL1_rs10946398AC +
#                             BMI + Age
#                             , data = train_data, family = binomial)

# Logistic regression (2)
# logistic_model <- glm(T2D ~
#                             KCNJ11_rs5219GluGlu + KCNJ11_rs5219GluLys +
#                             CDKN2B_rs10811661CC + CDKN2B_rs10811661CT +
#                             CDKAL1_rs7756992AA + CDKAL1_rs7756992AG +
#                             CDKAL1_rs9465871CC + CDKAL1_rs9465871CT +
#                             CDKAL1_rs10946398AA + CDKAL1_rs10946398AC +
#                             BMI + Age
#                             , data = train_data, family = binomial)
# Logistic regression (3)
# logistic_model <- glm(T2D ~
#                         KCNJ11_rs5219GluGlu * KCNJ11_rs5219GluLys *
#                         CDKN2B_rs10811661CC * CDKN2B_rs10811661CT *
#                         BMI * Age
#                       , data = train_data, family = binomial)
# summary(logistic_model)

# Stepwise: Logistic regression
mInit <- glm(T2D ~ 1, data = train_data, family = binomial)
Scope <- formula(T2D ~ KCNJ11_rs5219GluGlu * KCNJ11_rs5219GluLys *
                       CDKN2B_rs10811661CC * CDKN2B_rs10811661CT *
                       BMI * Age, data = train_data, family = binomial)
logistic_model <- step(mInit, scope = Scope, direction = "both")
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




