# 02_logistic_regression.R

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
library(ggplot2)

# Correlation matrix
cor_matrix <- cor(d_clean)
# Plot the correlation matrix as a heatmap
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.cex = 0.5)

# Train/test split
set.seed(253)
train_index <- createDataPartition(d_clean$T2D, p = 0.8, list = FALSE)
train_data <- d_clean[train_index, ]
test_data <- d_clean[-train_index, ]

# Logistic regression: general (1)
lr_general <- glm(T2D ~ FTO_rs8050136AA + FTO_rs8050136CA +
                            FTO_rs7202116GG + FTO_rs7202116AG +
                            FTO_rs9930506AA + FTO_rs9930506AG +
                            SLC30A8_rs13266634CC + SLC30A8_rs13266634CT +
                            KCNJ11_rs5219GluGlu + KCNJ11_rs5219GluLys +
                            CDKN2B_rs10811661CC + CDKN2B_rs10811661CT +
                            CDKAL1_rs7756992AA + CDKAL1_rs7756992AG +
                            CDKAL1_rs9465871CC + CDKAL1_rs9465871CT +
                            CDKAL1_rs7754840CC + CDKAL1_rs7754840CG +
                            CDKAL1_rs10946398AA + CDKAL1_rs10946398AC +
                            BMI + Age
                            , data = train_data, family = binomial)
summary(lr_general)

# # Logistic regression: highest significance (2)
lr_basic <- glm(T2D ~
                            KCNJ11_rs5219GluGlu + KCNJ11_rs5219GluLys +
                            CDKN2B_rs10811661CC + CDKN2B_rs10811661CT +
                            CDKAL1_rs7756992AA + CDKAL1_rs7756992AG +
                            BMI + Age
                            , data = train_data, family = binomial)
summary(lr_basic)

# #Logistic regression: highest significance and dependent (3)
# logistic_model <- glm(T2D ~
#                         KCNJ11_rs5219GluGlu * KCNJ11_rs5219GluLys *
#                         CDKN2B_rs10811661CC * CDKN2B_rs10811661CT *
#                         BMI * Age
#                       , data = train_data, family = binomial)
# summary(logistic_model)

# Logistic regression: based on paper conclusions (4)
lr_paper <- glm(T2D ~
                            SLC30A8_rs13266634CC + SLC30A8_rs13266634CT +
                            KCNJ11_rs5219GluGlu + KCNJ11_rs5219GluLys +
                            CDKN2B_rs10811661CC + CDKN2B_rs10811661CT +
                            CDKAL1_rs7756992AA + CDKAL1_rs7756992AG +
                            CDKAL1_rs9465871CC + CDKAL1_rs9465871CT +
                            CDKAL1_rs10946398AA + CDKAL1_rs10946398AC +
                            BMI + Age
                            , data = train_data, family = binomial)
summary(lr_paper)

# Stepwise: highest significance and dependant (1)
mInit <- glm(T2D ~ 1, data = train_data, family = binomial)
Scope <- formula(T2D ~ KCNJ11_rs5219GluGlu * KCNJ11_rs5219GluLys *
                       CDKN2B_rs10811661CC * CDKN2B_rs10811661CT *
                       CDKAL1_rs7756992AA * CDKAL1_rs7756992AG *
                       BMI * Age, data = train_data, family = binomial)
lr_stepwise <- step(mInit, scope = Scope, direction = "both")
summary(lr_stepwise)

# Stepwise: highest significance and independent (2)
# mInit <- glm(T2D ~ 1, data = train_data, family = binomial)
# Scope <- formula(T2D ~ KCNJ11_rs5219GluGlu + KCNJ11_rs5219GluLys +
#                       CDKN2B_rs10811661CC + CDKN2B_rs10811661CT +
#                       CDKAL1_rs7756992AA + CDKAL1_rs7756992AG +
#                       CDKAL1_rs9465871CC + CDKAL1_rs9465871CT +
#                       CDKAL1_rs10946398AA + CDKAL1_rs10946398AC +
#                       BMI + Age, data = train_data, family = binomial)
# logistic_model <- step(mInit, scope = Scope, direction = "both")
# summary(logistic_model)

# Store logistic models
saveRDS(lr_basic, file = "models/logistic_model_basic.rds")
saveRDS(lr_paper, file = "models/logistic_model_paper.rds")
saveRDS(lr_general, file = "models/logistic_model_general.rds")
saveRDS(lr_stepwise, file = "models/logistic_model_stepwise.rds")

# Predictions
predictions_basic<- predict(lr_basic, newdata = test_data, type = "response")
predicted_classes_basic <- ifelse(predictions_basic > 0.5, 1, 0)
predictions_paper <- predict(lr_paper, newdata = test_data, type = "response")
predicted_classes_paper <- ifelse(predictions_paper > 0.5, 1, 0)
predictions_general <- predict(lr_general, newdata = test_data, type = "response")
predicted_classes_general <- ifelse(predictions_general > 0.5, 1, 0)
predictions_stepwise <- predict(lr_stepwise, newdata = test_data, type = "response")
predicted_classes_stepwise <- ifelse(predictions_stepwise > 0.5, 1, 0)

# Evaluation
confusionMatrix(as.factor(predicted_classes_basic), as.factor(test_data$T2D))
conf_matrix_paper <- confusionMatrix(as.factor(predicted_classes_paper), as.factor(test_data$T2D))
confusionMatrix(as.factor(predicted_classes_general), as.factor(test_data$T2D))
conf_matrix_stepwise <- confusionMatrix(as.factor(predicted_classes_stepwise), as.factor(test_data$T2D))

# Store ROC Curve
saveRDS(conf_matrix_paper, file = "models/conf_matrix_paper.rds")
saveRDS(conf_matrix_stepwise, file = "models/conf_matrix_stepwise.rds")

# ROC Curve - AUC
roc_general <- roc(test_data$T2D, predictions_general)
roc_basic <- roc(test_data$T2D, predictions_basic)
roc_paper <- roc(test_data$T2D, predictions_paper)
roc_stepwise <- roc(test_data$T2D, predictions_stepwise)

# Store ROC Curve
saveRDS(roc_paper, file = "models/roc_paper.rds")
saveRDS(roc_stepwise, file = "models/roc_stepwise.rds")

auc_general <- auc(roc_general)
auc_basic <- auc(roc_basic)
auc_paper <- auc(roc_paper)
auc_stepwise <- auc(roc_stepwise)

# ROC plot of chosen models
ggplot() +
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
  annotate("text", x = 0.6, y = 0.25, label = paste("AUC (Paper):", round(auc_paper, 2)), color = "green", size = 4) +
  annotate("text", x = 0.6, y = 0.15, label = paste("AUC (Stepwise):", round(auc_stepwise, 2)), color = "purple", size = 4)

# Odds Ratios
odds_ratios <- exp(coef(lr_general))
print(odds_ratios)

# Confidence intervals for Odds Ratios
conf_int <- exp(confint(lr_general))
print(conf_int)

# Create a data frame for plotting
plot_odds <- data.frame(
  Variable = names(odds_ratios),                  # Predictor names
  OR = odds_ratios,                               # Odds ratios
  Lower = conf_int[, 1],                 # Lower confidence limits
  Upper = conf_int[, 2]                  # Upper confidence limits
)

# Remove intercept from the data frame
plot_odds <- plot_odds[plot_odds$Variable != "(Intercept)", ]

# Plot
ggplot(plot_odds, aes(x = Variable, y = OR, ymin = Lower, ymax = Upper)) +
  geom_pointrange(color = "blue") +         # Point for OR, line for CI
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # Reference line at OR = 1
  coord_flip() +                            # Flip the axes for horizontal view
  labs(
    title = "Odds Ratios with 95% Confidence Intervals",
    x = "Predictors",
    y = "Odds Ratio (OR)"
  ) +
  theme_minimal()

# # Precision-Recall curve
# pr_curve <- pr.curve(scores.class0 = predictions[test_data$T2D == 1],
#                      scores.class1 = predictions[test_data$T2D == 0],
#                      curve = TRUE)
# plot(pr_curve)




