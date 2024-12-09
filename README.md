# random-E11
The main objective of this project is to develop a robust and accurate predictive model capable of identifying risk factors and genetic markers associated with Type 2 diabetes mellitus (T2D). 

## Literature Review:

Based on the findings from: *Nikitin, A. G., et al. (2017).Association of polymorphic markers of genes FTO, KCNJ11, CDKAL1, SLC30A8, and CDKN2B with type 2 diabetes mellitus in the Russian population. PeerJ, 5, e3414.* https://doi.org/10.7717/peerj.3414:

> In the Russian population, genes, which affect insulin synthesis and secretion in the β-cells of the pancreas, play a central role in the development of T2DM.
> 
> The analysis of the frequency distribution of polymorphic markers for genes KCNJ11, CDKAL1, SLC30A8 and CDKN2B showed statistically significant associations with T2DM in the Russian population. The association between the FTO gene and T2DM was not statistically significant. The polymorphic markers rs5219 of the KCNJ11 gene, rs13266634 of the SLC30A8 gene, rs10811661 of the CDKN2B gene and rs9465871, rs7756992 and rs10946398 of the CDKAL1 gene showed a significant association with impaired glucose metabolism or impaired β-cell function.

## Data Processing

In **R/01_data_cleaning.R**, the following data preprocessing methods were applied:

### Handling Missing Data (NAs): 
Missing values were addressed using appropriate imputation techniques to ensure the data is complete and ready for analysis.

### Normalization of Numerical Variables: 
The numerical variables (such as Age and BMI) were scaled to standardize their range, ensuring that all variables contribute equally to the model.

### One-Hot Encoding of Categorical Variables: 
The categorical SNP variables were transformed using one-hot encoding, converting them into binary format (0 or 1), making them suitable for logistic regression.

## Model Training

In **R/02_logistic_regression.R**, various logistic regression models were trained and evaluated on the cleaned dataset. Key steps included:

### Model training: 
Several logistic regression models were trained to predict the likelihood of T2DM based on the SNPs, along with other factors like Age and BMI.

### Model evaluation: 

The best model was chosen based on several performance metrics:
- ROC Curves and AUC (Area Under the Curve) to assess the model's ability to discriminate between T2DM patients and control subjects.
- P-values of covariates to assess the statistical significance of each predictor.
- Odds Ratios to interpret the strength and direction of the association between each SNP and T2DM risk.
- Literature Review to ensure that the selected model aligns with previous research and known factors influencing T2DM.
- Splitwise method to iteratively compare various models, evaluating their performance through AIC.

## API Development

In  **R/API.R** an API was developed using Plumber and documented with Swagger to allow users to interact with the trained logistic regression models. The API provides HTTP requests to:

- Predict T2DM based on input data (Age, BMI, and SNPs).
- Return model metrics such as accuracy, kappa, sensitivity, specificity, and other performance statistics.

> [!NOTE]
> **How to test the model?**
>
> Download the local repo and open with RStudio. You can run the API locally and test it with Swagger from **03_API.R**.
> Make sure to edit the path for: models, ROC, conf_matrix and scale_values from line 9 to 19 to match your set up.
>
> You can type *pr <- plumb("YOUR_PATH/03_API.R")* and *pr$run(port = 8000)* on your console to run the API locally!





*By TheRandomGroup*


