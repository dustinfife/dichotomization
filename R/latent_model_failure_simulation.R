
set.seed(123)
n = 1000

# Step 1: Latent binary recovery class
latent_class = rbinom(n, 1, 0.5)

# Step 2: Predictors partially reflect the latent class
x1 = rnorm(n, mean = ifelse(latent_class == 1, 0.5, -0.5), sd = 1.5)
x2 = rnorm(n, mean = ifelse(latent_class == 1, 0.5, -0.5), sd = 1.5)

# Step 3: Outcome (Barthel) reflects latent class with noise
barthel_score = ifelse(latent_class == 1,
                       rnorm(n, mean = 65, sd = 10),
                       rnorm(n, mean = 55, sd = 10))

# Step 4: Dichotomize Barthel
barthel_binary = ifelse(barthel_score >= 60, 1, 0)

# Step 5: Data frame
d = data.frame(latent_class, x1, x2, barthel_score, barthel_binary)

# Step 6: Fit models
model_cont = lm(barthel_score ~ x1 + x2 + I(x2^2) + x1*x2, data = d)
model_bin = glm(barthel_binary ~ x1 + x2 + I(x2^2) + x1*x2, family = "binomial", data = d)

# Step 7: Evaluate performance
pred_cont = predict(model_cont)
rmse = sqrt(mean((d$barthel_score - pred_cont)^2))

pred_bin = predict(model_bin, type = "response")
library(pROC)
auc_val = auc(d$barthel_binary, pred_bin)

# Output
cat("RMSE (continuous model):", round(rmse, 2), "\n")
cat("AUC (dichotomized model):", round(auc_val, 3), "\n")

# Visualizations
library(flexplot)
flexplot(barthel_score ~ x2 | x1, data = d)
flexplot(barthel_binary ~ x2 | x1, data = d)
