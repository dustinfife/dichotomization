# Load necessary libraries
library(ggplot2)
library(dplyr)
library(pROC)

set.seed(123)

# 1. Generate latent binary outcomes and continuous Barthel scores
mu_recovered = 65
mu_not_recovered = 55
sd_shared = 10

# Simulate latent binary variable
latent_class = rbinom(n, 1, 0.5)
barthel_score = ifelse(latent_class == 1,
                       rnorm(n, mean = mu_recovered, sd = sd_shared),
                       rnorm(n, mean = mu_not_recovered, sd = sd_shared))

data = data.frame(latent_class, barthel_score)

# 2. Show overlap and difficulty of classification
ggplot(data, aes(x = barthel_score, fill = factor(latent_class))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Overlap of Barthel Scores by Latent Class",
       x = "Barthel Score", fill = "Latent Class")

# Misclassification at various cutoffs
cutoffs = seq(30, 90, by = 5)
misclass = sapply(cutoffs, function(cut) {
  pred_class = ifelse(data$barthel_score >= cut, 1, 0)
  mean(pred_class != data$latent_class)
})
misclass_df = data.frame(Cutoff = cutoffs, Misclassification = misclass)
print(misclass_df)

# 3. Compare models: continuous vs. dichotomized outcome
# Simulate a predictor variable correlated with Barthel score
data$predictor = data$barthel_score + rnorm(n, 0, 10)

# Model 1: Predict continuous Barthel score
model_cont = lm(barthel_score ~ predictor, data = data)
summary(model_cont)

# Model 2: Predict dichotomized Barthel score
data$barthel_binary = ifelse(data$barthel_score >= 60, 1, 0)
model_bin = glm(barthel_binary ~ predictor, data = data, family = binomial)
summary(model_bin)

# ROC curve for dichotomized model
roc_obj = roc(data$barthel_binary, predict(model_bin, type = "response"))
plot(roc_obj, main = "ROC Curve for Dichotomized Model")
auc(roc_obj)

# 4. Simulate data without latent binary variable
barthel_score2 = rnorm(n, mean = 60, sd = 15)
predictor2 = barthel_score2 + rnorm(n, 0, 10)
data2 = data.frame(barthel_score = barthel_score2, predictor = predictor2)
data2$barthel_binary = ifelse(data2$barthel_score >= 60, 1, 0)

# Model on continuous outcome
model_cont2 = lm(barthel_score ~ predictor, data = data2)
summary(model_cont2)

# Model on dichotomized outcome
model_bin2 = glm(barthel_binary ~ predictor, data = data2, family = binomial)
summary(model_bin2)

# ROC curve
roc_obj2 = roc(data2$barthel_binary, predict(model_bin2, type = "response"))
plot(roc_obj2, main = "ROC Curve for Dichotomized Model (No Latent Class)")
auc(roc_obj2)

# 5. Scenario where predictor-outcome relationship is non-linear
# Simulate non-linear relationship
data$predictor_nl = data$predictor
data$barthel_score_nl = data$predictor_nl^2 + rnorm(n, 0, 10)

# Model with linear assumption
model_nl = lm(barthel_score_nl ~ predictor_nl, data = data)
summary(model_nl)

# Model with quadratic term
model_quad = lm(barthel_score_nl ~ predictor_nl + I(predictor_nl^2), data = data)
summary(model_quad)

# Compare models
anova(model_nl, model_quad)