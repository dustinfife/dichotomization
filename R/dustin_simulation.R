require(flexplot)
require(tidyverse)
source("R/functions.R")


# simulate model where latent class is endogenous -------------------------
# Coefficients
pred_cor = .3
b0 = 60
b1 = .5
b2 = 0.5
b3 = .2  # Strong polynomial
b4 = .4   # Strong interaction


# x1/x2 main effects, x3 polynomial
# Simulate predictors
cor.matrix = matrix(0, nrow=3, ncol=3)
diag(cor.matrix) = 1
d = MASS::mvrnorm(n, c(0,0,0), Sigma = cor.matrix) %>%
  data.frame() %>%
  set_names(c("x1", "x2", "x3"))

# Simulate predictors
x1 = rnorm(n)
x2 = pred_cor * x1 + rnorm(n, 0, sqrt(1 - pred_cor^2))

# Linear predictor and probability
link = b0 + b1*x1 + b2*x2 + b3*x2^2 + b4*x1*x2
p = 1 / (1 + exp(-link))

# Simulate binary outcome
latent_class = rbinom(n, 1, p)


# Simulate barthel scores
mu_recovered     = 70
mu_not_recovered = 50
sd_shared = 10


barthel_score = ifelse(latent_class == 1,
                       rnorm(n, mean = mu_recovered, sd = sd_shared),
                       rnorm(n, mean = mu_not_recovered, sd = sd_shared))

d = data.frame(barthel_score, x1, x2, p, latent_class)
flexplot(p ~ x1, data=d, suppress_smooth=T)
flexplot(barthel_score ~ x1, data=d)



# Simulate model where latent class is exogenous --------------------------

set.seed(123)
n = 1000

# 1. Generate latent binary recovery class
latent_class = rbinom(n, 1, 0.5)

# 2. Generate x1 and x2 as features caused by the latent class 
# Add within-class variability
x1 = rnorm(n, mean = ifelse(latent_class == 1, 2, -2), sd = 1)
x2 = rnorm(n, mean = ifelse(latent_class == 1, 1, -1), sd = 1)

# 3. Simulate Barthel scores as a noisy function of latent class
mu_recovered = 65
mu_not_recovered = 55
sd_shared = 10

barthel_score = ifelse(latent_class == 1,
                       rnorm(n, mean = mu_recovered, sd = sd_shared),
                       rnorm(n, mean = mu_not_recovered, sd = sd_shared))

# Combine into a data frame
d = data.frame(latent_class, x1, x2, barthel_score, barthel_binary)
flexplot(barthel_score~x1, data=d)
  # this separation is weird and not seen in practice!


# Simulate model with two skewed distributions
# barthel with skewed distributions ---------------------------------------


# Coefficients
pred_cor = .3
b0 = -1.5
b1 = 1.75
b2 = 0.5
n = 1000

# Simulate predictors
x1 = rnorm(n)
x2 = pred_cor * x1 + rnorm(n, 0, sqrt(1 - pred_cor^2))

# Linear predictor and probability
link = b0 + b1*x1 + b2*x2
p = 1 / (1 + exp(-link))

# Simulate binary outcome
latent_class = rbinom(n, 1, p)

# Skewed recovery distributions
barthel_score = ifelse(latent_class == 1,
                         rbeta(n, 5, 2),    # right-skewed
                         rbeta(n, 2, 5))    # left-skewed

# Rescale to Barthel scale (e.g., 0–100)
barthel_score = barthel_score * 100

d = data.frame(barthel_score, x1, x2, p, latent_class) %>%
  mutate(barthel_binary = ifelse(barthel_score<mean(barthel_score), 0, 1))
flexplot(p ~ x1|x2, data=d, suppress_smooth=T)
flexplot(p ~ x2, data=d, suppress_smooth=T)
flexplot(barthel_score ~ x1, data=d)
  # interesting! The ogive shape shows up here too
  # further evidence there's not a latent binary variable in the background
flexplot(barthel_score ~ x2, data=d)
flexplot(barthel_score~1, data=d)
# fit model first
mod = lm(barthel_score~x1*x2, data=d)
d$barthel_predictions = predict(mod)
mod_dich = glm(barthel_binary~x1:x2, data=d, family=binomial)
probs = predict(mod_dich, type="response")
rand = runif(length(probs))
d$barthel_preds_binary =0; 
d$barthel_preds_binary[probs<rand] = 1
d$barthel_new_binary = ifelse(d$barthel_predictions<60, 0, 1)

summary_table(with(d, table(barthel_preds_binary, latent_class)))
summary_table(with(d, table(barthel_new_binary  , latent_class)))

# barthel with zero-inflated distributions ---------------------------------------


# Coefficients
pred_cor = .3
b0 = -1.5
b1 = 1.75
b2 = 0.5
n = 1000

# Simulate predictors
x1 = rnorm(n)
x2 = pred_cor * x1 + rnorm(n, 0, sqrt(1 - pred_cor^2))

# Linear predictor and probability
link = b0 + b1*x1 + b2*x2
p = 1 / (1 + exp(-link))

# Simulate binary outcome
latent_class = rbinom(n, 1, p)

# Simulate Barthel score based on latent class
barthel_score = numeric(n)

# Class 1: Recovered — platykurtic normal around 50
# Using t-distribution with low df to flatten the peak
idx1 = latent_class == 1
barthel_score[idx1] = 50 + rt(sum(idx1), df = 1.5) * 10  # fat tails, mean ~50

# Class 0: Not recovered — skewed right, mean ~10
idx0 = latent_class == 0
barthel_score[idx0] = rbeta(sum(idx0), 1.5, 5) * 30      # rescaled for mean ~10

# Clip to valid Barthel range if desired
barthel_score = pmin(pmax(barthel_score, 0), 100)

d = data.frame(barthel_score, x1, x2, p, latent_class) %>%
  mutate(barthel_binary = ifelse(barthel_score<mean(barthel_score), 0, 1))
flexplot(p ~ x1|x2, data=d, suppress_smooth=T)
flexplot(p ~ x2, data=d, suppress_smooth=T)
flexplot(barthel_score ~ x1, data=d)
# interesting! The ogive shape shows up here too
# further evidence there's not a latent binary variable in the background
flexplot(barthel_score ~ x2, data=d)
flexplot(barthel_score~1, data=d)
# fit model first
mod = lm(barthel_score~x1*x2, data=d)
d$barthel_predictions = predict(mod)
mod_dich = glm(barthel_binary~x1:x2, data=d, family=binomial)
probs = predict(mod_dich, type="response")
rand = runif(length(probs))
d$barthel_preds_binary =0; 
d$barthel_preds_binary[probs<rand] = 1
d$barthel_new_binary = ifelse(d$barthel_predictions<60, 0, 1)

summary_table(with(d, table(barthel_preds_binary, latent_class)))
summary_table(with(d, table(barthel_new_binary  , latent_class)))



# Simulate model without any silly dichotomization ------------------------

# Coefficients
pred_cor = .3
b0 = 60
b1 = .5
b2 = 0.5
b3 = .2  # Strong polynomial
b4 = .4   # Strong interaction


# x1/x2 main effects, x3 polynomial
# Simulate predictors
cor.matrix = matrix(0, nrow=3, ncol=3)
diag(cor.matrix) = 1
d = MASS::mvrnorm(n, c(0,0,0), Sigma = cor.matrix) %>%
  data.frame() %>%
  set_names(c("x1", "x2", "x3"))

# Linear predictor and probability
# Core interaction
d$barthel_score = with(d, 60 + 
                         1 * x1 - 1 * x2 - 8 * x1 * x2 + rnorm(n, 0, 5))
d$barthel_score = d$barthel_score + 
  ifelse(d$x3 < -1, 10*d$x3,
         ifelse(d$x3 > 1, -10*d$x3, 0))
d$barthel_binary = ifelse(d$barthel_score >= 60, 1, 0)
d$x1_binary = ifelse(d$x1<0, 0, 1)
# d = d %>%
#   mutate(barthel_score = case_when(
#     x3 <  -1.75                  ~ barthel_score + 0.5 *18* x3,
#     x3 >= -1.75 & x3 < 1 ~ barthel_score - 0.25 *18* x3,
#     x3 >  1                  ~ barthel_score + 0.4 *18* x3,
#     TRUE             ~ barthel_score  # fallback case
#   ))
flexplot(barthel_score ~ x1 | x2, data=d)
flexplot(barthel_binary ~ x2 | x1, data=d, method="logistic")

flexplot(barthel_score ~ x3, data=d)
flexplot(barthel_binary ~ x3, data=d, jitter=c(.1,.1), method="logistic")
continuous_model =  lm(barthel_score~x1 + x2 + x1:x2 + x3 + I(x3^2), data=d)
visualize(continuous_model)

continuous_model =  lm(barthel_score ~x1 + x2 + x1:x2 + x3 + I(x3^2), data=d)
binary_model     = glm(barthel_binary~x1 + x2 + x1:x2 + x3 + I(x3^2), data=d, family=binomial)
visualize(binary_model, formula = barthel_binary~x3)

d$barthel_binary = ifelse(d$barthel_score >= 60, 1, 0)
flexplot(barthel_score ~ x3, data=d)
flexplot(barthel_binary ~ x3, data=d, jitter=c(.1,.1), method="logistic")
flexplot(barthel_score ~ x1 | x2, data=d)
flexplot(barthel_score ~ 1, data=d)


flexplot(barthel_score ~ x2 | x1, data=d)
flexplot(barthel_binary ~ x2 | x1, data=d, method="logistic")
flexplot(barthel_score ~ x3, data=d)
flexplot(barthel_binary ~ x3, data=d, method="logistic")

# compare two: train on full then dichotomize vs. modeling as dichotomized..compare both to latent_class
continuous_model =  lm(barthel_score ~x1 + x2 + x1:x2 + x3 + I(x3^2), data=d)
binary_model     = glm(barthel_binary~x1 + x2 + x1:x2 + x3 + I(x3^2), data=d, family=binomial)

visualize(binary_model, plot="model", formula = barthel_binary~x1 | x2)
visualize(binary_model, plot="model", formula = barthel_binary~x3)


x = seq(0, 1, length.out = 100)
y = 20*x^3 - 30*x^2 + 12*x
plot(x,y)