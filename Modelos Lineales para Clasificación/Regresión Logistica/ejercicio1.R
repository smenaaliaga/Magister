library(tidymodels)

df <- read.csv('vaso.csv')

# vol : amount of air
# rate: rate of breathing
# vaso: condition of vasculature (1 = no; 0 = si)

summary(df)

df$vaso <- as.factor(df$vaso)

summary(df)

obj_model <- glm(formula = vaso ~ rate + vol, data = df, family = binomial(link = 'logit'))

summary(obj_model)

exp(obj_model$coefficients)

exp(2.875 -4.562 * 1.1 -5.179 * 0.4) / (1 + exp(2.875 -4.562 * 1.1 -5.179 * 0.4))

obj_model_sat <- glm(formula = vaso ~ as.factor(1:length(vaso)), data = df, family = binomial(link = 'logit'))

2 * (logLik(obj_model_sat) - logLik(obj_model))

summary(obj_model)
