require(tidyverse)
require(caret)
require(hnp)
require(pROC)
require(glmnet)
source("utils.R")

#--------------------------------------------------------------------------------------------------
# Configuração:
SEED <- 1111
# options(scipen = 99)

#--------------------------------------------------------------------------------------------------
# Carregando os dados
df_train <- read.csv("data/anonymous_train.csv")
df_test <- read.csv("data/anonymous_test.csv")

#--------------------------------------------------------------------------------------------------
# Stepwise:
set.seed(SEED)
full_model <- glm(factor(y) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
                    x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 +
                    x23 + x25 + x26 + x27 + x28 + x29 + x30 + x31 + x32 + x33 + x34,
                  data = df_train,
                  family = binomial())

step_model <- step(full_model, k = qchisq(0.05, df = 1, lower.tail = F), direction = "both")

summary(full_model)
logLik(full_model)

summary(step_model)
logLik(step_model)

anova(step_model, full_model, test = "Chisq")
lrtest(step_model, full_model)
qchisq(0.05, df = 14, lower.tail = F)
# a retirada das 14 covariáveis não afetou a qualidade do modelo (H0 não rejeitada)




set.seed(1111)
full_model <- train(form = factor(y) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
                      x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 +
                      x23 + x25 + x26 + x27 + x28 + x29 + x30 + x31 + x32 + x33 + x34,
                     data = df_train,
                     method = "glm",
                     family = "binomial",
                     trControl = trainControl(method = "cv", number = 5))

step_model <- train(form = factor(y) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
                      x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 +
                      x23 + x25 + x26 + x27 + x28 + x29 + x30 + x31 + x32 + x33 + x34,
                    data = df_train,
                    method = "glmStepAIC",
                    family = "binomial",
                    trControl = trainControl(method = "cv", number = 5))

summary(step_model)





# diagnóstico
summary(step_model)
logit_diag(step_model)
hnp(step_model)

# desempenho
pred <- predict(step_model, newdata = df_test, type = "response")
roc_curve <- roc(df_test$y, pred, plot=F, ci=T, ci.sp = T)
auc <- round(auc(roc_curve),4)
plot(roc_curve, print.thres = c(0.001, 0.05, 0.2, 0.4, 0.5, 0.55, 0.6, 0.65, 0.7, 0.8, 0.9, 0.95, 0.97, 0.999),
     print.thres.pattern.cex = 0.9, main = paste("AUC:", auc))

prevalence <- prop.table(table(rbind(df_train, df_test)$y))[2] 
cutoff <- as.numeric(coords(roc_curve, x = "best", best.method = "youden", 
                            best.weights=c(1.1, prevalence), transpose = F)[1])
print(paste("Cutoff:", cutoff))
coords(roc_curve, x = cutoff, ret = c("sensitivity", "specificity", "accuracy"), transpose = F)
pred <- as.factor(ifelse(pred > 0.53, 1, 0))
confusionMatrix(pred, as.factor(df_test$y), positive = "1")



