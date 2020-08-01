#-------------------------------------------------------------------------------------------------
# Script para ajuste do modelo de regressão logística
#
#
#                                                                        Antonio C. da Silva Júnior 
#                                                                               juniorssz@gmail.com
#                                                                23-Jul-2020 | Curitiba/PR - Brasil
#--------------------------------------------------------------------------------------------------
# Requisitos:
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
df_train <- read.csv("Data/anonymous_train.csv")
df_test <- read.csv("Data/anonymous_test.csv")

#--------------------------------------------------------------------------------------------------
# Stepwise:
set.seed(SEED)
full_model <- glm(y ~ .,
                  data = df_train,
                  family = binomial())

step_model <- step(full_model, k = qchisq(0.05, df = 1, lower.tail = F), direction = "both")











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


#--------------------------------------------------------------------------------------------------
# LASSO:
set.seed(SEED)

X_train <- model.matrix(y ~ ., df_train)
y_train <- df_train$y

X_test <- model.matrix(y ~ ., df_test)
y_test <- df_test$y

fit_lasso <- glmnet(X_train, y_train, family = 'binomial', alpha = 1, standardize = FALSE)
plot(fit_lasso, xvar="lambda", label=TRUE, lwd = 2, cex = 20)

cv_fit <- cv.glmnet(X_train, y_train, family = 'binomial', alpha = 1, nfolds = 5, grouped = F)
plot(cv_fit)

# best_lambda <- cv_fit$lambda.min
# fit_lasso_best <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda)
# pred <- predict(lasso_best, s = best_lam, newx = x_vars[x_test,])

coefs_lasso <- coef(fit_lasso, s = cv_fit$lambda.min)
coefs_lasso_names <- row.names(coefs_lasso)
coefs_lasso <- as.data.frame(matrix(coefs_lasso))
coefs_lasso$names <- coefs_lasso_names
coefs_lasso <- coefs_lasso %>%
  filter(V1 != 0)

features <- coefs_lasso$names[coefs_lasso$names != "(Intercept)"]
X_train <- df_train[features]
y_train <- df_train$y

set.seed(SEED)
fit_lasso_cv <- train(x = X_train,
                         y = y_train,
                         trControl = trainControl(method = "cv", number = 5),
                         method = "glm",
                         family = "binomial")

summary(fit_lasso_cv)
logit_diag(fit_lasso_cv$finalModel)
fit_lasso_final <- fit_lasso_cv$finalModel

# desempenho
pred <- predict(fit_lasso_final, newdata = df_test, type = "response")
roc_curve <- roc(df_test$y, pred, plot=F, ci=T, ci.sp = T)
auc <- round(auc(roc_curve),4)
plot(roc_curve, print.thres = c(0.001, 0.03, 0.1, 0.2, 0.4, 0.5, 0.6, 0.62, 0.7, 0.9, 0.95, 0.96, 0.999),
     print.thres.pattern.cex = 0.9, main = paste("AUC:", auc))

prevalence <- prop.table(table(rbind(df_train, df_test)$y))[2] 
cutoff <- as.numeric(coords(roc_curve, x = "best", best.method = "youden", 
                            best.weights=c(1.1, prevalence), transpose = F)[1])
print(paste("Cutoff:", cutoff))
coords(roc_curve, x = cutoff, ret = c("sensitivity", "specificity", "accuracy"), transpose = F)
pred <- as.factor(ifelse(pred > 0.53, 1, 0))
confusionMatrix(pred, as.factor(df_test$y), positive = "1")


#--------------------------------------------------------------------------------------------------











set.seed(SEED)

model <- glm(formula = y ~ .,
             data = df_train,
             family = binomial)


