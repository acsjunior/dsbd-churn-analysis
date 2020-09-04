require(statmod)

logit_diag <- function(model) {
  set.seed(SEED)
  res <- qres.binom(model)
  pred <- predict(model)
  par(mfrow=c(1,2))
  plot(res ~ pred, col = 'blue', xlab = 'Fitted values', ylab = 'Resíduals')
  lines(lowess(res ~ pred), col = 'red', lwd = 2)
  qqnorm(res, col = 'blue', main = '')
  qqline(res, lty = 2)
  par(mfrow=c(1,1))
}