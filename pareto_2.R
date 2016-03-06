library(dplyr)
base_q <- 1.5
pareto_df <- data_frame(probs = seq(from = 0.00, to = 0.999, by =.001)) %>%
  dplyr::mutate(pareto_x = (1 - probs)^(-1 / base_q))
#pareto_df

fit_q <- length(pareto_df$pareto_x) /
  sum(log(pareto_df$pareto_x))
fit_q # Note that this is greater than 1.5

qs <- lapply(X = seq(from = 1, to = 1.5, by = .001), FUN = function(cutpt){
  new_x <- pareto_df$pareto_x[pareto_df$pareto_x > cutpt]
  new_x <- new_x / cutpt
  new_fit_q <- length(new_x)/sum(log(new_x))
})
plot(y = qs, x = seq(from = 1, to = 1.5, by = .001))





plot(x = pareto_df$pareto_x, y = pareto_df$probs, xlim = c(0, 10),
  xlab = "x", ylab = "F(x)")
lines(x = pareto_df$pareto_x, y = pareto_df$probs, col = "red")
