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
  xlab = "x", ylab = "F(x)", lab = 'Percentile')
lines(x = pareto_df$pareto_x, y = pareto_df$probs, col = "red")


plot(x = 1, xlim = c(0,1), ylim = c(0, 2), type = 'n', 
  ylab = 'Change in F(x)', xlab = 'Percentile')
text_y = c(1.8, 1.6, 1.4)
my_colors <- c("red", "blue", "green")
  
params <- list(q1 = 0.50, q2 = 1, q3 = 3)

sapply(X = params, FUN = function(q) {
  tail_wt <- data_frame(ptile = seq(from = 0.00, to = 0.99, by =.01)) %>%
    dplyr::mutate(pareto_x = (1 - ptile)^(-1 / q))
  tw_plot_y <- (tail_wt$pareto_x[2:100] / tail_wt$pareto_x[1:99])
  tw_plot_x <- tail_wt$ptile[1:99]
  my_col = my_colors[q == params]
  lines(x = tw_plot_x, y = tw_plot_y, type = 'l', col = my_col)
  text(x = 0.1, y = text_y[q == params], label = paste('q = ', q),
    col = my_col, adj = 0)
  return(NULL)
})

set.seed(12345)
lnorm_sim <- rlnorm(n = 500, meanlog = 10, sdlog = 2)
#hist(log(lnorm_sim))
lnorm_sim <- lnorm_sim[order(lnorm_sim)]
#lnorm_sim <- (lnorm_sim/25000)[which(lnorm_sim > 25000)]

no_xs <- length(lnorm_sim)
lnorm_x <- seq(from = 0, to = 1, by = 1 / no_xs)
lnorm_x <- lnorm_x[2:no_xs]

lnorm_y <- lnorm_sim[2:no_xs] / lnorm_sim[1:(no_xs - 1)]

points(x = lnorm_x, y = lnorm_y)

