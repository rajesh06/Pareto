no_points <- 25

pareto_qs <- c(1.1, 1.5, 2.0, 3.0)
set.seed(12345)

dev.off()

par(mfrow = c(2,2))

sapply(X = pareto_qs, FUN = function(pareto_q){
  # simulate via inversion
  ptiles <- runif(n = no_points, min = 0, max = 1)

  data_Fx <- sort(ptiles)
  data_x <- (1 - data_Fx) ^ (-1 / pareto_q)

  model_Fx <- seq(from = 0, to = 1, length.out = no_points)
  model_x <- (1 - model_Fx) ^ (-1 / pareto_q)

  data_tw <- (data_Fx[2:no_points] - data_Fx[1:(no_points - 1)]) /
    (data_x[2:no_points] - data_x[1:(no_points - 1)])

  model_tw <- (model_Fx[2:no_points] - model_Fx[1:(no_points - 1)]) /
    (model_x[2:no_points] - model_x[1:(no_points - 1)])

  plot(x = data_x[1:(no_points - 1)], y = data_tw, ylim = c(0,2),
    ylab = 'Tail Weight', xlab = 'Normalized Claim Size')
  lines(x = model_x[1:(no_points - 1)], y = model_tw, col = 'red')
  text(x = mean(model_x[1:(no_points - 1)]), y = 2,
    labels = paste('q = ', pareto_q, '( n = ', no_points, ')'), adj = c(0,1))
  return(NULL)
}, simplify = TRUE)

