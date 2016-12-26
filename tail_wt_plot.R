tail_wt_df <- data.frame(
   = 1:100 / 100

)

library(magrittr)
no_points <- 100

pareto_q <- 1.5
set.seed(12345)

ptiles <- runif(n = no_points, min = 0, max = 1)

data_Fx <- sort(ptiles)
data_x <- (1 - data_Fx) ^ (-1 / pareto_q)

model_tw <- (data_Fx[2:no_points] - data_Fx[1:(no_points - 1)]) /
  (data_x[2:no_points] - data_x[1:(no_points - 1)])

model_tw <- (1 / no_points) / (
  (1 - (2:100)/100) ^ (-1/pareto_q) - (1 - (1:99)/100) ^ (-1/pareto_q)
  )

data_tw <- (1 / no_points) /
  (data_x[2:no_points] - data_x[1:(no_points - 1)])
#data_x[1:(no_points - 1)]
plot(x = 1:99, y = data_tw, ylim = c(0,2))
lines(x = 1:99, y = model_tw, col = 'red')
points(x = 1:99, y = model_tw, col = 'red')




