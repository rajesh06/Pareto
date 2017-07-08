pev <- function (x, q = 1.5){
  lev <- q - x ^ (1 - q)
  lev <- lev / (q - 1)
  return(lev)
}

b <- c(1 - 7.5^(-1.5), 1 - 3^(-1.5), 1 - 2.5^(-1.5) )
a <- pev(c(7.5,3), q = 1.5)
c <- (a[1] - a[2]) / (1 - b[2])
c * 25000 #confirm average severity
7 * (1 - b[2]) #confirm average freq
7 * (1 - b[2]) * c # loss

