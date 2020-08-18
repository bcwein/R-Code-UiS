# R programming 4

rm(list = ls())

l <- list(a = c(1, 2), b = TRUE, c = -4)

print(l)

print(l$d)

print(l[[3]])

fun <- function(arg) {
  argSquared <- arg
  return(list(arg = arg, argTimes2=arg*2, argSquared))
}

ret <- fun(5.0)
ret