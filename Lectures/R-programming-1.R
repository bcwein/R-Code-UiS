# Lecture: R Programming 1
rm(list = ls())

a <- 2 != 1
print(a)

if(a) {
  print("First")
} else {
  print("Second")
}

for (i in c(1, 2, 3, 6, 9)) {
  print(i)
}

for (i in 1:10) {
  print(i)
  if(i >= 5){
    break
  }
}

x <- rep(0.0, 10)
y <- rep(0.0, 10)

for (i in 1:10) {
  x[i] <- 2*i
  y[i] <- x[i] - 2 
}

# Vectorized code
x <- 2*(1:10)
y <- x - 2

