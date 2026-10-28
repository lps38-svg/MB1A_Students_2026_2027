
s <- 0.01 # selection coefficient
n <- 5000 # number gens
p1 <- 0.01 # start frequency
h <- 0.3 #dominance level
w <- 1 #mean fitness, it is reassigned in the loop anyway
p <- numeric(n)
p[1] <- p1

#frequency change calculations
for (i in 1:(n - 1)) {
  w <- 1 + (p[i]*p[i]*s) + (2*p[i]*(1-p[i])*h*s)
  p[i + 1] <- ((p[i]*p[i]*(1+s)) + (p[i]*(1-p[i])*(1+(h * s))))/w
}

# Plot the iteration number against the result
plot(1:n, p,
     type = "b",
     pch = 16,
     col = "blue",
     xlab = "Generation",
     ylab = "Allelic frequency",
     main = "Frequency over time",
     grid = TRUE)
