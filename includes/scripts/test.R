## @knitr libs

library(dplyr)
library(lingStuff)



## @knitr data

# Generate data
set.seed(1)
vot  <- rnorm(20, 15, 5)
vot  <- sort(vot)
phon <- c(0,1,0,0,0,0,0,1,0,1,0,1,0,1,1,1,1,1,1,1)
df   <- as.data.frame(cbind(vot, phon))



## @knitr fit

# Fit model
glm <- glm(phon ~ vot, data = df, family = "binomial")



## @knitr cross

# Get crossover point
cross_over(glm, cont_pred = "vot")



## @knitr plot

# Plot regression with crossover point
plot(df$vot, df$phon, xlab = "vot", ylab = "phon", 
     pch = 16, col = rgb(0, 0, 204, 102, maxColorValue = 255))
curve(predict(glm, data.frame(vot = x), type = "resp"), add = TRUE)
points(vot, fitted(glm), pch = 20)
abline(v = cross_over(glm, cont_pred = "vot"), lty = 2, lwd = 0.75)
abline(h = 0.5, v = 0)
