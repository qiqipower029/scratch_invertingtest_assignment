library(tidyverse)
set.seed(3)
n <- 25
x <- runif(n, 0, 100)
sigma <- 10
error <- rnorm(n, 0, sd = sigma)
y <- 45 + 0.1*x + 5e-4*x^2 + 5e-7*x^3 + 5e-10*x^4 + 5e-13*x^5 + error
true_y <- y - error
## simple model
simple_model <- lm(y~x)
## correct model
correct_model <- lm(y~poly(x,5))
## visualization
xy <- data.frame(x,y,true_y=true_y)
theme_set(theme_minimal())
xy %>% ggplot() + 
  aes(x = x, y = y) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y~x, se = F, aes(color="Simple model")) + 
  geom_smooth(method = "lm", formula = y~poly(x, 5), se = F, aes(color="Correct model")) +
  geom_line(aes(x = x, y = true_y, color="True relationship"), size = 1) +
  scale_colour_manual(name="legend", values=c("blue", "red", "green"))

## quality of fit
mean(abs(simple_model$fitted.values - y))
mean(abs(correct_model$fitted.values - y))

# Correct model achieved a better fit but has a problem of overfitting 
# since there are only 25 samples but it used 6 parameters to describe the relationship, 
# where the true coefficients of high-order terms are almost zero. So this
# resulted in high complexity of the model Complex model has a better fit
# but performs bad in prediction. Simple model may has higher bias, but does 
# better in prediction.