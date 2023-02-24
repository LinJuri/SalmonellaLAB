library("rstan")
library("bayesplot")
library("ggplot2")
library("rstanarm")  
burn_ins <- 10
updates <- 100
pikma.sim <- bugs ("pikma_data.txt", inits = inits, parameters, model = "pikma_model.txt", n.chains=1,n.burnin = burn_ins, n.iter=updates)
temp <- pikma.sim$sims.matrix

sum <- pikma.sim$summary
sum <- sum[1:nrow(sum) - 1,] ## Ignore deviance
param_names <- rownames(sum)
param_names <- rep(param_names,each =dim(pikma.sim$sims.array)[1] )
iterations <- rep(1:90, 52)
values <- pikma.sim$sims.matrix
values <- values[,ncol(values) - 1]
values <- c(values)

df <- data.frame(values,param_names,iterations)
names(df) <- c("parameter","type", "iteration")
ggplot(df, aes(iteration, parameter))+ 
  geom_line()+
  facet_wrap(vars(type))







temp2 <- temp[1:30,1:4]
params <- rep(c("a","b","c","d"), each = 30)
iter <- rep(1:30,4)
temp3 <- data.frame(c(temp2), params, iter)
names(temp3) <- c("parameter","type", "iteration")
ggplot(temp3, aes(iteration, parameter))+ 
  geom_line()+
  facet_wrap(vars(type))









head(mtcars) 
fit <- stan_glm(mpg ~ ., data = mtcars, seed = 1111)
color_scheme_set("red")
posterior <- as.array(fit)
mcmc_intervals(posterior, pars = c("cyl", "drat", "am", "sigma"))
