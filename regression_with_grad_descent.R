library(dplyr)


compute_error <- function(b,m,data)
{
  total_error =0
  for(i in 1:dim(data)[1])
  {
    x = data[i,1]
    y = data[i,2]
    total_error = total_error+(y-(m*x+b))^2
  }
  return(total_error/as.double(dim(data)[1]))
}

gradient_descent_runner <- function(data,starting_b,starting_m,learning_rate,num_iterations)
{
  b = starting_b
  m = starting_m
  for(i in 1:num_iterations)
  {
    b_m <- step_gradient(b,m,data,learning_rate)
    b <- as.vector(unlist(b_m))[1]
    m <- as.vector(unlist(b_m))[2]
  }
  return (b_m)
  
}

step_gradient <- function(b_current,m_current,data,learning_rate)
{
  b_gradient=0
  m_gradient=0
  n = dim(data)[1]
  for(i in 1:dim(data)[1])
  {
    x=data[i,1]
    y=data[i,2]
    
    b_gradient=b_gradient-((2/n)*(y-((m_current*x)+b_current)))
    m_gradient= m_gradient+ ((2/n)* x * (y - ((m_current*x)+b_current)))
                           
  }
  new_b = b_current - (learning_rate*b_gradient)
  new_m = m_current - (learning_rate*m_gradient)
  return(list(new_b,new_m))
}
  
set.seed(100)
data = data.frame(x=rnorm(100),y=rnorm(100))
learning_rate=0.00001
initial_b=0
initial_m=0
num_iterations=10000

lin_var = gradient_descent_runner(data, initial_b, initial_m, learning_rate, num_iterations)
lin_var = as.vector(unlist(lin_var))

data$pred <- (data$x)*lin_var[2]+lin_var[1]

library(ggplot2)
library(tidyr)

data %>% select(y,pred) %>%
  gather(type,value,1:2) %>%
  ggplot(aes(x=y,y=pred))+geom_point()