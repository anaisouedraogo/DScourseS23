library(nloptr)
library(modelsummary)

#OLS closed form solution

# Set the seed
set.seed(100)

# Define dimensions
N <- 100000
K <- 10

# Create X matrix
x <- matrix(rnorm(N * K, mean = 0, sd = 1), nrow = N, ncol = K)
x[, 1] <- 1  # replace first column with 1's

# Create epsilon vector
sigma <- 0.5
eps <- rnorm(N, mean = 0, sd = sigma)

# Create beta vector
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Create Y vector
y <- x %*% beta + eps

# Compute OLS estimate of beta
beta_hat <- solve(t(x) %*% x) %*% t(x) %*% y
beta_hat

-----------------------------------------------------------------
#gradient descent

# Set the learning rate
alpha <- 0.0000003

# Set the initial guess for beta
beta_gd <- rep(0, K)

# Define the gradient function
grad <- function(beta) {
  t(x) %*% (X %*% beta - y)
}

# Perform gradient descent
for (i in 1:1000) {
  beta_gd <- beta_gd - alpha * grad(beta_gd)
}

beta_gd
-------------------------------------------------------------------
#L-BFGS algorithm

# Our objective function
objfun <- function(beta,y,x) {
    return ( crossprod(y-X%*%beta) )
}
# Gradient of our objective function
gradient <- function(beta,y,x) {
  return ( as.vector(-2*t(x)%*%(y-x%*%beta)) )
}

# initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
# Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
# Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
print(result)
# Check solution
print(summary(lm(y~x)))

---------------------------------------------------------------------------
#nelder mead

# Our objective function
objfun <- function(beta,y,x) {
    return (sum((y-x%*%beta)^2))
}
# initial values
xstart <- 5
# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-8)
# Find the optimum!
res <- nloptr( x0=xstart,eval_f=objfun,opts=options)
print(res)

-------------------------------------------------------------------------------------

#mle

gradient <- function(theta,Y,X) {
grad <- as.vector(rep(0,length(theta))) 
beta <- theta[1:(length(theta)-1)]
sig <- theta[length(theta)] 
grad[1:(length(theta)-1)] <--t(X)%*%(Y- X%*%beta)/(sig^2)
grad[length(theta)] <- dim(X)[1]/sig- crossprod(Y-X%*%beta)/(sig ^3)
return ( grad ) 
}

# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)
# Optimize!
result <- nloptr( x0=,eval_f=objfun,opts=options,y=y,X=X)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]
# Check solution
print(summary(lm(y~x)))

-------------------------------------------------------------------------------------------
 #using lm
beta_lm <-lm(Y ~ X-1)
modelsummary(beta_lm, output='latex')

