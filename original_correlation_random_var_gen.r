# require(mvtnorm)
# 
# n <- 250
# corCeof <- 0.1
# 
# S <- matrix(c(1,corCeof,corCeof,1),2,2) #Correlation matrix
# AB <- rmvnorm(mean=c(0,0),sig=S,n=1000) #Our gaussian variables
# U <- pnorm(AB) #Now U is uniform - check using hist(U[,1]) or hist(U[,2])
# x <- qgamma(U[,1],2) #x is gamma distributed
# y <- qbeta(U[,2],1,2) #y is beta distributed
# plot(x,y) #They correlate!
# 



n <- 250
corCoef <- 0.9

S <- matrix(rep(corCoef,n*n),n,n) + diag(n)*(1-corCoef) # Correlation matrix 
AB <- rmvnorm(mean=rep(0,n),sig=S,n=1000) # Gen gaussian variables
U <- pnorm(AB) # Now U is uniform - check using hist(U[,1]) or hist(U[,2])


# Examples of new transformations:
x <- rnorm(U[,1],2) # x is gamma distributed
y <- qbeta(U[,2],1,2) # y is beta distributed
plot(x,y) # They correlate!



# http://www.r-bloggers.com/copulas-made-easy/




#######
## Other examples



n <- 100
dice <- rnorm(n)

corrCoef <- 0.99

n     <- n                    # length of vector
rho   <- corrCoef                  # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
x1    <- x1                 # fixed given data
x2    <- rnorm(n, 2, 0.5)      # new random data
X     <- cbind(x1, x2)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector







~~~~~~~~~~~~~~~~~~~~~~~
  library(MASS)

n <- 100 # set trials / matrix width
k <- nrows(na.omit(df.rap$pred)) # set matrix length


mu <- rep(0,4)
corrCoef <- 0.7
Sigma <- matrix(corrCoef, nrow=4, ncol=4) + diag(4)*.3

rawvars <- mvrnorm(n=n, mu=mu, Sigma=Sigma)

cov(rawvars); cor(rawvars)

plot(rawvars)