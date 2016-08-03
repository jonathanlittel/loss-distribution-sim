library(MASS)

n <- 100 # set trials / matrix width
k <- nrows(na.omit(df.rap$pred)) # set matrix length

first_rv_matrix <- runif(1)


mu <- rep(first_rv_matrix,2) # Produce the same mu n times
corrCoef <- 0.7
Sigma <- matrix(corrCoef, nrow=2, ncol=2) + diag(2)*.3 # not sure why this is *.3

rawvars <- mvrnorm(n=n, mu=mu, Sigma=Sigma)

cov(rawvars); cor(rawvars)

plot(rawvars)




# Another way

cor_Matrix <-  matrix(c (1.00, 0.90, 0.20 ,
                         0.90, 1.00, 0.40 ,
                         0.20, 0.40, 1.00), 
                      nrow=3,ncol=3,byrow=TRUE)

library(psych) 

fit<-principal(cor_Matrix, nfactors=2, rotate="none")

fit$loadings

loadings<-matrix(fit$loadings[1:2, 1:2],nrow=2,ncol=2,byrow=F)
loadings

#create three rannor variable

cases <- t(replicate(2, rnorm(3000)) ) #edited, changed to 3000 cases from 150 cases

multivar <- loadings %*% cases
T_multivar <- t(multivar)

var<-as.data.frame(T_multivar)

cor(var)