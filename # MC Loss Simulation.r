##################
# PAR as default #
##################

# Set parameters

n <- 100
set.seed(99)



PDs <- as.data.frame(
  replicate(n,
            (rap$predicted_wo))
)
  
EADs <- as.data.frame(
  replicate(n,                   # Don't really need to rep this
            rap$EAD              # Add lgd and guar here  
            )
)

rand <- as.data.frame(
  replicate(n,    # note that this doesn't allow for set.seed, maybe do loop..
    runif(nrow(PDs))
  )
)


outcome <- as.data.frame(
  replicate(n,    # note that this doesn't allow for set.seed, maybe do loop..
   rep(0, nrow(PDs))
  )
)


for(i in 1:dim(PDs)[1]) {
  for(j in 1:dim(PDs)[2]) {
    if(PDs[i,j]>rand[i,j]) outcome[i,j] <- 1
  }
}

loss <- EADs * rap$LGD * outcome

portfolio_loss <- apply(loss, 2, sum)