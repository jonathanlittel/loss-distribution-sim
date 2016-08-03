# Load file and setup r
##################
wd <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/",
            "Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation",
            sep="")
setwd(wd)
filename <- "predicted_default.csv"
rap_source <- read.csv(filename, header=TRUE, sep=",")


# Merge in ead and lgd information
filename <- "EAD and LGD analysis.csv"
guar_data <- read.csv(filename, header=TRUE, sep=",")
colnames(guar_data)[1] <- "LoanID"
# Merge keeping only LoanID in rap set, not those in guar but not in rap
rap_1 <- merge(x=rap_source, y=guar_data, by=c("LoanID"),all.x=TRUE)
# TODO this merge is adding two rows for some reason

# Merge in guarantee information
filename <- "List of 3rd Party Guarantees.csv"
third_party_guar_data <- read.csv(filename, header=TRUE, sep=",", stringsAsFactors = FALSE)
head(third_party_guar_data)
third_party_guar_data <- third_party_guar_data[,c(2,4)]
rap_2 <- merge(x=rap_1, y=third_party_guar_data, by=c("LoanID"), all.x=TRUE)

# Merge in rr pds
  filename <- "rr_predicted_default.csv"
  rr_pds <- read.csv(filename, header=TRUE, sep=",", stringsAsFactors = FALSE)
  keep_cols <- c('pd_RR', 'LoanID')
  rr_pds <- rr_pds[,names(rr_pds) %in% keep_cols]
  rap_all <- merge(x=rap_2, y=rr_pds, by=c("LoanID"), all.x=TRUE)


# Remove NAs for third party guarantees
rap_all$Guarantee_pct[is.na(rap_all$Guarantee_pct)] <- 0

rap <- rap_all

rap$pd_logit_model <- rap$pd
rap$pd <- rap$pd_RR

# Make a plot comparing the two pds

  regComp <- lm(rap$pd_RR~rap$pd_logit_model)
  plot(rap$pd_RR,rap$pd_logit_model)
  abline(regComp)
  
# rap.active <- rap.active[!is.na(rap.active$balance_0915),]
rap.active <- rap[which(rap$last_year==1 & !is.na(rap$pd)),]
# rap.active <- rap.active[!is.na(rap.active$pd),]
# rap.active <- rap.active[rap.active$last_year==1,]
# rap.active <- rap.active[rap.active$active==1,]   # this is screwing everything up
# sum(na.omit(rap.active$balance_0915))

# subset to get only one row per loan
# rap.active <- rap.active[rap.active$last_year==1,]

rap.active$balance_0915[is.na(rap.active$balance_0915)] <- 0

####################################
# Create variables for LGD and EAD #
####################################

# Create LGD, 90% for loans with no guarantee, or 1 minus guarantee pct for those with
# TODO maybe need to subtract recovery from guarantee?

# Set loss given default at 90%, or guarantee amount
LGD_no_guarantee <- 0.90
rap.active$LGD <- 0
rap.active$LGD <- ifelse(rap.active$Guarantee_pct<.01, LGD_no_guarantee, (1 - rap.active$Guarantee_pct))

# Create EAD, as 50% and 70% of original approved amount,
# for Lines of Credit and Term loan types, respectively
EAD_pct_LOC <- 0.49
EAD_pct_Term <- 0.69
rap.active$EAD <- ifelse(rap.active$Loan.Type=="Line of Credit", rap.active$Amount * EAD_pct_LOC, NA)
rap.active$EAD <- ifelse(rap.active$Loan.Type=="Term Loan", rap.active$Amount * EAD_pct_Term, rap.active$EAD)

# loss simulation #
##################

# Set parameters
n <- 10000
k <- nrow(rap.active)

# Create PD matrix
PDs <- as.data.frame(
  replicate(n,
            (rap.active$pd))
)

# Create EAD matrix
EADs <- as.data.frame(
  replicate(n,                   # Don't really need to rep this
            rap.active$EAD              # Add lgd and guar here  
  )
)

# Without using correlated rv matrix
# # Generates 0 if PD is less than than random number (no default)
# # or 1 if PD is less than or equal to random
# for(i in 1:dim(PDs)[1]) {
#   for(j in 1:dim(PDs)[2]) {
#     if(PDs[i,j]>rand[i,j]) outcome[i,j] <- 1
#   }
# }

# Generates matrix of correlated gaussian uniform random numbers
library(mvtnorm) 
corCoef <- 0.2

# TODO Should be able to update this to produce z by z corr matrix, 
# to do z number of correlated groups
# Not sure how this would work given current way of using the *rows* of
# the dataframe as the correlated series

S <- matrix(rep(corCoef,k*k),k,k) + diag(k)*(1-corCoef) # Correlation matrix
set.seed(102)
AB <- rmvnorm(mean=rep(0,k),sig=S,n=n) # Gen gaussian variables
U <- pnorm(AB) # Now U is uniform - check using hist(U[,1]) or hist(U[,2])
corr_rand <- t(U)

# Create random (from 0 to 1) matrix
set.seed(101)
w <- n * k
# k * n matrix, populated with k * n random numbers
rand <- matrix( runif(w),k,n )

# Create matrix that compares PDs with random number
outcome <- as.data.frame(
  replicate(n,
            rep(0, nrow(PDs))
  )
)

# Loop through blank outcome matrix, switching to 1 if PD vs RV indicates default
# Note if statement for correlated groups, to reference corr_rand instead of rand
for(i in 1:dim(PDs)[1]) {
  for(j in 1:dim(PDs)[2]) {
    if(rap.active$Sector.and.Perishability[i]!="Coffee")   {  # This is the group to be correlated
      if(PDs[i,j]>corr_rand[i,j]) outcome[i,j] <- 1    # compares to one random corr matrix if in correlated group
    } 
    else { if(PDs[i,j]>rand[i,j]) outcome[i,j] <- 1 }  # compares to another random corr matrix if not in correlated group
  }
}


# Multiplies everything together to get loss
loss <- EADs * rap.active$LGD * outcome

##################


# Summarize results
#####
library(ggplot2)
library(scales)
portfolio_loss <- apply(loss, 2, sum)
portfolio_loss_count <- apply(outcome, 2, sum)

df.loss_rr_under <- data.frame(portfolio_loss, portfolio_loss_count)

# loss_conf_level <- c("50%","83%", "95%", "99%")
loss_as_pct_of_bal_rr_under <- quantile(portfolio_loss, c(.1666, 0.5, 0.8333, 0.95,
                                                 0.99)) / sum(na.omit(rap.active$EAD))
loss_as_pct_of_bal_rr_under

loss_as_pct_of_bal_rr_under_by_1 <- quantile(portfolio_loss, probs = seq(0,1, by=0.01)) / sum(na.omit(rap.active$balance_0915))

sum(na.omit(rap.active$balance_0915))
sum(na.omit(rap.active$EAD))
mean(df.loss_rr_under$portfolio_loss) / sum(na.omit(rap.active$EAD))
sd(df.loss_rr_under$portfolio_loss) / sum(na.omit(rap.active$EAD))

library(ggplot2)
library(scales)
plot(sort(portfolio_loss_count), pch='.')
plot(density(portfolio_loss / sum(EADs[1]) ))

p1 <- ggplot(df.loss, aes(x=portfolio_loss)) + geom_histogram(alpha=.6, fill="grey")

p1 + scale_x_continuous(labels = dollar)

p2 <- ggplot(df.loss, aes(x=portfolio_loss/sum(na.omit(rap.active$EAD)))) 

p2 + geom_histogram(alpha=.6, fill="grey") + scale_x_continuous(labels = percent) +
  xlab("Loss as % of EAD") 


loss.count.plot <- ggplot(df.loss_rr_under, aes(x=sort(portfolio_loss_count))) + geom_dotplot()
loss.count.plot <- ggplot(df.loss_rr_under, aes(x=sort(portfolio_loss_count))) + geom_point()

plot(sort(df.loss_rr_under$portfolio_loss_count), pch='o', cex=.5)

write.csv(df.loss,'risk rating underwriting pd loss model outcomes.csv')
write.csv(outcome,'outcome.csv')
out <- rap.active[,c('pd_RR','LoanID', 'balance_0915','EAD', 'LGD')]
write.csv(out,'pds.csv')
write.csv(loss_as_pct_of_bal_rr_under_by_1,'quantiles.csv')
save.image() # Save for Stress_Test_Summary.rmd

# See below for a bit of documentation
# http://jonathanlittel.com/rc/RAP/Stress_Test_Summary.html