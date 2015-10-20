# Purpose is to take predicted loss probabilities from the regression model
# And produce simulation of portfolio loss

wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation"
setwd(wd)
filename <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation/predicted_default.csv"
rap <- read.csv(filename, header=TRUE, sep=",")

# Merge in guarantee information
filename <- "EAD and LGD analysis.csv"
guar_data <- read.csv(filename, header=TRUE, sep=",")
colnames(guar_data)[1] <- "LoanID"
# Merge keeping only LoanID in rap set, not those in guar but not in rap
rap <- merge(x=rap, y=guar_data, by=c("LoanID"),all.x=TRUE)

# subset to remove NA predicted defaults
rap <- rap[!is.na(rap$predicted_default),]
rap <- rap[!is.na(rap$balance_0615),]
rap <- rap[rap$last_year==1,]
sum(na.omit(rap$balance_0615))

# subset to get only one row per loan
# rap <- rap[rap$last_year==1,]

rap$balance_0615[is.na(rap$balance_0615)] <- 0
##################
# PAR as default #
##################

# Set parameters
# Create empty vector for recording output of trials
n <- 100
set.seed(99)
z <- rnorm(n) # don't actually need this
losses <- vector()
loss_count <- vector()
# Set loss given default
lgd <- 0.25

# Perform loss simulation for a number of trials n, and add total to vector 'losses' and count to 'loss_count'

	# 	# Simulate losses based on normally distributed probability of full loss at given provision rate
	# 	for (l in seq(along=1:length(rap$predicted_default))) {
	# 		mu <- rap$predicted_default[l]
	# 		# set.seed(i) # retain the same series of random numbers for reproducibility
	# 		if (runif(1) <= mu ) { 
	# 			# rap$loss <- rap$balance_0615[l] * lgd
	# 			# c( loss <- loss + rap$balance_0615[l] * lgd , counter <- counter + 1)
	# 			loss[l,i] <- rap$balance_0615[l] * lgd
	# 		}
	# 	}
	# 	# losses <- c(losses, loss)
	# 	# loss_count <- c(loss_count, counter)
	# }

for (i in seq(along=1:n)) {
	loss <- 0
	counter <- 0
	# Simulate losses based on normally distributed probability of full loss at given provision rate
	for (l in seq(along=1:length(rap$predicted_default))) {
		mu <- rap$predicted_default[l]
		# set.seed(n) # retain the same series of random numbers for reproducibility
		if (runif(1) <= mu ) { c( loss <- loss + rap$balance_0615[l] * lgd, counter <- counter + 1)}
	}
	losses <- c(losses, loss)
	loss_count <- c(loss_count, counter)
}	

##################
# WO as default #
##################

lossesWO <- vector()
loss_countWO <- vector()
# Set loss given default
lgd <- 0.65

for (i in seq(along=1:n)) {
	loss <- 0
	counter <- 0
	# Simulate losses based on normally distributed probability of full loss at given provision rate
	for (l in seq(along=1:length(rap$predicted_wo))) {
		mu <- rap$predicted_wo[l]
		# set.seed(n) # retain the same series of random numbers for reproducibility
		if (runif(1) <= mu ) { c( loss <- loss + rap$balance_0615[l] * lgd, counter <- counter + 1)}
	}
	lossesWO <- c(lossesWO, loss)
	loss_countWO <- c(loss_count, counter)
}	


# Write CI
loss_conf_level <- c("50%","95%", "99%","99.9%")
loss_amounts_PAR90 <- quantile(losses, c(0.5, 0.95, 0.99, 0.999))
loss_as_pct_of_portfolio_PAR90 <- quantile(losses, c(0.5,0.95, 0.99, 0.999)) / sum(na.omit(rap$balance_0615))

loss_amounts_Writeoff <- quantile(lossesWO, c(0.5, 0.95, 0.99, 0.999))
loss_as_pct_of_portfolio_Writeoff <- quantile(lossesWO, c(0.5, 0.95, 0.99, 0.999)) / sum(na.omit(rap$balance_0615))


output_default <- data.frame(loss_conf_level, loss_amounts_PAR90, loss_as_pct_of_portfolio_PAR90, loss_amounts_Writeoff, loss_as_pct_of_portfolio_Writeoff)
write.csv(output_default, "output_default_85pctWO.csv")



################
# Now graph it #
################

library(ggplot2)
p <- sum(na.omit(rap$balance_0615))
dens <- ggplot(rap, aes(x = losses/p))
dens + geom_density()



# + aes(y = ..count..)


percentileWO <- ecdf(lossesWO) # this function puts things into a form where you can input a number and get the estimate percentile in the distro
percentileWO(1/.89*5500000) # This is to estimate the prob of a $5.5M loss on a 105M portfolio, given our $89M data portfolio.