# Purpose is to take predicted loss probabilities from the regression model
# And produce simulation of portfolio loss

wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation"
setwd(wd)
filename <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation/predicted_default_Oct26.csv"
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
rap_all <- merge(x=rap_1, y=third_party_guar_data, by=c("LoanID"), all.x=TRUE)


# Remove NAs for third party guarantees
rap_all$Guarantee_pct[is.na(rap_all$Guarantee_pct)] <- 0

rap <- rap_all
rap$balance_0915 <- rap$June2016Balance
rap$June2016Balance <- NULL
# subset to remove NA predicted defaults and writeoffs

# rap <- rap[!is.na(rap$balance_0915),]
rap <- rap[!is.na(rap$pd),]
rap <- rap[rap$last_year==1,]
sum(na.omit(rap$balance_0915))

# subset to get only one row per loan
# rap <- rap[rap$last_year==1,]

rap$balance_0915[is.na(rap$balance_0915)] <- 0

# Create EAD, as 50% and 70% of original approved amount,
# for Lines of Credit and Term loan types, respectively
EAD_pct_LOC <- 0.49
EAD_pct_Term <- 0.69

# Create LGD, 10% for loans with no guarantee, or guarantee pct for those with
# TODO maybe need to subtract recovery from guarantee?

LGD_no_guarantee <- 0.90
rap$LGD <- 0
rap$LGD <- ifelse(rap$Guarantee_pct<.01, LGD_no_guarantee, rap$Guarantee_pct)

rap$EAD <- ifelse(rap$Loan.Type=="Line of Credit", rap$Amount * EAD_pct_LOC, NA)
rap$EAD <- ifelse(rap$Loan.Type=="Term Loan", rap$Amount * EAD_pct_Term, rap$EAD)

##################
# WO as default #
##################

n <- 100
set.seed(99)
losses <- vector()
loss_count <- vector()

# Subset to remove missing predicted writeoffs, multiple rows per loan,
# and loans with balances outstanding at period end
rap <- rap_all
rap <- rap[!is.na(rap$pd),]
rap <- rap[rap$last_year==1,]

rap$balance_0915 <- rap$June2016Balance
rap$June2016Balance <- NULL
rap <- rap[rap$balance_0915>0 & !is.na(rap$balance_0915),]

# Create LGD, 90% for loans with no guarantee, or 1 minus guarantee pct for those with
# TODO maybe need to subtract recovery from guarantee?

# Set loss given default at 90%, or guarantee amount
LGD_no_guarantee <- 0.90
rap$LGD <- 0
rap$LGD <- ifelse(rap$Guarantee_pct<.01, LGD_no_guarantee, (1 - rap$Guarantee_pct))

# Create EAD, as 50% and 70% of original approved amount,
# for Lines of Credit and Term loan types, respectively
EAD_pct_LOC <- 0.49
EAD_pct_Term <- 0.69
rap$EAD <- ifelse(rap$Loan.Type=="Line of Credit", rap$Amount * EAD_pct_LOC, NA)
rap$EAD <- ifelse(rap$Loan.Type=="Term Loan", rap$Amount * EAD_pct_Term, rap$EAD)


lossesWO <- vector()
loss_countWO <- vector()

rap$mu <- runif(nrow(rap))

### IMPORTANT toggle
# turn on/off active loans as of given date
note: this is broken and producing a 242,000 long vector, not sure why

for (i in seq(along=1:n)) {
	loss <- 0
	counter <- 0
	# Simulate losses based on normally distributed probability of 
	# full loss at given provision rate
	for (l in seq(along=1:length(rap$pd))) {
		mu <- rap$pd[l]
		# set.seed(n) # retain the same series of random numbers for reproducibility
		if (runif(1) <= mu ) {  loss <- loss + (rap$EAD * rap$LGD)}
	}
	lossesWO <- c(lossesWO, loss)
	loss_countWO <- c(loss_count, counter)
}	

# Write CI
loss_conf_level <- c("50%","95%", "99%","99.9%")
loss_amounts_PAR90 <- quantile(losses, c(0.5, 0.95, 0.99, 0.999))
loss_as_pct_of_portfolio_PAR90 <- quantile(losses, c(0.5,0.95, 0.99,
                                          0.999)) / sum(na.omit(rap$balance_0915))


loss_amounts_Writeoff <- quantile(lossesWO, c(0.5, 0.75, 0.95, 0.99))
loss_as_pct_of_portfolio_Writeoff <- quantile(lossesWO, c(0.5, 0.75, 0.95,
                                  0.99)) / sum(na.omit(rap$EAD))

plot(density(lossesWO/sum(na.omit(rap$EAD))))
loss_as_pct_of_portfolio_Writeoff


output_default <- data.frame(loss_conf_level, loss_amounts_PAR90,
                             loss_as_pct_of_portfolio_PAR90, loss_amounts_Writeoff,
                             loss_as_pct_of_portfolio_Writeoff)
write.csv(output_default, "output_default_85pctWO.csv")



################
# Now graph it #
################

library(ggplot2)
p <- sum(na.omit(rap$balance_0915))
dens <- ggplot(data.frame(lossesWO), aes(x = lossesWO/p))  # It doesn't liek that the data=rap
lossGraph <- dens + geom_density() + xlab("Losses as % of EAD for loans active as of June")
lossGraph

# + aes(y = ..count..)

# Check correlation of default on recover/loss etc, to validate assumption that relatively uncorrelated
# cor(rap$predicted_default,rap$EAD_pct_of_usage, use="pairwise.complete.obs")
# cor(rap$predicted_default,rap$recovery_collections_pct, use="pairwise.complete.obs")

percentileWO <- ecdf(lossesWO) # this function puts things into a form where you can input a number and get the estimate percentile in the distro
percentileWO(1/.89*5500000) # This is to estimate the prob of a $5.5M loss on a 105M portfolio, given our $89M data portfolio.

## Plot June Balance vs the EAD of June balance
actual_balance <- sort(rap$balance_0915)
EAD_balance <- sort(rap$EAD)
plot(actual_balance,type="l",col="red")
lines(EAD_balance,col="green")

write.csv(rap_all, "pds_ead_guar.csv")

write(lossGraph, "loss_distribution.html", append = TRUE)
write(loss_as_pct_of_portfolio_Writeoff, "loss_distribution.html", append = TRUE)
