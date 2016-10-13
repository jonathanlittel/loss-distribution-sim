library(lubridate) # for filtering on year
library(mvtnorm) 

# ------------------------------------
#  Load and select data
  wd_source <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/",
              "Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs",
              sep="")
  setwd(wd_source)

  uw_pds <- read.csv('pds_07.27.16.csv')
  pro_pds <- read.csv('loan_el_08.01.16.csv')
  pro_pds$date <- as.Date(as.character(pro_pds$date), "%Y-%m-%d")
  uw_pds$Close.Date <- as.Date(as.character(uw_pds$Close.Date), "%Y-%m-%d")
  active_date <- as.Date('2016-06-30')  # update this when period is updated
  df <- pro_pds %>%
    filter(date == active_date) %>%
    mutate(
    	loan_loss = balance * lgd * ead * (1 - guarantee)
    	) %>%
    select(
    	LoanID,
    	pd,
    	balance,
    	loan_loss,
    	risk_category
    	)

  uw_pds$LoanID[uw_pds$WO=='Non_Writeoff' & uw_pds$Max_Risk_Category=='Doubtful' & uw_pds$active==0]

  ead <- data.frame(
  	current     = 0.71,
  	sm          = 0.89,
  	substandard = 0.96,
  	doubtful    = 0.92
  	)
# ------------------------------------
#  VERIFY AND CHECK
  m <- data.frame(
  	balance = sum(df$balance),  # check this against subledger
    loan_count = nrow(df),      # check this against subledger 
    missing_loan_loss_count = sum(is.na(df$loan_loss)),
    missing_pd_count = sum(is.na(df$pd))
    )
  sum(df$balance[is.na(df$pd)])
  no_pd <- pro_pds %>% filter(date == active_date, is.na(pd))
  m$balance_missing_pd <- sum(no_pd$balance[is.na(no_pd$pd)])
  table(no_pd$risk_category)
  m$avg_loan_loss <- sum(df$pd * df$loan_loss, na.rm = TRUE)
  m$max_loan_loss <- sum(loan_loss)
  t(m) 
# ------------------------------------
#  IMPUTE MISSING VALUES
  # replace loan_loss where missing with 91% (uncollaterized) loss, and no guarantee
  sum(is.na(pro_pds$lgd))
  sum(is.na(df$loan_loss))
  lgd_not_collateralized <-  1 - 0.09
  df <- df %>%
    mutate(
    	loan_loss = ifelse(is.na(loan_loss), balance * ead$current * lgd_not_collateralized, loan_loss)
    	)
  # # replace pds with single values
  missing_current <- 0.09
  missing_sm      <- 0.57
  # df <- df %>% 
  #   mutate(
  #   	pd = replace(pd, is.na(pd) & risk_category==1, missing_current),
  #   	pd = replace(pd, is.na(pd) & risk_category==2, missing_sm)
  #   	)


  n_missing_pd <- sum(is.na(df$pd))
  # replace missing current pds with underwriting pds from 2014/2015  
  pd_set <- pro_pds %>%
    filter(year(date)>2015, risk_category == 1) %>%
    select(pd) 
    # using underwriting pd
	#  pd_set <- uw_pds %>%
    # filter(year(Close.Date)>2013) %>%
    # select(pd) 
	  
  set.seed(101)
  df <- df %>%
    mutate(              # NOTE! Currently this is just sampling ONE pd for all missing loans, the *same* pd 
    	pd_replacement = ifelse(is.na(pd) & risk_category==1, sample(pd_set$pd, 1, replace = FALSE), NA),
    	pd = ifelse(is.na(pd) & risk_category==1, pd_replacement, pd),  
    	pd = replace(pd, is.na(pd) & risk_category==2, missing_sm)
    	)
  apply(df, 2, function(x) sum(is.na(x)))

#---------------------
# alternate pd source

  # use historical average for pd
	# 2010-2013 Historical Average PD % for special mention loans is 57%
	# 2010-2013 Historical Average PD % for substandard loans  is 88%
	# 2010-2013 Historical Average PD % for doubtful loans is  97%

    pds_hist <- ifelse(df$risk_category == 4, 0.97,
  	ifelse(df$risk_category == 3, 0.88,
  		ifelse(df$risk_category == 2, 0.57,
  			df$pd)
  		)
  	)
  sum(is.na(pds_hist))
  
# ------------------------------------
#  SIMULATION

# params
	k         <- nrow(df)   # number of loans
	n         <- 1e4        # number of trials  
	pds       <- df$pd      # use pds_hist for historical avg method
	# pds       <- pds_hist      # use pds_hist for historical avg method
	loan_loss <- df$loan_loss
  corr_fact <- uw_pds$coffee   # need to map this to profile_pd

# Create random gaussian matrix
	set.seed(101)
	w <- n * k
	# k by n matrix, populated with k * n random numbers
	rand <- matrix( runif(w),k,n )

# Create correlated random gaussian matrix
	corCoef <- 0.1
	s <- matrix(rep(corCoef, k*k), k, k) + diag(k)*(1-corCoef) # Correlation matrix
	set.seed(102)
	ab <- rmvnorm(mean=rep(0,k),sig=s,n=n) # Gen gaussian variables
	u <- pnorm(ab)    # Now u rows are uniform random - check using hist(u[,1]) or hist(u[,2])
					  # u columns are correlated: cor(u[,1], u[,4])
	rand_cor <- t(u)  # rows are correlated

# create empty balance outcome matrix
	outcome_bal <- data.frame(
		replicate(n,
            rep(0, k)
            )
		)
	rownames(outcome_bal) <- df$LoanID
# for(i in 1:k) {
#   for(j in 1:n) {
#     if(corr_fact[i]== 1.1 )   {                                # This is the group to be correlated
#       if(pds[i]>rand_cor[i,j]) outcome_bal[i,j] <- loan_loss[i]    # compares to one random corr matrix if in correlated group
#     } 
#     else { if(pds[i]>rand[i,j]) outcome_bal[i,j] <- loan_loss[i] } # compares to another random corr matrix if not in correlated group
#   }
# }

for(i in 1:k) {
  for(j in 1:n) {
   if(pds[i]>rand[i,j]) outcome_bal[i,j] <- loan_loss[i] 
  }
}


loss_outcomes <- apply(outcome_bal, 2, sum)

# could merge back in risk category, and group losses by category
plot(density(loss_outcomes/sum(df$balance)))
plot(density(pds))
plot(sort(pds))
sum(pds * loan_loss)
sum(pds * loan_loss) / sum(df$balance)
summary( (loss_outcomes) / sum(df$balance) )
#-----------------------------------------------------
# OUTPUT
#-------
  library(xlsx)

  wd_out <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/",
            "Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation",
            "/output",
            sep="")
  setwd(wd_out)

  loss_quantiles <- quantile(loss_outcomes/sum(df$balance), probs = seq(0, 1, by = 0.01))
  names(loss_quantiles) <- paste(seq(0, 100, 1), "%", sep = "")
  names(loan_loss) <- df$LoanID
  write.xlsx(pds, 'portfolio_loss_distribution_08.02.16.xlsx', sheetName = 'pds', append = FALSE)
  write.xlsx(loan_loss, 'portfolio_loss_distribution_08.02.16.xlsx', sheetName = 'loan_loss', append = TRUE)
  write.xlsx(loss_outcomes, 'portfolio_loss_distribution_08.02.16.xlsx', 
  	sheetName = 'loss_outcomes', append = TRUE)
  write.xlsx(loss_quantiles, 'portfolio_loss_distribution_08.02.16.xlsx', 
  	sheetName = 'loss_quantiles', append = TRUE)

  write.csv(outcome_bal, 'loss_outcomes_by_loan.csv')

#-----------------------------------------------------
# GRAPHS
#-------
  dfp <- data.frame(
  	loss = loss_outcomes,
  	loss_rate = loss_outcomes / sum(df$balance)
  	)
  p.dist <- ggplot(dfp, aes(x = loss_rate)) + 
  			geom_density(fill = 'grey') + 
  			scale_x_continuous('Portfolio Loss', labels = scales::percent) +
  			# geom_vline(data = data.frame(loss_rate = median(dfp$loss_rate)))
  			geom_vline(xintercept = quantile(dfp$loss_rate, 0.50)) + 
  			geom_vline(xintercept = quantile(dfp$loss_rate, 0.90)) + 
  			geom_vline(xintercept = quantile(dfp$loss_rate, 0.95)) +
  			geom_vline(xintercept = quantile(dfp$loss_rate, 0.99))

  quant_table <- data.frame(
  	loss_rate = quantile(dfp$loss_rate, c(0.50, 0.90, 0.95, 0.99)),
  	loss      = quantile(dfp$loss,      c(0.50, 0.90, 0.95, 0.99))
  	)

# expected loss by risk category
  cat_table <- df %>% 
  mutate(pd = pds) %>%
  group_by(risk_category) %>%
  summarise(
  	expected_loan_loss = sum(pd * loan_loss),
  	expected_loss_rate = sum(pd * loan_loss) / sum(balance),
  	median_pd          = median(pd),
  	exposure           = sum(loan_loss),
  	balance            = sum(balance)
  	# mean_pd            = median(ead)
  	)
  cat_table$risk_category <- c('Current', 'Special Mention', 'Substandard', 'Doubtful')
  cat_table

  save.image('loss_sim_08.02.16.Rdata')
