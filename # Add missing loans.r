## Add PDs, balance, and risk status for missing loans


# Add margin_sd as mean
rap.active[!is.finite(rap.active$margin_sd)] <- mean(na.omit(rap$margin_sd))

# duplicate rows
# df.expanded <- df[rep(row.names(df), df$freq), 1:2]
rap$freq <- 0
rap$freq <- 
rap <- rap[rep(row.names(rap), )]

# 1292 should be in dataset now

LoanID <- c(1051,1292,1774,1817,1856,735)
balance_0915 <- c(80000,1008215,475713,231777,23400,103757)
pd1051 <- rap$pd[rap$LoanID==1052]
meanPD <- mean(na.omit(rap$pd))
pd <- c(pd1051, rep(meanPD,length(LoanID)-1))
last_year <- c(rep(1,length(LoanID)))
active <- c(rep(1,length(LoanID)))

Sector.and.Perishability <- c(rap$Sector.and.Perishability[rap$LoanID==1052],
                              'Coffee',...
                              'Coffee')


Current <- c(1,1,1,1,1,1)
Special_Mention <- c()
Substandard <- c()
Doubtful <- c()


missing_loans <- data.frame(LoanID, balance_0915,
                            pd)

rap_test <- merge(rap, missing_loans, all=TRUE)