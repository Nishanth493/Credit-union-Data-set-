require(plyr)
require(rpart)
require(ggplot2)

########## Nishanth's Dataset #############
## Read the portfolio data
portfolio = read.csv('C:/Users/nisha/Desktop/First American Assignment/Screen shots/PortfolioData.csv')
writeoff = read.csv('C:/Users/nisha/Desktop/First American Assignment/Screen shots/Writeoff.csv')

portfolio <- portfolio[,-(23:37)]
writeoff <- writeoff[-(587),]

names(portfolio)[1] <- "Account.Number"
names(writeoff)[1] <- "Account.Number"
summary(portfolio)

summary(writeoff)

## Portfolio data shape - 58828*22 vars
## Writeoff data shape - 586 * 22 vars
names(portfolio) ## Identifiers: Account.Number; Member.ID; 
names(writeoff)
head(writeoff)
writeoff = writeoff[,c(1:22)]

#### checking member.id
length(unique(portfolio$Member.ID)) ## Each record is for unique member id
length(unique(writeoff$Member.ID)) ## 586
length(intersect(portfolio$Member.ID,writeoff$Member.ID)) ## 0's

## checking account.number
length(unique(portfolio$Account.Number)) ## Each record is for unique member id
length(unique(writeoff$Account.Number)) ## 586
length(intersect(portfolio$Account.Number,writeoff$Account.Number)) ## 0's

###Distribution of Delinquent and Write.Offs
table(portfolio$Delinquent., portfolio$Write.Offs) ## 4% are delinquent and no write offs
table(writeoff$Delinquent., writeoff$Write.Offs) ## 78% are delinquent and 100% write offs
head(portfolio)

#### All the factor variables: Look for good graphs ; Co-Borrower; Applied.At, New.Used.Vehicle,
##Fixed.or.VariableRate ##Vehicle.Type
portfolio$response_var = ifelse(portfolio$Delinquent. == 'Y',1,0)
table(portfolio$response_var)

######### Categorical levels: Delinquent Data #####
##(1) Applied.At -- Not significant for Dealer or Branch
category1 = ddply(portfolio, .(Applied.At), summarize,
                  DQ = sum(response_var),
                  ListingsCount = length(Member.ID))
str(category1)
category1$DQ_perc = category1$DQ/category1$ListingsCount * 100 ## Branch - 4.3%; Dealer - 4.26%

##(2) New.Used.Vehicle
category2 = ddply(portfolio, .(New.Used.Vehicle), summarize,
                  DQ = sum(response_var),
                  ListingsCount = length(Member.ID))
str(category2)
category2$DQ_perc = category2$DQ/category2$ListingsCount* 100 ## New - 6.77%; Old - 3.46%

##(3) Fixed.or.VariableRate

category3 = ddply(portfolio, .(Fixed.or.Variable.Rate), summarize,
                  DQ = sum(response_var),
                  ListingsCount = length(Member.ID))
category3$DQ_perc = category3$DQ/category3$ListingsCount* 100 
### Not much difference: Fixed - 4.34%; Variable - 3.9%

## (4) Vehicle.Type 
category4 = ddply(portfolio, .(Vehicle.Type), summarize,
                  DQ = sum(response_var),
                  ListingsCount = length(Member.ID))
category4$DQ_perc = category4$DQ/category4$ListingsCount* 100 
## Economy - 4.3%; Luxury Sedan - 3.73%; Mini-Van - 5.64%; Muscle/sport - 3.83%;
## SUV - 4.16%; Truck - 4.06%

## (5) Co-Borrower
category5 = ddply(portfolio, .(Co.Borrower), summarize,
                  DQ = sum(response_var),
                  ListingsCount = length(Member.ID))
category5$DQ_perc = category5$DQ/category5$ListingsCount* 100 
## No : 4.83%; Yes - 2.17% ; Category with co-borrower yes has lesser DQ %


########## Numeric Variables - Decile Values #########################

## FICO Score, Age, Vehicle Value, Vehicle Year, Interest Rate, YearlySalary
## Create the decile values - FICO
names(portfolio)
portfolio$fico_decile <- with(portfolio, cut(portfolio$FICO, 
                                             breaks=quantile(portfolio$FICO, probs=seq(0,1, by=0.1), na.rm=TRUE), 
                                             include.lowest=TRUE))

category6 = ddply(portfolio, .(fico_decile), summarize,
                  DQ = sum(response_var),
                  ListingsCount = length(Member.ID))

category6_b = ddply(portfolio, .(fico_decile), summarize,
                   DQ = sum(response_var),
                   ListingsCount = length(Member.ID),
                   AvgIr = mean(Interest.Rate.))

str(category6_b)
category6_b$DQRate = round(category6$DQ/category6$ListingsCount *100,2)
category6_b
require(ggplot2)
graph_cat6 <- ggplot(category6, aes(x = category6$fico_decile,y = category6$DQRate), col("steelblue"), "curl") +
              geom_point() + 
              geom_smooth(method="loess", se=F)  + 
              labs(title="FICO(Decile) Vs. Delinquent", y="DQ-Rate", x="FICO(Decile)")

## Observation: <=675 definitely Delinquent


## Create the decile values - Age
names(portfolio)
portfolio$age_decile <- with(portfolio, cut(portfolio$Age, 
                                            breaks=quantile(portfolio$Age, probs=seq(0,1, by=0.1), na.rm=TRUE), 
                                            include.lowest=TRUE))

category7 = ddply(portfolio, .(age_decile), summarize,
                  DQ = sum(response_var),
                  ListingsCount = length(Member.ID))
category7$DQRate = round(category6$DQ/category6$ListingsCount *100,2)
category7
## Observation: Age<=31 definitely Delinquent
######## FICO and Age combinations

category8 = ddply(portfolio, .(age_decile,fico_decile), summarize,
                  DQ = sum(response_var),
                  ListingsCount = length(Member.ID))
category8$DQRate = round(category8$DQ/category8$ListingsCount *100,2)
category8
## Observation: FICO is a strong variable than Age



## Create the decile values - Vehicle Year
portfolio$vy_decile <- with(portfolio, cut(portfolio$Vehicle.Year, 
                                           breaks=quantile(portfolio$Vehicle.Year, probs=seq(0,1, by=0.1), na.rm=TRUE), 
                                           include.lowest=TRUE))

category9 = ddply(portfolio, .(vy_decile), summarize,
                  DQ = sum(response_var),
                  ListingsCount = length(Member.ID))
category9$DQRate = round(category9$DQ/category9$ListingsCount *100,2)
category9
# Observation: 2010-2012 has the highest DQ rate


## Create the decile values - InterestRate
portfolio$Interest.Rate. <- data.frame(portfolio$Interest.Rate.,stringsAsFactors = FALSE)

portfolio$ir_decile <- with(portfolio, cut(portfolio$Interest.Rate., 
                                           breaks=unique(quantile(portfolio$Interest.Rate., probs=seq(0,1, by=0.1), na.rm=TRUE)), 
                                           include.lowest=TRUE))

category10 = ddply(portfolio, .(ir_decile), summarize,
                   DQ = sum(response_var),
                   ListingsCount = length(Member.ID))
category10$DQRate = round(category10$DQ/category10$ListingsCount *100,2)
category10
## Observation: Positively correlated and 6.5% to 10.5% has the highest DQ rate double as of 5.5% to 6.5%

## Create the decile values - YearlySalary
portfolio$ys_decile <- with(portfolio, cut(portfolio$Member.Yearly.Salary, 
                                           breaks=unique(quantile(portfolio$Member.Yearly.Salary, probs=seq(0,1, by=0.1), na.rm=TRUE)), 
                                           include.lowest=TRUE))

category11 = ddply(portfolio, .(ys_decile), summarize,
                   DQ = sum(response_var),
                   ListingsCount = length(Member.ID))
category11$DQRate = round(category11$DQ/category11$ListingsCount *100,2)
category11

### Lowest DQ observed in last two deciles (68300, 150005)


## Create the new variables
## 
portfolio$FTI = round(portfolio$Funded.Amount/portfolio$Member.Yearly.Salary * 100,2)
summary(portfolio$FTI)
portfolio$fti_decile <- with(portfolio, cut(portfolio$FTI, 
                                            breaks=unique(quantile(portfolio$FTI, probs=seq(0,1, by=0.1), na.rm=TRUE)), 
                                            include.lowest=TRUE))

category12 = ddply(portfolio, .(fti_decile), summarize,
                   DQ = sum(response_var),
                   ListingsCount = length(Member.ID),
                   AvgIR = mean(Interest.Rate.))
category12$DQRate = round(category12$DQ/category12$ListingsCount *100,2)
category12
## Anything >=145% is highly risky and similarily any loan amount<= 80% is the safest bet

### Read the new data with new variables
portfolio_extra  = read.csv('C:/Users/nisha/Desktop/First American Assignment/Screen shots/Portfolio_ExtraCols.csv')
portfolio_extra <- portfolio_extra[,-(26:40)]  
names(portfolio_extra)
names(portfolio_extra)[1] <- "Account.Number"
portfolio_v1 = merge(portfolio, portfolio_extra, by = 'Account.Number', all.x = TRUE)

##### Correlation with DQ
nums <- unlist(lapply(portfolio_v1, is.numeric))  
numeric_cols = names(portfolio_v1)[nums]
drop_vars = c('Account.Number','Member.ID','Date.of.Birth','Member.Since','Funded.Date.','Maturity.Date.')
numeric_cols = setdiff(numeric_cols, drop_vars)
numeric_data = portfolio_v1[,numeric_cols]

cor_matrix = as.matrix(cor(numeric_data, use = 'pairwise.complete.obs'))
#cor_matrix = rquery.cormat(numeric_data, type="upper")
cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
cor_melt <- arrange(melt(cor_matrix), -abs(value))
cor_melt <- subset(cor_melt, is.na(cor_melt$value) == FALSE)
cor_melt <- cor_melt[order(-cor_melt$value),] 
cor_resp = subset(cor_melt, cor_melt$Var1 == 'response_var' | cor_melt$Var2 == 'response_var')


### Information Value with D
install.packages("tibble")
install.packages("Information")
library(Information)
IV_numeric <- Information::create_infotables(data=numeric_data, y="response_var", parallel=FALSE, bins = 10)
print(IV_numeric$Summary, row.names=FALSE)
IV.Rank_alt <- as_tibble(IV_numeric$Summary)
summary(IV.Rank_alt)
top_vars_IV = subset(IV.Rank_alt, IV.Rank_alt$IV > 0.05)$Variable ## Most significant are FICO, FTI, PercentRemaining, InterestRate
top_vars_IV_df = as.data.frame(top_vars_IV)

sf <- as.data.frame(IV_numeric$Summary, row.names=FALSE)

### Chi Sq for factor variables
data_nonnumeric = portfolio_v1[, (sapply(portfolio_v1, class) %in% c('character', 'factor'))]
data_nonnumeric$y_var = portfolio_v1$response_var
colnames(data_nonnumeric)
drop_vars = c('Co.Borrower.FICO','Delinquent.','Write.Offs')
data_nonnumeric = data_nonnumeric[,!(names(data_nonnumeric) %in% drop_vars)]

for(i in c(1:11)){
  print(colnames(data_nonnumeric)[i])
  tbl = table(data_nonnumeric[,c(i, 12)])
  print(chisq.test(tbl))
}

significant_categoricalvars = c('Co.Borrower','New.Used.Vehicle','Vehicle.Type')

######## Decision Tree for DQ (Training - 80%; Test - 20%)
imp_vars = c(top_vars_IV,significant_categoricalvars,'response_var')
req_sub = portfolio_v1[,imp_vars]

# Classification Tree with rpart
library(rpart)

# grow tree 
fit <- rpart(response_var ~.,
             method="class", data=portfolio_v1)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(fit)
plot(fit, uniform=TRUE, 
     main="Classification Tree for Delinquent")
text(fit, use.n=TRUE, all=TRUE, cex=.6)

# create attractive postscript plot of tree 
post(fit, file = "c:/tree.ps", 
     title = "Classification Tree for Delinquent")
