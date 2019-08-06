
#load packages needed

library(tableone)
library(Matching)

# Read data.File kept to working directory

data_obv_exp<-read.csv("data_obv_exp.csv")


#Shortist the retail behvavior indicating variables to use
xvars<-c("total_transactions","average_spend","item_in_basket")

#look at a table 1
table1<- CreateTableOne(vars=xvars,strata="treatment", data=data_obv_exp, test=FALSE)
## include standardized mean difference (SMD)
print(table1,smd=TRUE)


############################################
#Implement greedy matching on Mahalanobis distance
############################################

levels(data_obv_exp$treatment) <- c(FALSE,TRUE)
data_obv_exp$treatment <- as.logical(data_obv_exp$treatment)
greedymatch<-Match(Tr=data_obv_exp$treatment,M=1,X=data_obv_exp[xvars],replace=FALSE)
matched<-data_obv_exp[unlist(greedymatch[c("index.treated","index.control")]), ]

#get table 1 for matched data with standardized differences
matchedtab1_matching<-CreateTableOne(vars=xvars, strata ="treatment", 
                            data=matched, test = TRUE)
print(matchedtab1_matching, smd = TRUE)

#-overlapping density plots for # of transactions, to be used in the deck
ggplot(matched, aes(x=total_transactions, fill=treatment)) +geom_density(alpha=.4)



#outcome analysis
y_trt_avg_spend<-matched$average_spend_post[matched$treatment==1]
y_con_avg_spend<-matched$average_spend_post[matched$treatment==0]

#pairwise difference
diff_avg_spends<-y_trt_avg_spend-y_con_avg_spend

#paired t-test
t.test(diff_avg_spends)


#outcome analysis
y_trt_total_transactions<-matched$total_transactions_post[matched$treatment==1]
y_con_total_transactions<-matched$total_transactions_post[matched$treatment==0]

#pairwise difference
diff_total_transactions<-y_trt_total_transactions-y_con_total_transactions

#paired t-test
t.test(diff_total_transactions)


##########################
#propensity score matching
#########################


psmodel<-glm(treatment~total_transactions+average_spend+item_in_basket,
             family=binomial(),data=data_obv_exp)

#show coefficients etc
summary(psmodel)
#create propensity score
pscore<-psmodel$fitted.values


#do greedy matching on logit(PS) using Match with a caliper

logit <- function(p) {log(p)-log(1-p)}
psmatch<-Match(Tr=data_obv_exp$treatment,M=1,X=logit(pscore),replace=FALSE,caliper=100)
#the caliper choice is arbitrary,& more to illustrate usage,A large enough caliper has been taken so that no observations are dropped
# the smaller the caliper the stricter we are on the matching.
matched<-data_obv_exp[unlist(psmatch[c("index.treated","index.control")]), ]
xvars<-c("total_transactions","average_spend","item_in_basket")


#get standardized differences
matchedtab1_psm<-CreateTableOne(vars=xvars, strata ="treatment", 
                            data=matched, test = FALSE)
print(matchedtab1_psm, smd = TRUE)

#outcome analysis
y_trt_total_transactions<-matched$total_transactions_post[matched$treatment==1]
y_con_total_transactions<-matched$total_transactions_post[matched$treatment==0]

#pairwise difference
diffy_total_transactions<-y_trt_total_transactions-y_con_total_transactions

#paired t-test
t.test(diffy_total_transactions)



