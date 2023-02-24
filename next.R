


load('C:/Users/chris/OneDrive/Desktop/Capstone/OOA_Proj/projEnv.RData')

library(dplyr)
library(randomForest)
library(varImp)
library(ggplot2)
#library(caret)


logFix = c(
  "amt_ta_income",
  "amt_ta_discretionaryspending_philanthropy",
  "pct_pop_in_college",
  "pct_pop_in_grad_school",
  "pct_pop_in_private_school",
  "amt_tran_total_dollars_purchase",
  "n_tran_credit_card_purchase",
  "amt_financial_assessed_home_value",
  "n_demo_length_of_residence",
  "amt_financial_estimated_available_equity",
  "amt_financial_estimated_monthly_mortgage"
)

for (col in logFix) {
  col_index <- which(colnames(subTrim) == col)  # Get the index of the column to modify
  colnames(subTrim)[col_index] <- paste(col, "log", sep = "_")  # Modify the column name
}

#overall removals
subTrim = subTrim %>% select(c(-'n_tran_credit_card_purchase_log'))


#colnames(subTrim)

#colMeans(is.na(subTrim))
#,'amt_financial_estimated_available_equity_log'

subTrimRF = subTrim %>% select(-c('HomePostCode','HomeState','amt_financial_estimated_available_equity_log'))

subTrimRF$n_demo_length_of_residence_log[subTrimRF$n_demo_length_of_residence_log < 0] <- 0

subTrimRF$amt_financial_assessed_home_value_log[subTrimRF$amt_financial_assessed_home_value_log < 0 ] <- 0

#subTrimRF$amt_financial_estimated_available_equity_log[subTrimRF$amt_financial_estimated_available_equity_log < 0] <- 0

subTrimRF$amt_financial_estimated_monthly_mortgage_log[subTrimRF$amt_financial_estimated_monthly_mortgage_log < 0] <- 0


#drops from 250000 to 95762 :/ 
subTrimRF = na.omit(subTrimRF)


#intially rf coulndt run bc homepost and homestate had too many levels
#sapply(subTrimRF, nlevels)
colMeans(is.na(subTrimRF))


#CORRELATION
# BACHELOR AND GRADUATE HAVE HIGH COR
# INCOME AND NETWORTH HAVE HIGH COR
corr_simple(subTrimRF, 0.8)



################################################################################
#RF MODELING

rf <- randomForest(amt_lifetime_giving_log ~ .,
                   data=subTrimRF, importance=T,
                   ntree=25, do.trace=1, maxnodes=50 )
sort(rf$importance, decreasing = T)
importance(rf)

# x <- cbind(var_name = rownames(x), x)
# sort(x, decreasing = T)

plot(rf)


sum(is.na(subTrimRF))

################################################################################

## LOOKING INTO BINS FOR IMPACTFUL VARS
#VARS
# val_score_telemarketing_score 
# cat_score_p2p_persona_map 
# val_demo_age 
# val_score_philanthropic_score 

ggplot(subTrim, aes(y=amt_lifetime_giving_log, x=val_score_philanthropic_score))+
  stat_summary_bin(fun='mean', geom='bar', bins=20) + ylab("Lifetime Giving($)")+
  xlab("Philanthropic Score") + ggtitle("Philanthropic Score vs LTG")

ggplot(subTrim, aes(y=amt_lifetime_giving_log, x=val_demo_age))+
  stat_summary_bin(fun='mean', geom='bar', bins=20) + ylab("Lifetime Giving($)")+
  xlab("Age") + ggtitle("Age vs LTG")

#binning doesn't match blackbaud score
ggplot(subTrim, aes(y=amt_lifetime_giving_log, x=val_score_telemarketing_score))+
  stat_summary_bin(fun='mean', geom='bar', bins=20) + ylab("Lifetime Giving($)")+
  xlab("Telemarketing Score") + ggtitle("Telemarketing Score vs LTG")

ggplot(subTrim, aes(y=amt_lifetime_giving_log, x=cat_score_p2p_persona_map))+
  stat_summary_bin(fun='mean', geom='bar', bins=20) + ylab("Lifetime Giving($)")+
  xlab("Persona Map") + ggtitle("Persona Map vs LTG")

ggplot(subTrim, aes(y=amt_lifetime_giving_log, x=val_score_sustainer_score))+
  stat_summary_bin(fun='mean', geom='bar', bins=20) + ylab("Lifetime Giving($)")+
  xlab("Sustainer Score") + ggtitle("Sustainer Score vs LTG")



go_getter = subTrim[subTrim$cat_score_p2p_persona_map == '1 Go Getters',]
caring_contri = subTrim[subTrim$cat_score_p2p_persona_map == '2 Caring Contributors',]
causal_contri = subTrim[subTrim$cat_score_p2p_persona_map == '3 Causal Contributors',]
do_good = subTrim[subTrim$cat_score_p2p_persona_map == '4 Do Gooders',]
gen_joe = subTrim[subTrim$cat_score_p2p_persona_map == '5 Generous Joes',]
over_achieve = subTrim[subTrim$cat_score_p2p_persona_map == '6 Over Acheivers',]
cause_enthus  = subTrim[subTrim$cat_score_p2p_persona_map == '7 Cause Enthusiasts',]
thrill_seek  = subTrim[subTrim$cat_score_p2p_persona_map == '8 Thrill Seeker',]
avg_joe  = subTrim[subTrim$cat_score_p2p_persona_map == 'Average Joes',]

ggplot(subTrim, aes(y=amt_lifetime_giving_log, x=cat_score_p2p_persona_map))+
  stat_summary_bin(fun='mean', geom='bar', bins=20) + ylab("Lifetime Giving($)")+
  xlab("Persona") + ggtitle("Persona vs LTG")

subTrim %>% count(cat_score_p2p_persona_map, sort=T)

personas = c('go_getter', 'caring_contri', 'causal_contri',
             'do_good', 'gen_joe', 'over_achieve', 'cause_enthus', 'thrill_seek', 'avg_joe')

par(mfrow = c(3, 3))
for (i in 1:10){
  ggplot(personas[i], aes(y=amt_lifetime_giving_log, x=val_demo_age))+
  stat_summary_bin(fun='mean', geom='bar', bins=20) + ylab("Lifetime Giving($)")+
  xlab("age") + ggtitle("Age vs LTG")
}



temp = personas[1]
ggplot(personas[1], aes(y=amt_lifetime_giving_log, x=val_demo_age))+
  stat_summary_bin(fun='mean', geom='bar', bins=20) + ylab("Lifetime Giving($)")+
  xlab("age") + ggtitle("Age vs LTG")


