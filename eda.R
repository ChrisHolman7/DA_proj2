
load("~/school/Spring2023/Stat4911/osuAdvcProj/subset_adv_ltg_environment.RData")

library(tidyverse)
library(mice)

adv_sub = adv_sub_ltg
rm(adv_sub_ltg)

nullToNA = function(dataName) {
  return(replace(dataName, dataName=='null', NA))
}

sub2 = adv_sub
for (i in 1:ncol(adv_sub)) {
  if (is.character(adv_sub[,i]))
    sub2[,i] = nullToNA(adv_sub[,i])
}





# Test 

subRelRmv = sub2 %>% select(-"Not_in_use")
nNames = names(subRelRmv %>% select(starts_with("n")))
catNames = names(subRelRmv %>% select(starts_with("cat")))
valNames = names(subRelRmv %>% select(starts_with("val") & !ends_with("map")))
mapNames = names(subRelRmv %>% select(ends_with("map")))
amtNames = names(subRelRmv %>% select(starts_with("amt")))
indNames = names(subRelRmv %>% select(starts_with("ind")))
pctNames = names(subRelRmv %>% select(starts_with("p")))

names(subRelRmv[!(names(subRelRmv) %in% 
                    c(nNames, catNames, valNames, amtNames, indNames, pctNames, mapNames))])


subRelRmv = subRelRmv %>% mutate_at(c(nNames,valNames, amtNames, pctNames), 
                                    as.numeric)
subRelRmv = subRelRmv %>% mutate_at(c(catNames, mapNames, indNames,
                                      "HomeState", "HomePostCode"), as.factor)

subRelRmv = subRelRmv %>% select(-c("TAS_ID",
                                    "UniqueID", "SubmitDate", "Source", "_rescued_data", "HomeAddressStreet1",
                                    "HomeAddressStreet2", "HomeAddressSystemID", "FirstName", "MiddleName", 
                                    "LastName", "HomeCity"))


subRelRmv$HomePostCode = as.character(subRelRmv$HomePostCode)



subRelRmv$HomePostCode = substr(subRelRmv$HomePostCode,1,
                                nchar(subRelRmv$HomePostCode)-5)

subRelRmv$HomePostCode = as.factor(subRelRmv$HomePostCode)
subRelRmv$val_pop_family_income_cbsa_decile = 
  as.factor(subRelRmv$val_pop_family_income_cbsa_decile)

subRelRmv$amt_lifetime_giving_log = log(subRelRmv$amt_lifetime_giving+1)



length(unique(subRelRmv$HomePostCode))


hist(as.numeric(as.character((subRelRmv$cat_demo_date_of_birth[as.numeric(as.character(subRelRmv$cat_demo_date_of_birth)) > 190000]))))

# -------------------------------------------------------------------------
# Looking at subset of variables for distribution and ideas of use

myCut = subRelRmv[99:ncol(subRelRmv)]

names(myCut)



sum(is.na(myCut$amt_pop_per_capita_income))/nrow(myCut) # 0.1% NAs!
hist(log(myCut$amt_pop_per_capita_income)) # wants log transform
# Probably a good index of general income -- could be correlated with location

hist(myCut$val_pop_ispsa_index) 
sum(is.na(myCut$val_pop_ispsa_index))/nrow(myCut) # 0.1%
# General indicator of status 


hist(myCut$val_pop_family_income_state_decile) 
sum(is.na(myCut$val_pop_family_income_state_decile))/nrow(myCut) # 0.1%
# Another indicator of income -- is relative income (i.e. decile per state 
# more important), or absolute. 
# -- Could use this mostly and fill in NAs
# somehow by combining with amt_pop_per_capita_income
# -- Should this be kept as numeric or factor? Obviously it wouldn't make sense
# to give decimal deciles, but we want to maintain order

hist(myCut$val_pop_family_income_cbsa_decile)
sum(is.na(myCut$val_pop_family_income_cbsa_decile))/nrow(myCut) # 0.1%
# Very similar to previous -- probably only want to include one
# State is possibly a little more skewed left

hist(log(myCut$val_pop_home_value_state_index))
sum(is.na(myCut$val_pop_home_value_state_index))/nrow(myCut) # 0.1%
# Log transform helpful
# Could be good indicator of wealth via house
# -- worth noting this is specify the house they live in, not rentals

hist(log(myCut$val_pop_home_value_cbsa_index))
sum(is.na(myCut$val_pop_family_income_cbsa_decile))/nrow(myCut) # 0.1%
# Very similar as state, just slightly less spread. Probably only need one 
# again, might favor state just for more spread

hist(myCut$val_score_philanthropic_score)
sum(is.na(myCut$val_score_philanthropic_score))/nrow(myCut) # 0.01%
# Is a general estimate of likelihood to donate. Would be good to know how
# this corresponds with actual donations

barplot(table(myCut$cat_score_donor_persona), main="Cat Score Donor Persona")
sum(is.na(myCut$cat_score_donor_persona))/nrow(myCut) # 4%

par(mar=c(10,6,4,1))
barplot(table(myCut$cat_score_donor_persona_map), 
        main="Cat Score Donor Persona Map", las=2)
sum(is.na(myCut$cat_score_donor_persona_map))/nrow(myCut) # 4%
# This is literally the same as the non-map version just with different words
# since both are categories. Definitely only keep one. Question is if we keep
# map, should we cut the number? I don't see why not

barplot(table(myCut$cat_score_direct_marketing_ask_array), las=2)
sum(is.na(myCut$cat_score_direct_marketing_ask_array))/nrow(myCut) # 0.01%
# Uhh, it's the "suggested ask amount"? What the heck does that mean. Where
# does it come from? Seems kinda horrible

hist(myCut$val_score_direct_marketing_score)
sum(is.na(myCut$val_score_direct_marketing_score))/nrow(myCut) # 0.01%
# Likelihood to make direct marketing gift. Do we want to use their own 
# likelihood predictions?

par(mar=c(12,6,4,1))
barplot(table(myCut$val_score_direct_marketing_score_map)/c(1,1,1,2,5),
        las=2, main="Direct Marketing Score")
sum(is.na(myCut$val_score_direct_marketing_score_map))/nrow(myCut) # 0.01%
# Vast majority are just average or below average, even when adjusted for 
# range of categories. I feel like this makes it worse than the non-map version


hist(myCut$val_score_telemarketing_score)
sum(is.na(myCut$val_score_telemarketing_score))/nrow(myCut) # 0.01%
# Center is much lower with more right skew -- most likelihood because 
# people are just generally unlikely to answer telemarketing calls. 
# -- Maybe check how this tracks with age -- I can't imagine many young people
# have a good score. 
# -- In general I can't imagine it would be great by itself, but it could
# be added to a general "likelihood to respond" score

par(mar=c(12,6,4,1))
barplot(table(myCut$val_score_telemarketing_score_map)/c(1,1,1,2,5),
        las=2, main="TeleMarketing Score")
sum(is.na(myCut$val_score_telemarketing_score_map))/nrow(myCut) # 0.01%
# Vast majority are just average or below average, even when adjusted for 
# range of categories. I feel like this makes it worse than the non-map version

hist(myCut$val_score_online_score)
sum(is.na(myCut$val_score_online_score))/nrow(myCut) # 0.01%
# Much more uniform aside from very high scores

barplot(table(myCut$val_score_online_score_map)/c(1,1,1,2,5),
        las=2, main="Online Score")
sum(is.na(myCut$val_score_telemarketing_score_map))/nrow(myCut) # 0.01%
# Below average --> good similar, trails off for very good and excellent


hist(myCut$val_score_sustainer_score)
sum(is.na(myCut$val_score_sustainer_score))/nrow(myCut) # 0.01%
# More skewed right again, similar to telemarketing score. 
# Predicted likelihood of becoming a sustainer donor, so could be useful if
# it's good at that, but how do we know?

barplot(table(myCut$val_score_sustainer_score_map)/c(1,1,1,2,5),
        las=2, main="Sustainer Score")
sum(is.na(myCut$val_score_sustainer_score_map))/nrow(myCut) # 0.01%
# Vast majority in below average, skewed


hist(myCut$val_score_giving_tuesday_score)
sum(is.na(myCut$val_score_giving_tuesday_score))/nrow(myCut) # 0.01%
# Nothing special

barplot(table(myCut$val_score_giving_tuesday_score_map)/c(1,1,1,2,5),
        las=2, main="Giving Tuesday Score")
sum(is.na(myCut$val_score_telemarketing_score_map))/nrow(myCut) # 0.01%
# Nothing Special


hist(myCut$val_score_end_of_year_score)
sum(is.na(myCut$val_score_end_of_year_score))/nrow(myCut) # 0.01%
# Nothing special

barplot(table(myCut$val_score_end_of_year_score_map)/c(1,1,1,2,5),
        las=2, main="End of Year Score")
sum(is.na(myCut$val_score_end_of_year_score_map))/nrow(myCut) # 0.01%
# Nothing Special


# Many of the previous (and maybe next depending on what p2p means) vars
# have potential to be combined into general propoensity to give score


hist(myCut$val_score_p2p_event_score)
sum(is.na(myCut$val_score_p2p_event_score))/nrow(myCut) # 0.01%
# Nothing special
# p2p organization driven event participation

barplot(table(myCut$val_score_p2p_event_score_map)/c(1,1,1,2,5),
        las=2, main="P2P Event Score")
sum(is.na(myCut$val_score_p2p_event_score_map))/nrow(myCut) # 0.01%
# Nothing Special


hist(myCut$val_score_p2p_diy_score)
sum(is.na(myCut$val_score_p2p_diy_score))/nrow(myCut) # 0.01%
# Much higher peak (500 rather than 200) and much more normal
# p2p **individual** driven event 
# -- Could indicate people who are more driven to donate on their own vs.
# event showing people who donate when they have to for work

barplot(table(myCut$val_score_p2p_diy_score_map)/c(1,1,1,2,5),
        las=2, main="P2P Individual Score")
sum(is.na(myCut$val_score_p2p_diy_score_map))/nrow(myCut) # 0.01%
# Most in average, good/below average very similar


barplot(table(myCut$cat_score_p2p_persona), las=2, main="P2P Persona")
sum(is.na(myCut$cat_score_p2p_persona))/nrow(myCut) # 0.01%
# Even-ish distribution

barplot(table(myCut$cat_score_p2p_persona_map),
        las=2, main="P2P Persona")
sum(is.na(myCut$cat_score_p2p_persona_map))/nrow(myCut) # 0.01%
# Like donor persona, they're the same variable just with different category
# names. Pick one, if either


barplot(table(myCut$cat_demo_gender_map),
        las=2, main="Gender")
sum(is.na(myCut$cat_demo_gender_map))/nrow(myCut) # 40%
# Gonna be the exact same as gender, so only need one. 40% NAs is very high,
# what can we do about that? 


barplot(table(myCut$cat_demo_marital_status_map),
        las=2, main="Marital Status")
sum(is.na(myCut$cat_demo_marital_status_map))/nrow(myCut) # 40%
# Same as marital status. Notably very few single people, but a decent amount
# unknown and still 40% NA


# Cut
barplot(table(myCut$cat_demo_person_type_map),
        las=2, main="Person Type")
sum(is.na(myCut$cat_demo_person_type_map))/nrow(myCut) # 40%
# Same as person type, only 4 of 7 cats used, 40% NA, almost all answers 
# primary decision maker, second most other/blank, and what is ranked basically
# just gives young vs middle aged vs elderly --- don't think either person type
# var will be helpful


barplot(table(myCut$cat_demo_dwelling_size_map),
        las=2, main="Dwelling Size")
sum(is.na(myCut$cat_demo_dwelling_size_map))/nrow(myCut) # 45%
# Basically all data is from single family detached unit (94% after NAs)
# Don't think it will be useful


barplot(table(myCut$cat_financial_mortgage_remainder_amount_map),
        las=2, main="Mortgage Remainder")
sum(is.na(myCut$cat_financial_mortgage_remainder_amount_map))/nrow(myCut) # 50%
# Good distribution, but half NA is bad. Will be same as non-map


# Cut
barplot(table(myCut$cat_financial_estimated_income_range_map),
        las=2, main="Estimated Income")
sum(is.na(myCut$cat_financial_estimated_income_range_map))/nrow(myCut) # 31%
# Good distribution, but 31% NA is bad. Will be same as non-map. As always, 
# estimated could be an issue


barplot(table(myCut$cat_demo_occupation_map),
        las=2, main="Occupation")
sum(is.na(myCut$cat_demo_occupation_map))/nrow(myCut) # 31%
# Almost everybody is unknown on top of the NAs. Consider how we could re-group
# these, otherwise probably not worth using


barplot(table(myCut$cat_demo_education_map),
        las=2, main="Education")
sum(is.na(myCut$cat_demo_education_map))/nrow(myCut) # 31%
# High NA but good variance


barplot(table(myCut$cat_calc_political_persona_map),
        las=2, main="Politics")
sum(is.na(myCut$cat_calc_political_persona_map))/nrow(myCut) # 42%
# Okay apart from NAs. Again, this is just predicted


barplot(table(myCut$cat_calc_social_score_map),
        las=2, main="Social Score")
sum(is.na(myCut$cat_calc_social_score_map))/nrow(myCut) # 50%
# Okay apart from NAs. Again, this is just predicted. Also, I wouldn't think 
# it matters much apart from correlation with age


barplot(table(myCut$cat_demo_dual_income_map),
        las=2, main="Dual Income")
sum(is.na(myCut$cat_demo_dual_income_map))/nrow(myCut) # 50%
# NAs. Single income vs dual income could be interesting -- probably DONT
# want to use map for this one, or at least change the names


barplot(table(myCut$cat_ta_total_identified_assets_map),
        las=2, main="Total Assests")
sum(is.na(myCut$cat_ta_total_identified_assets_map))/nrow(myCut) # 33%
# Not a ton of variation, but intuitively this could be a good variable, so
# consider further


barplot(table(myCut$cat_ta_wealth_segments_map),
        las=2, main="Wealth Segments")
sum(is.na(myCut$cat_ta_wealth_segments_map))/nrow(myCut) # 5%
# Nothing special



myCut2 = myCut %>% select(-c("val_pop_family_income_state_decile", 
"val_pop_home_value_state_index", "cat_score_donor_persona_map",
"cat_score_direct_marketing_ask_array", "val_score_direct_marketing_score_map",
"val_score_telemarketing_score_map", "val_score_online_score_map",
"val_score_sustainer_score_map", "val_score_giving_tuesday_score_map",
"val_score_end_of_year_score_map", "val_score_p2p_event_score_map", 
"val_score_p2p_diy_score_map", "cat_score_p2p_persona", "cat_demo_gender_map",
"cat_demo_marital_status_map", "cat_demo_person_type_map", 
"cat_demo_dwelling_size_map", "cat_financial_mortgage_remainder_amount_map",
"cat_financial_estimated_income_range_map", "cat_demo_occupation_map",
"cat_demo_education_map", "cat_calc_political_persona_map",
"cat_calc_social_score_map", "cat_ta_total_identified_assets_map",
"cat_ta_wealth_segments_map", "cat_demo_dual_income_map", 
"val_score_philanthropic_score_map", 'amt_lifetime_giving',
"amt_pop_per_capita_income", "val_pop_ispsa_index", "val_pop_home_value_cbsa_index"))


md.pattern(myCut2 %>% select(-"cat_score_donor_persona"))

# Removing cat_score_donor_persona makes NAs effectively clear, we can just omit other rows.
# Question is, do we impute?


corr_simple <- function(data,sig){
  
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  df_cor <- df_cor %>% mutate_if(is.integer, as.numeric)
  corr <- cor(df_cor, use="pairwise.complete.obs")
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  corr[corr == 1] <- NA 
  corr <- as.data.frame(as.table(corr))
  corr <- na.omit(corr) 
  corr <- subset(corr, abs(Freq) > sig) 
  corr <- corr[order(-abs(corr$Freq)),] 
  print(corr)
}

corTab = corr_simple(myCut2, 0.7)

# Philanthropic score correlated highly with end of year score and p2p event 
# score. 
# Going to get rid the other two, with the understanding that they are different 
# and if phil score ends up being good, we can look deeper into those

myCut2 = myCut2 %>% select(-c("val_score_end_of_year_score", 
                              "val_score_p2p_event_score"))



# Per capita income --> ispsa index
# family income cbsa decile --> home val cbsa 
# per capita income --> home value
# ispsa index --> family income decile
# per capita income --> income cbsa decile
# ispsa index --> home val

# per capita income cor with ltg: 0.21
# family income decile: 0.19
# ispsa index: 0.2
# home val:0.18

# Get rid of all but 3 or 2? We'll wait for donating data and see which is 
# highest correlated

myCut2 = myCut2 %>% select(-"val_score_giving_tuesday_score")




plot(as.integer(as.numeric(myCut2$cat_score_donor_persona)), 
     as.numeric(as.integer(myCut2$val_score_online_score)))

ggplot(myCut2, aes(x=cat_score_donor_persona, 
                   y=val_score_online_score))+
  stat_summary_bin(fun='mean', geom='bar', bins=20) 


ggplot(myCut2, aes(x=cat_score_donor_persona, 
                   y=val_score_online_score)) + geom_point(position="jitter")


# social score, cat_demo_political_affiliation

myCut2 = cbind(myCut2, 
          cat_demo_political_affiliation=adv_sub$cat_demo_political_affiliation,
          cat_calc_social_score=adv_sub$cat_calc_social_score)

# -----------
# impute

tempData = mice(myCut2, m=5, maxit=1, meth='pmm', seed=20)
summary(tempData)

completedData = complete(tempData, 3)

xyplot(tempData, amt_lifetime_giving~val_score_online_score+
         val_score_philanthropic_score, pch=18, cex=1)

densityplot(tempData)

# modelFit1 = with(tempData, kmeans(centers=6)) don't know how to do with
# clustering yet

# ----------------------

kept = names(myCut2)

#"amt_pop_per_capita_income"         "val_pop_ispsa_index"              
# "val_pop_family_income_cbsa_decile" "val_pop_home_value_cbsa_index"    
# "val_score_philanthropic_score"     "cat_score_donor_persona"          
# "val_score_direct_marketing_score"  "val_score_telemarketing_score"    
# "val_score_online_score"            "val_score_sustainer_score"        
# "val_score_giving_tuesday_score"    "val_score_end_of_year_score"      
# "val_score_p2p_event_score"         "val_score_p2p_diy_score"          
# "cat_score_p2p_persona_map"         "amt_lifetime_giving_log" 

testSet = subRelRmv %>% select(all_of(kept))


