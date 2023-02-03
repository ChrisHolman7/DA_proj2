load("~/school/Spring2023/Stat4911/osuAdvcProj/subset_adv_environment.RData")

library(tidyverse)



# Ideas
----------------------------------------------------------------------------
# Get rid of TAS_ID
# Not_in_use
# HomeAdressStreet2
# UniqueID
# SubmitDate
# Source
# _rescued_data
# Names
# HomeCity


# How do we use homeAddress? Just zipcode instead? 
# Idea: do analysis without (since it's hard to keep locality), but can use
# once clusters achieved to see if there is a pattern

  
# Remove last 4 digits of zip code


# cat_demo_date_of_birth -- change into date? Find someway to encode that 
# January is close to December. Only need year, no month?

# cat_demo_gender -- 40% are null
# Marital status as well
# Person_type -- also kinda sucks? 40% null and 45% P

# HomeOwner -- 3% aren't home owners?

----------------------------------------------------------------------------

# col 39 not in use -- remove. Also called not_in_use for ease
  
# 67% of states are Ohio
sum(adv_sub$HomeState == "OH") / nrow(adv_sub)



# Doesn't actually do anything
# 
# # I want to look at homeowners
# 
# adv_sub$ind_demo_home_owner = as.factor(as.character(adv_sub$ind_demo_home_owner))
# adv_sub$cat_demo_date_of_birth = as.factor(as.character(adv_sub$cat_demo_date_of_birth))
# adv_sub$ind_demo_home_owner[adv_sub$ind_demo_home_owner=="null"] = NA
# adv_sub$cat_demo_date_of_birth[adv_sub$ind_demo_home_owner=="null"] = NA
# 
# hist(adv_sub$cat_demo_date_of_birth[adv_sub$ind_demo_home_owner==1])



# Working on Chris' thing

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


subRelRmv = subRelRmv %>% mutate_at(c(nNames,valNames, amtNames, pctNames), as.numeric)
subRelRmv = subRelRmv %>% mutate_at(c(catNames, mapNames, indNames,
                    "HomeState", "HomePostCode"), as.factor)

subRelRmv = subRelRmv %>% select(-c("TAS_ID",
"UniqueID", "SubmitDate", "Source", "_rescued_data", "HomeAddressStreet1",
"HomeAddressStreet2", "HomeAddressSystemID", "FirstName", "MiddleName", 
"LastName", "HomeCity"))

sapply(subRelRmv, class)



# Doesn't work yet because of NAs

corr_simple <- function(data,sig=0.5){
  
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  df_cor <- df_cor %>% mutate_if(is.integer, as.numeric)
  corr <- cor(df_cor)
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  corr[corr == 1] <- NA 
  corr <- as.data.frame(as.table(corr))
  corr <- na.omit(corr) 
  corr <- subset(corr, abs(Freq) > sig) 
  corr <- corr[order(-abs(corr$Freq)),] 
  print(corr)
}

corr_simple(subRelRmv)

subRelRmv$HomePostCode = as.character(subRelRmv$HomePostCode)



subRelRmv$HomePostCode = substr(subRelRmv$HomePostCode,1,
                                nchar(subRelRmv$HomePostCode)-5)

subRelRmv$HomePostCode = as.factor(subRelRmv$HomePostCode)

length(unique(subRelRmv$HomePostCode))
