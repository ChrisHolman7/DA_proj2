
load("~/school/Spring2023/Stat4911/osuAdvcProj/full_adv_environment_Feb23.RData")

library(tidyverse)
library(randomForest)
library(ggplot2)

adv_sub = adv
rm(adv)

nullToNA = function(dataName) {
  return(replace(dataName, dataName=='null', NA))
}

sub3 = adv
for (i in 1:ncol(adv)) {
  if (is.character(adv[,i]))
    sub3[,i] = nullToNA(adv[,i])
}





# Test 

subRelRmv = sub3 %>% select(-"Not_in_use")
nNames = names(subRelRmv %>% select(starts_with("n")))
catNames = names(subRelRmv %>% select(starts_with("cat")))
valNames = names(subRelRmv %>% select(starts_with("val") & !ends_with("map")))
mapNames = names(subRelRmv %>% select(ends_with("map")))
amtNames = names(subRelRmv %>% select(starts_with("amt")))
indNames = names(subRelRmv %>% select(starts_with("ind")))
pctNames = names(subRelRmv %>% select(starts_with("p")))


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

subRelRmv$FY_DONOR_GIFT_FIRST = as.numeric(subRelRmv$FY_DONOR_GIFT_FIRST)

subRelRmv = subRelRmv %>% select(-"n_tran_credit_card_purchase")


subRelRmv = subRelRmv %>% select(-c("cat_score_donor_persona_map",
                             "val_score_direct_marketing_score_map",
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
                             "val_score_philanthropic_score_map"))

badNames = names((sort(colSums(is.na(subRelRmv))/nrow(subRelRmv) > 
                         0.5))[(sort(colSums(is.na(subRelRmv))/nrow(subRelRmv) > 0.5))])
mostVars = (na.omit(subRelRmv %>% select(-all_of(badNames))))


################################################################################

levels(mostVars$HomeState)[levels(mostVars$HomeState) %in% c("AA", "AE", "AP")] = "Armed Forces"
levels(mostVars$HomeState)[levels(mostVars$HomeState) %in%
                            c("PR", "GU", "MP", "VI")] = "Territory"
levels(mostVars$cat_score_p2p_persona_map)[levels(mostVars$cat_score_p2p_persona_map) 
                                          == "Average Joes"] = "9 Average Joes"

mostVars$HomeState = droplevels(mostVars$HomeState, exclude = "AB")

mostVars = mostVars %>% select(-"HomePostCode")


# Also going to remove the other persona var because there's no point 
mostVars = mostVars %>% select(-"cat_score_donor_persona")

mostVars = mostVars %>% select(-"cat_calc_mosaic")


# Date of birth
mostVars$cat_demo_date_of_birth = as.numeric(substr(
  as.numeric(as.character(mostVars$cat_demo_date_of_birth)), 1, 4))



subTrimRF2 = mostVars





################################################################################
#RF MODELING

rf2 <- randomForest(cat_score_p2p_persona_map ~ .,
                   data=subTrimRF2, importance=T,
                   ntree=100, do.trace=1)

sort(abs(importance(rf2)[,8]), decreasing=T)


# Class error: full set with max nodes (original err)
# Cat 1: 29.4 (64.2)
# Cat 2: 19.8 (32.3)
# Cat 3: 32.5 (64.8)
# Cat 4: 40.0 (92.4)
# Cat 5: 36.8 (68.4)
# Cat 6: 30.6 (43.3)
# Cat 7: 46.2 (95.6)
# Cat 8: 20.4 (24.8)
# Cat 9: 53.6 (94.2)


# Importances:
# Cat 1: homeState (34.2), mortgage remainder, p2p diy, direct marketing, home val (27.9), 
#   lifetime giving, first gift, kindergarten, grad school, public school (24.8)

# Cat 2: pct pop public school (56.4), pctpop private school, pctpop kindergarten, 
#   pctpop preschool, homestate (50.5), mortgage, high school, number of adults, elementary,
#   first gift (45.7)

# Cat 3: mortgage remainder (58.0), homestate, pctpop kindergarten, pctpop preschool, 
#   discretionary spending entertainment (45.8), length res, first gift, private school, 
#   number of adults, direct marketing (40.9)

# Cat 4: pctpop public school (48.6), mortgage, preschool, kindergarten, high school (42.7),
#   number of adults, discretionary spending ent, length res, equity, online score (35.8)

# Cat 5: mortgage (61.2), public school, preschool, high school, equity (48.6), elementary,
#   kindergarten, number adults, political persona, number children***

# Cat 6: mortgage (85.9), equity, political persona, first gift***, preschool (49.9), 
#   kindergarten, homestate, private school, length res, disc spend international travel (48.0)

# Cat 7: mortgage (70.6), length of residence, equity, homestate, political persona (38.0), 
#   first gift, lifetime giving, age, num adults, telemarketing (35.8)

# Cat 8: mortgage (63.2), equity, public school, elementary, high school (38.6), 
#   direct marketing, homestate, p2p diy, avg dollar purchase, lifetime giving (35.7)

# Cat 9: mortgage (60.1), avg dollar purchase, length residence, direct marketing, 
#   telemarketing (34.7), p2p event, p2p diy, homestate, disc spend domestic travel, 
#   first gift (31.2)

# Overall accuracy: estimated equity (86.9), pct pop preschool, pct pop high school, 
#   political persona, mortgage remainder (76.1), num adults, public school, kindergarten,
#   length res, elementary (62.0)

# overall gini: giving tuesday (14356.2), p2p diy, telemarketing, p2p event, 
#   home value (10201.8), online, mortgage, sustainer, philanthropic, high school grad (8131.8)

# Very interesting: gender was negative on ALL of them, suggesting gender doesn't help at all

################################################################################
# Looking at vars

# Variables to look at:

# Equity - Important for 4-8 and overall accuracy 
#   Honestly equity just goes down as expected -- just general prediction here I think

# pct preschool - Important for 2-6 and overall accuracy
#   Not much here tbh

# pct high school - Important for 2,4,5,8 and overal acc
#   2,4,5,8 seem to have slightly less varied distributions, but same means

# pct grad school - Only important for 1 ***
#   Highest mean and generally higher, but not super significant 

# elementary - Important for 2,5,8 and overall acc
#   Not much. Basically all are exact same??

# public school - Important for 1,2,4,5,8, and overall acc
#   These all seem to be *slightly* lower, but it's like 0.2 vs 0.21-0.22

# kindergarten - Important for 1-6 and overall acc
#   1 and 2 are lower than others (still not sig tbh), but 3-6 are same as others

# high school grad - Important for overall gini
#   This is very different: 1 is lowest; 1-3 very low (~0.15), 6-7 next (~0.2), then 
#   4,5,8, and 9 (0.3-0.4)

# private school - Important for 2,3, and 6
#   Maybe these are just normal ones? Because 1 is higher and 4,5,8 are low

# political persona - Important for 5-7 and overall acc
#   5-7 have larger political 10s 

# mortgage - General money trend

# number of adults - Imp for 2-5 and 7. No patten on important ones, but there is pattern. 
#   4-9 all same ditribution. 2-3 same median but wider. 1 is only one with median 4 and not 3

# length of residence - Imp for 3,4,6,7,9. Low for all of these except 4, which is highest

# giving Tuesday - Only for gini. General trend for scores, where 3 under performs

# p2p diy - Imp for 1,8,9, and gini. Just 1 most and 8/9 least

# telemarketing - Important for 7, 9, and gini. 7 and 9 have lowest scores here

# p2p event - Important for 9 and overall gini: 9 (and 8) lowest scorers

# home value - Important for 1 and gini: Same trend as all money, 1 easily most

# online score - Important for 4 and overall gini: 4 has easily the largest variance,
#   but relatively in the middle

# sustainer score - Same as below

# philanthropic score - Only important for overall gini, but we see big diff. 3 is much lower
#   than typical trends. Goes 4, 2, 1, 5, 3, 6-9

# homestate - Hard to look at and don't see much; since it's important for so many cats, 
#   I think it's one without an overall trend that just helped prediction lower in the tree
#   since it has so many splits

# direct marketing - Important for 1,3,8,9
#   1-4 high, 7-9 particularly low

# lifetime giving - General trend

# first gift - All very similar

# discretionary spending entertainment - Same general trend in spending. 3 is imp and second most

# number of children - Don't really see the difference

# discretionary spending international travel - Same trend as other spending

# age - Important for 7, which is noticeably the lowest age (but only by a couple years)

# avg dollar purchase - Same as below

# disc spending domestic travel - Lowest for 4,5,8,9. Just follows general trend


boxplot(subTrimRF2$amt_financial_assessed_home_value~
          subTrimRF2$cat_score_p2p_persona_map, outline=F,notch=T)

barplot(table(subTrimRF2 %>% filter(cat_calc_political_persona == "03") %>% 
                select(cat_score_p2p_persona_map))/table(subTrimRF2$cat_score_p2p_persona_map))


for (ind in c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10")) {
  
  barplot(table(subTrimRF2 %>% filter(cat_calc_political_persona == ind) %>% 
                select(cat_score_p2p_persona_map))/table(subTrimRF2$cat_score_p2p_persona_map),
          main = paste("Political Persona", ind))
}

barplot(table(subTrimRF2 %>% filter(cat_calc_political_persona %in% c("08","09", "10")) %>% 
                select(cat_score_p2p_persona_map))/table(subTrimRF2$cat_score_p2p_persona_map))

# Political tendancies by p2p category

# Percent of each 

# Cat 1: 
#   Are: On-the-fence-liberals (mean 10% higher than av), Mild republicans (8% higher) 
#     super democrats (4% higher), 
#   Not: Ultraconservative (13% lower), conservative democrats (6% lower)

# Cat 2: 
#   Are: mild republicans  (9% higher)
#   Not: Ultraconservative (5% lower)

# Cat 3: 
#   Are: Mild republicans (11% higher)
#   Not: Ultraconservatives (8% lower), conservative democrats (4% lower)

# Cat 4:
#   Are: Ultraconservatives (6% higher)

# Cat 5: 
#   Are: Ultraconservatives (11% higher)
#   Not: Mild Republicans (8% lower)

# Cat 6: Even split

# Cat 7: 
#   Not: Ultraconservatives (6% lower)

# Cat 8:
#   Are: Conservative democrats (6% higher) and ultraconservatives (10% higher)
#   Not: Mild Republicans (14% lower)

# Cat 9:
#   Are: Left out democrats (5% higher) and conservative democrats (5% higher)
#   Not: Mild republicans (10% lower)



for (ind in c("1 Go Getters", "2 Caring Contributors", "3 Casual Contributors", "4 Do Gooders",
              "5 Generous Joes", "6 Over Achievers", "7 Cause Enthusiasts", "8 Thrill Seekers",
              "9 Average Joes")) {
  
  barplot(table(subTrimRF2 %>% filter(cat_score_p2p_persona_map == ind) %>% 
                  select(cat_calc_political_persona))/table(subTrimRF2$cat_calc_political_persona),
          main = paste("", ind))
}

table(subTrimRF2$cat_calc_political_persona) / nrow(subTrimRF2)


for (ind in c("1 Go Getters", "2 Caring Contributors", "3 Casual Contributors", "4 Do Gooders",
              "5 Generous Joes", "6 Over Achievers", "7 Cause Enthusiasts", "8 Thrill Seekers",
              "9 Average Joes")) {
  print(paste("Table", ind))
  print((table(subTrimRF2 %>% filter(cat_score_p2p_persona_map == ind) %>% 
        select(cat_calc_political_persona))/sum(table(subTrimRF2 %>% 
                                          filter(cat_score_p2p_persona_map == ind) %>% 
                                          select(cat_calc_political_persona))) -
    table(subTrimRF2$cat_calc_political_persona) / nrow(subTrimRF2)) * 100) 
}

# Just helpful 
aggregate(subTrimRF2$pct_pop_high_school_grad, 
          list(subTrimRF2$cat_score_p2p_persona_map), mean)




