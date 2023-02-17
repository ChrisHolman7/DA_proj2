# Putting things together

# As of now, this requires eda file to have ran already so data is in environment.

# TODO: I still need to change decile into factor

# -----------------------------------------------------------------------------
# Chris Data

chrisNames = c("cat_demo_political_affiliation",
               "cat_calc_social_score", "val_donor_education_charities", 
               "val_donor_private_foundation", "amt_ta_income", "amt_ta_discretionaryspending",                        
               "amt_ta_discretionaryspending_philanthropy", "amt_ta_networth",                       
               "amt_ta_investments_savings", "amt_ta_investments_savings_bonds",
               "pct_pop_some_college" , "pct_pop_associate_degree", "pct_pop_bachelor_degree",                         
               "pct_pop_graduate_degree", "pct_pop_in_preschool", "Ppct_pop_in_elementary",                          
               "pct_pop_in_high_school", "pct_pop_in_college", "pct_pop_in_grad_school",                                                             
               "pct_pop_in_public_school", "pct_pop_in_private_school", 
               "amt_tran_total_dollars_purchase", "amt_tran_avg_dollar_purchase",                   
               "n_tran_credit_card_purchase")

logFix = c("amt_ta_income", "amt_ta_discretionaryspending",                        
           "amt_ta_discretionaryspending_philanthropy",
           "pct_pop_in_college", "pct_pop_in_grad_school", 
           "pct_pop_in_private_school", "amt_tran_total_dollars_purchase", 
           "amt_tran_avg_dollar_purchase", "n_tran_credit_card_purchase")

chrisCut = subRelRmv %>% select(all_of(chrisNames))

# Doing transformations

# Have to take out net worth for a sec because there are negatives
chrisCut = chrisCut %>% mutate(across(logFix, function(x) log(x+1)))

# This has several hundred large amounts in the negatives, so I'm going to use
# z-scores and then do a log transform
worthMean = mean(chrisCut$amt_ta_networth, na.rm=T)
worthSd = sd(chrisCut$amt_ta_networth, na.rm=T)
hist(log((chrisCut$amt_ta_networth - worthMean)/worthSd+1))
chrisCut$amt_ta_networth = log((chrisCut$amt_ta_networth - worthMean)/worthSd+1)

chrisCut$amt_ta_investments_savings_bonds = 
  as.factor(chrisCut$amt_ta_investments_savings_bonds)

# Chris data is transformed
# -----------------------------------------------------------------------------




# -----------------------------------------------------------------------------
# Irfan Data

# Consider adding occupation back in if we re-categorize?

irNamesNot = c("cat_demo_date_of_birth","cat_demo_marital_status",
            "cat_demo_person_type","cat_financial_mortgage_remainder_amount",
            "cat_demo_occupation","ind_lifestyle_cont_animal",
            "ind_lifestyle_cont_child_welfare","ind_lifestyle_cont_conspoli",
            "ind_lifestyle_cont_culture","ind_lifestyle_cont_environment",
            "ind_lifestyle_cont_health","ind_lifestyle_cont_libpol",
            "ind_lifestyle_cont_political","ind_lifestyle_cont_religion",
            "ind_lifestyle_cont_social_services","ind_lifestyle_cause_volunteer")

irCut = subRelRmv[1:50] %>% select(-all_of(irNames))

names(irCut)


for (i in 1:ncol(irCut)) {
  if (class(irCut[,i]) == "factor") {
    barplot(table(irCut[,i]), main=paste('Feature', i))
  } else {
    hist(irCut[,i], main=paste('Feature', i))
  }
}

# I think we should also remove: dwelling size

logChange = c("amt_financial_assessed_home_value",
              "n_demo_length_of_residence", 
              "amt_financial_estimated_available_equity",
              "amt_financial_estimated_monthly_mortgage")

irCut = irCut %>% mutate(across(logChange, function(x) log(x)))


# Indicator variables with x% of data in a single category (x >= 95). I feel like
# it's hard to make the case for including them. Maybe if they're super correlated
# with ltg

# 98% money market, real estate, libpoli
# 97% for iras
# 96% priv comp, conspoli, mail response, multi buyer
# 95% humanitarian

# Looking at plots, they're not correlated much at all with ltg, nor are they
# distributed in an interesting way. I think we take them out, with the 
# understanding that if some type of "assets" or politics variable is important, 
# we could potentially use these to differentiate more.

irCut = irCut %>% select(-c("cat_demo_dwelling_size", "ind_investments_money_market",
            "ind_investments_real_estate", "ind_lifestyle_cause_libpoli",
            "ind_investments_iras", "ind_demo_private_company_ownership",
            "ind_lifestyle_cause_conspoli", "ind_purchase_mail_responsive_buyer",
            "ind_purchase_dm_multi_buyer", "ind_lifestyle_cont_humanitarian"))

# Irfan data done
# -----------------------------------------------------------------------------



# -----------------------------------------------------------------------------
# Add together

subTrim = cbind(myCut2, chrisCut, irCut)

# We can see there are only 4 correlated above 0.8, that's not that bad actually
corr_simple(subTrim, 0.7)

