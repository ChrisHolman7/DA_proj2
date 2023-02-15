# plan
# remove 13 rows that cameron removed in file
# look over 50-98
# check for map vs normal and get idea of ones that seem useful
# for cat vars check differecne in means
# for cont vars try looking for another var to behave as y
#   maybe the hist could have something? (bi modal)
#check proportions of indicator?
# google unsupervised exa


#data subsets
#adv_sub - subset from metzger
#sub2 - changing 'null' to NA
#sub3 - removed columns with no use
#sub4 - my subset of data (50-98) 

#class subset
#load('C:/Users/chris/OneDrive/Desktop/Capstone/OOA_Proj/subset_adv_environment.RData')

#new subset
#load('C:/Users/chris/OneDrive/Desktop/Capstone/OOA_Proj/subset_adv_ltg_environment.RData')

adv_sub = adv_sub_ltg

#libraries
library(mice)
library(dplyr)
library(ggplot2)
library(ggcorrplot)

#assessed home value

#just converts "null" to NA
nullToNA <- function(dataName){
  return(replace(dataName, dataName=='null',NA)) 
}

#turns numbers as strings into numbers
factorize <- function(dataName){
  return(as.numeric(as.character(dataName))) 
}

#does both 
nullAndFactor <- function(dataName){
  temp <- nullToNA(dataName)
  return(factorize(temp))
}


sum(adv_sub$val_score_philanthropic_score_map == 'null')
adv_sub$val_score_philanthropic_score_bin = strtoi(substr(adv_sub$val_score_philanthropic_score_map, 0,1))


#lots of people dont wanna donate
#or at least
hist(nullToNA(adv_sub$val_score_philanthropic_score_bin))

sub2 = adv_sub
for (i in 1:ncol(adv_sub)){
  if (is.character(adv_sub[,i])){
    sub2[,i] = nullToNA(adv_sub[,i])
  }
}
(colMeans(is.na(sub2)))


sub3 <- sub2 %>% select(-c('Not_in_use',"TAS_ID",
                           "UniqueID", "SubmitDate", "Source", "_rescued_data", "HomeAddressStreet1",
                           "HomeAddressStreet2", "HomeAddressSystemID", "FirstName", "MiddleName", 
                           "LastName", "HomeCity"))




##TESTING VARS 50-98
##LOOKING FOR VARS THAT HAVE SEPARATION B/W CATEGORIES OR ACROSS DISTRIBUTION


tester = function(data, title = 'data'){
  data = factorize(data)
  hist(data, main = c('Histogram of',title))
}

##FIRST APPROACH
#RESULTS:
#political persona seems to have differing freqs (maybe could be good)
#   but needs cleaned (00,01,02 dont match rest of 03-10)
#amt_ta_spending vars could encode very similar info
#   look into this


#numbers were originally chars
sub3$ind_purchase_dm_multi_buyer = factorize(sub3$ind_purchase_dm_multi_buyer)

hist(sub3$ind_purchase_dm_multi_buyer)

#numbers were originally chars
sub3$n_purchase_mail_upscale_buyer = factorize(sub3$n_purchase_mail_upscale_buyer)

hist(sub3$n_purchase_mail_upscale_buyer)

#seems polical persona has differing freqs along cats
#needs cleaned (00,01,02 do not fit rest of data as political spectrum)
sub3$cat_calc_political_persona = factorize(sub3$cat_calc_political_persona)

hist(sub3$cat_calc_political_persona)


tester(sub3$cat_calc_social_score, 'social score')

tester(sub3$amt_pop_per_capita_income,'percap income')

#making these prettier^
hist(factorize(sub3$cat_calc_social_score),main='Histogram of Social Score', xlab='Social Score (higher = more presence on social media)')
hist(factorize(sub3$n_purchase_mail_upscale_buyer),main='Histogram of Upscale Mail Purchases', xlab='Number of Purchases')

#relationship b/w area percap income and philantropy (strong relationship)
sub3$amt_ta_discretionaryspending_philanthropy = factorize(sub3$amt_ta_discretionaryspending_philanthropy)
sub3$amt_pop_per_capita_income = factorize(sub3$amt_pop_per_capita_income)
ggplot(sub3, aes(x=amt_pop_per_capita_income, y=amt_ta_discretionaryspending_philanthropy))+
  stat_summary_bin(fun='mean', geom='bar', bins=20) + xlab("Per Capita Income ($)")+
  ylab("Philanthropic Spending ($)") + ggtitle("Philanthropic spending vs Per Capita Income")


#adding col105 (philantropic score from blackbaud to use as pseudo dependent var)
sub4 = sub3[,c(50:98,105)]
#str(sub4)
char_cols = sapply(sub4,is.character)
char_cols[c('cat_ta_total_identified_assets','cat_demo_political_affiliation')] = FALSE
sub4[,char_cols] = as.data.frame(apply(sub4[,char_cols],2,as.numeric))
#str(sub4)

#now sub4 is all fixed up (null to na and chars to num)

#new movers vs !new movers have similar ta_income
hist(sub4$ind_life_new_mover_12mos)

par(mfrow=c(1,2))
options(scipen=999)
hist(sub4[sub4$ind_life_new_mover_12mos == 1,]$val_score_philanthropic_score)
hist(sub4[sub4$ind_life_new_mover_12mos == 0,]$val_score_philanthropic_score)

sub4_cor = na.omit(sub4[,14:30])
corData = cor(sub4_cor)

ggcorrplot(corData,title="TA Spending Categories")

# md.pattern(sub2[c(1:3),c(1:10)], rotate.names = TRUE)

plot(sub4$amt_ta_income[sub4$amt_ta_income>1000000], sub4$amt_ta_discretionaryspending_philanthropy[sub4$amt_ta_income>1000000])

 colmeans = sort(colMeans(is.na(sub4)))
 colmeans

#tester(sub4$cat_demo_political_affiliation, 'test')

barplot(prop.table(table(sub4$cat_demo_political_affiliation)))


tester(sub4$val_donor_private_foundation, 'private dno')

  tester(sub4$val_donor_education_charities, 'edu charities')

tester(sub4$ind_life_new_mover_12mos, 'newmover12')

tester(sub4$ind_life_new_homeowner_12mos,'newhome12')

tester(sub4$cat_demo_dual_income, 'dual income')

tester(sub4$ind_purchase_dm_multi_buyer,'multi buyer')

tester(sub4$n_purchase_mail_upscale_buyer, 'upscale mail')

tester(sub4$cat_calc_political_persona, 'politics')

tester(sub4$val_life_grandchildren, 'grandchildren')

#IND PURCHASE DM MULTI BUYER
sub4$ind_purchase_dm_multi_buyer_bin = strtoi(substr(sub4$ind_purchase_dm_multi_buyer,0,1))
sub4 %>% group_by(sub4$ind_purchase_dm_multi_buyer_bin<1) %>% summarize(Percen=n()/nrow(.))

#figuring out grandchildren proportions (split at 50)
sub4$val_life_grandchildren_bin = strtoi(substr(sub4$val_life_grandchildren,0,1)) 
sub4 %>% group_by(sub4$val_life_grandchildren_bin<=5) %>% summarize(Percen=n()/nrow(.))
# deno = sum(temp[0:9,]$Percen)

# temp$Percen / deno


sub4 %>% group_by(sub4$val_donor_private_foundation <95) %>% summarize(Percen=n()/nrow(.))


#PRIVATE FOUNDATION
sub4$val_donor_private_foundation_bin = strtoi(substr(sub4$val_donor_private_foundation,0,1))
sub4 %>% group_by(sub4$val_donor_private_foundation_bin<5) %>% summarize(Percen=n()/nrow(.))


#EDUCATION CHARITIES
sub4$val_donor_education_charities_bin = strtoi(substr(sub4$val_donor_education_charities,0,1))
sub4 %>% group_by(sub4$val_donor_education_charities_bin<5) %>% summarize(Percen=n()/nrow(.))

#NEW MOVER 12 MOS
sub4$ind_life_new_mover_12mos_bin = strtoi(substr(sub4$ind_life_new_mover_12mos,0,1))
sub4 %>% group_by(sub4$ind_life_new_mover_12mos_bin<1) %>% summarize(Percen=n()/nrow(.))


#NEW HOMEOWNER 12 MOS
sub4$ind_life_new_homeowner_12mos_bin = strtoi(substr(sub4$ind_life_new_homeowner_12mos,0,1))
sub4 %>% group_by(sub4$ind_life_new_homeowner_12mos_bin<1) %>% summarize(Percen=n()/nrow(.))

#POLITICAL AFFLIATION
sub4 %>% group_by(sub4$cat_demo_political_affiliation) %>% summarize(Percen=n()/nrow(.))

#TOTAL ASSETS
sub4 %>% group_by(sub4$cat_ta_total_identified_assets) %>% summarize(Percen=n()/nrow(.))

#higher education
boxplot(sub4[,36:40], las=0, horizontal = TRUE)

#education
boxplot(sub4[,41:45], las=0, horizontal = TRUE)

options(scipen=5)
ggplot(sub4, aes(x=amt_ta_networth)) + geom_histogram(binwidth=1000000, colour="black", fill="white")

#investments
# boxplot(sub4[,23:24], horizontal = TRUE)
# boxplot(sub4[,25:26], horizontal = TRUE)





##PCA
#on finance
library(factoextra)
finance_pca = prcomp(na.omit(sub4[,23:30]), scale =TRUE)
fviz_eig(finance_pca)

fviz_pca_var(finance_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

finance_pca$rotation
#checking seems least impactful (not by much, no major finding)

#on population dist
pop_pca = prcomp(na.omit(sub4[,36:49]), scale =TRUE)
#took more PC to explain substantial variance
fviz_eig(pop_pca)

fviz_pca_var(pop_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

pop_pca$rotation
#kindergarten has lower contribution than the rest, maybe remove

par(mfrow=c(1,1))


##ADDING LTG TO SUB$
sub4$amt_lifetime_giving = adv_sub$amt_lifetime_giving

sum(complete.cases(sub4))


sub4_ltg_cor = na.omit(sub4)

ltg_cor4 = cor(sub4)

