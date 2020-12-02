# Libraries neeeded for analysis
library(stringr)
library(rvest)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(rvest)
library(DataCombine)
library(tidyr)
library(pmdplyr)
library(plm)
library(RgoogleMaps)
library(naniar)
library(readr)
library(readxl)
library(lubridate)
library(magrittr)
library(filesstrings)
library(gt)
library(tidyverse)
library(glue) 
library(formattable)
library(glue)
library(dplyr)
library(epiDisplay)
library(Hmisc)
library(rJava)
library(xlsx)
library(tm)
library(doBy)
library(data.table)
library(plyr)

# Load Data
hhgen <- read_excel("C:/Users/S.Muslimwal/OneDrive/1. AIRD Staff/1. Onging Projects/2020/3. WEE-RDP Baseline for IE/6. Data Processing and Data Quality Assurance/1. Up to date Data/Data-22-nov-2020.xlsx", 
                    sheet = "General_Survey_main")

#Number of HHs in each CDC should be exactly 10
no_hh_in_each_cdc<- hhgen %>% 
  group_by(CDC) %>% 
  dplyr::summarise(no_hh_in_each_cdc=n())
hhgen<- hhgen %>% 
  left_join(no_hh_in_each_cdc, by="CDC")
rm(no_hh_in_each_cdc)

#export dataset with hhs/cdc problem
hhgen %>% 
  filter(no_hh_in_each_cdc>10) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"no_hhs_in_cdc_more_than_10.csv"))
hhgen %>% 
  filter(no_hh_in_each_cdc<10) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"no_hhs_in_cdc_less_than_10.csv"))

#duplicate HHs in each CDC
#adding HHs serial number variable
hhgen$hh_serial_gen<- paste(hhgen$province, hhgen$district, hhgen$CDC, hhgen$`S1#4serial`, sep = "-")
dups<- hhgen %>% 
  group_by(hh_serial_gen) %>% 
  dplyr::summarise(dups=n())
hhgen <- hhgen %>% 
  left_join(dups, by="hh_serial_gen")
rm(dups)

#export datasets with duplicate hhs problem
hhgen %>% 
  filter(dups>1) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"duplicate_hhs_in_CDC_probs.csv"))

#1. Strategy 2.1 Interveiw time ----
hhgen<- hhgen %>% 
  mutate(starttime_1=format(hhgen$starttime,"%H:%M:%S"))
hhgen<- hhgen %>% 
  mutate(endtime_1=format(hhgen$endtime,"%H:%M:%S"))
hhgen$starttime_1<- as.POSIXlt(hhgen$starttime_1,format="%H:%M:%S")
hhgen$endtime_1<- as.POSIXlt(hhgen$endtime_1, format="%H:%M:%S")

hhgen<- hhgen %>% 
  mutate(timedif=(as.numeric(endtime_1-starttime_1))-4.5)
hhgen$timedif<- hhgen$timedif*60
hhgen<- hhgen %>% #interviews less than 60 mins
  mutate(less_than_60mins=as.numeric(timedif<60 | timedif<0))
hhgen<- hhgen %>% #interviews more than 90 mins
  mutate(more_than_120_mins=as.numeric(timedif>120))

#List of Enumerators with problem of survey time
lesstime_enum<- hhgen %>% 
  group_by(enu_first) %>% 
  dplyr::summarise(int_time_less_than_60mins=((sum(less_than_60mins))/n())*100)
moretime_enum<- hhgen %>% 
  group_by(enu_first) %>% 
  dplyr::summarise(int_time_more_than_120_mins=((sum(more_than_120_mins))/n())*100,
                   average_int_time=mean(timedif))
time_probs_enum<- lesstime_enum %>% 
  full_join(moretime_enum, by="enu_first")
rm(lesstime_enum, moretime_enum)

#2.strategy 1.2 Missing Values----
#a. Refused and I don't knows 
hhgen <- hhgen %>% 
  mutate(idk=rowSums(hhgen==98, na.rm = T))
hhgen <- hhgen %>% 
  mutate(refusal=rowSums(hhgen==99, na.rm = T))
hhgen<- hhgen %>% 
  mutate(totidk_ref=(idk+refusal))
#add variable for refusal and IDK problems in data set
hhgen<- hhgen %>% 
  mutate(idk_refusal_prb=as.numeric(totidk_ref>4))
#enumerators with idks and refusal
idk_refusal_enum<- hhgen %>% 
  group_by(enu_first) %>% 
  dplyr::summarise(number_forms_with_idk_ref=sum(idk_refusal_prb))

#b.finding missing variablles(NAs) for incomplete surveys
hhgen<- hhgen %>%
  mutate(missings=apply(hhgen, 1, function(x) sum(is.na(x))))
hhgen$missings_prob<- as.numeric(hhgen$missings>220)
hhgen %>% 
  filter(hhgen$missings_prob==1) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"HHs_gen_missings_problem.csv"))


#c. Questionnaires with too much "No" Answers
hhgen<- hhgen %>%
  mutate(nops=apply(hhgen, 1, function(x)sum(x == 2, na.rm = T)))
hhgen<- hhgen %>% 
  mutate(nops_prob=as.numeric(nops>26))#on average the number of Nos is 26 in each form
too_much_Nos<- hhgen[hhgen$nops_prob,] 
#save datasets with too much Nos
write_excel_csv(too_much_Nos, paste(gsub(":", "-", Sys.Date()),"HHs_Too_Much_Nos.csv"))
#enumerators with too much NOs 
too_much_nos_enum<- too_much_Nos %>% 
  group_by(enu_first) %>% 
  dplyr::summarise(No_of_Too_much_NO_Forms=n())
rm(too_much_Nos)

#3. startegy 3.3 inconsistency in answers ----
#a. checking section 5 (assets)
#adding variable of section 5 problems at the end of datasets
sec5<- hhgen[ , grepl( "start-eligible-sec5group" , names( hhgen ) ) ]
sec5_now<- sec5[, grepl("A", names(sec5))]
sec5_now<- sec5_now %>%
  mutate(no_assets=apply(sec5_now, 1, function(x)sum(x == 0, na.rm = T)))
sec5_now$no_assets<- (sec5_now$no_assets/17)*100
#calculating the standard deviation of all assets to see the deviation of number of each assets
sec5_now$sd_assets<- apply(sec5_now, 1, sd, na.rm=T)
sec5_1year<- sec5[, grepl("tot", names(sec5))]
sec5_1year<- sec5_1year %>%
  mutate(no_assets=apply(sec5_1year, 1, function(x)sum(x == 0, na.rm = T)))
sec5_1year$no_assets<- (sec5_1year$no_assets/17)*100
#calculating the standard deviation of all assets to see the deviation of number of each assets
sec5_1year$sd_assets<- apply(sec5_1year, 1, sd, na.rm=T) 

hhgen$no_assets_prob<- as.numeric(sec5_now$no_assets>90 | sec5_1year$no_assets>90)
# in the following if the number of each assets are very same (10 cows, 10 bikes, 10 solars) it is a problem, or if there are too much of each an asset
hhgen$sd_assets_prob<- as.numeric(sec5_now$sd_assets>25 | sec5_now$sd_assets<10 |
                                    sec5_1year$sd_assets>25 | sec5_1year$sd_assets<10)
#Dataset with problems in sect 5 
hhgen %>% 
  filter(hhgen$no_assets_prob==1 | hhgen$sd_in_asset_prob==1) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"HHs_problem_section5.csv"))
#enumerators with problems in sec5
sec5_prob_enum<- hhgen %>% 
  group_by(enu_first) %>% 
  dplyr::summarise(no_assets_problem=sum(hhgen$no_assets_prob), 
                   sd_asset_problem=sum(hhgen$sd_in_asset_prob))
rm(sec5,j, sec5_1year, sec5_now, tot_asset_1year, tot_asset_now)

#b. HH size 
#adding variable of HH size problem at the end of the dataset
hhgen<- hhgen %>% 
  mutate(hhsize_prob=as.numeric(`start-eligible-sec2group-S2hhcount`<3))
#exporting data_set with HH size problem 
write_excel_csv(hhgen[hhgen$hhsize_prob==1,], paste(gsub(":", "-", Sys.Date()),"HHs_problem_HH_size.csv"))

#c. If very difficult in 7.A the amount should be low
hhgen$Percent_able_pay_emergency_savings<-(hhgen$`start-eligible-sec7group-borrowsourcer-S7.2Bspend`/hhgen$`start-eligible-sec7group-G7.2rand_val`)*100
hhgen$Percent_able_pay_emergency_savings_prob <- as.numeric(hhgen$`start-eligible-sec7group-S7.2Adiff`==4 &                                                                hhgen$Percent_able_pay_emergency_savings>80)
write_excel_csv(hhgen[hhgen$hhsize_prob==1,], paste(gsub(":", "-", Sys.Date()),"HHs_ability_pay_emergency_savings.csv"))

#4. strategy 1.4 Outliers ----
# a. too high income
# Generating total income variable
hhgen$avgincome<- (hhgen$`start-eligible-sec4group-annulincome-S4.9incomeBmax`+
                     hhgen$`start-eligible-sec4group-annulincome-S4.9incomeBmin`)/2
hhgen$total_income <- with(hhgen, ifelse(hhgen$`start-eligible-sec4group-annulincome-S4.9incomeA`==2,
                                         hhgen$avgincome, hhgen$`start-eligible-sec4group-annulincome-S4.9incomeB1`))
hhgen$avgincome<- NULL
#Not (Creteria for too much income is not set yet)

# b. too much savings
#caclulating total savings variable
total_savings<- hhgen[ , grepl( "start-eligible-sec6group" , names( hhgen ) ) ]
total_savings_1<- total_savings[, grepl("B1", names(total_savings))]
total_savings_2<- total_savings[, grepl("Bmin", names(total_savings))]
total_savings_3<- total_savings[, grepl("Bmax", names(total_savings))]
total_savings_4<- sum(total_savings_2, total_savings_3)
total_savings_5<- rowSums(total_savings_1, na.rm = T)
total_savings_6<- rowSums(total_savings_2, na.rm = T)
total_savings_7<- rowSums(total_savings_3, na.rm = T)
total_savings_8<- as.data.frame(cbind(total_savings_6,total_savings_7))
total_savings_9<- rowMeans(total_savings_8)
gtotal_savings<- total_savings_9+total_savings_5
hhgen$total_savings<- gtotal_savings
rm(total_savings, total_savings_1, total_savings_2, total_savings_3, total_savings_4,
   total_savings_5, total_savings_6, total_savings_7, total_savings_8, total_savings_9, gtotal_savings)
#compare savings with income 
hhgen<- hhgen %>% 
  mutate(savings_share_income=(total_savings/total_income))
hhgen<- hhgen %>% 
  mutate(too_much_savings_prob=as.numeric(savings_share_income>0.5))
#Export Data set with too_much_saving_problem
hhgen %>% 
  filter(too_much_savings_prob==1) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"HH_too_much_savings_problem.csv"))

#c. too much interest_rate
#import group 8th data (loans) 

hh_group8 <- read_excel("C:/Users/S.Muslimwal/OneDrive/1. AIRD Staff/1. Onging Projects/2020/3. WEE-RDP Baseline for IE/6. Data Processing and Data Quality Assurance/1. Up to date Data/Data-22-nov-2020.xlsx",
                        sheet = "General_Survey_Final-S8repeat")
#mergin hh_group8_with hh general
hh_group8<- hh_group8 %>% 
  left_join(hhgen, by=c("PARENT_KEY"="KEY"))

hh_group8$`inter_rate-S8G2amount` #cheeck for any extreme value

#d. too much food spending


#e. too much spending
section10<- hhgen[, grepl("start-eligible-sec10group", names(hhgen))]
section10_1<- section10[, grepl("spend", names(section10))]



