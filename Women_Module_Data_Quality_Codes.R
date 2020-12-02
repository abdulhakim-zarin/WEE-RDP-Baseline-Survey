# Load women modul data
women_module <- read_excel("C:/Users/S.Muslimwal/OneDrive/1. AIRD Staff/1. Onging Projects/2020/3. WEE-RDP Baseline for IE/6. Data Processing and Data Quality Assurance/1. Up to date Data/Data-22-nov-2020.xlsx", 
                           sheet = "WOMEN_Survey_main")

#1. Strategy 1. number of women interviews per CDC should be exactly 10----
no_women_int_in_each_cdc<- women_module %>% 
  group_by(S1.3cdc) %>% 
  dplyr::summarise(no_hh=n())
women_module<- women_module %>% 
  left_join(no_women_int_in_each_cdc, by="S1.3cdc")
rm(no_women_int_in_each_cdc)
#export dataset with hhs/cdc problem
women_module %>% 
  filter(no_hh>10) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"women_module_no_hhs_in_cdc_more_than_10.csv"))
women_module %>% #may be the enumerator doesn't complete his/her target in one day
  filter(no_hhc<10) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"women_module_no_hhs_in_cdc_less_than_10.csv"))

#duplicate women_interviews in each CDC
#adding HHs serial number variable
women_module$hh_serial<- paste(women_module$S1.1province, 
                               women_module$S1.2district, 
                               women_module$S1.3cdc,
                               women_module$`sec1group-S1.5serial`,
                               sep = "-")

dups<- women_module %>% 
  group_by(hh_serial) %>% 
  dplyr::summarise(dups=n()) %>% 
  dplyr::arrange(desc(dups))
women_module <- women_module %>% 
  left_join(dups, by="hh_serial")
rm(dups)
#export datasets with duplicate hhs problem
women_module %>% 
  filter(dups>1) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"duplicate_women_module_in_CDC_probs.csv"))

#1. Strategy 2.1 Interveiw time ----

women_module<- women_module %>% 
  mutate(starttime_1=format(women_module$starttime,"%H:%M:%S"))
women_module<- women_module %>% 
  mutate(endtime_1=format(women_module$endtime,"%H:%M:%S"))
women_module$starttime_1<- as.POSIXlt(women_module$starttime_1,format="%H:%M:%S")
women_module$endtime_1<- as.POSIXlt(women_module$endtime_1, format="%H:%M:%S")

women_module<- women_module %>% 
  mutate(timedif=(as.numeric(endtime_1-starttime_1)))

women_module<- women_module %>% #interviews less than 60 mins
  mutate(less_than_30mins=as.numeric(timedif<30 | timedif<0))
women_module<- women_module %>% #interviews more than 80 mins
  mutate(more_than_80_mins=as.numeric(timedif>80))

#List of Enumerators with problem of survey time
lesstime_enum_fem<- women_module %>% 
  group_by(enu_first) %>% 
  dplyr::summarise(int_time_less_than_30mins=((sum(less_than_60mins))/n())*100)
moretime_enum_fem<- women_module %>% 
  group_by(enu_first) %>% 
  dplyr::summarise(int_time_more_than_80_mins=((sum(more_than_120_mins))/n())*100,
                   average_int_time=mean(timedif))
time_probs_enum_fem<- lesstime_enum_fem %>% 
  full_join(moretime_enum_fem, by="enu_first")
rm(lesstime_enum_fem, moretime_enum_fem)
#export women_module dataset weith time problem 
women_module %>% 
  filter(less_than_30mins==1 | more_than_80_mins==1) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"women_module_time_problems.csv"))


#2. Strategy 1.4 Missings----

#a. interviews with too much "No" Answers
women_module<- women_module %>%
  mutate(nops=apply(women_module, 1, function(x)sum(x == 2, na.rm = T)))
women_module<- women_module %>% 
  mutate(nops_prob=as.numeric(nops>10))#10 is 3rd quartile in number of Nos in each forms
#save datasets with too much Nos
women_module %>% 
  filter(nops_prob==1) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"Women_module_Too_Much_Nos.csv"))

#b. Refused and I don't knows 
women_module <- women_module %>% 
  mutate(idk=rowSums(women_module==98, na.rm = T))
women_module <- women_module %>% 
  mutate(refusal=rowSums(women_module==99, na.rm = T))
women_module<- women_module %>% 
  mutate(totidk_ref=(idk+refusal))
boxplot(women_module$totidk_ref)
women_module<- women_module %>% 
  mutate(idk_refusal_prb=as.numeric(totidk_ref>3))
#exporting dataset with too much IDK and refusals
women_module %>% 
  filter(idk_refusal_prb==1) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"Women_module_idk_refusal_problem.csv"))

women_module<- women_module %>%
  mutate(missings=apply(women_module, 1, function(x) sum(is.na(x))))
women_module$missings_prob<- as.numeric(women_module$missings>100)
women_module %>% 
  filter(missings_prob==1) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"Women_module_missings_problem.csv"))

#3. Startegy 3.3. Inconsisitency in Data ----
#a. seciont 3 (too much "I don't go to this place")

section3<- women_module[, grepl("sec3group", names(women_module))]
section3_1<- section3[, grepl("A", names(section3))]
section3_1<- section3_1[,-seq(2,15,2)]
section3_1<- section3_1[,-8]
women_module$sec3_prob<- (apply(section3_1, 1, function(x)sum(x == 5, na.rm = T)))/7
women_module$sec3_prob<- as.numeric(women_module$sec3_prob>0.8)
rm(section3, section3_1)
#export data sets with problems in sec 3
women_module %>% 
  filter(sec3_prob==1) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"Women_module_travel_problem.csv"))

#4. strategy 1.4 Outliers ----
# a. too high income
women_module$`sec7group-S7.2Cincome`
boxplot(women_module$`sec7group-S7.2Cincome`)
# export data set with too much income 
women_module %>% 
  filter(`sec7group-S7.2Cincome`>400000) %>%#"400000" will be visually identified from the resulsts
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"Women_too_much_income_problem.csv"))




