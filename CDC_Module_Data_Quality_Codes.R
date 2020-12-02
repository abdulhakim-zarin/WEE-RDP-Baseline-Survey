# Library Needed for Analysis
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

#1.1.loading data 
cdcdata <- read_excel("C:/Users/S.Muslimwal/OneDrive/1. AIRD Staff/1. Onging Projects/2020/3. WEE-RDP Baseline for IE/6. Data Processing and Data Quality Assurance/1. Up to date Data/Data-27-28-nov-2020_for_c.xlsx")

#1. strategy 2.1 Survey Time ----
cdcdata<- cdcdata %>% #calculate time differance
  mutate(timedif=as.numeric(cdcdata$endtime - cdcdata$starttime))
cdcdata<- cdcdata %>% #interviews less than 30 mins
  mutate(lesstime=as.numeric(timedif<30))
cdcdata<- cdcdata %>% #interviews more than 30 mins
  mutate(moretime=as.numeric(timedif>70))

cdcdata %>% 
  filter(lesstime==1 | moretime==1) %>% 
  write_excel_csv(paste(gsub(":", "-", Sys.Date()),"cdc_time_diff.csv"))

#enumerators with under average interview times
lesstime_enum<- cdcdata %>% 
  filter(lesstime==1) %>% 
  group_by(enu_first) %>% 
  dplyr::summarise(less_than_30mins=count(n()))
#enumerators with over average interview time
morettime_enum<- cdcdata %>% 
  filter(moretime==1) %>% 
  group_by(enu_first) %>% 
  dplyr::summarise(more_than_70_mins=count(n()))

#2.strategy 1.2 Missing Values----
#a. Refused and I don't knows 
cdcdata <- cdcdata %>% 
  mutate(idk=rowSums(cdcdata==99, na.rm = T))
cdcdata <- cdcdata %>% 
  mutate(ref=rowSums(cdcdata==97, na.rm = T))
cdcdata<- cdcdata %>% 
  mutate(totidk_ref=(idk+ref))
#adding idk_refusal problem in dataset
cdcdata<- cdcdata %>% 
  mutate(idk_ref_prob=as.numeric((totidk_ref>=6)))
#enumerators with average I don't knows and refusals per interview
idk_ref_enum<- cdcdata %>% 
  group_by(enu_first) %>% 
  dplyr::summarise(total_idk_refusal=sum(idk_ref_prob))

#b. Missing values (NAs)
cdcdata<- cdcdata %>%
  mutate(missings=apply(cdcdata, 1, function(x) sum(is.na(x))))
cdcdata$missings<- cdcdata$missings-18
#Enumerators with average missings per interview
cdcdata<- cdcdata %>% 
  mutate(missing_prob=as.numeric(missings>24))
missing_enum<- cdcdata %>% 
  group_by(enu_first) %>% 
  dplyr::summarise(total_missing_variables=sum(missing_prob))

#3. Strategy 2.5. Duplicates interviews----
dupvar<- cdcdata %>% 
  group_by(CDC) %>% 
  dplyr::summarise(count=n())
cdcdata<- cbind(cdcdata, dupvar$count)
rm(dupvar)
#filter data with duplicates
dupcdcd<- cdcdata %>% 
  dplyr::filter(cdcdata$`dupvar$count`>1)
#save file of cdc duplicates data
if (nrow(dupcdcd)!=0) {
  write_excel_csv(dupcdcd, paste(gsub(":", "-", Sys.Date()),"cdcduplicatedata.xlsx"))
  print("No duplicate data")
}
rm(dupcdcd)
#enumerators with duplicate data (Should not more than 1)
cdcdata %>% 
  group_by(enu_first) %>% 
  dplyr::summarise(average_dups=mean(cdcdata$`dupvar$count`))

#4. Enumerators Daily Target ----
#all provinces
dailytarg<- cdcdata %>% 
  group_by(TODAY, enu_first) %>% 
  dplyr::summarise(dailytargit=sum(n()))

#report of dialy targets
dailytarg<- dailytarg %>% 
  mutate(dailytarg_prob=as.numeric(dailytargit>2))
write_excel_csv(dailytarg, 
                paste(gsub(":", "-", Sys.Date()),"_cdcdaily_tar.csv",sep=""))



#Final enumerators performance reqport for Ghafoori

f1<- idk_ref_enum %>% 
  full_join(lesstime_enum, by="enu_first")

f2<- f1 %>% 
  full_join(morettime_enum, by="enu_first")
final_report_enum_pref<- f2 %>% 
  full_join(missing_enum, by="enu_first")
final_report_enum_pref<- as.data.table(final_report_enum_pref)

final_report_enum_pref<- final_report_enum_pref[,-c(4,6)]

write_excel_csv(final_report_enum_pref, 
                paste(gsub(":", "-", Sys.Date()),"_cdc_enumerators_performance.csv",sep=""))
rm(f1, f2, idk_ref_enum, lesstime_enum, morettime_enum, missing_enum, dupcdcd)

#Exporting final dataset with DQ variables

write_excel_csv(cdcdata, 
                paste(gsub(":", "-", Sys.Date()),"cdc_data_with_dq_variables.csv",sep=""))
