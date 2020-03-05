# prep for model

library(lubridate)
library(stringr)
library(tidyverse)
library(timeDate)
raw_data <- read_csv("DATA/pathrise_data.csv")
# cleaning start dates
dat_clean= raw_data %>% mutate(start_month=substr(cohort_tag,1,3) 
                               , start_year=paste0('20',substr(cohort_tag,4,5))
                               ,start_group=substr(cohort_tag,6,6))

# if no cohort_tag, assign july 2020,group A
dat_clean = dat_clean %>% mutate(start_date= as_date(paste0(start_year,'-',start_month,'-01'))  ) %>%
  mutate(start_date=as_date(ifelse(is.na(cohort_tag),as_date("2020-07-01" ),as_date(start_date)) )) %>%
  mutate(start_group=ifelse(is.na(cohort_tag),'A', start_group )) 

dat_clean$first_week= as_date(timeNthNdayInMonth(dat_clean$start_date, nday = 1, nth = 1, format = "%Y-%m-%d"))
dat_clean$third_week= as_date(timeNthNdayInMonth(dat_clean$start_date, nday = 1, nth = 3, format = "%Y-%m-%d"))

first_week=dat_clean%>%filter(start_group%in%c('a','A')) %>% mutate(cohort_start_date=as_date(timeNthNdayInMonth(start_date, nday = 1, nth = 1, format = "%Y-%m-%d"))
)
third_week=dat_clean%>%filter(start_group%in%c('B','C')) %>% mutate(cohort_start_date=as_date(timeNthNdayInMonth(start_date, nday = 1, nth = 3, format = "%Y-%m-%d"))
)
#nrow(first_week)+nrow(third_week)
dat_clean=rbind(first_week,third_week) %>%select(-start_date)

# fill in duration  ------------------------------------------------------------------

get_duration_transform_ordinals.fun<- function(dat_clean){
  fill_in_duration <- dat_clean%>% filter(pathrise_status%in% c('Active')) %>% mutate(
    interval_col = cohort_start_date %--% as_date('2020-02-25') )
  fill_in_duration$program_duration_days= as.duration(fill_in_duration$interval_col)/ ddays(1)
  
  
  fill_in_duration <- fill_in_duration %>%select(-interval_col)
  
  non_active_dat <- dat_clean%>% filter(!(pathrise_status%in% c('Active')))
  dat=rbind(fill_in_duration, non_active_dat)
  
  dat= dat %>% mutate(education_level_ordered=as.numeric(factor(highest_level_of_education, levels=c("Some High School", "GED or equivalent","High School Graduate" , "Some College, No Degree"  , "Bachelor's Degree" ,"Master's Degree","Doctorate or Professional Degree")  ))
                      ,  experience_ordered=as.numeric(factor(professional_experience, levels=c("Less than one year","1-2 years","3-4 years","5+ years")))
                      , job_search_length_ordered= as.numeric(factor(length_of_job_search, levels=c( "Less than one month", "1-2 months" ,  "3-5 months", "6 months to a year"  , "Over a year"  )))
  ) %>% mutate(is_placed= ifelse(pathrise_status=='Placed',1,0)
               , is_censored= ifelse(!(pathrise_status %in% c('Placed','Withdrawn (Failed)' )), 1,0 ) ) %>%
    mutate(gender= factor(gender)
           , employment_status=factor(employment_status)
           , work_authorization_status= factor(work_authorization_status)
           , race=factor(race)
           , biggest_challenge_in_search= factor(biggest_challenge_in_search )) %>%
    select(-length_of_job_search)%>%  
    select(-highest_level_of_education ) %>%
    select(-professional_experience)%>%
    select(-start_group)%>%
    select(-first_week)%>%
    select(-third_week)
  
  
  # dat_initial= dat %>% mutate(program_duration_days=0, 
  #                     is_placed=0, is_censored=0)
  # dat=rbind(dat,dat_initial)
  
  
  return(dat)
}  
dat_clean_with_duration = get_duration_transform_ordinals.fun(dat_clean)

#remaining_na= dat_clean_with_duration %>% filter(is.na(program_duration_days))
# table(remaining_na$pathrise_status) # just left with Break and MIA

# impute  ------------------------------------------------------------------

library(mice)
data_imputation <- mice(dat_clean_with_duration,m=5,meth='pmm',maxit=50,seed=500)
dat_complete= mice::complete(data_imputation,1)


# unless we saw you get hired, your hired outcome was censored
# drop status: MIA, Break, 'Closed Lost','Deferred'
dat_complete <- dat_complete %>% 
  mutate(is_censored= ifelse(pathrise_status =='Placed',0,1))%>%
  filter(!(pathrise_status %in% c('MIA', 'Break', 'Closed Lost','Deferred') ))

dat_complete$interview_percent= 100*dat_complete$number_of_interviews/dat_complete$number_of_applications
dat_complete$number_of_applications<- as.numeric(as.character(dat_complete$number_of_applications))
dat_complete= dat_complete%>% 
  group_by(cohort_start_date,primary_track)%>%
  mutate(cohort_count=n())%>%
  ungroup() 

write_csv(dat_complete,'dat_complete.csv')

# survive!  ------------------------------------------------------------------








train <- dat_complete %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(dat_complete, train, by = 'id')
surv_placed_train= Surv(train$program_duration_days,train$is_placed)
surv_placed_test= Surv(test$program_duration_days,test$is_placed)

write_csv(dat_complete,'train.csv')
write_csv(dat_complete,'test.csv')


library(survival) 
# program duration indicated the length of susvival (surviving is not getting placed)
# the survival is censored for anyone who is not placed. being placed is like a "known death", and all other "deaths" would occur at some point past the last measurement 



library(ggfortify)
aa_fit <-aareg(surv_placed ~
#primary_track 
 #cohort_count
employment_status
#+ primary_track:gender
#+ biggest_challenge_in_search
#+ interview_percent
+ number_of_applications
#+ gender
#+ race
#+ education_level_ordered
#+ experience_ordered
# + job_search_length_ordered
, data = dat_complete)
