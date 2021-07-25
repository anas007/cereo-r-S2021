#Explore bike data char
#
#Explore relationship between Temp and riders
#
#Step 1: load bike data and look at metadata

library(tidyverse)

##ERead data ----

bike<-read.csv("data/daily_bike_data.csv")

head(bike)
str(bike)
sort(names(bike))

#Time trend of ridership
ggplot(data=bike)+
  geom_line(aes(x = (dteday), y=cnt))

ggplot(data=bike)+
  geom_point(aes(x = temp, y=cnt))

##Data  cleaning ----
#dplyr verbs for data trnasfomation
#select: select columns to keep
#filter: select rows to keep
#mutate: transforms data while keeping other columns
#transmute: creates new columns and doesnot keep other columns
#%>%: pipe" pipes the output from one command as the input for the next command

#One way of selecting spring records and just few cols
spring_bike<-filter(bike, season == "spring")
srping_bike_temp_cnt<-select(spring_bike, temp, cnt)

srping_bike_temp_cnt2<-bike%>%
  filter(season=="spring")%>%
  select(temp, cnt)

### Excercise: seelct weathersit and cnt for all winter records

winter_bike_weather_cnt<-bike%>%
  filter(season=="winter")%>%
  select(weathersit, cnt)

### Mutate and transmulte with factors and date
bike2<-bike%>%
  dplyr::mutate(
    weather_fac= factor(weathersit, 
                        levels=c(1,2,3,4),
                        labels = c("Clear", "Cloudy", "Rainy", "Heavy Rain")))
  
### Converting to and from dates
bike_dates<-bike%>%transmute(
  instant,
  date = dteday,
  date_num =as.Date(dteday, origin="1970-01-01"),
  date_char=as.character(dteday)
)

###Additional filtering and selecting
bike%>%select(-temp)

keep_var<-c("dteday", "cnt")
#keep_vars<-paste0("temp", 1:12)
bike%>%select(all_of(keep_var))

bike%>%filter(season!="spring")%>%
  select(season)%>%
  distinct()

bike%>%
  filter(season=="summer"|season=="winter")

seasons<-c("summer", "winter")

bike%>%
  filter(season %in% seasons)

####More dplyr
#summerize: Summary of multpile rowa for col
#group_by: perform an operation seperately for each group

bike2%>% 
  filter(season=="summer")%>%
  summarize(
  temp_mean=mean(temp),
  cnt_mean= mean(cnt),
  cnt_sum = sum(cnt)
)

bike2%>% 
  group_by(season)%>%
  summarize(
    temp_mean=mean(temp),
      cnt_sum = sum(cnt)
)

#What are season definition
#Create new season var with meterological definition
sort(names(bike))
bike%>%select(season, mnth) %>% distinct()

bike3 <- bike2 %>%
  mutate(
    season2 = 1 * (mnth %in% c("December", "January", "February")) +
      2 * (mnth %in% c("March", "April", "May")) +
      3 * (mnth %in% c("June", "July", "August")) +
      4 * (mnth %in% c("September", "October", "November"))) %>%
  mutate(
    season2 = factor(season2, levels = 0:4,
                     labels = c("Unknown", "Winter", "Spring", "Summer", "Fall"))
  )

bike3 %>%
  group_by(season2) %>%
  summarize(
    temp_mean = mean(temp),
    ride_sum = sum(cnt)
  )
###Facetting in ggplot
ggplot(data=bike3)+
  geom_point(aes(x=temp, y=cnt))+
  geom_smooth(aes(x=temp, y=cnt), method="lm",
              formula = y~poly(x,2))+
  facet_wrap(~season2)

##Pivoting wider to long and longer to wide
# Long to wide: data in multiple columns. e.g. temp1, temp2, temp3... temp12
# Wide to long: data in one column, classifier in other columns
# tidyr is the package that allows transformations
months <- c("January", "February", "March", "April",
            "May", "June", "July", "August",
            "September", "October", "November", "December")

tidybike<-bike3%>%
  select(yr, mnth, temp, cnt)%>%
  mutate(month=factor(mnth, levels = months,
                      labels=months))%>%
  group_by(yr, month)%>%
  summarize(temp_mean=mean(temp),
            rides=sum(cnt))
  
  
#Tidyr functions for long to wide
  #spread
  #pivot_wider
  tidybike%>%
    select(-rides)%>%
  pivot_wider(values_from=temp_mean,
              names_from=month,
              names_prefix = "temp_")
tidybike%>%
  select(-rides)%>%
  tidyr::spread(key=month, value=temp_mean)

rides<-tidybike%>%
  select(-temp_mean)%>%
  pivot_wider(values_from=rides,
              names_from=month,
              names_prefix = "ride_")%>%
  rename_with(tolower)%>%
  rename(year=yr)

#Going from wide to long
#Pivot longer
#gather
rides%>%gather(key="month", value = "rides", -year)

rides %>% 
  select(year, ride_january, ride_february) %>%
  pivot_longer(names_to = "month", 
               values_to = "rides", 
               cols = c("ride_january", "ride_february")) %>%
  mutate(month = substr(month, 7, nchar(month))) %>%
  mutate(month = toupper(month))


