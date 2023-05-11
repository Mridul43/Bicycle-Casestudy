#https://rpubs.com/Mridulcc/Bicycle-Casestudy

#load_package
library(tidyverse)
library(dplyr)
library(skimr)
library(janitor)
library(lubridate)
library(ggplot2)

#data_import

january_2022 <- read_csv("Bicycle/202201-divvy-tripdata.csv")
february_2022 <- read_csv("Bicycle/202202-divvy-tripdata.csv")
march_2022 <- read_csv("Bicycle/202203-divvy-tripdata.csv")
april_2022 <- read_csv("Bicycle/202204-divvy-tripdata.csv")
may_2022 <- read_csv("Bicycle/202205-divvy-tripdata.csv")
june_2022 <- read_csv("Bicycle/202206-divvy-tripdata.csv")
july_2022 <- read_csv("Bicycle/202207-divvy-tripdata.csv")
august_2022 <- read_csv("Bicycle/202208-divvy-tripdata.csv")
september_2022 <- read_csv("Bicycle/202209-divvy-publictripdata.csv")
october_2022 <- read_csv("Bicycle/202210-divvy-tripdata.csv")
november_2022 <- read_csv("Bicycle/202211-divvy-tripdata.csv")
december_2022 <- read_csv("Bicycle/202212-divvy-tripdata.csv")


#colnames(december_2022)
#colnames(january_2022)
#colnames(february_2022)
#colnames(march_2022)
#colnames(april_2022)
#colnames(may_2022)
#colnames(june_2022)
#colnames(july_2022)
#colnames(august_2022)
#colnames(september_2022)
#colnames(october_2022)
#colnames(november_2022)

#Chack_Duplicates
#get_dupes(january_2022)
#get_dupes(february_2022)
#get_dupes(march_2022)
#et_dupes(april_2022)
#et_dupes(may_2022)
#et-dupes(june_2022)
#get_dupes(july_2022)
#get_dupes(august_2022)
#get_dupes(september_2022)
#get_dupes(october_2022)
#et_dupes(november_2022)
#get_dupes(december_2022)

#bind_data
full_data <- rbind(january_2022,february_2022,march_2022,april_2022,june_2022,july_2022,august_2022,
                   september_2022,october_2022,november_2022,december_2022)
#see_fulldata
head(full_data)
summary(full_data)
colnames(full_data)
str(full_data)
glimpse(full_data)
skim_without_charts(full_data)

#save full data
#write.csv(full_data,file = "full_Cyclist_data.csv",row.names = FALSE)

get_dupes(full_data)

colSums(is.na(full_data))

#cleaning

final_data <- distinct(full_data) #remove_duplicate
drop_na(final_data)
remove_empty(final_data)
remove_missing(final_data)

#final_data %>%
  #summary()

# adding new columns date, month, day, year and day of the week
final_data$date <- as.Date(final_data$started_at) #The default format is yyyy-mm-dd
final_data$month <- format(as.Date(final_data$date), "%m")
final_data$day <- format(as.Date(final_data$date), "%d")
final_data$year <- format(as.Date(final_data$date), "%Y")
final_data$day_of_week <- format(as.Date(final_data$date), "%A")


# adding new column ride length
final_data$ride_length <- difftime(final_data$ended_at,final_data$started_at) 
is.factor(final_data$ride_length) #checking/verifying ride length data type

#converting ride length data type factor to numeric
final_data$ride_length <- as.numeric(final_data$ride_length)
is.factor(final_data$ride_length) 

cat("Number of rows :",nrow(final_data))
cat("Number of colams :",ncol(final_data))

#aggregate(final_data$ride_length ~ final_data$member_casual, FUN = mean)
#aggregate(final_data$ride_length ~ final_data$member_casual, FUN = median)
#aggregate(final_data$ride_length ~ final_data$member_casual, FUN = max)
#aggregate(final_data$ride_length ~ final_data$member_casual, FUN = min)


# Now, let's run the average ride time by each day for members vs casual users
aggregate(final_data$ride_length ~ final_data$member_casual + final_data$day_of_week, FUN = mean)

#Calculate the mean of ride_length and max ride_length
final_data %>% 
  group_by(member_casual) %>%
  summarise(average_ride_length = mean(ride_length), median_length = median(ride_length), 
  max_ride_length = max(ride_length), min_ride_length = min(ride_length))


#See members and casuals by the total ride taken (ride count)
final_data %>% 
  group_by(member_casual) %>% 
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(final_data))*100)


#final_data %>% 
  #group_by(member_casual) %>% 
 #summarize(count=length(ride_id),
            #percentage_of_total=(length(ride_id)))


#Calculate the average ride_length  day_of_the week
final_data %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(average_ride_length = mean(ride_length), .groups="drop")



#Calculate the number of rides for users by day_of_week by adding Count of trip_id to Values.
final_data %>%
  group_by(day_of_week, member_casual) %>%
  summarise(number_of_rides = n_distinct(ride_id))


#see the total rides taken by members and casuals by month 
final_data %>%  
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),.groups="drop") %>% 
  arrange(member_casual, month) 




#write.csv(final_data,file = "final_Cyclist_data.csv",row.names = FALSE)


#VISUALIZATION

#visualize member vs casual distribution
final_data %>% 
  group_by(member_casual) %>% 
  summarize(count=length(ride_id),
  percentage_of_total=(length(ride_id)/nrow(final_data))*100) %>%  
  ggplot(aes(x = member_casual, y = count, fill = member_casual)) +
  labs(title ="member vs casual distribution") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#visualize the average ride_length  day_of_the week
final_data %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(average_ride_length = mean(ride_length), .groups="drop")%>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride time of Members and Casual riders Vs. Day of the week")

# visualize the number of rides for users by day_of_week by adding Count of trip_id to Values.
final_data %>%
  group_by(day_of_week, member_casual) %>%
  summarise(number_of_rides = n_distinct(ride_id))%>%  
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides of Members and Casual riders Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

 #Let's visualize the total rides taken by members and casuals by month 
  final_data %>%  
    group_by(member_casual, month) %>% 
    summarise(number_of_rides = n(),.groups="drop") %>% 
    arrange(member_casual, month)  %>% 
    ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
    labs(title ="Total rides by Members and Casual riders by Month") +
    theme(axis.text.x = element_text(angle = 45)) +
    geom_col(width=0.5, position = position_dodge(width=0.5)) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE))





