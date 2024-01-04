##install following packages
##tidyverse for data import and wrangling
##libridate for date functions
##ggplot for visualization 

library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle data attributes
library(ggplot2) #helps visualize data

getwd() #displays working directory
setwd("/local-scratch/localhome/hvahedi/Google_DA_capstone_project/") #displays working directory

##step 1: packages for collecting data
library(dplyr)
library(readr)

#==========================================================================================================
##STEP 1: Collect Data
#==========================================================================================================
##get list of csv files in the folder and assign names of files to Q11_files vector
Q11_files <- list.files(path = "/Google_DA_capstone_project/agg_Q1/", pattern = "*.csv", full.names = TRUE)

Q11_2022 <- Q11_files %>% lapply(read_csv) %>% bind_rows()
## %>% takes output of last function and inputs into next function
## lapply(read_csv) applies read_csv function to all csv files from 'Q11_files'
## result of above is a list of data frames, one for each file
## bind_rows() function combines multiple data frames by row
## bind_rows() combines multiple data frames by row, matches columns by name, fills
## missing columns with NA, produces single data frame that contains all 
## rows and columns from input data frames 


##search for any rows that contain NA in any column
na_rows <- Q11_2022 [!complete.cases(Q11_2022), ]

##=================================================
##step 2: wrangle data and combine into single file
## ================================================

# compare column names of the files together
# column names MUST match into single file 
colnames(Q11_2022)
colnames(Q22_2022)
colnames(Q33_2022)
colnames(Q44_2022)


#inspect dataframes and look for inconguencies
str(Q11_2022)
str(Q22_2022)
str(Q33_2022)
str(Q44_2022)


# convert ride_id and rideable_type to character so they can stack correctly
Q44_2022 <- mutate(Q44_2022, ride_id = as.character(ride_id)
            ,rideable_type = as.character(rideable_type))

Q33_2022 <- mutate(Q33_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

Q22_2022 <- mutate(Q22_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

Q11_2022 <- mutate(Q11_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))


##stack all quarter data frames into one big data frame
all_trips <- bind_rows(Q11_2022,Q22_2022,Q33_2022,Q44_2022)

##remove lat, long fields because they aren't needed
all_trips <- all_trips %>% 
    select(-c(start_lat, start_lng, end_lat, end_lng))

#==========================================================
#STEP 3: CLEANUP AND ADD DATA TO PREPARE FOR ANALYSIS
#==========================================================
str(all_trips) ## see list of columns and data types (numeric, character, etc)
head(all_trips) ## see first 6 rows of data frame
tail(all_trips) ## see last 6 rows of data frame
summary(all_trips) ##statistical summary of data

table(all_trips$member_casual) ##see how many observations fall under each usertype

##add columns that list date, month, day, year of each ride
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

##add a "ride_length" column and calculate trip time within all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

##inspect columns' structure
str(all_trips)

#convert ride_length datatype from string to numeric so we can run calculations
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

##remove data which contains negative ride length because bikes were taken 
##out of service for maintenance or potentially invalid due to
##ride lengths less than 30 seconds 
all_trips_v3 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<30),]

#=================================================================
#STEP 4: Descriptive Analysis
#=================================================================

#ride_length descriptive analysis
mean(all_trips_v3$ride_length) #calculates average of ride_length 
median(all_trips_v3$ride_length) #calculates median of ride_length minimizing impact of outliers
max(all_trips_v3$ride_length) #longest ride
min(all_trips_v3$ride_length) #shortest ride


summary(all_trips_v3$ride_length) #condense output of 4 lines above to one line from the column

##compare members and casual riders with respect to min/max/median/mean
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = mean)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = median)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = max)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = min)

##see avg ride time by each weekday for members vs. casuals
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)

#order weekdays from Mon to Fri in correct orientation 
all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels=c("Monday", 
                            "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# analyze ridership data by type and weekday
all_trips_v3 %>% 
  mutate(weekday = wday(started_at,label = TRUE)) %>% #creates weekday field
  group_by(member_casual,weekday) %>% ##groups by ridertype and weekday
  summarise(number_of_rides = n() #calculates number of rides and avg duration
  ,average_duration = mean(ride_length)) %>% #calculates avg duration
  arrange(member_casual,weekday) #sorts by ridertype and weekday

#visualize number of rides by weekday
all_trips_v3 %>% #pass variable as argument to next function
  mutate(weekday = wday(started_at,label=TRUE)) %>% #find weekday and pass as argument to next function
  group_by(member_casual, weekday) %>% #group by rider type & weekday
  summarise(number_of_rides = n() #summarize rider counts & ride duration
            ,average_duration = mean(ride_length)) %>% #pass summarise function as argument to next function
  arrange(member_casual, weekday) %>% #arrange summarise function by rider type & weekday
  ggplot(aes(x=weekday,y=number_of_rides,fill=member_casual)) + ##plot weekday vs. rider counts and separate by ridertype
  geom_col(position="dodge") +
  ggtitle("2022: Number of Riders by Weekday")

#visualize number of riders by month
all_trips_v3 %>% #pass variable as argument to next function
  mutate(month_of_year=month(started_at,label=TRUE)) %>% #find month of year and pass as argument to next function
  group_by(member_casual, month_of_year) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month_of_year) %>% 
  ggplot(aes(x=month_of_year,y=number_of_rides,fill=member_casual)) +
    geom_col(position="dodge") +
    ggtitle("2022: Monthly Breakdown of Riders by Type")

#visualize avg duration by weekday
all_trips_v3 %>% 
  mutate(weekday=wday(started_at,label=TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration_in_minutes=mean(ride_length)/60) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=average_duration_in_minutes,fill=member_casual)) + #plot weekday vs. trip duration separated by ridertype
  geom_col(position="dodge") +
  ggtitle("2022: Average Ride Duration Broken Down by Weekday")

#visualize avg duration by month
all_trips_v3 %>% 
  mutate(month_of_year=month(started_at,label=TRUE)) %>% 
  group_by(member_casual,month_of_year) %>% 
  summarise(number_of_rides = n()
            ,average_duration_in_minutes=mean(ride_length)/60) %>% 
  arrange(member_casual,month_of_year) %>% 
  ggplot(aes(x=month_of_year,y=average_duration_in_minutes,fill=member_casual)) + #plot weekday vs. trip duration separated by ridertype
  geom_col(position="dodge") +
  ggtitle("2022: Average Ride Duration Broken Down by Month")

#visualize avg ride time for total member and casual riders separately
all_trips_v3 %>% 
  group_by(member_casual) %>% 
  summarise(number_of_rides = n()
            ,Average_Trip_Time_in_Minutes=mean(ride_length)/60) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x=member_casual,y=Average_Trip_Time_in_Minutes,fill=member_casual)) + #plot weekday vs. trip duration separated by ridertype
  geom_col(position="dodge") +
  ggtitle("Member vs. Casual: Average Trip Times ")

#visualize total pie of members vs. casuals in a pie chart
all_trips_v3 %>% #pass all_trips_v3 dataframe as input to function
  group_by(member_casual) %>% #group by rider type
  summarise(count=n()) %>% #prepares data for further analysis by creating new data frame that contains summary statistics
  mutate(prop=count/sum(count)*100) %>%  #calculate proportions or percentages associated with user types
  
  ##for making pie chart need following:
  ggplot(aes(x="",y=prop,fill=member_casual, label=paste0(round(prop,2),"%"))) + ##initiate plot, and round percentage label values to 2 dec places. 
    geom_col(width = 1) + #create bar chart, initially, with height of bars representing values in data
    geom_text(position = position_stack(vjust = 0.5)) + #adjust position 
    coord_polar(theta="y") + #convert bar chart to pie chart
    labs(x=NULL,y=NULL,fill="Rider Type") + #used to modify labels for axis, legend, plot
    theme_void() + ##creates empty theme for my plot such as removing background axis/grid lines, etc.
    ggtitle("2022: Proportion of Members vs. Casual Riders")

#=====================================================================================
# STEP 5: export summary file for further analysis
#=====================================================================================
#create csv file that can be used to produce a visual in Excel or tableau

counts <- aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)
write.csv(counts, file = '/local-scratch/localhome/hvahedi/Google_DA_capstone_project/avg_ride_length.csv')
  
