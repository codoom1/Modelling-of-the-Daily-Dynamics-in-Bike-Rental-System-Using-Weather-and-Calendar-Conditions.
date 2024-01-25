
library(tidyverse)
library(rvest)
library(dplyr)
library(modeest)  # Required for mode calculation

### Set working directory
setwd("/Users/christopherodoom/Desktop/Data_Science/Paper_bike_rentals")

####Bike rentals paper code

### Loading required library
library(tidyverse)
library(lubridate)
library(readr)
####Getting the bike rental dataset




bike_d <- read_csv("201802-capitalbikeshare-tripdata.csv")
head(bike_d)


## Selecting useful variables
coln=c(2,3,9,10,13)
dat_bike <- bike_d[,coln]

colnames(dat_bike) <- c("biketype","date", "latitude","longitude", "membership")

##### Removing the time from the date
# Convert the "date" column to a Date object
# dat_bike$date <- mdy_hm(dat_bike$date)
# Extract only the date part
dat_bike$date <- as.Date(dat_bike$date, format="%m/%d/%y")

dat_bike$biketype <- as.factor(dat_bike$biketype)
dat_bike$membership <- as.factor(dat_bike$membership)

### Checking structure of the data
str(dat_bike)
# Calculate mean for numeric columns
numeric_cols <- sapply(dat_bike, is.numeric)
mean_values <- dat_bike %>%
  group_by(date) %>% 
  summarize(across(.cols = names(dat_bike)[numeric_cols], .fns = mean, na.rm = TRUE))

# Calculate mode for categorical columns

# Function to calculate the mode
get_mode <- function(x) {
  unique_values <- unique(x)
  tab <- tabulate(match(x, unique_values))
  unique_values[tab == max(tab)]
}

categorical_cols <- sapply(dat_bike, is.factor)
mode_values <- dat_bike %>%
  group_by(date) %>% 
  summarize(across(.cols = names(dat_bike)[categorical_cols], .fns = get_mode))

# Combine the mean and mode results
bikedata <- bind_cols(mean_values, mode_values)
dat <- bikedata 


new_dat <- dat_bike %>%
  group_by(date) %>%
  summarize(registered = sum(membership == "member"),
            casual = sum(membership == "casual"))

new_dat <- new_dat %>% 
  mutate(total_rentals=registered+casual,latitude=dat$latitude, longitude=dat$longitude)


##########################################################################
#####Getting 2018 to 2020 data
# Define the range of years and months you want to iterate over
years <- 2018:2020
months <- 1:12

# Create an empty list to store the data frames
bike_data <- data.frame()

# Iterate over years and months
for (year in years) {
  for (month in months) {
    # Generate the file name based on the year and month
    file_name <- paste0(year, formatC(month, width = 2, flag = "0"), "-capitalbikeshare-tripdata.csv")
    
    # Read the CSV file into a data frame
    
    bike_df <- read_csv(file_name)
    
    ## Selecting useful variables
    coln=c(2,3,9,10,13)
    dat_bike <- bike_df[,coln]
    
    colnames(dat_bike) <- c("biketype","date", "latitude","longitude", "membership")
    
    ##### Removing the time from the date
    # Convert the "date" column to a Date object
    #dat_bike$date <- mdy_hm(dat_bike$date)
    # Extract only the date part
    dat_bike$date <- as.Date(dat_bike$date, format="%m/%d/%y")
    
    dat_bike$biketype <- as.factor(dat_bike$biketype)
    dat_bike$membership <- as.factor(dat_bike$membership)
    
    ### Checking structure of the data
    str(dat_bike)
    # Calculate mean for numeric columns
    numeric_cols <- sapply(dat_bike, is.numeric)
    mean_values <- dat_bike %>%
      group_by(date) %>% 
      summarize(across(.cols = names(dat_bike)[numeric_cols], .fns = mean, na.rm = TRUE))
    
    # Calculate mode for categorical columns
    
    # Function to calculate the mode
    get_mode <- function(x) {
      unique_values <- unique(x)
      tab <- tabulate(match(x, unique_values))
      unique_values[tab == max(tab)]
    }
    
    categorical_cols <- sapply(dat_bike, is.factor)
    mode_values <- dat_bike %>%
      group_by(date) %>% 
      summarize(across(.cols = names(dat_bike)[categorical_cols], .fns = get_mode))
    
    # Combine the mean and mode results
    bikedata <- bind_cols(mean_values, mode_values)
    dat <- bikedata 
    
    new_dat <- dat_bike %>%
      group_by(date) %>%
      summarize(registered = sum(membership == "member"),
                casual = sum(membership == "casual"))
    
    new_dat1 <- new_dat %>% 
      mutate(total_rentals=registered+casual,latitude=dat$latitude, longitude=dat$longitude)
    
    # Combine all data frames into a single data frame
    bike_data <- bind_rows(bike_data, new_dat1)
  }
  
}


View(bike_data) 

#########################################################################

#####Getting 2020 data
# Define the range of years and months you want to iterate over
years <- 2020
months <- 4:12

# Create an empty list to store the data frames
bike_data1 <- data.frame()

# Iterate over years and months
for (year in years) {
  for (month in months) {
    # Generate the file name based on the year and month
    file_name <- paste0(year, formatC(month, width = 2, flag = "0"), "-capitalbikeshare-tripdata.csv")
    
    # Read the CSV file into a data frame
    
    bike_df <- read_csv(file_name)
    
    ## Selecting useful variables
    coln=c(2,3,9,10,13)
    dat_bike <- bike_df[,coln]
    
    colnames(dat_bike) <- c("biketype","date", "latitude","longitude", "membership")
    
    ##### Removing the time from the date
    # Convert the "date" column to a Date object
    #dat_bike$date <- mdy_hm(dat_bike$date)
    # Extract only the date part
    dat_bike$date <- as.Date(dat_bike$date, format="%m/%d/%y")
    
    dat_bike$biketype <- as.factor(dat_bike$biketype)
    dat_bike$membership <- as.factor(dat_bike$membership)
    
    ### Checking structure of the data
    str(dat_bike)
    # Calculate mean for numeric columns
    numeric_cols <- sapply(dat_bike, is.numeric)
    mean_values <- dat_bike %>%
      group_by(date) %>% 
      summarize(across(.cols = names(dat_bike)[numeric_cols], .fns = mean, na.rm = TRUE))
    
    # Calculate mode for categorical columns
    
    # Function to calculate the mode
    get_mode <- function(x) {
      unique_values <- unique(x)
      tab <- tabulate(match(x, unique_values))
      unique_values[tab == max(tab)]
    }
    
    categorical_cols <- sapply(dat_bike, is.factor)
    mode_values <- dat_bike %>%
      group_by(date) %>% 
      summarize(across(.cols = names(dat_bike)[categorical_cols], .fns = get_mode))
    
    # Combine the mean and mode results
    bikedata <- bind_cols(mean_values, mode_values)
    dat <- bikedata 
    
    new_dat <- dat_bike %>%
      group_by(date) %>%
      summarize(registered = sum(membership == "member"),
                casual = sum(membership == "casual"))
    
    new_dat1 <- new_dat %>% 
      mutate(total_rentals=registered+casual,latitude=dat$latitude, longitude=dat$longitude)
    
    # Combine all data frames into a single data frame
    bike_data1 <- bind_rows(bike_data1, new_dat1)
  }
  
}


View(bike_data1) 






#############################################################################
#####Getting 2021 to 2023 data
# Define the range of years and months you want to iterate over
years <- 2021:2023
months <- 1:12

# Create an empty list to store the data frames
bike_data <- data.frame()

# Iterate over years and months
for (year in years) {
  for (month in months) {
    # Generate the file name based on the year and month
    file_name <- paste0(year, formatC(month, width = 2, flag = "0"), "-capitalbikeshare-tripdata.csv")
    
    # Read the CSV file into a data frame
    
    bike_df <- read_csv(file_name)
    
    ## Selecting useful variables
    coln=c(2,3,9,10,13)
    dat_bike <- bike_df[,coln]
    
    colnames(dat_bike) <- c("biketype","date", "latitude","longitude", "membership")
    
    ##### Removing the time from the date
    # Convert the "date" column to a Date object
    #dat_bike$date <- mdy_hm(dat_bike$date)
    # Extract only the date part
    dat_bike$date <- as.Date(dat_bike$date, format="%m/%d/%y")
    
    dat_bike$biketype <- as.factor(dat_bike$biketype)
    dat_bike$membership <- as.factor(dat_bike$membership)
    
    ### Checking structure of the data
    str(dat_bike)
    # Calculate mean for numeric columns
    numeric_cols <- sapply(dat_bike, is.numeric)
    mean_values <- dat_bike %>%
      group_by(date) %>% 
      summarize(across(.cols = names(dat_bike)[numeric_cols], .fns = mean, na.rm = TRUE))
    
    # Calculate mode for categorical columns
    
    # Function to calculate the mode
    get_mode <- function(x) {
      unique_values <- unique(x)
      tab <- tabulate(match(x, unique_values))
      unique_values[tab == max(tab)]
    }
    
    categorical_cols <- sapply(dat_bike, is.factor)
    mode_values <- dat_bike %>%
      group_by(date) %>% 
      summarize(across(.cols = names(dat_bike)[categorical_cols], .fns = get_mode))
    
    # Combine the mean and mode results
    bikedata <- bind_cols(mean_values, mode_values)
    dat <- bikedata 
    
    new_dat <- dat_bike %>%
      group_by(date) %>%
      summarize(registered = sum(membership == "member"),
                casual = sum(membership == "casual"))
    
    new_dat1 <- new_dat %>% 
      mutate(total_rentals=registered+casual,latitude=dat$latitude, longitude=dat$longitude)
    
    # Combine all data frames into a single data frame
    bike_data <- bind_rows(bike_data, new_dat1)
  }
  
}


View(bike_data) 
############################################################################



Bigdata <- bind_rows(bike_data1,bike_data)

bike_rental<-Bigdata
write.csv(bike_rental, file = "~/Desktop/bike_rental.csv")








#############################################################################

##### Getting the weather data
url1 <- "https://i-weather.com/weather/washington/history/monthly-history?gid=4140963&station=19064&month=1&year=2021&language=english&country=us-united-states"

df1 <- url1 %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)
df= df1[[7]]
View(df)
################################################################################################


tables <- data.frame()
# Define the base URL
base_url <- "https://i-weather.com/weather/washington/history/monthly-history?gid=4140963&station=19064"

# Iterate over the months from 1 to 12
for(j in c(2021,2022,2023)){
for (i in 1:12) {
  # Create the URL for each month
  url <- paste0(base_url, "&month=", i, "&year=", j,"&language=english&country=us-united-states")
  
  # Scrape the table for each month
  df1 <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = TRUE)
  
  # Assign the table to a data frame
  df <- df1[[7]]
  tables <- bind_rows(tables, df)
  # Do further processing or analysis with the data frame for the current month
  # ...
  
  # Print the table for the current month
  # cat("Table for month", i, ":\n")
  # print(df)
}
}

head(tables)




url2 <-"https://www.timeanddate.com/weather/usa/washington-dc/historic?month=1&year=2021&hd=20210101"

f1 <- url2 %>% 
  read_html()%>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE) 

###Selectind dataframe of interest
f2=f1[[2]]


# Remove the header row and make the first row the new header
new_header <- as.character(f2[1, ])
f2 <- f2[-1, ]
colnames(f2) <- new_header

# Remove columns 2 and 6
f2 <- f2[, -c(2, 6)]

#### Remove last row
f2 <- f2[-nrow(f2),]

# Remove symbols, units, and characters from specific columns
cols_to_clean <- c(2, 4, 5, 6, 7)

clean_column_values <- function(column) {
  gsub("[^0-9.]+", "", as.character(column))
}

f3 <- f2 %>%
  mutate(across(all_of(cols_to_clean), clean_column_values))


#### Aggregating the data


f3$Temp <-as.numeric(f3$Temp)
f3$Wind <-as.numeric(f3$Wind)
f3$Humidity <-as.numeric(f3$Humidity)
f3$Barometer <-as.numeric(f3$Barometer)
f3$Visibility <-as.numeric(f3$Visibility)
f3$Weather <- as.factor(f3$Weather)


# Calculate mean for numeric columns
numeric_cols <- sapply(f3, is.numeric)
mean_values <- f3 %>%
  summarize(across(.cols = names(f3)[numeric_cols], .fns = mean, na.rm = TRUE))

# Calculate mode for categorical columns

# Function to calculate the mode
get_mode <- function(x) {
  unique_values <- unique(x)
  tab <- tabulate(match(x, unique_values))
  unique_values[tab == max(tab)]
}

categorical_cols <- sapply(f3, is.factor)
mode_values <- f3 %>%
  summarize(across(.cols = names(f3)[categorical_cols], .fns = get_mode))

# Combine the mean and mode results
aggregated_data <- bind_cols(mean_values, mode_values)
dat <- aggregated_data 

View(dat)
weekend<-c("Sat","Sun")
# Example string
string <- f3$Time[1]
# Extract the day of the week using regular expression
day_of_week <- sub("^.*([A-Za-z]{3}),.*$", "\\1", string)

# Print the day of the week
print(day_of_week)
 t1 <- dat %>% mutate(Weekday=ifelse(day_of_week %in% weekend,0, 1))
# Set the column names
#colnames(aggregated_data) <- colnames(f3)


###### Another site
# Define the base URL for the second website
base_url2 <- "https://www.timeanddate.com/weather/usa/washington-dc/historic"

# Initialize an empty data frame to store the tables
tables1 <- data.frame()

# Iterate over the years
for (j in c(2021, 2022, 2023)) {
  # Iterate over the months
  for (i in 1:12) {
    
  # Create the URL for the second website
  url2 <- paste0(base_url2, "?month=", i,"&year=", j)
  
  # Scrape the table from the second website
  f1 <- url2 %>% 
    read_html()%>% 
    html_nodes("table") %>% 
    html_table(fill = TRUE) 
  
  ###Selectind dataframe of interest
  f2=f1[[2]]
  
  # Remove the header row and make the first row the new header
  new_header <- as.character(f2[1, ])
  f2 <- f2[-1, ]
  colnames(f2) <- new_header
  
  # Remove columns 2 and 6
  f2 <- f2[, -c(2, 6)]
  
  #### Remove last row
  f2 <- f2[-nrow(f2),]
  
  # Remove symbols, units, and characters from specific columns
  cols_to_clean <- c(2, 4, 5, 6, 7)
  
  clean_column_values <- function(column) {
    gsub("[^0-9.]+", "", as.character(column))
  }
  
  f3 <- f2 %>%
    mutate(across(all_of(cols_to_clean), clean_column_values))
  
  
  #### Aggregating the data
  
  f3$Temp <-as.numeric(f3$Temp)
  f3$Wind <-as.numeric(f3$Wind)
  f3$Humidity <-as.numeric(f3$Humidity)
  f3$Barometer <-as.numeric(f3$Barometer)
  f3$Visibility <-as.numeric(f3$Visibility)
  f3$Weather <- as.factor(f3$Weather)
  
  # Calculate mean for numeric columns
  numeric_cols <- sapply(f3, is.numeric)
  mean_values <- f3 %>%
    summarize(across(.cols = names(f3)[numeric_cols], .fns = mean, na.rm = TRUE))
  
  # Calculate mode for categorical columns
  
  # Function to calculate the mode
  get_mode <- function(x) {
    unique_values <- unique(x)
    tab <- tabulate(match(x, unique_values))
    unique_values[tab == max(tab)]
  }
  
  categorical_cols <- sapply(f3, is.factor)
  mode_values <- f3 %>%
    summarize(across(.cols = names(f3)[categorical_cols], .fns = get_mode))
  
  # Combine the mean and mode results
  aggregated_data <- bind_cols(mean_values, mode_values)
  dat <- aggregated_data 
  
  
  # Combine the table with the existing tables
  tables1 <- bind_rows(tables1, dat)
  
  # Do further processing or analysis with the data frame for the current year
  # ...
  
  # Print the table for the current year
  # cat("Table for year", j, ":\n")
  # print(df)
  }
}

# Display the combined tables
View(tables1)





#####Scraping the weather element from www.timeanddate.com
#################################################################################

library(dplyr)

# Define the base URL for the second website
base_url2 <- "https://www.timeanddate.com/weather/usa/washington-dc/historic"

# Initialize an empty data frame to store the tables
tables1 <- data.frame()

# Iterate over the years
for (year in 2020:2023) {
  # Iterate over the months
  for (month in 1:12) {
    # Get the number of days in the current month and year
    days_in_month <- seq(as.Date(paste(year, month, "01", sep = "-")),
                         by = "month",
                         length.out = 2)[2] - 1
    
    # Iterate over the days
    for (day in 1:day(days_in_month)) {
      print(day)
      # Create the URL for the second website
      url2 <- paste0(base_url2, "?month=", month, "&year=", year, "&hd=", year, sprintf("%02d", month), sprintf("%02d", day))
      
      # Scrape the table from the second website
      f1 <- url2 %>% 
        read_html() %>% 
        html_nodes("table") %>% 
        html_table(fill = TRUE) 
      
      ### Select the dataframe of interest
      f2 <- f1[[2]]
      
      # Remove the header row and make the first row the new header
      new_header <- as.character(f2[1, ])
      f2 <- f2[-1, ]
      colnames(f2) <- new_header
      
      # Remove columns 2 and 6
      f2 <- f2[, -c(2, 6)]
      
      #### Remove last row
      f2 <- f2[-nrow(f2), ]
      
      # Remove symbols, units, and characters from specific columns
      cols_to_clean <- c(2, 4, 5, 6, 7)
      
      clean_column_values <- function(column) {
        gsub("[^0-9.]+", "", as.character(column))
      }
      
      f3 <- f2 %>%
        mutate(across(all_of(cols_to_clean), clean_column_values))
      
      #### Aggregating the data
      f3$Temp <- as.numeric(f3$Temp)
      f3$Wind <- as.numeric(f3$Wind)
      f3$Humidity <- as.numeric(f3$Humidity)
      f3$Barometer <- as.numeric(f3$Barometer)
      f3$Visibility <- as.numeric(f3$Visibility)
      f3$Weather <- as.factor(f3$Weather)
      
      # Calculate mean for numeric columns
      numeric_cols <- sapply(f3, is.numeric)
      mean_values <- f3 %>%
        summarize(across(.cols = names(f3)[numeric_cols], .fns = mean, na.rm = TRUE))
      
      # Calculate mode for categorical columns
      
      # Function to calculate the mode
      get_mode <- function(x) {
        unique_values <- unique(x)
        tab <- tabulate(match(x, unique_values))
        unique_values[tab == max(tab)]
      }
      
      categorical_cols <- sapply(f3, is.factor)
      mode_values <- f3 %>%
        summarize(across(.cols = names(f3)[categorical_cols], .fns = get_mode))
      
      # Combine the mean and mode results
      aggregated_data <- bind_cols(mean_values, mode_values)
      dat <- aggregated_data 
      weekend<-c("Sat","Sun")
      # Example string
      string <- f3$Time[1]
      # Extract the day of the week using regular expression
      day_of_week <- sub("^.*([A-Za-z]{3}),.*$", "\\1", string)
      
      # Print the day of the week
      print(day_of_week)
      tab <-dat %>% mutate(Year=year, Month=month,day=day,Weekday=ifelse(day_of_week %in% weekend,0, 1))
      # Combine the table with the existing tables
      tables1 <- bind_rows(tables1, tab)
      
    }
  }
    
}

Weather_data <- tables1[1:1405,]
# Mutate the "season" column based on the "month" column
#View(tables1)
Weath <- Weather_data %>% 
  mutate(season = case_when(
    Month %in% c(3, 4, 5) ~ "Spring",
    Month %in% c(6, 7, 8) ~ "Summer",
    Month %in% c(9, 10, 11) ~ "Autumn",
    Month %in% c(12, 1, 2) ~ "Winter"
  ))


weath1 <- Weath %>% 
  mutate(date = as.Date(paste(Year, Month, day, sep = "-")))

#View(Weath_data)

#new_dat<-bind_rows(Weath,Weath_data)

#View(new_dat)

write.csv(weath1, file = "~/Desktop/weather_data.csv")



########################################################################
### Required data
Weatherdata<-read_csv("weather_data.csv")
bikeed <- read_csv("bike_rental.csv")

wetdata <-weath1[106:1391,]


wetdata1 <- wetdata[!duplicated(wetdata$date), ]

#########################################################################

paper_data <- bind_cols(bikeed[,-1],wetdata1[,-12])

View(paper_data)
write.csv(paper_data, file = "~/Desktop/bikew_data.csv")





###################################################################

### Holiday data
# Store the date strings in a vector
date_strings <- c("January 1, 2020", "January 20, 2020", "February 17, 2020", "April 8, 2020",
                  "May 25, 2020", "June 19, 2020", "July 4, 2020", "September 7, 2020",
                  "October 12, 2020", "November 11, 2020", "November 26, 2020", "December 25, 2020",
                  "January 1, 2021", "January 18, 2021", "February 15, 2021", "April 16, 2021",
                  "May 31, 2021", "June 18, 2021", "July 4, 2021", "September 6, 2021",
                  "October 11, 2021", "November 11, 2021", "November 25, 2021", "December 25, 2021",
                  "January 1, 2022", "January 17, 2022", "February 21, 2022", "April 15, 2022",
                  "May 30, 2022", "June 19, 2022", "July 4, 2022", "September 5, 2022",
                  "October 10, 2022", "November 11, 2022", "November 24, 2022", "December 25, 2022")

# Convert the date strings to Date objects
holidays <- as.Date(date_strings, format = "%B %d, %Y")

# Store the date strings for 2023 holidays
date_strings_2023 <- c("January 1, 2023", "January 2, 2023", "January 16, 2023", "February 20, 2023",
                       "April 17, 2023", "May 29, 2023", "June 19, 2023", "July 4, 2023",
                       "September 4, 2023", "October 9, 2023", "November 10, 2023",
                       "November 23, 2023", "December 25, 2023")

# Combine the existing holidays with 2023 holidays
holidays <- c(holidays, as.Date(date_strings_2023, format = "%B %d, %Y"))

###### Adding holidays to the data
paper_data1 <-paper_data %>% 
  mutate(holiday =ifelse(date %in% holidays,1,0)) %>% 
  mutate(workinday = ifelse(Weekday==0,0, ifelse(holiday==1,0,1)))

View(paper_data1)

write.csv(paper_data1, file = "~/Desktop/paperbike_data.csv")

summary(paper_data1$Weather)
