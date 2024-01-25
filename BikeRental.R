
#Title: "Modelling the daily dynamics in bike rental system using weather and environmental conditions- A semi-paramteric Approach"
#author: "Christopher Odomm, A. Boateng Sarah Fobi, Daniel Mposa" #

rm(list = ls())

## Loading reqiure packages
library(readr)
library(ggplot2)
library(yarrr)
library(tidyverse)
library(plotly)
library(mgcv)
library(pander)
library(gratia)
library(DHARMa)
library(leaflet)
library(dbscan)

## Importinng the processed data
dat <- read_csv("paperbike_data.csv")
head(dat)

##Renaming some columns
colnames(dat)[colnames(dat)=="Wind"] = "windspeed"
colnames(dat)[colnames(dat)=="Temp"] <- "temp"
colnames(dat)[colnames(dat)=="Humidity"] <- "humidity"
colnames(dat)[colnames(dat)=="Barometer"] <- "atmospres"
colnames(dat)[colnames(dat)=="workinday"] <- "workingday"
colnames(dat)[colnames(dat)=="Visibility"] <- "visibility"
colnames(dat)[colnames(dat)=="Weather"] <- "weather"
colnames(dat)[colnames(dat)=="Year"] <- "year"
colnames(dat)[colnames(dat)=="Weekday"] <- "weekday"
colnames(dat)[colnames(dat)=="...1"] <- "instant"
colnames(dat)[colnames(dat)=="Month"] <- "month"
colnames(dat)

## Converting categorical variables to factor.
dat$season <- as.factor(dat$season)
dat$holiday <- as.factor(dat$holiday)
dat$weekday <- as.factor(dat$weekday)
dat$workingday <- as.factor(dat$workingday)
dat$weather <- as.factor(dat$weather)
#dat$workingday <- as.factor(dat$workingday)
dat$month <- as.factor(dat$month)
dat$year <- as.factor(dat$year)
dat$day <-as.factor(dat$day)

##Performing exploratory Data analysis
fig1 <- dat %>%  ggplot(aes(x = total_rentals)) +
  geom_histogram(aes(y = ..ncount..), bins = 20, fill = "steelblue", col = "black") +
  theme_bw()+
  labs(title = "Distribution of Count of Total Rental Bikes  ",
       x = "Count of Total Bikes Rented ",
       y = "Density");fig1


par(mfrow=c(1,3))
dat$total_rentals %>% hist(col = "palegreen", main = "Total Rental")
dat$registered %>% hist(col = "palegreen", main = "Registered Rental")
dat$casual %>% hist(col = "palegreen", main = "Casual Rental")

fig2 <-dat %>%  ggplot(aes(x = month, y = total_rentals, col = season)) +
  geom_boxplot() +
  theme_bw()+
  labs(title = "Boxplot of Count of Total Rental Bikes Against month",
       x = "Month " ,
       y = "count of total rental bikes");fig2

fig3 <- dat %>% ggplot(aes(x = season, y = total_rentals, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#00b386", "#0090ac","#10b0ec","#0000ac"))+
  theme_bw()+
  labs(title = "Scatterplot of Count of Total Rental Bikes Against season",
       x = "season " ,
       y = "count of total rental bikes");fig3

fig4 <- dat %>%  ggplot(aes(x = temp, y =total_rentals, col = visibility)) +
  geom_point() +
  geom_smooth() +
  theme_bw()+
  labs(title = "Scatterplot of Count of Total Rental Bikes Against Temperature by Visibility",
       x = "temperature  " ,
       y = "count of total rental bikes");fig4

fig41 <- dat %>%  ggplot(aes(x = temp, y =total_rentals, col = humidity)) +
  geom_point() +
  geom_smooth() +
  theme_bw()+
  labs(title = "Scatterplot of Count of Total Rental Bikes Against Temperature by Visibility",
       x = "temperature  " ,
       y = "count of total rental bikes");fig41

pirateplot(formula = total_rentals ~ holiday + workingday ,
           data = dat,
           cex.names = 0.75,
           main = "Pirateplot of songbirds metabolic rate",
           xlab = "Age",
           ylab = "Count of total reantal bikes")

fig6 <-dat %>%  ggplot(aes(x = registered, y = total_rentals)) +
  geom_point() +
  geom_smooth() +
  theme_bw()+
  labs(title = "Scatterplot of Count of Total Rental Bikes Against Count of Registered Users",
       x = "Count of registered users  " ,
       y = "count of total rental bikes");fig6

fig7 <- dat %>% ggplot(aes(x = casual, y =total_rentals )) +
  geom_point() +
  geom_smooth() +
  theme_bw()+
  labs(title = "Scatterplot of Count of Total Rental Bikes Against Count of Casual Users",
       x = "Count of casual users  " ,
       y = "count of total rental bikes");fig7

fig8 <- dat %>% ggplot(aes(x=windspeed, y=total_rentals, col=weekday))+
  geom_point();fig8

# Gather the columns related to "casual" and "registered"
gathered_df <- dat %>%
  select(instant, date, casual, registered, humidity,windspeed,workingday) %>%
  gather(key = "rental_type", value = "rentals", casual:registered)

# Check the gathered data frame
fig9 <- gathered_df %>%
  ggplot(aes(x = rental_type, y = rentals, fill = workingday)) +
  geom_boxplot() +
  xlab("membership type") +
  scale_fill_manual(values = c("green", "yellow"));fig9

#### Scatterplot matrix
fig10 <- dat %>% dplyr::select(temp,humidity, visibility,windspeed,casual,registered,total_rentals) %>% pairs();fig10


correlation_matrix <- cor(dat[, c("total_rentals", "temp", "windspeed", "humidity", "visibility")])
print(correlation_matrix)
library(corrplot)
corrplot(correlation_matrix)

#### Descriptive 
summary(dat[, c("total_rentals", "temp", "windspeed", "humidity", "visibility")]) %>% pander()

gat_df <- dat %>%
  select(instant, date, casual, registered, humidity,windspeed,holiday,day) %>%
  gather(key = "rental_type", value = "rentals", casual:registered)
### The goal is to visualise the day effect on rentals
#gat_df$day <-as.numeric(gat_df$day)
fig11 <- gat_df %>% ggplot(aes(x=day, y=rentals, col=rental_type))+
  geom_boxplot();fig11

## Location of stations
# Create a leaflet map
map <- leaflet(dat) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, popup = ~paste("Total Rentals: ", total_rentals))

# Display the map
map

# Spatial density plot for the total

ggplot(dat, aes(x = longitude, y = latitude, color = total_rentals)) +
  geom_point(size = 6) +
  labs(title = "Spatial Density of Bike Rentals", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Spatial density plot for registered

ggplot(dat, aes(x = longitude, y = latitude, color = registered)) +
  geom_point(size = 6) +
  labs(title = "Spatial Density of Bike Rentals", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Spatial density plot for casual 

ggplot(dat, aes(x = longitude, y = latitude, color = casual)) +
  geom_point(size = 6) +
  labs(title = "Spatial Density of Bike Rentals", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Spatial clustering
coordinates <- dat[, c("longitude", "latitude")]
### Finding suitable DBSCAN parameters
kNNdistplot(coordinates, minPts = 2)
abline(h=0.0002, col="red", lty=3)

dbscan_result <- dbscan(coordinates, eps = 0.0004, minPts = 5)
clusters <- dbscan_result$cluster

ggplot(dat, aes(x = longitude, y = latitude, color = factor(clusters))) +
  geom_point(size = 3) +
  labs(title = "Spatial Clustering of Bike Rentals", x = "Longitude", y = "Latitude") +
  theme_minimal()

###+++++++++++++++++++++++++ END OF EDA ++++++++++++++++++###########################

## Modelling total rentals condition on weather conditions and location

##Check conditions(Assumptions) of poisson regression
mean(dat$total_rentals)
var(dat$total_rentals)

##Checking for dispersion using the dispersiontest

dat$day <-as.numeric(dat$day)
mod.initial <- gam(total_rentals~visibility+windspeed+season+
                     workingday+year+s(month, bs="re")+
                     s(longitude,latitude)+s(temp)+
                     s(temp, by=workingday)+s(humidity), data = dat, family = poisson)
summary(mod.initial)

dispersiontest(mod.initial) ##. True dispersion is greater than 1 so we have over dispersion


#### modeling using Quasi-poisson

dat$day <-as.numeric(dat$day)
mod1 <- gam(total_rentals~visibility+windspeed+season+workingday+year+s(month, bs="re")+s(longitude,latitude)+s(temp)+s(temp, by=workingday)+s(humidity), data = dat, family = quasipoisson)

summary(mod1)

## Table 1: Parameteric Estimates
para1 <- summary(mod1)
para1$p.table %>% pander()

## Table 2: Non Parameteric Estimates
nonpara1 <- summary(mod1)

nonpara1$s.table %>% pander(caption = "Deviation explained =87%")

## Model1 Performance measure
perm1 <- summary(mod1)
perm1$dev.expl*100

## Drawing the effect 
draw(mod1, select = 2) ### Map

## Smooths for other predictors
draw(mod1, select = c(3,4,5,6))

## Checking number of knots
gam.check(mod1)

appraise(mod1)

## Model2 for registered rentals
mod2 <- gam(registered~visibility+windspeed+season+workingday+year+s(month, bs="re")+s(longitude,latitude)+s(temp)+s(temp, by=workingday)+s(humidity), data = dat, family = quasipoisson)

summary(mod2)


## Table 3: Parameteric Estimates
para2 <- summary(mod2)
para2$p.table %>% pander()

## Table 4: Non-Parameteric Estimates
nonpara2 <- summary(mod2)
nonpara2$s.table %>% pander()


draw(mod2,select = 2)
draw(mod2,select = c(3,4,5,6))
appraise(mod2)

### Model3 Casual rentals
mod3 <- gam(casual~visibility+windspeed+season+workingday+year+s(month, bs="re")+s(longitude,latitude)+s(temp)+s(temp, by=workingday)+s(humidity), data = dat, family = quasipoisson)

summary(mod3)

## Table 5: Parameteric Estimates
para3 <- summary(mod3)
para3$p.table %>% pander()


## Table 6: Parameteric Estimates

nonpara3 <- summary(mod3)
nonpara3$s.table %>% pander()

draw(mod3, select = 2)
draw(mod3, select = c(3,4,5,6))
appraise(mod3)
## Check efficiency of Knots
gam.check(mod3)


## Comparng model 2 and model 3
a=para3$p.coeff
b=para2$p.coeff
est <-names(para2$p.coeff)
coff1 <- tibble(a,b,est)
colnames(coff1) <- c("casual", "registered", "Parametric Est")
coff1

coff1 %>%
  gather('Type', 'Effect', -`Parametric Est`)%>%
  dplyr::filter(`Parametric Est`!="(Intercept)")%>% 
  ggplot(aes(x = Effect, y=`Parametric Est`)) +
  geom_col(aes(fill = Type), position = 'dodge')


