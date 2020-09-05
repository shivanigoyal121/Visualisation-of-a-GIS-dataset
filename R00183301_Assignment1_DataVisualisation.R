### ASSIGNMENT 1(DATA VISUALISATION AND FACTOR ANALYSIS)
### SUBMITTED BY - SHIVANI GOYAL
### R00183301


#Installing and loading all the Packages 
install.packages("VIM")
library(VIM)
install.packages("tidyr")
library(tidyr)
install.packages("stringr")
library(stringr)
install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)
library(reshape2)
library(dplyr)
install.packages("maps")
library(maps)


#reading the dataset
cneos_fireball_data <- read.csv("F:/MS- SEM2/Data Visualisation/cneos_fireball_data.csv")
View(cneos_fireball_data)

fireball_data <- cneos_fireball_data

#to check the structure of data
str(fireball_data)



###PREPROCESSING OF DATA

#to split Peak.brightness date time into seperate columns
fireball_data1 <- fireball_data %>% separate(Peak.Brightness.Date.Time..UT., c("Year", "Month","Date and Time"), "-") %>%
  separate("Date and Time", c("Date", "Time")," ")
str(fireball_data1)

##to check the number of missing values
sum(is.na(fireball_data1))
#2660 missing values

#Visualising missing values
aggr(fireball_data1, numbers = TRUE,prop = FALSE,  cex.lab = 1.2)

#Here. If velocity is missing, then vx, vy, vz data is missing. 
#410 data is missing for combination of velocity, vx,vy,vz and altitude.

#conversion of energy from Joules to Kilotons
fireball_data1$Total.Radiated.Energy..Kt. <- round(fireball_data1$Total.Radiated.Energy..J./(4.185*10^12),3)
str(fireball_data1)


##Latitude and Longitude ranges and directions

#adds the feature Lat.NS if the direction is North or South respectively
fireball_data1$Lat.NS <- as.factor(ifelse(str_sub(fireball_data1$Latitude..deg.., -1) == "N", "North", ifelse(str_sub(fireball_data1$Latitude..deg.., -1) == "S","South","")))
#adds the feature Latitude.Degree containing the degree of Latitude
fireball_data1$Latitude.Degree <- as.numeric(str_extract(fireball_data1$Latitude..deg.., "\\d+\\.*\\d*"))


#to convert south direction degree to negative values
fireball_data1$Latitude.Degree <- ifelse(fireball_data1$Lat.NS == "South", -abs(fireball_data1$Latitude.Degree), fireball_data1$Latitude.Degree)
#adds the feature 'Latitude.Grid' which is Latitude range
fireball_data1$Latitude.Grid <- cut(fireball_data1$Latitude.Degree, seq(-90, 90, 30))

#adds the feature Lat.EW if the direction is East or West respectively
fireball_data1$Lng.EW <-as.factor(ifelse(str_sub(fireball_data1$Longitude..deg.., -1) == "W", "West", ifelse(str_sub(fireball_data1$Longitude..deg.., -1) == "E","East","")))
#adds the feature Longitude.Degree containing the degree of Longitude
fireball_data1$Longitude.Degree <-as.numeric(str_extract(fireball_data1$Longitude..deg.., "\\d+\\.*\\d*"))

#displays the structure of dataset
str(fireball_data1)

#to convert west direction degree to negative values
fireball_data1$Longitude.Degree <- ifelse(fireball_data1$Lng.EW == "West", -abs(fireball_data1$Longitude.Degree), fireball_data1$Longitude.Degree)
#adds the feature 'Latitude.Grid' which is Longitude range
fireball_data1$Longitude.Grid <- cut(fireball_data1$Longitude.Degree, seq(-180, 180, 30))

#adds a new feature 'Direction.NSEW' having the direction 
fireball_data1$Direction.NSEW <- as.factor(paste(fireball_data1$Lat.NS, fireball_data1$Lng.EW))

#displays the structure of dataset
str(fireball_data1)


##1st Graph

#Visualise the Bolide's Radiated Energy at peak brightness, into Latitude & Longitude grids
fireball_data1 %>% filter(!is.na(Latitude.Grid)) %>% ggplot(aes(y = Latitude.Grid, x = Longitude.Grid))+
  geom_jitter(aes(size = sqrt(Total.Radiated.Energy..Kt.)), color = "green", alpha = 0.20)+
  geom_text(aes(label=ifelse(sqrt(Total.Radiated.Energy..Kt.)>8, "Most Energy Radiated Region","")), vjust = "outward", size = 2.5)+
  facet_grid( Lat.NS ~ Lng.EW, scales = "free", switch = "both")+
  labs(title = "Bolides Radiated Energy at Lat. & Long.", x = "Longitude's Range", y = "Latitude's Range")+
  theme(legend.position = "top")+
  scale_size_continuous(name = "Sqrt[Total Radiated Energy (kt)]")




##2nd graph -  To Visualise Relationship between Diameter of bolide on Altitude(km) and Total Radiated Energy(kt)

#displays the Altitude(km) based energy gradients 
Altitude.Energy.Gradient.Table <- matrix(c(1.0, 0.35, 1.0, 0.40, 0.9, 0.40, 0.9, 0.50, 0.7, 0.50, 0.4, 0.50), ncol = 2, byrow = TRUE)
colnames(Altitude.Energy.Gradient.Table) <- c("Blast Efficiency Factor","Thermal Radiation percent")
rownames(Altitude.Energy.Gradient.Table) <- c("Altitude(km) <= 12.192", "12.192 < Alt(km) <= 27.432", "27.432 < Alt(km) <= 30.480", "30.480 < Alt(km) <= 36.576", "36.576 < Alt(km) <= 45.720", "Altitude(km) > 45.720")
Altitude.Energy.Gradient.Table <- as.table(Altitude.Energy.Gradient.Table)
Altitude.Energy.Gradient.Table


#Add and calculates the feature Before explosion of Bolide's Total Energy
fireball_data1  <- 
  mutate(fireball_data1 , Before_explodingTotEnergy.kt = round(
    if_else(Altitude..km. <= 12.192, (Total.Radiated.Energy..Kt./(0.35*1)),
            if_else(Altitude..km. > 12.192 & Altitude..km. <= 27.432, (Total.Radiated.Energy..Kt./(0.40*1)),
                    if_else(Altitude..km. > 27.432 & Altitude..km. <= 30.48, (Total.Radiated.Energy..Kt./(0.4*0.9)),
                            if_else(Altitude..km. > 30.48 & Altitude..km. <= 36.576, (Total.Radiated.Energy..Kt./(0.5*0.9)),
                                    if_else(Altitude..km. > 36.576 & Altitude..km. <= 45.72, (Total.Radiated.Energy..Kt./(0.5*0.7)), (Total.Radiated.Energy..Kt./(0.5*0.4))))))), 2))

#checks the structure of fireball_data1 dataset
str(fireball_data1)

#Add and calculates the Radius(m) feature using Table02 information
fireball_data1 <- 
  mutate(fireball_data1, Radius.m = round(
    if_else(Altitude..km. <= 12.192 , (110*0.3048)*((Total.Radiated.Energy..Kt./(0.35*1))^(0.4)),
            if_else(Altitude..km. > 12.192 & Altitude..km. <= 27.432, (110*0.3048)*((Total.Radiated.Energy..Kt./(0.40*1))^(0.4)),
                    if_else(Altitude..km. > 27.432 & Altitude..km. <= 30.48, (110*0.3048)*((Total.Radiated.Energy..Kt./(0.4*0.9))^(0.4)),
                            if_else(Altitude..km. > 30.48 & Altitude..km. <= 36.576, (110*0.3048)*((Total.Radiated.Energy..Kt./(0.5*0.9))^(0.4)),
                                    if_else(Altitude..km. > 36.576 & Altitude..km. <= 45.72,(110*0.3048)*((Total.Radiated.Energy..Kt./(0.5*0.7))^(0.4)), (110*0.3048)*((Total.Radiated.Energy..Kt./(0.5*0.4))^(0.4))))))), 2))

#checks the structure of fireball_data1 dataset
str(fireball_data1)

#Add and calculates the Diameter upper and lower limit feature
fireball_data1 <- mutate(fireball_data1, diameter.HiLimit = round(Radius.m/10,2))
fireball_data1 <- mutate(fireball_data1, diameter.LoLimit = round(Radius.m/15,2))

#checks the structure of fireball_data1 dataset
str(fireball_data1)


#Visualising the Relationship between Diameter of bolide before exploding on Altitude(km) and Total Radiated Energy(kt)
fireball_data1 %>% filter(!is.na(Altitude..km.))%>% ggplot(aes(x = Altitude..km., y = Total.Radiated.Energy..Kt.))+
  geom_jitter(aes(size = (diameter.LoLimit+diameter.HiLimit)/2),color = "blue",alpha = 0.20)+
  geom_smooth(model = lm, color = "yellow")+
  geom_text(aes(label=ifelse(Total.Radiated.Energy..Kt.>80, "Most Energy Radiated Region","")), vjust = "inward", size = 3.5)+
  labs(title = "Relation between Total Radiated Energy(kt) & Altitude(km)", x = "Altitude(km)", y = "Log of Total Radiated Energy(kt)")+
  theme_light()+theme(legend.position = "top")+
  scale_size_continuous(name = "Bolide's Average Diameter (m)")+
  scale_y_log10()



##OBSERVATIONS


##GRAPH 03:- To Visualise the Relationship between Total Radiated Energy with Altitude(km) Direction Wise 
fireball_data1 %>% filter(!is.na(Altitude..km.)) %>% ggplot(aes(x = Altitude..km., y = Total.Radiated.Energy..Kt.))+
  geom_jitter(color = "navyblue",alpha = 0.72)+
  geom_smooth(model = lm,color = "red")+
  facet_wrap(~ Direction.NSEW, scales = "free")+
  labs(title = "Relation between Radiated Energy with Altitude Direction Wise", x = "Altitude(km) at Peak Brightness", y = "Total Radiated Energy")+
  scale_y_log10()


##GRAPH 4:- To Visualise the Relationship between Impacted Energy with Altitude(km) Direction Wise 
fireball_data1 %>% filter(!is.na(Altitude..km.)) %>% ggplot(aes(x = Altitude..km., y = Calculated.Total.Impact.Energy..kt.))+
  geom_jitter(color = "orange",alpha = 0.72)+
  geom_smooth(model = lm)+
  facet_wrap(~ Direction.NSEW, scales = "free")+
  labs(title = "Relation between Radiated Energy with Altitude Direction Wise", x = "Altitude(km) at Peak Brightness", y = "Total Radiated Energy")+
  scale_y_log10()



##GRAPH 5: To Visualise the relation of Bolides' Radius, Altitude & Velocity 

#displays Bolides' Air Bursts' radius (m) at various Altitudes (km) above geoid vs pre-impact Velocities (km/s) at peak brightness
fireball_data1 %>% filter(!is.na(Altitude..km.) & !is.na(Velocity..km.s.))%>% ggplot(aes(y= Velocity..km.s., x= Altitude..km.))+
  geom_point(aes(size = log(Radius.m)), color = "violet", alpha = 0.50)+
  geom_smooth(model = lm,color = "red")+
  facet_grid(~ Direction.NSEW)+
  labs(title ="Bolides' radius at various Altitudes & Velocity", x = "Velocity (km/s) at Airburst", y = "Altitude (km) at Airburst")+theme(legend.position = "top")+scale_size_continuous(name = "Log of Bolide's Airburst's radius (m)")



##GRAPH 6:- To Visualise the number of Bolides' observed based on Impacted Energy with respective directions

#Frequency Of Bolides' with Missing Data
fireball_data1 %>% filter(is.na(Altitude..km.)) %>% ggplot(aes(x = Calculated.Total.Impact.Energy..kt. , colour = Direction.NSEW))+
  geom_histogram(bins = 100)+
  facet_wrap(~ Direction.NSEW, scales = "free")+
  scale_x_log10()+
  labs(title = "Frequency of Bolides with missing data", y = "Frequency", x = "x - axis on Log10 scale")



#Removes the missing data from the datset and stores on 'nonMissingData'
nonMissingData <- na.omit(fireball_data1)
#filtes the only required columns
nonMissingData <- nonMissingData %>% select("Year","Month","Date",7,8,9,10,11,13,14,16,17,19,20,21)
#displays the structure of 'nonMissingData' dataset
str(nonMissingData)
View(nonMissingData)


##GRAPH 7 - Univariate Analysis on 'Year'

min(nonMissingData$Year) 
max(nonMissingData$Year)

#converts the 'Year' feature as numeric datatype
nonMissingData$Year <- as.numeric(nonMissingData$Year)

#plots the histogram of Year
h <- hist(nonMissingData$Year, plot = FALSE)
plot(h, xaxt = "n", xlab = "Year", ylab = "Counts",
     main = "No Of Bolides Observed per Year ", col = "green", border = "blue")
axis(1, h$mids, labels = levels(nonMissingData$Year), tick = FALSE , padj= -1.5)

#Most of the events were observed in 2015
#Least were observed in 1999.



##GRAPH 8 - Univariate Analysis on 'Month' 

#converts the 'Month' feature as numeric datatype
nonMissingData$Month <- as.numeric(nonMissingData$Month)

#plots the Histogram of Month
hist(nonMissingData$Month, col='pink',ylab = 'No of Bolides', xlab='Months',main = 'No of Bolides observed per month', labels=levels(nonMissingData$Month), xaxt='n')
axis(1, at=unique(nonMissingData$Month), labels=levels(nonMissingData$Month))

#Most of the cases are observed in starting month i.e of January.


##GRAPH 9 - Univariate Analysis on 'Months for year 2015'

#filter outs the data for 2015 year
year2015 <- nonMissingData %>% filter(Year == 2015)

#converts the 'Month' feature as numeric datatype
nonMissingData$Month <- as.numeric(nonMissingData$Month)

#plots the Histogram of Months of 2015 Year
hist(year2015$Month,breaks = 12, col='yellow',border = 'blue', ylab = 'No of Bolides', xlab='Months',main = 'No of Bolides observed per month for 2015 year', labels=levels(year2015$Month), xaxt='n')
axis(1, at=unique(year2015$Month),xlim = c(1,12), labels=levels(year2015$Month))

#In the starting months of the year 2015, were the most number of fireballs observed.


##GRAPH 10 - To visualise for non missing data, the number of Bolides' observed based on Impacted Energy with respective directions
#Frequency Of Bolides' with Non - Missing Data
nonMissingData %>% ggplot(aes(x = Calculated.Total.Impact.Energy..kt. , colour = Direction.NSEW))+
  geom_histogram(bins = 100)+
  facet_wrap(~ Direction.NSEW, scales = "free")+
  scale_x_log10()+
  labs(title = "Frequency of Bolides for non - missing data", y = "Frequency", x = "x - axis on Log10 scale")


##GRAPH 11- To visualise the Bolides' observed Altitude w.r.t Latitude and Longitude Grid
#Bolide's observed Altitude Raster map
p <- ggplot(nonMissingData, aes(Latitude.Grid , Longitude.Grid )) +
  geom_raster(aes(fill=Altitude..km.)) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  labs(x="Latitude",
       y="Longitude",
       title = "Bolide's observed Altitude Raster map ",
       fill = "Altitude") +
  theme(text = element_text(family = 'Fira Sans'),
        plot.title = element_text(hjust = 0.5))

ggplotly(p)


##GRAPH 12- To visualise the Fireball's events on world map yearly

#Visualising Fireballs on the world map yearly displaying Latitude,Longitude and Radiated Energy
p1 <- ggplot()+
  theme_minimal()+
  geom_polygon(data = map_data("world"), aes(x=long, y = lat, group = group), fill="navyblue", color = "navyblue",alpha=0.15) +
  geom_point(data=fireball_data1 , aes(x=Longitude.Degree, y=Latitude.Degree, frame = Year, size = 5*Total.Radiated.Energy..Kt.), shape = 21, col = "coral", alpha = 0.72, fill = alpha("cornsilk", 0.1)) +
  geom_point(data=fireball_data1 , aes(x=Longitude.Degree, y=Latitude.Degree, frame = Year, size = 2*Total.Radiated.Energy..Kt.), shape = 21, col = "firebrick", alpha = 0.81, fill = alpha("yellow", 0.15)) +
  scale_y_continuous(breaks = c(-90, -60, -30, 0, 30, 60, 90)) + 
  scale_x_continuous(breaks = c(-180, -150, -120, -90, -60, -30, 0, 30, 60, 90, 120, 150, 180))+
  labs(x = "Longitude", y = "Latitude", title = "Fireballs observed at peak Brightness")


ggplotly(p1) %>% animation_opts(frame = 2000, transition = 100, redraw = TRUE) %>% 
  animation_slider(currentvalue = list(prefix = "Year: ", font = list(color="navyblue")))










