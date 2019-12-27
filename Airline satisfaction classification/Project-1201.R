# Load the necessary pacakges
library(dplyr)
library(ggplot2)
library(ggrepel)
require(ggmap)
require(maps)
library(arules)
library(arulesViz)
library(kernlab)
library("ggthemes")
library("RColorBrewer")

##### Data Acquisition
# read the data
RawData <- read.csv(file="/Users/harperhe/Documents/IST 687/Project/Satisfaction Survey.csv", header=TRUE, sep=",")
str(RawData)
# Find the columns containing NAs
colSums(is.na(RawData))

##### Data Cleansing and Munging
# remove 9 unusual satisfaction values
CleanData <- RawData[ ! RawData$Satisfaction %in% c('4.00.5', '4.00.2.00',1.5,2.5,3.5,4.5), ]
# Remove the Airline.Code and Flight.date attribute
CleanData <- CleanData[,-(15:16)]
# check the data
str(CleanData)
# Delete the white spece
CleanData$Airline.Name <- gsub('\\s+', '', CleanData$Airline.Name)
# Find the columns containing NAs
colSums(is.na(CleanData))
# transform attribute "satisfaction" to numeric
CleanData$Satisfaction <- as.numeric(as.character(CleanData$Satisfaction))

# Build subsets and clean NAs
# Build a subset for customers whose flights have been cancelled
CancelledSubset <- CleanData[which(CleanData$Flight.cancelled == "Yes"), ]
str(CancelledSubset)
colSums(is.na(CancelledSubset))

# Build a subset for customers whose flights have not been cancelled
UncancelledSubset <- CleanData[which(CleanData$Flight.cancelled == "No"), ]
str(UncancelledSubset)
colSums(is.na(UncancelledSubset))

# Remove the rows containing NAs
Satisfaction <- na.omit(UncancelledSubset, cols=c("Arrival.Delay.in.Minutes", "Flight.time.in.minutes"))
str(Satisfaction)

##### Data Transformation
# Group some attributes for descriptive analysis and linear regression
Satisfaction$Age.Group <-ifelse(Satisfaction$Age < 18,'15-18',
                                ifelse(Satisfaction$Age >=18 & Satisfaction$Age <=24,'18-24',
                                       ifelse(Satisfaction$Age >=25 & Satisfaction$Age <=34,'25-34',
                                              ifelse(Satisfaction$Age >=35 & Satisfaction$Age <=44,'35-44',
                                                     ifelse(Satisfaction$Age >=45 & Satisfaction$Age <=54,'45-54',
                                                            ifelse(Satisfaction$Age >=55 & Satisfaction$Age <=64,'55-64','65+'
                                       ))))))
Satisfaction$Age.Group <- as.factor(Satisfaction$Age.Group)
str(Satisfaction)
Satisfaction$Scheduled.Departure.Hour.Group <- ifelse(Satisfaction$Scheduled.Departure.Hour >=1 & Satisfaction$Scheduled.Departure.Hour <=5,'early morning (1am-5am)',
                                                              ifelse(Satisfaction$Scheduled.Departure.Hour >=6 & Satisfaction$Scheduled.Departure.Hour <=11,'morning (6am-11am)',
                                                                     ifelse(Satisfaction$Scheduled.Departure.Hour >=12 & Satisfaction$Scheduled.Departure.Hour <=17,'afternoon (12pm-5pm)','evening (6pm-11pm)'
                                                                     )))
Satisfaction$Scheduled.Departure.Hour.Group  <- as.factor(Satisfaction$Scheduled.Departure.Hour.Group)
# Map each numeric attribute to a category 
# Price.Sensitivity,Year.of.First.Flight,No.of.Flights.p.a,No..of.other.Loyalty.Cards,Departure.Delay.in.Minutes,Flight.time.in.minutes, Flight.Distance 
Satisfaction$Price.Sensitivity.Group <- as.factor(ifelse(Satisfaction$Price.Sensitivity >=4, "sensitive", 'notsensitive'))
Satisfaction$Year.of.First.Flight.Group <- as.factor(ifelse(Satisfaction$Year.of.First.Flight <= 2007,"2003-2007","2008-2012"))
Satisfaction$Satisfaction.Group <- as.factor(ifelse(Satisfaction$Satisfaction >= 4,"satisfied","unsatisfied"))
FlightFeature1 <- function(v){
  vBuckets <- v
  q <- quantile(v, c(0.4, 0.6))
  vBuckets <- replicate(length(v), "Average")
  vBuckets[v <= q[1]] <- "Low"
  vBuckets[v > q[2]] <- "High"
  return(vBuckets)
}
quantile(Satisfaction$Departure.Delay.in.Minutes, probs = c(0.2))
quantile(Satisfaction$Arrival.Delay.in.Minutes, probs = c(0.2))

quantile(Satisfaction$Departure.Delay.in.Minutes, probs = c(0.8))
quantile(Satisfaction$Arrival.Delay.in.Minutes, probs = c(0.8))

quantile(Satisfaction$No.of.Flights.p.a., probs = c(0.6))

Satisfaction$No.of.Flights.p.a.Group <- as.factor(FlightFeature1(Satisfaction$No.of.Flights.p.a.))
Satisfaction$No..of.other.Loyalty.Cards.Group <- as.factor(FlightFeature1(Satisfaction$No..of.other.Loyalty.Cards))
Satisfaction$Departure.Delay.in.Minutes.Group <-as.factor(FlightFeature1(Satisfaction$Departure.Delay.in.Minutes))
Satisfaction$Flight.time.in.minutes.Group <- as.factor(FlightFeature1(Satisfaction$Flight.time.in.minutes))
Satisfaction$Flight.Distance.Group <- as.factor(FlightFeature1(Satisfaction$Flight.Distance))

str(Satisfaction)

# Build a subset for customers whose flights have been delayed
DelaySubset <- Satisfaction[which(Satisfaction$Arrival.Delay.greater.5.Mins == "yes"), ]
str(DelaySubset)

# Build a subset for customers whose flights have not been delayed
NoDelaySubset <- Satisfaction[which(Satisfaction$Arrival.Delay.greater.5.Mins == "no"), ]
str(NoDelaySubset)

# Create a subset of the customers whose flight haven't been cancelled of SE airlines.
SESubset <-  as.data.frame(filter(Satisfaction, Airline.Name=="SoutheastAirlinesCo."))
str(SESubset)
NoSESubset <- as.data.frame(filter(Satisfaction, Airline.Name!="SoutheastAirlinesCo."))
str(NoSESubset)

##### Descriptive statistics & Visualizations

#### flight status
# Calculate average satisfaction on different flight status
CancelledSati <- mean(CancelledSubset$Satisfaction)
CancelledSati
DelaySati<-mean(DelaySubset$Satisfaction)
DelaySati
NoDelaySati<-mean(NoDelaySubset$Satisfaction)
NoDelaySati

# Create a dataframe showing the flight status, number of customers and their Average Satisfaction
SatiByFlightStatus <-data.frame("Flight.Status"=c("cancel","delay","ontime"), "Number.of.customers"=c(nrow(CancelledSubset),nrow(DelaySubset),nrow(NoDelaySubset)))
SatiByFlightStatus$Average.Satisfaction <- c(CancelledSati,DelaySati,NoDelaySati)
str(SatiByFlightStatus)
SatiByFlightStatus

# Sample flight status distribution - using pie chart
label_value <- paste('(', round(SatiByFlightStatus$Number.of.customers/sum(SatiByFlightStatus$Number.of.customers) * 100, 1), '%)', sep = '')
label_value
label <- paste(SatiByFlightStatus$Flight.Status, label_value, sep = " ")
label
FlightStatusPieChart <- ggplot(data = SatiByFlightStatus, mapping = aes(x = 'Content', y = Number.of.customers, fill = Flight.Status))+
  geom_bar(stat = 'identity', position = 'stack', width = 1)+
  coord_polar(theta = 'y') + labs(x = '', y = '', title = '')+
  theme(axis.text = element_blank()) + theme(axis.ticks = element_blank())+
  scale_fill_manual(breaks = SatiByFlightStatus$Flight.Status, labels = label,values = c("#FFD966", "#37474F","#77909C"))+
  theme(legend.text = element_text(size=20))
FlightStatusPieChart

# Average satifaction of flight status - using bar chart
FlightStatusSatiCol <- ggplot(SatiByFlightStatus, aes(x=Flight.Status,y=Average.Satisfaction))+
  geom_col(width = 0.3,fill="#77909C", colour="#6E7B8B")+
  labs(title="Average satisfaction of different flight status",x="Flight Status", y="Average Satisfaction")+
  theme(legend.text = element_text(size=20),axis.text =element_text(size=10))
FlightStatusSatiCol

#### Satisfaction distribution
summary(CleanData$Satisfaction)
SatiDistHist <- ggplot(CleanData, aes(x= Satisfaction))+
  geom_histogram(binwidth = 0.2,fill="#77909C", colour="#6E7B8B")+
  labs(title="Satisfaction of All the Customers")
SatiDistHist
SatiDist <- as.data.frame(CleanData %>% 
                                group_by(Satisfaction) %>%
                                summarize(CustomerNumber=n()))
SatiDist

#### Satisfaction of different genders
# Calculate the average satisfaction of different genders
SatiByGender <- as.data.frame(Satisfaction %>% 
                                group_by(Gender) %>%
                                summarize(CustomerNumber=n(),AverageSatisfaction = mean(Satisfaction)))
SatiByGender

# Barchart to decribe the average satisfaction of different genders
GenSatiCol <- ggplot(SatiByGender, aes(x=Gender,y=AverageSatisfaction))+
  geom_col(width = 0.3,fill="#77909C", colour="#6E7B8B")+
  labs(title="Average satisfaction of different genders",x="Gender", y="Average Satisfaction")+
  theme(legend.text = element_text(size=20),axis.text =element_text(size=10))
GenSatiCol

#### Satisfaction of different ages
# Calculate the average satisfaction of different ages
SatiByAge <- as.data.frame(Satisfaction %>% 
                                group_by(Age.Group) %>%
                                summarize(CustomerNumber=n(),AverageSatisfaction = mean(Satisfaction)))
SatiByAge
# Barchart to decribe the average satisfaction of different ages
AgeSatiCol <- ggplot(SatiByAge, aes(x=Age.Group,y=AverageSatisfaction))+
  geom_col(width = 0.3,fill="#77909C", colour="#6E7B8B")+
  labs(title="Average satisfaction of different ages",x="Age.Group", y="Average Satisfaction")+
  theme(legend.text = element_text(size=20),axis.text =element_text(size=15))
AgeSatiCol

#### Satisfaction of different classes
# Calculate the average satisfaction of different classes
SatiByClass <- as.data.frame(Satisfaction %>% 
                             group_by(Class) %>%
                             summarize(CustomerNumber=n(),AverageSatisfaction = mean(Satisfaction)))
SatiByClass
# Barchart to decribe the average satisfaction of different class
ClassSatiCol <- ggplot(SatiByClass, aes(x=Class,y=AverageSatisfaction))+
  geom_col(width = 0.3,fill="#77909C", colour="#6E7B8B")+
  labs(title="Average satisfaction of different classes",x="Class", y="Average Satisfaction")+
  theme(legend.text = element_text(size=20),axis.text =element_text(size=15))
ClassSatiCol

#### Satisfaction of different airlines
SatByAirlines <- data.frame(Satisfaction %>%
                              group_by(Airline.Name) %>%
                              summarize(CustomerNumber=n(),AverageSatisfaction = mean(Satisfaction)))
str(SatByAirlines)
SatByAirlines <- SatByAirlines[order(-SatByAirlines$AverageSatisfaction),] 
SatByAirlinesPlot <- ggplot(SatByAirlines,aes(x=reorder(Airline.Name,AverageSatisfaction), y=AverageSatisfaction))+
  geom_col(fill="#77909C", colour="#6E7B8B",width=0.5)+
  labs(title="Average Satisfaction of All the Airlines", x="Airline Names", y="Average Satisfaction")+
  coord_flip()+
  theme(axis.text.x = element_text(size = 14,color="black"),axis.text.y = element_text(size = 14,color="black"))+
  theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))
SatByAirlinesPlot

# Satisfaction of different type of travels
SatiByType <- data.frame(Satisfaction %>%
                              group_by(Type.of.Travel) %>%
                              summarize(CustomerNumber=n(),AverageSatisfaction = mean(Satisfaction)))
str(SatiByType)
SatiByType
# Bar chart to decribe the average satisfaction of different types
TypeSatiCol <- ggplot(SatiByType, aes(x=Type.of.Travel,y=AverageSatisfaction))+
  geom_col(width = 0.3,fill="#77909C", colour="#6E7B8B")+
  labs(title="Average satisfaction of different types of travel",x="Type", y="Average Satisfaction")+
  theme(legend.text = element_text(size=20),axis.text =element_text(size=15))
TypeSatiCol 

#### Satisfaction of different locations
# Visualization of origin and destinations
states <- map_data("state")
# Satisfaction of different Origin.States
# Calculate mean 'satisfaction' of guests grouped by variable 'Origin.States'.
SatByOriStates <- data.frame(Satisfaction %>%
                               group_by(Origin.State) %>%
                               summarize(CustomerNumber=n(),AverageSatisfaction = mean(Satisfaction)))
str(SatByOriStates)
SatByOriStates <- SatByOriStates[order(SatByOriStates$AverageSatisfaction),] 
SatByOriStates$Origin.State <- tolower(SatByOriStates$Origin.State)

SatByOriStatesMap <- ggplot(SatByOriStates, aes(map_id = Origin.State))+ 
  geom_map(map = states, aes(fill = AverageSatisfaction))+ 
  expand_limits(x=states$long, y=states$lat)+
  coord_map() + ggtitle("Average Satisfaction for Origin States") + labs (x="Longitude", y="Latitude")+
  scale_fill_gradient(high = "#6E7B8B",low = "white")
SatByOriStatesMap

# Satisfaction of different Destination.States
SatByDesStates <- data.frame(Satisfaction %>%
                                 group_by(Destination.State) %>%
                               summarize(CustomerNumber=n(),AverageSatisfaction = mean(Satisfaction)))
str(SatByDesStates)
SatByDesStates <- SatByDesStates [order(SatByDesStates$AverageSatisfaction),] 
SatByDesStates
SatByDesStates$Destination.State<- tolower(SatByDesStates$Destination.State)

SatByDesStatesMap <- ggplot(SatByDesStates, aes(map_id = Destination.State))+ 
  geom_map(map = states, aes(fill = AverageSatisfaction))+ 
  expand_limits(x=states$long, y=states$lat)+
  coord_map() + ggtitle("Average Satisfaction for Destination States") + labs (x="Longitude", y="Latitude")+
  scale_fill_gradient(high = "#6E7B8B",low = "white")
SatByDesStatesMap

# Satisfaction of different Origin.Cities
# Calculate mean 'satisfaction' of guests grouped by variable 'Origin.Cities'.
SatByOriCity <- data.frame(Satisfaction %>%
                             group_by(Orgin.City) %>%
                             summarize(CustomerNumber=n(),AverageSatisfaction = mean(Satisfaction)))
str(SatByOriCity)
SatByOriCity <- SatByOriCity[order(SatByOriCity$AverageSatisfaction),] 
SatByOriCity
LowSatOriCity <- SatByOriCity[1:10,]

# Draw the map of the origin cities with the lowest satisfaction
LowSatOriCityGeo <- cbind(geocode(as.character(LowSatOriCity$Orgin.City),source = "dsk"), LowSatOriCity)
LowSatOriCityGeo$Orgin.City <- tolower(LowSatOriCityGeo$Orgin.City)
LowSatOriCityGeo
str(LowSatOriCityGeo)

LowSatOriCityMap <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group),colour = alpha("grey", 1/2), fill = "white") + 
  coord_map() +
  geom_point(data=LowSatOriCityGeo, aes(x=lon, y=lat, size=AverageSatisfaction), color="#FFD966")+
  geom_text_repel(data=LowSatOriCityGeo, aes(x=lon, y=lat, label=Orgin.City),size=5, color="#77909C",fontface = "bold",vjust=1)+
  ggtitle("Ten origin cities with the lowest saftisfaction ")
LowSatOriCityMap

# Calculate mean 'satisfaction' of guests grouped by variable 'Des.Cities'.
SatByDesCity <- data.frame(Satisfaction %>%
                             group_by(Destination.City) %>%
                             summarize(CustomerNumber=n(),AverageSatisfaction = mean(Satisfaction)))
str(SatByDesCity)
SatByDesCity <- SatByDesCity[order(SatByDesCity$AverageSatisfaction),] 
SatByDesCity
LowSatDesCity <- SatByDesCity[1:10,]

# Draw the map of the destination cities with the lowest satisfaction
LowSatDesCityGeo <- cbind(geocode(as.character(LowSatDesCity$Destination.City),source = "dsk"), LowSatDesCity)
LowSatDesCityGeo$Destination.City <- tolower(LowSatDesCityGeo$Destination.City)
LowSatDesCityGeo
str(LowSatDesCityGeo)

LowSatDesCityMap <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group),colour = alpha("grey", 1/2), fill = "white") + 
  coord_map() +
  geom_point(data=LowSatDesCityGeo, aes(x=lon, y=lat, size=AverageSatisfaction), color="#FFD966")+
  geom_text_repel(data=LowSatDesCityGeo, aes(x=lon, y=lat, label=Destination.City),size=3, color="#77909C",fontface = "bold",vjust=1)+
  ggtitle("Ten destination cities with the lowest saftisfaction")
LowSatDesCityMap 



##### Association rules mining
# transforma the data frame to transactions
SatiTrans <- data.frame(Satisfaction$Satisfaction.Group,
                        Satisfaction$Airline.Status,
                        Satisfaction$Gender,
                        Satisfaction$Type.of.Travel,
                        Satisfaction$Class,
                        Satisfaction$Arrival.Delay.greater.5.Mins,
                        Satisfaction$Age.Group,
                        Satisfaction$Scheduled.Departure.Hour.Group,
                        Satisfaction$Price.Sensitivity.Group,
                        Satisfaction$Year.of.First.Flight.Group,
                        Satisfaction$No.of.Flights.p.a.Group,
                        Satisfaction$No..of.other.Loyalty.Cards.Group,
                        Satisfaction$Departure.Delay.in.Minutes.Group,
                        Satisfaction$Flight.time.in.minutes.Group,
                        Satisfaction$Flight.Distance.Group)
str(SatiTrans)
colSums(is.na(SatiTrans))
SatiTrans <- as(SatiTrans,"transactions")
class(SatiTrans)
str(SatiTrans)

# Use the inspect( ), itemFrequency( ), and itemFrequencyPlot( ) commands to explore the contents of SatiTrans.
inspect(head(SatiTrans))
inspect(tail(SatiTrans))
summary(SatiTrans)
itemFrequency(SatiTrans)
itemFrequencyPlot(SatiTrans, support=0.5, cex.names=0.5)
itemFrequencyPlot(SatiTrans, support=0.3, cex.names=0.5)

# Run the apriori command to try and predict satisfied customers (as defined by their overall satisfaction being high – above 7).
SatirRuleset2 <- apriori(SatiTrans, parameter = list(support=0.1, confidence=0.4,minlen=2), appearance = list(rhs ="Satisfaction.Satisfaction.Group=unsatisfied"))
ruleFeature2 <- inspect(SatirRuleset2)
plot(SatirRuleset2,jitter=0)
# Find the rules with high lift
GoodSatiRules2 <- SatirRuleset2[quality(SatirRuleset2)$lift > 2]
GoodSatiRules2
inspect(GoodSatiRules2)
plot(GoodSatiRules2,jitter=0)

# Improve the level of support and confidence to run the apriori command
SatirRuleset1 <- apriori(SatiTrans, parameter = list(support=0.25, confidence=0.4,minlen=2), appearance = list(rhs ="Satisfaction.Satisfaction.Group=unsatisfied"))
ruleFeature1 <- inspect(SatirRuleset1)
plot(SatirRuleset1)
# Find the rules with high lift
GoodSatiRules1 <- SatirRuleset1[quality(SatirRuleset1)$lift > 1.15]
GoodSatiRules1
inspect(GoodSatiRules1)
plot(GoodSatiRules1)
# Find the rules with high support
HighSuppRules <- SatirRuleset1[quality(SatirRuleset1)$support > 0.35]
inspect(HighSuppRules)
# Find the rules with high confidence
HighConfiRules <- SatirRuleset1[quality(SatirRuleset1)$confidence > 0.6]
inspect(HighConfiRules)

##### Linear Model
#### Simple linear model 
### Customers characteristic 
# SatiVsAge
lm.SatiVsAge <- lm(formula= Satisfaction~ Age, data=Satisfaction)
summary(lm.SatiVsAge)
ggplot(Satisfaction,aes(x=Age, y=Satisfaction))+
  geom_point()+
  stat_smooth(method = "lm", col="#FFD966")

# SatiVsAgeGroup
lm.SatiVsAgeGroup <- lm(formula= Satisfaction~ Age.Group, data=Satisfaction)
summary(lm.SatiVsAgeGroup)
ggplot(Satisfaction,aes(x=Age.Group, y=Satisfaction))+
  geom_point()+
  stat_smooth(method = "lm", col="#FFD966")

# SatiVsPriceSensitivity
lm.SatiVsPriceSensitivity <- lm(formula= Satisfaction~ Price.Sensitivity, data=Satisfaction)
summary(lm.SatiVsPriceSensitivity)
ggplot(Satisfaction,aes(x=Price.Sensitivity, y=Satisfaction))+
  geom_point()+
  stat_smooth(method = "lm", col="#FFD966")

# Sati Vs Consuming behavior in airport
lm.SatiVsConsume <- lm(formula= Satisfaction~ Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport, data=Satisfaction)
summary(lm.SatiVsConsume)

# Sati Vs Shopping.Amount.at.Airport
lm.SatiVsShoppingAmount <- lm(formula= Satisfaction~ Shopping.Amount.at.Airport, data=Satisfaction)
summary(lm.SatiVsShoppingAmount )
ggplot(Satisfaction,aes(x=Shopping.Amount.at.Airport, y=Satisfaction))+
  geom_point()+
  stat_smooth(method = "lm", col="#FFD966")

### Flight experience characteristic 
# SatiVsYearFirst
lm.SatiVsYearFirst <- lm(formula= Satisfaction~ Year.of.First.Flight, data=Satisfaction)
summary(lm.SatiVsYearFirst)

ggplot(Satisfaction,aes(x=Year.of.First.Flight, y=Satisfaction))+
  geom_point()+
  stat_smooth(method = "lm", col="#FFD966")

# Sati Vs No.of.Flights.p.a.
lm.SatiVsNoFlight <- lm(formula= Satisfaction~ No.of.Flights.p.a., data=Satisfaction)
lm.SatiVsNoFlight 

ggplot(Satisfaction,aes(x=No.of.Flights.p.a., y=Satisfaction))+
  geom_point()+
  stat_smooth(method = "lm", col="#FFD966")

### Flight characteristic 

# Sati Vs Delay
lm.SatiVsDelay <- lm(formula= Satisfaction~ Departure.Delay.in.Minutes + 
                       Arrival.Delay.in.Minutes, data=Satisfaction)
summary(lm.SatiVsDelay)

# Sati Vs Departure Delay
ggplot(Satisfaction,aes(x=Departure.Delay.in.Minutes, y=Satisfaction))+
  geom_point()+
  stat_smooth(method = "lm", col="#FFD966")

# Sati Vs Arrival Delay
ggplot(Satisfaction,aes(x=Arrival.Delay.in.Minutes, y=Satisfaction))+
  geom_point()+
  stat_smooth(method = "lm", col="#FFD966")

# Sati Vs Length of travel
OnwaySatilm <- lm(formula= Satisfaction~ Flight.time.in.minutes + Flight.Distance, data=Satisfaction)
summary(OnwaySatilm)

# Sati Vs Flight.time.in.minutes
ggplot(Satisfaction,aes(x=Flight.time.in.minutes, y=Satisfaction))+
  geom_point()+
  stat_smooth(method = "lm", col="#FFD966")

# Sati Vs Flight.Distance
ggplot(Satisfaction,aes(x=Flight.Distance, y=Satisfaction))+
  geom_point()+
  stat_smooth(method = "lm", col="#FFD966")

# Sati Vs Scheduled.Departure.Hour
lm.SatiVsScheduled.Departure.Hour <- lm(formula= Satisfaction~ Scheduled.Departure.Hour, data=Satisfaction)
summary(lm.SatiVsScheduled.Departure.Hour)
# Sati Vs Scheduled.Departure.Hour
ggplot(Satisfaction,aes(x=Scheduled.Departure.Hour, y=Satisfaction))+
  geom_point()+
  stat_smooth(method = "lm", col="#FFD966")

# Sati Vs Day.of.Month
lm.SatiVsDay.of.Month <- lm(formula= Satisfaction~ Day.of.Month, data=Satisfaction)
summary(lm.SatiVsDay.of.Month)
# Sati Vs Day.of.Month
ggplot(Satisfaction,aes(x=Day.of.Month, y=Satisfaction))+
  geom_point()+
  stat_smooth(method = "lm", col="#FFD966")

#### Multiple linear model
### Grouping attributes
## 1)	Customers characteristic 
lm.CustomersCharacteristic<- lm(formula=Satisfaction~Age+Gender+Price.Sensitivity+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport, data = Satisfaction)
summary(lm.CustomersCharacteristic)

## 2) Flight experience characteristic 
# a)	Previous flight experience: Year of First Flight; No of Flights, Percent of Flight with other Airlines, No. Of other Loyalty Cards
# b)	Current flight experience: Airline Status, Type of Travel, Class,
str(Satisfaction)
lm.ExperienceCharacteristic <- lm(formula=Satisfaction~Year.of.First.Flight+No.of.Flights.p.a.+X..of.Flight.with.other.Airlines+Type.of.Travel+No..of.other.Loyalty.Cards+Class+Airline.Status, data = Satisfaction)
summary(lm.ExperienceCharacteristic)

## 3)	Flight characteristic (12 attributes)
# a)	Geography: Flight Distance
# b)	Delay and cancellation: Scheduled Departure Hour, Departure Delay in Minutes, Arrival Delay in Minutes, Flight time in minutes,
lm.FlightCharacteristic<- lm(formula=Satisfaction~Day.of.Month+Scheduled.Departure.Hour+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = Satisfaction)
summary(lm.FlightCharacteristic)

### A full model
## Below are the models developed using stepwise
# Using 'Arrival.Delay.greater.5.Mins' instead of 'Arrival.Delay.in.Minutes' -- Adjusted R-squared:  0.3791 
lmAllSati <- lm(formula=Satisfaction~Age+Gender+Price.Sensitivity+Year.of.First.Flight+No.of.Flights.p.a.+X..of.Flight.with.other.Airlines+Type.of.Travel+No..of.other.Loyalty.Cards+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Class+Day.of.Month+Scheduled.Departure.Hour.Group+Flight.time.in.minutes+Flight.Distance+Arrival.Delay.greater.5.Mins, data = Satisfaction)
summary(lmAllSati)
# Keep the significant attributes -- Adjusted R-squared: 0.3791 
lmSigSati <- lm(formula=Satisfaction~Age+Gender+Price.Sensitivity+Year.of.First.Flight+No.of.Flights.p.a.+X..of.Flight.with.other.Airlines+Type.of.Travel+No..of.other.Loyalty.Cards+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Class+Scheduled.Departure.Hour.Group+Arrival.Delay.greater.5.Mins, data = Satisfaction)
summary(lmSigSati)
# Only using the numeric attribute to do the linear regression--Adjusted R-squared:  0.1048 
lmAllNumSati <- lm(formula=Satisfaction~Age+Price.Sensitivity+Year.of.First.Flight+No.of.Flights.p.a.+X..of.Flight.with.other.Airlines+No..of.other.Loyalty.Cards+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data = Satisfaction)
summary(lmAllNumSati)
# Using 'Arrival.Delay.in.Minutes' instead of 'Arrival.Delay.greater.5.Mins' -- Adjusted R-squared:  0.3589
lmAllSati2 <- lm(formula=Satisfaction~Age+Gender+Price.Sensitivity+Year.of.First.Flight+No.of.Flights.p.a.+X..of.Flight.with.other.Airlines+Type.of.Travel+No..of.other.Loyalty.Cards+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Class+Day.of.Month+Scheduled.Departure.Hour.Group+Flight.time.in.minutes+Flight.Distance+Arrival.Delay.in.Minutes, data = Satisfaction)
summary(lmAllSati2)
# Using 'Age.Group' instead of 'Age' & 'Arrival.Delay.greater.5.Mins' instead of 'Arrival.Delay.in.Minutes' --Adjusted R-squared:  0.3974 
lmAllSati3 <- lm(formula=Satisfaction~Age.Group+Gender+Price.Sensitivity+Year.of.First.Flight+No.of.Flights.p.a.+X..of.Flight.with.other.Airlines+Type.of.Travel+No..of.other.Loyalty.Cards+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Class+Day.of.Month+Scheduled.Departure.Hour.Group+Flight.time.in.minutes+Flight.Distance+Arrival.Delay.greater.5.Mins, data = Satisfaction)
summary(lmAllSati3)
# Keep the significant attributes -- Adjusted R-squared: 0.3974
lmSigSati3 <- lm(formula=Satisfaction~Age.Group+Gender+Price.Sensitivity+Year.of.First.Flight+No.of.Flights.p.a.+Type.of.Travel+No..of.other.Loyalty.Cards+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Class+Scheduled.Departure.Hour.Group+Arrival.Delay.greater.5.Mins, data = Satisfaction)
summary(lmSigSati3)
# Using 'Age.Group' instead of 'Age' &'Arrival.Delay.in.Minutes' instead of 'Arrival.Delay.greater.5.Mins' --Adjusted R-squared:  0.3772 
lmAllSati4 <- lm(formula=Satisfaction~Age.Group+Gender+Price.Sensitivity+Year.of.First.Flight+No.of.Flights.p.a.+X..of.Flight.with.other.Airlines+Type.of.Travel+No..of.other.Loyalty.Cards+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Class+Day.of.Month+Scheduled.Departure.Hour.Group+Flight.time.in.minutes+Flight.Distance+Arrival.Delay.in.Minutes, data = Satisfaction)
summary(lmAllSati4)

## Below are the models developed based on the association rules mining results
# linear regression based on the results of ARS (using numeric attributes)- Adjusted R-squared:  0.4427 
lmAllSati5 <- lm(formula=Satisfaction~Airline.Status+Gender+Price.Sensitivity+Year.of.First.Flight+Type.of.Travel+No..of.other.Loyalty.Cards+Class+Departure.Delay.in.Minutes+Arrival.Delay.greater.5.Mins, data = Satisfaction)
summary(lmAllSati5)

# linear regression based on the results of ARS (using category attributes) - Adjusted R-squared: 0.4432 
lmAllSati6 <- lm(formula=Satisfaction~Airline.Status+Gender+Price.Sensitivity.Group+Year.of.First.Flight.Group+Type.of.Travel+No..of.other.Loyalty.Cards.Group+Class+Departure.Delay.in.Minutes.Group+Arrival.Delay.greater.5.Mins, data = Satisfaction)
summary(lmAllSati6)

# So the lmAllSati6 is the best model

##### Support Vector Machines
# Considering the data volumn, we'd better only use the Southeast Airlines' data to do the modeling.
table(SESubset$Satisfaction.Group)
# Create training and test data sets
Satinrows <- nrow(SESubset)
Sati.random.indexes <- sample(1:Satinrows, replace=FALSE)
SatiCutPoint <- floor(Satinrows/3*2)
SatiTrainData <- SESubset[Sati.random.indexes[1:SatiCutPoint],]
SatiTestData <- SESubset[Sati.random.indexes[(SatiCutPoint+1):Satinrows],]
# Use the dim( ) function to demonstrate that the resulting training data set and test data set contain the appropriate number of cases.
dim(SatiTrainData)
str(SatiTrainData)
dim(SatiTestData)
str(SatiTrainData)

#### Build a support vector model using the ksvm( ) function using all the variables to predict an unsatisfied customer. 
SatisvmOutput <- ksvm(Satisfaction.Group ~ Age+Gender+Price.Sensitivity+Year.of.First.Flight+Type.of.Travel+No..of.other.Loyalty.Cards+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Class+Scheduled.Departure.Hour+Arrival.Delay.greater.5.Mins, data=SatiTrainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
SatisvmOutput

# Making a Prediction variable based on number of votes
SatiSvmPrediction <- predict(SatisvmOutput, SatiTestData, type = "votes") 
str(SatiSvmPrediction)
head(SatiSvmPrediction[2,])

# Creating a composite table based on satisfied customers and SVM Prediction
SatiCompTable<-data.frame(SatiTestData$Satisfaction.Group,SatiSvmPrediction[2,])   
# Creating a confusion matrix
ConfusionMatrix<-table(SatiCompTable)                               
ConfusionMatrix 
# Creating a dataframe containing sum of errors
SatiErrorSum <- ConfusionMatrix[1,2]+ConfusionMatrix[2,1]
# Creating percentage of error rate
SatiErrorRate<-SatiErrorSum/sum(ConfusionMatrix)*100            	
SatiErrorRate

#### Use only numeric variables
SatisvmOutput2 <- ksvm(Satisfaction.Group ~ Age+Price.Sensitivity+Year.of.First.Flight+No.of.Flights.p.a.+X..of.Flight.with.other.Airlines+No..of.other.Loyalty.Cards+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Day.of.Month+Scheduled.Departure.Hour+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Flight.Distance, data=SatiTrainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
SatisvmOutput2

SatiSvmPrediction2 <- predict(SatisvmOutput2, SatiTestData, type = "votes") # Making a Prediction variable based on number of votes
str(SatiSvmPrediction2)
head(SatiSvmPrediction2[2,])

# Creating a composite table based on satisfied customers and SVM Prediction
SatiCompTable2<-data.frame(SatiTestData$Satisfaction.Group,SatiSvmPrediction2[2,])   
# Creating a confusion matrix
ConfusionMatrix2<-table(SatiCompTable2)                               
ConfusionMatrix2 
# Creating a dataframe containing sum of errors
SatiErrorSum2 <- ConfusionMatrix2[1,2]+ConfusionMatrix2[2,1]
# Creating percentage of error rate
SatiErrorRate2<-SatiErrorSum2/sum(ConfusionMatrix2)*100            	
SatiErrorRate2

#### change different variables to check the error rate
SatisvmOutput3 <- ksvm(Satisfaction.Group ~ No..of.other.Loyalty.Cards+Gender+Type.of.Travel+Age+Price.Sensitivity+Year.of.First.Flight+No.of.Flights.p.a.+X..of.Flight.with.other.Airlines+Class+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport+Scheduled.Departure.Hour, data=SatiTrainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
SatisvmOutput3

SatiSvmPrediction3 <- predict(SatisvmOutput3, SatiTestData, type = "votes") 
# Making a Prediction variable based on number of votes
str(SatiSvmPrediction3)
head(SatiSvmPrediction3[2,])

# Creating a composite table based on satisfied customers and SVM Prediction
SatiCompTable3<-data.frame(SatiTestData$Satisfaction.Group,SatiSvmPrediction3[2,])   
# Creating a confusion matrix
ConfusionMatrix3<-table(SatiCompTable3)                               
ConfusionMatrix3
# Creating a dataframe containing sum of errors
SatiErrorSum3 <- ConfusionMatrix3[1,2]+ConfusionMatrix3[2,1]
# Creating percentage of error rate
SatiErrorRate3<-SatiErrorSum3/sum(ConfusionMatrix3)*100            	
SatiErrorRate3

SatisvmOutput4 <- ksvm(Satisfaction.Group ~ Gender+Type.of.Travel+No..of.other.Loyalty.Cards+Class+Airline.Status+Price.Sensitivity, data=SatiTrainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
SatisvmOutput4
SatiSvmPrediction4 <- predict(SatisvmOutput4, SatiTestData, type = "votes") # Making a Prediction variable based on number of votes
str(SatiSvmPrediction4)
head(SatiSvmPrediction4[2,])
# Creating a composite table based on satisfied customers and SVM Prediction
SatiCompTable4<-data.frame(SatiTestData$Satisfaction.Group,SatiSvmPrediction4[2,])   
# Creating a confusion matrix
ConfusionMatrix4<-table(SatiCompTable4)                               
ConfusionMatrix4
# Creating a dataframe containing sum of errors
SatiErrorSum4 <- ConfusionMatrix4[1,2]+ConfusionMatrix4[2,1]
# Creating percentage of error rate
SatiErrorRate4<-SatiErrorSum4/sum(ConfusionMatrix4)*100            	
SatiErrorRate4

#### Use grouped attributes to build model
### 1)	Customers characteristic (5 attributes)
#a)	Demographic: Age, Gender
#b)	Consuming behavior: Shopping Amount at Airport; Eating and Drinking at Airport; Price Sensitivity
SvmCC <- ksvm(Satisfaction.Group ~ Age+Gender+Price.Sensitivity+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport, data=SatiTrainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
SvmCC
# Making a Prediction variable based on number of votes
SvmCCPrediction <- predict(SvmCC, SatiTestData, type = "votes") 
str(SvmCCPrediction)
head(SvmCCPrediction[2,])

# Creating a composite table based on satisfied customers and SVM Prediction
SvmCCCompTable<-data.frame(SatiTestData$Satisfaction.Group,SvmCCPrediction[2,])   
# Creating a confusion matrix
SvmCCConfusionMatrix<-table(SvmCCCompTable)                               
SvmCCConfusionMatrix
# Creating a dataframe containing sum of errors
SvmCCErrorSum <- SvmCCConfusionMatrix[1,2]+SvmCCConfusionMatrix[2,1]
# Creating percentage of error rate
SvmCCErrorRate<-SvmCCErrorSum/sum(SvmCCConfusionMatrix)*100            	
SvmCCErrorRate

SvmCC2 <- ksvm(Satisfaction.Group ~ Age+Price.Sensitivity+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport, data=SatiTrainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
SvmCC2
SvmCCPrediction2 <- predict(SvmCC2, SatiTestData, type = "votes") # Making a Prediction variable based on number of votes
str(SvmCCPrediction2)
head(SvmCCPrediction2[2,])

# Creating a composite table based on satisfied customers and SVM Prediction
SvmCCCompTable2<-data.frame(SatiTestData$Satisfaction.Group,SvmCCPrediction2[2,])   
# Creating a confusion matrix
SvmCCConfusionMatrix2<-table(SvmCCCompTable2)                               
SvmCCConfusionMatrix2
# Creating a dataframe containing sum of errors
SvmCCErrorSum2 <- SvmCCConfusionMatrix2[1,2]+SvmCCConfusionMatrix2[2,1]
# Creating percentage of error rate
SvmCCErrorRate2<-SvmCCErrorSum2/sum(SvmCCConfusionMatrix2)*100            	
SvmCCErrorRate2

### 2)	Flight experience characteristic (7 attributes)
#a)	Previous flight experience: Year of First Flight; No of Flights, Percent of Flight with other Airlines, No. Of other Loyalty Cards
#b)	Current flight experience: Airline Status, Type of Travel, Class,
str(SatiTrainData)
SvmFE <- ksvm(Satisfaction.Group ~Year.of.First.Flight+No.of.Flights.p.a.+X..of.Flight.with.other.Airlines+No..of.other.Loyalty.Cards , data=SatiTrainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
SvmFE
SvmFEPrediction <- predict(SvmFE, SatiTestData, type = "votes") # Making a Prediction variable based on number of votes
str(SvmFEPrediction)
head(SvmFEPrediction[2,])

# Creating a composite table based on satisfied customers and SVM Prediction
SvmFECompTable<-data.frame(SatiTestData$Satisfaction.Group,SvmFEPrediction[2,])   
# Creating a confusion matrix
SvmFEConfusionMatrix<-table(SvmFECompTable)                               
SvmFEConfusionMatrix
# Creating a dataframe containing sum of errors
SvmFEErrorSum <- SvmFEConfusionMatrix[1,2]+SvmFEConfusionMatrix[2,1]
# Creating percentage of error rate
SvmFEErrorRate<-SvmFEErrorSum/sum(SvmFEConfusionMatrix)*100            	
SvmFEErrorRate

### 3)	Flight characteristic (12 attributes)
#a)	Geography: Origin City, Origin State, Destination City, Destination State, Flight Distance
#b)	Delay and cancellation: Scheduled Departure Hour, Departure Delay in Minutes, Arrival Delay in Minutes, Flight time in minutes, Arrival Delay greater 5 Mins, Flight cancelled
SvmFC <- ksvm(Satisfaction.Group ~Flight.Distance+Scheduled.Departure.Hour+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes, data=SatiTrainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
SvmFC
SvmFCPrediction <- predict(SvmFC, SatiTestData, type = "votes") # Making a Prediction variable based on number of votes
str(SvmFCPrediction)
head(SvmFCPrediction[2,])

# Creating a composite table based on satisfied customers and SVM Prediction
SvmFCCompTable<-data.frame(SatiTestData$Satisfaction.Group,SvmFCPrediction[2,])   
# Creating a confusion matrix
SvmFCConfusionMatrix<-table(SvmFCCompTable)                               
SvmFCConfusionMatrix
# Creating a dataframe containing sum of errors
SvmFCErrorSum <- SvmFCConfusionMatrix[1,2]+SvmFCConfusionMatrix[2,1]
# Creating percentage of error rate
SvmFCErrorRate<-SvmFCErrorSum/sum(SvmFCConfusionMatrix)*100            	
SvmFCErrorRate

### Using the result of Association Rules Mining to build the model
# Use the numeric variables
SatisvmOutput5 <- ksvm(Satisfaction.Group ~ Airline.Status+Gender+Price.Sensitivity+Year.of.First.Flight+Type.of.Travel+No..of.other.Loyalty.Cards+Class+Departure.Delay.in.Minutes+Arrival.Delay.greater.5.Mins, data=SatiTrainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
SatisvmOutput5

# Making a Prediction variable based on number of votes
SatiSvmPrediction5 <- predict(SatisvmOutput5, SatiTestData, type = "votes") 
str(SatiSvmPrediction5)
head(SatiSvmPrediction5[2,])

# Creating a composite table based on satisfied customers and SVM Prediction
SatiCompTable5<-data.frame(SatiTestData$Satisfaction.Group,SatiSvmPrediction5[2,])   
# Creating a confusion matrix
ConfusionMatrix5<-table(SatiCompTable5)                               
ConfusionMatrix5
# Creating a dataframe containing sum of errors
SatiErrorSum5 <- ConfusionMatrix5[1,2]+ConfusionMatrix5[2,1]
# Creating percentage of error rate
SatiErrorRate5<-SatiErrorSum5/sum(ConfusionMatrix5)*100            	
SatiErrorRate5

### Using the result of Association Rules Mining to build the model
# Use the category variables
SatisvmOutput6 <- ksvm(Satisfaction.Group ~ Airline.Status+Gender+
                         Price.Sensitivity.Group+Year.of.First.Flight.Group+
                         Type.of.Travel+No..of.other.Loyalty.Cards.Group+
                         Class+Departure.Delay.in.Minutes.Group+
                         Arrival.Delay.greater.5.Mins, 
                       data=SatiTrainData, kernel= "rbfdot", kpar = "automatic", 
                       C = 5, cross = 3, prob.model = TRUE)
SatisvmOutput6

# Making a Prediction variable based on number of votes
SatiSvmPrediction6 <- predict(SatisvmOutput6, SatiTestData, type = "votes") 
str(SatiSvmPrediction6)
head(SatiSvmPrediction6[2,])

# Creating a composite table based on satisfied customers and SVM Prediction
SatiCompTable6<-data.frame(SatiTestData$Satisfaction.Group,SatiSvmPrediction6[2,])   
# Creating a confusion matrix
ConfusionMatrix6<-table(SatiCompTable6)                               
ConfusionMatrix6
# Creating a dataframe containing sum of errors
SatiErrorSum6 <- ConfusionMatrix6[1,2]+ConfusionMatrix6[2,1]
# Creating percentage of error rate
SatiErrorRate6<-SatiErrorSum6/sum(ConfusionMatrix6)*100            	
SatiErrorRate6

### Use SESubest to model, use WestAirways to predict
# Build a model with Southeast airlines
SatisvmOutput7 <- ksvm(Satisfaction.Group ~ Airline.Status+Gender+
                         Price.Sensitivity.Group+Year.of.First.Flight.Group+
                         Type.of.Travel+No..of.other.Loyalty.Cards.Group+
                         Class+Departure.Delay.in.Minutes.Group+
                         Arrival.Delay.greater.5.Mins, 
                       data=SESubset, kernel= "rbfdot", kpar = "automatic", 
                       C = 5, cross = 3, prob.model = TRUE)
SatisvmOutput7
# Create a subset of WestAirwaysInc
WASubset <- data.frame(filter(Satisfaction, Airline.Name=="WestAirwaysInc."))
# Making a Prediction variable based on number of votes
SatiSvmPrediction7 <- predict(SatisvmOutput7, WASubset, type = "votes") 
str(SatiSvmPrediction7)
head(SatiSvmPrediction7[2,])
# Creating a composite table based on satisfied customers and SVM Prediction
SatiCompTable7<-data.frame(WASubset$Satisfaction.Group,SatiSvmPrediction7[2,])   
# Creating a confusion matrix
ConfusionMatrix7<-table(SatiCompTable7)                               
ConfusionMatrix7
# Creating a dataframe containing sum of errors
SatiErrorSum7 <- ConfusionMatrix7[1,2]+ConfusionMatrix7[2,1]
# Creating percentage of error rate
SatiErrorRate7<-SatiErrorSum7/sum(ConfusionMatrix7)*100            	
SatiErrorRate7

# use EnjoyFlyingAirServices to predict
WFSubset <- data.frame(filter(Satisfaction, Airline.Name=="EnjoyFlyingAirServices"))
# Making a Prediction variable based on number of votes
SatiSvmPrediction8 <- predict(SatisvmOutput7, WFSubset, type = "votes") 
str(SatiSvmPrediction8)
head(SatiSvmPrediction8[2,])

# Creating a composite table based on satisfied customers and SVM Prediction
SatiCompTable8<-data.frame(WFSubset$Satisfaction.Group,SatiSvmPrediction8[2,])
# Creating a confusion matrix
ConfusionMatrix8<-table(SatiCompTable8)                               
ConfusionMatrix8
# Creating a dataframe containing sum of errors
SatiErrorSum8 <- ConfusionMatrix8[1,2]+ConfusionMatrix8[2,1]
# Creating percentage of error rate
SatiErrorRate8<-SatiErrorSum8/sum(ConfusionMatrix8)*100            	
SatiErrorRate8
