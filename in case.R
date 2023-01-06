---
  title: "Cyclistic Analysis"
output:
  html_document:
  toc: TRUE
toc_depth: 1
number_sections: false
toc_float: true
df_print: paged
editor_options: 
  markdown: 
  wrap: 72
---
  
  \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

# Imported Data

------------------------------------------------------------------------
  
  ```{r}
library(ggplot2)
library(scales)
```

To begin my final analysis of the Cyclistic Bike-Share company data, I
imported the data I cleaned and processed in MySQL into R to preform my
analysis.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
# Loaded in the libraries that I would be using in this analysis
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(leaflet) 
library("viridis")
library(readr)
```

```{r}
cyclistic.cleaned.data <- read.csv("cyclistic.cleaned.data.csv")
locations <- read.csv("locations.csv")
```

```{r}
# Renamed the columns in the data set to match the SQL script
colnames(cyclistic.cleaned.data) <- c("ride_id", "rideable_type", "time_trip_started", "time_trip_ended", "bike_station_start", "start_station_id", "bike_station_end", "end_station_id", "start_lat", "start_lng", "end_lat", "end_lng", "customer_type", "trip_duration_minutes", "weekday", "month", "time_of_day")
cyclistic.cleaned.data$customer_type <- gsub('[\n]','',cyclistic.cleaned.data$customer_type)
```

# Summary Statistics

------------------------------------------------------------------------
  
  I began my final analysis by summarizing the descriptive statistics for
the whole Cyclistic data set.

```{r}
(cyclistic.cleaned.data %>%
    summarise(Ride.Count = n(), Average.Trip.Duration = mean(trip_duration_minutes), Classic.Bike.Count = sum(with(cyclistic.cleaned.data, rideable_type== "classic_bike")), Electric.Bike.Count = sum(with(cyclistic.cleaned.data, rideable_type== "electric_bike")), Docked.Bike.Count = sum(with(cyclistic.cleaned.data, rideable_type== "docked_bike"))))
```

# Data Visualization

------------------------------------------------------------------------
  
  After this I partitioned the data set by weekday, time of day, and month
and visualized this data.

## Week

```{r, fig.width = 10, fig.height = 6}
# Count of Cyclistic's  bike trips for each day of the week.
(weekday_mode <- cyclistic.cleaned.data %>%
    summarise(Monday = sum(with(cyclistic.cleaned.data, weekday== "Monday")), Tuesday = sum(with(cyclistic.cleaned.data, weekday== "Tuesday")), Wednesday = sum(with(cyclistic.cleaned.data, weekday== "Wednesday")), Thursday = sum(with(cyclistic.cleaned.data, weekday== "Thursday")), Friday =  sum(with(cyclistic.cleaned.data, weekday== "Friday")), Saturday = sum(with(cyclistic.cleaned.data, weekday== "Saturday")), Sunday = sum(with(cyclistic.cleaned.data, weekday== "Sunday"))))

name_week <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
cyclistic.cleaned.data$weekday <- factor(cyclistic.cleaned.data$weekday, levels = name_week)

#Graphing of the count of bike trips for each day of the week and the average bike trip duration for each day of the week
week <- ggplot(data=cyclistic.cleaned.data, aes(x=weekday, fill=weekday)) + geom_bar(position="dodge", stat = "count") + labs(x="Weekday", y="Number of Rides",title="Number of Rides by the 71Day of Week", fill = 'Weekday') + scale_y_continuous(labels = scales::comma) + scale_fill_viridis(discrete = TRUE, option = "B")
(ggplotly(week))

avg_week <- ggplot(data=cyclistic.cleaned.data, aes(x=weekday, fill=weekday)) + labs(x="Weekday", y="Average Trip Duration\nin Minutes",title="Average Trip Duration by the Day of Week", fill = 'Weekday') + stat_summary(aes(y =trip_duration_minutes), fun = "mean", geom = "bar", position = position_dodge(width = .9)) + scale_y_continuous(labels = scales::comma) + scale_fill_viridis(discrete = TRUE, option = "B") 
(ggplotly(avg_week))
```

## Time of Day

```{r, fig.width = 10,fig.height = 6 }
# Count of Cyclistic's bike trips for each hour of the day.
(time_mode <- cyclistic.cleaned.data %>%
    summarise('12AM' = sum(with(cyclistic.cleaned.data, time_of_day== "12AM")), '1AM' = sum(with(cyclistic.cleaned.data, time_of_day== "1AM")),'2AM' = sum(with(cyclistic.cleaned.data, time_of_day== "2AM")), '3AM' = sum(with(cyclistic.cleaned.data, time_of_day== "3AM")),'4AM' = sum(with(cyclistic.cleaned.data, time_of_day== "4AM")),'5AM' = sum(with(cyclistic.cleaned.data, time_of_day== "5AM")), '6AM' = sum(with(cyclistic.cleaned.data, time_of_day== "6AM")), '7AM' = sum(with(cyclistic.cleaned.data, time_of_day== "7AM")), '8AM' = sum(with(cyclistic.cleaned.data, time_of_day== "8AM")), '9AM' = sum(with(cyclistic.cleaned.data, time_of_day== "9AM")),'10AM' = sum(with(cyclistic.cleaned.data, time_of_day== "10AM")),'11AM' = sum(with(cyclistic.cleaned.data, time_of_day== "11AM")), '12PM' = sum(with(cyclistic.cleaned.data, time_of_day== "12PM")), '1PM' = sum(with(cyclistic.cleaned.data, time_of_day== "1PM")),'2PM' = sum(with(cyclistic.cleaned.data, time_of_day== "2AM")), '3PM' = sum(with(cyclistic.cleaned.data, time_of_day== "3AM")),'4PM' = sum(with(cyclistic.cleaned.data, time_of_day== "4AM")),'5PM' = sum(with(cyclistic.cleaned.data, time_of_day== "5AM")),'6PM' = sum(with(cyclistic.cleaned.data, time_of_day== "6AM")),'7PM' = sum(with(cyclistic.cleaned.data, time_of_day== "7AM")),'8PM' = sum(with(cyclistic.cleaned.data, time_of_day== "8AM")),'9PM' = sum(with(cyclistic.cleaned.data, time_of_day== "9AM")),'10PM' = sum(with(cyclistic.cleaned.data, time_of_day== "10AM")),'11PM' = sum(with(cyclistic.cleaned.data, time_of_day== "11PM"))))

name_time <- c('12AM', '1AM', '2AM', '3AM', '4AM', '5AM', '6AM', '7AM', '8AM', '9AM', '10AM', '11AM', '12PM', '1PM', '2PM', '3PM', '4PM', '5PM', '6PM', '7PM', '8PM', '9PM', '10PM', '11PM')
cyclistic.cleaned.data$time_of_day <- factor(cyclistic.cleaned.data$time_of_day, levels = name_time)

#Graphing of the count of bike trips for each hour of the day and the average bike trip duration for each hour of the day
time <- ggplot(cyclistic.cleaned.data, aes(x = time_of_day, fill = time_of_day)) + geom_bar() + labs(x="Time of Day", y="Number of Rides",title = "Number of Rides by the Time of Day (AM/PM)", fill = 'Time of Day (AM/PM)') + scale_y_continuous(labels = scales::comma) + scale_fill_viridis(discrete = TRUE, option = "B") + theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1))
(ggplotly(time))

avg_time <- ggplot(data=cyclistic.cleaned.data, aes(x=time_of_day, fill=time_of_day)) + labs(x="Time of Day (AM/PM)", y="Average Trip Duration\nin Minutes",title="Average Trip Duration by the Time of Day (AM/PM)", fill = 'Time of Day (AM/PM)') + stat_summary(aes(y =trip_duration_minutes), fun = "mean", geom = "bar", position = position_dodge(width = .9)) + scale_y_continuous(labels = scales::comma) + scale_fill_viridis(discrete = TRUE, option = "B") + theme(axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8))
(ggplotly(avg_time))
```

## Month

```{r, fig.width = 10,fig.height = 6}
# Count of Cyclistic's bike trips for each month
(month_mode <- cyclistic.cleaned.data %>%
    summarise(January = sum(with(cyclistic.cleaned.data, month== "January")), February = sum(with(cyclistic.cleaned.data, month== "February")), March = sum(with(cyclistic.cleaned.data, month== "March")), April = sum(with(cyclistic.cleaned.data, month== "April")), May =  sum(with(cyclistic.cleaned.data, month== "May")), June = sum(with(cyclistic.cleaned.data, month== "June")), July = sum(with(cyclistic.cleaned.data, month== "July")), August = sum(with(cyclistic.cleaned.data, month== "August")), September = sum(with(cyclistic.cleaned.data, month== "September")), October = sum(with(cyclistic.cleaned.data, month== "October")), November = sum(with(cyclistic.cleaned.data, month== "November")), December = sum(with(cyclistic.cleaned.data, month== "December"))))

name_month <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
cyclistic.cleaned.data$month <- factor(cyclistic.cleaned.data$month, levels = name_month)

month <- ggplot(cyclistic.cleaned.data, aes(month, fill=month)) + geom_bar() + labs(x="Month", y="Number of Rides",title="Number of Rides by the Month", fill='Month') + scale_y_continuous(labels = scales::comma) + scale_fill_viridis(discrete = TRUE, option = "B") + theme(axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8))
(ggplotly(month))

avg_month <- ggplot(data=cyclistic.cleaned.data, aes(x=month, fill=month)) + stat_summary(aes(y =trip_duration_minutes), fun = "mean", geom = "bar", position = position_dodge(width = .9)) + labs(x="Month", y="Average Trip Duration\nin Minutes",title="Average Trip Duration by the Month", fill='Month') + scale_y_continuous(labels = scales::comma) + scale_fill_viridis(discrete = TRUE, option = "B") + theme(axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8))
(ggplotly(avg_month))


{r, fig.width = 10,fig.height = 6}
(cyclistic.cleaned.data %>%
    summarise(Casual= sum(with(cyclistic.cleaned.data, customer_type== "casual")), Member = sum(with(cyclistic.cleaned.data, customer_type== "member"))))

# Graphing the split in bike usage and the average length of trips by customer type 
cust <- ggplot(cyclistic.cleaned.data, aes(customer_type, fill = customer_type)) + geom_bar() + labs(x="Customer Type", y="Number of Rides",title="Number of Rides by Customer Type", fill='Customer Type') +  scale_y_continuous(labels = scales::comma) + scale_fill_viridis(discrete = TRUE, option = "F", end = .40) + scale_x_discrete(labels=c("casual" = "Casual", "member" = "Member"))
(ggplotly(cust))

avg_cust <- ggplot(cyclistic.cleaned.data, aes(x=customer_type, fill = customer_type)) + labs(x="Customer Type", y="Average Trip Duration",title="Average Trip Duration by Customer Type", fill='Customer Type')  +  scale_y_continuous(labels = scales::comma) + stat_summary(aes(y=trip_duration_minutes), fun = "mean", geom = "bar") +  scale_fill_viridis(discrete = TRUE, option = "B", end = .55) + scale_x_discrete(labels=c("casual" = "Casual", "member" = "Member"))
(ggplotly(avg_cust))

# Examining the split in ride type by customer plan, then plotting the observed split

name_ride <- c("Classic\nBike", "Docked_Bike", "Electric Bike")
cyclistic.cleaned.data$rideable_type <- factor(cyclistic.cleaned.data$rideable_type, levels = name_ride)

ride_plot <- ggplot(data=cyclistic.cleaned.data, aes(x=rideable_type, fill=customer_type)) + geom_bar(position="dodge", stat = "count") + labs(x="Bike Type", y="Number of Rides",title="Number of Rides for Bike Types by Customer Type", fill='Customer Type') +  scale_y_continuous(labels = scales::comma) +  scale_fill_viridis(discrete = TRUE, option = "B", end = .6) + scale_x_discrete(labels=c("classic_bike" = "Classic Bike", "docked_bike" = "Docked Bike", "electric_bike" "Electric Bike"))
(ggplotly(ride_plot))

avg_ride <- ggplot(data=cyclistic.cleaned.data, aes(x=rideable_type, fill=customer_type))+ stat_summary(aes(y=trip_duration_minutes), fun = "mean", geom = "bar", position = position_dodge(0.8)) + labs(x="Bike Type", y="Average Trip Duration",title="Average Trip Duration for Bike Types by Customer Type", fill='Customer Type') +  scale_y_continuous(labels = scales::comma) +  scale_fill_viridis(discrete = TRUE, option = "B", end = .5) + scale_x_discrete(labels=c("classic_bike" = "Classic Bike", "docked_bike" = "Docked Bike", "electric_bike" "Electric Bike"))
(ggplotly(avg_ride))


Popular/Unpopular Bike Stations

Finally I examined Cyclistic s bike station locations , and found the
most popular and least popular bike stations;then plotted the location
of these stations onto a map of the city of Chicago.

```{r,out.width="100%"}
# Renamed the columns in the data set to match the SQL script
(colnames(locations) <- c("bike_station_combine", "count", "combine_lng", "combine_lat"))

#Beginning to create the map, first creating sequence based off above plot
mybins <- seq(0, 80000, by = 20000)

#Creating hover text for map 
mytext <- paste(
  "Bike Station Name: ", locations$bike_station_combine, "<br/>", 
  "Count of Trips: ", locations$count, sep="") %>%
  lapply(htmltools::HTML)

#Creating of color palette for location data on map
mypalette<- colorBin(palette="inferno", domain=locations$count, na.color="transparent", bins=mybins, reverse = TRUE)

#Creation of map using leaflet package
map <- leaflet(locations) %>% 
  addTiles() %>%  
  
  # Centering the map over the city of Chicago
  setView(
    lng = -87.650826, lat = 41.879907,  zoom = 11.5
  ) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Plotting each bike station location on the map, and counting the usage of the the bike station
  addCircleMarkers(
    ~ combine_lng, ~ combine_lat, 
    fillColor = ~ mypalette(count), 
    fillOpacity = 0.7, 
    color = "white", 
    radius = 8, 
    stroke = FALSE,
    # Adding in text to the map to identify the name of the bike station and its usage
    label = mytext,
    labelOptions = labelOptions(
      style = list( 
        "font-weight" = "normal", 
        padding = "3px 8px"
      ), 
      textsize = "13px", 
      direction = "auto"
    ) 
  ) %>%
  addLegend( 
    pal = mypalette, 
    values = locations$count, 
    opacity = 0.9,
    title = "Count of Trips", 
    position = "bottomright"
  )
map
```

## Top

```{r,out.width="100%"}
#Identifying and mapping the top fifty used bike station locations
(locations_top <- locations %>%                
    arrange(desc(count)) %>% 
    group_by(bike_station_combine) %>%
    rename("Bike.Station" = "bike_station_combine", "Count" = "count", "Longitude" = "combine_lng", "Latitude" = "combine_lat") %>%
    head(50))

mybins1 <- seq(30000, 80000, by = 10000)
mypalette1<- colorBin( palette="inferno", domain=locations_top$Count, na.color="transparent", bins=mybins1, reverse = TRUE)

mytext1 <- paste(
  "Bike Station Name: ", locations_top$Bike.Station, "<br/>", 
  "Count of Trips: ", locations_top$Count, sep="") %>%
  lapply(htmltools::HTML)

(map <- leaflet(locations_top, width = "100%") %>% 
    addTiles() %>%  
    setView(
      lng = -87.63875, lat = 41.90419,  zoom = 12.5
    ) %>% 
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(
      ~ Longitude, ~ Latitude,  
      fillColor = ~ mypalette1(Count), 
      fillOpacity = 0.7, 
      color = "white", 
      radius = 8, 
      stroke = FALSE,
      label = mytext1,
      labelOptions = labelOptions(
        style = list( 
          "font-weight" = "normal", 
          padding = "3px 8px"
        ), 
        textsize = "13px", 
        direction = "auto"
      ) 
    ) %>%
    addLegend( 
      pal = mypalette1, 
      values = locations_top$Count, 
      opacity = 0.9,
      title = "Count of Trips", 
      position = "bottomright"
    ))
```

## Bottom

```{r,out.width="100%"}
#Identifying and mapping the fifty least used bike station locations
(locations_bot <- locations %>%                
    arrange(count) %>% 
    group_by(bike_station_combine) %>%
    rename("Bike.Station" = "bike_station_combine", "Count" = "count", "Longitude" = "combine_lng", "Latitude" = "combine_lat") %>%
    head(50))

mybins2 <- seq(0, 5, by = 1)
mypalette2<- colorBin(palette="inferno", domain=locations_bot$Count, na.color="transparent", bins=mybins2, reverse = FALSE)

mytext2 <- paste(
  "Bike Station Name: ", locations_bot$Bike.Station, "<br/>", 
  "Count of Trips: ", locations_bot$Count, sep="") %>%
  lapply(htmltools::HTML)

(map <- leaflet(locations_bot) %>% 
    addTiles() %>%  
    setView(
      lng = -87.75331, lat = 41.84328,  zoom = 10
    ) %>% 
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(
      ~ Longitude, ~ Latitude,  
      fillColor = ~ mypalette2(Count), 
      fillOpacity = 0.7, 
      color = "white", 
      radius = 8, 
      stroke = FALSE,
      label = mytext2,
      labelOptions = labelOptions(
        style = list( 
          "font-weight" = "normal", 
          padding = "3px 8px"
        ), 
        textsize = "13px", 
        direction = "auto"
      ) 
    ) %>%
    addLegend( 
      pal = mypalette2, 
      values = locations_bot$Count, 
      opacity = 0.9,
      title = "Count of Trips", 
      position = "bottomright"
    ))
```



---
  title: "Final Report"
output:
  html_document:
  toc: TRUE
toc_depth: 1
number_sections: false
toc_float: true
df_print: paged
editor_options: 
  markdown: 
  wrap: 72
---
  \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_


---
  # Objective
  
  ------------------------------------------------------------------------
  
  Until now, Cyclistic's marketing strategy relied on building general
awareness and appealing to broad consumer segments. One approach that
helped make these things possible was the flexibility of its pricing
plans; being single-ride passes, full-day passes, and annual
memberships.Customers who purchase single-ride or full-day passes are
referred to as casual riders. Customers who purchase annual memberships
are Cyclistic members.

Recently, Cyclistic's finance analysts have concluded that annual
members are much more profitable than casual riders. Although the
pricing flexibility helps Cyclistic attract more customers, Cyclistic's
Director of Marketing Lily Moreno believes that maximizing the number of
annual members will be key to future growth. Therefore rather than
creating a marketing campaign that targets all-new customers, Director
Moreno has set a clear goal:

-   Design marketing strategies aimed at converting casual riders into
    annual members.

In order to do that, the marketing analyst team needs to better
understand how annual members and casual riders differ, why casual
riders would buy a membership, and how digital media could affect their
marketing tactics. Therefore this final report delivers analyst and
recommendations on how Cyclistic can convert existing causal riders into
annual memberships.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
# Loaded in the libraries that I would be using in this analysis
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(leaflet) 
library("viridis")
library(readr)

# Loading in data
cyclistic.cleaned.data <- read.csv("cyclistic.cleaned.data.csv")
locations <- read.csv("locations.csv")

colnames(cyclistic.cleaned.data) <- c("ride_id", "rideable_type", "time_trip_started", "time_trip_ended", "bike_station_start", "start_station_id", "bike_station_end", "end_station_id", "start_lat", "start_lng", "end_lat", "end_lng", "customer_type", "trip_duration_minutes", "weekday", "month", "time_of_day")
cyclistic.cleaned.data$customer_type <- gsub('[\n]','',cyclistic.cleaned.data$customer_type)

colnames(locations) <- c("bike_station_combine", "count", "combine_lng", "combine_lat")
```

# Analysis & Findings

------------------------------------------------------------------------

## [Bike Ride Analysis]{.underline}

After final analysis of the 5,163,998 bike rides Cyslistic's customers
took over the year-long timespan between July 2021 and July 2022 in R
Studio, I discovered many important findings about Cyclistic's customers
usage of the bike-share program, and the differences between how annual
members and casual riders utilized the bike-share program. To begin I
found that the average customer prefers to ride on the Classic bike, and
the average customer's trip on their bike rental takes approximately 17
minutes to complete.

```{r}
(cyclistic.cleaned.data %>%
    summarise(Average.Trip.Duration = mean(trip_duration_minutes), Classic.Bike.Count = sum(with(cyclistic.cleaned.data, rideable_type== "classic_bike")), Electric.Bike.Count = sum(with(cyclistic.cleaned.data, rideable_type== "electric_bike")), Docked.Bike.Count = sum(with(cyclistic.cleaned.data, rideable_type== "docked_bike"))))
```

Additionally, breaking down further by membership type, we see that
while 2,969,713 riders are annual members, 2,194,285 riders are casual
riders meaning their is huge market of potential new annual member
converts. Moreover looking at the differences in how the different
riders use the bike-share program, we see that while all riders tend to
prefer to use the Classic bike, annual members took much shorter bike
trips at approximately 12 minutes, while casual members bike trip were
much long at approximately 24 minutes.

```{r}
(cyclistic.cleaned.data %>%
    group_by(customer_type) %>%
    arrange(customer_type) %>%
    summarise(Average.Trip.Duration = mean(trip_duration_minutes), Classic.Bike.Count = sum(rideable_type ==  "classic_bike"), Electric.Bike.Count = sum(rideable_type == "electric_bike"), Docked.Bike.Count = sum(rideable_type== "docked_bike")))
```

## [Time Analysis]{.underline}

Furthermore analyzing the time that these trips took place shows that
overall, Cyclistic's customers prefer to ride during the summer months
with July being the most popular month averaging 795,447 rides with bike
usage dropping heavily during the winter. We also see that trip duration
follows this trend going up to \~20 minutes during summer and falling
down to \~13 minutes during the winter. Looking at which weekday
Cyclistic's customers prefer shows that generally customers use the
bike-share program equally throughout the week with a slight preference
for Saturday averaging 861,783 rides. Furthermore, riders also took
slightly longer trips during the weekends rising from around \~15
minutes to \~20 minutes during the end of the week. Lastly checking
overall customer preference along the time of day shows that bike usage
follows a normal distribution as starting around 5AM, averaging 41,816
customers begin use the ride-share program before peaking at around
514,116 customers around 5PM. However despite this trips throughout the
day stayed around \~16 minutes on average only falling for a short
period of time during 5AM-8AM.

```{r}
(cyclistic.cleaned.data %>%
    group_by(month) %>%
    summarise(Count = n(),
              Average.Trip.Duration = mean(trip_duration_minutes))  %>%
    arrange(desc(Count)))

(cyclistic.cleaned.data %>%
    group_by(Weekday = weekday) %>%
    summarise(Count = n(),
              Average.Trip.Duration = mean(trip_duration_minutes)) %>% arrange(desc(Count)))

(cyclistic.cleaned.data %>%
    group_by(Time.of.Day = time_of_day) %>%
    summarise(Count = n(),
              Average.Trip.Duration = mean(trip_duration_minutes)) %>% arrange(desc(Count)))

```

Moving back to the breakdown between annual customers and casual riders
we see that throughout the year annual members take more trips than
casual riders, except during the summer months of July and August. We
see this trend repeated when we look closer, and further separate the
months into weekdays; as annual members generally take more trips during
the week, except during the weekend days of Saturday and Sunday where
casual riders outpace annual members. Moreover as shown across the month
and weekday analysis, causal riders take approximately twice as long
during their bike trips, as across both time analyses, annual members
trip's duration were \~60% shorter than casual riders duration. Lastly
turning towards the analysis of the time of day we get a better
understanding of how both customer types utilize the bike-share program.
Specifically we see that while annual members exceed causal rider
throughout most of the day, they both follow the same normal
distribution peaking at around 5PM. Furthermore at the end of the day
starting at 10PM casual riders overtake annual members till early
morning around 5AM.

```{r}
(cyclistic.cleaned.data %>%
   group_by(Customer.Type = customer_type, Month = month) %>%
   summarise(Count = n(),
  Average.Trip.Duration = mean(trip_duration_minutes))  %>%
   arrange(desc(Count), .by_group=TRUE))
 
(cyclistic.cleaned.data %>%
   group_by(Customer.Type = customer_type, Weekday = weekday) %>%
   summarise(Count = n(),
  Average.Trip.Duration = mean(trip_duration_minutes)) %>% 
    arrange(desc(Count), .by_group=TRUE))

(cyclistic.cleaned.data %>%
   group_by(Customer.Type = customer_type, Time.of.Day = time_of_day) %>%
   summarise(Count = n(),
  Average.Trip.Duration = mean(trip_duration_minutes)) %>% 
    arrange(desc(Count), .by_group=TRUE))

```

## [Geo-spatial Analysis]{.underline}

Finally, analyzing all of Cyclistic's bike station in Chicago indicated
a huge variance in usage between each of the bike station locations;
indicating areas in Chicago where Cyclistic's marketing team can focus
their campaign efforts to maximize customer engagement, and areas for
Cyclistic's marketing team to avoid. To begin, analyzing the map of all
the Cyclistic's bike stations across Chicago displays that customers
preferred bike stations along the eastern coast of Chicago with the bike
stations along Chicago's periphery being more unpopular with Cyclistic's
customers.

```{r}
mybins <- seq(0, 80000, by = 20000)
#Creating hover text for map 
mytext <- paste(
   "Bike Station Name: ", locations$bike_station_combine, "<br/>", 
   "Count of Trips: ", locations$count, sep="") %>%
  lapply(htmltools::HTML)
#Creating of color palette for location data on map
mypalette<- colorBin(palette="inferno", domain=locations$count, na.color="transparent", bins=mybins, reverse = TRUE)
#Creation of map using leaflet package
(map <- leaflet(locations) %>% 
  addTiles() %>%  
# Centering the map over the city of Chicago
  setView(
    lng = -87.66984677949904, lat = 41.89617798482842,  zoom = 11.5
        ) %>%
  addProviderTiles("CartoDB.Positron") %>%
# Plotting each bike station location on the map, and counting the usage of the the bike station
  addCircleMarkers(
    ~ combine_lng, ~ combine_lat, 
    fillColor = ~ mypalette(count), 
    fillOpacity = 0.7, 
    color = "white", 
    radius = 8, 
    stroke = FALSE,
# Adding in text to the map to identify the name of the bike station and its usage
    label = mytext,
    labelOptions = labelOptions(
      style = list( 
        "font-weight" = "normal", 
        padding = "3px 8px"
        ), 
      textsize = "13px", 
      direction = "auto"
      ) 
    ) %>%
  addLegend( 
    pal = mypalette, 
    values = locations$count, 
    opacity = 0.9,
    title = "Count of Trips", 
    position = "bottomright"
    ))
```

Diving further into this analysis demonstrates that while the previous
map shows that most of Cyclistic's customers use the bike-share program
within the dense area of bike stations close to the city center;
analyzing the top 10% of Cyclistic's bike stations shows that
Cyclistic's most used bike stations are on the north side of Chicago's
city center that aligns with the area that contains the major tourist
attractions in Chicago. Furthermore the popularity of the bike stations
with close proximity to Chicago's coast suggests many customers use
Cyclistic for scenic bike riding along the coast. Therefore, in order to
maximize the number of annual membership; Cyclistic's marketing analyst
team should focus any campaign strategy or efforts in Chicago's city
center, and the eastern and northern regions bordering Chicago's city
center.

```{r}
(locations_top <- locations %>%                
  arrange(desc(count)) %>% 
  group_by(bike_station_combine) %>%
    rename("Bike.Station" = "bike_station_combine", "Count" = "count", "Longitude" = "combine_lng", "Latitude" = "combine_lat") %>%
    head(120))
mybins1 <- seq(30000, 80000, by = 10000)
mypalette1<- colorBin( palette="inferno", domain=locations_top$Count, na.color="transparent", bins=mybins1, reverse = TRUE)

mytext1 <- paste(
   "Bike Station Name: ", locations_top$Bike.Station, "<br/>", 
   "Count of Trips: ", locations_top$Count, sep="") %>%
  lapply(htmltools::HTML)

(map <- leaflet(locations_top, width = "100%") %>% 
  addTiles() %>%  
  setView(
    lng = -87.63875, lat = 41.90419,  zoom = 11.5
        ) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
   ~ Longitude, ~ Latitude,  
    fillColor = ~ mypalette1(Count), 
    fillOpacity = 0.7, 
    color = "white", 
    radius = 8, 
    stroke = FALSE,
    label = mytext1,
    labelOptions = labelOptions(
      style = list( 
        "font-weight" = "normal", 
        padding = "3px 8px"
        ), 
      textsize = "13px", 
      direction = "auto"
      ) 
    ) %>%
  addLegend( 
    pal = mypalette1, 
    values = locations_top$Count, 
    opacity = 0.9,
    title = "Count of Trips", 
    position = "bottomright"
    ))

```

Lastly, analyzing solely the bottom 10% of Cyclistic's bike stations
shows that customers tended not to used the Cyclistic's bike stations
that where away from the city center of Chicago moving towards the
residential neighborhoods with specifically the areas to the south and
northwest of Chicago's city center being used the least . Therefore,
Cyclistic's marketing analyst team should avoid focusing their marketing
efforts in this area.

```{r}
(locations_bot <- locations %>%                
  arrange(count) %>% 
  group_by(bike_station_combine) %>%
  rename("Bike.Station" = "bike_station_combine", "Count" = "count", "Longitude" = "combine_lng", "Latitude" = "combine_lat") %>%
  head(120))

mybins2 <- seq(0, 5, by = 1)
mypalette2<- colorBin(palette="inferno", domain=locations_bot$Count, na.color="transparent", bins=mybins2, reverse = FALSE)

mytext2 <- paste(
   "Bike Station Name: ", locations_bot$Bike.Station, "<br/>", 
   "Count of Trips: ", locations_bot$Count, sep="") %>%
  lapply(htmltools::HTML)

(map <- leaflet(locations_bot) %>% 
  addTiles() %>%  
  setView(
    lng = -87.75331, lat = 41.84328,  zoom = 11
        ) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
   ~ Longitude, ~ Latitude,  
    fillColor = ~ mypalette2(Count), 
    fillOpacity = 0.7, 
    color = "white", 
    radius = 8, 
    stroke = FALSE,
    label = mytext2,
    labelOptions = labelOptions(
      style = list( 
        "font-weight" = "normal", 
        padding = "3px 8px"
        ), 
      textsize = "13px", 
      direction = "auto"
      ) 
    ) %>%
  addLegend( 
    pal = mypalette2, 
    values = locations_bot$Count, 
    opacity = 0.9,
    title = "Count of Trips", 
    position = "bottomright"))
```

# Recommendations

------------------------------------------------------------------------

Finally, as stated earlier the purpose of this analysis was to identify
trends to better understand how annual members and casual riders differ
to better inform marketing strategies aimed at converting casual riders
into annual members; and as a result of my analysis I have generated
three recommendations for the Cyclistic's marketing analyst team that
outline how to convert Cyclistic's existing causal riders into annual
memberships.

The three marketing recommendations for the marketing analyst team are
outlined below:

1.   ***A Digital Ad Campign highlighting the Tourist Attractions in
    Chicago's Metro Hub and Coast***
  
  The final analysis of Cyclistic's bike stations locations displayed
    that riders' most popular bike stations were stationed near
Chicago's metro hub and coast, validating prior knowledge that most
    riders use the bike-share program for leisure. Moreover deeper
    analysis into causal riders supported this analysis as well, as
    analysis showed that causal riders preferred to ride on the weekend
    and after regular work hours suggesting casual riders were riding
    for fun rather than for commuting to work. Therefore, the Cyclistic
    marketing team should capitalized on this by running an digital ad
    campaign targeted towards causal riders that both highlights the
    major tourist attractions in metro Chicago and scenic coastline and
    promotes the use of Cyclistics bikes. Finally this could be
    accomplished by combining; promotional ads that display Cyclistic's
customers enjoying their time on bikes at major metro Chicago spots,
and by creating a series of suggested tailored scenic bike routes
through metro Chicago that encourage riders to try the different
bike routes over a extended period of time. Thus, encourageing
casual riders to switch to the annual membership in order to save on
the multiple scenic bike rides.

2.   ***A Digital Marketing Campaign highlighting a Discount on Electric
Bikes for Annual Members***
  
  Furthermore while most riders use the bike-share program for
leisure, analysis showed that causal riders overtake annual members
during the late night hours of 10PM till 5AM, suggesting that these
later riders were using the bike for commuting and travel.
Additionally, across all time casual riders take bike trips that are
significantly longer than annual members, therefore I recommend a
targeted marketing campaign promoting a discount on electric bikes
for annual members, that would run digital ads at bike stations at
night. Thus, casual riders would be incentivezie to convert to
annual memberships to attain the benefits of shorter trips times on
electric bikes or conservation of energy from no longer cycling,
which would be especially beneficial to this section of casual
riders traveling or commuting at night after a long tiring day.

3.   ***Offer Discounted Cyclistic Annual Memberships during the
Summer***
  
  Finally the Cyclistic analyst marketing team should take advantage
of the significant increase in causal riders during the summer
season by offering an promotional discount that lowers the price of
the annual membership during the month of July, August, and
September. Furthermore as a result of Chicago's clement weather
    during this time, Cyclistic can also capitalize on the increase in
    ridership and good weather by partnering the discount with ads
    highlighting leisurely rides in the pleasant weather.

    ::: {.tocify-extend-page data-unique="tocify-extend-page" style="height: 0;"}
    :::
    
    
    
    
    ---
title: "Google Analytics Capstone Project - Cyclisitic Case Study"
output:
  html_document:
    toc: TRUE
    toc_depth: 1
    number_sections: false
    toc_float: true
editor_options: 
  markdown: 
    wrap: 72
---

<div class="tocify-extend-page" data-unique="tocify-extend-page" style="height: 0;"></div>