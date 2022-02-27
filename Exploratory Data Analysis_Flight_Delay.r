library(tidyverse)
flight <- read_csv('Jan_2020_ontime.csv')
View(flight)
glimpse(flight)
summary(flight$DISTANCE)

ggplot(data = flight) +
  geom_bar(mapping = aes(x = ARR_DEL15))

ggplot(data = flight) +
  geom_bar(mapping = aes(x = OP_UNIQUE_CARRIER))

flight %>% count(CANCELLED)

ggplot(data = flight) +
  geom_histogram_interactive(mapping = aes(x = DISTANCE),binwidth = 50)

#How many flights from MIAMI to JFK in Jan 2020

Miami_flights <- flight %>% 
  select(DAY_OF_MONTH,OP_CARRIER_FL_NUM,OP_CARRIER, ORIGIN, DEST,
         DEP_TIME,ARR_TIME, DISTANCE, DEP_DEL15, ARR_DEL15, CANCELLED  ) %>%
  filter(ORIGIN=='MIA',DEST=='JFK')

View(Miami_flights)

num_flights <- Miami_flights %>% summarise(count=n())
print(num_flights)

Carriers <- Miami_flights %>%
  group_by(OP_CARRIER) %>%
  summarise(count=n())

View(Carriers)

#Miami_flights %>% count(OP_CARRIER)

ggplot(data = Carriers, mapping = aes(x = OP_CARRIER, y= count)) +
  geom_col(position = "dodge", fill="pink") +
  ggtitle("Carriers between MIA and JFK") +
  xlab("Carriers") +
  ylab("count") +
  geom_text(aes(label = count))

#No.of flights delayed for departure from MIA to JFK

delayed_flights <- Miami_flights %>% 
  group_by(DEP_DEL15) %>%
  summarise(count=n())

View(delayed_flights)

delayed_flights <-delayed_flights %>% drop_na(DEP_DEL15)

delayed_flights$DEP_DEL15[delayed_flights$DEP_DEL15==0]="On Time"

delayed_flights$DEP_DEL15[delayed_flights$DEP_DEL15==1]="Delayed"


ggplot(data= delayed_flights, 
       mapping= aes(x=DEP_DEL15, y=count)) +
  geom_col(fill="green") +
  ggtitle('JFK flights on time/delay')+ 
  xlab('Ontime/delay')+ 
  ylab('Count') + 
  geom_text(aes(label=count))
