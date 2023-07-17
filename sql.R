library(DBI)
library(dplyr)

# set the working directory that contains the files
setwd("~/Documents/Programming_for_DS/Harvard Dataverse/Assignment 03/")

# part 2
# ======== create the database ========
if (file.exists("airline2.db")) 
  file.remove("airline2.db")
conn <- dbConnect(RSQLite::SQLite(), "airline2.db")

# ======== write to the database ========
# load in the data from the csv files
airports <- read.csv("airports.csv", header = TRUE)
carriers <- read.csv("carriers.csv", header = TRUE)
planes <- read.csv("plane-data.csv", header = TRUE)

dbWriteTable(conn, "airports", airports)
dbWriteTable(conn, "carriers", carriers)
dbWriteTable(conn, "planes", planes)

for(i in c(2006:2008)) {
  ontime <- read.csv(paste0(i, ".csv"), header = TRUE)
  if(i == 2006) {
    dbWriteTable(conn, "ontime", ontime)
  } else {
    dbWriteTable(conn, "ontime", ontime, append = TRUE)
  }
}


# ======== queries via DBI ========
q1 <- dbGetQuery(conn, 
                 "SELECT model AS model, AVG(ontime.DepDelay) AS avg_delay
FROM planes JOIN ontime USING(tailnum)
WHERE ontime.Cancelled = 0 AND ontime.Diverted = 0 AND ontime.DepDelay > 0
GROUP BY model
ORDER BY avg_delay")
print(paste(q1[1, "model"], "has the lowest associated average departure delay."))

q2 <- dbGetQuery(conn, 
                 "SELECT airports.city AS city, COUNT(*) AS total
FROM airports JOIN ontime ON ontime.dest = airports.iata
WHERE ontime.Cancelled = 0
GROUP BY airports.city
ORDER BY total DESC")
print(paste(q2[1, "city"], "has the highest number of inbound flights (excluding cancelled flights)"))



q3 <- dbGetQuery(conn, 
                 "SELECT carriers.Description AS carrier, COUNT(*) AS total
FROM carriers JOIN ontime ON ontime.UniqueCarrier = carriers.Code
WHERE ontime.Cancelled = 1
AND carriers.Description IN ('United Air Lines Inc.', 'American Airlines Inc.', 'Pinnacle Airlines Inc.', 'Delta Air Lines Inc.')
GROUP BY carriers.Description
ORDER BY total DESC")

print(paste(q3[1, "carrier"], "has the highest number of cancelled flights"))

q4 <- dbGetQuery(conn, 
                 "SELECT 
q1.carrier AS carrier, (CAST(q1.numerator AS FLOAT)/ CAST(q2.denominator AS FLOAT)) AS ratio
FROM
(
  SELECT carriers.Description AS carrier, COUNT(*) AS numerator
  FROM carriers JOIN ontime ON ontime.UniqueCarrier = carriers.Code
  WHERE ontime.Cancelled = 1 AND carriers.Description IN ('United Air Lines Inc.', 'American Airlines Inc.', 'Pinnacle Airlines Inc.', 'Delta Air Lines Inc.')
  GROUP BY carriers.Description
) AS q1 JOIN 
(
  SELECT carriers.Description AS carrier, COUNT(*) AS denominator
  FROM carriers JOIN ontime ON ontime.UniqueCarrier = carriers.Code
  WHERE carriers.Description IN ('United Air Lines Inc.', 'American Airlines Inc.', 'Pinnacle Airlines Inc.', 'Delta Air Lines Inc.')
  GROUP BY carriers.Description
) AS q2 USING(carrier)
ORDER BY ratio DESC")
print(paste(q4[1, "carrier"], "highest number of cancelled flights, relative to their number of total flights"))
  
# ======== queries via dplyr ========

planes_db <- tbl(conn, "planes")
ontime_db <- tbl(conn, "ontime")
carriers_db <- tbl(conn, "carriers")
airports_db <- tbl(conn, "airports")

q1 <- ontime_db %>% 
  rename_all(tolower) %>%
  inner_join(planes_db, by = "tailnum", suffix = c(".ontime", ".planes")) %>%
  filter(Cancelled == 0 & Diverted == 0 & DepDelay > 0) %>%
  group_by(model) %>%
  summarize(avg_delay = mean(DepDelay, na.rm = TRUE)) %>%
  arrange(avg_delay) 

print(head(q1, 1))

show_query(q1)

q2 <- ontime_db %>% 
  inner_join(airports_db, by = c("Dest" = "iata")) %>%
  filter(Cancelled == 0) %>%
  group_by(city) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) 

print(head(q2, 1))

q3 <- ontime_db %>% 
  inner_join(carriers_db, by = c("UniqueCarrier" = "Code")) %>%
  filter(Cancelled == 1 & Description %in% c('United Air Lines Inc.', 'American Airlines Inc.', 'Pinnacle Airlines Inc.', 'Delta Air Lines Inc.')) %>%
  group_by(Description) %>%
  summarize(total = n()) %>%
  arrange(desc(total))

print(head(q3, 1))

q4a <- inner_join(ontime_db, carriers_db, by = c("UniqueCarrier" = "Code")) %>%
  filter(Cancelled == 1 & Description %in% c('United Air Lines Inc.', 'American Airlines Inc.', 'Pinnacle Airlines Inc.', 'Delta Air Lines Inc.')) %>%
  group_by(Description) %>%
  summarize(numerator = n()) %>%
  rename(carrier = Description)

q4b <- inner_join(ontime_db, carriers_db, by = c("UniqueCarrier" = "Code")) %>%
  filter(Description %in% c('United Air Lines Inc.', 'American Airlines Inc.', 'Pinnacle Airlines Inc.', 'Delta Air Lines Inc.')) %>%
  group_by(Description) %>%
  summarize(denominator = n()) %>%
  rename(carrier = Description)

q4 <- inner_join(q4a, q4b, by = "carrier") %>%
  mutate_if(is.integer, as.double) %>%
  mutate(ratio = numerator/denominator) %>%
  select(carrier, ratio) %>%
  arrange(desc(ratio)) 

print(head(q4, 1))

# part 4: A simplified solution for the query 4 in practice quiz (G to H)
q4_simplified <- inner_join(ontime_db, carriers_db, by = c("UniqueCarrier" = "Code")) %>%
  filter(Description %in% c('United Air Lines Inc.', 'American Airlines Inc.', 'Pinnacle Airlines Inc.', 'Delta Air Lines Inc.')) %>%
  rename(carrier = Description) %>%
  group_by(carrier) %>%
  summarise(ratio = mean(Cancelled, na.rm = TRUE)) %>%
  arrange(desc(ratio))

print(head(q4_simplified, 1))

dbDisconnect(conn)


----------------------------------------------------------------------
qa <- dbGetQuery(conn, 
                 "SELECT CITY from airports")
print(qa)











