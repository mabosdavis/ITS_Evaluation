# load packages
pacman::p_load(tidyverse, lubridate, fuzzyjoin)

# load data
devices <- read_csv("data/vms_devices.csv")
msg <- read_csv("data/vms_messages.csv")
msg_2 <- read_csv("data/vms_messages_2.csv")

# clean data
msg <- msg %>% 
  select(Device_ID, AIMS_Location, Timestamp, 
         Category, Condensed, Message)

msg_2 <- msg_2 %>%
  mutate(Device_ID = ID) %>% 
  select(Device_ID, AIMS_Location, Timestamp, 
         Category, Condensed, Message)

# combine data
messages <- rbind(msg, msg_2)

# make Timestamp a date
messages <- messages %>% 
  mutate(Time = mdy_hm(Timestamp, 
                       tz = 'America/Denver')) %>% 
  select(Device_ID, AIMS_Location, Time, 
         Category, Condensed, Message)

# find alt route messages
messages_tibble <- messages %>% 
  mutate(Alt = grepl(" alt", messages$Message,
                     ignore.case = TRUE),
         Crash = grepl("crash", messages$Message,
                     ignore.case = TRUE),
         Caution = grepl("caution", messages$Message,
                       ignore.case = TRUE))

# create tibble with alt route messages
messages_alt <- messages_tibble %>%
  filter(Category == c('Misc','Congestion'),
         Alt == TRUE)

# load TIM crash data
crash <- read_csv("data/TIM_clean_2020.csv")

# clean data
crash <- crash %>% 
  filter(!is.na(`Total Excess Travel Time`))

# make Date a date
crash <- crash %>% 
  mutate(Date = mdy(Date))

crash$datetime <- as.POSIXct(paste(crash$Date, crash$Time),
                               format = "%Y-%m-%d %H:%M:%S")

# left join crash and message data
left <- crash %>% 
  difference_left_join(messages, by = datetime, max_dist = 2, )




