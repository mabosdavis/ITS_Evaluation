# load packages
pacman::p_load(tidyverse, lubridate, fuzzyjoin)

# load data
devices <- read_csv("data/vms_devices.csv")
msg <- read_csv("data/VMS_Join.R OG Files/vms_messages.csv")
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
messages <- messages %>% 
  mutate(Alt = grepl(" alt", messages$Message,
                     ignore.case = TRUE),
         Crash = grepl("crash", messages$Message,
                     ignore.case = TRUE),
         Caution = grepl("caution", messages$Message,
                       ignore.case = TRUE))

# # create tibble with alt route messages
# messages_alt <- messages %>%
#   filter(Category == c('Misc','Congestion'),
#          Alt == TRUE)

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

# filter dates and category
messages_edit <- messages %>% 
  filter(Time >= "2020-03-01 00:00:00",
         Time <= "2020-10-01 12:00:00",
         Category == c("Misc","Congestion"))

# left join crash and message data
crash <- crash %>% 
  mutate(upper_time = datetime + minutes(120))

# left join
crash_edit <- fuzzy_left_join(crash, messages_edit,
                  by = c("datetime"="Time",
                         "upper_time"="Time"),
                  match_fun=list(`<=`, `>=`))

crash_edits <- crash_edit %>% 
  select(`#`, Date, Time.x, `Time Range #`,Location, Time.y, 
         AIMS_Location, Condensed, Message, Alt, 
         Crash, Caution, Category, Device_ID)
         
