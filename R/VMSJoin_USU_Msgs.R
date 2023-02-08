# load packages
pacman::p_load(tidyverse, lubridate, fuzzyjoin)

# load data
devices <- read_csv("data/vms_devices.csv")
msg <- as.tibble(readRDS("data/1.VMS_records_crashes.rds"))

# # clean data
# msg <- msg %>% 
#   select(Device_ID, AIMS_Location, Timestamp, 
#          Category, Condensed, Message)
# 
# msg_2 <- msg_2 %>%
#   mutate(Device_ID = ID) %>% 
#   select(Device_ID, AIMS_Location, Timestamp, 
#          Category, Condensed, Message)
# 
# # combine data
# messages <- rbind(msg, msg_2)
# 
# # make Timestamp a date
# messages <- messages %>% 
#   mutate(Time = mdy_hm(Timestamp, 
#                        tz = 'America/Denver')) %>% 
#   select(Device_ID, AIMS_Location, Time, 
#          Category, Condensed, Message)
#
# # find alt route messages
# messages <- messages %>% 
#   mutate(Alt = grepl(" alt", messages$Message,
#                      ignore.case = TRUE),
#          Crash = grepl("crash", messages$Message,
#                      ignore.case = TRUE),
#          Caution = grepl("caution", messages$Message,
#                        ignore.case = TRUE))
#
# # create tibble with alt route messages
# messages_alt <- messages %>%
#   filter(Category == c('Misc','Congestion'),
#          Alt == TRUE)

# load TIM crash data
crash <- as.tibble(read_csv("data/TIM_clean_2020.csv"))

# filter out rows without Total Excess Travel Time
crash <- crash %>% 
  filter(!is.na(`Total Excess Travel Time`))

# create one datetime column
crash$C.start.time <- as.POSIXct(as.character(
  paste(crash$Date, crash$Time)), format="%m/%d/%Y %H:%M:%S")

# rename columns to be consistent with msg
crash <- crash %>%
  rename(C.crash.id = `#`,
         C.crash.location = Location,
         C.crash.type = `Crash Type`,
         C.crash.type.num = `Crash Type #`,
         C.affected.volume = `Affected Volume`,
         C.total.excess.travel.time = `Total Excess Travel Time`,
         C.time.range = `Time Range`,
         C.time.range.num = `Time Range #`
         )


# # make Date a date
# crash <- crash %>% 
#   mutate(Date = mdy(Date))
# 
# crash$datetime <- as.POSIXct(paste(crash$Date, crash$Time),
#                              format = "%Y-%m-%d %H:%M:%S")
# 
# # filter dates and category
# messages_edit <- messages %>% 
#   filter(Time >= "2020-03-01 00:00:00",
#          Time <= "2020-10-01 12:00:00",
#          Category == c("Misc","Congestion"))

# left join crash and message data
crash <- crash %>% 
  mutate(C.two.hrs = C.start.time + minutes(120))

# left join
crash_edit <- fuzzy_left_join(crash, msg,
                  by = c("C.start.time"="V.start.time",
                         "C.two.hrs"="V.start.time"),
                  match_fun=list(`<=`, `>=`))

crash_edits <- crash_edit %>% 
  select(`#`, Date, Time.x, `Time Range #`,Location, Time.y, 
         AIMS_Location, Condensed, Message, Alt, 
         Crash, Caution, Category, Device_ID)
         
