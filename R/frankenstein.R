#################################################################
########################Frankenstein Code########################
#################################################################

# load libraries
pacman::p_load(readxl, tidyverse, lubridate, fuzzyjoin)

# read in VMS messages containing "crash" from 2018-2022 
msg <- read_excel("data/msg_crash_18-22.xlsx")

# rename columns
msg <- msg %>% 
  rename(V.device.id = DeviceId,
         V.start.time = MsgDateTime,
         V.message = Message,
         V.source.type = MsgSourceType, 
         V.source.name = MsgSourceName)

# clean message data
msg <- msg %>% 
  filter(V.source.type == "USER") %>% 
  select(V.device.id, V.start.time, V.message) 

# ensure that the time is set to MST (originally UTC)
msg$V.start.time <- msg$V.start.time %>% 
  with_tz( tzone = "US/Mountain")

# filter only msgs from March-August
msg <- msg %>% 
  mutate(V.month = month(V.start.time)) %>% 
  filter(V.month >= 3,
         V.month <= 8)

msg$V.month <- NULL
  
#################################################################
#######################Clean VMS Messages########################
#################################################################

# To-add
# - Select messages of interest
# - Add end time of message (from complete dataset)
# - Add duration of message


# # split date and time into two columns
# msg <- msg %>% 
#   mutate(V.start.time = ymd_hms(V.start.time)) %>% 
#   separate(V.start.time, into = c('date', 'time'), sep=' ', 
#            remove = FALSE)

# manipulate VMS message column
msg$x1 <- sapply(strsplit(as.character(msg$V.message),'><'), 
                 "[", 1)
msg$x2 <- sapply(strsplit(as.character(msg$V.message),'><'), 
                 "[", 2)
msg$x3 <- sapply(strsplit(as.character(msg$V.message),'><'), 
                 "[", 3)
msg$x4 <- sapply(strsplit(as.character(msg$V.message),'><'), 
                 "[", 4)
msg$x5 <- sapply(strsplit(as.character(msg$V.message),'><'), 
                 "[", 5)
msg$x6 <- sapply(strsplit(as.character(msg$V.message),'><'), 
                 "[", 6)
msg$x7 <- sapply(strsplit(as.character(msg$V.message),'><'), 
                 "[", 7)
msg$x8 <- sapply(strsplit(as.character(msg$V.message),'><'), 
                 "[", 8)
msg$V.message <- NULL

# extracting exact VMS message
temp <- as.data.frame(str_match(msg$x2, '>\\s*(.*?)\\s*<'))
msg$Message1 <- temp$V2

temp <- as.data.frame(str_match(msg$x3, '>\\s*(.*?)\\s*<'))
msg$Message2 <- temp$V2

temp <- as.data.frame(str_match(msg$x4, '>\\s*(.*?)\\s*<'))
msg$Message3 <- temp$V2

temp <- as.data.frame(str_match(msg$x5, '>\\s*(.*?)\\s*<'))
msg$Message4 <- temp$V2

temp <- as.data.frame(str_match(msg$x6, '>\\s*(.*?)\\s*<'))
msg$Message5 <- temp$V2

temp <- as.data.frame(str_match(msg$x7, '>\\s*(.*?)\\s*<'))
msg$Message6 <- temp$V2; rm(temp)

msg$x1 <- NULL; msg$x2 <- NULL; msg$x3 <- NULL; msg$x4 <- NULL; 
msg$x5 <- NULL; msg$x6 <- NULL; msg$x7 <- NULL; msg$x8 <- NULL; 

# Finally extract VMS message only
msg$V.message <- with(msg, paste0(Message1, " ", Message2, " ", 
                                Message3, " ", Message4, " ", 
                                Message5, " ", Message6))

msg$V.message <- gsub("NA","",as.character(msg$V.message))

# remove unnecessary columns
msg$Message1 <- NULL; msg$Message2 <- NULL; msg$Message3 <- NULL;
msg$Message4 <- NULL; msg$Message5 <- NULL; msg$Message6 <- NULL;

#################################################################
######################Manual Data Cleaning#######################
#################################################################

# To add
# - remove travel time messages (containing $ or !)
# - Check with Hayden that the group_by is doing what it should

# remove duplicate rows
msg <- msg %>%
  mutate(V.year = year(V.start.time),
         V.month = month(V.start.time),
         V.day = day(V.start.time)) %>% 
  group_by(V.device.id, V.year, V.month, V.day) %>% 
  distinct(V.message, .keep_all = TRUE) %>% 
  ungroup()

msg$V.year <- NULL; msg$V.month <- NULL; msg$V.day <- NULL

# remove rows that contain travel times
msg <- msg %>% 
  mutate(tt = ifelse(grepl("\\$", V.message)==TRUE, 
                     TRUE, FALSE)) %>% 
  filter(tt == FALSE)

msg$tt <- NULL


#################################################################
###################Left Join Msg and Device Data#################
#################################################################

# Next section
# - VMS device location milepost
# - VMS route and bound

# load devices data
devices <- read.csv("data/vms_devices.csv")

# clean devices data for relevant info
devices <- devices %>% 
  select(Device_ID, AIMS_Location, PrimaryLocation, Milepost) %>%  
  rename(V.device.id = Device_ID,
         V.location = AIMS_Location,
         V.route = PrimaryLocation,
         V.milepost = Milepost)

# left join devices data to msgs data
msg <- left_join(msg, devices, 
                  by = join_by(V.device.id == V.device.id)) %>% 
  filter(!is.na(V.location))

rm(devices)

#################################################################
########################Clean Crash Data#########################
#################################################################

# load crash data
crash_18 <- read_csv("data/TIM_clean_2018.csv")
crash_22 <- read_csv("data/TIM_clean_2022.csv")

crash <- rbind(crash_18, crash_22)

rm(crash_18, crash_22)

# make Date a date
crash <- crash %>% 
  mutate(Date = mdy(Date))

# combine date and time into one column
crash$datetime <- as.POSIXct(paste(crash$Date, crash$Time),
                             format = "%Y-%m-%d %H:%M:%S",
                             tz = "US/Mountain")

# create 2hr time buffer from crash start time
crash <- crash %>% 
  mutate(upper_time = datetime + minutes(120))

# clean crash column names
crash <- crash %>% 
  select(`#`, Location, `Affected Volume`, 
         `Total Excess Travel Time`, datetime, upper_time) %>% 
  rename(C.crash.id = `#`,
         C.location = Location, 
         C.affected.volume = `Affected Volume`,
         C.TETT = `Total Excess Travel Time`,
         C.start.time = datetime, 
         C.two.hours = upper_time)

#################################################################
##################Left join crash to msg data####################
#################################################################

# left join - this takes a few seconds
join <- fuzzy_left_join(crash, 
                        msg,
                        by = c("C.start.time"="V.start.time",
                               "C.two.hours"="V.start.time"),
                              match_fun=list(`<=`, `>=`))
#################################################################
###############General Cleaning to Final Dataset#################
#################################################################

# arrange columns
join <- join %>% 
  select(C.TETT, C.crash.id, C.location, V.location, V.message, 
         C.start.time, V.start.time, V.device.id)

# create year column to enable filtering
join <- join %>% 
  mutate(V.year = year(V.start.time))

# column for difference in start times
join$V.duration <- difftime(join$V.start.time, join$C.start.time, 
                            tz = "US/Mountain",
                            units = "mins")

# column for VMS msg route and bound
join$V.route.bound <- sapply(strsplit(
  as.character(join$V.location),'@'), "[", 1)
join$V.route <- sapply(strsplit(
  as.character(join$V.route.bound),' '), "[", 1)
join$V.bound <- sapply(strsplit(
  as.character(join$V.route.bound),' '), "[", 2)
join$V.route.bound <- NULL

write_rds(join, file = "data/join.rds")

# column for crash route

# To-do
# - get this section to work where it pulls out the route info
# # VMS device location milepost
# join1 <- as.data.frame(str_match(join$C.location, 
#                                 '\\s* (I \\d+ \\w?) \\s*,'))
# join$C.route <- join$V2; rm(join1)

#################################################################
#############Load and Clean 2018 and 2022 VMS data###############
#################################################################

# load vms data
vms_18_1 <- read_csv("data/2018_9-12_Vmsalt.csv")
vms_18_2 <- read_csv("data/2018_1-8_Vmsalt.csv")
vms_22 <- read_csv("data/2022_vmsalt.csv")

vms <- rbind(vms_18_1, vms_18_2, vms_22)

rm(vms_18_1, vms_18_2, vms_22)

# rename columns
vms <- vms %>% 
  rename(V.device.id = DeviceId,
         V.start.time = MsgDateTime,
         V.message = Message,
         V.source.type = MsgSourceType, 
         V.source.name = MsgSourceName)

# clean message data
vms <- vms %>%
  filter(V.source.type == "USER") %>%
  select(V.device.id, V.start.time, V.message)

# make V.start.time a datetime
vms$V.start.time <- as.POSIXct(vms$V.start.time,
                             format = "%m/%d/%Y %H:%M",
                             tz = "US/Mountain")

# filter only msgs from March-August
vms <- vms %>% 
  mutate(V.month = month(V.start.time)) %>% 
  filter(V.month >= 3,
         V.month <= 8)

vms$V.month <- NULL

# Create day, month, and year column for filtering
vms <- vms %>% 
  mutate(V.month = month(V.start.time),
         V.year = year(V.start.time),
         V.day = day(V.start.time))

# remove duplicate rows
vms <- vms %>%
  group_by(V.device.id, V.year, V.month, V.day) %>% 
  distinct(V.message, .keep_all = TRUE) %>% 
  ungroup()

# load devices data
devices <- read.csv("data/VMS_Join.R OG Files/vms_devices.csv")

# clean devices data for relevant info
devices <- devices %>% 
  select(Device_ID, AIMS_Location, PrimaryLocation, Milepost) %>%  
  rename(V.device.id = Device_ID,
         V.location = AIMS_Location,
         V.route = PrimaryLocation,
         V.milepost = Milepost)

# left join devices data to vms data
vms <- left_join(vms, devices, 
                 by = join_by(V.device.id == V.device.id)) %>% 
  filter(!is.na(V.location))

rm(devices)


#################################################################
#######Clean Full VMS data to be usable for reference############
#################################################################

# manipulate VMS message column
vms$x1 <- sapply(strsplit(as.character(vms$V.message),'><'), 
                 "[", 1)
vms$x2 <- sapply(strsplit(as.character(vms$V.message),'><'), 
                 "[", 2)
vms$x3 <- sapply(strsplit(as.character(vms$V.message),'><'), 
                 "[", 3)
vms$x4 <- sapply(strsplit(as.character(vms$V.message),'><'), 
                 "[", 4)
vms$x5 <- sapply(strsplit(as.character(vms$V.message),'><'), 
                 "[", 5)
vms$x6 <- sapply(strsplit(as.character(vms$V.message),'><'), 
                 "[", 6)
vms$x7 <- sapply(strsplit(as.character(vms$V.message),'><'), 
                 "[", 7)
vms$x8 <- sapply(strsplit(as.character(vms$V.message),'><'), 
                 "[", 8)
vms$V.message <- NULL

# extracting exact VMS message
temp <- as.data.frame(str_match(vms$x2, '>\\s*(.*?)\\s*<'))
vms$Message1 <- temp$V2

temp <- as.data.frame(str_match(vms$x3, '>\\s*(.*?)\\s*<'))
vms$Message2 <- temp$V2

temp <- as.data.frame(str_match(vms$x4, '>\\s*(.*?)\\s*<'))
vms$Message3 <- temp$V2

temp <- as.data.frame(str_match(vms$x5, '>\\s*(.*?)\\s*<'))
vms$Message4 <- temp$V2

temp <- as.data.frame(str_match(vms$x6, '>\\s*(.*?)\\s*<'))
vms$Message5 <- temp$V2

temp <- as.data.frame(str_match(vms$x7, '>\\s*(.*?)\\s*<'))
vms$Message6 <- temp$V2; rm(temp)

vms$x1 <- NULL; vms$x2 <- NULL; vms$x3 <- NULL; 
vms$x4 <- NULL; vms$x5 <- NULL; vms$x6 <- NULL; 
vms$x7 <- NULL; vms$x8 <- NULL; 

# Finally extract VMS message only
vms$V.message <- with(vms, paste0(Message1, " ", Message2, " ", 
                                  Message3, " ", Message4, " ", 
                                  Message5, " ", Message6))

vms$V.message <- gsub("NA","",as.character(vms$V.message))

# remove unnecessary columns
vms$Message1 <- NULL; vms$Message2 <- NULL; 
vms$Message3 <- NULL; vms$Message4 <- NULL; 
vms$Message5 <- NULL; vms$Message6 <- NULL;




