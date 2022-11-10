# load packages
pacman::p_load(tidyverse, lubridate, zoo)

# load data
devices <- read_csv("data/vms_devices.csv")
msg <- read_csv("data/vms_messages.csv")
msg_2 <- read_csv("data/vms_messages_2.csv")

# clean data
msg_2 <- msg_2 %>%
  mutate(AIMS_Location = ...2,
         Device_ID = `Device ID`,
         Timestamp = `Date+Time`) %>%
  filter(Device_ID, AIMS_Location, Timestamp, Length, Category, Condensed, 
         Message, Location)
