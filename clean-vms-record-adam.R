################################################################################
################################################################################

######### YEAR-2020 ###########
##### Cleaning VMS records to the desired form #####

#packages
library("readxl")
library ("tidyverse")

# import the VMS records
df1 <- read_excel("C:\\Users\\Adam Hill\\Box\\ITS Effectiveness in Utah\\Data\\VMS record cleaning - adjusted\\2020-2021 VMS messages\\vms_20-22_1.xlsx")
df2 <- read_excel("C:\\Users\\Adam Hill\\Box\\ITS Effectiveness in Utah\\Data\\VMS record cleaning - adjusted\\2020-2021 VMS messages\\vms_20-22_2.xlsx")

dfn <- rbind(df1, df2 )
rm(df1); rm(df2)

base <- read_excel("C:\\Users\\Adam Hill\\Box\\ITS Effectiveness in Utah\\Data\\VMS record cleaning - adjusted\\2020-2021 VMS messages\\vms_all.xlsx")

df <- inner_join(dfn, base, by="Device ID")

df$OID_ <- NULL;df$Latitude <- NULL;df$Longitude <- NULL;df$DateInstalled<- NULL; df$Urban_Description <- NULL;df$Access_Category <- NULL;
df$Functional_Class <- NULL;df$Speed_Limit <- NULL;df$Lanes <- NULL;df$AADT2020 <- NULL;df$AADT2019 <- NULL;df$AADT2018 <- NULL; df$SUTRK2020 <- NULL;
df$SUTRK2019 <- NULL;df$SUTRK2018 <- NULL; df$Type <- NULL;df$CUTRK2020 <- NULL;df$CUTRK2019 <- NULL;df$CUTRK2018 <- NULL;

rm(dfn)
# rename the columns
names(df)[names(df) == 'Device ID'] <- 'V.device.id'
names(df)[names(df) == 'AIMSLocationName'] <- 'V.device.location'
names(df)[names(df) == 'Date+Time'] <- 'V.start.time'
names(df)[names(df) == 'Message'] <- 'V.message'
names(df)[names(df) == 'Location'] <- 'V.source.type'
#names(df)[names(df) == 'MsgSourceName'] <- 'V.source.name'


# date and time as character (for some row operations)
df$V.start.time <- as.character(df$V.start.time)


# select the VMS devices of interest only
#df <- df[df$V.device.id == 100245 | df$V.device.id == 100235  | df$V.device.id == 10  | df$V.device.id == 9| 
#           df$V.device.id == 8  | df$V.device.id == 7  | df$V.device.id == 100036  | df$V.device.id == 11 |
#           df$V.device.id == 100215  | df$V.device.id == 100236  | df$V.device.id == 6  | df$V.device.id == 1 |
#           df$V.device.id == 2 | df$V.device.id == 100029  | df$V.device.id == 73  | df$V.device.id == 36  | 
#           df$V.device.id == 32  | df$V.device.id == 35  | df$V.device.id == 100004  | df$V.device.id == 100268  |
#           df$V.device.id == 99997,]

# adding end time of message
df$V.end.time <- sapply(1:nrow(df), function(x) df$V.start.time[x+1]) 


# # fix date and time into two columns
# df$Date <- sapply(strsplit(as.character(df$Date_time),' '), "[", 1)
# df$Time <- sapply(strsplit(as.character(df$Date_time),' '), "[", 2)
# df$Date_time <- NULL

# VMS device location milepost
temp <- as.data.frame(str_match(df$V.device.location, 'MP \\s*(.*?)\\s*,'))
df$V.mile.post <- temp$V2; rm(temp)

# VMS route and bound
df$V.route.bound <- sapply(strsplit(as.character(df$V.device.location),'@'), "[", 1)
df$V.route <- sapply(strsplit(as.character(df$V.route.bound),' '), "[", 1)
df$V.bound <- sapply(strsplit(as.character(df$V.route.bound),' '), "[", 2)
df$V.route.bound <- NULL

# manipulate VMS message column
df$x1 <- sapply(strsplit(as.character(df$V.message),'><'), "[", 1)
df$x2 <- sapply(strsplit(as.character(df$V.message),'><'), "[", 2)
df$x3 <- sapply(strsplit(as.character(df$V.message),'><'), "[", 3)
df$x4 <- sapply(strsplit(as.character(df$V.message),'><'), "[", 4)
df$x5 <- sapply(strsplit(as.character(df$V.message),'><'), "[", 5)
df$x6 <- sapply(strsplit(as.character(df$V.message),'><'), "[", 6)
df$x7 <- sapply(strsplit(as.character(df$V.message),'><'), "[", 7)
df$x8 <- sapply(strsplit(as.character(df$V.message),'><'), "[", 8)
df$V.message1 <- df$V.message
df$V.message <- NULL

# add Beacon column
temp <- as.data.frame(str_match(df$x1, 'BEACON="\\s*(.*?)\\s*"'))
df$V.beacon <- temp$V2

# add Font column
temp <- as.data.frame(str_match(df$x1, 'FONT="\\s*(.*?)\\s*"'))
df$V.font <- temp$V2

# add Flashon column
temp <- as.data.frame(str_match(df$x1, 'FLASHON="\\s*(.*?)\\s*"'))
df$V.flashon <- temp$V2

# add Flashoff column
temp <- as.data.frame(str_match(df$x1, 'FLASHOFF="\\s*(.*?)\\s*"'))
df$V.flashoff <- temp$V2

# extracting exact VMS message
temp <- as.data.frame(str_match(df$x2, '>\\s*(.*?)\\s*<'))
df$Message1 <- temp$V2

temp <- as.data.frame(str_match(df$x3, '>\\s*(.*?)\\s*<'))
df$Message2 <- temp$V2

temp <- as.data.frame(str_match(df$x4, '>\\s*(.*?)\\s*<'))
df$Message3 <- temp$V2

temp <- as.data.frame(str_match(df$x5, '>\\s*(.*?)\\s*<'))
df$Message4 <- temp$V2

temp <- as.data.frame(str_match(df$x6, '>\\s*(.*?)\\s*<'))
df$Message5 <- temp$V2

temp <- as.data.frame(str_match(df$x7, '>\\s*(.*?)\\s*<'))
df$Message6 <- temp$V2; rm(temp)

# remove unnecessary columns
df$x1 <- NULL; df$x2 <- NULL; df$x3 <- NULL; df$x4 <- NULL; df$x5 <- NULL; 
df$x6 <- NULL; df$x7 <- NULL; df$x8 <- NULL; 

# Finally extract VMS message only
df$V.message <- with(df, paste0(Message1, " ", Message2, " ", Message3, " ", 
                                Message4, " ", Message5, " ", Message6))

df$V.message <- gsub("NA","",as.character(df$V.message))


# No of frames and lines in VMS message
df$Message_num <- ifelse (!is.na(df$Message1), 1, 0)
df$Message_num <- ifelse (!is.na(df$Message2), 2, df$Message_num)
df$Message_num <- ifelse (!is.na(df$Message3), 3, df$Message_num)
df$Message_num <- ifelse (!is.na(df$Message4), 4, df$Message_num)
df$Message_num <- ifelse (!is.na(df$Message5), 5, df$Message_num)
df$Message_num <- ifelse (!is.na(df$Message6), 6, df$Message_num)

df$V.frames <- ifelse(df$Message_num > 3, 2, 1)
df$V.lines.per.frame <- df$Message_num/df$V.frames

# remove unnecessary columns
df$Message1 <- NULL; df$Message2 <- NULL; df$Message3 <- NULL; df$Message4 <- NULL;
df$Message5 <- NULL; df$Message6 <- NULL; df$Message7 <- NULL; df$Message8 <- NULL;
df$Message_num <- NULL

# changing date and time to time format
df$V.start.time <- as.POSIXct(df$V.start.time, format= "%Y-%m-%d %H:%M:%S")
df$V.end.time <- as.POSIXct(df$V.end.time, format= "%Y-%m-%d %H:%M:%S")

# add duration of message display (seconds)
df$V.duration <- difftime(df$V.end.time, df$V.start.time)

# drop some columns
df$V.font <- NULL
df$V.beacon <- NULL
df$V.flashon <- NULL
df$V.flashoff <- NULL

# reorder columns
df <- df[c("V.device.id", "V.device.location","V.message", "V.message1", "V.route", "V.bound", 
           "V.mile.post", "V.source.type", "V.start.time", 
           "V.end.time", "V.duration", "V.frames", "V.lines.per.frame")]

# data frame with USER source only
#df <- df[df$V.source.type != "FMS",]
#df <- df[df$V.source.type != "INTERNAL",]
#df <- df[df$V.source.type != "NONE",]


# creates seperate data frames for different message types
#dfa <- df[grepl "SNOW|ICE|SPEED|SLOW|ICY", df$V.message),]
#df_1 <- df[ grepl(paste("SNOW", collapse="|") , df$V.message),] 
#df_2 <- df[ grepl(paste("SLOW", collapse="|"), df$V.message),] 
#df_3 <- df[ grepl(paste("ICE", collapse="|"), df$V.message),]
#df_4 <- df[ grepl(paste("ICY", collapse="|"), df$V.message),]
#df_5 <- df[ grepl(paste("SPEED", collapse="|"), df$V.message),]
#dfn <- rbind(df_1, df_2, df_3, df_4, df_5 )

#test statement, but it overwrites itself
#df$V.intent <- ifelse (grepl("SNOW", df$V.message)==TRUE, "Weather", "na")
#df$V.intent <- ifelse (grepl("ICE", df$V.message)==TRUE, "Weather", "na")
#df$V.intent <- ifelse (grepl("SLOW", df$V.message)==TRUE, "Weather", "na")

#basically "or" statements. can have multiple classifications
df$V.intent <- case_when(
  grepl("ICE", df$V.message1)|grepl("ICY", df$V.message1)|grepl("WINTER", df$V.message1)|grepl("FOG", df$V.message1)|grepl("SLICK", df$V.message1)|grepl("WATER", df$V.message1)|grepl("SNOW", df$V.message1) ~ "weather",
  TRUE~"na"
)
df$V.intent <- ifelse (grepl("CHAINS", df$V.message1), "na", df$V.intent)

#Selecting only the messages most explicitly saying to reduce speed.
#Implied might also be effective, but that's not the purpose of the study
df$V.slow <- ifelse (grepl("SLOW", df$V.message1), TRUE, FALSE)
df$V.caution <- ifelse (grepl("CAUTION", df$V.message1), TRUE, FALSE)
df$V.speed <- ifelse (grepl("SPEED", df$V.message1), TRUE, FALSE)

df$V.canyon <- case_when(
  df$V.device.id == "100006" | df$V.device.id == "100007" ~ "Sardine",
  df$V.device.id == "100149" ~ "Ogden",
  df$V.device.id == "100270" ~ "Weber",
  df$V.device.id == "100223" | df$V.device.id == "100225" | df$V.device.id == "100227" | df$V.device.id == "100224" ~ "Provo",
  TRUE~"na"
)

df$V.select.location <- case_when(
  df$V.device.id == "100006" | df$V.device.id == "100007" ~ "true",
  df$V.device.id == "100149" ~ "true",
  df$V.device.id == "100270" ~ "true",
  df$V.device.id == "100223" | df$V.device.id == "100225" | df$V.device.id == "100227" | df$V.device.id == "100224" ~ "true",
)
df1 <- df %>%
  filter(V.intent == "weather")

df1$V.select.message <- case_when(
  df1$V.slow == TRUE  ~ "true",
  df1$V.speed == TRUE ~ "true",
  df1$V.caution == TRUE ~ "true"
)

#df1 <- df[df$V.intent != "na",]

df2 <- df1%>%
  filter(V.select.message == "true")%>%
  filter(V.select.location == "true")
  
#df2 <- df1[df1$V.selected == "Sardine",]

# write the file with VMS records recorded to crashes
write.csv(df2, file = "VMS_records_weather.csv")
saveRDS(df, file = "VMS_records.rds")

#rm(df); 

################################################################################
################################################################################