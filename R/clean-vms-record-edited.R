################################################################################
################################################################################

######### YEAR-2020 ###########
##### Cleaning VMS records to the desired form #####

#packages
library("stringr")
library("readxl")

# import the VMS records
df1 <- read_excel("VMS record cleaning - sample/2020/VMS History 01 2020.xlsx")
df2 <- read_excel("VMS record cleaning - sample/2020/VMS History 02 2020.xlsx")

df <- rbind(df1, df2)
rm(df1); rm(df2); 

# rename the columns
names(df)[names(df) == 'Device ID'] <- 'V.device.id'
names(df)[names(df) == 'DeviceName'] <- 'V.device.location'
names(df)[names(df) == 'MsgDateTime'] <- 'V.start.time'
names(df)[names(df) == 'Message'] <- 'V.message'
names(df)[names(df) == 'MsgSourceType'] <- 'V.source.name'


# date and time as character (for some row operations)
df$V.start.time <- as.character(df$V.start.time)


# select the VMS devices of interest only
# df <- df[df$V.device.id == 100245 | df$V.device.id == 100235  | df$V.device.id == 10  | df$V.device.id == 9| 
#            df$V.device.id == 8  | df$V.device.id == 7  | df$V.device.id == 100036  | df$V.device.id == 11 |
#            df$V.device.id == 100215  | df$V.device.id == 100236  | df$V.device.id == 6  | df$V.device.id == 1 |
#            df$V.device.id == 2 | df$V.device.id == 100029  | df$V.device.id == 73  | df$V.device.id == 36  | 
#            df$V.device.id == 32  | df$V.device.id == 35  | df$V.device.id == 100004  | df$V.device.id == 100268  |
#            df$V.device.id == 99997,]

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

# create source type column
df$V.source.type <- ifelse (df$V.source.name=="FMS", "FMS", "")
df$V.source.type <- ifelse (df$V.source.name=="INTERNAL", "INTERNAL", df$V.source.type)
df$V.source.type <- ifelse (df$V.source.name=="NONE", "NONE", df$V.source.type)
df$V.source.type <- ifelse (grepl("SCHEDULER", df$V.source.name)==TRUE, "SCHEDULER", df$V.source.type)
df$V.source.type <- ifelse (df$V.source.name=="VSLAutomation", "VSL", df$V.source.type)

# data frame with USER source only
df <- df[df$V.source.type == "",]

# reorder columns
df <- df[c("V.device.id", "V.device.location","V.route", "V.bound", 
           "V.mile.post", "V.source.name", "V.start.time", 
           "V.end.time", "V.duration", "V.frames", "V.lines.per.frame", 
           "V.font", "V.beacon", "V.flashon", "V.flashoff","V.message")]

# drop some columns
df$V.font <- NULL
df$V.beacon <- NULL
df$V.flashon <- NULL
df$V.flashoff <- NULL
df$V.source.type <- NULL
df$V.source.name <- NULL


# data frame with crash messages only
df1 <- df[ grepl(paste("CRASH", collapse="|"), df$V.message),] 


# write the file with VMS records recorded to crashes
write.csv(df1, file = "1.VMS_records_crashes.csv")
saveRDS(df1, file = "1.VMS_records_crashes.rds")

#rm(df); 

################################################################################
################################################################################