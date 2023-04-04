pacman::p_load(tidyverse, lubridate)

# load in flow and control data
df <- read_csv("data/flowdata.csv")
df1 <- read_csv("data/controldata.csv")

df <- df %>% 
  rename(MFlow = `MFlow(Veh/5Min)`,
         OFlow = `OFlow(Veh/5Min)`) %>% 
  mutate(Mtime = mdy_hm(Mtime))

df1 <- df1 %>% 
  rename(MFlow = `MFlow(Veh/5Min)`,
         OFlow = `OFlow(Veh/5Min)`) %>% 
  mutate(Mtime = mdy_hm(Mtime))

# add diversion rates column
df <- df %>% 
  mutate(divrate = OFlow / (MFlow + OFlow))

df1 <- df1 %>% 
  mutate(divrate = OFlow / (MFlow + OFlow))

# for now, filter to just the crash
df <- df %>% 
  filter(Crash == 87)
df <- df %>% 
  filter()

# weighted average exit rate
df_tab <- df %>% 
  group_by(Crash, Mtime) %>% 
  summarise(waer_f = sum(divrate * MFlow)/sum(MFlow)) %>% 
  mutate(hour = hour(Mtime),
         min = minute(Mtime))

# weighted average exit rate
df1_tab <- df1 %>% 
  group_by(Crash, Mtime) %>% 
  summarise(waer_c = sum(divrate * MFlow)/sum(MFlow)) %>% 
  mutate(hour = hour(Mtime),
         min = minute(Mtime))

# left join
df_tab_combine <- left_join(df_tab, df1_tab, by = c("Crash", "min", "hour")) %>% 
  select(!c(hour, min))

# don't do holidays for the previous week
# probably throw out holidays, hard to find control
# get the difference in control and crash and can look at it as a function of time
# pull in year as well for the left join becuase the numbers reset

t.test(df_tab_combine$waer_f, df_tab_combine$waer_c, 
       paired = TRUE)

                            