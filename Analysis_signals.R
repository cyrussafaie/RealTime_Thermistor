dim(temp_events)
plot(temp_events$temp,type = "l")
class(temp_events)

hist(temp_events$temp)
diff(temp_events$temp)
?diff

library(dplyr)
library(plyr)
library(rollply)
library(dtplyr)
library(zoo)
temp_events=temp_events %>%
        #group_by(group) %>%
        mutate(Diff = c(0, diff(temp_events$temp)))
temp_events=temp_events %>%
        mutate(mean_past10=rollapply(Diff,list(-10:0), function(a) mean(a),fill = NA))

plot(temp_events$mean_past10)
hist(temp_events$Diff)
min(temp_events$Diff)


write.csv(temp_events,"time_events.csv",row.names = F)
#temp_events_org=temp_events
