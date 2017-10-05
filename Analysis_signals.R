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
temp_events_org=temp_events_org[,1:2]
names(temp_events_org)

temp_events_org=temp_events_org %>%
        #group_by(group) %>%
        mutate(Diff = c(0, diff(temp_events_org$temp)))
temp_events_org=temp_events_org %>%
        mutate(mean_past10=rollapply(Diff,list(-10:0), function(a) mean(a),fill = NA)
               ,mean_past20=rollapply(Diff,list(-20:0), function(a) mean(a),fill = NA)
               ,mean_past40=rollapply(Diff,list(-40:0), function(a) mean(a),fill = NA))
plot(temp_events_org$Diff)
plot(temp_events_org$mean_past10)
plot(temp_events_org$mean_past20)
plot(temp_events_org$mean_past40)
abline(h=0.002, col='red')
hist(temp_events$Diff)
min(temp_events$Diff)


write.csv(temp_events,"time_events.csv",row.names = F)
#temp_events_org=temp_events
