dim(temp_events)
plot(temp_events$temp,type = "l")
hist(temp_events$temp)
plot(diff(temp_events$temp))
class(temp_events)

#for stable diff has to be 0 with prior and mean of 10 prior diffs should also be 0 
library(dplyr)
library(plyr)
library(rollply)
library(dtplyr)
library(zoo)
temp_events_org=temp_events
plot(temp_events_org[,2])
#names(temp_events_org)
tail(temp_events_org)



temp_events_org$time=strptime(temp_events_org$time, "%Y-%m-%d %H:%M:%S")
temp_events_org$secs=60*60*temp_events_org$time$hour+60*temp_events_org$time$min+temp_events_org$time$sec
head(temp_events_org)

tempMeanPerSecond=aggregate(temp ~ secs, temp_events_org, mean)
plot(diff(tempMeanPerSecond$temp))
par(new=T)
plot(tempMeanPerSecond$temp,col=2)
par(new=T)
plot(rollapply(diff(tempMeanPerSecond$temp),list(-50:0), function(a) mean(a),fill = NA),col=3)

dim(temp_events_org)
dim(tempMeanPerSecond)
difftime(temp_events_org$time[temp_events_counter], temp_events_org$time[temp_events_counter-1], unit="sec")
head(temp_events_org)

temp_events_org=temp_events_org %>%
        #group_by(group) %>%
        mutate(Diff = c(0, diff(temp_events_org$temp)))
plot(temp_events_org[,2])
par(new=T)
plot(temp_events_org$Diff,col=2)

temp_events_org=temp_events_org %>%
        mutate(mean_past10=rollapply(Diff,list(-10:0), function(a) mean(a),fill = NA)
               ,mean_past20=rollapply(Diff,list(-20:0), function(a) mean(a),fill = NA)
               ,mean_past40=rollapply(Diff,list(-40:0), function(a) mean(a),fill = NA))
tail(temp_events_org,400)
dim(temp_events_org)


plot(temp_events_org$Diff)
plot(temp_events_org$mean_past10)
plot(temp_events_org$mean_past20)
plot(temp_events_org$mean_past40)
abline(h=0.002, col='red')
hist(temp_events$Diff)
min(temp_events$Diff)


write.csv(temp_events,"time_events.csv",row.names = F)
temp_events_org=read.csv("time_events.csv")
#temp_events_org=temp_events
aa=zoo::rollmean(c(0, diff(temp_events_org[,2], 1)), 300
              , fill = NA, align = c("right"))
plot(abs(aa))
plot(temp_events_org[,2])
