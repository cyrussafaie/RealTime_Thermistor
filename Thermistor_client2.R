#!/usr/bin/env Rscript
source("Thermistor_connection.R")

# global vars
temp_events_counter <- 0                                        # new indication event counter
signals_counter <- 0                                            # our signals counter
BUF_SIZE <- 10000                                               # we create buffers in advance:
temp_events <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),     # dataframe for new temperature observations 
                          temp=as.numeric(rep(NaN,BUF_SIZE)) )  
signals <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),         # dataframe for our signals
                      is_stable=as.logical(rep(NA,BUF_SIZE)),
                      stable_temp=as.numeric(rep(NaN,BUF_SIZE)))

initial_timestamp <- Sys.time()      # start-of-program timestamp
plot_timestamp <- initial_timestamp  # we use plot_timestamp to draw the plot once in a second


# user defined handler
## arguments:
#### rcvd_temp - received temperature (numeric vector of unit length)
## returns:
#### list(send_signal,is_stable,temp) wher 
#### -- list$is_stable is a boolean vector of unit length which value is TRUE when the process becomes stable 
####    and FALSE when the process becomes unstable
#### -- list$temp is an object temperature estimate (numeric vector of unit length)
new_indication_handler <- function(rcvd_temp) {
    now <- Sys.time()
    # log event if you want
    #message("EVENT at ", now, ", temp=", rcvd_temp)
    
    # update temp_events dataframe (append last value):
    temp_events_counter <<- temp_events_counter + 1
    temp_events[temp_events_counter,] <<- list(now, rcvd_temp)
   
    # this is just an example how handler may decide the process is stable
    send_signal <- FALSE
    is_stable <- NA
    stable_temp <- NaN
    require(zoo)
    is.even <- function(x) x %% 2 == 0
    is.odd <- function(x) x %% 2 != 0   
    
    if (signals_counter == 0 &&
        (difftime(now, initial_timestamp, unit = "sec") >= 60) && 
        #(difftime(signals[signals_counter,1], signals[signals_counter-1,1], unit = "sec") >= 35) &&
        (abs(zoo::rollmean(c(0, na.trim(diff(temp_events[1:temp_events_counter, 2], 1))), 250
                      , fill = NA, align = c("right"))) <= 0.0005)[temp_events_counter]
    )
            {
            message(now, " PROCESS IS STABLE NOW")
            send_signal <- TRUE
            is_stable <- TRUE
            stable_temp <- rcvd_temp
    }

        else if (is.odd(signals_counter) &&
         (difftime(temp_events[temp_events_counter,1], initial_timestamp, unit = "sec") >= 10) &&
         (difftime(now, signals[signals_counter,1], unit = "sec") >= 50) &&
         (abs(zoo::rollmean(
                 c(0, na.trim(diff(temp_events[1:temp_events_counter, 2], 1))),
                 400
                 ,
                 fill = NA,
                 align = c("right")
         )) >= 0.0005)[temp_events_counter])
{
        message(now, " PROCESS IS UNSTABLE NOW")
        send_signal <- TRUE
        is_stable <- FALSE
        stable_temp <- rcvd_temp
}

        else if ( is.even(signals_counter) &&
         (difftime(temp_events[temp_events_counter,1], initial_timestamp, unit = "sec") >= 35) &&
         (difftime(now, signals[signals_counter,1], unit = "sec") >= 50) &&
         (abs(zoo::rollmean(
                 c(0, na.trim(diff(temp_events[1:temp_events_counter, 2], 1))),
                 400
                 ,
                 fill = NA,
                 align = c("right")
         )) <= 0.0005)[temp_events_counter])
{
        message(now, " PROCESS IS STABLE NOW")
        send_signal <- TRUE
        is_stable <- TRUE
        stable_temp <- rcvd_temp
}
    
    
    
    
    
    
    
    # TS <- zoo(c(4, 5, 7, 3, 9, 8, NA))
    #  rollapply(TS, width = 3,  FUN = mean, na.rm = F, align = "right") 
    # if else (is.odd(signals_counter)==TRUE && difftime(now, initial_timestamp, unit="sec") >= 10
    #      #         && diff(temp_events[temp_events_counter,2],temp_events[temp_events_counter-1,2])>0
    #      # && diff(temp_events[,2])<0
    #      #&& mean(temp_events[1:temp_events_counter,2]-temp_events[temp_events_counter-1,2])<0)
    #      && mean(diff(temp_events[1:temp_events_counter,2],50))<0)
    # {
    #         message(now, " PROCESS IS STABLE NOW")
    #         send_signal <- TRUE
    #         is_stable <- FALSE
    #         stable_temp <- rcvd_temp
    # }    
    # 
    
    if (send_signal) {
        # update signals dataframe (append last value):
        signals_counter <<- signals_counter + 1
        signals[signals_counter,] <<- list(now, is_stable, stable_temp)
    }

    Draw()
    
    return( list(send_signal=send_signal, is_stable=is_stable, temp=stable_temp) )  # return list of 3 objects
}


# plot once in a second
Draw <- function()
{
    now <- Sys.time()
    if (difftime(now, plot_timestamp, unit='sec') >= 1) {
        plot_timestamp <<- now;
        if (temp_events_counter > 0) {
            # draw temperature chart
            plot(x= difftime(temp_events$time[1:temp_events_counter], initial_timestamp, unit="sec"),
                 y=temp_events$temp[1:temp_events_counter], 
                 type='s', xlab='time (seconds)', ylab='temp');
            
            if (signals_counter > 0) {
                # draw stable and unstable points
                good_signals <- signals[1:signals_counter,]
                points(x=difftime(good_signals$time[good_signals$is_stable], initial_timestamp, unit="sec"),
                       y=good_signals$stable_temp[good_signals$is_stable],
                       pch=19, col='green');  # green circles for stable points
                points(x=difftime(good_signals$time[!good_signals$is_stable], initial_timestamp, unit="sec"),
                       y=good_signals$stable_temp[!good_signals$is_stable],
                       pch=19, col='red');  # red circles for unstable points
            }
        }
    }
}


# server options
host <- "104.236.199.43"
port <- 30001
login <- "Your email"
password <- "Your streaming password"
stream_name <- "Thermistor"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handler
result <- Connect(host, port, login, password, stream_name, new_indication_handler, catch_handler_errors)

# remove empty values from buffers
temp_events <- temp_events[!is.na(temp_events$time),]
signals <- signals[!is.na(signals$time),]

# after all you can dump your data/results and analyze it later
dump(c("temp_events", "signals", "result"), file = "results.txt")

