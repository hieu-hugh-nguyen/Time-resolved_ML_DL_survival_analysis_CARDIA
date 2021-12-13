# plot the cumulative number of censored cases over time of follow-up:
cum_freq_censored = function(time, breaks){ 
  hist = hist(time, breaks = breaks)
  breaks = hist$breaks
  duration.cut = cut(time, breaks, right=FALSE) 
  duration.freq = table(duration.cut)
  cumfreq0 = c(0, cumsum(duration.freq)) 
  plot(breaks/365.25, cumfreq0)
  lines(breaks/365.25,cumfreq0)
}

# example: 
time = data.full$time[data.full$event == 0 & data.full$time < 365.25*23]
cum_freq_censored(time, breaks = 25)
