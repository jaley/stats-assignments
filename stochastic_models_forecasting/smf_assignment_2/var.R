# Load data
latdol.df <- read.table("~/Downloads/LatDol2.dat", 
                        header=F,
                        col.names = c("year", "month", "avg", "end"))

# Create the time series object
end.avg <- data.frame(dend=diff(latdol.df$end), 
                      davg=diff(latdol.df$avg))
end.avg.ts <- ts(end.avg)

# Visualise first order differences
plot(end.avg.ts)

# Fit model
ae.ar <- ar(end.avg.ts, order.max = 10)

# AIC is minimized at VAR(5):
#
# ae.ar$aic
# 0           1           2           3           4           5           6 
# 163.8942334  12.8969310  10.5168392   0.8765526   1.0855581   0.0000000   2.5364097 
# 7           8           9          10 
# 7.1443091   8.3526617  14.7442086  18.0807729 

# Pull in portes package to perform Portmanteau tests
require(portes)
portest(ae.ar)

# Lags Statistic       df      pvalue
# 5  8.946211  0.00000 0.000999001
# 10 24.448968 11.42857 0.000999001
# 15 41.469536 26.45161 0.010989011
# 20 57.767384 41.46341 0.018981019
# 25 72.615234 56.47059 0.037962038
# 30 88.362401 71.47541 0.057942058

# Produce forecasts 3 months ahead
p <- predict(ae.ar, n.ahead=3)

p.davg <- c(end.avg.ts[,2], p$pred[,2])
p.avg <- diffinv(p.davg, xi=latdol.df$avg[1])

# Predicted next three months:
# 0.5870314 
# 0.5885197 
# 0.5899396
