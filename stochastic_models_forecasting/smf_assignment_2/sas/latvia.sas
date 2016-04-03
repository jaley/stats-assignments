/* load data */
data lat;
infile "/folders/myfolders/LatDol2.dat";
input year month avg end;

/* configure date format */
date = mdy(month, 1, year);
format date MMYYS.;

/* time series visualisations for raw data, unmodified */  
proc sgplot data=lat;
 series x=date y=avg  ;
 xaxis min=14686;

proc sgplot data=lat;
 series x=date y=end;

/* arima fitting */
proc arima data=lat;

identify var=avg(1);
estimate p=1 noint;
forecast id=date interval=month lead=12;

/*
 For avg variable, ARIMA(1, 1, 0) seems to fit well:
 
 Equation: (1 - 0.29972L)(1 - L)Yt = Et
 (note: mean=0, using noint)
 
 Data are clearly not stationary until differenced once.
 Following this, we see 001
 
 Portmanteau statistics are not signficant, residuals are uncorrelated and
 resonably normal, hence model fits well.
*/

identify var=end(1);
estimate noint;
forecast lead=12;

/*
 For end variable, ARIMA(0, 1, 0) seems to fit best:
 
 After first differencing, ACF shows no correlation at lags > 0, so
 process can effectively be modelled as white noise at this point.
 
 Equation: (1 - L)Yt = Et
 (again, zero mean)
 
 Portmanteau statistics are not significant, no residual correlation, etc.
*/


RUN;
