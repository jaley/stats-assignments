/* load data */
data lat;
infile "/folders/myfolders/LatDol2.dat";
input year month avg end;

/* configure date format */
date = mdy(month, 1, year);
format date MMYYS.;

proc arima data=lat;

identify var=end(1);
estimate noint;
identify var=avg(1) crosscor=end(1);
estimate input=(1 $ / (1) end) noint;
forecast lead=3;

/*

Model eqn:

dYt = 0.586L/(1 + 0.224L)dXt + Et

Forecasts for variable avg
Obs	Forecast	Std Error	95% Confidence Limits
145	0.5903	0.0055	0.5794	0.6011
146	0.5909	0.0092	0.5729	0.6088
147	0.5907	0.0113	0.5686	0.6129
*/


run;