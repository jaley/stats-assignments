require(ggplot2)
library(scales)

## Load data from text file
rainfall <- read.table("data/MelbourneAirport.txt", header=T)
rainfall$date <- as.Date(ISOdate(rainfall$Year, rainfall$Month, rainfall$Day))

### Question 1 
### (i)

## Overall average rainfall
mean(rainfall$Rain)

# Answer: average proportion days with rain = 0.392334

## Average rainfall by month:
monthly.rainfall <- aggregate(Rain ~ Year + Month, data=rainfall, FUN=mean)
monthly.rainfall$date <- as.Date(ISOdate(monthly.rainfall$Year, 
                                         monthly.rainfall$Month, 1))

# plot to check for seasonality
ggplot(monthly.rainfall, aes(x=date, y=Rain)) +
  geom_line() +
  scale_x_date(labels = date_format("%b %Y")) +
  xlab("") +
  ylab("Proportion of days with rain in month")

# Answer: we can clear seasonality every 12 months, with higher proportions
# of rainfall in the winter months (for the southern hemisphere), peaking around
# June, and lower proportions in the summer, around December and January.

### (ii)

# Load the fitting code from lectures
source('hmm_fitting.R')

# Fit models

# For a single state, both pi0 and gamma0 must be scalar
rainfall.mle1 <- binary.HMM.mle(rainfall$Rain, 1, 0.99, 0.99)
rainfall.mle2 <- binary.HMM.mle(rainfall$Rain, 2, 
                                c(0.9, 0.1), 
                                matrix(c(0.9, 0.1, 
                                         0.1, 0.9), 
                                       nrow = 2))
rainfall.mle3 <- binary.HMM.mle(rainfall$Rain, 3,
                                c(0.7, 0.2, 0.1), 
                                matrix(c(0.7, 0.2,  0.1, 
                                         0.2, 0.7, 0.1,
                                         0.1,  0.2, 0.7), 
                                       nrow = 3))

# Answer:

# m=1:
# pi = 0.392
# gamma = 1
# delta = 1

# m=2:
# pi = (0.848, 0.000)
# gamma = 0.654, 0.346,
#         0.298, 0.702,
# delta = (0.463, 0.537)

# m=3
# pi = (1.000, 0.000, 0.000)
# gamma = 0.555, 0.243, 0.203,
#         0.392, 0.608, 0.001,
#         0.218, 0.000, 0.782
# delta = (0.392, 0.243, 0.365)

### (iii)

# m  neg-ll    aic       bic
# 1  9785.496  19572.99  19580.58
# 2  9254.946  18517.89  18548.25  ***
# 3  9249.611  18517.22  18585.53

# The model with m=2 gives the best fit, as this gives the best
# score by BIC and almost the same score as m=3 for AIC.

# The additional third state in m=3 seems to only give a small improvement in terms of 
# likelihood, but as the growth in parameters is quadratic in m, we see the
# penalities in AIC and more so in BIC lead us to pick the more parsimonious model

# We can see that we have some natural parameter values in m=3 for both gamma and
# pi that are very close to zero and one. This can cause problems with convergence,
# and if we truly believe that a good model for this data should have a third state,
# we would probably do better by constraining these parameters to zero or one 
# respectively, then just estimating the remaining parameters. With no intuitive basis
# for this, however, it doesn't seem like a good approach so we'd probably be better
# off working with the two-state model.

### (iv)

# Perform local decoding for 3-state model
local.decoding.3 <- binary.HMM.local_decoding(rainfall$Rain,
                                              3,
                                              rainfall.mle3$pi,
                                              rainfall.mle3$gamma)

# Perform global decoding for 3-state model
global.decoding.3 <- binary.HMM.viterbi(rainfall$Rain,
                                        3,
                                        rainfall.mle3$pi,
                                        rainfall.mle3$gamma)

# Add to data frame
rainfall$local3 <- local.decoding.3
rainfall$global3 <- global.decoding.3

head(rainfall, n = 20)

#    Year Month Day Rain local3 global3       
# 1  1971     1   1    1      1       1
# 2  1971     1   2    1      1       1
# 3  1971     1   3    1      1       1
# 4  1971     1   4    1      1       1
# 5  1971     1   5    1      1       1
# 6  1971     1   6    0      3       3
# 7  1971     1   7    0      3       3
# 8  1971     1   8    0      3       3
# 9  1971     1   9    0      3       3
# 10 1971     1  10    0      3       3
# 11 1971     1  11    0      3       3
# 12 1971     1  12    0      3       3
# 13 1971     1  13    0      3       3
# 14 1971     1  14    1      1       1
# 15 1971     1  15    0      3       3
# 16 1971     1  16    0      3       3
# 17 1971     1  17    0      3       3
# 18 1971     1  18    0      3       3
# 19 1971     1  19    0      3       3
# 20 1971     1  20    0      3       3

# plot
ggplot(rainfall, aes(x=date, y=local3)) +
  geom_line() +
  scale_x_date() +
  xlim(c(as.Date(ISOdate(2008, 1, 1)), as.Date(ISOdate(2010, 1, 1))))

### (v)

mean(rainfall$local3 == rainfall$global3)
# == 1, therefore the local and global decodings are the same

# median value for pi is pi2, therefore table for global decoded state = 2:
state.2 <- rainfall[rainfall$global3 == 2, ]
head(state.2, n = 20)

#     Year Month Day Rain local3 global3
# 26  1971     1  26    0      2       2
# 28  1971     1  28    0      2       2
# 33  1971     2   2    0      2       2
# 36  1971     2   5    0      2       2
# 37  1971     2   6    0      2       2
# 38  1971     2   7    0      2       2
# 39  1971     2   8    0      2       2
# 53  1971     2  22    0      2       2
# 72  1971     3  13    0      2       2
# 73  1971     3  14    0      2       2
# 83  1971     3  24    0      2       2
# 84  1971     3  25    0      2       2
# 87  1971     3  28    0      2       2
# 88  1971     3  29    0      2       2
# 89  1971     3  30    0      2       2
# 111 1971     4  21    0      2       2
# 113 1971     4  23    0      2       2
# 114 1971     4  24    0      2       2
# 115 1971     4  25    0      2       2
# 117 1971     4  27    0      2       2

# Characterisation of states:

# State 1: Represents a rainy spell, where it will almost certainly rain.
#          From this state, we have a reasonably high (0.55) probability of
#          staying here, i.e. rain continuing. We will otherwise move to state
#          2 or 3 with similar probabilities, see below.
#
# States 2 and 3: these both represent dry spells, where there is likely no rain
#                 at all. State 2 represents a slightly longer dry spell than
#                 state 3.
#
# Given that states 2 and 3 are very similar in behaviour, but for slightly different
# probabilities of returning to the rainy state (there is no connection between them),
# it feels like we could probably represent this data quite well with the two state
# model, where there's simply a rainy spell and a dry spell state.
