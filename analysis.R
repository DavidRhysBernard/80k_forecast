# code for basic analysis of 80000 hours podcast data

library(tidyverse)
library(lubridate)

dat <- read_csv("episodes.csv")


### graphs
ggplot(dat, aes(x=hours)) + geom_histogram(binwidth=0.25, center = 0.125) +
    coord_cartesian(xlim=c(0,5)) + scale_y_continuous(breaks=c(0,2,4,6,8,10,12)) +
    theme_minimal()

ggplot(dat, aes(x=date, y=hours)) + geom_point() + 
    coord_cartesian(ylim=c(0,5)) + theme_minimal()

# cumulative frequency graph
ggplot(dat, aes(x=date, y=cumsum)) + geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) + theme_minimal()

# yearly
max(dat$cumsum)/3
reg <- lm(cumsum ~ date, dat)
reg$coefficients[2] * 366

# yearly totals
cumsum2020 <- max(dat$cumsum) - max(dat$cumsum[dat$year==2019])
max(dat$cumsum[dat$year==2019]) - max(dat$cumsum[dat$year==2018])
max(dat$cumsum[dat$year==2018]) - max(dat$cumsum[dat$year==2017])
max(dat$cumsum[dat$year==2017]) 

#2020 rate
drate20 <- cumsum2020/180

# first half of year totals
fir_18 <- max(dat$cumsum[dat$year==2018 & dat$month<7]) - max(dat$cumsum[dat$year==2017])
fir_19 <- max(dat$cumsum[dat$year==2019 & dat$month<7]) - max(dat$cumsum[dat$year==2018])

# yearly rates + prediction from regression
reg17 <- lm(cumsum ~ date, dat[dat$year==2017,])
reg17$coefficients[2]
reg17$coefficients[2] * 366

reg18 <- lm(cumsum ~ date, dat[dat$year==2018,])
reg18$coefficients[2]
reg18$coefficients[2] * 366

reg19 <- lm(cumsum ~ date, dat[dat$year==2019,])
reg19$coefficients[2]
reg19$coefficients[2] * 366

reg20 <- lm(cumsum ~ date, dat[dat$year==2020,])
reg20$coefficients[2]
reg20$coefficients[2] * 366

# amandango's extrapolations
sec_17 <- max(dat$cumsum[dat$year==2017]) - max(dat$cumsum[dat$year==2017 & dat$month<7])
sec_18 <- max(dat$cumsum[dat$year==2018]) - max(dat$cumsum[dat$year==2018 & dat$month<7])
sec_19 <- max(dat$cumsum[dat$year==2019]) - max(dat$cumsum[dat$year==2019 & dat$month<7])

drate17 <- sec_17/183
drate18 <- sec_18/183
drate19 <- sec_19/183

avg_drate <- mean(c(drate17, drate18, drate19))

186 * avg_drate
186 * drate17
186 * drate18
186 * drate19
186 * drate20

cumsum2020 + 186*avg_drate
cumsum2020 + 186*drate17
cumsum2020 + 186*drate18
cumsum2020 + 186*drate19
cumsum2020 + 186*drate20
cumsum2020 + 186*0.2
