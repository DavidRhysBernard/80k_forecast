# code for basic analysis of 80000 hours podcast data

library(tidyverse)
library(lubridate)

dat <- read_csv("episodes.csv")

dat <- dat[order(dat$date),]
dat <- mutate(dat, cumsum = cumsum(hours))

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
reg$coefficients[2] * 365

# 2020
max(dat$cumsum) - dat$cumsum[67]

reg20 <- lm(cumsum ~ date, dat[dat$date > as.Date('2020-01-01'),])
reg20$coefficients[2] * 365

(summary(reg20)$coefficients[2,1] + 2*summary(reg20)$coefficients[2,2]) * 365 # upper bound




