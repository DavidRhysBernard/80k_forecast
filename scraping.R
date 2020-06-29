# code for downloading 80000 hours podcast data

library(tidyverse)
library(rvest)
library(RSelenium)
library(splitstackshape)
library(lubridate)

### Extract info
#Specifying the url for desired website to be scraped
url <- 'https://podcasts.apple.com/us/podcast/80-000-hours-podcast/id1245002988'

# Download binaries, start driver, and get client object.
rd <- rsDriver(browser = "chrome", port = 4443L, chromever = "83.0.4103.39")
ffd <- rd$client

# Navigate to page.
ffd$navigate('https://podcasts.apple.com/us/podcast/80-000-hours-podcast/id1245002988')

Sys.sleep(5)

for (i in 1:12){
    # Find the load button and assign, then send click event.
    load_btn <- ffd$findElement(using = "css selector", ".list-button .link")
    load_btn$clickElement()
    
    # Wait for elements to load.
    Sys.sleep(2)
}

# Get HTML data 
html_data <- ffd$getPageSource()[[1]]

#Reading the HTML code from the website
webpage <- read_html(html_data)

# extracting episodes
episodes <- webpage %>% html_nodes(".tracks__track--podcast") %>% html_text()


### Cleaning data
# sorting into table
episodes <- as_tibble(episodes)
episodes <- cSplit(episodes, "value", "\n")
episodes <- select(episodes, -c(3,5))
colnames(episodes) <- c('date', 'title', 'description', 'length')

#cleaning variables
episodes$date <- mdy(episodes$date)
episodes$title <- as.character(episodes$title)
episodes$description <- as.character(episodes$description)
episodes$length <- as.character(episodes$length)

# keeping only new episodes
episodes <- filter(episodes, !str_detect(title, 'Classic episode'))
episodes <- filter(episodes, str_detect(title, '#|Rob & Howie|Arden & Rob|plastic straws')) 

# adding '0 hr ' to those less than one hour
episodes$length <- ifelse(!str_detect(episodes$length, "hr"),  
                          str_c("0 hr ", episodes$length), episodes$length)

# in terms of hours
episodes$hours <- hm(episodes$length)
episodes$hours <- hour(episodes$hours) + minute(episodes$hours)/60

# year and month
episodes$year <- as.numeric(str_sub(episodes$date, 1, 4))
episodes$month <- as.numeric(str_sub(episodes$date, 6, 7))

# cumulative hours
episodes <- episodes[order(episodes$date),]
episodes <- mutate(episodes, cumsum = cumsum(hours))

write_csv(episodes, "episodes.csv")




