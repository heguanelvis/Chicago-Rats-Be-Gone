###############################################
######### Project Libraries and Setup #########
###############################################

library(lubridate)
library(tidyverse)
library(dplyr)
library(gghighlight)
library(ggrepel)
library(ggthemes)
library(readxl)
library(scales)
library(statar)
library(stringr)
library(tidyr)
library(here)

options(digits=4)

###################################
######### Reading in Data #########
###################################

chicago_rodents <- read_csv(here("data", 
                                 "Chicago_311_Rodent_2014-2018.csv"))

boston_311 <- read_csv(here("data", 
                            "Boston_311.csv"))

dc_311_2018 <- read_csv(here("data",
                        "DC_Service_Requests_2018.csv"))

nyc_rodents_2018 <- read_csv(here("data",
                             "NYC_311_Rodent_2018.csv"))

la_rodents_2018 <- read_csv(here("data",
                                 "LA_Dead_Animal_Removal_2018.csv"))

detroit_rodents_2018 <- read_csv(here("data",
                                      "Detroit_Rodent_2018.csv"))

###################################################
######### Processing Rodent Data for 2018 #########
###################################################

chicago_rodents_2018 <-
  chicago_rodents %>%
  filter(year(mdy(`Creation Date`)) == 2018)

boston_rodents_2018 <-
  boston_311 %>%
  filter(str_detect(TYPE, "Rodent"),
         year(open_dt) == 2018)

dc_rodents_2018 <-
  dc_311_2018 %>%
  filter(str_detect(SERVICECODEDESCRIPTION, "Rodent"))

##################################################################
######### 2018 Citywise Rodent Complaint Comparison Plot #########
##################################################################

city_mon_summary <- function(df, date_col, city_name) {
  monthly_summary <-
    df %>%
    group_by(month(date_col, 
                   label = TRUE, 
                    abbr = TRUE)) %>%
    summarise(`Number of Complaints` = n(),
                                City = city_name) %>%
    rename(Month = 1)
  return(monthly_summary)
}

chicago_months <- city_mon_summary(chicago_rodents_2018,
                                   mdy(chicago_rodents_2018$`Creation Date`),
                                   "Chicago")

boston_months <- city_mon_summary(boston_rodents_2018, 
                                  boston_rodents_2018$open_dt, 
                                  "Boston")

dc_months <- city_mon_summary(dc_rodents_2018,
                              dc_rodents_2018$ADDDATE,
                              "Washington DC")

nyc_months <- city_mon_summary(nyc_rodents_2018,
                               mdy_hms(nyc_rodents_2018$`Created Date`),
                               "New York City")

la_months <- city_mon_summary(la_rodents_2018,
                              mdy_hms(la_rodents_2018$CreatedDate),
                              "Los Angeles")

detroit_months <- city_mon_summary(detroit_rodents_2018,
                                   mdy_hms(detroit_rodents_2018$`Created At`),
                                   "Detroit")

city_months_18 <- bind_rows(chicago_months, 
                            boston_months, 
                            dc_months, 
                            nyc_months,
                            la_months,
                            detroit_months)

p1 <-
  city_months_18 %>%
  ggplot(aes(x = Month, 
             y = `Number of Complaints`, 
         group = City,
         color = City)) +
  geom_point() +
  geom_line() + 
  geom_line(data = chicago_months,
            size = 1.1) +
  labs(title = "Rat Complaints by highly affected US Cities in 2018",
    subtitle = "Chicago generally has the most rat complaints all year round.",
     caption = "Source: City Open Data Portals") +
  scale_colour_wsj("colors6", "") +
  theme_wsj() +
  theme(plot.title = element_text(size = 15),
     plot.subtitle = element_text(size = 13),
      plot.caption = element_text(size = 12),
       plot.margin = margin(t = 30, 
                            r = 30, 
                            b = 30, 
                            l = 30, 
                         unit = "pt"))

ggsave(here("output", "city_compare.pdf"), 
       plot = p1, 
      width = 9, 
     height = 6,
      units = "in")


