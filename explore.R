library(lubridate)
library(tidyverse)
library(dplyr)
library(gghighlight)
library(ggrepel)
library(readxl)
library(scales)
library(statar)
library(stringr)
library(tidyr)
library(here)

######### Reading in Data #########
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

######### Processing Rodent Data for 2018 #########
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

nyc_rodents_2018

la_rodents_2018

detroit_rodents_2018






