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
library(httr)
library(jsonlite)
library(magrittr)

options(digits = 4)

###################################
######### Reading in Data #########
###################################

census_tracts <- 
  read_csv(here("data", 
                "tract_community.csv"))

community_numbers <-
  read_excel(here("data", 
                  "community_numbers.xlsx"))

chicago_rodents <- 
  read_csv(here("data", 
                "Chicago_311_Rodent_2014-2018.csv"))

boston_311 <- 
  read_csv(here("data",
                "Boston_311.csv"))

dc_311_2018 <- 
  read_csv(here("data",
                "DC_Service_Requests_2018.csv"))

nyc_rodents_2018 <- 
  read_csv(here("data",
                "NYC_311_Rodent_2018.csv"))

la_rodents_2018 <- 
  read_csv(here("data",
                "LA_Dead_Animal_Removal_2018.csv"))

detroit_rodents_2018 <- 
  read_csv(here("data",
                "Detroit_Rodent_2018.csv"))

##########################################
######### Scraping Data from ACS #########
##########################################

base <- "https://api.census.gov/data/2016/acs/acs5?"
acs_vars <- "get=B00001_001E,B07013_001E,B07013_003E,B19301_001E"
state_tracts <- "&for=tract:*&in=state:17"
AUTH_TOKEN <- 
  read_delim(here("data", "Auth.txt"), 
             delim = "/", 
         col_names = F)$X1
key <- paste("&key=", AUTH_TOKEN, sep = "")

url <- paste(base, acs_vars, state_tracts, key, sep = "")
response <- GET(url)
all_il_tracts <- 
  jsonlite::fromJSON(content(response, as = "text")) %>%
  data.frame() %>%
  slice(-1)

colnames(all_il_tracts) <- 
  c("Population",
    "Total_Living_In_Area",
    "Renter_Count",
    "Income_Per_Capita",
    "State",
    "County",
    "Tract")

###################################
######### Preparing Data  #########
###################################

census_tracts <-
  census_tracts %>%
  left_join(community_numbers, 
            by = c("Community Area" = "Number"))

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

chicago_rat_community_17 <-
  chicago_rodents %>%
  filter(year(mdy(`Creation Date`)) == 2017) %>%
  group_by(`Community Area`) %>%
  summarise(`Number of Complaints` = n()) %>%
  inner_join(community_numbers, by = c("Community Area" = "Number")) %>%
  select(Community, `Number of Complaints`)

to_numeric <- 
  function(column) {
    return(as.numeric(levels(column))[column])
    }

Chicago_communities <- 
  all_il_tracts %>%
  mutate(Census_Tract = str_c(State, County, Tract),
         Total_Income = to_numeric(Income_Per_Capita) * 
                        to_numeric(Population)) %>%
  select(Census_Tract, 
         Population,
         Total_Income,
         Total_Living_In_Area, 
         Renter_Count) %>%
  mutate(Census_Tract = as.numeric(Census_Tract)) %>%
  inner_join(census_tracts, 
             by = c("Census_Tract" = "Census Tract")) %>%
  mutate_at(vars(-"Community",
                 -"Total_Income"), 
            .funs = "to_numeric") %>%
  select(-Census_Tract, -`Community Area`)

Chicago_communities <- 
  Chicago_communities %>%
  group_by(Community) %>%
  summarise_all(.funs = sum) %>%
  mutate(`Income Per Capita` = round(Total_Income / Population, 0),
         `Renter Proportion` = Renter_Count / Total_Living_In_Area) %>%
  select(Community, Population, `Renter Proportion`, `Income Per Capita`)

chicago_rat_community_17 <-
  Chicago_communities %>%
  inner_join(chicago_rat_community_17, by = "Community")

chicago_rat_community_17 %<>%
  mutate(`Number of Requests per 10000 People` = 
           round(`Number of Complaints` / Population * 10000, 0)
         )
  
#####################################################################
######### Plot 1: 2018 Citywise Rodent Complaint Comparison #########
#####################################################################

city_mon_summary <- 
  function(df, date_col, city_name) {
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

chicago_months <- 
  city_mon_summary(chicago_rodents_2018,
                   mdy(chicago_rodents_2018$`Creation Date`),
                   "Chicago")

boston_months <- 
  city_mon_summary(boston_rodents_2018,
                   boston_rodents_2018$open_dt,
                   "Boston")

dc_months <- 
  city_mon_summary(dc_rodents_2018,
                   dc_rodents_2018$ADDDATE,
                   "Washington DC")

nyc_months <- 
  city_mon_summary(nyc_rodents_2018,
                   mdy_hms(nyc_rodents_2018$`Created Date`),
                   "New York City")

la_months <-
  city_mon_summary(la_rodents_2018,
                   mdy_hms(la_rodents_2018$CreatedDate),
                   "Los Angeles")

detroit_months <- 
  city_mon_summary(detroit_rodents_2018,
                   mdy_hms(detroit_rodents_2018$`Created At`),
                   "Detroit")

city_months_18 <- 
  bind_rows(chicago_months,
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
  labs(title = "Rat Complaints by Highly Affected US Cities in 2018",
    subtitle = "Chicago generally has the most rat complaints all year round",
     caption = "\nSource: City Open Data Portals") +
  scale_color_manual(values = c("#098154", 
                                "#c72e29",
                                "#016392", 
                                "#be9c2e", 
                                "#fb832d",
                                "#000000")) +
  theme_wsj() +
  theme(plot.title = element_text(size = 15,
                                 hjust = 0.42),
     plot.subtitle = element_text(size = 13,
                                 hjust = 0.42),
      plot.caption = element_text(size = 10),
       plot.margin = margin(t = 30, 
                            r = 30, 
                            b = 30, 
                            l = 30, 
                         unit = "pt"),
        axis.title = element_text(size = 13,
                                  face = "bold")) +
  guides(color = guide_legend(title = NULL))

ggsave(here("output", "city_compare.pdf"), 
       plot = p1, 
      width = 11, 
     height = 7.5,
      units = "in")

##########################################################################
######### Plot 2: 2017 Chicago Rodent Requests Community Analysis#########
##########################################################################

more_renter_low_income <-
  chicago_rat_community_17 %>%
  filter(`Income Per Capita` <= 21708) %>%
  mutate(`Income Level` = "Communities\nunder 180% FPL\n(Federal Poverty Level)")
  
p2 <- 
  chicago_rat_community_17 %>%
  ggplot(aes(x = `Renter Proportion`,
             y = `Number of Requests per 10000 People`)) +
  geom_point(aes(size = `Income Per Capita`), 
                alpha = 0.8, 
                color = "gray") +
  geom_point(data = more_renter_low_income,
         aes(size = `Income Per Capita`, 
            color = `Income Level`)) +
  geom_smooth(data = more_renter_low_income, 
             color = "dodgerblue4",
                se = F,
      aes(linetype = "For Lower Income \nCommunties, the Larger\nRenter Proportion, \nthe Less Complaints"),
              size = 1.1) +
  labs(
    title = "Lower-income Communities with More Renters Are Less \nLikely to Complain about Rats in Chicago (2017)",
 subtitle = "Lower-income renters may care more about \nother socioeconomic issues than rat problems",
  caption = "Source: Chicago Data Portal & American Community Survey",
        x = "Proportion of People Renting in Community",
        y = "Number of Rodent Complaints"
    ) +
  scale_color_wsj("colors6", "Income Level") +
  scale_size(breaks = c(20000, 40000, 60000, 80000),
             labels = c("20000 US dollars",
                        "40000 US dollars",
                        "60000 US dollars",
                        "80000 US dollars")) +
  scale_linetype_manual(values = c("dashed", "dotted")) +
  theme_wsj() +
  theme(plot.title = element_text(size = 15,
                                 hjust = 0.4),
     plot.subtitle = element_text(size = 13,
                                 hjust = 0.4),
      plot.caption = element_text(size = 10),
       plot.margin = margin(t = 30, 
                            r = 30, 
                            b = 30, 
                            l = 30, 
                         unit = "pt"),
       legend.text = element_text(size = 9.5),
      legend.title = element_text(size = 11, 
                                  face = "bold"),
  legend.direction = "vertical",
   legend.position = "right",
        axis.title = element_text(size = 13, 
                                  face = "bold")) +
   guides(linetype = guide_legend("Community Trend"))

ggsave(here("output", "community_analysis.pdf"), 
       plot = p2, 
      width = 11, 
     height = 7.5,
      units = "in")

###############################################################
######### Plot 3: 2014 t0 2018 Response Time Analysis #########
###############################################################

p3 <-
  chicago_rodents %>%
  filter(Status == "Completed") %>%
  mutate(Resolving_Days = as.numeric(mdy(`Completion Date`)) - 
                          as.numeric(mdy(`Creation Date`)),
         `Days to Resolve\nRat Complaints` = fct_collapse(
           factor(Resolving_Days),
           "within 3 days" = as.character(0:3),
             "4 to 7 days" = as.character(4:7),
            "8 to 14 days" = as.character(8:14),
           "15 to 30 days" = as.character(15:30),
            "over 30 days" = as.character(31:500)
         ),
          year = year(mdy(`Creation Date`)),
         month = month(mdy(`Creation Date`), 
                       label = TRUE, 
                        abbr = TRUE)
         ) %>%
  group_by(year, month) %>%
  ggplot() +
  geom_bar(aes(x = month, 
            fill = `Days to Resolve\nRat Complaints`),
        position = position_stack(reverse = TRUE)) +
  facet_wrap(vars(year), nrow = 5) +
  labs(title = "Time to Respond to Rat Complaints Has Dropped over\nthe Past Few Years in Chicago (2014 to 2018)",
    subtitle = "Since 2017, most rat complaints are resolved within 7 days",
     caption = "Source: Chicago Data Portal",
           x = "Month",
           y = "Number of Resolved Complaints") +
  scale_fill_manual(values = c("#098154", 
                               "#016392", 
                               "#be9c2e", 
                               "#fb832d", 
                               "#c72e29",
                               "#000000"), 
                 "Days to Resolve\nRat Complaints") +
  theme_wsj() +
  theme(plot.title = element_text(size = 15,
                                 hjust = 0.4),
     plot.subtitle = element_text(size = 13,
                                 hjust = 0.4),
      plot.caption = element_text(size = 10),
       plot.margin = margin(t = 30, 
                            r = 30, 
                            b = 30, 
                            l = 30, 
                            unit = "pt"),
       legend.text = element_text(size = 9.5),
      legend.title = element_text(size = 11, 
                                  face = "bold"),
  legend.direction = "vertical",
   legend.position = "right",
        axis.title = element_text(size = 13, 
                                  face = "bold"),
        strip.text = element_text(face = "bold"))

ggsave(here("output", "response_analysis.pdf"), 
       plot = p3, 
      width = 11, 
     height = 7.5,
      units = "in")
