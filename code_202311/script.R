library(tidyverse)
library(countrycode)
library(readxl)

#trade_data <- read_excel("./WITS-Partner-Timeseries.xlsx")
trade_data <- WITS_Partner_Timeseries %>% select(-"Partner Name", -"Trade Flow", -"Indicator")
trade_data <- pivot_longer(trade_data, 
                          cols = c("2002",        
                                   "2003",          "2004",          "2005",         "2006",         "2007",         
                                   "2008",          "2009" ,         "2010" ,         "2011",          "2012",         
                                   "2013",          "2014",          "2015",          "2016",          "2017",         
                                   "2018" ,         "2019",          "2020"         ), 
                          names_to = "Year", 
                          values_to = "Share")
trade_data <- trade_data %>% mutate(cty_name = countrycode(trade_data$`Reporter Name`, "country.name", "cown"))
trade_data <- trade_data %>% mutate(Year = as.numeric(Year))
##############
China_post12 <- dfAgree %>% filter(year >= 2002 & ccode1 == 710) %>% mutate(cty_name = countrycode(ccode2, "cown", "country.name"))


###########
combined <- left_join(China_post12, trade_data, by = c("ccode2" = "cty_name", "year" = "Year")) %>% 
  mutate(Share = Share / 100)

# combined <- combined %>% filter(ccode2 == 2 | ccode2 == 365 | ccode2 == 731 | ccode2 == 732)

  # ggplot(China_post12, aes(x = year, y = agree, group = cty_name, color = cty_name)) + 
  # geom_line() +
  # theme_minimal() +
  # geom_vline(xintercept = c(2015, 2016, 2017), linetype = "dashed", color = "black") +
  # labs(title = "Agree Score over Years by Country",
  #      x = "Year",
  #      y = "Agree",
  #      color = "Country")  

  
combined %>% filter(ccode2 == 2) %>%  ggplot(., aes(x = year)) +
    geom_line(aes(y = agree, color = "agree")) +
    geom_line(aes(y = Share, color = "Share")) +
    scale_color_manual(values = c("agree" = "blue", "Share" = "red")) +
    geom_vline(xintercept = c(2015, 2016, 2017), linetype = "dashed", color = "black") +
    labs(title = "China-US Over-Time Change of Political Scores and Export Share",
         x = "Year",
         y = "Value",
         color = "Legend") 

combined %>% filter(ccode2 == 365) %>%  ggplot(., aes(x = year)) +
  geom_line(aes(y = agree, color = "agree")) +
  geom_line(aes(y = Share, color = "Share")) +
  scale_color_manual(values = c("agree" = "blue", "Share" = "red")) +
  geom_vline(xintercept = c(2013, 2014, 2015, 2017, 2018, 2019), linetype = "dashed", color = "black") +
  labs(title = "China-Russia Over-Time Change of Political Scores and Export Share",
       x = "Year",
       y = "Value",
       color = "Legend") 


# 160, 900