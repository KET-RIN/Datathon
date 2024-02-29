#setting working directory
setwd("~/Desktop/DATATHON")

#loading packages
library(tidyverse)

#reading the data
datathon <- read.csv("~/Downloads/Brains/HKU Datathon/Global Superstore.csv")

   #Data Cleaning
#renaming the columns
datathon <- datathon %>%
  rename(Row_ID = "Row.ID",Order_ID = "Order.ID",Order_Date = "Order.Date",Ship_date="Ship.Date",Ship_mode="Ship.Mode",
        Customer_ID = "Customer.ID",Customer_Name = "Customer.Name",Product_ID = "Product.ID",Sub_Category = "Sub.Category",
        Product_Name = "Product.Name",Shipping_Cost = "Shipping.Cost",Order_Priority = "Order.Priority",Postal_Code = "Postal.Code") 

#removing the unwanted columns
new_columns <- c(#"Row_ID", 
"Order_ID", "Order_Date", "Ship_date", "Ship_mode",
"Customer_ID", #"Customer_Name", "Segment", Postal_Code,
"City", "State", "Country", 
"Market", "Region", "Product_ID", "Category", "Sub_Category", 
#"Product_Name",
"Sales", "Quantity", "Discount", "Profit", "Shipping_Cost","Order_Priority") 

#new data with the required columns
datathon_new <- datathon[,new_columns]

#changing the dates to be in one format
datathon_new <- datathon_new %>%
  mutate(Ship_date = gsub('/','-',datathon_new$Ship_date),
         Order_Date = gsub('/','-',datathon_new$Order_Date))

#changing datatypes
datathon_new <- transform(datathon_new,
                      Order_Date= dmy(datathon$Order_Date),
                      Ship_date = dmy(datathon$Ship_date),
                      Segment = as.factor(datathon$Segment),
                      Ship_mode = as.factor(datathon$Ship_mode),
                      City = as.factor(datathon$City),
                      State = as.factor(datathon$State),
                      Market = as.factor(datathon$Market),
                      Region = as.factor(datathon$Region),
                      Category = as.factor(datathon$Category),
                      Sub_Category = as.factor(datathon$Sub_Category),
                      Order_Priority = as.factor(datathon$Order_Priority)) 


datathon_new <- datathon_new %>%
  mutate(Year = as.factor(year(Ship_date)))

datathon_new <- datathon_new %>% 
  mutate(Month = month(Ship_date,label = T))

# datathon_new <- datathon_new %>% 
#   mutate(Month = month(Ship_date,label = T,abbr=F))

#average delivery time
datathon_new <- datathon_new %>%
  mutate(delivery_time =  Ship_date -Order_Date)


