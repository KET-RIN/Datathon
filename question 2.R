
#QUESTION2
#The country with top sales
top_sale_country <- datathon_new %>%
  group_by(Country) %>%
  summarise(total_sales = round(sum(Sales))) %>% 
  arrange(desc(total_sales)) %>% 
  filter(total_sales >= 172021) %>%
  mutate(propotion =round(total_sales/sum(total_sales)*100,2)) %>%
  ggplot(aes(x= reorder(Country,-total_sales), y =total_sales,label =paste(propotion,"%",sep =""), fill = Country))+
  geom_col(width = .6)+
  theme_bw()+
  geom_text(vjust = -.1)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(title = "Countries With Top Sales",
       x = "Country",
       y = "Total Sales")+
  theme(plot.title = element_text(face = "bold",hjust = .5),
        axis.text = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

profits <- datathon_new %>%
  group_by(Year,Month) %>%
  summarise(monthly_profit = sum(Profit))
 ggplot(data = profits,aes(x = Month,y = monthly_profit,group = Year,color=Year))+
  geom_line()+
   theme_bw()+
   labs(title = "Yearly Profits By Month",
        x ="Month",
        y = "Monthly profit")+
   theme(plot.title = element_text(hjust = .5))

#2015 monthly profits
#2015 total profits are only availabe in the month of January making the value too small to fit in the general scale
 profits2015 <- profits  %>%
  filter(Year == 2015) %>% view()

#profit by Category
category_profit <-datathon_new %>%
  group_by(Category) %>%
  summarise(categorical_profit = round(sum(Profit),2)) 
ggplot(data = category_profit,aes(x = Category, y = categorical_profit ,fill = Category,label =categorical_profit))+
  geom_col(width = .4)+
  theme_bw()+
  geom_text(vjust = -.1)+
  labs(title = "Profits By Category",
       x ="Category",
       y = "Profit")+
  theme(plot.title = element_text(hjust = .5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

#profit by Sub_Category
sub_category_profit <-datathon_new %>%
  group_by(Sub_Category) %>%
  summarise(sub_categorical_profit = round(sum(Profit))) 
ggplot(data = sub_category_profit,aes(x = reorder(Sub_Category,sub_categorical_profit), y = sub_categorical_profit ,fill = Sub_Category,label =sub_categorical_profit))+
  geom_point()+
  theme_bw()+
  geom_hline(yintercept = 0,color = "black")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  geom_text(nudge_x = 0,
            nudge_y = 7000)+
  labs(title = "Profits By Sub Category",
       x ="Sub Category",
       y = "Profit")+
  theme(plot.title = element_text(hjust = .5),
        axis.ticks.y = element_blank(),
        legend.position = "none")

#sales by month
month_sales <- datathon_new %>%
  group_by(Month,Year) %>%
  summarise(monthly_sales = round(sum(Sales))) 
ggplot(data = month_sales,aes(x = reorder(Month,monthly_sales), y = monthly_sales,color = Year,group =Year))+
geom_point()+
  geom_line(linewidth = .3)+
  theme_bw()+
 scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(title = " Total Sales By Year Per Month",
       x ="Month",
       y = "Sales")+
  theme(plot.title = element_text(hjust = .5),
        axis.ticks.y = element_blank())

#sales by Category
category_sales <- datathon_new %>%
  group_by(Category) %>%
  summarise(categorical_sales = round(sum(Sales)/1000000,2)) 
ggplot(data = category_sales,aes(x =Category, y =categorical_sales,label = categorical_sales,fill =Category))+
  geom_col(width = .3)+
  theme_bw()+
  geom_text(vjust =-.1)+
  labs(title = "Total Sales By Category",
       x ="Category",
       y = "Sales in Millions")+
  theme(plot.title = element_text(hjust = .5),
        axis.text.y = element_blank(),
        axis.ticks.y =element_blank(),
        legend.position = "none",
        panel.grid = element_blank())

 #Sales by sub category
sub_category_sales <- datathon_new %>%
  group_by(Sub_Category) %>%
  summarise(sub_categorical_sales = round(sum(Sales)/1000000,2)) 
ggplot(data = sub_category_sales,aes(x =reorder(Sub_Category,-sub_categorical_sales), y =sub_categorical_sales,label = sub_categorical_sales,fill =Sub_Category))+
  geom_col(width = .3)+
  theme_bw()+
  geom_text(vjust =-.1)+
  labs(title = "Total Sales By Sub  Category",
       x ="Sub Category",
       y = "Sales in Millions")+
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),
        axis.ticks.y =element_blank(),
        legend.position = "none",
        panel.grid = element_blank())

#Average delivery time accross the countries
time <- datathon_new %>%
  group_by(Country) %>%
  summarise(average_delivery_time = round(mean(delivery_time))) %>%
  arrange(desc(average_delivery_time)) %>%
  view()
  
#Top 5 profit-making product  types on yearly basis
 profitability1 <- datathon_new %>%
   group_by(Year,Sub_Category) %>%
   summarise(most_profitable = round(sum(Profit))) %>%
   arrange(Year,desc(most_profitable)) %>% 
   slice(1:5) %>%
   filter(Year != 2015)
p1 <-  ggplot(data =profitability1,aes(x=Year,y=most_profitable,fill=Sub_Category,label =most_profitable))+geom_col(position = position_dodge())+
   geom_text(position = position_dodge(.9),angle =90,hjust = .9,size=5)+
   theme_bw()+
   scale_fill_brewer(palette = "Set1")+
   labs(title = "Top Five Most Profitable Products Per Year",
        x ="Year",
        y = "Profit")+
   theme(plot.title = element_text(hjust = .5),
         axis.text.y = element_blank(),
         axis.ticks.y =element_blank(),
         legend.text = element_text(face = "bold"),
         legend.position = 'bottom')
 p1
#2015
profitability2 <- datathon_new %>%
  group_by(Year,Sub_Category) %>%
  summarise(most_profitable = round(sum(Profit))) %>%
  arrange(Year,desc(most_profitable)) %>% 
  slice(1:5) %>%
  filter(Year == 2015)
p2 <-  ggplot(data =profitability2,aes(x=Year,y=most_profitable,fill=Sub_Category,label =most_profitable))+geom_col(position = position_dodge())+
  geom_text(position = position_dodge(.9),angle =90,hjust = 1,size=5)+
  theme_bw()+
  scale_fill_brewer(palette = "Set1")+
  labs(title = "Top Five Most Profitable Products in 2015 January",
       x ="Year",
       y = "Profit")+
  theme(plot.title = element_text(hjust = .5),
        axis.text.y = element_blank(),
        axis.ticks.y =element_blank(),
        legend.position = "none")
p2

library(patchwork)
product_profit <- p1 +p2
product_profit

