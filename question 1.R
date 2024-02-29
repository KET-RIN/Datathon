
#Question 1
#most frequent customers
frequent_customers <- datathon_new %>%
  group_by(Customer_ID) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  filter(Frequency>=77)

ggplot(data =frequent_customers,aes(x = Frequency, y = reorder(Customer_ID,Frequency),label = Frequency))+
  geom_point()+
  theme_bw()+
  geom_text(nudge_x = .3,
            nudge_y = .3)+
  labs(title = "Most Frequent Customers",
       y = 'Customer ID')+
  theme(plot.title = element_text(face = 'bold',hjust = .5,color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(face = 'bold',color = 'black'),
        axis.text.y =  element_text(face = 'bold',color = 'black'))

#least frequent customers
least_frequent_customers <- datathon_new %>%
  group_by(Customer_ID) %>%
  summarise(Frequency = n()) %>%
  arrange(Frequency) 


#Most popular market Regions
market_Region <-datathon_new %>%
  group_by(Region) %>%
  summarise(Frequency = n())

ggplot(data = market_Region, aes(x = reorder(Region,-Frequency), y =Frequency, fill = Region, label = Frequency))+
  geom_col(width = .5)+
  theme_classic()+
  geom_text(vjust = -.3)+
  scale_x_discrete(guide = guide_axis(n.dodge =2 ))+
  labs(title = 'Total Customers In Each Region',
       x = 'Region')+
  theme(plot.title = element_text(face = 'bold',hjust = .5,color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(face = 'bold',color = 'black'),
        axis.text.x =  element_text(face = 'bold',color = 'black'),
        legend.position = 'none')



#Defining high and low buyers
high_buyers <- datathon_new %>%
  group_by(Customer_ID) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  filter(Frequency>=60)

#high buyers' IDs
high_buyers_IDs <- high_buyers$Customer_ID

#Adding a new column of high and low buyers
datathon_new <- datathon_new %>%
  mutate(buyer_category = ifelse(Customer_ID %in% high_buyers_IDs,'High Buyer','Low Buyer'))

#Profit between the groups
datathon_new %>%
  group_by(buyer_category) %>%
  summarise(profit = sum(Profit)) %>%
  ggplot(aes(x = buyer_category,y = profit,label = round(profit),fill = buyer_category ))+
  geom_col(width = .4)+
  theme_classic()+
  geom_text(vjust = 2,size=4,)+
  labs(title = 'Profit Margin Between The Two\n Buyer Groups')+
  scale_fill_brewer(palette = 'Set1')+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none',
        axis.title = element_text(face = 'bold',colour = 'black'),
        axis.text.x = element_text(face = 'bold',color = 'black'),
        plot.title = element_text(face = 'bold',colour = 'black',hjust = .5))

#Regions Among the high Buyers
datathon_new %>%
  group_by(Region,buyer_category) %>%
  summarise(Frequencies = n()) %>%
  ggplot(aes(x=reorder(Region,-Frequencies),y=Frequencies,fill=buyer_category,label= Frequencies)) +
  geom_col() +
  geom_text(position = position_stack(vjust=.6))+
  labs(title = 'Buyer Category By Region',x='Region')+
  scale_fill_brewer(palette = 'Set1') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #legend.position = 'none',
        axis.title = element_text(face = 'bold',colour = 'black'),
        axis.text.x = element_text(face = 'bold',color = 'black'),
        plot.title = element_text(face = 'bold',colour = 'black',hjust = .5))

#category of goods bought by the different buyers
percent <- datathon_new %>%
  group_by(Category,buyer_category) %>%
  summarise(Totals = n())

percent$percentage <- ''

frntr <- sum(percent$Totals[percent$Category=='Furniture'])
office <- sum(percent$Totals[percent$Category=='Office Supplies'])
Technology <- sum(percent$Totals[percent$Category=='Technology'])

for (row in 1:nrow(percent)) {
  percent$percentage[row] <- ifelse(percent$Category[row] == 'Furniture',round(percent$Totals[row]/frntr*100),
                                    ifelse(percent$Category[row] == 'Office Supplies',round(percent$Totals[row]/office *100),round(percent$Totals[row]/Technology *100)))
}

percentage <- percent %>%
  ggplot(aes(x = Category, y = percentage,label = paste(percentage,'%'),fill = buyer_category))+
  geom_col(width = .3)+
  geom_text(position = position_stack(vjust = .5))+
  scale_fill_brewer(palette = 'Set1')+
  theme_classic()+
  labs(title =  'Percentage Of Products Bought By\n Each Buyer Group')+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold'),
        axis.title = element_text(face = 'bold',colour = 'black'),
        axis.text.x = element_text(face = 'bold',color = 'black'),
        plot.title = element_text(face = 'bold',colour = 'black',hjust = .5))

