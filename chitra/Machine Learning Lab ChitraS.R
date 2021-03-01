library(VIM)
library(dplyr)
library(mice)
library(caret)
library(ggplot2)
library(Hmisc)
library(car)

orders=read.csv("./data/Orders.csv")
returns=read.csv("./data/Returns.csv")

#Problem 1
orders = orders %>% mutate(Sales=gsub("$","",Sales,fixed =T)) %>% mutate(Sales=gsub(",","",Sales,fixed =T)) %>% mutate(Sales=as.numeric(Sales))
orders = orders %>% mutate(Profit=gsub("$","",Profit,fixed =T)) %>% mutate(Profit=gsub(",","",Profit,fixed =T)) %>% mutate(Profit=as.numeric(Profit))

#Problem 2


#Problem 3
orders_returns = inner_join(orders,returns)
orders_returns %>% filter(Returned=="Yes") %>% summarise(total_profit=sum(Profit)) # $ 60769.25

customers_returns = orders_returns %>% filter(Returned=="Yes") %>% group_by(Customer.ID,Region) %>% summarise(total=n()) 
customers_returns %>%  filter(total>1) %>%  n_distinct() #543
customers_returns %>%  filter(total>5) %>%  n_distinct() #46

orders_returns %>% filter(Returned=="Yes") %>% group_by(Region) %>% summarise(total=n())  %>% arrange(., desc(total)) %>% head() # Central America
orders_returns %>% filter(Returned=="Yes") %>% group_by(Category) %>% summarise(total=n())  %>% arrange(., desc(total)) %>% head() # Office Supplies

orders_returns %>% filter(Returned=="Yes") %>% group_by(Category,Sub.Category) %>% summarise(total=n())  %>% arrange(., desc(total)) %>% head() #Office Supplies Binders

#Problem 4
orders$Process.Time <- as.Date(as.character(orders$Ship.Date), format="%m/%d/%Y")- as.Date(as.character(orders$Order.Date), format="%m/%d/%Y")
products_returns = orders_returns %>% group_by(Product.ID) %>% summarise(total=n())
orders = inner_join(orders,products_returns) %>% mutate(Times.Returned =total)

#Problem 5
set.seed(0)
index = sample(1:nrow(orders), size= nrow(orders)*0.8)
train_data = orders[index, ]
test_data = orders[-index, ]

#Problem 6
model.initial=lm(Sales~ Discount+Shipping.Cost+Process.Time+Times.Returned+Order.Priority,data=train_data)
summary(model.initial)
plot(model.initial)
influencePlot(model.initial)
vif(model.initial)
avPlots(model.initial)