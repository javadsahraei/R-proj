

library(tidyverse)
library(tidyr)
library(stringr)


data <- read_csv("fraudData.csv",
                 col_types = 
                   cols(
                     discount_code = col_character()
                   )) %>%
  mutate(hasVoucher = case_when(
             is.na(discount_code) ~ 0,
             TRUE ~ 1),
         prefix = substr(discount_code, 1,3))

data <- rename(data, c(
  "date" = "paid_date",
  "invoice" = "count",
  "ticket" = "count_2",
  "GMV" = "sum",
  "totalPrice" = "sum_2"
))
data <- mutate(data, dumpRate = 1- (totalPrice/GMV),
                     date = lubridate::as_date(date))

data$mobile = as.factor(data$mobile)



days <- data.frame(date = sort(unique(data$date), decreasing = TRUE),
                   num = seq(1, length(unique(data$date)))) %>%
  mutate(weekRound = floor(num/7),
         weekNonRound = num %% 7,
         weekNum = case_when(
           weekNonRound == 0 ~ weekRound,
           TRUE ~ weekRound + 1
         ),
         week = paste("W -",weekNum))

data <- left_join(x = data, y = days, by = "date")

weekLevels <- data.frame(a = c(1:max(unique(days$weekNum)))) %>%
  transmute(weekLevels = paste("W -",a))




data$week = factor(data$week, levels = unique(days$week))


    
# Shows how many times each user has ordered
users <- data %>% group_by(mobile) %>% summarise(count = n(),
                                                 ticket = sum(ticket),
                                                 order = sum(invoice),
                                                 days = n_distinct(date),
                                                 weeks = n_distinct(week),
                                                 channels = n_distinct(channel),
                                                 vouchers = sum(as.numeric(hasVoucher)))

users$vouchers = as.factor(users$vouchers)
data$channel = as.factor(data$channel)


users <- users %>%  mutate(basketSize = ticket / order,
                           orderPerDay = count / days,
                           orderPerWeek = count / weeks)

ggplot(users %>% filter(count > 1),
       aes(x = orderPerDay, y = orderPerWeek, color = vouchers))+
  geom_point()+
  geom_jitter()

# shows how many times users has ordered in one day
usersOrdersInOneDay <- data %>% 
  group_by(date, mobile) %>% 
  summarise(count = n(),
            ticket = sum(ticket))

# Shows how many days each user has been active
usersActiveDays <- data %>%
  group_by(mobile) %>%
  summarise(activeDays = n_distinct(date),
            invoice = sum(invoice),
            ticket = sum(ticket),
            dumpRate = 1- sum(totalPrice)/sum(GMV))

# shows how many times users has ordered in one week
usersOrdersInOneWeek <- data %>% 
  group_by(week, mobile) %>% 
  summarise(count = n(),
            ticket = sum(ticket))

# Shows how many weeks each user has been active
usersActiveWeeks <- data %>%
  group_by(mobile) %>%
  summarise(activeWeeks = n_distinct(week),
            invoice = sum(invoice),
            ticket = sum(ticket),
            dumpRate = 1- sum(totalPrice)/sum(GMV))

# Weeks users

weeksData <- data %>% group_by(week) %>% summarise(activeUsers = n_distinct(mobile),
                                                   orders = sum(invoice)) %>%
  mutate(AUO = orders / activeUsers)



ggplot(weeksData, aes(x = week, y = AUO))+
  geom_point()

a <- users %>% filter(count > 2) %>% select(mobile)
potentialFrausUsers <- users$mobile[users$count > 2]


ggplot(data = 
         data %>% 
         filter(date > "2020-06-01",
                mobile %in% potentialFrausUsers) %>%
         group_by(week, mobile) %>% 
         summarise(Invoice = sum(invoice)),
       aes(x = week, y = orderPerWeek))+
  geom_point()+
  geom_jitter()

write_csv(users, "users.csv")

a <- data %>% 
  filter(date > "2020-06-01") %>%
  group_by(week, mobile, channel) %>% 
  summarise(count = n(),
            ticket = sum(ticket),
            order = sum(invoice),
            days = n_distinct(date),
            weeks = n_distinct(week),
            channels = n_distinct(channel),
            vouchers = sum(as.numeric(hasVoucher))) %>%
  filter(order > 2)

ggplot(data = a, aes(x = week, y = order))+
  geom_point(aes(color = channel))+
  geom_jitter()





b <- data %>% 
  filter(date > "2020-08-01") %>%
  group_by(week, mobile) %>% 
  summarise(count = n(),
            ticket = sum(ticket),
            order = sum(invoice),
            days = n_distinct(date),
            weeks = n_distinct(week),
            channels = n_distinct(channel),
            vouchers = sum(as.numeric(hasVoucher)))

b<-   mutate(b, voucher = as.factor(case_when(
  vouchers == 0 ~ "No voucher",
  vouchers == 1 ~ "One voucher",
  vouchers == 2 ~ "Two vouchers",
  vouchers == 3 ~ "Three vouchers",
    TRUE ~ "more than Three voucher"
  )))

ggplot(data = b,aes(x = week, y = order))+
  geom_point(aes(color = voucher))+
  geom_jitter()+
  labs(title = "Distribution of the orders of the users",
       subtitle = "Each point represents the number of orders that a unique users has placed.",
       ylab = "Number of placed orders in a week")





c <- data %>% 
  filter(date > "2020-08-01") %>%
  group_by(week, mobile, channel) %>% 
  summarise(count = n(),
            ticket = sum(ticket),
            order = sum(invoice),
            days = n_distinct(date),
            weeks = n_distinct(week),
            vouchers = sum(as.numeric(hasVoucher)))

c <-   mutate(c, voucher = as.factor(case_when(
  vouchers == 0 ~ "No voucher",
  vouchers == 1 ~ "One voucher",
  vouchers == 2 ~ "Two vouchers",
  vouchers == 3 ~ "Three vouchers",
  TRUE ~ "more than Three voucher"
)))



ggplot(data = c,aes(x = week, y = order))+
  geom_point(aes(color = voucher))+
  geom_jitter()+
  facet_grid(~channel)+
  theme(axis.text.x = element_text(angle = 90))






d <- data %>% 
  filter(date > "2020-08-01") %>%
  group_by(week, mobile, channel) %>% 
  summarise(count = n(),
            ticket = sum(ticket),
            order = sum(invoice),
            days = n_distinct(date),
            weeks = n_distinct(week),
            vouchers = sum(as.numeric(hasVoucher)))

d <-   mutate(d, 
              NumberOfOrder = 
                factor(case_when(
                  vouchers == 1 ~ "Once",
                  vouchers == 2 ~ "Two vouchers",
                  vouchers == 3 ~ "Three vouchers",
                  vouchers == 4 ~ "Four vouchers",
                  TRUE ~ "more than Four voucher"
                  ),
                  levels = c(
                    "Once",
                   "Two vouchers",
                   "Three vouchers",
                   "Four vouchers",
                   "more than Four voucher")))



ggplot(data = d %>% filter(order>3))+
  geom_bar(aes(x = week, y = ..count..,fill = as.factor(order)),
           color = "black", 
           position = "fill")+
  labs(title = "Share of customers by the number of orders in a week",
       x = "Weeks",
       y = "Share in total")+
  facet_grid(~channel)+
  theme(axis.text.x = element_text(angle = 90))


ggplot(data = d %>% filter(order<6, order > 1))+
  geom_bar(aes(x = voucher),
           color = "black")+
  labs(title = "Share of customers by the number of orders in a week",
       x = "Weeks",
       y = "Share in total")+
  facet_grid(channel~order)+
  theme(axis.text.x = element_text(angle = 90))




e <- data %>% 
  filter(date > "2020-08-01")%>%
  group_by(week, mobile) %>%
  summarise(count = sum(invoice),
            GMV = sum(GMV),
            totalPrice = sum(totalPrice))

e <- mutate(e, discount = (GMV - totalPrice)/GMV)

ggplot(e %>% filter(discount < 0.20, discount > 0.01),aes(x = count, y = discount))+
  geom_point()+
  geom_jitter()+
  facet_wrap(~week)

ggplot(e, aes(x = discount))+
  geom_density()+
  facet_wrap(~week)
  
