

library(tidyverse)
library(reshape2)
library(forcats)
library(scales)


# read the data based on format of this link(https://metabase.pinsvc.net/question/3317)
data <- readr::read_csv("crawlData.csv")


# make characters to facotr
data$channel <- as.factor(data$channel)
data$airline_code <- as.factor(data$airline_code)
data$charter <- as.factor(data$charter)




manualDump <- read_csv("manualDump2.csv")
manualDumpAirline <- read_csv("manualDumpAirline.csv")

manualDump$channel = as.factor(manualDump$channel)
manualDump$airline = as.factor(manualDump$airline)


# Combining data into one
#data <- rbind(data, native, snappTripApp)

data %>% group_by(channel) %>% summarise(n())
manualDump %>% group_by(channel) %>% summarise(n())

# Define the admin pricing on each channel
x1 <- left_join(x = data, y = manualDump,
               by = c("channel" = "channel",
                      "origin_iata" = "origin",
                      "destination_iata" = "destination",
                      "airline_code" = "airline"))
x2 <- filter(x1, !is.na(discountRate))
x3 <- filter(x1, is.na(discountRate))

y1 <- left_join(x = x3, y = rename(manualDumpAirline, discountRate = dumpRate),
                by = c("channel" = "channel",
                       "airline_code" = "airline"))

#y2 <- filter(y1, !is.na(discountRate))


#z <- left_join(x = select(y2, - discountRate), y = rename(manualDumpAirline, discountRate = dumpRate),
 #              by = c("channel" = "channel",
   #                   "airline_code" = "airline"))

data <- rbind(x2,select(y1, - discountRate.x) %>% rename(discountRate = discountRate.y))

# data$rule_dump_percentage[data$channel == "snappjek_native"] = 0
# data$rule_dump_percentage[data$channel == "snapptrip_app"] = 0

data <- mutate(data, 
               finalSnappPrice = 
                 case_when(
                   is.na(discountRate) ~ reference_price * ((100-rule_dump_percentage)/100),
                   discountRate == 0 ~ reference_price,
                   discountRate > rule_dump_percentage ~ reference_price * ((100 - discountRate)/100),
                   TRUE ~ reference_price * ((100-rule_dump_percentage)/100),
                 ),
               AfterPricing = case_when(
                 finalSnappPrice < competitor_price ~ "Winner",
                 finalSnappPrice == competitor_price ~ "Equal",
                 finalSnappPrice > competitor_price ~ "Looser"),
               BeforePricing = case_when(
                 reference_price < competitor_price ~ "Winner",
                 reference_price == competitor_price ~ "Equal",
                 reference_price > competitor_price ~ "Looser"
               ),
               PW = as.factor(lubridate::as_date(departure_date)-lubridate::as_date(rule_created_at)))


#data <- mutate(data, finalSnappPrice = reference_price * ((100-rule_dump_percentage)/100),
#               AfterPricing = case_when(
#                 finalSnappPrice < competitor_price ~ "Winner",
 #                finalSnappPrice == competitor_price ~ "Equal",
  #               finalSnappPrice > competitor_price ~ "Looser"),
   #            BeforePricing = case_when(
    #             reference_price < competitor_price ~ "Winner",
     #            reference_price == competitor_price ~ "Equal",
      #           reference_price > competitor_price ~ "Looser"
       #        ),
        #       PW = as.factor(lubridate::as_date(departure_date)-lubridate::as_date(rule_created_at)))





dataForPlot <- pivot_longer(data = data, 
                            cols = c("AfterPricing", "BeforePricing"), 
                            names_to = "Status", 
                            values_to = "Situation") %>%
  mutate(Status = factor(Status, levels = c("BeforePricing", "AfterPricing")),
         Situation = factor(Situation, levels = c("Looser", "Equal", "Winner")))





web <- dataForPlot %>% filter(channel == "web", rule_created_at == max(rule_created_at)) %>%
  group_by(Status, Situation) %>%
  summarise(count = n()) %>%
  mutate(Score = count / sum(count),
         channel = "web")

snappjek_native <- dataForPlot %>% filter(channel == "snappjek_native", rule_created_at == max(rule_created_at)) %>%
  group_by(Status, Situation) %>%
  summarise(count = n()) %>%
  mutate(Score = count / sum(count),
         channel = "snappjek_native")


snapptrip_app <- dataForPlot %>% filter(channel == "snapptrip_app", rule_created_at == max(rule_created_at)) %>%
  group_by(Status, Situation) %>%
  summarise(count = n()) %>%
  mutate(Score = count / sum(count),
         channel = "snapptrip_app")

snappjek <- dataForPlot %>% filter(channel == "snappjek", rule_created_at == max(rule_created_at)) %>%
  group_by(Status, Situation) %>%
  summarise(count = n()) %>%
  mutate(Score = count / sum(count),
         channel = "snappjek")





# Making the data ready for charts
#graphDataMain <- left_join(x = web, y = snappjek, by = c("Status", "Situation")) %>%
#  pivot_longer(cols = c("perc.x","perc.y"),
#               names_to = "result",
#               values_to = "Score") %>%
#  mutate(Channel = case_when(
#    result == "perc.x" ~  "Web",
#    result == "perc.y" ~ "SnappJek"
#  ))

graphDataMain <- rbind(web, snappjek_native, snapptrip_app, snappjek)


# Total Situation
ggplot(data = graphDataMain, aes(x = Status,
                                 y = Score, 
                                 fill = Situation))+
  geom_bar(position = position_dodge(),
           stat = "identity",
           color = "black")+
  facet_grid(~channel)+
  scale_y_continuous(labels=scales::percent, limits = c(0,1.15))+
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values=c("red", "blue", "green"))+
  scale_fill_brewer(palette="Set1")+
  geom_text(aes(label=percent(Score,accuracy = 1)), 
            position=position_dodge(width=1), vjust=-0.5, size = 3)+
  labs(title = "Pricing situation",
       subtitle = "Separated by channels")+
  ggsave(filename = "Total.jpeg")

  

#x1 <- c("Dec", "Apr", "Jan", "Mar", "Jan", "Dec") 
#month_level <- c("Jan", "Feb", "Mar", "Apr", "Dec")

#y1 <- factor(x1, levels = month_level)

# Separating by PW

webPW <- dataForPlot %>% filter(channel == "web", rule_created_at == max(rule_created_at)) %>%
  group_by(PW, Status, Situation) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count),
         channel = "web")

snappjekPW <- dataForPlot %>% filter(channel == "snappjek", rule_created_at == max(rule_created_at)) %>%
  group_by(PW, Status, Situation) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count),
         channel = "snappjek")

snappjekNativePW <- dataForPlot %>% filter(channel == "snappjek_native", rule_created_at == max(rule_created_at)) %>%
  group_by(PW, Status, Situation) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count),
         channel = "snappjek_native")

snapptripAppPW <- dataForPlot %>% filter(channel == "snapptrip_app", rule_created_at == max(rule_created_at)) %>%
  group_by(PW, Status, Situation) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count),
         channel = "snapptrip_app")


#graphDataPW <- left_join(x = webPW, y = snappjekPW, by = c("Status", "Situation", "PW")) %>%
#  pivot_longer(cols = c("perc.x","perc.y"),
#               names_to = "result",
#               values_to = "Score") %>%
##  mutate(Channel = case_when(
#                 result == "perc.x" ~  "Web",
#                 result == "perc.y" ~ "SnappJek"
 #              ))

graphDataPW <- rbind(webPW, snappjekPW, snappjekNativePW, snapptripAppPW)


# Total Situation PW 
ggplot(data = graphDataPW)+
  geom_bar(aes(x = Situation,
               y = Score, 
               fill = Channel),
           position = "dodge",
           stat = "identity")+
  facet_grid(PW~Status)+
  scale_y_continuous(labels=scales::percent)
Score
# Total Situation PW
ggplot(data = graphDataPW,aes(x = Status,
                              y = perc, 
                              fill = Situation,
                              ))+
  geom_bar(position = position_dodge(),
           stat = "identity",
           color = "black")+
  facet_grid(PW~channel)+
  scale_y_continuous(labels=scales::percent, limits = c(0,1.15))+
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values=c("red", "blue", "green"))+
  scale_fill_brewer(palette="Set1")+
  geom_text(aes(label=percent(perc, accuracy = 1)), 
            position=position_dodge(width=1), vjust=-0.5, size = 3)+
  labs(title = "Pricing situation",
       subtitle = "Separated by channels and Purchase window")+
  ggsave(filename = "SituationPW.jpeg")


  levels = c("Today", "Tomorrow","The day after tomorrow")

ggsave("PricingSituation.jpeg",
       plot = last_plot(),
       device = "jpeg")





dataForPlot %>% filter(channel=="snappjek") %>%
  group_by(airline_code, PW, Status, Situation) %>%
  summarise(count = n()) %>% 
  mutate(Score = count / sum(count)) %>%
  ggplot(aes( x = Status,
         y = Score, 
         fill = Situation,
  ))+
  geom_bar(position = position_dodge(),
           stat = "identity",
           color = "black")+
  facet_grid(PW~airline_code)+
  scale_y_continuous(labels=scales::percent)+
  #scale_fill_brewer(palette="Spectral")+
  #scale_fill_manual(values=c("red", "blue", "green"))+
  scale_fill_brewer(palette="Set1")+
  geom_text(aes(label=percent(Score)), 
            position=position_dodge(width=1), vjust=0, size = 5)+
  labs(title = "Pricing situation",
       subtitle = "Separated by channels and Purchase window")


write_csv(filter(dataForPlot, channel == "snappjek", Status == "AfterPricing", Situation == "Looser", PW == 0), "Looser.csv")

write_csv(filter(dataForPlot, channel == "snappjek", Status == "BeforePricing", Situation == "Looser"), "Looser.csv")
