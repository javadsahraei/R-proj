library(tidyverse)
library(lubridate)
library(readxl)
library(scales)
library(ggrepel)


inputData <- readxl::read_excel("input10Oct.xlsx",
                                col_types = c(
                                  "guess",
                                  "guess",
                                  "guess",
                                  "guess",
                                  "guess",
                                  "guess",
                                  "guess",
                                  "guess",
                                  "guess",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric",
                                  "numeric"
                                  ))




data <- rename(inputData, c(
                    "Airline" = "airline_name_en",
                    "SystemName" = "system_name",
                    "anNumber" = "an_number",
                    "CancellationDate" = "cancellation_date",
                    "DepDate" = "departure_date",
                    "AgentName" = "agent_name",
                    "SystemDes" = "system_description",
                    "AgencyPenalty" = "sum",
                    "ActualRefund" = "sum_2",
                    "OriginalPrice" = "sum_3",
                    "TotalPrice" = "sum_4",
                    "RuleEnginePenalty" = "sum - sum",
                    "Ticket" = "count"))



data2 <- mutate(filter(data, !is.na(AgencyPenalty), AgencyPenalty > 10000),
                CancellationDate = lubridate::as_datetime(CancellationDate) + lubridate::hours(4) + minutes(30),
                DepDate = lubridate::as_datetime(DepDate) + lubridate::hours(4) + minutes(30),
                Airline = as.factor(Airline),
                SystemName = as.factor(SystemName),
                AgentName = as.factor(AgentName),
                Route = as.factor(paste(from_city_iata_code, to_city_iata_code, sep = " to ")),
                SystemDes = as.factor(SystemDes),
                Diff = lubridate::as.difftime(DepDate - CancellationDate),
                CanHour = lubridate::hour(CancellationDate),
                DepHour = lubridate::hour(DepDate),
                Category = case_when(
                  Diff <= 3 ~ "Under 3 Hours to Dep",
                  Diff < 12 - 3 + DepHour ~ "Until 12 One day before flight",
                  Diff < 48 + 12 - 3 + DepHour ~ "3 days to flight",
                  TRUE ~ "More than 3 days"),
                RuleEnginePenaltyRate = RuleEnginePenalty / OriginalPrice,
                AgencyPenaltyRate = AgencyPenalty / OriginalPrice,
                PenaltyDiff = abs(RuleEnginePenalty - AgencyPenalty),
                BusinessStatus = case_when(
                  (RuleEnginePenalty - AgencyPenalty) > 0 ~ "Benefit",
                  (RuleEnginePenalty - AgencyPenalty) < 0 ~ "Loss",
                  TRUE ~ "Correct"),
                RuleEngineAccuracy = case_when(
                  BusinessStatus == "Correct" ~ 1,
                  PenaltyDiff/AgencyPenalty > 1 ~ 0,
                  TRUE ~ 1- PenaltyDiff/AgencyPenalty),
                TicketPrice = OriginalPrice / Ticket,
                PriceRange = case_when(
                  TicketPrice < 2000000 ~ "< 200",
                  TicketPrice < 3000000 ~ "200 <= Price < 300",
                  TicketPrice < 4000000 ~ "300 <= Price < 400",
                  TicketPrice < 5000000 ~ "400 <= Price < 500",
                  TRUE ~ "> 500"
                ),
                AccuracyRange = case_when(
                  RuleEngineAccuracy < 0.1 ~ "Less than 10%",
                  RuleEngineAccuracy < 0.2 ~ "Less than 20%",
                  RuleEngineAccuracy < 0.3 ~ "Less than 30%",
                  RuleEngineAccuracy < 0.4 ~ "Less than 40%",
                  RuleEngineAccuracy < 0.5 ~ "Less than 50%",
                  RuleEngineAccuracy < 0.6 ~ "Less than 60%",
                  RuleEngineAccuracy < 0.7 ~ "Less than 70%",
                  RuleEngineAccuracy < 0.8 ~ "Less than 80%",
                  RuleEngineAccuracy < 0.9 ~ "Less than 90%",
                  TRUE ~ "More than 90%"
                ))

data2$Category = factor(data2$Category, levels = c("Under 3 Hours to Dep",
                                                   "Until 12 One day before flight",
                                                   "3 days to flight",
                                                   "More than 3 days"))

data2$BusinessStatus = factor(data2$BusinessStatus, levels = c("Loss",
                                                               "Benefit",
                                                               "Correct"))

data2$PriceRange = factor(data2$PriceRange, levels = c("< 200",
                                                       "200 <= Price < 300",
                                                       "300 <= Price < 400",
                                                       "400 <= Price < 500",
                                                       "> 500"))

data2$AccuracyRange = factor(data2$AccuracyRange, levels = c("Less than 10%",
                                                          "Less than 20%",
                                                          "Less than 30%",
                                                          "Less than 40%",
                                                          "Less than 50%",
                                                          "Less than 60%",
                                                          "Less than 70%",
                                                          "Less than 80%",
                                                          "Less than 90%",
                                                          "More than 90%"))


ggplot(data = data2)+
  geom_bar(aes(x = Category, fill = Airline))

ggplot(data = filter(data2, Diff <150))+
  geom_density(aes(x = Diff))


ggplot(data = data2)+
  geom_bar(aes(x = Airline, y = PenaltyDiff, fill = BusinessStatus), stat = "identity",
           position = "dodge")


# Total Business Status ###############################################
ggplot(data = data2 %>% filter(BusinessStatus != "Correct") %>% group_by(BusinessStatus) %>%
       summarise(Diff = sum(PenaltyDiff)),
       aes(x = BusinessStatus, y = Diff, fill = BusinessStatus))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = comma(Diff)), vjust=-0.5) + 
  scale_y_continuous(labels = comma, limits = c(0 , 1.2 * data2 %>% filter(BusinessStatus != "Correct") %>% group_by(BusinessStatus) %>%
                                                          summarise(Diff = sum(PenaltyDiff))%>% select(Diff) %>% max()))+
  scale_fill_brewer(palette="Set1")+
  labs(title = "Total Benefit & Loss",
       subtitle = paste("From ", 
                        min(lubridate::as_date(data2$CancellationDate)),
                        " To ",
                        max(lubridate::as_date(data2$CancellationDate)),
                        "total balace is",
                        comma(sum(data2$PenaltyDiff[data2$BusinessStatus == "Benefit"]) - 
                          sum(data2$PenaltyDiff[data2$BusinessStatus == "Loss"])),
                        "IRR"),
       x = "Status",
       y = "Amount (IRR)")

  
# Accuracy of predictions #############################################################

ggplot(data = filter(data2, !is.na(AgencyPenalty)),aes(x = BusinessStatus))+
  geom_bar(aes(x = BusinessStatus, fill = BusinessStatus), color = "black")+
  geom_text(aes(label = stat(count)), stat = "count", vjust = - 0.5)+
  scale_y_continuous(limits = c(0, 30 + data2 %>% filter(!is.na(AgencyPenalty)) %>% group_by(BusinessStatus) %>%
                       summarise(a = n()) %>% select(a) %>% max()))+
  labs(title = "Count of predictions by status",
       subtitle = paste(percent(length(data2$BusinessStatus[data2$BusinessStatus == "Correct"])/length(data2$BusinessStatus)),
                        "of the predictions were 100% correct."),
       y = "Number of occurance")+
  scale_fill_brewer(palette="Set1")


# Benefit & Loss by airlines#########################################################

ggplot(data = filter(data2, !is.na(AgencyPenalty)), aes(x = BusinessStatus))+
  geom_bar(aes(fill = BusinessStatus))+
  facet_wrap(~Airline)+
  scale_fill_brewer(palette="Set1")+
  geom_text(aes(label = stat(count)), stat = "count", vjust = -0.5)+
  scale_y_continuous(limits = c(0,95))+
  labs(title = "Count of situations by Airlines",
       x = "Situation",
       y = "Number of occurance")+
  theme(axis.text.x = element_text(angle = 90))


# Amount of loss & Benefit by Airlines  #####################################################

ggplot(data = filter(data2, !is.na(AgencyPenalty), BusinessStatus != "Correct") %>% 
         group_by(Airline, BusinessStatus) %>%
         summarise(PenaltyDiff = sum(PenaltyDiff)))+
  geom_bar(aes(x = Airline,y = PenaltyDiff, fill = BusinessStatus), color = "black",
           stat = "identity", position = "dodge")+
  scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Amount of Benefit/Loss by Airlines",
       y = "Ammount (IRR)")


# Accuracy by Accuracy Range ######################################

ggplot(data = data2,aes(x = AccuracyRange))+
  geom_bar(aes(fill = Airline))+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  scale_y_continuous(limits = c(0,100))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Count of cases by Accuracy Range and Airlines",
       y = "Number of occurance")
  


# Accuracy by Airline and Price Range


ggplot(data = data2)+
  geom_point(aes(x = AgencyPenaltyRate, y = RuleEnginePenaltyRate))+
  facet_wrap(~Ailine)

ggplot(data = data2 %>% filter(Airline == "Caspian", RuleEngineAccuracy < 0.9),aes(x = AgencyPenaltyRate, y = RuleEnginePenaltyRate))+
    geom_point(aes(color = AgentName), size = 2)+
  facet_grid(Category~PriceRange)+
  labs(title = "Scattering the RuleEngine & Agency Penalty",
       subtitle = "Separated by Price Range & distance to flight for Mahan")+
  xlim(0,1)+
  ylim(0,1)+
  geom_abline(slope = 1)+
  geom_text_repel(aes(label = Route), size = 2)



ggplot(data = data2 %>% filter(Airline == "Iran Air", RuleEngineAccuracy < 2),aes(x = AgencyPenaltyRate, y = RuleEnginePenaltyRate))+
  geom_point()+
  facet_grid(Category~PriceRange)+
  labs(title = "Scattering the RuleEngine & Agency Penalty",
       subtitle = "Separated by Price Range & distance to flight for IranAir")+
  xlim(0,1)+
  ylim(0,1)+
  geom_abline(slope = 1)+
  geom_text_repel(aes(label = Route), size = 2)

              
coord_fixed(ratio = 1)+           
              
ggplot(data = data2 %>% filter(Airline != "Varesh"))+
  geom_density(aes(x = RuleEngineAccuracy))+
  facet_wrap(~Airline)+
  
  

  



  

# 
ggplot(data = filter(data2, Diff < 50, Airline == "Iran Air"))+
  geom_point(aes(x = Diff, y = RuleEngineAccuracy, color = Airline))+
  facet_grid(PriceRange~Category)



data2 %>% filter(BusinessStatus != "Correct", Airline == "Iran Air") %>% group_by(BusinessStatus)
summarise(Diff = sum(PenaltyDiff))
               