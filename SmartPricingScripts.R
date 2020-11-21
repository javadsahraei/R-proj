install.packages("tidyverse",repos = "http://cran.us.r-project.org")
library("tidyverse")
library(tidyverse)


# Data entry & business limits----------------------------------------------------------------
dataInput <- readr::read_csv("inputData.csv")          # From Revenue Management             - 
airlineLimit <- readr::read_csv("AirlineLimit.csv")    # From Supply (Static)                -
airlineW <- readr::read_csv("AirlineW.csv")            # From Metabase                       -
total_target_discount <- 3                             # From Budget                         -
min_discount <- 0.3                                    # From Business                       -
threshold <- 10                                        # From Business                       - 
#---------------------------------------------------------------------------------------------


# Make data ready for regression analysis-----------------------------------------------------

revisedData <- select(dataInput,
                      origin_iata,
                      destination_iata,
                      date_of_purchase,
                      date_of_flight,
                      time_of_purchase,
                      number_book_requests,
                      number_search,
                      avg_discount,
                      avg_originalprice) %>%
  filter(time_of_purchase > 7) %>%
  group_by(origin_iata,
           destination_iata,
           date_of_purchase,
           date_of_flight,
           time_of_purchase) %>%
  summarise(book_requests = sum(number_book_requests),
            search = sum(number_search),
            discount = mean(avg_discount),
            original = mean(avg_originalprice)) %>%
  mutate(CR = book_requests / search,
         discountRate = discount / original,
         pw = as.integer(as.Date.character(date_of_flight) - as.Date.character(date_of_purchase))) %>%
  select(origin_iata, destination_iata, CR, discountRate, pw, time_of_purchase) %>%
  mutate(pw_range = isTRUE(pw < 2))

revisedData$pw_range[revisedData$pw < 2] = 1
revisedData$pw_range[revisedData$pw >= 2] = 2

#---------------------------------------------------------------------------------------------


# Top Routes ---------------------------------------------------------------------------------

topRoutes <- dataInput %>% 
  group_by(origin_iata,
           destination_iata) %>%
  summarise(search = sum(number_search)) %>%
  arrange(desc(search))
#---------------------------------------------------------------------------------------------


# Regression modelling -----------------------------------------------------------------------

regSummary <- revisedData %>%
  group_by(origin_iata,
           destination_iata,
           pw_range) %>%
  summarise() %>%
  mutate(coeff = "",
         RS = "",
         pr = "")

a <- tibble(origin_iata = "", destination_iata = "", pw_range = "", coeff = "", RS = "", pr = "")

revisedData$pw_range = as.integer(revisedData$pw_range)
a$pw_range = as.integer(a$pw_range)
a$coeff = as.double(a$coeff)
a$RS = as.double(a$RS)
a$pr = as.double(a$pr)


for(i in 1:2) { 
  for(j in 1:50) {
    skip_to_next <- FALSE
    regData <- revisedData %>% filter(pw_range == i,
                                      origin_iata == as.character(topRoutes[j,1]),
                                      destination_iata == as.character(topRoutes[j,2]))
    regModel <- lm(CR ~ discountRate, data = regData)
    
    a[j + (i - 1) * 50,1] = as.character(topRoutes[j,1])
    a[j + (i - 1) * 50,2] = as.character(topRoutes[j,2])
    a[j + (i - 1) * 50,3] = i
    a[j + (i - 1) * 50,4] = regModel$coefficients[2]
    a[j + (i - 1) * 50,5] = summary(regModel)$r.squared
    a[j + (i - 1) * 50,6] = tryCatch(summary(regModel)$coefficients[2,4], 
                                     error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next){next}
  }
}

#---------------------------------------------------------------------------------------------


#Create final table -------- -----------------------------------------------------------------

# join sales weights and limitations
firstTemplate <- left_join(airlineW , airlineLimit, by = c("ticket_airline_code", "charter"))
# add coeffs
secondTemplate <- full_join(x = firstTemplate, y = a, by = c("from_city_iata_code" = "origin_iata",
                                                             "to_city_iata_code" = "destination_iata"))

# generate initial discounts
secondTemplate$coeff = as.double(secondTemplate$coeff)
secondTemplate$pr = as.double(secondTemplate$pr)
secondTemplate$coeff[secondTemplate$coeff < 0] = 0
finalTable <- secondTemplate %>%
  mutate(salesShare = sum / sum(sum),
         coeffShare = coeff / sum(coeff, na.rm = TRUE),
         discount = coeffShare * MaxDiscount * 100)

# Apply final rules
finalTable$discount[is.na(finalTable$discount)] = min_discount    # min discount for those no top routes
finalTable$discount[finalTable$coeff == 0] = min_discount         # min discount for those coeff < 0 
finalTable$discount[finalTable$pr > 0.1] = min_discount           # min discount for those pr > 0.1


# check total discount

total_discount <- finalTable %>%
  transmute(discount,
            salesShare,
            total = discount * salesShare)

discount_harmonizer = total_target_discount/sum(total_discount$total)

finalTableRevised <- finalTable %>%
  mutate(finalDiscount = discount * discount_harmonizer)
finalTableRevised$finalDiscount[finalTableRevised$discount == min_discount] = min_discount

total_discount2 <- finalTableRevised %>%
  transmute(finalDiscount,
            salesShare,
            total = finalDiscount * salesShare)
sum(total_discount2$total)

# Apply airlines conditions
for(i in 1:nrow(finalTableRevised)) {
  if(finalTableRevised$MaxDiscount[i] < finalTableRevised$finalDiscount[i]) {
    finalTableRevised$finalDiscount[i] = finalTableRevised$MaxDiscount[i]
  }
  if(finalTableRevised$finalDiscount[i] > threshold) {
    finalTableRevised$finalDiscount[i] = threshold
  }
}

finalTableRevised$pw_range[is.na(finalTableRevised$pw_range)] = "No diff"

output <- finalTableRevised %>%
  select(from_city_iata_code,
         to_city_iata_code,
         ticket_airline_code,
         charter,
         pw_range,
         finalDiscount)
readr::write_csv(output, "output.csv")

