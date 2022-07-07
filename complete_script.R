library(dplyr)
library(tidyverse)
library(magrittr)
library(RMySQL)

#connecting to the database
jugnu_db = dbConnect(MySQL(),
                     user='###############',
                     password='##########',
                     dbname='#########',
                     host='###################')

#query for data extraction
data_query = dbSendQuery(jugnu_db,
                         "Write the SQL Query Here")


raw_data =  fetch(data_query, n=-1)
#writexl::write_xlsx(raw_data_test, "test.xlsx")

#calculating lag
order_difference <- raw_data %>% 
  arrange(StoreCode, OrderDate) %>%
  group_by(StoreCode) %>% 
  summarise(OrderDate = OrderDate,
            DateDiff = OrderDate - lag(OrderDate)) %>%
  mutate(OrderType = ifelse(is.na(DateDiff), "First Delivery", "Repeat Delivery")) %>%
  mutate(OrderRank = rank(OrderDate, ties.method = 'first'))

#converting seconds into days
order_difference$DateDiff <- order_difference$DateDiff/86400

#correcting the datatypes and removing text from date_diff column
order_difference$DateDiff <- sub('sec', '', order_difference$DateDiff)
class(order_difference$DateDiff) = 'Numeric'
class(order_difference$DateDiff)

#exporting data to excel
writexl::write_xlsx(order_difference, "purchase_pattern_data.xlsx")


#testing out ideas
#testing_ideas <- order_difference %>%
#  group_by(as.factor(order_rank)) %>%
#  summarise(avg_days = mean(date_diff))

#testing_ideas <- order_difference %>%
#  group_by(order_rank) %>%
#  summarise_at(vars(date_diff), list(name = mean))


#head(testing_ideas)

#transform(order_difference, date_diff = as.numeric(date_diff))

#class(order_difference$date_diff)

#sum(is.na(order_difference$date_diff))
purchase_pattern_data$DateDiff[is.na(purchase_pattern_data$DateDiff)] <- 0

#sum(is.na(order_difference$date_diff))

#sum(order_difference$date_diff)

#order_difference %>% group_by(StoreCode) %>% across(list(name = mean))

#histing
hist(purchase_pattern_data$DateDiff, breaks = 90)

class(purchase_pattern_data$DateDiff)

test_hist <- hist(as.numeric(sub$DateDiff), breaks=mean(sub$DateDiff))

test_hist

summary(test_hist)

purchase_pattern_data$DateDiff <- as.double(purchase_pattern_data$DateDiff)


sub <- subset(purchase_pattern_data, StoreCode == "Some Store Code")
hist(sub$DateDiff)


box <- boxplot(DateDiff~StoreCode,data=sub, main="SomeBoxPlot",
               xlab="StoreCode", ylab="Difference in Dates")

box
boxplot.stats()

box[!box %in% boxplot.stats(box)$out]
rm.outlier(box, fill = FALSE, median = FALSE, opposite = FALSE)

library("outliers")

date_diff_only <- purchase_pattern_data[c('StoreCode','DateDiff')]

testing_stats <- do.call("cbind", lapply(date_diff_only, function(x) boxplot.stats(x)$stats))

lapply(purchase_pattern_data['DateDiff'], boxplot.stats)

testing_stats <- purchase_pattern_data %>%
  group_by(StoreCode) %>%
  summarise(boxplot= list( setNames(boxplot.stats(DateDiff)$stats,
                                    c('lower_whisker','lower_hinge','median','upper_hinge','upper_whisker') ) ) ) %>%
  unnest_wider(boxplot)

final_data <- merge(order_difference, testing_stats, by = "StoreCode")
final_data <- final_data[c('StoreCode', 'OrderNumber', 'OrderDate', 'DateDiff', 'OrderRank', 'OrderType', 'upper_whisker')]

final_data_subset <- subset(final_data, DateDiff <= upper_whisker)
