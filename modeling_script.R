library(dplyr)
library(readxl)
library(writexl)
library(RMySQL)

#connecting to the database
jugnu_db = dbConnect(MySQL(),
                     user='##########',
                     password='##########',
                     dbname='##########',
                     host='##########')

#query for data extraction
data_query = dbSendQuery(jugnu_db,
                         "Write the SQL query here")


raw_data =  fetch(data_query, n=-1)
writexl::write_xlsx(raw_data, "raw_data.xlsx")


#changing data type
raw_data$OrderDate <- as.Date(raw_data$OrderDate)

#removing duplicate order dates
#no_dup_data <- (distinct(dist_data, StoreCode, OrderDate))


#calculating datediff which is basically the lag between two consecutive orders
model_data <- raw_data  %>% 
  arrange(StoreCode, OrderDate) %>%
  group_by(StoreCode) %>% 
  summarise(OrderDate = OrderDate,
            DateDiff = OrderDate - lag(OrderDate)) %>%
  mutate(OrderType = ifelse(is.na(DateDiff), "First Delivery", "Repeat Delivery")) %>%
  mutate(OrderRank = rank(OrderDate, ties.method = 'first'))

#replacing NA's by 0 in DateDiff column
model_data$DateDiff[is.na(model_data$DateDiff)] <- 0

#converting seconds into days
#model_data$DateDiff <- model_data$DateDiff/86400

#correcting the datatypes and removing text from DateDiff column
model_data$DateDiff <- sub('days', '', model_data$DateDiff)
class(model_data$DateDiff) = 'Numeric'
#class(model_data$DateDiff)

#removing outliers in DateDiff using a boxplot.stats function
boxplot_stats_data <- model_data %>%
  group_by(StoreCode) %>%
  summarise(boxplot= list(setNames(boxplot.stats(as.numeric(DateDiff))$stats,
                                   c('LowerWhisker','LowerHinger','Median','UpperHinge','UpperWhisker') ) ) ) %>%
  unnest_wider(boxplot)


#merging stats together and building a final dataframe for modeling
model_data_with_outliers <- merge(model_data, boxplot_stats_data, by = "StoreCode")
final_model_data <- merge(model_data, boxplot_stats_data, by = "StoreCode")
final_model_data <- final_model_data[c('StoreCode',
                                       'OrderDate',
                                       'DateDiff',
                                       'OrderRank',
                                       'OrderType',
                                       'UpperWhisker')]

outliers_data <- subset(final_model_data, as.numeric(DateDiff) > as.numeric(UpperWhisker))
final_model_data <- subset(final_model_data, as.numeric(DateDiff) <= as.numeric(UpperWhisker))

#exporting data for whatever
write_xlsx(model_data_with_outliers, "complete_data_no_removal.xlsx")
write_xlsx(outliers_data, "outliers_data.xlsx")
write_xlsx(final_model_data, "churn_modeling_data.xlsx")