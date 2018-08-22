library(tidyverse)
library(tm)
options(scipen = 999)

# read data
testset_Apr <- read_csv("data/testset_Apr (1).csv")
testset_Sessioninfo <- read_csv("data/testset_Sessioninfo.csv")
internal_search_test <- read_csv("data/internal_search_test.csv")
test_time_of_day <- read_csv("data/test_time_of_day.csv")

# join master data with sessioninfo table
test <- right_join(testset_Sessioninfo, testset_Apr, by = c("ssessionid" = "sessionid"))

# clean data
sum(is.na(test$temperature)) # NA in region, weather, temp
table(test$temperature)
test$region <- ifelse(is.na(test$region) == T, "none", test$region)
test$weathercondition <- ifelse(is.na(test$weathercondition) == T, "none", test$weathercondition)
test$temperature <- ifelse(is.na(test$temperature) == T, "none", test$temperature)

# convert testa type
test$country <- as.factor(test$country)
test$region <- as.factor(test$region)
test$day_of_week <- as.factor(test$day_of_week)
test$weathercondition <- as.factor(test$weathercondition)
test$devicetype <- as.factor(test$devicetype)
test$ismobile <- as.factor(test$ismobile)
test$temperature <- as.numeric(test$temperature)

# imputation
mean_temp <- mean(test$temperature, na.rm = T)
test$temperature <- ifelse(is.na(test$temperature) == T, mean_temp, test$temperature)
summary(test$temperature)

# remove unimportant devicetype
table(test$devicetype)
test <- test %>%
  filter(!grepl("console", devicetype)) %>%
  filter(!grepl("mediahub", devicetype)) %>%
  filter(!grepl("smallscreen", devicetype)) %>%
  filter(!grepl("tv", devicetype))

# rename domain
test$domain <- ifelse(test$domain == "google",
  ifelse(test$paidcampaign == "no", "google_organic", "google_paid"), test$domain
)

# convert data type
test$domain <- as.factor(test$domain)
test$paidcampaign <- as.factor(test$paidcampaign)

## Change keyword to "none" if paidcampaign = "no"
test <- test %>%
  mutate(keyword = ifelse(paidcampaign == "no", "none", keyword))

## search term grouping
kw_arrange <- test %>%
  group_by(keyword) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# create vector for brand keyword search
brand_name <- c(
  "oldnavy", "oldnavy.com", "okdnavy", "oldmavy", "oldnacy", "ildnavy",
  "gap", "thegap", "gap.com",
  "bananarepublic", "banana", "bannanarepublic", "banannarepublic", "republicbanana"
)
kw_arrange$kw_mof <- gsub(" ", "", kw_arrange$keyword)
kw <- as_vector(kw_arrange[, 3])
result <- matrix(NA, length(kw), length(brand_name))

for (i in 1:length(kw)) {
  for (j in 1:length(brand_name)) {
    if (grepl(brand_name[j], kw[i])) {
      result[i, j] <- substring(kw[i], regexpr(brand_name[j], kw[i]) + nchar(brand_name[j]))
    } else {
      result[i, j] <- 0
    }
  }
}
kw_arrange$check <- apply(result, 1, function(x) {
  sum(x != 0)
})
kw_arrange$class <- ifelse(kw_arrange$kw_mof %in% brand_name, "brand", ifelse(kw_arrange$check != 0, "brand_cat", "cat"))

# join table with keyword
test <- test %>% left_join(kw_arrange)
test <- select(test, -n, -kw_mof, -check)
test <- mutate(test, class = ifelse(keyword == "none", "none", class))

## Convert buyer to factor
test$buyer <- as.factor(ifelse(test$buyer == "1", "yes", "no"))

## Convert temperature to double
test$temperature <- as.double(test$temperature)

## NA cleaning
colSums(is.na(test))
test$class <- ifelse(is.na(test$class) == TRUE, "none", test$class)
test <- test %>%
  drop_na(ssessionid) %>%
  drop_na(country) %>%
  drop_na(day_of_week) %>%
  drop_na(devicetype) %>%
  drop_na(ismobile) %>%
  drop_na(keyword)

# remove duplicated id
test$ssessionid.y <- NULL

## internal search word cleaning
internal_search_test$search <- internal_search_test$keyword

# word freq - raw data
internal_search_test %>% group_by(search) %>% summarize(freq = n()) %>% arrange(desc(freq))

# clean text by stemming
internal_search_test$search_new <- stemDocument(internal_search_test$search, language = "english")

# find top 10 words
top_10_word <- internal_search_test %>%
  group_by(search_new) %>%
  summarize(freq = n()) %>%
  arrange(desc(freq)) %>%
  head(10) %>%
  .[, 1] %>%
  as.list()

# create new variable to find whether it's in top 10 or not
internal_search_test <- internal_search_test %>%
  mutate(top_10 = ifelse(search_new %in% top_10_word$search_new, 1, 0)) %>%
  group_by(ssessionid) %>%
  summarise(sum = sum(top_10)) %>%
  arrange(desc(sum)) %>%
  mutate(search_internal = ifelse(sum > 0, "top_10", "others")) %>%
  select(-sum)

# join with master data
test_combined <- right_join(internal_search_test, test, by = "ssessionid")

# clean data and distribution of search
test_combined$search_internal <- ifelse(is.na(test_combined$search_internal) == T, "no search", 
                                        test_combined$search_internal)
table(test_combined$search_internal)
test_combined$search_internal <- as.factor(test_combined$search_internal)

# Join time_of_day table
testset <- right_join(test_time_of_day, test_combined, by = "ssessionid")
testset$time_of_day <- as.factor(testset$time_of_day)
testset$search_internal <- as.factor(testset$search_internal)

# check data structure and save it 
str(testset)
# saveRDS(testset, file = "data/testset.rds")
