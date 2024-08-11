

# Load Packages, Objects, Functions, and Data=======================================================
#packages
library(pacman) 
pacman::p_load(here, tidyverse, janitor, skimr, visdat, rstatix)

#functions
source(here("code", "00_helper_fns.R"))

#data
fp_train_in <- here("data", "tidy_data", "train_merged.rds")
df_train <- readRDS(fp_train_in)


# Feature Engineering===============================================================================
## Date features
df_train %>% 
  #create time-related variables
  mutate(day=wday(date, label=TRUE),
         month=month(date, label=TRUE),
         quarter=quarter(date),
         #create field that indicates whether the day is a workday or not
         work_day=case_when(
           !day %in% c("Sat", "Sun") & is.na(type_holiday)|type_holiday=="Event" ~ TRUE,
           type_holiday=="Work Day"                                              ~ TRUE,
           day %in% c("Sat", "Sun")                                              ~ FALSE,
           !day %in% c("Sat", "Sun") & 
             type_holiday %in% c("Holiday", "Additional", "Bridge")              ~ FALSE,
           TRUE                                                                  ~ NA)
  ) %>% 
  
## Create lagged features
  #by store and family
  group_by(store_nbr, family) %>%
  #create lags by 1 day and 1 week
  mutate(sales_lag1=lag(sales, 1),
         sales_lag7=lag(sales, 7)) %>%

## Rolling statistics
  #still grouped by store and family
  mutate(sales_mean7=
         sales_mean30=)





## Interactions
## Interactions: Consider interactions between features (e.g., promotions during holidays, 
  #sales trends by product family).



# Create Lagged Features: For time-series forecasting, creating lagged features (e.g., sales from previous days, weeks, or months) can be useful.
# Rolling Statistics: Compute rolling means, medians, or other statistics over different windows to capture trends and seasonality.
# Interactions: Consider interactions between features (e.g., promotions during holidays, sales trends by product family).




#FYI
# Apply Feature Scaling and Assess==================================================================
## Apply min-max scaling
df_transactions <- df_transactions0 %>%
  mutate(transactions=(transactions-min(transactions))/(max(transactions)-min(transactions)))

df_train <- df_train0 %>%
  mutate(across(c(sales, onpromotion), ~(.x-min(.x))/(max(.x)-min(.x))))


## Assess scaled data
### Summary stats
summary(df_transactions0$transactions)
summary(df_transactions$transactions)



### Visually
df_transactions %>%
  ggplot(aes(x=transactions)) +
  geom_histogram(color="black", fill="steelblue") +
  theme_bw()

df_transactions %>%
  ggplot(aes(sample=transactions)) +
  geom_qq() +
  geom_qq_line() +
  theme_bw()

#does not seem to help much with non-normality


## Apply quantile transformation
df_transactions <- df_transactions0 %>%
  mutate(transactions=qnorm((rank(transactions, na.last = "keep") - 0.5) / length(transactions)))


#histogram
df_transactions %>%
  ggplot(aes(x=transactions)) +
  geom_histogram(color="black", fill="steelblue") +
  theme_bw()
#normal distribution

#q-q plot
df_transactions %>%
  ggplot(aes(sample=transactions)) +
  geom_qq() +
  geom_qq_line() +
  theme_bw()
#normal distribution


## Apply log + 1 transform
df_transactions <- df_transactions0 %>%
  mutate(transactions=log(transactions + 1))

df_train <- df_train0 %>%
  mutate(across(c(sales, onpromotion), ~log(.x + 1)))

#transactions
df_transactions %>%
  ggplot(aes(x=transactions)) +
  geom_histogram(color="black", fill="steelblue") +
  theme_bw()
#symmetrical and normalish distribution

#sales
df_train %>%
  ggplot(aes(x=sales)) +
  geom_histogram(color="black", fill="steelblue") +
  theme_bw()

#onpromotion
df_train %>%
  ggplot(aes(x=onpromotion)) +
  geom_histogram(color="black", fill="steelblue") +
  theme_bw()

