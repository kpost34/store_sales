#02_data-eda.R

#This script 1) performs EDA and 2) merges dataframes

# Load Packages, Functions, and Data================================================================
#packages
library(pacman) 
pacman::p_load(here, tidyverse, janitor, skimr, rstatix)

#functions
source(here("code", "00_helper_fns.R"))

#data
source(here("code", "01_data-clean_impute.R"))



# Detect Outliers===================================================================================
## Which datasets?
df_holidays #no numerical vars
df_stores0 #no numerical vars
df_transactions0 #transactions
df_oil_imp #dcoilwtico_mavg
df_train0 #sales, onpromotion


## Transactions--------------------
df_transactions0 %>%
  mutate(transactions_z=calc_zscore(transactions),
         transactions_is_outlier=transactions_z > 3) %>%
  reframe(total=sum(transactions_is_outlier),
          pct=(total/n())*100)
#1430, 1.71%


## Oil--------------------
df_oil_imp %>%
  mutate(oil_z=calc_zscore(dcoilwtico_mavg),
         oil_is_outlier= oil_z > 3) %>%
  reframe(total=sum(oil_is_outlier),
          pct=(total/n())*100)
#0, 0%

df_oil_imp %>%
  calc_outlier_n_pct(dcoilwtico_mavg, fn=calc_zscore, thresh=3)
  


## Train: sales--------------------
### Overall
# z-score
#using simple function
df_train0 %>%
  mutate(sales_z=calc_zscore(sales),
         sales_is_outlier=sales_z > 3) %>%
  reframe(total=sum(sales_is_outlier),
          pct=(total/n())*100)
#65,073, 2.17%

#using wrapper function
df_train0 %>%
  calc_outlier_n_pct(sales, fn=calc_zscore, thresh=3)

#iqr
df_train0 %>%
  calc_iqr(sales, 3) %>% #higher threshold
  reframe(total=sum(sales_is_outlier),
          pct=(total/n())*100)
#323,814, 10.8%; much greater than zscore


## Train: sales and onpromotion--------------------
### Overall
#hard-coded
df_train0 %>%
  mutate(across(c(sales, onpromotion), calc_zscore, .names="{.col}_z"),
         across(ends_with("_z"), ~.x > 3, .names="{.col}_outlier")) %>%
  reframe(across(ends_with("_outlier"), sum, .names="{.col}_n"),
          across(ends_with("_outlier"), ~(sum(.x)/n())*100, .names="{.col}_pct"))

#using wrapper function
df_train0 %>%
  calc_outlier_n_pct(sales, onpromotion, calc_zscore, 3)


### By store_nbr
df_train0 %>%
  group_by(store_nbr) %>%
  calc_outlier_n_pct(sales, onpromotion, fn=calc_zscore, 3) %>%
  mutate(store_nbr=as.integer(store_nbr)) %>%
  arrange(store_nbr) %>%
  View() #max pct: 2.3% (onpromotion) and 4.4% (sales)


### By FAMILY
df_train0 %>%
  group_by(family) %>%
  calc_outlier_n_pct(sales, onpromotion, fn=calc_zscore, 3) %>%
  View() 
#onpromotion is NA for BOOKS...
#max pct outliers: 4.7% for onpromotion and 2.7% for sales

df_train0 %>%
  filter(family=="BOOKS") %>%
  distinct(onpromotion)
#all 0s so no variability


### By store_nbr and FAMILY
df_train0 %>%
  group_by(store_nbr, family) %>%
  calc_outlier_n_pct(sales, onpromotion, fn=calc_zscore, 3) %>%
  mutate(store_nbr=as.integer(store_nbr)) %>%
  arrange(family, store_nbr) %>%
  View() 
#onpromotion: 9.26%
#sales: 5.11%



# Visualize Distributions===========================================================================
## Transactions--------------------
df_transactions0 %>%
  ggplot(aes(x=transactions)) +
  geom_histogram(color="black", fill="steelblue") +
  theme_bw()
#skewed, longish right tail


## Oil--------------------
df_oil_imp %>%
  ggplot(aes(x=dcoilwtico_mavg)) +
  geom_histogram(color="black", fill="steelblue") +
  theme_bw()
#bimodal distribution

df_oil_imp %>%
  ggplot(aes(x=date, y=dcoilwtico_mavg)) +
  geom_line() +
  theme_bw()
#bimodal pattern seen in prices over time plot


## Train: sales--------------------
### Overall
df_train0 %>%
  ggplot(aes(x=sales)) +
  geom_histogram(color="black", fill="steelblue") +
  theme_bw()
#long right-tail
skim(df_train0)
#many 0s with a max value of 12,4717


### By store_nbr
#first half
df_train0 %>%
  mutate(store_nbr=as.integer(store_nbr)) %>%
  filter(store_nbr < 28) %>%
  ggplot(aes(x=sales)) +
  geom_histogram(color="black", fill="steelblue") +
  facet_wrap(~store_nbr) +
  theme_bw()

#second half
df_train0 %>%
  mutate(store_nbr=as.integer(store_nbr)) %>%
  filter(store_nbr > 27) %>%
  ggplot(aes(x=sales)) +
  geom_histogram(color="black", fill="steelblue") +
  facet_wrap(~store_nbr) +
  theme_bw()


### By family
vec_family <- df_train0 %>%
  pull(family) %>%
  unique() %>%
  as.character()

#first half
df_train0 %>%
  filter(family %in% vec_family[1:17]) %>%
  ggplot(aes(x=sales)) +
  geom_histogram(color="black", fill="steelblue") +
  facet_wrap(~family) +
  theme_bw()

#second half
df_train0 %>%
  filter(family %in% vec_family[18:33]) %>%
  ggplot(aes(x=sales)) +
  geom_histogram(color="black", fill="steelblue") +
  facet_wrap(~family) +
  theme_bw()


## Train: onpromotion--------------------
### Overall
df_train0 %>%
  ggplot(aes(x=onpromotion)) +
  geom_histogram(color="black", fill="steelblue") +
  theme_bw()
#long right-tail


### By store_nbr
#first half
df_train0 %>%
  mutate(store_nbr=as.integer(store_nbr)) %>%
  filter(store_nbr < 28) %>%
  ggplot(aes(x=onpromotion)) +
  geom_histogram(color="black", fill="steelblue") +
  facet_wrap(~store_nbr) +
  theme_bw()

#second half
df_train0 %>%
  mutate(store_nbr=as.integer(store_nbr)) %>%
  filter(store_nbr > 27) %>%
  ggplot(aes(x=onpromotion)) +
  geom_histogram(color="black", fill="steelblue") +
  facet_wrap(~store_nbr) +
  theme_bw()


### By family
#first half
df_train0 %>%
  filter(family %in% vec_family[1:17]) %>%
  ggplot(aes(x=onpromotion)) +
  geom_histogram(color="black", fill="steelblue") +
  facet_wrap(~family) +
  theme_bw()

#second half
df_train0 %>%
  filter(family %in% vec_family[18:33]) %>%
  ggplot(aes(x=onpromotion)) +
  geom_histogram(color="black", fill="steelblue") +
  facet_wrap(~family) +
  theme_bw()


## Train: sales and onpromotion
### Determine distinct numbers of values for promotion and sales
df_n_promo_sales <- df_train0 %>%
  mutate(store_nbr=as.integer(store_nbr)) %>%
  # sample_n(size=200000) %>%
  group_by(store_nbr, family) %>%
  reframe(n_promotion=n_distinct(onpromotion),
          n_sales=n_distinct(sales))

### Filter data then run Shapiro tests on variable data
#store_nbr
df_train0 %>%
  mutate(store_nbr=as.integer(store_nbr)) %>% 
  anti_join(
    df_n_promo_sales %>%
      filter(n_promotion==1|n_sales==1) #filter highly 'constant' values
  ) %>%
  sample_n(size=100000) %>%
  group_by(store_nbr) %>%
  shapiro_test(vars=c("onpromotion", "sales")) %>%
  arrange(desc(p)) 
#no p close to .05

#family
df_train0 %>%
  mutate(store_nbr=as.integer(store_nbr)) %>% 
  anti_join(
    df_n_promo_sales %>%
      filter(n_promotion==1|n_sales==1) #filter highly 'constant' values
  ) %>%
  sample_n(size=130000) %>%
  group_by(family) %>%
  shapiro_test(vars=c("onpromotion", "sales")) %>%
  arrange(desc(p))
#no p close to .05


#Conclusions:
#1) lowish percentage of outliers: max 2.2% of the three numerical variables
#2) histograms and q-q plots as well as shapiro tests indicate non-normal distributions
#NOTE: for report, show histograms to demonstrate skewed data



# Data Integration==================================================================================
## Prepare holiday DF for joining
#create crosswalk
df_state_city <- df_stores0 %>%
  distinct(state, city) %>%
  mutate(national="Ecuador", .before="state") %>%
  arrange(state)

#create national holidays df with each city populated
df_holidays_n <- df_holidays %>%
  filter(locale=="National") %>%
  left_join(
    df_state_city %>%
      select(!state),
    by=c("locale_name"="national")
  )
  
#create regional holidays df with each associated city populated
df_holidays_r <- df_holidays %>%
  filter(locale=="Regional") %>%
  left_join(
    df_state_city %>%
      select(!national),
    by=c("locale_name"="state")
  )

#combine local, regional, and national holidays and rename fields
df_holidays_full <- df_holidays %>%
  filter(locale=="Local") %>%
  mutate(city=locale_name) %>%
  bind_rows(
    df_holidays_n,
    df_holidays_r
  ) %>%
  rename_with(.cols=c(locale, locale_name, transferred),
              .fn=~paste(.x, "holiday", sep="_"))


## Merge data
df_train <- df_train0 %>%
  left_join(df_stores0) %>%
  left_join(
    df_holidays_full,
    by=c("date", "city")
  ) %>%
  left_join(df_transactions0) %>%
  left_join(df_oil_imp) %>%
  relocate(transactions, .after="onpromotion") %>%
  relocate(dcoilwtico_mavg, .after="cluster")




# Save Dataframe====================================================================================
fp_train_out <- here("data", "tidy_data", "train_merged.rds")

# saveRDS(df_train, fp_train_out)



