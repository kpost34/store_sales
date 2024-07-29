#01_data-clean_eda.R

#This script 1) performs initial data cleaning 2) explores data


# Load Packages, Objects, Functions, and Data=======================================================
#packages
library(pacman) 
pacman::p_load(here, tidyverse, janitor, skimr, visdat, rstatix)

#objects
# source(here("code", "00_objects.R"))

#functions
source(here("code", "00_helper_fns.R"))

#data
df_train0 <- read_csv(here("data", "raw_data", "train.csv"),
                      col_types="cDcfdi")

df_holidays0 <- read_csv(here("data", "raw_data", "holidays_events.csv"),
                         col_types="Dfffcl")

df_oil0 <- read_csv(here("data", "raw_data", "oil.csv"),
                    col_types="Dd")

df_stores0 <- read_csv(here("data", "raw_data", "stores.csv"),
                       col_types="cffff")

df_transactions0 <- read_csv(here("data", "raw_data", "transactions.csv"),
                             col_types="Dci")

df_test0 <- read_csv(here("data", "raw_data", "test.csv"),
                     col_types="cDcfd")



# Initial Data Checking=============================================================================
## Dataframe Info
df_train0
#id = identifier
#store_nbr = store where products sold
#family = type of product sold
#sales = total sales for a product family at a particular store at a given date
#onpromotion = total number of items in a product family that were being promoted at a store at a
  #given date

df_holidays0 
#date (self-explanatory)
#type = type of holiday or event
#locale = area of celebration (local, regional, or national)
#locale_name (self-explanatory)
#description (self-explanatory)
#transferred = a holiday that falls on date but transferred to another day by the gov't and thus
  #becomes a normal day -- to find the transferred date, search for "Transferred" under type


df_oil0
#date (self-explantory)
#dcoilwtico = daily oil price

df_stores0 
#store_nbr = store number
#city, state, and type metadata
#cluster = grouping of similar stores


df_transactions0
#transactions = number of transactions


## Data checks/missingness
### Glimpse/skim
glimpse(df_train0)
glimpse(df_holidays0)
glimpse(df_oil0)
glimpse(df_stores0)
glimpse(df_transactions0)

skim(df_train0) #no missing data
skim(df_holidays0) #no missing data
skim(df_oil0) #43 missing dcoilwtico
skim(df_stores0) #no missing data
skim(df_transactions0) #no missing data


### Visualizations
vis_dat(df_holidays0)
vis_dat(df_oil0)
vis_dat(df_stores0)
vis_dat(df_transactions0)



# Missingness=======================================================================================
## Explore missingness visually
#overall pattern of missingness
df_oil0 %>%
  mutate(missing=is.na(dcoilwtico)) %>%
  ggplot() +
  geom_line(aes(x=date, y=dcoilwtico), color="blue") +
  geom_point(data=. %>% filter(missing),
             aes(x=date, y=0), color="red", size=0.5) +
  theme_bw()
#red dots on y-axis indicate missing data, so these occur throughout data range and thus do not
  #appear to relate to y values or the date


## Assess if data are ever missing consecutively
#pull missingness of oil DF
vec_oil_miss <- df_oil0 %>%
  mutate(missing=is.na(dcoilwtico)) %>%
  pull(missing)

#grab run information from missingness data
df_oil_miss <- rle(vec_oil_miss) %>%
  map(enframe) %>%
  reduce(inner_join, "name") %>%
  select(-name) %>%
  select(value="value.y", n_run="value.x") 

df_oil_miss %>%
  filter(value) %>%
  arrange(desc(n_run))
#only once do we have consecutive missing values, and it's a string of just 2

#locate where there's consecutive missingness
df_oil_miss %>%
  mutate(row=row_number(), .before="value") %>%
  mutate(cum_rows=cumsum(n_run)) %>%
  filter(n_run==2) %>%
  pull(cum_rows)
#1176, meaning rows 1175 and 1176

df_oil0 %>%
  mutate(row=row_number()) %>%
  filter(row %in% 1175:1176)
#confirmed: these are missing together


## Compute total missingness
sum(vec_oil_miss) #43
(sum(vec_oil_miss)/length(vec_oil_miss)) * 100 #3.5%


## Imputation
### Impute using different methods
#linear interpolation--problematic b/c first value is NA
library(zoo)

# df_oil0$dcoilwtico_imp <- na.approx(df_oil0$dcoilwtico)

#spline interpolation
df_oil0$dcoilwtico_spline <- na.spline(df_oil0$dcoilwtico)

#moving average
library(imputeTS)

df_oil0$dcoilwtico_mavg <- na_ma(df_oil0$dcoilwtico, k=2, weighting="simple")


### Compare methods visually
df_oil0 %>%
  mutate(missing=is.na(dcoilwtico)) %>%
  ggplot() +
  geom_line(aes(x=date, y=dcoilwtico), color="skyblue") +
  geom_point(data=. %>% filter(missing),
             aes(x=date, y=0), color="red", size=0.5) +
  geom_point(data=. %>% filter(missing),
             aes(x=date, y=dcoilwtico_spline),
             color="darkgreen",
             size=1.5) +
  geom_point(data=. %>% filter(missing),
             aes(x=date, y=dcoilwtico_mavg),
             color="purple",
             size=1.5) +
  theme_bw()

df_oil0 %>%
  mutate(missing=is.na(dcoilwtico)) %>%
  set_names(c("date", "raw", "spline_interp", "mavg_interp", "missing")) %>%
  pivot_longer(cols=!c(date, missing), 
               names_to="data_type",
               values_to="sales") %>%
  mutate(data_type=factor(data_type, levels=c("raw", "spline_interp", "mavg_interp"))) %>%
  ggplot() +
  geom_line(aes(x=date, y=sales, color=data_type)) +
  geom_point(data=. %>% filter(missing),
             aes(x=date, y=0), color="red", size=0.5) +
  scale_color_viridis_d(end=0.6, guide="none") +
  facet_wrap(~data_type, nrow=3) +
  theme_bw() 
#from a visual perspective, they look almost identical to each other


### Compare using summary stats
#summary
df_oil0 %>%
  select(-date) %>%
  purrr::map(summary)
#very similar...but mavg might be slightly closer to raw, so I'll select that method


## Finalize imputation
df_oil_imp <- df_oil0 %>%
  select(date, dcoilwtico_mavg)



# Detect Outliers===================================================================================
## Which datasets?
df_holidays0 #no numerical vars
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
df_train0 %>%
  mutate(store_nbr=as.integer(store_nbr)) %>%
  sample_n(size=200000) %>%
  group_by(store_nbr, family) %>%
  shapiro_test(onpromotion)
  shapiro_test(vars=c("sales", "onpromotion"))
#p=1.92e-85




#Conclusions:
#1) lowish percentage of outliers: max 2.2% of the three numerical variables
#2) histograms and q-q plots as well as shapiro tests indicate non-normal distributions
#NOTE: for report, show histograms to demonstrate skewed data


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










