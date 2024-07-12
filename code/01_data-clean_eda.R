#01_data-clean_eda.R

#This script 1) performs initial data cleaning 2) explores data


# Load Packages, Objects, Functions, and Data=======================================================
#packages
library(pacman) 
pacman::p_load(here, tidyverse, janitor, skimr, visdat)

#objects
# source(here("code", "00_objects.R"))

#functions
# source(here("code", "00_helper_fns.R"))

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

#need to add col types: specifically changing doubles to chr and chr to fcts

# Initial Data Checking and Wrangling===============================================================
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
             aes(x=date, y=0), color="red") +
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

df_oil0$dcoilwtico_imp <- na.approx(df_oil0$dcoilwtico)

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
             aes(x=date, y=0), color="red") +
  geom_point(data=. %>% filter(missing),
             aes(x=date, y=dcoilwtico_spline),
             color="darkgreen",
             size=2) +
  geom_point(data=. %>% filter(missing),
             aes(x=date, y=dcoilwtico_mavg),
             color="purple",
             size=2) +
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
             aes(x=date, y=0), color="red") +
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











