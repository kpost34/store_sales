

# Load Packages, Objects, Functions, and Data=======================================================
#packages
library(pacman) 
pacman::p_load(here, tidyverse, janitor, skimr, visdat, rstatix, zoo, bestNormalize)

#functions
source(here("code", "00_helper_fns.R"))

#data
fp_train_in <- here("data", "tidy_data", "train_merged.rds")
df_train <- readRDS(fp_train_in)


# Feature Engineering===============================================================================
## Date features
df_train_feat <- df_train %>% 
  #create time-related variables
  mutate(day=wday(date, label=TRUE),
         month=month(date, label=TRUE),
         quarter=quarter(date),
         quarter=as.ordered(quarter),
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
  mutate(sales_lag1=lag(sales, n=1),
         sales_lag7=lag(sales, n=7)) %>% 

## Rolling statistics
  #still grouped by store and family
  mutate(sales_mean7=rollmean(sales, k=7, fill=NA, align="right"),
         sales_mean30=rollmean(sales, k=30, fill=NA, align="right")) %>%
  ungroup()



# Handle Non-Normality and Outliers=================================================================
#From previous script...
  #outliers are minimal except for grouping by store and family yields onpromotion: 9.26% and
    #sales: 5.11%
  #distributions are either bimodal (oil) or have long, right tails (others)


## Grouping---------------------
#oil - no grouping 
#transactions - by store_nbr
#sales (and sales-related vars) - by store_nbr and family
#onpromotion - by store_nbr and family



## Transformations------------------------------------------
vec_transforms <- c("untransformed", "log1", "yj", "scale", "qt")

### Oil---------------------
#### Prepare data
df_oil <- df_train_feat %>%
  select(date, dcoilwtico_mavg) %>%
  distinct() %>%
  filter(!is.na(dcoilwtico_mavg))


#### Perform transformation and pivot
df_oil_trans <- df_oil %>%
  mutate(oil_yj=yeojohnson(dcoilwtico_mavg) %>% predict(), #yeo-johnson
         oil_qt=orderNorm(dcoilwtico_mavg) %>% predict(), #quantile transform
         oil_log1=log1p(dcoilwtico_mavg), #log + 1
         oil_scale=scale(dcoilwtico_mavg, center=TRUE, scale=IQR(dcoilwtico_mavg))) %>% #robust scaling
  pivot_longer(!date, names_to="type", values_to="cost") %>% 
  mutate(type=ifelse(!str_detect(type, "^oil"),
                     "untransformed",
                     str_remove(type, "^oil_")),
         type=factor(type, levels=vec_transforms))


#### Assess transformation
##### Histograms
df_oil_trans %>%
  ggplot() +
  geom_histogram(aes(x=cost, fill=type), color="black") +
  facet_wrap(~type, scales="free") +
  scale_color_viridis_d("B") +
  theme_bw() +
  theme(legend.position="bottom")
#the quantile transformation clearly has the best distribution--turns the bimodal one into a
  #Gaussian one


##### Q-q plots
df_oil_trans %>%
  ggplot() +
  geom_qq(aes(sample=cost, color=type)) +
  geom_qq_line(aes(sample=cost), color='black') +
  facet_wrap(~type, scales="free") +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position="none")
#no question that qt fits data best
  

##### Shapiro tests
df_oil_trans %>%
  group_by(type) %>%
  shapiro_test(cost)
#again, qt fits data best



### Transactions---------------------
#### Prepare data
df_transactions <- df_train_feat %>%
  select(date, store_nbr, transactions) %>%
  distinct()


#### Perform transformations and pivot
df_transactions_trans <- df_transactions %>%
  filter(!is.na(transactions)) %>% #NAs produced from joining
  group_by(store_nbr) %>%
  mutate(transactions_yj=yeojohnson(transactions) %>% predict(), #yeo-johnson
         transactions_qt=orderNorm(transactions) %>% predict(), #quantile transformation
         transactions_log1=log1p(transactions), #log + 1
         transactions_scale=scale(transactions, center=TRUE, scale=IQR(transactions))) %>% #robust scaling
  ungroup() %>%
  rename_with(.cols=!c(date, store_nbr),
              .fn=~ifelse(.x=="transactions",
                          "untransformed",
                          str_remove(.x, "^transactions_"))) 

df_transactions_trans_long <- df_transactions_trans %>%
  pivot_longer(!c(date, store_nbr), names_to="type", values_to="transactions") %>%
  mutate(type=factor(type, levels=vec_transforms))


#### Assess transformation
##### Overall
df_transactions_trans_long %>% 
  ggplot() +
  geom_histogram(aes(x=transactions, fill=type), color="black") +
  facet_wrap(~type, scales="free") +
  scale_fill_viridis_d("A") +
  theme_bw() +
  theme(legend.position="none")
#unsurprisingly, qt has the best distribution but it may be 'over'-transforming it. log x + 1
  #appears to be close to normal and same with the y-j transform


##### By store_nbr
#get sample
set.seed(100)
samp_transaction <- sample.int(54, 9)


###### Histograms
list_transactions_hist <- vec_transforms %>%
  purrr::map(function(x) {
    make_grouped_hist(dat=df_transactions_trans,
                      group=store_nbr,
                      var=!!sym(x),
                      filt=samp_transaction)
  }) %>%
  set_names(vec_transforms)

list_transactions_hist$untransformed
list_transactions_hist$yj
list_transactions_hist$log1
list_transactions_hist$qt
list_transactions_hist$scale
#qt 'looks' the best but concerns over over-transformation


###### Density plots
list_transactions_dens <- vec_transforms %>%
  purrr::map(function(x) {
    make_grouped_density(dat=df_transactions_trans,
                        group=store_nbr,
                        var=!!sym(x),
                        filt=samp_transaction)
  }) %>%
  set_names(vec_transforms)

list_transactions_dens$untransformed
list_transactions_dens$yj
list_transactions_dens$log1
list_transactions_dens$qt
list_transactions_dens$scale
#qt-transformed has the best distributions--others retain long tails and bimodal distributions


##### Q-q plots
list_transactions_qq <- vec_transforms %>%
  purrr::map()


df_transactions_trans_long %>%
  filter(store_nbr %in% samp_transaction,
         type=="untransformed") %>%
  ggplot() +
  geom_qq(aes(sample=transactions, color=store_nbr)) +
  geom_qq_line(aes(sample=transactions), color='black') +
  facet_wrap(~store_nbr, scales="free") +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position="none")
#no question that qt fits data best
  

##### Shapiro tests
df_oil_trans %>%
  group_by(type) %>%
  shapiro_test(cost)
#again, qt fits data best



## Sales and sales-related---------------------
### Prepare data
df_sales <- df_train_feat %>%
  select(date, store_nbr, family, starts_with("sales")) %>%
  distinct()
  

### Perform transformations
sales_vars <- c("sales", "sales_lag1", "sales_lag7", "sales_mean7", "sales_mean30")

vec_family <- df_sales %>%
  pull(family) %>%
  unique() %>%
  as.character()

set.seed(37)
samp_sales <- sample.int(54, 4)
samp_family <- vec_family[sample.int(33, 3)]


df_sales_trans <- df_sales %>%
  #apply filters here b/c of lack of ram
  filter(store_nbr %in% samp_sales,
         family %in% samp_family) %>%
  #combine store number and family into var
  mutate(store_fam=paste0(family, " (", store_nbr, ")"), .after="family") %>%
  group_by(store_nbr) %>%
  #transform variables
  apply_transform(vars=sales_vars, fn="yj") %>% #yeo-johnson (right-skewed)
  apply_transform(vars=sales_vars, fn="qt") %>% #quantile transform (bimodal/to normalize)
  apply_transform(vars=sales_vars, fn="log1") %>% #log + 1 (right-skewed)
  apply_transform(vars=sales_vars, fn="scale") %>% #robust scaling (reduce outliers)
  ungroup() %>%
  rename_with(.cols=c(sales, sales_lag1, sales_lag7, sales_mean7, sales_mean30),
              .fn=~paste0(.x, "__untransformed"))

  
### Pivot to long form
df_sales_trans_long <- df_sales_trans %>%
  pivot_longer(cols=!c(date, store_nbr, family, store_fam),
               names_to=c("variable", ".value"),
               names_sep="__") %>%
  pivot_longer(cols=!c(date, store_nbr, family, store_fam, variable),
               names_to="transform_type",
               values_to="value") 


### Assess transformation
#### Sales
vec_transform <- c("untransformed", "scale", "yj", "log1}", "qt")

#### Histograms
list_sales_hist <- vec_transform %>%
  purrr::map(function(x) {
    make_hist(dat=df_sales_trans_long,
              var="sales",
              transform=x,
              val=value,
              facet=store_fam)
  }) %>%
  set_names(vec_transform)

list_sales_hist$untransformed
list_sales_hist$scale
list_sales_hist$yj
list_sales_hist$`log1`
list_sales_hist$qt


#### Density
list_sales_dens <- vec_transform %>%
  purrr::map(function(x) {
    make_density(dat=df_sales_trans_long,
                 var="sales",
                 transform=x,
                 val=value,
                 facet=store_fam)
  }) %>%
  set_names(vec_transform)

list_sales_dens$untransformed
list_sales_dens$scale
list_sales_dens$yj
list_sales_dens$`log1`
list_sales_dens$qt


#### sales_lag7
list_sales_lag7_dens <- vec_transform %>%
  purrr::map(function(x) {
    make_density(dat=df_sales_trans_long,
                var="sales_lag7",
                transform=x,
                val=value,
                facet=store_fam)
  }) %>%
  set_names(vec_transform)

list_sales_lag7_dens$untransformed
list_sales_lag7_dens$scale
list_sales_lag7_dens$yj
list_sales_lag7_dens$`log1`
list_sales_lag7_dens$qt


#### sales_mean30
list_sales_mean30_dens <- vec_transform %>%
  purrr::map(function(x) {
    make_density(dat=df_sales_trans_long,
                var="sales_mean30",
                transform=x,
                val=value,
                facet=store_fam)
  }) %>%
  set_names(vec_transform)

list_sales_mean30_dens$untransformed
list_sales_mean30_dens$scale
list_sales_mean30_dens$yj
list_sales_mean30_dens$`log1`
list_sales_mean30_dens$qt



## Promotions---------------------
### Prepare data
df_promo <- df_train_feat %>%
  select(date, store_nbr, family, onpromotion) %>%
  distinct()


### Perform transformations
vec_family <- df_promo %>%
  pull(family) %>%
  unique() %>%
  as.character()

set.seed(33)
samp_promo <- sample.int(54, 4)
samp_fam <- vec_family[sample.int(33, 3)]


df_promo_trans <- df_promo %>%
  #apply filters here b/c of lack of ram
  filter(store_nbr %in% samp_promo,
         family %in% samp_fam) %>%
  #combine store number and family into var
  mutate(store_fam=paste0(family, " (", store_nbr, ")"), .after="family") %>%
  group_by(store_nbr) %>%
  #transform variables
  apply_transform(vars="onpromotion", fn="yj") %>% #yeo-johnson (right-skewed)
  apply_transform(vars="onpromotion", fn="qt") %>% #quantile transform (bimodal/to normalize)
  apply_transform(vars="onpromotion", fn="log1") %>% #log + 1 (right-skewed)
  apply_transform(vars="onpromotion", fn="scale") %>% #robust scaling (reduce outliers)
  ungroup() %>%
  rename(onpromotion__untransformed="onpromotion")


### Pivot to long form
df_promo_trans_long <- df_promo_trans %>%
  rename_with(.cols=!c(date, store_nbr, family, store_fam),
              .fn=~str_remove_all(.x, "^onpromotion__|\\}")) %>%
  pivot_longer(cols=!c(date, store_nbr, family, store_fam),
               names_to="transform_type",
               values_to="value")


### Check data
df_promo_trans_long %>% 
  filter(transform_type=="untransformed") %>%
  mutate(value_0=value==0) %>%
  group_by(store_fam) %>%
  reframe(n=n(),
          n_0=sum(value_0),
          pct_0=(n_0/n)*100,
          min=min(value),
          mean=mean(value),
          median=median(value),
          max=max(value))
#data have such a high proportion of 0s that a transformation won't help much


### Assess transformation
list_promo_hist <- vec_transform %>%
  purrr::map(function(x) {
    make_hist(dat=df_promo_trans_long,
              transform=x,
              val=value,
              facet=store_fam)
  }) %>%
  set_names(vec_transform)

list_promo_hist$untransformed #high freq of 0s
list_promo_hist$scale #error
list_promo_hist$yj #resembles untransformed closely
list_promo_hist$`log1` #error
list_promo_hist$qt #resembles untransformed closely
#given that there's no discernible benefit to the distribution following transformation, leave
  #untransformed




#STOPPING POINT: figuring out why I can't get density plots for some transforms to display--
  #rec'd doing an 'is.finite(x)' filter upstream when creating the dfs

#transformations selected
  #oil: qt
  #transactions: qt
  #sales and sales-related:
  #onpromotion: untransformed

  

  
# Encoding Categorical Variables====================================================================
  
  
# Time-Series Specific Techniques===================================================================
## Differencing



## Decomposition



# Model Selection and Validation====================================================================
## Train/test split



## Cross-Validation






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

