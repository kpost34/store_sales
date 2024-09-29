# Helper functions for Store Sales Project

# Exploratory Data Analysis=========================================================================
## Functions to calculate z-score
calc_zscore <- function(var) {
  (var-mean(var, na.rm=TRUE)) / sd(var, na.rm=TRUE)
}

calc_outlier_n_pct <- function(df, var1, var2=NULL, fn, thresh) {
  df %>%
    mutate(across(c({{var1}}, {{var2}}), fn, .names="{.col}_score"),
           across(ends_with("_score"), ~.x > thresh, .names="{.col}_outlier")) %>%
    reframe(across(ends_with("_outlier"), sum, .names="{.col}_n"),
            across(ends_with("_outlier"), 
                   ~{(sum(.x)/n())*100} %>%
                     signif(3), .names="{.col}_pct")) %>%
    ungroup() %>%
    rename_with(.cols=everything(), ~str_remove(.x, "_score")) %>%
    relocate(order(names(.))) %>%
    relocate(!ends_with(c("_n", "_pct")))
    # relocate(!ends_with(c("_n", "_pct")), sort(ends_with(c("_n", "_pct"))))
}
  

## Function to calculate iqr and determine if value is outlier
calc_iqr <- function(dat, var, thresh) {
  var_q1 <- rlang::englue("{{var}}_q1")
  var_q3 <- rlang::englue("{{var}}_q3")
  var_iqr <- rlang::englue("{{var}}_iqr")
  
  dat %>%
    mutate(!!sym(var_q1) := quantile({{var}}, 0.25, na.rm=TRUE),
           !!sym(var_q3) := quantile({{var}}, 0.75, na.rm=TRUE),
           "{{var}}_iqr" := !!sym(var_q3) - !!sym(var_q1),
           "{{var}}_is_outlier" := ({{var}} < (!!sym(var_q1) - thresh * !!sym(var_iqr))) |
                                ({{var}} > (!!sym(var_q3) + thresh * !!sym(var_iqr))))
}



# Feature Engineering===============================================================================
## Function to apply data transformations for normality and to reduce outliers
apply_transform <- function(dat, vars, fn) {
  dat %>%
    {if(fn=="yj") mutate(dat, across(all_of(vars), 
                                     ~yeojohnson(.x) %>% predict(),
                                     .names="{.col}__yj")) else .} %>%
    {if(fn=="qt") mutate(dat, 
                         across(all_of(vars),
                                ~orderNorm(.x) %>% predict(),
                                .names="{.col}__qt")) else .} %>%
    {if(fn=="log1") mutate(dat,
                           across(all_of(vars), log1p, .names="{.col}__log1")) else .} %>%
    {if(fn=="scale") mutate(dat,
                           across(all_of(vars),
                                  ~scale(.x, center=TRUE, scale=IQR(.x, na.rm=TRUE)),
                                  .names="{.col}__scale")) else .}
}



## Function to generate histogram of transactions grouped by store_nbr
make_grouped_hist <- function(dat, group, var, filt) {
  dat %>% 
    select(date, {{group}}, {{var}}) %>%
    filter({{group}} %in% as.character(filt)) %>%
    ggplot() +
    geom_histogram(aes(x={{var}}, fill={{group}}), color="black") +
    facet_wrap(vars({{group}}), scales="free") +
    scale_fill_viridis_d("A") +
    theme_bw() +
    theme(legend.position="none")
}


## Function to generate density plot of transactions grouped by store_nbr
make_grouped_density <- function(dat, group, var, filt) {
  dat %>% 
    select(date, {{group}}, {{var}}) %>%
    filter({{group}} %in% as.character(filt)) %>%
    ggplot() +
    geom_density(aes(x={{var}}, fill={{group}}), color="black") +
    facet_wrap(vars({{group}}), scales="free") +
    scale_fill_viridis_d("A") +
    theme_bw() +
    theme(legend.position="none")
}


## Function to generate qqplot of transactions grouped by store_nbr
make_grouped_qqplot <- function(dat, group, var, filt) {
  dat %>%
    filter({{group}} %in% as.character(filt)) %>%
    ggplot() +
    geom_qq(aes(sample={{var}}, color={{group}})) +
    geom_qq_line(aes(sample={{var}}), color='black') +
    facet_wrap(~store_nbr, scales="free") +
    scale_color_viridis_d() +
    theme_bw() +
    theme(legend.position="none")
}


## Function to generate histograms of sales or promotions faceted by store_fam
make_hist <- function(dat, var=NA, transform, val, facet) {
  dat %>%
    {if(!is.na(var)) filter(., variable==var) else .} %>%
    filter(transform_type==transform) %>%
    ggplot() +
    geom_histogram(aes(x={{val}}, fill={{facet}}), color='black') +
    facet_wrap(vars({{facet}}), scales="free") +
    scale_fill_viridis_d("A") +
    theme_bw() +
    theme(legend.position="none")
}


## Function to generate density plot of sales or promotions faceted by store_fam
make_density <- function(dat, var=NA, transform, val, facet) {
  dat %>%
    {if(!is.na(var)) filter(., variable==var) else .} %>%
    filter(transform_type==transform) %>%
    ggplot() +
    geom_density(aes(x={{val}}, color={{facet}})) +
    facet_wrap(vars({{facet}}), scales="free") +
    scale_fill_viridis_d("A") +
    theme_bw() +
    theme(legend.position="none")
}

## Function to generate qqplots of sales or promotions faceted by store_fam
make_qqplot <- function(dat, var=NA, transform, val, facet) {
  dat %>%
    {if(!is.na(var)) filter(., variable==var) else .} %>%
    filter(transform_type==transform) %>%
    ggplot() +
    geom_qq(aes(sample={{val}}, color={{facet}})) +
    geom_qq_line(aes(sample={{val}}), color='black') +
    facet_wrap(vars({{facet}}), scales="free") +
    scale_fill_viridis_d("A") +
    theme_bw() +
    theme(legend.position="none")
}


## Function to apply min-max scaling
min_max_scale <- function(var) {
  var_mmscale <- (var - min(var, na.rm=TRUE)/(max(var, na.rm=TRUE) - min(var, na.rm=TRUE))
  
  return(var_mmscale)
}












