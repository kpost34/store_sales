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





