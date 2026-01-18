ncbreak <- 
function (data, outcomes, predictor, nest) 
{
    suppressWarnings({
        suppressMessages({
            cross_break <- list()
            for (i in 1:length(outcomes)) {
                o_var <- outcomes[i]
                by_var <- predictor
                base_name <- paste0("base_size", i)
                nvar <- nest
                df_tmp <- data %>% select(all_of(c(o_var, by_var, 
                  nvar))) %>% filter(!is.na(.data[[nvar]])) %>% 
                  mutate(`:=`(!!base_name, case_when(is.na(.data[[by_var]]) ~ 
                    NA, is.na(.data[[o_var]]) ~ NA, TRUE ~ "Unweighted bases")))
                var_label(df_tmp[[base_name]]) <- ""
                perc <- df_tmp %>% tbl_strata(strata = {
                  {
                    nvar
                  }
                }, ~.x %>% tbl_summary(include = all_of(c(o_var, 
                  base_name)), by = .data[[by_var]], missing = "no", 
                  statistic = list(all_categorical() ~ "{p}", 
                    {
                      {
                        base_name
                      }
                    } ~ "{n}"), digits = list(all_categorical() ~ 
                    0)) %>% add_stat_label(label = {
                  {
                    base_name
                  }
                } ~ "", location = "column") %>% add_overall(last = TRUE) %>% 
                  modify_header(all_stat_cols() ~ "**{level}**", 
                    label ~ "**Variable**", stat_0 ~ "**Total**", 
                    stat_label ~ "") %>% bold_labels() %>% add_p(include = !all_of(base_name)) %>% 
                  modify_footnote(all_stat_cols() ~ NA), .header = "**{strata}**, N = {n}")
                cross_break[[i]] <- perc
            }
            table <- cross_break %>% tbl_stack() %>% modify_caption(paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
                "Crosstabulation by ", var_label(data[[by_var]]), 
                ", nested by ", var_label(data[[nvar]]), ".", 
                "</div>")) %>% modify_column_hide(contains("stat_label_") & 
                !contains("stat_label_1"))
        })
    })
    return(table)
}
