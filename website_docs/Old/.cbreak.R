.cbreak <- 
function (data, outcome, crossbreak, ci = FALSE) 
{
    cross_break <- list()
    for (i in 1:length(outcome)) {
        o_var <- outcome[i]
        by_var <- crossbreak
        base_name <- paste0("base_size", i)
        df_tmp <- data %>% select(all_of(c(outcome, by_var))) %>% 
            mutate(`:=`(!!base_name, case_when(is.na(.data[[by_var]]) ~ 
                NA, is.na(.data[[o_var]]) ~ NA, TRUE ~ "Unweighted bases")))
        var_label(df_tmp[[base_name]]) <- ""
        perc <- df_tmp %>% tbl_summary(include = all_of(c(o_var, 
            base_name)), by = .data[[by_var]], missing = "no", 
            statistic = list(all_categorical() ~ "{p}", {
                {
                  base_name
                }
            } ~ "{n}"), digits = list(all_categorical() ~ 0)) %>% 
            add_stat_label(label = {
                {
                  base_name
                }
            } ~ "", location = "column") %>% add_overall(last = TRUE) %>% 
            modify_header(all_stat_cols() ~ "**{level}**", label ~ 
                "**Variable**", stat_0 ~ "**Total**", stat_label ~ 
                "") %>% bold_labels() %>% add_p(include = !all_of(base_name))
        if (ci == TRUE) {
            perc <- perc %>% add_ci(include = -base_name, statistic = list(all_categorical() ~ 
                "[{conf.low} - {conf.high}]"), style_fun = all_categorical() ~ 
                label_style_sigfig(scale = 100, digits = 1))
        }
        cross_break[[i]] <- perc
    }
    table <- cross_break %>% tbl_stack() %>% modify_caption("<div style='text-align: left; font-weight: bold; color: black'> Unweighted percentages</div>") %>% 
        modify_footnote(all_stat_cols() ~ NA) %>% modify_spanning_header(all_stat_cols() ~ 
        paste0("**", var_label(data[[by_var]]), "**"), starts_with("ci_stat") ~ 
        paste0("**", var_label(data[[by_var]]), "**"), ends_with("_0") ~ 
        NA)
    return(table)
}
