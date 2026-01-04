svy_cbreak_interim <- 
function (data, outcome, crossbreak, ci = FALSE) 
{
    cross_break <- list()
    for (i in 1:length(outcome)) {
        o_var <- outcome[i]
        by_var <- crossbreak
        base_name <- paste0("base_size", i)
        df_tmp <- data %>% select(all_of(c(o_var, by_var))) %>% 
            mutate(`:=`(!!base_name, case_when(is.na(.data[[by_var]]) ~ 
                NA, is.na(.data[[o_var]]) ~ NA, TRUE ~ "Unweighted bases")))
        var_label(df_tmp[["variables"]][[base_name]]) <- ""
        perc_break <- df_tmp %>% filter(!is.na(.data[[by_var]])) %>% 
            tbl_svysummary(include = all_of(c(o_var, base_name)), 
                by = .data[[by_var]], missing = "no", statistic = list(all_categorical() ~ 
                  "{p}", base_name ~ "{n_unweighted}"), digits = list(all_categorical() ~ 
                  0)) %>% modify_header(all_stat_cols() ~ "**{level}**", 
            label ~ "**Variable**") %>% bold_labels() %>% add_p(include = !all_of(base_name))
        perc_total <- df_tmp %>% tbl_svysummary(include = all_of(c(o_var, 
            base_name)), missing = "no", statistic = list(all_categorical() ~ 
            "{p}", base_name ~ "{n_unweighted}"), digits = list(all_categorical() ~ 
            0)) %>% add_stat_label(label = {
            {
                base_name
            }
        } ~ "", location = "column") %>% modify_header(all_stat_cols() ~ 
            "**{level}**", label ~ "**Variable**", stat_0 ~ "**Total**", 
            stat_label ~ "") %>% bold_labels()
        if (ci == TRUE) {
            perc_total <- perc_total %>% add_ci(include = -base_name, 
                statistic = list(all_categorical() ~ "[{conf.low} - {conf.high}]"), 
                style_fun = all_categorical() ~ label_style_sigfig(scale = 100, 
                  digits = 1))
            perc_break <- perc_break %>% add_ci(include = -base_name, 
                statistic = list(all_categorical() ~ "[{conf.low} - {conf.high}]"), 
                style_fun = all_categorical() ~ label_style_sigfig(scale = 100, 
                  digits = 1))
        }
        perc <- tbl_merge(list(perc_total, perc_break), tab_spanner = F)
        cross_break[[i]] <- perc
    }
    table <- cross_break %>% tbl_stack() %>% modify_caption("<div style='text-align: left; font-weight: bold; color: black'> Weighted percentages</div>") %>% 
        modify_footnote(all_stat_cols() ~ "Weighted percentages. Unweighted base sizes shown below each table. ") %>% 
        modify_spanning_header(all_stat_cols() ~ paste0("**", 
            var_label(svy_df[["variables"]][[by_var]]), "**"), 
            starts_with("ci_stat") ~ paste0("**", var_label(svy_df[["variables"]][[by_var]]), 
                "**"), ends_with("_0") ~ NA)
    return(table)
}
