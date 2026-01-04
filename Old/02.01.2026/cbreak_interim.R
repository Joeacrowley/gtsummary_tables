cbreak_interim <- 
function (data, outcome, crossbreak, ci = FALSE) 
{
    cross_break <- list()
    bases <- list()
    for (i in 1:length(outcome)) {
        o_var <- outcome[i]
        by_var <- crossbreak
        base_name <- paste0("base_size", i)
        o_lab <- paste0(var_label(data[o_var], null_action = "fill"))
        df_tmp <- data %>% select(all_of(c(outcome, by_var))) %>% 
            mutate(`:=`(!!base_name, case_when(is.na(across(all_of(by_var))) ~ 
                NA, is.na(across(all_of(o_var))) ~ NA, TRUE ~ 
                o_lab)))
        var_label(df_tmp[[base_name]]) <- ""
        if (base_name == "base_size1") {
            var_label(df_tmp[[base_name]]) <- "Unweighted sample sizes"
        }
        perc <- df_tmp %>% tbl_summary(include = all_of(c(o_var)), 
            by = .data[[by_var]], missing = "no", statistic = list(all_categorical() ~ 
                "{p}"), digits = list(all_categorical() ~ 0)) %>% 
            add_overall(last = F) %>% modify_header(all_stat_cols() ~ 
            "**{level} (%)**", label ~ "**Variable**", stat_0 ~ 
            "**Total (%)**", ) %>% bold_labels() %>% add_p()
        base_of_table <- df_tmp %>% tbl_summary(include = all_of(base_name), 
            by = .data[[by_var]], missing = "no", statistic = list({
                {
                  base_name
                }
            } ~ "{n}"), digits = list(all_categorical() ~ 0)) %>% 
            add_overall(last = F) %>% modify_header(all_stat_cols() ~ 
            "**{level} (%)**", label ~ "**Variable**", stat_0 ~ 
            "**Total (%)**", ) %>% bold_labels()
        if (ci == TRUE) {
            perc <- perc %>% add_ci(statistic = list(all_categorical() ~ 
                "[{conf.low} - {conf.high}]"), style_fun = all_categorical() ~ 
                label_style_sigfig(scale = 100, digits = 1))
        }
        cross_break[[i]] <- perc
        bases[[i]] <- base_of_table
    }
    table <- append(cross_break, bases) %>% tbl_stack() %>% modify_caption("<div style='text-align: left; font-weight: bold; color: black'> Unweighted percentages</div>") %>% 
        modify_footnote(all_stat_cols() ~ NA) %>% modify_spanning_header(all_stat_cols() ~ 
        paste0("**", var_label(data[[by_var]]), "**"), starts_with("ci_stat") ~ 
        paste0("**", var_label(data[[by_var]]), "**"), ends_with("_0") ~ 
        NA)
    base_break_row <- which(table[["table_body"]] %>% pull(label) == 
        "Unweighted sample sizes")
    table <- table %>% modify_table_body(~.x %>% filter(!(grepl("base_size([2-9]|[1-9][0-9]|100)", 
        variable, ignore.case = T) & label == "")) %>% add_row(.before = base_break_row))
    return(table)
}
