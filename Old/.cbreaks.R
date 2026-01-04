.cbreaks <- 
function (data, predictor, outcomes) 
{
    suppressWarnings({
        suppressMessages({
            cross_break <- list()
            for (i in 1:length(outcomes)) {
                o_var <- outcomes[i]
                by_var <- predictor
                data <- data %>% select(all_of(c(outcomes, by_var))) %>% 
                  mutate(base_size = case_when(is.na(.data[[by_var]]) ~ 
                    NA, is.na(.data[[o_var]]) ~ NA, TRUE ~ "Unweighted bases"))
                var_label(data$base_size) <- ""
                cross_break[[i]] <- data %>% tbl_summary(include = all_of(c(o_var, 
                  "base_size")), by = .data[[by_var]], missing = "no", 
                  statistic = list(all_categorical() ~ "{p}", 
                    base_size = "{n}"), digits = list(all_categorical() ~ 
                    0)) %>% add_stat_label(label = base_size ~ 
                  "", location = "column") %>% add_overall(last = TRUE) %>% 
                  modify_header(all_stat_cols() ~ "**{level}**", 
                    label ~ "**Variable**", stat_0 ~ "**Total**", 
                    stat_label ~ "") %>% bold_labels() %>% modify_caption("<div style='text-align: left; font-weight: bold; color: black'> Weighted percentages</div>") %>% 
                  modify_footnote(everything() ~ NA) %>% modify_spanning_header(all_stat_cols() ~ 
                  paste0("**", var_label(data[[by_var]]), "**"))
            }
        })
    })
    return(cross_break)
}
