mtotals <- 
function (outcomes, data, caption = "Unweighted percentages") 
{
    result <- map(outcomes, ~tbl_summary(data = data %>% mutate(across(all_of(.x), 
        ~fct_drop(.x), .names = "the_merging_variable"), mutate(across(all_of(.x), 
        ~case_when(!is.na(.x) ~ "Unweighted base"), .names = "base"))) %>% 
        set_variable_labels(base = " "), include = c(the_merging_variable, 
        base), statistic = list(all_categorical() ~ "{p}", base ~ 
        "{n}"), label = list(the_merging_variable ~ " "), missing = "no", 
        digits = everything() ~ 0) %>% modify_header(stat_0 = paste0("**", 
        var_label(data[[.x]]), "**"), label = "") %>% modify_footnote(everything() ~ 
        NA))
    result <- tbl_merge(result, tab_spanner = F)
    result <- result %>% modify_table_body(~.x %>% dplyr::mutate(row_id = row_number()) %>% 
        dplyr::mutate(across(starts_with("stat_"), ~case_when(row_id == 
            1 ~ "%", TRUE ~ .x))) %>% dplyr::select(-row_id))
    result <- result %>% modify_caption(paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
        caption, "</div>"))
    return(result)
}
