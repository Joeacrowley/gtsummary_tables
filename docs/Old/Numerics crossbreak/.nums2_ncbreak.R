.nums2_ncbreak <- 
function (outcomes, predictor, data, nvar) 
{
    table <- data %>% filter(!is.na(.data[[nvar]])) %>% tbl_strata(strata = {
        {
            nvar
        }
    }, ~.x %>% tbl_summary(type = list(all_continuous() ~ "continuous2", 
        all_categorical() ~ "continuous2"), include = all_of(outcomes), 
        by = all_of(predictor), missing = "no", statistic = list(all_continuous() ~ 
            c("{mean}", "{median}", "{sd}", "{min} - {max}", 
                "{N_nonmiss}")), digits = list(all_continuous() ~ 
            c(1, 1, 1, 1, 1, 0))) %>% add_overall(last = T) %>% 
        add_p() %>% bold_labels() %>% modify_header(all_stat_cols() ~ 
        "**{level}**", label ~ ""), .header = "**{strata}**, N = {n}", 
        ) %>% modify_caption(paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
        "Crosstabulation by ", var_label(data[[predictor]]), 
        ", nested by ", var_label(data[[nvar]]), ".", "</div>"))
    return(table)
}
