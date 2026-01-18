.svy_nums2_cbreak <- 
function (outcomes, predictor, data) 
{
    data %>% tbl_svysummary(type = all_continuous() ~ "continuous2", 
        include = all_of(outcomes), by = all_of(predictor), missing = "no", 
        statistic = list(all_continuous() ~ c("{mean}", "{mean.std.error}", 
            "{sd}", "{median}", "{min} - {max}", "{N_nonmiss_unweighted}")), 
        digits = list(all_continuous() ~ c(1, 2, 1, 1, 1, 1, 
            0))) %>% add_overall(last = T) %>% add_p() %>% bold_labels() %>% 
        modify_header(all_stat_cols() ~ "**{level}**", label ~ 
            "") %>% modify_table_body(~.x %>% dplyr::mutate(label = case_when(label == 
        "N not Missing (unweighted)" ~ "N (unweighted)", TRUE ~ 
        label)))
}
