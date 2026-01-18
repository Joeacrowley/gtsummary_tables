svy_nums2 <- 
function (outcomes, data, stats = "default", digits = c(1, 2, 
    1, 1, 1, 1, 0)) 
{
    if ("default" %in% stats) {
        stats_to_use <- c("{mean}", "{mean.std.error}", "{sd}", 
            "{median}", "{min} - {max}", "{N_nonmiss_unweighted}")
    }
    else {
        stats_to_use <- stats
    }
    table <- data %>% tbl_svysummary(type = all_continuous() ~ 
        "continuous2", include = all_of(outcomes), missing = "no", 
        statistic = list(all_continuous() ~ stats_to_use), digits = list(all_continuous() ~ 
            digits)) %>% modify_header(all_stat_cols() ~ "**{level}**", 
        label ~ "**Variable**", stat_0 ~ "**Statistics (weighted)**") %>% 
        bold_labels() %>% modify_table_body(~.x %>% dplyr::mutate(label = case_when(label == 
        "N not Missing (unweighted)" ~ "N (unweighted)", TRUE ~ 
        label)))
    if ("default" %in% stats) {
        table <- table %>% add_stat_label(label = list(all_continuous2() ~ 
            c("Mean", "Mean SE", "Mean SD", "Median", "Range", 
                "N (unw.)")))
    }
    return(table)
}
