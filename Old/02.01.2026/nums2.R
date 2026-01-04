nums2 <- 
function (outcomes, data, stats = c("{mean}", "{median}", "{sd}", 
    "{min} - {max}", "{N_nonmiss}"), digits = c(1, 1, 1, 1, 1, 
    0)) 
{
    data %>% tbl_summary(type = all_continuous() ~ "continuous2", 
        include = all_of(outcomes), missing = "no", statistic = list(all_continuous() ~ 
            stats), digits = list(all_continuous() ~ digits)) %>% 
        modify_header(all_stat_cols() ~ "**{level}**", label ~ 
            "**Variable**", stat_0 ~ "**Statistics**") %>% bold_labels()
}
