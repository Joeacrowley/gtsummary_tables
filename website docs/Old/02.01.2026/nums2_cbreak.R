nums2_cbreak <- 
function (outcomes, predictor, data) 
{
    suppressWarnings({
        suppressMessages({
            data %>% tbl_summary(type = all_continuous() ~ "continuous2", 
                include = all_of(outcomes), by = all_of(predictor), 
                missing = "no", statistic = list(all_continuous() ~ 
                  c("{mean}", "{sd}", "{median}", "{min} - {max}", 
                    "{N_nonmiss}")), digits = list(all_continuous() ~ 
                  c(1, 1, 1, 1, 1, 0))) %>% add_overall(last = T) %>% 
                add_p() %>% bold_labels() %>% modify_header(all_stat_cols() ~ 
                "**{level}**", label ~ "")
        })
    })
}
