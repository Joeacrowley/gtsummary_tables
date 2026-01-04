nums <- 
function (outcomes, data, stats = "{mean} ({sd})", stats_title = "**Mean (SD)**", 
    ci = NULL) 
{
    table <- data %>% tbl_summary(include = all_of(outcomes), 
        missing = "no", statistic = list(all_continuous() ~ stats), 
        digits = list(all_continuous() ~ 1)) %>% modify_header(all_stat_cols() ~ 
        "**{level}**", label ~ "**Variable**", stat_0 ~ stats_title) %>% 
        modify_footnote(everything() ~ NA) %>% add_n(last = TRUE)
    if (!is.null(ci)) {
        table <- table %>% add_ci(statistic = list(all_continuous() ~ 
            "{conf.low} - {conf.high}"), style_fun = all_continuous() ~ 
            label_style_sigfig(scale = 1, digits = 2))
    }
    return(table)
}
