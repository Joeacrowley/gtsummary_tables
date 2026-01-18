svy_nums <- 
function (outcomes, data, stats = "{mean} ({sd})", stats_title = "**Mean (SD)**", 
    caption = NULL, ci = NULL) 
{
    table <- data %>% tbl_svysummary(include = all_of(outcomes), 
        missing = "no", statistic = list(all_continuous() ~ stats), 
        digits = list(all_continuous() ~ 1)) %>% modify_header(all_stat_cols() ~ 
        "**{level}**", label ~ "**Variable**", stat_0 ~ stats_title) %>% 
        modify_footnote(everything() ~ NA) %>% add_n(statistic = "{N_nonmiss_unweighted}", 
        last = TRUE, col_label = "**N (unw)**")
    if (is.null(caption)) {
        caption <- "Weighted summary statistics"
    }
    table <- table %>% modify_caption(paste0("<div style='text-align: left; font-weight: bold; color: black'> ", 
        caption, "</div>"))
    if (!is.null(ci)) {
        table <- table %>% add_ci(statistic = list(all_continuous() ~ 
            "{conf.low} - {conf.high}"), style_fun = all_continuous() ~ 
            label_style_sigfig(scale = 1, digits = 2))
    }
    return(table)
}
