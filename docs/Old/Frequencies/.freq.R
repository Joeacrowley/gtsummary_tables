.freq <- 
function (data, variables, stat = "{p}", caption = NULL, ci = NULL) 
{
    statistic_label <- ifelse(stat == "{p}", " (%)", "") %>% 
        paste0("**{level}", ., "**")
    footnot_text <- ifelse(stat == "{p}", "Unweighted percentages. Base size shown in column N", 
        "Unweighted count. Base size shown in column N")
    if (is.null(caption)) {
        caption <- ifelse(stat == "{p}", "Unweighted percentages", 
            "Unweighted count")
    }
    table <- data %>% tbl_summary(include = all_of(variables), 
        missing = "no", statistic = list(all_categorical() ~ 
            stat), digits = list(all_categorical() ~ 0)) %>% 
        modify_header(all_stat_cols() ~ statistic_label, label ~ 
            "**Variable**") %>% bold_labels() %>% modify_caption(paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
        caption, "</div>")) %>% add_n(last = TRUE) %>% modify_footnote(all_stat_cols() ~ 
        footnot_text)
    if (!is.null(ci)) {
        table <- table %>% add_ci(statistic = list(all_categorical() ~ 
            "{conf.low}-{conf.high}"), style_fun = all_categorical() ~ 
            label_style_sigfig(scale = 100, digits = 1))
    }
    return(table)
}
