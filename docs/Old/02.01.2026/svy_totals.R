svy_totals <- 
function (data, variables, stat = "{p}", caption = NULL) 
{
    statistic_label <- ifelse(stat == "{p}", " (%)", "") %>% 
        paste0("**{level}", ., "**")
    footnot_text <- ifelse(stat == "{p}", "Weighted percentages. Base size shown in column N", 
        "Weighted count. Base size shown in column N")
    if (is.null(caption)) {
        caption <- ifelse(stat == "{p}", "Weighted percentages", 
            "Weighted count")
    }
    table <- data %>% tbl_svysummary(include = all_of(variables), 
        missing = "no", statistic = list(all_categorical() ~ 
            stat), digits = list(all_categorical() ~ 0)) %>% 
        modify_header(all_stat_cols() ~ statistic_label, label ~ 
            "**Variable**") %>% bold_labels() %>% modify_caption(paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
        caption, "</div>")) %>% add_n(statistic = "{N_nonmiss_unweighted}", 
        last = TRUE) %>% modify_footnote(all_stat_cols() ~ footnot_text)
    return(table)
}
