tables <- 
function (data, variables, crossbreaks = NULL, stats_cat = "{p}", 
    stats_num = "{mean}", ci = NULL, num_digits = 2, split_num = F, 
    sig = NULL, caption = NULL, bases = NULL, source = NULL, 
    footnotes = NULL) 
{
    if (is.null(crossbreaks)) {
        crossbreaks <- list(NULL)
    }
    tables_int <- map(crossbreaks, ~data %>% totals(variables_int = variables, 
        crossbreak = .x, stats_cat_int = stats_cat, stats_num_int = stats_num, 
        ci_int = ci, num_digits_int = num_digits, split_num_int = split_num, 
        sig_int = sig))
    if (length(tables_int) > 1) {
        for (i in 2:length(tables_int)) {
            tables_int[[i]] <- tables_int[[i]] %>% modify_table_body(~.x %>% 
                select(!contains("stat_0")) %>% select(!contains("stat_label")))
        }
        variable_labels <- data %>% select(all_of(crossbreaks)) %>% 
            var_label(unlist = T) %>% paste0("**", ., "**")
        tables <- tbl_merge(tables_int, tab_spanner = variable_labels) %>% 
            modify_spanning_header(contains("stat_label") ~ NA, 
                contains("stat_0") ~ NA)
    }
    else {
        tables <- tables_int[[1]]
    }
    if (!is.null(crossbreaks)) {
        bases_variables <- c(variables, crossbreaks)
    }
    footnotes <- gtsummary_table_notes(bases_info = bases, vars = bases_variables, 
        source_note = source, other_footnotes = footnotes)
    if (footnotes != "") {
        tables <- tables %>% modify_source_note(source_note = footnotes, 
            text_interpret = "html")
    }
    if (!is.null(caption)) {
        tables <- tables %>% modify_caption(paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
            caption, "</div>"))
    }
    if (class(data) %>% grep("survey.design", ., value = T) %>% 
        length() > 0) {
        stat_types <- "All statistics shown in this table are weighted."
    }
    else {
        stat_types <- "All statistics shown in this table are unweighted."
    }
    tables <- tables %>% modify_footnote_header(stat_types, columns = contains("stat_label"), 
        replace = F)
    return(tables)
}
