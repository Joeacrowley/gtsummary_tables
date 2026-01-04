cbreaks <- 
function (data, outcomes, crossbreaks, ci = FALSE, bases = NULL, 
    source = NULL, footnotes = NULL) 
{
    tables_int <- map(crossbreaks, ~data %>% cbreak_interim(outcome = outcomes, 
        crossbreak = .x, ci = ci))
    if (length(tables_int) > 1) {
        for (i in 2:length(tables_int)) {
            tables_int[[i]] <- tables_int[[i]] %>% modify_table_body(~.x %>% 
                select(!contains("stat_0")) %>% select(!contains("stat_label")))
        }
    }
    variable_labels <- data %>% select(all_of(crossbreaks)) %>% 
        var_label(unlist = T) %>% paste0("**", ., "**")
    tables <- tbl_merge(tables_int, tab_spanner = variable_labels) %>% 
        modify_spanning_header(contains("stat_label") ~ NA, contains("stat_0") ~ 
            NA)
    footnotes <- gtsummary_table_notes(bases_info = bases, vars = c(outcomes, 
        crossbreaks), source_note = source, other_footnotes = footnotes)
    if (footnotes != "") {
        tables <- tables %>% modify_source_note(source_note = footnotes, 
            text_interpret = "html")
    }
    return(tables)
}
