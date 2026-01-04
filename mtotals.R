mtotals <- 
function (outcomes, data, caption = NULL, bases = NULL, source = NULL, 
    footnotes = NULL) 
{
    whether_survey_data <- if (class(data) %>% grep("survey.design", 
        ., value = T) %>% length() > 0) {
        T
    }
    else {
        F
    }
    if (whether_survey_data == T) {
        n_statistic <- "{n_unweighted}"
    }
    else {
        n_statistic <- "{n}"
    }
    result <- map(outcomes, function(outcome) {
        data2 <- data %>% mutate(across(all_of(outcome), ~fct_drop(.x), 
            .names = "the_merging_variable"), across(all_of(outcome), 
            ~case_when(!is.na(.x) ~ "Unweighted base"), .names = "base"))
        if (whether_survey_data == T) {
            var_label(data2[["variables"]]["base"]) <- ""
            outcome_variable_label <- data[["variables"]] %>% 
                pull(outcome) %>% var_label() %>% as.vector()
        }
        else {
            var_label(data2["base"]) <- ""
            outcome_variable_label <- data %>% pull(outcome) %>% 
                var_label() %>% as.vector()
        }
        if (whether_survey_data == T) {
            summary_fun <- tbl_svysummary
        }
        else {
            summary_fun <- tbl_summary
        }
        result <- summary_fun(data = data2, include = c(the_merging_variable, 
            base), statistic = list(all_categorical() ~ "{p}", 
            base ~ n_statistic), label = list(the_merging_variable ~ 
            " "), missing = "no", digits = everything() ~ 0) %>% 
            modify_header(stat_0 = paste0("**", outcome_variable_label, 
                "**"), label = "") %>% modify_footnote(everything() ~ 
            NA)
        return(result)
    })
    result <- tbl_merge(result, tab_spanner = F)
    result <- result %>% modify_table_body(~.x %>% dplyr::mutate(row_id = row_number()) %>% 
        dplyr::mutate(across(starts_with("stat_"), ~case_when(row_id == 
            1 ~ "%", TRUE ~ .x))) %>% dplyr::select(-row_id))
    if (!is.null(caption)) {
        result <- result %>% modify_caption(paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
            caption, "</div>"))
    }
    footnotes <- gtsummary_table_notes(bases_info = bases, vars = outcomes, 
        source_note = source, other_footnotes = footnotes)
    if (whether_survey_data == T) {
        stat_types <- "All statistics shown in this table are weighted."
    }
    else {
        stat_types <- "All statistics shown in this table are unweighted."
    }
    footnotes <- paste0(stat_types, "<br>", footnotes)
    if (footnotes != "") {
        result <- result %>% modify_source_note(source_note = footnotes, 
            text_interpret = "html")
    }
    return(result)
}
