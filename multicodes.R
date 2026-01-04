multicodes <- 
function (df, vars, ci = FALSE, caption = NULL, breaks = NULL, 
    sig = F, bases = NULL, source = NULL, footnotes = NULL) 
{
    whether_survey_data <- if (class(df) %>% grep("survey.design", 
        ., value = T) %>% length() > 0) {
        T
    }
    else {
        F
    }
    dataset_reference <- if (whether_survey_data == F) {
        df
    }
    else {
        df[["variables"]]
    }
    chk1 <- splitter_present(dataset_reference, vars)
    chk2 <- bases_equal(dataset_reference, vars)
    if (chk1 == TRUE & chk2 == TRUE) {
        dataset_reference <- dataset_reference %>% mutate(unweighted_base = case_when(!is.na(df %>% 
            pull(vars[1])) ~ "Unweighted bases", TRUE ~ NA))
        var_label(dataset_reference$unweighted_base) <- ""
        var_set <- c(vars, "unweighted_base")
        variable_label_stem <- dataset_reference %>% pull(vars[1]) %>% 
            var_label() %>% str_split_i(., ": ", 1)
        dataset_reference <- revise_labels(dataset_reference, 
            vars = vars)
        if (whether_survey_data == T) {
            df2 <- df
            df2[["variables"]] <- dataset_reference
        }
        else {
            df2 <- dataset_reference
        }
        if (whether_survey_data == T) {
            summary_fun <- tbl_svysummary
        }
        else {
            summary_fun <- tbl_summary
        }
        if (whether_survey_data == T) {
            n_statistic <- "{n_unweighted}"
        }
        else {
            n_statistic <- "{n}"
        }
        tbl <- df2 %>% summary_fun(include = all_of(var_set), 
            type = list(unweighted_base ~ "categorical"), missing = "no", 
            statistic = list(all_dichotomous() ~ "{p}", all_categorical() ~ 
                "{p}", unweighted_base ~ n_statistic), digits = list(all_dichotomous() ~ 
                1))
        if (ci == TRUE) {
            tbl <- tbl %>% add_ci(include = -unweighted_base, 
                statistic = list(all_categorical() ~ "[{conf.low} - {conf.high}]"), 
                style_fun = all_categorical() ~ label_style_sigfig(scale = 100, 
                  digits = 1))
        }
        if (!is.null(breaks)) {
            suppressWarnings({
                suppressMessages({
                  brk_tbls <- map(breaks, ~df2 %>% filter(!is.na(.data[[.x]])) %>% 
                    summary_fun(include = all_of(var_set), by = all_of(.x), 
                      type = list(unweighted_base ~ "categorical"), 
                      missing = "no", statistic = list(all_dichotomous() ~ 
                        "{p}", all_categorical() ~ "{p}", unweighted_base ~ 
                        n_statistic), digits = list(all_dichotomous() ~ 
                        1)))
                  if (sig == TRUE) {
                    brk_tbls <- map(brk_tbls, ~.x %>% add_p(include = !all_of("unweighted_base")))
                  }
                  if (ci == TRUE) {
                    brk_tbls <- map(brk_tbls, ~.x %>% add_ci(include = -unweighted_base, 
                      statistic = list(all_categorical() ~ "[{conf.low} - {conf.high}]"), 
                      style_fun = all_categorical() ~ label_style_sigfig(scale = 100, 
                        digits = 1)))
                  }
                  brk_tbls <- map2(brk_tbls, breaks, ~.x %>% 
                    modify_spanning_header(all_stat_cols() ~ 
                      paste0("**", var_label(dataset_reference[[.y]]), 
                        "**"), starts_with("ci_stat") | contains("p.value") ~ 
                      paste0("**", var_label(dataset_reference[[.y]]), 
                        "**"), ))
                  tbl <- append(list(tbl), brk_tbls) %>% tbl_merge(tab_spanner = FALSE)
                })
            })
        }
        if (!is.null(breaks) & is.null(caption)) {
            caption <- paste0("Crosstabulation of ", variable_label_stem)
        }
        else if (is.null(caption)) {
            caption <- ""
        }
        tbl <- tbl %>% modify_header(all_stat_cols() ~ "**{level}**", 
            label ~ paste0("**", variable_label_stem, "**"), 
            ) %>% modify_caption(paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
            caption, "</div>"))
        if (whether_survey_data == T) {
            stat_types <- "All statistics shown in this table are weighted."
        }
        else {
            stat_types <- "All statistics shown in this table are unweighted."
        }
        tbl <- tbl %>% modify_footnote(all_stat_cols() ~ NA) %>% 
            modify_footnote_header(columns = "label", stat_types, 
                replace = F)
        if (is.null(breaks)) {
            tbl <- tbl %>% modify_header(starts_with("stat_0") ~ 
                "**%**")
        }
        if (!is.null(breaks)) {
            bases_variables <- c("multicode_labels", breaks)
        }
        if (!is.null(bases)) {
            description_of_multicode_variables <- bases[[2]] %>% 
                unique
            names(description_of_multicode_variables) <- "multicode_labels"
            label_of_multicode_variables <- variable_label_stem
            names(label_of_multicode_variables) <- "multicode_labels"
            bases[[2]] <- c(bases[[2]], description_of_multicode_variables)
            bases[[3]] <- c(bases[[3]], label_of_multicode_variables)
            bases[[4]] <- c(bases[[4]], label_of_multicode_variables)
        }
        footnotes <- gtsummary_table_notes(bases_info = bases, 
            vars = bases_variables, source_note = source, other_footnotes = footnotes)
        if (footnotes != "") {
            tbl <- tbl %>% modify_source_note(source_note = footnotes, 
                text_interpret = "html")
        }
        return(tbl)
    }
    else {
        helpful_error_message <- paste0("Not working: chk1 = ", 
            chk1, ", chk2 = ", chk2)
        return(helpful_error_message)
    }
}
