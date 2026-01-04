nest_multicodes <- 
function (df, vars, ci = FALSE, caption = NULL, breaks = NULL, 
    sig = F, nest = NULL, bases = NULL, source = NULL, footnotes = NULL) 
{
    if (ci == F & sig == F) {
        appendages <- function(x) {
            x
        }
    }
    else if (ci == F & sig == T) {
        appendages <- function(x) {
            x %>% add_p(include = !all_of("unweighted_base"))
        }
    }
    else if (ci == T & sig == T) {
        appendages <- function(x) {
            x %>% add_p(include = !all_of("unweighted_base")) %>% 
                add_ci(include = -unweighted_base, statistic = list(all_categorical() ~ 
                  "[{conf.low} - {conf.high}]"), style_fun = all_categorical() ~ 
                  label_style_sigfig(scale = 100, digits = 1))
        }
    }
    else if (ci == T & sig == F) {
        appendages <- function(x) {
            x %>% add_ci(include = -unweighted_base, statistic = list(all_categorical() ~ 
                "[{conf.low} - {conf.high}]"), style_fun = all_categorical() ~ 
                label_style_sigfig(scale = 100, digits = 1))
        }
    }
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
                1)) %>% modify_header(all_stat_cols() ~ "**Total**")
        if (ci == TRUE) {
            tbl <- tbl %>% add_ci(include = -unweighted_base, 
                statistic = list(all_categorical() ~ "[{conf.low} - {conf.high}]"), 
                style_fun = all_categorical() ~ label_style_sigfig(scale = 100, 
                  digits = 1))
        }
        suppressWarnings({
            suppressMessages({
                brk_tbl <- df2 %>% filter(!is.na(.data[[breaks]])) %>% 
                  summary_fun(include = all_of(var_set), by = all_of(breaks), 
                    type = list(unweighted_base ~ "categorical"), 
                    missing = "no", statistic = list(all_dichotomous() ~ 
                      "{p}", all_categorical() ~ "{p}", unweighted_base ~ 
                      n_statistic), digits = list(all_dichotomous() ~ 
                      1))
                if (sig == TRUE) {
                  brk_tbl <- brk_tbl %>% add_p(include = !all_of("unweighted_base"))
                }
                if (ci == TRUE) {
                  brk_tbl <- brk_tbl %>% add_ci(include = -unweighted_base, 
                    statistic = list(all_categorical() ~ "[{conf.low} - {conf.high}]"), 
                    style_fun = all_categorical() ~ label_style_sigfig(scale = 100, 
                      digits = 1))
                }
                brk_tbl <- brk_tbl %>% modify_spanning_header(all_stat_cols() ~ 
                  paste0("**", var_label(dataset_reference[breaks]), 
                    " (overall)**"), starts_with("ci_stat") | 
                  contains("p.value") ~ paste0("**", var_label(dataset_reference[breaks]), 
                  " (overall)**"), )
                nest_tbl <- df2 %>% filter(!is.na(.data[[breaks]]) & 
                  !is.na(.data[[nest]])) %>% tbl_strata(strata = {
                  {
                    nest
                  }
                }, ~.x %>% summary_fun(include = all_of(var_set), 
                  by = all_of(breaks), type = list(unweighted_base ~ 
                    "categorical"), missing = "no", statistic = list(all_dichotomous() ~ 
                    "{p}", all_categorical() ~ "{p}", unweighted_base ~ 
                    n_statistic), digits = list(all_dichotomous() ~ 
                    1)) %>% appendages(), .header = "**{strata}**")
            })
        })
        tbl <- list(tbl, brk_tbl, nest_tbl) %>% tbl_merge(tab_spanner = FALSE)
        if (is.null(caption)) {
            caption <- paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
                "Crosstabulation by ", var_label(dataset_reference[[breaks]]), 
                ", nested by ", var_label(dataset_reference[[nest]]), 
                ".", "</div>")
        }
        else {
            caption <- paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
                caption, "</div>", "<div style='text-align: left; font-style: italic; color: black'>", 
                "<br>Crosstabulation by ", var_label(dataset_reference[[breaks]]), 
                ", nested by ", var_label(dataset_reference[[nest]]), 
                ".</div>")
        }
        tbl <- tbl %>% modify_header(all_stat_cols() ~ "**{level}**", 
            label ~ paste0("**", variable_label_stem, "**"), 
            ) %>% modify_caption(caption)
        if (whether_survey_data == T) {
            stat_types <- "All statistics shown in this table are weighted."
        }
        else {
            stat_types <- "All statistics shown in this table are unweighted."
        }
        tbl <- tbl %>% modify_footnote(all_stat_cols() ~ NA) %>% 
            modify_footnote_header(columns = "label", stat_types, 
                replace = F)
        bases_variables <- c("multicode_labels", breaks, nest)
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
