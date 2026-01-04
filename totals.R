totals <- 
function (data, variables_int, crossbreak = NULL, stats_cat_int = "{p}", 
    stats_num_int = "{mean}", ci_int = NULL, num_digits_int = 2, 
    split_num_int = F, sig_int = NULL) 
{
    whether_survey_data <- if (class(data) %>% grep("survey.design", 
        ., value = T) %>% length() > 0) {
        T
    }
    else {
        F
    }
    if (is.null(ci_int)) {
        num_type <- ifelse(split_num_int == T, "continuous2", 
            "continuous")
        stats_num_int2 <- if (split_num_int == T) {
            stats_num_int
        }
        else {
            paste0(stats_num_int, collapse = " ")
        }
    }
    else {
        num_type <- "continuous"
        stats_num_int2 <- paste0(stats_num_int, collapse = " ")
    }
    if (!is.null(crossbreak)) {
        if (whether_survey_data == T) {
            temp_data <- data[["variables"]]
        }
        else {
            temp_data <- data
        }
        for (i in 1:length(variables_int)) {
            base_name <- paste0("base_size", i)
            o_lab <- paste0(var_label(temp_data[variables_int[i]], 
                null_action = "fill"))
            temp_data <- temp_data %>% mutate(`:=`(!!base_name, 
                case_when(is.na(across(all_of(crossbreak))) ~ 
                  NA, is.na(across(all_of(variables_int[i]))) ~ 
                  NA, TRUE ~ o_lab)), across(all_of(crossbreak), 
                ~fct_drop(.x)))
            var_label(temp_data[[base_name]]) <- ""
        }
        var_label(temp_data[["base_size1"]]) <- "Unweighted sample sizes"
        base_names <- paste0("base_size", 1:length(variables_int))
        crossbreak_var <- rlang::sym(crossbreak)
        if (whether_survey_data == T) {
            final_data <- data
            final_data[["variables"]] <- temp_data
        }
        else {
            final_data <- temp_data
        }
    }
    else {
        final_data <- data
        crossbreak_var <- NULL
    }
    if (whether_survey_data == T) {
        summary_fun <- tbl_svysummary
    }
    else {
        summary_fun <- tbl_summary
    }
    table <- final_data %>% summary_fun(type = all_continuous() ~ 
        num_type, include = all_of(variables_int), by = crossbreak_var, 
        missing = "no", statistic = list(all_categorical() ~ 
            stats_cat_int, all_continuous() ~ stats_num_int2), 
        digits = list(all_categorical() ~ 1, all_continuous() ~ 
            num_digits_int)) %>% modify_header(all_stat_cols() ~ 
        "**{level}**", label ~ "**Variable**") %>% bold_labels() %>% 
        add_stat_label(location = "column")
    if (is.null(crossbreak)) {
        if (whether_survey_data == T) {
            n_overall <- "{N_nonmiss_unweighted}"
        }
        else {
            n_overall <- "{N_nonmiss}"
        }
        table <- table %>% add_n(last = TRUE, statistic = n_overall)
    }
    else {
        table <- table %>% add_overall(last = F) %>% modify_header(stat_0 ~ 
            "**Total**")
    }
    if (!is.null(ci_int)) {
        table <- table %>% add_ci(statistic = list(all_categorical() ~ 
            "{conf.low} - {conf.high}", all_continuous() ~ "{conf.low} - {conf.high}"), 
            style_fun = list(all_categorical() ~ label_style_percent(digits = 0), 
                all_continuous() ~ label_style_sigfig(scale = 1, 
                  digits = 2)))
    }
    if (!is.null(crossbreak)) {
        if (whether_survey_data == T) {
            n_statistic <- "{n_unweighted}"
        }
        else {
            n_statistic <- "{n}"
        }
        base_of_table <- final_data %>% summary_fun(include = all_of(base_names), 
            by = crossbreak_var, missing = "no", statistic = list(all_of(base_names) ~ 
                n_statistic), digits = list(all_categorical() ~ 
                0)) %>% add_overall(last = F) %>% modify_header(all_stat_cols() ~ 
            "**{level}**", label ~ "**Variable**", stat_0 ~ "**Total**") %>% 
            bold_labels()
        if (!is.null(sig_int)) {
            table <- table %>% add_p()
        }
        table <- list(table, base_of_table) %>% tbl_stack()
        table <- table %>% modify_spanning_header(all_stat_cols() ~ 
            paste0("**", var_label(temp_data[crossbreak]), "**"), 
            starts_with("ci_stat") ~ paste0("**", var_label(temp_data[crossbreak]), 
                "**"), ends_with("_0") ~ NA)
        base_break_row <- which(table[["table_body"]] %>% pull(label) == 
            "Unweighted sample sizes")
        table <- table %>% modify_table_body(~.x %>% filter(!(grepl("base_size([2-9]|[1-9][0-9]|100)", 
            variable, ignore.case = T) & label == "")) %>% add_row(.before = base_break_row))
    }
    table <- table %>% modify_footnote(all_stat_cols() ~ NA)
    return(table)
}
