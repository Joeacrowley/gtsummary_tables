nested_tables <- 
function (data, variables_int, crossbreak = NULL, nest = NULL, 
    stats_cat_int = "{p}", stats_num_int = "{mean}", ci_int = NULL, 
    num_digits_int = 2, split_num_int = F, sig_int = NULL, caption = NULL, 
    bases = NULL, source = NULL, footnotes = NULL) 
{
    add_conf <- ifelse(is.null(ci_int), F, T)
    add_p <- ifelse(is.null(sig_int), F, T)
    if (add_conf == F & add_p == F) {
        appendages <- function(x) {
            x %>% add_stat_label(location = "column")
        }
    }
    else if (add_conf == F & add_p == T) {
        appendages <- function(x) {
            x %>% add_stat_label(location = "column") %>% add_p()
        }
    }
    else if (add_conf == T & add_p == T) {
        appendages <- function(x) {
            x %>% add_stat_label(location = "column") %>% add_p() %>% 
                add_ci(statistic = list(all_categorical() ~ "{conf.low} - {conf.high}", 
                  all_continuous() ~ "{conf.low} - {conf.high}"), 
                  style_fun = list(all_categorical() ~ label_style_percent(digits = 0), 
                    all_continuous() ~ label_style_sigfig(scale = 1, 
                      digits = 2)))
        }
    }
    else if (add_conf == T & add_p == F) {
        appendages <- function(x) {
            x %>% add_stat_label(location = "column") %>% add_ci(statistic = list(all_categorical() ~ 
                "{conf.low} - {conf.high}", all_continuous() ~ 
                "{conf.low} - {conf.high}"), style_fun = list(all_categorical() ~ 
                label_style_percent(digits = 0), all_continuous() ~ 
                label_style_sigfig(scale = 1, digits = 2)))
        }
    }
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
    if (whether_survey_data == F) {
        for (i in 1:length(variables_int)) {
            base_name <- paste0("base_size", i)
            o_lab <- paste0(var_label(data[variables_int[i]], 
                null_action = "fill"))
            data <- data %>% mutate(`:=`(!!base_name, case_when(is.na(across(all_of(c(crossbreak)))) ~ 
                NA, is.na(across(all_of(c(nest)))) ~ NA, is.na(across(all_of(variables_int[i]))) ~ 
                NA, TRUE ~ o_lab)), across(all_of(crossbreak), 
                ~fct_drop(.x)))
            var_label(data[[base_name]]) <- ""
        }
        var_label(data[["base_size1"]]) <- "Unweighted sample sizes"
    }
    else if (whether_survey_data == T) {
        for (i in 1:length(variables_int)) {
            base_name <- paste0("base_size", i)
            o_lab <- paste0(var_label(data[["variables"]][variables_int[i]], 
                null_action = "fill"))
            data <- data %>% mutate(`:=`(!!base_name, case_when(is.na(across(all_of(c(crossbreak)))) ~ 
                NA, is.na(across(all_of(c(nest)))) ~ NA, is.na(across(all_of(variables_int[i]))) ~ 
                NA, TRUE ~ o_lab)), across(all_of(crossbreak), 
                ~fct_drop(.x)))
            var_label(data[["variables"]][[base_name]]) <- ""
        }
        var_label(data[["variables"]][["base_size1"]]) <- "Unweighted sample sizes"
    }
    base_names <- paste0("base_size", 1:length(variables_int))
    crossbreak_var <- rlang::sym(crossbreak)
    final_data <- data %>% filter(!is.na(.data[[crossbreak]]) & 
        !is.na(.data[[nest]]))
    if (whether_survey_data == T) {
        temp_data <- data[["variables"]]
    }
    else {
        temp_data <- data
    }
    if (whether_survey_data == T) {
        summary_fun <- tbl_svysummary
    }
    else {
        summary_fun <- tbl_summary
    }
    table <- final_data %>% tbl_strata(strata = {
        {
            nest
        }
    }, ~.x %>% summary_fun(type = all_continuous() ~ num_type, 
        include = all_of(variables_int), by = crossbreak_var, 
        missing = "no", statistic = list(all_categorical() ~ 
            stats_cat_int, all_continuous() ~ stats_num_int2), 
        digits = list(all_categorical() ~ 1, all_continuous() ~ 
            num_digits_int)) %>% modify_header(all_stat_cols() ~ 
        "**{level}**", label ~ "**Variable**") %>% bold_labels() %>% 
        add_overall(last = F) %>% modify_header(stat_0 ~ "**Total**") %>% 
        appendages(), .header = "**{strata}**")
    if (whether_survey_data == T) {
        n_statistic <- "{n_unweighted}"
    }
    else {
        n_statistic <- "{n}"
    }
    base_of_table <- final_data %>% tbl_strata(strata = {
        {
            nest
        }
    }, ~.x %>% summary_fun(include = all_of(base_names), by = crossbreak_var, 
        missing = "no", statistic = list(all_of(base_names) ~ 
            n_statistic), digits = list(all_categorical() ~ 0)) %>% 
        add_overall(last = F) %>% modify_header(all_stat_cols() ~ 
        "**{level}**", label ~ "**Variable**", stat_0 ~ "**Total**") %>% 
        bold_labels(), .header = "**{strata}**")
    table <- list(table, base_of_table) %>% tbl_stack()
    stat_label_names <- table[["table_body"]] %>% names %>% grep("stat_label", 
        ., value = T) %>% .[-1]
    table <- table %>% modify_table_body(~.x %>% dplyr::select(!all_of(stat_label_names)))
    if (whether_survey_data == T) {
        for (x in c(variables_int, base_names)) {
            table <- table %>% modify_table_body(~.x %>% dplyr::mutate(label = case_when(label == 
                x ~ var_label(temp_data[x]) %>% unlist, TRUE ~ 
                label)))
        }
    }
    base_break_row <- which(table[["table_body"]] %>% pull(label) == 
        "Unweighted sample sizes")
    table <- table %>% modify_table_body(~.x %>% filter(!(grepl("base_size([2-9]|[1-9][0-9]|100)", 
        variable, ignore.case = T) & label == "")) %>% add_row(.before = base_break_row))
    variables <- c(variables_int, crossbreak, nest)
    footnotes <- gtsummary_table_notes(bases_info = bases, vars = variables_int, 
        source_note = source, other_footnotes = footnotes)
    if (footnotes != "") {
        table <- table %>% modify_source_note(source_note = footnotes, 
            text_interpret = "html")
    }
    if (is.null(caption)) {
        caption <- paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
            "Crosstabulation by ", var_label(temp_data[[crossbreak]]), 
            ", nested by ", var_label(temp_data[[nest]]), ".", 
            "</div>")
    }
    else {
        caption <- paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
            caption, "</div>", "<div style='text-align: left; font-style: italic; color: black'>", 
            "<br>Crosstabulation by ", var_label(temp_data[[crossbreak]]), 
            ", nested by ", var_label(temp_data[[nest]]), ".</div>")
    }
    table <- table %>% modify_caption(caption)
    if (class(data) %>% grep("survey.design", ., value = T) %>% 
        length() > 0) {
        stat_types <- "All statistics shown in this table are weighted."
    }
    else {
        stat_types <- "All statistics shown in this table are unweighted."
    }
    table <- table %>% modify_footnote_header(stat_types, columns = contains("stat_label"), 
        replace = F)
    return(table)
}
