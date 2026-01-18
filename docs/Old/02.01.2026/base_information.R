base_information <- 
function (data, general_base, specific_bases) 
{
    everything_else <- general_base
    var_descriptions <- specific_bases
    whether_survey_data <- if (class(data) %>% grep("survey.design", 
        ., value = T) %>% length() > 0) {
        T
    }
    else {
        F
    }
    data2 <- if (whether_survey_data == T) {
        data[["variables"]]
    }
    else {
        data
    }
    variable_labels <- data2 %>% select(names(var_descriptions)) %>% 
        var_label(unlist = T, null_action = "fill")
    all_variable_labels <- data2 %>% var_label(unlist = T, null_action = "fill")
    base_descriptions <- list(everything_else, var_descriptions, 
        variable_labels, all_variable_labels)
    return(base_descriptions)
}
