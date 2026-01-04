revise_labels <- 
function (df, vars) 
{
    revised_labels_as_list <- map(vars, function(xxx) {
        variable_label <- df %>% select(all_of(xxx)) %>% var_label %>% 
            str_split_i(., ": ", 2)
        return(variable_label)
    })
    names(revised_labels_as_list) <- vars
    var_label(df) <- revised_labels_as_list
    return(df)
}
