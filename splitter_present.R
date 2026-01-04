splitter_present <- 
function (df, vars) 
{
    labels_contain_splitter <- df %>% select(all_of(vars)) %>% 
        var_label(unlist = T, null_action = "fill") %>% map_int(., 
        ~str_count(.x, ": "))
    labels_contain_splitter
    all_contain_splitter <- length(labels_contain_splitter) == 
        sum(labels_contain_splitter)
    splitter_appears_once_each <- max(labels_contain_splitter) == 
        1
    result <- all_contain_splitter == TRUE & splitter_appears_once_each == 
        TRUE
    return(result)
}
