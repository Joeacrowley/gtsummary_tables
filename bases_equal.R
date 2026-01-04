bases_equal <- 
function (df, vars) 
{
    unique_base_sizes <- df %>% select(all_of(vars)) %>% map_int(., 
        ~sum(is.na(.x))) %>% unique()
    unique_base_sizes
    result <- length(unique_base_sizes) == 1
    return(result)
}
