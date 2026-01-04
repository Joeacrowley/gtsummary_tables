.col_width <- 
function (huxt) 
{
    no_of_header_rows <- huxt %>% header_rows %>% sum
    header_row <- no_of_header_rows
    header_row_char_length <- huxt[header_row, ] %>% as_vector() %>% 
        map_int(., str_length)
    header_row_char_length[1] <- huxt[header_row + 1, 1] %>% 
        as_vector() %>% map_int(., str_length)
    header_proposed_width <- header_row_char_length + 3
    header_proposed_width_cut_off <- ifelse(header_proposed_width > 
        30, 30, header_proposed_width)
    header_proposed_width_cut_off <- ifelse(header_proposed_width_cut_off < 
        10, 10, header_proposed_width_cut_off)
    if (header_proposed_width[1] > 30) {
        header_proposed_width_cut_off[1] <- ifelse(header_proposed_width[1] > 
            50, 50, header_proposed_width[1])
    }
    if (header_proposed_width[1] < 20) {
        header_proposed_width_cut_off[1] <- ifelse(header_proposed_width_cut_off[1] < 
            20, 20, header_proposed_width_cut_off[1])
    }
    return(header_proposed_width_cut_off)
}
