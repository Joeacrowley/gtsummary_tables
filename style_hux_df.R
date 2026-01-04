style_hux_df <- 
function (df, align = 1) 
{
    whether_any_sample_sizes_below_table <- df[["table_body"]] %>% 
        pull(label) == "Unweighted sample sizes"
    starting_row_for_base_sizes <- which(whether_any_sample_sizes_below_table) - 
        1
    if (any(whether_any_sample_sizes_below_table)) {
        df <- df %>% modify_table_body(~.x %>% mutate(label = case_when(row_number() == 
            starting_row_for_base_sizes ~ " ", TRUE ~ label)))
    }
    temp_capt <- df[["table_styling"]]$caption %>% str_split_i(., 
        "\\>", 2) %>% str_split_i(., "\\<", 1) %>% trimws()
    if (length(temp_capt) == 0) {
        temp_capt <- " "
    }
    ht <- as_hux_table(df)
    caption(ht) <- NA
    font(ht) <- "Arial"
    font_size(ht) <- 11
    header_row_num <- ht %>% header_rows() %>% sum
    number_of_body_rows <- nrow(df[["table_body"]])
    first_body_row <- header_row_num + 1
    final_body_row <- header_row_num + number_of_body_rows
    first_footer_row <- final_body_row + 1
    final_footer_row <- nrow(ht)
    starting_row_for_base_sizes_ht <- starting_row_for_base_sizes + 
        header_row_num
    header_rows <- 1:header_row_num
    body_rows <- first_body_row:final_body_row
    footer_rows <- first_footer_row:final_footer_row
    ht <- map_background_color(ht, by_rows("#f5f7fa", "#ffffff")) %>% 
        set_background_color(row = 1:header_row_num, value = "grey90") %>% 
        set_background_color(row = footer_rows, value = "white")
    ht <- set_bold(ht, 1:header_row_num, everywhere, TRUE)
    ht <- set_top_border(ht, row = 1:final_body_row, col = everywhere, 
        value = 1)
    ht <- set_bottom_border(ht, row = 1:final_body_row, col = everywhere, 
        value = 1)
    ht <- set_left_border(ht, row = 1:final_body_row, col = everywhere, 
        value = 1)
    ht <- set_right_border(ht, row = 1:final_body_row, col = everywhere, 
        value = 1)
    ht <- set_top_border(ht, row = c(1, first_body_row), col = everywhere, 
        value = 2)
    ht <- set_bottom_border(ht, row = c(final_body_row, starting_row_for_base_sizes_ht), 
        col = everywhere, value = 2)
    if (any(whether_any_sample_sizes_below_table)) {
        ht <- set_top_padding(ht, row = starting_row_for_base_sizes + 
            header_row_num, value = 5)
        ht <- set_bottom_padding(ht, row = starting_row_for_base_sizes + 
            header_row_num, value = 5)
    }
    rows_to_pad <- which(df[["table_body"]]["row_type"] == "label") + 
        2
    ht <- set_top_padding(ht, row = rows_to_pad, value = 3)
    ht <- set_bottom_padding(ht, row = rows_to_pad, value = 3)
    align_vec <- ifelse(seq_along(df) <= align, "left", "right")
    for (col in seq_along(align_vec)) {
        ht <- set_align(ht, everywhere, col, align_vec[col])
    }
    na_string(ht) <- ""
    if (temp_capt != " ") {
        empty_cols <- matrix("", nrow = 1, ncol = ncol(ht))
        empty_hux <- as_hux(empty_cols)
        empty_hux[1, 1] <- temp_capt
        empty_hux <- empty_hux %>% merge_across(1, everywhere)
        empty_hux <- set_header_rows(empty_hux, 1, T)
        ht <- rbind(empty_hux, ht)
        ht <- set_font(ht, 1, everywhere, "Arial")
        ht <- set_font_size(ht, 1, everywhere, 13)
        ht <- set_bold(ht, 1, everywhere, TRUE)
        ht <- set_align(ht, 1, everywhere, "left")
        ht <- set_bottom_border(ht, row = 1, col = everywhere, 
            value = 2)
    }
    top_border_color(ht) <- "grey50"
    bottom_border_color(ht) <- "grey50"
    left_border_color(ht) <- "grey50"
    right_border_color(ht) <- "grey50"
    return(ht)
}
