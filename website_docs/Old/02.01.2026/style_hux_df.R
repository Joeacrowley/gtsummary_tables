style_hux_df <- 
function (df, align = 1) 
{
    temp_capt <- df[["table_styling"]]$caption %>% str_split_i(., 
        "\\>", 2) %>% str_split_i(., "\\<", 1) %>% trimws()
    ht <- as_hux_table(df)
    caption(ht) <- NA
    font(ht) <- "Arial"
    font_size(ht) <- 11
    background_color(ht) <- "white"
    header_row_num <- ht %>% header_rows() %>% sum
    ht <- set_bold(ht, 1:header_row_num, everywhere, TRUE)
    ht <- set_background_color(ht, 1, everywhere, "#8f99de")
    if (header_row_num > 1) {
        ht <- set_background_color(ht, 2, everywhere, "#abb5e5")
    }
    if (header_row_num > 2) {
        ht <- set_background_color(ht, 3, everywhere, "#c7cced")
    }
    top_border(ht) <- 1.5
    bottom_border(ht) <- 1.5
    left_border(ht) <- 1.5
    right_border(ht) <- 1.5
    top_border_color(ht) <- "#8f99de"
    bottom_border_color(ht) <- "#8f99de"
    left_border_color(ht) <- "#8f99de"
    right_border_color(ht) <- "#8f99de"
    align_vec <- ifelse(seq_along(df) <= align, "left", "right")
    for (col in seq_along(align_vec)) {
        ht <- set_align(ht, everywhere, col, align_vec[col])
    }
    na_string(ht) <- ""
    set_number_format(ht, everywhere, value = fmt_pretty(digits = 1))
    empty_cols <- matrix("", nrow = 1, ncol = ncol(ht))
    empty_hux <- as_hux(empty_cols)
    empty_hux[1, 1] <- temp_capt
    empty_hux <- empty_hux %>% merge_across(1, everywhere)
    ht <- rbind(empty_hux, ht)
    set_font(ht, 1, everywhere, "Arial")
    set_font_size(ht, 1, everywhere, 13)
    set_bold(ht, 1, everywhere, TRUE)
    set_align(ht, 1, everywhere, "left")
    return(ht)
}
