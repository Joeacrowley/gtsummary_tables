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

    ht <- as_hux_table(df) %>% set_all_borders(0)
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
    ht <- set_bottom_border(ht, row = 1:(final_body_row-1), col = everywhere, 
        value = 1)
    ht <- set_left_border(ht, row = 1:final_body_row, col = everywhere, 
        value = 1)
    ht <- set_right_border(ht, row = 1:final_body_row, col = everywhere, 
        value = 1)
    ht <- set_top_border(ht, row = c(1, first_body_row), col = everywhere, 
        value = 2)
    ht <- set_bottom_border(ht, row = c(final_body_row, final_footer_row), 
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
    align_vec <- ifelse(1:ncol(ht) <= align, "left", "right")
    for (col in seq_along(align_vec)) {
        ht <- set_align(ht, everywhere, col, align_vec[col])
    }
    na_string(ht) <- ""
    
    if (length(footer_rows) > 0) {
      new_footer_blocks <- list()
      for (r in footer_rows) {
        raw_text <- ht[[r, 1]]
        if (str_detect(raw_text, "<br|<b>")) {
          footer_lines <- raw_text %>% str_split_1("(?i)<br\\s*/?>") %>% trimws()
          footer_lines <- footer_lines[footer_lines != ""]
          is_bold_line <- str_detect(footer_lines, "^<b>.*</b>$")
          footer_lines_clean <- footer_lines %>% str_replace_all("(?i)</?b>", "")
          n_lines <- length(footer_lines_clean)
          new_rows <- as_hux(matrix(footer_lines_clean, nrow = n_lines, ncol = ncol(ht)))
          for (rr in seq_len(n_lines)) new_rows <- merge_across(new_rows, rr, everywhere)
          for (rr in which(is_bold_line)) new_rows <- set_bold(new_rows, rr, everywhere, TRUE)
          new_footer_blocks[[length(new_footer_blocks) + 1]] <- new_rows
        } else {
          untouched_row <- merge_across(ht[r, ], 1, everywhere)
          new_footer_blocks[[length(new_footer_blocks) + 1]] <- untouched_row
        }
      }
      combined_footer <- do.call(rbind, new_footer_blocks)
      ht <- rbind(ht[1:(min(footer_rows) - 1), ], combined_footer)
    }
    
    caption_divs <- str_match_all(df[["table_styling"]]$caption, "<div[^>]*>(.*?)</div>")[[1]][, 2]
    caption_parts <- caption_divs %>% str_replace_all("<[^>]*>", " ") %>% str_squish()
    caption_parts <- caption_parts[caption_parts != ""]
    if (length(caption_parts) == 0) {
      caption_parts <- " "
    }
    
    if (!(length(caption_parts) == 1 && caption_parts[1] == " ")) {
      n_parts <- length(caption_parts)
      empty_cols <- matrix("", nrow = n_parts, ncol = ncol(ht))
      empty_hux <- as_hux(empty_cols)
      for (r in seq_len(n_parts)) {
        empty_hux[r, 1] <- caption_parts[r]
        empty_hux <- empty_hux %>% merge_across(r, everywhere)
      }
      empty_hux <- set_header_rows(empty_hux, seq_len(n_parts), TRUE)
      ht <- rbind(empty_hux, ht)
      ht <- set_font(ht, 1:n_parts, everywhere, "Arial")
      ht <- set_font_size(ht, 1:n_parts, everywhere, 13)
      ht <- set_bold(ht, 1, everywhere, TRUE)
      if (n_parts > 1) {
        ht <- set_italic(ht, 2:n_parts, everywhere, TRUE)
      }
      ht <- set_align(ht, 1:n_parts, everywhere, "left")
      ht <- set_bottom_border(ht, row = n_parts, col = everywhere, value = 2)
    }
    
    top_border_color(ht) <- "grey50"
    bottom_border_color(ht) <- "grey50"
    left_border_color(ht) <- "grey50"
    right_border_color(ht) <- "grey50"
    return(ht)
}
