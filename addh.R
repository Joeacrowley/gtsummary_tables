addh <- 
function (ht, workbook, sheet) 
{
    workbook_interim <- as_Workbook(ht, Workbook = workbook, 
        start_row = 2, start_col = 2, sheet = sheet)
    end_row <- 1 + nrow(ht)
    end_col <- 1 + ncol(ht)
    showGridLines(workbook_interim, sheet, showGridLines = FALSE)
    removeRowHeights(workbook_interim, sheet, rows = 2:end_row)
    addStyle(workbook_interim, sheet, style = createStyle(wrapText = T), 
        cols = 2:end_col, rows = 2:end_row, gridExpand = TRUE, 
        stack = T)
    total_width <- sum(ht %>% .col_width())
    row_line_counts <- apply(as.matrix(ht), 1, function(row_cells) {
        max(sapply(row_cells, function(cell) {
            segments <- str_split(cell, "\n")[[1]]
            sum(ceiling(pmax(nchar(segments), 1)/total_width))
        }))
    })
    buffer_lines <- 0.3
    row_heights <- pmax((row_line_counts + buffer_lines) * 15, 
        15)
    setRowHeights(workbook_interim, sheet, rows = 2:end_row, 
        heights = row_heights)
    removeColWidths(workbook_interim, sheet, cols = 2:end_col)
    col_widths <- ht %>% .col_width()
    setColWidths(workbook_interim, sheet, cols = 2:end_col, widths = col_widths)
    setColWidths(workbook_interim, sheet, cols = 1, widths = 4)
    addStyle(workbook_interim, sheet, style = createStyle(wrapText = T, 
        fontName = "Arial", fontSize = 13, textDecoration = "bold"), 
        cols = 2:end_col, rows = 2, gridExpand = TRUE, stack = T)
    setRowHeights(workbook_interim, sheet, rows = 2, heights = 30)
    return(workbook_interim)
}
