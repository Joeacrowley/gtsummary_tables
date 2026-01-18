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
