gtsummary_table_notes <- 
function (bases_info = NULL, vars, filter_description = NULL, 
    source_note = NULL, other_footnotes = NULL) 
{
    if (!is.null(bases_info)) {
        base_notes <- create_bases(base_info = bases_info, variables = vars) %>% 
            prepare_base_for_table()
    }
    else {
        base_notes <- ""
    }
    if (!is.null(filter_description)) {
        filter_description <- paste0(filter_description, collapse = "<br>")
        base_notes <- paste0(base_notes, "<br><br><b>Filter:</b> ", 
            filter_description)
    }
    if (!is.null(source_note)) {
        source_note <- paste0(source_note, collapse = "<br><br>")
        base_notes <- paste0(base_notes, "<br><br><b>Source:</b> ", 
            source_note)
    }
    if (!is.null(other_footnotes)) {
        other_footnotes <- paste0(other_footnotes, collapse = "<br><br>")
        base_notes <- paste0(base_notes, "<br><br><b>Footnotes:</b><br>", 
            other_footnotes)
    }
    return(base_notes)
}
