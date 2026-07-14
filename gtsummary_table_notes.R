
# Also need some capacity to add a source description, or several I suppose, ideally. 
gtsummary_table_notes <- function(
    bases_info = NULL, # The object with base descriptions (from base_information() function)
    vars, # Character vector of variable names
    filter_description = NULL, # Plain-English description of any table-level filter, e.g. "Men only"
    source_note = NULL, # A string describing the data source
    other_footnotes = NULL# Other character strings as footnotes, each on its own line. 
){
  
  if(!is.null(bases_info)){
    base_notes <- 
      create_bases(
        base_info = bases_info, variables = vars
      ) %>% 
      prepare_base_for_table() 
  } else {
    base_notes <- ""
  }
  
  if (!is.null(filter_description)) {
    filter_description <- paste0(filter_description, collapse = "<br>")
    base_notes <- paste0(base_notes, "<br><br><b>Filter:</b> ", filter_description)
  }
  
  if (!is.null(source_note)) {
    source_note <- paste0(source_note, collapse = "<br><br>")
    base_notes <- paste0(base_notes, "<br><br><b>Source:</b> ", source_note)
  }
  
  if(!is.null(other_footnotes)){
    other_footnotes <- paste0(other_footnotes, collapse = "<br><br>")
    base_notes <- paste0(base_notes, "<br><br><b>Footnotes:</b><br>", other_footnotes)
  }
  
  return(base_notes)
  
}