svy_nums2_ncbreak <- 
function (outcomes, predictor, data, nvar, caption = NULL) 
{
    suppressWarnings({
        suppressMessages({
            table <- data %>% filter(!is.na(.data[[nvar]])) %>% 
                tbl_strata(strata = {
                  {
                    nvar
                  }
                }, ~.x %>% tbl_svysummary(type = list(all_continuous() ~ 
                  "continuous2", all_categorical() ~ "continuous2"), 
                  include = all_of(outcomes), by = all_of(predictor), 
                  missing = "no", statistic = list(all_continuous() ~ 
                    c("{mean}", "{median}", "{sd}", "{min} - {max}", 
                      "{N_nonmiss_unweighted}")), digits = list(all_continuous() ~ 
                    c(1, 1, 1, 1, 1, 0))) %>% add_overall(last = T) %>% 
                  add_p() %>% bold_labels() %>% modify_header(all_stat_cols() ~ 
                  "**{level}**", label ~ "") %>% modify_table_body(~.x %>% 
                  dplyr::mutate(label = case_when(label == "N not Missing (unweighted)" ~ 
                    "N (unweighted)", TRUE ~ label))), .header = "**{strata}**, N = {n_unweighted}")
            if (is.null(caption)) {
                o_lab <- ""
                if (length(outcomes) == 1) {
                  o_lab <- paste0("of ", var_label(svy_df[["variables"]][[outcomes]], 
                    null_action = "fill"), " ")
                }
                capt <- paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
                  "Crosstabulation ", o_lab, "by ", var_label(svy_df[["variables"]][[predictor]], 
                    null_action = "fill"), ", nested by ", var_label(svy_df[["variables"]][[nvar]], 
                    null_action = "fill"), ".</div>")
            }
            else {
                capt <- caption
            }
            table <- table %>% modify_caption(capt)
        })
    })
    return(table)
}
