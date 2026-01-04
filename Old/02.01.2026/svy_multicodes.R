svy_multicodes <- 
function (df, vars, ci = FALSE, caption = "Weighted percentages", 
    breaks = NULL, sig = FALSE) 
{
    chk1 <- splitter_present(df[["variables"]], vars)
    chk2 <- bases_equal(df[["variables"]], vars)
    if (chk1 == TRUE & chk2 == TRUE) {
        df <- df %>% mutate(unweighted_base = case_when(!is.na(df %>% 
            pull(vars[1])) ~ "Unweighted bases", TRUE ~ NA))
        var_label(df[["variables"]]$unweighted_base) <- ""
        var_set <- c(vars, "unweighted_base")
        variable_label_stem <- df[["variables"]] %>% pull(vars[1]) %>% 
            var_label() %>% str_split_i(., ": ", 1)
        df[["variables"]] <- revise_labels(df[["variables"]], 
            vars = vars)
        tbl <- df %>% tbl_svysummary(include = all_of(var_set), 
            type = list(unweighted_base ~ "categorical"), missing = "no", 
            statistic = list(all_dichotomous() ~ "{p}", all_categorical() ~ 
                "{p}", unweighted_base ~ "{n_unweighted}"), digits = list(all_dichotomous() ~ 
                1))
        if (ci == TRUE) {
            tbl <- tbl %>% add_ci(include = -unweighted_base, 
                statistic = list(all_categorical() ~ "[{conf.low} - {conf.high}]"), 
                style_fun = all_categorical() ~ label_style_sigfig(scale = 100, 
                  digits = 1))
        }
        if (!is.null(breaks)) {
            suppressWarnings({
                suppressMessages({
                  brk_tbls <- map(breaks, ~df %>% filter(!is.na(.data[[.x]])) %>% 
                    tbl_svysummary(include = all_of(var_set), 
                      by = all_of(.x), type = list(unweighted_base ~ 
                        "categorical"), missing = "no", statistic = list(all_dichotomous() ~ 
                        "{p}", all_categorical() ~ "{p}", unweighted_base ~ 
                        "{n_unweighted}"), digits = list(all_dichotomous() ~ 
                        1)))
                  if (sig == TRUE) {
                    brk_tbls <- map(brk_tbls, ~.x %>% add_p(include = !all_of("unweighted_base")))
                  }
                  if (ci == TRUE) {
                    brk_tbls <- map(brk_tbls, ~.x %>% add_ci(include = -unweighted_base, 
                      statistic = list(all_categorical() ~ "[{conf.low} - {conf.high}]"), 
                      style_fun = all_categorical() ~ label_style_sigfig(scale = 100, 
                        digits = 1)))
                  }
                  brk_tbls <- map2(brk_tbls, breaks, ~.x %>% 
                    modify_spanning_header(all_stat_cols() ~ 
                      paste0("**", var_label(df[["variables"]][[.y]]), 
                        "**"), starts_with("ci_stat") | contains("p.value") ~ 
                      paste0("**", var_label(df[["variables"]][[.y]]), 
                        "**"), ))
                  tbl <- append(list(tbl), brk_tbls) %>% tbl_merge(tab_spanner = FALSE)
                })
            })
        }
        if (!is.null(breaks) & caption == "Unweighted percentages") {
            caption <- paste0("Crosstabulation of ", variable_label_stem, 
                ". Unweighted percentages.")
        }
        tbl <- tbl %>% modify_header(all_stat_cols() ~ "**{level}**", 
            label ~ paste0("**", variable_label_stem, "**"), 
            ) %>% modify_caption(paste0("<div style='text-align: left; font-weight: bold; color: black'>", 
            caption, "</div>")) %>% modify_footnote(all_stat_cols() ~ 
            NA)
        if (is.null(breaks)) {
            tbl <- tbl %>% modify_header(starts_with("stat_0") ~ 
                "**%**")
        }
        return(tbl)
    }
    else {
        helpful_error_message <- paste0("Not working: chk1 = ", 
            chk1, ", chk2 = ", chk2)
        return(helpful_error_message)
    }
}
