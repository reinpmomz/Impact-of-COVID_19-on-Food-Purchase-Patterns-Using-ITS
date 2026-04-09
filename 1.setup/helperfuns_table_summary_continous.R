library(dplyr)
library(gtsummary)
library(flextable)
library(labelled)
library(sjlabelled)

### continous summary tables
continous_table <- 
  function(df, foot_note = NULL, caption = "", continous_var, sum=FALSE, display_mean = TRUE, p_value = TRUE,
           continous_digits = 1, include_vars = NULL, flex_table = TRUE, par_test = FALSE, var_equal = FALSE) {
    
  ### dividing factor/character/logical and numeric/integer variables
  factor_character_vars <- names(df[sapply(df, function(v) is.factor(v) | is.character(v) | is.logical(v) | is.Date(v))])
  
  df <- (df %>%
           dplyr::select(all_of(c(factor_character_vars, continous_var))
                  ) %>%
           dplyr::mutate(across(all_of(factor_character_vars), sjlabelled::as_factor))
         )
  
  df_new <- df %>%
    dplyr::select(-any_of(continous_var))
  
  ## divide factor_character_vars into two levels and more than two levels
  factor_character_vars_levels_2 <- factor_character_vars[sapply(lapply(df_new, unique), length) == 2]
  factor_character_vars_levels_3 <- factor_character_vars[sapply(lapply(df_new, unique), length) > 2]
  
  summ1 <- if (sum == FALSE) {
    summ <- if (display_mean == TRUE) {
    gtsummary::tbl_continuous(df
                              ,variable = any_of(continous_var)
                              ,include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                              ,statistic = everything() ~ c(
                                "{mean} ({sd})")
                              ,digits = list(everything() ~ continous_digits
                                             )
                              ) 
    } else {
      gtsummary::tbl_continuous(df
                                ,variable = any_of(continous_var)
                                ,include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                                ,statistic = everything() ~ c(
                                  "{mean} ({sd}) / {median} ({p25}, {p75})")
                                ,digits = list(everything() ~ continous_digits
                                               )
                                )
    }
    summ
  } else {
    summ <- if (display_mean == TRUE) {
      gtsummary::tbl_continuous(df
                                , variable = any_of(continous_var)
                                , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                                , statistic = everything() ~ c(
                                  "{sum} / {mean} ({sd})")
                                , digits = list(everything() ~ continous_digits
                                                )
                                )
    } else {
      gtsummary::tbl_continuous(df
                                , variable = any_of(continous_var)
                                , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                                , statistic = everything() ~ c(
                                  "{sum}")
                                , digits = list(everything() ~ continous_digits
                                                )
                                )
    }
    summ
    }
  
  summ2 <- if (p_value == TRUE) {
    if (par_test == TRUE) {
      summ1 %>%
      add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
            test = list(all_of(factor_character_vars_levels_2) ~ "t.test",
                        all_of(factor_character_vars_levels_3) ~ "oneway.test"
                        ),
            test.args = all_categorical() ~ list(var.equal = var_equal)
            ) %>%
      bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05) 
      modify_table_body(
        ~ .x %>%
          dplyr::relocate(c(statistic), .before = p.value)
        ) %>%
        modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                       ) %>% 
        modify_header(label = "**Variables**", statistic = "**Test statistic**"
                      # update the column header
                      )
      
    } else {
    summ1 %>%
      add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
      bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05) 
      modify_table_body(
        ~ .x %>%
          dplyr::relocate(c(statistic), .before = p.value)
        ) %>%
        modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                       ) %>% 
        modify_header(label = "**Variables**", statistic = "**Test statistic**"
                      # update the column header
                      )
    }
  } else { 
    summ1 %>% 
      modify_header(label = "**Variables**"# update the column header
                    ) 
    }
  
  summ3 <- summ2 %>% 
    bold_labels() %>%
    italicize_levels() %>%
    modify_caption(caption)
  
  summ4 <- if (is.null(foot_note)) {
    summ3
  } else {
    summ3 %>%
      modify_footnote(all_stat_cols() ~ foot_note)
  }
  
  summ5 <- if (flex_table == TRUE) { 
    summ4 %>%
      gtsummary::as_flex_table() 
    # as_kable_extra() covert gtsummary object to knitrkable object. 
    #as_flex_table() maintains identation, footnotes, spanning headers
  } else {
    summ4
  }
  
  summ5
  
}

### continous_by(Two way Anova) summary tables
continous_by_table <- 
  function(df, foot_note = NULL, caption = "", continous_var, by_vars, sum=FALSE, display_mean = TRUE,
           p_value = TRUE, continous_digits = 1, include_vars = NULL, flex_table = TRUE) {
    ### dividing factor/character/logical and numeric/integer variables
    factor_character_vars <- names(df[sapply(df, function(v) is.factor(v) | is.character(v) | is.logical(v) | is.Date(v))])
    
    df <- (df %>%
             dplyr::select(all_of(c(factor_character_vars, continous_var))
                    ) %>%
             dplyr::mutate(across(all_of(factor_character_vars), sjlabelled::as_factor))
           )
    
    out <- lapply(by_vars, function(x){
      summ1 <- if (sum == FALSE) {
        summ <- if (display_mean == TRUE) {
          gtsummary::tbl_continuous(df
                                    ,variable = any_of(continous_var)
                                    , by = any_of(x)
                                    ,include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                                    ,statistic = everything() ~ c(
                                      "{mean} ({sd})")
                                    ,digits = list(everything() ~ continous_digits
                                                   )
                                    ) 
          } else {
          gtsummary::tbl_continuous(df
                                    ,variable = any_of(continous_var)
                                    , by = any_of(x)
                                    ,include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                                    ,statistic = everything() ~ c(
                                      "{mean} ({sd}) / {median} ({p25}, {p75})")
                                    ,digits = list(everything() ~ continous_digits
                                                   )
                                    )
            }
        summ
        } else {
        summ <- if (display_mean == TRUE) {
          gtsummary::tbl_continuous(df
                                    , variable = any_of(continous_var)
                                    , by = any_of(x)
                                    , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                                    , statistic = everything() ~ c(
                                      "{sum} / {mean} ({sd})")
                                    , digits = list(everything() ~ continous_digits
                                                    )
                                    )  
          } else {
          gtsummary::tbl_continuous(df
                                    , variable = any_of(continous_var)
                                    , by = any_of(x)
                                    , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                                    , statistic = everything() ~ c(
                                      "{sum}")
                                    , digits = list(everything() ~ continous_digits
                                                    )
                                    ) 
            }
        summ
        }
    
      summ2 <- if (p_value == TRUE) {
        summ1 %>%
          add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
          bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05) 
          modify_table_body(
            ~ .x %>%
              dplyr::relocate(c(statistic), .before = p.value)
            ) %>%
          modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                         ) %>% 
          modify_header(label = "**Variables**", statistic = "**Test statistic**"
                        # update the column header
                        )
      } else { 
          summ1 %>% 
          modify_header(label = "**Variables**"# update the column header
                        ) 
        }
    
      summ3 <- summ2 %>%  
      bold_labels() %>%
      italicize_levels() %>%
      modify_caption(caption)
      
      summ4 <- if (is.null(foot_note)) {
        summ3
      } else {
        summ3 %>%
          modify_footnote(all_stat_cols() ~ foot_note)
      }
      
      summ5 <- if (flex_table == TRUE) { 
        summ4 %>%
          gtsummary::as_flex_table() 
        # as_kable_extra() covert gtsummary object to knitrkable object. 
        #as_flex_table() maintains identation, footnotes, spanning headers
      } else {
        summ4
      }
    
      summ5
      
    })
    out
    
  }

### continous strata summary tables
continous_strata_table <- 
  function(df, foot_note = "", caption = "", strata_var, continous_var, display_mean = TRUE, sum=FALSE, p_value = TRUE,
           continous_digits = 1, include_vars = NULL, flex_table = TRUE, par_test = FALSE, var_equal = FALSE) {
    
    ### dividing factor/character/logical and numeric/integer variables
    factor_all_character_vars <- names(df[sapply(df, function(v) is.factor(v) | is.character(v) | is.logical(v) | is.Date(v))])
    
    factor_character_vars <- factor_all_character_vars[!factor_all_character_vars %in% strata_var]
    
    df <- (df %>%
             dplyr::select(all_of(c(factor_all_character_vars, continous_var))
                    ) %>%
             dplyr::mutate(across(all_of(factor_all_character_vars), sjlabelled::as_factor))
           )
    
    index_strata <- df[[strata_var]]
    
    label_strata <- if (is.null(labelled::var_label(index_strata))) {x
    } else {
      labelled::var_label(index_strata)
    }
    
    df_new <- df %>%
      dplyr::select(-any_of(c(continous_var, strata_var)))
    
    ## divide factor_character_vars into two levels and more than two levels
    factor_character_vars_levels_2 <- factor_character_vars[sapply(lapply(df_new, unique), length) == 2]
    factor_character_vars_levels_3 <- factor_character_vars[sapply(lapply(df_new, unique), length) > 2]
    
    summ2 <- if (p_value == FALSE) {
      summ1 <- if (sum == FALSE) {
        summ <- if (display_mean == TRUE) {
        tbl_strata(df,
                 strata = any_of(strata_var),
                 .tbl_fun = ~ .x %>%
                   gtsummary::tbl_continuous(
                                 variable = any_of(continous_var)
                                , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                                , statistic = everything() ~ c(
                                  "{mean} ({sd})")
                                , digits = list(everything() ~ continous_digits
                                                )
                                ) %>% 
                   modify_header(label = "**Variables**" # update the column header
                   ) %>% 
                   bold_labels() %>%
                   italicize_levels() %>% 
                   modify_footnote(all_stat_cols() ~ foot_note) %>%
                   modify_caption(caption)
                 , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                 ) 
        } else {
          tbl_strata(df,
                     strata = any_of(strata_var),
                     .tbl_fun = ~ .x %>%
                       gtsummary::tbl_continuous(
                         variable = any_of(continous_var)
                         , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                         , statistic = everything() ~ c(
                           "{mean} ({sd}) / {median} ({p25}, {p75})")
                         , digits = list(everything() ~ continous_digits
                         )
                       ) %>% 
                       modify_header(label = "**Variables**" # update the column header
                       ) %>% 
                       bold_labels() %>%
                       italicize_levels() %>% 
                       modify_footnote(all_stat_cols() ~ foot_note) %>%
                       modify_caption(caption)
                     , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
          )
        }
        summ
        } else { 
          summ <- if (display_mean == TRUE) {
            tbl_strata(df,
                       strata = any_of(strata_var),
                       .tbl_fun = ~ .x %>%
                         gtsummary::tbl_continuous(
                           variable = any_of(continous_var)
                           , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                           , statistic = everything() ~ c(
                             "{sum} / {mean} ({sd})")
                           , digits = list(everything() ~ continous_digits
                           )
                         ) %>% 
                         modify_header(label = "**Variables**"# update the column header
                         ) %>% 
                         bold_labels() %>%
                         italicize_levels() %>% 
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption) 
                       , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                       ) 
          } else {
            tbl_strata(df,
                       strata = any_of(strata_var),
                       .tbl_fun = ~ .x %>%
                         gtsummary::tbl_continuous(
                           variable = any_of(continous_var)
                           , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                           , statistic = everything() ~ c(
                             "{sum}")
                           , digits = list(everything() ~ continous_digits
                           )
                         ) %>% 
                         modify_header(label = "**Variables**"# update the column header
                         ) %>% 
                         bold_labels() %>%
                         italicize_levels() %>% 
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption) 
                       , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                       )
          } 
          } 
      summ1
      
      } else {
        if (par_test == TRUE) {
          summ1 <- if (sum == FALSE) {
            summ <- if (display_mean == TRUE) {
              tbl_strata(df,
                         strata = any_of(strata_var),
                         .tbl_fun = ~ .x %>%
                           gtsummary::tbl_continuous(
                             variable = any_of(continous_var)
                             , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                             , statistic = everything() ~ c(
                               "{mean} ({sd})")
                             , digits = list(everything() ~ continous_digits
                             )
                           ) %>%
                           bold_labels() %>%
                           italicize_levels() %>% 
                           add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
                                 test = list(all_of(factor_character_vars_levels_2) ~ "t.test",
                                             all_of(factor_character_vars_levels_3) ~ "oneway.test"
                                             ),
                                 test.args = all_categorical() ~ list(var.equal = var_equal)
                                 ) %>%
                           bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
                           modify_table_body(
                             ~ .x %>%
                               dplyr::relocate(c(statistic), .before = p.value)
                             ) %>%
                           modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                                          ) %>% 
                           modify_header(label = "**Variables**", statistic = "**Test statistic**"
                                         # update the column header
                                         ) %>%
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption)
                         , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                         )
            } else {
              tbl_strata(df,
                         strata = any_of(strata_var),
                         .tbl_fun = ~ .x %>%
                           gtsummary::tbl_continuous(
                             variable = any_of(continous_var)
                             , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                             , statistic = everything() ~ c(
                               "{mean} ({sd}) / {median} ({p25}, {p75})")
                             , digits = list(everything() ~ continous_digits
                             )
                           ) %>%
                           bold_labels() %>%
                           italicize_levels() %>% 
                           add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
                                 test = list(all_of(factor_character_vars_levels_2) ~ "t.test",
                                             all_of(factor_character_vars_levels_3) ~ "oneway.test"
                                             ),
                                 test.args = all_categorical() ~ list(var.equal = var_equal)
                                 ) %>%
                           bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05) 
                           modify_table_body(
                             ~ .x %>%
                               dplyr::relocate(c(statistic), .before = p.value)
                             ) %>%
                           modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                                          ) %>% 
                           modify_header(label = "**Variables**", statistic = "**Test statistic**"
                                         # update the column header
                                         ) %>%
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption)
                         , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                         )
            }
            
            summ
          } else {
            summ <- if (display_mean == TRUE) {
              tbl_strata(df,
                         strata = any_of(strata_var),
                         .tbl_fun = ~ .x %>%
                           gtsummary::tbl_continuous(
                             variable = any_of(continous_var)
                             , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                             , statistic = everything() ~ c(
                               "{sum} / {mean} ({sd})")
                             , digits = list(everything() ~ continous_digits
                             )
                           ) %>% 
                           bold_labels() %>%
                           italicize_levels() %>%
                           add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
                                 test = list(all_of(factor_character_vars_levels_2) ~ "t.test",
                                             all_of(factor_character_vars_levels_3) ~ "oneway.test"
                                             ),
                                 test.args = all_categorical() ~ list(var.equal = var_equal)
                                 ) %>%
                           bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05) 
                           modify_table_body(
                             ~ .x %>%
                               dplyr::relocate(c(statistic), .before = p.value)
                             ) %>%
                           modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                                          ) %>% 
                           modify_header(label = "**Variables**", statistic = "**Test statistic**"
                                         # update the column header
                                         ) %>%
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption)
                         , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                         ) 
              } else {
                tbl_strata(df,
                           strata = any_of(strata_var),
                           .tbl_fun = ~ .x %>%
                             gtsummary::tbl_continuous(
                               variable = any_of(continous_var)
                               , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                               , statistic = everything() ~ c(
                                 "{sum}")
                               , digits = list(everything() ~ continous_digits
                               )
                             ) %>%
                             bold_labels() %>%
                             italicize_levels() %>%
                             add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
                                   test = list(all_of(factor_character_vars_levels_2) ~ "t.test",
                                               all_of(factor_character_vars_levels_3) ~ "oneway.test"
                                               ),
                                   test.args = all_categorical() ~ list(var.equal = var_equal)
                                   ) %>%
                             bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
                             modify_table_body(
                               ~ .x %>%
                                 dplyr::relocate(c(statistic), .before = p.value)
                               ) %>%
                             modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                                            ) %>% 
                             modify_header(label = "**Variables**", statistic = "**Test statistic**"
                                           # update the column header
                                           ) %>%
                             modify_footnote(all_stat_cols() ~ foot_note) %>%
                             modify_caption(caption)
                           , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                           ) 
              }
            summ
          }
          summ1
          
        } else {
          summ1 <- if (sum == FALSE) {
            summ <- if (display_mean == TRUE) {
              tbl_strata(df,
                         strata = any_of(strata_var),
                         .tbl_fun = ~ .x %>%
                           gtsummary::tbl_continuous(
                             variable = any_of(continous_var)
                             , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                             , statistic = everything() ~ c(
                               "{mean} ({sd})")
                             , digits = list(everything() ~ continous_digits
                             )
                           ) %>%
                           bold_labels() %>%
                           italicize_levels() %>% 
                           add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
                           bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
                           modify_table_body(
                             ~ .x %>%
                               dplyr::relocate(c(statistic), .before = p.value)
                             ) %>%
                           modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                                          ) %>% 
                           modify_header(label = "**Variables**", statistic = "**Test statistic**"
                                         # update the column header
                                         ) %>%
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption)
                         , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                         )
            } else {
              tbl_strata(df,
                         strata = any_of(strata_var),
                         .tbl_fun = ~ .x %>%
                           gtsummary::tbl_continuous(
                             variable = any_of(continous_var)
                             , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                             , statistic = everything() ~ c(
                               "{mean} ({sd}) / {median} ({p25}, {p75})")
                             , digits = list(everything() ~ continous_digits
                             )
                           ) %>%
                           bold_labels() %>%
                           italicize_levels() %>% 
                           add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
                           bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05) 
                           modify_table_body(
                             ~ .x %>%
                               dplyr::relocate(c(statistic), .before = p.value)
                             ) %>%
                           modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                                          ) %>% 
                           modify_header(label = "**Variables**", statistic = "**Test statistic**"
                                         # update the column header
                                         ) %>%
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption)
                         , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                         )
            }
            
            summ
          } else {
            summ <- if (display_mean == TRUE) {
              tbl_strata(df,
                         strata = any_of(strata_var),
                         .tbl_fun = ~ .x %>%
                           gtsummary::tbl_continuous(
                             variable = any_of(continous_var)
                             , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                             , statistic = everything() ~ c(
                               "{sum} / {mean} ({sd})")
                             , digits = list(everything() ~ continous_digits
                             )
                           ) %>% 
                           bold_labels() %>%
                           italicize_levels() %>%
                           add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
                           bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05) 
                           modify_table_body(
                             ~ .x %>%
                               dplyr::relocate(c(statistic), .before = p.value)
                             ) %>%
                           modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                                          ) %>% 
                           modify_header(label = "**Variables**", statistic = "**Test statistic**"
                                         # update the column header
                                         ) %>%
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption)
                         , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                         ) 
              } else {
                tbl_strata(df,
                           strata = any_of(strata_var),
                           .tbl_fun = ~ .x %>%
                             gtsummary::tbl_continuous(
                               variable = any_of(continous_var)
                               , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                               , statistic = everything() ~ c(
                                 "{sum}")
                               , digits = list(everything() ~ continous_digits
                               )
                             ) %>%
                             bold_labels() %>%
                             italicize_levels() %>%
                             add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
                             bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
                             modify_table_body(
                               ~ .x %>%
                                 dplyr::relocate(c(statistic), .before = p.value)
                               ) %>%
                             modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                                            ) %>% 
                             modify_header(label = "**Variables**", statistic = "**Test statistic**"
                                           # update the column header
                                           ) %>%
                             modify_footnote(all_stat_cols() ~ foot_note) %>%
                             modify_caption(caption)
                           , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                           ) 
              }
            summ
          }
          summ1
      }
      
    }
    
    summ3 <- if (flex_table == TRUE) { 
      summ2 %>%
        gtsummary::as_flex_table() 
      # as_kable_extra() covert gtsummary object to knitrkable object. 
      #as_flex_table() maintains identation, footnotes, spanning headers
    } else {
      summ2
    }
    
    summ3
    
  }

### continous_by(Two way Anova) strata summary tables
continous_by_strata_table <- 
  function(df, foot_note = "", caption = "", strata_var, continous_var, by_vars, display_mean = TRUE,
           sum=FALSE, p_value = TRUE, continous_digits = 1, include_vars = NULL, flex_table = TRUE) {
    
    ### dividing factor/character/logical and numeric/integer variables
    factor_character_vars <- names(df[sapply(df, function(v) is.factor(v) | is.character(v) | is.logical(v) | is.Date(v))])
    
    df <- (df %>%
             dplyr::select(all_of(c(factor_character_vars, continous_var))
                    ) %>%
             dplyr::mutate(across(all_of(factor_character_vars), sjlabelled::as_factor))
           )
    
    index_strata <- df[[strata_var]]
    
    label_strata <- if (is.null(labelled::var_label(index_strata))) {x
      } else {
        labelled::var_label(index_strata)
        }
    
    out <- lapply(by_vars, function(y){
      summ2 <- if (p_value == FALSE) {
        summ1 <- if (sum == FALSE) {
          summ <- if (display_mean == TRUE) {
          tbl_strata(df,
                   strata = any_of(strata_var),
                   .tbl_fun = ~ .x %>%
                     gtsummary::tbl_continuous(
                                   variable = any_of(continous_var)
                                  , by = any_of(y)
                                  , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                                  , statistic = everything() ~ c(
                                    "{mean} ({sd})")
                                  , digits = list(everything() ~ continous_digits
                                                  )
                                  ) %>% 
                     modify_header(label = "**Variables**" # update the column header
                     ) %>% 
                     bold_labels() %>%
                     italicize_levels() %>% 
                     modify_footnote(all_stat_cols() ~ foot_note) %>%
                     modify_caption(caption)
                   , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                   ) 
          } else {
            tbl_strata(df,
                       strata = any_of(strata_var),
                       .tbl_fun = ~ .x %>%
                         gtsummary::tbl_continuous(
                           variable = any_of(continous_var)
                           , by = any_of(y)
                           , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                           , statistic = everything() ~ c(
                             "{mean} ({sd}) / {median} ({p25}, {p75})")
                           , digits = list(everything() ~ continous_digits
                           )
                         ) %>% 
                         modify_header(label = "**Variables**" # update the column header
                         ) %>% 
                         bold_labels() %>%
                         italicize_levels() %>% 
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption)
                       , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
            )
          }
          summ
          } else { 
            summ <- if (display_mean == TRUE) {
              tbl_strata(df,
                         strata = any_of(strata_var),
                         .tbl_fun = ~ .x %>%
                           gtsummary::tbl_continuous(
                             variable = any_of(continous_var)
                             , by = any_of(y)
                             , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                             , statistic = everything() ~ c(
                               "{sum} / {mean} ({sd})")
                             , digits = list(everything() ~ continous_digits
                             )
                           ) %>% 
                           modify_header(label = "**Variables**"# update the column header
                           ) %>% 
                           bold_labels() %>%
                           italicize_levels() %>% 
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption) 
                         , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                         ) 
            } else {
              tbl_strata(df,
                         strata = any_of(strata_var),
                         .tbl_fun = ~ .x %>%
                           gtsummary::tbl_continuous(
                             variable = any_of(continous_var)
                             , by = any_of(y)
                             , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                             , statistic = everything() ~ c(
                               "{sum}")
                             , digits = list(everything() ~ continous_digits
                             )
                           ) %>% 
                           modify_header(label = "**Variables**"# update the column header
                           ) %>% 
                           bold_labels() %>%
                           italicize_levels() %>% 
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption) 
                         , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                         )
            } 
            } 
        summ1
        
        } else {
        summ1 <- if (sum == FALSE) {
          summ <- if (display_mean == TRUE) {
            tbl_strata(df,
                       strata = any_of(strata_var),
                       .tbl_fun = ~ .x %>%
                         gtsummary::tbl_continuous(
                           variable = any_of(continous_var)
                           , by = any_of(y)
                           , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                           , statistic = everything() ~ c(
                             "{mean} ({sd})")
                           , digits = list(everything() ~ continous_digits
                           )
                         ) %>%
                         bold_labels() %>%
                         italicize_levels() %>% 
                         add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
                         bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
                         modify_table_body(
                           ~ .x %>%
                             dplyr::relocate(c(statistic), .before = p.value)
                           ) %>%
                         modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                                        ) %>% 
                         modify_header(label = "**Variables**", statistic = "**Test statistic**"
                                       # update the column header
                                       ) %>%
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption)
                       , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                       )
          } else {
            tbl_strata(df,
                       strata = any_of(strata_var),
                       .tbl_fun = ~ .x %>%
                         gtsummary::tbl_continuous(
                           variable = any_of(continous_var)
                           , by = any_of(y)
                           , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                           , statistic = everything() ~ c(
                             "{mean} ({sd}) / {median} ({p25}, {p75})")
                           , digits = list(everything() ~ continous_digits
                           )
                         ) %>%
                         bold_labels() %>%
                         italicize_levels() %>% 
                         add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
                         bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05) 
                         modify_table_body(
                           ~ .x %>%
                             dplyr::relocate(c(statistic), .before = p.value)
                           ) %>%
                         modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                                        ) %>% 
                         modify_header(label = "**Variables**", statistic = "**Test statistic**"
                                       # update the column header
                                       ) %>%
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption)
                       , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                       )
          }
          
          summ
        } else {
          summ <- if (display_mean == TRUE) {
            tbl_strata(df,
                       strata = any_of(strata_var),
                       .tbl_fun = ~ .x %>%
                         gtsummary::tbl_continuous(
                           variable = any_of(continous_var)
                           , by = any_of(y)
                           , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                           , statistic = everything() ~ c(
                             "{sum} / {mean} ({sd})")
                           , digits = list(everything() ~ continous_digits
                           )
                         ) %>% 
                         bold_labels() %>%
                         italicize_levels() %>%
                         add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
                         bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05) 
                         modify_table_body(
                           ~ .x %>%
                             dplyr::relocate(c(statistic), .before = p.value)
                           ) %>%
                         modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                                        ) %>% 
                         modify_header(label = "**Variables**", statistic = "**Test statistic**"
                                       # update the column header
                                       ) %>%
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption)
                       , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                       ) 
            } else {
              tbl_strata(df,
                         strata = any_of(strata_var),
                         .tbl_fun = ~ .x %>%
                           gtsummary::tbl_continuous(
                             variable = any_of(continous_var)
                             , by = any_of(y)
                             , include = if (is.null(include_vars)) {everything()} else {any_of(include_vars)}
                             , statistic = everything() ~ c(
                               "{sum}")
                             , digits = list(everything() ~ continous_digits
                             )
                           ) %>%
                           bold_labels() %>%
                           italicize_levels() %>%
                           add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
                           bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
                           modify_table_body(
                             ~ .x %>%
                               dplyr::relocate(c(statistic), .before = p.value)
                             ) %>%
                           modify_fmt_fun(c(statistic) ~ label_style_number(digits = continous_digits)
                                          ) %>% 
                           modify_header(label = "**Variables**", statistic = "**Test statistic**"
                                         # update the column header
                                         ) %>%
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption)
                         , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                         ) 
            }
          summ
        }
        summ1
        
      }
      
      summ3 <- if (flex_table == TRUE) { 
        summ2 %>%
          gtsummary::as_flex_table() 
        # as_kable_extra() covert gtsummary object to knitrkable object. 
        #as_flex_table() maintains identation, footnotes, spanning headers
      } else {
        summ2
      }
      
      summ3
      
    })
    out
    
  }

