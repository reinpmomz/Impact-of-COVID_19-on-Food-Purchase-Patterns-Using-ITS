library(gtsummary)

## Setting gt summary theme
my_gtsummary_theme <- gtsummary::set_gtsummary_theme(
  list(
    ## round large p-values to three places
    #"pkgwide-fn:pvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 3),
    ## report mean (sd) and n (percent) as default stats in `tbl_summary()`
    #"tbl_summary-fn:percent_fun" = function(x) gtsummary::style_percent(x, digits = 1), 
    ## less than 10% are rounded to digits + 1 places
    #"tbl_summary-str:continuous_stat" = "{mean} ({sd})",
    "style_number-arg:big.mark" = ""
    #"tbl_summary-str:categorical_stat" = "{n} ({p}%)" #"{n} / {N} ({p}%)"
  )
)

### Setting `Compact` theme
gtsummary_compact_theme <- gtsummary::theme_gtsummary_compact()

