paletter <- function(df, var){
  pal <- leaflet::colorNumeric(c("#eb4034", "#008a02"), floor(min(df[[base::as.character(var)]])):ceiling(max(df[[base::as.character(var)]])))
  palfunc <- function(x){
    dplyr::case_when(
      x > 0 ~ pal(x),
      x == 0 ~ "#b0b0b0",
      is.na(x) ~ "#b0b0b0",
      is.null(x) ~ "#b0b0b0",
      TRUE ~ as.character(x)
    )
  }
  return(palfunc(df[[var]]))
}