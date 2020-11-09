MoransI <- function(SDF, vec="sum_hex_vl_fob", style="B"){
  w <- spdep::poly2nb(SDF, row.names=colnames(SDF))
  ww <-  spdep::nb2listw(w, style=style)
  res <- paste0(
    "Moran's I: ", round(spdep::moran.test(SDF[[as.character(vec)]], ww, randomisation=FALSE)$estimate[[1]], 3), 
    "\n", 
    "p-valor: ", round(spdep::moran.test(SDF[[as.character(vec)]], ww, randomisation=FALSE)$p.value, 3)+0.001
  )
  return(res)
}
