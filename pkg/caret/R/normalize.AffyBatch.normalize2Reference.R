"normalize.AffyBatch.normalize2Reference" <-
  function (abatch, type = c("separate", "pmonly", "mmonly", "together"), ref = NULL) 
{
  type <- match.arg(type)
  if ((type == "pmonly") | (type == "separate")) {
    pms <- unlist(pmindex(abatch))
    noNA <- rowSums(is.na(intensity(abatch)[pms, , drop = FALSE])) == 0
    pms <- pms[noNA]
    intensity(abatch)[pms, ] <- normalize2Reference(
                                                    intensity(abatch)[pms, , drop = FALSE],
                                                    refData = ref, ties = TRUE)
  }
  if ((type == "mmonly") | (type == "separate")) {
    mms <- unlist(mmindex(abatch))
    noNA <- rowSums(is.na(intensity(abatch)[mms, , drop = FALSE])) == 0
    mms <- mms[noNA]
    intensity(abatch)[mms, ] <- normalize2Reference(
                                                    intensity(abatch)[mms, , drop = FALSE],
                                                    refData = ref, ties = TRUE)
  }
  if (type == "together") {
    pms <- unlist(indexProbes(abatch, "both"))
    intensity(abatch)[pms, ] <- normalize2Reference(
                                                    intensity(abatch)[pms, , drop = FALSE],
                                                    refData = ref, ties = TRUE)
  }
  return(abatch)
}

