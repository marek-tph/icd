icd_data_icd9cm_leaf_v32 <- function() {
  if (exists("icd9cm2014_leaf")) return(get("icd9cm2014_leaf"))
  get_icd9cm2014_leaf()
}
