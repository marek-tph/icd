icd_data_icd9cm_leaf_v32 <- function() {
  if (exists("icd9cm2014_leaf")) return(get("icd9cm2014_leaf"))
  get_icd9cm2014_leaf()
}

.get_lazy <- function(var_name) {
  ns <- asNamespace("icd")
  lz <- ns$.__NAMESPACE__.$lazydata
  get(var_name, lz)
}

.exists_in_lazy <- function(var_name) {
  ns <- asNamespace("icd")
  lz <- ns$.__NAMESPACE__.$lazydata
  exists(var_name, lz)
}

.get_anywhere <- function(var_name, fetch = FALSE) {
  if (.verbose()) {
    message(".get_anywhere: ", var_name)
    if (.exists_in_lazy(var_name)) message("lazy")
    if (.exists_in_cache(var_name)) message("cache")
    ns <- asNamespace("icd")
    if (exists(var_name, ns)) message("from package namespace itself")
    if (fetch) message("will try to fetch") else message("not going to fetch")
  }
  if (.exists_in_lazy(var_name)) return(.get_lazy(var_name))
  if (.exists_in_cache(var_name)) return(.get_from_cache(var_name))
  ns <- asNamespace("icd")
  if (exists(var_name, ns)) return(get(var_name, ns))
  if (fetch && exists(.get_fetcher_name(var_name), ns, mode = "function")) {
    return(.get_fetcher_fun(var_name)())
  }
  .absent_action_switch(
    paste(var_name, "not available in icd.data regular or lazy data")
  )
}

.exists_anywhere <- function(var_name, fetch = FALSE) {
  ns <- asNamespace("icd")
  if (.exists_in_lazy(var_name) ||
    .exists_in_cache(var_name) ||
    exists(var_name, ns)) {
    return(TRUE)
  }
  if (fetch && exists(.get_fetcher_name(var_name), ns, mode = "function")) {
    dat <- with_absent_action("silent", .get_fetcher_fun(var_name))
    if (!is.null(dat) && is.data.frame(dat)) return(TRUE)
  }
  FALSE
}
